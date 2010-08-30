#include <dfsch/lib/crypto.h>
#include <dfsch/random.h>
#include <stdlib.h>
#ifdef unix
#include <sys/times.h>
#include <sys/resource.h>
#endif

#ifdef __WIN32__
#include <windows.h>
#include <rpc.h>
#endif

#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

typedef struct prng_state_t {
  dfsch_type_t* type;
  dfsch_salsa20_state_t salsa;
  uint8_t output[64];
  size_t output_offset;
  int use_system_sources_p;
} prng_state_t;

typedef struct system_sources_t {
  uint8_t uninitialized[16];
  time_t time;
  void* sp;
  uint8_t sys_random[16];
  int stdlib_random;

#ifdef unix
  int fd;
  struct tms tms;
  clock_t clock;
  struct rusage rusage;
  pid_t pid;
  int hostid;
#endif

#ifdef __WIN32__
  DWORD pid;
  DWORD tid;
  DWORD ticks;
  UUID uuid;
#endif
} system_sources_t;


static dfsch_crypto_hash_context_t* get_background_entropy_hash();
static void get_data_from_background_pool(uint8_t buf[64]){
  dfsch_crypto_hash_context_t* h = get_background_entropy_hash();
  h->algo->result(h, buf);
  h->algo->setup(h, NULL, 0);
  h->algo->process(h, buf, 64);
  h = dfsch_crypto_hash_setup(DFSCH_CRYPTO_SHA512, NULL, 0);
  h->algo->process(h, buf, 64);
  h->algo->result(h, buf);
}

static void fill_system_sources(system_sources_t* ss){
  int fd;
  ss->time = time(NULL);
  ss->sp = &fd;
  ss->stdlib_random = random();
#ifdef unix
  ss->pid = getpid();
  ss->clock = times(&(ss->tms));
  ss->hostid = gethostid();
  getrusage(RUSAGE_SELF, &(ss->rusage));
  
  fd = open("/dev/urandom", O_RDONLY | O_NONBLOCK);
  if (fd < 0) {
    fd = open("/dev/random", O_RDONLY | O_NONBLOCK);
  }
  if (fd >= 0){
    read(fd, &(ss->sys_random), 16);
    close(fd);
  }
  ss->fd = fd;
#endif
#ifdef __WIN32__
  ss->pid = GetCurrentProcessId();
  ss->tid = GetCurrentThreadId();
  ss->ticks = GetTickCount();
  UuidCreate(&(ss->uuid));
#endif
}

static void hash_system_sources(uint8_t buf[32], uint8_t old_output[64]){
  dfsch_crypto_hash_context_t* 
    hs = dfsch_crypto_hash_setup(DFSCH_CRYPTO_SHA256, NULL, 0);
  system_sources_t ss;
  fill_system_sources(&ss);
  hs->algo->process(hs, &ss, sizeof(system_sources_t));
  hs->algo->process(hs, old_output, 64);
  hs->algo->process(hs, &hs, sizeof(void*));
  hs->algo->process(hs, buf, 32);
  
  if (old_output[0] == 0){
    int fd = open("/dev/random", O_RDONLY);
    uint8_t buf[16];
    if (fd >= 0){
      read(fd, buf, 16);
      hs->algo->process(hs, buf, 16);
      hs->algo->process(hs, &fd, sizeof(int));
      close(fd);
    }
  }

  if (old_output[1] == 0){
    uint8_t buf[64];
    get_data_from_background_pool(buf);
    hs->algo->process(hs, buf, 64);    
  }

  hs->algo->result(hs, buf);
}

static char* get_random_seed_file_name(){
  char* randfile = getenv("DFSCH_RANDFILE");
  char* homedir = getenv("HOME");
  char* tempdir = getenv("TEMP"); /* windows compatibility */
  
  if (randfile){
    return randfile;
  } else if (homedir) {
    return dfsch_saprintf("%s/.dfsch-random", homedir);
  } else if (tempdir) {
    return dfsch_saprintf("%s/.dfsch-random", tempdir);    
  }
}

static void write_random_file(){
  char* fname = get_random_seed_file_name();
  int fd;
  uint8_t buf[64];

  if (!fname){
    return;
  }

  fd = open(fname, O_CREAT | O_TRUNC | O_WRONLY, 0600);
  if (fd < 0){
    return;
  }

  get_data_from_background_pool(buf);
  write(fd, buf, 64);
  close(fd);
}

static void read_random_file(){
  char* fname = get_random_seed_file_name();
  int fd;
  uint8_t buf[1024];

  if (!fname){
    return;
  }

  fd = open(fname, O_RDONLY, 0600);
  if (fd < 0){
    return;
  }

  read(fd, buf, 64);
  close(fd);
  dfsch_crypto_put_entropy(buf, 64);
}


static dfsch_crypto_hash_context_t* get_background_entropy_hash(){
  static dfsch_crypto_hash_context_t* hash = NULL;
  if (!hash){
    system_sources_t ss;
    hash = dfsch_crypto_hash_setup(DFSCH_CRYPTO_SHA512, NULL, 0);
    fill_system_sources(&ss);
    hash->algo->process(hash, &ss, sizeof(system_sources_t));
    read_random_file();
    write_random_file();
    atexit(write_random_file);
  }
  return hash;
}


static void prng_get_bytes(prng_state_t* state, uint8_t* buf, size_t len){
  while (len){
    if (state->output_offset >= 64){
      if (state->use_system_sources_p){
        uint8_t buf[32];
        hash_system_sources(buf, state->output);
        dfsch_salsa20_addkey(&(state->salsa), buf);
      }

      dfsch_salsa20_get_keystream_block(&(state->salsa), state->output);

      state->output_offset = 0;
    }
    *buf = state->output[state->output_offset];
    state->output_offset++;
    len--;
    buf++;
  }
}


dfsch_random_state_type_t dfsch_crypto_prng_state_type = {
  {
    DFSCH_RANDOM_STATE_TYPE_TYPE,
    DFSCH_RANDOM_STATE_TYPE,
    sizeof(prng_state_t),
    "crypto:prng-state",
    NULL,
    NULL,
    NULL,
    NULL
  },
  (dfsch_random_get_bytes_t)prng_get_bytes,
  0
};

dfsch_object_t* dfsch_crypto_make_prng_state(uint8_t* seed, int seed_len,
                                             int use_system_sources_p){
  prng_state_t* state = dfsch_make_object(DFSCH_CRYPTO_PRNG_STATE_TYPE);
  dfsch_crypto_hash_context_t* 
    hs = dfsch_crypto_hash_setup(DFSCH_CRYPTO_SHA256, NULL, 0);
  uint8_t buf[32];
  uint64_t iv;
  hs->algo->process(hs, seed, seed_len);
  hs->algo->process(hs, buf, 32);

  state->use_system_sources_p = use_system_sources_p;

  if (use_system_sources_p){
    iv = time(NULL);
  } else {
    iv = 0xdeadbeef;
  }

  dfsch_salsa20_setkey(&(state->salsa), buf);
  dfsch_salsa20_setiv(&(state->salsa), iv);
  state->output_offset = 64;
  dfsch_salsa20_get_keystream_block(&(state->salsa), state->output);
  iv <<= 32;
  iv += 0x31415926;
  dfsch_salsa20_setiv(&(state->salsa), iv);

  return state;
}
dfsch_object_t* dfsch_crypto_get_fast_prng_state(){
  static dfsch_object_t* state = NULL;
  if (!state){
    uint32_t buf[96];
    strncpy(buf, dfsch_get_build_id(), 64);
    strncat(buf, __DATE__ __TIME__, 64);
    hash_system_sources(buf, buf);
    get_data_from_background_pool(buf + 32);
    state = dfsch_crypto_make_prng_state(buf, 96, 0);
  }
  return state;
}
dfsch_object_t* dfsch_crypto_get_safe_prng_state(){
  static dfsch_object_t* state = NULL;
  if (!state){
    uint32_t buf[96];
    strncpy(buf, dfsch_get_build_id(), 64);
    strncat(buf, __DATE__ __TIME__, 64);
    hash_system_sources(buf, buf);
    get_data_from_background_pool(buf + 32);
    state = dfsch_crypto_make_prng_state(buf, 96, 1);
  }
  return state;
}
  
void dfsch_crypto_put_entropy(uint8_t* buf, size_t len){
  dfsch_crypto_hash_context_t* h = get_background_entropy_hash();
  h->algo->process(h, buf, len);
}

