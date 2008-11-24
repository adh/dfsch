#include <dfsch/random.h>
#include <sys/times.h>
#include <sys/resource.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

dfsch_type_t dfsch_random_state_type = {
  DFSCH_ABSTRACT_TYPE,
  NULL,
  0,
  "random-state",
};
dfsch_type_t dfsch_random_state_type_type = {
  DFSCH_META_TYPE,
  DFSCH_STANDARD_TYPE,
  sizeof(dfsch_random_state_type),
  "random-state-type",
};

static dfsch_object_t* random_state;

typedef struct random_init_t {
  uint8_t uninitialized[16];
  time_t time;
  struct tms tms;
  clock_t clock;
  struct rusage rusage;
  uint8_t sys_random[16];
} random_init_t;

static dfsch_object_t* make_default_state(){
  random_init_t seed;
  int fd;
  seed.time = time(NULL);
  seed.clock = times(&seed.tms);
  getrusage(RUSAGE_SELF, &seed.rusage);
  
  fd = open("/dev/urandom", O_RDONLY | O_NONBLOCK);
  if (fd < 0) {
    fd = open("/dev/random", O_RDONLY | O_NONBLOCK);
  }
  if (fd >= 0){
    read(fd, &seed.sys_random, 16);
    close(fd);
  }

  return dfsch_make_default_random_state(&seed, sizeof(random_init_t));
}

dfsch_object_t* dfsch_get_random_state(){
  if (!random_state){
    random_state = make_default_state();
  }
  return random_state;
}
void dfsch_set_random_state(dfsch_object_t* state){
  random_state = state;
}

void dfsch_random_get_bytes(dfsch_object_t* state, uint8_t* buf, size_t len){
  if (!state){
    state = dfsch_get_random_state();
  }
  if (DFSCH_TYPE_OF(state)->type != DFSCH_RANDOM_STATE_TYPE_TYPE){
    dfsch_error("Not a random state", state);
  }
  ((dfsch_random_state_type_t*)(DFSCH_TYPE_OF(state)))->get_bytes(state, buf, len);
}
int dfsch_random_get_integer(dfsch_object_t* state, int max){
  int bits;
  int val;
  do {
    dfsch_random_get_bytes(state, &bits, sizeof(unsigned int));
    if (bits < 0){
      bits = -bits;
    }
    val = bits % max;
  } while (bits - val - (max+1) < 0);
  return val;
}
double dfsch_random_get_double(dfsch_object_t* state){
  uint64_t val;
  dfsch_random_get_bytes(state, &val, sizeof(uint64_t));
  val &= (1LL << 53) - 1;
  return (double)val/(double)(1LL << 53);
}
dfsch_object_t* dfsch_random_get_number(dfsch_object_t* state, 
                                        dfsch_object_t* max){
  if (DFSCH_TYPE_OF(max) == DFSCH_FIXNUM_TYPE){
    return DFSCH_MAKE_FIXNUM(dfsch_random_get_integer(state, 
                                                      DFSCH_FIXNUM_REF(max)));
  }
  return 
    dfsch_number_mul(dfsch_make_number_from_double(dfsch_random_get_double(state)),
                     max);
}
dfsch_object_t* dfsch_random_get_bignum(dfsch_object_t* state,
                                        size_t len){
  uint8_t *buf;
  if ((len % 8) != 0){
    buf = GC_MALLOC_ATOMIC(len / 8 + 1);
    dfsch_random_get_bytes(state, buf, len / 8 + 1);
    *buf &= (1 << (len & 7)) - 1;
    return dfsch_bignum_to_number(dfsch_bignum_from_bytes(buf, len / 8 + 1));
  } else {
    buf = GC_MALLOC_ATOMIC(len / 8);
    dfsch_random_get_bytes(state, buf, len / 8);
    return dfsch_bignum_to_number(dfsch_bignum_from_bytes(buf, len / 8));
  }
}

typedef struct default_state_t {
  dfsch_type_t* type;
  int counter;
  uint32_t sum;
  uint32_t state[8][2];
  uint32_t skeys[4];
  uint32_t okeys[8];
} default_state_t;

static void default_get_bytes(default_state_t* state, uint8_t* buf, size_t len){
  uint32_t v = state->sum;
  uint32_t u = state->counter;
  int i;
  state->counter++;
  for (i = 0; i < 8; i++){
    v += (((u << 4) ^ (u >> 5)) + u) ^ 
      (state->sum + state->skeys[state->sum & 3]);
    state->sum += 0x9e3779b9;
    u += (((u << 4) ^ (u >> 5)) + u) ^ 
      (state->sum + state->state[i][0]);
    v += (((u << 4) ^ (u >> 5)) + u) ^ 
      (state->sum + state->state[i][1]);
    state->sum += 0x9e3779b9;
    u += (((u << 4) ^ (u >> 5)) + u) ^ 
      (state->sum + state->skeys[(state->sum >> 11) & 3]);
    state->okeys[(state->counter + 1) & 7] ^= v ^ u;
  }
  state->state[state->counter & 7][0] = u;
  state->state[state->counter & 7][1] = v;

  while (len){
    for (i = 0; i < 4; i++){
      v += (((u << 4) ^ (u >> 5)) + u) ^ 
        (state->sum + state->okeys[state->sum & 7]);
      state->sum += 0x9e3779b9;
      u += (((u << 4) ^ (u >> 5)) + u) ^ 
        (state->sum + state->okeys[(state->sum >> 11) & 7]);
    }
    *buf = u;
    buf++;
    len--;
    u >>= 8;
    v >>= 8;
    u += state->sum;
    v += state->counter;
  }
}

dfsch_random_state_type_t dfsch_default_random_state_type = {
  {
    DFSCH_RANDOM_STATE_TYPE_TYPE,
    DFSCH_RANDOM_STATE_TYPE,
    sizeof(default_state_t),
    "default-random-state",
    NULL,
    NULL,
    NULL,
    NULL
  },
  (dfsch_random_get_bytes_t)default_get_bytes,
  0
};
dfsch_object_t* dfsch_make_default_random_state(uint8_t* seed, size_t len){
  default_state_t* state = dfsch_make_object(DFSCH_DEFAULT_RANDOM_STATE_TYPE);
  uint32_t u = 0x01234567;
  uint32_t v = 0x89abcdef;
  int i, j;
  
  state->counter = 0;
  state->sum = 0;

  for (j = 0; j < 8; j++){
    for (i = 0; i < 8; i++){
      v += (((u << 4) ^ (u >> 5)) + u) ^ 
        (state->sum + (seed[state->sum % len] | 
                       (seed[(state->sum + 1) % len] << 8) |
                       (seed[(state->sum + 2) % len] << 16) |
                       (seed[(state->sum + 3) % len] << 24)));
      state->sum += 0x9e3779b9;
      u += (((u << 4) ^ (u >> 5)) + u) ^ 
        (state->sum + (seed[(state->sum >> 11) % len] |
                       (seed[((state->sum >> 11) + 1) % len] << 8) |
                       (seed[((state->sum >> 11) + 2) % len] << 16) |
                       (seed[((state->sum >> 11) + 3) % len] << 24)));
    }
    state->state[j][0] = u;
    state->state[j][1] = v;
  }

  for (j = 0; j < 2; j++){
    for (i = 0; i < 8; i++){
      v += (((u << 4) ^ (u >> 5)) + u) ^ 
        (state->sum + (seed[state->sum % len] | 
                       (seed[(state->sum + 1) % len] << 8) |
                       (seed[(state->sum + 2) % len] << 16) |
                       (seed[(state->sum + 3) % len] << 24)));
      state->sum += 0x9e3779b9;
      u += (((u << 4) ^ (u >> 5)) + u) ^ 
        (state->sum + (seed[(state->sum >> 11) % len] |
                       (seed[((state->sum >> 11) + 1) % len] << 8) |
                       (seed[((state->sum >> 11) + 2) % len] << 16) |
                       (seed[((state->sum >> 11) + 3) % len] << 24)));
    }
    state->skeys[j + 0] = u;
    state->skeys[j + 1] = v;
  }

  for (j = 0; j < 2; j++){
    for (i = 0; i < 8; i++){
      v += (((u << 4) ^ (u >> 5)) + u) ^ 
        (state->sum + (seed[state->sum % len] | 
                       (seed[(state->sum + 1) % len] << 8) |
                       (seed[(state->sum + 2) % len] << 16) |
                       (seed[(state->sum + 3) % len] << 24)));
      state->sum += 0x9e3779b9;
      u += (((u << 4) ^ (u >> 5)) + u) ^ 
        (state->sum + (seed[(state->sum >> 11) % len] |
                       (seed[((state->sum >> 11) + 1) % len] << 8) |
                       (seed[((state->sum >> 11) + 2) % len] << 16) |
                       (seed[((state->sum >> 11) + 3) % len] << 24)));
    }
    state->okeys[j + 0] = u;
    state->okeys[j + 1] = v;
  }
  
  return (dfsch_object_t*) state;
}
typedef struct file_state_t {
  dfsch_type_t* type;
  int fd;
} file_state_t;

static void file_get_bytes(file_state_t* state, uint8_t* buf, size_t len){
  ssize_t r;
  while (len){
    r = read(state->fd, buf, len);
    if (r < 0){
      if (errno != EAGAIN){
        dfsch_error("Error reading random source", state);
      }
    }
  }
}

dfsch_random_state_type_t dfsch_file_random_state_type = {
  {
    DFSCH_RANDOM_STATE_TYPE_TYPE,
    DFSCH_RANDOM_STATE_TYPE,
    sizeof(file_state_t),
    "file-random-state",
    NULL,
    NULL,
    NULL,
    NULL
  },
  (dfsch_random_get_bytes_t)file_get_bytes,
  0
};
static void file_state_finalizer(file_state_t* state){
  if (state->fd >= 0){
    close(state->fd);
    state->fd = -1;
  }
}

dfsch_object_t* dfsch_make_file_random_state(char* filename){
  file_state_t* state = dfsch_make_object(DFSCH_FILE_RANDOM_STATE_TYPE);

  state->fd = open(filename, O_RDONLY);
  if (state->fd < 0){
    dfsch_error("Cannot open file", dfsch_make_string_cstr(filename));
  }

  GC_REGISTER_FINALIZER(state, (GC_finalization_proc)file_state_finalizer,
                        NULL, NULL, NULL);

  return state;
}

DFSCH_DEFINE_PRIMITIVE(random_bytes, 0){
  size_t len;
  uint8_t *buf;
  dfsch_object_t* state;
  DFSCH_LONG_ARG(args, len);
  DFSCH_OBJECT_ARG_OPT(args, state, NULL);
  DFSCH_ARG_END(args);
  buf = GC_MALLOC_ATOMIC(len);
  dfsch_random_get_bytes(state, buf, len);

  return dfsch_make_string_buf(buf, len);
}

DFSCH_DEFINE_PRIMITIVE(random_flonum, 0){
  dfsch_object_t* state;
  DFSCH_OBJECT_ARG_OPT(args, state, NULL);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_double(dfsch_random_get_double(state));
}
DFSCH_DEFINE_PRIMITIVE(random_bignum, 0){
  size_t len;
  dfsch_object_t* state;
  DFSCH_LONG_ARG(args, len);
  DFSCH_OBJECT_ARG_OPT(args, state, NULL);
  DFSCH_ARG_END(args);

  return dfsch_random_get_bignum(state, len);
}
DFSCH_DEFINE_PRIMITIVE(random, 0){
  dfsch_object_t* max;
  dfsch_object_t* state;
  DFSCH_OBJECT_ARG(args, max);
  DFSCH_OBJECT_ARG_OPT(args, state, NULL);
  DFSCH_ARG_END(args);

  return dfsch_random_get_number(state, max);
}
DFSCH_DEFINE_PRIMITIVE(make_default_random_state, 0){
  dfsch_strbuf_t* seed;
  DFSCH_BUFFER_ARG(args, seed);
  DFSCH_ARG_END(args);
  return dfsch_make_default_random_state(seed->ptr, seed->len);
}
DFSCH_DEFINE_PRIMITIVE(make_file_random_state, 0){
  char* filename;
  DFSCH_STRING_ARG(args, filename);
  DFSCH_ARG_END(args);
  return dfsch_make_file_random_state(filename);
}

void dfsch__random_register(dfsch_object_t *ctx){ 
  dfsch_define_cstr(ctx, "<random-state>", DFSCH_RANDOM_STATE_TYPE);
  dfsch_define_cstr(ctx, "<default-random-state>", 
                    DFSCH_DEFAULT_RANDOM_STATE_TYPE);
  dfsch_define_cstr(ctx, "<file-random-state>", DFSCH_FILE_RANDOM_STATE_TYPE);

  dfsch_define_cstr(ctx, "random-bytes", DFSCH_PRIMITIVE_REF(random_bytes));
  dfsch_define_cstr(ctx, "random-flonum", DFSCH_PRIMITIVE_REF(random_flonum));
  dfsch_define_cstr(ctx, "random-bignum", DFSCH_PRIMITIVE_REF(random_bignum));
  dfsch_define_cstr(ctx, "random", DFSCH_PRIMITIVE_REF(random));

  dfsch_define_cstr(ctx, "make-default-random-state", 
                    DFSCH_PRIMITIVE_REF(make_default_random_state));
  dfsch_define_cstr(ctx, "make-file-random-state", 
                    DFSCH_PRIMITIVE_REF(make_file_random_state));
}
