/*
 * dfsch - Scheme-like Lisp dialect
 *   Random number generation frmaework
 * Copyright (C) 2005-2009 Ales Hakl
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */

#include <dfsch/random.h>
#include <sys/time.h>
#ifdef unix
#include <sys/times.h>
#include <sys/resource.h>
#endif

#include <dfsch/bignum.h>

#ifdef __WIN32__
#include <windows.h>
#endif

#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <dfsch/sha256.h>

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
  void* sp;
  int mii_chan_constant;
  struct timeval tv;
#ifdef unix
  struct tms tms;
  clock_t clock;
  struct rusage rusage;
  pid_t pid;
#endif

#ifdef __WIN32__
  DWORD pid;
  DWORD tid;
#endif

  uint8_t sys_random[16];
} random_init_t;

static void init_seed(random_init_t* seed){
  int fd;
  seed->time = time(NULL);
  seed->sp = &seed;
  seed->mii_chan_constant = 8; 
  /* You just come up with some random number, like eight -- Mitsuki  :) */
  gettimeofday(&(seed->tv), NULL);
#ifdef unix
  seed->pid = getpid();
  seed->clock = times(&(seed->tms));
  getrusage(RUSAGE_SELF, &(seed->rusage));
  
  fd = open("/dev/urandom", O_RDONLY | O_NONBLOCK);
  if (fd < 0) {
    fd = open("/dev/random", O_RDONLY | O_NONBLOCK);
  }
  if (fd >= 0){
    read(fd, seed->sys_random, 16);
    close(fd);
  }
#endif
#ifdef __WIN32__
  seed->pid = GetCurrentProcessId();
  seed->tid = GetCurrentThreadId();
#endif
}

static dfsch_object_t* make_default_state(){
  random_init_t seed;
  init_seed(&seed);
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
int64_t dfsch_random_get_integer(dfsch_object_t* state, int64_t max){
  int64_t bits;
  uint64_t val;
  do {
    dfsch_random_get_bytes(state, &bits, sizeof(int64_t));
    if (bits < 0){
      bits = -bits;
    }
    val = bits % max;
  } while (bits - val + (max+1) < 0);
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
    int64_t v = dfsch_random_get_integer(state, 
                                         DFSCH_FIXNUM_REF(max));
    return DFSCH_MAKE_FIXNUM(v);
  } else if (DFSCH_TYPE_OF(max) == DFSCH_BIGNUM_TYPE){
    return dfsch_number_mod(dfsch_random_get_bignum(state,
                                                    dfsch_number_msb(max)),
                            max); // XXX: this has skewed distribution
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
    return dfsch_bignum_to_number(dfsch_bignum_from_bytes(buf, len / 8 + 1, 0));
  } else {
    buf = GC_MALLOC_ATOMIC(len / 8);
    dfsch_random_get_bytes(state, buf, len / 8);
    return dfsch_bignum_to_number(dfsch_bignum_from_bytes(buf, len / 8, 0));
  }
}

typedef struct default_state_t {
  dfsch_type_t* type;
  int mt_index;
  uint32_t mt[624];
  int ob_index;
  uint32_t obuf;
} default_state_t;

static uint8_t mt_get_one_byte(default_state_t* state){
  int i;
  uint32_t y;

  if (state->ob_index == 0){
    if (state->mt_index == 0){
      for (i = 0; i < 624; i++){
        y = (state->mt[i] &  0x80000000) +
          (state->mt[(i + 1) % 624] & 0x7fffffff);
        state->mt[i] = state->mt[(i + 397) % 624] ^ (y >> 1);
        if (y & 0x00000001){
          state->mt[i] ^= 0x9908b0df;
        }
      }
    }
    
    y = state->mt[state->mt_index];
    y ^= y >> 11;
    y ^= (y << 7) & 0x9d2c5680;
    y ^= (y << 15) & 0xefc60000;
    y ^= y >> 18;
    state->ob_index = 3;
    state->obuf = y >> 8;
    return y & 0xff;
  } else {
    state->ob_index--;
    y = state->obuf;
    state->obuf = y >> 8;
    return y & 0xff;
  }

}

static void default_get_bytes(default_state_t* state, uint8_t* buf, size_t len){
  while (len){
    *buf = mt_get_one_byte(state);
    buf++;
    len--;
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
  1
};
dfsch_object_t* dfsch_make_default_random_state(uint8_t* seed, size_t len){
  default_state_t* state = dfsch_make_object(DFSCH_DEFAULT_RANDOM_STATE_TYPE);
  int i;
  
  state->ob_index = 0;
  state->mt_index = 0;
  
  if (len == 0){
    seed = "0000";
    len = 4;
  }

  state->mt[0] = seed[0 % len] 
    | (seed[1 % len] << 8)
    | (seed[2 % len] << 16)
    | (seed[3 % len] << 24);

  for (i = 1; i < 624; i++){
    state->mt[i] = (seed[(i*4 + 0) % len] 
                    | (seed[(i*4 + 1) % len] << 8)
                    | (seed[(i*4 + 2) % len] << 16)
                    | (seed[(i*4 + 3) % len] << 24)) ^ 
      ((0x6c078965 * (state->mt[i - 1] ^ (state->mt[i-1] >> 30))) + 1);
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
    len -= r;
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

typedef struct lcg_state_t {
  dfsch_type_t* type;
  uint32_t state;
} lcg_state_t;

static uint8_t lcg_get_byte(lcg_state_t* lcg){
  lcg->state = (1103515245 * lcg->state) + 12345;
  return (lcg->state >> 24);
}

static void lcg_get_bytes(lcg_state_t* state, uint8_t* buf, size_t len){
  while (len){
    *buf = lcg_get_byte(state);
    buf++;
    len--;
  }
}

dfsch_random_state_type_t dfsch_lcg_random_state_type = {
  {
    DFSCH_RANDOM_STATE_TYPE_TYPE,
    DFSCH_RANDOM_STATE_TYPE,
    sizeof(lcg_state_t),
    "lcg-random-state",
    NULL,
    NULL,
    NULL,
    NULL
  },
  (dfsch_random_get_bytes_t)lcg_get_bytes,
  1
};

dfsch_object_t* dfsch_make_lcg_random_state(uint32_t seed){
  lcg_state_t* lcg = dfsch_make_object(DFSCH_LCG_RANDOM_STATE_TYPE);

  lcg->state = seed;

  return lcg;
}

static pthread_mutex_t id_mutex = PTHREAD_MUTEX_INITIALIZER;
static random_init_t id_seed;
static char id_last[32];
static int id_init = 0;

void dfsch_get_random_id(char buf[16]){
  uint64_t left;
  uint64_t right;
  dfsch_sha256_context_t ctx;

  pthread_mutex_lock(&id_mutex);

  if (!id_init){
    init_seed(&id_seed);
  }
  
  dfsch_sha256_setup(&ctx);
  dfsch_sha256_process(&ctx, &id_seed, sizeof(id_seed));
  dfsch_sha256_process(&ctx, id_last, 32);
  dfsch_sha256_result(&ctx, id_last);
  
  memcpy(buf, id_last, 16);

  pthread_mutex_unlock(&id_mutex);
}

void dfsch_get_random_scoped_id(char buf[20], char scope[16]){
  char sig_buf[32];
  dfsch_sha256_context_t ctx;
  dfsch_get_random_id(buf);
  
  dfsch_sha256_setup(&ctx);
  dfsch_sha256_process(&ctx, buf, 16);
  dfsch_sha256_process(&ctx, scope, 16);
  dfsch_sha256_result(&ctx, sig_buf);
  memcpy(buf + 16, sig_buf, 4);
}

int dfsch_check_scoped_id(char id[20], char scope[16]){
  char sig_buf[32];
  dfsch_sha256_context_t ctx;
  
  dfsch_sha256_setup(&ctx);
  dfsch_sha256_process(&ctx, id, 16);
  dfsch_sha256_process(&ctx, scope, 16);
  dfsch_sha256_result(&ctx, sig_buf);

  return memcmp(id + 16, sig_buf, 4) == 0;
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

  return dfsch_make_byte_vector(buf, len);
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

DFSCH_DEFINE_PRIMITIVE(make_lcg_random_state, 0){
  long seed;
  DFSCH_LONG_ARG(args, seed);
  DFSCH_ARG_END(args);
  return dfsch_make_lcg_random_state(seed);
}

DFSCH_DEFINE_PRIMITIVE(get_random_id, 0){
  char buf[16];
  DFSCH_ARG_END(args);

  dfsch_get_random_id(buf);

  return dfsch_make_byte_vector(buf, 16);
}

DFSCH_DEFINE_PRIMITIVE(get_random_scoped_id, 0){
  char buf[20];
  dfsch_strbuf_t* scope;
  DFSCH_BUFFER_ARG(args, scope);
  DFSCH_ARG_END(args);

  if (scope->len != 16 && scope->len != 20){
    dfsch_error("ID scope must have 16 or 20 bytes", NULL);
  }

  dfsch_get_random_scoped_id(buf, scope->ptr);

  return dfsch_make_byte_vector(buf, 20);
}

DFSCH_DEFINE_PRIMITIVE(check_scoped_id, 0){
  dfsch_strbuf_t* id;
  dfsch_strbuf_t* scope;
  DFSCH_BUFFER_ARG(args, id);
  DFSCH_BUFFER_ARG(args, scope);
  DFSCH_ARG_END(args);

  if (id->len != 20){
    dfsch_error("Scoped ID must have 20 bytes", NULL);
  }
  if (scope->len != 16 && scope->len != 20){
    dfsch_error("ID scope must have 16 or 20 bytes", NULL);
  }

  return dfsch_bool(dfsch_check_scoped_id(id->ptr, scope->ptr));
}



void dfsch__random_register(dfsch_object_t *ctx){ 
  dfsch_defcanon_cstr(ctx, "<random-state>", DFSCH_RANDOM_STATE_TYPE);
  dfsch_defcanon_cstr(ctx, "<default-random-state>", 
                    DFSCH_DEFAULT_RANDOM_STATE_TYPE);
  dfsch_defcanon_cstr(ctx, "<file-random-state>", DFSCH_FILE_RANDOM_STATE_TYPE);
  dfsch_defcanon_cstr(ctx, "<lcg-random-state>", DFSCH_LCG_RANDOM_STATE_TYPE);

  dfsch_defcanon_cstr(ctx, "random-bytes", DFSCH_PRIMITIVE_REF(random_bytes));
  dfsch_defcanon_cstr(ctx, "random-flonum", DFSCH_PRIMITIVE_REF(random_flonum));
  dfsch_defcanon_cstr(ctx, "random-bignum", DFSCH_PRIMITIVE_REF(random_bignum));
  dfsch_defcanon_cstr(ctx, "random", DFSCH_PRIMITIVE_REF(random));

  dfsch_defcanon_cstr(ctx, "make-default-random-state", 
                    DFSCH_PRIMITIVE_REF(make_default_random_state));
  dfsch_defcanon_cstr(ctx, "make-file-random-state", 
                    DFSCH_PRIMITIVE_REF(make_file_random_state));
  dfsch_defcanon_cstr(ctx, "make-lcg-random-state", 
                    DFSCH_PRIMITIVE_REF(make_lcg_random_state));

  dfsch_defcanon_cstr(ctx, "get-random-id", 
                    DFSCH_PRIMITIVE_REF(get_random_id));
  dfsch_defcanon_cstr(ctx, "get-random-scoped-id", 
                    DFSCH_PRIMITIVE_REF(get_random_scoped_id));
  dfsch_defcanon_cstr(ctx, "check-scoped-id", 
                    DFSCH_PRIMITIVE_REF(check_scoped_id));
}
