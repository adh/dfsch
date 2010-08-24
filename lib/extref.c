/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   External object references
 * Copyright (C) 2005-2008 Ales Hakl
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include "dfsch/lib/extref.h"
#include <dfsch/number.h>
#include <pthread.h>
#include <stdint.h>
#include <gc/gc.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>
#include <string.h>

static uint32_t instance_id[4];
static int instance_id_set = 0;
static uint64_t serial = 0;
static pthread_mutex_t serial_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t extref_mutex = PTHREAD_MUTEX_INITIALIZER;

typedef struct extref_t extref_t;
struct extref_t {
  uint32_t uniqid[3];
  time_t timeout;
  time_t valid_to;
  int mode;
  dfsch_object_t* object;
  extref_t* next;
};

static extref_t** extrefs;
static size_t extref_mask = 0;
static size_t extref_count = 0;
static time_t extref_next_gc;

static void extref_resize();

static void* extref_gc_thread(void* ignore){
  time_t cur_time;

  while (1){
    cur_time = time(NULL);
    pthread_mutex_lock(&extref_mutex);
    cur_time = time(NULL);
    if (cur_time >= extref_next_gc){
      extref_resize();
    }
    pthread_mutex_unlock(&extref_mutex);

#ifdef __WIN32__
      Sleep(10000); /* compatibility and portability ftw! */
#else
      sleep(10);
#endif
  }
}

static extref_t* extref_get(uint32_t uniqid[3]){
  extref_t* i;

  if (extref_mask == 0 || extref_count == 0){
    return NULL;
  }

  pthread_mutex_lock(&extref_mutex);

  i = extrefs[uniqid[0] & extref_mask];

  while (i){
    if (memcmp(i->uniqid, uniqid, sizeof(uint32_t)*3) == 0){
      pthread_mutex_unlock(&extref_mutex);
      return i;
    }
    i = i->next;
  }
  pthread_mutex_unlock(&extref_mutex);
  return NULL;
}

static void extref_init(){
  pthread_t thread;
  extrefs = GC_MALLOC(sizeof(extref_t*)*256);
  extref_mask = 255;
  extref_next_gc = time(NULL) + 60; 
  memset(extrefs, 0, sizeof(extref_t*)*256);
  pthread_create(&thread, NULL, extref_gc_thread, NULL);
  pthread_detach(thread);
}

static void extref_put(extref_t* new){
  pthread_mutex_lock(&extref_mutex);
  if (extref_mask == 0){
    extref_init();
  }

  extref_count++;
  new->next = extrefs[new->uniqid[0] & extref_mask];
  extrefs[new->uniqid[0] & extref_mask] = new;

  if (extref_count > extref_mask){
    extref_resize();
  }

  if (extref_next_gc > new->valid_to){
    extref_next_gc = new->valid_to;
  }

  pthread_mutex_unlock(&extref_mutex);
}

static void extref_resize(){
  size_t i;
  extref_t* j;
  extref_t* k;
  time_t cur_time;
  extref_t** new_extrefs;
  size_t new_mask;

  cur_time = time(NULL);
  extref_count = 0;

  for (i = 0; i <= extref_mask; i++){
    j = extrefs[i];
    while (j && j->valid_to < cur_time){
      j = j->next;
    }
    extrefs[i] = j;
    while (j) {
      if (j->valid_to <= cur_time){
        k->next = j->next;
      } else {
        k = j;
        extref_count++;
        if (j->valid_to < extref_next_gc || extref_next_gc < cur_time){
          extref_next_gc = j->valid_to;
        } 
      }
      j = j->next;
    }
  }

  if (extref_next_gc < cur_time){
    extref_next_gc = cur_time + 86400;
  }

  if (extref_count > extref_mask){
    new_mask = ((extref_mask + 1) * 2) - 1;
  } else if (extref_count < extref_mask/2 && extref_mask > 255) {
    new_mask = ((extref_mask + 1) / 2) - 1;
  } else {
    return;
  }

  new_extrefs = GC_MALLOC(sizeof(extref_t*) * (new_mask + 1));
  memset(new_extrefs, 0, sizeof(extref_t*) * (new_mask + 1));

  for (i = 0; i <= extref_mask; i++){
    j = extrefs[i];
    while (j) {
      k = j->next;
      j->next = new_extrefs[j->uniqid[0] & new_mask];
      new_extrefs[j->uniqid[0] & new_mask] = j;
      j = k;
    }
  }

  extref_mask = new_mask;
  extrefs = new_extrefs;
}




static int idchar_value(char ch){
  if ((ch >= 'A') && (ch <= 'Z')){
    return ch - 'A';
  } else if ((ch >= 'a') && (ch <= 'z')){
    return ch - 'a' + 26;
  } else if ((ch >= '0') && (ch <= '9')){
    return ch - '0' + 52;
  } else if (ch == '.'){
    return 62;
  } else if (ch == '-'){
    return 63;
  }
  return -1;
}
static const char id_chars[] = 
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789.-";

static uint64_t get_serial(){
  uint64_t ret;
  pthread_mutex_lock(&serial_mutex);
  ret = serial;
  serial++;
  pthread_mutex_unlock(&serial_mutex);
  return ret;
}

static void extref_init_instance_id(){
  uint32_t buf[4];
  int fd;

  instance_id[0] = getpid();
  instance_id[1] = time(NULL);
  instance_id[2] = 0;
  instance_id[3] = 0;

  fd = open("/dev/urandom", O_RDONLY);
  if (fd < 0){
    fd = open("/dev/random", O_RDONLY);
  }
  if (fd >= 0){
    instance_id[2] = fd;
    read(fd, &buf, sizeof(buf));
    instance_id[0] ^= buf[0];
    instance_id[1] ^= buf[1];
    instance_id[2] ^= buf[2];
    instance_id[3] ^= buf[2];
    close (fd);
  }
}

static void create_uniq_id(uint32_t uid[3]){
  int i;
  uint32_t sum=0, delta=0x9E3779B9;
  uint64_t serial;
  char* ret;

  serial = get_serial();

  uid[0] = serial >> 32;
  uid[1] = serial;
  uid[2] = 0;
  
  if (!instance_id_set){
    extref_init_instance_id();
    instance_id_set = 1;
  }

  for(i=0; i<32; i++) {
    uid[0] += ((uid[1] << 4 ^ uid[1] >> 5) + uid[1]) 
      ^ (sum + instance_id[sum & 3]);
    sum += delta;
    uid[1] += ((uid[0] << 4 ^ uid[0] >> 5) + uid[0]) 
      ^ (sum + instance_id[sum>>11 & 3]);
  }
  for(i=0; i<32; i++) {
    uid[2] += ((uid[1] << 4 ^ uid[1] >> 5) + uid[1])
      ^ (sum + instance_id[sum & 3]);
    sum += delta;
    uid[1] += ((uid[2] << 4 ^ uid[2] >> 5) + uid[2]) 
      ^ (sum + instance_id[sum>>11 & 3]);
  }

}

static char* encode_uniqid(uint32_t uniqid[3]){
  uint32_t v0, v1, v2, v3;
  char* ret = GC_MALLOC_ATOMIC(17);

  v0 = uniqid[0];
  v1 = uniqid[1];
  v2 = uniqid[2];
  v3 = ((v0 & 0xff000000) >> 8) | 
    ((v1 & 0xff000000) >> 16) | 
    (v2 & 0xff000000) >> 24;

  ret = GC_MALLOC_ATOMIC(17);
  ret[0] = id_chars[(v0 >> 18) & 0x3f];
  ret[1] = id_chars[(v0 >> 12) & 0x3f];
  ret[2] = id_chars[(v0 >> 6) & 0x3f];
  ret[3] = id_chars[(v0 >> 0) & 0x3f];

  ret[4] = id_chars[(v1 >> 18) & 0x3f];
  ret[5] = id_chars[(v1 >> 12) & 0x3f];
  ret[6] = id_chars[(v1 >> 6) & 0x3f];
  ret[7] = id_chars[(v1 >> 0) & 0x3f];

  ret[8] = id_chars[(v2 >> 18) & 0x3f];
  ret[9] = id_chars[(v2 >> 12) & 0x3f];
  ret[10] = id_chars[(v2 >> 6) & 0x3f];
  ret[11] = id_chars[(v2 >> 0) & 0x3f];

  ret[12] = id_chars[(v3 >> 18) & 0x3f];
  ret[13] = id_chars[(v3 >> 12) & 0x3f];
  ret[14] = id_chars[(v3 >> 6) & 0x3f];
  ret[15] = id_chars[(v3 >> 0) & 0x3f];
  ret[16] = 0;

  return ret;
}
static int decode_uniqid(uint32_t uniqid[3], char* str){
  uint32_t v3;
  int i;

  if (strlen(str)!=16){
    return 0;
  }

  for (i = 0; i < 16; i++){
    if (idchar_value(str[i]) == -1){
      return 0;
    }
  }

  uniqid[0] = (idchar_value(str[0]) << 18)
    | (idchar_value(str[1]) << 12)
    | (idchar_value(str[2]) << 6)
    | (idchar_value(str[3]) << 0);

  uniqid[1] = (idchar_value(str[4]) << 18)
    | (idchar_value(str[5]) << 12)
    | (idchar_value(str[6]) << 6)
    | (idchar_value(str[7]) << 0);

  uniqid[2] = (idchar_value(str[8]) << 18)
    | (idchar_value(str[9]) << 12)
    | (idchar_value(str[10]) << 6)
    | (idchar_value(str[11]) << 0);

  v3 = (idchar_value(str[12]) << 18)
    | (idchar_value(str[13]) << 12)
    | (idchar_value(str[14]) << 6)
    | (idchar_value(str[15]) << 0);

  uniqid[0] |= (v3 << 8) & 0xff000000;
  uniqid[1] |= (v3 << 16) & 0xff000000;
  uniqid[2] |= (v3 << 24) & 0xff000000;

  return 1;
}


char* dfsch_extref_create(dfsch_object_t* object, time_t timeout, int mode){
  extref_t* er = GC_MALLOC(sizeof(extref_t));
  create_uniq_id(er->uniqid);
  er->timeout = timeout;
  er->mode = mode;
  er->valid_to = time(NULL) + timeout;
  er->object = object;

  extref_put(er);
  
  return encode_uniqid(er->uniqid);
}
dfsch_object_t* dfsch_extref_ref(char* ref){
  extref_t* er;
  uint32_t uniq_id[3];

  if (!decode_uniqid(uniq_id, ref)){
    dfsch_error("Invalid handle", dfsch_make_string_cstr(ref));
  }

  er = extref_get(uniq_id);

  if (!er){
    dfsch_error("Expired handle", dfsch_make_string_cstr(ref));
  }

  switch(er->mode){
  case DFSCH_EXTREF_REFRESH:
    er->valid_to = er->timeout + time(NULL);
    break;
  case DFSCH_EXTREF_ONCEONLY:
    er->valid_to = 0;
    break;
  }

  return er->object;
}

