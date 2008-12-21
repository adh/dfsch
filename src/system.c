/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Basic operating system services
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

#include "internal.h"
#include "util.h"
#include <dfsch/dfsch.h>
#include <dfsch/number.h>

#include <time.h>
#include <sys/times.h>
#include <unistd.h>
#include <limits.h>


typedef struct decoded_time_t {
  dfsch_type_t* type;
  struct tm tm;
} decoded_time_t;

static int decoded_time_equal_p(decoded_time_t* a, decoded_time_t* b){
  return (a->tm.tm_sec == b->tm.tm_sec)
    && (a->tm.tm_min == b->tm.tm_min)
    && (a->tm.tm_hour == b->tm.tm_hour)
    && (a->tm.tm_mday == b->tm.tm_mday)
    && (a->tm.tm_mon == b->tm.tm_mon)
    && (a->tm.tm_year == b->tm.tm_year)
    && (a->tm.tm_wday == b->tm.tm_wday)
    && (a->tm.tm_yday == b->tm.tm_yday)
    && (a->tm.tm_isdst == b->tm.tm_isdst);
}

static void decoded_time_write(decoded_time_t* time, 
                               dfsch_writer_state_t* state){
  dfsch_write_unreadable(state, time, "%04d-%02d-%02dT%02d:%02d:%02d",
                         time->tm.tm_year+1900, time->tm.tm_mon+1, 
                         time->tm.tm_mday,
                         time->tm.tm_hour, time->tm.tm_min, time->tm.tm_sec);
}

static dfsch_object_t* decoded_time_apply(decoded_time_t* time, 
                                          dfsch_object_t* args,
                                          dfsch_tail_escape_t* esc){
  dfsch_object_t* selector;

  DFSCH_OBJECT_ARG(args, selector);
  DFSCH_ARG_END(args);

  if (dfsch_compare_symbol(selector, "sec")){
    return dfsch_make_number_from_long(time->tm.tm_sec);
  } else if (dfsch_compare_symbol(selector, "min")){
    return dfsch_make_number_from_long(time->tm.tm_min);
  } else if (dfsch_compare_symbol(selector, "hour")){
    return dfsch_make_number_from_long(time->tm.tm_hour);
  } else if (dfsch_compare_symbol(selector, "date")){
    return dfsch_make_number_from_long(time->tm.tm_mday);
  } else if (dfsch_compare_symbol(selector, "month")){
    return dfsch_make_number_from_long(time->tm.tm_mon + 1);
  } else if (dfsch_compare_symbol(selector, "year")){
    return dfsch_make_number_from_long(time->tm.tm_year + 1900);
  } else if (dfsch_compare_symbol(selector, "day")){
    return dfsch_make_number_from_long(time->tm.tm_wday);
  } else if (dfsch_compare_symbol(selector, "year-day")){
    return dfsch_make_number_from_long(time->tm.tm_yday + 1);
  } else if (dfsch_compare_symbol(selector, "dst?")){
    return dfsch_bool(time->tm.tm_isdst == 1);
  }

  dfsch_error("exception:no-such-decoded-time-field", selector);
}

size_t decoded_time_hash(decoded_time_t* time){
  return time->tm.tm_sec ^ time->tm.tm_min ^ time->tm.tm_hour 
    ^ time->tm.tm_yday ^ time->tm.tm_year;
}

dfsch_type_t decoded_time_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(decoded_time_t),
  "decoded-time",
  (dfsch_type_equal_p_t)decoded_time_equal_p,
  (dfsch_type_write_t)decoded_time_write,
  (dfsch_type_apply_t)decoded_time_apply,
  (dfsch_type_hash_t)decoded_time_hash
};

dfsch_object_t* dfsch_make_decoded_time(){
  return dfsch_make_object(&decoded_time_type);
}

struct tm* dfsch_decoded_time_get_tm(dfsch_object_t* time){
  if (DFSCH_TYPE_OF(time) != &decoded_time_type){
    dfsch_error("exception:not-a-decoded-time", time);
  }

  return &(((decoded_time_t*)time)->tm);
}

static dfsch_object_t* native_decode_universal_time(void* baton,
                                                    dfsch_object_t* args,
                                                    dfsch_tail_escape_t* esc){
  time_t time;
  dfsch_object_t* utc;
  dfsch_object_t* ret;

  DFSCH_LONG_ARG(args, time);
  DFSCH_OBJECT_ARG_OPT(args, utc, NULL);
  DFSCH_ARG_END(args);

  ret = dfsch_make_decoded_time();

  if (utc){
    gmtime_r(&time, dfsch_decoded_time_get_tm(ret));
  } else {
    localtime_r(&time, dfsch_decoded_time_get_tm(ret));
  }
 
  return ret;
}

static dfsch_object_t* native_encode_universal_time(void* baton,
                                                    dfsch_object_t* args,
                                                    dfsch_tail_escape_t* esc){
  struct tm tm;
  DFSCH_LONG_ARG(args, tm.tm_sec);
  DFSCH_LONG_ARG(args, tm.tm_min);
  DFSCH_LONG_ARG(args, tm.tm_hour);

  DFSCH_LONG_ARG(args, tm.tm_mday);
  DFSCH_LONG_ARG(args, tm.tm_mon);
  DFSCH_LONG_ARG(args, tm.tm_year);
  DFSCH_ARG_END(args);

  tm.tm_mon -= 1;
  tm.tm_year -= 1900;

  return dfsch_make_number_from_long(mktime(&tm));
}

static dfsch_object_t* native_get_decoded_time(void* baton,
                                               dfsch_object_t* args,
                                               dfsch_tail_escape_t* esc){
  time_t t;
  dfsch_object_t* utc;
  dfsch_object_t* ret;

  DFSCH_OBJECT_ARG_OPT(args, utc, NULL);
  DFSCH_ARG_END(args);

  ret = dfsch_make_decoded_time();

  t = time(NULL);

  if (utc){
    gmtime_r(&t, dfsch_decoded_time_get_tm(ret));
  } else {
    localtime_r(&t, dfsch_decoded_time_get_tm(ret));
  }
 
  return ret;  
}

static dfsch_object_t* native_get_universal_time(void* baton,
                                                 dfsch_object_t* args,
                                                 dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(time(NULL));
}

static dfsch_object_t* native_iso_format_time(void* baton,
                                              dfsch_object_t* args,
                                              dfsch_tail_escape_t* esc){
  char t = ' ';
  dfsch_object_t* use_t;
  dfsch_object_t* time;
  struct tm* tm;
  DFSCH_OBJECT_ARG(args, time);
  DFSCH_OBJECT_ARG_OPT(args, use_t, NULL);
  DFSCH_ARG_END(args);

  if (use_t){
    t = 'T';
  }

  tm = dfsch_decoded_time_get_tm(time);

  return dfsch_make_string_cstr(saprintf("%04d-%02d-%02d%c%02d:%02d:%02d",
                                         tm->tm_year+1900, tm->tm_mon+1, 
                                         tm->tm_mday, t,
                                         tm->tm_hour, tm->tm_min, tm->tm_sec));
}

DFSCH_DEFINE_PRIMITIVE(get_internal_real_time, NULL){
  struct tms t;
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(times(&t) & LONG_MAX);
}
DFSCH_DEFINE_PRIMITIVE(get_internal_run_time, NULL){
  struct tms t;
  DFSCH_ARG_END(args);

  times(&t);

  return dfsch_make_number_from_long((t.tms_utime + t.tms_stime) & LONG_MAX);
}



void dfsch__system_register(dfsch_object_t *ctx){
  dfsch_define_cstr(ctx, "<decoded-time>", &decoded_time_type);


  dfsch_define_cstr(ctx, "decode-universal-time", 
                    dfsch_make_primitive(native_decode_universal_time, NULL));
  dfsch_define_cstr(ctx, "encode-universal-time", 
                    dfsch_make_primitive(native_encode_universal_time, NULL));
  dfsch_define_cstr(ctx, "get-decoded-time", 
                    dfsch_make_primitive(native_get_decoded_time, NULL));
  dfsch_define_cstr(ctx, "get-universal-time", 
                    dfsch_make_primitive(native_get_universal_time, NULL));
  dfsch_define_cstr(ctx, "iso-format-time", 
                    dfsch_make_primitive(native_iso_format_time, NULL));
  dfsch_define_cstr(ctx, "get-internal-real-time", 
                    DFSCH_PRIMITIVE_REF(get_internal_real_time));
  dfsch_define_cstr(ctx, "get-internal-run-time", 
                    DFSCH_PRIMITIVE_REF(get_internal_run_time));
  dfsch_define_cstr(ctx, "internal-time-units-per-second", 
                    dfsch_make_number_from_long(sysconf(_SC_CLK_TCK)));
}
