/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Generic operating system interface
 * Copyright (C) 2005-2010 Ales Hakl
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <dfsch/lib/os.h>

#include <dfsch/number.h>
#include <dfsch/load.h>
#include <dfsch/conditions.h>

#include "src/util.h"

#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <stdlib.h>
#include <fcntl.h>
#include <dirent.h>
#include <sys/time.h>
#include <time.h>

typedef struct dir_t {
  dfsch_type_t *type;
  DIR *dir;
  int open;
} dir_t;

static dfsch_type_t dir_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(dir_t),
  "os:directory",
  NULL, // equal?
  NULL, // write
  NULL, // apply
};


static void dir_finalizer(dir_t* dir, void* cd){
  if (dir->open){
    closedir(dir->dir);
    dir->open = 0;
  }
}

dfsch_object_t* dfsch_os_opendir(char* name){
  dir_t* dir = (dir_t*)dfsch_make_object(&dir_type);

  dir->dir = opendir(name);
  if (!dir->dir){
    dfsch_operating_system_error("opendir");
  }
  dir->open = 1;
  GC_REGISTER_FINALIZER(dir, (GC_finalization_proc)dir_finalizer,
                        NULL, NULL, NULL);
  return (dfsch_object_t*) dir;
}

void dfsch_os_closedir(dfsch_object_t* dir_obj){
  dir_t* dir;

  dir = DFSCH_ASSERT_INSTANCE(dir_obj, &dir_type);

  if (dir->open){
    closedir(dir->dir);
    dir->open = 0;
  }
}

char* dfsch_os_readdir(dfsch_object_t* dir_obj){
  dir_t* dir;
  struct dirent* dent;

  dir = DFSCH_ASSERT_INSTANCE(dir_obj, &dir_type);

  if (!dir->open){
    dfsch_error("Directory object is closed", dir_obj);    
  }

  errno = 0;
  dent = readdir(dir->dir);
  if (!dent){
    if (errno != 0){
      dfsch_operating_system_error("readdir");    
    } else {
      return NULL;
    }
  }

  return dent->d_name;
}

typedef struct stat_t {
  dfsch_type_t *type;
  struct stat st;
} stat_t;

static void stat_write(stat_t* st, dfsch_writer_state_t* state){
  dfsch_write_unreadable(state, 
                         (dfsch_object_t*)st,
                         "dev 0x%lx ino %ld mode 0%lo nlink %ld uid %ld gid %d "
                         "rdev 0x%x size %lld atime %ld mtime %ld ctime %ld ", 
                         (long)st->st.st_dev,
                         (long)st->st.st_ino,
                         (long)st->st.st_mode,
                         (long)st->st.st_nlink,
                         (long)st->st.st_uid,
                         (long)st->st.st_gid,
                         (long)st->st.st_rdev,
                         (long long)st->st.st_size,
                         (long)st->st.st_atime,
                         (long)st->st.st_mtime,
                         (long)st->st.st_ctime);
}

static dfsch_object_t* stat_apply(stat_t *st, dfsch_object_t *args,
                                  dfsch_tail_escape_t *esc){
  dfsch_object_t* selector;

  DFSCH_OBJECT_ARG(args, selector);
  DFSCH_ARG_END(args);

  if (dfsch_compare_keyword(selector, "dev")){
    return dfsch_make_number_from_long(st->st.st_dev);
  } else if (dfsch_compare_keyword(selector, "ino")){
    return dfsch_make_number_from_long(st->st.st_ino);
  } else if (dfsch_compare_keyword(selector, "mode")){
    return dfsch_make_number_from_long(st->st.st_mode);
  } else if (dfsch_compare_keyword(selector, "nlink")){
    return dfsch_make_number_from_long(st->st.st_nlink);
  } else if (dfsch_compare_keyword(selector, "uid")){
    return dfsch_make_number_from_long(st->st.st_uid);
  } else if (dfsch_compare_keyword(selector, "gid")){
    return dfsch_make_number_from_long(st->st.st_gid);
  } else if (dfsch_compare_keyword(selector, "rdev")){
    return dfsch_make_number_from_long(st->st.st_rdev);
  } else if (dfsch_compare_keyword(selector, "size")){
    return dfsch_make_number_from_long(st->st.st_size);
  } else if (dfsch_compare_keyword(selector, "atime")){
    return dfsch_make_number_from_long(st->st.st_atime);
  } else if (dfsch_compare_keyword(selector, "mtime")){
    return dfsch_make_number_from_long(st->st.st_mtime);
  } else if (dfsch_compare_keyword(selector, "ctime")){
    return dfsch_make_number_from_long(st->st.st_ctime);
  } else if (dfsch_compare_keyword(selector, "isreg")){
    return dfsch_bool(S_ISREG(st->st.st_mode));
  } else if (dfsch_compare_keyword(selector, "isdir")){
    return dfsch_bool(S_ISDIR(st->st.st_mode));
  } else if (dfsch_compare_keyword(selector, "ischr")){
    return dfsch_bool(S_ISCHR(st->st.st_mode));
  } else if (dfsch_compare_keyword(selector, "isblk")){
    return dfsch_bool(S_ISBLK(st->st.st_mode));
  } else if (dfsch_compare_keyword(selector, "isfifo")){
    return dfsch_bool(S_ISFIFO(st->st.st_mode));
  } else if (dfsch_compare_keyword(selector, "islnk")){
#ifdef S_ISLNK
    return dfsch_bool(S_ISLNK(st->st.st_mode));
#else
    return 0;
#endif
    
  }

  dfsch_error("No such stat field", selector);
}

static dfsch_type_t stat_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(stat_t),
  "os:stat-struct",
  NULL, // equal?
  (dfsch_type_write_t)stat_write, // write
  (dfsch_type_apply_t)stat_apply, // apply
};

dfsch_object_t* dfsch_os_make_stat_struct(){
  return dfsch_make_object(&stat_type);
}

struct stat* dfsch_os_get_stat(dfsch_object_t* stat){
  if (DFSCH_TYPE_OF(stat) != &stat_type){
    dfsch_error("Not a stat struct", stat);
  }

  return &(((stat_t*)stat)->st);
}

