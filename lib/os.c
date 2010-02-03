/*
 * dfsch - dfox's quick and dirty scheme implementation
 *   Unix operating system interface
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif


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

DFSCH_DEFINE_PRIMITIVE(access, NULL){
  char* path;
  int amode;
  DFSCH_STRING_ARG(args, path);
  DFSCH_FLAG_PARSER_BEGIN(args);
  DFSCH_FLAG_SET("r", R_OK, amode);
  DFSCH_FLAG_SET("w", W_OK, amode);
  DFSCH_FLAG_SET("x", X_OK, amode);
  DFSCH_FLAG_SET("f", F_OK, amode);
  DFSCH_FLAG_PARSER_END(args);

  if (access(path, amode) == -1){
    if (errno != EPERM || errno != ENOENT){
      dfsch_operating_system_error("access");
    } else {
      return NULL;
    }
  }

  return DFSCH_SYM_TRUE;
}


DFSCH_DEFINE_PRIMITIVE(chdir, NULL){
  char* dir;
  DFSCH_STRING_ARG(args, dir);
  DFSCH_ARG_END(args);

  if (chdir(dir) != 0){
    dfsch_operating_system_error("chdir");
  }
  return NULL;
}



DFSCH_DEFINE_PRIMITIVE(clock, NULL){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(clock());
}

DFSCH_DEFINE_PRIMITIVE(close, NULL){
  int fd;
  DFSCH_LONG_ARG(args, fd);
  DFSCH_ARG_END(args);

  if (close(fd) != 0){
    dfsch_operating_system_error("close");
  }
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(closedir, NULL){
  dfsch_object_t* dir;
  DFSCH_OBJECT_ARG(args, dir);
  DFSCH_ARG_END(args);

  dfsch_os_closedir(dir);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(creat, NULL){
  int fd;
  char* path;
  mode_t mode;

  DFSCH_STRING_ARG(args, path);
  DFSCH_LONG_ARG(args, mode);
  DFSCH_ARG_END(args);

  fd = creat(path, mode);

  if (fd == -1){
    dfsch_operating_system_error("creat");
  }

  return dfsch_make_number_from_long(fd);
}

DFSCH_DEFINE_PRIMITIVE(dup, NULL){
  int fd;
  DFSCH_LONG_ARG(args, fd);
  DFSCH_ARG_END(args);

  fd = dup(fd);

  if (fd < 0){
    dfsch_operating_system_error("dup");
  }
  return dfsch_make_number_from_long(fd);
}

DFSCH_DEFINE_PRIMITIVE(dup2, NULL){
  int oldfd;
  int newfd;
  DFSCH_LONG_ARG(args, oldfd);
  DFSCH_LONG_ARG(args, newfd);
  DFSCH_ARG_END(args);

  newfd = dup2(oldfd, newfd);

  if (newfd < 0){
    dfsch_operating_system_error("dup2");
  }
  return dfsch_make_number_from_long(newfd);
}

DFSCH_DEFINE_PRIMITIVE(exit, NULL){
  int status;
  DFSCH_LONG_ARG(args, status);
  DFSCH_ARG_END(args);

  exit(status);
}

DFSCH_DEFINE_PRIMITIVE(fstat, NULL){
  int fd;
  dfsch_object_t* res;
  DFSCH_LONG_ARG(args, fd);
  DFSCH_ARG_END(args);

  res = dfsch_os_make_stat_struct();

  if (fstat(fd, dfsch_os_get_stat(res)) != 0){
    dfsch_operating_system_error("fstat");
  }
  return res;
}
DFSCH_DEFINE_PRIMITIVE(getcwd, NULL){
  char* buf;
  char* ret;
  dfsch_object_t* obj;
  size_t len;

  DFSCH_ARG_END(args);


  obj = dfsch_make_string_cstr(dfsch_getcwd());
  return obj;
}
DFSCH_DEFINE_PRIMITIVE(getpid, NULL){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(getpid());
}


DFSCH_DEFINE_PRIMITIVE(getenv, NULL){
  char* name;
  char* value;
  DFSCH_STRING_ARG(args, name);
  DFSCH_ARG_END(args);

  value = getenv(name);

  if (value){
    return dfsch_make_string_cstr(value);
  } else {
    return NULL;
  }
}
DFSCH_DEFINE_PRIMITIVE(gettimeofday, NULL){
  struct timeval tv;
  DFSCH_ARG_END(args);

  gettimeofday(&tv, NULL);

  /*
   * SUSv3 says that "No errors are defined", Linux man page says something 
   * about EFAULT (which clearly cannot happen here, and if it does, it's not 
   * because of client code) and EINVAL (which seems like utter bullshit).
   */

  return dfsch_vector(2, 
                      dfsch_make_number_from_long(tv.tv_sec),
                      dfsch_make_number_from_long(tv.tv_usec));
}
DFSCH_DEFINE_PRIMITIVE(isatty, NULL){
  int fd;
  int ret;
  DFSCH_LONG_ARG(args, fd);
  DFSCH_ARG_END(args);

  ret = isatty(fd);

  if (ret == 0){
    if (errno != ENOTTY){
      dfsch_operating_system_error("isatty");
    }else{
      return NULL;
    }
  }

  return DFSCH_SYM_TRUE;
}
DFSCH_DEFINE_PRIMITIVE(lseek, NULL){
  int fd;
  dfsch_object_t* whence;
  off_t offset;
  int w;

  DFSCH_LONG_ARG(args, fd);
  DFSCH_LONG_ARG(args, offset);
  DFSCH_OBJECT_ARG(args, whence);
  DFSCH_ARG_END(args);

  if (dfsch_compare_keyword(whence, "set")){
    w = SEEK_SET;
  } else if (dfsch_compare_keyword(whence, "cur")){
    w = SEEK_CUR;
  } else if (dfsch_compare_keyword(whence, "end")){
    w = SEEK_END;
  } else {
    dfsch_error("Unknown whence value", whence);
  }

  if (lseek(fd, offset, w) != 0){
    dfsch_operating_system_error("lseek");
  }
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(mkdir, NULL){
  char* path;
  DFSCH_STRING_ARG(args, path);
  DFSCH_ARG_END(args);

#ifdef __unix__
  if (mkdir(path, 0777) != 0){
    dfsch_operating_system_error("mkdir");
  }
#else
  if (mkdir(path) != 0){
    dfsch_operating_system_error("mkdir");
  }
#endif

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(open, NULL){
  char* path;
  mode_t mode = 0;
  int oflag = 0;
  int fd;

  DFSCH_STRING_ARG(args, path);
  DFSCH_FLAG_PARSER_BEGIN_SYM_ONLY(args);
  DFSCH_FLAG_SET("rdonly", O_RDONLY, oflag);
  DFSCH_FLAG_SET("wronly", O_WRONLY, oflag);
  DFSCH_FLAG_SET("rdwr", O_RDWR, oflag);
  DFSCH_FLAG_SET("append", O_APPEND, oflag);
  DFSCH_FLAG_SET("creat", O_CREAT, oflag);
  DFSCH_FLAG_SET("excl", O_EXCL, oflag);
#ifdef O_DSYNC
  DFSCH_FLAG_SET("dsync", O_DSYNC, oflag);
#endif
#ifdef O_NOCTTY
  DFSCH_FLAG_SET("noctty", O_NOCTTY, oflag);
#endif
#ifdef O_NONBLOCK
  DFSCH_FLAG_SET("nonblock", O_NONBLOCK, oflag);
#endif
#ifdef O_RSYNC
  DFSCH_FLAG_SET("rsync", O_RSYNC, oflag);
#endif
#ifdef O_SYNC
  DFSCH_FLAG_SET("sync", O_SYNC, oflag);
#endif
  DFSCH_FLAG_SET("trunc", O_TRUNC, oflag);
  DFSCH_FLAG_PARSER_END(args);
  DFSCH_LONG_ARG_OPT(args, mode, 0);

  fd = open(path, oflag, mode);
  
  if (fd == -1){
    dfsch_operating_system_error("open");
  }
  
  return dfsch_make_number_from_long(fd);
}
DFSCH_DEFINE_PRIMITIVE(opendir, NULL){
  char* path;
  DFSCH_STRING_ARG(args, path);
  DFSCH_ARG_END(args);

  return dfsch_os_opendir(path);
}

DFSCH_DEFINE_PRIMITIVE(raise, NULL){
  int sig;
  DFSCH_LONG_ARG(args, sig);
  DFSCH_ARG_END(args);

  if (raise(sig) != 0){
    dfsch_operating_system_error("raise");
  }
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(read, NULL){
  int fd;
  size_t len;
  char* buf;
  ssize_t ret;
  dfsch_object_t* obj;

  DFSCH_LONG_ARG(args, fd);
  DFSCH_LONG_ARG(args, len);
  DFSCH_ARG_END(args);

  // TODO: do not copy string

  buf = GC_MALLOC_ATOMIC(len);
  ret = read(fd, buf, len);
  
  if (ret == -1){
    int e = errno;
    GC_FREE(buf);
    dfsch_operating_system_error("read");
  } else if (ret == 0) {
    GC_FREE(buf);
    return NULL;
  } else {
    obj = dfsch_make_string_buf(buf, ret);
    GC_FREE(buf);
    return obj;
  }

}
DFSCH_DEFINE_PRIMITIVE(readdir, NULL){
  dfsch_object_t* dir;
  DFSCH_OBJECT_ARG(args, dir);
  DFSCH_ARG_END(args);

  return dfsch_make_string_cstr(dfsch_os_readdir(dir));
}
DFSCH_DEFINE_PRIMITIVE(rename, NULL){
  char* old;
  char* new;
  DFSCH_STRING_ARG(args, old);
  DFSCH_STRING_ARG(args, new);
  DFSCH_ARG_END(args);

  if (rename(old, new) != 0){
    dfsch_operating_system_error("rename");
  }
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(rmdir, NULL){
  char* path;
  DFSCH_STRING_ARG(args, path);
  DFSCH_ARG_END(args);

  if (rmdir(path) != 0){
    dfsch_operating_system_error("rmdir");
  }
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(stat, NULL){
  char* path;
  dfsch_object_t* res;
  DFSCH_STRING_ARG(args, path);
  DFSCH_ARG_END(args);

  res = dfsch_os_make_stat_struct();

  if (stat(path, dfsch_os_get_stat(res)) != 0){
    if (errno == ENOENT){
      return NULL;
    }
    dfsch_operating_system_error("stat");
  }
  return res;
}

DFSCH_DEFINE_PRIMITIVE(write, NULL){
  int fd;
  ssize_t ret;
  dfsch_strbuf_t* buf;

  DFSCH_LONG_ARG(args, fd);
  DFSCH_BUFFER_ARG(args, buf);
  DFSCH_ARG_END(args);

  ret = write(fd, buf->ptr, buf->len);
  
  if (ret == -1){
    dfsch_operating_system_error("write");
  } 

  return dfsch_make_number_from_long(ret);
}
DFSCH_DEFINE_PRIMITIVE(unlink, NULL){
  char* path;
  DFSCH_STRING_ARG(args, path);
  DFSCH_ARG_END(args);

  if (unlink(path) != 0){
    dfsch_operating_system_error("unlink");
  }
  return NULL;
}


dfsch_object_t* dfsch_module_os_register(dfsch_object_t* ctx){
  dfsch_package_t* os_pkg = dfsch_make_package("os");

  dfsch_define_pkgcstr(ctx, os_pkg, "access", 
                    DFSCH_PRIMITIVE_REF(access));
  dfsch_define_pkgcstr(ctx, os_pkg, "chdir", 
                    DFSCH_PRIMITIVE_REF(chdir));
  dfsch_define_pkgcstr(ctx, os_pkg, "clock", 
                    DFSCH_PRIMITIVE_REF(clock));
  dfsch_define_pkgcstr(ctx, os_pkg, "close", 
                    DFSCH_PRIMITIVE_REF(close));
  dfsch_define_pkgcstr(ctx, os_pkg, "closedir", 
                    DFSCH_PRIMITIVE_REF(closedir));
  dfsch_define_pkgcstr(ctx, os_pkg, "creat", 
                    DFSCH_PRIMITIVE_REF(creat));
  dfsch_define_pkgcstr(ctx, os_pkg, "dup", 
                    DFSCH_PRIMITIVE_REF(dup));
  dfsch_define_pkgcstr(ctx, os_pkg, "dup2", 
                    DFSCH_PRIMITIVE_REF(dup2));
  dfsch_define_pkgcstr(ctx, os_pkg, "exit", 
                    DFSCH_PRIMITIVE_REF(exit));
  dfsch_define_pkgcstr(ctx, os_pkg, "fstat", 
                    DFSCH_PRIMITIVE_REF(fstat));
  dfsch_define_pkgcstr(ctx, os_pkg, "getcwd", 
                    DFSCH_PRIMITIVE_REF(getcwd));
  dfsch_define_pkgcstr(ctx, os_pkg, "getpid", 
                    DFSCH_PRIMITIVE_REF(getpid));
  dfsch_define_pkgcstr(ctx, os_pkg, "gettimeofday", 
                    DFSCH_PRIMITIVE_REF(gettimeofday));
  dfsch_define_pkgcstr(ctx, os_pkg, "isatty", 
                    DFSCH_PRIMITIVE_REF(isatty));
  dfsch_define_pkgcstr(ctx, os_pkg, "lseek", 
                    DFSCH_PRIMITIVE_REF(lseek));
  dfsch_define_pkgcstr(ctx, os_pkg, "mkdir", 
                    DFSCH_PRIMITIVE_REF(mkdir));
  dfsch_define_pkgcstr(ctx, os_pkg, "open", 
                    DFSCH_PRIMITIVE_REF(open));
  dfsch_define_pkgcstr(ctx, os_pkg, "opendir", 
                    DFSCH_PRIMITIVE_REF(opendir));
  dfsch_define_pkgcstr(ctx, os_pkg, "raise", 
                    DFSCH_PRIMITIVE_REF(raise));
  dfsch_define_pkgcstr(ctx, os_pkg, "read", 
                    DFSCH_PRIMITIVE_REF(read));
  dfsch_define_pkgcstr(ctx, os_pkg, "readdir", 
                    DFSCH_PRIMITIVE_REF(readdir));
  dfsch_define_pkgcstr(ctx, os_pkg, "rename", 
                    DFSCH_PRIMITIVE_REF(rename));
  dfsch_define_pkgcstr(ctx, os_pkg, "rmdir", 
                    DFSCH_PRIMITIVE_REF(rmdir));
  dfsch_define_pkgcstr(ctx, os_pkg, "stat", 
                    DFSCH_PRIMITIVE_REF(stat));
  dfsch_define_pkgcstr(ctx, os_pkg, "write", 
                    DFSCH_PRIMITIVE_REF(write));
  dfsch_define_pkgcstr(ctx, os_pkg, "unlink", 
                    DFSCH_PRIMITIVE_REF(unlink));
  
  dfsch_provide(ctx, "os");

  return NULL;
}

