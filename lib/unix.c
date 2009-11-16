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

#include "dfsch/lib/unix.h"

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
#include <sys/wait.h>
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
  "unix:directory",
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

dfsch_object_t* dfsch_unix_opendir(char* name){
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

void dfsch_unix_closedir(dfsch_object_t* dir_obj){
  dir_t* dir;
  if (DFSCH_TYPE_OF(dir_obj) != &dir_type){
    dfsch_error("unix:not-a-directory", dir_obj);
  }
  dir = (dir_t*)dir_obj;

  if (dir->open){
    closedir(dir->dir);
    dir->open = 0;
  }
}

char* dfsch_unix_readdir(dfsch_object_t* dir_obj){
  dir_t* dir;
  struct dirent* dent;
  if (DFSCH_TYPE_OF(dir_obj) != &dir_type){
    dfsch_error("unix:not-a-directory", dir_obj);
  }
  dir = (dir_t*)dir_obj;

  if (!dir->open){
    dfsch_error("unix:directory-object-is-closed", dir_obj);    
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
                         "rdev 0x%x size %lld atime %ld mtime %ld ctime %ld "
                         "blksize %ld blocks %ld", 
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
                         (long)st->st.st_ctime,
                         (long)st->st.st_blksize,
                         (long)st->st.st_blocks);
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
  } else if (dfsch_compare_keyword(selector, "blksize")){
    return dfsch_make_number_from_long(st->st.st_blksize);
  } else if (dfsch_compare_keyword(selector, "blocks")){
    return dfsch_make_number_from_long(st->st.st_blocks);
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
    return dfsch_bool(S_ISLNK(st->st.st_mode));
  }

  dfsch_error("unix:no-such-stat-field", selector);
}

static dfsch_type_t stat_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(stat_t),
  "unix:stat-struct",
  NULL, // equal?
  (dfsch_type_write_t)stat_write, // write
  (dfsch_type_apply_t)stat_apply, // apply
};

dfsch_object_t* dfsch_unix_make_stat_struct(){
  return dfsch_make_object(&stat_type);
}

struct stat* dfsch_unix_get_stat(dfsch_object_t* stat){
  if (DFSCH_TYPE_OF(stat) != &stat_type){
    dfsch_error("unix:not-a-stat-struct", stat);
  }

  return &(((stat_t*)stat)->st);
}

DFSCH_DEFINE_PRIMITIVE(mode, NULL){
  mode_t mode = 0;
  
  DFSCH_FLAG_PARSER_BEGIN(args);
  DFSCH_FLAG_SET("suid", S_ISUID, mode);
  DFSCH_FLAG_SET("sgid", S_ISGID, mode);
  DFSCH_FLAG_SET("svtx", 01000, mode); // XXX
  DFSCH_FLAG_SET("sticky", 01000, mode);
  DFSCH_FLAG_SET("rusr", S_IRUSR, mode);
  DFSCH_FLAG_SET("ur", S_IRUSR, mode);
  DFSCH_FLAG_SET("wusr", S_IWUSR, mode);
  DFSCH_FLAG_SET("uw", S_IWUSR, mode);
  DFSCH_FLAG_SET("xusr", S_IXUSR, mode);
  DFSCH_FLAG_SET("ux", S_IXUSR, mode);
  DFSCH_FLAG_SET("rgrp", S_IRGRP, mode);
  DFSCH_FLAG_SET("gr", S_IRGRP, mode);
  DFSCH_FLAG_SET("wgrp", S_IWGRP, mode);
  DFSCH_FLAG_SET("gw", S_IWGRP, mode);
  DFSCH_FLAG_SET("xgrp", S_IXGRP, mode);
  DFSCH_FLAG_SET("gx", S_IXGRP, mode);
  DFSCH_FLAG_SET("roth", S_IROTH, mode);
  DFSCH_FLAG_SET("or", S_IROTH, mode);
  DFSCH_FLAG_SET("woth", S_IWOTH, mode);
  DFSCH_FLAG_SET("ow", S_IWOTH, mode);
  DFSCH_FLAG_SET("xoth", S_IXOTH, mode);
  DFSCH_FLAG_SET("ox", S_IXOTH, mode);
  DFSCH_FLAG_PARSER_END(args);

  return dfsch_make_number_from_long(mode);
}

typedef struct signal_name_t {
  char * name;
  int signal;
} signal_name_t;

static signal_name_t signals[] = {
  {"abrt", SIGABRT},
  {"alrm", SIGALRM},
  {"bus", SIGBUS},
  {"chld", SIGCHLD},
  {"cont", SIGCONT},
  {"fpe", SIGFPE},
  {"hup", SIGHUP},
  {"ill", SIGILL},
  {"int", SIGINT},
  {"kill", SIGKILL},
  {"pipe", SIGPIPE},
  {"quit", SIGQUIT},
  {"segv", SIGSEGV},
  {"stop", SIGSTOP},
  {"term", SIGTERM},
  {"tstp", SIGTSTP},
  {"ttin", SIGTTIN},
  {"ttou", SIGTTOU},
  {"usr1", SIGUSR1},
  {"usr2", SIGUSR2},
  {"poll", SIGPOLL},
  {"prof", SIGPROF},
  {"sys", SIGSYS},
  {"trap", SIGTRAP},
  {"urg", SIGURG},
  {"vtalrm", SIGVTALRM},
  {"xcpu", SIGXCPU},
  {"xfsz", SIGXFSZ}
};

DFSCH_DEFINE_PRIMITIVE(sig, NULL){
  dfsch_object_t* sym;
  int i;

  DFSCH_OBJECT_ARG(args, sym);
  DFSCH_ARG_END(args);
  
  for (i = 0; i < sizeof(signals)/sizeof(signal_name_t); i++){
    if (dfsch_compare_keyword(sym, signals[i].name)){
      return dfsch_make_number_from_long(signals[i].signal);
    }
  }

  dfsch_error("unix:unknown-signal", sym);
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

DFSCH_DEFINE_PRIMITIVE(fchdir, NULL){
  int dir;
  DFSCH_LONG_ARG(args, dir);
  DFSCH_ARG_END(args);

  if (fchdir(dir) != 0){
    dfsch_operating_system_error("fchdir");
  }
  return NULL;
}



DFSCH_DEFINE_PRIMITIVE(chmod, NULL){
  char* fname;
  mode_t mode;
  DFSCH_STRING_ARG(args, fname);
  DFSCH_LONG_ARG(args, mode);
  DFSCH_ARG_END(args);

  if (chmod(fname, mode) != 0){
    dfsch_operating_system_error("chmod");
  }
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(fchmod, NULL){
  int file;
  mode_t mode;
  DFSCH_LONG_ARG(args, file);
  DFSCH_LONG_ARG(args, mode);
  DFSCH_ARG_END(args);

  if (fchmod(file, mode) != 0){
    dfsch_operating_system_error("fchmod");
  }
  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(chown, NULL){
  char* fname;
  uid_t user;
  gid_t group;
  DFSCH_STRING_ARG(args, fname);
  DFSCH_LONG_ARG(args, user);
  DFSCH_LONG_ARG(args, group);
  DFSCH_ARG_END(args);

  if (chown(fname, user, group) != 0){
    dfsch_operating_system_error("chown");
  }
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(fchown, NULL){
  int file;
  uid_t user;
  gid_t group;
  DFSCH_LONG_ARG(args, file);
  DFSCH_LONG_ARG(args, user);
  DFSCH_LONG_ARG(args, group);
  DFSCH_ARG_END(args);

  if (fchown(file, user, group) != 0){
    dfsch_operating_system_error("fchown");
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

  dfsch_unix_closedir(dir);

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

// TODO: exec

DFSCH_DEFINE_PRIMITIVE(exit, NULL){
  int status;
  DFSCH_LONG_ARG(args, status);
  DFSCH_ARG_END(args);

  exit(status);
}

DFSCH_DEFINE_PRIMITIVE(fork, NULL){
  pid_t pid;
  DFSCH_ARG_END(args);

  pid = fork();

  if (pid == -1){
    dfsch_operating_system_error("fork");
  }

  return dfsch_make_number_from_long(pid);
}
DFSCH_DEFINE_PRIMITIVE(fstat, NULL){
  int fd;
  dfsch_object_t* res;
  DFSCH_LONG_ARG(args, fd);
  DFSCH_ARG_END(args);

  res = dfsch_unix_make_stat_struct();

  if (fstat(fd, dfsch_unix_get_stat(res)) != 0){
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

  len = pathconf(".", _PC_PATH_MAX);
  if (len == -1){
    len = 65536; // Insane default
  }

  buf = GC_MALLOC_ATOMIC(len+1);
  ret = getcwd(buf, len+1);
  if (!ret){
    int err = errno;
    GC_FREE(buf);
    dfsch_operating_system_error_saved(err, "getcwd");
  }

  obj = dfsch_make_string_cstr(ret);
  GC_FREE(buf);
  return obj;
}
DFSCH_DEFINE_PRIMITIVE(getegid, NULL){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(getegid());
}
DFSCH_DEFINE_PRIMITIVE(geteuid, NULL){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(geteuid());
}
DFSCH_DEFINE_PRIMITIVE(getgid, NULL){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(getgid());
}
DFSCH_DEFINE_PRIMITIVE(getuid, NULL){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(getuid());
}

DFSCH_DEFINE_PRIMITIVE(getpgid, NULL){
  pid_t pid;
  DFSCH_LONG_ARG(args, pid);
  DFSCH_ARG_END(args);

  pid = getpgid(pid);

  if (pid == -1){
    dfsch_operating_system_error("getpgid");
  }

  return dfsch_make_number_from_long(pid);
}
DFSCH_DEFINE_PRIMITIVE(getpgrp, NULL){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(getpgrp());
}
DFSCH_DEFINE_PRIMITIVE(getsid, NULL){
  pid_t pid;
  DFSCH_LONG_ARG_OPT(args, pid, 0)
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(getsid(pid));
}
DFSCH_DEFINE_PRIMITIVE(getpid, NULL){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(getpid());
}
DFSCH_DEFINE_PRIMITIVE(getppid, NULL){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(getppid());
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
DFSCH_DEFINE_PRIMITIVE(kill, NULL){
  pid_t pid;
  int sig;
  DFSCH_LONG_ARG(args, pid);
  DFSCH_LONG_ARG(args, sig);
  DFSCH_ARG_END(args);


  if (kill(pid, sig) != 0){
    dfsch_operating_system_error("kill");
  }
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(killpg, NULL){
  pid_t pgrp;
  int sig;
  DFSCH_LONG_ARG(args, pgrp);
  DFSCH_LONG_ARG(args, sig);
  DFSCH_ARG_END(args);


  if (killpg(pgrp, sig) != 0){
    dfsch_operating_system_error("killpg");
  }
  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(link, NULL){
  char* old;
  char* new;
  DFSCH_STRING_ARG(args, old);
  DFSCH_STRING_ARG(args, new);
  DFSCH_ARG_END(args);

  if (link(old, new) != 0){
    dfsch_operating_system_error("link");
  }
  return NULL;
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
    dfsch_error("unix:unknown-whence-value", whence);
  }

  if (lseek(fd, offset, w) != 0){
    dfsch_operating_system_error("lseek");
  }
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(lstat, NULL){
  char* path;
  dfsch_object_t* res;
  DFSCH_STRING_ARG(args, path);
  DFSCH_ARG_END(args);

  res = dfsch_unix_make_stat_struct();

  if (lstat(path, dfsch_unix_get_stat(res)) != 0){
    dfsch_operating_system_error("lstat");
  }
  return res;
}
DFSCH_DEFINE_PRIMITIVE(mkdir, NULL){
  char* path;
  mode_t mode;
  DFSCH_STRING_ARG(args, path);
  DFSCH_LONG_ARG(args, mode);
  DFSCH_ARG_END(args);

  if (mkdir(path, mode) != 0){
    dfsch_operating_system_error("mkdir");
  }
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(mkfifo, NULL){
  char* path;
  mode_t mode;
  DFSCH_STRING_ARG(args, path);
  DFSCH_LONG_ARG(args, mode);
  DFSCH_ARG_END(args);

  if (mkfifo(path, mode) != 0){
    dfsch_operating_system_error("mkfifo");
  }
  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(nice, NULL){
  int incr;
  DFSCH_LONG_ARG(args, incr);
  DFSCH_ARG_END(args);

  if (nice(incr) != 0){
    dfsch_operating_system_error("mkfifo");
  }
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
  DFSCH_FLAG_SET("dsync", O_DSYNC, oflag);
  DFSCH_FLAG_SET("excl", O_EXCL, oflag);
  DFSCH_FLAG_SET("noctty", O_NOCTTY, oflag);
  DFSCH_FLAG_SET("nonblock", O_NONBLOCK, oflag);
  DFSCH_FLAG_SET("rsync", O_RSYNC, oflag);
  DFSCH_FLAG_SET("sync", O_SYNC, oflag);
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

  return dfsch_unix_opendir(path);
}
DFSCH_DEFINE_PRIMITIVE(pipe, NULL){
  int fds[2];
  DFSCH_ARG_END(args);

  if (pipe(fds) != 0){
    dfsch_operating_system_error("pipe");
  }

  return dfsch_list(2,
                    dfsch_make_number_from_long(fds[0]),
                    dfsch_make_number_from_long(fds[1]));
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

  return dfsch_make_string_cstr(dfsch_unix_readdir(dir));
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
DFSCH_DEFINE_PRIMITIVE(setegid, NULL){
  gid_t gid;
  DFSCH_LONG_ARG(args, gid);
  DFSCH_ARG_END(args);

  if (setegid(gid) != 0){
    dfsch_operating_system_error("setegid");
  }
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(seteuid, NULL){
  uid_t uid;
  DFSCH_LONG_ARG(args, uid);
  DFSCH_ARG_END(args);

  if (seteuid(uid) != 0){
    dfsch_operating_system_error("seteuid");
  }
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(setgid, NULL){
  gid_t gid;
  DFSCH_LONG_ARG(args, gid);
  DFSCH_ARG_END(args);

  if (setgid(gid) != 0){
    dfsch_operating_system_error("setgid");
  }
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(setuid, NULL){
  uid_t uid;
  DFSCH_LONG_ARG(args, uid);
  DFSCH_ARG_END(args);

  if (setuid(uid) != 0){
    dfsch_operating_system_error("setuid");
  }
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(setpgid, NULL){
  pid_t pid;
  pid_t pgid;
  DFSCH_LONG_ARG(args, pid);
  DFSCH_LONG_ARG(args, pgid);
  DFSCH_ARG_END(args);

  if (setpgid(pid, pgid) != 0){
    dfsch_operating_system_error("setpgid");
  }
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(setsid, NULL){
  pid_t sid;
  DFSCH_ARG_END(args);

  sid = setsid();

  if (sid < 0){
    dfsch_operating_system_error("setpgid");
  }
  return dfsch_make_number_from_long(sid);
}
DFSCH_DEFINE_PRIMITIVE(setpgrp, NULL){
  pid_t pgid;
  DFSCH_ARG_END(args);

  pgid = setsid();

  if (pgid < 0){
    dfsch_operating_system_error("setpgid");
  }
  return dfsch_make_number_from_long(pgid);
}
DFSCH_DEFINE_PRIMITIVE(sleep, NULL){
  unsigned int seconds;
  DFSCH_LONG_ARG(args, seconds);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(sleep(seconds));
}
DFSCH_DEFINE_PRIMITIVE(stat, NULL){
  char* path;
  dfsch_object_t* res;
  DFSCH_STRING_ARG(args, path);
  DFSCH_ARG_END(args);

  res = dfsch_unix_make_stat_struct();

  if (stat(path, dfsch_unix_get_stat(res)) != 0){
    if (errno == ENOENT){
      return NULL;
    }
    dfsch_operating_system_error("stat");
  }
  return res;
}

DFSCH_DEFINE_PRIMITIVE(symlink, NULL){
  char* old;
  char* new;
  DFSCH_STRING_ARG(args, old);
  DFSCH_STRING_ARG(args, new);
  DFSCH_ARG_END(args);

  if (symlink(old, new) != 0){
    dfsch_operating_system_error("symlink");
  }
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(sync, NULL){
  DFSCH_ARG_END(args);

  sync();
  
  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(wait, NULL){
  int stat;
  pid_t pid;
  DFSCH_ARG_END(args);

  pid = wait(&stat);

  if (pid == -1){
    dfsch_operating_system_error("wait");
  }
  
  return dfsch_list(2, 
                    dfsch_make_number_from_long(pid),
                    dfsch_make_number_from_long(stat));
}
DFSCH_DEFINE_PRIMITIVE(waitpid, NULL){
  int stat;
  pid_t pid;
  int options = 0;
  DFSCH_LONG_ARG(args, pid);
  DFSCH_FLAG_PARSER_BEGIN(args);
#ifdef WCONTINUED
  DFSCH_FLAG_SET("continued", WCONTINUED, options);
#endif
  DFSCH_FLAG_SET("nohang", WNOHANG, options);
  DFSCH_FLAG_SET("untraced", WUNTRACED, options);
  DFSCH_FLAG_PARSER_END(args);

  pid = waitpid(pid, &stat, options);

  if (pid == -1){
    dfsch_operating_system_error("waitpid");
  } else if (pid == 0) {
    return NULL;
  } else {
    return dfsch_list(2, 
                      dfsch_make_number_from_long(pid),
                      dfsch_make_number_from_long(stat));
  }
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


dfsch_object_t* dfsch_module_unix_register(dfsch_object_t* ctx){
  dfsch_define_cstr(ctx, "unix:mode", 
                    DFSCH_PRIMITIVE_REF(mode));
  dfsch_define_cstr(ctx, "unix:sig", 
                    DFSCH_PRIMITIVE_REF(sig));


  dfsch_define_cstr(ctx, "unix:access", 
                    DFSCH_PRIMITIVE_REF(access));
  dfsch_define_cstr(ctx, "unix:chdir", 
                    DFSCH_PRIMITIVE_REF(chdir));
  dfsch_define_cstr(ctx, "unix:fchdir", 
                    DFSCH_PRIMITIVE_REF(fchdir));
  dfsch_define_cstr(ctx, "unix:chmod", 
                    DFSCH_PRIMITIVE_REF(chmod));
  dfsch_define_cstr(ctx, "unix:fchmod", 
                    DFSCH_PRIMITIVE_REF(fchmod));
  dfsch_define_cstr(ctx, "unix:chown", 
                    DFSCH_PRIMITIVE_REF(chown));
  dfsch_define_cstr(ctx, "unix:fchown", 
                    DFSCH_PRIMITIVE_REF(fchown));
  dfsch_define_cstr(ctx, "unix:clock", 
                    DFSCH_PRIMITIVE_REF(clock));
  dfsch_define_cstr(ctx, "unix:close", 
                    DFSCH_PRIMITIVE_REF(close));
  dfsch_define_cstr(ctx, "unix:closedir", 
                    DFSCH_PRIMITIVE_REF(closedir));
  dfsch_define_cstr(ctx, "unix:creat", 
                    DFSCH_PRIMITIVE_REF(creat));
  dfsch_define_cstr(ctx, "unix:dup", 
                    DFSCH_PRIMITIVE_REF(dup));
  dfsch_define_cstr(ctx, "unix:dup2", 
                    DFSCH_PRIMITIVE_REF(dup2));
  dfsch_define_cstr(ctx, "unix:exit", 
                    DFSCH_PRIMITIVE_REF(exit));
  dfsch_define_cstr(ctx, "unix:fork", 
                    DFSCH_PRIMITIVE_REF(fork));
  dfsch_define_cstr(ctx, "unix:fstat", 
                    DFSCH_PRIMITIVE_REF(fstat));
  dfsch_define_cstr(ctx, "unix:getcwd", 
                    DFSCH_PRIMITIVE_REF(getcwd));
  dfsch_define_cstr(ctx, "unix:getegid", 
                    DFSCH_PRIMITIVE_REF(getegid));
  dfsch_define_cstr(ctx, "unix:geteuid", 
                    DFSCH_PRIMITIVE_REF(geteuid));
  dfsch_define_cstr(ctx, "unix:getgid", 
                    DFSCH_PRIMITIVE_REF(getgid));
  dfsch_define_cstr(ctx, "unix:getuid", 
                    DFSCH_PRIMITIVE_REF(getuid));
  dfsch_define_cstr(ctx, "unix:getenv", 
                    DFSCH_PRIMITIVE_REF(getenv));
  dfsch_define_cstr(ctx, "unix:getpgid", 
                    DFSCH_PRIMITIVE_REF(getpgid));
  dfsch_define_cstr(ctx, "unix:getpgrp", 
                    DFSCH_PRIMITIVE_REF(getpgrp));
  dfsch_define_cstr(ctx, "unix:getsid", 
                    DFSCH_PRIMITIVE_REF(getsid));
  dfsch_define_cstr(ctx, "unix:getpid", 
                    DFSCH_PRIMITIVE_REF(getpid));
  dfsch_define_cstr(ctx, "unix:getppid", 
                    DFSCH_PRIMITIVE_REF(getppid));
  dfsch_define_cstr(ctx, "unix:gettimeofday", 
                    DFSCH_PRIMITIVE_REF(gettimeofday));
  dfsch_define_cstr(ctx, "unix:isatty", 
                    DFSCH_PRIMITIVE_REF(isatty));
  dfsch_define_cstr(ctx, "unix:kill", 
                    DFSCH_PRIMITIVE_REF(kill));
  dfsch_define_cstr(ctx, "unix:killpg", 
                    DFSCH_PRIMITIVE_REF(killpg));
  dfsch_define_cstr(ctx, "unix:link", 
                    DFSCH_PRIMITIVE_REF(link));
  dfsch_define_cstr(ctx, "unix:lseek", 
                    DFSCH_PRIMITIVE_REF(lseek));
  dfsch_define_cstr(ctx, "unix:lstat", 
                    DFSCH_PRIMITIVE_REF(lstat));
  dfsch_define_cstr(ctx, "unix:mkdir", 
                    DFSCH_PRIMITIVE_REF(mkdir));
  dfsch_define_cstr(ctx, "unix:mkfifo", 
                    DFSCH_PRIMITIVE_REF(mkfifo));
  dfsch_define_cstr(ctx, "unix:nice", 
                    DFSCH_PRIMITIVE_REF(nice));
  dfsch_define_cstr(ctx, "unix:open", 
                    DFSCH_PRIMITIVE_REF(open));
  dfsch_define_cstr(ctx, "unix:opendir", 
                    DFSCH_PRIMITIVE_REF(opendir));
  dfsch_define_cstr(ctx, "unix:pipe", 
                    DFSCH_PRIMITIVE_REF(pipe));
  dfsch_define_cstr(ctx, "unix:raise", 
                    DFSCH_PRIMITIVE_REF(raise));
  dfsch_define_cstr(ctx, "unix:read", 
                    DFSCH_PRIMITIVE_REF(read));
  dfsch_define_cstr(ctx, "unix:readdir", 
                    DFSCH_PRIMITIVE_REF(readdir));
  dfsch_define_cstr(ctx, "unix:rename", 
                    DFSCH_PRIMITIVE_REF(rename));
  dfsch_define_cstr(ctx, "unix:rmdir", 
                    DFSCH_PRIMITIVE_REF(rmdir));
  dfsch_define_cstr(ctx, "unix:setegid", 
                    DFSCH_PRIMITIVE_REF(setegid));
  dfsch_define_cstr(ctx, "unix:seteuid", 
                    DFSCH_PRIMITIVE_REF(seteuid));
  dfsch_define_cstr(ctx, "unix:setgid", 
                    DFSCH_PRIMITIVE_REF(setgid));
  dfsch_define_cstr(ctx, "unix:setuid", 
                    DFSCH_PRIMITIVE_REF(setuid));
  dfsch_define_cstr(ctx, "unix:setpgid", 
                    DFSCH_PRIMITIVE_REF(setpgid));
  dfsch_define_cstr(ctx, "unix:setpgrp", 
                    DFSCH_PRIMITIVE_REF(setpgrp));
  dfsch_define_cstr(ctx, "unix:setsid", 
                    DFSCH_PRIMITIVE_REF(setsid));
  dfsch_define_cstr(ctx, "unix:sleep", 
                    DFSCH_PRIMITIVE_REF(sleep));
  dfsch_define_cstr(ctx, "unix:stat", 
                    DFSCH_PRIMITIVE_REF(stat));
  dfsch_define_cstr(ctx, "unix:symlink", 
                    DFSCH_PRIMITIVE_REF(symlink));
  dfsch_define_cstr(ctx, "unix:sync", 
                    DFSCH_PRIMITIVE_REF(sync));
  dfsch_define_cstr(ctx, "unix:wait", 
                    DFSCH_PRIMITIVE_REF(wait));
  dfsch_define_cstr(ctx, "unix:waitpid", 
                    DFSCH_PRIMITIVE_REF(waitpid));
  dfsch_define_cstr(ctx, "unix:write", 
                    DFSCH_PRIMITIVE_REF(write));
  dfsch_define_cstr(ctx, "unix:unlink", 
                    DFSCH_PRIMITIVE_REF(unlink));
  
  dfsch_provide(ctx, "unix");

  return NULL;
}

