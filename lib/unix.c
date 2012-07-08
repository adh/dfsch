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

#define _XOPEN_SOURCE 500

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <dfsch/lib/os.h>

#include <dfsch/number.h>
#include <dfsch/load.h>
#include <dfsch/conditions.h>
#include <dfsch/random.h>
#include <dfsch/magic.h>

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
#include <sys/mman.h>
#include <syslog.h>
#include <ftw.h>
#include <pwd.h>
#include <grp.h>
#include <wordexp.h>

static void gen_salt(unsigned char* buf, size_t len){
  static char* b64 = 
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789./";
  dfsch_random_get_bytes(NULL, buf, len);
  while (len){
    *buf = b64[(*buf) & 0x3f];
    buf++;
    len--;
  }
}

DFSCH_DEFINE_PRIMITIVE(crypt, NULL){
  char* key;
  char* salt;
  dfsch_object_t* ret;
  DFSCH_STRING_ARG(args, key);
  DFSCH_STRING_ARG_OPT(args, salt, NULL);
  DFSCH_ARG_END(args);

  if (salt == NULL){
#ifdef __GLIBC__
    salt = GC_MALLOC_ATOMIC(27);
    salt[0] = '$';
    salt[1] = '1';
    salt[2] = '$';
    gen_salt(salt + 3, 22);
    salt[25] = '$';
    salt[26] = 0;
#else
    salt = GC_MALLOC_ATOMIC(3);
    gen_salt(salt, 2);
    salt[2] = 0;    
#endif
  }

  dfsch_lock_libc();
  salt = crypt(key, salt);
  
  if (!salt){
    dfsch_operating_system_error("crypt");
  }

  ret = dfsch_make_string_cstr(salt);
  dfsch_unlock_libc();

  return ret;
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

  res = dfsch_os_make_stat_struct();

  if (fstat(fd, dfsch_os_get_stat(res)) != 0){
    dfsch_operating_system_error("fstat");
  }
  return res;
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
DFSCH_DEFINE_PRIMITIVE(lstat, NULL){
  char* path;
  dfsch_object_t* res;
  DFSCH_STRING_ARG(args, path);
  DFSCH_ARG_END(args);

  res = dfsch_os_make_stat_struct();

  if (lstat(path, dfsch_os_get_stat(res)) != 0){
    dfsch_operating_system_error("lstat");
  }
  return res;
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

static void mmap_region_finalizer(dfsch_string_t* obj,
                                  void* cd){
  if (munmap(obj->buf.ptr, obj->buf.len) == -1){
    perror(";; mmap-region-finalizer error");
  }
}

DFSCH_DEFINE_PRIMITIVE(mmap, "Map file into memory"){
  int flags = 0;
  int fd;
  size_t length;
  off_t offset;
  char* ptr;
  dfsch_object_t* obj;
  struct stat st;

  DFSCH_LONG_ARG(args, length);
  DFSCH_FLAG_PARSER_BEGIN_ONE(args, flags);
  DFSCH_FLAG_SET("shared", MAP_SHARED, flags);
  DFSCH_FLAG_SET("private", MAP_PRIVATE, flags);
  DFSCH_FLAG_PARSER_END(args);
  DFSCH_LONG_ARG(args, fd);
  DFSCH_INT64_ARG_OPT(args, offset, 0);

  if (fstat(fd, &st) == -1){
    dfsch_operating_system_error("mmap");
  }

  if (length + offset > st.st_size && S_ISREG(st.st_mode)){
    length = st.st_size - offset;
  }

  ptr = mmap(NULL, length, PROT_READ | PROT_WRITE, flags, fd, offset);
  if (!ptr){
    dfsch_operating_system_error("mmap");
  }
  
  obj = dfsch_make_byte_vector_nocopy(ptr, length);
  GC_REGISTER_FINALIZER(obj, (GC_finalization_proc)mmap_region_finalizer,
                        NULL, NULL, NULL);
  return obj;
}

DFSCH_DEFINE_PRIMITIVE(openlog, NULL){
  char* ident;
  static char* id_set = NULL;
  int option = 0;
  int facility = 0;
  DFSCH_STRING_ARG(args, ident);
  DFSCH_FLAG_PARSER_BEGIN_ONE(args, facility);
#ifdef LOG_AUTHPRIV
  DFSCH_FLAG_SET("authpriv", LOG_AUTHPRIV, facility);
#endif
#ifdef LOG_CRON
  DFSCH_FLAG_SET("cron", LOG_CRON, facility);
#endif
#ifdef LOG_DAEMON
  DFSCH_FLAG_SET("daemon", LOG_DAEMON, facility);
#endif
#ifdef LOG_FTP
  DFSCH_FLAG_SET("ftp", LOG_FTP, facility);
#endif
#ifdef LOG_KERN
  DFSCH_FLAG_SET("kern", LOG_KERN, facility);
#endif
#ifdef LOG_LOCAL0
  DFSCH_FLAG_SET("local0", LOG_LOCAL0, facility);
#endif
#ifdef LOG_LOCAL1
  DFSCH_FLAG_SET("local1", LOG_LOCAL1, facility);
#endif
#ifdef LOG_LOCAL2
  DFSCH_FLAG_SET("local2", LOG_LOCAL2, facility);
#endif
#ifdef LOG_LOCAL3
  DFSCH_FLAG_SET("local3", LOG_LOCAL3, facility);
#endif
#ifdef LOG_LOCAL4
  DFSCH_FLAG_SET("local4", LOG_LOCAL4, facility);
#endif
#ifdef LOG_LOCAL5
  DFSCH_FLAG_SET("local5", LOG_LOCAL5, facility);
#endif
#ifdef LOG_LOCAL6
  DFSCH_FLAG_SET("local6", LOG_LOCAL6, facility);
#endif
#ifdef LOG_LOCAL7
  DFSCH_FLAG_SET("local7", LOG_LOCAL7, facility);
#endif
#ifdef LOG_LPR
  DFSCH_FLAG_SET("lpr", LOG_LPR, facility);
#endif
#ifdef LOG_MAIL
  DFSCH_FLAG_SET("mail", LOG_MAIL, facility);
#endif
#ifdef LOG_NEWS
  DFSCH_FLAG_SET("news", LOG_NEWS, facility);
#endif
#ifdef LOG_SYSLOG
  DFSCH_FLAG_SET("syslog", LOG_SYSLOG, facility);
#endif
#ifdef LOG_USER
  DFSCH_FLAG_SET("user", LOG_USER, facility);
#endif
#ifdef LOG_UUCP
  DFSCH_FLAG_SET("uucp", LOG_UUCP, facility);
#endif
  DFSCH_FLAG_PARSER_END(args);
  DFSCH_FLAG_PARSER_BEGIN(args);
  DFSCH_FLAG_SET("cons", LOG_CONS, option);
  DFSCH_FLAG_SET("ndelay", LOG_NDELAY, option);
  DFSCH_FLAG_SET("nowait", LOG_NOWAIT, option);
  DFSCH_FLAG_SET("odelay", LOG_ODELAY, option);
  DFSCH_FLAG_SET("pid", LOG_PID, option);
#ifdef LOG_PERROR
  DFSCH_FLAG_SET("perror", LOG_PERROR, option);
#endif
  DFSCH_FLAG_PARSER_END(args);

  dfsch_lock_libc();

  ident = strdup(ident);
  openlog(ident, option, facility);
  if (id_set){
    free(id_set);
  }
  id_set = ident;

  dfsch_unlock_libc;

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(closelog, NULL){
  DFSCH_ARG_END(args);

  closelog();
  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(syslog, NULL){
  int priority;
  char* message;
  DFSCH_FLAG_PARSER_BEGIN_ONE(args, priority);
#ifdef LOG_EMERG
  DFSCH_FLAG_SET("emerg", LOG_EMERG, priority);
#endif
#ifdef LOG_ALERT
  DFSCH_FLAG_SET("alert", LOG_ALERT, priority);
#endif
#ifdef LOG_CRIT
  DFSCH_FLAG_SET("crit", LOG_CRIT, priority);
#endif
#ifdef LOG_ERR
  DFSCH_FLAG_SET("err", LOG_ERR, priority);
#endif
#ifdef LOG_WARNING
  DFSCH_FLAG_SET("warning", LOG_WARNING, priority);
#endif
#ifdef LOG_NOTICE
  DFSCH_FLAG_SET("notice", LOG_NOTICE, priority);
#endif
#ifdef LOG_INFO
  DFSCH_FLAG_SET("info", LOG_INFO, priority);
#endif
#ifdef LOG_DEBUG
  DFSCH_FLAG_SET("debug", LOG_DEBUG, priority);
#endif
  DFSCH_FLAG_PARSER_END(args);
  DFSCH_STRING_ARG(args, message);
  DFSCH_ARG_END(args);

  syslog(priority, "%s", message);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(setlogmask, NULL){
  int priority;
  DFSCH_FLAG_PARSER_BEGIN(args);
#ifdef LOG_EMERG
  DFSCH_FLAG_SET("emerg", LOG_EMERG, priority);
#endif
#ifdef LOG_ALERT
  DFSCH_FLAG_SET("alert", LOG_ALERT, priority);
#endif
#ifdef LOG_CRIT
  DFSCH_FLAG_SET("crit", LOG_CRIT, priority);
#endif
#ifdef LOG_ERR
  DFSCH_FLAG_SET("err", LOG_ERR, priority);
#endif
#ifdef LOG_WARNING
  DFSCH_FLAG_SET("warning", LOG_WARNING, priority);
#endif
#ifdef LOG_NOTICE
  DFSCH_FLAG_SET("notice", LOG_NOTICE, priority);
#endif
#ifdef LOG_INFO
  DFSCH_FLAG_SET("info", LOG_INFO, priority);
#endif
#ifdef LOG_DEBUG
  DFSCH_FLAG_SET("debug", LOG_DEBUG, priority);
#endif
  DFSCH_FLAG_PARSER_END(args);

  setlogmask(priority);

  return NULL;
}

static pthread_mutex_t ftw_mutex;
static dfsch_object_t* ftw_fun;
static pthread_once_t ftw_mutex_once = PTHREAD_ONCE_INIT;

static void ftw_mutex_init(){
  pthread_mutexattr_t attr;
  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init(&ftw_mutex, &attr);
  pthread_mutexattr_destroy(&attr);
}

static void ftw_enter(){
  pthread_once(&ftw_mutex_once, ftw_mutex_init);
  pthread_mutex_lock(&ftw_mutex);
}

static void ftw_leave(){
  pthread_mutex_unlock(&ftw_mutex);
}

static int ftw_cb(const char *fpath, const struct stat *sb,
                  int typeflag, struct FTW *ftwbuf){
  dfsch_object_t* tfo;
  int ret;

  switch (typeflag){
  case FTW_F:
    tfo = dfsch_make_keyword("FTW_F");
    break;
  case FTW_D:
    tfo = dfsch_make_keyword("FTW_D");
    break;
  case FTW_DNR:
    tfo = dfsch_make_keyword("FTW_DNR");
    break;
  case FTW_NS:
    tfo = dfsch_make_keyword("FTW_NS");
    break;
  case FTW_DP:
    tfo = dfsch_make_keyword("FTW_DP");
    break;
  case FTW_SL:
    tfo = dfsch_make_keyword("FTW_SL");
    break;
  case FTW_SLN:
    tfo = dfsch_make_keyword("FTW_SLN");
    break;
  default:
    tfo = dfsch_make_number_from_long(typeflag);
  }

  DFSCH_SCATCH_BEGIN {
    ret = (dfsch_apply(ftw_fun,
                       dfsch_list(5, 
                                  dfsch_make_string_cstr(fpath),
                                  dfsch_os_cons_stat_struct(sb),
                                  tfo,
                                  dfsch_make_number_from_long(ftwbuf->base),
                                  dfsch_make_number_from_long(ftwbuf->base))) 
           == NULL) ? 0 : 1;
  } DFSCH_SCATCH {
    ret = -1;
  } DFSCH_SCATCH_END;

  return ret;
}

DFSCH_DEFINE_PRIMITIVE(nftw, NULL){
  char* dirpath;
  dfsch_object_t* fun;
  int nopenfd;
  int flags = 0;
  int res;
  dfsch_object_t* old_fun;
  dfsch__thread_info_t* ti = dfsch__get_thread_info();

  DFSCH_STRING_ARG(args, dirpath);
  DFSCH_OBJECT_ARG(args, fun);
  DFSCH_LONG_ARG(args, nopenfd);
  DFSCH_FLAG_PARSER_BEGIN(args);
  DFSCH_FLAG_SET("chdir", FTW_CHDIR, flags);
  DFSCH_FLAG_SET("depth", FTW_DEPTH, flags);
  DFSCH_FLAG_SET("mount", FTW_MOUNT, flags);
  DFSCH_FLAG_SET("phys", FTW_PHYS, flags);
  DFSCH_FLAG_PARSER_END(args);

  ftw_enter();
  old_fun = ftw_fun;
  ftw_fun = fun;

  DFSCH_UNWIND {
    res = nftw(dirpath, ftw_cb, nopenfd, flags);
    if (ti->throw_tag){
      dfsch__continue_unwind(ti);
    }
    if (res == -1){
      dfsch_operating_system_error("nftw");      
    }

  } DFSCH_PROTECT {
    ftw_fun = old_fun;
    ftw_leave();
  } DFSCH_PROTECT_END;
  
  return NULL;
}

typedef struct passwd_object_t {
  dfsch_type_t* type;
  char   *pw_name;       /* username */
  char   *pw_passwd;     /* user password */
  long    pw_uid;        /* user ID */
  long    pw_gid;        /* group ID */
  char   *pw_gecos;      /* user information */
  char   *pw_dir;        /* home directory */
  char   *pw_shell;      /* shell program */
} passwd_object_t;

static void passwd_write(dfsch_object_t*obj, dfsch_writer_state_t* state){
  dfsch_write_unreadable_with_slots(state, obj);
}

static dfsch_slot_t passwd_slots[] = {
  DFSCH_STRING_SLOT(passwd_object_t, pw_name, DFSCH_SLOT_ACCESS_RO,
                    "User name"),
  DFSCH_STRING_SLOT(passwd_object_t, pw_passwd, DFSCH_SLOT_ACCESS_RO,
                    "User password"),
  DFSCH_LONG_SLOT(passwd_object_t, pw_uid, DFSCH_SLOT_ACCESS_RO,
                  "User ID"),
  DFSCH_LONG_SLOT(passwd_object_t, pw_uid, DFSCH_SLOT_ACCESS_RO,
                  "Primary group ID"),
  DFSCH_STRING_SLOT(passwd_object_t, pw_gecos, DFSCH_SLOT_ACCESS_RO,
                    "User information"),
  DFSCH_STRING_SLOT(passwd_object_t, pw_dir, DFSCH_SLOT_ACCESS_RO,
                    "Home directory"),
  DFSCH_STRING_SLOT(passwd_object_t, pw_shell, DFSCH_SLOT_ACCESS_RO,
                    "User shell"),
  DFSCH_SLOT_TERMINATOR
};

static dfsch_type_t passwd_type = {
  .type = DFSCH_STANDARD_TYPE,
  .superclass = NULL,
  .name = "unix:passwd",
  .size = sizeof(passwd_object_t),
  .slots = passwd_slots,
  .write = passwd_write,
};

static dfsch_object_t* cons_passwd(struct passwd *pwd){
  passwd_object_t* obj = dfsch_make_object(&passwd_type);
  obj->pw_name = dfsch_stracpy(pwd->pw_name);
  obj->pw_passwd = dfsch_stracpy(pwd->pw_passwd);
  obj->pw_uid = pwd->pw_uid;
  obj->pw_gid = pwd->pw_gid;
  obj->pw_gecos = dfsch_stracpy(pwd->pw_gecos);
  obj->pw_dir = dfsch_stracpy(pwd->pw_dir);
  obj->pw_shell = dfsch_stracpy(pwd->pw_shell);
  return obj;
}

DFSCH_DEFINE_PRIMITIVE(getpwnam, "Get user record by user name"){
  char* name;
  struct passwd* res;
  dfsch_object_t* ret;
  
  DFSCH_STRING_ARG(args, name);
  DFSCH_ARG_END(args);

  dfsch_lock_libc();
  res = getpwnam(name);
  if (!res){
    if (errno != 0){
      dfsch_unlock_libc();
      dfsch_operating_system_error("getpwnam");
    }
    ret = NULL;
  } else {
    ret = cons_passwd(res);
  }
  dfsch_unlock_libc();

  return ret;
}

DFSCH_DEFINE_PRIMITIVE(getpwuid, "Get user record by user ID"){
  long uid;
  struct passwd* res;
  dfsch_object_t* ret;
  
  DFSCH_LONG_ARG_OPT(args, uid, getuid());
  DFSCH_ARG_END(args);

  dfsch_lock_libc();
  res = getpwuid(uid);
  if (!res){
    if (errno != 0){
      dfsch_unlock_libc();
      dfsch_operating_system_error("getpwuid");
    }
    ret = NULL;
  } else {
    ret = cons_passwd(res);
  }
  dfsch_unlock_libc();

  return ret;
}

DFSCH_DEFINE_PRIMITIVE(getpwents, "Get all user records"){
  struct passwd* res;
  dfsch_list_collector_t* lc = dfsch_make_list_collector();
  
  DFSCH_ARG_END(args);

  dfsch_lock_libc();
  setpwent();
 
  errno = 0;
  for (;;){
    res = getpwent();
    if (!res){
      if (errno != 0){
        int err = errno;
        endpwent();
        dfsch_unlock_libc();
        dfsch_operating_system_error_saved(err, "getpwent");
      } else {
        break;
      }
    }
    dfsch_list_collect(lc, cons_passwd(res));
  }
  endpwent();
  dfsch_unlock_libc();

  return dfsch_collected_list(lc);
}


typedef struct group_object_t {
  dfsch_type_t* type;
  char   *gr_name;       /* group name */
  gid_t   gr_gid;        /* group ID */
  dfsch_object_t* *gr_mem;        /* group members */
} group_object_t;

static void group_write(dfsch_object_t*obj, dfsch_writer_state_t* state){
  dfsch_write_unreadable_with_slots(state, obj);
}

static dfsch_slot_t group_slots[] = {
  DFSCH_STRING_SLOT(group_object_t, gr_name, DFSCH_SLOT_ACCESS_RO,
                    "Group name"),
  DFSCH_LONG_SLOT(group_object_t, gr_gid, DFSCH_SLOT_ACCESS_RO,
                  "Group ID"),
  DFSCH_OBJECT_SLOT(group_object_t, gr_mem, DFSCH_SLOT_ACCESS_RO,
                    "Group members"),
  DFSCH_SLOT_TERMINATOR
};

static dfsch_type_t group_type = {
  .type = DFSCH_STANDARD_TYPE,
  .superclass = NULL,
  .name = "unix:group",
  .size = sizeof(group_object_t),
  .slots = group_slots,
  .write = group_write,
};

static dfsch_object_t* cons_group(struct group *grp){
  dfsch_list_collector_t* lc = dfsch_make_list_collector();
  group_object_t* obj = dfsch_make_object(&group_type);
  char** i = grp->gr_mem;
  obj->gr_name = dfsch_stracpy(grp->gr_name);
  obj->gr_gid = grp->gr_gid;
  
  while (*i){
    dfsch_list_collect(lc, dfsch_make_string_cstr(*i));
    i++;
  }

  obj->gr_mem = dfsch_collected_list(lc);

  return obj;
}

DFSCH_DEFINE_PRIMITIVE(getgrnam, "Get group record by group name"){
  char* name;
  struct group* res;
  dfsch_object_t* ret;
  
  DFSCH_STRING_ARG(args, name);
  DFSCH_ARG_END(args);

  dfsch_lock_libc();
  res = getgrnam(name);
  if (!res){
    if (errno != 0){
      dfsch_unlock_libc();
      dfsch_operating_system_error("getgrnam");
    }
    ret = NULL;
  } else {
    ret = cons_group(res);
  }
  dfsch_unlock_libc();

  return ret;
}

DFSCH_DEFINE_PRIMITIVE(getgrgid, "Get group record by group ID"){
  long uid;
  struct group* res;
  dfsch_object_t* ret;
  
  DFSCH_LONG_ARG_OPT(args, uid, getgid());
  DFSCH_ARG_END(args);

  dfsch_lock_libc();
  res = getgrgid(uid);
  if (!res){
    if (errno != 0){
      dfsch_unlock_libc();
      dfsch_operating_system_error("getgrgid");
    }
    ret = NULL;
  } else {
    ret = cons_group(res);
  }
  dfsch_unlock_libc();

  return ret;
}

DFSCH_DEFINE_PRIMITIVE(getgrents, "Get all group records"){
  struct group* res;
  dfsch_list_collector_t* lc = dfsch_make_list_collector();
  
  DFSCH_ARG_END(args);

  dfsch_lock_libc();
  setgrent();
 
  errno = 0;
  for (;;){
    res = getgrent();
    if (!res){
      if (errno != 0){
        int err = errno;
        endgrent();
        dfsch_unlock_libc();
        dfsch_operating_system_error_saved(err, "getgrent");
      } else {
        break;
      }
    }
    dfsch_list_collect(lc, cons_group(res));
  }
  endgrent();
  dfsch_unlock_libc();

  return dfsch_collected_list(lc);
}

DFSCH_DEFINE_PRIMITIVE(getgroups,
		       "Get list of supplementary group IDs of current process"){
  dfsch_list_collector_t* lc = dfsch_make_list_collector();
  int size;
  int i;
  gid_t* list;
  DFSCH_ARG_END(args);

  size = getgroups(0, NULL);
  if (size < 0){
    dfsch_operating_system_error("getgroups");
  }
  list = malloc(sizeof(gid_t) * size);
  if (getgroups(size, list) < 0){
    int err = errno;
    free(list);
    dfsch_operating_system_error_saved(err, "getgroups");
  }
  
  for (i = 0; i < size; i++){
    dfsch_list_collect(lc, dfsch_make_number_from_long(list[i]));
  }

  return dfsch_collected_list(lc);
}

DFSCH_DEFINE_PRIMITIVE(wordexp, 
                       "Perform shell word expansion on string"){
  char* string;
  wordexp_t we;
  int flags = 0;
  int ret;
  dfsch_list_collector_t* lc;
  int i;

  DFSCH_STRING_ARG(args, string);
  DFSCH_FLAG_PARSER_BEGIN(args);
  DFSCH_FLAG_SET("nocmd", WRDE_NOCMD, flags);
  DFSCH_FLAG_SET("showerr", WRDE_SHOWERR, flags);
  DFSCH_FLAG_SET("undef", WRDE_UNDEF, flags);
  DFSCH_FLAG_PARSER_END(args);

  ret = wordexp(string, &we, flags);

  if (ret != 0){
    switch(ret){
    case WRDE_BADCHAR:
      dfsch_error("Invalid character during word expansion",
                  dfsch_make_string_cstr(string));
    case WRDE_BADVAL:
      dfsch_error("Undefined variable referenced during word expansion",
                  dfsch_make_string_cstr(string));
    case WRDE_CMDSUB:
      dfsch_error("Command expansion is not allowed",
                  dfsch_make_string_cstr(string));
    case WRDE_SYNTAX:
      dfsch_error("Syntax error during word expansion",
                  dfsch_make_string_cstr(string));
    default:
      dfsch_error("Unknown during word expansion",
                  dfsch_make_string_cstr(string));
    }
  }

  lc = dfsch_make_list_collector();

  for (i = 0; i < we.we_wordc; i++){
    dfsch_list_collect(lc, dfsch_make_string_cstr(we.we_wordv[i]));
  }

  wordfree(&we);

  return dfsch_collected_list(lc);
}

dfsch_object_t* dfsch_module_unix_register(dfsch_object_t* ctx){
  dfsch_package_t* unix_pkg = dfsch_make_package("unix",
                                                 "UNIX-specific system "
                                                 "interfaces");

  dfsch_require(ctx, "os", NULL);

  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "<passwd>", &passwd_type);
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "<group>", &group_type);

  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "crypt", 
                         DFSCH_PRIMITIVE_REF(crypt));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "fchdir", 
                         DFSCH_PRIMITIVE_REF(fchdir));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "chmod", 
                         DFSCH_PRIMITIVE_REF(chmod));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "fchmod", 
                         DFSCH_PRIMITIVE_REF(fchmod));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "chown", 
                         DFSCH_PRIMITIVE_REF(chown));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "fchown", 
                         DFSCH_PRIMITIVE_REF(fchown));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "fork", 
                         DFSCH_PRIMITIVE_REF(fork));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "getegid", 
                         DFSCH_PRIMITIVE_REF(getegid));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "geteuid", 
                         DFSCH_PRIMITIVE_REF(geteuid));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "getgid", 
                         DFSCH_PRIMITIVE_REF(getgid));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "getuid", 
                         DFSCH_PRIMITIVE_REF(getuid));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "getpgid", 
                         DFSCH_PRIMITIVE_REF(getpgid));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "getpgrp", 
                         DFSCH_PRIMITIVE_REF(getpgrp));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "getsid", 
                         DFSCH_PRIMITIVE_REF(getsid));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "getpid", 
                         DFSCH_PRIMITIVE_REF(getpid));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "getppid", 
                         DFSCH_PRIMITIVE_REF(getppid));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "kill", 
                         DFSCH_PRIMITIVE_REF(kill));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "killpg", 
                         DFSCH_PRIMITIVE_REF(killpg));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "link", 
                         DFSCH_PRIMITIVE_REF(link));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "lstat", 
                         DFSCH_PRIMITIVE_REF(lstat));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "mkfifo", 
                         DFSCH_PRIMITIVE_REF(mkfifo));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "nice", 
                         DFSCH_PRIMITIVE_REF(nice));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "pipe", 
                         DFSCH_PRIMITIVE_REF(pipe));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "setegid", 
                         DFSCH_PRIMITIVE_REF(setegid));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "seteuid", 
                         DFSCH_PRIMITIVE_REF(seteuid));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "setgid", 
                         DFSCH_PRIMITIVE_REF(setgid));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "setuid", 
                         DFSCH_PRIMITIVE_REF(setuid));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "setpgid", 
                         DFSCH_PRIMITIVE_REF(setpgid));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "setpgrp", 
                         DFSCH_PRIMITIVE_REF(setpgrp));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "setsid", 
                         DFSCH_PRIMITIVE_REF(setsid));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "symlink", 
                         DFSCH_PRIMITIVE_REF(symlink));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "sync", 
                         DFSCH_PRIMITIVE_REF(sync));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "wait", 
                         DFSCH_PRIMITIVE_REF(wait));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "waitpid", 
                         DFSCH_PRIMITIVE_REF(waitpid));

  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "mmap", 
                         DFSCH_PRIMITIVE_REF(mmap));

  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "openlog", 
                         DFSCH_PRIMITIVE_REF(openlog));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "closelog", 
                         DFSCH_PRIMITIVE_REF(closelog));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "syslog", 
                         DFSCH_PRIMITIVE_REF(syslog));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "setlogmask", 
                         DFSCH_PRIMITIVE_REF(setlogmask));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "nftw", 
                         DFSCH_PRIMITIVE_REF(nftw));

  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "getpwnam", 
                         DFSCH_PRIMITIVE_REF(getpwnam));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "getpwuid", 
                         DFSCH_PRIMITIVE_REF(getpwuid));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "getpwents", 
                         DFSCH_PRIMITIVE_REF(getpwents));

  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "getgrnam", 
                         DFSCH_PRIMITIVE_REF(getgrnam));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "getgrgid", 
                         DFSCH_PRIMITIVE_REF(getgrgid));
  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "getgrents", 
                         DFSCH_PRIMITIVE_REF(getgrents));

  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "getgroups", 
                         DFSCH_PRIMITIVE_REF(getgroups));

  dfsch_defcanon_pkgcstr(ctx, unix_pkg, "wordexp", 
                         DFSCH_PRIMITIVE_REF(wordexp));

  
  dfsch_provide(ctx, "unix");

  return NULL;
}

