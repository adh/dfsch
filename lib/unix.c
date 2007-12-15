#include "dfsch/lib/unix.h"

#include <dfsch/number.h>
#include <dfsch/load.h>

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

static void throw_errno(int e, char* function){
  dfsch_error("unix:error", dfsch_list(3, 
                                       dfsch_make_symbol(function),
                                       dfsch_make_number_from_long(e),
                                       dfsch_make_string_cstr(strerror(e))));
}


typedef struct dir_t {
  dfsch_type_t *type;
  DIR *dir;
  int open;
} dir_t;

static dfsch_type_t dir_type = {
  DFSCH_STANDARD_TYPE,
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
    throw_errno(errno, "opendir");
  }
  dir->open = 1;
  GC_REGISTER_FINALIZER(dir, (GC_finalization_proc)dir_finalizer,
                        NULL, NULL, NULL);
  return (dfsch_object_t*) dir;
}

void dfsch_unix_closedir(dfsch_object_t* dir_obj){
  dir_t* dir;
  if (!dir_obj || dir_obj->type != &dir_type){
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
  if (!dir_obj || dir_obj->type != &dir_type){
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
      throw_errno(errno, "readdir");    
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

static char* stat_write(stat_t* st, int max_depth, int readable){
  return saprintf("#<stat-struct 0x%x dev 0x%lx ino %ld mode 0%lo nlink %ld uid %ld "
                  "gid %d rdev 0x%x size %lld atime %ld mtime %ld ctime %ld "
                  "blksize %ld blocks %ld>", 
                  st, 
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

  if (dfsch_compare_symbol(selector, "dev")){
    return dfsch_make_number_from_long(st->st.st_dev);
  } else if (dfsch_compare_symbol(selector, "ino")){
    return dfsch_make_number_from_long(st->st.st_ino);
  } else if (dfsch_compare_symbol(selector, "mode")){
    return dfsch_make_number_from_long(st->st.st_mode);
  } else if (dfsch_compare_symbol(selector, "nlink")){
    return dfsch_make_number_from_long(st->st.st_nlink);
  } else if (dfsch_compare_symbol(selector, "uid")){
    return dfsch_make_number_from_long(st->st.st_uid);
  } else if (dfsch_compare_symbol(selector, "gid")){
    return dfsch_make_number_from_long(st->st.st_gid);
  } else if (dfsch_compare_symbol(selector, "rdev")){
    return dfsch_make_number_from_long(st->st.st_rdev);
  } else if (dfsch_compare_symbol(selector, "size")){
    return dfsch_make_number_from_long(st->st.st_size);
  } else if (dfsch_compare_symbol(selector, "blksize")){
    return dfsch_make_number_from_long(st->st.st_blksize);
  } else if (dfsch_compare_symbol(selector, "blocks")){
    return dfsch_make_number_from_long(st->st.st_blocks);
  } else if (dfsch_compare_symbol(selector, "atime")){
    return dfsch_make_number_from_long(st->st.st_atime);
  } else if (dfsch_compare_symbol(selector, "mtime")){
    return dfsch_make_number_from_long(st->st.st_mtime);
  } else if (dfsch_compare_symbol(selector, "ctime")){
    return dfsch_make_number_from_long(st->st.st_ctime);
  } else if (dfsch_compare_symbol(selector, "isreg")){
    return dfsch_bool(S_ISREG(st->st.st_mode));
  } else if (dfsch_compare_symbol(selector, "isdir")){
    return dfsch_bool(S_ISDIR(st->st.st_mode));
  } else if (dfsch_compare_symbol(selector, "ischr")){
    return dfsch_bool(S_ISCHR(st->st.st_mode));
  } else if (dfsch_compare_symbol(selector, "isblk")){
    return dfsch_bool(S_ISBLK(st->st.st_mode));
  } else if (dfsch_compare_symbol(selector, "isfifo")){
    return dfsch_bool(S_ISFIFO(st->st.st_mode));
  } else if (dfsch_compare_symbol(selector, "islnk")){
    return dfsch_bool(S_ISLNK(st->st.st_mode));
  }

  dfsch_error("unix:no-such-stat-field", selector);
}

static dfsch_type_t stat_type = {
  DFSCH_STANDARD_TYPE,
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
  if (!stat || stat->type != &stat_type){
    dfsch_error("unix:not-a-stat-struct", stat);
  }

  return &(((stat_t*)stat)->st);
}

static dfsch_object_t* native_mode(void* baton, dfsch_object_t* args,
                                   dfsch_tail_escape_t* esc){
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

static dfsch_object_t* native_sig(void* baton, dfsch_object_t* args,
                                  dfsch_tail_escape_t* esc){
  dfsch_object_t* sym;
  int i;

  DFSCH_OBJECT_ARG(args, sym);
  DFSCH_ARG_END(args);
  
  for (i = 0; i < sizeof(signals)/sizeof(signal_name_t); i++){
    if (dfsch_compare_symbol(sym, signals[i].name)){
      return dfsch_make_number_from_long(signals[i].signal);
    }
  }

  dfsch_error("unix:unknown-signal", sym);
}

static dfsch_object_t* native_access(void* baton, dfsch_object_t* args,
                                     dfsch_tail_escape_t* esc){
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
      throw_errno(errno, "access");
    } else {
      return NULL;
    }
  }

  return dfsch_sym_true();
}


static dfsch_object_t* native_chdir(void* baton, dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  char* dir;
  DFSCH_STRING_ARG(args, dir);
  DFSCH_ARG_END(args);

  if (chdir(dir) != 0){
    throw_errno(errno, "chdir");
  }
  return NULL;
}

static dfsch_object_t* native_fchdir(void* baton, dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  int dir;
  DFSCH_LONG_ARG(args, dir);
  DFSCH_ARG_END(args);

  if (fchdir(dir) != 0){
    throw_errno(errno, "fchdir");
  }
  return NULL;
}



static dfsch_object_t* native_chmod(void* baton, dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  char* fname;
  mode_t mode;
  DFSCH_STRING_ARG(args, fname);
  DFSCH_LONG_ARG(args, mode);
  DFSCH_ARG_END(args);

  if (chmod(fname, mode) != 0){
    throw_errno(errno, "chmod");
  }
  return NULL;
}
static dfsch_object_t* native_fchmod(void* baton, dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  int file;
  mode_t mode;
  DFSCH_LONG_ARG(args, file);
  DFSCH_LONG_ARG(args, mode);
  DFSCH_ARG_END(args);

  if (fchmod(file, mode) != 0){
    throw_errno(errno, "fchmod");
  }
  return NULL;
}

static dfsch_object_t* native_chown(void* baton, dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  char* fname;
  uid_t user;
  gid_t group;
  DFSCH_STRING_ARG(args, fname);
  DFSCH_LONG_ARG(args, user);
  DFSCH_LONG_ARG(args, group);
  DFSCH_ARG_END(args);

  if (chown(fname, user, group) != 0){
    throw_errno(errno, "chown");
  }
  return NULL;
}
static dfsch_object_t* native_fchown(void* baton, dfsch_object_t* args,
                                     dfsch_tail_escape_t* esc){
  int file;
  uid_t user;
  gid_t group;
  DFSCH_LONG_ARG(args, file);
  DFSCH_LONG_ARG(args, user);
  DFSCH_LONG_ARG(args, group);
  DFSCH_ARG_END(args);

  if (fchown(file, user, group) != 0){
    throw_errno(errno, "fchown");
  }
  return NULL;
}

static dfsch_object_t* native_clock(void* baton, dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(clock());
}

static dfsch_object_t* native_close(void* baton, dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  int fd;
  DFSCH_LONG_ARG(args, fd);
  DFSCH_ARG_END(args);

  if (close(fd) != 0){
    throw_errno(errno, "close");
  }
  return NULL;
}
static dfsch_object_t* native_closedir(void* baton, dfsch_object_t* args,
                                       dfsch_tail_escape_t* esc){
  dfsch_object_t* dir;
  DFSCH_OBJECT_ARG(args, dir);
  DFSCH_ARG_END(args);

  dfsch_unix_closedir(dir);

  return NULL;
}

static dfsch_object_t* native_creat(void* baton, dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  int fd;
  char* path;
  mode_t mode;

  DFSCH_STRING_ARG(args, path);
  DFSCH_LONG_ARG(args, mode);
  DFSCH_ARG_END(args);

  fd = creat(path, mode);

  if (fd == -1){
    throw_errno(errno, "creat");
  }

  return dfsch_make_number_from_long(fd);
}

static dfsch_object_t* native_dup(void* baton, dfsch_object_t* args,
                                  dfsch_tail_escape_t* esc){
  int fd;
  DFSCH_LONG_ARG(args, fd);
  DFSCH_ARG_END(args);

  fd = dup(fd);

  if (fd < 0){
    throw_errno(errno, "dup");
  }
  return dfsch_make_number_from_long(fd);
}

static dfsch_object_t* native_dup2(void* baton, dfsch_object_t* args,
                                   dfsch_tail_escape_t* esc){
  int oldfd;
  int newfd;
  DFSCH_LONG_ARG(args, oldfd);
  DFSCH_LONG_ARG(args, newfd);
  DFSCH_ARG_END(args);

  newfd = dup2(oldfd, newfd);

  if (newfd < 0){
    throw_errno(errno, "dup2");
  }
  return dfsch_make_number_from_long(newfd);
}

// TODO: exec

static dfsch_object_t* native_exit(void* baton, dfsch_object_t* args,
                                   dfsch_tail_escape_t* esc){
  int status;
  DFSCH_LONG_ARG(args, status);
  DFSCH_ARG_END(args);

  exit(status);
}

static dfsch_object_t* native_fork(void* baton, dfsch_object_t* args,
                                   dfsch_tail_escape_t* esc){
  pid_t pid;
  DFSCH_ARG_END(args);

  pid = fork();

  if (pid == -1){
    throw_errno(errno, "fork");
  }

  return dfsch_make_number_from_long(pid);
}
static dfsch_object_t* native_fstat(void* baton, dfsch_object_t* args,
                                   dfsch_tail_escape_t* esc){
  int fd;
  dfsch_object_t* res;
  DFSCH_LONG_ARG(args, fd);
  DFSCH_ARG_END(args);

  res = dfsch_unix_make_stat_struct();

  if (fstat(fd, dfsch_unix_get_stat(res)) != 0){
    throw_errno(errno, "fstat");
  }
  return res;
}
static dfsch_object_t* native_getcwd(void* baton, dfsch_object_t* args,
                                     dfsch_tail_escape_t* esc){
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
    throw_errno(err, "getcwd");
  }

  obj = dfsch_make_string_cstr(ret);
  GC_FREE(buf);
  return obj;
}
static dfsch_object_t* native_getegid(void* baton, dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(getegid());
}
static dfsch_object_t* native_geteuid(void* baton, dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(geteuid());
}
static dfsch_object_t* native_getgid(void* baton, dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(getgid());
}
static dfsch_object_t* native_getuid(void* baton, dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(getuid());
}

static dfsch_object_t* native_getpgid(void* baton, dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  pid_t pid;
  DFSCH_LONG_ARG(args, pid);
  DFSCH_ARG_END(args);

  pid = getpgid(pid);

  if (pid == -1){
    throw_errno(errno, "getpgid");
  }

  return dfsch_make_number_from_long(pid);
}
static dfsch_object_t* native_getpgrp(void* baton, dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(getpgrp());
}
static dfsch_object_t* native_getsid(void* baton, dfsch_object_t* args,
                                     dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(getsid());
}
static dfsch_object_t* native_getpid(void* baton, dfsch_object_t* args,
                                     dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(getpid());
}
static dfsch_object_t* native_getppid(void* baton, dfsch_object_t* args,
                                     dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(getppid());
}


static dfsch_object_t* native_getenv(void* baton, dfsch_object_t* args,
                                     dfsch_tail_escape_t* esc){
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
static dfsch_object_t* native_gettimeofday(void* baton, dfsch_object_t* args,
                                           dfsch_tail_escape_t* esc){
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
static dfsch_object_t* native_isatty(void* baton, dfsch_object_t* args,
                                     dfsch_tail_escape_t* esc){
  int fd;
  int ret;
  DFSCH_LONG_ARG(args, fd);
  DFSCH_ARG_END(args);

  ret = isatty(fd);

  if (ret == 0){
    if (errno != ENOTTY){
      throw_errno(errno, "isatty");
    }else{
      return NULL;
    }
  }

  return dfsch_sym_true();
}
static dfsch_object_t* native_kill(void* baton, dfsch_object_t* args,
                                   dfsch_tail_escape_t* esc){
  pid_t pid;
  int sig;
  DFSCH_LONG_ARG(args, pid);
  DFSCH_LONG_ARG(args, sig);
  DFSCH_ARG_END(args);


  if (kill(pid, sig) != 0){
    throw_errno(errno, "kill");
  }
  return NULL;
}
static dfsch_object_t* native_killpg(void* baton, dfsch_object_t* args,
                                     dfsch_tail_escape_t* esc){
  pid_t pgrp;
  int sig;
  DFSCH_LONG_ARG(args, pgrp);
  DFSCH_LONG_ARG(args, sig);
  DFSCH_ARG_END(args);


  if (killpg(pgrp, sig) != 0){
    throw_errno(errno, "killpg");
  }
  return NULL;
}

static dfsch_object_t* native_link(void* baton, dfsch_object_t* args,
                                   dfsch_tail_escape_t* esc){
  char* old;
  char* new;
  DFSCH_STRING_ARG(args, old);
  DFSCH_STRING_ARG(args, new);
  DFSCH_ARG_END(args);

  if (link(old, new) != 0){
    throw_errno(errno, "link");
  }
  return NULL;
}
static dfsch_object_t* native_lseek(void* baton, dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  int fd;
  dfsch_object_t* whence;
  off_t offset;
  int w;

  DFSCH_LONG_ARG(args, fd);
  DFSCH_LONG_ARG(args, offset);
  DFSCH_OBJECT_ARG(args, whence);
  DFSCH_ARG_END(args);

  if (dfsch_compare_symbol(whence, "set")){
    w = SEEK_SET;
  } else if (dfsch_compare_symbol(whence, "cur")){
    w = SEEK_CUR;
  } else if (dfsch_compare_symbol(whence, "end")){
    w = SEEK_END;
  } else {
    dfsch_error("unix:unknown-whence-value", whence);
  }

  if (lseek(fd, offset, w) != 0){
    throw_errno(errno, "lseek");
  }
  return NULL;
}
static dfsch_object_t* native_lstat(void* baton, dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  char* path;
  dfsch_object_t* res;
  DFSCH_STRING_ARG(args, path);
  DFSCH_ARG_END(args);

  res = dfsch_unix_make_stat_struct();

  if (lstat(path, dfsch_unix_get_stat(res)) != 0){
    throw_errno(errno, "lstat");
  }
  return res;
}
static dfsch_object_t* native_mkdir(void* baton, dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  char* path;
  mode_t mode;
  DFSCH_STRING_ARG(args, path);
  DFSCH_LONG_ARG(args, mode);
  DFSCH_ARG_END(args);

  if (mkdir(path, mode) != 0){
    throw_errno(errno, "mkdir");
  }
  return NULL;
}
static dfsch_object_t* native_mkfifo(void* baton, dfsch_object_t* args,
                                     dfsch_tail_escape_t* esc){
  char* path;
  mode_t mode;
  DFSCH_STRING_ARG(args, path);
  DFSCH_LONG_ARG(args, mode);
  DFSCH_ARG_END(args);

  if (mkfifo(path, mode) != 0){
    throw_errno(errno, "mkfifo");
  }
  return NULL;
}

static dfsch_object_t* native_nice(void* baton, dfsch_object_t* args,
                                   dfsch_tail_escape_t* esc){
  int incr;
  DFSCH_LONG_ARG(args, incr);
  DFSCH_ARG_END(args);

  if (nice(incr) != 0){
    throw_errno(errno, "mkfifo");
  }
  return NULL;
}

static dfsch_object_t* native_open(void* baton, dfsch_object_t* args,
                                   dfsch_tail_escape_t* esc){
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
    throw_errno(errno, "open");
  }
  
  return dfsch_make_number_from_long(fd);
}
static dfsch_object_t* native_opendir(void* baton, dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  char* path;
  DFSCH_STRING_ARG(args, path);
  DFSCH_ARG_END(args);

  return dfsch_unix_opendir(path);
}
static dfsch_object_t* native_pipe(void* baton, dfsch_object_t* args,
                                   dfsch_tail_escape_t* esc){
  int fds[2];
  DFSCH_ARG_END(args);

  if (pipe(fds) != 0){
    throw_errno(errno, "pipe");
  }

  return dfsch_list(2,
                    dfsch_make_number_from_long(fds[0]),
                    dfsch_make_number_from_long(fds[1]));
}


static dfsch_object_t* native_raise(void* baton, dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  int sig;
  DFSCH_LONG_ARG(args, sig);
  DFSCH_ARG_END(args);

  if (raise(sig) != 0){
    throw_errno(errno, "raise");
  }
  return NULL;
}
static dfsch_object_t* native_read(void* baton, dfsch_object_t* args,
                                   dfsch_tail_escape_t* esc){
  int fd;
  size_t len;
  char* buf;
  ssize_t ret;
  dfsch_object_t* obj;

  DFSCH_LONG_ARG(args, fd);
  DFSCH_LONG_ARG(args, len);
  DFSCH_ARG_END(args);

  buf = GC_MALLOC_ATOMIC(len);
  ret = read(fd, buf, len);
  
  if (ret == -1){
    int e = errno;
    GC_FREE(buf);
    throw_errno(e, "read");
  } else if (ret == 0) {
    GC_FREE(buf);
    return NULL;
  } else {
    obj = dfsch_make_string_buf(buf, ret);
    GC_FREE(buf);
    return obj;
  }

}
static dfsch_object_t* native_readdir(void* baton, dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  dfsch_object_t* dir;
  DFSCH_OBJECT_ARG(args, dir);
  DFSCH_ARG_END(args);

  return dfsch_make_string_cstr(dfsch_unix_readdir(dir));
}
static dfsch_object_t* native_rename(void* baton, dfsch_object_t* args,
                                     dfsch_tail_escape_t* esc){
  char* old;
  char* new;
  DFSCH_STRING_ARG(args, old);
  DFSCH_STRING_ARG(args, new);
  DFSCH_ARG_END(args);

  if (rename(old, new) != 0){
    throw_errno(errno, "rename");
  }
  return NULL;
}
static dfsch_object_t* native_rmdir(void* baton, dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  char* path;
  DFSCH_STRING_ARG(args, path);
  DFSCH_ARG_END(args);

  if (rmdir(path) != 0){
    throw_errno(errno, "rmdir");
  }
  return NULL;
}
static dfsch_object_t* native_setegid(void* baton, dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  gid_t gid;
  DFSCH_LONG_ARG(args, gid);
  DFSCH_ARG_END(args);

  if (setegid(gid) != 0){
    throw_errno(errno, "setegid");
  }
  return NULL;
}
static dfsch_object_t* native_seteuid(void* baton, dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  uid_t uid;
  DFSCH_LONG_ARG(args, uid);
  DFSCH_ARG_END(args);

  if (seteuid(uid) != 0){
    throw_errno(errno, "seteuid");
  }
  return NULL;
}
static dfsch_object_t* native_setgid(void* baton, dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  gid_t gid;
  DFSCH_LONG_ARG(args, gid);
  DFSCH_ARG_END(args);

  if (setgid(gid) != 0){
    throw_errno(errno, "setgid");
  }
  return NULL;
}
static dfsch_object_t* native_setuid(void* baton, dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  uid_t uid;
  DFSCH_LONG_ARG(args, uid);
  DFSCH_ARG_END(args);

  if (setuid(uid) != 0){
    throw_errno(errno, "setuid");
  }
  return NULL;
}
static dfsch_object_t* native_setpgid(void* baton, dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  pid_t pid;
  pid_t pgid;
  DFSCH_LONG_ARG(args, pid);
  DFSCH_LONG_ARG(args, pgid);
  DFSCH_ARG_END(args);

  if (setpgid(pid, pgid) != 0){
    throw_errno(errno, "setpgid");
  }
  return NULL;
}
static dfsch_object_t* native_setsid(void* baton, dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  pid_t sid;
  DFSCH_ARG_END(args);

  sid = setsid();

  if (sid < 0){
    throw_errno(errno, "setpgid");
  }
  return dfsch_make_number_from_long(sid);
}
static dfsch_object_t* native_setpgrp(void* baton, dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  pid_t pgid;
  DFSCH_ARG_END(args);

  pgid = setsid();

  if (pgid < 0){
    throw_errno(errno, "setpgid");
  }
  return dfsch_make_number_from_long(pgid);
}
static dfsch_object_t* native_sleep(void* baton, dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  unsigned int seconds;
  DFSCH_LONG_ARG(args, seconds);
  DFSCH_ARG_END(args);

  return dfsch_make_number_from_long(sleep(seconds));
}
static dfsch_object_t* native_stat(void* baton, dfsch_object_t* args,
                                   dfsch_tail_escape_t* esc){
  char* path;
  dfsch_object_t* res;
  DFSCH_STRING_ARG(args, path);
  DFSCH_ARG_END(args);

  res = dfsch_unix_make_stat_struct();

  if (stat(path, dfsch_unix_get_stat(res)) != 0){
    throw_errno(errno, "stat");
  }
  return res;
}

static dfsch_object_t* native_symlink(void* baton, dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
  char* old;
  char* new;
  DFSCH_STRING_ARG(args, old);
  DFSCH_STRING_ARG(args, new);
  DFSCH_ARG_END(args);

  if (symlink(old, new) != 0){
    throw_errno(errno, "symlink");
  }
  return NULL;
}
static dfsch_object_t* native_sync(void* baton, dfsch_object_t* args,
                                   dfsch_tail_escape_t* esc){
  DFSCH_ARG_END(args);

  sync();
  
  return NULL;
}
static dfsch_object_t* native_wait(void* baton, dfsch_object_t* args,
                                   dfsch_tail_escape_t* esc){
  int stat;
  pid_t pid;
  DFSCH_ARG_END(args);

  pid = wait(&stat);

  if (pid == -1){
    throw_errno(errno, "wait");
  }
  
  return dfsch_list(2, 
                    dfsch_make_number_from_long(pid),
                    dfsch_make_number_from_long(stat));
}
static dfsch_object_t* native_waitpid(void* baton, dfsch_object_t* args,
                                      dfsch_tail_escape_t* esc){
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
    throw_errno(errno, "waitpid");
  } else if (pid == 0) {
    return NULL;
  } else {
    return dfsch_list(2, 
                      dfsch_make_number_from_long(pid),
                      dfsch_make_number_from_long(stat));
  }
}
static dfsch_object_t* native_write(void* baton, dfsch_object_t* args,
                                    dfsch_tail_escape_t* esc){
  int fd;
  ssize_t ret;
  dfsch_strbuf_t* buf;

  DFSCH_LONG_ARG(args, fd);
  DFSCH_BUFFER_ARG(args, buf);
  DFSCH_ARG_END(args);

  ret = write(fd, buf->ptr, buf->len);
  
  if (ret == -1){
    int e = errno;
    throw_errno(e, "write");
  } 

  return dfsch_make_number_from_long(ret);
}
static dfsch_object_t* native_unlink(void* baton, dfsch_object_t* args,
                                     dfsch_tail_escape_t* esc){
  char* path;
  DFSCH_STRING_ARG(args, path);
  DFSCH_ARG_END(args);

  if (unlink(path) != 0){
    throw_errno(errno, "unlink");
  }
  return NULL;
}






dfsch_object_t* dfsch_unix_register(dfsch_object_t* ctx){
  dfsch_define_cstr(ctx, "unix:mode", 
                    dfsch_make_primitive(native_mode, NULL));
  dfsch_define_cstr(ctx, "unix:sig", 
                    dfsch_make_primitive(native_sig, NULL));


  dfsch_define_cstr(ctx, "unix:access", 
                    dfsch_make_primitive(native_access, NULL));
  dfsch_define_cstr(ctx, "unix:chdir", 
                    dfsch_make_primitive(native_chdir, NULL));
  dfsch_define_cstr(ctx, "unix:fchdir", 
                    dfsch_make_primitive(native_fchdir, NULL));
  dfsch_define_cstr(ctx, "unix:chmod", 
                    dfsch_make_primitive(native_chmod, NULL));
  dfsch_define_cstr(ctx, "unix:fchmod", 
                    dfsch_make_primitive(native_fchmod, NULL));
  dfsch_define_cstr(ctx, "unix:chown", 
                    dfsch_make_primitive(native_chown, NULL));
  dfsch_define_cstr(ctx, "unix:fchown", 
                    dfsch_make_primitive(native_fchown, NULL));
  dfsch_define_cstr(ctx, "unix:clock", 
                    dfsch_make_primitive(native_clock, NULL));
  dfsch_define_cstr(ctx, "unix:close", 
                    dfsch_make_primitive(native_close, NULL));
  dfsch_define_cstr(ctx, "unix:closedir", 
                    dfsch_make_primitive(native_closedir, NULL));
  dfsch_define_cstr(ctx, "unix:creat", 
                    dfsch_make_primitive(native_creat, NULL));
  dfsch_define_cstr(ctx, "unix:dup", 
                    dfsch_make_primitive(native_dup, NULL));
  dfsch_define_cstr(ctx, "unix:dup2", 
                    dfsch_make_primitive(native_dup2, NULL));
  dfsch_define_cstr(ctx, "unix:exit", 
                    dfsch_make_primitive(native_exit, NULL));
  dfsch_define_cstr(ctx, "unix:fork", 
                    dfsch_make_primitive(native_fork, NULL));
  dfsch_define_cstr(ctx, "unix:fstat", 
                    dfsch_make_primitive(native_fstat, NULL));
  dfsch_define_cstr(ctx, "unix:getcwd", 
                    dfsch_make_primitive(native_getcwd, NULL));
  dfsch_define_cstr(ctx, "unix:getegid", 
                    dfsch_make_primitive(native_getegid, NULL));
  dfsch_define_cstr(ctx, "unix:geteuid", 
                    dfsch_make_primitive(native_geteuid, NULL));
  dfsch_define_cstr(ctx, "unix:getgid", 
                    dfsch_make_primitive(native_getgid, NULL));
  dfsch_define_cstr(ctx, "unix:getuid", 
                    dfsch_make_primitive(native_getuid, NULL));
  dfsch_define_cstr(ctx, "unix:getenv", 
                    dfsch_make_primitive(native_getenv, NULL));
  dfsch_define_cstr(ctx, "unix:getpgid", 
                    dfsch_make_primitive(native_getpgid, NULL));
  dfsch_define_cstr(ctx, "unix:getpgrp", 
                    dfsch_make_primitive(native_getpgrp, NULL));
  dfsch_define_cstr(ctx, "unix:getsid", 
                    dfsch_make_primitive(native_getsid, NULL));
  dfsch_define_cstr(ctx, "unix:getpid", 
                    dfsch_make_primitive(native_getpid, NULL));
  dfsch_define_cstr(ctx, "unix:getppid", 
                    dfsch_make_primitive(native_getppid, NULL));
  dfsch_define_cstr(ctx, "unix:gettimeofday", 
                    dfsch_make_primitive(native_gettimeofday, NULL));
  dfsch_define_cstr(ctx, "unix:isatty", 
                    dfsch_make_primitive(native_isatty, NULL));
  dfsch_define_cstr(ctx, "unix:kill", 
                    dfsch_make_primitive(native_kill, NULL));
  dfsch_define_cstr(ctx, "unix:killpg", 
                    dfsch_make_primitive(native_killpg, NULL));
  dfsch_define_cstr(ctx, "unix:link", 
                    dfsch_make_primitive(native_link, NULL));
  dfsch_define_cstr(ctx, "unix:lseek", 
                    dfsch_make_primitive(native_lseek, NULL));
  dfsch_define_cstr(ctx, "unix:lstat", 
                    dfsch_make_primitive(native_lstat, NULL));
  dfsch_define_cstr(ctx, "unix:mkdir", 
                    dfsch_make_primitive(native_mkdir, NULL));
  dfsch_define_cstr(ctx, "unix:mkfifo", 
                    dfsch_make_primitive(native_mkfifo, NULL));
  dfsch_define_cstr(ctx, "unix:nice", 
                    dfsch_make_primitive(native_nice, NULL));
  dfsch_define_cstr(ctx, "unix:open", 
                    dfsch_make_primitive(native_open, NULL));
  dfsch_define_cstr(ctx, "unix:opendir", 
                    dfsch_make_primitive(native_opendir, NULL));
  dfsch_define_cstr(ctx, "unix:pipe", 
                    dfsch_make_primitive(native_pipe, NULL));
  dfsch_define_cstr(ctx, "unix:raise", 
                    dfsch_make_primitive(native_raise, NULL));
  dfsch_define_cstr(ctx, "unix:read", 
                    dfsch_make_primitive(native_read, NULL));
  dfsch_define_cstr(ctx, "unix:readdir", 
                    dfsch_make_primitive(native_readdir, NULL));
  dfsch_define_cstr(ctx, "unix:rename", 
                    dfsch_make_primitive(native_rename, NULL));
  dfsch_define_cstr(ctx, "unix:rmdir", 
                    dfsch_make_primitive(native_rmdir, NULL));
  dfsch_define_cstr(ctx, "unix:setegid", 
                    dfsch_make_primitive(native_setegid, NULL));
  dfsch_define_cstr(ctx, "unix:seteuid", 
                    dfsch_make_primitive(native_seteuid, NULL));
  dfsch_define_cstr(ctx, "unix:setgid", 
                    dfsch_make_primitive(native_setgid, NULL));
  dfsch_define_cstr(ctx, "unix:setuid", 
                    dfsch_make_primitive(native_setuid, NULL));
  dfsch_define_cstr(ctx, "unix:setpgid", 
                    dfsch_make_primitive(native_setpgid, NULL));
  dfsch_define_cstr(ctx, "unix:setpgrp", 
                    dfsch_make_primitive(native_setpgrp, NULL));
  dfsch_define_cstr(ctx, "unix:setsid", 
                    dfsch_make_primitive(native_setsid, NULL));
  dfsch_define_cstr(ctx, "unix:sleep", 
                    dfsch_make_primitive(native_sleep, NULL));
  dfsch_define_cstr(ctx, "unix:stat", 
                    dfsch_make_primitive(native_stat, NULL));
  dfsch_define_cstr(ctx, "unix:symlink", 
                    dfsch_make_primitive(native_symlink, NULL));
  dfsch_define_cstr(ctx, "unix:sync", 
                    dfsch_make_primitive(native_sync, NULL));
  dfsch_define_cstr(ctx, "unix:wait", 
                    dfsch_make_primitive(native_wait, NULL));
  dfsch_define_cstr(ctx, "unix:waitpid", 
                    dfsch_make_primitive(native_waitpid, NULL));
  dfsch_define_cstr(ctx, "unix:write", 
                    dfsch_make_primitive(native_write, NULL));
  dfsch_define_cstr(ctx, "unix:unlink", 
                    dfsch_make_primitive(native_unlink, NULL));
  
  dfsch_provide(ctx, "unix");

  return NULL;
}

