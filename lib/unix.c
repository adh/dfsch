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
#include <sys/wait.h>
#include <fcntl.h>
#include <dirent.h>
#include <sys/time.h>
#include <time.h>
#include <sys/mman.h>


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

  if (length + offset > st.st_size){
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

dfsch_object_t* dfsch_module_unix_register(dfsch_object_t* ctx){
  dfsch_package_t* unix_pkg = dfsch_make_package("unix");

  dfsch_require(ctx, "os", NULL);

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
  
  dfsch_provide(ctx, "unix");

  return NULL;
}

