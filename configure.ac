
dnl AM_CONFIG_HEADER(src/config.h) - keep it simple

dnl package name and version number

PACKAGE=dfsch
VERSION=0.2dev
AC_INIT(dfsch, 0.2dev)
AC_CONFIG_SRCDIR(src/repl.c)
AM_INIT_AUTOMAKE
dnl AC_CONFIG_AUX_DIR(config)
dnl checks for programs

dnl AC_PROG_INTLTOOL - We dont want i18n

AC_PROG_CC
AC_PROG_LIBTOOL
AC_PROG_CPP
AC_PROG_AWK
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_RANLIB



# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
dnl AC_C_INLINE
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h stdlib.h string.h strings.h unistd.h],, [not_posix="1"])
AC_FUNC_MALLOC
AC_CHECK_FUNCS([memmove memset strcasecmp strchr strerror strpbrk],, [not_posix="1"])

if test x$not_posix == x1; then
        AC_MSG_RESULT()
        AC_MSG_RESULT(****************************************************************)
        AC_MSG_RESULT(Your system lacks some of standard C features)
        AC_MSG_RESULT(****************************************************************)
        AC_MSG_RESULT()
        AC_MSG_ERROR(Seems like you are going to make a new port...)
fi


AC_CHECK_HEADERS(gc/gc.h, [have_gc="1"], [have_gc="0"])
AC_CHECK_LIB(gc, GC_malloc,, [have_gc="0"], [-ldl -lpthread])

if test "$have_gc" != "1"; then
        AC_MSG_RESULT()
        AC_MSG_RESULT(****************************************************************)
        AC_MSG_RESULT(Boehm Garbage Collector (gc) not found!)
        AC_MSG_RESULT()
        AC_MSG_RESULT(See http://www.hpl.hp.com/personal/Hans_Boehm/gc/index.html)
        AC_MSG_RESULT(****************************************************************)
        AC_MSG_RESULT()
        AC_MSG_ERROR(Cannot compile without gc!)
fi

AC_CHECK_HEADERS(readline/readline.h, [have_readline="1"], [have_readline="0"])
AC_CHECK_LIB(readline, readline,, [have_readline="0"], [-lncurses])

if test "$have_readline" != "1"; then
        AC_MSG_RESULT()
        AC_MSG_RESULT(****************************************************************)
        AC_MSG_RESULT(GNU Readline not found!)
        AC_MSG_RESULT()
        AC_MSG_RESULT(See http://cnswww.cns.cwru.edu/php/chet/readline/rltop.html)
        AC_MSG_RESULT(****************************************************************)
        AC_MSG_RESULT()
        AC_MSG_ERROR(Cannot compile without readline!)
fi

AC_ARG_ENABLE([profile],
  AC_HELP_STRING([--enable-profile], [Enable profiling]),
  [enable_profile="yes"])

if test x"$enable_profile" = x"yes"; then 
   CFLAGS="$CFLAGS -pg"
fi 

AC_SYS_LARGEFILE 


dnl automake stuff
AM_MAINTAINER_MODE


dnl create makefiles
AC_OUTPUT(Makefile)