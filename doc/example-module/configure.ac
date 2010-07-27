
dnl AM_CONFIG_HEADER(src/config.h) - keep it simple

dnl package name and version number

PACKAGE=dfsch-example
VERSION=0.1
AC_INIT(dfsch-example, 0.1)
AC_CONFIG_SRCDIR(src/example.c)
AC_CONFIG_HEADERS(config.h)
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

if test x$have_gc != x1; then
        AC_MSG_RESULT()
        AC_MSG_RESULT(****************************************************************)
        AC_MSG_RESULT(Boehm Garbage Collector (gc) not found!)
        AC_MSG_RESULT()
        AC_MSG_RESULT(See http://www.hpl.hp.com/personal/Hans_Boehm/gc/index.html)
        AC_MSG_RESULT(****************************************************************)
        AC_MSG_RESULT()
        AC_MSG_ERROR(Cannot compile without gc!)
fi

AC_ARG_WITH([dfsch],
  AC_HELP_STRING([--with-dfsch=PATH], [Path to dfsch installation]),
  CPPFLAGS="$CPPFLAGS -I${withval}/include -I${withval}"
  LDFLAGS="$LDFLAGS -L${withval}/lib -L${withval}")

AC_CHECK_HEADERS(dfsch/dfsch.h, [have_dfsch="1"], [have_dfsch="0"])
AC_CHECK_LIB(dfsch, dfsch_cons,, [have_dfsch="0"])

if test x$have_dfsch != x1; then
        AC_MSG_RESULT()
        AC_MSG_RESULT(****************************************************************)
        AC_MSG_RESULT(dfsch not found!)
        AC_MSG_RESULT(****************************************************************)
        AC_MSG_RESULT()
        AC_MSG_ERROR(There is little point in dfsch extension without dfsch.)
fi

CFLAGS="$CFLAGS -D_REENTRANT -DGC_THREADS -D_POSIX_C_SOURCE=200112L"

AC_SYS_LARGEFILE 

dfschlibexecdir="$libdir/dfsch/"
AC_SUBST(dfschlibexecdir)

dnl automake stuff
AM_MAINTAINER_MODE

dnl create makefiles
AC_OUTPUT(Makefile)