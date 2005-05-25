
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
AC_CHECK_HEADERS([fcntl.h stdlib.h string.h strings.h unistd.h])

AC_CHECK_HEADERS(gc/gc.h)

AC_FUNC_MALLOC
AC_CHECK_FUNCS([memmove memset strcasecmp strchr strerror strpbrk])

AC_ARG_ENABLE(compiler, AC_HELP_STRING([--enable-compiler],[dfsch compiler (dsc)]),[enable_dsc="yes"])

dnl automake stuff
AM_MAINTAINER_MODE
AM_CONDITIONAL(DSC, test "$enable_dsc" = yes)


dnl create makefiles
AC_OUTPUT(Makefile)