
dnl AM_CONFIG_HEADER(src/config.h) - keep it simple

dnl package name and version number

PACKAGE=dfsch
VERSION=0.1.1
AC_INIT(dfsch, 0.1.1)
AC_CONFIG_SRCDIR(src/repl.c)
AM_INIT_AUTOMAKE
dnl AC_CONFIG_AUX_DIR(config)
dnl checks for programs

dnl AC_PROG_INTLTOOL - We dont want i18n

AC_PROG_CC
AC_PROG_LIBTOOL



# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
dnl AC_C_INLINE

AC_CHECK_HEADERS(gc/gc.h)


dnl automake stuff
AM_MAINTAINER_MODE

dnl create makefiles
AC_OUTPUT(Makefile)