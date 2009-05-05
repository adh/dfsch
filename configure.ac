
dnl AM_CONFIG_HEADER(src/config.h) - keep it simple

dnl package name and version number

AC_INIT(dfsch, 0.2.3-dev)
AC_CONFIG_SRCDIR(src/repl.c)
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

AC_CHECK_HEADERS(readline/readline.h, [have_readline="yes"], [have_readline="no"])
AC_CHECK_LIB(readline, readline, [RL_LIBS="-lreadline -lncurses"], [have_readline="no"], [-lncurses])


AC_ARG_ENABLE([profile],
  AC_HELP_STRING([--enable-profile], [Enable profiling]))

if test x"$enable_profile" = x"yes"; then 
   CFLAGS="$CFLAGS -pg -fno-omit-frame-pointer -fno-inline-functions -fno-optimize-sibling-calls"
fi 

AC_ARG_WITH([readline],
  AC_HELP_STRING([--with-readline], [Use readline library]))

if test x"$with_readline" != x"no"; then 
        if test x"$have_readline" = x"yes"; then
           AC_DEFINE(USE_READLINE, [1], [Use readline for input])        
           READLINE_LIBS=$RL_LIBS
        fi
fi

AC_ARG_WITH([expat],
  AC_HELP_STRING([--with-expat=PATH], [Path to expat installation]),
  CPPFLAGS="$CPPFLAGS -I${withval}/include -I${withval}"
  LDFLAGS="$LDFLAGS -L${withval}/lib -L${withval}")

AC_CHECK_HEADERS(expat.h, [have_expat="1"], [have_expat="0"])
AC_CHECK_LIB(expat, XML_ParserCreate,, [have_expat="0"])

if test x$have_expat != x1; then
        AC_MSG_RESULT()
        AC_MSG_RESULT(****************************************************************)
        AC_MSG_RESULT(expat not found!)
        AC_MSG_RESULT(****************************************************************)
        AC_MSG_RESULT()
        AC_MSG_ERROR(expat XML parser is required.)
fi


AC_SYS_LARGEFILE 

dfschlibexecdir="${libdir}/dfsch/"
dfschlibscmdir="${datadir}/dfsch/scm/"

CFLAGS="$CFLAGS -D_REENTRANT -DGC_THREADS"' -DDFSCH_LIB_SCM_DIR=\""$(dfschlibscmdir)"\" -DDFSCH_LIB_SO_DIR=\""$(dfschlibexecdir)"\"'

AC_DEFINE_UNQUOTED(HOST_TRIPLET, "$host", [Host triplet])
dnl AC_DEFINE_UNQUOTED(DFSCH_LIB_SCM_DIR, "$dfschlibscmdir", [Host triplet])
dnl AC_DEFINE_UNQUOTED(DFSCH_LIB_SO_DIR, "$dfschlibexecdir", [Host triplet])

AC_SUBST(dfschlibexecdir)
AC_SUBST(dfschlibscmdir)

AC_SUBST(READLINE_LIBS)

dnl automake stuff
AM_MAINTAINER_MODE

dnl create makefiles
AC_OUTPUT(Makefile)