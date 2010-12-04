
dnl AM_CONFIG_HEADER(src/config.h) - keep it simple

dnl package name and version number

AC_INIT(dfsch, 0.4.0-dev)
AC_CONFIG_SRCDIR(src/repl.c)
AC_CONFIG_HEADERS(config.h)
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE
dnl AC_CONFIG_AUX_DIR(config)
dnl checks for programs

dnl AC_PROG_INTLTOOL - We dont want i18n


LT_INIT

AC_PROG_CC

if test $host != $build; then
   AC_CHECK_PROGS(CC_FOR_BUILD, gcc cc)
fi


if test -z "$CC_FOR_BUILD"; then
   CC_FOR_BUILD=$CC
fi
 
AC_SUBST(CC_FOR_BUILD)

AC_PROG_CPP
AC_PROG_AWK
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET


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
AC_CHECK_LIB(gc, GC_malloc,, [have_gc="0"], [])

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

use_readline=no
if test x"$with_readline" != x"no"; then 
        if test x"$have_readline" = x"yes"; then
            use_readline=yes
            AC_DEFINE(USE_READLINE, [1], [Use readline for input])        
            READLINE_LIBS=$RL_LIBS
        fi
fi

AC_ARG_WITH([expat],
  AC_HELP_STRING([--with-expat=PATH], [Path to expat installation]),
  CPPFLAGS="$CPPFLAGS -I${withval}/include -I${withval}"
  LDFLAGS="$LDFLAGS -L${withval}/lib -L${withval}")

AC_CHECK_HEADERS(expat.h, [have_expat="1"], [have_expat="0"])
AC_CHECK_LIB(expat, XML_ParserCreate, 
                    [have_expat=$have_expat], [have_expat="0"])

if test x$have_expat != x1; then
        AC_MSG_RESULT()
        AC_MSG_RESULT(****************************************************************)
        AC_MSG_RESULT(expat not found!)
        AC_MSG_RESULT(****************************************************************)
        AC_MSG_RESULT()
        AC_MSG_ERROR(expat XML parser is required.)
fi

enable_pcre=yes

AC_ARG_ENABLE([pcre],
    AS_HELP_STRING([--disable-pcre], [Disable pcre module]))
AC_ARG_WITH([pcre],
  AC_HELP_STRING([--with-pcre=PATH], [Path to PCRE installation]),
  CPPFLAGS="$CPPFLAGS -I${withval}/include -I${withval}"
  LDFLAGS="$LDFLAGS -L${withval}/lib -L${withval}")

AC_CHECK_HEADERS(pcre.h, [have_pcre="1"], [have_pcre="0"])
AC_CHECK_LIB(pcre, pcre_compile, 
                    [have_pcre=$have_pcre], [have_pcre="0"])

if [ have_pcre = "0" ] ; then
    enable_pcre=no
fi

AM_CONDITIONAL(PCRE, test x"$enable_pcre" == x"yes")

enable_tk_gui=yes

AC_ARG_ENABLE([tk-gui],
    AS_HELP_STRING([--disable-tk-gui], [Disable tk-gui module]))

SC_PATH_TCLCONFIG
SC_LOAD_TCLCONFIG

if test x$"$enable_tk_gui" == x"yes"; then
    AC_MSG_CHECKING([for a compatible version of Tcl])      
    if test \( "${TCL_MAJOR_VERSION}" -gt 8 \) -o \( "${TCL_MAJOR_VERSION}" -eq 8 -a "${TCL_MINOR_VERSION}" -ge 4 \); then          
        AC_SUBST(TCL_INCLUDE_SPEC)
        AC_MSG_RESULT(yes (${TCL_MAJOR_VERSION}.${TCL_MINOR_VERSION}))
    else
        enable_tk_gui=no
        AC_MSG_RESULT(no)
    fi
fi

SC_PATH_TKCONFIG
SC_LOAD_TKCONFIG

if test x$"$enable_tk_gui" == x"yes"; then
    AC_MSG_CHECKING([for a compatible version of Tk])      
    if test \( "${TK_MAJOR_VERSION}" -gt 8 \) -o \( "${TK_MAJOR_VERSION}" -eq 8 -a "${TK_MINOR_VERSION}" -ge 4 \); then          
        AC_SUBST(TK_INCLUDE_SPEC)
        AC_MSG_RESULT(yes (${TK_MAJOR_VERSION}.${TK_MINOR_VERSION}))
    else
        enable_tk_gui=no
        AC_MSG_RESULT(no)
    fi
fi

AM_CONDITIONAL(TKGUI, test x"$enable_tk_gui" == x"yes")

AC_SYS_LARGEFILE 

case $host in 
     *mingw*) win32=true ;;
     *) win32=false ;;
esac
AM_CONDITIONAL(WIN32, test x$win32 == xtrue)


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

cat <<EOF
dfsch $VERSION configuration:

Optional modules:
readline ........... : $use_readline
tk-gui ............. : $enable_tk_gui
pcre ............... : $enable_pcre


EOF
