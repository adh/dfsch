
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
AC_CHECK_LIB(readline, readline, [RL_LIBS="-lreadline"], [have_readline="no"], [])


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

AC_ARG_WITH([zlib],
  AC_HELP_STRING([--with-zlib=PATH], [Path to zlib installation]),
  CPPFLAGS="$CPPFLAGS -I${withval}/include -I${withval}"
  LDFLAGS="$LDFLAGS -L${withval}/lib -L${withval}")

AC_CHECK_HEADERS(zlib.h, [have_zlib="1"], [have_zlib="0"])
AC_CHECK_LIB(z, uncompress, 
                    [have_zlib=$have_zlib], [have_zlib="0"])

if test x$have_zlib != x1; then
        AC_MSG_RESULT()
        AC_MSG_RESULT(****************************************************************)
        AC_MSG_RESULT(zlib not found!)
        AC_MSG_RESULT(****************************************************************)
        AC_MSG_RESULT()
        AC_MSG_ERROR(zlib is required.)
fi


enable_pcre=yes

AC_ARG_ENABLE([pcre],
    AS_HELP_STRING([--disable-pcre], [Disable pcre module]))
AC_ARG_WITH([pcre],
  AC_HELP_STRING([--with-pcre=PATH], [Path to PCRE installation]),
  CPPFLAGS="$CPPFLAGS -I${withval}/include -I${withval}"
  LDFLAGS="$LDFLAGS -L${withval}/lib -L${withval}")

if test x$"$enable_pcre" == x"yes"; then
    AC_CHECK_HEADERS(pcre.h, [have_pcre="1"], [have_pcre="0"])
    AC_CHECK_LIB(pcre, pcre_compile, 
                     [dummy_pcre=], [have_pcre="0"])

    if test x"$have_pcre" == x"0" ; then
        enable_pcre=no
    fi
fi

AM_CONDITIONAL(PCRE, test x"$enable_pcre" == x"yes")

enable_tk_gui=yes


AC_ARG_ENABLE([tk-gui],
    AS_HELP_STRING([--disable-tk-gui], [Disable tk-gui module]))

AC_ARG_WITH([tk-includes],
  AC_HELP_STRING([--with-tk=PATH], [Path to Tk includes]),
  tk_includes="${withval}")
AC_ARG_WITH([tcl-includes],
  AC_HELP_STRING([--with-tcl=PATH], [Path to Tcl includes]),
  tcl_includes="${withval}")

if test x$"$enable_tk_gui" == x"yes"; then
    if test x"$tcl_includes" == x; then
        CPPFLAGS="$CPPFLAGS -I/usr/include/tcl"
    else
        CPPFLAGS="$CPPFLAGS -I${tcl_includes}"
    fi

    if test x"$tk_includes" == x; then
        CPPFLAGS="$CPPFLAGS -I/usr/include/tk"
    else
        CPPFLAGS="$CPPFLAGS -I${tk_includes}"
    fi

    AC_CHECK_HEADERS(tcl.h, [have_tcltk="1"], [have_tcltk="0"])
    AC_CHECK_LIB(tcl, Tcl_Init, 
                     [dummy_tcltk=], [have_tcltk="0"])
    AC_CHECK_HEADERS(tk.h, [have_tcltk="1"], [have_tcltk="0"])
    AC_CHECK_LIB(tk, Tk_Init, 
                     [dummy_tcltk=], [have_tcltk="0"])

    if test x"$have_tcltk" == x"0" ; then
        enable_tk_gui=no
    fi
fi

AM_CONDITIONAL(TKGUI, test x"$enable_tk_gui" == x"yes")

enable_sqlite=yes

AC_ARG_ENABLE([sqlite],
    AC_HELP_STRING([--disable-sqlite], [Disable sqlite module]))
AC_ARG_WITH([sqlite],
  AC_HELP_STRING([--with-sqlite=PATH], [Path to Sqlite installation]),
  CPPFLAGS="$CPPFLAGS -I${withval}/include -I${withval}"
  LDFLAGS="$LDFLAGS -L${withval}/lib -L${withval}")

if test x$"$enable_sqlite" == x"yes"; then
    AC_CHECK_HEADERS(sqlite.h, [have_sqlite="1"], [have_sqlite="0"])
    AC_CHECK_LIB(sqlite, sqlite_open, 
                     [dummy_sqlite=], [have_sqlite="0"])

    if test x"$have_sqlite" == x"0" ; then
        enable_sqlite=no
    fi
fi

AM_CONDITIONAL(SQLITE, test x"$enable_sqlite" == x"yes")

enable_sqlite3=yes

AC_ARG_ENABLE([sqlite3],
    AC_HELP_STRING([--disable-sqlite3], [Disable sqlite3 module]))
AC_ARG_WITH([sqlite3],
  AC_HELP_STRING([--with-sqlite3=PATH], [Path to Sqlite3 installation]),
  CPPFLAGS="$CPPFLAGS -I${withval}/include -I${withval}"
  LDFLAGS="$LDFLAGS -L${withval}/lib -L${withval}")

if test x$"$enable_sqlite3" == x"yes"; then
    AC_CHECK_HEADERS(sqlite3.h, [have_sqlite3="1"], [have_sqlite3="0"])
    AC_CHECK_LIB(sqlite3, sqlite3_open, 
                     [dummy_sqlite3=], [have_sqlite3="0"])

    if test x"$have_sqlite3" == x"0" ; then
        enable_sqlite3=no
    fi
fi

AM_CONDITIONAL(SQLITE3, test x"$enable_sqlite3" == x"yes")

enable_tokyo_cabinet=yes

AC_ARG_ENABLE([tokyo-cabinet],
    AC_HELP_STRING([--disable-tokyo-cabinet], [Disable tokyocabinet module]))
AC_ARG_WITH([tokyo-cabinet],
  AC_HELP_STRING([--with-tokyo-cabinet=PATH], [Path to TOKYOCABINET installation]),
  CPPFLAGS="$CPPFLAGS -I${withval}/include -I${withval}"
  LDFLAGS="$LDFLAGS -L${withval}/lib -L${withval}")

if test x$"$enable_tokyo_cabinet" == x"yes"; then
   AC_CHECK_HEADERS(tchdb.h, [have_tokyocabinet="1"], [have_tokyocabinet="0"])
   AC_CHECK_LIB(tokyocabinet, tcadbget, 
                [have_tokyocabinet=$have_tokyocabinet], [have_tokyocabinet="0"])

   if test x"$have_tokyocabinet" == x"0" ; then
      enable_tokyo_cabinet=no
   fi
fi

AM_CONDITIONAL(TOKYO_CABINET, test x"$enable_tokyo_cabinet" == x"yes")

enable_tokyo_tyrant=yes

AC_ARG_ENABLE([tokyo-tyrant],
    AC_HELP_STRING([--disable-tokyo-tyrant], [Disable tokyotyrant module]))
AC_ARG_WITH([tokyo-tyrant],
  AC_HELP_STRING([--with-tokyo-tyrant=PATH], [Path to TOKYOTYRANT installation]),
  CPPFLAGS="$CPPFLAGS -I${withval}/include -I${withval}"
  LDFLAGS="$LDFLAGS -L${withval}/lib -L${withval}")

if test x$"$enable_tokyo_tyrant" == x"yes"; then
   AC_CHECK_HEADERS(tcrdb.h, [have_tokyotyrant="1"], [have_tokyotyrant="0"])
   AC_CHECK_LIB(tokyotyrant, tcrdbget, 
                [have_tokyotyrant=$have_tokyotyrant], [have_tokyotyrant="0"],
                [-ltokyocabinet])

   if test x"$have_tokyotyrant" == x"0" ; then
      enable_tokyo_tyrant=no
   fi
fi

AM_CONDITIONAL(TOKYO_TYRANT, test x"$enable_tokyo_tyrant" == x"yes")


enable_gd=yes

AC_ARG_ENABLE([gd],
    AC_HELP_STRING([--disable-gd], [Disable gd module]))
AC_ARG_WITH([gd],
  AC_HELP_STRING([--with-gd=PATH], [Path to Gd installation]),
  CPPFLAGS="$CPPFLAGS -I${withval}/include -I${withval}"
  LDFLAGS="$LDFLAGS -L${withval}/lib -L${withval}")

if test x$"$enable_gd" == x"yes"; then
    AC_CHECK_HEADERS(gd.h, [have_gd="1"], [have_gd="0"])
    AC_CHECK_LIB(gd, gdImageCreate, 
                     [dummy_gd=], [have_gd="0"])

    if test x"$have_gd" == x"0" ; then
        enable_gd=no
    fi
fi

AM_CONDITIONAL(GD, test x"$enable_gd" == x"yes")

enable_ffi=yes

AC_ARG_ENABLE([ffi],
    AC_HELP_STRING([--disable-ffi], [Disable foreign function interface]))
AC_ARG_WITH([ffi],
  AC_HELP_STRING([--with-libffi=PATH], [Path to libffi installation]),
  CPPFLAGS="$CPPFLAGS -I${withval}/include -I${withval}"
  LDFLAGS="$LDFLAGS -L${withval}/lib -L${withval}")

if test x$"$enable_ffi" == x"yes"; then
    AC_CHECK_HEADERS(ffi.h, [have_ffi="1"], [have_ffi="0"])
    AC_CHECK_LIB(ffi, ffi_call, 
                     [dummy_ffi=], [have_ffi="0"])

    if test x"$have_ffi" == x"0" ; then
        enable_ffi=no
    fi
fi

AM_CONDITIONAL(FFI, test x"$enable_ffi" == x"yes")


enable_curl=yes

AC_ARG_ENABLE([curl],
    AC_HELP_STRING([--disable-curl], [Disable curl module]))
AC_ARG_WITH([curl],
  AC_HELP_STRING([--with-libcurl=PATH], [Path to libcurl installation]),
  CPPFLAGS="$CPPFLAGS -I${withval}/include -I${withval}"
  LDFLAGS="$LDFLAGS -L${withval}/lib -L${withval}")

if test x$"$enable_curl" == x"yes"; then
    AC_CHECK_HEADERS(curl/curl.h, [have_curl="1"], [have_curl="0"])
    AC_CHECK_LIB(curl, curl_easy_perform, 
                     [dummy_curl=], [have_curl="0"])

    if test x"$have_curl" == x"0" ; then
        enable_curl=no
    fi
fi

AM_CONDITIONAL(CURL, test x"$enable_curl" == x"yes")




AC_SYS_LARGEFILE 

case $host in 
     *mingw*) win32=true ;;
     *) win32=false ;;
esac
AM_CONDITIONAL(WIN32, test x$win32 == xtrue)


dfschlibexecdir="${libdir}/dfsch"
dfschlibscmdir="${datadir}/dfsch/scm"

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
================================================

dfsch $VERSION configuration:

Optional features:
readline ........... : $use_readline

Optional modules:
tk-gui ............. : $enable_tk_gui
pcre ............... : $enable_pcre
sqlite ............. : $enable_sqlite
sqlite3 ............ : $enable_sqlite3
tokyo-cabinet ...... : $enable_tokyo_cabinet
tokyo-tyrant ....... : $enable_tokyo_tyrant
gd ................. : $enable_gd
FFI ................ : $enable_ffi
CURL ............... : $enable_curl


EOF
