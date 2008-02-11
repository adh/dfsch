
bin_PROGRAMS = dfsch-repl
nobase_include_HEADERS = dfsch/lib/threads.h \
	dfsch/lib/unix.h \
	dfsch/lib/regex.h \
	dfsch/lib/inet.h \
	dfsch/lib/extref.h \
	dfsch/strings.h \
	dfsch/parse.h \
	dfsch/promise.h \
	dfsch/hash.h \
	dfsch/dfsch.h \
	dfsch/number.h \
	dfsch/wrapper.h \
	dfsch/magic.h \
	dfsch/object.h\
	dfsch/weak.h\
	dfsch/pprint.h\
	dfsch/format.h\
	dfsch/ports.h\
	dfsch/load.h \
	dfsch/util.h \
	dfsch/compiler.h

lib_LTLIBRARIES = libdfsch.la \
	libdfsch-extref.la \
	libdfsch-inet.la \
	libdfsch-regex.la \
	libdfsch-threads.la

dfschlibexec_LTLIBRARIES = threads.la\
	regex.la \
	unix.la \
	inet.la \
	gcollect.la \
	extref.la

dfschlibscm_DATA = scm/lib.scm

man_MANS = doc/dfsch-repl.1

EXTRA_DIST = scm/morse.scm tests/scm-test.sh scm/test.scm scm/lib.scm \
	doc/dfsch-repl.1


libdfsch_la_SOURCES = src/dfsch.c dfsch/dfsch.h src/types.h src/internal.h\
	src/parse.c dfsch/parse.h 	\
	src/native.c src/native_cxr.c	\
	src/control.c src/system.c 	\
	src/util.c src/util.h 		\
	src/hash.c dfsch/hash.h 	\
	src/promise.c dfsch/promise.h 	\
	src/number.c dfsch/number.h 	\
	src/strings.c dfsch/strings.h 	\
	src/wrapper.c dfsch/wrapper.h 	\
	src/object.c dfsch/object.h	\
	src/generic.c dfsch/generic.h	\
	src/weak.c dfsch/weak.h		\
	src/pprint.c dfsch/pprint.h	\
	src/format.c dfsch/format.h	\
	src/ports.c dfsch/ports.h	\
	src/load.c dfsch/load.h		\
	src/compiler.c dfsch/compiler.h

libdfsch_la_LDFLAGS = -version-info 5:0:0
libdfsch_la_LIBADD =  -lgc -lpthread -ldl -lm

libdfsch_threads_la_SOURCES = lib/threads.c dfsch/lib/threads.h
libdfsch_threads_la_LDFLAGS = -version-info 1:0:0
libdfsch_threads_la_LIBADD =  libdfsch.la -lpthread

threads_la_SOURCES = lib/threads_mod.c
threads_la_LDFLAGS = -version-info 1:0:0  -module
threads_la_LIBADD =  libdfsch.la -lpthread libdfsch-threads.la

libdfsch_regex_la_SOURCES = lib/regex.c dfsch/lib/regex.h
libdfsch_regex_la_LDFLAGS = -version-info 2:0:0
libdfsch_regex_la_LIBADD =  libdfsch.la

regex_la_SOURCES = lib/regex_mod.c 
regex_la_LDFLAGS = -version-info 1:0:0 -module
regex_la_LIBADD =  libdfsch.la libdfsch-regex.la

unix_la_SOURCES = lib/unix.c dfsch/lib/unix.h
unix_la_LDFLAGS = -version-info 1:0:0 -module
unix_la_LIBADD =  libdfsch.la

libdfsch_inet_la_SOURCES = lib/inet.c dfsch/lib/inet.h
libdfsch_inet_la_LDFLAGS = -version-info 2:0:0
libdfsch_inet_la_LIBADD =  libdfsch.la

inet_la_SOURCES = lib/inet_mod.c
inet_la_LDFLAGS = -version-info 2:0:0 -module
inet_la_LIBADD =  libdfsch-inet.la libdfsch.la

gcollect_la_SOURCES = lib/gcollect.c dfsch/lib/gcollect.h
gcollect_la_LDFLAGS = -version-info 1:0:0 -module
gcollect_la_LIBADD =  libdfsch.la

libdfsch_extref_la_SOURCES = lib/extref.c dfsch/lib/extref.h
libdfsch_extref_la_LDFLAGS = -version-info 1:0:0
libdfsch_extref_la_LIBADD =  libdfsch.la

extref_la_SOURCES = lib/extref_mod.c
extref_la_LDFLAGS = -version-info 1:0:0 -module
extref_la_LIBADD =  libdfsch.la libdfsch-extref.la

dfsch_repl_SOURCES = src/repl.c
dfsch_repl_LDADD = libdfsch.la \
	@READLINE_LIBS@		


TESTS = tests/scm-test.sh

