
bin_PROGRAMS = dfsch-repl
nobase_include_HEADERS = dfsch/lib/threads.h \
	dfsch/lib/unix.h \
	dfsch/lib/regex.h \
	dfsch/lib/inet.h \
	dfsch/lib/extref.h \
	dfsch/lib/cdebug.h \
	dfsch/lib/console.h \
	dfsch/strings.h \
	dfsch/parse.h \
	dfsch/promise.h \
	dfsch/hash.h \
	dfsch/dfsch.h \
	dfsch/types.h \
	dfsch/number.h \
	dfsch/wrapper.h \
	dfsch/magic.h \
	dfsch/object.h\
	dfsch/weak.h\
	dfsch/format.h\
	dfsch/ports.h\
	dfsch/load.h \
	dfsch/util.h \
	dfsch/bignum.h \
	dfsch/conditions.h \
	dfsch/introspect.h \
	dfsch/random.h

lib_LTLIBRARIES = libdfsch.la \
	libdfsch-console.la \
	libdfsch-extref.la \
	libdfsch-inet.la \
	libdfsch-regex.la \
	libdfsch-threads.la \
	libdfsch-cdebug.la \
	libdfsch-collections.la

dfschlibexec_LTLIBRARIES = threads.la\
	regex.la \
	unix.la \
	inet.la \
	gcollect.la \
	extref.la \
	cdebug.la \
	console.la \
	collections.la

dfschlibscm_DATA = scm/struct.scm

man_MANS = doc/dfsch-repl.1

EXTRA_DIST = scm/morse.scm tests/scm-test.sh tests/interp-test.scm scm/struct.scm \
	doc/dfsch-repl.1 src/udata-gen.c src/UnicodeData.txt doc/dfsch.doc

libdfsch_la_SOURCES = src/dfsch.c dfsch/dfsch.h src/types.h src/internal.h\
	src/parse.c dfsch/parse.h 	\
	src/native.c src/native_cxr.c	\
	src/control.c src/system.c 	\
	src/util.c src/util.h 		\
	src/hash.c dfsch/hash.h 	\
	src/promise.c dfsch/promise.h 	\
	src/number.c dfsch/number.h src/bignum.c dfsch/bignum.h	\
	src/strings.c dfsch/strings.h 	src/udata.h\
	src/wrapper.c dfsch/wrapper.h 	\
	src/object.c dfsch/object.h	\
	src/weak.c dfsch/weak.h		\
	src/format.c dfsch/format.h	\
	src/ports.c dfsch/ports.h	\
	src/load.c dfsch/load.h		\
	src/conditions.c dfsch/conditions.h \
	src/introspect.c dfsch/introspect.h \
	src/random.c dfsch/random.h

libdfsch_la_LDFLAGS = -version-info 5:0:0
libdfsch_la_LIBADD =  -lgc -lpthread -ldl -lm

libdfsch_cdebug_la_SOURCES = lib/cdebug.c dfsch/lib/cdebug.h
libdfsch_cdebug_la_LDFLAGS = -version-info 0:0:0
libdfsch_cdebug_la_LIBADD =  libdfsch.la libdfsch-console.la -lpthread

cdebug_la_SOURCES = lib/cdebug_mod.c
cdebug_la_LDFLAGS = -avoid-version  -module
cdebug_la_LIBADD =  libdfsch.la -lpthread libdfsch-cdebug.la

libdfsch_console_la_SOURCES = lib/console.c dfsch/lib/console.h
libdfsch_console_la_LDFLAGS = -version-info 0:0:0
libdfsch_console_la_LIBADD =  libdfsch.la @READLINE_LIBS@

console_la_SOURCES = lib/console_mod.c
console_la_LDFLAGS = -avoid-version  -module
console_la_LIBADD =  libdfsch.la libdfsch-console.la

libdfsch_threads_la_SOURCES = lib/threads.c dfsch/lib/threads.h
libdfsch_threads_la_LDFLAGS = -version-info 1:0:0
libdfsch_threads_la_LIBADD =  libdfsch.la -lpthread

threads_la_SOURCES = lib/threads_mod.c
threads_la_LDFLAGS = -avoid-version  -module
threads_la_LIBADD =  libdfsch.la -lpthread libdfsch-threads.la

libdfsch_regex_la_SOURCES = lib/regex.c dfsch/lib/regex.h
libdfsch_regex_la_LDFLAGS = -version-info 2:0:0
libdfsch_regex_la_LIBADD =  libdfsch.la

regex_la_SOURCES = lib/regex_mod.c 
regex_la_LDFLAGS = -avoid-version -module
regex_la_LIBADD =  libdfsch.la libdfsch-regex.la

unix_la_SOURCES = lib/unix.c dfsch/lib/unix.h
unix_la_LDFLAGS = -avoid-version -module
unix_la_LIBADD =  libdfsch.la

libdfsch_inet_la_SOURCES = lib/inet.c dfsch/lib/inet.h
libdfsch_inet_la_LDFLAGS = -version-info 2:0:0
libdfsch_inet_la_LIBADD =  libdfsch.la

inet_la_SOURCES = lib/inet_mod.c
inet_la_LDFLAGS = -avoid-version -module
inet_la_LIBADD =  libdfsch-inet.la libdfsch.la

gcollect_la_SOURCES = lib/gcollect.c dfsch/lib/gcollect.h
gcollect_la_LDFLAGS = -avoid-version -module
gcollect_la_LIBADD =  libdfsch.la

libdfsch_extref_la_SOURCES = lib/extref.c dfsch/lib/extref.h
libdfsch_extref_la_LDFLAGS = -version-info 1:0:0
libdfsch_extref_la_LIBADD =  libdfsch.la

extref_la_SOURCES = lib/extref_mod.c
extref_la_LDFLAGS = -avoid-version -module
extref_la_LIBADD =  libdfsch.la libdfsch-extref.la

libdfsch_collections_la_SOURCES = lib/collections.c dfsch/lib/collections.h
libdfsch_collections_la_LDFLAGS = -version-info 1:0:0
libdfsch_collections_la_LIBADD =  libdfsch.la

collections_la_SOURCES = lib/collections_mod.c
collections_la_LDFLAGS = -avoid-version -module
collections_la_LIBADD =  libdfsch.la libdfsch-collections.la


dfsch_repl_SOURCES = src/repl.c
dfsch_repl_LDADD = libdfsch.la libdfsch-cdebug.la libdfsch-console.la

TESTS = tests/scm-test.sh

