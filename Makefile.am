
bin_PROGRAMS = dfsch-repl
nobase_include_HEADERS = dfsch/lib/threads.h \
	dfsch/lib/unix.h \
	dfsch/lib/load.h \
	dfsch/strings.h \
	dfsch/parse.h \
	dfsch/promise.h \
	dfsch/hash.h \
	dfsch/dfsch.h \
	dfsch/number.h \
	dfsch/wrapper.h \
	dfsch/magic.h \
	dfsch/object.h\
	dfsch/weak.h

lib_LTLIBRARIES     = libdfsch.la \
	libdfsch_load.la libdfsch_threads.la libdfsch_regex.la libdfsch_unix.la

man_MANS = doc/dfsch-repl.1

EXTRA_DIST = scm/morse.scm tests/scm-test.sh scm/test.scm scm/lib.scm \
	doc/dfsch-repl.1


libdfsch_la_SOURCES = src/dfsch.c dfsch/dfsch.h src/types.h src/internal.h\
	src/parse.c dfsch/parse.h \
	src/native.c src/native_cxr.c\
	src/control.c \
	src/util.c src/util.h \
	src/hash.c dfsch/hash.h \
	src/promise.c dfsch/promise.h \
	src/number.c dfsch/number.h \
	src/strings.c dfsch/strings.h \
	src/wrapper.c dfsch/wrapper.h \
	src/object.c dfsch/object.h\
	src/weak.c src/weak.h

libdfsch_la_LDFLAGS = -version-info 4:0:0
libdfsch_la_LIBADD =  -lgc -lpthread -ldl -lm

libdfsch_threads_la_SOURCES = lib/threads.c dfsch/lib/threads.h
libdfsch_threads_la_LDFLAGS = -version-info 1:0:0
libdfsch_threads_la_LIBADD =  libdfsch.la -lpthread

libdfsch_load_la_SOURCES = lib/load.c dfsch/lib/load.h
libdfsch_load_la_LDFLAGS = -version-info 1:0:0
libdfsch_load_la_LIBADD =  libdfsch.la

libdfsch_regex_la_SOURCES = lib/regex.c dfsch/lib/regex.h
libdfsch_regex_la_LDFLAGS = -version-info 1:0:0
libdfsch_regex_la_LIBADD =  libdfsch.la

libdfsch_unix_la_SOURCES = lib/unix.c dfsch/lib/unix.h
libdfsch_unix_la_LDFLAGS = -version-info 1:0:0
libdfsch_unix_la_LIBADD =  libdfsch.la


dfsch_repl_SOURCES = src/repl.c
dfsch_repl_LDADD = libdfsch.la \
	libdfsch_load.la libdfsch_threads.la libdfsch_regex.la \
	libdfsch_unix.la \
	@READLINE_LIBS@		


TESTS = tests/scm-test.sh

