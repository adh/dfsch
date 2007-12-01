
bin_PROGRAMS = dfsch-repl
nobase_include_HEADERS = dfsch/lib/threads.h \
	dfsch/lib/unix.h \
	dfsch/lib/regex.h \
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
	dfsch/load.h

lib_LTLIBRARIES     = libdfsch.la

dfschlibexec_LTLIBRARIES = threads.la regex.la unix.la

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
	src/weak.c dfsch/weak.h\
	src/pprint.c dfsch/pprint.h\
	src/format.c dfsch/format.h\
	src/ports.c dfsch/ports.h\
	src/load.c dfsch/load.h

libdfsch_la_LDFLAGS = -version-info 4:0:0
libdfsch_la_LIBADD =  -lgc -lpthread -ldl -lm

threads_la_SOURCES = lib/threads.c dfsch/lib/threads.h
threads_la_LDFLAGS = -version-info 1:0:0  -module
threads_la_LIBADD =  libdfsch.la -lpthread

regex_la_SOURCES = lib/regex.c dfsch/lib/regex.h
regex_la_LDFLAGS = -version-info 1:0:0 -module
regex_la_LIBADD =  libdfsch.la

unix_la_SOURCES = lib/unix.c dfsch/lib/unix.h
unix_la_LDFLAGS = -version-info 1:0:0 -module
unix_la_LIBADD =  libdfsch.la


dfsch_repl_SOURCES = src/repl.c
dfsch_repl_LDADD = libdfsch.la \
	threads.la regex.la unix.la \
	@READLINE_LIBS@		


TESTS = tests/scm-test.sh

