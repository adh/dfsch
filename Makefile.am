
bin_PROGRAMS = dfsch-repl
nobase_include_HEADERS = \
	dfsch/dfsch.h dfsch/stream.h dfsch/load.h \
	dfsch/hash.h dfsch/number.h dfsch/strings.h

lib_LTLIBRARIES     = libdfsch.la libdfsch_load.la

EXTRA_DIST = scm/morse.scm tests/scm-test.sh scm/test.scm


libdfsch_la_SOURCES = src/dfsch.c dfsch/dfsch.h src/object.h src/internal.h\
	src/stream.c dfsch/stream.h \
	src/native.c\
	src/util.c src/util.h \
	src/hash.c dfsch/hash.h \
	src/promise.c dfsch/promise.h \
	src/number.c dfsch/number.h \
	src/strings.c dfsch/strings.h 

libdfsch_la_LDFLAGS = -version-info 4:0:0
libdfsch_la_LIBADD =  -lgc -lpthread -ldl

libdfsch_load_la_SOURCES = src/load.c dfsch/load.h
libdfsch_load_la_LDFLAGS = -version-info 1:0:0
libdfsch_load_la_LIBADD =  libdfsch.la


dfsch_repl_SOURCES = src/repl.c
dfsch_repl_LDADD = libdfsch.la  libdfsch_load.la -lreadline -lncurses


TESTS = tests/scm-test.sh

