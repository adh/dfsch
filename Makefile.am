
bin_PROGRAMS = dfsch-repl
nobase_include_HEADERS = dfsch/dfsch.h dfsch/stream.h
lib_LTLIBRARIES     = libdfsch.la libdfsch_load.la

EXTRA_DIST = scm/morse.scm


libdfsch_la_SOURCES = src/dfsch.c dfsch/dfsch.h src/stream.c dfsch/stream.h
libdfsch_la_LDFLAGS = -version-info 3:0:2
libdfsch_la_LIBADD =  -lgc -lpthread -ldl

libdfsch_load_la_SOURCES = src/load.c dfsch/load.h
libdfsch_load_la_LDFLAGS = -version-info 1:0:0
libdfsch_load_la_LIBADD =  libdfsch.la


dfsch_repl_SOURCES = src/repl.c
dfsch_repl_LDADD = libdfsch.la  libdfsch_load.la -lreadline -lncurses


