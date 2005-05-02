
bin_PROGRAMS = dfsch

lib_LTLIBRARIES     = libdfsch.la
libdfsch_la_SOURCES = src/dfsch.c src/dfsch.h src/stream.c src/stream.h
libdfsch_la_LDFLAGS = -version-info 1:0:1

dfsch_SOURCES = src/repl.c
dfsch_LDADD = libdfst.la -lreadline -lgc # TODO: do this in portable manner
