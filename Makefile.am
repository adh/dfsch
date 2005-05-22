
bin_PROGRAMS = dfsch

EXTRA_DIST = scm/morse.scm

include_HEADERS = dfsch/dfsch.h dfsch/stream.h

lib_LTLIBRARIES     = libdfsch.la libdfsch_import.la

libdfsch_la_SOURCES = src/dfsch.c dfsch/dfsch.h src/stream.c dfsch/stream.h
libdfsch_la_LDFLAGS = -version-info 2:0:1

libdfsch_import_la_SOURCES = src/import.c dfsch/import.h
libdfsch_import_la_LDFLAGS = -version-info 1:0:0

dfsch_SOURCES = src/repl.c
dfsch_LDADD = libdfsch.la  -lreadline -lgc -lncurses -ldl 
