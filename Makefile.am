
if DSC
bin_PROGRAMS = dfsch dsc
include_HEADERS = dfsch/dfsch.h dfsch/stream.h dfsch/dsc.h
lib_LTLIBRARIES     = libdfsch.la libdfsch_import.la libdsc.la
else
bin_PROGRAMS = dfsch
include_HEADERS = dfsch/dfsch.h dfsch/stream.h
lib_LTLIBRARIES     = libdfsch.la libdfsch_import.la
endif

EXTRA_DIST = scm/morse.scm


libdfsch_la_SOURCES = src/dfsch.c dfsch/dfsch.h src/stream.c dfsch/stream.h
libdfsch_la_LDFLAGS = -version-info 2:0:1

libdfsch_import_la_SOURCES = src/import.c dfsch/import.h
libdfsch_import_la_LDFLAGS = -version-info 1:0:0

dfsch_SOURCES = src/repl.c
dfsch_LDADD = libdfsch.la  -lreadline -lgc -lncurses -ldl -lpthread

dsc_SOURCES = dsc/dsc.c dsc/gcc_backend.c
dsc_LDADD = libdfsch.la libdsc.la -lgc -ldl -lpthread

libdsc_la_SOURCES = dsc/libdsc.c dfsch/dsc.h
libdsc_la_LDFLAGS = -version-info 1:0:0