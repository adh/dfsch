dfschlibexec_LTLIBRARIES = example.la libdfsch-example.la

nobase_include_headers = dfsch-ext/example.h

libdfsch_example_la_SOURCES = src/example.c dfsch-ext/example.h
libdfsch_example_la_LDFLAGS = -version-info 1:0:0 -no-undefined
libdfsch_example_la_LIBADD = -ldfsch

example_la_SOURCES = src/example_mod.c
example_la_LIBADD = -ldfsch libdfsch-example.la
example_la_LDFLAGS = -module -no-undefined