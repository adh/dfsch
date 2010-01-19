
ACLOCAL_AMFLAGS=-I m4

bin_PROGRAMS = dfsch-repl dfsch-source-tool

nobase_include_HEADERS = dfsch/lib/threads.h \
	dfsch/lib/unix.h \
	dfsch/lib/regex.h \
	dfsch/lib/inet.h \
	dfsch/lib/extref.h \
	dfsch/lib/cdebug.h \
	dfsch/lib/console.h \
	dfsch/lib/xml.h \
	dfsch/lib/sxml.h \
	dfsch/lib/json.h \
	dfsch/strings.h \
	dfsch/defines.h \
	dfsch/parse.h \
	dfsch/hash.h \
	dfsch/dfsch.h \
	dfsch/types.h \
	dfsch/number.h \
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
	dfsch/random.h \
	dfsch/writer.h \
	dfsch/eqhash.h \
	dfsch/mkhash.h \
	dfsch/compile.h

lib_LTLIBRARIES = libdfsch.la \
	libdfsch-console.la \
	libdfsch-extref.la \
	libdfsch-inet.la \
	libdfsch-regex.la \
	libdfsch-threads.la \
	libdfsch-cdebug.la \
	libdfsch-collections.la \
	libdfsch-process.la \
	libdfsch-cmdopts.la \
	libdfsch-xml.la libdfsch-sxml.la \
	libdfsch-json.la \
	libdfsch-http.la \
	libdfsch-socket-port.la

dfschlibexec_LTLIBRARIES = threads.la \
	regex.la \
	unix.la \
	inet.la \
	gcollect.la \
	extref.la \
	cdebug.la \
	console.la \
	collections.la \
	process.la \
	cmdopts.la \
	xml.la \
	sxml.la \
	json.la \
	socket-port.la

dfschlibscm_DATA = scm/stream-functions.scm \
	scm/simple-tests.scm \
	scm/unix-utils.scm

man_MANS = doc/dfsch-repl.1

EXTRA_DIST = scm/morse.scm tests/scm-test.sh tests/interp-test.scm scm/struct.scm \
	doc/dfsch-repl.1 src/udata-gen.c src/UnicodeData.txt doc/dfsch.doc scm/docgen.scm

libdfsch_la_SOURCES = src/dfsch.c dfsch/dfsch.h src/internal.h\
	src/types.c src/types.h dfsch/types.h src/package.c\
	src/parse.c dfsch/parse.h 	\
	src/primitives.c src/native_cxr.c	\
	src/forms.c src/system.c src/macros.c	\
	src/util.c src/util.h 		\
	src/hash.c dfsch/hash.h 	\
	src/number.c dfsch/number.h src/bignum.c dfsch/bignum.h	\
	src/strings.c dfsch/strings.h 	udata.h udata.c\
	src/object.c dfsch/object.h	\
	src/weak.c dfsch/weak.h		\
	src/format.c dfsch/format.h	\
	src/ports.c dfsch/ports.h	\
	src/load.c dfsch/load.h		\
	src/conditions.c dfsch/conditions.h \
	src/introspect.c dfsch/introspect.h \
	src/random.c dfsch/random.h	\
	src/writer.c dfsch/writer.h	\
	src/eqhash.c dfsch/eqhash.h	\
	src/generic.c dfsch/generic.h	\
	src/mkhash.c dfsch/mkhash.h	\
	src/compile.c dfsch/compile.h

noinst_PROGRAMS=udata-gen
BUILT_SOURCES=udata.c udata.h
strings.o: udata.h
udata.h: udata-gen$(EXEEXT) src/UnicodeData.txt
	./udata-gen header $(srcdir)/src/UnicodeData.txt udata.h
udata.c: udata-gen$(EXEEXT) src/UnicodeData.txt
	./udata-gen data $(srcdir)/src/UnicodeData.txt udata.c

udata_gen_SOURCES=src/udata-gen.c
udata_gen_LDADD=-lgc

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
threads_la_LDFLAGS = -avoid-version -module
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

libdfsch_process_la_SOURCES = lib/process.c dfsch/lib/process.h
libdfsch_process_la_LDFLAGS = -version-info 1:0:0
libdfsch_process_la_LIBADD =  libdfsch.la

process_la_SOURCES = lib/process_mod.c
process_la_LDFLAGS = -avoid-version -module
process_la_LIBADD =  libdfsch.la libdfsch-process.la

libdfsch_cmdopts_la_SOURCES = lib/cmdopts.c dfsch/lib/cmdopts.h
libdfsch_cmdopts_la_LDFLAGS = -version-info 1:0:0
libdfsch_cmdopts_la_LIBADD =  libdfsch.la

cmdopts_la_SOURCES = lib/cmdopts_mod.c
cmdopts_la_LDFLAGS = -avoid-version -module
cmdopts_la_LIBADD =  libdfsch.la libdfsch-cmdopts.la

libdfsch_xml_la_SOURCES = lib/xml.c
libdfsch_xml_la_LIBADD = libdfsch.la -lexpat
libdfsch_xml_la_LDFLAGS =

libdfsch_sxml_la_SOURCES = lib/sxml.c
libdfsch_sxml_la_LIBADD = libdfsch.la -lexpat libdfsch-xml.la libdfsch-inet.la
libdfsch_sxml_la_LDFLAGS =

xml_la_SOURCES = lib/xml_mod.c
xml_la_LIBADD = libdfsch.la -lexpat libdfsch-xml.la
xml_la_LDFLAGS = -module -avoid-version

sxml_la_SOURCES = lib/sxml_mod.c
sxml_la_LIBADD = libdfsch.la -lexpat libdfsch-sxml.la
sxml_la_LDFLAGS = -module -avoid-version

libdfsch_json_la_SOURCES = lib/json.c
libdfsch_json_la_LIBADD = libdfsch.la libdfsch-inet.la
libdfsch_json_la_LDFLAGS = -version-info 1:0:0

json_la_SOURCES = lib/json_mod.c
json_la_LIBADD = libdfsch.la libdfsch-json.la
json_la_LDFLAGS = -module -avoid-version

libdfsch_socket_port_la_SOURCES = lib/socket-port.c
libdfsch_socket_port_la_LIBADD = libdfsch.la
libdfsch_socket_port_la_LDFLAGS = -version-info 1:0:0

socket_port_la_SOURCES = lib/socket-port_mod.c
socket_port_la_LIBADD = libdfsch.la libdfsch-socket-port.la
socket_port_la_LDFLAGS = -module -avoid-version


libdfsch_http_la_SOURCES = lib/http.c
libdfsch_http_la_LIBADD = libdfsch.la -lexpat libdfsch-inet.la
libdfsch_http_la_LDFLAGS = -version-info 1:0:0


dfsch_repl_SOURCES = src/repl.c
dfsch_repl_LDADD = libdfsch.la libdfsch-cdebug.la \
	libdfsch-console.la libdfsch-cmdopts.la

dfsch_source_tool_SOURCES = src/source-tool.c
dfsch_source_tool_LDADD = -lz

check_PROGRAMS = platform-test c-api-test json-parser-test

platform_test_SOURCES = tests/platform-test.c tests/test-macros.h
platform_test_LDADD =  -lgc -lpthread -ldl -lm libdfsch-cmdopts.la
c_api_test_SOURCES = tests/c-api-test.c tests/test-macros.h
c_api_test_LDADD = libdfsch.la libdfsch-cmdopts.la

json_parser_test_SOURCES = tests/json-parser-test.c
json_parser_test_LDADD = libdfsch.la libdfsch-json.la

TESTS = tests/scm-test.sh platform-test c-api-test

