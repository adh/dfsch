
ACLOCAL_AMFLAGS=-I m4

bin_PROGRAMS = dfsch-repl dfsch-source-tool dfsch-run

nobase_include_HEADERS = dfsch/lib/threads.h \
	dfsch/lib/os.h \
	dfsch/lib/posix-regex.h \
	dfsch/lib/inet.h \
	dfsch/lib/cdebug.h \
	dfsch/lib/cinspect.h \
	dfsch/lib/console.h \
	dfsch/lib/xml.h \
	dfsch/lib/sxml.h \
	dfsch/lib/json.h \
        dfsch/lib/socket-port.h \
        dfsch/lib/http.h \
	dfsch/lib/csv.h \
	dfsch/lib/zlib.h \
	dfsch/lib/tk-gui.h \
	dfsch/lib/pcre.h \
	dfsch/lib/crypto.h \
	dfsch/lib/tokyo-cabinet.h \
	dfsch/lib/tokyo-tyrant.h \
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
	dfsch/compiler.h \
	dfsch/generate.h \
	dfsch/backquote.h \
	dfsch/specializers.h\
	dfsch/serdes.h \
	dfsch/strhash.h \
	dfsch/sha256.h

if TKGUI
tkgui_libs=libdfsch-tk-gui.la
tkgui_modules=tk-gui-interface.la
else
tkgui_libs=
tkgui_modules=
endif

if PCRE
pcre_libs=libdfsch-pcre.la
pcre_modules=pcre.la
else
pcre_libs=
pcre_modules=
endif

if TOKYO_CABINET
tc_libs=libdfsch-tokyo-cabinet.la
tc_modules=tokyo-cabinet.la
else
tc_libs=
tc_modules=
endif

if TOKYO_TYRANT
tt_libs=libdfsch-tokyo-tyrant.la
tt_modules=tokyo-tyrant.la
else
tt_libs=
tt_modules=
endif


if WIN32
nonwin32_libs=
else
nonwin32_libs=\
	libdfsch-posix-regex.la \
	libdfsch-http.la \
	libdfsch-socket-port.la
endif

if SQLITE
sqlite_modules=sqlite.la
else
sqlite_modules=
endif

if SQLITE3
sqlite3_modules=sqlite3.la
else
sqlite3_modules=
endif

lib_LTLIBRARIES = libdfsch.la \
	libdfsch-os.la \
	libdfsch-console.la \
	libdfsch-extref.la \
	libdfsch-inet.la \
	libdfsch-threads.la \
	libdfsch-cdebug.la \
	libdfsch-cinspect.la \
	libdfsch-collections.la \
	libdfsch-process.la \
	libdfsch-cmdopts.la \
	libdfsch-xml.la libdfsch-sxml.la \
	libdfsch-json.la \
	libdfsch-zlib.la \
	libdfsch-csv.la \
	libdfsch-crypto.la \
	$(nonwin32_libs)\
	$(tkgui_libs)\
	$(pcre_libs)\
	$(tc_libs) \
	$(tt_libs)



if WIN32
nonwin32_modules=
else
nonwin32_modules=\
	unix.la \
	socket-port.la \
	http.la \
	posix-regex.la
endif

dfschlibexec_LTLIBRARIES = threads.la \
	inet.la \
	gcollect.la \
	extref.la \
	cdebug.la \
	cinspect.la \
	console.la \
	collections.la \
	process.la \
	cmdopts.la \
	xml.la \
	sxml.la \
	json.la \
	os.la \
	zlib.la \
	csv.la \
	crypto.la \
	sql-support.la \
	$(nonwin32_modules)\
	$(tkgui_modules)\
	$(pcre_modules)\
	$(sqlite_modules)\
	$(sqlite3_modules)\
	$(tc_modules)\
	$(tt_modules)


dfschlibscm_DATA = lib-scm/stream-functions.scm \
	lib-scm/simple-tests.scm \
	lib-scm/os-utils.scm \
	lib-scm/tk-gui.scm \
	lib-scm/http-server.scm \
	lib-scm/sql.scm

man_MANS = doc/dfsch-repl.1

libdfsch_la_SOURCES = src/dfsch.c dfsch/dfsch.h src/internal.h version.h\
	src/types.c src/types.h dfsch/types.h src/package.c\
	src/parse.c dfsch/parse.h 	\
	src/primitives.c src/native_cxr.c	\
	src/forms.c src/system.c src/macros.c	\
	src/util.c src/util.h 		\
	src/hash.c dfsch/hash.h 	\
	src/number.c dfsch/number.h src/bignum.c dfsch/bignum.h	\
	src/strings.c dfsch/strings.h 	udata.h udata.c\
	src/object.c dfsch/object.h	\
	src/format.c dfsch/format.h	\
	src/ports.c dfsch/ports.h	\
	src/load.c dfsch/load.h		\
	src/conditions.c dfsch/conditions.h \
	src/introspect.c dfsch/introspect.h \
	src/random.c dfsch/random.h	\
	src/writer.c dfsch/writer.h	\
	src/eqhash.c dfsch/eqhash.h	\
	src/generic.c dfsch/generic.h	\
	src/weak.c dfsch/weak.h		\
	src/mkhash.c dfsch/mkhash.h	\
	src/compiler.c dfsch/compiler.h	\
	src/backquote.c dfsch/backquote.h \
	src/trace.c dfsch/trace.h	\
	src/specializers.c dfsch/specializers.h\
	src/strhash.c dfsch/strhash.h	\
	src/serdes.c dfsch/serdes.h	\
	src/sha256.c dfsch/sha256.h	\
	std-lib.c


BUILT_SOURCES=udata.c udata.h version.h std-lib.c
strings.o: udata.h
udata.h: udata-gen$(EXEEXT) $(srcdir)/src/UnicodeData.txt
	./udata-gen$(EXEEXT) header $(srcdir)/src/UnicodeData.txt udata.h
udata.c: udata-gen$(EXEEXT) $(srcdir)/src/UnicodeData.txt
	./udata-gen$(EXEEXT) data $(srcdir)/src/UnicodeData.txt udata.c

stdlib_sources = \
	$(srcdir)/core-lib/std-lib.scm\
	$(srcdir)/core-lib/conditions.scm\
	$(srcdir)/core-lib/objects.scm

std-lib.c: $(stdlib_sources) boot-source-tool$(EXEEXT)
	./boot-source-tool$(EXEEXT) -c dfsch__std_lib \
	$(stdlib_sources) -o std-lib.c

CLEANFILES=udata.c udata.h udata-gen$(EXEEXT) version.h std-lib.c boot-source-tool$(EXEEXT)

udata-gen$(EXEEXT): $(srcdir)/src/udata-gen.c 
	$(CC_FOR_BUILD) -I$(srcdir) -o udata-gen$(EXEEXT) $(srcdir)/src/udata-gen.c

boot-source-tool$(EXEEXT): $(srcdir)/src/source-tool.c
	$(CC_FOR_BUILD) -I$(srcdir) -o boot-source-tool$(EXEEXT) -lz -lgc \
	-DPACKAGE_VERSION=\"\" $(srcdir)/src/source-tool.c

EXTRA_DIST = tests/scm-test.sh tests/interp-test.scm \
	doc/dfsch-repl.1\
	src/udata-gen.c src/UnicodeData.txt doc/dfsch.doc tools/docgen.scm \
	make-version-h.sh git-make-stamp.sh \
	$(dfschlibscm_DATA) \
	examples/tk-gui-demo.scm \
	examples/http-server-demo.scm \
	$(stdlib_sources)


libdfsch_la_LDFLAGS = -version-info 5:0:0 -no-undefined
if WIN32
libdfsch_la_LIBADD =  -lgc -lpthreadGC2 -lm -lz
pthread_libadd = -lpthreadGC2
else
libdfsch_la_LIBADD =  -lgc -lpthread -ldl -lm -lz
pthread_libadd=
endif

libdfsch_cdebug_la_SOURCES = lib/cdebug.c dfsch/lib/cdebug.h
libdfsch_cdebug_la_LDFLAGS = -version-info 0:0:0 -no-undefined
libdfsch_cdebug_la_LIBADD =  libdfsch.la libdfsch-console.la $(pthread_libadd)

cdebug_la_SOURCES = lib/cdebug_mod.c
cdebug_la_LDFLAGS = -avoid-version  -module -no-undefined -shrext .dsl
cdebug_la_LIBADD =  libdfsch.la $(pthread_libadd) libdfsch-cdebug.la

libdfsch_cinspect_la_SOURCES = lib/cinspect.c dfsch/lib/cinspect.h
libdfsch_cinspect_la_LDFLAGS = -version-info 0:0:0 -no-undefined
libdfsch_cinspect_la_LIBADD =  libdfsch.la libdfsch-console.la $(pthread_libadd)

cinspect_la_SOURCES = lib/cinspect_mod.c
cinspect_la_LDFLAGS = -avoid-version  -module -no-undefined -shrext .dsl
cinspect_la_LIBADD =  libdfsch.la $(pthread_libadd) libdfsch-cinspect.la


libdfsch_console_la_SOURCES = lib/console.c dfsch/lib/console.h
libdfsch_console_la_LDFLAGS = -version-info 0:0:0 -no-undefined
libdfsch_console_la_LIBADD =  libdfsch.la @READLINE_LIBS@

console_la_SOURCES = lib/console_mod.c
console_la_LDFLAGS = -avoid-version  -module -shrext .dsl -no-undefined
console_la_LIBADD =  libdfsch.la libdfsch-console.la

libdfsch_threads_la_SOURCES = lib/threads.c dfsch/lib/threads.h
libdfsch_threads_la_LDFLAGS = -version-info 1:0:0 -no-undefined
libdfsch_threads_la_LIBADD =  libdfsch.la $(pthread_libadd)

threads_la_SOURCES = lib/threads_mod.c
threads_la_LDFLAGS = -avoid-version -module -shrext .dsl -no-undefined
threads_la_LIBADD =  libdfsch.la $(pthread_libadd) libdfsch-threads.la

libdfsch_posix_regex_la_SOURCES = lib/posix-regex.c dfsch/lib/posix-regex.h
libdfsch_posix_regex_la_LDFLAGS = -version-info 2:0:0 -no-undefined
libdfsch_posix_regex_la_LIBADD =  libdfsch.la

posix_regex_la_SOURCES = lib/posix-regex_mod.c 
posix_regex_la_LDFLAGS = -avoid-version -module -shrext .dsl -no-undefined
posix_regex_la_LIBADD =  libdfsch.la libdfsch-posix-regex.la

unix_la_SOURCES = lib/unix.c dfsch/lib/unix.h
unix_la_LDFLAGS = -avoid-version -module -shrext .dsl -no-undefined
unix_la_LIBADD =  libdfsch.la libdfsch-os.la

libdfsch_os_la_SOURCES = lib/os.c dfsch/lib/os.h
libdfsch_os_la_LDFLAGS = -version-info 1:0:0 -no-undefined
libdfsch_os_la_LIBADD =  libdfsch.la

os_la_SOURCES = lib/os_mod.c
os_la_LDFLAGS = -avoid-version -module -shrext .dsl -no-undefined
os_la_LIBADD =  libdfsch.la libdfsch-os.la

libdfsch_zlib_la_SOURCES = lib/zlib.c dfsch/lib/zlib.h
libdfsch_zlib_la_LDFLAGS = -version-info 1:0:0 -no-undefined
libdfsch_zlib_la_LIBADD =  libdfsch.la -lz

zlib_la_SOURCES = lib/zlib_mod.c
zlib_la_LDFLAGS = -avoid-version -module -shrext .dsl -no-undefined
zlib_la_LIBADD =  libdfsch.la libdfsch-zlib.la -lz

libdfsch_inet_la_SOURCES = lib/inet.c dfsch/lib/inet.h
libdfsch_inet_la_LDFLAGS = -version-info 2:0:0 -no-undefined
libdfsch_inet_la_LIBADD =  libdfsch.la

inet_la_SOURCES = lib/inet_mod.c
inet_la_LDFLAGS = -avoid-version -module -shrext .dsl -no-undefined
inet_la_LIBADD =  libdfsch-inet.la libdfsch.la

gcollect_la_SOURCES = lib/gcollect.c dfsch/lib/gcollect.h
gcollect_la_LDFLAGS = -avoid-version -module -shrext .dsl -no-undefined
gcollect_la_LIBADD =  libdfsch.la

libdfsch_extref_la_SOURCES = lib/extref.c dfsch/lib/extref.h
libdfsch_extref_la_LDFLAGS = -version-info 1:0:0 -no-undefined
libdfsch_extref_la_LIBADD =  libdfsch.la -lpthread

extref_la_SOURCES = lib/extref_mod.c
extref_la_LDFLAGS = -avoid-version -module -shrext .dsl -no-undefined
extref_la_LIBADD =  libdfsch.la libdfsch-extref.la

libdfsch_collections_la_SOURCES = lib/collections.c dfsch/lib/collections.h
libdfsch_collections_la_LDFLAGS = -version-info 1:0:0 -no-undefined
libdfsch_collections_la_LIBADD =  libdfsch.la

collections_la_SOURCES = lib/collections_mod.c
collections_la_LDFLAGS = -avoid-version -module -shrext .dsl -no-undefined
collections_la_LIBADD =  libdfsch.la libdfsch-collections.la

libdfsch_process_la_SOURCES = lib/process.c dfsch/lib/process.h
libdfsch_process_la_LDFLAGS = -version-info 1:0:0 -no-undefined
libdfsch_process_la_LIBADD =  libdfsch.la

process_la_SOURCES = lib/process_mod.c
process_la_LDFLAGS = -avoid-version -module -shrext .dsl -no-undefined
process_la_LIBADD =  libdfsch.la libdfsch-process.la

libdfsch_cmdopts_la_SOURCES = lib/cmdopts.c dfsch/lib/cmdopts.h
libdfsch_cmdopts_la_LDFLAGS = -version-info 1:0:0 -no-undefined
libdfsch_cmdopts_la_LIBADD =  libdfsch.la

cmdopts_la_SOURCES = lib/cmdopts_mod.c
cmdopts_la_LDFLAGS = -avoid-version -module -shrext .dsl -no-undefined
cmdopts_la_LIBADD =  libdfsch.la libdfsch-cmdopts.la

libdfsch_xml_la_SOURCES = lib/xml.c
libdfsch_xml_la_LIBADD = libdfsch.la -lexpat
libdfsch_xml_la_LDFLAGS = -no-undefined

libdfsch_sxml_la_SOURCES = lib/sxml.c
libdfsch_sxml_la_LIBADD = libdfsch.la -lexpat libdfsch-xml.la libdfsch-inet.la
libdfsch_sxml_la_LDFLAGS = -no-undefined

xml_la_SOURCES = lib/xml_mod.c
xml_la_LIBADD = libdfsch.la -lexpat libdfsch-xml.la
xml_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined

sxml_la_SOURCES = lib/sxml_mod.c
sxml_la_LIBADD = libdfsch.la -lexpat libdfsch-sxml.la
sxml_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined

libdfsch_json_la_SOURCES = lib/json.c
libdfsch_json_la_LIBADD = libdfsch.la libdfsch-inet.la
libdfsch_json_la_LDFLAGS = -version-info 1:0:0 -no-undefined

json_la_SOURCES = lib/json_mod.c
json_la_LIBADD = libdfsch.la libdfsch-json.la
json_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined

libdfsch_socket_port_la_SOURCES = lib/socket-port.c
libdfsch_socket_port_la_LIBADD = libdfsch.la
libdfsch_socket_port_la_LDFLAGS = -version-info 1:0:0

socket_port_la_SOURCES = lib/socket-port_mod.c
socket_port_la_LIBADD = libdfsch.la libdfsch-socket-port.la
socket_port_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined


libdfsch_http_la_SOURCES = lib/http.c
libdfsch_http_la_LIBADD = libdfsch.la -lexpat libdfsch-inet.la
libdfsch_http_la_LDFLAGS = -version-info 1:0:0 -no-undefined

http_la_SOURCES = lib/http_mod.c
http_la_LIBADD = libdfsch.la libdfsch-http.la
http_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined


libdfsch_csv_la_SOURCES = lib/csv.c
libdfsch_csv_la_LIBADD = libdfsch.la
libdfsch_csv_la_LDFLAGS = -no-undefined

csv_la_SOURCES = lib/csv_mod.c
csv_la_LIBADD = libdfsch.la libdfsch-csv.la
csv_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined


libdfsch_tk_gui_la_SOURCES = lib/tk-gui.c
libdfsch_tk_gui_la_CFLAGS = $(TCL_INCLUDE_SPEC) $(TK_INCLUDE_SPEC)
libdfsch_tk_gui_la_LIBADD = libdfsch.la $(TCL_LIB_SPEC) $(TK_LIB_SPEC)
libdfsch_tk_gui_la_LDFLAGS = -no-undefined

tk_gui_interface_la_SOURCES = lib/tk-gui_mod.c
tk_gui_interface_la_CFLAGS = $(TCL_INCLUDE_SPEC) $(TK_INCLUDE_SPEC)
tk_gui_interface_la_LIBADD = libdfsch.la libdfsch-tk-gui.la $(TCL_LIB_SPEC) $(TK_LIB_SPEC)
tk_gui_interface_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined

libdfsch_crypto_la_SOURCES = lib/crypto/crypto.c \
	lib/crypto/modes.c \
	lib/crypto/aes.c lib/crypto/aes_tab.h \
	lib/crypto/xtea.c \
	lib/crypto/blowfish.c \
	lib/crypto/hmac.c \
	lib/crypto/sha256-desc.c \
	lib/crypto/sha512.c \
	lib/crypto/sha1.c \
	lib/crypto/md5.c \
	lib/crypto/md4.c \
	lib/crypto/curve25519.c \
	lib/crypto/rsa.c \
	lib/crypto/rsa-pad.c \
	lib/crypto/salsa20.c \
	lib/crypto/prng.c \
	lib/crypto/macros.h

if WIN32
prng_libadd = -lrpcrt4
else
prng_libadd = -lrt
endif

libdfsch_crypto_la_LIBADD = libdfsch.la $(prng_libadd)
libdfsch_crypto_la_LDFLAGS = -no-undefined -version-info 1:0:0


crypto_la_SOURCES = lib/crypto/module.c
crypto_la_LIBADD = libdfsch.la libdfsch-crypto.la
crypto_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined


dfsch_repl_SOURCES = src/repl.c
dfsch_repl_LDADD = libdfsch.la libdfsch-cdebug.la \
	libdfsch-console.la libdfsch-cmdopts.la libdfsch-cinspect.la

libdfsch_pcre_la_SOURCES = lib/pcre.c
libdfsch_pcre_la_LIBADD = libdfsch.la -lpcre
libdfsch_pcre_la_LDFLAGS = -no-undefined

pcre_la_SOURCES = lib/pcre_mod.c
pcre_la_LIBADD = libdfsch.la libdfsch-pcre.la
pcre_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined

sqlite_la_SOURCES = lib/sql/sqlite.c
sqlite_la_LIBADD = libdfsch.la -lsqlite
sqlite_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined

sqlite3_la_SOURCES = lib/sql/sqlite3.c
sqlite3_la_LIBADD = libdfsch.la -lsqlite3
sqlite3_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined

sql_support_la_SOURCES = lib/sql/sql-support.c
sql_support_la_LIBADD = libdfsch.la
sql_support_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined

libdfsch_tokyo_cabinet_la_SOURCES = lib/tokyo-cabinet.c
libdfsch_tokyo_cabinet_la_LIBADD = libdfsch.la -ltokyocabinet
libdfsch_tokyo_cabinet_la_LDFLAGS = -no-undefined

tokyo_cabinet_la_SOURCES = lib/tokyo-cabinet_mod.c
tokyo_cabinet_la_LIBADD = libdfsch.la libdfsch-tokyo-cabinet.la
tokyo_cabinet_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined

libdfsch_tokyo_tyrant_la_SOURCES = lib/tokyo-tyrant.c
libdfsch_tokyo_tyrant_la_LIBADD = libdfsch.la -ltokyocabinet -ltokyotyrant
libdfsch_tokyo_tyrant_la_LDFLAGS = -no-undefined

tokyo_tyrant_la_SOURCES = lib/tokyo-tyrant_mod.c
tokyo_tyrant_la_LIBADD = libdfsch.la libdfsch-tokyo-tyrant.la
tokyo_tyrant_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined

if WIN32
gui_flags = -mwindows
else
gui_flags =
endif

dfsch_run_SOURCES = src/run.c
dfsch_run_LDADD = libdfsch.la libdfsch-cmdopts.la
dfsch_run_LDFLAGS = $(gui_flags)

dfsch_source_tool_SOURCES = src/source-tool.c
dfsch_source_tool_LDADD = -lz

check_PROGRAMS = platform-test c-api-test json-parser-test

platform_test_SOURCES = tests/platform-test.c tests/test-macros.h
platform_test_LDADD =  -lgc $(pthread_libadd) -lm libdfsch-cmdopts.la
c_api_test_SOURCES = tests/c-api-test.c tests/test-macros.h
c_api_test_LDADD = libdfsch.la libdfsch-cmdopts.la

json_parser_test_SOURCES = tests/json-parser-test.c
json_parser_test_LDADD = libdfsch.la libdfsch-json.la

TESTS = tests/scm-test.sh platform-test c-api-test

dist-hook:
	$(srcdir)/git-make-stamp.sh $(distdir)/snapshot.stamp

version.h:
	$(srcdir)/make-version-h.sh $(srcdir)/snapshot.stamp

