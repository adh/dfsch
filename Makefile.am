
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
	dfsch/lib/shtml.h \
	dfsch/lib/json.h \
        dfsch/lib/socket-port.h \
        dfsch/lib/http.h \
	dfsch/lib/csv.h \
	dfsch/lib/ini-file.h \
	dfsch/lib/zlib.h \
	dfsch/lib/tk-gui.h \
	dfsch/lib/pcre.h \
	dfsch/lib/crypto.h \
	dfsch/lib/tokyo-cabinet.h \
	dfsch/lib/tokyo-tyrant.h \
	dfsch/lib/gd.h \
	dfsch/lib/ffi.h \
	dfsch/lib/curl.h \
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

if GD
gd_libs=libdfsch-gd.la
gd_modules=gd.la
else
gd_libs=
gd_modules=
endif

if FFI
ffi_libs=libdfsch-ffi.la
ffi_modules=ffi.la
else
ffi_libs=
ffi_modules=
endif

if CURL
curl_libs=libdfsch-curl.la
curl_modules=curl.la
else
curl_libs=
curl_modules=
endif

if RRD
rrd_modules=rrd.la
else
rrd_modules=
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

if DB_SQLITE
db_sqlite_modules=db-sqlite.la
else
db_sqlite_modules=
endif

if DBI
dbi_modules=dbi.la
else
dbi_modules=
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
	libdfsch-shtml.la \
	libdfsch-json.la \
	libdfsch-zlib.la \
	libdfsch-csv.la \
	libdfsch-ini-file.la \
	libdfsch-crypto.la \
	libdfsch-minizip.la \
	$(nonwin32_libs)\
	$(tkgui_libs)\
	$(pcre_libs)\
	$(tc_libs) \
	$(tt_libs) \
	$(gd_libs) \
	$(ffi_libs) \
	$(curl_libs)



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
	shtml.la \
	json.la \
	os.la \
	zlib.la \
	csv.la \
	ini-file.la \
	crypto.la \
	sql-support.la \
	markdown.la \
	fastlz.la \
	minizip.la \
	$(nonwin32_modules)\
	$(tkgui_modules)\
	$(pcre_modules)\
	$(sqlite_modules)\
	$(sqlite3_modules)\
	$(db_sqlite_modules)\
	$(dbi_modules)\
	$(tc_modules)\
	$(tt_modules)\
	$(gd_modules)\
	$(ffi_modules)\
	$(curl_modules)\
	$(rrd_modules)


dfschlibscm_DATA = lib-scm/stream-functions.scm \
	lib-scm/simple-tests.scm \
	lib-scm/os-utils.scm \
	lib-scm/tk-gui.scm \
	lib-scm/http-server.scm \
	lib-scm/sql.scm \
	lib-scm/dfsch-unit.scm \
	lib-scm/markdown-tools.scm \
	lib-scm/match.scm

man_MANS = doc/dfsch-repl.1

vm_SOURCES = src/dfsch.c dfsch/dfsch.h src/internal.h src/version.c\
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
	src/specializers.c dfsch/specializers.h\
	src/strhash.c dfsch/strhash.h	\
	src/serdes.c dfsch/serdes.h	\
	src/sha256.c dfsch/sha256.h	\
	ext/fastlz/fastlz.c ext/fastlz/fastlz.h \
	std-lib.c sys-lib.c

libdfsch_la_SOURCES = $(vm_SOURCES) version.h


BUILT_SOURCES=udata.c udata.h version.h std-lib.c sys-lib.c
strings.o: udata.h
udata.h: udata-gen$(EXEEXT) $(srcdir)/data/UnicodeData.txt
	./udata-gen$(EXEEXT) header $(srcdir)/data/UnicodeData.txt udata.h
udata.c: udata-gen$(EXEEXT) $(srcdir)/data/UnicodeData.txt
	./udata-gen$(EXEEXT) data $(srcdir)/data/UnicodeData.txt udata.c

stdlib_sources = \
	$(srcdir)/core-lib/std-lib.scm\
	$(srcdir)/core-lib/conditions.scm\
	$(srcdir)/core-lib/objects.scm

syslib_sources = \
	$(srcdir)/core-lib/system.scm

std-lib.c: $(stdlib_sources) boot-source-tool$(EXEEXT)
	./boot-source-tool$(EXEEXT) -c dfsch__std_lib \
	$(stdlib_sources) -o std-lib.c

sys-lib.c: $(syslib_sources) boot-source-tool$(EXEEXT)
	./boot-source-tool$(EXEEXT) -c dfsch__sys_lib \
	$(syslib_sources) -o sys-lib.c

CLEANFILES=udata.c udata.h udata-gen$(EXEEXT) version.h std-lib.c boot-source-tool$(EXEEXT) sys-lib.c

udata-gen$(EXEEXT): $(srcdir)/src/udata-gen.c 
	$(CC_FOR_BUILD) -I$(srcdir) -o udata-gen$(EXEEXT) $(srcdir)/src/udata-gen.c

boot-source-tool$(EXEEXT): $(srcdir)/src/source-tool.c
	$(CC_FOR_BUILD) -I$(srcdir) -o boot-source-tool$(EXEEXT) -lz -lgc \
	-DPACKAGE_VERSION=\"\" $(srcdir)/src/source-tool.c

fastlz_files = 	ext/fastlz/LICENSE \
	ext/fastlz/README.TXT \
	ext/fastlz/6pack.c \
	ext/fastlz/6unpack.c

upskirt_files = ext/upskirt/array.c		  ext/upskirt/kilt.c \
	ext/upskirt/array.h		  ext/upskirt/lace.c \
	ext/upskirt/benchmark.c		  ext/upskirt/LICENSE \
	ext/upskirt/BSDmakefile		  ext/upskirt/markdown.c \
	ext/upskirt/buffer.c		  ext/upskirt/markdown.h \
	ext/upskirt/buffer.h		  ext/upskirt/README \
	ext/upskirt/expanded_markdown.sh  ext/upskirt/renderers.c \
	ext/upskirt/GNUmakefile		  ext/upskirt/renderers.h

minizip_files = ext/minizip/minizip.h ext/minizip/minizip.c \
	ext/minizip/ioapi.h ext/minizip/ioapi.c

EXTRA_DIST = tests/scm-test.sh tests/scm-test-interp.sh tests/interp-test.scm \
	doc/dfsch-repl.1\
	src/udata-gen.c data/UnicodeData.txt tools/docgen.scm \
	make-version-h.sh git-make-stamp.sh \
	$(dfschlibscm_DATA) \
	examples/tk-gui-demo.scm \
	examples/http-server-demo.scm \
	$(stdlib_sources) \
	$(syslib_sources) \
	tests/main.scm \
	tests/language-tests.scm \
	tests/r5rs-tests.scm \
	tests/fix-regression-tests.scm \
	tests/compiler-tests.scm \
	$(fastlz_files) \
	$(upskirt_files)


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
unix_la_LIBADD =  libdfsch.la libdfsch-os.la -lcrypt

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

fastlz_la_SOURCES = lib/fastlz_mod.c
fastlz_la_LDFLAGS = -avoid-version -module -shrext .dsl -no-undefined
fastlz_la_LIBADD =  libdfsch.la

libdfsch_minizip_la_SOURCES = lib/minizip.c dfsch/lib/minizip.h \
	ext/minizip/unzip.c ext/minizip/unzip.h \
	ext/minizip/zip.c ext/minizip/zip.h \
	ext/minizip/ioapi.c ext/minizip/ioapi.h
libdfsch_minizip_la_LDFLAGS = -version-info 2:0:0 -no-undefined
libdfsch_minizip_la_LIBADD =  libdfsch.la

minizip_la_SOURCES = lib/minizip_mod.c
minizip_la_LDFLAGS = -avoid-version -module -shrext .dsl -no-undefined
minizip_la_LIBADD =  libdfsch.la libdfsch-minizip.la

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

libdfsch_shtml_la_SOURCES = lib/shtml.c
libdfsch_shtml_la_LIBADD = libdfsch.la libdfsch-inet.la
libdfsch_shtml_la_LDFLAGS = -version-info 1:0:0 -no-undefined

shtml_la_SOURCES = lib/shtml_mod.c
shtml_la_LIBADD = libdfsch.la libdfsch-shtml.la
shtml_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined

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

libdfsch_ini_file_la_SOURCES = lib/ini-file.c
libdfsch_ini_file_la_LIBADD = libdfsch.la
libdfsch_ini_file_la_LDFLAGS = -no-undefined

ini_file_la_SOURCES = lib/ini-file_mod.c
ini_file_la_LIBADD = libdfsch.la libdfsch-ini-file.la
ini_file_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined


libdfsch_tk_gui_la_SOURCES = lib/tk-gui.c
libdfsch_tk_gui_la_LIBADD = libdfsch.la -ltcl -ltk
libdfsch_tk_gui_la_CFLAGS = -I/usr/include/tcl
libdfsch_tk_gui_la_LDFLAGS = -no-undefined

tk_gui_interface_la_SOURCES = lib/tk-gui_mod.c
tk_gui_interface_la_CFLAGS = -I/usr/include/tcl
tk_gui_interface_la_LIBADD = libdfsch.la libdfsch-tk-gui.la -ltcl -ltk
tk_gui_interface_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined

libdfsch_crypto_la_SOURCES = lib/crypto/crypto.c \
	lib/crypto/internal.h \
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
	lib/crypto/macros.h \
	lib/crypto/fe25519.h lib/crypto/fe25519.c \
	lib/crypto/ge25519.h lib/crypto/ge25519.c \
	lib/crypto/sc25519.h lib/crypto/sc25519.c \
	lib/crypto/sign25519.c \
	lib/crypto/rc4.c

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

db_sqlite_la_SOURCES = lib/sql/sqlite3.c
db_sqlite_la_LIBADD = libdfsch.la -ldb_sql
db_sqlite_la_CFLAGS = -DBERKELEY_DB
db_sqlite_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined

dbi_la_SOURCES = lib/sql/dbi.c
dbi_la_LIBADD = libdfsch.la -ldbi
dbi_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined

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
libdfsch_tokyo_tyrant_la_LIBADD = libdfsch.la libdfsch-tokyo-cabinet.la \
	-ltokyocabinet -ltokyotyrant
libdfsch_tokyo_tyrant_la_LDFLAGS = -no-undefined

tokyo_tyrant_la_SOURCES = lib/tokyo-tyrant_mod.c
tokyo_tyrant_la_LIBADD = libdfsch.la libdfsch-tokyo-tyrant.la
tokyo_tyrant_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined

libdfsch_gd_la_SOURCES = lib/gd.c
libdfsch_gd_la_LIBADD = libdfsch.la -lgd
libdfsch_gd_la_LDFLAGS = -no-undefined

gd_la_SOURCES = lib/gd_mod.c
gd_la_LIBADD = libdfsch-gd.la libdfsch.la
gd_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined

libdfsch_ffi_la_SOURCES = lib/ffi.c
libdfsch_ffi_la_LIBADD = libdfsch.la -lffi
libdfsch_ffi_la_LDFLAGS = -no-undefined

ffi_la_SOURCES = lib/ffi_mod.c
ffi_la_LIBADD = libdfsch-ffi.la libdfsch.la
ffi_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined


markdown_la_SOURCES = lib/markdown_mod.c \
	ext/upskirt/markdown.c \
	ext/upskirt/markdown.h \
	ext/upskirt/array.c \
	ext/upskirt/buffer.c \
	ext/upskirt/array.h \
	ext/upskirt/buffer.h \
	ext/upskirt/renderers.h \
	ext/upskirt/renderers.c
markdown_la_LIBADD = libdfsch.la
markdown_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined

libdfsch_curl_la_SOURCES = lib/curl.c
libdfsch_curl_la_LIBADD = libdfsch.la -lcurl
libdfsch_curl_la_LDFLAGS = -no-undefined

curl_la_SOURCES = lib/curl_mod.c
curl_la_LIBADD = libdfsch-curl.la libdfsch.la
curl_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined

rrd_la_SOURCES = lib/rrd_mod.c
rrd_la_LIBADD = libdfsch.la
rrd_la_LDFLAGS = -module -shrext .dsl -avoid-version -no-undefined -lrrd


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

TESTS = tests/scm-test.sh tests/scm-test-interp.sh

dist-hook:
	$(srcdir)/git-make-stamp.sh $(distdir)/snapshot.stamp

version.h: $(vm_SOURCES) $(stdlib_sources) $(syslib_sources)
	$(srcdir)/make-version-h.sh $(srcdir)/snapshot.stamp

.PHONY: documentation

MODULES = cmdopts crypto curl rrd csv extref fastlz ffi gcollect gd http inet \
	ini-file \
	json markdown minizip os pcre posix-regex process shtml socket-port \
	sxml threads tokyo-cabinet tokyo-tyrant unix xml zlib \
	\
	sqlite sqlite3 dbi\
	\
	dfsch-unit http-server markdown-tools match tk-gui

documentation: all
	./dfsch-run -L ./.libs -L $(srcdir)/lib-scm \
		$(srcdir)/tools/docgen.scm --chapters $(srcdir)/doc/manual.md \
		--notes $(srcdir)/doc/notes.md \
		documentation
	$(srcdir)/gen-module-docs.sh $(srcdir) "$(MODULES)"