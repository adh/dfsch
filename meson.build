project('dfsch', 'c',
  version: '0.4.1-dev',
  meson_version: '>=0.63.0',
  default_options: ['warning_level=1', 'c_std=gnu89'])

cc = meson.get_compiler('c')
cc_native = meson.get_compiler('c', native: true)
fs = import('fs')

is_windows = host_machine.system() == 'windows'

pkgconfig_dep = dependency('pkgconfig', required: false)

gc_dep = dependency('bdw-gc', required: false)
if not gc_dep.found()
  gc_dep = cc.find_library('gc', required: true)
endif
add_project_dependencies(gc_dep, language: 'c')

gc_dep_native = dependency('bdw-gc', required: false, native: true)
if not gc_dep_native.found()
  gc_dep_native = cc_native.find_library('gc', required: true)
endif

threads_dep = dependency('threads')
z_dep = dependency('zlib', required: true)
expat_dep = dependency('expat', required: true)
z_dep_native = dependency('zlib', required: true, native: true)

readline_dep = dependency('readline', required: false)
use_readline = false
if get_option('readline').enabled()
  if not readline_dep.found()
    error('readline was explicitly enabled but not found')
  endif
  use_readline = true
elif get_option('readline').auto() and readline_dep.found()
  use_readline = true
endif

m_dep = cc.find_library('m', required: false)
dl_dep = cc.find_library('dl', required: false)

prefix = get_option('prefix')
dfschlibexecdir = join_paths(prefix, get_option('libdir'), 'dfsch')
dfschlibscmdir = join_paths(prefix, get_option('datadir'), 'dfsch', 'scm')

triplet = '@0@-unknown-@1@'.format(host_machine.cpu_family(), host_machine.system())

cdata = configuration_data()
cdata.set('HAVE_STDBOOL_H', 1)
cdata.set('HAVE_STDINT_H', 1)
cdata.set('HAVE_STDLIB_H', 1)
cdata.set('HAVE_STDIO_H', 1)
cdata.set('HAVE_STRING_H', 1)
cdata.set('HAVE_STRINGS_H', 1)
cdata.set('HAVE_SYS_TYPES_H', 1)
cdata.set('HAVE_SYS_STAT_H', 1)
cdata.set('HAVE_INTTYPES_H', 1)
cdata.set('STDC_HEADERS', 1)
cdata.set('HAVE_GC_GC_H', 1)
cdata.set('HAVE_LIBGC', 1)
cdata.set('HAVE_ZLIB_H', 1)
cdata.set('HAVE_EXPAT_H', 1)
if use_readline
  cdata.set('USE_READLINE', 1)
endif
cdata.set_quoted('HOST_TRIPLET', triplet)
cdata.set_quoted('DFSCH_LIB_SCM_DIR', dfschlibscmdir)
cdata.set_quoted('DFSCH_LIB_SO_DIR', dfschlibexecdir)
cdata.set_quoted('PACKAGE', meson.project_name())
cdata.set_quoted('PACKAGE_NAME', meson.project_name())
cdata.set_quoted('PACKAGE_TARNAME', meson.project_name())
cdata.set_quoted('PACKAGE_VERSION', meson.project_version())
cdata.set_quoted('PACKAGE_STRING', '@0@ @1@'.format(meson.project_name(), meson.project_version()))
cdata.set_quoted('PACKAGE_BUGREPORT', '')
cdata.set_quoted('PACKAGE_URL', '')
cdata.set_quoted('VERSION', meson.project_version())
cdata.set('_FILE_OFFSET_BITS', 64)
configure_file(output: 'config.h', configuration: cdata)

version_h = vcs_tag(
  input: 'version.h.in',
  output: 'version.h',
  fallback: 'snapshot',
  command: ['git', 'describe', '--always', '--dirty'])

add_project_arguments(
  '-DHAVE_CONFIG_H',
  '-D_REENTRANT',
  '-DGC_THREADS',
  language: 'c')

if host_machine.system() == 'darwin'
  add_project_arguments(
    '-Wno-error=implicit-function-declaration',
    '-Wno-incompatible-function-pointer-types',
    '-Wno-int-conversion',
    language: 'c')
endif

inc_root = include_directories('.', 'src', 'ext/minizip', 'ext/upskirt')

unicode_data = files('data/UnicodeData.txt')

udata_gen = executable('udata-gen', 'src/udata-gen.c',
  native: true,
  include_directories: inc_root)

udata_h = custom_target('udata-header',
  output: 'udata.h',
  input: unicode_data,
  command: [udata_gen, 'header', '@INPUT@', '@OUTPUT@'])

udata_c = custom_target('udata-source',
  output: 'udata.c',
  input: unicode_data,
  depends: [udata_h],
  command: [udata_gen, 'data', '@INPUT@', '@OUTPUT@'])

boot_source_tool = executable('boot-source-tool', 'src/source-tool.c',
  native: true,
  include_directories: inc_root,
  c_args: ['-DPACKAGE_VERSION=""'],
  dependencies: [gc_dep_native, z_dep_native])

stdlib_sources = files(
  'core-lib/std-lib.scm',
  'core-lib/conditions.scm',
  'core-lib/objects.scm')

syslib_sources = files('core-lib/system.scm')

std_lib_c = custom_target('std-lib-c',
  output: 'std-lib.c',
  input: stdlib_sources,
  command: [boot_source_tool, '-c', 'dfsch__std_lib', '-o', '@OUTPUT@', '@INPUT@'])

sys_lib_c = custom_target('sys-lib-c',
  output: 'sys-lib.c',
  input: syslib_sources,
  command: [boot_source_tool, '-c', 'dfsch__sys_lib', '-o', '@OUTPUT@', '@INPUT@'])

core_deps = [gc_dep, threads_dep, z_dep]
if m_dep.found()
  core_deps += [m_dep]
endif
if dl_dep.found()
  core_deps += [dl_dep]
endif

vm_sources = [
  version_h,
  'src/dfsch.c',
  'src/version.c',
  'src/types.c',
  'src/package.c',
  'src/parse.c',
  'src/primitives.c',
  'src/native_cxr.c',
  'src/forms.c',
  'src/system.c',
  'src/macros.c',
  'src/util.c',
  'src/hash.c',
  'src/number.c',
  'src/bignum.c',
  'src/strings.c',
  'src/object.c',
  'src/format.c',
  'src/ports.c',
  'src/load.c',
  'src/conditions.c',
  'src/introspect.c',
  'src/random.c',
  'src/writer.c',
  'src/eqhash.c',
  'src/generic.c',
  'src/weak.c',
  'src/mkhash.c',
  'src/compiler.c',
  'src/backquote.c',
  'src/specializers.c',
  'src/strhash.c',
  'src/serdes.c',
  'src/sha256.c',
  'ext/fastlz/fastlz.c',
  udata_h,
  udata_c,
  std_lib_c,
  sys_lib_c,
]

libdfsch = shared_library('dfsch', vm_sources,
  include_directories: inc_root,
  dependencies: core_deps,
  version: '5.0.0',
  soversion: '5',
  install: true)

mod_install_dir = join_paths(get_option('libdir'), 'dfsch')

console_lib_deps = [libdfsch]
if use_readline
  console_lib_deps += [readline_dep]
endif

libdfsch_console = shared_library('dfsch-console', 'lib/console.c',
  include_directories: inc_root,
  link_with: [libdfsch],
  dependencies: use_readline ? [readline_dep] : [],
  install: true)

console_mod = shared_module('console', 'lib/console_mod.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_console],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

libdfsch_cdebug = shared_library('dfsch-cdebug', 'lib/cdebug.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_console],
  dependencies: [threads_dep],
  install: true)

shared_module('cdebug', 'lib/cdebug_mod.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_cdebug],
  dependencies: [threads_dep],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

libdfsch_cinspect = shared_library('dfsch-cinspect', 'lib/cinspect.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_console],
  dependencies: [threads_dep],
  install: true)

shared_module('cinspect', 'lib/cinspect_mod.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_cinspect],
  dependencies: [threads_dep],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

libdfsch_threads = shared_library('dfsch-threads', 'lib/threads.c',
  include_directories: inc_root,
  link_with: [libdfsch],
  dependencies: [threads_dep],
  install: true)

shared_module('threads', 'lib/threads_mod.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_threads],
  dependencies: [threads_dep],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

libdfsch_os = shared_library('dfsch-os', 'lib/os.c',
  include_directories: inc_root,
  link_with: [libdfsch],
  install: true)

os_mod = shared_module('os', 'lib/os_mod.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_os],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

libdfsch_zlib = shared_library('dfsch-zlib', 'lib/zlib.c',
  include_directories: inc_root,
  link_with: [libdfsch],
  dependencies: [z_dep],
  install: true)

shared_module('zlib', 'lib/zlib_mod.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_zlib],
  dependencies: [z_dep],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

shared_module('fastlz', 'lib/fastlz_mod.c',
  include_directories: inc_root,
  link_with: [libdfsch],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

libdfsch_minizip = shared_library('dfsch-minizip',
  ['lib/minizip.c', 'ext/minizip/unzip.c', 'ext/minizip/zip.c', 'ext/minizip/ioapi.c'],
  include_directories: inc_root,
  link_with: [libdfsch],
  dependencies: [z_dep],
  install: true)

shared_module('minizip', 'lib/minizip_mod.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_minizip],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

libdfsch_inet = shared_library('dfsch-inet', 'lib/inet.c',
  include_directories: inc_root,
  link_with: [libdfsch],
  install: true)

shared_module('inet', 'lib/inet_mod.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_inet],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

gcollect_mod = shared_module('gcollect', 'lib/gcollect.c',
  include_directories: inc_root,
  link_with: [libdfsch],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

libdfsch_extref = shared_library('dfsch-extref', 'lib/extref.c',
  include_directories: inc_root,
  link_with: [libdfsch],
  dependencies: [threads_dep],
  install: true)

shared_module('extref', 'lib/extref_mod.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_extref],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

libdfsch_collections = shared_library('dfsch-collections', 'lib/collections.c',
  include_directories: inc_root,
  link_with: [libdfsch],
  install: true)

shared_module('collections', 'lib/collections_mod.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_collections],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

libdfsch_process = shared_library('dfsch-process', 'lib/process.c',
  include_directories: inc_root,
  link_with: [libdfsch],
  install: true)

shared_module('process', 'lib/process_mod.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_process],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

libdfsch_cmdopts = shared_library('dfsch-cmdopts', 'lib/cmdopts.c',
  include_directories: inc_root,
  link_with: [libdfsch],
  install: true)

cmdopts_mod = shared_module('cmdopts', 'lib/cmdopts_mod.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_cmdopts],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

libdfsch_xml = shared_library('dfsch-xml', 'lib/xml.c',
  include_directories: inc_root,
  link_with: [libdfsch],
  dependencies: [expat_dep],
  install: true)

shared_module('xml', 'lib/xml_mod.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_xml],
  dependencies: [expat_dep],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

libdfsch_sxml = shared_library('dfsch-sxml', 'lib/sxml.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_xml, libdfsch_inet],
  dependencies: [expat_dep],
  install: true)

shared_module('sxml', 'lib/sxml_mod.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_sxml],
  dependencies: [expat_dep],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

libdfsch_shtml = shared_library('dfsch-shtml', 'lib/shtml.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_inet],
  install: true)

shared_module('shtml', 'lib/shtml_mod.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_shtml],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

libdfsch_json = shared_library('dfsch-json', 'lib/json.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_inet],
  install: true)

shared_module('json', 'lib/json_mod.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_json],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

if not is_windows
  libdfsch_posix_regex = shared_library('dfsch-posix-regex', 'lib/posix-regex.c',
    include_directories: inc_root,
    link_with: [libdfsch],
    install: true)

  shared_module('posix-regex', 'lib/posix-regex_mod.c',
    include_directories: inc_root,
    link_with: [libdfsch, libdfsch_posix_regex],
    name_prefix: '',
    name_suffix: 'dsl',
    install: true,
    install_dir: mod_install_dir)

  shared_module('unix', 'lib/unix.c',
    include_directories: inc_root,
    link_with: [libdfsch, libdfsch_os],
    name_prefix: '',
    name_suffix: 'dsl',
    install: true,
    install_dir: mod_install_dir)

  libdfsch_socket_port = shared_library('dfsch-socket-port', 'lib/socket-port.c',
    include_directories: inc_root,
    link_with: [libdfsch],
    install: true)

  shared_module('socket-port', 'lib/socket-port_mod.c',
    include_directories: inc_root,
    link_with: [libdfsch, libdfsch_socket_port],
    name_prefix: '',
    name_suffix: 'dsl',
    install: true,
    install_dir: mod_install_dir)

  libdfsch_http = shared_library('dfsch-http', 'lib/http.c',
    include_directories: inc_root,
    link_with: [libdfsch, libdfsch_inet],
    dependencies: [expat_dep],
    install: true)

  shared_module('http', 'lib/http_mod.c',
    include_directories: inc_root,
    link_with: [libdfsch, libdfsch_http],
    name_prefix: '',
    name_suffix: 'dsl',
    install: true,
    install_dir: mod_install_dir)
endif

libdfsch_csv = shared_library('dfsch-csv', 'lib/csv.c',
  include_directories: inc_root,
  link_with: [libdfsch],
  install: true)

shared_module('csv', 'lib/csv_mod.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_csv],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

libdfsch_ini_file = shared_library('dfsch-ini-file', 'lib/ini-file.c',
  include_directories: inc_root,
  link_with: [libdfsch],
  install: true)

shared_module('ini-file', 'lib/ini-file_mod.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_ini_file],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

libdfsch_crypto = shared_library('dfsch-crypto',
  [
    'lib/crypto/crypto.c',
    'lib/crypto/modes.c',
    'lib/crypto/aes.c',
    'lib/crypto/xtea.c',
    'lib/crypto/blowfish.c',
    'lib/crypto/hmac.c',
    'lib/crypto/sha256-desc.c',
    'lib/crypto/sha512.c',
    'lib/crypto/sha1.c',
    'lib/crypto/md5.c',
    'lib/crypto/md4.c',
    'lib/crypto/curve25519.c',
    'lib/crypto/rsa.c',
    'lib/crypto/rsa-pad.c',
    'lib/crypto/salsa20.c',
    'lib/crypto/prng.c',
    'lib/crypto/fe25519.c',
    'lib/crypto/ge25519.c',
    'lib/crypto/sc25519.c',
    'lib/crypto/sign25519.c',
    'lib/crypto/rc4.c',
  ],
  include_directories: inc_root,
  link_with: [libdfsch],
  dependencies: is_windows ? [cc.find_library('rpcrt4', required: true)] : [],
  install: true)

shared_module('crypto', 'lib/crypto/module.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_crypto],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

shared_module('sql-support', 'lib/sql/sql-support.c',
  include_directories: inc_root,
  link_with: [libdfsch],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

shared_module('markdown',
  [
    'lib/markdown_mod.c',
    'ext/upskirt/markdown.c',
    'ext/upskirt/array.c',
    'ext/upskirt/buffer.c',
    'ext/upskirt/renderers.c',
  ],
  include_directories: inc_root,
  link_with: [libdfsch],
  name_prefix: '',
  name_suffix: 'dsl',
  install: true,
  install_dir: mod_install_dir)

pcre_dep = cc.find_library('pcre', required: false)
pcre_extra_inc = []
if fs.exists('/opt/homebrew/include/pcre.h')
  pcre_extra_inc += include_directories('/opt/homebrew/include')
endif
have_pcre = pcre_dep.found() and cc.has_header('pcre.h', include_directories: pcre_extra_inc)
if get_option('pcre').enabled() and not have_pcre
  error('pcre option enabled but working libpcre headers and library were not found')
endif
if (get_option('pcre').enabled() and have_pcre) or (get_option('pcre').auto() and have_pcre)
  libdfsch_pcre = shared_library('dfsch-pcre', 'lib/pcre.c',
    include_directories: [inc_root, pcre_extra_inc],
    link_with: [libdfsch],
    dependencies: [pcre_dep],
    install: true)

  shared_module('pcre', 'lib/pcre_mod.c',
    include_directories: [inc_root, pcre_extra_inc],
    link_with: [libdfsch, libdfsch_pcre],
    dependencies: [pcre_dep],
    name_prefix: '',
    name_suffix: 'dsl',
    install: true,
    install_dir: mod_install_dir)
endif

tcl_dep = cc.find_library('tcl', required: false)
tk_dep = cc.find_library('tk', required: false)
tcl_inc_opt = get_option('tcl_include')
tk_inc_opt = get_option('tk_include')
tk_extra_inc = []
if tcl_inc_opt != ''
  if fs.exists(tcl_inc_opt)
    tk_extra_inc += include_directories(tcl_inc_opt)
  endif
endif
if tk_inc_opt != ''
  if fs.exists(tk_inc_opt)
    tk_extra_inc += include_directories(tk_inc_opt)
  endif
endif
if tcl_inc_opt == '' and fs.exists('/opt/homebrew/include/tcl-tk')
  tk_extra_inc += include_directories('/opt/homebrew/include/tcl-tk')
endif
if tk_inc_opt == '' and fs.exists('/opt/homebrew/include/tcl-tk')
  tk_extra_inc += include_directories('/opt/homebrew/include/tcl-tk')
endif
have_tk = tcl_dep.found() and tk_dep.found() and cc.has_header('tcl.h', include_directories: tk_extra_inc) and cc.has_header('tk.h', include_directories: tk_extra_inc)
if get_option('tk_gui').enabled() and not have_tk
  error('tk_gui enabled but working tcl/tk headers and libraries were not found')
endif
if (get_option('tk_gui').enabled() and have_tk) or (get_option('tk_gui').auto() and have_tk)
  libdfsch_tk_gui = shared_library('dfsch-tk-gui', 'lib/tk-gui.c',
    include_directories: [inc_root, tk_extra_inc],
    link_with: [libdfsch],
    dependencies: [tcl_dep, tk_dep],
    install: true)

  shared_module('tk-gui-interface', 'lib/tk-gui_mod.c',
    include_directories: [inc_root, tk_extra_inc],
    link_with: [libdfsch, libdfsch_tk_gui],
    dependencies: [tcl_dep, tk_dep],
    name_prefix: '',
    name_suffix: 'dsl',
    install: true,
    install_dir: mod_install_dir)
endif

sqlite_dep = cc.find_library('sqlite', required: false)
if get_option('sqlite').enabled() and not sqlite_dep.found()
  error('sqlite option enabled but libsqlite not found')
endif
if (get_option('sqlite').enabled() and sqlite_dep.found()) or (get_option('sqlite').auto() and sqlite_dep.found())
  shared_module('sqlite', 'lib/sql/sqlite.c',
    include_directories: inc_root,
    link_with: [libdfsch],
    dependencies: [sqlite_dep],
    name_prefix: '',
    name_suffix: 'dsl',
    install: true,
    install_dir: mod_install_dir)
endif

sqlite3_dep = cc.find_library('sqlite3', required: false)
if get_option('sqlite3').enabled() and not sqlite3_dep.found()
  error('sqlite3 option enabled but libsqlite3 not found')
endif
if (get_option('sqlite3').enabled() and sqlite3_dep.found()) or (get_option('sqlite3').auto() and sqlite3_dep.found())
  shared_module('sqlite3', 'lib/sql/sqlite3.c',
    include_directories: inc_root,
    link_with: [libdfsch],
    dependencies: [sqlite3_dep],
    name_prefix: '',
    name_suffix: 'dsl',
    install: true,
    install_dir: mod_install_dir)
endif

db_sql_dep = cc.find_library('db_sql', required: false)
if get_option('db_sqlite').enabled() and not db_sql_dep.found()
  error('db_sqlite option enabled but libdb_sql not found')
endif
if (get_option('db_sqlite').enabled() and db_sql_dep.found()) or (get_option('db_sqlite').auto() and db_sql_dep.found())
  shared_module('db-sqlite', 'lib/sql/sqlite3.c',
    include_directories: inc_root,
    c_args: ['-DBERKELEY_DB'],
    link_with: [libdfsch],
    dependencies: [db_sql_dep],
    name_prefix: '',
    name_suffix: 'dsl',
    install: true,
    install_dir: mod_install_dir)
endif

dbi_dep = dependency('dbi', required: false)
if get_option('dbi').enabled() and not dbi_dep.found()
  error('dbi option enabled but libdbi not found')
endif
if (get_option('dbi').enabled() and dbi_dep.found()) or (get_option('dbi').auto() and dbi_dep.found())
  shared_module('dbi', 'lib/sql/dbi.c',
    include_directories: inc_root,
    link_with: [libdfsch],
    dependencies: [dbi_dep],
    name_prefix: '',
    name_suffix: 'dsl',
    install: true,
    install_dir: mod_install_dir)
endif

tokyocabinet_dep = cc.find_library('tokyocabinet', required: false)
tokyotyrant_dep = cc.find_library('tokyotyrant', required: false)
if get_option('tokyo_cabinet').enabled() and not tokyocabinet_dep.found()
  error('tokyo_cabinet enabled but libtokyocabinet not found')
endif
if (get_option('tokyo_cabinet').enabled() and tokyocabinet_dep.found()) or (get_option('tokyo_cabinet').auto() and tokyocabinet_dep.found())
  tc_deps = [tokyocabinet_dep]
  if tokyotyrant_dep.found()
    tc_deps += [tokyotyrant_dep]
  endif
  libdfsch_tokyo_cabinet = shared_library('dfsch-tokyo-cabinet', 'lib/tokyo-cabinet.c',
    include_directories: inc_root,
    link_with: [libdfsch],
    dependencies: tc_deps,
    install: true)

  shared_module('tokyo-cabinet', 'lib/tokyo-cabinet_mod.c',
    include_directories: inc_root,
    link_with: [libdfsch, libdfsch_tokyo_cabinet],
    name_prefix: '',
    name_suffix: 'dsl',
    install: true,
    install_dir: mod_install_dir)
endif

if get_option('tokyo_tyrant').enabled() and not (tokyotyrant_dep.found() and tokyocabinet_dep.found())
  error('tokyo_tyrant enabled but required libraries not found')
endif
if (get_option('tokyo_tyrant').enabled() and tokyotyrant_dep.found() and tokyocabinet_dep.found()) or (get_option('tokyo_tyrant').auto() and tokyotyrant_dep.found() and tokyocabinet_dep.found())
  libdfsch_tokyo_tyrant = shared_library('dfsch-tokyo-tyrant', 'lib/tokyo-tyrant.c',
    include_directories: inc_root,
    link_with: [libdfsch, libdfsch_tokyo_cabinet],
    dependencies: [tokyocabinet_dep, tokyotyrant_dep],
    install: true)

  shared_module('tokyo-tyrant', 'lib/tokyo-tyrant_mod.c',
    include_directories: inc_root,
    link_with: [libdfsch, libdfsch_tokyo_tyrant],
    name_prefix: '',
    name_suffix: 'dsl',
    install: true,
    install_dir: mod_install_dir)
endif

gd_dep = cc.find_library('gd', required: false)
if get_option('gd').enabled() and not gd_dep.found()
  error('gd enabled but libgd not found')
endif
if (get_option('gd').enabled() and gd_dep.found()) or (get_option('gd').auto() and gd_dep.found())
  libdfsch_gd = shared_library('dfsch-gd', 'lib/gd.c',
    include_directories: inc_root,
    link_with: [libdfsch],
    dependencies: [gd_dep],
    install: true)

  shared_module('gd', 'lib/gd_mod.c',
    include_directories: inc_root,
    link_with: [libdfsch, libdfsch_gd],
    name_prefix: '',
    name_suffix: 'dsl',
    install: true,
    install_dir: mod_install_dir)
endif

ffi_dep = dependency('libffi', required: false)
if not ffi_dep.found()
  ffi_dep = cc.find_library('ffi', required: false)
endif
ffi_extra_inc = []
if fs.exists('/opt/homebrew/opt/libffi/include/ffi.h')
  ffi_extra_inc += include_directories('/opt/homebrew/opt/libffi/include')
endif
have_ffi = ffi_dep.found() and cc.has_header('ffi.h', include_directories: ffi_extra_inc)
if get_option('ffi').enabled() and not have_ffi
  error('ffi enabled but working libffi headers and library were not found')
endif
if (get_option('ffi').enabled() and have_ffi) or (get_option('ffi').auto() and have_ffi)
  libdfsch_ffi = shared_library('dfsch-ffi', 'lib/ffi.c',
    include_directories: [inc_root, ffi_extra_inc],
    link_with: [libdfsch],
    dependencies: [ffi_dep],
    install: true)

  shared_module('ffi', 'lib/ffi_mod.c',
    include_directories: [inc_root, ffi_extra_inc],
    link_with: [libdfsch, libdfsch_ffi],
    dependencies: [ffi_dep],
    name_prefix: '',
    name_suffix: 'dsl',
    install: true,
    install_dir: mod_install_dir)
endif

curl_dep = dependency('libcurl', required: false)
if not curl_dep.found()
  curl_dep = cc.find_library('curl', required: false)
endif
if get_option('curl').enabled() and not curl_dep.found()
  error('curl enabled but libcurl not found')
endif
if (get_option('curl').enabled() and curl_dep.found()) or (get_option('curl').auto() and curl_dep.found())
  libdfsch_curl = shared_library('dfsch-curl', 'lib/curl.c',
    include_directories: inc_root,
    link_with: [libdfsch],
    dependencies: [curl_dep],
    install: true)

  shared_module('curl', 'lib/curl_mod.c',
    include_directories: inc_root,
    link_with: [libdfsch, libdfsch_curl],
    name_prefix: '',
    name_suffix: 'dsl',
    install: true,
    install_dir: mod_install_dir)
endif

rrd_dep = cc.find_library('rrd', required: false)
if get_option('rrd').enabled() and not rrd_dep.found()
  error('rrd enabled but librrd not found')
endif
if (get_option('rrd').enabled() and rrd_dep.found()) or (get_option('rrd').auto() and rrd_dep.found())
  shared_module('rrd', 'lib/rrd_mod.c',
    include_directories: inc_root,
    link_with: [libdfsch],
    dependencies: [rrd_dep],
    name_prefix: '',
    name_suffix: 'dsl',
    install: true,
    install_dir: mod_install_dir)
endif

dfsch_repl = executable('dfsch-repl', 'src/repl.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_cdebug, libdfsch_console, libdfsch_cmdopts, libdfsch_cinspect],
  install: true)

executable('dfsch-run', 'src/run.c',
  include_directories: inc_root,
  link_with: [libdfsch, libdfsch_cmdopts],
  link_args: is_windows ? ['-mwindows'] : [],
  install: true)

executable('dfsch-source-tool', 'src/source-tool.c',
  include_directories: inc_root,
  dependencies: [z_dep],
  install: true)

install_subdir('dfsch', install_dir: get_option('includedir'))

install_data([
  'lib-scm/stream-functions.scm',
  'lib-scm/simple-tests.scm',
  'lib-scm/os-utils.scm',
  'lib-scm/tk-gui.scm',
  'lib-scm/http-server.scm',
  'lib-scm/sql.scm',
  'lib-scm/dfsch-unit.scm',
  'lib-scm/markdown-tools.scm',
  'lib-scm/match.scm',
], install_dir: join_paths(get_option('datadir'), 'dfsch', 'scm'))

install_man('doc/dfsch-repl.1')

test_harness_module_deps = [cmdopts_mod, os_mod, gcollect_mod]

test('scm-test', dfsch_repl,
  depends: test_harness_module_deps,
  args: ['-L', meson.project_build_root(), '-L', join_paths(meson.project_source_root(), 'lib-scm'), '-L', join_paths(meson.project_source_root(), 'tests'), join_paths(meson.project_source_root(), 'tests', 'main.scm')])

test('scm-test-interp', dfsch_repl,
  depends: test_harness_module_deps,
  args: ['-X-compile_on_apply', '-L', meson.project_build_root(), '-L', join_paths(meson.project_source_root(), 'lib-scm'), '-L', join_paths(meson.project_source_root(), 'tests'), join_paths(meson.project_source_root(), 'tests', 'main.scm')])

summary({
  'readline': use_readline,
  'pcre': pcre_dep.found(),
  'tk_gui': have_tk,
  'sqlite': sqlite_dep.found(),
  'sqlite3': sqlite3_dep.found(),
  'db_sqlite': db_sql_dep.found(),
  'dbi': dbi_dep.found(),
  'tokyo_cabinet': tokyocabinet_dep.found(),
  'tokyo_tyrant': tokyotyrant_dep.found(),
  'gd': gd_dep.found(),
  'ffi': ffi_dep.found(),
  'curl': curl_dep.found(),
  'rrd': rrd_dep.found(),
}, section: 'Optional Features and Modules')
