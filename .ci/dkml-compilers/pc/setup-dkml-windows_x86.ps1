# setup-dkml
#   Short form: sd4
  
<#
.SYNOPSIS

Setup Diskuv OCaml (DKML) compiler on a desktop PC.

.DESCRIPTION

Setup Diskuv OCaml (DKML) compiler on a desktop PC.

.PARAMETER PC_PROJECT_DIR
Context variable for the project directory. Defaults to the current directory.

.PARAMETER FDOPEN_OPAMEXE_BOOTSTRAP
Input variable.

.PARAMETER CACHE_PREFIX
Input variable.

.PARAMETER OCAML_COMPILER
Input variable. -DKML_COMPILER takes priority. If -DKML_COMPILER is not set and -OCAML_COMPILER is set, then the specified OCaml version tag of dkml-compiler (ex. 4.12.1) is used.

.PARAMETER DKML_COMPILER
Input variable. Unspecified or blank is the latest from the default branch (main) of dkml-compiler. @repository@ is the latest from Opam.

.PARAMETER SKIP_OPAM_MODIFICATIONS
Input variable. If true (the default is false) then the opam root and switches will not be created or modified.

.PARAMETER SECONDARY_SWITCH
Input variable. If true then the secondary switch named 'two' is created.

.PARAMETER PRIMARY_SWITCH_SKIP_INSTALL
Input variable. If true no dkml-base-compiler will be installed in the 'dkml' switch.

.PARAMETER CONF_DKML_CROSS_TOOLCHAIN
Input variable. Unspecified or blank is the latest from the default branch (main) of conf-dkml-cross-toolchain. @repository@ is the latest from Opam.

.PARAMETER DISKUV_OPAM_REPOSITORY
Input variable. Defaults to the value of -DEFAULT_DISKUV_OPAM_REPOSITORY_TAG (see below)

.PARAMETER DKML_HOME
Input variables. If specified then DiskuvOCamlHome, DiskuvOCamlBinaryPaths and DiskuvOCamlDeploymentId will be set, in addition to the always-present DiskuvOCamlVarsVersion, DiskuvOCamlVersion
and DiskuvOCamlMSYS2Dir.

# autogen from global_env_vars.
.PARAMETER DKML_VERSION
Environment variable.

.PARAMETER DEFAULT_DISKUV_OPAM_REPOSITORY_TAG
Environment variable.

.PARAMETER DEFAULT_DKML_COMPILER
Environment variable.

.PARAMETER PIN_ASTRING
Environment variable.

.PARAMETER PIN_BASE
Environment variable.

.PARAMETER PIN_BASE64
Environment variable.

.PARAMETER PIN_BIGARRAY_COMPAT
Environment variable.

.PARAMETER PIN_BOS
Environment variable.

.PARAMETER PIN_CAMLP_STREAMS
Environment variable.

.PARAMETER PIN_CHROME_TRACE
Environment variable.

.PARAMETER PIN_CMDLINER
Environment variable.

.PARAMETER PIN_CONF_DKML_SYS_OPAM
Environment variable.

.PARAMETER PIN_CONF_PKG_CONFIG
Environment variable.

.PARAMETER PIN_CONF_SQLITE3
Environment variable.

.PARAMETER PIN_CPPO
Environment variable.

.PARAMETER PIN_CRUNCH
Environment variable.

.PARAMETER PIN_CSEXP
Environment variable.

.PARAMETER PIN_CSTRUCT
Environment variable.

.PARAMETER PIN_CTYPES_FOREIGN
Environment variable.

.PARAMETER PIN_CTYPES
Environment variable.

.PARAMETER PIN_CUDF
Environment variable.

.PARAMETER PIN_DIGESTIF
Environment variable.

.PARAMETER PIN_DISKUVBOX
Environment variable.

.PARAMETER PIN_DKML_APPS
Environment variable.

.PARAMETER PIN_DKML_BASE_COMPILER
Environment variable.

.PARAMETER PIN_DKML_BUILD_DESKTOP
Environment variable.

.PARAMETER PIN_DKML_C_PROBE
Environment variable.

.PARAMETER PIN_DKML_COMPILER_ENV
Environment variable.

.PARAMETER PIN_DKML_COMPILER_SRC
Environment variable.

.PARAMETER PIN_DKML_COMPONENT_COMMON_DESKTOP
Environment variable.

.PARAMETER PIN_DKML_COMPONENT_COMMON_OPAM
Environment variable.

.PARAMETER PIN_DKML_COMPONENT_COMMON_UNIXUTILS
Environment variable.

.PARAMETER PIN_DKML_COMPONENT_OCAMLCOMPILER_COMMON
Environment variable.

.PARAMETER PIN_DKML_COMPONENT_OCAMLCOMPILER_NETWORK
Environment variable.

.PARAMETER PIN_DKML_COMPONENT_OCAMLCOMPILER_OFFLINE
Environment variable.

.PARAMETER PIN_DKML_COMPONENT_OFFLINE_DESKTOP_FULL
Environment variable.

.PARAMETER PIN_DKML_COMPONENT_OFFLINE_OPAMSHIM
Environment variable.

.PARAMETER PIN_DKML_COMPONENT_OFFLINE_UNIXUTILS
Environment variable.

.PARAMETER PIN_DKML_COMPONENT_STAGING_DESKTOP_FULL
Environment variable.

.PARAMETER PIN_DKML_COMPONENT_STAGING_DKMLCONFDIR
Environment variable.

.PARAMETER PIN_DKML_COMPONENT_STAGING_OCAMLRUN
Environment variable.

.PARAMETER PIN_DKML_COMPONENT_STAGING_OPAM32
Environment variable.

.PARAMETER PIN_DKML_COMPONENT_STAGING_OPAM64
Environment variable.

.PARAMETER PIN_DKML_COMPONENT_STAGING_UNIXUTILS
Environment variable.

.PARAMETER PIN_DKML_COMPONENT_STAGING_WITHDKML
Environment variable.

.PARAMETER PIN_DKML_COMPONENT_XX_CONSOLE
Environment variable.

.PARAMETER PIN_DKML_EXE_LIB
Environment variable.

.PARAMETER PIN_DKML_EXE
Environment variable.

.PARAMETER PIN_DKML_INSTALL_INSTALLER
Environment variable.

.PARAMETER PIN_DKML_INSTALL_RUNNER
Environment variable.

.PARAMETER PIN_DKML_INSTALL
Environment variable.

.PARAMETER PIN_DKML_INSTALLER_OCAML_COMMON
Environment variable.

.PARAMETER PIN_DKML_INSTALLER_OCAML_OFFLINE
Environment variable.

.PARAMETER PIN_DKML_PACKAGE_CONSOLE
Environment variable.

.PARAMETER PIN_DKML_RUNTIME_COMMON_NATIVE
Environment variable.

.PARAMETER PIN_DKML_RUNTIME_COMMON
Environment variable.

.PARAMETER PIN_DKML_RUNTIME_DISTRIBUTION
Environment variable.

.PARAMETER PIN_DKML_RUNTIMELIB
Environment variable.

.PARAMETER PIN_DKML_RUNTIMESCRIPTS
Environment variable.

.PARAMETER PIN_DKML_WORKFLOWS
Environment variable.

.PARAMETER PIN_DUNE_ACTION_PLUGIN
Environment variable.

.PARAMETER PIN_DUNE_BUILD_INFO
Environment variable.

.PARAMETER PIN_DUNE_CONFIGURATOR
Environment variable.

.PARAMETER PIN_DUNE_GLOB
Environment variable.

.PARAMETER PIN_DUNE_PRIVATE_LIBS
Environment variable.

.PARAMETER PIN_DUNE_RPC_LWT
Environment variable.

.PARAMETER PIN_DUNE_RPC
Environment variable.

.PARAMETER PIN_DUNE_SITE
Environment variable.

.PARAMETER PIN_DUNE
Environment variable.

.PARAMETER PIN_DYN
Environment variable.

.PARAMETER PIN_EITHER
Environment variable.

.PARAMETER PIN_EQAF
Environment variable.

.PARAMETER PIN_EXTLIB
Environment variable.

.PARAMETER PIN_EZJSONM
Environment variable.

.PARAMETER PIN_FEATHER
Environment variable.

.PARAMETER PIN_FIBER
Environment variable.

.PARAMETER PIN_FIX
Environment variable.

.PARAMETER PIN_FMT
Environment variable.

.PARAMETER PIN_FPATH
Environment variable.

.PARAMETER PIN_GRAPHICS
Environment variable.

.PARAMETER PIN_HEX
Environment variable.

.PARAMETER PIN_INTEGERS
Environment variable.

.PARAMETER PIN_JANE_STREET_HEADERS
Environment variable.

.PARAMETER PIN_JINGOO
Environment variable.

.PARAMETER PIN_JSONM
Environment variable.

.PARAMETER PIN_JSONRPC
Environment variable.

.PARAMETER PIN_JST_CONFIG
Environment variable.

.PARAMETER PIN_LAMBDA_TERM
Environment variable.

.PARAMETER PIN_LOGS
Environment variable.

.PARAMETER PIN_LSP
Environment variable.

.PARAMETER PIN_LWT
Environment variable.

.PARAMETER PIN_LWT_REACT
Environment variable.

.PARAMETER PIN_MCCS
Environment variable.

.PARAMETER PIN_MDX
Environment variable.

.PARAMETER PIN_MENHIR
Environment variable.

.PARAMETER PIN_MENHIRLIB
Environment variable.

.PARAMETER PIN_MENHIRSDK
Environment variable.

.PARAMETER PIN_MERLIN_LIB
Environment variable.

.PARAMETER PIN_METAPP
Environment variable.

.PARAMETER PIN_METAQUOT
Environment variable.

.PARAMETER PIN_MEW
Environment variable.

.PARAMETER PIN_MEW_VI
Environment variable.

.PARAMETER PIN_NUM
Environment variable.

.PARAMETER PIN_OCAML_COMPILER_LIBS
Environment variable.

.PARAMETER PIN_OCAML_LSP_SERVER
Environment variable.

.PARAMETER PIN_OCAML_VERSION
Environment variable.

.PARAMETER PIN_OCAML
Environment variable.

.PARAMETER PIN_OCAMLBUILD
Environment variable.

.PARAMETER PIN_OCAMLC_LOC
Environment variable.

.PARAMETER PIN_OCAMLFIND
Environment variable.

.PARAMETER PIN_OCAMLFORMAT_LIB
Environment variable.

.PARAMETER PIN_OCAMLFORMAT_RPC_LIB
Environment variable.

.PARAMETER PIN_OCAMLFORMAT
Environment variable.

.PARAMETER PIN_OCP_INDENT
Environment variable.

.PARAMETER PIN_OCPLIB_ENDIAN
Environment variable.

.PARAMETER PIN_ODOC_PARSER
Environment variable.

.PARAMETER PIN_ODOC
Environment variable.

.PARAMETER PIN_ORDERING
Environment variable.

.PARAMETER PIN_PARSEXP
Environment variable.

.PARAMETER PIN_PP
Environment variable.

.PARAMETER PIN_PPX_ASSERT
Environment variable.

.PARAMETER PIN_PPX_BASE
Environment variable.

.PARAMETER PIN_PPX_COLD
Environment variable.

.PARAMETER PIN_PPX_COMPARE
Environment variable.

.PARAMETER PIN_PPX_DERIVERS
Environment variable.

.PARAMETER PIN_PPX_DERIVING
Environment variable.

.PARAMETER PIN_PPX_ENUMERATE
Environment variable.

.PARAMETER PIN_PPX_EXPECT
Environment variable.

.PARAMETER PIN_PPX_GLOBALIZE
Environment variable.

.PARAMETER PIN_PPX_HASH
Environment variable.

.PARAMETER PIN_PPX_HERE
Environment variable.

.PARAMETER PIN_PPX_IGNORE_INSTRUMENTATION
Environment variable.

.PARAMETER PIN_PPX_INLINE_TEST
Environment variable.

.PARAMETER PIN_PPX_OPTCOMP
Environment variable.

.PARAMETER PIN_PPX_PIPEBANG
Environment variable.

.PARAMETER PIN_PPX_SEXP_CONV
Environment variable.

.PARAMETER PIN_PPX_YOJSON_CONV_LIB
Environment variable.

.PARAMETER PIN_PPXLIB
Environment variable.

.PARAMETER PIN_PTIME
Environment variable.

.PARAMETER PIN_QRC
Environment variable.

.PARAMETER PIN_RE
Environment variable.

.PARAMETER PIN_REACT
Environment variable.

.PARAMETER PIN_REFL
Environment variable.

.PARAMETER PIN_RESULT
Environment variable.

.PARAMETER PIN_RRESULT
Environment variable.

.PARAMETER PIN_SEQ
Environment variable.

.PARAMETER PIN_SEXPLIB
Environment variable.

.PARAMETER PIN_SEXPLIB0
Environment variable.

.PARAMETER PIN_SHA
Environment variable.

.PARAMETER PIN_SPAWN
Environment variable.

.PARAMETER PIN_SQLITE3
Environment variable.

.PARAMETER PIN_STDCOMPAT
Environment variable.

.PARAMETER PIN_STDIO
Environment variable.

.PARAMETER PIN_STDLIB_SHIMS
Environment variable.

.PARAMETER PIN_STDUNE
Environment variable.

.PARAMETER PIN_TIME_NOW
Environment variable.

.PARAMETER PIN_TOPKG
Environment variable.

.PARAMETER PIN_TRAVERSE
Environment variable.

.PARAMETER PIN_TRIE
Environment variable.

.PARAMETER PIN_TSORT
Environment variable.

.PARAMETER PIN_TYXML
Environment variable.

.PARAMETER PIN_UCHAR
Environment variable.

.PARAMETER PIN_UTOP
Environment variable.

.PARAMETER PIN_UUCP
Environment variable.

.PARAMETER PIN_UUIDM
Environment variable.

.PARAMETER PIN_UUSEG
Environment variable.

.PARAMETER PIN_UUTF
Environment variable.

.PARAMETER PIN_WITH_DKML
Environment variable.

.PARAMETER PIN_XDG
Environment variable.

.PARAMETER PIN_YOJSON
Environment variable.

.PARAMETER PIN_ZED
Environment variable.

#>
[CmdletBinding()]
param (
  # Context variables
  [Parameter(HelpMessage='Defaults to the current directory')]
  [string]
  $PC_PROJECT_DIR = $PWD,
  
  # Input variables
  [string]
  $FDOPEN_OPAMEXE_BOOTSTRAP = "false",
  [string]
  $CACHE_PREFIX = "v1",
  [string]
  $OCAML_COMPILER = "",
  [string]
  $DKML_COMPILER = "",
  [string]
  $SKIP_OPAM_MODIFICATIONS = "false",
  [string]
  $SECONDARY_SWITCH = "false",
  [string]
  $PRIMARY_SWITCH_SKIP_INSTALL = "false",
  [string]
  $CONF_DKML_CROSS_TOOLCHAIN = "@repository@",
  [string]
  $DISKUV_OPAM_REPOSITORY = "",
  [string]
  $DKML_HOME = ""

  # Conflicts with automatic variable $Verbose
  # [Parameter()]
  # [string]
  # $VERBOSE = "false"
    
  # Environment variables (can be overridden on command line)
  # autogen from global_env_vars.
  ,[Parameter()] [string] $DKML_VERSION = "2.0.3"
  ,[Parameter()] [string] $DEFAULT_DISKUV_OPAM_REPOSITORY_TAG = "2.0.3"
  ,[Parameter()] [string] $DEFAULT_DKML_COMPILER = "2.0.3"
  ,[Parameter()] [string] $PIN_ASTRING = "0.8.5"
  ,[Parameter()] [string] $PIN_BASE = "v0.16.1"
  ,[Parameter()] [string] $PIN_BASE64 = "3.5.1"
  ,[Parameter()] [string] $PIN_BIGARRAY_COMPAT = "1.1.0"
  ,[Parameter()] [string] $PIN_BOS = "0.2.1"
  ,[Parameter()] [string] $PIN_CAMLP_STREAMS = "5.0.1"
  ,[Parameter()] [string] $PIN_CHROME_TRACE = "3.10.0"
  ,[Parameter()] [string] $PIN_CMDLINER = "1.2.0"
  ,[Parameter()] [string] $PIN_CONF_DKML_SYS_OPAM = "1"
  ,[Parameter()] [string] $PIN_CONF_PKG_CONFIG = "2+cpkgs"
  ,[Parameter()] [string] $PIN_CONF_SQLITE3 = "3.1+cpkgs"
  ,[Parameter()] [string] $PIN_CPPO = "1.6.9"
  ,[Parameter()] [string] $PIN_CRUNCH = "3.3.1"
  ,[Parameter()] [string] $PIN_CSEXP = "1.5.2"
  ,[Parameter()] [string] $PIN_CSTRUCT = "6.2.0"
  ,[Parameter()] [string] $PIN_CTYPES_FOREIGN = "0.19.2-windowssupport-r7"
  ,[Parameter()] [string] $PIN_CTYPES = "0.19.2-windowssupport-r7"
  ,[Parameter()] [string] $PIN_CUDF = "0.10"
  ,[Parameter()] [string] $PIN_DIGESTIF = "1.1.4"
  ,[Parameter()] [string] $PIN_DISKUVBOX = "0.2.0"
  ,[Parameter()] [string] $PIN_DKML_APPS = "2.0.3"
  ,[Parameter()] [string] $PIN_DKML_BASE_COMPILER = "4.14.0~v2.0.3"
  ,[Parameter()] [string] $PIN_DKML_BUILD_DESKTOP = "2.0.3"
  ,[Parameter()] [string] $PIN_DKML_C_PROBE = "3.0.0"
  ,[Parameter()] [string] $PIN_DKML_COMPILER_ENV = "2.0.3"
  ,[Parameter()] [string] $PIN_DKML_COMPILER_SRC = "2.0.3"
  ,[Parameter()] [string] $PIN_DKML_COMPONENT_COMMON_DESKTOP = "2.0.3"
  ,[Parameter()] [string] $PIN_DKML_COMPONENT_COMMON_OPAM = "2.2.0~alpha0~20221228"
  ,[Parameter()] [string] $PIN_DKML_COMPONENT_COMMON_UNIXUTILS = "0.2.0"
  ,[Parameter()] [string] $PIN_DKML_COMPONENT_OCAMLCOMPILER_COMMON = "4.14.0~v2.0.3"
  ,[Parameter()] [string] $PIN_DKML_COMPONENT_OCAMLCOMPILER_NETWORK = "4.14.0~v2.0.3"
  ,[Parameter()] [string] $PIN_DKML_COMPONENT_OCAMLCOMPILER_OFFLINE = "4.14.0~v2.0.3"
  ,[Parameter()] [string] $PIN_DKML_COMPONENT_OFFLINE_DESKTOP_FULL = "2.0.3"
  ,[Parameter()] [string] $PIN_DKML_COMPONENT_OFFLINE_OPAMSHIM = "2.2.0~alpha0~20221228"
  ,[Parameter()] [string] $PIN_DKML_COMPONENT_OFFLINE_UNIXUTILS = "0.2.0"
  ,[Parameter()] [string] $PIN_DKML_COMPONENT_STAGING_DESKTOP_FULL = "2.0.3"
  ,[Parameter()] [string] $PIN_DKML_COMPONENT_STAGING_DKMLCONFDIR = "2.0.3"
  ,[Parameter()] [string] $PIN_DKML_COMPONENT_STAGING_OCAMLRUN = "4.14.0~v2.0.3"
  ,[Parameter()] [string] $PIN_DKML_COMPONENT_STAGING_OPAM32 = "2.2.0~alpha0~20221228"
  ,[Parameter()] [string] $PIN_DKML_COMPONENT_STAGING_OPAM64 = "2.2.0~alpha0~20221228"
  ,[Parameter()] [string] $PIN_DKML_COMPONENT_STAGING_UNIXUTILS = "0.2.0"
  ,[Parameter()] [string] $PIN_DKML_COMPONENT_STAGING_WITHDKML = "2.0.3"
  ,[Parameter()] [string] $PIN_DKML_COMPONENT_XX_CONSOLE = "0.1.1"
  ,[Parameter()] [string] $PIN_DKML_EXE_LIB = "2.0.3"
  ,[Parameter()] [string] $PIN_DKML_EXE = "2.0.3"
  ,[Parameter()] [string] $PIN_DKML_INSTALL_INSTALLER = "0.5.1"
  ,[Parameter()] [string] $PIN_DKML_INSTALL_RUNNER = "0.5.1"
  ,[Parameter()] [string] $PIN_DKML_INSTALL = "0.5.1"
  ,[Parameter()] [string] $PIN_DKML_INSTALLER_OCAML_COMMON = "2.0.3"
  ,[Parameter()] [string] $PIN_DKML_INSTALLER_OCAML_OFFLINE = "2.0.3"
  ,[Parameter()] [string] $PIN_DKML_PACKAGE_CONSOLE = "0.5.1"
  ,[Parameter()] [string] $PIN_DKML_RUNTIME_COMMON_NATIVE = "2.0.3"
  ,[Parameter()] [string] $PIN_DKML_RUNTIME_COMMON = "2.0.3"
  ,[Parameter()] [string] $PIN_DKML_RUNTIME_DISTRIBUTION = "2.0.3"
  ,[Parameter()] [string] $PIN_DKML_RUNTIMELIB = "2.0.3"
  ,[Parameter()] [string] $PIN_DKML_RUNTIMESCRIPTS = "2.0.3"
  ,[Parameter()] [string] $PIN_DKML_WORKFLOWS = "2.0.3"
  ,[Parameter()] [string] $PIN_DUNE_ACTION_PLUGIN = "3.8.3"
  ,[Parameter()] [string] $PIN_DUNE_BUILD_INFO = "3.8.3"
  ,[Parameter()] [string] $PIN_DUNE_CONFIGURATOR = "3.8.3"
  ,[Parameter()] [string] $PIN_DUNE_GLOB = "3.8.3"
  ,[Parameter()] [string] $PIN_DUNE_PRIVATE_LIBS = "3.8.3"
  ,[Parameter()] [string] $PIN_DUNE_RPC_LWT = "3.8.3"
  ,[Parameter()] [string] $PIN_DUNE_RPC = "3.8.3"
  ,[Parameter()] [string] $PIN_DUNE_SITE = "3.8.3"
  ,[Parameter()] [string] $PIN_DUNE = "3.8.3"
  ,[Parameter()] [string] $PIN_DYN = "3.8.3"
  ,[Parameter()] [string] $PIN_EITHER = "1.0.0"
  ,[Parameter()] [string] $PIN_EQAF = "0.9"
  ,[Parameter()] [string] $PIN_EXTLIB = "1.7.9"
  ,[Parameter()] [string] $PIN_EZJSONM = "1.3.0"
  ,[Parameter()] [string] $PIN_FEATHER = "0.3.0"
  ,[Parameter()] [string] $PIN_FIBER = "3.7.0"
  ,[Parameter()] [string] $PIN_FIX = "20230505"
  ,[Parameter()] [string] $PIN_FMT = "0.9.0"
  ,[Parameter()] [string] $PIN_FPATH = "0.7.3"
  ,[Parameter()] [string] $PIN_GRAPHICS = "5.1.2"
  ,[Parameter()] [string] $PIN_HEX = "1.5.0"
  ,[Parameter()] [string] $PIN_INTEGERS = "0.7.0"
  ,[Parameter()] [string] $PIN_JANE_STREET_HEADERS = "v0.16.0"
  ,[Parameter()] [string] $PIN_JINGOO = "1.4.4"
  ,[Parameter()] [string] $PIN_JSONM = "1.0.2"
  ,[Parameter()] [string] $PIN_JSONRPC = "1.16.2"
  ,[Parameter()] [string] $PIN_JST_CONFIG = "v0.16.0"
  ,[Parameter()] [string] $PIN_LAMBDA_TERM = "3.3.1"
  ,[Parameter()] [string] $PIN_LOGS = "0.7.0"
  ,[Parameter()] [string] $PIN_LSP = "1.16.2"
  ,[Parameter()] [string] $PIN_LWT = "5.6.1"
  ,[Parameter()] [string] $PIN_LWT_REACT = "1.2.0"
  ,[Parameter()] [string] $PIN_MCCS = "1.1+13"
  ,[Parameter()] [string] $PIN_MDX = "2.3.0"
  ,[Parameter()] [string] $PIN_MENHIR = "20230608"
  ,[Parameter()] [string] $PIN_MENHIRLIB = "20230608"
  ,[Parameter()] [string] $PIN_MENHIRSDK = "20230608"
  ,[Parameter()] [string] $PIN_MERLIN_LIB = "4.9-414"
  ,[Parameter()] [string] $PIN_METAPP = "0.4.4+win"
  ,[Parameter()] [string] $PIN_METAQUOT = "0.5.2"
  ,[Parameter()] [string] $PIN_MEW = "0.1.0"
  ,[Parameter()] [string] $PIN_MEW_VI = "0.5.0"
  ,[Parameter()] [string] $PIN_NUM = "1.4"
  ,[Parameter()] [string] $PIN_OCAML_COMPILER_LIBS = "v0.12.4"
  ,[Parameter()] [string] $PIN_OCAML_LSP_SERVER = "1.16.2"
  ,[Parameter()] [string] $PIN_OCAML_VERSION = "3.6.1"
  ,[Parameter()] [string] $PIN_OCAML = "4.14.0"
  ,[Parameter()] [string] $PIN_OCAMLBUILD = "0.14.2+win+unix"
  ,[Parameter()] [string] $PIN_OCAMLC_LOC = "3.8.3"
  ,[Parameter()] [string] $PIN_OCAMLFIND = "1.9.5"
  ,[Parameter()] [string] $PIN_OCAMLFORMAT_LIB = "0.25.1"
  ,[Parameter()] [string] $PIN_OCAMLFORMAT_RPC_LIB = "0.25.1"
  ,[Parameter()] [string] $PIN_OCAMLFORMAT = "0.25.1"
  ,[Parameter()] [string] $PIN_OCP_INDENT = "1.8.2-windowssupport"
  ,[Parameter()] [string] $PIN_OCPLIB_ENDIAN = "1.2"
  ,[Parameter()] [string] $PIN_ODOC_PARSER = "2.0.0"
  ,[Parameter()] [string] $PIN_ODOC = "2.2.0"
  ,[Parameter()] [string] $PIN_ORDERING = "3.8.3"
  ,[Parameter()] [string] $PIN_PARSEXP = "v0.16.0"
  ,[Parameter()] [string] $PIN_PP = "1.1.2"
  ,[Parameter()] [string] $PIN_PPX_ASSERT = "v0.16.0"
  ,[Parameter()] [string] $PIN_PPX_BASE = "v0.16.0"
  ,[Parameter()] [string] $PIN_PPX_COLD = "v0.16.0"
  ,[Parameter()] [string] $PIN_PPX_COMPARE = "v0.16.0"
  ,[Parameter()] [string] $PIN_PPX_DERIVERS = "1.2.1"
  ,[Parameter()] [string] $PIN_PPX_DERIVING = "5.2.1"
  ,[Parameter()] [string] $PIN_PPX_ENUMERATE = "v0.16.0"
  ,[Parameter()] [string] $PIN_PPX_EXPECT = "v0.16.0"
  ,[Parameter()] [string] $PIN_PPX_GLOBALIZE = "v0.16.0"
  ,[Parameter()] [string] $PIN_PPX_HASH = "v0.16.0"
  ,[Parameter()] [string] $PIN_PPX_HERE = "v0.16.0"
  ,[Parameter()] [string] $PIN_PPX_IGNORE_INSTRUMENTATION = "v0.16.0"
  ,[Parameter()] [string] $PIN_PPX_INLINE_TEST = "v0.16.0"
  ,[Parameter()] [string] $PIN_PPX_OPTCOMP = "v0.16.0"
  ,[Parameter()] [string] $PIN_PPX_PIPEBANG = "v0.16.0"
  ,[Parameter()] [string] $PIN_PPX_SEXP_CONV = "v0.16.0"
  ,[Parameter()] [string] $PIN_PPX_YOJSON_CONV_LIB = "v0.16.0"
  ,[Parameter()] [string] $PIN_PPXLIB = "0.30.0"
  ,[Parameter()] [string] $PIN_PTIME = "1.1.0"
  ,[Parameter()] [string] $PIN_QRC = "0.1.1~dune"
  ,[Parameter()] [string] $PIN_RE = "1.11.0"
  ,[Parameter()] [string] $PIN_REACT = "1.2.2"
  ,[Parameter()] [string] $PIN_REFL = "0.4.1"
  ,[Parameter()] [string] $PIN_RESULT = "1.5"
  ,[Parameter()] [string] $PIN_RRESULT = "0.7.0"
  ,[Parameter()] [string] $PIN_SEQ = "base"
  ,[Parameter()] [string] $PIN_SEXPLIB = "v0.16.0"
  ,[Parameter()] [string] $PIN_SEXPLIB0 = "v0.16.0"
  ,[Parameter()] [string] $PIN_SHA = "1.15.4"
  ,[Parameter()] [string] $PIN_SPAWN = "v0.15.1"
  ,[Parameter()] [string] $PIN_SQLITE3 = "5.1.0+msvc"
  ,[Parameter()] [string] $PIN_STDCOMPAT = "19+optautoconf"
  ,[Parameter()] [string] $PIN_STDIO = "v0.16.0"
  ,[Parameter()] [string] $PIN_STDLIB_SHIMS = "0.3.0"
  ,[Parameter()] [string] $PIN_STDUNE = "3.8.3"
  ,[Parameter()] [string] $PIN_TIME_NOW = "v0.16.0"
  ,[Parameter()] [string] $PIN_TOPKG = "1.0.7"
  ,[Parameter()] [string] $PIN_TRAVERSE = "0.3.0"
  ,[Parameter()] [string] $PIN_TRIE = "1.0.0"
  ,[Parameter()] [string] $PIN_TSORT = "2.1.0"
  ,[Parameter()] [string] $PIN_TYXML = "4.5.0"
  ,[Parameter()] [string] $PIN_UCHAR = "0.0.2"
  ,[Parameter()] [string] $PIN_UTOP = "2.13.1"
  ,[Parameter()] [string] $PIN_UUCP = "15.0.0"
  ,[Parameter()] [string] $PIN_UUIDM = "0.9.8"
  ,[Parameter()] [string] $PIN_UUSEG = "15.0.0"
  ,[Parameter()] [string] $PIN_UUTF = "1.0.3"
  ,[Parameter()] [string] $PIN_WITH_DKML = "2.0.3"
  ,[Parameter()] [string] $PIN_XDG = "3.9.0"
  ,[Parameter()] [string] $PIN_YOJSON = "2.1.0"
  ,[Parameter()] [string] $PIN_ZED = "3.2.2"
)

$ErrorActionPreference = "Stop"

# Reset environment so no conflicts with a parent Opam or OCaml system
if (Test-Path Env:OPAMROOT)             { Remove-Item Env:OPAMROOT }
if (Test-Path Env:OPAMSWITCH)           { Remove-Item Env:OPAMSWITCH }
if (Test-Path Env:OPAM_SWITCH_PREFIX)   { Remove-Item Env:OPAM_SWITCH_PREFIX }
if (Test-Path Env:CAML_LD_LIBRARY_PATH) { Remove-Item Env:CAML_LD_LIBRARY_PATH }
if (Test-Path Env:OCAMLLIB)             { Remove-Item Env:OCAMLLIB }
if (Test-Path Env:OCAML_TOPLEVEL_PATH)  { Remove-Item Env:OCAML_TOPLEVEL_PATH }

# Pushdown context variables
$env:PC_CI = 'true'
$env:PC_PROJECT_DIR = $PC_PROJECT_DIR

# Pushdown input variables
$env:FDOPEN_OPAMEXE_BOOTSTRAP = $FDOPEN_OPAMEXE_BOOTSTRAP
$env:CACHE_PREFIX = $CACHE_PREFIX
$env:OCAML_COMPILER = $OCAML_COMPILER
$env:DKML_COMPILER = $DKML_COMPILER
$env:SKIP_OPAM_MODIFICATIONS = $SKIP_OPAM_MODIFICATIONS
$env:SECONDARY_SWITCH = $SECONDARY_SWITCH
$env:PRIMARY_SWITCH_SKIP_INSTALL = $PRIMARY_SWITCH_SKIP_INSTALL
$env:CONF_DKML_CROSS_TOOLCHAIN = $CONF_DKML_CROSS_TOOLCHAIN
$env:DISKUV_OPAM_REPOSITORY = $DISKUV_OPAM_REPOSITORY
$env:DKML_HOME = $DKML_HOME

# Set matrix variables
# autogen from pc_vars. only windows_x86
$env:dkml_host_os = "windows"
$env:opam_root_cacheable = "${env:PC_PROJECT_DIR}/.ci/o"
$env:abi_pattern = "win32-windows_x86"
$env:msys2_system = "MINGW32"
$env:msys2_packages = "mingw-w64-i686-pkg-config"
$env:exe_ext = ".exe"
$env:bootstrap_opam_version = "2.2.0-alpha-20221228"
$env:opam_abi = "windows_x86"
$env:dkml_host_abi = "windows_x86"
$env:opam_root = "${env:PC_PROJECT_DIR}/.ci/o"
$env:vsstudio_hostarch = "x64"
$env:vsstudio_arch = "x86"
$env:ocaml_options = "ocaml-option-32bit"


# Set environment variables
# autogen from global_env_vars.
$env:DKML_VERSION = $DKML_VERSION
$env:DEFAULT_DISKUV_OPAM_REPOSITORY_TAG = $DEFAULT_DISKUV_OPAM_REPOSITORY_TAG
$env:DEFAULT_DKML_COMPILER = $DEFAULT_DKML_COMPILER
$env:PIN_ASTRING = $PIN_ASTRING
$env:PIN_BASE = $PIN_BASE
$env:PIN_BASE64 = $PIN_BASE64
$env:PIN_BIGARRAY_COMPAT = $PIN_BIGARRAY_COMPAT
$env:PIN_BOS = $PIN_BOS
$env:PIN_CAMLP_STREAMS = $PIN_CAMLP_STREAMS
$env:PIN_CHROME_TRACE = $PIN_CHROME_TRACE
$env:PIN_CMDLINER = $PIN_CMDLINER
$env:PIN_CONF_DKML_SYS_OPAM = $PIN_CONF_DKML_SYS_OPAM
$env:PIN_CONF_PKG_CONFIG = $PIN_CONF_PKG_CONFIG
$env:PIN_CONF_SQLITE3 = $PIN_CONF_SQLITE3
$env:PIN_CPPO = $PIN_CPPO
$env:PIN_CRUNCH = $PIN_CRUNCH
$env:PIN_CSEXP = $PIN_CSEXP
$env:PIN_CSTRUCT = $PIN_CSTRUCT
$env:PIN_CTYPES_FOREIGN = $PIN_CTYPES_FOREIGN
$env:PIN_CTYPES = $PIN_CTYPES
$env:PIN_CUDF = $PIN_CUDF
$env:PIN_DIGESTIF = $PIN_DIGESTIF
$env:PIN_DISKUVBOX = $PIN_DISKUVBOX
$env:PIN_DKML_APPS = $PIN_DKML_APPS
$env:PIN_DKML_BASE_COMPILER = $PIN_DKML_BASE_COMPILER
$env:PIN_DKML_BUILD_DESKTOP = $PIN_DKML_BUILD_DESKTOP
$env:PIN_DKML_C_PROBE = $PIN_DKML_C_PROBE
$env:PIN_DKML_COMPILER_ENV = $PIN_DKML_COMPILER_ENV
$env:PIN_DKML_COMPILER_SRC = $PIN_DKML_COMPILER_SRC
$env:PIN_DKML_COMPONENT_COMMON_DESKTOP = $PIN_DKML_COMPONENT_COMMON_DESKTOP
$env:PIN_DKML_COMPONENT_COMMON_OPAM = $PIN_DKML_COMPONENT_COMMON_OPAM
$env:PIN_DKML_COMPONENT_COMMON_UNIXUTILS = $PIN_DKML_COMPONENT_COMMON_UNIXUTILS
$env:PIN_DKML_COMPONENT_OCAMLCOMPILER_COMMON = $PIN_DKML_COMPONENT_OCAMLCOMPILER_COMMON
$env:PIN_DKML_COMPONENT_OCAMLCOMPILER_NETWORK = $PIN_DKML_COMPONENT_OCAMLCOMPILER_NETWORK
$env:PIN_DKML_COMPONENT_OCAMLCOMPILER_OFFLINE = $PIN_DKML_COMPONENT_OCAMLCOMPILER_OFFLINE
$env:PIN_DKML_COMPONENT_OFFLINE_DESKTOP_FULL = $PIN_DKML_COMPONENT_OFFLINE_DESKTOP_FULL
$env:PIN_DKML_COMPONENT_OFFLINE_OPAMSHIM = $PIN_DKML_COMPONENT_OFFLINE_OPAMSHIM
$env:PIN_DKML_COMPONENT_OFFLINE_UNIXUTILS = $PIN_DKML_COMPONENT_OFFLINE_UNIXUTILS
$env:PIN_DKML_COMPONENT_STAGING_DESKTOP_FULL = $PIN_DKML_COMPONENT_STAGING_DESKTOP_FULL
$env:PIN_DKML_COMPONENT_STAGING_DKMLCONFDIR = $PIN_DKML_COMPONENT_STAGING_DKMLCONFDIR
$env:PIN_DKML_COMPONENT_STAGING_OCAMLRUN = $PIN_DKML_COMPONENT_STAGING_OCAMLRUN
$env:PIN_DKML_COMPONENT_STAGING_OPAM32 = $PIN_DKML_COMPONENT_STAGING_OPAM32
$env:PIN_DKML_COMPONENT_STAGING_OPAM64 = $PIN_DKML_COMPONENT_STAGING_OPAM64
$env:PIN_DKML_COMPONENT_STAGING_UNIXUTILS = $PIN_DKML_COMPONENT_STAGING_UNIXUTILS
$env:PIN_DKML_COMPONENT_STAGING_WITHDKML = $PIN_DKML_COMPONENT_STAGING_WITHDKML
$env:PIN_DKML_COMPONENT_XX_CONSOLE = $PIN_DKML_COMPONENT_XX_CONSOLE
$env:PIN_DKML_EXE_LIB = $PIN_DKML_EXE_LIB
$env:PIN_DKML_EXE = $PIN_DKML_EXE
$env:PIN_DKML_INSTALL_INSTALLER = $PIN_DKML_INSTALL_INSTALLER
$env:PIN_DKML_INSTALL_RUNNER = $PIN_DKML_INSTALL_RUNNER
$env:PIN_DKML_INSTALL = $PIN_DKML_INSTALL
$env:PIN_DKML_INSTALLER_OCAML_COMMON = $PIN_DKML_INSTALLER_OCAML_COMMON
$env:PIN_DKML_INSTALLER_OCAML_OFFLINE = $PIN_DKML_INSTALLER_OCAML_OFFLINE
$env:PIN_DKML_PACKAGE_CONSOLE = $PIN_DKML_PACKAGE_CONSOLE
$env:PIN_DKML_RUNTIME_COMMON_NATIVE = $PIN_DKML_RUNTIME_COMMON_NATIVE
$env:PIN_DKML_RUNTIME_COMMON = $PIN_DKML_RUNTIME_COMMON
$env:PIN_DKML_RUNTIME_DISTRIBUTION = $PIN_DKML_RUNTIME_DISTRIBUTION
$env:PIN_DKML_RUNTIMELIB = $PIN_DKML_RUNTIMELIB
$env:PIN_DKML_RUNTIMESCRIPTS = $PIN_DKML_RUNTIMESCRIPTS
$env:PIN_DKML_WORKFLOWS = $PIN_DKML_WORKFLOWS
$env:PIN_DUNE_ACTION_PLUGIN = $PIN_DUNE_ACTION_PLUGIN
$env:PIN_DUNE_BUILD_INFO = $PIN_DUNE_BUILD_INFO
$env:PIN_DUNE_CONFIGURATOR = $PIN_DUNE_CONFIGURATOR
$env:PIN_DUNE_GLOB = $PIN_DUNE_GLOB
$env:PIN_DUNE_PRIVATE_LIBS = $PIN_DUNE_PRIVATE_LIBS
$env:PIN_DUNE_RPC_LWT = $PIN_DUNE_RPC_LWT
$env:PIN_DUNE_RPC = $PIN_DUNE_RPC
$env:PIN_DUNE_SITE = $PIN_DUNE_SITE
$env:PIN_DUNE = $PIN_DUNE
$env:PIN_DYN = $PIN_DYN
$env:PIN_EITHER = $PIN_EITHER
$env:PIN_EQAF = $PIN_EQAF
$env:PIN_EXTLIB = $PIN_EXTLIB
$env:PIN_EZJSONM = $PIN_EZJSONM
$env:PIN_FEATHER = $PIN_FEATHER
$env:PIN_FIBER = $PIN_FIBER
$env:PIN_FIX = $PIN_FIX
$env:PIN_FMT = $PIN_FMT
$env:PIN_FPATH = $PIN_FPATH
$env:PIN_GRAPHICS = $PIN_GRAPHICS
$env:PIN_HEX = $PIN_HEX
$env:PIN_INTEGERS = $PIN_INTEGERS
$env:PIN_JANE_STREET_HEADERS = $PIN_JANE_STREET_HEADERS
$env:PIN_JINGOO = $PIN_JINGOO
$env:PIN_JSONM = $PIN_JSONM
$env:PIN_JSONRPC = $PIN_JSONRPC
$env:PIN_JST_CONFIG = $PIN_JST_CONFIG
$env:PIN_LAMBDA_TERM = $PIN_LAMBDA_TERM
$env:PIN_LOGS = $PIN_LOGS
$env:PIN_LSP = $PIN_LSP
$env:PIN_LWT = $PIN_LWT
$env:PIN_LWT_REACT = $PIN_LWT_REACT
$env:PIN_MCCS = $PIN_MCCS
$env:PIN_MDX = $PIN_MDX
$env:PIN_MENHIR = $PIN_MENHIR
$env:PIN_MENHIRLIB = $PIN_MENHIRLIB
$env:PIN_MENHIRSDK = $PIN_MENHIRSDK
$env:PIN_MERLIN_LIB = $PIN_MERLIN_LIB
$env:PIN_METAPP = $PIN_METAPP
$env:PIN_METAQUOT = $PIN_METAQUOT
$env:PIN_MEW = $PIN_MEW
$env:PIN_MEW_VI = $PIN_MEW_VI
$env:PIN_NUM = $PIN_NUM
$env:PIN_OCAML_COMPILER_LIBS = $PIN_OCAML_COMPILER_LIBS
$env:PIN_OCAML_LSP_SERVER = $PIN_OCAML_LSP_SERVER
$env:PIN_OCAML_VERSION = $PIN_OCAML_VERSION
$env:PIN_OCAML = $PIN_OCAML
$env:PIN_OCAMLBUILD = $PIN_OCAMLBUILD
$env:PIN_OCAMLC_LOC = $PIN_OCAMLC_LOC
$env:PIN_OCAMLFIND = $PIN_OCAMLFIND
$env:PIN_OCAMLFORMAT_LIB = $PIN_OCAMLFORMAT_LIB
$env:PIN_OCAMLFORMAT_RPC_LIB = $PIN_OCAMLFORMAT_RPC_LIB
$env:PIN_OCAMLFORMAT = $PIN_OCAMLFORMAT
$env:PIN_OCP_INDENT = $PIN_OCP_INDENT
$env:PIN_OCPLIB_ENDIAN = $PIN_OCPLIB_ENDIAN
$env:PIN_ODOC_PARSER = $PIN_ODOC_PARSER
$env:PIN_ODOC = $PIN_ODOC
$env:PIN_ORDERING = $PIN_ORDERING
$env:PIN_PARSEXP = $PIN_PARSEXP
$env:PIN_PP = $PIN_PP
$env:PIN_PPX_ASSERT = $PIN_PPX_ASSERT
$env:PIN_PPX_BASE = $PIN_PPX_BASE
$env:PIN_PPX_COLD = $PIN_PPX_COLD
$env:PIN_PPX_COMPARE = $PIN_PPX_COMPARE
$env:PIN_PPX_DERIVERS = $PIN_PPX_DERIVERS
$env:PIN_PPX_DERIVING = $PIN_PPX_DERIVING
$env:PIN_PPX_ENUMERATE = $PIN_PPX_ENUMERATE
$env:PIN_PPX_EXPECT = $PIN_PPX_EXPECT
$env:PIN_PPX_GLOBALIZE = $PIN_PPX_GLOBALIZE
$env:PIN_PPX_HASH = $PIN_PPX_HASH
$env:PIN_PPX_HERE = $PIN_PPX_HERE
$env:PIN_PPX_IGNORE_INSTRUMENTATION = $PIN_PPX_IGNORE_INSTRUMENTATION
$env:PIN_PPX_INLINE_TEST = $PIN_PPX_INLINE_TEST
$env:PIN_PPX_OPTCOMP = $PIN_PPX_OPTCOMP
$env:PIN_PPX_PIPEBANG = $PIN_PPX_PIPEBANG
$env:PIN_PPX_SEXP_CONV = $PIN_PPX_SEXP_CONV
$env:PIN_PPX_YOJSON_CONV_LIB = $PIN_PPX_YOJSON_CONV_LIB
$env:PIN_PPXLIB = $PIN_PPXLIB
$env:PIN_PTIME = $PIN_PTIME
$env:PIN_QRC = $PIN_QRC
$env:PIN_RE = $PIN_RE
$env:PIN_REACT = $PIN_REACT
$env:PIN_REFL = $PIN_REFL
$env:PIN_RESULT = $PIN_RESULT
$env:PIN_RRESULT = $PIN_RRESULT
$env:PIN_SEQ = $PIN_SEQ
$env:PIN_SEXPLIB = $PIN_SEXPLIB
$env:PIN_SEXPLIB0 = $PIN_SEXPLIB0
$env:PIN_SHA = $PIN_SHA
$env:PIN_SPAWN = $PIN_SPAWN
$env:PIN_SQLITE3 = $PIN_SQLITE3
$env:PIN_STDCOMPAT = $PIN_STDCOMPAT
$env:PIN_STDIO = $PIN_STDIO
$env:PIN_STDLIB_SHIMS = $PIN_STDLIB_SHIMS
$env:PIN_STDUNE = $PIN_STDUNE
$env:PIN_TIME_NOW = $PIN_TIME_NOW
$env:PIN_TOPKG = $PIN_TOPKG
$env:PIN_TRAVERSE = $PIN_TRAVERSE
$env:PIN_TRIE = $PIN_TRIE
$env:PIN_TSORT = $PIN_TSORT
$env:PIN_TYXML = $PIN_TYXML
$env:PIN_UCHAR = $PIN_UCHAR
$env:PIN_UTOP = $PIN_UTOP
$env:PIN_UUCP = $PIN_UUCP
$env:PIN_UUIDM = $PIN_UUIDM
$env:PIN_UUSEG = $PIN_UUSEG
$env:PIN_UUTF = $PIN_UUTF
$env:PIN_WITH_DKML = $PIN_WITH_DKML
$env:PIN_XDG = $PIN_XDG
$env:PIN_YOJSON = $PIN_YOJSON
$env:PIN_ZED = $PIN_ZED

# https://patchwork.kernel.org/project/qemu-devel/patch/20211215073402.144286-17-thuth@redhat.com/
$env:CHERE_INVOKING = "yes" # Preserve the current working directory
$env:MSYSTEM = $env:msys2_system # Start a 64 bit environment if CLANG64, etc.

########################### before_script ###############################

# Troubleshooting
If ( "${env:VERBOSE}" -eq "true" ) { Get-ChildItem 'env:' }

# -----
# MSYS2
# -----
#
# https://www.msys2.org/docs/ci/
# https://patchwork.kernel.org/project/qemu-devel/patch/20211215073402.144286-17-thuth@redhat.com/

if ( Test-Path -Path msys64\usr\bin\pacman.exe ) {
  Write-Host "Re-using MSYS2 from cache."
}
else {
  Write-Host "Download the archive ..."
  If ( !(Test-Path -Path msys64\var\cache ) ) { New-Item msys64\var\cache -ItemType Directory | Out-Null }
  If ( !(Test-Path -Path msys64\var\cache\msys2.exe ) ) { Invoke-WebRequest "https://github.com/msys2/msys2-installer/releases/download/2022-09-04/msys2-base-x86_64-20220904.sfx.exe" -outfile "msys64\var\cache\msys2.exe" }

  Write-Host "Extract the archive ..."
  msys64\var\cache\msys2.exe -y # Extract to msys64
  Remove-Item msys64\var\cache\msys2.exe # Delete the archive again
  ((Get-Content -path msys64\etc\post-install\07-pacman-key.post -Raw) -replace '--refresh-keys', '--version') | Set-Content -Path msys64\etc\post-install\07-pacman-key.post
  msys64\usr\bin\bash -lc "sed -i 's/^CheckSpace/#CheckSpace/g' /etc/pacman.conf"

  Write-Host "Run for the first time ..."
  msys64\usr\bin\bash -lc ' '
}
Write-Host "Update MSYS2 ..."
msys64\usr\bin\bash -lc 'pacman --noconfirm -Syuu' # Core update (in case any core packages are outdated)
msys64\usr\bin\bash -lc 'pacman --noconfirm -Syuu' # Normal update
if ("${env:CI}" -eq "true") { taskkill /F /FI "MODULES eq msys-2.0.dll" } # Only safe to kill MSYS2 in CI

Write-Host "Install matrix, required and CI packages ..."
#   Packages for GitLab CI:
#     dos2unix (used to translate PowerShell written files below in this CI .yml into MSYS2 scripts)
msys64\usr\bin\bash -lc 'set -x; pacman -Sy --noconfirm --needed ${msys2_packages}  wget make rsync diffutils patch unzip git tar xz dos2unix'

Write-Host "Uninstall MSYS2 conflicting executables ..."
msys64\usr\bin\bash -lc 'rm -vf /usr/bin/link.exe' # link.exe interferes with MSVC's link.exe

Write-Host "Installing VSSetup for the Get-VSSetupInstance function ..."
Install-Module VSSetup -Scope CurrentUser -Force

Write-Host "Writing scripts ..."

# POSIX and AWK scripts

If ( !(Test-Path -Path.ci\sd4 ) ) { New-Item .ci\sd4 -ItemType Directory | Out-Null }

$Content = @'
#!/bin/sh

# ------------------------ Log Formatting ------------------------

TXT_SECTION="\e[94m" # bright blue
TXT_CLEAR="\e[0m"

if [ "${GITLAB_CI:-}" = "true" ]; then
    # https://docs.gitlab.com/ee/ci/jobs/#expand-and-collapse-job-log-sections
    print_section_start() {
        print_section_start_NAME=$1
        shift
        printf "\e[0Ksection_start:%s:%s[collapsed=true]\r\e[0K" \
            "$(date +%s)" \
            "$print_section_start_NAME"
    }
    print_section_end() {
        print_section_end_NAME=$1
        shift
        printf "\e[0Ksection_end:%s:%s\r\e[0K\n" \
            "$(date +%s)" \
            "$print_section_end_NAME"
    }
elif [ -n "${GITHUB_ENV:-}" ]; then
    # https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions#grouping-log-lines
    print_section_start() {
        print_section_start_NAME=$1
        shift
        printf "::group::"
    }
    print_section_end() {
        print_section_end_NAME=$1
        shift
        printf "::endgroup::\n"
    }
else
    print_section_start() {
        print_section_start_NAME=$1
        shift
    }
    print_section_end() {
        print_section_end_NAME=$1
        shift
    }
fi

section_begin() {
    # https://docs.gitlab.com/ee/ci/yaml/script.html#add-color-codes-to-script-output
    section_NAME=$1
    shift
    section_HEADER=$1
    shift
    print_section_start "$section_NAME"
    printf "${TXT_SECTION}%s${TXT_CLEAR}\n" "$section_HEADER"
}

section_end() {
    section_NAME=$1
    shift
    print_section_end "$section_NAME"
}

# ------------------- Other Functions -----------------

transfer_dir() {
    transfer_dir_SRC=$1
    shift
    transfer_dir_DST=$1
    shift
    # Remove the destination directory completely, but make sure the parent of the
    # destination directory exists so `mv` will work
    install -d "$transfer_dir_DST"
    rm -rf "$transfer_dir_DST"
    # Move
    mv "$transfer_dir_SRC" "$transfer_dir_DST"
}

# Set TEMP variable which is used, among other things, for OCaml's
# [Filename.temp_dir_name] on Win32, and by with-dkml.exe on Windows
export_temp_for_windows() {
    if [ -x /usr/bin/cygpath ]; then
        if [ -n "${RUNNER_TEMP:-}" ]; then
            # GitHub Actions
            TEMP=$(cygpath -am "$RUNNER_TEMP")
        else
            # GitLab CI/CD or desktop
            install -d .ci/tmp
            TEMP=$(cygpath -am ".ci/tmp")
        fi
        export TEMP
    fi
}

# Fixup opam_root on Windows to be mixed case.
# On input the following variables must be present:
# - opam_root
# - opam_root_cacheable
# On output the input variables will be modified _and_ the
# following variables will be available:
# - original_opam_root
# - original_opam_root_cacheable
# - unix_opam_root
# - unix_opam_root_cacheable
fixup_opam_root() {
    # shellcheck disable=SC2034
    original_opam_root=$opam_root
    # shellcheck disable=SC2034
    original_opam_root_cacheable=$opam_root_cacheable
    if [ -x /usr/bin/cygpath ]; then
        opam_root=$(/usr/bin/cygpath -m "$opam_root")
        opam_root_cacheable=$(/usr/bin/cygpath -m "$opam_root_cacheable")
        unix_opam_root=$(/usr/bin/cygpath -u "$opam_root")
        unix_opam_root_cacheable=$(/usr/bin/cygpath -u "$opam_root_cacheable")
    else
        # shellcheck disable=SC2034
        unix_opam_root=$opam_root
        # shellcheck disable=SC2034
        unix_opam_root_cacheable=$opam_root_cacheable
    fi
}
'@
Set-Content -Path ".ci\sd4\common-values.sh" -Encoding Unicode -Value $Content
msys64\usr\bin\bash -lc 'dos2unix .ci/sd4/common-values.sh'


$Content = @'
#!/bin/sh

# ================
# checkout-code.sh
# ================
#
# Checkouts all of the git source code.
#
# This should be done outside of
# dockcross (used by Linux) since a Docker-in-Docker container can have
# difficulties doing a git checkout (the Git credentials for any private
# repositories are likely not present). We don't care about any private
# repositories for DKML but any code that extends this (ex. DKSDK) may
# need to use private repositories.

set -euf

setup_WORKSPACE_VARNAME=$1
shift
setup_WORKSPACE=$1
shift

if [ -x /usr/bin/cygpath ]; then
    setup_WORKSPACE=$(/usr/bin/cygpath -au "$setup_WORKSPACE")
fi

# ------------------------ Functions ------------------------

# shellcheck source=./common-values.sh
. .ci/sd4/common-values.sh

# Disable automatic garbage collection
git_disable_gc() {
    git_disable_gc_NAME=$1
    shift
    git -C ".ci/sd4/g/$git_disable_gc_NAME" config --local gc.auto 0
}

# Mimic the behavior of GitHub's actions/checkout@v3
# - the plus symbol in 'git fetch ... origin +REF:refs/tags/v0.0' overrides any existing REF
git_checkout() {
    git_checkout_NAME=$1
    shift
    git_checkout_URL=$1
    shift
    git_checkout_REF=$1
    shift

    if [ -e ".ci/sd4/g/$git_checkout_NAME" ]; then
        git_disable_gc "$git_checkout_NAME"
        git -C ".ci/sd4/g/$git_checkout_NAME" remote set-url origin "$git_checkout_URL"
        git -C ".ci/sd4/g/$git_checkout_NAME" fetch --no-tags --progress --no-recurse-submodules --depth=1 origin "+${git_checkout_REF}:refs/tags/v0.0"
    else
        install -d ".ci/sd4/g/$git_checkout_NAME"
        git -C ".ci/sd4/g/$git_checkout_NAME" -c init.defaultBranch=main init
        git_disable_gc "$git_checkout_NAME"
        git -C ".ci/sd4/g/$git_checkout_NAME" remote add origin "$git_checkout_URL"
        git -C ".ci/sd4/g/$git_checkout_NAME" fetch --no-tags --prune --progress --no-recurse-submodules --depth=1 origin "+${git_checkout_REF}:refs/tags/v0.0"
    fi
    git -C ".ci/sd4/g/$git_checkout_NAME" -c advice.detachedHead=false checkout --progress --force refs/tags/v0.0
    git -C ".ci/sd4/g/$git_checkout_NAME" log -1 --format='%H'
}

# ---------------------------------------------------------------------

section_begin checkout-info "Summary: code checkout"

# shellcheck disable=SC2154
echo "
================
checkout-code.sh
================
.
---------
Arguments
---------
WORKSPACE_VARNAME=$setup_WORKSPACE_VARNAME
WORKSPACE=$setup_WORKSPACE
.
------
Inputs
------
VERBOSE=${VERBOSE:-}
.
------
Matrix
------
dkml_host_abi=$dkml_host_abi
.
"

section_end checkout-info

install -d .ci/sd4/g

# dkml-runtime-distribution

#   For 'Diagnose Visual Studio environment variables (Windows)' we need dkml-runtime-distribution
#   so that 'Import-Module Machine' and 'Get-VSSetupInstance' can be run.
#   The version doesn't matter too much, as long as it has a functioning Get-VSSetupInstance
#   that supports the Visual Studio versions of the latest GitLab CI and GitHub Actions machines.
#   commit 4d6f1bfc3510c55ba4273cb240e43727854b5718 = WinSDK 19041 and VS 14.29
case "$dkml_host_abi" in
windows_*)
    section_begin checkout-dkml-runtime-distribution 'Checkout dkml-runtime-distribution'
    git_checkout dkml-runtime-distribution https://github.com/diskuv/dkml-runtime-distribution.git "4d6f1bfc3510c55ba4273cb240e43727854b5718"
    section_end checkout-dkml-runtime-distribution
    ;;
esac

'@
Set-Content -Path ".ci\sd4\run-checkout-code.sh" -Encoding Unicode -Value $Content
msys64\usr\bin\bash -lc 'dos2unix .ci/sd4/run-checkout-code.sh'


$Content = @'
#!/bin/sh
set -euf

# Constants
SHA512_DEVNULL='cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e'

setup_WORKSPACE_VARNAME=$1
shift
setup_WORKSPACE=$1
shift

if [ -x /usr/bin/cygpath ]; then
    setup_WORKSPACE=$(/usr/bin/cygpath -au "$setup_WORKSPACE")
fi

# ------------------ Variables and functions ------------------------

# shellcheck source=./common-values.sh
. .ci/sd4/common-values.sh

if [ "${VERBOSE:-}" = "true" ]; then
    do_tar_rf() {
        tar rvf "$@"
    }
else
    do_tar_rf() {
        tar rf "$@"
    }
fi

# Make the standard input work as an OCaml string.
# This currently only escapes backslashes and double quotes.
escape_arg_as_ocaml_string() {
    escape_arg_as_ocaml_string_ARG=$1
    shift
    printf "%s" "$escape_arg_as_ocaml_string_ARG" | sed 's#\\#\\\\#g; s#"#\\"#g;'
}

# Fixup opam_root on Windows to be mixed case. Set original_* and unix_* as well.
fixup_opam_root

# Set TEMP variable for Windows
export_temp_for_windows

# Load VS studio environment
if [ -e .ci/sd4/vsenv.sh ]; then
    # shellcheck disable=SC1091
    . .ci/sd4/vsenv.sh
fi

# -------------------------------------------------------------------

section_begin setup-info "Summary: setup-dkml"

SKIP_OPAM_MODIFICATIONS=${SKIP_OPAM_MODIFICATIONS:-false} # default is false

# shellcheck disable=SC2154
echo "
=============
setup-dkml.sh
=============
.
---------
Arguments
---------
WORKSPACE_VARNAME=$setup_WORKSPACE_VARNAME
WORKSPACE=$setup_WORKSPACE
.
------
Inputs
------
FDOPEN_OPAMEXE_BOOTSTRAP=${FDOPEN_OPAMEXE_BOOTSTRAP:-}
DISKUV_OPAM_REPOSITORY=${DISKUV_OPAM_REPOSITORY:-}
DKML_COMPILER=${DKML_COMPILER:-}
OCAML_COMPILER=${OCAML_COMPILER:-}
CONF_DKML_CROSS_TOOLCHAIN=${CONF_DKML_CROSS_TOOLCHAIN:-}
SKIP_OPAM_MODIFICATIONS=${SKIP_OPAM_MODIFICATIONS:-}
SECONDARY_SWITCH=${SECONDARY_SWITCH:-}
PRIMARY_SWITCH_SKIP_INSTALL=${PRIMARY_SWITCH_SKIP_INSTALL:-}
MANYLINUX=${MANYLINUX:-}
DKML_HOME=${DKML_HOME:-}
VERBOSE=${VERBOSE:-}
.
-------------------
Generated Constants
-------------------
DKML_VERSION=$DKML_VERSION
DEFAULT_DISKUV_OPAM_REPOSITORY_TAG=$DEFAULT_DISKUV_OPAM_REPOSITORY_TAG
DEFAULT_DKML_COMPILER=$DEFAULT_DKML_COMPILER
.
------
Matrix
------
dkml_host_abi=$dkml_host_abi
bootstrap_opam_version=$bootstrap_opam_version
abi_pattern=$abi_pattern
opam_root=${opam_root}
opam_root_cacheable=${opam_root_cacheable}
original_opam_root=${original_opam_root}
original_opam_root_cacheable=${original_opam_root_cacheable}
unix_opam_root=${unix_opam_root}
unix_opam_root_cacheable=${unix_opam_root_cacheable}
dockcross_image=${dockcross_image:-}
dockcross_image_custom_prefix=${dockcross_image_custom_prefix:-}
dockcross_run_extra_args=${dockcross_run_extra_args:-}
docker_runner=${docker_runner:-}
in_docker=${in_docker:-}
ocaml_options=${ocaml_options:-}
.
----
Pins
----
"
set | grep ^PIN_
echo ".
"
case "$dkml_host_abi" in
windows_*)
    # shellcheck disable=SC2153
    echo "
-------------
Visual Studio
-------------
VS_DIR=$VS_DIR
VS_VCVARSVER=$VS_VCVARSVER
VS_WINSDKVER=$VS_WINSDKVER
VS_MSVSPREFERENCE=$VS_MSVSPREFERENCE
VS_CMAKEGENERATOR=$VS_CMAKEGENERATOR
.
"
    ;;
esac
section_end setup-info

do_bootstrap() {
    # Bootstrap from historical release
    runit_BOOTSTRAPPED=0

    #   Bootstrap opam from fdopen (Windows)
    if [ "$runit_BOOTSTRAPPED" = 0 ] && [ "${FDOPEN_OPAMEXE_BOOTSTRAP:-}" = "true" ]; then
        if [ -e .ci/sd4/opam64/bin/opam.exe ] && [ -e .ci/sd4/opam64/bin/opam-installer.exe ]; then
            runit_BOOTSTRAPPED=1
        else
            case "$dkml_host_abi" in
            windows_*)
                echo 'Bootstrap opam from fdopen (Windows) ...'
                install -d .ci/sd4/bs/bin
                wget -O "$setup_WORKSPACE"/.ci/sd4/opam64.tar.xz https://github.com/fdopen/opam-repository-mingw/releases/download/0.0.0.2/opam64.tar.xz

                # this stalls: tar xvCfJ "$setup_WORKSPACE"/.ci/sd4 "$setup_WORKSPACE"/.ci/sd4/opam64.tar.xz
                xz -v -d "$setup_WORKSPACE"/.ci/sd4/opam64.tar.xz
                tar xvCf .ci/sd4 .ci/sd4/opam64.tar

                rm -rf "$setup_WORKSPACE"/.ci/sd4/bs/bin/Opam.Runtime.amd64
                mv -v "$setup_WORKSPACE"/.ci/sd4/opam64/bin/Opam.Runtime.amd64/ "$setup_WORKSPACE"/.ci/sd4/bs/bin/
                mv -v "$setup_WORKSPACE"/.ci/sd4/opam64/bin/opam.exe "$setup_WORKSPACE"/.ci/sd4/bs/bin/
                mv -v "$setup_WORKSPACE"/.ci/sd4/opam64/bin/opam-installer.exe "$setup_WORKSPACE"/.ci/sd4/bs/bin/

                # diagnostics
                ldd "$setup_WORKSPACE"/.ci/sd4/bs/bin/opam.exe
                ldd "$setup_WORKSPACE"/.ci/sd4/bs/bin/opam-installer.exe

                runit_BOOTSTRAPPED=1
                ;;
            esac
        fi
    fi

    #   Bootstrap from historical release
    if [ "$runit_BOOTSTRAPPED" = 0 ] && [ "$bootstrap_opam_version" != "os" ]; then
        install -d .ci/sd4/bs
        cd .ci/sd4/bs

        if [ ! -e version ] || [ "$(cat version)" != "$bootstrap_opam_version" ]; then
            echo 'Bootstrap opam from historical release (non-Windows; Windows non-fdopen) ...'
            if command -v curl > /dev/null 2> /dev/null; then
                curl -L -o opam.tar.gz "https://github.com/diskuv/dkml-component-opam/releases/download/v${bootstrap_opam_version}/dkml-component-staging-opam.tar.gz"
            else
                wget -O opam.tar.gz "https://github.com/diskuv/dkml-component-opam/releases/download/v${bootstrap_opam_version}/dkml-component-staging-opam.tar.gz"
            fi
            tar tvfz opam.tar.gz
            tar xfz opam.tar.gz "./staging-files/${dkml_host_abi}/"
            rm -rf bin/
            mv "staging-files/${dkml_host_abi}/bin" .
            rm -rf "${abi_pattern}"
            printf "%s" "${bootstrap_opam_version}" >version
        fi

        rm -f opam.tar.gz
        cd ../../..

        runit_BOOTSTRAPPED=1
    fi

    #   Bootstrap from package manager or GitHub ocaml/opam release
    case "$runit_BOOTSTRAPPED,$bootstrap_opam_version,$dkml_host_abi" in
    0,os,darwin_*)
        if ! command -v opam; then
            echo 'Bootstrap opam from package manager (macOS) ...'
            brew install gpatch
            brew install opam
        fi
        runit_BOOTSTRAPPED=1
        ;;
    0,os,linux_x86)
        if [ ! -x .ci/sd4/bs/bin/opam ]; then
            echo 'Bootstrap opam from GitHub ocaml/opam release (Linux x86) ...'
            install -d .ci/sd4/bs/bin
            if command -v curl > /dev/null 2> /dev/null; then
                curl -L -o .ci/sd4/bs/bin/opam.tmp https://github.com/ocaml/opam/releases/download/2.1.2/opam-2.1.2-i686-linux
            else
                wget -O .ci/sd4/bs/bin/opam.tmp https://github.com/ocaml/opam/releases/download/2.1.2/opam-2.1.2-i686-linux
            fi
            sha512_check=$(openssl sha512 2>&1 </dev/null | cut -f 2 -d ' ')
            if [ "$SHA512_DEVNULL" = "$sha512_check" ]; then
                sha512=$(openssl sha512 ".ci/sd4/bs/bin/opam.tmp" 2>/dev/null | cut -f 2 -d ' ')
                check="85a480d60e09a7d37fa0d0434ed97a3187434772ceb4e7e8faa5b06bc18423d004af3ad5849c7d35e72dca155103257fd6b1178872df8291583929eb8f884b6a"
                test "$sha512" = "$check"
                chmod +x .ci/sd4/bs/bin/opam.tmp
                mv .ci/sd4/bs/bin/opam.tmp .ci/sd4/bs/bin/opam
            else
                echo "openssl 512 option unsupported."
                exit 61
            fi
        fi
        runit_BOOTSTRAPPED=1
        ;;
    0,os,linux_x86_64)
        if [ ! -x .ci/sd4/bs/bin/opam ]; then
            echo 'Bootstrap opam from GitHub ocaml/opam release (Linux x86_64) ...'
            install -d .ci/sd4/bs/bin
            if command -v curl > /dev/null 2> /dev/null; then
                curl -L -o .ci/sd4/bs/bin/opam.tmp https://github.com/ocaml/opam/releases/download/2.1.2/opam-2.1.2-x86_64-linux
            else
                wget -O .ci/sd4/bs/bin/opam.tmp https://github.com/ocaml/opam/releases/download/2.1.2/opam-2.1.2-x86_64-linux
            fi
            sha512_check=$(openssl sha512 2>&1 </dev/null | cut -f 2 -d ' ')
            if [ "$SHA512_DEVNULL" = "$sha512_check" ]; then
                sha512=$(openssl sha512 ".ci/sd4/bs/bin/opam.tmp" 2>/dev/null | cut -f 2 -d ' ')
                check="c0657ecbd4dc212587a4da70c5ff0402df95d148867be0e1eb1be8863a2851015f191437c3c99b7c2b153fcaa56cac99169c76ec94c5787750d7a59cd1fbb68b"
                test "$sha512" = "$check"
                chmod +x .ci/sd4/bs/bin/opam.tmp
                mv .ci/sd4/bs/bin/opam.tmp .ci/sd4/bs/bin/opam
            else
                echo "openssl 512 option unsupported."
                exit 61
            fi
        fi
        runit_BOOTSTRAPPED=1
        ;;
    esac
}
section_begin bootstrap-opam 'Bootstrap opam'
do_bootstrap
section_end bootstrap-opam

# Start environment distribution tarball
#   We use .tar rather than .tar.gz/.tar.bz2 because we can repeatedly add to an uncompressed .tar. But we need to
#   start with an empty tarball since some tar programs will only add ('tar rf xyz.tar') to an existing .tar.
install -d .ci/sd4/dist
tar cf .ci/sd4/dist/run-with-env.tar -T /dev/null

do_get_dockcross() {
    if [ -n "${dockcross_image:-}" ]; then
        # The dockcross script is super-slow
        section_begin get-dockcross 'Get dockcross binary (ManyLinux)'
        install -d .ci/sd4
        #   shellcheck disable=SC2086
        docker run ${dockcross_run_extra_args:-} --rm "${dockcross_image_custom_prefix:-}${dockcross_image:-}" >.ci/sd4/dockcross.gen

        # PROBLEM 1
        # ---------
        # Super-annoying stderr output from dockcross at line:
        #    tty -s && [ -z "$MSYS" ] && TTY_ARGS=-ti
        # When there is no tty, get:
        #   tty: ignoring all arguments
        #   not a tty
        # So replace 'tty -s &&' with 'false &&'
        sed 's/tty -s &&/false \&\&/' .ci/sd4/dockcross.gen >.ci/sd4/dockcross-real
        rm -f .ci/sd4/dockcross.gen
        chmod +x .ci/sd4/dockcross-real

        # PROBLEM 2
        # ---------
        # By default dockcross for ManyLinux will chown -R all python packages; super-slow (~10 seconds)!
        # Confer: https://github.com/dockcross/dockcross/blob/master/manylinux-common/pre_exec.sh
        # That kills speed for any repetitive dockcross invocation.
        #
        # BUT it is unnecessary to chown -R when the current user is root, because inside the Docker container
        # the files are already root!
        #
        # The chown -R (within pre_exec.sh) is not run when the user ids are not passed in.
        # Confer: https://github.com/dockcross/dockcross/blob/96d87416f639af0204bdd42553e4b99315ca8476/imagefiles/entrypoint.sh#L21-L53
        #
        # So explicitly call the entrypoint if root!
        if echo "${dockcross_run_extra_args:-}" | grep -q linux/386; then
            # https://github.com/dockcross/dockcross/blob/master/linux-x86/linux32-entrypoint.sh
            # But only when `--platform linux/386` because the container image may be overridden.
            dockcross_entrypoint=/dockcross/linux32-entrypoint.sh
        else
            dockcross_entrypoint=/dockcross/entrypoint.sh
        fi
        cat > .ci/sd4/dockcross <<EOF
#!/bin/bash
set -euf
BUILDER_UID="\$( id -u )"
BUILDER_GID="\$( id -g )"
if [ "\$BUILDER_UID" = 0 ] && [ "\$BUILDER_GID" = 0 ]; then
    # ---------- Start of dockcross script snippet -------
    # Verbatim from
    # https://github.com/dockcross/dockcross/blob/96d87416f639af0204bdd42553e4b99315ca8476/imagefiles/dockcross#L175-L204
    # except 1) disabling of USER_IDS

    # Bash on Ubuntu on Windows
    UBUNTU_ON_WINDOWS=\$([ -e /proc/version ] && grep -l Microsoft /proc/version || echo "")
    # MSYS, Git Bash, etc.
    MSYS=\$([ -e /proc/version ] && grep -l MINGW /proc/version || echo "")
    # CYGWIN
    CYGWIN=\$([ -e /proc/version ] && grep -l CYGWIN /proc/version || echo "")

    #if [ -z "\$UBUNTU_ON_WINDOWS" -a -z "\$MSYS" -a "\$OCI_EXE" != "podman" ]; then
    #    USER_IDS=(-e BUILDER_UID="\$( id -u )" -e BUILDER_GID="\$( id -g )" -e BUILDER_USER="\$( id -un )" -e BUILDER_GROUP="\$( id -gn )")
    #fi

    # Change the PWD when working in Docker on Windows
    if [ -n "\$UBUNTU_ON_WINDOWS" ]; then
        WSL_ROOT="/mnt/"
        CFG_FILE=/etc/wsl.conf
            if [ -f "\$CFG_FILE" ]; then
                    CFG_CONTENT=\$(cat \$CFG_FILE | sed -r '/[^=]+=[^=]+/!d' | sed -r 's/\s+=\s/=/g')
                    eval "\$CFG_CONTENT"
                    if [ -n "\$root" ]; then
                            WSL_ROOT=\$root
                    fi
            fi
        HOST_PWD=\`pwd -P\`
        HOST_PWD=\${HOST_PWD/\$WSL_ROOT//}
    elif [ -n "\$MSYS" ]; then
        HOST_PWD=\$PWD
        HOST_PWD=\${HOST_PWD/\//}
        HOST_PWD=\${HOST_PWD/\//:\/}
    elif [ -n "\$CYGWIN" ]; then
        for f in pwd readlink cygpath ; do
            test -n "\$(type "\${f}" )" || { echo >&2 "Missing functionality (\${f}) (in cygwin)." ; exit 1 ; } ;
        done ;
        HOST_PWD="\$( cygpath -w "\$( readlink -f "\$( pwd ;)" ; )" ; )" ;
    else
        HOST_PWD=\$PWD
        [ -L \$HOST_PWD ] && HOST_PWD=\$(readlink \$HOST_PWD)
    fi

    # ---------- End of dockcross script snippet -------

    # Handle: dockcross --args "-v X:Y --platform P"
    ARGS=
    if [ "\$#" -ge 1 ] && [ "\$1" = "--args" ]; then
        shift
        ARGS=\$1
        shift
    fi

    # Directly invoke entrypoint
    exec docker run --entrypoint /bin/bash \
        --rm \
        \${ARGS:-} \
         -v "\$HOST_PWD":/work \
        ${dockcross_image_custom_prefix:-}${dockcross_image:-} ${dockcross_entrypoint} "\$@"
else
    HERE=\$(dirname "\$0")
    HERE=\$(cd "\$HERE" && pwd)
    exec "\$HERE/dockcross-real" "\$@"
fi
EOF
        chmod +x .ci/sd4/dockcross

        # Bundle for consumers of setup-dkml.yml
        do_tar_rf .ci/sd4/dist/run-with-env.tar .ci/sd4/dockcross .ci/sd4/dockcross-real

        section_end get-dockcross
    fi
}
do_get_dockcross

if [ -n "${dockcross_image:-}" ]; then
    # rsync needs to be available, even after Docker container disappears
    if [ ! -e .ci/sd4/bs/bin/rsync ]; then
        section_begin get-opam-prereqs-in-dockcross 'Get Opam prerequisites (ManyLinux)'
        install -d .ci/sd4/bs/bin
        # Install rsync with 'yum' (ManyLinux) or 'apt' (dockcross/linux-x64, etc.)
        # if not present.
        #   shellcheck disable=SC2016
        .ci/sd4/dockcross --args "${dockcross_run_extra_args:-}" sh -c 'if ! command -v rsync; then if command -v yum; then sudo yum install -y rsync; else sudo apt-get install -qq -o=Dpkg::Use-Pty=0 -y rsync; fi; fi && install $(command -v rsync) .ci/sd4/bs/bin'
        section_end get-opam-prereqs-in-dockcross
    fi
fi

# Opam prerequisites for using opam (not for installing opam)

{
    if [ -n "${docker_runner:-}" ]; then
        # rsync needs to be available, even after Docker container disappears
        if [ ! -e .ci/sd4/bs/bin/rsync.deps ]; then
            section_begin get-opam-prereqs-in-docker 'Get Opam prerequisites (Linux Docker)'
            install -d .ci/sd4/bs/bin
            ${docker_runner} sh -c '
            apt-get update -qq -o=Dpkg::Use-Pty=0 &&
            apt-get install -qq -o=Dpkg::Use-Pty=0 -y rsync &&
            ldd /usr/bin/rsync &&
            ls -l /lib/i386-linux-gnu/libpopt.so.0 /lib/i386-linux-gnu/libacl.so.1 /lib/i386-linux-gnu/libattr.so.1 &&
            tar cCfhz / /work/.ci/sd4/bs/bin/deps.tar.gz /usr/bin/rsync /lib/i386-linux-gnu/libpopt.so.0
        '
            touch .ci/sd4/bs/bin/rsync.deps
            section_end get-opam-prereqs-in-docker
        fi
    fi

    # Bundle Opam prerequisites (ManyLinux or Linux Docker)
    if [ -n "${docker_runner:-}" ] || [ -n "${dockcross_image:-}" ]; then
        # Bundle for consumers of setup-dkml.yml
        do_tar_rf .ci/sd4/dist/run-with-env.tar .ci/sd4/bs/bin/rsync
    fi
}

# Get Opam Cache
do_get_opam_cache() {
    if [ "$unix_opam_root_cacheable" = "$unix_opam_root" ]; then return; fi
    if [ ! -e "$unix_opam_root_cacheable" ]; then return; fi
    section_begin get-opam-cache "Transferring Opam cache to $original_opam_root_cacheable"
    echo Starting transfer # need some output or GitLab CI will not display the section duration
    transfer_dir "$unix_opam_root_cacheable" "$unix_opam_root"
    echo Finished transfer
    section_end get-opam-cache
}
do_get_opam_cache

# Setup Opam

do_write_opam_scripts() {
    case "${FDOPEN_OPAMEXE_BOOTSTRAP:-},$dkml_host_abi" in
    true,windows_*)
        # With fdopen's opam.exe, 'os-distribution = "cygwinports"'. But native Windows opam.exe has 'os-distribution = "win32"'.
        # But on Windows we always want MSYS2 or native Windows libraries, not Cygwin. If cygwinports then
        # code like https://github.com/ocaml/opam-repository/blob/08cbb8258bd4bf30cd6f307c958911a29d537b54/packages/conf-pkg-config/conf-pkg-config.2/opam#L36
        # will fail. So always set 'os-distribution = "win32"' on Windows.
        PATCH_OS_DISTRIBUTION_WIN32=true
        # With fdopen's opam.exe, no 'exe = ".exe"' is set because Cygwin does not need file extensions.
        # Native Windows requires a .exe extension.
        PATCH_EXE_WIN32=true
        ;;
    *)
        PATCH_OS_DISTRIBUTION_WIN32=false
        PATCH_EXE_WIN32=false
        ;;
    esac

    # ---------------------
    # Empty opam repository
    # ---------------------

    install -d .ci/sd4/eor
    cat >.ci/sd4/eor/repo <<EOF
opam-version: "2.0"
browse: "https://opam.ocaml.org/pkg/"
upstream: "https://github.com/ocaml/opam-repository/tree/master/"
EOF

    # ---------------
    # Create Opam troubleshooting script
    #   Dump logs modified within the last 4 hours
    # ---------------

    cat >.ci/sd4/troubleshoot-opam.sh <<EOF
#!/bin/sh
set -euf
OPAMROOT=\$1
shift
if find . -maxdepth 0 -mmin -240 2>/dev/null >/dev/null; then
    FINDARGS="-mmin -240" # is -mmin supported? BSD (incl. macOS), MSYS2, GNU
else
    FINDARGS="-mtime -1" # use 1 day instead. Solaris
fi
printf "\n\n========= [START OF TROUBLESHOOTING] ===========\n\n" >&2
find "\$OPAMROOT"/log -mindepth 1 -maxdepth 1 \$FINDARGS -name "*.out" ! -name "log-*.out" ! -name "ocaml-variants-*.out" | while read -r dump_on_error_LOG; do
    dump_on_error_BLOG=\$(basename "\$dump_on_error_LOG")
    printf "\n\n========= [TROUBLESHOOTING] %s ===========\n\n" "\$dump_on_error_BLOG" >&2
    awk -v BLOG="\$dump_on_error_BLOG" '{print "[" BLOG "]", \$0}' "\$dump_on_error_LOG" >&2
done
printf "\nScroll up to see the [TROUBLESHOOTING] logs that begin at the [START OF TROUBLESHOOTING] line\n" >&2
EOF

    chmod +x .ci/sd4/troubleshoot-opam.sh
    do_tar_rf .ci/sd4/dist/run-with-env.tar .ci/sd4/troubleshoot-opam.sh

    # ---------------
    # Create Opam support scripts (not needed for all platforms)
    #   The PATH to find opam must work internally in setup-dkml.yml (sd4/bs/bin) and
    #   by consumers of setup-dkml.yml (sd4/opamexe)
    # ---------------

    USER_ID=$(id -u)
    GROUP_ID=$(id -g)
    USER_NAME=$(id -un)
    GROUP_NAME=$(id -gn)

    case "${opam_root}" in
    /* | ?:*) # /a/b/c or C:\Windows
        validate_supports_docker() {
            echo "Docker only supported with relative paths for the opam root, not: ${opam_root}" >&2
            exit 3
        }
        ;;
    *) # relative path
        validate_supports_docker() {
            true
        }
        cat >.ci/sd4/run-in-docker <<EOF
#!/bin/sh
set -euf
export PATH="/work/.ci/local/bin:/work/.ci/sd4/bs/bin:/work/.ci/sd4/opamexe:\$PATH"
export OPAMROOT=/work/${opam_root}
export OPAMROOTISOK=1
if [ "${PATCH_OS_DISTRIBUTION_WIN32}" = true ]; then export OPAMVAR_os_distribution=win32; fi
if [ "${PATCH_EXE_WIN32}" = true ]; then export OPAMVAR_exe=.exe; fi

# Reset environment so no conflicts with a parent Opam or OCaml system
unset OPAM_SWITCH_PREFIX
unset OPAMSWITCH
unset CAML_LD_LIBRARY_PATH
unset OCAMLLIB
unset OCAML_TOPLEVEL_PATH

prog=\$1
shift

# Optionally skip troubleshooting
troubleshooting=1
if [ "\$#" -ge 1 ] && [ "\$prog" = opam ] && [ "\$1" = "--no-troubleshooting" ]; then
    shift
    troubleshooting=0
fi

echo "Running inside Docker container: \$prog \$*" >&2
set +e
"\$prog" "\$@"
exitcode=\$?
if [ \$troubleshooting = 1 ] && [ \$prog = opam ]; then
    [ \$exitcode = 0 ] || "/work/.ci/sd4/troubleshoot-opam.sh" \$OPAMROOT
fi
exit \$exitcode
EOF
        chmod +x .ci/sd4/run-in-docker
        ;;
    esac

    cat >.ci/sd4/deescalate <<EOF
#!/bin/sh
set -euf

if [ -e /work/.ci/sd4/bs/bin/deps.tar.gz ]; then
    tar xCfz / /work/.ci/sd4/bs/bin/deps.tar.gz
fi

groupadd -g ${GROUP_ID} ${GROUP_NAME}
useradd -l -m -u ${USER_ID} -g ${GROUP_ID} ${USER_NAME}
exec runuser -u ${USER_NAME} -g ${GROUP_NAME} -- "\$@"
EOF

    chmod +x .ci/sd4/deescalate

    # -----------------------------------
    # Create run-with-env
    # -----------------------------------

    install -d .ci/sd4/dist

    if [ -x .ci/sd4/dockcross ]; then
        # Adding empty dockcross root volume avoids:
        #    cp: target ‘/home/root/’ is not a directory
        #    chown: cannot access ‘/home/root’: No such file or directory
        # from https://github.com/dockcross/dockcross/blob/96d87416f639af0204bdd42553e4b99315ca8476/imagefiles/entrypoint.sh#L31-L32
        install -d .ci/sd4/edr

        cat >.ci/sd4/run-with-env <<EOF
#!/bin/sh
set -euf

HERE=\$(dirname "\$0")
HERE=\$(cd "\$HERE" && pwd)
PROJECT_DIR=\$(cd "\$HERE"/../.. && pwd)

# Optionally enable terminal if and only if '-it' option given
termargs=
if [ "\$#" -ge 1 ] && [ "\$1" = "-it" ]; then
    shift
    termargs=-it
fi

exec bash "\${PROJECT_DIR}"/.ci/sd4/dockcross --args "\${termargs} -v \${PROJECT_DIR}/.ci/sd4/edr:/home/root ${dockcross_run_extra_args:-}" /work/.ci/sd4/run-in-docker "\$@"
EOF
        chmod +x .ci/sd4/run-with-env

        validate_supports_docker

        # Bundle for consumers of setup-dkml.yml
        echo '__ run-in-docker __' >&2
        cat .ci/sd4/run-in-docker >&2
        echo '___________________' >&2
        do_tar_rf .ci/sd4/dist/run-with-env.tar .ci/sd4/run-with-env .ci/sd4/run-in-docker .ci/sd4/edr

    elif [ -n "${docker_runner:-}" ]; then

        cat >.ci/sd4/run-with-env <<EOF
#!/bin/sh
set -euf
exec ${docker_runner:-} /work/.ci/sd4/deescalate /work/.ci/sd4/run-in-docker "\$@"
EOF
        chmod +x .ci/sd4/run-with-env

        validate_supports_docker

        # Bundle for consumers of setup-dkml.yml
        echo '__ run-in-docker __' >&2
        cat .ci/sd4/run-in-docker >&2
        echo '________________________' >&2
        echo '__ deescalate __' >&2
        cat .ci/sd4/deescalate >&2
        echo '________________' >&2
        do_tar_rf .ci/sd4/dist/run-with-env.tar .ci/sd4/run-with-env .ci/sd4/run-in-docker .ci/sd4/deescalate

    else

        cat >.ci/sd4/run-with-env <<EOF
#!/bin/sh
set -euf

HERE=\$(dirname "\$0")
HERE=\$(cd "\$HERE" && pwd)
PROJECT_DIR=\$(cd "\$HERE"/../.. && pwd)

export PATH="\${PROJECT_DIR}/.ci/local/bin:\${PROJECT_DIR}/.ci/sd4/bs/bin:\${PROJECT_DIR}/.ci/sd4/opamexe:\$PATH"
export OPAMROOT='${opam_root}'
export OPAMROOTISOK=1
if [ "${PATCH_OS_DISTRIBUTION_WIN32}" = true ]; then export OPAMVAR_os_distribution=win32; fi
if [ "${PATCH_EXE_WIN32}" = true ]; then export OPAMVAR_exe=.exe; fi

# Reset environment so no conflicts with a parent Opam or OCaml system
unset OPAM_SWITCH_PREFIX
unset OPAMSWITCH
unset CAML_LD_LIBRARY_PATH
unset OCAMLLIB
unset OCAML_TOPLEVEL_PATH

prog=\$1
shift

# Optionally skip troubleshooting
troubleshooting=1
if [ "\$#" -ge 1 ] && [ "\$prog" = opam ] && [ "\$1" = "--no-troubleshooting" ]; then
    shift
    troubleshooting=0
fi

echo "Running: \$prog \$*" >&2
set +e
"\$prog" "\$@"
exitcode=\$?
if [ \$troubleshooting = 1 ] && [ \$prog = opam ]; then
    [ \$exitcode = 0 ] || "\${PROJECT_DIR}/.ci/sd4/troubleshoot-opam.sh" \$OPAMROOT
fi
exit \$exitcode
EOF
        chmod +x .ci/sd4/run-with-env

        # Bundle for consumers of setup-dkml.yml
        do_tar_rf .ci/sd4/dist/run-with-env.tar .ci/sd4/run-with-env

    fi
    echo '__ run-with-env __' >&2
    cat .ci/sd4/run-with-env >&2
    echo '__________________' >&2

    # ------
    # cmdrun
    # ------

    install -d .ci/sd4/opamrun
    cat >.ci/sd4/opamrun/cmdrun <<EOF
#!/bin/sh
set -euf

HERE=\$(dirname "\$0")
HERE=\$(cd "\$HERE" && pwd)
PROJECT_DIR=\$(cd "\$HERE"/../../.. && pwd)

# Add MSVC compiler environment if available
if [ -e "\${PROJECT_DIR}/.ci/sd4/msvcenv" ]; then
    _oldpath="\$PATH"
    # shellcheck disable=SC1091
    . "\${PROJECT_DIR}/.ci/sd4/msvcenv"
    PATH="\$PATH:\$_oldpath"

    # MSVC (link.exe) needs a TMP as well.
    # Confer: https://docs.microsoft.com/en-us/cpp/build/reference/linking?view=msvc-170#link-environment-variables
    if [ -z "\${TMP:-}" ]; then
        # GitHub Actions as of 2022-10 does not set TMP. GitLab CI/CD does.
        TMP="\$RUNNER_TEMP"
    fi
    export TMP
    if [ -x /usr/bin/cygpath ]; then
        TMP=\$(/usr/bin/cygpath -aw "\$TMP")
    fi
fi

# Windows
if [ -n "\${COMSPEC:-}" ]; then
    # We must place MSYS2 in front of path so that MSYS2
    # tar.exe is used instead of Windows tar.exe.
    PATH="/usr/bin:\$PATH"
fi

exec "\${PROJECT_DIR}/.ci/sd4/run-with-env" "\$@"
EOF
    chmod +x .ci/sd4/opamrun/cmdrun

    # -------
    # opamrun
    # -------

    install -d .ci/sd4/opamrun
    cat >.ci/sd4/opamrun/opamrun <<EOF
#!/bin/sh
set -euf

HERE=\$(dirname "\$0")
HERE=\$(cd "\$HERE" && pwd)
PROJECT_DIR=\$(cd "\$HERE"/../../.. && pwd)

exec "\${PROJECT_DIR}/.ci/sd4/opamrun/cmdrun" opam "\$@"
EOF
    chmod +x .ci/sd4/opamrun/opamrun

    # Bundle for consumers of setup-dkml.yml
    do_tar_rf .ci/sd4/dist/run-with-env.tar .ci/sd4/opamrun
}
section_begin 'write-opam-scripts' 'Write opam scripts'
do_write_opam_scripts
section_end 'write-opam-scripts'

# Expose opamrun (also used for consumers of setup-dkml.yml) to GitHub
if [ -n "${GITHUB_PATH:-}" ]; then
    opamrunabs="$setup_WORKSPACE/.ci/sd4/opamrun"
    if [ -x /usr/bin/cygpath ]; then opamrunabs=$(/usr/bin/cygpath -aw "$opamrunabs"); fi
    echo "$opamrunabs" >>"$GITHUB_PATH"
    # Special case: GITHUB_PATH does not influence msys2.CMD of msys2/setup-msys2@v2, so place in real MSYS2 PATH
    if [ -n "${MSYSTEM:-}" ]; then
        install -d /usr/local/bin
        install .ci/sd4/opamrun/opamrun /usr/local/bin/opamrun
    fi
fi

# Place opamrun in the immediate PATH
PATH="$setup_WORKSPACE/.ci/sd4/opamrun:$PATH"

#   Complicated Opam sequence is because:
#   1. Opam's default curl does not work on Windows,
#      and `opam init` does not provide a way to change it (TODO: need
#      a PR!).
#   2. We have to separate the Opam download cache from the other Opam
#      caches
if [ "${SKIP_OPAM_MODIFICATIONS:-}" = "false" ] && [ ! -s "$opam_root/.ci.root-init" ]; then # non-empty init file so can be cached irrespective of existence
    section_begin opam-init 'Initialize opam root'

    # Clear any partial previous attempt
    rm -rf "$opam_root"

    case "$dkml_host_abi,${in_docker:-}" in
    windows_*,*)
        eor=$(cygpath -am "$setup_WORKSPACE"/.ci/sd4/eor)
        opamrun init --disable-sandboxing --no-setup --kind local --bare "$eor"
        case "$(opamrun --version)" in
        2.0.*) echo 'download-command: wget' >>"$opam_root/config" ;;
        *) opamrun option --yes --global download-command=wget ;;
        esac
        ;;
    *,true)
        opamrun init --disable-sandboxing --no-setup --kind local --bare "/work/.ci/sd4/eor"
        ;;
    *)
        opamrun init --disable-sandboxing --no-setup --kind local --bare "$setup_WORKSPACE/.ci/sd4/eor"
        ;;
    esac
    echo yes > "$opam_root/.ci.root-init"

    section_end opam-init
fi

if [ "${SKIP_OPAM_MODIFICATIONS:-}" = "false" ]; then
    section_begin opam-vars "Summary: opam global variables"
    opamrun --no-troubleshooting var --global || true
    section_end opam-vars
fi

# Build OCaml

do_switch_create() {
    do_switch_create_NAME=$1
    shift

    section_begin "switch-create-$do_switch_create_NAME" "Create opam switch '$do_switch_create_NAME'"
    # Create, or recreate, the Opam switch. The Opam switch should not be
    # cached except for the compiler (confer docs for setup-ocaml GitHub
    # Action) which is the 'dkml' switch (or the 'two' switch).
    # Check if the switch name is present in the Opam root (which may come from cache)
    NOMINALLY_PRESENT=false
    if opamrun switch list --short | grep "^${do_switch_create_NAME}\$"; then NOMINALLY_PRESENT=true; fi

    # Check if the switch is actually present in case of cache incoherence
    # or corrupt Opam state that could result in:
    #   Error:  No config file found for switch dkml. Switch broken?
    if [ $NOMINALLY_PRESENT = true ] && [ ! -e "$opam_root/$do_switch_create_NAME/.opam-switch/switch-config" ]; then
        # Remove the switch name from Opam root, and any partial switch state.
        # Ignore inevitable warnings/failure about missing switch.
        opamrun --no-troubleshooting switch remove "$do_switch_create_NAME" --yes || true
        rm -rf "${opam_root:?}/$do_switch_create_NAME"
        NOMINALLY_PRESENT=false
    fi

    if [ $NOMINALLY_PRESENT = false ]; then
        opamrun switch create "$do_switch_create_NAME" --empty --yes
    fi
    section_end "switch-create-$do_switch_create_NAME"
}
if [ "${SKIP_OPAM_MODIFICATIONS:-}" = "false" ]; then
    do_switch_create dkml
    if [ "${SECONDARY_SWITCH:-}" = "true" ]; then
        do_switch_create two
    else
        section_begin "switch-create-two" "Create empty opam switch 'two'"
        # Always create a secondary switch ... just empty. Avoid problems with cache content missing
        # and idempotency.
        opamrun --no-troubleshooting switch remove two --yes || true
        rm -rf "$opam_root/two"
        opamrun switch create two --empty --yes
        section_end "switch-create-two"
    fi
fi

do_switch_active() {
    section_begin "switch-active" "Set dkml as active switch"
    opamrun switch set dkml --yes
    section_end "switch-active"
}
if [ "${SKIP_OPAM_MODIFICATIONS:-}" = "false" ]; then
    do_switch_active
fi

do_opam_repositories_add() {
    section_begin "opam-repo-add" "Add 'diskuv' opam repository"
    if ! opamrun --no-troubleshooting repository list -s | grep '^diskuv'; then
        opamrun repository add diskuv "git+https://github.com/diskuv/diskuv-opam-repository.git#${DISKUV_OPAM_REPOSITORY:-$DEFAULT_DISKUV_OPAM_REPOSITORY_TAG}" --yes --dont-select
    fi
    section_end "opam-repo-add"
}
if [ "${SKIP_OPAM_MODIFICATIONS:-}" = "false" ]; then
    do_opam_repositories_add
fi

do_opam_repositories_config() {
    do_opam_repositories_config_NAME=$1
    shift

    section_begin "opam-repo-$do_opam_repositories_config_NAME" "Attach repositories to $do_opam_repositories_config_NAME"

    if [ ! -s "$opam_root/.ci.$do_opam_repositories_config_NAME.repo-init" ]; then # non-empty init file so can be cached irrespective of existence
        opamrun --no-troubleshooting repository remove default --switch "$do_opam_repositories_config_NAME" --yes || true
        opamrun --no-troubleshooting repository remove diskuv --switch "$do_opam_repositories_config_NAME" --yes || true
        opamrun repository add default --switch "$do_opam_repositories_config_NAME" --yes
        opamrun repository add diskuv --switch "$do_opam_repositories_config_NAME" --yes
        echo yes > "$opam_root/.ci.$do_opam_repositories_config_NAME.repo-init"
    fi

    section_end "opam-repo-$do_opam_repositories_config_NAME"
}
if [ "${SKIP_OPAM_MODIFICATIONS:-}" = "false" ]; then
    do_opam_repositories_config dkml
    if [ "${SECONDARY_SWITCH:-}" = "true" ]; then
        do_opam_repositories_config two
    fi
fi

do_opam_repositories_update() {
    section_begin "opam-repo-update" "Update opam repositories"
    # The default repository may be the initial 'eor' (empty) repository
    opamrun repository set-url default https://opam.ocaml.org --yes
    # Always set the `diskuv` repository url since it can change
    opamrun repository set-url diskuv "git+https://github.com/diskuv/diskuv-opam-repository.git#${DISKUV_OPAM_REPOSITORY:-$DEFAULT_DISKUV_OPAM_REPOSITORY_TAG}" --yes --dont-select
    # Update both `default` and `diskuv` Opam repositories
    opamrun update default diskuv
    section_end "opam-repo-update"
}
if [ "${SKIP_OPAM_MODIFICATIONS:-}" = "false" ]; then
    do_opam_repositories_update
fi

do_pins() {
    do_pins_NAME=$1
    shift

    section_begin "opam-pins-$do_pins_NAME" "Opam pins for $do_pins_NAME switch"
    ### BEGIN pin-adds. DO NOT EDIT THE LINES IN THIS SECTION
    # Managed by bump-packages.cmake
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version astring "${PIN_ASTRING}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version base "${PIN_BASE}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version base64 "${PIN_BASE64}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version bigarray-compat "${PIN_BIGARRAY_COMPAT}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version bos "${PIN_BOS}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version camlp-streams "${PIN_CAMLP_STREAMS}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version chrome-trace "${PIN_CHROME_TRACE}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version cmdliner "${PIN_CMDLINER}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version conf-dkml-sys-opam "${PIN_CONF_DKML_SYS_OPAM}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version conf-pkg-config "${PIN_CONF_PKG_CONFIG}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version conf-sqlite3 "${PIN_CONF_SQLITE3}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version cppo "${PIN_CPPO}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version crunch "${PIN_CRUNCH}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version csexp "${PIN_CSEXP}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version cstruct "${PIN_CSTRUCT}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ctypes "${PIN_CTYPES}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ctypes-foreign "${PIN_CTYPES_FOREIGN}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version cudf "${PIN_CUDF}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version digestif "${PIN_DIGESTIF}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version diskuvbox "${PIN_DISKUVBOX}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-apps "${PIN_DKML_APPS}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-base-compiler "${PIN_DKML_BASE_COMPILER}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-build-desktop "${PIN_DKML_BUILD_DESKTOP}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-c-probe "${PIN_DKML_C_PROBE}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-compiler-env "${PIN_DKML_COMPILER_ENV}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-compiler-src "${PIN_DKML_COMPILER_SRC}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-component-common-desktop "${PIN_DKML_COMPONENT_COMMON_DESKTOP}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-component-common-opam "${PIN_DKML_COMPONENT_COMMON_OPAM}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-component-common-unixutils "${PIN_DKML_COMPONENT_COMMON_UNIXUTILS}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-component-ocamlcompiler-common "${PIN_DKML_COMPONENT_OCAMLCOMPILER_COMMON}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-component-ocamlcompiler-network "${PIN_DKML_COMPONENT_OCAMLCOMPILER_NETWORK}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-component-ocamlcompiler-offline "${PIN_DKML_COMPONENT_OCAMLCOMPILER_OFFLINE}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-component-offline-desktop-full "${PIN_DKML_COMPONENT_OFFLINE_DESKTOP_FULL}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-component-offline-opamshim "${PIN_DKML_COMPONENT_OFFLINE_OPAMSHIM}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-component-offline-unixutils "${PIN_DKML_COMPONENT_OFFLINE_UNIXUTILS}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-component-staging-desktop-full "${PIN_DKML_COMPONENT_STAGING_DESKTOP_FULL}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-component-staging-dkmlconfdir "${PIN_DKML_COMPONENT_STAGING_DKMLCONFDIR}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-component-staging-ocamlrun "${PIN_DKML_COMPONENT_STAGING_OCAMLRUN}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-component-staging-opam32 "${PIN_DKML_COMPONENT_STAGING_OPAM32}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-component-staging-opam64 "${PIN_DKML_COMPONENT_STAGING_OPAM64}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-component-staging-unixutils "${PIN_DKML_COMPONENT_STAGING_UNIXUTILS}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-component-staging-withdkml "${PIN_DKML_COMPONENT_STAGING_WITHDKML}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-component-xx-console "${PIN_DKML_COMPONENT_XX_CONSOLE}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-exe "${PIN_DKML_EXE}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-exe-lib "${PIN_DKML_EXE_LIB}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-install "${PIN_DKML_INSTALL}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-install-installer "${PIN_DKML_INSTALL_INSTALLER}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-install-runner "${PIN_DKML_INSTALL_RUNNER}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-installer-ocaml-common "${PIN_DKML_INSTALLER_OCAML_COMMON}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-installer-ocaml-offline "${PIN_DKML_INSTALLER_OCAML_OFFLINE}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-package-console "${PIN_DKML_PACKAGE_CONSOLE}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-runtime-common "${PIN_DKML_RUNTIME_COMMON}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-runtime-common-native "${PIN_DKML_RUNTIME_COMMON_NATIVE}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-runtime-distribution "${PIN_DKML_RUNTIME_DISTRIBUTION}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-runtimelib "${PIN_DKML_RUNTIMELIB}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-runtimescripts "${PIN_DKML_RUNTIMESCRIPTS}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dkml-workflows "${PIN_DKML_WORKFLOWS}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dune "${PIN_DUNE}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dune-action-plugin "${PIN_DUNE_ACTION_PLUGIN}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dune-build-info "${PIN_DUNE_BUILD_INFO}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dune-configurator "${PIN_DUNE_CONFIGURATOR}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dune-glob "${PIN_DUNE_GLOB}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dune-private-libs "${PIN_DUNE_PRIVATE_LIBS}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dune-rpc "${PIN_DUNE_RPC}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dune-rpc-lwt "${PIN_DUNE_RPC_LWT}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dune-site "${PIN_DUNE_SITE}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version dyn "${PIN_DYN}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version either "${PIN_EITHER}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version eqaf "${PIN_EQAF}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version extlib "${PIN_EXTLIB}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ezjsonm "${PIN_EZJSONM}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version feather "${PIN_FEATHER}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version fiber "${PIN_FIBER}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version fix "${PIN_FIX}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version fmt "${PIN_FMT}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version fpath "${PIN_FPATH}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version graphics "${PIN_GRAPHICS}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version hex "${PIN_HEX}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version integers "${PIN_INTEGERS}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version jane-street-headers "${PIN_JANE_STREET_HEADERS}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version jingoo "${PIN_JINGOO}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version jsonm "${PIN_JSONM}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version jsonrpc "${PIN_JSONRPC}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version jst-config "${PIN_JST_CONFIG}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version lambda-term "${PIN_LAMBDA_TERM}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version logs "${PIN_LOGS}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version lsp "${PIN_LSP}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version lwt "${PIN_LWT}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version lwt_react "${PIN_LWT_REACT}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version mccs "${PIN_MCCS}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version mdx "${PIN_MDX}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version menhir "${PIN_MENHIR}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version menhirLib "${PIN_MENHIRLIB}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version menhirSdk "${PIN_MENHIRSDK}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version merlin-lib "${PIN_MERLIN_LIB}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version metapp "${PIN_METAPP}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version metaquot "${PIN_METAQUOT}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version mew "${PIN_MEW}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version mew_vi "${PIN_MEW_VI}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version num "${PIN_NUM}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ocaml "${PIN_OCAML}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ocaml-compiler-libs "${PIN_OCAML_COMPILER_LIBS}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ocaml-lsp-server "${PIN_OCAML_LSP_SERVER}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ocaml-version "${PIN_OCAML_VERSION}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ocamlbuild "${PIN_OCAMLBUILD}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ocamlc-loc "${PIN_OCAMLC_LOC}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ocamlfind "${PIN_OCAMLFIND}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ocamlformat "${PIN_OCAMLFORMAT}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ocamlformat-lib "${PIN_OCAMLFORMAT_LIB}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ocamlformat-rpc-lib "${PIN_OCAMLFORMAT_RPC_LIB}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ocp-indent "${PIN_OCP_INDENT}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ocplib-endian "${PIN_OCPLIB_ENDIAN}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version odoc "${PIN_ODOC}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version odoc-parser "${PIN_ODOC_PARSER}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ordering "${PIN_ORDERING}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version parsexp "${PIN_PARSEXP}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version pp "${PIN_PP}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ppx_assert "${PIN_PPX_ASSERT}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ppx_base "${PIN_PPX_BASE}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ppx_cold "${PIN_PPX_COLD}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ppx_compare "${PIN_PPX_COMPARE}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ppx_derivers "${PIN_PPX_DERIVERS}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ppx_deriving "${PIN_PPX_DERIVING}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ppx_enumerate "${PIN_PPX_ENUMERATE}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ppx_expect "${PIN_PPX_EXPECT}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ppx_globalize "${PIN_PPX_GLOBALIZE}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ppx_hash "${PIN_PPX_HASH}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ppx_here "${PIN_PPX_HERE}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ppx_ignore_instrumentation "${PIN_PPX_IGNORE_INSTRUMENTATION}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ppx_inline_test "${PIN_PPX_INLINE_TEST}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ppx_optcomp "${PIN_PPX_OPTCOMP}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ppx_pipebang "${PIN_PPX_PIPEBANG}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ppx_sexp_conv "${PIN_PPX_SEXP_CONV}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ppx_yojson_conv_lib "${PIN_PPX_YOJSON_CONV_LIB}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ppxlib "${PIN_PPXLIB}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version ptime "${PIN_PTIME}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version qrc "${PIN_QRC}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version re "${PIN_RE}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version react "${PIN_REACT}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version refl "${PIN_REFL}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version result "${PIN_RESULT}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version rresult "${PIN_RRESULT}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version seq "${PIN_SEQ}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version sexplib "${PIN_SEXPLIB}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version sexplib0 "${PIN_SEXPLIB0}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version sha "${PIN_SHA}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version spawn "${PIN_SPAWN}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version sqlite3 "${PIN_SQLITE3}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version stdcompat "${PIN_STDCOMPAT}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version stdio "${PIN_STDIO}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version stdlib-shims "${PIN_STDLIB_SHIMS}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version stdune "${PIN_STDUNE}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version time_now "${PIN_TIME_NOW}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version topkg "${PIN_TOPKG}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version traverse "${PIN_TRAVERSE}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version trie "${PIN_TRIE}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version tsort "${PIN_TSORT}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version tyxml "${PIN_TYXML}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version uchar "${PIN_UCHAR}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version utop "${PIN_UTOP}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version uucp "${PIN_UUCP}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version uuidm "${PIN_UUIDM}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version uuseg "${PIN_UUSEG}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version uutf "${PIN_UUTF}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version with-dkml "${PIN_WITH_DKML}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version xdg "${PIN_XDG}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version yojson "${PIN_YOJSON}"
    opamrun pin add --switch "$do_pins_NAME"  --yes --no-action -k version zed "${PIN_ZED}"
    ### END pin-adds. DO NOT EDIT THE LINES ABOVE
    section_end "opam-pins-$do_pins_NAME"

    # --------------
    # REMAINING PINS
    # --------------

    # These come after [pin-adds] section since [pin-adds] may need to be overridden by
    # users' choice.

    # dkml-base-compiler

    if [ "${DKML_COMPILER:-}" != '@repository@' ] && [ -z "${DKML_COMPILER:-}" ] && [ -z "${OCAML_COMPILER:-}" ]; then
        section_begin checkout-dkml-base-compiler "Pin dkml-base-compiler to default ${DEFAULT_DKML_COMPILER} (neither dkml-base-compiler nor OCAML_COMPILER specified) for $do_pins_NAME switch"
        opamrun pin add --switch "$do_pins_NAME" --yes --no-action dkml-base-compiler "https://github.com/diskuv/dkml-compiler.git#${DEFAULT_DKML_COMPILER}"
        section_end checkout-dkml-base-compiler
    elif [ "${DKML_COMPILER:-}" != '@repository@' ] && [ -n "${DKML_COMPILER:-}" ] && [ -z "${OCAML_COMPILER:-}" ]; then
        section_begin checkout-dkml-base-compiler "Pin dkml-base-compiler to $DKML_COMPILER (dkml-base-compiler specified; no OCAML_COMPILER specified) for $do_pins_NAME switch"
        opamrun pin add --switch "$do_pins_NAME" --yes --no-action dkml-base-compiler "https://github.com/diskuv/dkml-compiler.git#${DKML_COMPILER}"
        section_end checkout-dkml-base-compiler
    elif [ -n "${OCAML_COMPILER:-}" ]; then
        # Validate OCAML_COMPILER (OCAML_COMPILER specified)
        case "${OCAML_COMPILER:-}" in
        4.12.1) true ;;
        4.14.0) true ;;
        *)
            echo "OCAML_COMPILER version ${OCAML_COMPILER:-} is not supported" >&2
            exit 109
            ;;
        esac

        section_begin checkout-dkml-base-compiler "Pin dkml-base-compiler (OCAML_COMPILER specified) for $do_pins_NAME switch"
        opamrun pin add --switch "$do_pins_NAME" --yes --no-action dkml-base-compiler "https://github.com/diskuv/dkml-compiler.git#${OCAML_COMPILER}-v${DKML_VERSION}"
        section_end checkout-dkml-base-compiler
    fi

    # conf-dkml-cross-toolchain

    if [ "${CONF_DKML_CROSS_TOOLCHAIN:-}" != '@repository@' ]; then
        section_begin checkout-conf-dkml-cross-toolchain "Pin conf-dkml-cross-toolchain for $do_pins_NAME switch"
        opamrun pin add --switch "$do_pins_NAME" --yes --no-action conf-dkml-cross-toolchain "https://github.com/diskuv/conf-dkml-cross-toolchain.git#$CONF_DKML_CROSS_TOOLCHAIN"
        section_end checkout-conf-dkml-cross-toolchain
    fi
}

if [ "${SKIP_OPAM_MODIFICATIONS:-}" = "false" ]; then
    do_pins dkml
    if [ "${SECONDARY_SWITCH:-}" = "true" ]; then
        do_pins two
    fi
fi

do_use_vsstudio() {
    do_use_vsstudio_NAME=$1
    shift
    case "$dkml_host_abi" in
    windows_*)
        section_begin "use-vsstudio-$do_use_vsstudio_NAME" "Use Visual Studio in dkml-* Opam packages (Windows) for $do_use_vsstudio_NAME switch"

        # shellcheck disable=SC2153
        E_VS_DIR=$(escape_arg_as_ocaml_string "$VS_DIR")
        # shellcheck disable=SC2153
        E_VS_VCVARSVER=$(escape_arg_as_ocaml_string "$VS_VCVARSVER")
        # shellcheck disable=SC2153
        E_VS_WINSDKVER=$(escape_arg_as_ocaml_string "$VS_WINSDKVER")
        # shellcheck disable=SC2153
        E_VS_MSVSPREFERENCE=$(escape_arg_as_ocaml_string "$VS_MSVSPREFERENCE")
        # shellcheck disable=SC2153
        E_VS_CMAKEGENERATOR=$(escape_arg_as_ocaml_string "$VS_CMAKEGENERATOR")

        case "$(opamrun --version)" in
        2.0.*)
            if [ "${in_docker}" = "true" ]; then
                echo Opam 2.0 support in dockcross to use a portable opam var prefix not yet implemented
                exit 67
            fi
            OP=$(opamrun var prefix --switch "$do_use_vsstudio_NAME")
            OPSC=$OP/.opam-switch/switch-config
            if grep setenv: "$OPSC"; then
                echo "INFO: Updating switch-config. Old was:"
                awk '{print ">> " $0}' "$OPSC"

                awk '$1=="setenv:"{x=1} x==0{print} x==1 && $0=="]"{x=0}' "$OPSC" >"$OPSC".trimmed
                mv "$OPSC".trimmed "$OPSC"
            fi
            echo 'setenv: [' >>"$OPSC"
            echo '  [DKML_COMPILE_SPEC = "1"]' >>"$OPSC"
            echo '  [DKML_COMPILE_TYPE = "VS"]' >>"$OPSC"
            echo "  [DKML_COMPILE_VS_DIR = \"$E_VS_DIR\"]" >>"$OPSC"
            echo "  [DKML_COMPILE_VS_VCVARSVER = \"$E_VS_VCVARSVER\"]" >>"$OPSC"
            echo "  [DKML_COMPILE_VS_WINSDKVER = \"$E_VS_WINSDKVER\"]" >>"$OPSC"
            echo "  [DKML_COMPILE_VS_MSVSPREFERENCE = \"$E_VS_MSVSPREFERENCE\"]" >>"$OPSC"
            echo "  [DKML_COMPILE_VS_CMAKEGENERATOR = \"$E_VS_CMAKEGENERATOR\"]" >>"$OPSC"
            echo "  [DKML_HOST_ABI = \"${dkml_host_abi}\"]" >>"$OPSC"
            echo ']' >>"$OPSC"
            cat "$OPSC" >&2 # print
            ;;
        *)
            opamrun option --switch "$do_use_vsstudio_NAME" setenv= # reset
            opamrun option --switch "$do_use_vsstudio_NAME" setenv+='DKML_COMPILE_SPEC = "1"'
            opamrun option --switch "$do_use_vsstudio_NAME" setenv+='DKML_COMPILE_TYPE = "VS"'
            opamrun option --switch "$do_use_vsstudio_NAME" setenv+="DKML_COMPILE_VS_DIR = \"$E_VS_DIR\""
            opamrun option --switch "$do_use_vsstudio_NAME" setenv+="DKML_COMPILE_VS_VCVARSVER = \"$E_VS_VCVARSVER\""
            opamrun option --switch "$do_use_vsstudio_NAME" setenv+="DKML_COMPILE_VS_WINSDKVER = \"$E_VS_WINSDKVER\""
            opamrun option --switch "$do_use_vsstudio_NAME" setenv+="DKML_COMPILE_VS_MSVSPREFERENCE = \"$E_VS_MSVSPREFERENCE\""
            opamrun option --switch "$do_use_vsstudio_NAME" setenv+="DKML_COMPILE_VS_CMAKEGENERATOR = \"$E_VS_CMAKEGENERATOR\""
            opamrun option --switch "$do_use_vsstudio_NAME" setenv+="DKML_HOST_ABI = \"${dkml_host_abi}\""
            opamrun option --switch "$do_use_vsstudio_NAME" setenv # print
            ;;
        esac

        # shellcheck disable=SC2016
        opamrun exec --switch "$do_use_vsstudio_NAME" -- sh -c 'echo $VCToolsRedistDir'

        section_end "use-vsstudio-$do_use_vsstudio_NAME"
        ;;
    esac
}
if [ "${SKIP_OPAM_MODIFICATIONS:-}" = "false" ]; then
    do_use_vsstudio dkml
    if [ "${SECONDARY_SWITCH:-}" = "true" ]; then
        do_use_vsstudio two
    fi
fi

# Because dune.X.Y.Z+shim (and any user DKML packages) requires DKML installed (after all, it is just
# a with-dkml.exe shim), we need either dkmlvars-v2.sexp or DKML environment
# variables. Confer: Dkml_runtimelib.Dkml_context.get_dkmlversion
#
# grep matches either:
#   [... [DiskuvOCamlVersion = "1.0.1"] ...]
#   DiskuvOCamlVersion = "1.0.1"
do_setenv() {
    do_setenv_SWITCH=$1
    shift
    section_begin "setenv-$do_setenv_SWITCH" "Set opam option for $do_setenv_SWITCH switch"
    opamrun option --switch "$do_setenv_SWITCH" setenv > ".ci/sd4/setenv.$do_setenv_SWITCH.txt"
    if ! grep -q '\(^|\[\)DiskuvOCamlVarsVersion ' ".ci/sd4/setenv.$do_setenv_SWITCH.txt"; then
        opamrun option --switch "$do_setenv_SWITCH" setenv+='DiskuvOCamlVarsVersion = "2"'
    fi
    if ! grep -q '\(^|\[\)DiskuvOCamlVersion ' ".ci/sd4/setenv.$do_setenv_SWITCH.txt"; then
        opamrun option --switch "$do_setenv_SWITCH" setenv+="DiskuvOCamlVersion = \"$DKML_VERSION\""
    fi
    if [ "$do_setenv_SWITCH" = dkml ] && [ -n "${DKML_HOME:-}" ]; then
      do_setenv_DKMLHOME_ESCAPED="$DKML_HOME"
      do_setenv_USRBIN_ESCAPED="$DKML_HOME/usr/bin"
      do_setenv_BIN_ESCAPED="$DKML_HOME/bin"
      if [ -x /usr/bin/cygpath ]; then
        do_setenv_DKMLHOME_ESCAPED=$(/usr/bin/cygpath -aw "$do_setenv_DKMLHOME_ESCAPED" | sed 's/\\/\\\\/g')
        do_setenv_USRBIN_ESCAPED=$(/usr/bin/cygpath -aw "$do_setenv_USRBIN_ESCAPED" | sed 's/\\/\\\\/g')
        do_setenv_BIN_ESCAPED=$(/usr/bin/cygpath -aw "$do_setenv_BIN_ESCAPED" | sed 's/\\/\\\\/g')
      fi
      if ! grep -q '\(^|\[\)DiskuvOCamlHome ' ".ci/sd4/setenv.$do_setenv_SWITCH.txt"; then
          opamrun option --switch "$do_setenv_SWITCH" setenv+="DiskuvOCamlHome = \"$do_setenv_DKMLHOME_ESCAPED\""
      fi
      if ! grep -q '\(^|\[\)DiskuvOCamlBinaryPaths ' ".ci/sd4/setenv.$do_setenv_SWITCH.txt"; then
          opamrun option --switch "$do_setenv_SWITCH" setenv+="DiskuvOCamlBinaryPaths = \"$do_setenv_USRBIN_ESCAPED;$do_setenv_BIN_ESCAPED\""
      fi
      if ! grep -q '\(^|\[\)DiskuvOCamlDeploymentId ' ".ci/sd4/setenv.$do_setenv_SWITCH.txt"; then
          opamrun option --switch "$do_setenv_SWITCH" setenv+="DiskuvOCamlDeploymentId = \"setup-dkml-switch-$do_setenv_SWITCH\""
      fi
    fi
    case "${dkml_host_abi}" in
    windows_*)
        if ! grep -q '\(^|\[\)DiskuvOCamlMSYS2Dir ' ".ci/sd4/setenv.$do_setenv_SWITCH.txt"; then
            if [ -x /usr/bin/cygpath ]; then
                MSYS2_DIR_NATIVE=$(/usr/bin/cygpath -aw /)
            else
                # If we are already inside MSYS2 then MSYSTEM_PREFIX should be set. But cygpath should be there as well!!
                echo "FATAL: Could not locate MSYS2: there was no cygpath" >&2
                exit 3
            fi
            MSYS2_DIR_NATIVE_ESCAPED=$(printf "%s" "$MSYS2_DIR_NATIVE" | sed 's/\\/\\\\/g')
            opamrun option --switch "$do_setenv_SWITCH" setenv+="DiskuvOCamlMSYS2Dir = \"$MSYS2_DIR_NATIVE_ESCAPED\""
        fi
    esac
    section_end "setenv-$do_setenv_SWITCH"
}
if [ "${SKIP_OPAM_MODIFICATIONS:-}" = "false" ]; then
    do_setenv dkml
    if [ "${SECONDARY_SWITCH:-}" = "true" ]; then
        do_setenv two
    fi
fi

do_install_compiler() {
    do_install_compiler_NAME=$1
    shift
    section_begin "install-compiler-$do_install_compiler_NAME" "Install OCaml compiler for $do_install_compiler_NAME switch"
    opamrun pin list --switch "$do_install_compiler_NAME"
    # shellcheck disable=SC2086
    opamrun upgrade --switch "$do_install_compiler_NAME" --yes dkml-base-compiler conf-dkml-cross-toolchain ${ocaml_options:-}
    section_end "install-compiler-$do_install_compiler_NAME"
}
if [ "${SKIP_OPAM_MODIFICATIONS:-}" = "false" ]; then
    if ! [ "${PRIMARY_SWITCH_SKIP_INSTALL:-}" = "true" ]; then
        do_install_compiler dkml
    fi
    if [ "${SECONDARY_SWITCH:-}" = "true" ]; then
        do_install_compiler two
    fi
fi

do_summary() {
    do_summary_NAME=$1
    shift
    section_begin "summary-$do_summary_NAME" "Summary: $do_summary_NAME switch"
    opamrun var --switch "$do_summary_NAME"
    opamrun exec --switch "$do_summary_NAME" -- ocamlc -config
    section_end "summary-$do_summary_NAME"
}
if [ "${SKIP_OPAM_MODIFICATIONS:-}" = "false" ]; then
    do_summary dkml
    if [ "${SECONDARY_SWITCH:-}" = "true" ]; then
        do_summary two
    fi
fi

'@
Set-Content -Path ".ci\sd4\run-setup-dkml.sh" -Encoding Unicode -Value $Content
msys64\usr\bin\bash -lc 'dos2unix .ci/sd4/run-setup-dkml.sh'

$Content = @'
# MSVC environment variables:
# 1. https://docs.microsoft.com/en-us/cpp/build/reference/cl-environment-variables?view=msvc-170
# 2. https://docs.microsoft.com/en-us/cpp/build/reference/linking?view=msvc-170#link-environment-variables (except TMP)
# 3. VCToolsRedistDir: https://docs.microsoft.com/en-us/cpp/windows/redistributing-visual-cpp-files?view=msvc-170#locate-the-redistributable-files
BEGIN{FS="="}
$1=="CL"||$1=="_CL_"||$1=="INCLUDE"||$1=="LIBPATH" {print "export " $0}
$1=="LINK"||$1=="_LINK_"||$1=="LIB"||$1=="PATH"    {print "export " $0}
$1=="VCToolsRedistDir"                             {print "export " $0}

'@
Set-Content -Path ".ci\sd4\msvcenv.awk" -Encoding Unicode -Value $Content
msys64\usr\bin\bash -lc 'dos2unix .ci/sd4/msvcenv.awk'


$Content = @'
{
    # trim leading and trailing space
    sub(/^ */, "");
    sub(/ *$/, "");

    print "export PATH='" $0 "'";
}
'@
Set-Content -Path ".ci\sd4\msvcpath.awk" -Encoding Unicode -Value $Content
msys64\usr\bin\bash -lc 'dos2unix .ci/sd4/msvcpath.awk'

# PowerShell (UTF-16) and Batch (ANSI) scripts


$Content = @'
# Diagnose Visual Studio environment variables (Windows)
# This wastes time and has lots of rows! Only run if "VERBOSE" GitHub input key.
if ( "${env:VERBOSE}" -eq "true" ) {
    if (Test-Path -Path "C:\Program Files (x86)\Windows Kits\10\include") {
        Get-ChildItem "C:\Program Files (x86)\Windows Kits\10\include"
    }
    if (Test-Path -Path "C:\Program Files (x86)\Windows Kits\10\Extension SDKs\WindowsDesktop") {
        Get-ChildItem "C:\Program Files (x86)\Windows Kits\10\Extension SDKs\WindowsDesktop"
    }

    $env:PSModulePath += "$([System.IO.Path]::PathSeparator).ci\sd4\g\dkml-runtime-distribution\src\windows"
    Import-Module Machine

    $allinstances = Get-VSSetupInstance
    $allinstances | ConvertTo-Json -Depth 5
}

# Make export expression [SN]NAME=[SV]VALUE[EV]
# where [SN] is start name and [SV] and [EV] are start and end value
if (("${env:GITLAB_CI}" -eq "true") -or ("${env:PC_CI}" -eq "true")) {
    # Executed immediately in POSIX shell, so must be a real POSIX shell variable declaration
    $ExportSN = "export "
    $ExportSV = "'"
    $ExportEV = "'"
    $ExportExt = ".sh"
} else {
    # Goes into $env:GITHUB_ENV, so must be plain NAME=VALUE
    $ExportSN = ""
    $ExportSV = ""
    $ExportEV = ""
    $ExportExt = ".github"
}

# Locate Visual Studio (Windows)
if ("${env:vsstudio_dir}" -eq "" -and (!(Test-Path -Path .ci/sd4/vsenv${ExportExt}) -or !(Test-Path -Path .ci/sd4/vsenv.ps1))) {
    $env:PSModulePath += "$([System.IO.Path]::PathSeparator).ci\sd4\g\dkml-runtime-distribution\src\windows"
    Import-Module Machine

    $CompatibleVisualStudios = Get-CompatibleVisualStudios -ErrorIfNotFound
    $CompatibleVisualStudios
    $ChosenVisualStudio = ($CompatibleVisualStudios | Select-Object -First 1)
    $VisualStudioProps = Get-VisualStudioProperties -VisualStudioInstallation $ChosenVisualStudio
    $VisualStudioProps

    Write-Output "${ExportSN}VS_DIR=${ExportSV}$($VisualStudioProps.InstallPath)${ExportEV}" > .ci/sd4/vsenv${ExportExt}
    Write-Output "${ExportSN}VS_VCVARSVER=${ExportSV}$($VisualStudioProps.VcVarsVer)${ExportEV}" >> .ci/sd4/vsenv${ExportExt}
    Write-Output "${ExportSN}VS_WINSDKVER=${ExportSV}$($VisualStudioProps.WinSdkVer)${ExportEV}" >> .ci/sd4/vsenv${ExportExt}
    Write-Output "${ExportSN}VS_MSVSPREFERENCE=${ExportSV}$($VisualStudioProps.MsvsPreference)${ExportEV}" >> .ci/sd4/vsenv${ExportExt}
    Write-Output "${ExportSN}VS_CMAKEGENERATOR=${ExportSV}$($VisualStudioProps.CMakeGenerator)${ExportEV}" >> .ci/sd4/vsenv${ExportExt}

    Write-Output "`$env:VS_DIR='$($VisualStudioProps.InstallPath)'" > .ci/sd4/vsenv.ps1
    Write-Output "`$env:VS_VCVARSVER='$($VisualStudioProps.VcVarsVer)'" >> .ci/sd4/vsenv.ps1
    Write-Output "`$env:VS_WINSDKVER='$($VisualStudioProps.WinSdkVer)'" >> .ci/sd4/vsenv.ps1
    Write-Output "`$env:VS_MSVSPREFERENCE='$($VisualStudioProps.MsvsPreference)'" >> .ci/sd4/vsenv.ps1
    Write-Output "`$env:VS_CMAKEGENERATOR='$($VisualStudioProps.CMakeGenerator)'" >> .ci/sd4/vsenv.ps1
}

# Link to hardcoded Visual Studio (Windows)
if ("${env:vsstudio_dir}" -ne "") {
    Write-Output "${ExportSN}VS_DIR=${ExportSV}${env:vsstudio_dir}${ExportEV}" > .ci/sd4/vsenv${ExportExt}
    Write-Output "${ExportSN}VS_VCVARSVER=${ExportSV}${env:vsstudio_vcvarsver}${ExportEV}" >> .ci/sd4/vsenv${ExportExt}
    Write-Output "${ExportSN}VS_WINSDKVER=${ExportSV}${env:vsstudio_winsdkver}${ExportEV}" >> .ci/sd4/vsenv${ExportExt}
    Write-Output "${ExportSN}VS_MSVSPREFERENCE=${ExportSV}${env:vsstudio_msvspreference}${ExportEV}" >> .ci/sd4/vsenv${ExportExt}
    Write-Output "${ExportSN}VS_CMAKEGENERATOR=${ExportSV}${env:vsstudio_cmakegenerator}${ExportEV}" >> .ci/sd4/vsenv${ExportExt}

    Write-Output "`$env:VS_DIR='${env:vsstudio_dir}'" > .ci/sd4/vsenv.ps1
    Write-Output "`$env:VS_VCVARSVER='${env:vsstudio_vcvarsver}'" >> .ci/sd4/vsenv.ps1
    Write-Output "`$env:VS_WINSDKVER='${env:vsstudio_winsdkver}'" >> .ci/sd4/vsenv.ps1
    Write-Output "`$env:VS_MSVSPREFERENCE='${env:vsstudio_msvspreference}'" >> .ci/sd4/vsenv.ps1
    Write-Output "`$env:VS_CMAKEGENERATOR='${env:vsstudio_cmakegenerator}'" >> .ci/sd4/vsenv.ps1
}

'@
Set-Content -Path ".ci\sd4\config-vsstudio.ps1" -Encoding Unicode -Value $Content


$Content = @'
@ECHO OFF

REM The OCaml dkml-base-compiler will compile fine but any other
REM packages (ocamlbuild, etc.) which
REM need a native compiler will fail without the MSVC compiler in the
REM PATH. There isn't a `with-dkml.exe` alternative available at
REM this stage of the GitHub workflow.
SET VSCMD_DEBUG=2
SET VSCMD_SKIP_SENDTELEMETRY=1
call "%VS_DIR%\Common7\Tools\VsDevCmd.bat" -no_logo -host_arch=%vsstudio_hostarch% -arch=%vsstudio_arch% -vcvars_ver=%VS_VCVARSVER% -winsdk=%VS_WINSDKVER%
if %ERRORLEVEL% neq 0 (
    echo.
    echo.The "%VS_DIR%\Common7\Tools\VsDevCmd.bat" command failed
    echo.with exit code %ERRORLEVEL%.
    echo.
    exit /b %ERRORLEVEL%
)

REM VsDevCmd.bat turns off echo; be explicit if we want it on or off
@echo OFF

REM MSVC environment variables in Unix format.
echo %PATH% > .ci\sd4\msvcpath


REM * We can't use `bash -lc` directly to query for all MSVC environment variables
REM   because it stomps over the PATH. So we are inside a Batch script to do the query.
msys64\usr\bin\bash -lc "set | grep -v '^PATH=' | awk -f .ci/sd4/msvcenv.awk > .ci/sd4/msvcenv"
'@
Set-Content -Path ".ci\sd4\get-msvcpath-into-msys2.bat" -Encoding Default -Value $Content

msys64\usr\bin\bash -lc "sh .ci/sd4/run-checkout-code.sh PC_PROJECT_DIR '${env:PC_PROJECT_DIR}'"
if ($LASTEXITCODE -ne 0) {
  throw "run-checkout-code.sh failed"
}

# Diagnose Visual Studio environment variables (Windows)
# This wastes time and has lots of rows! Only run if "VERBOSE" GitHub input key.

If ( "${env:VERBOSE}" -eq "true" ) {
  if (Test-Path -Path "C:\Program Files (x86)\Windows Kits\10\include") {
    Get-ChildItem "C:\Program Files (x86)\Windows Kits\10\include"
  }
  if (Test-Path -Path "C:\Program Files (x86)\Windows Kits\10\Extension SDKs\WindowsDesktop") {
    Get-ChildItem "C:\Program Files (x86)\Windows Kits\10\Extension SDKs\WindowsDesktop"
  }

  $env:PSModulePath += "$([System.IO.Path]::PathSeparator).ci\sd4\g\dkml-runtime-distribution\src\windows"
  Import-Module Machine

  $allinstances = Get-VSSetupInstance
  $allinstances | ConvertTo-Json -Depth 5
}
.ci\sd4\config-vsstudio.ps1
msys64\usr\bin\bash -lc "dos2unix .ci/sd4/vsenv.sh"
Get-Content .ci/sd4/vsenv.sh
Get-Content .ci/sd4/vsenv.ps1

# Capture Visual Studio compiler environment
& .ci\sd4\vsenv.ps1
& .ci\sd4\get-msvcpath-into-msys2.bat
msys64\usr\bin\bash -lc "cat .ci/sd4/msvcpath | tr -d '\r' | cygpath --path -f - | awk -f .ci/sd4/msvcpath.awk >> .ci/sd4/msvcenv"    
msys64\usr\bin\bash -lc "tail -n100 .ci/sd4/msvcpath .ci/sd4/msvcenv"

msys64\usr\bin\bash -lc "sh .ci/sd4/run-setup-dkml.sh PC_PROJECT_DIR '${env:PC_PROJECT_DIR}'"
if ($LASTEXITCODE -ne 0) {
  throw "run-setup-dkml.sh failed"
}

########################### script ###############################

Write-Host @"
Finished setup.

To continue your testing, run in PowerShell:
  `$env:CHERE_INVOKING = "yes"
  `$env:MSYSTEM = "$env:msys2_system"
  `$env:dkml_host_abi = "$env:dkml_host_abi"
  `$env:abi_pattern = "$env:abi_pattern"
  `$env:opam_root = "$env:opam_root"
  `$env:exe_ext = "$env:exe_ext"

Now you can use 'opamrun' to do opam commands like:

  msys64\usr\bin\bash -lc 'PATH="`$PWD/.ci/sd4/opamrun:`$PATH"; opamrun install XYZ.opam'
  msys64\usr\bin\bash -lc 'PATH="`$PWD/.ci/sd4/opamrun:`$PATH"; opamrun exec -- bash'
  msys64\usr\bin\bash -lc 'sh ci/build-test.sh'
"@
