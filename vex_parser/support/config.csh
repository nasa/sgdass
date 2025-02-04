#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Script  config.csh  sets numerous variables needed for             *
# *   compilation and linking vex_parser. It creates files with directives *
# *   for FORTRAN-compilers and C-compiler. It creates include files     *
# *   using template files for includes and local files with             *
# *   preferences.                                                       *
# *                                                                      *
# *   Usage:  config.csh                                                 *
# *                                                                      *
# *  ###  15-JUN-2009  config.csh  v1.8  (c)  L. Petrov  15-MAY-2019 ### *
# *                                                                      *
# ************************************************************************
#
setenv SUPPORT_PATH `dirname $0`
cd     $SUPPORT_PATH
cd ../
setenv CONF_LOG   ${VEX_PARSER_ROOT}/temp/conf.log
if (    -f ${VEX_PARSER_ROOT}/Makefile ) then
     rm -f ${VEX_PARSER_ROOT}/Makefile
endif
set OS_name = `uname`
switch ( $OS_name )
   case "HP-UX":
     set ECHO = "/bin/echo"
     set qb = "\0044\0050"
     set qe = "\0051"
     set qt = "\0042"
     breaksw
   case "SunOS":
     set ECHO = "/bin/echo"
     set qb = "\044\050"
     set qe = "\051"
     set qt = "\042"
     breaksw
   case "Linux":
     set ECHO = "/bin/echo -e"
     set qb = "\044\050"
     set qe = "\051"
     set qt = "\042"
     breaksw
   case "Darwin":
     set ECHO = "/bin/echo"
     set qt = '"'
     set qb = "#\!"
     set qe = "\051"
     breaksw
endsw
#
$ECHO "config.csh: Check petools ... \c" | tee -a  $CONF_LOG
if ( -d $PETOOLS_DIR == 0 ) then
     echo "vex_parser config: petools directory $PETOOLS_DIR was not found"
     echo "check option --with-petools"
     echo "Error in configuration"
     exit 1
endif
if ( -d $PETOOLS_DIR/lib == 0 ) then
     echo "vex_parser config: petools directory $PETOOLS_DIR/lib was not found"
     echo "check option --with-petools"
     echo "Error in configuration"
     exit 1
endif
if ( -d $PETOOLS_DIR/bin == 0 ) then
     echo "vex_parser config: petools directory $PETOOLS_DIR/bin was not found"
     echo "check option --with-petools"
     echo "Error in configuration"
     exit 1
endif
if ( -f $PETOOLS_DIR/lib/libpetools.so == 0 && -f $PETOOLS_DIR/lib/libpetools.dylib == 0 ) then
     echo "vex_parser config: petools file $PETOOLS_DIR/lib/libpetools.so or $PETOOLS_DIR/lib/libpetools.dylib was not found"
     echo "check option --with-petools"
     echo "Error in configuration"
     exit 1
endif
$ECHO "ok"
source $PETOOLS_DIR/bin/petools_vars
#
# --- Check whether gmake is found in your system
#
set gmake_string = `which gmake`
if ( "$gmake_string" == "" ) then 
     echo "gmake was not found in your system."
     echo "Error in configuration"
     exit 1
endif
#
set    date_stamp  = `date "+%Y.%m.%d-%H:%M:%S" | tr "[a-z]" "[A-Z]"`
set    host_name   = `uname -n`
#
source ${PETOOLS_DIR}/bin/petools_vars
set out_file = ${VEX_PARSER_ROOT}/Makefile
if ( "$PETOOLS_VERSION" < "$PETOOLS_VERSION_MIN" ) then 
     echo "Eeeeh. You have petools version $PETOOLS_VERSION, but "
     echo "$PETOOLS_VERSION_MIN is needed. Please upgrade" 
     echo "Error in configuration"
     exit 1 
endif
#
if ( $VEX_PARSER_NOOPT == "YES" ) then
     setenv MK5_F95        "$MK5_F95_NOOPT"
     setenv MK5_F95_OPT    "$MK5_F95_NOOPT"
     setenv MK5_F95_OPTEST "$MK5_F95_NOOPT"
endif
#
if ( `uname` == "Linux" ) then
     set num_threads     = `cat /proc/cpuinfo | grep "core id" | wc -l`
     set num_thr_per_cpu = `lscpu | grep '^Thread(s) per core:' | awk '{print $4}'`
     set num_cores = `expr $num_threads / $num_thr_per_cpu`
else
     set num_cores = `sysctl -n  machdep.cpu.core_count`
endif
cat   ${VEX_PARSER_ROOT}/Makefile.in1 > $out_file 
$ECHO "VEX_PARSER_ROOT    = $VEX_PARSER_ROOT"                >> $out_file
$ECHO "PETOOLS_PREFIX   = $PETOOLS_PREFIX"                   >> $out_file
$ECHO "PETOOLS_LIB      = $PETOOLS_LIB"                      >> $out_file
$ECHO "SOLVE_EXTRA_LIB  = $SOLVE_EXTRA_LIB"                  >> $out_file
$ECHO "SOLVE_LIB_VEC    = $SOLVE_LIB_VEC"                    >> $out_file
$ECHO "SOLVE_LIB_BLAS   = $SOLVE_LIB_BLAS"                   >> $out_file
$ECHO "MK5_C            = $MK5_C"                            >> $out_file
$ECHO "MK5_F95_OPTEST   = $MK5_F95_OPTEST"                   >> $out_file
$ECHO "MK5_F95_OPT      = $MK5_F95_OPT"                      >> $out_file
$ECHO "MK5_F95_NOOPT    = $MK5_F95_NOOPT"                    >> $out_file
$ECHO "MK5_F95          = $MK5_F95"                          >> $out_file
$ECHO "MK5_LINK         = $MK5_LINK"                         >> $out_file
$ECHO "MK5_C_LINK       = $MK5_C_LINK"                       >> $out_file
$ECHO "VEX_PARSER_ROOT    = $VEX_PARSER_ROOT"                >> $out_file
$ECHO "VEX_PARSER_PREFIX  = $VEX_PARSER_PREFIX"              >> $out_file
$ECHO "VEX_PARSER_BIN     = $VEX_PARSER_PREFIX/bin"          >> $out_file
$ECHO "VEX_PARSER_LIB     = $VEX_PARSER_PREFIX/lib"          >> $out_file
$ECHO "VEX_PARSER_OS      = "`uname`                         >> $out_file
$ECHO "VEX_PARSER_VERSION = $VEX_PARSER_VERSION"             >> $out_file
$ECHO "NUM_PROC         = $num_cores"                        >> $out_file
$ECHO "CONF_LOG         = $CONF_LOG"                         >> $out_file
$ECHO "BUILD_LOG        = $BUILD_LOG"                        >> $out_file
cat   ${VEX_PARSER_ROOT}/Makefile.in2                        >> $out_file
#
set out_vars = ${VEX_PARSER_ROOT}/support/vex_parser_vars
if ( -f $out_vars ) rm -f $out_vars
$ECHO "#\!/bin/csh"                                          > $out_vars
$ECHO "setenv VEX_PARSER_ROOT    $VEX_PARSER_ROOT"          >> $out_vars
$ECHO "setenv PETOOLS_PREFIX   $PETOOLS_PREFIX"             >> $out_vars
$ECHO "setenv PETOOLS_LIB      ${qt}$PETOOLS_LIB${qt}"      >> $out_vars
$ECHO "setenv SOLVE_EXTRA_LIB  ${qt}$SOLVE_EXTRA_LIB${qt}"  >> $out_vars
$ECHO "setenv SOLVE_LIB_VEC    ${qt}$SOLVE_LIB_VEC${qt}"    >> $out_vars
$ECHO "setenv SOLVE_LIB_BLAS   ${qt}$SOLVE_LIB_BLAS${qt}"   >> $out_vars
$ECHO "setenv MK5_C            ${qt}$MK5_C${qt}"            >> $out_vars
$ECHO "setenv MK5_F95_OPTEST   ${qt}$MK5_F95_OPTEST${qt}"   >> $out_vars
$ECHO "setenv MK5_F95_OPT      ${qt}$MK5_F95_OPT${qt}"      >> $out_vars
$ECHO "setenv MK5_F95_NOOPT    ${qt}$MK5_F95_OPT${qt}"      >> $out_vars
$ECHO "setenv MK5_F95          ${qt}$MK5_F95${qt}"          >> $out_vars
$ECHO "setenv MK5_LINK         ${qt}$MK5_LINK${qt}"         >> $out_vars
$ECHO "setenv MK5_C_LINK       ${qt}$MK5_C_LINK${qt}"       >> $out_vars
$ECHO "setenv VEX_PARSER_ROOT    $VEX_PARSER_ROOT"          >> $out_vars
$ECHO "setenv VEX_PARSER_PREFIX  $VEX_PARSER_PREFIX"        >> $out_vars
$ECHO "setenv VEX_PARSER_BIN     $VEX_PARSER_PREFIX/bin"    >> $out_vars
$ECHO "setenv VEX_PARSER_LIB     $VEX_PARSER_PREFIX/lib"    >> $out_vars
$ECHO "setenv VEX_PARSER_OS      "`uname`                   >> $out_vars
$ECHO "setenv VEX_PARSER_VERSION $VEX_PARSER_VERSION"       >> $out_vars
#
source ${VEX_PARSER_ROOT}/support/vex_parser_vars
#
set export_vars = ${VEX_PARSER_ROOT}/support/export_vex_parser_vars
if ( -f $export_vars  ) rm -f $export_vars 
$ECHO "#\!/bin/csh"                                      > $export_vars
$ECHO "setenv VEX_PARSER_VERSION $VEX_PARSER_VERSION"   >> $export_vars
$ECHO "setenv VEX_PARSER_PREFIX  $VEX_PARSER_PREFIX"    >> $export_vars
#
if ( -f $VEX_PARSER_PREFIX/include/vex_parser.i ) rm -f $VEX_PARSER_PREFIX/include/vex_parser.i 
if ( -f $VEX_PARSER_PREFIX/include/sat_eph.i    ) rm -f $VEX_PARSER_PREFIX/include/sat_eph.i
#
$ECHO "setenv VEX_PARSER_LIB    ${qt}-lgomp -L$VEX_PARSER_PREFIX/lib -lvex_parser${qt}"      >> $export_vars
$ECHO "setenv VEX_PARSER_LIB_STATIC ${qt}-lgomp $VEX_PARSER_PREFIX/lib/libvex_parser.a${qt}" >> $export_vars
#
chmod g+rw,o+r ${VEX_PARSER_ROOT}/Makefile
chmod g+rw,o+r ${VEX_PARSER_ROOT}/support/vex_parser_vars
chmod g+rw,o+r ${VEX_PARSER_ROOT}/support/export_vex_parser_vars
#
$ECHO "config.csh is done"
