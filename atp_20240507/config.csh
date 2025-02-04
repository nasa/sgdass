#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Script  config.csh  sets numerous variables needed for             *
# *   compilation and linking atp. It creates files with directives     *
# *   for FORTRAN-compilers and C-compiler. It creates include files     *
# *   using template files for includes and local files with             *
# *   preferences.                                                       *
# *                                                                      *
# *   Usage:  config.csh                                                 *
# *                                                                      *
# *  ###  27-JUL-2022  config.csh  v1.8  (c)  L. Petrov  27-JUL-2022 ### *
# *                                                                      *
# ************************************************************************
#
setenv SUPPORT_PATH `dirname $0`
cd     $SUPPORT_PATH
cd ../
setenv CONF_LOG   ${ATP_ROOT}/temp/conf.log
if (    -f ${ATP_ROOT}/Makefile ) then
     rm -f ${ATP_ROOT}/Makefile
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
     echo "atp config: petools directory $PETOOLS_DIR was not found"
     echo "check option --with-petools"
     echo "Error in configuration"
     exit 1
endif
if ( -d $PETOOLS_DIR/lib == 0 ) then
     echo "atp config: petools directory $PETOOLS_DIR/lib was not found"
     echo "check option --with-petools"
     echo "Error in configuration"
     exit 1
endif
if ( -d $PETOOLS_DIR/bin == 0 ) then
     echo "atp config: petools directory $PETOOLS_DIR/bin was not found"
     echo "check option --with-petools"
     echo "Error in configuration"
     exit 1
endif
if ( -f $PETOOLS_DIR/lib/libpetools.so == 0 && -f $PETOOLS_DIR/lib/libpetools.dylib == 0 ) then
     echo "atp config: petools file $PETOOLS_DIR/lib/libpetools.so or $PETOOLS_DIR/lib/libpetools.dylib was not found"
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
set out_file = ${ATP_ROOT}/Makefile
if ( "$PETOOLS_VERSION" < "$PETOOLS_VERSION_MIN" ) then 
     echo "Eeeeh. You have petools version $PETOOLS_VERSION, but "
     echo "$PETOOLS_VERSION_MIN is needed. Please upgrade" 
     echo "Error in configuration"
     exit 1 
endif
#
if ( -d  $NERS_PREFIX == 0 ) then
     echo "vtd config: ners install directory $NERS_PREFIX was not found"
     echo "vtd config: Please install ners"
     exit 1
endif
#
if ( -d  $NERS_PREFIX/include == 0 ) then
     echo "vtd config: ners include directory $NERS_PREFIX/include was not found"
     echo "vtd config: Please install ners"
     exit 1
endif
#
if ( -f  $NERS_PREFIX/include/ners_local.i == 0 ) then
     echo "vtd config: ners version file $NERS_PREFIX/include/ners_local.i was not found"
     echo "vtd config: Please install ners"
     exit 1
endif
set NERS_VERSION = `cat $NERS_PREFIX/include/ners_local.i | grep "NERS__VERSION =" | awk '{print substr($5,2,8)}'`
if ( "$NERS_VERSION" < "$NERS_VERSION_MIN" ) then
     echo "Eeeeh. You have ners version $NERS_VERSION, but $NERS_VERSION_MIN is needed."
     echo "Please upgrade."
     echo "You can download the new version of ners from http://earthrotation.net/ners"
     exit 1
endif
#
if ( $ATP_NOOPT == "YES" ) then
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
cat   ${ATP_ROOT}/Makefile.in1 > $out_file 
$ECHO "ATP_ROOT           = $ATP_ROOT"                         >> $out_file
$ECHO "PETOOLS_PREFIX     = $PETOOLS_PREFIX"                   >> $out_file
$ECHO "PETOOLS_LIB        = $PETOOLS_LIB"                      >> $out_file
$ECHO "NERS_PREFIX        = $NERS_PREFIX"                      >> $out_file
$ECHO "NERS_LIB           = -L $NERS_PREFIX/lib -lners"        >> $out_file
$ECHO "NERS_INC           = $NERS_PREFIX/include"              >> $out_file
$ECHO "SOLVE_EXTRA_LIB    = $SOLVE_EXTRA_LIB"                  >> $out_file
$ECHO "SOLVE_LIB_VEC      = $SOLVE_LIB_VEC"                    >> $out_file
$ECHO "SOLVE_LIB_BLAS     = $SOLVE_LIB_BLAS"                   >> $out_file
$ECHO "SOLVE_LIB_PGPLOT   = $SOLVE_LIB_PGPLOT"                 >> $out_file
$ECHO "SOLVE_LIB_PGPLOT_A = $SOLVE_LIB_PGPLOT_A"               >> $out_file
$ECHO "MK5_C              = $MK5_C"                            >> $out_file
$ECHO "MK5_F95_OPTEST     = $MK5_F95_OPTEST"                   >> $out_file
$ECHO "MK5_F95_OPT        = $MK5_F95_OPT"                      >> $out_file
$ECHO "MK5_F95_NOOPT      = $MK5_F95_NOOPT"                    >> $out_file
$ECHO "MK5_F95            = $MK5_F95"                          >> $out_file
$ECHO "MK5_LINK           = $MK5_LINK"                         >> $out_file
$ECHO "MK5_C_LINK         = $MK5_C_LINK"                       >> $out_file
$ECHO "ATP_ROOT           = $ATP_ROOT"                         >> $out_file
$ECHO "ATP_PREFIX         = $ATP_PREFIX"                       >> $out_file
$ECHO "ATP_BIN            = $ATP_PREFIX/bin"                   >> $out_file
$ECHO "ATP_LIB            = $ATP_PREFIX/lib"                   >> $out_file
$ECHO "ATP_OS             = "`uname`                           >> $out_file
$ECHO "ATP_VERSION        = $ATP_VERSION"                      >> $out_file
$ECHO "NUM_PROC           = $num_cores"                        >> $out_file
$ECHO "CONF_LOG           = $CONF_LOG"                         >> $out_file
$ECHO "BUILD_LOG          = $BUILD_LOG"                        >> $out_file
cat   ${ATP_ROOT}/Makefile.in2                               >> $out_file
#
set out_vars = ${ATP_ROOT}/support/atp_vars
if ( -f $out_vars ) rm -f $out_vars
$ECHO "#\!/bin/csh"                                            > $out_vars
$ECHO "setenv ATP_ROOT           $ATP_ROOT"                   >> $out_vars
$ECHO "setenv PETOOLS_PREFIX     $PETOOLS_PREFIX"             >> $out_vars
$ECHO "setenv PETOOLS_LIB        ${qt}$PETOOLS_LIB${qt}"      >> $out_vars
$ECHO "setenv NERS_LIB           ${qt}-L $NERS_PREFIX/lib -lners${qt}" >> $out_vars
$ECHO "setenv NERS_INC           $NERS_PREFIX/include"        >> $out_vars
$ECHO "setenv SOLVE_EXTRA_LIB    ${qt}$SOLVE_EXTRA_LIB${qt}"  >> $out_vars
$ECHO "setenv SOLVE_LIB_VEC      ${qt}$SOLVE_LIB_VEC${qt}"    >> $out_vars
$ECHO "setenv SOLVE_LIB_BLAS     ${qt}$SOLVE_LIB_BLAS${qt}"   >> $out_vars
$ECHO "setenv SOLVE_LIB_PGPLOT   ${qt}$SOLVE_LIB_PGPLOT${qt}"   >> $out_vars
$ECHO "setenv SOLVE_LIB_PGPLOT_A ${qt}$SOLVE_LIB_PGPLOT_A${qt}" >> $out_vars
$ECHO "setenv MK5_C              ${qt}$MK5_C${qt}"            >> $out_vars
$ECHO "setenv MK5_F95_OPTEST     ${qt}$MK5_F95_OPTEST${qt}"   >> $out_vars
$ECHO "setenv MK5_F95_OPT        ${qt}$MK5_F95_OPT${qt}"      >> $out_vars
$ECHO "setenv MK5_F95_NOOPT      ${qt}$MK5_F95_OPT${qt}"      >> $out_vars
$ECHO "setenv MK5_F95            ${qt}$MK5_F95${qt}"          >> $out_vars
$ECHO "setenv MK5_LINK           ${qt}$MK5_LINK${qt}"         >> $out_vars
$ECHO "setenv MK5_C_LINK         ${qt}$MK5_C_LINK${qt}"       >> $out_vars
$ECHO "setenv ATP_ROOT           $ATP_ROOT"                   >> $out_vars
$ECHO "setenv ATP_PREFIX         $ATP_PREFIX"                 >> $out_vars
$ECHO "setenv ATP_BIN            $ATP_PREFIX/bin"             >> $out_vars
$ECHO "setenv ATP_LIB            $ATP_PREFIX/lib"             >> $out_vars
$ECHO "setenv ATP_OS             "`uname`                     >> $out_vars
$ECHO "setenv ATP_VERSION        $ATP_VERSION"                >> $out_vars
#
source ${ATP_ROOT}/support/atp_vars
#
set export_vars = ${ATP_ROOT}/support/export_atp_vars
if ( -f $export_vars  ) rm -f $export_vars 
$ECHO "#\!/bin/csh"                                      > $export_vars
$ECHO "setenv ATP_VERSION $ATP_VERSION"   >> $export_vars
$ECHO "setenv ATP_PREFIX  $ATP_PREFIX"    >> $export_vars
#
if ( -f $ATP_PREFIX/include/atp.i ) rm -f $ATP_PREFIX/include/atp.i 
#
$ECHO "setenv ATP_LIB    ${qt}-lgomp -L$ATP_PREFIX/lib -latp${qt}"      >> $export_vars
$ECHO "setenv ATP_LIB_STATIC ${qt}-lgomp $ATP_PREFIX/lib/libatp.a${qt}" >> $export_vars
#
chmod g+rw,o+r ${ATP_ROOT}/Makefile
chmod g+rw,o+r ${ATP_ROOT}/support/atp_vars
chmod g+rw,o+r ${ATP_ROOT}/support/export_atp_vars
#
$ECHO "config.csh is done"
