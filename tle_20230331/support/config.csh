#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Script  config.csh  sets numerous variables needed for             *
# *   compilation and linking tle. It creates files with directives      *
# *   for FORTRAN-compilers and C-compiler. It creates include files     *
# *   using template files for includes and local files with             *
# *   preferences.                                                       *
# *                                                                      *
# *   Usage:  config.csh                                                 *
# *                                                                      *
# *  ###  23-MAR-2023   config.csh  v1.1  (c)  L. Petrov 19-MAY-2024 ### *
# *                                                                      *
# ************************************************************************
#
setenv SUPPORT_PATH `dirname $0`
cd     $SUPPORT_PATH
cd ../
setenv CONF_LOG   ${TLE_ROOT}/temp/conf.log
if (    -f ${TLE_ROOT}/Makefile ) then
     rm -f ${TLE_ROOT}/Makefile
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
     echo "tle config: petools directory $PETOOLS_DIR was not found"
     echo "check option --with-petools"
     echo "Error in configuration"
     exit 1
endif
if ( -d $PETOOLS_DIR/lib == 0 ) then
     echo "tle config: petools directory $PETOOLS_DIR/lib was not found"
     echo "check option --with-petools"
     echo "Error in configuration"
     exit 1
endif
if ( -d $PETOOLS_DIR/bin == 0 ) then
     echo "tle config: petools directory $PETOOLS_DIR/bin was not found"
     echo "check option --with-petools"
     echo "Error in configuration"
     exit 1
endif
if ( -f $PETOOLS_DIR/lib/libpetools.so == 0 && -f $PETOOLS_DIR/lib/libpetools.dylib == 0 ) then
     echo "tle config: petools file $PETOOLS_DIR/lib/libpetools.so or $PETOOLS_DIR/lib/libpetools.dylib was not found"
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
set out_file = ${TLE_ROOT}/Makefile
if ( "$PETOOLS_VERSION" < "$PETOOLS_VERSION_MIN" ) then 
     echo "Eeeeh. You have petools version $PETOOLS_VERSION, but "
     echo "$PETOOLS_VERSION_MIN is needed. Please upgrade" 
     echo "Error in configuration"
     exit 1 
endif
#
# --- Check spc-client
#
if ( -d $SPC_PREFIX == 0 ) then
     echo "pima config: spd client directory $SPC_PREFIX was not found"
     exit 1
endif
if ( -d $SPC_PREFIX/lib == 0 ) then
     echo "pima config: spd client directory $SPC_PREFIX.lib was not found"
     exit 1
endif
if ( -d $SPC_PREFIX/include == 0 ) then
     echo "pima config: spd client directory $SPC_PREFIX/include was not found"
     exit 1
endif
if ( -f $SPC_PREFIX/lib/libspc.a == 0 ) then
     echo "pima config: spd client library $SPC_PREFIX/lib/libspc.a was not found"
     exit 1
endif
#
# --- Check vtd
#
if ( -d $VTD_PREFIX == 0 ) then
     echo "pima config: vtd directory $VTD_PREFIX was not found"
     exit 1
endif
if ( -d $VTD_PREFIX/lib == 0 ) then
     echo "pima config: vtd directory $VTD_PREFIX/lib was not found"
     exit 1
endif
if ( -d $VTD_PREFIX/bin == 0 ) then
     echo "pima config: vtd directory $VTD_PREFIX/bin was not found"
     exit 1
endif
if ( -f $VTD_PREFIX/bin/vtd_vars ) then
      source $VTD_PREFIX/bin/vtd_vars 
      if ( $VTD_VERSION < $VTD_VERSION_MIN ) then
           echo "pima config: you have too old vtd version $VTD_VERSION"
           echo "pima config: while $VTD_VERSION_MIN version is needed. Please upgrade"
           exit 1
      endif
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
set NERS_STANDALONE = `cat $NERS_PREFIX/include/ners_local.i | grep "NERS__NERS_STANDALONE = " | awk '{print substr($5,2,8)}' | sed 's@"@@g'`
if ( "$NERS_STANDALONE" == "YES" ) then
     echo "vtd config: ners package was configured without petools"
     echo "vtd config: Please re-configure and re-install ners with petools support"
     exit 1
endif
#
sed "s|@TLE_SHARE@|${TLE_PREFIX}/share|g" example/proto_tle_example_to_coo.f  > example/tle_example_to_coo.f
sed "s|@TLE_SHARE@|${TLE_PREFIX}/share|g" example/proto_tle_example_to_azel.f > example/tle_example_to_azel.f
sed "s|@TLE_SHARE@|${TLE_PREFIX}/share|g" example/proto_tle_dir_to_azel.f     > example/tle_dir_to_azel.f
#
if ( $TLE_NOOPT == "YES" ) then
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
cat   ${TLE_ROOT}/Makefile.in1 > $out_file 
$ECHO "PETOOLS_PREFIX     = $PETOOLS_PREFIX"                  >> $out_file
$ECHO "PETOOLS_LIB        = $PETOOLS_LIB"                     >> $out_file
$ECHO "VTD_LIB            = ${qt}$VTD_LIB${qt}"               >> $out_file
$ECHO "VTD_INC            = ${qt}$VTD_PREFIX/include${qt}"    >> $out_file
$ECHO "NERS_PREFIX        = $NERS_PREFIX"                     >> $out_file
$ECHO "NERS_LIB           = -L $NERS_PREFIX/lib -lners"       >> $out_file
$ECHO "NERS_INC           = $NERS_PREFIX/include"             >> $out_file
$ECHO "CFITSIO_LIB        = -L $CFITSIO_PREFIX/lib -lcfitsio" >> $out_file
$ECHO "SPC_LIB            = -L $SPC_PREFIX/lib -lspc"         >> $out_file
$ECHO "SOLVE_EXTRA_LIB    = $SOLVE_EXTRA_LIB"                 >> $out_file
$ECHO "SOLVE_LIB_VEC      = $SOLVE_LIB_VEC"                   >> $out_file
$ECHO "SOLVE_LIB_BLAS     = $SOLVE_LIB_BLAS"                  >> $out_file
$ECHO "SOLVE_LIB_PGPLOT   = $SOLVE_LIB_PGPLOT"                >> $out_file
$ECHO "SOLVE_LIB_PGPLOT_A = $SOLVE_LIB_PGPLOT_A"              >> $out_file
$ECHO "MK5_C              = $MK5_C"                           >> $out_file
$ECHO "MK5_F95_OPTEST     = $MK5_F95_OPTEST"                  >> $out_file
$ECHO "MK5_F95_OPT        = $MK5_F95_OPT"                     >> $out_file
$ECHO "MK5_F95_NOOPT      = $MK5_F95_NOOPT"                   >> $out_file
$ECHO "MK5_F95            = $MK5_F95"                         >> $out_file
$ECHO "MK5_LINK           = $MK5_LINK"                        >> $out_file
$ECHO "MK5_C_LINK         = $MK5_C_LINK"                      >> $out_file
$ECHO "TLE_ROOT           = $TLE_ROOT"                        >> $out_file
$ECHO "TLE_PREFIX         = $TLE_PREFIX"                      >> $out_file
$ECHO "TLE_BIN            = $TLE_PREFIX/bin"                  >> $out_file
$ECHO "TLE_LIB            = $TLE_PREFIX/lib"                  >> $out_file
$ECHO "TLE_OS             = "`uname`                          >> $out_file
$ECHO "TLE_VERSION        = $TLE_VERSION"                     >> $out_file
$ECHO "NUM_PROC           = $num_cores"                       >> $out_file
$ECHO "CONF_LOG           = $CONF_LOG"                        >> $out_file
$ECHO "BUILD_LOG          = $BUILD_LOG"                       >> $out_file
$ECHO " "                                                     >> $out_file
cat   ${TLE_ROOT}/Makefile.in2                                >> $out_file
#
set out_vars = ${TLE_ROOT}/support/tle_vars
if ( -f $out_vars ) rm -f $out_vars
$ECHO "#\!/bin/csh"                                          > $out_vars
$ECHO "setenv PETOOLS_PREFIX     $PETOOLS_PREFIX"                  >> $out_vars
$ECHO "setenv PETOOLS_LIB        ${qt}$PETOOLS_LIB${qt}"           >> $out_vars
$ECHO "setenv VTD_LIB            ${qt}$VTD_LIB${qt}"               >> $out_vars
$ECHO "setenv VTD_INC            ${qt}$VTD_PREFIX/include${qt}"        >> $out_vars
$ECHO "setenv NERS_LIB           ${qt}-L $NERS_PREFIX/lib -lners${qt}" >> $out_vars
$ECHO "setenv NERS_INC           $NERS_PREFIX/include"             >> $out_vars
$ECHO "setenv SPC_PREFIX         $SPC_PREFIX"                      >> $out_vars
$ECHO "setenv SPC_LIB            ${qt}-L $SPC_PREFIX/lib -lspc${qt}"   >> $out_vars
$ECHO "setenv CFITSIO_PREFIX     $CFITSIO_PREFIX"                  >> $out_vars
$ECHO "setenv SOLVE_EXTRA_LIB    ${qt}$SOLVE_EXTRA_LIB${qt}"       >> $out_vars
$ECHO "setenv SOLVE_LIB_VEC      ${qt}$SOLVE_LIB_VEC${qt}"         >> $out_vars
$ECHO "setenv SOLVE_LIB_BLAS     ${qt}$SOLVE_LIB_BLAS${qt}"        >> $out_vars
$ECHO "setenv SOLVE_LIB_PGPLOT   ${qt}$SOLVE_LIB_PGPLOT${qt}"      >> $out_vars
$ECHO "setenv SOLVE_LIB_PGPLOT_A ${qt}$SOLVE_LIB_PGPLOT_A${qt}"    >> $out_vars
$ECHO "setenv MK5_C              ${qt}$MK5_C${qt}"                 >> $out_vars
$ECHO "setenv MK5_F95_OPTEST     ${qt}$MK5_F95_OPTEST${qt}"        >> $out_vars
$ECHO "setenv MK5_F95_OPT        ${qt}$MK5_F95_OPT${qt}"           >> $out_vars
$ECHO "setenv MK5_F95_NOOPT      ${qt}$MK5_F95_OPT${qt}"           >> $out_vars
$ECHO "setenv MK5_F95            ${qt}$MK5_F95${qt}"               >> $out_vars
$ECHO "setenv MK5_LINK           ${qt}$MK5_LINK${qt}"              >> $out_vars
$ECHO "setenv MK5_C_LINK         ${qt}$MK5_C_LINK${qt}"            >> $out_vars
$ECHO "setenv TLE_ROOT           $TLE_ROOT"                        >> $out_vars
$ECHO "setenv TLE_PREFIX         $TLE_PREFIX"                      >> $out_vars
$ECHO "setenv TLE_BIN            $TLE_PREFIX/bin"                  >> $out_vars
$ECHO "setenv TLE_LIB            $TLE_PREFIX/lib"                  >> $out_vars
$ECHO "setenv TLE_OS             "`uname`                          >> $out_vars
$ECHO "setenv TLE_VERSION        $TLE_VERSION"                     >> $out_vars
#
source ${TLE_ROOT}/support/tle_vars
#
set export_vars = ${TLE_ROOT}/support/export_tle_vars
if ( -f $export_vars  ) rm -f $export_vars 
$ECHO "#\!/bin/csh"                                      > $export_vars
$ECHO "setenv TLE_VERSION $TLE_VERSION"   >> $export_vars
$ECHO "setenv TLE_PREFIX  $TLE_PREFIX"    >> $export_vars
#
if ( -f $TLE_PREFIX/include/tle.i ) rm -f $TLE_PREFIX/include/tle.i 
if ( -f $TLE_PREFIX/include/sat_eph.i    ) rm -f $TLE_PREFIX/include/sat_eph.i
#
$ECHO "setenv TLE_LIB    ${qt}-lgomp -L$TLE_PREFIX/lib -ltle${qt}"      >> $export_vars
$ECHO "setenv TLE_LIB_STATIC ${qt}-lgomp $TLE_PREFIX/lib/libtle.a${qt}" >> $export_vars
#
chmod g+rw,o+r ${TLE_ROOT}/Makefile
chmod g+rw,o+r ${TLE_ROOT}/support/tle_vars
chmod g+rw,o+r ${TLE_ROOT}/support/export_tle_vars
#
$ECHO "config.csh is done"
