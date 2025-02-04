#!/bin/tcsh -f
# ************************************************************************
# *                                                                      *
# *   Script  config.csh  sets numerous variables needed for             *
# *   compilation and linking of package malo. It creates files with     *
# *   directives for FORTRAN-compilers and C-compiler.                   *
# *   It creates include files using template files for includes and     *
# *   local files with preferences.                                      *
# *                                                                      *
# *   Usage:  config.csh                                                 *
# *                                                                      *
# *  ###  12-OCT-2012 config.csh  v7.6   (c) L. Petrov 05-JAN-2024  ###  *
# *                                                                      *
# ************************************************************************
#
setenv SUPPORT_PATH `dirname $0`
#
cd     $SUPPORT_PATH
cd ../
setenv CONF_LOG   ${MALO_ROOT}/temp/conf.log
if (    -f ${MALO_ROOT}/Makefile ) then
     rm -f ${MALO_ROOT}/Makefile
endif
set MALO_OS = `uname`
switch ( $MALO_OS )
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
     setenv MALO_SOLIB libmalo.so
     breaksw
   case "Darwin":
     set ECHO = "/bin/echo"
     set qt = '"'
     setenv MALO_SOLIB libmalo.1.dylib
     breaksw
endsw
if ( `uname` == "Linux" ) then
     set num_threads     = `cat /proc/cpuinfo | grep "core id" | wc -l`
     set num_thr_per_cpu = `lscpu | grep '^Thread(s) per core:' | awk '{print $4}'`
     set num_cores = `expr $num_threads / $num_thr_per_cpu`
else
     set num_cores = `sysctl -n  machdep.cpu.core_count`
endif
#
# --- Check petools
#
$ECHO "config.csh: Check petools ... \c" | tee -a  $CONF_LOG
if ( -d $PETOOLS_PREFIX == 0 ) then
     echo "malo config: petools directory $PETOOLS_PREFIX was not found"
     exit 1
endif
if ( -d $PETOOLS_PREFIX/lib == 0 ) then
     echo "malo config: petools directory $PETOOLS_PREFIX/lib was not found"
     exit 1
endif
if ( -d $PETOOLS_PREFIX/bin == 0 ) then
     echo "malo config: petools directory $PETOOLS_PREFIX/bin was not found"
     exit 1
endif
if ( -f $PETOOLS_PREFIX/bin/petools_vars ) then
      source $PETOOLS_PREFIX/bin/petools_vars 
      if ( $PETOOLS_VERSION < $PETOOLS_VERSION_MIN ) then
           echo "malo config: you have too old petools version $PETOOLS_VERSION"
           echo "malo config: while $PETOOLS_VERSION_MIN version is needed. Please upgrade"
           exit 1
      endif
endif
if ( "$PETOOLS_VERSION" < "$PETOOLS_VERSION_MIN" ) then
     echo "Eeeeh. You have petools version $PETOOLS_VERSION, but "
     echo "$PETOOLS_VERSION_MIN is needed. Please upgrade"
     exit 1
endif
$ECHO "ok"
$ECHO "config.csh: Check ners ... \c" | tee -a  $CONF_LOG
#
if ( -d  $NERS_PREFIX == 0 ) then
     echo "malo config: ners install directory $NERS_PREFIX was not found"
     echo "malo config: Please install ners"
     exit 1
endif
#
if ( -d  $NERS_PREFIX/include == 0 ) then
     echo "malo config: ners include directory $NERS_PREFIX/include was not found"
     echo "malo config: Please install ners"
     exit 1
endif
#
if ( -f  $NERS_PREFIX/include/ners_local.i == 0 ) then
     echo "malo config: ners version file $NERS_PREFIX/include/ners_local.i was not found"
     echo "malo config: Please install ners"
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
# --- Check vtd
#
$ECHO "ok"
$ECHO "config.csh: Check vtd ... \c" | tee -a  $CONF_LOG
if ( -d $VTD_PREFIX == 0 ) then
     echo "malo config: vtd directory $VTD_PREFIX was not found. Please check value of --with-vtd argument"
     exit 1
endif
if ( -d $VTD_PREFIX/lib == 0 ) then
     echo "malo config: vtd directory $VTD_PREFIX/lib was not found. Please check value of --with-vtd argument"
     exit 1
endif
if ( -d $VTD_PREFIX/include == 0 ) then
     echo "malo config: vtd directory $VTD_PREFIX/include was not found. Please check value of --with-vtd argument"
     exit 1
endif
if ( -f $VTD_PREFIX/include/vtd.i == 0 ) then
     echo "malo config: vtd include file $VTD_PREFIX/include/vtd.i was not found. Please check value of --with-vtd argument"
     exit 1
endif
#
# --- Check cfitsio
#
$ECHO "ok"
$ECHO "config.csh: Check cfitsio ... \c" | tee -a  $CONF_LOG
if ( -d $VTD_CFITSIO_PREFIX == 0 ) then
     echo "malo config: cfitgsio directory $VTD_CFITSIO_PREFIX was not found"
     exit 1
endif
if ( -d $VTD_CFITSIO_PREFIX/lib == 0 ) then
     echo "malo config: cfitsio directory $VTD_CFITSIO_PREFIX/lib was not found"
     exit 1
endif
#
# --- Check fourpack
#
$ECHO "ok"
$ECHO "config.csh: Check fourpack ... \c" | tee -a  $CONF_LOG
if ( -d $FOURPACK_PREFIX == 0 ) then
     echo "malo config: fourpack directory $FOURPACK_PREFIX was not found"
     exit 1
endif
if ( -d $FOURPACK_PREFIX/lib == 0 ) then
     echo "malo config: fourpack directory $FOURPACK_PREFIX/lib was not found"
     exit 1
endif
if ( -d $FOURPACK_PREFIX/bin == 0 ) then
     echo "malo config: fourpack directory $FOURPACK_PREFIX/bin was not found"
     exit 1
endif
if ( -f $FOURPACK_PREFIX/bin/fourpack_vars ) then
      source $FOURPACK_PREFIX/bin/fourpack_vars 
      if ( $FOURPACK_VERSION < $FOURPACK_VERSION_MIN ) then
           echo "malo config: you have too old library fourpack version $FOURPACK_VERSION"
           echo "malo config: while $FOURPACK_VERSION_MIN version is needed. Please upgrade"
           exit 1
      endif
endif
$ECHO "ok"
$ECHO "config.csh: Check fftw ... \c" | tee -a  $CONF_LOG
if ( -d $FFTW_DIR == 0 ) then
     echo "malo config: fftw directory $FFTW_DIR was not found. Please check --with-fftw argument"
     exit 1
endif
if ( -d $FFTW_DIR/lib == 0 ) then
     echo "malo config: fftw directory $FFTW_DIR/lib was not found. Please check --with-fftw argument"
     exit 1
endif
if ( -d $FFTW_DIR/include == 0 ) then
     echo "malo config: fftw directory $FFTW_DIR/include was not found. Please check --with-fftw argument"
     exit 1
endif
if ( -f $FFTW_DIR/include/fftw3.f == 0 ) then
     echo "fourpack config: include files for fftw were not found"
     echo "check option --with-fftw"
     echo "Error in configuration"
     exit 1
endif
#
# --- Check netcdf
#
$ECHO "ok"
$ECHO "config.csh: Check netcdf ...\c" | tee -a  $CONF_LOG
if ( -d $NETCDF_PREFIX == 0 ) then
     echo "malo config: netcdf directory $NETCDF_PREFIX was not found"
     exit 1
endif
if ( -d $NETCDF_PREFIX/lib == 0 ) then
     echo "malo config: netcdf directory $NETCDF_PREFIX/lib was not found"
     exit 1
endif
if ( -d $NETCDF_PREFIX/include == 0 ) then
     echo "malo config: netcdf directory $NETCDF_PREFIX/include was not found"
     exit 1
endif
#
# --- Check hdf5
#
$ECHO "ok"
$ECHO "config.csh: Check hdf5 ...\c" | tee -a  $CONF_LOG
if ( -d $HDF5_PREFIX == 0 ) then
     echo "malo config: hdf5 directory $HDF5_PREFIX was not found"
     exit 1
endif
if ( -d $HDF5_PREFIX/lib == 0 ) then
     echo "malo config: hdf5 directory $HDF5_PREFIX/lib was not found"
     exit 1
endif
if ( -d $HDF5_PREFIX/include == 0 ) then
     echo "malo config: hdf5 directory $HDF5_PREFIX/include was not found"
     exit 1
endif
#
# --- Check hdf4
#
$ECHO "ok"
$ECHO "config.csh: Check hdf4 ...\c" | tee -a  $CONF_LOG
if ( -d $HDF4_PREFIX == 0 ) then
     echo "malo config: hdf4 directory $HDF4_PREFIX was not found"
     exit 1
endif
if ( -d $HDF4_PREFIX/lib == 0 ) then
     echo "malo config: hdf4 directory $HDF4_PREFIX/lib was not found"
     exit 1
endif
if ( -d $HDF4_PREFIX/include == 0 ) then
     echo "malo config: hdf4 directory $HDF4_PREFIX/include was not found"
     exit 1
endif
#
# --- Check w3
#
$ECHO "ok"
$ECHO "config.csh: Check w3 ...\c" | tee -a  $CONF_LOG
if ( -d $W3_PREFIX == 0 ) then
     echo "malo config: w3 directory $W3_PREFIX was not found"
     exit 1
endif
if ( -d $W3_PREFIX/lib == 0 ) then
     echo "malo config: w3 directory $W3_PREFIX/lib was not found"
     exit 1
endif
if ( -d $W3_PREFIX/include == 0 ) then
     echo "malo config: w3 directory $W3_PREFIX/include was not found"
     exit 1
endif
#
# --- Check spd_client
#
$ECHO "ok"
$ECHO "config.csh: Check spd_client ...\c" | tee -a  $CONF_LOG
if ( -f $SPD_CLIENT_DIR/lib/libspc.a == 0 ) then
     echo "malo config: spd_client file $SPD_CLIENT_DIR/lib/libspc.a  was not found"
     echo "malo config: Please install spd_client"
     exit 1
endif
if ( -f $SPD_CLIENT_DIR/include/spd.i == 0 ) then
     echo "malo config: spd_client file $SPD_CLIENT_DIR/include/spd.i was not found"
     echo "malo config: Please install spd_client"
     exit 1
endif
if ( -f $SPD_CLIENT_DIR/include/spd_local.i == 0 ) then
     echo "malo config: spd_client file $SPD_CLIENT_DIR/include/spd_local.i was not found"
     echo "malo config: Please install spd_client"
     exit 1
endif
if ( `cat $SPD_CLIENT_DIR/include/spd_local.i | grep "SPC__PETOOLS = " | grep no` != "" ) then
     echo "malo config: spd_client was installed without petools"
     echo "malo config: Please re-install spd_client with petools"
     exit 1
endif
#
# --- Check whether gmake is found in your system
#
$ECHO "ok"
$ECHO "config.csh: Check gmake ...\c" | tee -a  $CONF_LOG
set gmake_string = `which gmake`
if ( "$gmake_string" == "" ) then 
     echo "gmake was not found in your system."
     exit 1
endif
$ECHO "ok"
#
set DATE_ISO   = `date "+%Y.%m.%d_%H:%M:%S"`
set root_len   = `echo $MALO_ROOT         | awk '{print length($1)}'`
set pref_len   = `echo $MALO_PREFIX       | awk '{print length($1)}'`
set share_len  = `echo $MALO_SHARE        | awk '{print length($1)}'`
set model_len  = `echo $MALO_MODEL        | awk '{print length($1)}'`
set dev_len    = `echo $MALO_DEV_MODEL    | awk '{print length($1)}'`
set script_len = `echo $MALO_SCRIPT       | awk '{print length($1)}'`
set version_len = `echo $MALO_VERSION     | awk '{print length($1)}'`
if ( $model_len == 0 ) set model_len = 1
if ( $dev_len   == 0 ) set dev_len   = 1
cat $MALO_ROOT/include/malo_local_templ.i | \
    sed "s@%%@Automatically generated on $DATE_ISO from malo_local_templ.i@g" | \
    sed "s@%ROOT_LEN%@$root_len@g"            | \
    sed "s@%PREFIX_LEN%@$pref_len@g"          | \
    sed "s@%SHARE_LEN%@$share_len@g"          | \
    sed "s@%MODEL_LEN%@$model_len@g"          | \
    sed "s@%MODEL_DEV_LEN%@$dev_len@g"        | \
    sed "s@%SCRIPT_LEN%@$script_len@g"        | \
    sed "s@%VERSION_LEN%@$version_len@g"      | \
    sed "s@%ROOT_VAL%@$MALO_ROOT@g"           | \
    sed "s@%PREFIX_VAL%@$MALO_PREFIX@g"       | \
    sed "s@%SHARE_VAL%@$MALO_SHARE@g"         | \
    sed "s@%MODEL_VAL%@$MALO_MODEL@g"         | \
    sed "s@%MODEL_DEV_VAL%@$MALO_DEV_MODEL@g" | \
    sed "s@%SCRIPT_VAL%@$MALO_SCRIPT@g"       | \
    sed "s@%VERSION_VAL%@$MALO_VERSION@g"       \
    > $MALO_ROOT/include/malo_local.i 
#
echo '#\!/bin/bash -f'                                   > $MALO_ROOT/support/malo_service_update.sh
echo 'ulimit -s 2000000 > /dev/null 2>&1'               >> $MALO_ROOT/support/malo_service_update.sh
echo "$MALO_ROOT/script/malo_service_update.sh "'$1 $2' >> $MALO_ROOT/support/malo_service_update.sh
chmod o+x,g+x,u+x $MALO_ROOT/support/malo_service_update.sh
set    date_stamp  = `date "+%Y.%m.%d-%H:%M:%S" | tr "[a-z]" "[A-Z]"`
set    host_name   = `uname -n`
#
echo "malo config  $date_stamp  on  $host_name" >>! $CONF_LOG
##
set out_file = ${MALO_ROOT}/Makefile
#
cat   ${MALO_ROOT}/Makefile.in1 > $out_file 
$ECHO "MALO_ROOT        = $MALO_ROOT"                    >> $out_file
$ECHO "PETOOLS_PREFIX   = $PETOOLS_PREFIX"               >> $out_file
$ECHO "PETOOLS_LIB      = $PETOOLS_LIB"                  >> $out_file
$ECHO "CFITSIO_PREFIX   = $VTD_CFITSIO_PREFIX"           >> $out_file
$ECHO "NERS_LIB         = $NERS_PREFIX/lib"              >> $out_file
$ECHO "NERS_INC         = $NERS_PREFIX/include"          >> $out_file
$ECHO "SPD_CLIENT_DIR   = $SPD_CLIENT_DIR"               >> $out_file
$ECHO "SPD_CLIENT_LIB   = $SPD_CLIENT_DIR/lib"           >> $out_file
$ECHO "SOLVE_EXTRA_LIB  = $SOLVE_EXTRA_LIB"              >> $out_file
$ECHO "SOLVE_LIB_VEC    = $SOLVE_LIB_VEC"                >> $out_file
$ECHO "SOLVE_LIB_BLAS   = $SOLVE_LIB_BLAS"               >> $out_file
$ECHO "FOURPACK_PREFIX  = $FOURPACK_PREFIX"              >> $out_file
$ECHO "NETCDF_PREFIX    = $NETCDF_PREFIX"                >> $out_file
$ECHO "HDF4_PREFIX      = $HDF4_PREFIX"                  >> $out_file
$ECHO "HDF5_PREFIX      = $HDF5_PREFIX"                  >> $out_file
$ECHO "W3_PREFIX        = $W3_PREFIX"                    >> $out_file
$ECHO "MK5_C            = $MK5_C"                        >> $out_file
$ECHO "MK5_F95_OPT      = $MK5_F95_OPT"                  >> $out_file
$ECHO "MK5_F95          = $MK5_F95"                      >> $out_file
$ECHO "MK5_LINK         = $MK5_LINK"                     >> $out_file
$ECHO "MALO_ROOT        = $MALO_ROOT"                    >> $out_file
$ECHO "MALO_PREFIX      = $MALO_PREFIX"                  >> $out_file
$ECHO "MALO_SCRIPT      = $MALO_SCRIPT"                  >> $out_file
$ECHO "MALO_SHARE       = $MALO_SHARE"                   >> $out_file
$ECHO "MALO_MODEL       = $MALO_MODEL"                   >> $out_file
$ECHO "MALO_DEV_MODEL   = $MALO_DEV_MODEL"               >> $out_file
$ECHO "MALO_BIN         = $MALO_PREFIX/bin"              >> $out_file
$ECHO "MALO_LIB         = $MALO_PREFIX/lib"              >> $out_file
$ECHO "MALO_INC         = $MALO_PREFIX/include"          >> $out_file
$ECHO "MALO_VERSION     = $MALO_VERSION"                 >> $out_file
$ECHO "MALO_PREFIX      = $MALO_PREFIX"                  >> $out_file
$ECHO "VTD_PREFIX       = $VTD_PREFIX"                   >> $out_file
$ECHO "MALO_SOLIB       = $MALO_SOLIB"                   >> $out_file
$ECHO "MALO_WEB_EXE     = $MALO_WEB_EXE"                 >> $out_file
$ECHO "MALO_BACKUP_DIR  = $MALO_BACKUP_DIR"              >> $out_file
$ECHO "MALO_WEB_DIR     = $MALO_WEB_DIR"                 >> $out_file
$ECHO "MALO_OS          = $MALO_OS"                      >> $out_file
$ECHO "NUM_PROC         = $num_cores"                    >> $out_file
cat   ${MALO_ROOT}/Makefile.in2                          >> $out_file
#
set out_vars = ${MALO_ROOT}/support/malo_vars
if ( -f $out_vars ) rm -f $out_vars
$ECHO "#\!/bin/csh"                                            > $out_vars
$ECHO "setenv MALO_ROOT        $MALO_ROOT"                    >> $out_vars
$ECHO "setenv PETOOLS_PREFIX   $PETOOLS_PREFIX"               >> $out_vars
$ECHO "setenv PETOOLS_LIB      ${qt}$PETOOLS_LIB${qt}"        >> $out_vars
$ECHO "setenv CFITSIO_PREFIX   ${qt}$VTD_CFITSIO_PREFIX${qt}" >> $out_vars
$ECHO "setenv NERS_LIB         $NERS_PREFIX/lib"              >> $out_vars
$ECHO "setenv NERS_INC         $NERS_PREFIX/include"          >> $out_vars
$ECHO "setenv SPD_CLIENT_DIR   $SPD_CLIENT_DIR"               >> $out_vars
$ECHO "setenv SPD_CLIENT_LIB   $SPD_CLIENT_DIR/lib"           >> $out_vars
$ECHO "setenv SOLVE_EXTRA_LIB  ${qt}$SOLVE_EXTRA_LIB${qt}"    >> $out_vars
$ECHO "setenv SOLVE_LIB_VEC    ${qt}$SOLVE_LIB_VEC${qt}"      >> $out_vars
$ECHO "setenv SOLVE_LIB_BLAS   ${qt}$SOLVE_LIB_BLAS${qt}"     >> $out_vars
$ECHO "setenv FOURPACK_PREFIX  $FOURPACK_PREFIX"              >> $out_vars
$ECHO "setenv HDF4_PREFIX      $HDF4_PREFIX"                  >> $out_vars
$ECHO "setenv HDF5_PREFIX      $HDF5_PREFIX"                  >> $out_vars
$ECHO "setenv NERS_PREFIX      $NERS_PREFIX"                  >> $out_vars
$ECHO "setenv VTD_PREFIX       $VTD_PREFIX"                   >> $out_vars
$ECHO "setenv W3_PREFIX        $W3_PREFIX"                    >> $out_vars
$ECHO "setenv NETCDF_PREFIX    $NETCDF_PREFIX"                >> $out_vars
$ECHO "setenv MK5_C            ${qt}$MK5_C${qt}"              >> $out_vars
$ECHO "setenv MK5_F95_OPT      ${qt}$MK5_F95_OPT${qt}"        >> $out_vars
$ECHO "setenv MK5_F95          ${qt}$MK5_F95${qt}"            >> $out_vars
$ECHO "setenv MK5_LINK         ${qt}$MK5_LINK${qt}"           >> $out_vars
$ECHO "setenv MALO_ROOT        $MALO_ROOT"                    >> $out_vars
$ECHO "setenv MALO_PREFIX      $MALO_PREFIX"                  >> $out_vars
$ECHO "setenv MALO_SCRIPT      $MALO_SCRIPT"                  >> $out_vars
$ECHO "setenv MALO_SHARE       $MALO_SHARE"                   >> $out_vars
$ECHO "setenv MALO_MODEL       $MALO_MODEL"                   >> $out_vars
$ECHO "setenv MALO_DEV_MODEL   $MALO_DEV_MODEL"               >> $out_vars
$ECHO "setenv MALO_BIN         $MALO_PREFIX/bin"              >> $out_vars
$ECHO "setenv MALO_INC         $MALO_PREFIX/include"          >> $out_vars
$ECHO "setenv MALO_LIB         ${qt}-L$MALO_PREFIX/lib -lmalo -L$VTD_PREFIX/lib -lvtd -L$VTD_CFITSIO_PREFIX/lib  -lcfitsio -L$NERS_PREFIX/lib -lners -L$SPD_CLIENT_DIR/lib -lspc $FOURPACK_LIB $FFTW_LIB -L$NETCDF_PREFIX/lib -lnetcdff -lnetcdf -L$HDF5_PREFIX/lib -lhdf5 -lhdf5_hl -lhdf5_fortran -L$HDF4_PREFIX/lib -ldf -lmfhdf -L$W3_PREFIX/lib -lw3 $PETOOLS_LIB${qt}" >> $out_vars
$ECHO "setenv MALO_SOLIB       $MALO_SOLIB"                   >> $out_vars
$ECHO "setenv MALO_OS          $MALO_OS"                      >> $out_vars
$ECHO "setenv MALO_VERSION     $MALO_VERSION"                 >> $out_vars
$ECHO "source $PETOOLS_PREFIX/bin/petools_vars"               >> $out_vars
$ECHO "source $FOURPACK_PREFIX/bin/fourpack_vars"             >> $out_vars
$ECHO "umask 0022"                                            >> $out_vars
#
source ${MALO_ROOT}/support/malo_vars
#
cd $MALO_ROOT
make uninstall_include
#
echo "config.csh is done"
