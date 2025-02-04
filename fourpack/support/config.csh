#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Script  config.csh  sets numerous variables needed for             *
# *   compilation and linking fourpack. It creates files with directives *
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
setenv CONF_LOG   ${FOURPACK_ROOT}/temp/conf.log
if (    -f ${FOURPACK_ROOT}/Makefile ) then
     rm -f ${FOURPACK_ROOT}/Makefile
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
if ( "$FFTW_DIR" != "" ) then
     if ( "$MKL_DIR" != "" ) then
          echo "fourpack config: --with-fftw and --with-mkl options are mutually exclisive"
          echo "Error in configuration"
          exit 1
     endif
endif
#
if ( "$MKL_DIR" != "" ) then
     if ( "$FFTW_DIR" != "" ) then
          echo "fourpack config:  options are mutually exclisive"
          echo "Error in configuration"
          exit 1
     endif
endif
#
$ECHO "config.csh: Check petools ... \c" | tee -a  $CONF_LOG
if ( -d $PETOOLS_DIR == 0 ) then
     echo "fourpack config: petools directory $PETOOLS_DIR was not found"
     echo "check option --with-petools"
     echo "Error in configuration"
     exit 1
endif
if ( -d $PETOOLS_DIR/lib == 0 ) then
     echo "fourpack config: petools directory $PETOOLS_DIR/lib was not found"
     echo "check option --with-petools"
     echo "Error in configuration"
     exit 1
endif
if ( -d $PETOOLS_DIR/bin == 0 ) then
     echo "fourpack config: petools directory $PETOOLS_DIR/bin was not found"
     echo "check option --with-petools"
     echo "Error in configuration"
     exit 1
endif
    if ( -f $PETOOLS_DIR/lib/libpetools.so == 0 && $PETOOLS_DIR/lib/libpetools.dylib == 0 ) then
     echo "fourpack config: petools file $PETOOLS_DIR/lib/libpetools.so or $PETOOLS_DIR/lib/libpetools.dylib was not found"
     echo "check option --with-petools"
     echo "Error in configuration"
     exit 1
endif
$ECHO "ok"
source $PETOOLS_DIR/bin/petools_vars
$ECHO "config.csh: Check fftw include files ... \c" | tee -a  $CONF_LOG
if ( -f $FFTW_DIR/include/fftw3.f == 0 ) then
     echo "fourpack config: include files for fftw were not found"
     echo "check option --with-fftw"
     echo "Error in configuration"
     exit 1
endif
$ECHO "ok"
if ( "$MKL_DIR" != "" ) then
     $ECHO "config.csh: Check mkl include files ... \c" | tee -a  $CONF_LOG
     if ( -f $MKL_DIR/include/mkl_dfti.f90 == 0 ) then
          echo "fourpack config: include files for mkl were not found"
          exit 1
     endif  
     cd src
     $MK5_FC -c $MKL_DIR/include/mkl_dfti.f90 
     set status_comp = $status
     cd ../
     if ( $status_comp != 0 ) then
          echo "fourpack config: Failure to compile mkl include file $MKL_DIR/include/mkl_dfti.f90"
          echo "Error in configuration"
     endif
#
     if ( -d "$MKL_DIR/lib/em64t" ) then
          setenv MKL_LIB_PREF "em64t"
       else if ( -d "$MKL_DIR/lib/intel64_lin" ) then
          setenv MKL_LIB_PREF "intel64_lin"
       else if ( -d "$MKL_DIR/lib/32" ) then
          setenv MKL_LIB_PREF "32"
       else
          echo "fourpack config: Failure to find apporiate MKL_LIB directory: "
          echo "Did not find $MKL_DIR/lib/em64t"
          echo "Did not find $MKL_DIR/lib/intel64_lin"
          echo "Error in configuration"
     endif
#
     if ( -d $MKL_DIR/lib/${MKL_LIB_PREF}/libguide.a || -d $MKL_DIR/lib/${MKL_LIB_PREF}/libguide.so ) then 
           setenv MKL_LIBS "-L${MKL_DIR}/lib/${MKL_LIB_PREF} -lmkl_intel_thread -lmkl_intel_lp64 -lmkl_core -lguide"
	 else
	   if ( "echo $MK5_F95 | grep gfortran" == "" ) then 
                setenv MKL_LIBS "-L${MKL_DIR}/lib/${MKL_LIB_PREF} -lmkl_intel_thread -lmkl_intel_lp64 -lmkl_core"
	      else
                setenv MKL_LIBS        "-L${MKL_DIR}/lib/${MKL_LIB_PREF} -lmkl_gf_lp64 -lmkl_gnu_thread -lmkl_core -lgomp -ldl"
           endif
     endif
     $ECHO "ok"
endif  
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
set out_file = ${FOURPACK_ROOT}/Makefile
if ( "$PETOOLS_VERSION" < "$PETOOLS_VERSION_MIN" ) then 
     echo "Eeeeh. You have petools version $PETOOLS_VERSION, but "
     echo "$PETOOLS_VERSION_MIN is needed. Please upgrade" 
     echo "Error in configuration"
     exit 1 
endif
#
$ECHO "config.csh: Check fftw library... \c" | tee -a  $CONF_LOG
${FOURPACK_ROOT}/support/fftw_test.csh $FFTW_DIR non_openmp >>& $CONF_LOG
if ( $status == 0 ) then
     $ECHO "ok"
  else 
     echo "Error in configuration"
     echo "Test of fftw library failed. Please check lof file $CONF_LOG"
     exit 1 
endif
#
$ECHO "config.csh: Check openmp fftw library... \c" | tee -a  $CONF_LOG
${FOURPACK_ROOT}/support/fftw_test.csh $FFTW_DIR openmp >>& $CONF_LOG
if ( $status == 0 ) then
     $ECHO "ok"
  else 
     echo "Error in configuration"
     echo "Test of openmp fftw library failed. Please check lof file $CONF_LOG"
     echo "NB: you should complile fftw with openmp support"
     exit 1 
endif
#
if ( $FOURPACK_NOOPT == "YES" ) then
     setenv MK5_F95        "$MK5_F95_NOOPT"
     setenv MK5_F95_OPT    "$MK5_F95_NOOPT"
     setenv MK5_F95_OPTEST "$MK5_F95_NOOPT"
endif
#
if ( $MKL_DIR == "" ) then
     setenv MK5_F95     "$MK5_F95     -I$FFTW_DIR/include"
     setenv MK5_F95_OPT "$MK5_F95_OPT -I$FFTW_DIR/include"
     setenv MK5_F95_OPTEST "$MK5_F95_OPTEST -I$FFTW_DIR/include"
   else 
     setenv MK5_F95     "$MK5_F95     -I$FFTW_DIR/include -I$MKL_DIR -D MKL"
     setenv MK5_F95_OPT "$MK5_F95_OPT -I$FFTW_DIR/include -I$MKL_DIR -D MKL"
     setenv MK5_F95_OPTEST "$MK5_F95_OPTEST -I$FFTW_DIR/include -I$MKL_DIR -D MKL"
endif
#
if ( `uname` == "Linux" ) then
     set num_threads     = `cat /proc/cpuinfo | grep "core id" | wc -l`
     set num_thr_per_cpu = `lscpu | grep '^Thread(s) per core:' | awk '{print $4}'`
     set num_cores = `expr $num_threads / $num_thr_per_cpu`
else
     set num_cores = `sysctl -n  machdep.cpu.core_count`
endif
cat   ${FOURPACK_ROOT}/Makefile.in1 > $out_file 
$ECHO "FOURPACK_ROOT    = $FOURPACK_ROOT"                    >> $out_file
$ECHO "PETOOLS_PREFIX   = $PETOOLS_PREFIX"                   >> $out_file
$ECHO "PETOOLS_LIB      = $PETOOLS_LIB"                      >> $out_file
$ECHO "SOLVE_EXTRA_LIB  = $SOLVE_EXTRA_LIB"                  >> $out_file
$ECHO "SOLVE_LIB_VEC    = $SOLVE_LIB_VEC"                    >> $out_file
$ECHO "SOLVE_LIB_BLAS   = $SOLVE_LIB_BLAS"                   >> $out_file
$ECHO "MK5_C            = $MK5_C"                            >> $out_file
$ECHO "MK5_F95_OPTEST   = $MK5_F95_OPTEST"                   >> $out_file
$ECHO "MK5_F95_OPT      = $MK5_F95_OPT"                      >> $out_file
$ECHO "MK5_F95          = $MK5_F95"                          >> $out_file
$ECHO "MK5_LINK         = $MK5_LINK"                         >> $out_file
$ECHO "MK5_C_LINK       = $MK5_C_LINK"                       >> $out_file
$ECHO "FOURPACK_ROOT    = $FOURPACK_ROOT"                    >> $out_file
$ECHO "FOURPACK_PREFIX  = $FOURPACK_PREFIX"                  >> $out_file
$ECHO "FOURPACK_BIN     = $FOURPACK_PREFIX/bin"              >> $out_file
$ECHO "FOURPACK_LIB     = $FOURPACK_PREFIX/lib"              >> $out_file
$ECHO "FOURPACK_SHARED  = $FOURPACK_SHARED"                  >> $out_file
$ECHO "FFTW_DIR         = $FFTW_DIR"                         >> $out_file
$ECHO "FFTW_INC         = $FFTW_DIR/include"                 >> $out_file
$ECHO "FFTW_LIB         = -L$FFTW_DIR/lib -lfftw3 -lfftw3_omp -lfftw3f -lfftw3f_omp" >> $out_file
if ( $MKL_DIR == "" ) then
      $ECHO "MKL_INCLUDE = ${qt}${qt}"                       >> $out_file
      $ECHO "MKL_LIBS    = ${qt}${qt}"                       >> $out_file
   else 
      $ECHO "MKL_INCLUDE = $MKL_DIR/include"                 >> $out_file
      $ECHO "MKL_LIBS    = $MKL_LIBS"                        >> $out_file
endif
$ECHO "FOURPACK_OS      = "`uname`                        >> $out_file
$ECHO "FOURPACK_VERSION = $FOURPACK_VERSION"              >> $out_file
$ECHO "NUM_PROC         = $num_cores"                     >> $out_file
$ECHO "CONF_LOG         = $CONF_LOG"                      >> $out_file
$ECHO "BUILD_LOG        = $BUILD_LOG"                     >> $out_file
cat   ${FOURPACK_ROOT}/Makefile.in2                       >> $out_file
#
set out_vars = ${FOURPACK_ROOT}/support/fourpack_vars
if ( -f $out_vars ) rm -f $out_vars
$ECHO "#\!/bin/csh"                                          > $out_vars
$ECHO "setenv FOURPACK_ROOT    $FOURPACK_ROOT"              >> $out_vars
$ECHO "setenv PETOOLS_PREFIX   $PETOOLS_PREFIX"             >> $out_vars
$ECHO "setenv PETOOLS_LIB      ${qt}$PETOOLS_LIB${qt}"      >> $out_vars
$ECHO "setenv SOLVE_EXTRA_LIB  ${qt}$SOLVE_EXTRA_LIB${qt}"  >> $out_vars
$ECHO "setenv SOLVE_LIB_VEC    ${qt}$SOLVE_LIB_VEC${qt}"    >> $out_vars
$ECHO "setenv SOLVE_LIB_BLAS   ${qt}$SOLVE_LIB_BLAS${qt}"   >> $out_vars
$ECHO "setenv MK5_C            ${qt}$MK5_C${qt}"            >> $out_vars
$ECHO "setenv MK5_F95_OPTEST   ${qt}$MK5_F95_OPTEST${qt}"   >> $out_vars
$ECHO "setenv MK5_F95_OPT      ${qt}$MK5_F95_OPT${qt}"      >> $out_vars
$ECHO "setenv MK5_F95          ${qt}$MK5_F95${qt}"          >> $out_vars
$ECHO "setenv MK5_LINK         ${qt}$MK5_LINK${qt}"         >> $out_vars
$ECHO "setenv MK5_C_LINK       ${qt}$MK5_C_LINK${qt}"       >> $out_vars
$ECHO "setenv FOURPACK_ROOT    $FOURPACK_ROOT"              >> $out_vars
$ECHO "setenv FOURPACK_PREFIX  $FOURPACK_PREFIX"            >> $out_vars
$ECHO "setenv FOURPACK_BIN     $FOURPACK_PREFIX/bin"        >> $out_vars
$ECHO "setenv FOURPACK_LIB     $FOURPACK_PREFIX/lib"        >> $out_vars
$ECHO "setenv FOURPACK_SHARED  $FOURPACK_SHARED"            >> $out_vars
$ECHO "setenv FFTW_DIR         $FFTW_DIR"                   >> $out_vars
$ECHO "setenv FFTW_INC         $FFTW_DIR/include"           >> $out_vars
$ECHO "setenv FFTW_LIB         ${qt}-L$FFTW_DIR/lib -lfftw3 -lfftw3_omp -lfftw3f -lfftw3f_omp${qt}" >> $out_vars
if ( $MKL_DIR == "" ) then
     $ECHO "setenv MKL_INCLUDE  ${qt}${qt}"                 >> $out_vars
     $ECHO "setenv MKL_LIB      ${qt}${qt}"                 >> $out_vars
     $ECHO "setenv MKL_LIBS     ${qt}${qt}"                 >> $out_vars
   else 
     $ECHO "setenv MKL_INCLUDE      $MKL_DIR/include"       >> $out_vars
     $ECHO "setenv MKL_LIBS         ${qt}${MKL_LIBS}${qt}"  >> $out_vars
endif
$ECHO "setenv FOURPACK_OS      "`uname`                     >> $out_vars
$ECHO "setenv FOURPACK_VERSION $FOURPACK_VERSION"           >> $out_vars
#
source ${FOURPACK_ROOT}/support/fourpack_vars
#
set export_vars = ${FOURPACK_ROOT}/support/export_fourpack_vars
if ( -f $export_vars  ) rm -f $export_vars 
$ECHO "#\!/bin/csh"                                      > $export_vars
$ECHO "setenv FOURPACK_VERSION $FOURPACK_VERSION"       >> $export_vars
$ECHO "setenv FOURPACK_PREFIX  $FOURPACK_PREFIX"        >> $export_vars
$ECHO "setenv FFTW_INC         $FFTW_DIR/include"       >> $export_vars
$ECHO "setenv FFTW_LIB         ${qt}-L$FFTW_DIR/lib -lfftw3 -lfftw3_omp -lfftw3f -lfftw3f_omp${qt}" >> $export_vars
#
if ( -f $FOURPACK_PREFIX/include/fourpack.i           ) rm -f $FOURPACK_PREFIX/include/fourpack.i 
if ( -f $FOURPACK_PREFIX/include/fourpack_constants.i ) rm -f $FOURPACK_PREFIX/include/fourpack_constants.i
if ( $MKL_DIR == "" ) then
     $ECHO "setenv FOURPACK_LIB    ${qt}-lgomp -L$FOURPACK_PREFIX/lib -lfourpack${qt}" >> $export_vars
     $ECHO "setenv FOURPACK_LIB_STATIC ${qt}-lgomp $FOURPACK_PREFIX/lib/libfourpack.a${qt}" >> $export_vars
   else
     $ECHO "setenv FOURPACK_LIB     ${qt}-lgomp -L$FOURPACK_PREFIX/lib -lfourpack ${MKL_LIBS}${qt}" >> $export_vars
     $ECHO "setenv FOURPACK_LIB_STATIC     ${qt}-lgomp $FOURPACK_PREFIX/lib/libfourpack.a ${MKL_LIBS}${qt}" >> $export_vars
endif
chmod g+rw,o+r ${FOURPACK_ROOT}/Makefile
chmod g+rw,o+r ${FOURPACK_ROOT}/support/fourpack_vars
chmod g+rw,o+r ${FOURPACK_ROOT}/support/export_fourpack_vars
echo "config.csh is done"
