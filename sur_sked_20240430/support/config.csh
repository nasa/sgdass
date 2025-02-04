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
# *  ###  07-JAN-2013  config.csh  v1.4  (c) L. Petrov  17-MAY-2024 ###  *
# *                                                                      *
# ************************************************************************
#
setenv SUPPORT_PATH `dirname $0`
#
set cfitsio_min_version = 3.0
#
cd     $SUPPORT_PATH
cd ../
setenv CONF_LOG   ${SUR_SKED_ROOT}/temp/conf.log
setenv BUILD_LOG  ${SUR_SKED_ROOT}/temp/build.log
if (    -f ${SUR_SKED_ROOT}/Makefile ) then
     rm -f ${SUR_SKED_ROOT}/Makefile
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
     breaksw
endsw
set    date_stamp  = `date "+%Y.%m.%d-%H:%M:%S" | tr "[a-z]" "[A-Z]"`
set    host_name   = `uname -n`
#
# --- Check ners
#
$ECHO "config.csh: Check ners ... \c" | tee -a  $CONF_LOG
if ( -d $NERS_PREFIX == 0 ) then
     echo "sur_sked config: ners directory $NERS_PREFIX was not found"
     exit 1
endif
if ( -d $NERS_PREFIX/lib == 0 ) then
     echo "sur_sked config: ners directory $NERS_PREFIX/lib was not found"
     exit 1
endif
if ( -d $NERS_PREFIX/bin == 0 ) then
     echo "sur_sked config: ners directory $NERS_PREFIX/bin was not found"
     exit 1
endif
if ( -f $NERS_PREFIX/bin/ners_vars ) then
      source $NERS_PREFIX/bin/ners_vars 
      if ( $NERS_VERSION < $NERS_VERSION_MIN ) then
           echo "sur_sked config: you have too old ners version $NERS_VERSION"
           echo "sur_sked config: while $NERS_VERSION_MIN version is needed. Please upgrade"
           exit 1
      endif
endif
echo "ok" 
#
# --- Check whether gmake is found in your system
#
$ECHO "config.csh: Check make ... \c" | tee -a  $CONF_LOG
set gmake_string = `which make`
if ( "$gmake_string" == "" ) then 
     echo "make was not found in your system."
     exit 1
endif
echo "ok" 
#
if ( $SUR_SKED_PYTHON_ONLY != "YES" ) then
#
# --- Check petools
#
     $ECHO "config.csh: Check petools ... \c" | tee -a  $CONF_LOG
     if ( -d $PETOOLS_PREFIX == 0 ) then
          echo "sur_sked config: petools directory $PETOOLS_PREFIX was not found"
          exit 1
     endif
     if ( -d $PETOOLS_PREFIX/lib == 0 ) then
          echo "sur_sked config: petools directory $PETOOLS_PREFIX/lib was not found"
          exit 1
     endif
     if ( -d $PETOOLS_PREFIX/bin == 0 ) then
          echo "sur_sked config: petools directory $PETOOLS_PREFIX/bin was not found"
          exit 1
     endif
     if ( -f $PETOOLS_PREFIX/bin/petools_vars ) then
           source $PETOOLS_PREFIX/bin/petools_vars 
           if ( $PETOOLS_VERSION < $PETOOLS_VERSION_MIN ) then
                echo "sur_sked config: you have too old petools version $PETOOLS_VERSION"
                echo "sur_sked config: while $PETOOLS_VERSION_MIN version is needed. Please upgrade"
                exit 1
           endif
     endif
     if ( -f $PETOOLS_PREFIX/lib/libpetools.a == 0 ) then
          echo "sur_sked config: petools file $PETOOLS_PREFIX/lib/libpetools.a was not found"
          exit 1
     endif
#
     if ( "$PETOOLS_VERSION" < "$PETOOLS_VERSION_MIN" ) then
          echo "Eeeeh. You have petools version $PETOOLS_VERSION, but "
          echo "$PETOOLS_VERSION_MIN is needed. Please upgrade"
          exit 1
     endif
     echo "ok" 
     $ECHO "config.csh: Check vtd ... \c" | tee -a  $CONF_LOG
#
# -- Check vtd
#
     if ( -d $VTD_PREFIX == 0 ) then
          echo "sur_sked config: vtd directory $VTD_PREFIX was not found"
          exit 1
     endif
     if ( -d $VTD_PREFIX/lib == 0 ) then
          echo "sur_sked config: vtd directory $VTD_PREFIX/lib was not found"
          exit 1
     endif
     if ( -d $VTD_PREFIX/bin == 0 ) then
          echo "sur_sked config: vtd directory $VTD_PREFIX/bin was not found"
          exit 1
     endif
     if ( -f $VTD_PREFIX/bin/vtd_vars ) then
           source $VTD_PREFIX/bin/vtd_vars 
           if ( $VTD_VERSION < $VTD_VERSION_MIN ) then
                echo "sur_sked config: you have too old vtd version $VTD_VERSION"
                echo "sur_sked config: while $VTD_VERSION_MIN version is needed. Please upgrade"
                exit 1
           endif
       else
          echo "sur_sked config: cannot find file $VTD_PREFIX/bin/vtd_vars"
          exit 1
     endif
     echo "ok" 
#
     setenv CFITSIO_LIB "-L $CFITSIO_PREFIX/lib -lcfitsio"
     setenv CFITSIO_INC "-I $CFITSIO_PREFIX/include"
     $ECHO "config.csh: Check cfitsio library... \c" | tee -a  $CONF_LOG
     $SUR_SKED_ROOT/support/check_cfitsio_version.csh >>&  $CONF_LOG
     if ( $status != 0 ) then
          echo "Failed to link against cfitsio library" | tee -a  $CONF_LOG
          exit 1
     endif
     echo "ok" 
#
     $ECHO "config.csh: Check cfitsio version >= $cfitsio_min_version ... \c" | tee -a  $CONF_LOG
     set cfitsio_version = `$SUR_SKED_ROOT/bin/check_cfitsio_version.e | awk '{print $3}'`
     if ( `$SUR_SKED_ROOT/support/version_equal_or_greater.csh $cfitsio_version $cfitsio_min_version` == 0    ) then
           echo "But found $cfitsio_version"
           echo "Please upgrade cfitsio to version $cfitsio_min_version or newer"
           exit 1
     endif
     echo $cfitsio_version 
endif
#
echo "sur_sked config  $date_stamp  on  $host_name" >>! $CONF_LOG
if ( $SUR_SKED_SHARE_DIR== "" ) then
     setenv SUR_SKED_SHARE_DIR ${SUR_SKED_PREFIX}/share/sur_sked
endif
#
if ( $SUR_SKED_EXP_DIR == "" ) setenv SUR_SKED_EXP_DIR ${SUR_SKED_SHARE_DIR}/exp
if ( $SUR_SKED_PRC_DIR == "" ) setenv SUR_SKED_PRC_DIR ${SUR_SKED_SHARE_DIR}/prc
if ( $SUR_SKED_SEQ_DIR == "" ) setenv SUR_SKED_SEQ_DIR ${SUR_SKED_SHARE_DIR}/seq
if ( $SUR_SKED_STP_DIR == "" ) setenv SUR_SKED_STP_DIR ${SUR_SKED_SHARE_DIR}/stp
#
cat ${SUR_SKED_ROOT}/support/sur_sked_config.tmpl         | \
    sed "s|@T@|Generated by sur_sked configuration on $date_stamp|g" | \
    sed "s|@sur_sked_prefix@|$SUR_SKED_PREFIX|g"    | \
    sed "s|@ners_prefix@|$NERS_PREFIX|g"            | \
    sed "s|@stp_dir@|$SUR_SKED_STP_DIR|g"           | \
    sed "s|@prc_dir@|$SUR_SKED_PRC_DIR|g"           | \
    sed "s|@seq_dir@|$SUR_SKED_SEQ_DIR|g"           | \
    sed "s|@exp_dir@|$SUR_SKED_EXP_DIR|g"           | \
    sed "s|@share_dir@|$SUR_SKED_SHARE_DIR|g"       > \
    ${SUR_SKED_ROOT}/support/sur_sked_config.py
#
set out_file = ${SUR_SKED_ROOT}/Makefile
#
set len = `echo $SUR_SKED_PREFIX|awk '{printf length }'`
set DATE_ISO  = `date "+%Y.%m.%d_%H:%M:%S"`
if ( `uname` == "Linux" ) then
     set num_threads     = `cat /proc/cpuinfo | grep "core id" | wc -l`
     set num_thr_per_cpu = `lscpu | grep '^Thread(s) per core:' | awk '{print $4}'`
     set num_cores = `expr $num_threads / $num_thr_per_cpu`
else
     set num_cores = `sysctl -n  machdep.cpu.core_count`
endif
#
cat   ${SUR_SKED_ROOT}/Makefile.in1 > $out_file 
$ECHO "SUR_SKED_ROOT      = $SUR_SKED_ROOT"                        >> $out_file
$ECHO "SUR_SKED_PREFIX    = $SUR_SKED_PREFIX"                      >> $out_file
$ECHO "SUR_SKED_BIN       = $SUR_SKED_PREFIX/bin"                  >> $out_file
$ECHO "SUR_SKED_LIB       = $SUR_SKED_PREFIX/lib"                  >> $out_file
$ECHO "SUR_SKED_INC       = $SUR_SKED_PREFIX/include"              >> $out_file
if ( $SUR_SKED_PYTHON_ONLY != "YES" ) then
     $ECHO "PETOOLS_PREFIX    = $PETOOLS_PREFIX"                  >> $out_file
     $ECHO "PETOOLS_LIB       = $PETOOLS_LIB"                     >> $out_file
     $ECHO "SOLVE_EXTRA_LIB   = $SOLVE_EXTRA_LIB"                 >> $out_file
     $ECHO "SOLVE_LIB_VEC     = $SOLVE_LIB_VEC"                   >> $out_file
     $ECHO "SOLVE_LIB_BLAS    = $SOLVE_LIB_BLAS"                  >> $out_file
     $ECHO "MK5_C             = $MK5_C"                           >> $out_file
     $ECHO "MK5_LINK          = $MK5_LINK"                        >> $out_file
     if ( $SUR_SKED_NOOPT == "NO" ) then 
          $ECHO "MK5_F95_OPT  = ${qt}$MK5_F95_OPT${qt}"           >> $out_file
          $ECHO "MK5_F95      = ${qt}$MK5_F95${qt}"               >> $out_file
       else
          $ECHO "MK5_F95_OPT  = ${qt}$MK5_F95_NOOPT${qt}"         >> $out_file
          $ECHO "MK5_F95      = ${qt}$MK5_F95_NOOPT${qt}"         >> $out_file
     endif
     $ECHO "MK5_F95_NOOPT     = ${qt}$MK5_F95_NOOPT${qt}"         >> $out_file
     $ECHO "VTD_LIB           = $VTD_LIB"                         >> $out_file
     $ECHO "VTD_INC           = $VTD_PREFIX/include"              >> $out_file
     $ECHO "NERS_LIB          = $NERS_LIB"                        >> $out_file
     $ECHO "NERS_INC          = $NERS_PREFIX/include"             >> $out_file
     $ECHO "CFITSIO_LIB       = -L $CFITSIO_PREFIX/lib -lcfitsio" >> $out_file
     $ECHO "CFITSIO_INC       = -I $CFITSIO_PREFIX/include"       >> $out_file
endif
$ECHO "SUR_SKED_SHARE_DIR = $SUR_SKED_SHARE_DIR"                  >> $out_file
$ECHO "SUR_SKED_EXP_DIR   = $SUR_SKED_EXP_DIR"                    >> $out_file
$ECHO "SUR_SKED_PRC_DIR   = $SUR_SKED_PRC_DIR"                    >> $out_file
$ECHO "SUR_SKED_SEQ_DIR   = $SUR_SKED_SEQ_DIR"                    >> $out_file
$ECHO "SUR_SKED_STP_DIR   = $SUR_SKED_STP_DIR"                    >> $out_file
$ECHO "SUR_SKED_VERSION   = $SUR_SKED_VERSION"                    >> $out_file
$ECHO "NUM_PROC           = $num_cores"                           >> $out_file
$ECHO "CONF_LOG           = $CONF_LOG"                            >> $out_file
$ECHO "BUILD_LOG          = $BUILD_LOG"                           >> $out_file
if ( $SUR_SKED_PYTHON_ONLY != "YES" ) then
     cat   ${SUR_SKED_ROOT}/Makefile.in2                          >> $out_file
  else
     cat ${SUR_SKED_ROOT}/Makefile.in2 | sed "s@bin_install script_install@script_install@g" >> $out_file
endif
#
set out_vars = ${SUR_SKED_ROOT}/support/sur_sked_vars
if ( -f $out_vars ) rm -f $out_vars
$ECHO "#\!/bin/csh"                                               > $out_vars
$ECHO "setenv SUR_SKED_ROOT $SUR_SKED_ROOT"                      >> $out_vars
if ( $SUR_SKED_PYTHON_ONLY != "YES" ) then
     $ECHO "setenv PETOOLS_PREFIX   $PETOOLS_PREFIX"             >> $out_vars
     $ECHO "setenv PETOOLS_LIB      ${qt}$PETOOLS_LIB${qt}"      >> $out_vars
     $ECHO "setenv SOLVE_EXTRA_LIB  ${qt}$SOLVE_EXTRA_LIB${qt}"  >> $out_vars
     $ECHO "setenv SOLVE_LIB_VEC    ${qt}$SOLVE_LIB_VEC${qt}"    >> $out_vars
     $ECHO "setenv SOLVE_LIB_BLAS   ${qt}$SOLVE_LIB_BLAS${qt}"   >> $out_vars
     $ECHO "setenv VTD_LIB          ${qt}$VTD_LIB${qt}"          >> $out_vars
     $ECHO "setenv VTD_INC          $VTD_PREFIX/include"         >> $out_vars
     $ECHO "setenv NERS_LIB         ${qt}$NERS_LIB${qt}"         >> $out_vars
     $ECHO "setenv NERS_INC         $VTD_PREFIX/include"         >> $out_vars
     $ECHO "setenv CFITSIO_LIB      ${qt}-L $CFITSIO_PREFIX/lib -lcfitsio${qt}" >> $out_vars
     $ECHO "setenv CFITSIO_INC      ${qt}-I $CFITSIO_PREFIX/include${qt}"       >> $out_vars
     $ECHO "setenv MK5_C            ${qt}$MK5_C${qt}"            >> $out_vars
     $ECHO "setenv MK5_LINK         ${qt}$MK5_LINK${qt}"         >> $out_vars
     if ( $SUR_SKED_NOOPT == "NO" ) then 
          $ECHO "setenv MK5_F95_OPT ${qt}$MK5_F95_OPT${qt}"      >> $out_vars
          $ECHO "setenv MK5_F95     ${qt}$MK5_F95${qt}"          >> $out_vars
       else
          $ECHO "setenv MK5_F95_OPT ${qt}$MK5_F95_NOOPT${qt}"    >> $out_vars
          $ECHO "setenv MK5_F95     ${qt}$MK5_F95_NOOPT${qt}"    >> $out_vars
     endif
     $ECHO "setenv MK5_F95_NOOPT ${qt}$MK5_F95_NOOPT${qt}"       >> $out_vars
endif
$ECHO "setenv SUR_SKED_ROOT        $SUR_SKED_ROOT"               >> $out_vars
$ECHO "setenv SUR_SKED_PREFIX      $SUR_SKED_PREFIX"             >> $out_vars
$ECHO "setenv SUR_SKED_BIN         $SUR_SKED_PREFIX/bin"         >> $out_vars
$ECHO "setenv SUR_SKED_LIB         $SUR_SKED_PREFIX/lib"         >> $out_vars
$ECHO "setenv SUR_SKED_INC         $SUR_SKED_PREFIX/include"     >> $out_vars
$ECHO "setenv SUR_SKED_SHARE_DIR   $SUR_SKED_SHARE_DIR"          >> $out_vars
$ECHO "setenv SUR_SKED_EXP_DIR     $SUR_SKED_EXP_DIR"            >> $out_vars
$ECHO "setenv SUR_SKED_PRC_DIR     $SUR_SKED_PRC_DIR"            >> $out_vars
$ECHO "setenv SUR_SKED_SEQ_DIR     $SUR_SKED_SEQ_DIR"            >> $out_vars
$ECHO "setenv SUR_SKED_STP_DIR     $SUR_SKED_STP_DIR"            >> $out_vars
$ECHO "setenv SUR_SKED_VERSION     $SUR_SKED_VERSION"            >> $out_vars
#
source ${SUR_SKED_ROOT}/support/sur_sked_vars
#
echo "config.csh is done"
