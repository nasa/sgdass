#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Script  config.csh  sets numerous variables needed for             *
# *   compilation and linking ners.                                      *
# *                                                                      *
# *   Usage:  config.csh                                                 *
# *                                                                      *
# *  ###  15-JUN-2016  config.csh  v2.3  (c) L. Petrov  15-MAY-2019 ###  *
# *                                                                      *
# ************************************************************************
#
setenv LANG   C
setenv LC_ALL C
setenv wget_min_vers 1.13
setenv SUPPORT_PATH `dirname $0`
#
cd     $SUPPORT_PATH
cd ../
if (    -f ${NERS_ROOT}/Makefile ) then
     rm -f ${NERS_ROOT}/Makefile
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
# --- Check whether gmake is found in your system
#
set gmake_string = `which gmake`
if ( "$gmake_string" == "" ) then 
     echo "gmake was not found in your system."
     exit 1
endif
#
set    date_stamp  = `date "+%Y.%m.%d-%H:%M:%S" | tr "[a-z]" "[A-Z]"`
set    host_name   = `uname -n`
#
if ( $PETOOLS_PREFIX != "NO" ) then
#
# --- Check petools
#
     if ( -d $PETOOLS_PREFIX == 0 ) then
          echo "ners config: petools directory $PETOOLS_PREFIX was not found"
          exit 1
     endif
     if ( -d $PETOOLS_PREFIX/lib == 0 ) then
          echo "ners config: petools directory $PETOOLS_PREFIX/lib was not found"
          exit 1
     endif
     if ( -d $PETOOLS_PREFIX/bin == 0 ) then
          echo "ners config: petools directory $PETOOLS_PREFIX/bin was not found"
          exit 1
     endif
     if ( -f $PETOOLS_PREFIX/bin/petools_vars ) then
           source $PETOOLS_PREFIX/bin/petools_vars 
           if ( $PETOOLS_VERSION < $PETOOLS_VERSION_MIN ) then
                echo "ners config: you have too old petools version $PETOOLS_VERSION"
                echo "ners config: while $PETOOLS_VERSION_MIN version is needed. Please upgrade"
                exit 1
           endif
     endif
     if ( "$PETOOLS_VERSION" < "$PETOOLS_VERSION_MIN" ) then
          echo "Eeeeh. You have petools version $PETOOLS_VERSION, but "
          echo "$PETOOLS_VERSION_MIN is needed. Please upgrade"
          exit 1
     endif
     if ( -f $PETOOLS_PREFIX/lib/libpetools.a == 0 ) then
          echo "ners config: petools file $PETOOLS_PREFIX/lib/libpetools.a was not found"
          exit 1
     endif
     setenv NERS_STANDALONE NO
  else
#
# --- Remove stale executables
#
     if (    -f ${NERS_ROOT}/bin/check_endian ) then
          rm -f ${NERS_ROOT}/bin/check_endian
     endif
#
     if (    -f ${NERS_ROOT}/bin/learn_fortran_true ) then
          rm -f ${NERS_ROOT}/bin/learn_fortran_true
     endif
#
     if (    -f ${NERS_ROOT}/bin/learn_fortran_false) then
          rm -f ${NERS_ROOT}/bin/learn_fortran_false
     endif
     setenv PETOOLS_ROOT $NERS_ROOT
     setenv PETOOLS_BITS 64
#
# --- First set envirnonment variables for compilers in no_endian_check and
# --- no true/false check mode
#
     $ECHO "config.csh: Set environment variables for compilers... \c" | tee -a  $CONF_LOG
     setenv NO_ENDIAN_CHECK     "YES"
     setenv NO_TRUE_FALSE_CHECK "YES"
     source ${NERS_ROOT}/support/compile_var.csh
     if ( $status != 0 ) then
          echo "config.csh: FAILURE" | tee -a $CONF_LOG
          echo "config.csh: Error in setting environment variables for compilation" | tee -a  $CONF_LOG
          echo "config.csh: See file " $CONF_LOG
          exit 1
     endif
     echo "ok"
#
# --- Compile check_endian program
#
     $ECHO "config.csh: Check endian... \c" | tee -a  $CONF_LOG
     if ( -f $NERS_ROOT/bin/check_endian ) rm -f $NERS_ROOT/bin/check_endian
     $MK5_C -c -o /tmp/check_endian.o $NERS_ROOT/support/check_endian.c >>& $CONF_LOG
     if ( $status != 0 ) then
          echo "config.csh: FAILURE" | tee -a $CONF_LOG
          echo "config.csh: $MK5_C -c -o /tmp/check_endian.o $NERS_ROOT/support/check_endian.c "
          echo "config.csh: Error in compilation check_endian program " | tee -a  $CONF_LOG
          echo "config.csh: See file " $CONF_LOG
          exit 1
     endif
#
# --- ... and link it
#
     $MK5_C -o $NERS_ROOT/bin/check_endian /tmp/check_endian.o >>& $CONF_LOG
     if ( $status != 0 ) then
          echo "config.csh: FAILURE" | tee -a $CONF_LOG
          echo "config.csh: Error in linking check_endian program " | tee -a  $CONF_LOG
          echo "config.csh: See file " $CONF_LOG
          exit 1
     endif
     rm -f /tmp/check_endian.o
     echo "  Your architecture is `$NERS_ROOT/bin/check_endian`" | tee -a  $CONF_LOG
#
# --- Compile learn_fortran_true program
#
     $ECHO "config.csh: Check fortran_true... \c" | tee -a  $CONF_LOG
     if ( -f $NERS_ROOT/bin/learn_fortran_true ) rm -f $NERS_ROOT/bin/learn_fortran_true
     $MK5_F95 -c -o /tmp/learn_fortran_true.o $NERS_ROOT/support/learn_fortran_true.f >>& $CONF_LOG
     if ( $status != 0 ) then
          echo "config.csh: FAILURE" | tee -a $CONF_LOG
          echo "config.csh: Error in compilation learn_fortran_true program " | tee -a  $CONF_LOG
          echo "config.csh: MK5_F95=$MK5_F95"
          echo "config.csh: See file " $CONF_LOG
          exit 1
     endif
#
# --- ... and link it
#
     $MK5_LINK -o $NERS_ROOT/bin/learn_fortran_true /tmp/learn_fortran_true.o >>& $CONF_LOG
     if ( $status != 0 ) then
          echo "config.csh: FAILURE" | tee -a $CONF_LOG
          echo "config.csh: Error in linking learn_fortran_true program " | tee -a  $CONF_LOG
          echo "config.csh: MK5_LINK=$MK5_LINK"
          echo "config.csh: See file " $CONF_LOG
          exit 1
     endif
     rm -f /tmp/learn_fortran_true.o
     echo "  Fortran .TRUE. is `$NERS_ROOT/bin/learn_fortran_true`" | tee -a  $CONF_LOG
#
# --- Compile learn_fortran_false program
#
     $ECHO "config.csh: Check fortran_false... \c" | tee -a  $CONF_LOG
     if ( -f $NERS_ROOT/bin/learn_fortran_false ) rm -f $NERS_ROOT/bin/learn_fortran_false
     $MK5_F95 -c -o /tmp/learn_fortran_false.o $NERS_ROOT/support/learn_fortran_false.f >>& $CONF_LOG
     if ( $status != 0 ) then
          echo "config.csh: FAILURE" | tee -a $CONF_LOG
          echo "config.csh: Error in compilation learn_fortran_false program " | tee -a  $CONF_LOG
          echo "config.csh: See file " $CONF_LOG
          exit 1
     endif
#
# --- ... and link it
#
     $MK5_LINK -o $NERS_ROOT/bin/learn_fortran_false /tmp/learn_fortran_false.o >>& $CONF_LOG
     if ( $status != 0 ) then
          echo "config.csh: FAILURE" | tee -a $CONF_LOG
          echo "config.csh: Error in linking learn_fortran_false program " | tee -a  $CONF_LOG
          echo "config.csh: See file " $CONF_LOG
          exit 1
     endif
     rm -f /tmp/learn_fortran_false.o
#
# --- Setting all environment variables needed for compilation and linking
#
     $ECHO "config.csh: Set options for compilers ... \c" | tee -a  $CONF_LOG
     source $NERS_ROOT/support/compile_var.csh
     if ( $?PETOOLS_COMPVAR_DEF == 0 ) then
          echo "config.csh: FAILURE" | tee -a $CONF_LOG
          echo "config.csh: No suitable Fortran compiler was found." | tee -a $CONF_LOG
          echo "config.csh: Refer to installation instruction"
          echo "config.csh: Look at file " $CONF_LOG
          exit 1
     endif
     echo "ok"
     ${NERS_ROOT}/support/f95_version.csh | tee -a $CONF_LOG
     ${NERS_ROOT}/support/cc_version.csh  | tee -a $CONF_LOG
     unsetenv PETOOLS_ROOT
     unsetenv PETOOLS_COMPVAR_DEF
     setenv   NERS_STANDALONE YES
endif
#
echo "ners config  $date_stamp  on  $host_name" >>! $CONF_LOG
$ECHO "config.csh: Check wget version ... \c" | tee -a  $CONF_LOG
set wget_version = `wget --version | head -1 | awk '{print $3}'`
if ( $wget_version == "" ) then
     $ECHO "failure" | tee -a  $CONF_LOG
     $ECHO "Please install wget https://www.gnu.org/software/wget" | tee -a  $CONF_LOG
     exit 1
endif
if ( `$NERS_ROOT/support/version_equal_or_greater.csh $wget_version $wget_min_vers` == 0    ) then
     $ECHO "You have wget version $wget_version" | tee -a  $CONF_LOG
     $ECHO "but version $wget_min_vers or higher is required" | tee -a  $CONF_LOG
     $ECHO "Please upgrade wget https://www.gnu.org/software/wget" | tee -a  $CONF_LOG
      exit 1
endif
$ECHO $wget_version  | tee -a  $CONF_LOG
#
set out_file = ${NERS_ROOT}/Makefile
if ( -f $out_file ) rm -f $out_file
#
set lp = `echo $NERS_PREFIX | awk '{print length}'`
set lc = `echo ${NERS_PREFIX}/share/ners.config | awk '{print length}'`
set ls = `echo ${NERS_STANDALONE} | awk '{print length}'`
set lv = `echo ${NERS_VERSION} | awk '{print length}'`
cat ${NERS_ROOT}/include/ners_local_templ.i | \
    sed -e "s|@D|$date_stamp|g" | \
    sed -e "s|@LP|$lp|g" | \
    sed -e "s|@LC|$lc|g" | \
    sed -e "s|@LS|$ls|g" | \
    sed -e "s|@LV|$lv|g" | \
    sed -e "s|@P|$NERS_PREFIX|g" | \
    sed -e "s|@C|$NERS_PREFIX/share/ners.config|g" | \
    sed -e "s|@S|$NERS_STANDALONE|g" | \
    sed -e "s|@V|$NERS_VERSION|g" \
    > ${NERS_ROOT}/include/ners_local.i 
#
if ( -f ${NERS_ROOT}/support/ners_len.o ) rm ${NERS_ROOT}/support/ners_len.o 
if ( -f ${NERS_ROOT}/support/ners_len   ) rm ${NERS_ROOT}/support/ners_len
make -f ${NERS_ROOT}/support/ners_len.mak >>&  ${NERS_ROOT}/temp/conf.log
if ( $status != 0 ) then
     $ECHO "Error in compiling test program ners_len failure" | tee -a  $CONF_LOG
     $ECHO "Please look at log file $CONF_LOG"
     exit 1
endif
set ners_len = `${NERS_ROOT}/support/ners_len`
if ( $status != 0 ) then
     $ECHO "Error in running test program ners_len" | tee -a  $CONF_LOG
     exit 1
endif
cat ${NERS_ROOT}/include/ners_templ.h                         | \
    sed -e "s|@NERS__LEN@|`${NERS_ROOT}/support/ners_len`|g"   | \
    sed -e "s|@NERS__CONFIG@|$NERS_PREFIX/share/ners.config|g"   \
    > ${NERS_ROOT}/include/ners.h
#
set DATE_ISO  = `date "+%Y.%m.%d_%H:%M:%S"`
#
cat   ${NERS_ROOT}/Makefile.in1 > $out_file 
$ECHO "NERS_ROOT    = $NERS_ROOT"                        >> $out_file
$ECHO "MK5_C            = $MK5_C"                        >> $out_file
if ( $NERS_NOOPT == "NO" ) then
      $ECHO "MK5_F95_OPTEST   = $MK5_F95_OPTEST"               >> $out_file
      $ECHO "MK5_F95_OPT      = $MK5_F95_OPT"                  >> $out_file
      $ECHO "MK5_F95          = $MK5_F95"                      >> $out_file
  else
      $ECHO "MK5_F95_OPTEST   = $MK5_F95_NOOPT"                >> $out_file
      $ECHO "MK5_F95_OPT      = $MK5_F95_NOOPT"                >> $out_file
      $ECHO "MK5_F95          = $MK5_F95_NOOPT"                >> $out_file
endif
$ECHO "MK5_LINK         = $MK5_LINK"                     >> $out_file
$ECHO "MK5_C_LINK       = $MK5_C_LINK"                   >> $out_file
$ECHO "LDFLAGS          = $MK5_LDFLAGS"                  >> $out_file
$ECHO "NERS_ROOT        = $NERS_ROOT"                    >> $out_file
$ECHO "NERS_PREFIX      = $NERS_PREFIX"                  >> $out_file
$ECHO "NERS_PYTHON      = $NERS_PYTHON"                  >> $out_file
$ECHO "NERS_BIN         = $NERS_PREFIX/bin"              >> $out_file
$ECHO "NERS_LIB         = $NERS_PREFIX/lib"              >> $out_file
$ECHO "NERS_INC         = $NERS_PREFIX/include"          >> $out_file
$ECHO "PETOOLS_PREFIX   = $PETOOLS_PREFIX"               >> $out_file
if ( "$PETOOLS_PREFIX" != "NO" ) then
     $ECHO "PETOOLS_INC      = $PETOOLS_PREFIX/include"        >> $out_file
     $ECHO "PETOOLS_LIB      = $PETOOLS_LIB"                   >> $out_file
     $ECHO "SOLVE_LIB_BLAS   = $SOLVE_LIB_BLAS"                >> $out_file
     $ECHO "NERS_LAPACK      = ${qt}${qt}"                     >> $out_file
     $ECHO "NERS_STANDALONE  = NO"                             >> $out_file
else
     $ECHO "PETOOLS_LIB      = $NERS_ROOT/lib/libners_petools.a" >> $out_file
     $ECHO "PETOOLS_INC      = $NERS_ROOT/ners_petools"        >> $out_file
     $ECHO "SOLVE_LIB_BLAS   = ${qt}${qt}"                     >> $out_file
     $ECHO "NERS_F95_PUNCH   = $MK5_F95_OPT" | sed 's@-ffree-form@@g' | sed 's@ -FR @ -fixed @g'  >> $out_file
     $ECHO "NERS_LAPACK      = ${qt}$NERS_ROOT/lib/libners_lapack.a ${qt}" >> $out_file
     $ECHO "NERS_STANDALONE  = YES"                             >> $out_file
endif 
setenv NERS_F95_PUNCH `echo $MK5_F95_OPT | sed 's@-ffree-form@@g' | sed 's@ -FR @ -fixed @g'` >> $out_file
$ECHO "NERS_VERSION     = $NERS_VERSION"                 >> $out_file
$ECHO "NERS_OS          = $OS_name"                      >> $out_file
$ECHO "NUM_PROC         = $num_cores"                    >> $out_file
$ECHO "CONF_LOG         = $CONF_LOG"                     >> $out_file
$ECHO "BUILD_LOG        = $BUILD_LOG"                    >> $out_file
cat   ${NERS_ROOT}/Makefile.in2                          >> $out_file
#
set out_vars = ${NERS_ROOT}/support/ners_vars
if ( -f $out_vars ) rm -f $out_vars
$ECHO "#\!/bin/csh"                                          > $out_vars
$ECHO "setenv NERS_ROOT        $NERS_ROOT"                  >> $out_vars
$ECHO "setenv MK5_C            ${qt}$MK5_C${qt}"            >> $out_vars
if ( $NERS_NOOPT == "NO" ) then
     $ECHO "setenv MK5_F95_OPT      ${qt}$MK5_F95_OPT${qt}"      >> $out_vars
     $ECHO "setenv MK5_F95_OPTEST   ${qt}$MK5_F95_OPTEST${qt}"   >> $out_vars
     $ECHO "setenv MK5_F95          ${qt}$MK5_F95${qt}"          >> $out_vars
   else
     $ECHO "setenv MK5_F95_OPT      ${qt}$MK5_F95_NOOPT${qt}"    >> $out_vars
     $ECHO "setenv MK5_F95_OPTEST   ${qt}$MK5_F95_NOOPT${qt}"    >> $out_vars
     $ECHO "setenv MK5_F95          ${qt}$MK5_F95_NOOPT${qt}"    >> $out_vars
endif
$ECHO "setenv MK5_LINK         ${qt}$MK5_LINK${qt}"         >> $out_vars
$ECHO "setenv MK5_C_LINK       ${qt}$MK5_C_LINK${qt}"       >> $out_vars
$ECHO "setenv LDFLAGS          ${qt}$MK5_LDFLAGS${qt}"      >> $out_vars
$ECHO "setenv NERS_ROOT        $NERS_ROOT"                  >> $out_vars
$ECHO "setenv NERS_PREFIX      $NERS_PREFIX"                >> $out_vars
$ECHO "setenv NERS_PYTHON      $NERS_PYTHON"                >> $out_vars
$ECHO "setenv NERS_BIN         $NERS_PREFIX/bin"            >> $out_vars
$ECHO "setenv NERS_LIB         $NERS_PREFIX/lib"            >> $out_vars
$ECHO "setenv NERS_INC         $NERS_PREFIX/include"        >> $out_vars
if ( "$PETOOLS_PREFIX" == "NO" ) then
     $ECHO "setenv PETOOLS_LIB      $NERS_ROOT/lib/libners_petools.a" >> $out_vars
     $ECHO "setenv PETOOLS_INC      $NERS_ROOT/ners_petools"          >> $out_vars
     $ECHO "setenv SOLVE_LIB_BLAS   ${qt}${qt}"                       >> $out_vars
     $ECHO "setenv NERS_F95_PUNCH   ${qt}$NERS_F95_PUNCH${qt}"        >> $out_vars
     $ECHO "setenv NERS_LAPACK      $NERS_ROOT/lib/libners_lapack.a"  >> $out_vars
     $ECHO "setenv NERS_STANDALONE  YES"                              >> $out_vars
  else
     $ECHO "setenv PETOOLS_PREFIX   $PETOOLS_PREFIX"             >> $out_vars
     $ECHO "setenv PETOOLS_INC      $PETOOLS_PREFIX/include"     >> $out_vars
     $ECHO "setenv PETOOLS_LIB      ${qt}$PETOOLS_LIB${qt}"      >> $out_vars
     $ECHO "setenv SOLVE_LIB_BLAS   ${qt}$SOLVE_LIB_BLAS${qt}"   >> $out_vars
     $ECHO "setenv NERS_LAPACK      ${qt}${qt}"                  >> $out_vars
     $ECHO "setenv NERS_STANDALONE  NO"                          >> $out_vars
endif
$ECHO "setenv NERS_VERSION     $NERS_VERSION"               >> $out_vars
$ECHO "setenv NERS_OS          $OS_name"                    >> $out_vars
#
source ${NERS_ROOT}/support/ners_vars
#
# --- Create a file with export environoment variables
#
set export_vars = ${NERS_ROOT}/support/export_ners_vars
if ( -f $export_vars  ) rm -f $export_vars 
$ECHO "#\!/bin/csh"                               > $export_vars
$ECHO "setenv NERS_PREFIX    $NERS_PREFIX"       >> $export_vars
if ( $NERS_STANDALONE == "YES" ) then
     $ECHO "setenv NERS_LIB      ${qt}-L$NERS_PREFIX/lib -lners${qt}" >> $export_vars
   else
     $ECHO "setenv NERS_LIB      ${qt}-L$NERS_PREFIX/lib -lners -L$PETOOLS_PREFIX/lib -lpetools${qt}" >> $export_vars
endif
$ECHO "setenv NERS_INCLUDE   $NERS_PREFIX/include" >> $export_vars
$ECHO "setenv NERS_VERSION   $NERS_VERSION"        >> $export_vars
#
# --- Clean up
#
if ( -f $NERS_ROOT/bin/check_endian        ) rm -f $NERS_ROOT/bin/check_endian 
if ( -f $NERS_ROOT/bin/learn_fortran_true  ) rm -f $NERS_ROOT/bin/learn_fortran_true
if ( -f $NERS_ROOT/bin/learn_fortran_false ) rm -f $NERS_ROOT/bin/learn_fortran_false 
make -f ${NERS_ROOT}/support/ners_len.mak clean >>&  $CONF_LOG
#
chmod g+rw,o+r $NERS_ROOT/Makefile
chmod g+rw,o+r $NERS_ROOT/support/ners_vars
chmod g+rw,o+r $NERS_ROOT/support/export_ners_vars
#
echo "config.csh is done"
