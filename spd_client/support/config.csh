#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Script  config.csh  sets numerous variables needed for             *
# *   compilation and linking spd_client. It creates files with          *
# *   directives for FORTRAN-compilers and C-compiler. It creates        *
# *   include files using template files for includes and local files    *
# *   with preferences.                                                  *
# *                                                                      *
# *   Usage:  config.csh                                                 *
# *                                                                      *
# *  ###  18-JAN-2008  config.csh  v2.8  (c)  L. Petrov  18-JAN-2024 ### *
# *                                                                      *
# ************************************************************************
#
setenv SUPPORT_PATH `dirname $0`
cd     $SUPPORT_PATH
cd ../
setenv CONF_LOG   ${SPC_ROOT}/temp/conf.log
if (    -f ${SPC_ROOT}/Makefile ) then
     rm -f ${SPC_ROOT}/Makefile
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
#
$ECHO "config.csh: Check Fortran compiler ... \c" | tee -a $CONF_LOG
if ( $?SPC_FC == 0 ) then
     setenv SPC_FC `${SPC_ROOT}/support/fortran_compiler_guess.csh`
     if ( $status != 0 ) then
          echo "config.csh: FAILURE" | tee -a $CONF_LOG
          echo "config.csh: No Fortran compiler was found " | tee -a  $CONF_LOG
          echo "config.csh: Refer to installation instruction"
          echo "config.csh: Look at file " $CONF_LOG
          exit 1
     endif
endif
echo "ok"
#
# Check: do you have a C compiler?
#
$ECHO "config.csh: Check C compiler ... \c" | tee -a $CONF_LOG
${SPC_ROOT}/support/check_file.csh $SPC_CC command;
if ( $status != 0 ) then
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: C compiler has not been found " | tee -a  $CONF_LOG
     echo "config.csh: Refer to installation instruction"
     echo "config.csh: Look at file " $CONF_LOG
     exit 1
endif
#
# --- Remove stale executables
#
if (    -f ${SPC_ROOT}/bin/check_endian ) then
     rm -f ${SPC_ROOT}/bin/check_endian
endif
#
if (    -f ${SPC_ROOT}/bin/learn_fortran_true ) then
     rm -f ${SPC_ROOT}/bin/learn_fortran_true
endif
#
if (    -f ${SPC_ROOT}/bin/learn_fortran_false) then
     rm -f ${SPC_ROOT}/bin/learn_fortran_false
endif
#
# --- First set envirnonment variables for compilers in no_endian_check and
# --- no true/false check mode
#
$ECHO "config.csh: Set environment variables for compilers... \c" | tee -a  $CONF_LOG
setenv NO_ENDIAN_CHECK     "YES"
setenv NO_TRUE_FALSE_CHECK "YES"
#
# --- Set legacy variables in order to conform to compile_var.csh
#
setenv PETOOLS_ROOT $SPC_ROOT
setenv MK5_CC       $SPC_CC
setenv MK5_FC       $SPC_FC
#
source ${SPC_ROOT}/support/compile_var.csh
if ( $status != 0 ) then
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: Error in setting environment variables for compilation" | tee -a  $CONF_LOG
     echo "config.csh: See file " $CONF_LOG
     exit 1
endif
echo "ok"
#
# --- Export compiler related variables
#
setenv SPC_C_OPENMP    "$MK5_C_OPENMP"
setenv SPC_F_OPENMP    "$MK5_F_OPENMP"
setenv SPC_F95_NOOPT   "$MK5_F95_NOOPT"
if ( $SPC_NOOPT == "NO" ) then
     setenv SPC_F95         "$MK5_F95"
     setenv SPC_F95_OPT     "$MK5_F95_OPT"
     setenv SPC_F95_OPTEST  "$MK5_F95_OPTEST"
  else
     setenv SPC_F95         "$MK5_F95_NOOPT"
     setenv SPC_F95_OPT     "$MK5_F95_NOOPT"
     setenv SPC_F95_OPTEST  "$MK5_F95_NOOPT"
endif
setenv SPC_C           "$MK5_C"
setenv SPC_LINK        "$MK5_LINK"
setenv SPC_C_LINK      "$MK5_C_LINK"
#
# --- Compile check_endian program
#
$ECHO "config.csh: Check endian... \c" | tee -a  $CONF_LOG
if ( -f $SPC_ROOT/bin/check_endian ) rm -f $SPC_ROOT/bin/check_endian
$SPC_C -c -o /tmp/check_endian.o $SPC_ROOT/support/check_endian.c >>& $CONF_LOG
if ( $status != 0 ) then
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: $SPC_C -c -o /tmp/check_endian.o $SPC_ROOT/support/check_endian.c "
     echo "config.csh: Error in compilation check_endian program " | tee -a  $CONF_LOG
     echo "config.csh: See file " $CONF_LOG
     exit 1
endif
#
# --- ... and link it
#
$SPC_C -o $SPC_ROOT/bin/check_endian /tmp/check_endian.o >>& $CONF_LOG
if ( $status != 0 ) then
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: Error in linking check_endian program " | tee -a  $CONF_LOG
     echo "config.csh: See file " $CONF_LOG
     exit 1
endif
rm -f /tmp/check_endian.o
echo "  Your architecture is `$SPC_ROOT/bin/check_endian`" | tee -a  $CONF_LOG
#
# --- Compile learn_fortran_true program
#
$ECHO "config.csh: Check fortran_true... \c" | tee -a  $CONF_LOG
if ( -f $SPC_ROOT/bin/learn_fortran_true ) rm -f $SPC_ROOT/bin/learn_fortran_true
$SPC_F95 -c -o /tmp/learn_fortran_true.o $SPC_ROOT/support/learn_fortran_true.f >>& $CONF_LOG
if ( $status != 0 ) then
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: Error in compilation learn_fortran_true program " | tee -a  $CONF_LOG
     echo "config.csh: SPC_F95=$SPC_F95"
     echo "config.csh: See file " $CONF_LOG
     exit 1
endif
#
# --- ... and link it
#
$SPC_LINK -o $SPC_ROOT/bin/learn_fortran_true /tmp/learn_fortran_true.o >>& $CONF_LOG
if ( $status != 0 ) then
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: Error in linking learn_fortran_true program " | tee -a  $CONF_LOG
     echo "config.csh: SPC_LINK=$SPC_LINK"
     echo "config.csh: See file " $CONF_LOG
     exit 1
endif
rm -f /tmp/learn_fortran_true.o
echo "  Fortran .TRUE.  is `$SPC_ROOT/bin/learn_fortran_true`" | tee -a  $CONF_LOG
#
# --- Compile learn_fortran_false program
#
$ECHO "config.csh: Check fortran_false... \c" | tee -a  $CONF_LOG
if ( -f $SPC_ROOT/bin/learn_fortran_false ) rm -f $SPC_ROOT/bin/learn_fortran_false
$SPC_F95 -c -o /tmp/learn_fortran_false.o $SPC_ROOT/support/learn_fortran_false.f >>& $CONF_LOG
if ( $status != 0 ) then
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: Error in compilation learn_fortran_false program " | tee -a  $CONF_LOG
     echo "config.csh: See file " $CONF_LOG
     exit 1
endif
#
# --- ... and link it
#
$SPC_LINK -o $SPC_ROOT/bin/learn_fortran_false /tmp/learn_fortran_false.o >>& $CONF_LOG
if ( $status != 0 ) then
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: Error in linking learn_fortran_false program " | tee -a  $CONF_LOG
     echo "config.csh: See file " $CONF_LOG
     exit 1
endif
echo " Fortran .FALSE. is `$SPC_ROOT/bin/learn_fortran_false`" | tee -a  $CONF_LOG
rm -f /tmp/learn_fortran_false.o
#
# --- Setting all environment variables needed for compilation and linking
#
$ECHO "config.csh: Set options for compilers ... \c" | tee -a  $CONF_LOG
source $SPC_ROOT/support/compile_var.csh
if ( $?PETOOLS_COMPVAR_DEF == 0 ) then
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: No suitable Fortran compiler was found." | tee -a $CONF_LOG
     echo "config.csh: Refer to installation instruction"
     echo "config.csh: Look at file " $CONF_LOG
     exit 1
endif
echo "ok"
#
# --- Again compiler related variables
#
setenv SPC_C_OPENMP    "$MK5_C_OPENMP"
setenv SPC_F_OPENMP    "$MK5_F_OPENMP"
setenv SPC_F95_NOOPT   "$MK5_F95_NOOPT"
if ( $SPC_NOOPT == "NO" ) then
     setenv SPC_F95         "$MK5_F95"
     setenv SPC_F95_OPT     "$MK5_F95_OPT"
     setenv SPC_F95_OPTEST  "$MK5_F95_OPTEST"
  else
     setenv SPC_F95         "$MK5_F95_NOOPT"
     setenv SPC_F95_OPT     "$MK5_F95_NOOPT"
     setenv SPC_F95_OPTEST  "$MK5_F95_NOOPT"
endif
setenv SPC_C           "$MK5_C"
setenv SPC_LINK        "$MK5_LINK"
setenv SPC_C_LINK      "$MK5_C_LINK"
#
# --- Check whether make is found in your system
#
set gmake_string = `which make`
if ( "$gmake_string" == "" ) then 
     echo "make was not found in your system."
     exit 1
endif
#
if ( $SPC_PETOOLS != "" ) then
     source $SPC_PETOOLS/bin/petools_vars
     setenv SPC_PETOOLS_LIB "$PETOOLS_LIB"
     setenv SPC_PETOOLS_INC "$PETOOLS_INCLUDE"
     setenv SPC_LAPACK_LIB  "$SOLVE_LIB_VEC $SOLVE_LIB_BLAS $SOLVE_LIB_M $SOLVE_LIB_LCL $SOLVE_EXTRA_LIB"
     setenv SPC_PETOOLS_USE "USE_PETOOLS"
     if ( $?LD_LIBRARY_PATH ) then
          setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:$SPC_PETOOLS/lib
	else
          setenv LD_LIBRARY_PATH $SPC_PETOOLS/lib
     endif
     $ECHO "config.csh: Check petools... \c" | tee -a  $CONF_LOG
     $SPC_F95 -c -o $SPC_ROOT/temp/petools_test.o $SPC_ROOT/support/petools_test.f  >& $SPC_ROOT/temp/err.log
     if ( $status != 0 ) then

          echo "config.csh: FAILURE" | tee -a $CONF_LOG
          echo "config.csh: failure in an attempt to compile petools test"
          echo "config.csh: Look at file " $CONF_LOG
          exit 1
     endif
     $SPC_LINK -o $SPC_ROOT/temp/petools_test.e $SPC_ROOT/temp/petools_test.o \
                  $SPC_PETOOLS_LIB $SPC_LAPACK_LIB >& $SPC_ROOT/temp/err.log
     if ( $status != 0 ) then
          cat $SPC_ROOT/temp/err.log | tee -a $CONF_LOG
          echo "config.csh: FAILURE" | tee -a $CONF_LOG
          echo "config.csh: failure in an attempt to link petools test against petools library"
          echo "config.csh: Look at file " $CONF_LOG
          exit 1
     endif
     $SPC_ROOT/temp/petools_test.e >& $SPC_ROOT/temp/err.log
     if ( $status != 0 ) then
          cat $SPC_ROOT/temp/err.log | tee -a $CONF_LOG
          echo "config.csh: FAILURE" | tee -a $CONF_LOG
          echo "config.csh: failure in an attempt to run petools test"
          echo "config.csh: Look at file " $CONF_LOG
          exit 1
     endif
     echo "ok"
     rm $SPC_ROOT/temp/petools_test.o
     rm $SPC_ROOT/temp/petools_test.e
     rm $SPC_ROOT/temp/err.log
  else 
#
# ------- Copy directivies for the preprocessor of the Fortran95 compiler which
# ------- will be actually used
#
##if ( `echo $PETOOLS_BLAS | grep -i intel` != "" ) then
     cp $MK5_PREP_DIRECTIVES_UNDSC    ${PETOOLS_ROOT}/include/mk5_preprocessor_directives.inc
#
     set SPC_F95_VERSION = `${SPC_ROOT}/support/f95_version.csh version $SPC_F95`
     set SPC_CC_VERSION  = `${SPC_ROOT}/support/cc_version.csh  version $SPC_CC`
     echo "C       compiler version: $SPC_CC_VERSION"  | tee -a $CONF_LOG
     echo "Fortram compiler version: $SPC_F95_VERSION" | tee -a $CONF_LOG
#
     if ( $SPC_LAPACK != "" ) then
          setenv SPC_LAPACK_LIB  "-L$SPC_LAPACK/lib -llapack"
          $ECHO "config.csh: Check lapack... \c" | tee -a  $CONF_LOG
          $SPC_F95 -c -o $SPC_ROOT/temp/lapack_test.o $SPC_ROOT/support/lapack_test.f  >& $SPC_ROOT/temp/err.log
          if ( $status != 0 ) then

               echo "config.csh: FAILURE" | tee -a $CONF_LOG
               echo "config.csh: failure in an attempt to compile lapack test"
               echo "config.csh: Look at file " $CONF_LOG
               exit 1
          endif
          $SPC_LINK -o $SPC_ROOT/temp/lapack_test.e $SPC_ROOT/temp/lapack_test.o \
                       $SPC_LAPACK_LIB >& $SPC_ROOT/temp/err.log
          if ( $status != 0 ) then
               cat $SPC_ROOT/temp/err.log | tee -a $CONF_LOG
               echo "config.csh: FAILURE" | tee -a $CONF_LOG
               echo "config.csh: failure in an attempt to link lapack test against lapack library"
               echo "config.csh: Look at file " $CONF_LOG
               exit 1
          endif
          $SPC_ROOT/temp/lapack_test.e >& $SPC_ROOT/temp/err.log
          if ( $status != 0 ) then
               cat $SPC_ROOT/temp/err.log | tee -a $CONF_LOG
               echo "config.csh: FAILURE" | tee -a $CONF_LOG
               echo "config.csh: failure in an attempt to run lapack test"
               echo "config.csh: Look at file " $CONF_LOG
               exit 1
          endif
          echo "ok"
          rm $SPC_ROOT/temp/lapack_test.o
          rm $SPC_ROOT/temp/lapack_test.e
          rm $SPC_ROOT/temp/err.log
        else 
          setenv SPC_PETOOLS_LIB $SPC_ROOT/lib/libspc.a
          setenv SPC_LAPACK_LIB  $SPC_ROOT/lib/libspc.a
     endif
     setenv SPC_PETOOLS_INC ""
     setenv SPC_PETOOLS_USE "DO_NOT_USE_PETOOLS"
endif
#
set DATE_LONG = `date "+%d-%h-%Y %H:%M:%S" | tr "[a-z]" "[A-Z]"`
set SPC_PREFIX_LEN  = `echo $SPC_PREFIX            | awk '{print length($1)}'`
set SPC_ROOT_LEN    = `echo $SPC_ROOT              | awk '{print length($1)}'`
set SPC_VERSION_LEN = `echo $SPC_VERSION           | awk '{print length($1)}'`
set SPC_DOC_LEN     = `echo $SPC_PREFIX/spd/doc    | awk '{print length($1)}'`
set SPC_SHARE_LEN   = `echo $SPC_PREFIX/spd/share  | awk '{print length($1)}'`
set SPC_SCRIPT_LEN  = `echo $SPC_PREFIX/spd/script | awk '{print length($1)}'`
set SPC_VERSION_LEN = `echo $SPC_VERSION           | awk '{print length($1)}'`

if ( $SPC_PETOOLS == "" ) then
     set SPC__PETOOLS_LEN = 2
     set SPC__PETOOLS_USE = "no"
  else 
     set SPC__PETOOLS_LEN = 3
     set SPC__PETOOLS_USE = "yes"
endif
#
# --- Create an include file with local customizations
#
cat <<EOF > ${SPC_ROOT}/include/spd_local.i
!
! --- Local customization of SPD
! --- This file was created automaticly on $DATE_LONG
! 
      CHARACTER  SPD__PREFIX*$SPC_PREFIX_LEN, SPD__ROOT*$SPC_ROOT_LEN, SPD__DOC*$SPC_DOC_LEN, SPD__SHARE*$SPC_SHARE_LEN, SPD__SCRIPT*$SPC_SCRIPT_LEN, SPD__VERSION*$SPC_VERSION_LEN, SPC__PETOOLS*$SPC__PETOOLS_LEN
      PARAMETER  ( SPD__PREFIX  = '$SPC_PREFIX' )
      PARAMETER  ( SPD__ROOT    = '$SPC_ROOT' )
      PARAMETER  ( SPD__DOC     = '$SPC_PREFIX/spd/doc'    )
      PARAMETER  ( SPD__SHARE   = '$SPC_PREFIX/spd/share'  )
      PARAMETER  ( SPD__SCRIPT  = '$SPC_PREFIX/spd/script' )
      PARAMETER  ( SPC__PETOOLS = '$SPC__PETOOLS_USE' )
      PARAMETER  ( SPD__VERSION = '$SPC_VERSION' )
EOF
# 
# --- End of SPD local customization include block
#
cat $SPC_ROOT/src/spd_client_version_template.f | \
    sed "s/@SPD_CLIENT_VERSION_STR@/$SPC_VERSION/g" > \
    $SPC_ROOT/src/spd_client_version.f 
#
if ( -d $SPC_ROOT/example ) then
     cat $SPC_ROOT/example/example_01_template.csh | \
         sed 's@$SPC_ROOT@'"$SPC_ROOT@g" > \
         $SPC_ROOT/example/example_01.csh
     cat $SPC_ROOT/example/example_02_template.csh | \
         sed 's@$SPC_ROOT@'"$SPC_ROOT@g" > \
         $SPC_ROOT/example/example_02.csh
     cat $SPC_ROOT/example/example_03_template.csh | \
         sed 's@$SPC_ROOT@'"$SPC_ROOT@g" > \
         $SPC_ROOT/example/example_03.csh
     cat $SPC_ROOT/example/conf_example_template.shs | \
         sed 's@$SPC_ROOT@'"$SPC_ROOT@g" > \
         $SPC_ROOT/example/conf_example.shs
     cat $SPC_ROOT/example/conf_example_template.sbs | \
         sed 's@$SPC_ROOT@'"$SPC_ROOT@g" > \
         $SPC_ROOT/example/conf_example.sbs
     chmod o+x,g+x,u+x $SPC_ROOT/example/example_01.csh
     chmod o+x,g+x,u+x $SPC_ROOT/example/example_02.csh
     chmod o+x,g+x,u+x $SPC_ROOT/example/example_03.csh
     set new_spd_share_name = $SPC_PREFIX/spd/share
     set old_spd_share_name = `cat ${SPC_ROOT}/example/spd_cli_example_m.m | grep "spd_share  ="  | awk '{print $3}' | sed "s@'@@g"` 
     if ( `uname` == "Darwin" ) then
           sed -i '' "s@$old_spd_share_name@$new_spd_share_name@g" ${SPC_ROOT}/example/spd_cli_example_m.m
       else
           sed -i    "s@$old_spd_share_name@$new_spd_share_name@g" ${SPC_ROOT}/example/spd_cli_example_m.m
     endif
#
     set new_spd_path = $SPC_PREFIX
     set old_spd_path = `cat ${SPC_ROOT}/example/spd_cli_example.cnf | grep "SPD_PATH:"  | awk '{print $2}' | sed "s@'@@g"` 
     if ( `uname` == "Darwin" ) then
          sed -i '' "s@$old_spd_path@$new_spd_path@g" ${SPC_ROOT}/example/spd_cli_example.cnf
       else
          sed -i    "s@$old_spd_path@$new_spd_path@g" ${SPC_ROOT}/example/spd_cli_example.cnf
     endif
     set SPD_EXAMPLE_AVAILABLE = YES
   else
     set SPD_EXAMPLE_AVAILABLE = NO
endif
#
set DATE_LONG = `date "+%d-%h-%Y %H:%M:%S" | tr "[a-z]" "[A-Z]"`
#
if ( `uname` == "Linux" ) then
     set num_threads     = `lscpu | grep '^CPU(s):' | awk '{print $2}'`
     set num_thr_per_cpu = `lscpu | grep '^Thread(s) per core:' | awk '{print $4}'`
     set num_cores = `expr $num_threads / $num_thr_per_cpu`
  else if ( `uname` == "Darwin" ) then
     set num_cores = `sysctl -n  machdep.cpu.core_count`
  else 
     set num_cores = 1
endif
#
if ( `echo $SPC_F95 | grep GNU` != "" ) then
      setenv SPC_OPENMP "-fopenmp"
   else
      setenv SPC_OPENMP "-openmp"
endif
set out_file = ${SPC_ROOT}/Makefile
if ( -f $out_file ) rm -f $out_file
cat ${SPC_ROOT}/Makefile.in1 > $out_file
echo "" >> $out_file
echo "SPC_NUM_PROC = $num_cores" >> $out_file
echo "" >> $out_file
#
$ECHO "SPC_C            = $SPC_C"                        >> $out_file
$ECHO "SPC_F95_NOOPT    = $SPC_F95_NOOPT"                >> $out_file
$ECHO "SPC_F95_OPT      = $SPC_F95_OPT"                  >> $out_file
$ECHO "SPC_F95          = $SPC_F95"                      >> $out_file
$ECHO "SPC_LINK         = $SPC_LINK"                     >> $out_file
$ECHO "SPC_C_LINK       = $SPC_C_LINK"                   >> $out_file
$ECHO "SPC_ROOT         = $SPC_ROOT"                     >> $out_file
$ECHO "SPC_PREFIX       = $SPC_PREFIX"                   >> $out_file
$ECHO "SPC_BIN          = $SPC_PREFIX/bin"               >> $out_file
$ECHO "SPC_LIB          = $SPC_PREFIX/lib"               >> $out_file
$ECHO "SPC_PETOOLS_USE  = $SPC_PETOOLS_USE"              >> $out_file
$ECHO "SPC_PETOOLS_INC  = $SPC_PETOOLS_INC"              >> $out_file
$ECHO "SPC_PETOOLS_LIB  = $SPC_PETOOLS_LIB"              >> $out_file
$ECHO "SPC_LAPACK_LIB   = $SPC_LAPACK_LIB"               >> $out_file
$ECHO "SPC_LDFLAGS      = $SPC_LDFLAGS"                  >> $out_file
$ECHO "SPC_OS           = $SPC_OS"                       >> $out_file
$ECHO "SPC_OPENMP       = $SPC_OPENMP"                   >> $out_file
$ECHO "SPC_VERSION      = $SPC_VERSION"                  >> $out_file
$ECHO "SPC_F95_PUNCH    = $SPC_F95_OPT" | sed 's@-ffree-form@@g' | sed 's@ -FR @ -fixed @g'  >> $out_file
$ECHO "PETOOLS_SOURCE_DIR = $PETOOLS_SOURCE_DIR"         >> $out_file
$ECHO "SPD_SOURCE_DIR     = $SPD_SOURCE_DIR"             >> $out_file
$ECHO "LAPACK_SOURCE_DIR  = $LAPACK_SOURCE_DIR"          >> $out_file
$ECHO "SPD_EXAMPLE_AVAILABLE = $SPD_EXAMPLE_AVAILABLE"   >> $out_file
$ECHO "NUM_PROC         = $num_cores"                    >> $out_file
$ECHO "CONF_LOG         = $CONF_LOG"                     >> $out_file
$ECHO "BUILD_LOG        = $BUILD_LOG"                    >> $out_file
cat ${SPC_ROOT}/Makefile.in2                             >> $out_file
setenv SPC_F95_PUNCH `echo $SPC_F95_OPT | sed 's@-ffree-form@@g' | sed 's@ -FR @ -fixed @g'` >> $out_file
#
set out_vars = ${SPC_ROOT}/support/spc_vars
if ( -f $out_vars ) rm -f $out_vars
$ECHO "#\!/bin/csh"                                          > $out_vars
$ECHO "setenv SPC_C            ${qt}${SPC_C}${qt}"          >> $out_vars
$ECHO "setenv SPC_F95_NOOPT    ${qt}$SPC_F95_NOOPT${qt}"    >> $out_vars
$ECHO "setenv SPC_F95_OPT      ${qt}$SPC_F95_OPT${qt}"      >> $out_vars
$ECHO "setenv SPC_F95          ${qt}$SPC_F95${qt}"          >> $out_vars
$ECHO "setenv SPC_F95_PUNCH    ${qt}$SPC_F95_PUNCH${qt}"    >> $out_vars
$ECHO "setenv SPC_LINK         ${qt}$SPC_LINK${qt}"         >> $out_vars
$ECHO "setenv SPC_C_LINK       ${qt}$SPC_C_LINK${qt}"       >> $out_vars
$ECHO "setenv SPC_ROOT         $SPC_ROOT"                   >> $out_vars
$ECHO "setenv SPC_PREFIX       $SPC_PREFIX"                 >> $out_vars
$ECHO "setenv SPC_BIN          $SPC_PREFIX/bin"             >> $out_vars
$ECHO "setenv SPC_LIB          $SPC_PREFIX/lib"             >> $out_vars
$ECHO "setenv SPC_PETOOLS_USE  $SPC_PETOOLS_USE"            >> $out_vars
$ECHO "setenv SPC_PETOOLS_INC  ${qt}$SPC_PETOOLS_INC${qt}"  >> $out_vars
$ECHO "setenv SPC_PETOOLS_LIB  ${qt}$SPC_PETOOLS_LIB${qt}"  >> $out_vars
$ECHO "setenv SPC_LAPACK_LIB   ${qt}$SPC_LAPACK_LIB${qt}"   >> $out_vars
$ECHO "setenv SPC_LDFLAGS      ${qt}$SPC_LDFLAGS${qt}"      >> $out_vars
$ECHO "setenv SPC_OS           $SPC_OS"                     >> $out_vars
$ECHO "setenv SPC_OPENMP       $SPC_OPENMP"                 >> $out_vars
$ECHO "setenv SPC_VERSION      $SPC_VERSION"                >> $out_vars
#
source ${SPC_ROOT}/support/spc_vars
#
if ( `uname` == "SunOS" ) then
      set pwd_save = `pwd`
      cd  $SUPPORT_PATH 
      make -f sun_preproc.mak clean
      make -f sun_preproc.mak 
      set status_make = $status
      cd $pwd_save
      if ( $status_make != 0 ) then
           echo "Error in compiling/linking sun_preproc"
	   exit 1
      endif
endif
rm $SPC_ROOT/bin/check_endian
rm $SPC_ROOT/bin/learn_fortran_true
rm $SPC_ROOT/bin/learn_fortran_false
if ( -f $SPC_PREFIX/include/spd.i ) rm $SPC_PREFIX/include/spd.i 
if ( -d $SPC_ROOT/example == 0 ) then
     echo "No example was found. You need spd_client_xxxxxxxx_example.tar.bz2"
endif
chmod g+rw,o+r ${SPC_ROOT}/Makefile
chmod g+rw,o+r ${SPC_ROOT}/support/spc_vars
#
echo "config.csh is done"
