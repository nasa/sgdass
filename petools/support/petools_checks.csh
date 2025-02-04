#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   C-shell program petools_checks makes checks of system environment  *
# *   and make a conclusion whether all system programs and system       *
# *   libraries are present in order to compile and link programs of     *
# *   petools library.                                                   *
# *                                                                      *
# * ## 16-NAY-2000 petools_checks.csh v1.16 (c) L. Petrov 15-MAY-2017 ## *
# *                                                                      *
# ************************************************************************
setenv LANG   C
setenv LC_ALL C
if ( $?PETOOLS_ROOT == 0 ) then
    echo "---  petools_checks: Environment variable PETOOLS_ROOT was not defined  ---"
    exit ( 2 )
endif
if ( $1 == "" ) then
     set log_file = /tmp/mk5__$$.log
  else
     set log_file = $1
endif
set OS_name = `uname`
switch ( $OS_name )
   case "HP-UX":
     set ECHO = "/bin/echo"
     set qt = "\0042"
     setenv MAKE gmake
     breaksw
   case "SunOS":
     set ECHO = "/bin/echo"
     set qt = "\042"
     setenv MAKE gmake
     breaksw
   case "Linux":
     set ECHO = "/bin/echo -e"
     set qt = "\042"
     setenv MAKE make
     breaksw
   case "Darwin":
     set ECHO = "/bin/echo"
     set qt = "\042"
     setenv MAKE make
     breaksw
endsw
set CHECK = "${PETOOLS_ROOT}/support/check_file.csh"
set PWD_OLD = `pwd`
set tmp_obj = /tmp/petools__obj__$$
#
# --- C shell
#
# $ECHO "Checking path ...\c" | tee -a $log_file
# set old_path = `pwd`
# cd ${PETOOLS_ROOT}/support/
# csh_test.csh | tee -a $log_file
# if ( $status == 0 ) then
#      $ECHO "  ok" | tee -a $log_file
#      cd $old_path
#   else
#      cd $old_path
#      $ECHO " " | tee -a $log_file
#      $ECHO "petools_checks: PATH=$PATH"
#      $ECHO "petools_checks: Your path envirnoment variable path as defined for csh " | tee -a $log_file
#      $ECHO "petools_checks: does not contain . directory. Please, correct your " | tee -a $log_file
#      $ECHO "petools_checks: startup command files and try again. Consult your " | tee -a $log_file
#      $ECHO "petools_checks: system administrator, if you cannot solve this problem yourself." | tee -a $log_file
#      goto error
# endif
#
# --- Commands
#
$CHECK awk    command; if ( $status != 0 ) goto error
$CHECK cc     command; if ( $status != 0 ) goto error
$CHECK csh    command; if ( $status != 0 ) goto error
$CHECK find   command; if ( $status != 0 ) goto error
$CHECK $MAKE  command; if ( $status != 0 ) goto error
#if ( `uname` == "Linux" ) then
#     $CHECK mail   command; if ( $status != 0 ) goto error
#  else
#     $CHECK mailx  command; if ( $status != 0 ) goto error
#endif
if ( `uname` == "SunOS" ) then
     $CHECK /usr/ucb/expr  command; if ( $status != 0 ) goto error
endif
$CHECK sed    command; if ( $status != 0 ) goto error
$CHECK sh     command; if ( $status != 0 ) goto error
$CHECK unzip  command; if ( $status != 0 ) goto error
$CHECK zip    command; if ( $status != 0 ) goto error
#
# --- Include files
#
if ( $PETOOLS_PGPLOT == "YES" ) then
     $CHECK $SOLVE_PGPLOT_X_INC file_read; if ( $status != 0 ) goto error
endif
#
# --- Libaraies with explicit names
#
if ( $PETOOLS_PGPLOT == "YES" ) then
     $CHECK $SOLVE_LIB_XHP11   file_read; if ( $status != 0 ) goto error
     $CHECK $SOLVE_LIB_X11     file_read; if ( $status != 0 ) goto error
     $CHECK $SOLVE_LIB_XT      file_read; if ( $status != 0 ) goto error
endif
#
# --- Libraries with implicit names
#
if ( `uname` == "HP-UX" ) then
      $PETOOLS_ROOT/support/libcheck.csh; if ( $status != 0 ) goto error
endif
#
if ( $PETOOLS_PGPLOT == "YES" ) then
     $ECHO "Checking X11 include files ...\c" | tee -a $log_file
     ${MK5_C} -I $MK5_X11_INCLUDE -c -o $tmp_obj $PETOOLS_ROOT/support/x11_test.c >>& $log_file
     if ( $status == 0 ) then
         $ECHO "  ok" | tee -a $log_file
         rm -f $tmp_obj
       else
         $ECHO " "
         $ECHO "petools_checks: Tyu-u-u! Your installation of X11 is screwed up. :-(" | tee -a $log_file
         $ECHO "petools_checks: System cannot find some library files. " | tee -a $log_file
         $ECHO "petools_checks: Please, talk with your system administrator" | tee -a $log_file
         rm -f $tmp_obj
         goto error
     endif
endif
#
$ECHO "Checking all libraries together ...\c" | tee -a $log_file
cd $PETOOLS_ROOT/support
if ( "$PETOOLS_BLAS" == "NO" ) then
     set    SOLVE_LIB_BLAS_SAVE = "$SOLVE_LIB_BLAS"
     setenv SOLVE_LIB_BLAS ""
   else
     setenv SOLVE_LIB_BLAS  "$PETOOLS_BLAS"
endif
$MAKE -f f90_test.mak clean >>& $log_file
$MAKE -f f90_test.mak       >>& $log_file
if ( $status == 0 ) then
     rm -f ${PETOOLS_ROOT}/bin/f90_test
     rm -f f90_test.o
     $ECHO "  ok" | tee -a $log_file
  else
     $ECHO " "
     $ECHO "petools_checks: Something wrong with libraries: " | tee -a $log_file
     $ECHO "petools_checks: System cannot find some library files. " | tee -a $log_file
     $ECHO "petools_checks:  Consult your system administratopr, please" | tee -a $log_file
     rm -f ${PETOOLS_ROOT}/bin/f90_test
     rm -f f90_test.o
     echo "SOLVE_LIB_BLAS = $SOLVE_LIB_BLAS"
     echo "PETOOLS_BLAS   = $PETOOLS_BLAS"
     goto error
endif
#
if ( "$PETOOLS_BLAS" == "NO" ) then
     setenv SOLVE_LIB_BLAS  "$SOLVE_LIB_BLAS_SAVE"
endif
#
if ( $1 == "" ) then
     rm -f $log_file
endif
$ECHO "petools_checks -- All checks are ok" | tee -a $log_file
cd $PWD_OLD
rm -f $PETOOLS_ROOT/support/*.o >& /dev/null
if ( -f /tmp/mk5__$$.log ) rm -f /tmp/mk5__$$.log
exit 0
#
error:
$ECHO "petools_checks -- FAILURE in configuration checks" | tee -a $log_file
$ECHO "petools_checks -- Look in $log_file for details"
cd $PWD_OLD
rm -f ${PETOOLS_ROOT}/support/*.o >& /dev/null
exit 1
