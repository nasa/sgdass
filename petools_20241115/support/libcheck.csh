#!/bin/csh
# ************************************************************************
# *                                                                      *
# *   C-shell program for checkiing libraries needed for petools.        *
# *   If they present and OK, then it returns 0 exit code.               *
# *                                                                      *
# * ### 25-JUN-2002  libcheck.csh  v2.1 (c)  L. Petrov  03-MAY-2004 ###  *
# *                                                                      *
# ************************************************************************
setenv LANG   C
setenv LC_ALL C
set    libvec_check_make  = $PETOOLS_ROOT/support/libvec_check.mak
set    libvec_check_temp  = /tmp/libvec__$$.out
setenv libvec_check_exec    /tmp/libvec__$$.e
#
set    libblas_check_make = $PETOOLS_ROOT/support/libblas_check.mak
set    libblas_check_temp = /tmp/libblas__$$.out
setenv libblas_check_exec   /tmp/libblas__$$.e
#
set    liblapack_check_make = $PETOOLS_ROOT/support/liblapack_check.mak
set    liblapack_check_temp = /tmp/liblapack__$$.out
setenv liblapack_check_exec   /tmp/liblapack__$$.e
#
set saved_pwd = `pwd`
cd  $PETOOLS_ROOT/support
#
set OS_name = `uname`
switch ( $OS_name )
   case "HP-UX":
     set ECHO = "/bin/echo"
     set qt = "\0042"
     breaksw
   case "SunOS":
     set ECHO = "/bin/echo"
     set qt = "\042"
     breaksw
   case "Linux":
     set ECHO = "/bin/echo -e"
     set qt = "\042"
     breaksw
endsw
#
$ECHO "Checking library libvec  ...\c"
make -f $libvec_check_make >& $libvec_check_temp 
set status_libvec_check = $status
if ( $status_libvec_check != 0 ) then
     $ECHO "  failed"
     cat $libvec_check_temp 
     cd $saved_pwd
     exit (1)
  else 
     $ECHO "  ok"
     make -f $libvec_check_make clean >& $libvec_check_temp 
     rm -f $libvec_check_temp 
endif
#
$ECHO "Checking library libblas ...\c"
make -f $libblas_check_make >& $libblas_check_temp 
set status_libblas_check = $status
if ( $status_libblas_check != 0 ) then
     $ECHO "  failed"
     cat $libblas_check_temp 
     cd $saved_pwd$
     exit (1)
  else 
     $ECHO "  ok"
     make -f $libblas_check_make clean >& $libblas_check_temp 
     rm -f $libblas_check_temp 
endif
#
$ECHO "Checking library liblapack ...\c"
make -f $liblapack_check_make >& $liblapack_check_temp 
set status_liblapack_check = $status
if ( $status_liblapack_check != 0 ) then
     $ECHO "  failed"
     cat $liblapack_check_temp 
     cd $saved_pwd$
     exit (1)
  else 
     $ECHO "  ok"
     make -f $liblapack_check_make clean >& $liblapack_check_temp 
     rm -f $liblapack_check_temp 
endif
cd $saved_pwd
exit 0
