#!/bin/csh -f
setenv LANG   C
setenv LC_ALL C
make -f $PETOOLS_ROOT/support/check_openmp.mak
if ( $status != 0 ) then
     echo "Failure in an attempt to run OpenMP check"
     exit 1
endif
if ( -f $PETOOLS_ROOT/bin/check_openmp ) then
        $PETOOLS_ROOT/bin/check_openmp 
	if ( $status != 0 ) then
             echo "Failure in an attempt to run OpenMP check"
             make -f $PETOOLS_ROOT/support/check_openmp.mak clean
	     exit 1
        endif
  else 
        echo "Failure in an attempt to link against openMP library"
        make -f $PETOOLS_ROOT/support/check_openmp.mak clean
	exit 1
endif
make -f $PETOOLS_ROOT/support/check_openmp.mak clean
exit 0
