#!/bin/csh -f
setenv LANG   C
setenv LC_ALL C
make -f $PETOOLS_ROOT/support/check_libblas.mak
if ( $status != 0 ) then
     echo "Failure in an attempt to run BLAS library check"
#     make -f $PETOOLS_ROOT/support/check_libblas.mak clean
     exit 1
endif
if ( -f $PETOOLS_ROOT/bin/check_libblas ) then
        $PETOOLS_ROOT/bin/check_libblas 
	if ( $status != 0 ) then
             echo "Failure in an attempt to run BLAS library check"
             make -f $PETOOLS_ROOT/support/check_libblas.mak clean
	     exit 1
        endif
  else 
        echo "Failure in an attempt to link against BLAS library"
        make -f $PETOOLS_ROOT/support/check_libblas.mak clean
	exit 1
endif
make -f $PETOOLS_ROOT/support/check_libblas.mak clean
exit 0
