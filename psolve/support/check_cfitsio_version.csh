#!/bin/csh -f
setenv LC_ALL C
setenv LANG   C
make -f $SOLVE_ROOT/support/check_cfitsio_version.mak
if( $status != 0 ) then
    echo "Cannot link against cfitsio library"
    make -f $SOLVE_ROOT/support/check_cfitsio_version.mak clean 
    exit 1
endif
if ( -f $SOLVE_ROOT/bin/check_cfitsio_version.e ) then
        $SOLVE_ROOT/bin/check_cfitsio_version.e 
	if ( $status != 0 ) then
             echo "Cannot find cfitsio library"
             make -f $SOLVE_ROOT/support/check_cfitsio_version.mak clean 
	     exit 1
        endif
  else 
        echo "I Cannot link against cfitsio library"
        make -f $SOLVE_ROOT/support/check_cfitsio_version.mak clean 
	exit 1
endif
exit 0
