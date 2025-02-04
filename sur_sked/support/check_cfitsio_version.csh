#!/bin/csh -f
make -f $SUR_SKED_ROOT/support/check_cfitsio_version.mak
if( $status != 0 ) then
    echo "Cannot link against cfitsio library"
    make -f $SUR_SKED_ROOT/support/check_cfitsio_version.mak clean 
    exit 1
endif
if ( -f $SUR_SKED_ROOT/bin/check_cfitsio_version.e ) then
        $SUR_SKED_ROOT/bin/check_cfitsio_version.e 
	if ( $status != 0 ) then
             echo "Cannot find cfitsio library"
             make -f $SUR_SKED_ROOT/support/check_cfitsio_version.mak clean 
	     exit 1
        endif
  else 
        echo "I Cannot link against cfitsio library"
        make -f $SUR_SKED_ROOT/support/check_cfitsio_version.mak clean 
	exit 1
endif
exit 0
