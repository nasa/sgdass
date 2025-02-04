#!/bin/csh -f
umask 0022
make -f $VTD_ROOT/support/check_cfitsio_version.mak
if( $status != 0 ) then
    echo "Cannot link against cfitsio library"
    make -f $VTD_ROOT/support/check_cfitsio_version.mak clean 
    exit 1
endif
if ( -f $VTD_ROOT/bin/check_cfitsio_version.e ) then
        $VTD_ROOT/bin/check_cfitsio_version.e 
	if ( $status != 0 ) then
             echo "Cannot find cfitsio library"
             make -f $VTD_ROOT/support/check_cfitsio_version.mak clean 
	     exit 1
        endif
  else 
        echo "I Cannot link against cfitsio library"
        make -f $VTD_ROOT/support/check_cfitsio_version.mak clean 
	exit 1
endif
exit 0
