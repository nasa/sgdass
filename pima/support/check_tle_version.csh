#!/bin/csh -f
umask 0002
make -f $PIMA_ROOT/support/check_tle_version.mak
if( $status != 0 ) then
    echo "Cannot link against tle library"
    make -f $PIMA_ROOT/support/check_tle_version.mak clean 
    exit 1
endif
if ( -f $PIMA_ROOT/bin/check_tle_version.e ) then
        $PIMA_ROOT/bin/check_tle_version.e 
	if ( $status != 0 ) then
             echo "Cannot find tle library"
             make -f $PIMA_ROOT/support/check_tle_version.mak clean 
	     exit 1
        endif
  else 
        echo "Cannot link against tle library"
        make -f $PIMA_ROOT/support/check_tle_version.mak clean 
	exit 1
endif
exit 0
