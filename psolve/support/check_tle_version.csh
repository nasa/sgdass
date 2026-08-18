#!/bin/csh -f
umask 0022
make -f $SOLVE_ROOT/support/check_tle_version.mak
if( $status != 0 ) then
    echo "Cannot link against tle library"
    make -f $SOLVE_ROOT/support/check_tle_version.mak clean 
    exit 1
endif
if ( -f $SOLVE_ROOT/bin/check_tle_version.e ) then
        $SOLVE_ROOT/bin/check_tle_version.e 
	if ( $status != 0 ) then
             echo "Cannot find tle library"
             make -f $SOLVE_ROOT/support/check_tle_version.mak clean 
	     exit 1
        endif
  else 
        echo "Cannot link against tle library"
        make -f $SOLVE_ROOT/support/check_tle_version.mak clean 
	exit 1
endif
exit 0
