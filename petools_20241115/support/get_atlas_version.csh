#!/bin/csh -f
make -f $PETOOLS_ROOT/support/get_atlas_version.mak
if ( -f $PETOOLS_ROOT/bin/get_atlas_version ) then
	if ( $status != 0 ) then
             echo "Cannot find atlas library"
             make -f $PETOOLS_ROOT/support/get_atlas_version.mak clean
	     exit 1
        endif
  else 
        echo "Cannot link against atlas library"
        echo "Please install atals library"
        make -f $PETOOLS_ROOT/support/get_atlas_version.mak clean
	exit 1
endif
echo "ok" 
exit 0
