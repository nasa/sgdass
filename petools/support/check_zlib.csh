#!/bin/csh -f
setenv LANG   C
setenv LC_ALL C
if ( -f $ZLIB_INC_DIR/zlib.h == 0 ) then
     echo "did not find zlib header file $ZLIB_INC_DIR/zlib.h"
     exit 1
endif
make -f $PETOOLS_ROOT/support/check_zlib.mak
if( $status != 0 ) then
    make -f $PETOOLS_ROOT/support/check_zlib.mak clean  
    exit 1
endif
$PETOOLS_ROOT/bin/zlib_test.e
if( $status != 0 ) then
    make -f $PETOOLS_ROOT/support/check_zlib.mak clean  
    exit 1
endif
make -f $PETOOLS_ROOT/support/check_zlib.mak clean  
exit 0
