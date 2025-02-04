#!/bin/csh -f
setenv LANG   C
setenv LC_ALL C
if ( -f $PNG_INC_DIR/png.h == 0 ) then
     echo "did not find png header file $PNG_INC_DIR/png.h"
     exit 1
endif
make -f $PETOOLS_ROOT/support/check_png.mak
if( $status != 0 ) then
    make -f $PETOOLS_ROOT/support/check_png.mak clean  
    exit 1
endif
$PETOOLS_ROOT/bin/png_test.e
if( $status != 0 ) then
    make -f $PETOOLS_ROOT/support/check_png.mak clean  
    exit 1
endif
make -f $PETOOLS_ROOT/support/check_png.mak clean  
exit 0
