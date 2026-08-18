#!/bin/csh -f
setenv LANG   C
setenv LC_ALL C
make -f $PETOOLS_ROOT/support/ncurses_check.mak clean
make -f $PETOOLS_ROOT/support/ncurses_check.mak
if ( -f $PETOOLS_ROOT/bin/ncurses_check.e ) then
        echo "ok" | $PETOOLS_ROOT/bin/ncurses_check.e 
	if ( $status != 0 ) then
             echo "Cannot find ncurses library"
             echo "Please install ncurses library from http://cnswww.cns.cwru.edu/php/chet/ncurses/rltop.html"
             make -f $PETOOLS_ROOT/support/ncurses_check.mak clean
	     exit 1
        endif
  else 
        echo "Cannot link against ncurses library"
        echo "Please install ncurses library from http://cnswww.cns.cwru.edu/php/chet/ncurses/rltop.html"
	exit 1
endif
make -f $PETOOLS_ROOT/support/ncurses_check.mak clean
exit 0

