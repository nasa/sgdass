#!/bin/csh -f
setenv LANG   C
setenv LC_ALL C
make -f $PETOOLS_ROOT/support/readline_check.mak clean
make -f $PETOOLS_ROOT/support/readline_check.mak
if ( -f $PETOOLS_ROOT/bin/readline_check.e ) then
        echo "ok" | $PETOOLS_ROOT/bin/readline_check.e 
	if ( $status != 0 ) then
             echo "Cannot find readline library"
             echo "Please install readline library from http://cnswww.cns.cwru.edu/php/chet/readline/rltop.html"
             make -f $PETOOLS_ROOT/support/readline_check.mak clean
	     exit 1
        endif
  else 
        echo "Cannot link against readline library"
        echo "Please install readline library from http://cnswww.cns.cwru.edu/php/chet/readline/rltop.html"
	exit 1
endif
make -f $PETOOLS_ROOT/support/readline_check.mak clean
exit 0
