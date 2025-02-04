#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   This C-shell routine checks versions in the form "x", "x.x", or    *
# *   "xx.xx.xx" and retirns 1 if the first version is equal or greater  *
# *   and it returns 0 otherwise.                                        *
# *                                                                      *
# * ## version_equal_or_greater.csh  v1.2 (c) L. Petrov 12-MAY-2017 ###  *
# *                                                                      *
# ************************************************************************
#
set arg1 = `echo $1 | tr "." " " | tr "-" " " | tr -c '0-9' " "`
set arg2 = `echo $2 | tr "." " "`
if ( $arg1[1] < $arg2[1] ) then
     echo "0"
     exit
  else if ( $arg1[1] > $arg2[1] ) then
     echo "1"
     exit
  else 
     if ( $#arg1 <  2 && $#arg2 < 2  ) then
          echo "1"
          exit  1
     endif
#
     if ( $#arg1 == 1 && $#arg2 > 1  ) then
          echo "0"
          exit  1
     endif
#
     if ( $#arg1 >  1 && $#arg2 == 1 ) then
          echo "1"
          exit  1
     endif
#
     if ( $arg1[2] < $arg2[2] ) then
          echo "0"
          exit
       else if ( $arg1[2] > $arg2[2] ) then
          echo "1"
          exit
       else 
          if ( $#arg1 <  3 && $#arg2 < 3  ) then
               echo "1"
	       exit  1
          endif
          if ( $#arg1 == 2 && $#arg2 > 2  ) then
	       echo "0"
               exit  0
          endif
          if ( $#arg1 >  2 && $#arg2 == 2 ) then
	       echo "1"
               exit  1
          endif
          if ( $arg1[3] < $arg2[3] ) then
               echo "0"
               exit
            else if ( $arg1[3] > $arg2[3] ) then
               echo "1"
               exit
            else 
               echo "1"
               exit
  else 
endif
