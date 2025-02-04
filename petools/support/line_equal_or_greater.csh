#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   This C-shell routine retirns 1 if the first character argument     *
# *   is alphabetically greater or equal than the second argument and it *
# *   returns 0 otherwise.                                               *
# *                                                                      *
# *  ### 04-NOV-2003               v1.0 (c)  L. Petrov  04-NOV-2003 ###  *
# *                                                                      *
# ************************************************************************
set temp_file = /tmp/comparizon_$$
set temp_file_sorted = /tmp/comparizon_sorted_$$
if ($#argv < 2) exit (-1)
echo "$1"  >  $temp_file
echo "$2"  >> $temp_file
sort -nr $temp_file > $temp_file_sorted
set  first_line = "`cat $temp_file_sorted`"
rm -f $temp_file $temp_file_sorted
#
if ( "$first_line[1]" == "$1" ) then
     echo 1
   else
     echo 0
endif
