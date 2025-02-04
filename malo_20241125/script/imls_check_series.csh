#!/bin/csh -f
set temp1_file = /tmp/malo_1__$$.check
set temp2_file = /tmp/malo_2__$$.check
/bin/echo -n "$1 $2 $3  "
ls -c1 /imls/$1/$2/$3 | grep -v '#' | sort > $temp1_file; 
$Ex/check_listing_gaps.e $temp1_file $4 | tee $temp2_file
if ( `cat $temp2_file | wc -w` == "0" ) then
      set str_beg = `cat $temp1_file | head -1`
      set str_end = `cat $temp1_file | tail -1`
      echo "$str_beg  $str_end"
   else
      echo ""
endif
rm  $temp1_file 
rm  $temp2_file 
