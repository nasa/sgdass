#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Program
# *                                                                      *
# *   Copyright (c) 1975-2025 United States Government as represented by *
# *   the Administrator of the National Aeronautics and Space            *
# *   Administration. All Rights Reserved.                               *
# *   License: NASA Open Source Software Agreement (NOSA).               *
# *                                                                      *
# * ### 22-JUL-2004  calib_search_form.csh v1.4 (d) L. Petrov  21-NOV-2025 ###  *
# *                                                                      *
# ************************************************************************
limit stacksize         2000000
setenv GOMP_STACKSIZE   2000000
setenv PGPLOT_FONT      /opt64/bin/grfont.dat
setenv PGPLOT_DEV       /XW
setenv PGPLOT_XW_MARGIN 1.0
setenv LD_LIBRARY_PATH  /opt64/lib:/progs/spd_20090409/src:/opt64/lib/python3.3/lib-dynload
setenv GOMP_STACKSIZE 20000
#
setenv search_calib_exe  /astrogeo/web_exec/search_calib.e 
setenv plot_name         /apache_temp/calib_plot__$$
setenv search_calib_conf /astrogeo/calib/search_calib.cnf
set imdb_dos_file = /astrogeo/imdb_dos.txt
set max_requests  = 6
#
echo "404: okay"
#
/bin/echo -e "Content-type: text/html\n\n"
/bin/echo '<\!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">'
/bin/echo '<HTML LANG="en">'
/bin/echo '<HEAD>'
/bin/echo '<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1">'
#
if ( `echo "$QUERY_STRING " | grep '|'` != "" ) then
     echo "<PRE>"
     echo "Unsupported QUERY_STRING: ", $QUERY_STRING 
     echo "</PRE>"
     exit 0
endif     
#
if ( `expr length "$QUERY_STRING "` > 128 ) then
     echo "<PRE>"
     echo "QUERY_STRING is too long"
     echo "</PRE>"
     exit 0
endif     
#
set res = `python3 /astrogeo/web_exec/main_url_sanitizer.py $QUERY_STRING url`
set is_query_ok = $status
#
if ( $is_query_ok != 0 ) then
     echo "<PRE>"
     echo "Wrong QUERY_STRING $is_query_ok"
     echo "QUERY_STRING: $QUERY_STRING"
     echo "</PRE>"
     exit 0
endif
#
set num_requests = `ps -eaf | grep "imdb_\|search_\|calib_" | grep -v grep | wc -l` 
if ( $num_requests > $max_requests ) then
     set now = `date '+%Y%m%d_%H%M%S'`
     echo calib_search_form.csh $now $REMOTE_ADDR $num_requests  | \
         awk '{printf "%-21s %-20s %-15s %3d\n", $1, $2, $3, $4}' >> \
     $imdb_dos_file
     set tim_to_sleep = 1.`shuf -i 10-99 -n 1`
     sleep $tim_to_sleep 
     echo "<PRE>"
     echo "Too many requests. Please try later."
     echo "</PRE>"
     exit 0
endif
#
$search_calib_exe  "QUERY: $QUERY_STRING" \
                   $search_calib_conf \
                   $plot_name 
