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
# * ### 22-JUL-2004 imdb_get_sources.csh v1.1 (d) L. Petrov  21-NOV-2025 ###  *
# *                                                                      *
# ************************************************************************
limit stacksize       2000000
setenv GOMP_STACKSIZE 2000000
#
setenv  Ex        /astrogeo/web_exec
setenv  conf_file /astrogeo/calib/imdb_search.cnf
#
setenv PGPLOT_FONT /opt64/bin/grfont.dat
setenv PGPLOT_DEV  /XW
setenv PGPLOT_XW_MARGIN 1.0
setenv SOLVE_LIB_PGPLOT /opt64/lib/libpgplot.so 
setenv PGPLOT_DIR       /opt64/lib
setenv SOLVE_PGPLOT_X_INC /usr/include/X11
setenv GOMP_STACKSIZE 20000
set imdb_dos_file = /astrogeo/imdb_dos.txt
set max_requests  = 6
#
#
echo "404: okay"
#
/bin/echo -e "Content-type: text/html\n\n<body><html>"
if ( `echo "$QUERY_STRING " | grep '|'` != "" ) then
     echo "Unsupported QUERY_STRING: ", $QUERY_STRING 
     echo $1
     exit 0
endif     
if ( `expr length "$QUERY_STRING "` > 128 ) then
     echo "<PRE>"
     echo "QUERY_STRING is too long"
     echo "</PRE>"
     exit 0
endif     
#
set res = `python3 /astrogeo/web_exec/main_url_sanitizer.py "$QUERY_STRING" url`
set is_query_ok = $status
#
if ( $is_query_ok != 0 ) then
     echo "<PRE>"
     echo "Wrong argument"
     echo "</PRE>"
     exit 0
endif
#
set num_requests = `ps -eaf | grep "imdb_\|search_\|calib_" | grep -v grep | wc -l` 
if ( $num_requests > $max_requests ) then
     set now = `date '+%Y%m%d_%H%M%S'`
     echo imdb_get_source.csh $now $REMOTE_ADDR $num_requests  | \
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
#echo "<PRE>"
#echo "$Ex/imdb_search.e list          $conf_file"
#echo "</PRE>"
#exit 0
if ( $QUERY_STRING == "" ) then
     $Ex/imdb_search.e list          $conf_file
  else if ( `echo $QUERY_STRING | grep get_list=do` != "" ) then
     $Ex/imdb_search.e list          $conf_file
  else if ( `echo $QUERY_STRING | grep source=` != "" ) then
     $Ex/imdb_search.e $QUERY_STRING $conf_file
  else if ( `echo $QUERY_STRING | grep source_name=` != "" ) then
     $Ex/imdb_search.e $QUERY_STRING $conf_file
  else if ( `echo $QUERY_STRING | grep source_coordinate` != "" ) then
     $Ex/imdb_search.e $QUERY_STRING $conf_file
  else 
     echo "Unsupported QUERY_STRING: ", $QUERY_STRING 
endif
