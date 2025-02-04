#!/bin/csh -f
limit stacksize         2000000
setenv GOMP_STACKSIZE   2000000
setenv PGPLOT_FONT      /opt64/bin/grfont.dat
setenv PGPLOT_DEV       /XW
setenv PGPLOT_XW_MARGIN 1.0
setenv LD_LIBRARY_PATH  /opt64/lib:/progs/spd_20090409/src:/opt64/lib/python3.3/lib-dynload
#
setenv search_calib_exe  /astrogeo.org/web_exec/search_calib.e 
setenv plot_name         /apache_temp/calib_plot__$$
setenv search_calib_conf /vlbi/imdb/search_calib.cnf
#
echo "404: okay"
#
/bin/echo -e "Content-type: text/html\n\n"
/bin/echo '<\!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">'
/bin/echo '<HTML LANG="ru">'
/bin/echo '<HEAD>'
/bin/echo '<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1">'

if ( `echo "$QUERY_STRING " | grep '|'` != "" ) then
     echo "<PRE>"
     echo "Unsupported QUERY_STRING: ", $QUERY_STRING 
     echo "</PRE>"
     exit 0
endif     

if ( `expr length "$QUERY_STRING "` > 128 ) then
     echo "<PRE>"
     echo "QUERY_STRING is too long"
     echo "</PRE>"
     exit 0
endif     

##set QUERY_STRING = 'ra=12_25_17.88&dec=-25_01_56.22&num_sou=5&format=html'
#echo "QUERY: $QUERY_STRING"
#
$search_calib_exe  "QUERY: $QUERY_STRING" \
                   $search_calib_conf \
                   $plot_name 
