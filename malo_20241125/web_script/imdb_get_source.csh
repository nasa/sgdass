#!/bin/csh -f
limit stacksize       2000000
setenv GOMP_STACKSIZE 2000000
#
setenv  Ex  /home/lpetrov/exec
setenv  conf_file /vlbi/imdb/imdb_search.cnf
#
setenv PGPLOT_FONT /opt64/bin/grfont.dat
setenv PGPLOT_DEV  /XW
setenv PGPLOT_XW_MARGIN 1.0
setenv SOLVE_LIB_PGPLOT /opt64/lib/libpgplot.so 
setenv PGPLOT_DIR       /opt64/lib
setenv SOLVE_PGPLOT_X_INC /usr/include/X11
setenv GOMP_STACKSIZE 20000
#
echo "404: okay"
#
/bin/echo -e "Content-type: text/html\n\n"
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

if ( $QUERY_STRING == "" ) then
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
