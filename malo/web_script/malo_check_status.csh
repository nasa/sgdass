#!/bin/csh
setenv BIN_DIR /opt64/bin
setenv MALO_SHARE  `$BIN_DIR/malo_inq share`
setenv MALO_SCRIPT `$BIN_DIR/malo_inq script`
python3 $MALO_SCRIPT/malo_stat.py -c $MALO_SHARE/astrogeo_stat_all.conf
#
echo "404: okay"
/bin/echo -e "Content-type: text/html\n\n"
#
/bin/echo '<\!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">'
/bin/echo '<HTML LANG="en">'
/bin/echo '<HEAD>'
if ( `hostname` == "astrogeo" ) then
     /bin/echo '<META HTTP-EQUIV="refresh" CONTENT="0;URL=http://alt.massloading.net/stat_all.html">'
else
     /bin/echo '<META HTTP-EQUIV="refresh" CONTENT="0;URL=http://massloading.net/stat_all.html">'
endif
/bin/echo '</HEAD>'
/bin/echo '<BODY>'
/bin/echo '</BODY>' 
/bin/echo '</HTML>' 
