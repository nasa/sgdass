#!/bin/csh -f
#

echo "404: okay"
/bin/echo -e "Content-type: text/html\n\n"
echo " "
echo '<\!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">'
echo '<HTML LANG="ru">'
echo '<HEAD>'
echo '<META http-equiv="Content-Type" content="text/html; charset=koi8-r">'
echo '</HEAD'
echo '<BODY>'
if ( $HTTP_HOST == "alt.massloading.net" ) then
     echo 'A mirror of the International Mass Loading Service <A HREF="http://alt.massloading.net"> http://alt.massloading.net </A> runs independently. If you cann access the main server, please use the mirror server.'
endif
</BODY>
</HTML>
