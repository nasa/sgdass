#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   C-shell program for parsing massloading subscription form.         *
# *                                                                      *
# * ### 23-JUL-2004 loading_form.csh  v2.0 (c) L. Petrov 02-AUG-2014 ### *
# *                                                                      *
# ************************************************************************
#
set exec_form         = /astrogeo.org/web_exec/loading_form.e 
set form_admin_e_mail = Leonid.Petrov@lpetrov.net
set from_address      = "apache@${SERVER_NAME}"
set temp_file         = /tmp/web__form__$$
#
# --- Settings for mail agent nail
#
setenv MAILRC      nail.rc
setenv NAILRC      nail.rc
#
setenv LC_CTYPE    ru_RU.KOI8-R
setenv LESSCHARSET koi8-r
setenv LC_ALL      ru_RU.KOI8-R
setenv LC_COLLATE  ru_RU.KOI8-R
setenv LANG        ru_RU.KOI8-R
#
setenv LC_CTYPE    C
setenv LESSCHARSET C
setenv LC_ALL      C
setenv LC_COLLATE  C
setenv LANG        C
#

echo "404: okay"
/bin/echo -e "Content-type: text/html\n\n"
echo " "
echo '<\!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">'
echo '<HTML LANG="ru">'
echo '<HEAD>'
echo '<META http-equiv="Content-Type" content="text/html; charset=koi8-r">'
echo "</HEAD>"
echo "<BODY>"

set host_line = `host $REMOTE_ADDR`
set REMOTE_HOST = $host_line[5]

if ( `expr substr  $QUERY_STRING 1 5 ` == "file=" ) then
      set len_str = `expr length $QUERY_STRING`
      set file_name = `expr substr  $QUERY_STRING 6 $len_str` 
      $exec_form send_form  $form_admin_e_mail $from_address $file_name 
      if ( -f $file_name ) rm -f $file_name 
      echo "Your request for subscribing list@massloading.net"
      echo "has been sent to the administrator of that list. <P>"
      echo "You will get confirmation by e-mail within several days."
      echo "<P>"
      echo " <FORM NAME=finish"
      echo "       ACTION=http://${SERVER_NAME}/"
      echo "       METHOD=GET >"
      echo "       <INPUT TYPE=submit VALUE='Finish'>"
      echo "       </FORM>"
      echo "<P>"
      echo "</BODY>"
      echo "</HTML>"
      exit 0
endif

set name        = (`$exec_form  name`)
set affiliation = (`$exec_form  affiliation`)
set e_mail      =  `$exec_form  e_mail`
set subscribe   =  `$exec_form  subscribe`
set password    =  `$exec_form  password`

set failure = "no"
if ( $password == "no_password" ) then
     echo "You did not enter PASSWORD."
     set failure = "yes"
     echo "<P>"
  else if ( $password == "failure" ) then
     echo "<FONT COLOR=A04030><B>Error: </B></FONT>"
     echo "Your password is wrong."
     echo "<P />"
     echo "Hmm. It looks like you are tired today and, probably, made a typo."
     echo "It is difficult to believe you have never seen it in your life or"
     echo "forgot a word for it. If in doubt, ask an advisory opinion of"
     echo "a 3+ years old kid."
     echo "<P>"
     set failure = "yes"
endif

set name_first = $name[1]
set affiliation_first = $affiliation[1]

if ( $password != "no_password" && $name_first == "" ) then
     echo "<FONT COLOR=A04030><B>Error: </B></FONT>"
     echo "The field NAME was not filled"
     echo "<P>"
     set failure = "yes"
endif

if ( $password != "no_password" && $affiliation_first == "" ) then
     echo "<FONT COLOR=A04030><B>Error: </B></FONT>"
     echo "The field AFFILIATION was not filled"
     echo "<P>"
     set failure = "yes"
endif

if ( $password != "no_password" && $e_mail == "" ) then
     echo "<FONT COLOR=A04030><B>Error: </B></FONT>"
     echo "The field E_MAIL ADDRESS was not filled"
     echo "<P>"
     set failure = "yes"
  else if ( $password != "no_password" && $e_mail == "invalid" ) then
     echo "<FONT COLOR=A04030><B>Error: </B></FONT>"
     echo "Your e_mail address is invalid"
     echo "<P>"
     set failure = "yes"
endif

if ( $failure == "yes" ) then
     echo "<P>"
     echo " <FORM NAME=continue"
     echo "       ACTION=http://${SERVER_NAME}/forms/loading_form.html"
     echo "       METHOD=GET >"
     echo "       <INPUT TYPE=submit VALUE='Try again'>"
     echo "       </FORM>"
     echo " <FORM NAME=cancel"
     echo "       ACTION=http://${SERVER_NAME}/"
     echo "       METHOD=GET >"
     echo "       <INPUT TYPE=submit VALUE='Cancel'>"
     echo "       </FORM>"
     echo "<P>"
   else
     if ( $password == "expert" ) then
         echo "Oh, I see you are an expert in etymology. Very good."
         echo "<P>"
     endif
     echo "Check your input: "
     echo "<PRE>"
     echo "NAME:        $name"
     echo "AFFILIATION: $affiliation"
     echo "E_MAIL:      $e_mail"
     echo "SUBSCRIBE:   $subscribe"
     echo "</PRE>"
     echo "<P>"

     setenv LC_ALL_OLD $LC_ALL 
     setenv LC_ALL      C  # otherwise date will be in different encoding
     set  date_long = `date "+%d-%h-%Y %H:%M:%S" | tr "[a-z]" "[A-Z]"`
     setenv LC_ALL LC_ALL_OLC
#
     echo "DATE:        $date_long"    >! $temp_file
     echo "REMOTE_HOST: $REMOTE_HOST"  >> $temp_file
     echo "NAME:        $name"         >> $temp_file
     echo "AFFILIATION: $affiliation"  >> $temp_file
     echo "E_MAIL:      $e_mail"       >> $temp_file
     echo "SUBSCRIBE:   $subscribe"    >> $temp_file

     echo " <FORM NAME=submit"
     echo "       ACTION=http://${SERVER_NAME}/cgi-bin/loading_form.csh?file=$temp_file"
     echo "       METHOD=POST >"
     echo "       <INPUT TYPE="SUBMIT" VALUE='Complete'>"
     echo "       </FORM>"
     echo " <FORM NAME=cancel"
     echo "       ACTION=http://$SERVER_NAME"
     echo "       METHOD=GET >"
     echo "       <INPUT TYPE=submit VALUE='Cancel'>"
     echo "       </FORM>"
     echo "<P>"
endif

echo "</BODY>"
echo "</HTML>"
