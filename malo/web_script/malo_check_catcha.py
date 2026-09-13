#!/usr/bin/python3 
# ************************************************************************
# *                                                                      *
# *   Catcha used my massloading.                                        *
# *                                                                      *
# *   Copyright (c) 1975-2025 United States Government as represented by *
# *   the Administrator of the National Aeronautics and Space            *
# *   Administration. All Rights Reserved.                               *
# *   License: NASA Open Source Software Agreement (NOSA).               *
# *                                                                      *
# * ## 13-AUG-2015 malo_check_catcha.py v2.3 (c) L. Petrov 2026.09.10  # *
# *                                                                      *
# ************************************************************************
import os, sys, string, stat, datetime, time, urllib, shutil
from urllib.parse import urlparse
from   malo_check_date   import *
from   malo_check_stafil import *
from   malo_subs         import *
from   url_sanitizer     import *

approved_urls = [ \
                  "http://massloading.sciencecloud.nasa.gov",  \
                  "https://massloading.sciencecloud.nasa.gov", \
                  "http://alt.massloading.net", \
                  "http://massloading.net", \
                  "https://massloading.net" \
                ]
#
# ------------------------------------------------------------------------
#
os.umask ( 2 )

config = config_class()

if ( "QUERY_STRING" in os.environ.keys() ):
     key_pairs = urllib.parse.parse_qs ( os.environ["QUERY_STRING"] )
else:
     print ( "404: okay" )
     print ( "Content-type: text/html\n\n" )
     print ( "" ) 
     print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
     print ( '<HTML LANG="en">' )
     print ( '<HEAD>' )
     print ( '<PRE>'  )
     print ( 'Internal server configuration error E1 in malo_check_catcha.py' )
     print ( 'Please reort to the mainter' ) 
     print ( '</PRE>'  )
     print ( '</HEAD>' )
     print ( '</HTML>' )
     exit ( 0 )

# print ( "404: okay" ); print ( "Content-type: text/html\n\n" ) ; print ( os.environ["QUERY_STRING"] ) ; exit ( 0 ) # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

date_req    = key_pairs["date_req"][0]
ans         = key_pairs["answer"][0]
mode        = key_pairs["mode"][0]
frame       = key_pairs["frame"][0]
model       = key_pairs["model"][0]
service     = key_pairs["service"][0]
n_sta       = key_pairs["n_sta"][0]
start_date  = key_pairs["start_date"][0]
stop_date   = key_pairs["stop_date"][0]
remote_addr = key_pairs["remote_addr"][0]
malo_http   = key_pairs["malo_http"][0]
silent      = key_pairs["silent"][0]
ip_unlim    = key_pairs["ip_unlim"][0]
email       = key_pairs["email"][0]


#
# --- Sanitize arguments
#
if ( not url_sanitizer ( date_req, "date" ) ):
     print ( "" ) 
     print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
     print ( '<HTML LANG="en">' )
     print ( '<HEAD>' )
     print ( '<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1"' )
     print ( '</HEAD' )
     print ( '<BODY>' )
     print ( '</PRE>' )
     print ( '<FONT COLOR="A04030"><B> Trap of internal control: wrong date argument </B></FONT> %s' % os.environ["QUERY_STRING"]  )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit  ( 1 )

if ( not url_sanitizer ( ans, "email" ) ):
     print ( "" ) 
     print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
     print ( '<HTML LANG="en">' )
     print ( '<HEAD>' )
     print ( '<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1"' )
     print ( '</HEAD' )
     print ( '<BODY>' )
     print ( '</PRE>' )
     print ( '<FONT COLOR="A04030"><B> Trap of internal control: wrong ans argument </B></FONT> %s' % os.environ["QUERY_STRING"]  )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit  ( 1 )

if ( not url_sanitizer ( mode, "email" ) ):
     print ( "" ) 
     print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
     print ( '<HTML LANG="en">' )
     print ( '<HEAD>' )
     print ( '<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1"' )
     print ( '</HEAD' )
     print ( '<BODY>' )
     print ( '</PRE>' )
     print ( '<FONT COLOR="A04030"><B> Trap of internal control: wrong mode argument </B></FONT> %s' % os.environ["QUERY_STRING"]  )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit  ( 1 )

if ( not url_sanitizer ( frame, "email" ) ):
     print ( "" ) 
     print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
     print ( '<HTML LANG="en">' )
     print ( '<HEAD>' )
     print ( '<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1"' )
     print ( '</HEAD' )
     print ( '<BODY>' )
     print ( '</PRE>' )
     print ( '<FONT COLOR="A04030"><B> Trap of internal control: wrong frame argument </B></FONT> %s' % os.environ["QUERY_STRING"]  )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit  ( 1 )

if ( not url_sanitizer ( model, "email" ) ):
     print ( "" ) 
     print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
     print ( '<HTML LANG="en">' )
     print ( '<HEAD>' )
     print ( '<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1"' )
     print ( '</HEAD' )
     print ( '<BODY>' )
     print ( '</PRE>' )
     print ( '<FONT COLOR="A04030"><B> Trap of internal control: wrong model argument </B></FONT> %s' % os.environ["QUERY_STRING"]  )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit  ( 1 )

if ( not url_sanitizer ( service, "email" ) ):
     print ( "" ) 
     print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
     print ( '<HTML LANG="en">' )
     print ( '<HEAD>' )
     print ( '<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1"' )
     print ( '</HEAD' )
     print ( '<BODY>' )
     print ( '</PRE>' )
     print ( '<FONT COLOR="A04030"><B> Trap of internal control: wrong service argument </B></FONT> %s' % os.environ["QUERY_STRING"]  )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit  ( 1 )

if ( not url_sanitizer ( n_sta, "dig" ) ):
     print ( "" ) 
     print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
     print ( '<HTML LANG="en">' )
     print ( '<HEAD>' )
     print ( '<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1"' )
     print ( '</HEAD' )
     print ( '<BODY>' )
     print ( '</PRE>' )
     print ( '<FONT COLOR="A04030"><B> Trap of internal control: wrong n_sta argument </B></FONT> %s' % os.environ["QUERY_STRING"]  )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit  ( 1 )

if ( not url_sanitizer ( start_date, "date" ) ):
     print ( "" ) 
     print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
     print ( '<HTML LANG="en">' )
     print ( '<HEAD>' )
     print ( '<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1"' )
     print ( '</HEAD' )
     print ( '<BODY>' )
     print ( '</PRE>' )
     print ( '<FONT COLOR="A04030"><B> Trap of internal control: wrong stop_date argument </B></FONT> %s' % os.environ["QUERY_STRING"]  )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit  ( 1 )

if ( not url_sanitizer ( stop_date, "date" ) ):
     print ( "" ) 
     print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
     print ( '<HTML LANG="en">' )
     print ( '<HEAD>' )
     print ( '<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1"' )
     print ( '</HEAD' )
     print ( '<BODY>' )
     print ( '</PRE>' )
     print ( '<FONT COLOR="A04030"><B> Trap of internal control: wrong stop_date argument </B></FONT> %s' % os.environ["QUERY_STRING"]  )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit  ( 1 )

if ( not url_sanitizer ( remote_addr, "dig" ) ):
     print ( "" ) 
     print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
     print ( '<HTML LANG="en">' )
     print ( '<HEAD>' )
     print ( '<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1"' )
     print ( '</HEAD' )
     print ( '<BODY>' )
     print ( '</PRE>' )
     print ( '<FONT COLOR="A04030"><B> Trap of internal control: wrong remote_addr argument </B></FONT> %s' % os.environ["QUERY_STRING"]  )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit  ( 1 )

if ( not url_sanitizer ( malo_http, "url" ) ):
     print ( "" ) 
     print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
     print ( '<HTML LANG="en">' )
     print ( '<HEAD>' )
     print ( '<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1"' )
     print ( '</HEAD' )
     print ( '<BODY>' )
     print ( '</PRE>' )
     print ( '<FONT COLOR="A04030"><B> Trap of internal control: wrong arg </B></FONT> %s' % os.environ["QUERY_STRING"]  )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit  ( 1 )


if ( not url_sanitizer ( silent, "email" ) ):
     print ( "" ) 
     print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
     print ( '<HTML LANG="en">' )
     print ( '<HEAD>' )
     print ( '<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1"' )
     print ( '</HEAD' )
     print ( '<BODY>' )
     print ( '</PRE>' )
     print ( '<FONT COLOR="A04030"><B> Trap of internal control: wrong silent </B></FONT> %s' % os.environ["QUERY_STRING"]  )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit  ( 1 )

if ( not url_sanitizer ( ip_unlim, "email" ) ):
     print ( "" ) 
     print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
     print ( '<HTML LANG="en">' )
     print ( '<HEAD>' )
     print ( '<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1"' )
     print ( '</HEAD' )
     print ( '<BODY>' )
     print ( '</PRE>' )
     print ( '<FONT COLOR="A04030"><B> Trap of internal control: wrong ip_unlim </B></FONT> %s' % os.environ["QUERY_STRING"]  )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit  ( 1 )

if ( not url_sanitizer ( email, "email" ) ):
     print ( "" ) 
     print ( 'Trap of internal control: wrong email %s' % os.environ["QUERY_STRING"]  )
     exit  ( 1 )

#
# --- End of string sanitization
#

status_file_name = config.ondemand_dir + "/req/" + date_req + "/status.txt" 
index_file_name  = config.ondemand_dir + "/req/" + date_req + "/index.html" 
url_str = 'https://' + os.environ["SERVER_NAME"] + '/ondemand/req/' + date_req

#
# --- Check whether the url string that we want to process is acceptable
# --- and refer to the url in the approved list
#
fl_approved = False
for url in approved_urls:
    if ( url_str.find(url) >= 0 ):
         fl_approved = True

if ( not fl_approved ):
     print ( "404: okay" )
     print ( "Content-type: text/html\n\n" )
     print ( "" ) 
     print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
     print ( '<HTML LANG="en">' )
     print ( '<HEAD>' )
     print ( '<PRE>'  )
     print ( 'url_str= %s' % url_str ) 
     print ( 'Internal server configuration error E2 in malo_check_catcha.py' )
     print ( 'Please reort to the mainter' ) 
     print ( '</PRE>'  )
     print ( '</HEAD>' )
     print ( '</HTML>' )
     exit ( 0 )

#
# --- Check whether malo_http string that we want to process is acceptable
# --- and refer to the url in the approved list
#
fl_approved = False
for url in approved_urls:
    if ( malo_http.find(url) >= 0 ):
         fl_approved = True

if ( not fl_approved ):
     print ( "404: okay" )
     print ( "Content-type: text/html\n\n" )
     print ( "" ) 
     print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
     print ( '<HTML LANG="en">' )
     print ( '<HEAD>' )
     print ( '<PRE>'  )
     print ( 'Internal server configuration error E3 in malo_check_catcha.py' )
     print ( 'Please reort to the mainter' ) 
     print ( '</PRE>'  )
     print ( '</HEAD>' )
     print ( '</HTML>' )
     exit ( 0 )

if ( silent == "no" ):
     print ( "404: okay" )
     print ( "Content-type: text/html\n\n" )
     print ( "" ) 
     print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
     print ( '<HTML LANG="en">' )
     print ( '<HEAD>' )
     print ( '<META http-equiv="Content-Type" content="text/html; Charset=iso-8859-1">' )

     if ( not os.path.isfile( status_file_name ) ):
          print ( '<META HTTP-EQUIV="refresh" CONTENT="0;' + \
                   "URL='" + malo_http + "#ondemand'" + '">' )
          print ( '</HEAD' )
          print ( '<BODY>' )
          exit ( 0 )

if ( ip_unlim == "no" ):
     ans_file = config.catcha_dir + "/" + date_req + ".txt"
     with open( ans_file ) as f:
          buf = f.readlines()
     correct_ans = buf[0].replace("\n",'').replace("\r",'')  
     
     if ( ans == correct_ans ):
          fl_ans = 0
     else:
          if ( ans in config.synonyms ):
               if ( config.synonyms[ans] == correct_ans ):
                    fl_ans = 0
               else:
                    fl_ans = 1
          else:
               fl_ans = 1
else:
      fl_ans = 0

if ( fl_ans  == 0 ):

     check_lock ( config.ondemand_lock, config.lock_min_wait, config.lock_max_wait )
     f= open ( config.ondemand_queue, "a" )
     f.write ( "%s %3s_%-16s %-12s %2s %5s %s %s %-15s %-38s %s %s\n" % \
               ( date_req, service, model, mode, frame, n_sta, start_date, stop_date, \
                 remote_addr, email, "U", "        " ) )
     f.close()
     os.remove ( config.ondemand_lock )

     if ( config.ivrb > 1 ): print ( 'index_file_name ' + index_file_name + "<P>" )

     f= open ( index_file_name, "w" )
     f.write ( ' <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">\n' )
     f.write ( ' <HTML><HEAD> <META HTTP-EQUIV="Content-Type" content="text/html; CHARSET=iso-8859-1">\n' )
     f.write ( ' <META HTTP-EQUIV="refresh" CONTENT="5">\n' )
     f.write ( ' </HEAD><BODY>\n' )
     if ( mode == "series" ):
          f.write ( date_req + ' &nbsp;&nbsp; Request for mass loading time series computation has been received <BR>\n' )
     else:
          f.write ( date_req + ' &nbsp;&nbsp; Request for mass loading computation has been received <BR>\n' )
     f.write ( 'Waiting for putting the request into the queue. Status is updated every 5 seconds.<BR>\n' )
     f.write ( ' </BODY></HTML>\n' )
     f.close()
     os.chmod ( index_file_name, \
                stat.S_IREAD + stat.S_IWRITE + stat.S_IEXEC + \
                stat.S_IRGRP + stat.S_IWGRP  + stat.S_IXGRP + \
                stat.S_IROTH + stat.S_IXOTH  )
     
     if ( silent == "no" ): print ( '</PRE>' )
     if ( config.ivrb > 1 ): print ( 'index_file_name ' + index_file_name + "<P>" )
     if ( silent == "no" ):
          print ( "Your request to compute mass loading displacement has been received.<P>" )
          print ( "Please, write down ID of your request: <B> " + date_req + "</B><BR>" )
          print ( "You can check the status of your request at " + \
                  '<A HREF="/ondemand/req/' + date_req + '/">' + \
                 '/ondemand/req/' + date_req + '/</A>.<P>' )
     
          if ( email != "n/a" ):
               print ( "Email will be sent to you when request mass loading series " + \
                       "will be ready for downloading." )
     
          print ( '<P>' )
          print ( '<HR size="1">'   )
          print ( 'Back to <A HREF="' + malo_http + '">' + malo_http + '</A>' )
          print ( '</BODY>' )
          print ( '</HTML>' )
     else:
          print ( 'If your request is not redirected automatically, follow this <A HREF="' + \
                   url_str + '">link</A>' )
          exit ( 0 )

else:
     request_dir = config.req_dir + "/" + date_req 
     if ( os.path.isdir(request_dir) ): shutil.rmtree ( request_dir )

     time.sleep ( 2.0 )
     print ( "<B>WRONG   answer</B>. If in doubt, ask an advisory opinion of a 3+ years-old child. :-)" )
     print ( '<P>' )
     print ( '<HR size="1">'   )
     print ( 'Back to <A HREF="' + malo_http + '">' + malo_http + '</A>' )

print ( '</HEAD' )
print ( '<BODY>' )
