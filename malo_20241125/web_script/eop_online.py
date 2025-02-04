#!/usr/bin/python3 
# ************************************************************************
# *                                                                      *
# *   Program  eop_online.py  works in the context of http cgi-bin       *
# *   program  with GET interface. It parses the QUERY_STRING and        *
# *   executes the services.                                             *
# *                                                                      *
# * ### 06-APR-2016 eop_online.py  v1.5 (c)  L. Petrov  23-OCT-2023 ###  *
# *                                                                      *
# ************************************************************************
import sys, os, shutil, subprocess, urllib, datetime
import datetime
from   urllib.parse import urlparse
from   malo_check_date   import *
from   malo_subs         import *
from   set_env           import *

eop_fcs_exe = "/opt64/bin/show_eop_fcs"
eop_fcs_bin = "/earthrotation/ners/eop.ners"
eop_start_date_file = "/earthrotation/ners/ners_start_date.txt"
eop_stop_date_file  = "/earthrotation/ners/ners_stop_date.txt"

param_list = [ \
               "utcmtai", \
               "ut1mtai", \
               "xpol", \
               "ypol", \
               "dpsi", \
               "deps", \
               "e1", \
               "e2", \
               "e3", \
               "lod", \
               "ut1rat", \
               "xpolr", \
               "ypolr", \
               "mat", \
               "matr", \
               "matrr", \
               "now_html" \
             ]

dig_and_dot_set = set('0123456789.')
date_set = set('0123456789.Tnow')
allowed_charset = set('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_@.;/:+-=?&%')

ivrb = 1

#
# ------------------------------------------------------------------------
#

os.umask ( 2 )
set_env()

print ( "404: okay" )
if ( ivrb > 1 ):
     print ( "Content-type: text/html\n\n" )
     print ( "" ) 
     print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
     print ( '<HTML LANG="en">' )
     print ( '<HEAD>' )
     print ( '<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1"' )
     print ( '</HEAD' )
     print ( '<BODY>' )

if ( ivrb > 4 ):
     print ( "Version: ", sys.version )
     print  ( "envir: ", os.environ["QUERY_STRING"] )

if ( not ( all( (temp_char in allowed_charset) for temp_char in os.environ["QUERY_STRING"]  ) ) ):
     if ( ivrb <= 1 ):
          print ( "Content-type: text/html\n\n" )
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
     exit  ( 0 )

key_pairs = urllib.parse.parse_qs ( os.environ["QUERY_STRING"] )

param = key_pairs["param"][0].lower()
try:
   content = key_pairs["content"][0].lower()
except:
   content = "text"

#

if ( ivrb < 2 ):
     print ( "Content-type: text/html\n" )

if ( content == "html" ):
     print ( "" ) 
     print ( "" ) 
     print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
     print ( '<HTML LANG="en">' )
     print ( '<HEAD>' )
     print ( '<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1"' )
     print ( '</HEAD' )
     print ( '<BODY>' )
     print ( '<PRE>' )

if ( content == "html" or content == "text" ):
#
# -- value service
#
     try:
        date_req  = key_pairs["req_date"][0].lower()
     except:
        date_req = datetime.datetime.utcnow().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:27]

     if ( not ( all( (temp_char in allowed_charset) for temp_char in date_req ) ) ):
          if ( ivrb <= 1 ):
               print ( "Content-type: text/html\n\n" )
               print ( "" ) 
               print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
               print ( '<HTML LANG="en">' )
               print ( '<HEAD>' )
               print ( '<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1"' )
               print ( '</HEAD' )
               print ( '<BODY>' )
          print ( '</PRE>' )
          print ( '<FONT COLOR="A04030"><B> Trap of internal control: wrong argument date_req </B></FONT> %s' % date_req )
          print ( '</BODY>' )
          print ( '</HTML>' )
          exit  ( 0 )

     if ( ivrb > 2 ):
          print ( "date_req = ", date_req )

#
# -- Check the dates
#
     if ( date_req == None or date_req == "now" ):
          date_req = datetime.datetime.utcnow().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:27]
     date_req = check_date ( date_req, "Requested date" )

     with open(eop_start_date_file) as f:
          eop_start_date = f.readline().replace("\n","").replace("-","_")
     f.close()

     with open(eop_stop_date_file) as f:
          eop_stop_date = f.readline().replace("\n","").replace("-","_")
     f.close()

     if ( eop_start_date == None ):
          print ( '</PRE>' )
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'Trap of internal control: eop_start_date is lost. Please try later' )
          exit ( 0 )

     if ( eop_stop_date == None ):
          print ( '</PRE>' )
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'Trap of internal control: eop_stop_date is lost. Please try later' )
          exit ( 0 )

     if ( date_req < eop_start_date ):
          print ( '</PRE>' )
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'The date ' + date_req + ' is too early' )
          exit ( 0 )

     if ( date_req > eop_stop_date):
          print ( 'eop_stop_date= ', eop_stop_date ) # %%%%%%%%%%%
          print ( '</PRE>' )
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'The date ' + date_req + ' is too late' )
          print ( 'The last supported date is ' + eop_stop_date )
          exit ( 0 )

#
# --- generate the command for computation of the EOP
#
     if ( not ( param in param_list ) ):
          print ( '</PRE>' )
          print ( '<FONT COLOR="A04030"><B> Trap of internal control: wrong param </B></FONT> %s' % param )
          exit  ( 0 )

     com = eop_fcs_exe + " " + eop_fcs_bin + " " + date_req + " " + param
          
     if ( ivrb > 2 ): 
          print ( "com= " + com )
     (ret, out ) = exe_nolog ( com )
     if ( ivrb > 2 ): 
          print ( "ret= ", ret )
          print ( "<P>" )
     if ( ret != 0 ):
          print ( '</PRE>' )
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' )
          print ( out )
          exit ( 0 )

     for line in out:
         print ( line )
     print ( '</PRE>' )

if ( content == "html" ):
     print ( '</PRE>' )
     print ( '<P>' )
     print ( '<A HREF="/earthrotation/eop_online.html"> Back</A to the Web interace of the Network Earth Rotation Service>' )
     print ( '<HR size="1">' )
     print ( '</BODY>' )
     print ( '</HTML>' )

exit  ( 0 )

