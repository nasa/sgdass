#!/usr/bin/env python3 
# ************************************************************************
# *                                                                      *
# *   Program  eop_online.py  works in the context of http cgi-bin       *
# *   program  with GET interface. It parses the QUERY_STRING and        *
# *   executes the services.                                             *
# *                                                                      *
# * ### 06-APR-2016 eop_online.py  v1.2 (c)  L. Petrov  23-OCT-2023 ###  *
# *                                                                      *
# ************************************************************************
import sys, os, shutil, subprocess, urllib, datetime
import datetime
from   urllib.parse import urlparse
from   malo_check_date   import *
from   malo_subs         import *
from   set_env           import *

ners_eopser = "/opt64/bin/ners_eopser"
ners_config = "/earthrotation/ners/cgi_temp/ners.config"
eop_start_date_file = "/earthrotation/ners/ners_start_date.txt"
eop_stop_date_file  = "/earthrotation/ners/ners_stop_date.txt"
ners__min_tim_step =  600.0

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
print ( "Content-type: text/html\n\n" )
print ( "" ) 
if ( ivrb > 1 ):
     print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
     print ( '<HTML LANG="ru">' )
     print ( '<HEAD>' )
     print ( '<META http-equiv="Content-Type" content="text/html; charset=koi8-r">' )
     print ( '</HEAD' )
     print ( '<BODY>' )
     print ( '<PRE>' )

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

try:
   eop_group  = key_pairs["eop_group"][0].lower()
except:
   eop_group = None

try:
    start_date = key_pairs["start_date"][0].lower()
except:
    start_date = None

try:
    stop_date  = key_pairs["stop_date"][0].lower()
except:
    stop_date  = None

try:
    time_step_str  = key_pairs["time_step"][0].lower()
except:
    time_step_str  = None

try:
    service    = key_pairs["service"][0].lower()
except:
    service    = None


#

if ( service == "series" ):
#
# -- series service
#

     if ( ivrb > 2 ):
          print ( "start_date = ", start_date )
          print ( "stop_date  = ", stop_date )
          print ( "time_step  = ", time_step_str )
          print ( "eop_group  = ", eop_group )
          print ( "service    = ", service )

#
# -- Check the dates
#
     if ( start_date == None or start_date == "now" ):
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'The start_date ' + start_date + ' is not defined' )
          exit ( 0 )
     start_date = check_date ( start_date, "Start date" )

     if ( stop_date == None or stop_date == "now" ):
          stop_date = datetime.datetime.utcnow().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:27]
     stop_date = check_date ( stop_date, "Stop date" )

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

     if ( start_date < eop_start_date ):
          print ( '</PRE>' )
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'The start date ' + start_date + ' is too early. ' + 
                  'The earilist epoch is ', eop_start_date  )
          exit ( 0 )

     if ( stop_date < eop_start_date ):
          print ( '</PRE>' )
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'The stop date ' + stop_date + ' is too early. ' + 
                  'The earilist epoch is ', eop_start_date  )
          exit ( 0 )

     if ( start_date > eop_stop_date):
          print ( '</PRE>' )
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'The start date ' + start_date + ' is too late. The latest date is ', eop_stop_date )
          exit ( 0 )

     if ( stop_date > eop_stop_date):
          print ( '</PRE>' )
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'The stop date ' + stop_date + ' is too late. The latest date is ', eop_stop_date )
          exit ( 0 )

     if ( time_step_str == None ):
          print ( '</PRE>' )
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'The time step was not defined' )
          exit ( 0 )
          
     if ( not ( all( (temp_char in dig_and_dot_set) for temp_char in time_step_str ) ) ):
          print ( '</PRE>' )
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'Wrong time_step %s' % time_step_str )
          exit ( 0 )

     try:               
          time_step = float(time_step_str)

     except: 
          print ( '</PRE>' )
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'Wrong format for time step ' + time_step_str + \
                  '. Time step should be a float number' )
          exit ( 0 )

     if ( time_step < ners__min_tim_step ):
          print ( '</PRE>' )
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'Time step is too small. Minimum time step %f' % ners__min_tim_step )
          exit ( 0 )

#
# --- generate the command for computation of the EOP series
#
     com = ners_eopser           + " " + \
           "-c " + ners_config   + " " + \
           "-p " + eop_group     + " " + \
           "-b " + start_date    + " " + \
           "-e " + stop_date     + " " + \
           "-s " + time_step_str 

     if ( ivrb > 2 ): 
          print ( "com= " + com )
     (ret, out ) = exe_nolog ( com )

     if ( ivrb > 2 ): 
          print ( "ret= ", ret )
          print ( "out= ", " ".join(out) )

     if ( ret != 0 ):
          print ( '</PRE>' )
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' )
          print ( " ".join(out) )
          exit ( 0 )

     print ( '<PRE>' )
     for line in out:
         print ( line )

     print ( '</PRE>' )
     print ( '<P>' )
     print ( '<A HREF="/earthrotation/eop_series.html"> Back to the Web interace of the Network Earth Rotation Service</A>' )
     print ( '<HR size="1">' )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit  ( 0 )

