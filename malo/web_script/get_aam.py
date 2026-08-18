#!/usr/bin/python3 
# ************************************************************************
# *                                                                      *
# *   Program  get_aam.py  works in the context of http cgi-bin program  *
# *   with GET intergace. It parses the QUERY_STRING and executes        *
# *   one of the services:                                               *
# *                                                                      *
# *   value -- interpolates the values of the AAM from the operational   *
# *            foreacast.                                                *
# *                                                                      *
# *   plots -- generates the plot of the time series of the specificed   *
# *            AAM component.                                            *
# *                                                                      *
# *  ### 13-AUG-2015   get_aam.py  v2.0 (c)  L. Petrov  29-AUG-2023 ###  *
# *                                                                      *
# ************************************************************************
import sys, os, shutil, subprocess, urllib, datetime
sys.path.append('/auto')
from   urllib.parse import urlparse
from   malo_check_date   import *
from   malo_subs         import *
from   bfi_cli           import *
import datetime

hostname = os.uname()[1]

aam_fcs_exe = "/opt64/bin/aam_fcs_intrp"
aam_ser_exe = "/opt64/bin/aam_ser"

aam_first_date_file = { \
                        'atm_geosfcs':  '/earthrotation/aam/dates/aam_geosfcs_first_date.txt', \
                        'atm_geosfp':   '/earthrotation/aam/dates/aam_geosfp_first_date.txt', \
                        'atm_geosfpit': '/earthrotation/aam/dates/aam_geosfpit_first_date.txt', \
                        'atm_merra2':   '/earthrotation/aam/dates/aam_merra2_first_date.txt' \
                      }

aam_last_date_file = { \
                       'atm_geosfcs':   '/earthrotation/aam/dates/aam_geosfcs_last_date.txt', \
                       'atm_geosfp':    '/earthrotation/aam/dates/aam_geosfp_last_date.txt', \
                       'atm_geosfpit':  '/earthrotation/aam/dates/aam_geosfpit_last_date.txt', \
                       'atm_merra2':    '/earthrotation/aam/dates/aam_merra2_last_date.txt' \
                     }

aam_ser_file = { \
                 'atm_geosfcs':  '/aam/ser/aam_geos_fcs.txt', \
                 'atm_geosfp':   '/aam/ser/aam_geosfp.txt', \
                 'atm_geosfpit': '/aam/ser/aam_geosfpit.txt', \
                 'atm_merra2':   '/aam/ser/aam_merra2.txt' \
               }

aam_dir = { \
              'atm_geosfcs':  '/aam/geos_fcs', \
              'atm_geosfp':   '/aam/geosfp',   \
              'atm_geosfpit': '/aam/geosfpit', \
              'atm_merra2':   '/aam/merra2'    \
          }


#
# --- Special logic for support of slow cloud machine used as a frontend: 
# --- the command is executed at the fast backend
#
if ( "aws" in hostname ):
     plot_dir      = "/Volumes/arc/aws/ex/earthrotation/aam/plots"
else:
     plot_dir      = "/earthrotation/aam/plots"

plot_html_dir = "/earthrotation/aam/plots"

os.environ["PGPLOT_DIR"]  = "/opt64/bin"
os.environ["PGPLOT_FONT"] = "/opt64/bin/grfont.dat"
os.environ["PGPLOT_XW_MARGIN"] = "1.0"
os.environ["LD_LIBRARY_PATH"] = "/opt64/lib:/opt64/lib64"

ivrb = 1

#
# ------------------------------------------------------------------------
#

os.umask ( 2 )

print ( "404: okay" )
print ( "Content-type: text/html\n\n" )
print ( "" ) 
if ( ivrb > 1 ):
     print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
     print ( '<HTML LANG="en">' )
     print ( '<HEAD>' )
     print ( '<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' )
     print ( '</HEAD' )
     print ( '<BODY>' )
     print ( '<PRE>' )

if ( ivrb > 4 ):
     print ( "Version: ", sys.version )
     print  ( "envir: ", os.environ["QUERY_STRING"] )

key_pairs = urllib.parse.parse_qs ( os.environ["QUERY_STRING"] )


model   = key_pairs["model"][0].lower()
service = key_pairs["service"][0].lower()

if ( service == "value" ):
#
# -- value service
#
     try:
        date_req  = key_pairs["req_date"][0].lower()
     except:
        html_error ( "You have to specify requested date. No default is accepted" )
        exit ( 0 )

     if ( ivrb > 2 ):
          print ( "model = ", model )
          print ( "service = ", service )
          print ( "date_req = ", date_req )

#
# -- Check the dates
#
     date_req = check_date ( date_req, "Requested date" )
     if ( date_req == None ):
          exit ( 0 )
     with open(aam_first_date_file['atm_geosfcs']) as f:
          aam_first_date = f.readline().replace("\n","")
     f.close()

     with open(aam_last_date_file['atm_geosfcs']) as f:
          aam_last_date  = f.readline().replace("\n","")
     f.close()

     if ( ivrb > 2 ):
          print ( "aam_first_date = ", aam_first_date )
          print ( "aam_last_date  = ", aam_last_date )


     check_aam_first_date = check_date ( aam_first_date, "First AAM date" )
     if ( check_aam_first_date == None ):
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'Trap of internal control. Please try later' )
          exit ( 0 )

     check_aam_last_date = check_date ( aam_last_date, "Last AAM date" )
     if ( check_aam_first_date == None ):
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'Trap of internal control. Please try later' )
          exit ( 0 )

     if ( date_req < check_aam_first_date ):
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'The date ' + date_req + ' is too early' )
          exit ( 0 )

     if ( date_req > check_aam_last_date ):
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'The date ' + date_req + ' is too late' )
          exit ( 0 )

#
# --- generate the command for interpolation and execute it
#
     com = aam_fcs_exe            + " " + \
           aam_dir['atm_geosfcs'] + " " + \
           date_req               + " " + \
           "etab"
          
     if ( ivrb > 2 ): 
          print ( "com= " + com )
     if ( "aws" in hostname ):
          (ret, out ) = bfi_cli ( com, 60.0 )
     else:
          (ret, out ) = exe_nolog ( com )
     if ( ivrb > 2 ): 
          print ( "ret= ", ret )
          print ( "out= ", out )
     if ( ret != 0 ):
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' )
          print ( out )
          exit ( 0 )

     print ( '<PRE>' )
     for line in out:
         print ( line )

     print ( '</PRE>' )
     print ( '<P>' )
     print ( '<A HREF="/earthrotation/aam/aam_val.html"> Back </A>' )
     print ( '<HR size="1">' )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit  ( 0 )

elif ( service == "plot" ):
#
# -- "plot" service
#
     try:
        start_date  = key_pairs["start_date"][0].lower()
     except:
        html_error ( "You have to specify start date. No default is accepted" )
        exit ( 0 )

     try:
        stop_date = key_pairs["stop_date"][0].lower()
     except:
        html_error ( "You have to specify stop date. No default is accepted" )
        exit ( 0 )

     comp = key_pairs["AAM_component"][0].upper()

     if ( ivrb > 2 ):
          print ( "service    = ", service    )
          print ( "model      = ", model      )
          print ( "comp       = ", comp       )
          print ( "start_date = ", start_date )
          print ( "stop_date  = ", stop_date  )
          print ( "aam_first_date_file = ", aam_first_date_file )

#
# --- check the dates
#
     start_date = check_date ( start_date, "Start date" )
     if ( start_date == None ):
          exit ( 0 )

     stop_date  = check_date ( stop_date,  "Stop date" )
     if ( stop_date == None ):
          exit ( 0 )
     
     if ( start_date > stop_date ):
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'Start date ' + start_date + ' is later than ' + \
                  'stop_date ' + stop_date )
          exit ( 0 )

     with open(aam_first_date_file[model]) as f:
          aam_first_date = f.readline().replace("\n","")
     f.close()

     with open(aam_last_date_file[model]) as f:
          aam_last_date  = f.readline().replace("\n","")
     f.close()

     with open(aam_first_date_file[model]) as f:
          aam_first_date = f.readline().replace("\n","")
     f.close()

     with open(aam_last_date_file[model]) as f:
          aam_last_date  = f.readline().replace("\n","")
     f.close()

     check_aam_first_date = check_date ( aam_first_date, "First AAM date" )
     if ( check_aam_first_date == None ):
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'Trap of internal control. Please try later' )
          exit ( 0 )

     check_aam_last_date = check_date ( aam_last_date, "Last AAM date" )
     if ( check_aam_first_date == None ):
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'Trap of internal control. Please try later' )
          exit ( 0 )

     if ( start_date < check_aam_first_date ):
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'The start date ' + start_date + ' is too early: ' + \
                  'it should be no earlier than ' + check_aam_first_date )
          exit ( 0 )

     if ( stop_date > check_aam_last_date ):
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'The stop date ' + stop_date + ' is too late: ' + \
                  'it should be no later than ' + check_aam_last_date )
          exit ( 0 )

     time_now = str(datetime.datetime.now().strftime("%Y%m%d_%H%M%S"))

     filout  = plot_dir + "/plot_" + time_now + ".gif"
     filhtml = plot_html_dir + "/plot_" + time_now + ".gif"

#
# --- generate the command for interpolation and execute it
#
     com = aam_ser_exe    + " " + \
           aam_dir[model] + " " + \
           comp           + " " + \
           start_date     + " " + \
           stop_date      + " " + \
           filout

     if ( ivrb > 2 ): 
          print ( "com= " + com )

#
# --- ... and execute it
#
     if ( "aws" in hostname ):
          (ret, out ) = bfi_cli   ( com, 120.0 )
     else:
          (ret, out ) = exe_nolog ( com )
     if ( ivrb > 2 ): 
          print ( "ret= ", ret )
          print ( "out= ", out )
          print ( "filhtml= ", filhtml )
          print ( '<IMG     SRC="' + filhtml + '">' )
          print ( '<P>' )
     if ( ret != 0 ):
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' )
          print ( out )
          print ( '</PRE>' )
          exit ( 0 )

     print ( '<IMG     SRC="' + filhtml + '">' )
     print ( '<P>' )
     print ( '<A HREF="/earthrotation/aam/aam_plot.html"> Back </A>' )
     print ( '<HR size="1">' )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit  ( 0 )
