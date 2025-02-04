#!/usr/bin/python3 
#
# ************************************************************************
# *                                                                      *
# *   Program malo_select_file.py processes web form for selecting       *
# *   loading file.                                                      *
# *                                                                      *
# * ### 08-APR-2015  malo_load_int.py v2.0 (c) L. Petrov 31-AUG-2023 ### *
# *                                                                      *
# ************************************************************************
import os, sys, string, stat, datetime, shutil, urllib, subprocess
sys.path.append('/auto')
from   urllib.parse import urlparse
from   malo_check_date   import *
from   malo_check_stafil import *
from   malo_subs         import *
from   malo_set_catcha   import *
from   bfi_cli           import *

bin_pref   = "/opt64/bin"
lib_pref   = "/opt64/lib"
lib64_pref = "/opt64/lib"

os.environ['LD_LIBRARY_PATH'] = lib_pref + ":" + lib64_pref

os.putenv ( "PGPLOT_DIR",  "/opt64/bin" )
os.putenv ( "PGPLOT_FONT", "/opt64/bin/grfont.dat" )
os.putenv ( "PGPLOT_XW_MARGIN", "1.0" )

dir_date      = "/massloading"
dir_int       = "/imls_ondemand/plots"
aws_dir_int   = "/ex/" + dir_int
dir_imls      = "/imls"
aws_host_name = "massloading.smce.nasa.gov"

ivrb = 1

if ( len(sys.argv) <= 1  ):
     if ( "SERVER_NAME" in os.environ ):
          if ( os.environ["SERVER_NAME"] == aws_host_name ):
               if ( ivrb > 4 ):
                    print ( "404: okay" )
                    print ( "Content-type: text/html\n\n" )
                    print ( "" ) 
                    print ( "sys.argv = ", sys.argv )
                    exit ( 0 )
          
               com = "/astrogeo/web_exec/malo_load_int.py" + " " + \
                     os.environ["SERVER_NAME"] + " " + \
                     os.environ["SERVER_PORT"] + " " + \
                     os.environ["QUERY_STRING"]
          
               (ret,out) = bfi_cli ( com, timeout=120.0 )
               for line in out:
                   print ( line )
               exit ( 0 )
else:
     os.environ["SERVER_NAME"]  = sys.argv[1]
     os.environ["SERVER_PORT"]  = sys.argv[2]
     os.environ["QUERY_STRING"] = sys.argv[3]

html_pref  = "http://" + os.environ["SERVER_NAME"] + "/ondemand/plots"

def exe ( command ):
#"""
#    Spawn a supborcess, execute command in the context of the suborpess,
#    wait for its completion and return completion code and results.
#"""
    words = command.split()
    (ret, out) = subprocess.getstatusoutput ( "ulimit -s 8000000; " + command )
    return ( ret, out.split ( "\n" ) )
#
# ------------------------------------------------------------------------
#
os.umask ( 2 )

config = config_class()
print ( "404: okay" )
print ( "Content-type: text/html\n\n" )
print ( "" ) 
print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
print ( '<HTML LANG="en">' )
print ( '<HEAD>' )
print ( '<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' )
print ( '</HEAD' )
print ( '<BODY>' )

if ( ivrb > 4 ):
     print  ( "Server:  ", os.environ["SERVER_NAME"] )
     print  ( "Version: ", sys.version )
     print  ( "envir: ", os.environ["QUERY_STRING"] )
     print  ( "<PRE>" )
     print  ( "envir: ", os.environ )
     print  ( "<PRE>" )

key_pairs = urllib.parse.parse_qs ( os.environ["QUERY_STRING"] )

model   = key_pairs["model"][0].lower()
format  = key_pairs["format"][0].lower()
product = key_pairs["product"][0].lower()
smoothing_type = key_pairs["smoothing_type"][0]
try:
   smoothing_int  = key_pairs["smoothing_int"][0]
except:
   smoothing_int  = "0.001"
if ( smoothing_int < "0.001" ): smoothing_int = "0.001"
submit  = key_pairs["submit"][0].lower()
if ( "start_date" in key_pairs.keys() ):
     start_date = key_pairs["start_date"][0]
else:
     start_date = ""

if ( "stop_date" in key_pairs.keys() ):
     stop_date = key_pairs["stop_date"][0]
else:
     stop_date = ""


if ( ivrb > 3 ):
     print ( "<P>" )
     print ( "<br>model:      ", model )
     print ( "<br>format:     ", format )
     print ( "<br>product:    ", product )
     print ( "<br>start_date: ", start_date )
     print ( "<br>stop_date:  ", stop_date )
     print ( "<br>smoothing_type: ", smoothing_type )
     print ( "<br>smoothing_int:  ", smoothing_int  )
     print ( "<br>submit:     ", submit )
     print ( "<P>&nbsp;<P>" )


res= check_date ( start_date, "start_date" )

if ( res == None ):
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit ( 0 )

res= check_date ( stop_date,  "stop_date" )
if ( res == None ):
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit ( 0 )


if ( stop_date < start_date ):
     print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
             ' the stop date is prior the start date' )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit ( 0 )

if ( smoothing_type != "RAW" and smoothing_int == "" ):
     print ( '<BR>Please enter smooting interval in days</BR>' )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit ( 0 )

load_type  = model[0:model.find("_")]
load_model = model[model.find("_")+1:]

if ( ivrb > 3 ): print ( "<br>load_type:  ", load_type )
if ( ivrb > 3 ): print ( "<br>load_model: ", load_model )
if ( ivrb > 3 ): print ( "<br>model: ", model )

if (   model == "atm_geosfpit" ):
       file_first_date = dir_date + "/atm/dates/geosfpit_loading_first_date.txt"
       file_last_date  = dir_date + "/atm/dates/geosfpit_loading_last_date.txt"
       title = "Integrated atm GEOSFPIT loading in mm"
elif ( model == "atm_geosfp_507" ):
          file_first_date = dir_date + "/atm/dates/geosfp_507_loading_first_date.txt"
          file_last_date  = dir_date + "/atm/dates/geosfp_507_loading_last_date.txt"
          title = "Integrated atm GEOSFP-507 loading in mm"
elif ( model == "atm_geosfp_511" ):
          file_first_date = dir_date + "/atm/dates/geosfp_511_loading_first_date.txt"
          file_last_date  = dir_date + "/atm/dates/geosfp_511_loading_last_date.txt"
          title = "Integrated atm GEOSFP-511 loading in mm"
elif ( model == "atm_geosfp" ):
          file_first_date = dir_date + "/atm/dates/geosfp_loading_first_date.txt"
          file_last_date  = dir_date + "/atm/dates/geosfp_loading_last_date.txt"
          title = "Integrated atm GEOSFP loading in mm"
elif ( model == "atm_merra2" ):
          file_first_date = dir_date + "/atm/dates/merra2_loading_first_date.txt"
          file_last_date  = dir_date + "/atm/dates/merra2_loading_last_date.txt"
          title = "Integrated atm MERRA2 loading in mm"
if ( model == "lws_geosfpit" ):
          file_first_date = dir_date + "/lws/dates/geosfpit_loading_first_date.txt"
          file_last_date  = dir_date + "/lws/dates/geosfpit_loading_last_date.txt"
          title = "Integrated lws GEOSFPIT loading in mm"
elif ( model == "lws_merra2" ):
          file_first_date = dir_date + "/lws/dates/merra2_loading_first_date.txt"
          file_last_date  = dir_date + "/lws/dates/merra2_loading_last_date.txt"
          title = "Integrated lws MERRA2 loading in mm"
elif ( model == "lws_noah025" ):
          file_first_date = dir_date + "/lws/dates/noah025_loading_first_date.txt"
          file_last_date  = dir_date + "/lws/dates/noah025_loading_last_date.txt"
          title = "Integrated lws NOAH025 loading in mm"
elif ( model == "nto_omct05" ):
          file_first_date = dir_date + "/nto/dates/omct05_loading_first_date.txt"
          file_last_date  = dir_date + "/nto/dates/omct05_loading_lasst_date.txt"
          title = "Integrated nto OMCT05 loading in mm"
elif ( model == "nto_mpiom06" ):
          file_first_date = dir_date + "/nto/dates/mpiom06_loading_first_date.txt"
          file_last_date  = dir_date + "/nto/dates/mpiom06_loading_lasst_date.txt"
          title = "Integrated nto MPIOM06 loading in mm"

if ( format == "ascii_file" ):
     filout  = dir_int   + "/" + model + "_int_" + str(os.getpid()) + ".txt"
     filhtml = html_pref + "/" + model + "_int_" + str(os.getpid()) + ".txt"
     com = bin_pref + "/loading_int_time_series " + dir_imls + "/load_int/" + \
           load_type + "/" + load_model + "/" + " " + \
           start_date + " " + stop_date + " " + smoothing_type + " " + \
           smoothing_int + " " + filout + " " + title
     if ( ivrb > 2 ): print ( " com = " + com )

     print ( "<BR>Compute the time series, please wait<BR>" )
     (ret, buf ) = exe_nolog ( com )
     if ( ret != 0 ):
          print ( "<BR>Error in computation of time series" )
          print ( buf )
          print ( '</PRE>' )
     else:
          print ( '</PRE>' )
          print ( 'Output file is <A HREF="' + filhtml + '">here</A><P>' )

     if ( len(sys.argv) >= 1  ):
          com = "cp " + filout + " " + filout.replace(dir_int,aws_dir_int)
          (ret, err ) = exe_nolog ( com )
          if ( ret != 0 ):
               print ( '<PRE>' )
               print ( '<BR>Error in copying results to the front-end with command</BR>' )
               print ( '<BR>' + com + '</BR>' )
               print ( '</PRE>' )
     exit  ( 0 )        

elif ( format == "plot" ):
     filout      = dir_int     + "/" + model + "_int_" + str(os.getpid()) + ".gif"
     filhtml_lis = [ html_pref + "/" + model + "_int_" + str(os.getpid()) + "_total.gif",  \
                     html_pref + "/" + model + "_int_" + str(os.getpid()) + "_land.gif",   \
                     html_pref + "/" + model + "_int_" + str(os.getpid()) + "_ocean.gif",  \
                     html_pref + "/" + model + "_int_" + str(os.getpid()) + "_ocean66.gif" \
                   ]
     com = bin_pref + "/loading_int_time_series " + dir_imls + "/load_int/" + \
           load_type + "/" + load_model + "/" + " " + \
           start_date + " " + stop_date + " " + smoothing_type + " " + \
           smoothing_int + " " + filout + " " + '"' + title + '"'
     if ( ivrb > 1 ): print ( " com = " + com )

     print ( "<BR>Compute the time series and generate plots, please wait...<BR>" )
     (ret, buf ) = exe_nolog ( com )
     if ( ret != 0 ):
          print ( "<BR>Error in computation of time series" )
          print ( buf )
          print ( '</PRE>' )
     else:
          print ( '</PRE>' )
          print ( '<P>' )
          print ( '<B>COMPLETED.</B> Results are ready:' )
          print ( '<P>' )
          print ( 'Plot of the <A HREF="' + filhtml_lis[0] + '">total intergrated loading</A><P>' )
          print ( 'Plot of the <A HREF="' + filhtml_lis[1] + '">intergrated loading over the land</A><P>' )
          print ( 'Plot of the <A HREF="' + filhtml_lis[2] + '">intergrated loading over the ocean</A><P>' )
          print ( 'Plot of the <A HREF="' + filhtml_lis[3] + '">intergrated loading over the ocean within 66&deg;</A><P>' )
     if ( len(sys.argv) >= 1  ):
          for filhtml in filhtml_lis:
              filin  = filhtml.replace(html_pref,dir_int)
              filout = filhtml.replace(html_pref,aws_dir_int)
              com = "cp " + filin + " " + filout
              (ret, err ) = exe_nolog ( com )
              if ( ret != 0 ):
                   print ( '<PRE>' )
                   print ( '<BR>Error in copying results to the front-end with command</BR>' )
                   print ( '<BR>' + com + '</BR>' )
                   print ( '</PRE>' )

print ( '</BODY>' )
print ( '</HTML>' )
exit  ( 0 )
