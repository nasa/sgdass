#!/usr/bin/python3 
#
# ************************************************************************
# *                                                                      *
# *   Program malo_select_file.py processes web form for selecting       *
# *   loading file.                                                      *
# *                                                                      *
# * ## 08-APR-2015 malo_select_file.py v2.2 (c) L. Petrov 15-NOV-2022 ## *
# *                                                                      *
# ************************************************************************
#
import os, sys, string, stat, datetime, shutil, urllib, subprocess
sys.path.append('/auto')
from   urllib.parse import urlparse
from   malo_check_date   import *
from   malo_check_stafil import *
from   malo_subs         import *
from   malo_set_catcha   import *
from   set_env           import *
from   bfi_cli           import *

dir_date  = "/massloading"
dir_loa   = "/imls"
dir_spd   = "/spd"
aws_host_name = "massloading.smce.nasa.gov"

if ( len(sys.argv) > 1 ):
     os.environ["SERVER_NAME"]  = sys.argv[1]
     os.environ["SERVER_PORT"]  = sys.argv[2]
     os.environ["QUERY_STRING"] = sys.argv[3]
     
if ( os.environ["SERVER_PORT"] == "80" ):
     html_pref = "http://" + os.environ["SERVER_NAME"]
elif ( os.environ["SERVER_PORT"] == "443" ):
     html_pref = "https://" + os.environ["SERVER_NAME"]
else:
     html_pref = "http://" + os.environ["SERVER_NAME"] + ":" + os.environ["SERVER_PORT"] 

ivrb = 1

if ( len(sys.argv) <= 1  ):
     if ( os.environ["SERVER_NAME"] == aws_host_name ):
          if ( ivrb > 4 ):
               print ( "404: okay" )
               print ( "Content-type: text/html\n\n" )
               print ( "" ) 
               print ( "sys.argv = ", sys.argv )
     
          com = "/astrogeo/web_exec/malo_select_file.py" + " " + \
                os.environ["SERVER_NAME"] + " " + \
                os.environ["SERVER_PORT"] + " " + \
                os.environ["QUERY_STRING"]
     
          (ret,out) = bfi_cli ( com, timeout=120.0 )
          for line in out:
              print ( line )
          exit ( 0 )


#
# ------------------------------------------------------------------------
#
os.umask ( 2 )
set_env()

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
     print  ( "</PRE>" )

key_pairs = urllib.parse.parse_qs ( os.environ["QUERY_STRING"] )

model   = key_pairs["model"][0].lower()
type    = key_pairs["type"][0].lower()
origin  = key_pairs["origin"][0].lower()
format  = key_pairs["format"][0].lower()
product = key_pairs["product"][0].lower()
submit  = key_pairs["submit"][0].lower()

if ( "start_date" in key_pairs.keys() ):
     start_date = key_pairs["start_date"][0]
else:
     start_date = ""
if ( "stop_date" in key_pairs.keys() ):
     stop_date = key_pairs["stop_date"][0]
else:
     stop_date = ""

if ( submit == "get info" ):
     print ( 'No information is available at the moment.' )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit ( 0 )


if ( ivrb > 3 ):
     print ( "<P>" )
     print ( "<br>env:        ", os.environ )
     print ( "<br>model:      ", model )
     print ( "<br>type:       ", type )
     print ( "<br>origin:     ", origin )
     print ( "<br>format:     ", format )
     print ( "<br>product:    ", product )
     print ( "<br>start_date: ", start_date )
     print ( "<br>stop_date:  ", stop_date )
     print ( "<br>submit:     ", submit )
     print ( "<P>&nbsp;<P>" )

load_type = model[0:3]

if ( load_type == "toc" ):
     if ( type == "station_series" or type == "grid_series" or \
          type == "grid_series"    or type == "spl_series"  or \
          type == "int_series"                                 ):
          print ( "<B>Wrong request:</B> typ= ", type, " load_type= ", load_type, "</BR>" )
          print ( "<B>Wrong request:</B> ocean tide loading time " + 
                  "series do not exist. Please select loading harmonic variations" )
          print ( '</BODY>' )
          print ( '</HTML>' )
          exit ( 0 )

if ( start_date == "" or stop_date == "" ):
     if ( type == "station_series" or type == "grid_series" or type == "int_series" ):
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  'start and stop dates are not specified' )
          print ( '</BODY>' )
          print ( '</HTML>' )
          exit ( 0 )
          

if ( not ( type == "stations_harmonics" or type == "grid_harmonics" ) ):
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

#
# --- Transform start_date to the condensed format
#

     start_date = start_date[0:4] + start_date[5:7] + start_date[8:] 
     ip = start_date.find(":")
     if ( ip > 1 ): start_date = start_date[0:ip] + start_date[ip+1:]
     ip = start_date.find(":")
     if ( ip > 1 ): start_date = start_date[0:ip] + start_date[ip+1:]

     if ( len(start_date) >= 8 and len(start_date) < 11 ): 
          start_date = start_date[0:8] + "_0000"
     if ( len(start_date) >= 11 and len(start_date) < 13 ): 
          start_date = start_date[0:11] + "00"
     if ( len(start_date) > 13 ): start_date = start_date[0:13]

#
# -- Transform stop_date to the condensed format
#
     stop_date = stop_date[0:4] + stop_date[5:7] + stop_date[8:] 
     ip = stop_date.find(":")
     if ( ip > 1 ): stop_date = stop_date[0:ip] + stop_date[ip+1:]
     ip = stop_date.find(":")
     if ( ip > 1 ): stop_date = stop_date[0:ip] + stop_date[ip+1:]

     if ( len(stop_date) >= 8 and len(stop_date) < 11 ): 
          stop_date = stop_date[0:8] + "_0000"
     if ( len(stop_date) >= 11 and len(stop_date) < 13 ): 
          stop_date = stop_date[0:11] + "00"
     if ( len(stop_date) > 13 ): stop_date = stop_date[0:13]

     if ( stop_date < start_date ):
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  ' the stop date is prior the start date' )
          print ( '</BODY>' )
          print ( '</HTML>' )
          exit ( 0 )


if ( model == "atm_geosfpit" ):
     if ( type == "station_series" ): 
          if ( origin == "total_mass" ) :
               dir = dir_loa + "/load_list/atm/geosfpit"
          else:
               dir = dir_loa + "/load_d1_list/atm/geosfpit"
          file_first_date = dir_date + "/atm/dates/geosfpit_loading_first_date.txt"
          file_last_date  = dir_date + "/atm/dates/geosfpit_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )
     elif ( type == "grid_series" ): 
          if ( origin == "total_mass" ) :
               dir = dir_loa + "/load_grid/atm/geosfpit"
          else:
               dir = dir_loa + "/load_d1_grid/atm/geosfpit"
          file_first_date = dir_date + "/atm/dates/geosfpit_loading_first_date.txt"
          file_last_date  = dir_date + "/atm/dates/geosfpit_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )
     elif ( type == "spl_series" ): 
          if ( origin == "total_mass" ) :
               dir = dir_loa + "/load_spl/atm/geosfpit"
          else:
               dir = dir_loa + "/load_d1_spl/atm/geosfpit"
          file_first_date = dir_date + "/atm/dates/geosfpit_loading_first_date.txt"
          file_last_date  = dir_date + "/atm/dates/geosfpit_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )
     elif ( type == "int_series" ): 
          file_first_date = dir_date + "/atm/dates/geosfpit_loading_first_date.txt"
          file_last_date  = dir_date + "/atm/dates/geosfpit_loading_last_date.txt"
          if ( origin == "total_mass" ) :
               dir = dir_loa + "/load_int/atm/geosfpit"
          else:
               dir = dir_loa + "/load_d1_int/atm/geosfpit"
          file_list = sorted ( os.listdir(dir) )
     elif ( type == "vgep" ): 
          dir = dir_loa + "/vgep/atm/geosfpit"
          file_first_date = dir_date + "/atm/dates/geosfpit_loading_first_date.txt"
          file_last_date  = dir_date + "/atm/dates/geosfpit_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )
elif ( model == "atm_merra2" ):
     if ( type == "station_series" ): 
          if ( origin == "total_mass" ) :
               dir = dir_loa + "/load_list/atm/merra2"
          else:
               dir = dir_loa + "/load_d1_list/atm/merra2"
          file_first_date = dir_date + "/atm/dates/merra2_loading_first_date.txt"
          file_last_date  = dir_date + "/atm/dates/merra2_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )
     elif ( type == "grid_series" ): 
          if ( origin == "total_mass" ) :
               dir = dir_loa + "/load_grid/atm/merra2"
          else:
               dir = dir_loa + "/load_d1_grid/atm/merra2"
          file_first_date = dir_date + "/atm/dates/merra2_loading_first_date.txt"
          file_last_date  = dir_date + "/atm/dates/merra2_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )
     elif ( type == "spl_series" ): 
          if ( origin == "total_mass" ) :
               dir = dir_loa + "/load_spl/atm/merra2"
          else:
               dir = dir_loa + "/load_d1_spl/atm/merra2"
          file_first_date = dir_date + "/atm/dates/merra2_loading_first_date.txt"
          file_last_date  = dir_date + "/atm/dates/merra2_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )
     elif ( type == "int_series" ): 
          file_first_date = dir_date + "/atm/dates/merra2_loading_first_date.txt"
          file_last_date  = dir_date + "/atm/dates/merra2_loading_last_date.txt"
          if ( origin == "total_mass" ) :
               dir = dir_loa + "/load_int/atm/merra2"
          else:
               dir = dir_loa + "/load_d1_int/atm/merra2"
          file_list = sorted ( os.listdir(dir) )
     elif ( type == "vgep" ): 
          dir = dir_loa + "/vgep/atm/merra2"
          file_first_date = dir_date + "/atm/dates/merra2_loading_first_date.txt"
          file_last_date  = dir_date + "/atm/dates/merra2_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )
if ( model == "lws_geosfpit" ):
     if ( type == "station_series" ): 
          if ( origin == "total_mass" ) :
               dir = dir_loa + "/load_list/lws/geosfpit"
          else:
               dir = dir_loa + "/load_d1_list/lws/geosfpit"
          file_first_date = dir_date + "/lws/dates/geosfpit_loading_first_date.txt"
          file_last_date  = dir_date + "/lws/dates/geosfpit_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )
     elif ( type == "grid_series" ): 
          if ( origin == "total_mass" ) :
               dir = dir_loa + "/load_grid/lws/geosfpit"
          else:
               dir = dir_loa + "/load_d1_grid/lws/geosfpit"
          file_first_date = dir_date + "/lws/dates/geosfpit_loading_first_date.txt"
          file_last_date  = dir_date + "/lws/dates/geosfpit_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )
     elif ( type == "spl_series" ): 
          if ( origin == "total_mass" ) :
               dir = dir_loa + "/load_spl/lws/geosfpit"
          else:
               dir = dir_loa + "/load_d1_spl/lws/geosfpit"
          file_first_date = dir_date + "/lws/dates/geosfpit_loading_first_date.txt"
          file_last_date  = dir_date + "/lws/dates/geosfpit_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )
     elif ( type == "int_series" ): 
          file_first_date = dir_date + "/lws/dates/geosfpit_loading_first_date.txt"
          file_last_date  = dir_date + "/lws/dates/geosfpit_loading_last_date.txt"
          if ( origin == "total_mass" ) :
               dir = dir_loa + "/load_int/lws/geosfpit"
          else:
               dir = dir_loa + "/load_d1_int/lws/geosfpit"
          file_list = sorted ( os.listdir(dir) )
     elif ( type == "vgep" ): 
          dir = dir_loa + "/vgep/lws/geosfpit"
          file_first_date = dir_date + "/lws/dates/geosfpit_loading_first_date.txt"
          file_last_date  = dir_date + "/lws/dates/geosfpit_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )
elif ( model == "lws_merra2" ):
     if ( type == "station_series" ): 
          if ( origin == "total_mass" ) :
               dir = dir_loa + "/load_list/lws/merra2"
          else:
               dir = dir_loa + "/load_d1_list/lws/merra2"
          file_first_date = dir_date + "/lws/dates/merra2_loading_first_date.txt"
          file_last_date  = dir_date + "/lws/dates/merra2_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )
     elif ( type == "grid_series" ): 
          if ( origin == "total_mass" ) :
               dir = dir_loa + "/load_grid/lws/merra2"
          else:
               dir = dir_loa + "/load_d1_grid/lws/merra2"
          file_first_date = dir_date + "/lws/dates/merra2_loading_first_date.txt"
          file_last_date  = dir_date + "/lws/dates/merra2_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )
     elif ( type == "spl_series" ): 
          if ( origin == "total_mass" ) :
               dir = dir_loa + "/load_spl/lws/merra2"
          else:
               dir = dir_loa + "/load_d1_spl/lws/merra2"
          file_first_date = dir_date + "/lws/dates/merra2_loading_first_date.txt"
          file_last_date  = dir_date + "/lws/dates/merra2_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )
     elif ( type == "int_series" ): 
          file_first_date = dir_date + "/lws/dates/merra2_loading_first_date.txt"
          file_last_date  = dir_date + "/lws/dates/merra2_loading_last_date.txt"
          if ( origin == "total_mass" ) :
               dir = dir_loa + "/load_int/lws/merra2"
          else:
               dir = dir_loa + "/load_d1_int/lws/merra2"
          file_list = sorted ( os.listdir(dir) )
     elif ( type == "vgep" ): 
          dir = dir_loa + "/vgep/lws/merra2"
          file_first_date = dir_date + "/lws/dates/merra2_loading_first_date.txt"
          file_last_date  = dir_date + "/lws/dates/merra2_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )
elif ( model == "nto_omct05" ):
     if ( type == "station_series" ): 
          if ( origin == "total_mass" ) :
               dir = dir_loa + "/load_list/nto/omct05"
          else:
               dir = dir_loa + "/load_d1_list/nto/omct05"
          file_first_date = dir_date + "/nto/dates/omct05_loading_first_date.txt"
          file_last_date  = dir_date + "/nto/dates/omct05_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )          
     elif ( type == "grid_series" ): 
          if ( origin == "total_mass" ) :                 
               dir = dir_loa + "/load_grid/nto/omct05"    
          else:                                           
               dir = dir_loa + "/load_d1_grid/nto/omct05" 
          file_first_date = dir_date + "/nto/dates/omct05_loading_first_date.txt"
          file_last_date  = dir_date + "/nto/dates/omct05_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )          
     elif ( type == "spl_series" ): 
          if ( origin == "total_mass" ) :                 
               dir = dir_loa + "/load_spl/nto/omct05"
          else:                                           
               dir = dir_loa + "/load_d1_spl/nto/omct05"
          file_first_date = dir_date + "/nto/dates/omct05_loading_first_date.txt"
          file_last_date  = dir_date + "/nto/dates/omct05_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )          
     elif ( type == "int_series" ):                       
          file_first_date = dir_date + "/nto/dates/omct05_loading_first_date.txt"
          file_last_date  = dir_date + "/nto/dates/omct05_loading_last_date.txt"
          if ( origin == "total_mass" ) :                 
               dir = dir_loa + "/load_int/nto/omct05"     
          else:                                           
               dir = dir_loa + "/load_d1_int/nto/omct05"  
          file_list = sorted ( os.listdir(dir) )          
     elif ( type == "vgep" ):                             
          dir = dir_loa + "/vgep/nto/omct05"              
          file_first_date = dir_date + "/nto/dates/omct05_loading_first_date.txt"
          file_last_date  = dir_date + "/nto/dates/omct05_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )
elif ( model == "nto_mpiom06" ):
     if ( type == "station_series" ): 
          if ( origin == "total_mass" ) :
               dir = dir_loa + "/load_list/nto/mpiom06"
          else:
               dir = dir_loa + "/load_d1_list/nto/mpiom06"
          file_first_date = dir_date + "/nto/dates/mpiom06_loading_first_date.txt"
          file_last_date  = dir_date + "/nto/dates/mpiom06_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )          
     elif ( type == "grid_series" ): 
          if ( origin == "total_mass" ) :                 
               dir = dir_loa + "/load_grid/nto/mpiom06"    
          else:                                           
               dir = dir_loa + "/load_d1_grid/nto/mpiom06" 
          file_first_date = dir_date + "/nto/dates/mpiom06_loading_first_date.txt"
          file_last_date  = dir_date + "/nto/dates/mpiom06_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )          
     elif ( type == "spl_series" ): 
          if ( origin == "total_mass" ) :                 
               dir = dir_loa + "/load_spl/nto/mpiom06"
          else:                                           
               dir = dir_loa + "/load_d1_spl/nto/mpiom06"
          file_first_date = dir_date + "/nto/dates/mpiom06_loading_first_date.txt"
          file_last_date  = dir_date + "/nto/dates/mpiom06_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )          
     elif ( type == "int_series" ):                       
          file_first_date = dir_date + "/nto/dates/mpiom06_loading_first_date.txt"
          file_last_date  = dir_date + "/nto/dates/mpiom06_loading_last_date.txt"
          if ( origin == "total_mass" ) :                 
               dir = dir_loa + "/load_int/nto/mpiom06"     
          else:                                           
               dir = dir_loa + "/load_d1_int/nto/mpiom06"  
          file_list = sorted ( os.listdir(dir) )          
     elif ( type == "vgep" ):                             
          dir = dir_loa + "/vgep/nto/mpiom06"              
          file_first_date = dir_date + "/nto/dates/mpiom06_loading_first_date.txt"
          file_last_date  = dir_date + "/nto/dates/mpiom06_loading_last_date.txt"
          file_list = sorted ( os.listdir(dir) )
elif ( model == "toc_got410c" ):
     if ( type == "station_series" ): 
          if ( origin == "total_mass" ) :
               dir = dir_loa + "/load_har_list/toc/got410c"
          else:
               dir = dir_loa + "/load_d1_har_list/toc/got410c"
          file_first_date = "n/a"
          file_last_date  = "n/a"
          file_list = sorted ( os.listdir(dir) )          

elif ( model == "spd_geosfpit" ):
       file_first_date = "/spd/dates/spd_geosfpit_first_date.txt"
       file_last_date  = "/spd/dates/spd_geosfpit_last_date.txt"
       if ( type == "asc" ):
           dir = dir_spd + "/asc/geosfpit"
       if ( type == "bin" ):
           dir = dir_spd + "/bin/geosfpit"
       file_list = sorted ( os.listdir(dir) )
elif ( model == "spd_merra2" ):
       file_first_date = "/spd/dates/spd_merra2_first_date.txt"
       file_last_date  = "/spd/dates/spd_merra2_last_date.txt"
       if ( type == "asc" ):
           dir = dir_spd + "/asc/merra2"
       if ( type == "bin" ):
           dir = dir_spd + "/bin/merra2"
       file_list = sorted ( os.listdir(dir) )

first_date = "n/a"
last_date  = "n/a"
if ( type == "station_series" or \
     type == "grid_series"    or \
     type == "spl_series"    or \
     type == "int_series"     or \
     type == "vgep"           or \
     type == "asc"            or \
     type == "bin"               ): 
     if ( load_type == "atm" or load_type == "lws" or load_type == "nto" or load_type == "spd" ):
          with open(file_first_date) as f:
               first_date = f.readline()
          with open(file_last_date) as f:
               last_date = f.readline()
     else:
         print ( "<B>Wrong request:</B> typ= ", type, " load_type= ", load_type )
         print ( "<B>Wrong request:</B> ocean tide loading time " + 
                 "series do not exist. Please select loading harmonic variations" )
         print ( '</BODY>' )
         print ( '</HTML>' )
         exit ( 0 )


if ( ivrb > 3 ):
     print ( "<br> first_date: ", first_date )
     print ( "<br> last_date:  ", last_date  )
     print ( "<p>" )

flag_found = 0

print ( '<PRE>' )
if ( type == "station_series" or \
     type == "grid_series"    or \
     type == "spl_series"     or \
     type == "int_series"     or \
     type == "vgep"           or \
     type == "asc"               ):
     for i in range(0,len(file_list)):
         ib = file_list[i][0:file_list[i].rfind("_")-1].rfind("_") + 1
         ie = file_list[i].rfind(".")
         date_file = file_list[i][ib:ie]
         if ( date_file >= start_date and date_file <= stop_date ):
              flag_found = 1
              if ( format == "url_list" ):
                   print ( html_pref + dir + "/" + file_list[i] )
              elif ( format == "wget_list" ):
                   print ( "wget -nv -nc -c --retry-connrefused --connect-timeout=20 --tries=32" + " " + \
                           html_pref + dir + "/" + file_list[i] )

if ( type == "bin" ):
     for i in range(0,len(file_list)):
         if ( file_list[i].rfind("~") > 0 ): continue
         if ( file_list[i].rfind("#") > 0 ): continue
         if ( file_list[i].rfind(".txt") > 0 or file_list[i].rfind(".bspd") > 0 ):
              flag_found = 1
              if ( format == "url_list" ):
                   print ( html_pref + dir + "/" + file_list[i] )
              elif ( format == "wget_list" ):
                   print ( "wget -nv -nc -c --retry-connrefused --connect-timeout=20 --tries=32" + " " + \
                            html_pref + dir + "/" + file_list[i] )

elif ( type == "stations_harmonics" ): 
       if ( model == "atm_geosfpit" ):
            if ( origin == "total_mass" ) :
                 file_har = dir_loa + "/load_har_list/atm/geosfpit/atm_geosfpit_harmod.hps"
            else:
                 file_har = dir_loa + "/load_d1_har_list/atm/geosfpit/atm_geosfpit_d1_harmod.hps"
       elif ( model == "atm_merra2" ):
            if ( origin == "total_mass" ) :
                 file_har = dir_loa + "/load_har_list/atm/merra2/atm_merra2_harmod.hps"
            else:
                 file_har = dir_loa + "/load_d1_har_list/atm/merra2/atm_merra2_d1_harmod.hps"
       elif ( model == "lws_geosfpit" ):
            if ( origin == "total_mass" ) :
                 file_har = dir_loa + "/load_har_list/lws/geosfpit/lws_geosfpit_harmod.hps"
            else:
                 file_har = dir_loa + "/load_d1_har_list/lws/geosfpit/lws_geosfpit_d1_harmod.hps"
       elif ( model == "lws_merra2" ):
            if ( origin == "total_mass" ) :
                 file_har = dir_loa + "/load_har_list/lws/merra2/atm_merra2_harmod.hps"
            else:
                 file_har = dir_loa + "/load_d1_har_list/lws/merra2/atm_merra2_d1_harmod.hps"
       elif ( model == "nto_omct05" ):
            if ( origin == "total_mass" ) :
                 file_har = dir_loa + "/load_har_list/nto/omct05/nto_omct05_harmod.hps"
            else:
                 file_har = dir_loa + "/load_d1_har_list/nto/omct05/nto_omct05_d1_harmod.hps"
       elif ( model == "nto_mpiom06" ):
            if ( origin == "total_mass" ) :
                 file_har = dir_loa + "/load_har_list/nto/omct05/nto_mpiom06_harmod.hps"
            else:
                 file_har = dir_loa + "/load_d1_har_list/nto/omct05/nto_mpiom06_d1_harmod.hps"
       elif ( model == "toc_got48" ):
            if ( origin == "total_mass" ) :
               file_har = dir_loa + "/load_har_list/toc/got48/toc_got48_harmod.hps"
            else:
               file_har = dir_loa + "/load_d1_har_list/toc/got48/toc_got48_d1_harmod.hps"
       elif ( model == "toc_fes2012" ):
            if ( origin == "total_mass" ) :
                 file_har = dir_loa + "/load_har_list/toc/fes2012/toc_fes2012_harmod.hps"
            else:
                 file_har = dir_loa + "/load_d1_har_list/toc/fes2012/toc_fes2012_d1_harmod.hps"
       elif ( model == "toc_got410c" ):
            if ( origin == "total_mass" ) :
               file_har = dir_loa + "/load_har_list/toc/got410c/toc_got410c_harmod.hps"
            else:
               file_har = dir_loa + "/load_d1_har_list/toc/got410c/toc_got410c_d1_harmod.hps"
       elif ( model == "toc_fes2014b" ):
            if ( origin == "total_mass" ) :
                 file_har = dir_loa + "/load_har_list/toc/fes2014b/toc_fes2014b_harmod.hps"
            else:
                 file_har = dir_loa + "/load_d1_har_list/toc/fes2014b/toc_fes2014b_d1_harmod.hps"
       elif ( model == "toc_equil01" ):
            if ( origin == "total_mass" ) :
                 file_har = dir_loa + "/load_har_list/toc/equil01/toc_equil02_harmod.hps"
            else:
                 file_har = dir_loa + "/load_d1_har_list/toc/equil01/toc_equil01_d1_harmod.hps"
       elif ( model == "toc_equil02" ):
            if ( origin == "total_mass" ) :
                 file_har = dir_loa + "/load_har_list/toc/equil02/toc_equil02_harmod.hps"
            else:
                 file_har = dir_loa + "/load_d1_har_list/toc/equil02/toc_equil02_d1_harmod.hps"

       if ( model == "nto_mpiom06" ):
            flag_found = -2
       else:
            if ( format == "url_list" ):
                 print ( html_pref +  file_har )
            elif ( format == "wget_list" ):
                 print ( "wget -nv -nc -c --retry-connrefused --connect-timeout=20 --tries=32" + " " + \
                          html_pref + file_har )
            flag_found = 1

elif ( type == "spl_harmonics" ): 
       if ( model == "atm_geosfpit" ):
            if ( origin == "total_mass" ) :
                 file_har = dir_loa + "/load_har_spl/atm/geosfpit/atm_geosfpit_spl_harmod.heb"
            else:
                 file_har = dir_loa + "/load_d1_har_spl/atm/geosfpit/atm_geosfpit_d1_spl_harmod.heb"
       elif ( model == "atm_merra2" ):
            if ( origin == "total_mass" ) :
                 file_har = dir_loa + "/load_har_spl/atm/merra2/atm_merra2_spl_harmod.heb"
            else:
                 file_har = dir_loa + "/load_d1_har_spl/atm/merra2/atm_merra2_d1_spl_harmod.heb"
       elif ( model == "lws_geosfpit" ):
            if ( origin == "total_mass" ) :
                 file_har = dir_loa + "/load_har_spl/lws/geosfpit/lws_geosfpit_spl_harmod.heb"
            else:
                 file_har = dir_loa + "/load_d1_har_spl/lws/geosfpit/lws_geosfpit_d1_spl_harmod.heb"
       elif ( model == "lws_merra2" ):
            if ( origin == "total_mass" ) :
                 file_har = dir_loa + "/load_har_spl/lws/merra2/atm_merra2_spl_harmod.heb"
            else:
                 file_har = dir_loa + "/load_d1_har_spl/lws/merra2/atm_merra2_d1_spl_harmod.heb"
       elif ( model == "nto_omct05" ):
            if ( origin == "total_mass" ) :
                 file_har = dir_loa + "/load_har_spl/nto/omct05/nto_omct05_spl_harmod.heb"
            else:
                 file_har = dir_loa + "/load_d1_har_spl/nto/omct05/nto_omct05_d1_spl_harmod.heb"
       elif ( model == "nto_mpiom06" ):
            if ( origin == "total_mass" ) :
                 file_har = dir_loa + "/load_har_spl/nto/mpiom06/nto_mpiom06_spl_harmod.heb"
            else:
                 file_har = dir_loa + "/load_d1_har_spl/nto/mpiom06/nto_mpiom06_d1_spl_harmod.heb"
            flag_found = -2
       elif ( model == "toc_got48" ):
            if ( origin == "total_mass" ) :
               file_har = dir_loa + "/load_har_spl/toc/got48/toc_got48_spl_harmod.heb"
            else:
               file_har = dir_loa + "/load_d1_har_spl/toc/got48/toc_got48_d1_spl_harmod.heb"
       elif ( model == "toc_fes2012" ):
            if ( origin == "total_mass" ) :
                 file_har = dir_loa + "/load_har_spl/toc/fes2012/toc_fes2012_spl_harmod.heb"
            else:
                 file_har = dir_loa + "/load_d1_har_spl/toc/fes2012/toc_fes2012_d1_spl_harmod.heb"
       elif ( model == "toc_got410c" ):
            if ( origin == "total_mass" ) :
               file_har = dir_loa + "/load_har_spl/toc/got410c/toc_got410c_spl_harmod.heb"
            else:
               file_har = dir_loa + "/load_d1_har_spl/toc/got410c/toc_got410c_d1_spl_harmod.heb"
       elif ( model == "toc_fes2014b" ):
            if ( origin == "total_mass" ) :
                 file_har = dir_loa + "/load_har_spl/toc/fes2014b/toc_fes2014b_spl_harmod.heb"
            else:
                 file_har = dir_loa + "/load_d1_har_spl/toc/fes2014b/toc_fes2014b_d1_spl_harmod.heb"
       elif ( model == "toc_equil01" ):
            if ( origin == "total_mass" ) :
                 file_har = dir_loa + "/load_har_spl/toc/equil01/toc_equil01_spl_harmod.heb"
            else:
                 file_har = dir_loa + "/load_d1_har_spl/toc/equil01/toc_equil01_d1_spl_harmod.heb"
       elif ( model == "toc_equil02" ):
            if ( origin == "total_mass" ) :
                 file_har = dir_loa + "/load_har_spl/toc/equil02/toc_equil02_spl_harmod.heb"
            else:
                 file_har = dir_loa + "/load_d1_har_spl/toc/equil02/toc_equil02_d1_spl_harmod.heb"

       if ( model == "nto_mpiom06" ):
            flag_found = -2
       else:
            if ( format == "url_list" ):
                 print ( html_pref +  file_har )
            elif ( format == "wget_list" ):
                 print ( "wget -nv -nc -c --retry-connrefused --connect-timeout=20 --tries=32" + " " + \
                          html_pref + file_har )
            flag_found = 1

elif ( type == "grid_harmonics" ):
       if ( model == "atm_geosfpit" ):
            if ( origin == "total_mass" ) : 
                 file_har = dir_loa + "/load_har_grid/atm/geosfpit/atm_geosfpit_harmod.nc"
            else:
                 file_har = dir_loa + "/load_d1_har_grid/atm/geosfpit/atm_geosfpit_d1_harmod.nc"
       elif ( model == "atm_merra2" ):
            if ( origin == "total_mass" ) : 
                 file_har = dir_loa + "/load_har_grid/atm/merra2/atm_merra2_harmod.nc"
            else:
                 file_har = dir_loa + "/load_d1_har_grid/atm/merra2/atm_merra2_d1_harmod.nc"
       elif ( model == "lws_geosfpit" ):
            if ( origin == "total_mass" ) : 
                 file_har = dir_loa + "/load_har_grid/lws/geosfpit/lws_geosfpit_harmod.nc"
            else:
                 file_har = dir_loa + "/load_d1_har_grid/lws/geosfpit/lws_geosfpit_d1_harmod.nc"
       elif ( model == "lws_merra2" ):
            if ( origin == "total_mass" ) : 
                 file_har = dir_loa + "/load_har_grid/lws/merra2/lws_merra2_harmod.nc"
            else:
                 file_har = dir_loa + "/load_d1_har_grid/lws/merra2/lws_merra2_d1_harmod.nc"
       elif ( model == "nto_omct05" ):
            if ( origin == "total_mass" ) : 
                 file_har = dir_loa + "/load_har_grid/nto/omct05/nto_omct_harmod.nc"
            else:
                 file_har = dir_loa + "/load_d1_har_grid/nto/omct05/nto_omct_d1_harmod.nc"
       elif ( model == "nto_mpiom06" ):
            if ( origin == "total_mass" ) : 
                 file_har = dir_loa + "/load_har_grid/nto/mpiom06/nto_omct_harmod.nc"
            else:
                 file_har = dir_loa + "/load_d1_har_grid/nto/mpiom06/nto_omct_d1_harmod.nc"
            flag_found = -2
       elif ( model == "toc_got48" ):
            if ( origin == "total_mass" ) : 
                 file_har = dir_loa + "/load_har_grid/toc/got48/toc_got48_harmod.nc"
            else:
                 file_har = dir_loa + "/load_d1_har_grid/toc/got48/toc_got48_d1_harmod.nc"
       elif ( model == "toc_fes2012" ):
            if ( origin == "total_mass" ) : 
                 file_har = dir_loa + "/load_har_grid/toc/fes2012/toc_fes2012_harmod.nc"
            else:
                 file_har = dir_loa + "/load_d1_har_grid/toc/fes2012/toc_fes2012_d1_harmod.nc"
       elif ( model == "toc_got410c" ):
            if ( origin == "total_mass" ) : 
                 file_har = dir_loa + "/load_har_grid/toc/got410c/toc_got410c_harmod.nc"
            else:
                 file_har = dir_loa + "/load_d1_har_grid/toc/got410c/toc_got410c_d1_harmod.nc"
       elif ( model == "toc_fes2014b" ):
            if ( origin == "total_mass" ) : 
                 file_har = dir_loa + "/load_har_grid/toc/fes2014b/toc_fes2014b_harmod.nc"
            else:
                 file_har = dir_loa + "/load_d1_har_grid/toc/fes2014b/toc_fes2014b_d1_harmod.nc"
       elif ( model == "toc_equil01" ):
            if ( origin == "total_mass" ) :
                 file_har = dir_loa + "/load_har_grid/toc/lpequil/toc_equil01_harmod.nc"
            else:
                 file_har = dir_loa + "/load_d1_har_grid/toc/lpequil/toc_equil01_d1_harmod.nc"
       elif ( model == "toc_equil02" ):
            if ( origin == "total_mass" ) :
                 file_har = dir_loa + "/load_har_grid/toc/lpequil/toc_equil02_harmod.nc"
            else:
                 file_har = dir_loa + "/load_d1_har_grid/toc/lpequil/toc_equil02_d1_harmod.nc"

       if ( model == "nto_mpiom06" ):
            flag_found = -2
       else:
            if ( format == "url_list" ):
                 print ( html_pref +  file_har )
            elif ( format == "wget_list" ):
                   print ( "wget -nv -nc -c --retry-connrefused --connect-timeout=20 --tries=32" + " " + \
                            html_pref + file_har )
            flag_found = 1



print ( '</PRE>' )

if ( flag_found == -2 ):
     print ( "<B>No files</B> for MPIOM06 model were computed, since harmonic variations " +
             "in ocean bottom are already taken into account in ocean tidal loading" )
if ( flag_found == 0 ):
     print ( "<B>No files</B> were found. Please change your search criteria." )

print ( '</BODY>' )
print ( '</HTML>' )
exit ( 0 )
