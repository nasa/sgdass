#!/usr/bin/python3 
#
# ************************************************************************
# *                                                                      *
# *   Program malo_ondemand.py processes the web form for on-demand      *
# *   loading computation, checks the input, launches computation,       *
# *   and redirect the uses to the web page associated with the request. *
# *                                                                      *
# * ## 08-MAY-2013  malo_ondemand.py  v3.4 (c) L. Petrov  15-NOV-2022 ## *
# *                                                                      *
# ************************************************************************
#
import os, sys, string, stat, time, datetime
from   malo_check_date   import *
from   malo_check_stafil import *
from   malo_subs         import *
from   malo_set_catcha   import *
from   set_env           import *

#
# ------------------------------------------------------------------------
#
os.umask ( 2 )

print ( "404: okay" )
print ( "Content-type: text/html\n\n" )
print ( "" ) 
print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
print ( '<HTML LANG="en">' )
print ( '<HEAD>' )

set_env()
config = config_class()
config.ivrb = 1

date_req = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
if ( os.environ["SERVER_PORT"] == "80" ):
     url_str = 'http://' + os.environ["SERVER_NAME"] + '/ondemand/req/' + date_req
elif ( os.environ["SERVER_PORT"] == "443" ):
     url_str = 'https://' + os.environ["SERVER_NAME"] + '/ondemand/req/' + date_req
else:
     url_str = 'http://' + os.environ["SERVER_NAME"] + ":" + os.environ["SERVER_PORT"] + '/ondemand/req/' + date_req

ip_unlim = check_ip ( config, os.environ['REMOTE_ADDR']  )

if ( ip_unlim == 0 ):
     lim = check_lim ( config, os.environ["SERVER_NAME"], os.environ['REMOTE_ADDR']  )
     if ( lim != 0 ):
          exit ( 0 )

print ( '<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' )

if ( len(os.environ["CONTENT_TYPE"]) > 128 ):
     print ( '</HEAD' )
     print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
             "Trap of internal control: too long line CONTENT_TYPE" )
     print ( '<PRE>' )
     print ( os.environ )
     print ( '</PRE>' )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit ( 0 )

if ( len(os.environ["CONTENT_LENGTH"]) > 128 ):
     print ( '</HEAD' )
     print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
             "Trap of internal control: too long line CONTENT_LENGTH" )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit ( 0 )

boundary = os.environ["CONTENT_TYPE"][30:]
input_len = int(os.environ["CONTENT_LENGTH"])

# print ( "ip_unlim= ", ip_unlim, ' rem_addr: ', os.environ['REMOTE_ADDR'], ' input_len = ', input_len, '<p>' ) # %%%%%%%%%%%%%%%%

if ( input_len > config.max_len ):
     print ( '</HEAD' )
     print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
             "Your input is too long. Try to reduce the number of " + \
             "stations" )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit ( 0 )

buf = sys.stdin.readlines()

if ( config.ivrb == 12 ):
     print ( '</HEAD' )
     print ( '<PRE>' )
     for i in range(0,len(buf)):
         print ( buf[i].replace("\n","") )
         print ( "BOU: ", os.environ["CONTENT_TYPE"][30:] )
     print ( '</PRE>' )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit ( 0 )

vars = {}
ind_fil_beg = 999999999
sta_fil = []
for i in range(0,len(buf)):
    if ( len(buf[i]) > 128 ):
         print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                 "Trap of internal control: line %d is too long " % i )
         print ( '</BODY>' )
         print ( '</HTML>' )
         exit ( 0 )
    word = buf[i].split()
    if ( len(word) > 0 ):   
         if ( word[0] == "Content-Disposition:" ):
              var_name = word[2].split("=")[1].replace('"','') 
              if ( len(word) == 3 ):
                   var_value = buf[i+2].replace("\n",'').replace("\r",'')  
                   vars[var_name] = var_value
              elif ( len(word) == 4 ):
                     ind_fil_beg = i+2

    if ( i >= ind_fil_beg ):
         if ( buf[i].replace( "\n",'').find(boundary) >= 0 ):
              ind_fil_beg = 999999999
         else:
              if ( len(buf[i]) > 0 ): 
                   if ( buf[i] != "\r\n" ):
                        sta_fil.append( removeNonAscii(buf[i].replace("\n",''))  )

if ( len(sta_fil) == 0 ):
     print ( '</HEAD' )
     print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
             "You did not specify the station file, or the station file is empty. Please try again." )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit  ( 1 )

if ( config.ivrb > 1 ):
     for var_name, var_value in vars.items():
         print ( var_name, "  >>" + var_value + "<< " )

if   ( vars["model"]  == "ATM_GEOSFPIT" ):
       vars["model"]   = "GEOSFPIT"       
       vars["service"] = "atm"

elif ( vars["model"]  == "ATM_GEOSFP" ):
       vars["model"]   = "GEOSFP"       
       vars["service"] = "atm"

elif ( vars["model"]  == "ATM_MERRA2" ):
       vars["model"]   = "MERRA2"       
       vars["service"] = "atm"

elif ( vars["model"]  == "LWS_GEOSFPIT" ):
       vars["model"]   = "GEOSFPIT"       
       vars["service"] = "lws"

elif ( vars["model"]  == "LWS_MERRA2" ):
       vars["model"]   = "MERRA2"       
       vars["service"] = "lws"

elif ( vars["model"]  == "NTO_OMCT05" ):
       vars["model"]   = "OMCT05"       
       vars["service"] = "nto"

elif ( vars["model"]  == "NTO_MPIOM06" ):
       vars["model"]   = "MPIOM06"       
       vars["service"] = "nto"
       if ( vars["mode"] == "harmonics" or vars["mode"] == "s1_harmonics" ): 
              print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                      "Harmonics cannot be computed for non-tidal ocean loading, since they " + \
                      "are already accounted in the ocean tidal loading model. Please select time series." )
              print ( '</PRE>' )
              print ( '</BODY>' )
              print ( '</HTML>' )
              exit ( 0 )

elif ( vars["model"]  == "TOC_GOT48" ):
       vars["model"]   = "GOT48"       
       vars["service"] = "toc"
       if ( vars["mode"] == "series" ): 
              print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                      "Time series cannot be computed for ocean loading. Please select harmonics" )
              print ( '</PRE>' )
              print ( '</BODY>' )
              print ( '</HTML>' )
              exit ( 0 )

elif ( vars["model"] == "TOC_GOT410C" ):
       vars["model"] = "GOT410C"       
       vars["service"] = "toc"
       if ( vars["mode"] == "series" ): 
              print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                      "Time series cannot be computed for ocean loading. Please select harmonics" )
              print ( '</PRE>' )
              print ( '</BODY>' )
              print ( '</HTML>' )
              exit ( 0 )

elif ( vars["model"] == "TOC_FES2012" ):
       vars["model"] = "FES2012"       
       vars["service"] = "toc"
       if ( vars["mode"] == "series" ): 
            print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                    "Time series cannot be computed for ocean loading. Please select harmonics" )
            print ( '</BODY>' )
            print ( '</HTML>' )
            exit ( 0 )

elif ( vars["model"] == "TOC_FES2014B" ):
       vars["model"] = "FES2014B"       
       vars["service"] = "toc"
       if ( vars["mode"] == "series" ): 
            print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                    "Time series cannot be computed for ocean loading. Please select harmonics" )
            print ( '</BODY>' )
            print ( '</HTML>' )
            exit ( 0 )

elif ( vars["model"] == "TOC_EQUIL01" ):
       vars["model"] = "EQUIL01"       
       vars["service"] = "toc"
       if ( vars["mode"] == "series" ): 
            print ( '</HEAD' )
            print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                    "Time series cannot be computed for ocean loading. Please select harmonics" )
            print ( '</BODY>' )
            print ( '</HTML>' )
            exit ( 0 )

elif ( vars["model"] == "TOC_EQUIL02" ):
       vars["model"] = "EQUIL02"       
       vars["service"] = "toc"
       if ( vars["mode"] == "series" ): 
            print ( '</HEAD' )
            print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                    "Time series cannot be computed for ocean loading. Please select harmonics" )
            print ( '</BODY>' )
            print ( '</HTML>' )
            exit ( 0 )


else:
     print ( '</HEAD' )
     print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
             "Unsupported model " + vars["model"] )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit ( 0 )
   
if ( not ( vars["mode"] == "series"       or \
           vars["mode"] == "harmonics"    or \
           vars["mode"] == "s1_harmonics"    ) ):
     print ( '</HEAD' )
     print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
             "Unsupported mode " + vars["mode"] )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit ( 0 )

if ( vars["frame"] == "total_mass" ): vars["frame"] = "cm"
if ( vars["frame"] == "solid_mass" ): vars["frame"] = "cf"

if ( not ( vars["frame"] == "cm"       or \
           vars["frame"] == "cf"       or \
           vars["frame"] == "d1"          ) ):
     print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
             "Unsupported frame " + vars["frame"] )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit ( 0 )

if ( vars["mode"] == "series" ):
     start_date = check_date ( vars["start_date"], "start_date" )
     if ( start_date == None ):
          print ( '</HEAD' )
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  "Wrong start date " + vars["start_date"] )
          print ( '</BODY>' )
          print ( '</HTML>' )
          exit ( 0 )
     
     stop_date = check_date ( vars["stop_date"], "stop_date" )
     if ( stop_date == None ):
          print ( '</HEAD' )
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  "Wrong stop date " + vars["stop_date"] )
          print ( '</BODY>' )
          print ( '</HTML>' )
          exit ( 0 )

     com = "/astrogeo/web_exec/malo_query.py " + \
           " -s " + vars["service"] + \
           " -m " + vars["model"] + \
           " -a get_loading_first_date"
     if ( os.environ["SERVER_NAME"] == "massloading.smce.nasa.gov" ):
         (ret, buf ) = exe_nolog ( "/auto/bfi_exec.py" + " " + com )
     else:
         (ret, buf ) = exe_nolog ( com )

     loading_first_date = buf[0][0:4]   + "." + \
                          buf[0][4:6]   + "." + \
                          buf[0][6:11]  + ":" + \
                          buf[0][11:13] + ":00.0"
   
     com = "/astrogeo/web_exec/malo_query.py " + \
           " -s " + vars["service"] + \
           " -m " + vars["model"] + \
           " -a get_loading_last_date"

     if ( os.environ["SERVER_NAME"] == "massloading.smce.nasa.gov" ):
         (ret, buf ) = exe_nolog ( "/auto/bfi_exec.py" + " " + com )
     else:
         (ret, buf ) = exe_nolog ( com )

     loading_last_date = buf[0][0:4]   + "." + \
                         buf[0][4:6]   + "." + \
                         buf[0][6:11]  + ":" + \
                         buf[0][11:13] + ":00.0"

     if ( config.ivrb > 1 ): 
          print ( "loading_first_dat: ", loading_first_date, \
                  "loading_last_date: ", loading_last_date[0]   )
     
     if ( start_date < loading_first_date ):
          print ( '</HEAD' )
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  "Start date " + start_date + \
                  " is too early: there are no data before " + \
                  loading_first_date )
          print ( 'buf= ', buf )
          print ( '</PRE>' )
          print ( '</BODY>' )
          print ( '</HTML>' )
          exit ( 0 )
     
     if ( stop_date < start_date ):
          print ( '</HEAD' )
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  "Stop date " + stop_date + \
                  " is before the start date " + start_date )
          print ( '</BODY>' )
          print ( '</HTML>' )
          exit ( 0 )
     
     if ( stop_date > loading_last_date ):
          print ( '</HEAD' )
          print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                  "Stop date " + stop_date + \
                  " is too late: there are no data after " + \
                  loading_last_date )
          print ( '</BODY>' )
          print ( '</HTML>' )
          exit ( 0 )

     date_start = parse_solve_date ( start_date, "start_date" )
     date_stop  = parse_solve_date ( stop_date,  "stop_date"  )
     tim_intrv  = (date_stop - date_start).days*86400 + \
                  (date_stop - date_start).seconds

else:
     start_date = "2000.01.01_12:00:00"
     stop_date  = "2000.01.01_12:00:00"
     loading_first_date = "2000.01.01_12:00:00"
     loading_last_date  = "2000.01.01_12:00:00"
     date_start = "2000.01.01_12:00:00"
     date_stop  = "2000.01.01_12:00:00"
     tim_intrv = 0
     
(sta_buf, n_sta ) = malo_check_sta_inp ( sta_fil )
if ( n_sta == 0 ):
     print ( '</HEAD' )
     print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
             "Error was found in your input station file" )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit ( 0 )

if ( ip_unlim == 0 and n_sta > config.max_sta ):
     print ( '</HEAD' )
     print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
             "The number of requested stations is too big: " + \
             "more than %d" % config.max_sta )
             
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit ( 0 )

email      = vars["email"]
if ( email == "" ): email = "n/a"


if ( tim_intrv < 0 ):
     print ( '</HEAD' )
     print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
             "stop date is before the start date" )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit ( 0 )

if ( ip_unlim == 0 and tim_intrv > config.max_intrv ):
     print ( '</HEAD' )
     print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' \
             'Requested time interval exceeded %3.1f years. ' \
             'Please reduce the time interval. ' % \
             ( config.max_intrv/(365.25*86400.) ) )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit ( 0 )

if ( ip_unlim == 1 and tim_intrv > config.malo_max_intrv ):
     print ( '</HEAD' )
     print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' \
             'Requested time interval exceeded %3.1f years. ' \
             'Please reduce the time interval. ' % \
             ( config.malo_max_intrv/(365.25*86400.) ) )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit ( 0 )

if ( config.ivrb > 1 ):
     print ( "start_date: ", start_date )
     print ( "stop_date:  ", stop_date  )
     print ( "email:      ", email      )
     print ( "n_sta:      ", n_sta      )
     print ( "tim_intrv:  ", tim_intrv  )

     for i in range(0,len(sta_buf)):
         print ( "res: ", sta_buf[i] )

os.mkdir ( config.ondemand_dir + "/req/" + date_req )
os.chmod ( config.ondemand_dir + "/req/" + date_req, \
           stat.S_IREAD + stat.S_IWRITE + stat.S_IEXEC + \
           stat.S_IRGRP + stat.S_IWGRP  + stat.S_IXGRP + \
           stat.S_IROTH + stat.S_IXOTH  )

sta_file_name = config.ondemand_dir + "/req/" + date_req + "/sta_fil.txt" 
if ( config.ivrb > 1 ): print ( "sta_file_name= ", sta_file_name )

sys.stdout.flush()
# time.sleep ( 4 )    # %%%%%%%%%%

if ( ip_unlim == 1 ):
      print ( '<META HTTP-EQUIV="refresh" CONTENT="0;' + \
               "URL='" + url_str + '">' )
print ( '</HEAD' )

f= open ( sta_file_name, "w" )
f.writelines ( ["%s\n" % item for item in sta_buf] )
f.close()
os.chmod ( sta_file_name, \
           stat.S_IREAD + stat.S_IWRITE + stat.S_IEXEC + \
           stat.S_IRGRP + stat.S_IWGRP  + stat.S_IXGRP + \
           stat.S_IROTH + stat.S_IXOTH  )

status_file_name = config.ondemand_dir + "/req/" + date_req + "/status.txt" 
index_file_name  = config.ondemand_dir + "/req/" + date_req + "/index.html" 

f= open ( index_file_name, "w" )
f.write ( ' <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">\n' )
f.write ( ' <HTML><HEAD> <META HTTP-EQUIV="Content-Type" content="text/html; charset=iso-8859-1">\n' )
f.write ( ' </HEAD><BODY></BODY></HTML>\n' )
f.close()

time_now = str(datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S")).replace( " ", "0" ) + \
           ".%3d" % int(float(datetime.datetime.now().microsecond)/1000.0)
f= open ( status_file_name, "a" )
f.write ( "You can use key 'Reload' at your browser to see the status update\n" )
f.write ( "=================================================================\n" )
f.write ( time_now + " Request received from " + \
          os.environ["REMOTE_ADDR"] + "\n" )
f.close()
os.chmod ( status_file_name, \
           stat.S_IREAD + stat.S_IWRITE + stat.S_IEXEC + \
           stat.S_IRGRP + stat.S_IWGRP  + stat.S_IXGRP + \
           stat.S_IROTH + stat.S_IXOTH  )

if ( ip_unlim == 0 ):
     malo_set_catcha ( date_req, vars["mode"], vars["frame"], vars["model"], \
                       vars["service"], n_sta, start_date[0:19], stop_date[0:19], \
                       os.environ["REMOTE_ADDR"], os.environ["HTTP_REFERER"], email  )
     catcha_html = config.req_dir + "/" + date_req + "/catcha.html"
     with open(catcha_html) as f:
          cah = f.readlines()

     print ( '</PRE>' )
     for i in range(0,len(cah)):
         print ( cah[i].replace("\n",'').replace("\r",'') )
else:
     query_string = "date_req=" + date_req + "&" + \
                    "model=" + vars["model"] + "&" + \
                    "mode=" + vars["mode"] + "&" + \
                    "frame=" + vars["frame"] + "&" + \
                    "service=" + vars["service"] + "&" + \
                    "n_sta=" + "%s" % n_sta + "&" + \
                    "start_date=" + start_date[0:19] + "&" + \
                    "stop_date=" + stop_date[0:19] + "&" + \
                    "remote_addr=" + os.environ["REMOTE_ADDR"] + "&" + \
                    "email=" + email + "&" + \
                    "malo_http=" + os.environ["HTTP_HOST"] + "&" + \
                    "answer=yes" + "&" + \
                    "ip_unlim=yes" + "&" + \
                    "silent=yes"

     query_string = query_string.replace(":","%3A").replace("/","%2F")

     os.environ["QUERY_STRING"] = query_string;

     (ret, buf ) = exe_nolog ( "./malo_check_catcha.py" )
     for i in range(0,len(buf)):
         print ( buf[i] )

print ( '</BODY>' )
print ( '</HTML>' )
