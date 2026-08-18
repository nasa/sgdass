#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program get_loading_ondemand.py
# *                                                                      *
# * # 06-JUL-2015 get_loading_ondemand.py v1.0 L. Petrov 28-AUG-2017 ### *
# *                                                                      *
# ************************************************************************
import sys, os, shutil, subprocess, time
import optparse 
import datetime
import time
import random
from   datetime import timedelta

if ( sys.version[:3] < "3.0" ): 
     print ( "This script cannot run under Python-2. Please use Python-3." ); 
     exit ( 1 )

version__label = "get_loading_ondemand version 1.2   of 2017.08.28"
model_names = [ \
                "ATM:GEOSFP",   \
                "ATM:GEOSFPIT", \
                "ATM:MERRA2",   \
                "LWS:GEOSFPIT", \
                "LWS:MERRA2",   \
                "NTO:OMCT",     \
                "TOC:GOT48",    \
                "TOC:GOT410C",  \
                "TOC:FES2012"   \
                "TOC:FES2014B"  \
              ]

model_types = [ \
                "time_series", \
                "harmonic_variations", \
                "s1_variations" \
              ]

frame_types = [ \
                "total_mass",  \
                "solid_mass",  \
                "d1_term_only" \
              ]
sleep_time = 6.0
num_tries  = 80000

#
# ------------------------------------------------------------------------
#
def exe ( command ):
#"""
#    Spawn a supborcess, execute command in the context of the suborpess,
#    wait for its completion and return completion code and results.
#"""
    words = command.split()
    time_str = str(datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.") + "%6d" % datetime.datetime.now().microsecond).replace( " ", "0" )
    (ret, out) = subprocess.getstatusoutput ( command )
    return ( ret, out.split ( "\n" ) )
#
# ------------------------------------------------------------------------
#
def get_loading_ondemand ( sta_file, host_name, start_time, stop_time, \
                           model, type, frame, output_pref, log, verb ):

#
# --- Check: can we write a file with prefix
#
    tmp_fil = output_pref + "%05d" % os.getpid()
    try:
        f = open ( tmp_fil, 'w'  )
        f.write ( "test" )
        f.close ( )
        os.remove ( tmp_fil ) 
    except:
        print ( "ERROR: cannot write in %s . Please check argument %s" % \
                (tmp_fil, output_pref ) )
        exit  ( 21 ) 

#
# --- Check for unsupported combinations of models and model types
#
    if ( model[0:3] == "TOC" and type == "time_series" ):
         print ( "ERROR: time series for model %s is not defined" % model )
         exit  ( 22 )
    elif ( model[0:3] == "TOC" and type == "harmonic_variations" ):
         type = "toc_harmon"
    elif ( model[0:3] == "TOC" and type == "s1_variations" ):
         type = "s1_harmonics"

#
# --- Check whether our IP is in the list of "unlimited" IP
#
    tmp_fil = '/tmp/malo__' + "%05d" % os.getpid()
    com =  "wget" + " " + "-O " + tmp_fil + " " + \
           "http://" + host_name + "/malo_check_limit.html"
    if ( verb > 2 ): print ( "About to execute command ", com )
    (ret, out) = exe ( com )
    if ( not ret == 0 ):
         print ( "ERROR: failed to communicate with the server %s" % host_name )
         exit  ( 23 )
    if ( verb > 3 ): print ( "Answer: ", out )

    if ( os.path.isfile ( tmp_fil ) ):  
#
# ------ Read and parse the file with results
#
         with open( tmp_fil ) as f:
              tmp_buf = f.readlines()
         f.close()
         os.remove ( tmp_fil )
         status = "limited"
         for i in range(0,len(tmp_buf)):
             if ( tmp_buf[i].replace("\n","") == "unlimited" ):
                  status = "unlimited"
    else:           
         print ( "ERROR: command %s did not return the status" % com )
    if ( verb > 1 ): print ( "Status: %s" % status )
    if ( not status == "unlimited" ): 
         print ( "ERROR: You cannot run get_loading_ondemenad.py on server %s " \
                 "because your IP is not registered for unlimited operations.\n" \
                 "Please visit http://%s/ip_registration_form.html to apply" \
                 " for registration of your IP address" % (host_name, host_name ) )
         exit  ( 24 )

    if ( type == "time_series" ):
#
# ------ Get start and stop date of the loading time series which the server
# ------ is able to compute
#
         url_date_start = "http://" + host_name + "/" + model[0:3].lower() + \
                          "/dates/" + model[4:len(model)].lower() + \
                          "_loading_first_date.txt"
         url_date_stop  = "http://" + host_name + "/" + model[0:3].lower() + \
                          "/dates/" + model[4:len(model)].lower() + \
                          "_loading_last_date.txt"
          
#
# ------ Execute wget command for reading the file with start time of available loading
#
         com = "wget -q -O - " + url_date_start
         if ( verb > 2 ): print ( "About to execute command ", com )
         (ret, out) = exe ( com )
         if ( not ret == 0 ):
              print ( "ERROR: failed to learn the start date from host ", host_name )
              exit  ( 25 )
         if ( verb > 2 ): print ( "Answer: ", out )
         data_start_date = out[0]
     
#
# ------ Execute wget command for reading the file with stop time of available loading
#
         com = "wget -q -O - " + url_date_stop
         if ( verb > 2 ): print ( "About to execute command ", com )
         (ret, out) = exe ( com )
         if ( not ret == 0 ):
              print ( "ERROR: failed to learn the stop date from host ", host_name )
              exit  ( 26 )
         if ( verb > 2 ): print ( "Answer: ", out )
         data_stop_date = out[0]
     
         if ( start_time != "check" and start_time != "check" and verb > 0 ):
              print ( "Start/stop dates for loading %s at the server %s are %s, %s" % \
                      ( model, host_name, data_start_date, data_stop_date ) )

    if ( type == "time_series" and start_time == "check" ):
         print ( "Model: %s start_time: %s" % ( model, data_start_date ) )

    if ( type == "time_series" and stop_time == "check" ):
         print ( "Model: %s stop_time:  %s" % ( model, data_stop_date ) )

    if ( start_time == "check" or stop_time == "check" ):
         exit ( 0 )

#
# --- Read station file
#
    try:
        with open( sta_file ) as f:
             sta_buf = f.readlines()
        f.close()
    except:
        print ( "ERROR: station file %s is not found" % sta_file )
        exit  ( 27 )
#
# --- Check station file
#
    k_sta = 0
    c_sta = []
    for i in range(0,len(sta_buf)):
        sta_buf[i].strip("\n")
        if ( sta_buf[i][0]   == '#' ): continue
        if ( len(sta_buf[i]) == 0 ): 
             print ( "ERROR: line %d of the input station file %s is empty" % \
                      ( i+1, sta_file) ) 
             exit  ( 28 )
        if ( sta_buf[i].split()[0] == 'SITLIST' ): continue
        if ( len(sta_buf[i].split()) < 4 ): 
             print ( "ERROR: line %d of the input station file %s has %d " \
                     "words, while at least 4 words were expected" % \
                      ( i+1, sta_file, len(sta_buf[i].split()) ) ) 
             exit  ( 29 )
        if ( k_sta > 0 ):
             if ( sta_buf[i].split()[0] in c_sta):
                  print ( "ERROR: station name %s in line %d of the input station " \
                          "file %s has already been defined" % \
                          ( sta_buf[i].split()[0], i+1, sta_file ) ) 
                  exit  ( 30 )
  
        k_sta = k_sta + 1             
        c_sta.append ( sta_buf[i].split()[0] )
        try:
             coo = []
             coo.append ( float(sta_buf[i].split()[1]) )
             coo.append ( float(sta_buf[i].split()[2]) )
             coo.append ( float(sta_buf[i].split()[3]) )
        except:
             print ( "ERROR: Error in parsing line %d of the input station " \
                     "file %s -- station coordinates should be float numbers" % \
                     ( i+1, sta_file ) ) 
             exit  ( 31 )

        radsq = coo[0]**2 + coo[1]**2 + coo[2]**2
        if ( radsq < (6378000. - 30000.0)**2 ):
             print ( "ERROR: Error in parsing line %d of the input station " \
                     "file %s -- station %s is located under the Earth's surface" % \
                     ( i+1, sta_file,sta_buf[i].split()[0]  ) ) 
             exit  ( 32 )

        if ( radsq > (6378000. + 40000.0)**2 ):
             print ( "ERROR: Error in parsing line %d of the input station " \
                     "file %s -- station %s is located well above the Earth's surface" % \
                     ( i+1, sta_file, sta_buf[i].split()[0]  ) ) 
             exit  ( 33 )

    if ( verb > 1 ): 
         print ( "%d stations were found in the input file %s" % (k_sta, sta_file) )
                              
#
# --- Check whether MALO ondeamd is running 
#
    com = "wget -q -O - http://" + host_name + "/cgi-bin/check_malo_ond.py"
    if ( verb > 2 ): print ( "About to execute command ", com )
    (ret, out) = exe ( com )
    if ( not ret == 0 ):
         print ( "ERROR: failed to learn whether MALO ondemand " + \
                 "deamon is running on server", host_name )
         if ( verb > 2 ): print ( "ret:    ", ret )
         if ( verb > 2 ): print ( "Answer: ", out )
         exit  ( 34 )
    if ( out[0].find("MALO ondemand is running") < 0 ):
         print ( "ERROR: MALO ondemand deamon is not running on server %s . " \
                 "Please try 5-15 minutes later. You can try alternative " \
                 "server. If the deamon is still not running after 15 minutes, " \
                 "please notify the server maintainer" % host_name )
         exit  ( 35 )
#
# --- Set the bouindary for the post-file
#
    boundary = "-----------------------------" + \
               ("%17.15f%17.15f" % ( random.random(), random.random() )).replace("0.","")

#
# --- Generate the context of the request file with parameters that 
# --- conform HTTP specifications for POST method
#
    f = open ( tmp_fil, 'w' )

#
# --- Model name
#
    print  ( boundary, file=f )
    print  ( 'Content-Disposition: form-data; name="model"\n',  file=f )
    print  ( model.replace(":","_"), file=f )

#
# --- Loading mode
#
    print  ( boundary, file=f )
    print  ( 'Content-Disposition: form-data; name="mode"\n',   file=f )
    print  ( type.replace("time_","").replace("_variation",""), file=f )

#
# --- Loading frame
#
    print  ( boundary, file=f )
    print  ( 'Content-Disposition: form-data; name="frame"\n',   file=f )
    print  ( frame.replace("_term_only",""), file=f )

#
# --- Start date
#
    print  ( boundary, file=f )
    print  ( 'Content-Disposition: form-data; name="start_date"\n',   file=f )
    print  ( start_time, file=f )

#
# --- Stop date
#
    print  ( boundary, file=f )
    print  ( 'Content-Disposition: form-data; name="stop_date"\n',   file=f )
    print  ( stop_time, file=f )

#
# --- Station file
#
    print  ( boundary, file=f )
    print  ( 'Content-Disposition: form-data; name="malo_input_file"; filename="%s"\n' % \
              sta_file, file=f )
    for i in range(0,len(sta_buf)):
        print  ( sta_buf[i].replace("\n",""), file=f )
       
#
# --- Empty email field
#
    print  ( boundary, file=f )
    print  ( 'Content-Disposition: form-data; name="email"\n',   file=f )
    print  ( "", file=f )
       
#
# --- Service field
#
    print  ( boundary, file=f )
    print  ( 'Content-Disposition: form-data; name="service"\n',   file=f )
    print  ( "get_loading_ondemand", file=f )

#
# - Submit field
#
    print  ( boundary, file=f )
    print  ( 'Content-Disposition: form-data; name="submit"\n',   file=f )
    print  ( "Submit", file=f )

    print  ( boundary, file=f )

    f.close()

    if ( verb > 3 ):
#
# ------ Print contents of the post-file
#
         print ( "post-file: \n" )
         with open( tmp_fil) as f:
              post_buf = f.readlines()
         f.close()

         for i in range(0,len(post_buf)):
             print  ( post_buf[i].replace("\n","") )
         
#
# --- Send wget request to compute loading on-demand to the remote server
#
    com = 'wget -q -O - --header="Content-Type: ' + boundary + '"' + " " + \
          "--post-file " + tmp_fil + " http://" + host_name + \
          "/cgi-bin/malo_ondemand.py"

    if ( verb > 2 ): print ( "About to execute command ", com )
    (ret, out) = exe ( com )
    os.remove ( tmp_fil )

    if ( not ret == 0 ):
         print ( "ERROR: failed to run command %s on server %s " % \
                 ( com, host_name ) )
         if ( verb > 2 ): print ( "ret:    ", ret )
         if ( verb > 2 ): print ( "Answer: ", out )
         exit  ( 36 )

#
# --- Parse the output. It should give us the URL with the request
#
    req_url = ""
    for str in out:
         if ( str.find('"http:') > -1 and str.find('">') > -1 ):
              req_url = str[str.index( '"http:' )+1:str.index( '">' )]
         elif ( str.find('http:"') > -1 ):
              req_url = str.replace("\n","" ) 

    if ( req_url == "" ):
         print ( "ERROR: server %s refused to accept a request for " \
                 "loading computation due to an error" % host_name )
         for str in out:
             str = str.replace('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">','' )
             str = str.replace('<HTML LANG="ru">','' )
             str = str.replace('<HEAD>','')
             str = str.replace('<META http-equiv="Content-Type" content="text/html; charset=koi8-r">','')
             str = str.replace('</HEAD>','')
             str = str.replace('<BODY>','')
             str = str.replace('</PRE>','')
             str = str.replace('</BODY>','')
             str = str.replace('</HTML>','')
             str = str.replace('<FONT COLOR="A04030"><B> ','' )
             str = str.replace('</B></FONT>','' )

             if ( len(str) == 0 ): continue
             print ( str )

         if ( verb > 2 ):
              print ( "" )
              print ( "Verbatim server response:" )
              print ( "" )
              for str in out:
                  print ( str )
         exit ( 37 )
         

    if ( verb > 2 ): print ( "Answer: ", out )
    if ( verb > 1 ): print ( "URL: ", req_url )
    if ( verb > 0 ): 
         print ( "Request is sent to server %s on %s" % \
                (host_name, datetime.datetime.now().strftime("%Y.%m.%d_%H:%M.%S") ) )

    com =  "wget" + " " + "-q -O " + tmp_fil + " " + req_url + "/log.txt"
#
# --- Read log-file of the request and sleep repeatedly
#
    fl_accept = 0
    for i in range(0,num_tries):
        time.sleep ( sleep_time )
        (ret, out) = exe ( com )
        if ( not ret == 0 ):
             if ( verb > 2 ): print ( "com:    ", com )
             if ( verb > 2 ): print ( "ret:    ", ret )
             if ( verb > 2 ): print ( "Answer: ", out )
             print ( "ERROR: failed to communicate with server %s" % host_name )
             exit  ( 38 )
        fl_finish = 0        
          
#
# ----- Read the log conents
#
        with open( tmp_fil) as f:
             buf = f.readlines()
        f.close()
        if ( fl_accept == 0 ):
             if ( verb > 0 ): 
                  print ( "Request is accepted by server %s" % host_name )
             fl_accept = 1

#
# ----- Parse log contents. Search for lines with "finished"
#
        for j in range(0,len(buf)):
            buf[j] = buf[j].replace("\n","")
            if ( buf[j].find("MALO: finished") > -1 ): fl_finish = 1
            if ( buf[j].find("MALO_LOADING_MODEL: finished") > -1 ): fl_finish = 1
            if ( buf[j].find("Successful") > -1 ): fl_finish = 1
            if ( buf[j].find("termination") > -1 ): fl_finish = 2

        if ( verb > 0 and len(buf) > 0 ): print ( buf[len(buf)-1] )
        if ( fl_finish > 0 ): break # Finished! Hurra!
        os.remove ( tmp_fil )

#
# --- Now read the file with links to results and extract the URL of indivdual output files.
# --- We make two attempts to be sure that server is synchronized
#
    if ( fl_finish == 2 ):
         print ( "See log file %s" % tmp_fil )
         print ( "ERROR: failure in loading computation on server %s " % host_name )
         exit  ( 39 )
    else:
        os.remove ( tmp_fil )
         

    for k in range(0,2):  # We make two tries
        if ( k > 0 ): time.sleep ( sleep_time ) # Sleep for a while
#
# ----- Retreive via wget the file with links
#
        com =  "wget" + " " + "-q -O " + tmp_fil + " " + req_url + "/index.html"
        (ret, out) = exe ( com )
        if ( not ret == 0 ):
             print ( "ERROR: failed to run command %s on server %s " % \
                     ( com, host_name ) )
             exit  ( 40 )

#
# ----- Read that file
#
        with open( tmp_fil) as f:
             buf = f.readlines()
        f.close()
        os.remove ( tmp_fil )

#
# ----- And parse it
#
        url_list = []
        for j in range(0,len(buf)):
            buf[j] = buf[j].replace("\n","").replace('"',' ')
#
# --------- Search for the link
#
            if ( buf[j].find("<LI>  <A HREF=") > -1 ):
#
# -------------- We found it. Wonderful! Put it in the list with URLs
#
                 if ( verb > 1 ): print ( "URL: ", buf[j].split()[3] )
                 url_list.append ( buf[j].split()[3] )             
        if ( len(url_list) > 0 ): break

    if ( len(url_list) == 0 ):
         print ( "ERROR: request was not processed in full due to an error" )
         for j in range(0,len(buf)):
             buf[j] = buf[j].replace('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">','' )
             buf[j] = buf[j].replace('<HTML LANG="ru">','' )
             buf[j] = buf[j].replace('<HEAD>','')
             buf[j] = buf[j].replace('<META http-equiv="Content-Type" content="text/html; charset=koi8-r">','')
             buf[j] = buf[j].replace('</HEAD>','')
             buf[j] = buf[j].replace('<BODY>','')
             buf[j] = buf[j].replace('</PRE>','')
             buf[j] = buf[j].replace('</BODY>','')
             buf[j] = buf[j].replace('</HTML>','')
             buf[j] = buf[j].replace('<FONT COLOR="A04030"><B> ','' )
             buf[j] = buf[j].replace('</B></FONT>','' )
             print ( buf[j] )
         exit ( 41 )
         
#
# --- Now retrieve indvidual files using wget
#
    for i in range(0,len(url_list)):
        id = url_list[i].rfind("/")
        file_name = url_list[i][id+1:] # filename part of the output file
#
# ----- Execute weget command
#
        com = "wget" + " " + "-q -O " + output_pref + file_name + " " + \
              url_list[i]
        (ret, out) = exe ( com )
        if ( verb > 2 ): print ( "About to run command ", com )
        if ( not ret == 0 ):
             print ( "ERROR: failed to run command %s" % com )
             for j in range(0,len(out)):
                 print ( out[j] )
                 exit  ( 42 )
        if ( verb > 0 ): print ( "Downloaded file: " + output_pref + file_name )
    return 0

#
# ------------------------------------------------------------------------
#
def main():
    
    opts = optparse.OptionParser( version=version__label )

    opts.add_option ( "-s", "--station-file", action="store", \
                      dest="sta_file", \
                      metavar="NAME", \
                      help="Name of the station file. Each line of the station " \
                           "file has four words seaprated by one more blanks and " \
                           "defines a station. The first word is the station name " + \
                           "no longer than 8 charactgers. The 2nd, 3d, and 4th words " + \
                           "are X, Y, and Z Carthesian coordinatges of the station " + \
                           "position in a crist fixed coordinate system, f.e. ITRF " + \
                           "in meters." \
                    )

    opts.add_option ( "-H", "--host-name", action="store", \
                      dest="host_name", \
                      metavar="NAME", \
                      help="Domain name of the International " + \
                           "Mass Loading Service host server." )

    opts.add_option ( "-b", "--start-time", action="store", \
                      dest="start_time", \
                      metavar="NAME", \
                      help="Tai start time of the interval for mass loading. " \
                           "Format YYYY.MM.DD_hh:mm:ss.s" )

    opts.add_option ( "-e", "--stop-time", action="store", \
                      dest="stop_time", \
                      metavar="NAME", \
                      help="Tai stop time of the interval for mass loading. " \
                           "Format YYYY.MM.DD_hh:mm:ss.s" )

    opts.add_option ( "-o", "--output-file-prefix", action="store", \
                      dest="output_pref", \
                      metavar="NAME", \
                      help="Prefix of output files. The output file name has two parts: " \
                           "the prefix defined in this option and suffix that contains " \
                           "the product name and the date of the loading. If the prefix " \
                           "has the trailing \, then the output file name will have " \
                           "only a server-generated suffix and will be placed in the " \
                           "directory defined in this parameters. Usually, the prefix contains " \
                           "both the direcoty name and additional characters that will be " \
                           "concatenated with the suffix" )

    opts.add_option ( "-m", "--model", action="store", \
                      dest="model", \
                      metavar="NAME", \
                      help="Model type. Has the two parts separated by column: fluid name " \
                           "and the name of the data product. Supported data types: %s" % \
                      model_names )

    opts.add_option ( "-t", "--type", action="store", \
                      dest="type", \
                      metavar="NAME", \
                      help="Data type. Supported data types: %s" % \
                            model_types )

    opts.add_option ( "-f", "--frame", action="store", \
                      dest="frame", \
                      metavar="NAME", \
                      help="Frame type. It controls the use degree 1 term. "\
                            "Supported frame types: %s" % \
                      frame_types )

    opts.add_option ( "-l", "--log-file", action="store", \
                      dest="log", \
                      metavar="NAME", \
                      default="-", \
                      help="Log file" )

    opts.add_option ( "-v", "--verbosity", action="store", \
                      dest="verb", \
                      metavar="NAME", \
                      type="int", \
                      default=0, \
                      help="Verbosity level. 0 -- silent, 1 -- info messages; 2 -- debugging" )

#
# --- Get and parse options
#
    opts, args = opts.parse_args()
    
#
# --- Check option values
#
    if ( opts.host_name == None ):
         print ( "Host name is not specified. Try get_loading_ondemand.py -h to see options" )
         exit  ( 1 )

    if ( opts.sta_file == None ):
         print ( "Station file is not specified. Try get_loading_ondemand.py -h to see options" )
         exit  ( 2 )

    if ( opts.model == None ):
         print ( "Model name is not specified. Try get_loading_ondemand.py -h to see options" )
         exit  ( 3 )
  
    if ( not opts.model in model_names ):
         print ( "Unsupported model name. Supported model names are ", model_names )
         exit  ( 4 )
  
    if ( not opts.type in model_types ):
         print ( "Unsupported model type. Supported model types are ", model_types )
         exit  ( 5 )

    if ( opts.start_time == None ):
         if ( opts.model_type == "time_series" ):
              print ( "Start time is not specified. Try get_loading_ondemand.py -h to see options" )
              exit  ( 6 )
         else:
              opts.start_time = "2000.01.01"

    if ( opts.stop_time == None ):
         if ( opts.model_type == "time_series" ):
              print ( "Stop time is not specified. Try get_loading_ondemand.py -h to see options" )
              exit  ( 7 )
         else:
              opts.stop_time = "2000.01.02"

    if ( not ( opts.start_time == "check" or opts.stop_time == "check" ) ):
         if ( opts.output_pref == None ):
              print ( "Output file prefix time is not specified. Try get_loading_ondemand.py -h to see options" )
              exit  ( 8 )

         if ( not opts.frame in frame_types ):
              print ( "Unsupported frame type. Supported frame types are ", frame_types )
              exit  ( 9 )

    get_loading_ondemand ( opts.sta_file, opts.host_name, \
                           opts.start_time, opts.stop_time, \
                           opts.model, opts.type, opts.frame, \
                           opts.output_pref, opts.log, opts.verb )
    exit ( 0 )

if __name__ == "__main__":
    vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    main()
