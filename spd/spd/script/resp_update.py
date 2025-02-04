#!/usr/bin/env python
"""
# ************************************************************************
# *                                                                      *
# *   
# *                                                                      *
# *  ### 21-JUN-2014 resp_update.py v1.0 (c)  L. Petrov 25-JUN-2014 ###  *
# *                                                                      *
# ************************************************************************
"""
import sys, os, shutil, time, subprocess
import locale
import optparse 
import math
import datetime
import codecs
from   Message  import Message
from   datetime import date, datetime, time, timedelta
resp_update__label = "resp_update.py  v 1.2 of 2016.08.22"
fmt__label      = "# RESP_CONFIG file.  Format of 2016.08.22"
config__num_par = 23 # The number of configuration parameters
locale.setlocale ( locale.LC_ALL, 'ru_RU')

class resp_config_class:
   def __init__ ( self, filename ):
       self.filename       = filename
#
       self.heb_dir        = None
       self.begin_date     = None
       self.look_back_days = None
       self.end_date       = None
       self.pivot_sds      = []
       self.time_step      = None
       self.step_ahead     = None
       self.url_template   = None
       self.geos_temp_dir  = None
       self.geos_heb_dir   = None
       self.resp_dir       = None
       self.geos_dir       = None
       self.to_heb_exe     = None
       self.to_resp_exe    = None
       self.oh_fil         = None
       self.geoid_fil      = None
       self.wget_com       = None
       self.compress_com   = None
#
       self.num_cpu        = None
       self.err_file       = None
       self.suc_file       = None
       self.lock_file      = None
       self.lock_timeout   = None

       self.date_list      = []

   def init ( self ):
       __init__ ( self )

#
# ------------------------------------------------------------------------
#
def parse_spd_oper_config ( config ):
#"""
#    Reads the configuration file which has format KEYWORD: VALUE 
#    and puts parsed information into fields of class config
#"""
   with open ( config.filename  ) as f:
        conf_buf = f.readlines()
   f.close ( )

   if ( conf_buf[0][0:len(fmt__label)] != fmt__label[0:len(fmt__label)] ):
        print ( "Unsupported format of config file " + config.filename + \
                "\n Format label found:   " + conf_buf[0] + \
                "\n While expected label: " + fmt__label + "\n" )
        exit ( 1 )

   num_par = 0
   for line in conf_buf:
       if ( line == fmt__label ): continue
       if   ( line.split()[0]       == "#" ): continue
       if   ( line.split()[0]       == "heb_dir:"  ): 
              config.heb_dir         = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]       == "begin_date:"    ):
              config.begin_date      = line.split()[1]  
              num_par = num_par + 1
       elif ( line.split()[0]       == "look_back_days:" ):
              config.look_back_days  = int(line.split()[1])
              num_par = num_par + 1
       elif ( line.split()[0]       == "end_date:" ):
              config.end_date        = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "pivot_sds:" ):
              num_pivots = len(line.split()) - 1
              for i in range(0,num_pivots):
                  config.pivot_sds.append ( line.split()[i+1] )
              config.pivot_sds       = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]       == "time_step:" ):
              config.time_step       = int(line.split()[1])
              num_par = num_par + 1
       elif ( line.split()[0]       == "step_ahead:" ):
              config.step_ahead      = int(line.split()[1])
              num_par = num_par + 1
       elif ( line.split()[0]       == "url_template:" ):
              config.url_template    = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]       == "geos_temp_dir:" ):
              config.geos_temp_dir   = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]       == "geos_heb_dir:" ):
              config.geos_heb_dir    = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]       == "resp_dir:" ):
              config.resp_dir        = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]       == "geos_dir:" ):
              config.geos_dir        = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]       == "to_heb_exe:" ):
              config.to_heb_exe      = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]       == "to_resp_exe:" ):
              config.to_resp_exe     = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]       == "oh_fil:" ):
              config.oh_fil          = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]       == "geoid_fil:" ):
              config.geoid_fil       = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]       == "wget_com:" ):
              config.wget_com        = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]       == "compress_com:" ):
              config.compress_com    = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]       == "num_cpu:" ):
              config.num_cpu         = int(line.split()[1])
              num_par = num_par + 1
       elif ( line.split()[0]       == "log_file:" ):
              config.log_file        = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]       == "err_file:" ):
              config.err_file        = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]       == "suc_file:" ):
              config.suc_file        = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]       == "lock_file:" ):
              config.lock_file       = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]       == "lock_timeout:" ):
              config.lock_timeout    = float(line.split()[1])
              num_par = num_par + 1
       else:
              print ( "Unrecognized keyword " + line.split()[0] + \
                      " in control file " + config.filename )
              exit ( 1 )
   if ( num_par < config__num_par ):
        print ( "Not all keywords were foudnd in in control file " + \
                 config.filename + " -- only %d, though %d were expected" % \
                 ( num_par, config__num_par ) )
        exit ( 1 )
#
# ------------------------------------------------------------------------
#
def print_log ( config, str ):
#"""
#   print string str into both log-file specified in config file and to  
#   stdout
#"""
    now = datetime.now() 
    
    print ( "resp_update: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str )
    sys.stdout.flush()
#
    config.log_file_handle = open ( config.log_file, "a" )
    print ( "resp_update: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str, \
            file=config.log_file_handle )
    config.log_file_handle.flush()
    config.log_file_handle.close()
#
# ------------------------------------------------------------------------
#
def print_suc ( config, str ):
#"""
#   print string str into suc-file specified in the config file
#"""
    now = datetime.now() 
    
#
    suc_file_handle = open ( config.suc_file, "w" )
    print ( "resp_update: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str, \
            file=suc_file_handle )
    suc_file_handle.flush()
    suc_file_handle.close()
#
# ------------------------------------------------------------------------
#
def print_err ( config, str ):
#"""
#   print string str into err-file specified in the config file
#"""
    now = datetime.now() 
    
#
    err_file_handle = open ( config.err_file, "w" )
    print ( "resp_update: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str, \
            file=err_file_handle )
    err_file_handle.flush()
    err_file_handle.close()

#
# ------------------------------------------------------------------------
#
def exe ( command, log_buf):
#"""
#    Spawn a supborcess, execute command in the context of the suborpess,
#    wait for its completion and return completion code and results.
#"""
    words = command.split()
    time_str = str(datetime.now().strftime("%Y.%m.%d_%H:%M:%S.") + "%6d" % datetime.now().microsecond).replace( " ", "0" )
    log_buf.append ( "@@@ " )
    log_buf.append ( "@@@ Running command " + command + " @@@" )
    log_buf.append ( "@@@ " + time_str )
    log_buf.append ( "@@@ "            )
    (ret, out) = subprocess.getstatusoutput ( command )
    log_buf.append ( out )
    return ( ret, out.split ( "\n" ) )

#
# ------------------------------------------------------------------------
#
def check_lock ( config ):
#"""
#    Procedure check_lock checks the lock file. Its name is specified
#    in config file. If lock file exist, we read it and extracts PID 
#    of the owner process and the date when the lock file has been
#    created. We check whether the owner process is running.
#    If its running, we check for how long. If less than lock_timeout
#    (specified in config file), we quit. In all other cases we
#    create new lock file and put there current PID and the current time
#"""
    if ( os.path.isfile(config.lock_file) ):
#
# ------ Read lock file and extract from the PID and start time of
# ------ the locking process
#
         with open ( config.lock_file ) as f:
              lines= f.readline()
         try:
              (pid, time) = lines.split( "\n")[0].split( " " ) 
         except:
#
# --------- It may happen the file is spoiled and we cannot read it correctly
#
            pid = 1 # process always runs, but cannot be killed
            time = "2010.04.19_12:08:13" # When Milan was born
         f.close()
#
# ------ Check whether the process is running
#
         try:
             os.kill(int(pid), 0)
             process_running = "yes"
         except:
             process_running = "no"

         if ( process_running == "yes" ):
#
# ----------- Check how long the process has been running
#
              time_tag = datetime.strptime ( time, "%Y.%m.%d_%H:%M:%S" )
              delta_since = datetime.now() - time_tag
              time_since = delta_since.days*86400.0 + delta_since.seconds
              if ( time_since < config.lock_timeout ):
#
# ---------------- The process running for less than it is defiend in config.lock_timeout
# ---------------- Let us quit quietly and let it finish
#
                   print ( "%s Another process get_geos_oper, pid %s is still running for %d seconds" % \
                           ( datetime.now().strftime("%Y.%m.%d_%H:%M:%S"), \
                             pid, int(time_since) ) \
                         )              
                   exit ( 0 )

#
# --- Create lock file and write there the PID of the current process
# --- and the current time
#
    lock = open ( config.lock_file, "w" )
    now = datetime.now() 
    print ( "%05d %s" % (os.getpid(), now.strftime("%Y.%m.%d_%H:%M:%S")), \
             file=lock )
    lock.close()
#
# ------------------------------------------------------------------------
#
def check_gap ( config, fil_resp_list, ivrb ):

#
# -- This routine checks gaps in spd-files. If it finds gaps, 
# -- it creates missing spd-files with slant path delays
#
    if ( len(fil_resp_list) < 2 ):
         return 0

    il = len(fil_resp_list[len(fil_resp_list)-1])
    date1 = fil_resp_list[0][il-17:il-4]
    date2 = fil_resp_list[1][il-17:il-4]

#
# - Find the time step
#
    time_step = datetime.strptime ( date2, "%Y%m%d_%H%M" ) - \
                datetime.strptime ( date1, "%Y%m%d_%H%M" )

    date_last = datetime.strptime ( date2, "%Y%m%d_%H%M"  )
    filmis_list = []
    for i in range(2,len(fil_resp_list)):
#
# ----- Extract the substring with data embedde in file name
#
        date_file = fil_resp_list[i][il-17:il-4]

        for k in range(1,len(fil_resp_list)):
#
# --------- Predict the date of the next file by adding the time step
#
            date_pred = date_last + k*time_step
            if ( date_pred.strftime("%Y%m%d_%H%M") < date_file ):
#
# -------------- Wow! We have detected a gap!
#
# -------------- Generate the name of missed spd-fle
#
                 fil_resp_miss = fil_resp_list[i][0:il-17] + \
                            date_pred.strftime("%Y%m%d_%H%M") + \
                            fil_resp_list[i][il-4:il] 
#
# -------------- Generate the name of heb file that corresponds to the this spd-file
#
                 heb_file = config.heb_dir + "/" + date_pred.strftime("%Y") + \
                            "/" + config.pivot_name + "/" + config.pivot_name + \
                            "_" + date_pred.strftime("%Y%m%d_%H%M") + \
                            ".heb.bz2"
                 if ( not os.path.isfile(heb_file) ): 
                      heb_file = heb_file + ".bz2"

                 if ( not os.path.isfile(heb_file) ):
#
# ------------------- heb-file does not exist. This sucks...
#
                      print     ( "check_gap: cannot file heb_file " + heb_file )
                      print_err ( "check_gap: cannot file heb_file " + heb_file )
                      exit ( 1 )

                 if ( ivrb > 2 ): print ( "Gap: ", fil_resp_miss, " heb_file= ", heb_file )

#
# -------------- Generate command line for computing missing path delay
#
                 com = config.spd_3d_bin + " " + \
                       config.spd_conf   + " " + \
                       heb_file + " " + \
                       config.spd_dir    + "/" + config.spd_pref + \
                       " 2 "
                 if ( ivrb > 2 ): print ( com )

                 ret = exe ( com, log_buf  )
                 if ( ret != 0 ):
                      print ( "ERROR in running command ", com )
                      print ( "Returning code: ", ret )
                      print_err ( config, "Error in computing slant path delay" )
                      exit ( 1 )
#
# -------------- Append the list of missed files
#
                 filmis_list.append ( fil_resp_miss )
            else:
                 break

        date_last = date_pred
        datetime.strptime ( date_file , "%Y%m%d_%H%M"  )

    if ( len(filmis_list) > 0 ):
         for i in range(0,len(filmis_list)-1):
             fil_resp_list.append ( filmis_list[i] )

    fil_resp_list.sort()    
    return fil_resp_list

#
# ------------------------------------------------------------------------
#
def geosfpit_resp_oper ( config, run_level, ivrb, log_buf ):
#"""
#  This is the main routine for processing remote geos data
#"""
#
# --- Check lock file, whether another get_geos_oper process is running
#
    check_lock ( config )

    config.log_file_handle = open ( config.log_file, "a" )
    print ( "# ========================================", \
            file=config.log_file_handle )
    config.log_file_handle.close()
#
    print ( "       " )
    print ( "==============================================" )
    print_log ( config, "started" )

#
# --- Get the begin date: config.look_back_days before the present
#
    time_begin  = datetime.now() - timedelta(days=config.look_back_days)
    fidat_begin = str(time_begin.strftime("%Y%m%d")).replace( " ", "0" ) + \
                  "_0000"
#
# --- Transform stop date into format parsable by python
#
    fidat_end   = config.end_date[0:4] + config.end_date[5:7] + \
                  config.end_date[8:13] + config.end_date[14:16] 
    if ( ivrb > 1 ): print ( "fidat_begin= ", fidat_begin, " fidat_end= ", fidat_end ) 
#
# --- Walk through the directory and generate the list of existing data in heb 
# --- format for the so-called pivotal dataset within the data range specified
# --- in the configuration file
#
    finam_list = []
    for paths, dirs, files in os.walk(config.heb_dir):
        for k in range(0,len(files)):
            name = paths + "/" + files[k]
            ih = name.rfind ( '.heb' )
            if ( ih == 0 ): ih = name.rfind ( '.heb.bz2' )
            if ( ih > 0 and name.find ( config.pivot_sds[0] + "/" + config.pivot_sds[0] + "_" ) > 1 ):
                 fidat = name[ih-13:ih]
                 if ( fidat >= fidat_begin and \
                      fidat <= fidat_end       ):
                      finam_list.append(name)

    if ( ivrb > 1 ): print ( "len(finam_list): ", len(finam_list) )
    if ( len(finam_list) > 0 ):
#
# ------ Sort this list. Sorting in alphabetic order = sorting in the chronological order
#
         finam_list.sort()
#
# ------ Check whether there is a gap between the data
#
         for i in range(1,len(finam_list)):
             ih = finam_list[i].rfind ( '.heb' )
#
# ---------- Extract date/time from the file name
#
             fidat_last = datetime.strptime ( finam_list[i-1][ih-13:ih], "%Y%m%d_%H%M")
             fidat      = datetime.strptime ( finam_list[i][ih-13:ih],   "%Y%m%d_%H%M")
             if ( ivrb > 2 ): print ( "fidat: ", str(fidat) )
#
# ---------- Compare the date of the current file and the previous one
#
             ddat       = fidat - fidat_last
             ddat_sec   = ddat.days*86400.0 + ddat.seconds
             if ( ddat_sec > config.time_step ):
#
# --------------- The gap exceeds the limit
#
                  num_missed = int(ddat_sec/config.time_step - 1)  
                  for k in range(0,num_missed):
#
# ------------------- Store dates of missed data
#
                      new_date = fidat_last + timedelta (seconds=(k+1)*config.time_step )
                      config.date_list.append ( new_date.strftime("%Y%m%d_%H%M") )
                      
#
# --- fidat_last is the date of the last heb-file.
# --- If no heb-files are present, then the nominal start date specified
# --- in the control file will be used
#
    if ( len(finam_list) > 0 ): 
         ih = finam_list[len(finam_list)-1].rfind ( '.heb' )
         fidat_last = datetime.strptime ( finam_list[len(finam_list)-1][ih-13:ih], \
                                                   "%Y%m%d_%H%M")
    else:
         fidat_last  = datetime.strptime ( fidat_begin, "%Y%m%d_%H%M") - timedelta ( seconds=config.time_step )
    if ( ivrb > 1 ): print ( "fidat_last: ", str(fidat_last) )
#
# --- In this contents 'now' may be in the future. 
# --- This is the date through which we will be looking for data
#
    now = datetime.now() + timedelta ( seconds=config.step_ahead*config.time_step )
    if ( ivrb > 2 ): print ( "now-1: " + now.strftime("%Y%m%d_%H%M") + " fidat_end= " + fidat_end )
    if ( now.strftime("%Y%m%d_%H%M") > fidat_end ):
         now = datetime.strptime ( fidat_end, "%Y%m%d_%H%M" )
    if ( ivrb > 2 ): print ( "now-2: " + now.strftime("%Y%m%d_%H%M") + " fidat_end= " + fidat_end )
#
# --- Add the dates of epochs of geos files which are after the last
# --- date of geos data found in the local system, but before the date
# --- throiugh which we are looking for the data
#
    for k in range(1,32768):
        new_date = fidat_last + timedelta (seconds=k*config.time_step )
        if ( new_date.strftime("%Y%m%d_%H%M") < now.strftime("%Y%m%d_%H%M") ):
             config.date_list.append ( new_date.strftime("%Y%m%d_%H%M") )

    if ( len(config.date_list) == 0 ):
         print_log  ( config, "No heb-files are to be fetched, " + \
                              "since they are all up to date" )
         exit ( 0 )

#
# --- Sorting dates
#
    config.date_list.sort()

#
# --- Generate the list of URLs using the template defined in the control file
#
    url_list = []
    for i in range (0,len(config.date_list)):
        url = config.url_template
        iy  = url.find ( "Y@@@@" )
        if ( iy > 1 ):
             url = url.replace ( config.url_template[iy:iy+5], \
                                 "Y" + config.date_list[i][0:4] )
        else:
             iy  = url.find ( "Y@@@" )
             if ( iy > 1 ):
                  url = url.replace ( config.url_template[iy:iy+4], \
                                      config.date_list[i][0:4] )

        iy  = url.find ( "Y%%%" )
        if ( iy > 1 ):
             url = url.replace ( config.url_template[iy:iy+4], \
                                 config.date_list[i][0:4] )
                  
        im  = url.find ( "M@@" )
        if ( im > 1 ):
             url = url.replace ( config.url_template[im:im+3], \
                                 "M" + config.date_list[i][4:6] )
        id  = url.find ( "D@@" )
        if ( id > 1 ):
             url = url.replace ( config.url_template[id:id+3], \
                                 "D" + config.date_list[i][6:8] )

        id  = url.find ( "D%%" )
        if ( id > 0 ):
             doy = datetime.strptime(config.date_list[i],"%Y%m%d_%H%M").timetuple().tm_yday
             url = url.replace ( config.url_template[id:id+3], \
                                 "%03d" % doy )

        it  = url.find ( "T@@@@@@@@@@@@" )
        if ( it > 1 ):
             url = url.replace ( config.url_template[it:it+13], \
                                 config.date_list[i] )
        if ( ivrb > 3 ): print ( "url: ", url ) 
        url_list.append(url)

#
# --- Now check whether these URL are present in the remote server
#
    if ( ivrb > 2 ): print ( "config.date_list = ", config.date_list )
    if ( ivrb > 2 ): print ( "url_list[0]= ", url_list[0] )
#
# --- Now cycle over the list of urls and fetch the data.
# --- Fetched data are processed
#
    log_buf = []
    n_fil = 0
    last_proc_date = "n/a"
    for i in range (0,len(url_list)):
        id = url_list[i].rfind("/")
        print_log ( config, "file %d (%d) %s" % (i+1, len(url_list), \
                    url_list[i][id+1:] + " is being processed" ) )
        (ret, proc_date) = fetch_geos_resp ( config, url_list[i], run_level, \
                                             ivrb, log_buf )
        n_fil = n_fil + ret
        if ( proc_date != "n/a" ):
             last_proc_date = proc_date

#
# --- Uhhh! We finished!
#

    print_log ( config, "finished" )
    if ( n_fil > 0 ):
         print_suc ( config, "Number of files processed: %d. Last file for date %s" % \
                   ( n_fil, last_proc_date ) )
    exit ( 0 )

#
# ===========================================================================
#
def fetch_geos_resp ( config, url, run_level, ivrb, log_buf):
#"""
#    This procedure:
#    1) fetches the data using wget
#    2) extracts the sought datasets converts them into heb-format
#    3) moves ofriginal heb-data into the storage archive
#    4) proces heb files: compute refractivity field, expand
#"""

    year  = url[len(url)-21:len(url)-17]
    month = url[len(url)-17:len(url)-15]

#
# --- Fetch the data into the temporary directory config.geos_temp_dir 
#
    orig_url = url
    id = orig_url.rfind ( "/" ) 
    for i in range (0,len(config.pivot_sds)):
        url = orig_url
        if ( i > 0 ):
             url = orig_url
             url = orig_url[0:id-1] + config.pivot_sds[i] + "/" + \
                   config.pivot_sds[i] + orig_url[id+2:] 

        wget_com = "cd " + \
             config.geos_temp_dir + " ; " + \
             config.wget_com + " --timeout=30 --tries=64 --retry-connrefused -nH --cut-dirs=8 -c --ftp-password=\"\" " + \
             url

        log_buf = []
        (ret, out) = exe ( wget_com, log_buf )
        if ( ret != 0 ):
             if ( ivrb > 2 ): print_log ( config, "wget error: %s " % out )
             return (0, "n/a:" ) ;

        if ( ivrb > 1 ): 
             print_log ( config, "Successfully executed command: %s" % wget_com )
        id = url.rfind("/")
        print_log ( config, "  Processing file " + url[id+1:] )
    
#
# ----- If necessary, create the output directory
#
        geo_finam = config.geos_temp_dir + url[id:]
        if ( not os.path.isdir(config.geos_heb_dir + "/" + year) ):
             os.mkdir ( config.geos_heb_dir + "/" + year )

#
# --- Process the fetched file: extracts the requested SDSs and 
# --- convert them into heb-format
#
    last_date = resp_geos_process ( config, geo_finam, run_level, ivrb, log_buf )

    if ( config.geos_dir == "/dev/null" or config.geos_dir == "NONE" or config.geos_dir == "blank" ):
#
# ------ Remove the the original datafile
#
         os.unlink ( config.geos_temp_dir + url[id:] )
    else:
#
# ------ Move the original data into the archive directory
#
         shutil.copy2 ( config.geos_temp_dir + url[id:],
                        config.geos_dir + url[id:] )
         os.unlink    ( config.geos_temp_dir + url[id:] )

    return (1, last_date)

#
# ===========================================================================
#
def resp_geos_process ( config, finam, run_level, ivrb, log_buf):

    Message ( "I2", "geos_orig_file: %s" % finam )
    if ( run_level == 0 or run_level == 11 or run_level == 1 ):
         if ( run_level == 1 ):
              geos_finam = finam
         elif ( run_level == 11 ):
              geos_finam = finam
         else:
              geos_finam = finam
         year = finam[len(finam)-21:len(finam)-17]

         for i in range (0,len(config.pivot_sds)):
             if ( config.to_heb_exe == "cp" ): 
                  id = geos_finam.rfind("/")
                  finam = geos_finam[0:id+1] + config.pivot_sds[i] + \
                          geos_finam[id+2:] 
                  if ( ivrb > 2 ): print_log ( config, "finam: " + finam ) 
                  to_heb_com = config.to_heb_exe   + " " + \
                               finam               + " " + \
                               config.geos_heb_dir + "/" + year + "/" + \
                               config.pivot_sds[i] + "/"

             else:
                  to_heb_com = config.to_heb_exe + " " + \
                               geos_finam        + " " + \
                               config.geos_heb_dir + "/" + year + " " + \
                               config.compress_com
                                                  
             if ( ivrb > 1 ): print ( "to_heb_com: ", to_heb_com ) 
             if ( ivrb > 1 ): print_log ( config, "to_heb_com: " + to_heb_com ) 
         
             (ret, out) = exe ( to_heb_com, log_buf )
             if ( ret != 0 ):
                  Message ( "E", "Error an attempt to extract the SDS(s) " + \
                                 "and write it(them) down in HEB-format" )
                  print ( out )
                  print_err ( config, "Error an attempt to extract the SDS(s) " + \
                                      "and write it(them) down in HEB-format" )
                  print_err ( config, out )
                  exit ( 1 )

         if ( config.time_step == 3600 ):
#
# ----------- In a case of 1 hourly data we process data every 3 hours:
# ----------- 00:00, 03:00, 06:00, 09:00, 12:00, 15:00, 18:00, 21:00 or
# ----------- 00:30, 03:30, 06:30, 09:30, 12:30, 15:30, 18:30, 21:30
#
              if ( geos_finam.rfind("_0100.") > 0 or \
                   geos_finam.rfind("_0130.") > 0 or \
                   geos_finam.rfind("_0200.") > 0 or \
                   geos_finam.rfind("_0230.") > 0 or \
                   geos_finam.rfind("_0400.") > 0 or \
                   geos_finam.rfind("_0430.") > 0 or \
                   geos_finam.rfind("_0500.") > 0 or \
                   geos_finam.rfind("_0530.") > 0 or \
                   geos_finam.rfind("_0700.") > 0 or \
                   geos_finam.rfind("_0730.") > 0 or \
                   geos_finam.rfind("_0800.") > 0 or \
                   geos_finam.rfind("_0830.") > 0 or \
                   geos_finam.rfind("_1000.") > 0 or \
                   geos_finam.rfind("_1030.") > 0 or \
                   geos_finam.rfind("_1100.") > 0 or \
                   geos_finam.rfind("_1130.") > 0 or \
                   geos_finam.rfind("_1300.") > 0 or \
                   geos_finam.rfind("_1330.") > 0 or \
                   geos_finam.rfind("_1400.") > 0 or \
                   geos_finam.rfind("_1430.") > 0 or \
                   geos_finam.rfind("_1600.") > 0 or \
                   geos_finam.rfind("_1630.") > 0 or \
                   geos_finam.rfind("_1700.") > 0 or \
                   geos_finam.rfind("_1730.") > 0 or \
                   geos_finam.rfind("_1900.") > 0 or \
                   geos_finam.rfind("_1930.") > 0 or \
                   geos_finam.rfind("_2000.") > 0 or \
                   geos_finam.rfind("_2030.") > 0 or \
                   geos_finam.rfind("_2200.") > 0 or \
                   geos_finam.rfind("_2230.") > 0 or \
                   geos_finam.rfind("_2300.") > 0 or \
                   geos_finam.rfind("_2330.") > 0    ):
                   Message ( "I", "Do not process further file %s " % geos_finam )
                   return ( "n/a" )

    if ( run_level == 0 or run_level == 11 or run_level == 2 ):
         if ( config.to_resp_exe == "none" ): 
              return 0
         
         if ( run_level == 2 ):
              heb_date_name = finam[finam.rfind("/")+3:finam.rfind("/")+16]
         else:
              heb_date_name = geos_finam[len(finam)-21:len(geos_finam)-8]

         com_level2 = config.to_resp_exe + " " + \
                      config.heb_dir + "/" + heb_date_name[0:4] + "/d/d_" + heb_date_name + ".heb.bz2 " + " " + \
                      config.resp_dir + " " + \
                      config.geoid_fil + " " + \
                      config.oh_fil + " " + \
                      " 2 "

         if  ( ivrb > 1 ): print ( "com_level2: " + com_level2 ) 
         (ret, out) = exe ( com_level2, log_buf )
         if ( ret != 0 ):
              Message ( "E", "Error an attempt to run program " + com_level2 )
              print_err ( config, "Error an attempt to generate " +
                                  "air refractivity B-spline expansion" )
              print ( out )
              print_err ( config, out )
              exit ( 1 )


    return ( heb_date_name )
#
# ------------------------------------------------------------------------
#
def main():
    os.environ["LANG"] = "utf-8"
    if ( sys.version[:3] < "3.0" ): 
         print ( "This script cannot run under Python-2" )
         exit ( 1 )

    opts = optparse.OptionParser( version=resp_update__label  )

    opts.add_option ( "-c", "--config", action="store", \
                      dest="config", \
                      metavar="NAME", \
                      help="Configuration file" )

    opts.add_option ( "-r", "--run-level", action="store", \
                      dest="run_level", \
                      default=0, \
                      type="int", \
                      metavar="NAME", \
                      help="Run level" )

    opts.add_option ( "-v", "--verbosity", action="store", \
                      dest="ivrb", \
                      default=0, \
                      type="int", \
                      metavar="NAME", \
                      help="Verbosity level" )

#
# --- Get and parse options
#
    opts, args = opts.parse_args()
#
# --- Check option values
#
    if ( opts.config == None ):
         print ( "Configuration file is not specied. Try get_geos_oper.py -h to see options" )
         exit ( 1 )

    if ( not os.path.isfile(opts.config) ):
         print ( "Configuration file ", opts.config, " does not exist" )
         exit ( 1 )

    config = resp_config_class ( opts.config ) 

    parse_spd_oper_config ( config ) 

    log_buf = []
    (n_fil, last_proc_date) = geosfpit_resp_oper ( config, opts.run_level, opts.ivrb, log_buf )
    if ( n_fil > 0 ):
         print_suc ( config, "Number of files processed: %d. Last file for date %s" % \
                   ( n_fil, last_proc_date ) )

if __name__ == "__main__":
    vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    main()
