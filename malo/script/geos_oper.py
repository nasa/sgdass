#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   Program for automatic processing GEOS output of numerical weather  *
# *   models.                                                            *
# *                                                                      *
# *   get_geos_oper analyzes the directory tree on a local computer      *
# *   with geos data in heb-format. It generates the list of geos data   *
# *   that are missing up to the current date, checks whether they are   *
# *   available at the remote server. It downloads those files that      *
# *   are available, parses them, converts to heb-format, compresses,    *
# *   and copies the original data to the storage directory.             *
# *                                                                      *
# * ### 27-NOV-2013   get_oper.py   v3.42 (c)  L. Petrov 30-NOV-2024 ### *
# *                                                                      *
# ************************************************************************
"""
import sys, os, shutil, time, subprocess, signal
import optparse 
import math
import datetime
from   datetime import timedelta
from   Message          import *
from   geos_oper_config import *

date_range = "1978.01.01 2049.12.31"
bds_lock_time = 100.0 # 100 seconds lock time for update of export bds data

time_dict = {"1": [""], \
             "4": ["0000", "0600", "1200", "1800"], \
             "8": ["0000", "0300", "0600", "0900", "1200", "1500", "1800", "2100"] \
            }

#
# --- Important constants
#
get_geos__label = "get_geos_oper.py  v 3.42 of 2024.11.30"

class config_class:
   def __init__ ( self, filename ):
       self.filename              = filename
       self.username              = None
       self.password              = None
       self.name                  = None
       self.pivot_sds             = []
       self.begin_date            = None
       self.look_back_days        = None
       self.end_date              = None
       self.time_step             = None
       self.url_template          = None
       self.geos_temp_dir         = None
       self.geos_heb_dir          = None
       self.geos_dir              = None
       self.spr_dir               = None
       self.to_heb_exe            = None
       self.to_love_exe           = None
       self.loading_exe           = None
       self.malo_exe              = None
       self.gen_spr_exe           = None
       self.gen_bdsp_exe          = None
       self.gmao_gh               = None
       self.malo_elev             = None
       self.love_dir              = None
       self.love_pref             = None
       self.spr_pref              = None
       self.love_wildcard         = None
       self.load_bdsp_dir         = None
       self.load_bdsp_export_dir  = None
       self.load_bdsp_sandbox_dir = None
       self.run_bdsp_export       = False
       self.load_grid_conf        = None
       self.load_list_conf        = None
       self.load_grid_dir         = None
       self.load_list_dir         = None
       self.load_grid_pref        = None
       self.load_list_pref        = None
       self.load_d1_grid_conf     = None
       self.load_d1_list_conf     = None
       self.load_d1_grid_dir      = None
       self.load_d1_list_dir      = None
       self.load_d1_grid_pref     = None
       self.load_d1_list_pref     = None
       self.load_grid_wc          = None
       self.load_list_wc          = None
       self.vgep_dir              = None
       self.vgep_pref             = None
       self.vgep_wc               = None
       self.aam_exe               = None
       self.aam_ser_exe           = None
       self.aam_igh               = None
       self.aam_ogh               = None
       self.aam_pref              = None
       self.aam_ser_file          = None
       self.compress_com          = None
       self.stop_file             = None
       self.log_file              = None
       self.log_file_handle       = None
       self.lock_file             = None
       self.lock_timeout          = None

       self.date_list             = []

   def init ( self ):
       __init__ ( self )

#
# ===========================================================================
#
def get_geos_oper ( config, run_level, ivrb ):
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
# --- Check the stop file
#
    try:
         with open ( config.stop_file ) as f:
                     lines= f.readline()
    except:
         lines=[]

    if ( len(lines) == 0 ):
         stop_line = ""
    elif ( len(lines) == 1 ):
         stop_line = lines
    else:
         stop_line = lines.split()[0]

    if ( stop_line == "stop" ):
         print ( "The service is stopped according to the stop file " + config.stop_file )
         return ( 0 )

#
# --- Get the begin date: config.look_back_days before the present
#
    time_begin  = datetime.datetime.now() - timedelta(days=config.look_back_days)
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
    for paths, dirs, files in os.walk(config.geos_heb_dir):
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
             fidat_last = datetime.datetime.strptime ( finam_list[i-1][ih-13:ih], "%Y%m%d_%H%M")
             try:
                 fidat_last = datetime.datetime.strptime ( finam_list[i-1][ih-13:ih], "%Y%m%d_%H%M")
             except:
                  Message ( "E", "Malformed file name " + finam_list[i-1] + \
                                 " in " + config.geos_heb_dir )
                  exit ( 1 )
             try:
                  fidat      = datetime.datetime.strptime ( finam_list[i][ih-13:ih],   "%Y%m%d_%H%M")
             except:
                  Message ( "E", "Malformed file name " + finam_list[i] + \
                                 " in " + config.geos_heb_dir )
                  exit ( 1 )
             if ( ivrb > 2 ): 
                  print ( "fidat: ", str(fidat), " finam: ", finam_list[i] )
#
# ---------- Compare the date of the current file and the previous one
#
             ddat       = fidat - fidat_last
             ddat_sec   = ddat.days*86400.0 + ddat.seconds
             if ( ddat_sec > config.time_step/config.epochs_per_file ):
#
# --------------- The gap exceeds the limit
#
                  num_missed = int(ddat_sec/(config.time_step/config.epochs_per_file) - 1)  
                  for k in range(0,num_missed):
#
# ------------------- Store dates of missed data
#
                      new_date = fidat_last + timedelta (seconds=(k+1)*config.time_step/config.epochs_per_file )
                      config.date_list.append ( new_date.strftime("%Y%m%d_%H%M") )
                      
#
# --- fidat_last is the date of the last heb-file.
# --- If no heb-files are present, then the nominal start date specified
# --- in the control file will be used
#
    if ( len(finam_list) > 0 ): 
         ih = finam_list[len(finam_list)-1].rfind ( '.heb' )
         fidat_last = datetime.datetime.strptime ( finam_list[len(finam_list)-1][ih-13:ih], \
                                                   "%Y%m%d_%H%M")
    else:
         fidat_last  = datetime.datetime.strptime ( fidat_begin, "%Y%m%d_%H%M") - timedelta ( seconds=config.time_step )

#
# --- In this contents 'now' may be in the future. 
# --- This is the date through which we will be looking for data
#
    if ( ivrb > 2 ): print ( "fidat_last= ", fidat_last ) # %%%%%%%%%
    now = datetime.datetime.now() + timedelta ( seconds=config.step_ahead*config.time_step )
    if ( ivrb > 2 ): print ( "now-1: " + now.strftime("%Y%m%d_%H%M") + " fidat_end= " + fidat_end )
    if ( now.strftime("%Y%m%d_%H%M") > fidat_end ):
         now = datetime.datetime.strptime ( fidat_end, "%Y%m%d_%H%M" )
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
         return ( 0 )

#
# --- Sorting dates
#
    config.date_list.sort()
    if ( ivrb > 2 ): print ( "config.date_list= ", config.date_list )

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
        im  = url.find ( "M@" )
        if ( im > 0 ):
             url = url.replace ( config.url_template[im:im+2], \
                                 config.date_list[i][4:6] )

        id  = url.find ( "D@@" )
        if ( id > 1 ):
             url = url.replace ( config.url_template[id:id+3], \
                                 "D" + config.date_list[i][6:8] )

        id  = url.find ( "D%%" )
        if ( id > 0 ):
             doy = datetime.datetime.strptime(config.date_list[i],"%Y%m%d_%H%M").timetuple().tm_yday
             url = url.replace ( config.url_template[id:id+3], \
                                 "%03d" % doy )

        it  = url.find ( "T@@@@@@@@@@@@" )
        if ( it > 1 ):
             url = url.replace ( config.url_template[it:it+13], \
                                 config.date_list[i] )

        it  = url.find ( "E@@@@@@@@@@@@@@" )
        if ( it > 1 ):
             geosit_date = config.date_list[i][0:4] + "-" + \
                           config.date_list[i][4:6] + "-" + \
                           config.date_list[i][6:8] + "T" + \
                           config.date_list[i][9:13]
             url = url.replace ( config.url_template[it-1:it+15], \
                                 geosit_date )

        it  = url.find ( "T@@@@@@@" )
        if ( it > 0 ):
             url = url.replace ( config.url_template[it:it+8], \
                                 config.date_list[i][0:8] )

        iu  = url.find ( "U@@@@@@@@@@@" )
        if ( iu > 0 ):
             repl_str = config.date_list[i][0:4] + "%03d" % \
                        datetime.datetime.strptime(config.date_list[i],"%Y%m%d_%H%M").timetuple().tm_yday + \
                        "." + config.date_list[i][9:13] 
             url = url.replace ( config.url_template[iu:iu+12], \
                                 repl_str )

        if ( ivrb > 3 ): print ( "url: ", url ) 
        url_list.append(url)

#
# --- Remove duplicates from the url_list
#
    temp_url_list = url_list    
    url_list = []
    for url in temp_url_list:
        if ( url not in url_list ):
             url_list.append ( url )
    
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
    err_url = []
    for i in range (0,len(url_list)):
        id = url_list[i].rfind("/")
        print_log ( config, "file %d (%d) %s" % (i+1, len(url_list), \
                    url_list[i][id+1:] + " is being processed" ) )
        (ret, proc_date) = fetch_geos ( config, url_list[i], run_level, \
                                        ivrb, log_buf )
        n_fil = n_fil + ret
        if ( proc_date == "n/f" ):
             continue
        elif ( proc_date == "skipped" ):
             continue
        elif ( proc_date == "n/a" ):
             err_url.append ( url_list[i] )
        else:
             last_proc_date = proc_date

#
# --- Uhhh! We finished!
#

    if ( n_fil > 0 and len(err_url) == 0 ):
         print_suc ( config, "Number of files processed: %d. Last file for date %s" % \
                   ( n_fil, last_proc_date ) )
    elif ( len(err_url) > 0 ):
         print_err ( config, "Error in processing %d input data files" % len(err_url) )
         for i in range (0,len(err_url)):
             append_err ( config, "Error in processing input url %s" % err_url[i] )

    print_log ( config, "finished" )

    return ( n_fil )

#
# ===========================================================================
#
def fetch_geos ( config, url, run_level, ivrb, log_buf):
#"""
#    This procedure:
#    1) fetches the data using wget
#    2) extracts the sought datasets converts them into heb-format
#    3) moves ofriginal heb-data into the storage archive
#"""

    if ( url.rfind("MERRA") > 0 ):
         year  = url[len(url)-12:len(url)-8]
         month = url[len(url)-8:len(url)-6]
    else:
         if ( "-" in url[len(url)-21:len(url)-17] ):
              year  = url[len(url)-23:len(url)-19]
              month = url[len(url)-18:len(url)-16]
         else:
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
             url = orig_url[0:id-1] + config.pivot_sds[i] + "/" + \
                   config.pivot_sds[i] + orig_url[id+2:] 

        wget_com = "cd " + \
             config.geos_temp_dir + \
             " ; wget --no-check-certificate --timeout=30 --tries=64 --retry-connrefused -nH --cut-dirs=12 -c"
        if ( config.password == "blank" ):
             wget_com = wget_com  + " " + "--ftp-password=\"\" " 
        else:
             wget_com = wget_com  + " --user=" + config.username + \
                                    " --http-password=" + config.password
        wget_com =  wget_com + " " + url

        log_buf = []
        if ( ivrb > 2 ): 
             print ( "wget_com: " + wget_com )
        (ret, out) = wget_exe ( wget_com, log_buf )
        if ( ret != 0 ):
             if ( ivrb == 1 or ivrb == 2 ): print ( "wget_com: " + wget_com )
             if ( ivrb > 0 ):
                  print (  "\n".join(out) )
             return (0, "n/f") ;

        if ( ivrb > 1 ): 
             print ( "Successfully exectued command: " + wget_com )
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
    last_date = geos_process ( config, geo_finam, run_level, ivrb, log_buf )

    if ( config.geos_dir == "/dev/null" or 
         config.geos_dir == "NONE"      or 
         config.geos_dir == "blank"      ):
#
# ------ Remove the the original datafile
#
         data_file = config.geos_temp_dir + url[id:] 
         if ( os.path.isfile ( data_file ) ):
              os.unlink ( data_file )
    else:
#
# ------ Move the original data into the archive directory
#
         shutil.copy2 ( config.geos_temp_dir + url[id:],
                        config.geos_dir + url[id:] )
         os.unlink    ( config.geos_temp_dir + url[id:] )

    if ( last_date == "n/a" ): 
         return (0, last_date)
    else:
         return (1, last_date)

#
# ===========================================================================
#
def geos_process ( config, finam, run_level, ivrb, log_buf):

#
# --- Get MALO share, script, bin directories
#
    malo_script_dir = os.popen("malo_inq script").read().rstrip()
    malo_share_dir  = os.popen("malo_inq share").read().rstrip()
    malo_bin_dir    = os.popen("malo_inq bin_static").read().rstrip()

    try:
         with open ( config.stop_file ) as f:
                     lines= f.readline()
    except:
         lines=[]

    if ( len(lines) == 0 ):
         stop_line = ""
    elif ( len(lines) == 1 ):
         stop_line = lines
    else:
         stop_line = lines.split()[0]

    if ( stop_line == "stop" ):
         print ( "The service is stopped according to the stop file " + config.stop_file )
         exit ( 0 )

    Message ( "I2", "geos_orig_file: %s" % finam )
    if ( run_level == 0 or run_level == 99 or run_level == 1 ):
         if ( run_level == 1 ):
              geos_finam = finam
         elif ( run_level == 99 ):
              geos_finam = finam
         else:
              geos_finam = finam
         if ( finam.rfind("MERRA") > 0 ):
              year = finam[len(finam)-12:len(finam)-8]
         else:
              if ( "-" in finam[len(finam)-21:len(finam)-17] ):
                   year = finam[len(finam)-23:len(finam)-19]
              else:
                   year = finam[len(finam)-21:len(finam)-17]

         for i in range (0,len(config.pivot_sds)):
             if ( config.to_heb_exe == "cp" ): 
                  id = geos_finam.rfind("/")
                  finam = geos_finam[0:id+1] + config.pivot_sds[i] + \
                          geos_finam[id+2:] 
                  if ( ivrb > 2 ): print ( "finam: ", finam ) 
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
         
             (ret, out) = exe ( to_heb_com, log_buf )
             if ( ret != 0 ):
                  Message ( "E", "Error an attempt to extract the SDS(s) " + \
                                 "and write it(them) down in HEB-format" )
                  print (  "\n".join(out) )
                  print_err ( config, "Error an attempt to extract the SDS(s) " + \
                                      "and write it(them) down in HEB-format" )
                  return ( "n/a" )

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
                   return ( "skipped" )

    if ( run_level == 0 or run_level == 99 or run_level == 2 ):
         if ( run_level == 2 ):
              if ( finam.rfind("MERRA") > 0 ):
                   heb_date_name = finam[finam.rfind(".nc4")-8:finam.rfind(".nc4")]
              else:
                   heb_date_name = geos_finam[finam.rfind("/")+3:finam.rfind("/")+16]
         else:
              if ( finam.rfind("MERRA") > 0 ):
                   heb_date_name = finam[finam.rfind(".nc4")-8:finam.rfind(".nc4")]
              else:
                   if ( "-" in geos_finam[len(finam)-21:len(geos_finam)-8] ):
                        heb_date_name = geos_finam[len(finam)-23:len(geos_finam)-8].replace("-","").replace("T","_")
                   else:
                        heb_date_name = geos_finam[len(finam)-21:len(geos_finam)-8]

         if ( config.gen_spr_exe.find("gen_spr") > 1 or \
              config.gen_spr_exe.find("malo_upgrid") > 1 or \
              config.gen_spr_exe == None                 or \
              config.gen_spr_exe == "NONE"               or \
              config.gen_spr_exe == "blank"               ) :

              com_gen_spr = None
         else:
              com_gen_spr = config.gen_spr_exe  + " " + \
                            config.geos_heb_dir + " " + \
                            config.acp_height   + " " + \
                            heb_date_name       + " " + \
                            config.gmao_gh      + " " + \
                            config.malo_elev    + " " + \
                            config.acp_dir      + " " + \
                            config.st_dir 

         if ( com_gen_spr != None ):
              if  ( ivrb > 1 ): print ( "com_gen_spr: " + com_gen_spr ) 
              (ret, out) = exe ( com_gen_spr, log_buf )
              if ( ret != 0 ):
                   Message ( "E", "Error an attempt to run gen_spr program" )
                   print (  "\n".join(out) )
                   print_err ( config, "Error an attempt to generate " +
                                       "surface pressure field" )
                   return ( "n/a" )
              if ( config.gen_spr_exe.find("gen_acp") > 0 ):
                   return ( heb_date_name )

    if ( run_level == 0 or run_level == 99 or run_level == 3 ):
         if ( not ( config.loading_exe == "blank" or config.loading_exe == "none" ) ):
              for i in range(0,config.epochs_per_file):
                  if ( config.epochs_per_file == 1 ):
                       heb_date = heb_date_name
                  else:
                       heb_date = heb_date_name + "_" + time_dict["%s" % config.epochs_per_file][i]

                  loading_com = config.loading_exe + " " + \
                                config.filename    + " " + \
                                "111111111"        + " " + \
                                 heb_date          + " " + \
                                "2"

                  if ( ivrb > 1 ): print ( "loading_com: " + loading_com )
                  (ret, out) = exe ( loading_com, log_buf )
                  if ( ret != 0 ):
                       Message ( "E", "Error an attempt to compute mass loading for epoch " + heb_date_name )
                       print_err ( config, "Error an attempt to compute mass loading for epoch " + heb_date_name )
                       print (  "\n".join(out) )
                       return ( "n/a" )

    if ( run_level == 0 or run_level == 99 or run_level == 4 ):
         if ( not ( config.gen_bdsp_exe == "blank" or config.gen_bdsp_exe == "none" ) ):
              if ( stop_line == "no_bin" or stop_line == "no_binary" ):
                   print ( "Update of binary files was stopped according to the stop file " + \
                            config.stop_file )
                   return ( heb_date_name )

              load_bdsp_com = config.gen_bdsp_exe + " " + \
                              config.load_list_dir + "/" + config.load_list_pref + " " + \
                              config.load_bdsp_dir + " " + \
                              "update" + " " + \
                              "end" + " " + \
                              "2"

              if ( ivrb > 1 ): print ( "load_bdsp_com: " + load_bdsp_com )
              (ret, out) = exe ( load_bdsp_com, log_buf )
              if ( ret != 0 ):
                   Message ( "E", "Error an attempt to convert loading to binary format" )
                   print_err ( config, "Error an attempt to convert loading to binary format" )
                   print (  "\n".join(out) )
                   return ( "n/a" )

    if ( run_level == 0 or run_level == 99 or run_level == 5 ):
         if ( not ( config.aam_exe == "blank" or config.aam_exe == "none" ) ):
              for i in range(0,config.epochs_per_file):
                  if ( run_level == 5 ):
                       heb_date = finam[finam.rfind("/")+3:finam.rfind("/")+16]
                  else:
                       if ( config.epochs_per_file == 1 ):
                            heb_date = heb_date_name
                       else:
                            heb_date = heb_date_name + "_" + time_dict["%s" % config.epochs_per_file][i]

                  aam_com = config.aam_exe      + " " + \
                            config.geos_heb_dir + " " + \
                            heb_date            + " " + \
                            config.aam_igh      + " " + \
                            config.aam_ogh      + " " + \
                            config.aam_ls_mask  + " " + \
                            config.aam_pref                 
                  if ( ivrb > 1 ): print ( "aam_com: " + aam_com )
                  (ret, out) = exe ( aam_com, log_buf )
                  if ( ret != 0 ):
                       Message ( "E", "Error an attempt to compute the atmospheric " + \
                                      "angular momentum" )
                       print_err ( config, "Error an attempt to compute the atmospheric " + \
                                           "angular momentum" )
                       print (  "\n".join(out) )
                       return ( "n/a" )

    if ( run_level == 0 or run_level == 99 or run_level == 6 ):
         if ( not ( config.aam_exe == "blank" or config.aam_exe == "none" ) ):
              aam_dir = config.aam_pref[0:config.aam_pref.rindex("/")]
              aam_ser_com = config.aam_ser_exe  + " " + \
                            aam_dir             + " " + \
                            "I1"                + " " + \
                            "begin"             + " " + \
                            "end"               + " " + \
                            config.aam_ser_file
              if ( ivrb > 1 ): print ( "aam_ser_com: " + aam_ser_com )
              (ret, out) = exe ( aam_ser_com, log_buf )
              if ( ret != 0 ):
                   Message ( "E", "Error an attempt to generate the file with " + \
                                  "series of atmospheric angular momentum" )
                   print_err ( config, "Error an attempt to generate the file with " + \
                                  "series of atmospheric angular momentum" )
                   print (  "\n".join(out) )
                   return ( "n/a" )

    if ( run_level == 0 or run_level == 99 or run_level == 7 ):
         if ( not ( config.int_loading_exe == "blank" or config.int_loading_exe == "none" ) ):
              for i in range(0,config.epochs_per_file):
                  if ( config.epochs_per_file == 1 ):
                       heb_date = heb_date_name
                  else:
                       heb_date = heb_date_name + "_" + time_dict["%s" % config.epochs_per_file][i]

                  com_int = config.int_loading_exe + " " + \
                            config.filename + " " + \
                            heb_date + " " + \
                            "3"
                  if ( ivrb > 1 ): print ( "com_int: " + com_int )
                  (ret, out) = exe ( com_int, log_buf )
                  if ( ret != 0 ):
                       Message   ( "E",    "Error an attempt to compute the integral over " + \
                                           "mass loading" )
                       print_err ( config, "Error an attempt to compute the integral over " + \
                                           "mass loading" )
                       print (  "\n".join(out) )
                       return ( "n/a" )

    if ( run_level == 1 ): 
         return ( "ok" )

    if ( len(heb_date_name) < 10 ):
         if ( finam.rfind("MERRA") > -1 and config.load_list_dir.find("lws") > 0  ):
              heb_date_name  = heb_date_name + "_2100"
         elif ( finam.rfind("GEOS.") > -1 ):
              heb_date_name  = heb_date_name + "_2100"
         else:
              heb_date_name  = heb_date_name + "_1800"

    return ( heb_date_name )

#
# ------------------------------------------------------------------------
#
def exe ( command, log_buf):
#"""
#    Spawn a supborcess, execute command in the context of the suborpess,
#    wait for its completion and return completion code and results.
#"""
    words = command.split()
    time_str = str(datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.") + "%6d" % datetime.datetime.now().microsecond).replace( " ", "0" )
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
def wget_exe ( com, log_buf ):
#"""
#    Routine wget_exe executes a command that includes wget in a secure way.
#    If there  username and/or password are specified in the command
#    line, it is put in the temporary configuration file and removed from 
#    the command line
#"""
#
# --- check for username/password commands
#
    config_list = []    
    for word in com.split():
        if ( "--ftp-password" in word ):
#
# ---------- Extract the ftp password
#
             config_line = word.split("=")[-1].replace('"','')
#
# ---------- Remove it from the command line
#
             com = com.replace(word,"")
#
# ---------- and append to the configuration file
#
             if ( config_line != "" ):
                  config_list.append ( "ftp-pasword = " + config_line )
        if ( "--user" in word ):
#
# ---------- Extract the user name
#
             config_line = word.split("=")[-1].replace('"','')
#
# ---------- Remove it from the command line
#
             com = com.replace(word,"")
#
# ---------- and append to the configuration file
#
             config_list.append ( "user = " + config_line )
        if ( "--http-password" in word ):
#
# ---------- Extract the http password
#
             config_line = word.split("=")[-1].replace('"','')
#
# ---------- Remove it from the command line
#
             com = com.replace(word,"")
#
# ---------- and append to the configuration file
#
             config_list.append ( "http-password = " + config_line )
    
    if ( len(config_list) > 0 ):
#
# ------ Yes! There were username/password in the wget command linie
#
         finam = "/dev/shm/wget__%08d.cnf" % os.getpid()
#
# ------ Write tehe context of configuration in the temporary file
#
         f=open(finam,"w")
         for line in config_list:
             print ( line, file=f )
         f.close()
#
# ------ Set u=rw,g=,o= permissions
#
         os.chmod ( finam, 0o600 )
#
# ------ Add --config=finam in the wget command
#
#
# ------ Add --config=finam in the wget command
#
         for word in com.split():
             if ( word == "wget" or "/wget" in word ):
                  com = com.replace(word,word + " --config=" + finam)

#
# --- Execute wget command
#
    (ret,err) = exe ( com, log_buf )
#
# --- Remove temorary file with configuration
#
    if ( len(config_list) > 0 ):
         os.unlink ( finam )
    return    ( ret, err )

#
# ------------------------------------------------------------------------
#
def my_signal_handler_term ( signal, frame ):
    global file_to_download
    finam = "/dev/shm/wget__%08d.cnf" % os.getpid()
    if ( os.path.isfile ( finam ) ):
         os.unlink ( finam )
    if ( file_to_download ):
         if ( os.path.isfile ( file_to_download ) ):
              os.unlink ( finam )
    print ( 'Terminated by TERM signal' )
    sys.exit(0)

#
# ------------------------------------------------------------------------
#
def print_log ( config, str ):
#"""
#   print string str into both log-file specified in config file and to  
#   stdout
#"""
    now = datetime.datetime.now() 
    
    print ( "geos_oper: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str )
    sys.stdout.flush()
#
    config.log_file_handle = open ( config.log_file, "a" )
    print ( "geos_oper: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str, \
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
    now = datetime.datetime.now() 
    
#
    suc_file_handle = open ( config.suc_file, "w" )
    print ( "geos_oper: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str, \
            file=suc_file_handle )
    suc_file_handle.flush()
    suc_file_handle.close()
#
# ------------------------------------------------------------------------
#
def append_suc ( config, str ):
#"""
#   append to the existing string str into suc-file specified in the config file
#"""
    now = datetime.datetime.now() 
    
#
    suc_file_handle = open ( config.suc_file, "a" )
    print ( "geos_oper: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str, \
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
    now = datetime.datetime.now() 
    
#
    err_file_handle = open ( config.err_file, "w" )
    print ( "geos_oper: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str, \
            file=err_file_handle )
    err_file_handle.flush()
    err_file_handle.close()
#
# ------------------------------------------------------------------------
#
def append_err ( config, str ):
#"""
#   print string str into err-file specified in the config file
#"""
    now = datetime.datetime.now() 
    
#
    err_file_handle = open ( config.err_file, "a" )
    print ( "geos_oper: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str, \
            file=err_file_handle )
    err_file_handle.flush()
    err_file_handle.close()

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
              time_tag = datetime.datetime.strptime ( time, "%Y.%m.%d_%H:%M:%S" )
              delta_since = datetime.datetime.now() - time_tag
              time_since = delta_since.days*86400.0 + delta_since.seconds
              if ( time_since < config.lock_timeout ):
#
# ---------------- The process running for less than it is defiend in config.lock_timeout
# ---------------- Let us quit quietly and let it finish
#
                   print ( "%s Another process get_geos_oper, pid %s is still running for %d seconds" % \
                           ( datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S"), \
                             pid, int(time_since) ) \
                         )              
                   exit ( 0 )

#
# --- Create lock file and write there the PID of the current process
# --- and the current time
#
    lock = open ( config.lock_file, "w" )
    now = datetime.datetime.now() 
    print ( "%05d %s" % (os.getpid(), now.strftime("%Y.%m.%d_%H:%M:%S")), \
             file=lock )
    lock.close()

#
# ------------------------------------------------------------------------
#
def bds_export ( config, ivrb ):
#   """
#   Generate path delay extension in the sandbox direcotry
#   """

    log_buf = []
#
# --- Check the internal directory with dispalements 
#
    end_epoch_int = "??"
    end_epoch_ext = "??"
    com = "bds_util check " + config.load_bdsp_dir

    if ( ivrb >=3 ):
         print ( "geos_oper.py: About to execute command %s" % com )
    (ret,out) = exe ( com, log_buf )
    if ( ret != 0 ):
         print ( "ERROR in running command ", com )
         print ( "Returning code: ", ret )
         print_err ( config, "Error in checking internal displacement directory %s" % \
                     config.load_bdsp_dir )
         return ( 1, "Error in bds_export" )

#
# --- Extract from there the end epoch and the time step of time series
#
    if ( ivrb >=3 ):
         print ( "geos_oper.py: Search for the end epoch in %s" % config.load_bdsp_dir )
    for line in out:
        if ( "End epoch:" in line ):
             end_epoch_int = line.split()[3]
        if ( "Time_step:" in line ):
             tim_step = float(line.split()[4])
    if ( end_epoch_int == "??" ):
         print_err ( config, "Trap of internal control: no End epoch from command %s" % com )
         print     (         "Trap of internal control: no End epoch from command %s" % com )
         return ( 1, "Error in bds_export" )

#
# --- Check the external directory with dislpacements and 
# --- extract the time step in the external directory
#
    com = "bds_util check " + config.load_bdsp_export_dir

    if ( ivrb >=3 ):
         print ( "geos_oper.py: About to execute command %s" % com )
    (ret,out) = exe ( com, log_buf )
    if ( ret != 0 ):
         print ( "ERROR in running command ", com )
         print ( "Returning code: ", ret )
         print_err ( config, "Error in checking external l displacement directory %s" % \
                     config.load_bdsp_export_dir )
         return ( 1, "Error in bds_export" )

    for line in out:
        if ( "End epoch:" in line ):
             end_epoch_ext = line.split()[3]

    if ( end_epoch_ext == "??" ):
         print_err ( config, "Trap of internal control: no End epoch from command %s" % com )
         return ( 1, "Error in bds_export" )

    if ( end_epoch_ext == end_epoch_int ):
         suc_str = "Internal and external binary directories %s and %s are in sync. End epoch %s " % \
                    ( config.load_bdsp_dir, config.load_bdsp_export_dir, end_epoch_int )
         if ( ivrb >= 0 ):
              print ( suc_str )
         return ( 0, suc_str )
    else:
#
# ------ External directory has less data then internal direcory.
# ------ Determine the time epochs of range that the exteranl
# ------ directory needs be updated [tim_extract_beg, tim_extract_int]
#
         tim_end_epoch_int = datetime.datetime.strptime ( end_epoch_int[0:19], "%Y.%m.%d-%H:%M:%S" )
         tim_end_epoch_ext = datetime.datetime.strptime ( end_epoch_ext[0:19], "%Y.%m.%d-%H:%M:%S" )         
         tim_extract_beg   = tim_end_epoch_ext + timedelta ( seconds= tim_step )
         extract_beg = datetime.datetime.strftime ( tim_extract_beg, "%Y.%m.%d-%H:%M:%S" )

         if ( ivrb >= 2 ):
              print ( "end_epoch_int: ", end_epoch_int, tim_end_epoch_int  )
              print ( "end_epoch_ext: ", end_epoch_ext, tim_end_epoch_ext  )
              print ( "time_step:     ", tim_step ) 
              print ( "extract_beg:   ", extract_beg )

#
# ------ Read the summary of the internal directory with slant path delays
#
         bds_sum_file = config.load_bdsp_dir + "/bds_summary.txt"
         bds_sum_buf = []
         with open(bds_sum_file,encoding="latin") as f:
              for line in f:
                  bds_sum_buf.append ( line.strip("\n").strip("\r") )
         f.close()

         if ( pyvers >= "0312000" ):
              tim_utc_now = datetime.datetime.now(datetime.timezone.utc)
         else:
              tim_utc_now = datetime.datetime.utcnow()
         
         if ( ivrb >= 3 ): 
              print ( "Extracting of sections of displacments" )
#
# ------ Extract a section in the  path delay file for each station
#
         bds_list = []
         for line in bds_sum_buf:
             if ( line[0:5] == "STA: " ):
#
# --------------- Get station name
#
                  sta_nam = line.split()[2]
#
# --------------- Build the name of the displacement file in binary format
#
                  bds_file = config.load_bdsp_dir + "/" + sta_nam + ".bds"

#
# --------------- Extract the section of slant path delay for the differences between
# --------------- the contents of internal slant path delays in binary format
# --------------- with respect to the contents of external slant path delays.
# --------------- A file with that section is placed in the internal binary directory
#
                  com = "bds_util extract " + bds_file + " " + extract_beg 

                  if ( ivrb >= 3 ): 
                       print ( "geos_oper.py: About to execute command %s" % com )

                  (ret,out) = exe ( com, log_buf )
                  if ( ret != 0 ):
                       print ( "ERROR in running command ", com )
                       print ( "Returning code: ", ret )
                       for line in out:
                           print     ( line )
                           print_err ( config, line )
                       print_err ( config, "Error in exporting binary displacement files" )

#
# -------------------- Remove all slant path extensions from the internal directory in a case of error
#
                       for paths, dirs, files in os.walk(config.load_bdsp_dir):
                           for file in files:
                               if ( file[-4:] == ".dat" ):
                                    finam = paths + "/" + file
                                    os.unlink ( finam )

                       exit ( 1 )
#
# --------------- Put the original file with slant path delay and the file
# --------------- with and extracted secltion of path delay into bds_list list
#
                  ext_file = bds_file + "_"  + datetime.datetime.strftime ( tim_extract_beg,   "%Y%m%d_%H%M%S" ) + \
                                         "__" + datetime.datetime.strftime ( tim_end_epoch_int, "%Y%m%d_%H%M%S" ) + \
                                         ".dat"
                  bds_list.append ( (bds_file, ext_file) )

#
# ------ Generate contents of the lock file
#
         str_utc_now = tim_utc_now.strftime("%Y%m%d_%H%M%S")
         str_utc_end = (tim_utc_now + timedelta(seconds=bds_lock_time)).strftime("%Y%m%d_%H%M%S")
         lock_contents = "pid: %08d  start: %s  end: %s " % ( os.getpid(), str_utc_now, str_utc_end )

#
# ------ Write the lock file in the sandbox directory
#
         ext_lock_file = config.load_bdsp_sandbox_dir + "/lock"
         if ( ivrb >= 3 ): 
              print ( "Writing lock in the sandbox direcotory %s" % ext_lock_file )
         f = open ( ext_lock_file, "w" )
         print ( lock_contents, file=f )
         f.close()

         if ( ivrb >= 3 ): 
              print ( "Move displacement files to the sandbox" )
#
# ------ Move displacement extension files to the sandbox
#
         for bds_tulip in bds_list:
             bds_file = bds_tulip[0]
             ext_file  = bds_tulip[1]
             com = "mv " + ext_file + " " + config.load_bdsp_sandbox_dir  + "/" 
             if ( ivrb >= 3 ):
                  print ( "About to execute %s" % com )
             (ret,out) = exe ( com, log_buf )
             if ( ret != 0 ):
                  print ( "ERROR in running command ", com )
                  print ( "Returning code: ", ret )
                  print_err ( config, "Error in exporting binary displament files" )
                  exit ( 1 )
         if ( ivrb >= 3 ): 
              print ( "Remove the lock file" )
#
# ------ Remove the lock file
#
         os.unlink ( ext_lock_file )
#
# ------ Write the update file in the sandbox directory signaling the frontend
#
         update_str = bds_list[0][1][-36:-4]
         update_file = config.load_bdsp_sandbox_dir + "/update_" + str_utc_now + ".txt"
         update_file_tmp = update_file + "__%08d" % os.getpid()

         f = open ( update_file_tmp, "w" )
         print ( update_str, file=f )
         f.close()
         os.rename ( update_file_tmp, update_file )

         return ( 0, "Created update file %s for epochs %s " % ( update_file, update_str ) )
#
# ------------------------------------------------------------------------
#
def main():

    os.putenv ( "OMP_NUM_THREADS", "1" )
    os.putenv ( "GOMP_STACKSIZE",  "2000000" )
##    os.putenv ( "LD_LIBRARY_PATH", "/opt64/lib:/opt64/lib/python3.3/lib-dynload:/usr/lib" )

    opts = optparse.OptionParser( version=get_geos__label  )

    opts.add_option ( "-c", "--config", action="store", \
                      dest="config", \
                      metavar="NAME", \
                      help="Configuration file" )

    opts.add_option ( "-r", "--run-level", action="store", \
                      dest="run_level", \
                      default=0, \
                      type="int", \
                      metavar="NAME", \
                      help="Configuration file" )

    opts.add_option ( "-v", "--verbosity", action="store", \
                      dest="ivrb", \
                      default=0, \
                      type="int", \
                      metavar="NAME", \
                      help="Verbosity level" )

    opts.add_option ( "-f", "--file", action="store", \
                      dest="file", \
                      default=None, \
                      metavar="NAME", \
                      help="File to process" )
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
         

    config = config_class ( opts.config ) 

    parse_geos_oper_config ( config )

#    if ( opts.file == None ):
#         n_fil = get_geos_oper ( config, opts.run_level, opts.ivrb )
#    else:
#         log_buf = []
#         (n_fil, last_date) = geos_process ( config, opts.file, opts.run_level, opts.ivrb, log_buf )

    n_fil = 1 # for tests

    if ( n_fil > 0 ):
         if ( config.run_bdsp_export ):
#
# ----------- Initiate update of the export slant path delays
#
              (ret, out) = bds_export ( config, opts.ivrb )
              if ( opts.ivrb > 0 ):
                   time_str_now = datetime.datetime.now().strftime("%Y.%m.%d_%H.%M.%S")
                   if ( ret == 0 ):
                        print ( "geos_oper %s finished updating sandbox directory %s" % \
                                 ( time_str_now, config.load_bdsp_sandbox_dir ) )
                   else:
                        print ( "geos_oper %s FAILURE in an attempt to update sandbox directory %s" % \
                                 ( time_str_now, config.load_bdsp_sandbox_dir ) )


if __name__ == "__main__":
    pyvers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( pyvers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    signal.signal ( signal.SIGINT, my_signal_handler_term )
    main()
