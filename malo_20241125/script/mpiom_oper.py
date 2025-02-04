#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   Program for automatic downloading MPIOMXX data.                    *
# *                                                                      *
# *   mpiom_oper analyzes the directory tree on a local computer         *
# *   with mpiom data. It generates the list of mpiom data that are      *
# *   missing up to the current date, checks whether they are available  *
# *   at the remote server. It downloads those files that are available, *
# *   parses them, uncompress, converts to shc-format, and copies the    *
# *   original data to the storage directory. It computes loading        *
# *   displacements.                                                     *
# *                                                                      *
# * ### 04-NOV-2013   mpiom_oper.py v3.5 (c) L. Petrov 08-NOV-2020  ###  *
# *                                                                      *
# ************************************************************************
"""
import sys, os, shutil, time, subprocess
import optparse 
import math
import datetime
from   datetime import timedelta
from   Message     import *
from   geos_oper_config import *

if ( sys.version[:3] < "3.0" ): print ( "This script cannot run under Python-2" ); exit ( 1 )

date_range = "1978.01.01 2025.01.01"
day_time_step = 86400.0
epoch_arr = [ "0000", "0300", "0600", "0900", "1200", "1500", "1800", "2100" ]

#
# --- Important constants
#
mpiom_oper__label = "mpiom_oper.py  v 3.35 of 2023.10.21"

debug = 1

class config_class:
   def __init__ ( self, filename ):
       self.filename          = filename
       self.name              = None
       self.pivot_sds         = []
       self.username          = None
       self.password          = None
       self.begin_date        = None
       self.look_back_days    = None
       self.end_date          = None
       self.time_step         = None
       self.url_template      = None
       self.geos_temp_dir     = None
       self.geos_heb_dir      = None
       self.geos_dir          = None
       self.spr_dir           = None
       self.to_heb_exe        = None
       self.to_love_exe       = None
       self.loading_exe       = None
       self.malo_exe          = None
       self.gen_spr_exe       = None
       self.gen_bdsp_exe      = None
       self.gmao_gh           = None
       self.malo_elev         = None
       self.love_dir          = None
       self.love_pref         = None
       self.spr_pref          = None
       self.love_wildcard     = None
       self.load_bdsp_dir     = None
       self.load_grid_conf    = None
       self.load_list_conf    = None
       self.load_grid_dir     = None
       self.load_list_dir     = None
       self.load_grid_pref    = None
       self.load_list_pref    = None
       self.load_d1_grid_conf = None
       self.load_d1_list_conf = None
       self.load_d1_grid_dir  = None
       self.load_d1_list_dir  = None
       self.load_d1_grid_pref = None
       self.load_d1_list_pref = None
       self.load_grid_wc      = None
       self.load_list_wc      = None
       self.vgep_dir          = None
       self.vgep_pref         = None
       self.vgep_wc           = None
       self.aam_exe           = None
       self.aam_ser_exe       = None
       self.aam_igh           = None
       self.aam_ogh           = None
       self.aam_pref          = None
       self.aam_ser_file      = None
       self.compress_com      = None
       self.stop_file         = None
       self.log_file          = None
       self.log_file_handle   = None
       self.lock_file         = None
       self.lock_timeout      = None

       self.date_list      = []


   def init ( self ):
       __init__ ( self )

#
# ===========================================================================
#
def get_mpiom_oper ( config, run_level, ivrb ):
#"""
#  This is the main routine for processing remote geos data
#"""
#
# --- Check lock file, whether another get_mpiom_oper process is running
#
    check_lock ( config )

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
    else:
         stop_line = lines.split()[0]

    if ( stop_line == "stop" ):
         print ( "The service is stopped according to the stop file " + config.stop_file )
         exit ( 0 )

    config.log_file_handle = open ( config.log_file, "a" )
    print ( "# ========================================", \
            file=config.log_file_handle )
    config.log_file_handle.close()
#
    print ( "       " )
    print ( "==========================================" )
    print_log ( config, "started" )

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

#
# --- Walk through the directory and generate the list of existing data 
# --- within the data range specified in the configuration file
#
    finam_list = []
    print ( "walk_dir= ", config.geos_dir + "/orig" ) # %%%%%%%%%%%%%%%%%
    for paths, dirs, files in os.walk(config.geos_dir + "/orig"):
        for k in range(0,len(files)):
            name = paths + "/" + files[k]
            ih = name.rfind ( '.asc.gz' )
            if ( ih > 0 and not ( "TIDES" in name ) ):
                 fidat = name[ih-15:ih-11] + name[ih-10:ih-8] + name[ih-7:ih-5] + "_0000"
                 if ( fidat >= fidat_begin and \
                      fidat <= fidat_end       ):
                      finam_list.append(name)

    if ( ivrb > 1 ): print ( "fidat_begin = ", fidat_begin, " fidat_end= ", fidat_end )
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
             ih = finam_list[i].rfind ( '.asc.gz' )
#
# ---------- Extract date/time from the file name
#
             try:
                  fidat_last = datetime.datetime.strptime ( finam_list[i-1][ih-15:ih-11] + finam_list[i-1][ih-10:ih-8] + finam_list[i-1][ih-7:ih-5], "%Y%m%d" )
                  fidat      = datetime.datetime.strptime ( finam_list[i]  [ih-15:ih-11] + finam_list[i]  [ih-10:ih-8] + finam_list[i]  [ih-7:ih-5], "%Y%m%d" )
             except:
                  continue
             if ( ivrb > 2 ): print ( "fidat: ", str(fidat) )
#
# ---------- Compare the date of the current file and the previous one
#
             ddat       = fidat - fidat_last
             ddat_sec   = ddat.days*86400.0 + ddat.seconds
             if ( ddat_sec > (day_time_step - 4.0) ):
#
# --------------- The gap exceeds the limit
#
                  num_missed = int(ddat_sec/day_time_step - 1)  
                  if ( ivrb > 3 ): print ( "ddat_sec= ", ddat_sec, " num_missed= ", num_missed, " day_time_step = ", day_time_step )
                  for k in range(0,num_missed):
#
# ------------------- Store dates of missed data
#
                      missed_date = fidat_last + timedelta (seconds=(k+1)*day_time_step )
                      config.date_list.append ( missed_date.strftime("%Y%m%d_%H%M") )
                      if ( ivrb > 3 ): print ( "Missed date: ", str(missed_date) )
                      
    if ( ivrb > 1 ): print ( "Total num_missed = ", len(config.date_list) )
    if ( ivrb > 1 and len(finam_list) ): print ( "Last element in finam_list: ", finam_list[len(finam_list)-1] )
#
# --- fidat_last is the date of the last heb-file.
# --- If no heb-files are present, then the nominal start date specified
# --- in the control file will be used
#
    if ( len(finam_list) > 0 ): 
         ih = finam_list[len(finam_list)-1].rfind ( '.asc.gz' )
         fidat_last = datetime.datetime.strptime ( \
               finam_list[len(finam_list)-1][ih-15:ih-11] + \
               finam_list[len(finam_list)-1][ih-10:ih-8]  + \
               finam_list[len(finam_list)-1][ih-7:ih-5], "%Y%m%d" )
    else:
         fidat_last  = datetime.datetime.strptime ( fidat_begin, "%Y%m%d_%H%M") - timedelta ( seconds=day_time_step )
    if ( ivrb > 1 ): print ( "fidat_last: ", str(fidat_last) )
    if ( ivrb > 4 ): print ( "finam_list= ", finam_list )
#
# --- In this contents 'now' may be in the future. 
# --- This is the date through which we will be looking for data
#
    now = datetime.datetime.now() + timedelta ( seconds=config.step_ahead*config.time_step )
    if ( debug > 0 ): print ( "now-1: " + now.strftime("%Y%m%d_%H%M") + " fidat_end= " + fidat_end )
    if ( now.strftime("%Y%m%d_%H%M") > fidat_end ):
         now = datetime.datetime.strptime ( fidat_end, "%Y%m%d_%H%M" )
    if ( debug > 0 ): print ( "now-2: " + now.strftime("%Y%m%d_%H%M") + " fidat_end= " + fidat_end )
#
# --- Add the dates of epochs of mpiom files which are after the last
# --- date of geos data found in the local system, but before the date
# --- through which we are looking for the data
#
    for k in range(1,512):
        new_date = fidat_last + timedelta (seconds=k*day_time_step )
        if ( new_date.strftime("%Y%m%d_%H%M") < now.strftime("%Y%m%d_%H%M") ):
             config.date_list.append ( new_date.strftime("%Y%m%d_%H%M") )

    if ( len(config.date_list) == 0 ):
         print_log  ( config, "No files are to be fetched, " + \
                              "since they are all up to date" )
         exit ( 0 )

#
# --- Sorting dates
#
    config.date_list.sort()
    if ( ivrb > 4 ): print ( "config.date_list= ", config.date_list )

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
        iy  = url.find ( "Y%%%%" )
        if ( iy > 1 ):
             url = url.replace ( config.url_template[iy:iy+5], \
                                 config.date_list[i][0:4] )
                  
        im  = url.find ( "M@@" )
        if ( im > 1 ):
             url = url.replace ( config.url_template[im:im+3], \
                                 "M" + config.date_list[i][4:6] )
        im  = url.find ( "M%%" )
        if ( im > 1 ):
             url = url.replace ( config.url_template[im+1:im+4], \
                                 config.date_list[i][4:6] )

        id  = url.find ( "D%%" )
        if ( id > 1 ):
             url = url.replace ( config.url_template[id+2:id+5], \
                                 config.date_list[i][6:8] )

        id  = url.find ( "D@@" )
        if ( id > 1 ):
             url = url.replace ( config.url_template[id:id+3], \
                                 "D" + config.date_list[i][6:8] )

        it  = url.find ( "T@@@@@@@@@@@@" )
        if ( it > 1 ):
             url = url.replace ( config.url_template[it:it+13], \
                                 config.date_list[i] )
        if ( ivrb > 3 ): print ( "url: ", url ) 
        url_list.append(url)

#
# --- Now check whether these URLs are present in the remote server
#
    if ( ivrb > 2 ): print ( "config.date_list = ", config.date_list )
    if ( ivrb > 2 ): print ( "url_list[0]= ", url_list[0] )
    if ( ivrb > 4 ): print ( "url_list= ", url_list )
    url_exists_list = []
    last_wget_command = '??'
    for i in range (0,len(config.date_list)):
        id = url_list[i].rfind ( "/" )
        ia = url_list[i].rfind ( "AOD1B_" )
        if ( ivrb > 1 ): 
             if ( i == 0 ):
                  print ( "Checking ", i+1, "st URL ", url_list[i] )
             elif ( i == 1 ):
                  print ( "Checking ", i+1, "nd URL ", url_list[i] )
             else:
                  print ( "Checking ", i+1, "th URL ", url_list[i] )
#
# ----- Execute command wget which will retrieve the listing of the 
# ----- directory tree where the data are supposed to be found
#
        if ( os.path.isfile(".listing") ):
             os.unlink ( ".listing" )
        wget_command = "cd " + config.geos_temp_dir + " ; " + \
                       "wget --timeout=30 --tries=64 -nH --cut-dirs=8 -r -np --no-remove-listing -q -c --retry-connrefused --ftp-password=\"\" -A 'PODSTANOVKA' " + \
                       url_list[i][0:id] + "/" + url_list[i][ia+6:ia+10] 
        if ( ivrb > 1 ): print ( "Executing command " + wget_command )
        if ( wget_command != last_wget_command ): 
             if ( ivrb > 2 ): print ( "wget_command: " + wget_command )
             log_buf = []
             ( ret, out) = wget_exe ( wget_command, log_buf )
             if ( ivrb > 4 ): print ( "wget_command ret= ", ret )
             if ( ivrb > 4 ): print ( "wget_command out= ", out )
          
#
# ----- wget is supposed to put listing of the file in listing_file.
# ----- However, if directory does not exist, wget will not create 
# ----- listing file
#
        listing_file = config.geos_temp_dir + "/.listing"
        if ( ivrb > 4 ): print ( "Here is listing file: " + listing_file, ' exist: ', os.path.isfile(listing_file) )
        if ( os.path.isfile(listing_file) ):
#
# ---------- Read the listing file
#
             with open ( listing_file ) as f:
                  listing = f.readlines()
             f.close ( )
             if ( ivrb > 4 ): print ( "Reading listing file: " + listing_file )
             for line in listing:
#
# -------------- ... and try to find the matching file name
#
                 if ( line.find(url_list[i][id+1:]) > 0 ):
                      if ( ivrb > 4 ): print ( "i= ", i, " url: ", url_list[i][id+1:], " line= ", line )
#
# ------------------- Found? Super! Add this url to the list of urls
# ------------------- to be fetched
#
                      url_exists_list.append ( url_list[i][0:id] + "/" + url_list[i][ia+6:ia+10] + "/" + url_list[i][id+1:] )

        last_wget_command = wget_command

    if ( len(url_exists_list) == 0 ):
         print_log ( config, "No mpiom-files to be fetched were found " + \
                             "at the remote server" )
         exit ( 0 )
    print_log ( config, "found %d files for downloading " % len( url_exists_list ) )

#
# --- Now cycle over the list of existing urls and fetch the data.
# --- Fetched data are processed
#
    n_fil = 0
    last_proc_date = "n/a"
    for i in range (0,len(url_exists_list)):
        print_log ( config, "file %d (%d) %s" % (i+1, len(url_exists_list), \
                    url_exists_list[i][id+1:] + " is being processed" ) )
        (ret, proc_date) = fetch_data ( config, url_exists_list[i], \
                                        run_level, ivrb, log_buf )
        id = url_exists_list[i].rfind ( "/" ) 

        n_fil = n_fil + ret
        if ( proc_date != "n/a" ):
             last_proc_date = proc_date
#
# --- Uhhh! We finished!
#
    print_log ( config, "finished" )
    if ( n_fil > 0 and proc_date != "n/a" ):
         print_suc ( config, "Number of files processed: %d. Last file for date %s" % \
                   ( n_fil, last_proc_date ) )
    else:
         print_err ( config, "Error in processing %d input data files" % n_fil )
         for i in range (0,len(url_list)):
             print_err ( config, "Error in processing input url %s" % url_list[i] )

    print_log ( config, "finished" )
    exit ( 0 )

#
# ===========================================================================
#
def fetch_data ( config, url, run_level, ivrb, log_buf):
#"""
#    This procedure:
#    1) fetches the data using wget
#    2) extracts the sought datasets converts them into heb-format
#    3) moves ofriginal heb-data into the storage archive
#"""
    year  = url[len(url)-22:len(url)-18]
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
             config.geos_temp_dir + \
             " ; wget --timeout=30 --tries=64 --retry-connrefused -nH --cut-dirs=8 -c --ftp-password=\"\" " + \
             url
        if ( ivrb > 2 ):
             print ( "Execute command: ", wget_com )

        log_buf = []
        (ret, out) = wget_exe ( wget_com, log_buf )
        if ( ret != 0 ):
             Message ( "E", "Error in attempt to download url " + url )
             Message ( "E", "Failed command: " + wget_com )
             print ( log_buf )
             print_err ( config, "Error in attempt to download a certain url" )
             print (  "\n".join(out) )
             exit ( 1 )
    
#
# ----- If necessary, create the output directory
#
        geo_finam = config.geos_temp_dir + url[id:]
        if ( ivrb > 2 ):
             Message ( "I2", "geo_finam: %s" % geo_finam )
        if ( config.geos_heb_dir != "none" ):
             if ( not os.path.isdir(config.geos_heb_dir + "/" + year) ):
                  os.mkdir ( config.geos_heb_dir + "/" + year )

#
# --- Process the fetched file: extract the requested SDSs and 
# --- convert them into heb-format
#
    last_date = mpiom_process ( config, geo_finam, run_level, ivrb, log_buf )

    if ( config.geos_dir == "none" or config.geos_dir == "/dev/null"  ):
#
# ------ Remove the the original datafile
#
         os.unlink ( config.geos_temp_dir + url[id:] )
    else:
#
# ------ Move the original data into the archive directory
#
         orig_dir = config.geos_dir + "/orig/" + year 
         if ( not os.path.isdir ( orig_dir ) ):
              os.mkdir ( orig_dir )
         shutil.copy2 ( config.geos_temp_dir + url[id:], orig_dir + "/" + url[id:] )
         os.unlink    ( config.geos_temp_dir + url[id:] )

    return (1, last_date)

#
# ===========================================================================
#
def mpiom_process ( config, finam, run_level, ivrb, log_buf):

    file_date = "n/a"
    if ( config.epochs_per_file == 1  ):
         heb_time = [ "0000" ]
    elif ( config.epochs_per_file == 4  ):
         heb_time = [ "0000", "0600", "1200", "1800" ]
    elif ( config.epochs_per_file == 8  ):
         heb_time = [ "0000", "0300", "0600", "0900", "1200", "1500", "1800", "2100" ]
    elif ( config.epochs_per_file == 24  ):
         heb_time = [ "0000", "0100", "0200", "0300", "0400", "0500", \
                      "0600", "0700", "0800", "0900", "1000", "1100", \
                      "1200", "1300", "1400", "1500", "1600", "1700", \
                      "1800", "1900", "2000", "2100", "2200", "2300"  ]
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
    else:
         stop_line = lines.split()[0]

    if ( stop_line == "stop" ):
         print ( "The service is stopped according to the stop file " + config.stop_file )
         exit ( 0 )

    Message ( "I2", "geos_orig_file: %s" % finam )

    if ( run_level == 0 or run_level == 11 or run_level == 1 ):
         id = finam.rfind ( "/" )

#
# ------ Uncompress original mpiom data
#
         mpiom_dir  = config.geos_dir + "/asc/" + finam[id+7:id+11]
#
# ------ Check whether the output directory exits, and if not, create it
#
         if ( not os.path.isdir( mpiom_dir ) ):
              os.mkdir ( mpiom_dir )
         mpiom_file = config.geos_dir + "/asc/" + finam[id+7:id+11] + finam[id:].replace(".asc.gz",".asc")
         unpack_com = "gzip -cdf " + finam + " > " + mpiom_file
         if ( ivrb > 1 ): print ( "unpack_com: ", unpack_com, flush=True )
         (ret, out) = exe ( unpack_com, log_buf )
         if ( ret != 0 ):
              Message ( "E", "Error an attempt to uncompress the MPIOM data" )
              print (  "\n".join(out) )
              print_err ( config, "Error an attempt to uncompress the MPIOM data" )
              exit ( 1 )

#
# ------ Copy original MPIOM file
#
         mpiom_orig_dir = config.geos_dir + "/orig/" + finam[id+7:id+11] 
         if ( not os.path.isdir( mpiom_orig_dir ) ):
              os.mkdir ( mpiom_orig_dir )
         copy_com = "cp " + finam + " " + config.geos_dir + "/orig/" + finam[id+7:id+11] + "/"
         if ( ivrb > 1 ): print ( "copy_com: ", copy_com, flush=True )
         (ret, out) = exe ( copy_com, log_buf )
         if ( ret != 0 ):
              Message ( "E", "Error an attempt to copy the MPIOM data" )
              print (  "\n".join(out) )
              print_err ( config, "Error an attempt to copy the MPIOM data" )
              exit ( 1 )

    if ( run_level == 0 or run_level == 99 or run_level == 2 ):
         if ( run_level == 2 ):
              mpiom_file = finam

         heb_date_name = mpiom_file[len(mpiom_file)-19:len(mpiom_file)-15] + \
                         mpiom_file[len(mpiom_file)-14:len(mpiom_file)-12] + \
                         mpiom_file[len(mpiom_file)-11:len(mpiom_file)-9] 
         if ( ivrb > 2 ):
              vrb_str = "%d" % ivrb
         else:
              vrb_str = "2"

         loading_com = config.loading_exe + " " + \
                       config.filename    + " " + \
                       "1111111111"       + " " + \
                       heb_date_name      + " " + \
                       vrb_str
         if ( ivrb > 1 ): print ( "loading_com: " + loading_com, flush=True )
         (ret, out) = exe ( loading_com, log_buf )
         if ( ret != 0 ):
              Message ( "E", "Error an attempt to compute mass loading for epoch" + heb_date_name )
              print_err ( config, "Error an attempt to compute mass loading for epoch " + heb_date_name )
              print (  "\n".join(out) )
              exit ( 1 )

    if ( run_level == 0 or run_level == 99 or run_level == 3 ):

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

         if ( ivrb > 1 ): print ( "load_bdsp_com: " + load_bdsp_com, flush=True )
         (ret, out) = exe ( load_bdsp_com, log_buf )
         if ( ret != 0 ):
              Message ( "E", "Error an attempt to convert loading to binary format" )
              print_err ( config, "Error an attempt to convert loading to binary format" )
              print (  "\n".join(out) )
              exit ( 1 )

    return  ( heb_date_name + "_" + heb_time[config.epochs_per_file-1] )

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
def print_log ( config, str ):
#"""
#   print string str into both log-file specified in config file and to  
#   stdout
#"""
    now = datetime.datetime.now() 
    
    print ( "mpiom_oper: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str )
    sys.stdout.flush()
#
    config.log_file_handle = open ( config.log_file, "a" )
    print ( "mpiom_oper: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str, \
            file=config.log_file_handle )
    config.log_file_handle.flush()
    config.log_file_handle.close()

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
                   print ( "%s Another process mpiom_oper, pid %s is still running for %d seconds" % \
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
def main():

    os.putenv ( "OMP_NUM_THREADS", "2" )
    os.putenv ( "GOMP_STACKSIZE",  "2000000" )
#    os.putenv ( "LD_LIBRARY_PATH", "/opt64/lib:/opt64/lib/python3.3/lib-dynload:/usr/lib" )

    opts = optparse.OptionParser( version=mpiom_oper__label )

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
         print ( "Configuration file is not specied. Try mpiom_oper.py -h to see options" )
         exit ( 1 )

    if ( not os.path.isfile(opts.config) ):
         print ( "Configuration file ", opts.config, " does not exist" )
         exit ( 1 )
         

    config = config_class ( opts.config ) 

    parse_geos_oper_config ( config )

    if ( opts.file == None ):
         get_mpiom_oper ( config, opts.run_level, opts.ivrb )
    else:
         log_buf = []
         geos_process ( config, opts.file, opts.run_level, opts.ivrb, log_buf)

if __name__ == "__main__":
    vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    main()
