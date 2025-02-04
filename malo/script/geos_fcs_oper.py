#!/usr/bin/env python
"""
# ************************************************************************
# *                                                                      *
# *   Program for automatic downloading GEOS data.                       *
# *                                                                      *
# *   get_geos_oper analyzes the directory tree on a local computer      *
# *   with geos data in heb-format. It generates the list of geos data   *
# *   that are missing up to the current date, checks whether they are   *
# *   available at the remote server. It downloads those files that      *
# *   are available, parses them, converts to heb-format, compresses,    *
# *   and copies the original data to the storage directory.             *
# *                                                                      *
# * ### 04-JAN-2015  geos_forecast.py v1.10 (c) L. Petrov 12-JAN-2024 ## *
# *                                                                      *
# ************************************************************************
"""
import sys, os, shutil, time, subprocess
import optparse 
import math
import datetime
from   datetime import timedelta
from   Message     import *
from   geos_oper_config           import *
from   parse_geos_forecast_config import *

#
# --- Important constants
#
get_geos__label = "geos_fcs_oper.py  v 1.11 of 2024.11.25"

#
# ===========================================================================
#
def get_geos_forecast ( config, run_level, dry_run, ivrb ):
#"""
#  This is the main routine for processing remote geos forecast data
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
    print ( "==========================================" )
    print_log ( config, "started" )
    sys.stdout.flush()

    pattern = "/" + config.pivot_sds[0] + "/" + config.pivot_sds[0] + "_"
    config.geosfp_heb_list  = get_directory_tree ( config.geosfp_heb_dir,  pattern )
    config.geosfcs_heb_list = get_directory_tree ( config.geosfcs_heb_dir, pattern )

    if ( ivrb > 5 ):
         print ( "len-fp: ", len(config.geosfp_heb_list) )
         for line in config.geosfp_heb_list:
             print ( "config.geosfp_heb_list: " + line )

         print ( "len-fcs: ", len(config.geosfcs_heb_list) )
         for line in config.geosfcs_heb_list:
             print ( "config.geosfcs_heb_list: " + line )

    il = len(config.geosfp_heb_list[len(config.geosfp_heb_list)-1])
    if ( config.geosfp_heb_list[len(config.geosfp_heb_list)-1][il-4:il] == ".bz2" ):
         dat_last_geosfp = config.geosfp_heb_list[len(config.geosfp_heb_list)-1][il-21:il-8]
    else:
         dat_last_geosfp = config.geosfp_heb_list[len(config.geosfp_heb_list)-1][il-14:il-4]

    date_last_geosfp = datetime.datetime.strptime ( dat_last_geosfp , "%Y%m%d_%H%M")

    url_list = []
    num_epc_back  = int(config.look_back_hours/config.time_step_mod_hours)
    num_epc_frwd  = int(config.look_ahead_hours/config.time_step_mod_hours)
    if ( pyvers >= "3.12" ):
         dat_now = datetime.datetime.now(datetime.UTC).strftime("%Y%m%d_%H")   
    else:
         dat_now = datetime.datetime.utcnow().strftime("%Y%m%d_%H")   
    if ( ivrb > 2 ):
         print ( "dat_last_geosfp: " + dat_last_geosfp + "   dat_now: " + dat_now )

    for i in range(-num_epc_back,num_epc_frwd):
#
# ----- Cycle over the range epochs back/epochs forward
#
        date_geosfp = date_last_geosfp + timedelta(hours=i*config.time_step_mod_hours)
        if ( date_geosfp.hour ==  3 or \
             date_geosfp.hour ==  9 or \
             date_geosfp.hour == 15 or \
             date_geosfp.hour == 21    ): continue
#
# ----- Epoch under consideration
#
        dat_geosfp  = date_geosfp.strftime("%Y%m%d_%H")
        hour_geosfp = int(date_geosfp.strftime("%H"))
        if ( dat_geosfp > dat_now ): continue
        if ( ivrb > 3 ):
             print ( "i=", i, " dat_geosfp = ", dat_geosfp )
        for j in range(0,int(config.look_ahead_hours/config.time_step_mod_hours)):
#
# --------- Cycle over epochs of the forecast
#
            fcs_dat = str ( (date_geosfp + timedelta(hours=j*config.time_step_mod_hours)).strftime("%Y%m%d_%H%M") )
#
# --------- Important: issue epoch that starts from 6 or 18 have no more 
# --------- than 11 forecast epochs. Therefore, we consider no more
# --------- than 11 forecast epochs. 
#
            if ( ( hour_geosfp == 6 or hour_geosfp == 18 ) and j > 10 ):
                 continue
#
# --------- Generage heb-file name
#
            heb_file = config.geosfcs_heb_dir + "/" + str(date_geosfp.year) + \
                            "/" + config.pivot_sds[0] + "/" + config.pivot_sds[0] + "_" + \
                            dat_geosfp + "+" + fcs_dat  + ".heb.bz2"
            if ( ivrb > 4 ):
                 if ( heb_file in config.geosfcs_heb_list ):
                      str_exist = "exist"
                 else:
                      str_exist = "not found"
                 print ( "i=", i, " j= ", j, " heb_file = ", heb_file, " ", str_exist )
            if ( heb_file in config.geosfcs_heb_list ): continue

            if ( not os.path.isfile ( heb_file ) ):
                 url_year  = "Y%04d" % date_geosfp.year
                 url_month = "M%02d" % date_geosfp.month
                 url_day   = "D%02d" % date_geosfp.day
                 url_hour  = "H%02d" % date_geosfp.hour
                 url_ymdh  = "%04d%02d%02d_%02d" % ( date_geosfp.year,  \
                                                     date_geosfp.month, \
                                                     date_geosfp.day,   \
                                                     date_geosfp.hour   )
                 url = config.url_template.replace("Y@@@@",url_year)\
                                          .replace("M@@",url_month)\
                                          .replace("D@@",url_day)\
                                          .replace("H@@",url_hour)\
                                          .replace("d%%%%%%%%_%%",url_ymdh)\
                                          .replace("e%%%%%%%%_%%%%",fcs_dat)
                 url_list.append ( url )

    if ( ivrb > 0 ):
         if ( len(url_list) == 0 ):
              print ( "get_forecast: no new files has been found" )
              exit ( 1 )
         else:
              print ( "get_forecast: ", len(url_list), " urls will be checked" )
              sys.stdout.flush()

    if ( ivrb > 3 ):
         print ( "url_list= ", "\n".join(url_list) )

    if ( ivrb == 13 ): exit ( 13 )
    nurl = 0
    purl = 0
    last_proc_date = "n/a"
    last_fcs_date  = "n/a"
    for url in url_list:
        nurl = nurl + 1
        if ( ivrb > 1 and not dry_run ):
             print ( "processing ", nurl, " ( ", len(url_list), " ) url: ", url )
             sys.stdout.flush()
        (ret, proc_date, fcs_date ) = process_forecast ( config, url, run_level, dry_run, ivrb )
        if ( ret == 0 ): 
             if ( proc_date != "n/a" ):
                  last_proc_date = proc_date
             if ( fcs_date != "n/a" ):
                  last_fcs_date = fcs_date
             purl = purl + 1
        elif ( ret == 1 ): 
             continue
        elif ( ret == 2 ): 
             print_err ( config, "Error in converting to heb of the url " + url )
             continue
        else: 
             print_err ( config, "Error in processing url " + url )

#
# --- We finished
#
    if ( purl > 0 ):
         print_suc ( config, "Number of files processed: %d. Last file for date %s" % \
                   ( nurl, last_proc_date ) )
         print_log ( config, "finished. %d files were processed" % purl )
         previous_last_fcs_date = " "
         if ( os.path.isfile(config.fcs_file) ):
#
# ----------- It is possible that the previosly processed fcs date is after 
# ----------- than the fcs file processed right now.. Let us check it.
# ----------- Read the fcs status file
#
              with open ( config.fcs_file ) as f:
                   fcs_str = f.readlines()
              f.close ( )
#
# ----------- Extract the last word
#
              fcs_word = fcs_str[0].split()[-1]
#
# ----------- Check, whether this is the date
#
              if ( len(fcs_word) == 13 ):
                   if ( fcs_word[8:9] == "_" ):
                        previous_last_fcs_date = fcs_word
#
# ----------- Check the dates. If the previous fcs data is the newest, 
# ----------- keep this.
#
              if ( previous_last_fcs_date > last_fcs_date ):
                   last_fcs_date = previous_last_fcs_date 
         print_fcs ( config, "Number of files processed: %d. Last file for date %s" % \
                   ( nurl, last_fcs_date ) )
         print_log ( config, "Number of files processed: %d. Last forecast date: %s, previous forecast date: %s " % \
                   ( nurl, last_fcs_date, previous_last_fcs_date ) )
    else:
         print_log ( config, "finished: no new files were processed" )

#
# ------------------------------------------------------------------------
#
def process_forecast ( config, url, run_level, dry_run, ivrb ):

    date_tag = "??????????????"
    id = url.rfind("/")+1
    log_buf = []
    if ( run_level == 0 or run_level == 1 ):
         wget_command = "wget " + \
                        "--no-check-certificate " + \
                        "--timeout=30 " + \
                        "--tries=64 " + \
                        "-nH " + \
                        "--cut-dirs=12 " + \
                        "-c " + \
                        "--retry-connrefused " + \
                        "--password=\"\" " + \
                        " -O " + \
                        config.geos_temp_dir + "/" + url[id:] + " " + \
                        url
         if ( dry_run ):
              print ( wget_command )
              return ( 0, 'n/a', 'n/a' )
         if ( ivrb > 2 ): 
              print ( "wget_command: ", wget_command ) 
              sys.stdout.flush()

         (ret, out) = wget_exe ( wget_command, log_buf )
         if ( ret != 0 ):
              loc_file = config.geos_temp_dir + "/" + url[id:] 
              if ( os.path.isdir(loc_file) ): os.remove ( loc_file )
              if ( ivrb > 0 ):
                   for str in out:
                       if ( "No such file"      in str or  \
                            "No such directory" in str or  \
                            "404 Not Found"     in str or  \
                            "File not found"    in str     ):
                             print ( "File not found: ", url )
                             return ( 1, 'n/a', 'n/a' )
                   print ( "Error in wget_command: ", wget_command )
                   for str in out:
                       print ( "Error: ", str )
                   sys.stdout.flush()
                   return ( 1, 'n/a', 'n/a' )

    if ( run_level == 0 or run_level == 2 ):
         year = url[id-17:id-13]
         if ( not os.path.isdir(config.geosfcs_heb_dir + "/" + year) ):
              os.mkdir (  config.geosfcs_heb_dir + "/" + year )
         to_heb_com = config.to_heb_exe + " " + config.geos_temp_dir + "/" + \
                      url[id:] + " " + config.geosfcs_heb_dir + "/" + year + " " + \
                      config.compress_com
         if ( dry_run ):
              print ( to_heb_com )
              return ( 0, 'n/a', 'n/a' )
         if ( ivrb > 1 ): 
              print ( "to_heb_command: ", to_heb_com ) 
              sys.stdout.flush()
         (ret, out) = exe ( to_heb_com )
         if ( ret != 0 ):
              Message ( "E", "Error an attempt to extract the SDS(s) " + \
                             "from " + config.geos_temp_dir + "/" + \
                              url[id:] + " and write it(them) down in HEB-format" )
              print ( out )
              sys.stdout.flush()
              return ( 2, 'n/a', 'n/a' )
         tmp_file = config.geos_temp_dir + "/" + url[id:]
         if ( os.path.isfile ( tmp_file ) ):
              os.remove ( tmp_file )

    if ( config.aam_exe != "blank" and ( run_level == 0 or run_level == 3 ) ):
         file_name = config.geos_temp_dir + "/" + url[id:] 
         date_tag = file_name[len(file_name)-33:len(file_name)-8]
         aam_com = config.aam_exe + " " + \
                   config.geosfcs_heb_dir + " " + \
                   date_tag + " " + \
                   config.aam_igh + " " + \
                   config.aam_ogh + " " + \
                   config.aam_ls_mask + " " + \
                   config.aam_pref                 
         if ( dry_run ):
              print ( aam_com )
              return ( 0, 'n/a', 'n/a' )
         if ( ivrb > 1 ): print ( "aam_com: " + aam_com )
         (ret, out) = exe ( aam_com )
         if ( ret != 0 ):
              Message ( "E", "Error an attempt to compute the atmospheric " + \
                             "angular momentum" )
              print_err ( config, "Error an attempt to compute the atmospheric " + \
                                  "angular momentum" )
              print ( out )
              exit ( 1 )

    if ( run_level == 0 or run_level == 4 ):
         if ( not ( config.aam_ser_exe == "blank" ) ):
              aam_dir = config.aam_pref[0:config.aam_pref.rindex("/")]
              aam_ser_com = config.aam_ser_exe + " " + \
                            aam_dir + " " + \
                            "now"   + " " + \
                            "ser"   + " " + \
                            config.aam_ser_file
              if ( dry_run ):
                   print ( aam_ser_com )
                   return ( 0, 'n/a', 'n/a' )
              if ( ivrb > 1 ): print ( "aam_ser_com: " + aam_ser_com )
              (ret, out) = exe ( aam_ser_com )
              if ( ret != 0 ):
                   Message ( "E", "Error an attempt to generate the file with " + \
                                  "series of atmospheric angular momentum" )
                   print_err ( config, "Error an attempt to generate the file with " + \
                                  "series of atmospheric angular momentum" )
                   print ( out )
                   exit ( 1 )

    
    proc_date = date_tag[0:11]+"00" 
    fcs_date  = date_tag[12:25]
    return ( 0, proc_date, fcs_date )

#
# ------------------------------------------------------------------------
#
def get_directory_tree ( dir, pattern ):
    finam_list = []
    for paths, dirs, files in os.walk(dir):
        for file in files:
            name = paths + "/" + file
            if ( name == "."  ): continue
            if ( name == ".." ): continue
            if ( name.rfind ( "~" ) > 1 ): continue
            if ( name.rfind ( "#" ) > 1 ): continue
            if ( pattern != "" ):
                 if ( name.rfind ( pattern ) < 0 ): continue
            if ( os.path.isdir(name) ):
                 new_list = get_directory_tree ( name, pattern )
                 if ( len(new_list) > 0): 
                      for new_file in new_list:
                          finam_list.append ( new_file )
            else:
                 finam_list.append ( name )
 
    finam_list.sort()
    return ( finam_list )

#
# ------------------------------------------------------------------------
#
def fcs_append_listing ( config, ivrb, url_dir ):
    wget_command = "cd " + config.geos_temp_dir + " ; " + \
                   "wget " + \
                   "--timeout=30 " + \
                   "--no-check-certificate " + \
                   "--tries=64 " + \
                   "-nH " + \
                   "--cut-dirs=12 " + \
                   "--no-remove-listing " + \
                   "-c " + \
                   "--retry-connrefused " + \
                   "--password=\"\" -A 'PODSTANOVKA' " + \
                   url_dir
    if ( ivrb > 2 ): print ( "wget_command: " + wget_command )
    (ret, out) = wget_exe ( wget_command, log_buf )
    if ( ivrb > 4 ): 
         print ( "wget_command ret= ", ret )
         print ( "wget_command out= ", out )
    listing_file = config.geos_temp_dir + "/.listing"
    with open ( listing_file ) as f:
        listing = f.readlines()
    f.close()
    for line in listing:
        name = line.split()[8]
        if ( name == '.'  ): continue   
        if ( name == '..' ): continue   
        if ( name[0:1] == 'Y' or \
             name[0:1] == 'M' or \
             name[0:1] == 'D' or \
             name[0:1] == 'H'    ):
             fcs_append_listing ( config, ivrb, url_dir + "/" + name + "/" )
        else:
             config.remote_url_list.append  ( url_dir + "/" + name  )


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
    (ret,err) = exe ( com )
#
# --- Remove temorary file with configuration
#
    if ( len(config_list) > 0 ):
         os.unlink ( finam )
    return    ( ret, err )

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
    print ( "get_forecast: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str, \
            file=suc_file_handle )
    suc_file_handle.flush()
    suc_file_handle.close()
#
# ------------------------------------------------------------------------
#
def print_fcs ( config, str ):
#"""
#   print string str into fcs-file specified in the config file
#"""
    now = datetime.datetime.now() 
    
#
    fcs_file_handle = open ( config.fcs_file, "w" )
    print ( "get_forecast: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str, \
            file=fcs_file_handle )
    fcs_file_handle.flush()
    fcs_file_handle.close()
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
    print ( "get_forecast: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str, \
            file=err_file_handle )
    err_file_handle.flush()
    err_file_handle.close()


#
# ------------------------------------------------------------------------
#
def print_log ( config, str ):
#"""
#   print string str into both log-file specified in config file and to  
#   stdout
#"""
    now = datetime.datetime.now() 
    
    print ( "get_geos_oper: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str )
    sys.stdout.flush()
#
    config.log_file_handle = open ( config.log_file, "a" )
    print ( "get_geos_oper: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str, \
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
def main():

    os.putenv ( "GOMP_STACKSIZE", "2000000")
##    os.putenv ( "LD_LIBRARY_PATH", "/opt64/lib:/progs/spd_20090409/src:/opt64/lib/python3.3/lib-dynload:/usr/lib" )

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

    opts.add_option ( "-d", "--dry-run", action="store_true", \
                      dest="dry_run", \
                      help="Dry run only" )

    opts.add_option ( "-u", "--url", action="store", \
                      dest="url", \
                      default=None, \
                      metavar="NAME", \
                      help="the url to process" )

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

    parse_geos_forecast_config ( config )

    if ( not opts.url ):
         get_geos_forecast ( config, opts.run_level, opts.dry_run, opts.ivrb )
    else:
         process_forecast  ( config, opts.url, opts.run_level, opts.dry_run, opts.ivrb )

if __name__ == "__main__":
    pyvers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( pyvers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    main()
