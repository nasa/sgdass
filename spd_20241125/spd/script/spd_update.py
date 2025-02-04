#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   Program  spd_update.py performs a cycle of computation slant path  *
# *   delay update.                                                      *
# *                                                                      *
# *  ### 21-JUN-2014 spd_update.py v3.0 (c)  L. Petrov  23-NOV-2024 ###  *
# *                                                                      *
# ************************************************************************
"""
import sys, os, shutil, time, subprocess
import optparse 
import math
import datetime
from   datetime import date, datetime, timedelta, timezone

spd_update__label = "spd_update.py  v 2.41 of 2024.11.29"
fmt__label      = "# SPD_CONFIG file.  Format of 2024.11.22"
config__num_par = 21 # The number of configuration parameters

test_mode = 0 # 0 for normal operation

class spd_config_class:
   def __init__ ( self, filename ):
       self.filename          = filename
#
       self.heb_dir             = None
       self.spd_conf            = None
       self.spd_dir             = None
       self.run_bspd_export     = False
       self.bspd_dir            = None
       self.bspd_export_dir     = None
       self.bspd_sandbox_dir    = None
       self.bspd_grace_int      = 60.0
       self.bspd_prefix         = None
       self.spd_prefix          = None
       self.spd_ext             = None
       self.pivot_name          = None
       self.spd_path            = None
       self.spd_3d_bin          = None
       self.spd_3d_toser        = None
       self.stop_file           = None
       self.err_file            = None
       self.suc_file            = None
       self.num_cpu             = None
       self.lock_file           = None
       self.lock_timeout        = None

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
       if ( line == fmt__label     ): continue
       if ( len(line.split()) == 0 ): continue
       if   ( line.split()[0]     == "#" ): continue
       if   ( line.split()[0]     == "heb_dir:"  ): 
              config.heb_dir       = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "spd_dir:"    ):
              config.spd_dir        = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "spd_conf:"   ):
              config.spd_conf       = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "bspd_dir:"   ):
              config.bspd_dir       = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "run_bspd_export:" ):
              par = line.split()[1].lower()
              if ( par == "yes" ):
                   config.run_bspd_export = True
              elif ( par == "no" or par == "none" ):
                   config.run_bspd_export = False
              else:
                   print ( "Erro in parsing configuration file %s" + \
                           " -- option run_bspd_export: " + \
                           " requires yes or no but %s was supplied" % \
                           ( config.filename, par ) )
                   exit ( 1 )

              num_par = num_par + 1
       elif ( line.split()[0]      == "spd_prefix:"   ):
              config.spd_prefix     = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "bspd_prefix:"  ):
              config.bspd_prefix    = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "bspd_export_dir:"  ):
              config.bspd_export_dir = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "bspd_sandbox_dir:"  ):
              config.bspd_sandbox_dir = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "bspd_grace_int:"  ):
              config.bspd_grace_int = float(line.split()[1])
              num_par = num_par + 1
       elif ( line.split()[0]      == "spd_ext:"    ):
              config.spd_ext        = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "pivot_name:" ):
              config.pivot_name     = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "spd_path:"   ):
              config.spd_path       = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "spd_3d_bin:" ):
              config.spd_3d_bin     = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "spd_3d_toser:" ):
              config.spd_3d_toser   = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "stop_file:"   ):
              config.stop_file      = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "err_file:"   ):
              config.err_file       = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "suc_file:"   ):
              config.suc_file       = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "lock_file:"  ):
              config.lock_file      = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "lock_timeout:"  ):
              config.lock_timeout   = float(line.split()[1])
              num_par = num_par + 1
       elif ( line.split()[0]      == "num_cpu:"    ):
              config.num_cpu        = int(line.split()[1])
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
def print_suc ( config, str ):
#"""
#   print string str into suc-file specified in the config file
#"""
    now = datetime.now() 
    
    suc_file_handle = open ( config.suc_file, "w" )
    print ( "spd_update: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str, \
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
    print ( "spd_update: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str, \
            file=err_file_handle )
    err_file_handle.flush()
    err_file_handle.close()

#
# ------------------------------------------------------------------------
#
def exe ( command ):
    """
    Auxiliary routine exe spawns a subprocess, 
    executes a shell command in the context of the subprocess, 
    waits for its completion, 
    and returns completion code and returns the output of the subprocess
    sent to stdout as a list of strings.
    """
    words = command.split()
    time_str = str(datetime.now().strftime("%Y.%m.%d_%H:%M:%S.") + "%6d" % datetime.now().microsecond).replace( " ", "0" )
    (ret, out) = subprocess.getstatusoutput ( command )
    return ( ret, out.split ( "\n" ) )

#
# ------------------------------------------------------------------------
#
def exe_nolog ( command ):
#"""
#    Spawn a supborcess, execute command in the context of the suborpess,
#    wait for its completion and return completion code and results.
#"""
    words = command.split()
    time_str = str(datetime.now().strftime("%Y.%m.%d_%H:%M:%S.") + "%6d" % datetime.now().microsecond).replace( " ", "0" )
    p = subprocess.Popen( command, shell=True)
    (pid, ret) = os.waitpid(p.pid, 0)
    return ( ret )

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
              (pid_str, time) = lines.split("\n")[0].split( " " ) 
         except:
#
# --------- It may happen the file is spoiled and we cannot read it correctly
#
            pid_str = "1" # process always runs, but cannot be killed
            time = "2010.04.19_12:08:13" # When Milan was born
         f.close()
         pid = int(pid_str)
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
#@              if ( time_since < config.lock_timeout ):
#@#
#@# ---------------- The process running for less than it is defiend in config.lock_timeout
#@# ---------------- Let us quit quietly and let it finish
#@#
#@                   print ( "%s Another process get_geos_oper, pid %d is still running for %d seconds" % \
#@                           ( datetime.now().strftime("%Y.%m.%d_%H:%M:%S"), \
#@                             pid, int(time_since) ) \
#@                         )              
#@                   exit ( 0 )

#
# --- Create lock file and write there the PID of the current process
# --- and the current time
#
    lock = open ( config.lock_file, "w" )
    now = datetime.now() 
    print ( "%08d %s" % (os.getpid(), now.strftime("%Y.%m.%d_%H:%M:%S")), \
             file=lock )
    lock.close()
#
# ------------------------------------------------------------------------
#
def check_gap ( config, filspd_list, ivrb ):

#
# -- This routine checks gaps in spd-files. If it finds gaps, 
# -- it creates missing spd-files with slant path delays
#
    if ( len(filspd_list) < 2 ):
         return 0

    il = len(filspd_list[len(filspd_list)-1])
    if ( filspd_list[len(filspd_list)-1][il-8:il] == ".spd.bz2" ):
         date1 = filspd_list[0][il-21:il-8]
         date2 = filspd_list[1][il-21:il-8]
    else:
         date1 = filspd_list[0][il-17:il-4]
         date2 = filspd_list[1][il-17:il-4]

#
# - Find the time step
#
    time_step = datetime.strptime ( date2, "%Y%m%d_%H%M" ) - \
                datetime.strptime ( date1, "%Y%m%d_%H%M" )

    date_last = datetime.strptime ( date2, "%Y%m%d_%H%M"  )
    filmis_list = []
    for i in range(2,len(filspd_list)):
#
# ----- Extract the substring with data embedded in file name
#
        il = len(filspd_list[i])
        if ( filspd_list[i][il-8:il] == ".spd.bz2" ):
             date_file = filspd_list[i][il-21:il-8]
        else:
             date_file = filspd_list[i][il-17:il-4]

        for k in range(1,len(filspd_list)):
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
                 if ( filspd_list[i][il-8:il] == ".spd.bz2" ):
                      filspd_miss = filspd_list[i][0:il-21] + \
                               date_pred.strftime("%Y%m%d_%H%M") + \
                               filspd_list[i][il-8:il] 
                 else:
                      filspd_miss = filspd_list[i][0:il-17] + \
                               date_pred.strftime("%Y%m%d_%H%M") + \
                               filspd_list[i][il-4:il] 
#
# -------------- Generate the name of heb file that corresponds to the this spd-file
#
                 heb_file = config.heb_dir + "/" + date_pred.strftime("%Y") + \
                            "/" + config.pivot_name + "/" + config.pivot_name + \
                            "_" + date_pred.strftime("%Y%m%d_%H%M") + \
                            ".heb"
                 if ( not os.path.isfile(heb_file) ): 
                      heb_file = heb_file + ".bz2"

                 if ( not os.path.isfile(heb_file) ):
#
# ------------------- heb-file does not exist. This sucks...
#
                      print     ( "check_gap: cannot find heb_file " + heb_file )
                      print_err ( config, "check_gap: cannot find heb_file " + heb_file )
                      exit ( 1 )

                 if ( ivrb > 2 ): print ( "Gap: ", filspd_miss, " heb_file= ", heb_file )

#
# -------------- Generate command line for computing missing path delay
#
                 com = config.spd_3d_bin + " " + \
                       config.spd_conf   + " " + \
                       heb_file + " " + \
                       config.spd_dir    + "/" + config.spd_prefix + \
                       " 2 "
                 if ( ivrb > 2 ): print ( com )

                 ret = exe_nolog ( com )
                 if ( ret != 0 ):
                      print ( "ERROR in running command ", com )
                      print ( "Returning code: ", ret )
                      print_err ( config, "Error in computing slant path delay" )
                      exit ( 1 )
#
# -------------- Append the list of missed files
#
                 filmis_list.append ( filspd_miss )
            else:
                 break

        date_last = date_pred
        try:
            datetime.strptime ( date_file, "%Y%m%d_%H%M"  )
        except:
            print ( "ERROR: broken name of the input file: %s" % filspd_list[i] )
            print ( "date_file >>%s<<" % date_file )
            print_err ( config, "ERROR: broken name of the inpu file: %s" % filspd_list[i] )
            print_err ( config, "date_file >>%s<<" % date_file )
            exit ( 1 )

    if ( len(filmis_list) > 0 ):
         for i in range(0,len(filmis_list)-1):
             filspd_list.append ( filmis_list[i] )

    filspd_list.sort()    
    return filspd_list

#
# ------------------------------------------------------------------------
#

def spd_update ( config, ivrb ):
#   """
#    Update of slant path delay in ascii format
#   """

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

    if ( ivrb > 1 ): 
         print  ( " " )
         print  ( "==========================================" )
         print  ( datetime.now().strftime("%Y.%m.%d_%H.%M.%S"), " spd_update.py started" )
    os.environ['OMP_NUM_THREADS'] = str(config.num_cpu)

    filspd_list = []
#
# ---- Make the list of files with slant path delay in ascii format
#
    for paths, dirs, files in os.walk(config.spd_dir):
        for k in range(0,len(files)):
            name = paths + "/" + files[k]
            if ( len(name) > 4 ):
                 if ( name[len(name)-4:len(name)] == ".spd" and \
                      name.find("#") < 0 ):
                      filspd_list.append ( name )
            if ( len(name) > 7 ):
                 if ( name[len(name)-8:len(name)] == ".spd.bz2" and \
                      name.find("#") < 0 ):
                      filspd_list.append ( name )

#
# ---- Sort that list 
#
    filspd_list.sort()    

    if ( len(filspd_list) < 1 ):
         print     ( "No spd files was found in directory " + config.spd_dir )
         print_err ( config, "No spd files was found in directory " + config.spd_dir )
         exit ( 1 )
      
#
# --- Get the epoch of the latest file with slant path delay
#
    il = len(filspd_list[len(filspd_list)-1])
    if ( filspd_list[len(filspd_list)-1][-8:] == ".spd.bz2" ):
         last_spd_date = filspd_list[len(filspd_list)-1][il-21:il-8]
    else:
         last_spd_date = filspd_list[len(filspd_list)-1][il-17:il-4]

    if ( ivrb > 1 ): 
         time_str_now = datetime.now().strftime("%Y.%m.%d_%H.%M.%S")
         print ( "spd_update.py %s Started processing. Last spd date: %s" %
                 ( time_str_now, last_spd_date ) )

#
# --- Check are there any gaps in splant path delay file list 
#
    filspd_list = check_gap ( config, filspd_list, ivrb )

#
# --- Walk over the directory tree with atmosphere state from the numerical weather model
#
    filheb_list = []
    for paths, dirs, files in os.walk(config.heb_dir):
        for k in range(0,len(files)):
            name = paths + "/" + files[k]
            if ( name.rfind("/d/d_") > 0 ):
                 il = len(name)
                 if ( name[il-4:il] == ".heb" ):
                      heb_date = name[il-17:il-4]
                 elif ( name[il-8:il] == ".heb.bz2" ):
                      heb_date = name[il-21:il-8]
                 if ( heb_date > last_spd_date ): 
                      filheb_list.append ( name )
#
# --- And sort that list
#
    filheb_list.sort()    

    if ( ivrb >= 2 and len(filheb_list) == 0 ):
         time_str_now = datetime.now().strftime("%Y.%m.%d_%H.%M.%S")
         print ( "spd_update.py %s No new heb-files.   Last spd date: %s" % \
                 ( time_str_now, last_spd_date ) )
         return ( 0, "No heb files" )

    if ( ivrb >= 2 ): 
         print ( "Files to process: ", len(filheb_list) )
         print ( "last_heb_date = ", filheb_list[len(filheb_list)-1] )

#
# --- Cycle over data files with numerical weather model in heb format
#
    if ( test_mode != 0 ): filheb_list = []
    num_files = len(filheb_list)
    for i in range(0,len(filheb_list)):
#
# ----- Generate command for slant path delay computation using
# ----- the output of numberical weather model file filheb_list[i]    
#
        com = config.spd_3d_bin + " " + \
              config.spd_conf   + " " + \
              filheb_list[i]    + " " + \
              config.spd_dir    + "/" + config.spd_prefix + \
              " %d" % ivrb
        if ( ivrb > 0 ):
             if ( filheb_list[i][il-4:il] == ".heb" ):
                  heb_date = filheb_list[i][il-17:il-4]
             elif ( filheb_list[i][il-8:il] == ".heb.bz2" ):
                  heb_date = filheb_list[i][il-21:il-8]

             time_str_now = datetime.now().strftime("%Y.%m.%d_%H.%M.%S")
             print ( "spd_update %s processing input file %d of %d for date %s" % \
                     ( time_str_now, i+1, num_files, heb_date) )
             sys.stdout.flush()

        if ( ivrb > 2 ): 
             print ( "About to execute command ", com )
         
#
# ----- Execute command
#
        ret = exe_nolog ( com )
        if ( ret != 0 ):
             print ( "ERROR in running command ", com )
             print ( "Returning code: ", ret )
             print_err ( config, "Error in computing slant path delay" )
             exit ( 1 )

    if ( ivrb >= 2 ):
         time_str_now = datetime.now().strftime("%Y.%m.%d_%H.%M.%S")
         print ( "spd_update %s finished processing %d input files " % \
                 ( time_str_now, len(filheb_list) ) )

    if ( stop_line == "no_bin" or stop_line == "no_binary" ):
         print ( "Update of binary files was stopped according to the stop file " + \
                  config.stop_file )
    else:
#
# ------ Update binary files with slant path delay in the internal directory
#
         com = config.spd_3d_toser + " " + \
               config.spd_dir  + " " + \
               config.bspd_dir + "/" + config.bspd_prefix + " " + \
               "update" + " " + \
               " %d" % ivrb

         if ( ivrb > 1 ): 
              print ( "spd_update.py: About to execute command %s" % com )

         ret = exe_nolog ( com )
         if ( ret != 0 ):
              print ( "ERROR in running command ", com )
              print ( "Returning code: ", ret )
              print_err ( config, "Error in converting path delays to binary format" )
              exit ( 1 )

         if ( ivrb > 1 ): 
              print  ( " ", datetime.now().strftime("%Y.%m.%d_%H.%M.%S"), 
                            " spd_update.py finished" )
              print  ( "  " )

    return ( num_files, heb_date )

#
# ------------------------------------------------------------------------
#
def spd_export ( config, ivrb ):
#   """
#   Generate path delay extension in the sandbox direcotry
#   """

#
# ------ Update the summary file in the external directory
#
    com = "spd_3d_toser /tmp/ " + config.bspd_export_dir + "/" + config.bspd_prefix + " " + \
          "summary %d" % ivrb 
    if ( ivrb > 2 ): 
         print ( "spd_update.py: About to execute command %s" % com )
    (ret,out) = exe ( com )
    if ( ret != 0 ):
         print ( "ERROR in running command ", com )
         print ( "Returning code: ", ret )
         print_err ( config, "Error in computing slant path delay" )
         return ( 1, "Error in spd_export" )

#
# --- Check the internal directory with path delays
#
    end_epoch_int = "??"
    end_epoch_ext = "??"
    com = "bspd_util check " + config.bspd_dir

    if ( ivrb >=3 ):
         print ( "spd_update.py: About to execute command %s" % com )
    (ret,out) = exe ( com )
    if ( ret != 0 ):
         print ( "ERROR in running command ", com )
         print ( "Returning code: ", ret )
         print_err ( config, "Error in computing slant path delay" )
         return ( 1, "Error in spd_export" )

#
# --- Extract from there the end epoch and the time step of time series
#
    if ( ivrb >=3 ):
         print ( "spd_update.py: Search for the end epoch" )
    for line in out:
        if ( "End epoch:" in line ):
             end_epoch_int = line.split()[3]
        if ( "Time_step:" in line ):
             tim_step = float(line.split()[4])
    if ( end_epoch_int == "??" ):
         print_err ( config, "Trap of internal control: no End epoch from command %s" % com )
         print     (         "Trap of internal control: no End epoch from command %s" % com )
         return ( 1, "Error in spd_export" )

#
# --- Check the external directory with path delays and 
# --- extract the time step in the external directory
#
    com = "bspd_util check " + config.bspd_export_dir

    if ( ivrb >=3 ):
         print ( "spd_update.py: About to execute command %s" % com )
    (ret,out) = exe ( com )
    if ( ret != 0 ):
         print ( "ERROR in running command ", com )
         print ( "Returning code: ", ret )
         print_err ( config, "Error in computing slant path delay" )
         return ( 1, "Error in spd_export" )

    for line in out:
        if ( "End epoch:" in line ):
             end_epoch_ext = line.split()[3]

    if ( end_epoch_ext == "??" ):
         print_err ( config, "Trap of internal control: no End epoch from command %s" % com )
         return ( 1, "Error in spd_export" )

    if ( end_epoch_ext == end_epoch_int ):
         if ( ivrb >= 0 ):
              print ( "Internal and external binary directories are in sync" )
         return ( 0, "Internal and external binary directories are in sync" )
    else:

#
# ------ External directory has less data then internal direcory.
# ------ Determine the time epochs of range that the exteranl
# ------ directory needs be updated [tim_extract_beg, tim_extract_int]
#
         tim_end_epoch_int = datetime.strptime ( end_epoch_int[0:19], "%Y.%m.%d-%H:%M:%S" )
         tim_end_epoch_ext = datetime.strptime ( end_epoch_ext[0:19], "%Y.%m.%d-%H:%M:%S" )         
         tim_extract_beg   = tim_end_epoch_ext + timedelta ( seconds= tim_step )
         extract_beg = datetime.strftime ( tim_extract_beg, "%Y.%m.%d-%H:%M:%S" )

         if ( ivrb >= 2 ):
              print ( "end_epoch_int: ", end_epoch_int, tim_end_epoch_int  )
              print ( "end_epoch_ext: ", end_epoch_ext, tim_end_epoch_ext  )
              print ( "time_step:     ", tim_step ) 
              print ( "extract_beg:   ", extract_beg )

#
# ------ Read the summary of the internal directory with slant path delays
#
         bspd_sum_file = config.bspd_dir + "/bspd_summary.txt"
         bspd_sum_buf = []
         with open(bspd_sum_file,encoding="latin") as f:
              for line in f:
                  bspd_sum_buf.append ( line.strip("\n").strip("\r") )
         f.close()

         if ( pyvers >= "0312000" ):
              tim_utc_now = datetime.now(timezone.utc)
         else:
              tim_utc_now = datetime.utcnow()
         
         if ( ivrb >= 3 ): 
              print ( "Extracting of sections of path delays" )
#
# ------ Extract a section in the  path delay file for each station
#
         bspd_list = []
         for line in bspd_sum_buf:
             if ( "Prefix:" in line ):
                  finam_prefix = line.split()[1]
                  
             if ( "Station_name:" in line ):
#
# --------------- Get station name
#
                  sta_nam = line[14:22].lower()
#
# --------------- Build the name of the slant path deley in binary format
#
                  bspd_file = config.bspd_dir + "/" + finam_prefix + sta_nam.replace(" ","_") + ".bspd"

#
# --------------- Extract the section of slant path delay for the differences between
# --------------- the contents of internal slant path delays in binary format
# --------------- with respect to the contents of external slant path delays.
# --------------- A file with that section is placed in the internal binary directory
#
                  com = "bspd_util extract " + bspd_file + " " + extract_beg 

                  if ( ivrb >= 3 ): 
                       print ( "spd_update.py: About to execute command %s" % com )

                  (ret,out) = exe ( com )
                  if ( ret != 0 ):
                       print ( "ERROR in running command ", com )
                       print ( "Returning code: ", ret )
                       print_err ( config, "Error in computing slant path delay" )

#
# -------------------- Remove all slant path extensions from the internal directory in a case of error
#
                       for paths, dirs, files in os.walk(config.bspd_dir):
                           for file in files:
                               if ( file[-4:] == ".dat" ):
                                    finam = paths + "/" + file
                                    os.unlink ( finam )

                       exit ( 1 )
#
# --------------- Put the oritinal file with slant path delay and the file
# --------------- with and extracted secltion of path delay into bspd_list list
#
                  ext_file = bspd_file + "_"  + datetime.strftime ( tim_extract_beg,   "%Y%m%d_%H%M%S" ) + \
                                         "__" + datetime.strftime ( tim_end_epoch_int, "%Y%m%d_%H%M%S" ) + \
                                         ".dat"
                  bspd_list.append ( (bspd_file, ext_file) )

#
# ------ Generate contents of the lock file
#
         str_utc_now = tim_utc_now.strftime("%Y%m%d_%H%M%S")
         str_utc_end = (tim_utc_now + timedelta(seconds=60)).strftime("%Y%m%d_%H%M%S")
         lock_contents = "pid: %08d  start: %s  end: %s " % ( os.getpid(), str_utc_now, str_utc_end )

#
# ------ Write the lock file in the sandbox directory
#
         ext_lock_file = config.bspd_sandbox_dir + "/lock"
         if ( ivrb >= 3 ): 
              print ( "Writing lock in the sandbox direcotory %s" % ext_lock_file )
         f = open ( ext_lock_file, "w" )
         print ( lock_contents, file=f )
         f.close()

         if ( ivrb >= 3 ): 
              print ( "Move slant path delay extension to the sandbox" )
#
# ------ Move slant path delay extensions to the sandbox
#
         for bspd_tulip in bspd_list:
             bspd_file = bspd_tulip[0]
             ext_file  = bspd_tulip[1]
             com = "mv " + ext_file + " " + config.bspd_sandbox_dir  + "/" 
             if ( ivrb >= 3 ):
                  print ( "About to execute %s" % com )
             (ret,out) = exe ( com )
             if ( ret != 0 ):
                  print ( "ERROR in running command ", com )
                  print ( "Returning code: ", ret )
                  print_err ( config, "Error in computing slant path delay" )
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
         update_str = bspd_list[0][1][-36:-4]
         update_file = config.bspd_sandbox_dir + "/update_" + str_utc_now + ".txt"
         update_file_tmp = update_file + "__%08d" % os.getpid()

         f = open ( update_file_tmp, "w" )
         print ( update_str, file=f )
         f.close()
         os.rename ( update_file_tmp, update_file )

         return ( 0, "Create update file %s for epochs %s " % ( update_file, update_str ) )
#
# ------------------------------------------------------------------------
#
def main():

    if ( sys.version[:3] < "3.0" ): 
         print ( "This script cannot run under Python-2" )
         exit ( 1 )

    opts = optparse.OptionParser( version=spd_update__label  )

    opts.add_option ( "-c", "--config", action="store", \
                      dest="config", \
                      metavar="NAME", \
                      help="Configuration file" )

    opts.add_option ( "-x", "--export-only", action="store_true", \
                      dest="export_only", \
                      default=False, \
                      metavar="NAME", \
                      help="Only export" )

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

#
# --- Initialize configuration
#
    config = spd_config_class ( opts.config ) 

#
# --- Read and parse configuration file
#
    parse_spd_oper_config ( config ) 

#
# --- Check locking status
#
    check_lock ( config )

    if ( opts.export_only ):
         (ret, out) = spd_export ( config, opts.ivrb )
         exit ( ret )

#
# --- Update slant path deays
#
    (n_fil, last_proc_date) = spd_update ( config, opts.ivrb )
    if ( test_mode != 0 ): n_fil = test_mode
    if ( n_fil > 0 ):
         if ( config.run_bspd_export ):
#
# ----------- Initiate update of the export slant path delays
#
              (ret, out) = spd_export ( config, opts.ivrb )
              print ( config, out[0] )
              if ( opts.ivrb > 0 ):
                   time_str_now = datetime.now().strftime("%Y.%m.%d_%H.%M.%S")
                   print ( "spd_update %s finished updating sandbox directory" % \
                           time_str_now )
         print_suc ( config, "Number of files processed: %d. Last file for date %s" % \
                     ( n_fil, last_proc_date ) )
               

if __name__ == "__main__":
    pyvers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( pyvers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    main()
