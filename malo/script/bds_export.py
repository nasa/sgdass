import sys, os, shutil, time, subprocess
import math
import datetime
from   datetime         import timedelta
from   geos_oper_config import *
from   Message          import *

bds_lock_time = 120.0 # 120 seconds lock time for update of export bds data

#
# ------------------------------------------------------------------------
#
def exe ( command, log_buf):
#"""
#    Spawn a subprocess, execute command in the context of the subprocess,
#    wait for its completion, and return completion the code and results.
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
def bds_export ( config, ivrb ):
#   """
#   Generate path delay extension in the sandbox directory
#   """

    log_buf = []
    pyvers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
#
# --- Check the internal directory with displacements 
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
         for line in out:
             print     ( line )
             print_err ( config, line )
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
    if ( ":" in config.load_bdsp_export_dir ):
         host = config.load_bdsp_export_dir.split(":")[0]
         load_bdsp_export_dir = config.load_bdsp_export_dir.split(":")[1]
         rem_com = "bds_util check " + load_bdsp_export_dir
         com = 'ssh ' + host + ' "source $HOME/.login ;  ' + rem_com + ' "'
    else:
         load_bdsp_export_dir = config.load_bdsp_export_dir
         com = "bds_util check " + load_bdsp_export_dir

    if ( ivrb >=3 ):
         print ( "geos_oper.py: About to execute command %s" % com )
    (ret,out) = exe ( com, log_buf )
    if ( ret != 0 ):
         print ( "ERROR in running command ", com )
         print ( "Returning code: ", ret )
         print_err ( config, "Error in checking external displacement directory %s" % \
                     config.load_bdsp_export_dir )
         for line in out:
             print     ( line )
             print_err ( config, line )
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
# ------ External directory has less data then internal directory.
# ------ Determine the time epochs of range that the exteranl
# ------ directory needs be updated [tim_extract_beg, tim_extract_int]
#
         tim_end_epoch_int = datetime.datetime.strptime ( end_epoch_int[0:19], "%Y.%m.%d-%H:%M:%S" )
         tim_end_epoch_ext = datetime.datetime.strptime ( end_epoch_ext[0:19], "%Y.%m.%d-%H:%M:%S" )         
         tim_extract_beg   = tim_end_epoch_ext + timedelta ( seconds= tim_step )
         extract_beg = datetime.datetime.strftime ( tim_extract_beg, "%Y.%m.%d-%H:%M:%S" )

         if ( ivrb >= 2 ):
              print ( "bds_export: External directory has less data then internal directory" )
              print ( "bds_export: end_epoch_int: ", end_epoch_int, tim_end_epoch_int  )
              print ( "bds_export: end_epoch_ext: ", end_epoch_ext, tim_end_epoch_ext  )
              print ( "bds_export: time_step:     ", tim_step ) 
              print ( "bds_export: extract_beg:   ", extract_beg )

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
         last_sta = "????"
         for line in bds_sum_buf:
             if ( line[0:5] == "STA: " ):
#
# --------------- Get station name
#
                  sta_nam = line.split()[2]
#
# --------------- Bypass duplicate station name
#
                  if ( sta_nam ==  last_sta ): continue
                  last_sta = sta_nam
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
              print ( "Writing lock in the sandbox directory %s" % ext_lock_file )
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
                  for line in out:
                      print     ( line )
                      print_err ( config, line )
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
