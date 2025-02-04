#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   Program  spd_frontend_update.py performs a cycle of slant path     *
# *   update at the frontend.                                            *
# *                                                                      *
# * # 21-NOV-2024 spd_frontend_update.py v1.0 (c L. Petrov 23-NOV-2024 # *
# *                                                                      *
# ************************************************************************
"""
import sys, os, shutil, time, subprocess
import optparse 
import math
import datetime
from   datetime import date, datetime, timedelta, timezone

spd_frontend_update__label = "spd_frontend_update.py  v 1.0 of 2024.11.24"
fmt__label        = "# SPD_FRONTEND_CONFIG file.  Format of 2024.11.22"
config__num_par   = 4 # The number of configuration parameters

time_eps = 180.0

class spd_config_class:
   def __init__ ( self, filename ):
       self.filename          = filename
#
       self.bspd_export_dir  = None
       self.bspd_sandbox_dir = None
       self.bspd_grace_int   = 60.0
       self.bspd_prefix      = None

   def init ( self ):
       __init__ ( self )

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
def parse_spd_frontend_config ( config ):
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
       if ( line.split()[0]      == "bspd_prefix:"  ):
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
def update_fronend ( config, ivrb ):
#   """
#
#   """
    if ( pyvers >= "0312000" ):
         tim_utc_now = datetime.now(timezone.utc).replace(tzinfo=None)
    else:
         tim_utc_now = datetime.utcnow()

    finam_update_list = []
    for paths, dirs, files in os.walk(config.bspd_sandbox_dir):
        for file in files:
            if ( file[0:7] == "update_" and file[-4:] == ".txt" ) :
                 finam_update_list.append ( paths + "/" + file )

    if ( len(finam_update_list) == 0 ):
         return ( 0, "spd_frontend_update %s: No updates" % datetime.now().strftime("%Y%m%d_%H%M%S") )

    if ( ivrb >= 1 ):
         time_str_now = datetime.now().strftime("%Y.%m.%d_%H.%M.%S")
         print ( "spd_frontend_update %s About to start updating export directory %s" % \
                  (  time_str_now, config.bspd_export_dir ) )
    update_file = finam_update_list[-1]
    with open(update_file,encoding="latin") as f:
         for line in f:
             update_line= line.strip("\n").strip("\r") 
    f.close()
    time_update_beg = datetime.strptime( update_line[0:15],  "%Y%m%d_%H%M%S" )
    time_update_end = datetime.strptime( update_line[17:31], "%Y%m%d_%H%M%S" )
#
# --- Check for the lock file
#
    ext_lock_file = config.bspd_sandbox_dir + "/lock"
    if ( os.path.isfile ( ext_lock_file ) ):
         with open ( ext_lock_file ) as f:
              for line in f:
                  lock_buf = ( line.strip("\n").strip("\r") )
         f.close ( )
         if ( len(lock_buf.split()) >= 6 ):
              lock_expiry_utc = datetime.strptime ( lock_buf.split()[6], "%Y%m%d_%H%M%S" ).replace(tzinfo=None)
              if ( lock_expiry_utc >= tim_utc_now ):
                   return ( 0, "spd_frontend_update %s Directory is locked" % datetime.now().strftime("%Y%m%d_%H%M%S") )
              else:
#
# ---------------- The lock file has expired
#
                   os.unlink ( ext_lock_file )
#
# --- Read the summary of slant path delays
#
    bspd_sum_file = config.bspd_export_dir + "/bspd_summary.txt"
    bspd_sum_buf = []
    with open(bspd_sum_file,encoding="latin") as f:
         for line in f:
             bspd_sum_buf.append ( line.strip("\n").strip("\r") )
    f.close()

    bspd_list = []
    for line in bspd_sum_buf:
        if ( "Prefix:" in line ):
              finam_prefix = line.split()[1]
                  
        if ( "Max_epoch:" in line ):
              str_bspd_end = line.split()[3][0:19]
              tim_bspd_end = datetime.strptime ( str_bspd_end, "%Y.%m.%d-%H:%M:%S" )
                  
        if ( "Sample_Interval:" in line ):
              tim_step = float(line.split()[1])
              if ( abs(tim_bspd_end + timedelta(seconds=tim_step) - time_update_beg) > timedelta(time_eps) ):
                   return ( 1, "ERROR spd_frontend_update %s Found a wrong update %s that differs from the bspd end date %s by more than one time step %s" % \
                               ( datetime.now().strftime("%Y%m%d_%H%M%S"), update_line, str_bspd_end, line.split()[1] ) )
                  
        if ( "Station_name:" in line ):
#
# ---------- Get station name
#
             sta_nam = line[14:22].lower()
#
# ---------- Build the name of the slant path deley in binary format
#
             bspd_file = config.bspd_export_dir + "/" + finam_prefix + sta_nam.replace(" ","_") + ".bspd"
             extn_file = bspd_file.replace(config.bspd_export_dir,config.bspd_sandbox_dir) + "_" + \
                         update_line + ".dat"
             if ( not os.path.isfile(extn_file) ):
                  return ( 1, "ERROR spd_frontend_update: %s did not find expected extension file %s" % \
                              ( datetime.now().strftime("%Y%m%d_%H%M%S"), extn_file ) )

             bspd_list.append ( (bspd_file, extn_file) )

#
# --- Generate the extended binary slant path delay files in the sandbox directory
#
    if ( ivrb >= 2 ): 
         print ( "spd_frontend_update.py: About to comuited slant path delay extensions" )

    for bspd_tulip in bspd_list:
        bspd_file = bspd_tulip[0]
        extn_file = bspd_tulip[1]
        com = "bspd_util extend " + bspd_file + " " + extn_file
        if ( ivrb >= 3 ): 
             print ( "spd_frontend_update.py: About to execute command %s" % com )
        (ret,out) = exe ( com )
        if ( ret != 0 ):
             for line in out:
                 print ( line )
             return ( 1, "ERROR spd_frontend_update: %s failed to execute command %s" % \
                              ( datetime.now().strftime("%Y%m%d_%H%M%S"), com ) )

#
# --- Create lock file in the export directory. Lock will
# --- expire after 2 x grace period
#
    export_lock_file     = config.bspd_export_dir + "/lock"
    export_lock_file_tmp = export_lock_file + "__%08d" % os.getpid()
    if ( pyvers >= "0312000" ):
         tim_utc_now = datetime.now(timezone.utc).replace(tzinfo=None)
    else:
         tim_utc_now = datetime.utcnow()

    export_lock_contents = "Update started on %s Expires on %s UTC" % \
           (   tim_utc_now.strftime("%Y%m%d_%H%M%S"), \
             ( tim_utc_now + timedelta(seconds=2*config.bspd_grace_int) ).strftime("%Y%m%d_%H%M%S") )
#
# --- Write lock file in the export directory
#
    f = open ( export_lock_file_tmp, "w" )
    print ( export_lock_contents, file=f )
    f.close()
    os.rename ( export_lock_file_tmp, export_lock_file )
    
    if ( ivrb >= 3 ): 
         print ( "spd_frontend_update.py: Sleep for the gracious peroid of %f sec" % config.bspd_grace_int )
#
# --- Sleep for the gracious period
#
    time.sleep ( config.bspd_grace_int )

#
# --- Move updated bspd files from the sandbox to the export directory
#
    if ( ivrb >= 2 ): 
         print ( "spd_frontend_update.py: Aboput to move updated files to %s" % config.bspd_export_dir )
    for bspd_tulip in bspd_list:
        export_bspd_file  = bspd_tulip[0]
        sandbox_bspd_file = bspd_tulip[0].replace(config.bspd_export_dir,config.bspd_sandbox_dir)
        if ( ivrb >= 2 ): 
             print ( "spd_frontend_update.py: About to move %s to %s" % ( sandbox_bspd_file, export_bspd_file ) )
        os.rename ( sandbox_bspd_file, export_bspd_file )

#
# --- Remove all files with ".dat" and ".txt" extensions from the sandbox direcotry
#
    for paths, dirs, files in os.walk(config.bspd_sandbox_dir):
        for file in files:
            if ( file[-4:] == ".dat" ) :
                 os.unlink ( paths + "/" + file )
            if ( file[-4:] == ".txt" ) :
                 os.unlink ( paths + "/" + file )
#
# ------ Update the summary file in the external directory
#
    com = "spd_3d_toser /tmp/ " + config.bspd_export_dir + "/" + config.bspd_prefix + " " + \
          "summary %d" % ivrb 
    if ( ivrb >= 3 ): 
         print ( "spd_update.py: About to execute command %s" % com )
         sys.stdout.flush()
    (ret,out) = exe ( com )
    if ( ret != 0 ):
         for line in out:
             print ( line )
         return ( 1, "ERROR spd_frontend_update: %s failed to update summary with command %s" % \
                     ( datetime.now().strftime("%Y%m%d_%H%M%S"), com ) )

#
# -- Check the export directory
#
    com = "bspd_util check " + config.bspd_export_dir
    if ( ivrb >= 3 ): 
         print ( "spd_update.py: About to execute command %s" % com )
         sys.stdout.flush()
    (ret,out) = exe ( com )
    if ( ret != 0 ):
         for line in out:
             print ( line )
         return ( 1, "ERROR spd_frontend_update: %s check slant path delay in %s failed " % \
                     ( datetime.now().strftime("%Y%m%d_%H%M%S"), config.bspd_export_dir ) )

#
# --- Finally, remove the export lock file
#
    os.unlink ( export_lock_file  ) 

    return ( 0, "OK %s was updated for %s " % ( config.bspd_export_dir, update_line ) )

#
# ------------------------------------------------------------------------
#
def main():

    opts = optparse.OptionParser( version=spd_frontend_update__label  )

    opts.add_option ( "-c", "--config", action="store", \
                      dest="config", \
                      metavar="NAME", \
                      help="Configuration file" )

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
         print ( "Configuration file is not specied. Try spd_frontend_update -h to see options" )
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
    parse_spd_frontend_config ( config ) 

    (ret,out) = update_fronend ( config, opts.ivrb )
    if ( ret != 0 ):
         print ( out )
    else:
         if ( opts.ivrb >= 2 ):
              print ( out ) 
         elif ( opts.ivrb >= 1 ):
                time_str_now = datetime.now().strftime("%Y.%m.%d_%H.%M.%S")
                if ( not "No updates" in out ):
                     print ( "spd_frontend_update %s %s" % (  time_str_now, out[2:] ) )

if __name__ == "__main__":
    pyvers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( pyvers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    main()
