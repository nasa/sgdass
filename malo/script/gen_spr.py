#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   Program for computing the series of surface pressure.              *
# *                                                                      *
# * ## 12-APR-2016 all_loading_grid.py v2.0 (c) L. Petrov 07-FEB-2017 ## *
# *                                                                      *
# ************************************************************************
"""
import sys, os, shutil, time, subprocess, datetime, signal
from   geos_oper_config  import *
from   malo_exe          import *
global malo_child_pid

TMP_DIR = "/tmp"

def gen_spr ( config, date_beg, date_end, parallel_opt, ivrb ):
#
# --- Generate command file
#
    com_list= []
    for paths, dirs, files in os.walk(config.geos_heb_dir):
        for k in range(0,len(files)):
#
# --------- Check for heb file with pivotal data
#
            heb_file = paths + "/" + files[k]
            if ( heb_file.find ( "/" + config.pivot_sds[0] + "/" ) < 1 ): continue
            ih = heb_file.rfind ( ".heb" )
            heb_date = heb_file[ih-13:ih]
#
# --------- Generate tha name of the surface pressure file
#
            spr_file = config.spr_dir + "/" + config.spr_pref + heb_date + ".heb.bz2"
            if ( date_beg == "new" ):
#
# -------------- Only new files. Skip command of the output file already exists
#
                 if ( os.path.isfile ( spr_file ) ): continue
            else:
#
# -------------- Skip the file that is out of date range
#
                 if ( heb_date < date_beg ): continue
                 if ( heb_date > date_end ): continue

#
# --------- Generate command for computation of the surface pressure
#
            com = config.gen_spr_exe  + " " + \
                  config.geos_heb_dir + " " + \
                  heb_date            + " " + \
                  config.gmao_gh      + " " + \
                  config.malo_elev    + " " + \
                  config.spr_dir + "/" + config.spr_pref + " " + \
                  config.compress_com
            com_list.append ( com )

#
# --- Sort the command list in the reverse chronoloigical order
#
    com_list.sort(reverse=True)

    com_file = TMP_DIR + "/spr__" + "%05d" % os.getpid() + ".fil"

#
# --- Write the list of commands
#
    w = open(com_file,"w")
    for line in com_list:
        print ( line, file=w )
    w.close()

#
# --- Make it executable
#
    os.chmod ( com_file, 0o755 )

#
# --- Generate a command that will execute this list in parallel
#
    spr_com = "parallel"            + " "    + \
              parallel_opt          + " -a " + \
              com_file

    if ( ivrb >= 2 ): 
         print ( "Command:", spr_com )
         print ( "Did not initiate execcution since in the debugging mode" )
         exit  ( 0 )
    if ( ivrb >=- 1 ): print ( "Executing command", spr_com )

#
# --- Exectue the parallel command
#
    (ret, out) = exe ( spr_com, ivrb )
    if ( ret != 0 ):
         print ( "Error in spr_com: ", "\n".join(out) )
         exit  ( 1 )
    else:
         os.unlink ( spr_com )
         print ( "Successfull processing %d files" % len(com_list) )


def main():

    if ( len(sys.argv) <= 3 ):
         print ( "Usage: gen_spr.py filcnf date_beg date_end [ivrb]" )
         exit ( 1 )
    filcnf    = sys.argv[1]
    date_beg  = sys.argv[2]
    date_end  = sys.argv[3]
    if ( len(sys.argv) > 4 ):
         ivrb      = int(sys.argv[4])
    else:
         ivrb = 0

    hostname = os.uname()[1]

    if   ( hostname == "astrogeo" or hostname == "earthrotation" ): 
           host_suffix = "astrogeo"
           parallel_opt = "-P 16 --keep-order"  # parallel option
    elif ( hostname == "gs698-geopod.gsfc.nasa.gov" ): 
           host_suffix = "geopod"
           parallel_opt = "-P 10 --keep-order"  # parallel option
    else:
           host_suffix = "unknown"
    
    config  = geos_config_class ( filcnf ) 
    parse_geos_oper_config ( config )

    gen_spr ( config, date_beg, date_end, parallel_opt, ivrb )


if __name__ == "__main__":
    vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    signal.signal ( signal.SIGINT,  malo_signal_handler )
    signal.signal ( signal.SIGTERM, malo_signal_handler )
    signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
    os.environ["OMP_NUM_THREADS"] = "1"
    main()
