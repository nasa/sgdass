#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   Program loa_spl_to_eph computes time series of mass loading for    *
# *   the list of stations using B-spline expansion of the mass loading  *
# *   on the globla grid.                                                *
# *                                                                      *
# *  ### 04-FEB-2017 loa_spl_to_eph v1.1 (c) L. Petrov  01-JUN-2017  ### *
# *                                                                      *
# ************************************************************************
"""
import sys, os, shutil, time, subprocess, datetime, signal
import optparse 
from   Message           import *
from   malo_exe          import *
from   geos_oper_config  import *
global malo_child_pid

os.environ["OMP_NUM_THREADS"] = "1"

transform_com = "loading_nc_to_spl_heb"
tmp_templ     = "/tmp/spl_to_eph"
tmp_file      = "%s__%05d" % ( tmp_templ, os.getpid() )

#
# -------------------------------------------------------------------------
#
def spl_signal_handler ( signal, frame ):
#
# --- handler to clean garbage
#
    global malo_child_pid
#
    try:
        if ( malo_child_pid ):
             os.kill ( malo_child_pid, signal )
    except:
        pass
#
    if ( os.path.isfile(heb_file) ): 
         try:
             os.unlink  ( tmp_file )
         except:
             pass
    print ( 'Terminated by TERM signal' )

    sys.exit(0)

#
# -------------------------------------------------------------------------
#

def load_spl_to_eph ( dir_in, sta_file, dir_out, date_beg, date_end, ivrb ):
#
# --- Learn how many cores to use depending on the host name
#
    hostname = os.uname()[1]
    if ( hostname == "astrogeo" ): 
         num_proc = 32
    elif ( hostname == "earthrotation" ): 
         num_proc = 4
    elif ( hostname == "gs698-geopod.gsfc.nasa.gov" ):
          num_proc = 10
    elif ( hostname == "gs61a-geodev-a" ): 
          num_proc = 30
    else:
         num_proc = 8

    parallel_opt  = "-P %s --keep-order" % num_proc # parallel option

#
# --- Scan input directory and generate command list
#
    com_list= []
    for paths, dirs, files in os.walk(dir_in):
        for k in range(0,len(files)):
#
# --------- generate name of the input spl_file and output eph_file
#
            spl_file = paths   + "/" + files[k]
            eph_file = dir_out + "/" + files[k].replace(".heb.bz2",".eph").replace("_spl_","_")
#
# --------- Extract the loading date
#
            ih = spl_file.rfind ( ".heb.bz2" )
            if ( ih == 0 ): continue
            loa_date = spl_file[ih-13:ih]
            if ( date_beg == "new" ):
#
# -------------- Only new files. Skip command of the output file already exists
#
                 if ( os.path.isfile ( eph_file ) ): continue
            else:
#
# -------------- Skip the file that is out of date range
#
                 if ( loa_date < date_beg ): continue
                 if ( loa_date > date_end ): continue
#
# --------- Generate command line...
#
            com = "loading_spl_heb_to_sta " + \
                   spl_file + " " + \
                   sta_file + " " + \
                   eph_file + " " + \
                   "%d" % ivrb
#
# --------- ... and put if in the list
#
            com_list.append ( com )
                    
    if ( ivrb > 0 ):
         print ( "The number of grid files to be processed: ", len(com_list) )
    
#
# --- Sort the command list
#
    com_list.sort(reverse=True)

#
# --- Write the comman into a temporary file
#
    f = open(tmp_file,"w")
    for com in com_list:
        print ( com, file=f )
    f.close


    if ( ivrb > 1 ):      
         print ( "command file to be executed: ", tmp_file )
    if ( ivrb == 3 ):      
         print ( "Debuggin mode: stop. Command file:", tmp_file )
         exit ( 1 )
#
# --- Generate a command that will execute the command file in parallel
#
    loa_com = "parallel"  + " " + \
              parallel_opt          + " " + \
              "-a " + tmp_file
#
# -- execute the command
#
    (ret, out) = exe ( loa_com, ivrb )
    if ( ret != 0 ):
         print ( "Error in loa_com: ", "\n".join(out) )
         exit  ( 1 )

    if ( ivrb < 2 ):      
#
# ------ Clean garabe
#
         if ( os.path.isfile(tmp_file) ): 
              try:
                 os.unlink  ( tmp_file )
              except:
                 pass
         print ( "\n".join(out) )

#
# ------------------------------------------------------------------------
#
def main():
    num_arg = len(sys.argv) - 1
    if ( num_arg < 3 ):
         print ( "Usage: loa_to_spl_heb.py dir_in station_file dir_out [date_beg] [date_end] [ivrb]" )
         exit ( 1 )
#
# --- Check arguments
#
    dir_in   = sys.argv[1]
    sta_file = sys.argv[2]
    dir_out  = sys.argv[3]
    if ( num_arg >= 4 ):
         date_beg = sys.argv[4]
    else:
         date_beg = "19700101_0000"

    if ( num_arg >= 5 ):
         date_end = sys.argv[5]
    else:
         date_end = "20491231_2359"

    if ( num_arg >= 6 ):
         ivrb    = int(sys.argv[6])
    else:
         ivrb    = 0

    if ( not os.path.isdir(dir_in) ):
         print ( "loa_spl_to_eph: input directory %s is not found" % dir_in )
         exit  ( 1 )

    if ( not os.path.isfile(sta_file) ):
         print ( "loa_spl_to_eph: input station file %s is not found" % sta_file )
         exit  ( 1 )

    if ( not os.path.isdir(dir_out) ):
         print ( "loa_spl_to_eph: put directory %s is not found" % dir_out )
         exit  ( 1 )
#
# -- Execute the utility
#
    load_spl_to_eph ( dir_in, sta_file, dir_out, date_beg, date_end, ivrb )


if __name__ == "__main__":
    vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    signal.signal ( signal.SIGINT,  spl_signal_handler )
    signal.signal ( signal.SIGTERM, spl_signal_handler )
    signal.signal ( signal.SIGTTOU, signal.SIG_IGN    )
    main()
