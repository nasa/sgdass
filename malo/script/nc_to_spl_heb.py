#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program nc_to_spl_heb reads the global gridded loading in netCDF   *
# *   format, expandes the displacement fields into B-spline basis and   *
# *   writes down the expabnsion in the specified output directory and   *
# *   compresses it.                                                     *
# *                                                                      *
# *  ### 03-DEC-2016   nc_to_spl_heb v1.1 (c) L. Petrov  04-FEB-2017 ### *
# *                                                                      *
# ************************************************************************
import sys, os, shutil, time, subprocess, datetime, signal
import optparse 
from   Message           import *
from   malo_exe          import *
from   merra_oper_config import *
from   geos_oper_config  import *
global malo_child_pid

os.environ["OMP_NUM_THREADS"] = "1"
transform_com = "loading_nc_to_spl_heb"
tmp_templ     = "/tmp/nc_to_spl"
tmp_file      = "%s__%05d" % ( tmp_templ, os.getpid() )

#
# -------------------------------------------------------------------------
#
def nc_signal_handler ( signal, frame ):
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

def nc_to_spl ( dir_in, dir_out, date_beg, date_end, ivrb ):
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
    else:
         num_proc = 8

    parallel_opt  = "-P 10 --keep-order"  # parallel option

#
# --- Scan input directory and generate command list
#
    com_list= []
    for paths, dirs, files in os.walk(dir_in):
        for k in range(0,len(files)):
            nc_file = paths + "/" + files[k]
#
# --------- Extract the loading date
#
            ih = nc_file.rfind ( ".nc" )
            if ( ih == 0 ): ih = nc_file.rfind ( ".nc.bz2" )
            if ( ih == 0 ): continue
            loa_date = nc_file[ih-13:ih]
            heb_file = nc_file.replace("/load_grid/","/load_spl/").replace(".nc",".heb").replace(".heb.bz2",".heb")
            il = len(heb_file)
            heb_file = heb_file[0:il-17] + "spl_" + heb_file[il-17:il]
            if ( date_beg == "new" ):
#
# -------------- Only new files. Skip command of the output file already exists
#
                 if ( os.path.isfile ( heb_file ) ): continue
                 if ( os.path.isfile ( heb_file + ".bz2" ) ): continue
            else:
#
# -------------- Skip the file that is out of date range
#
                 if ( loa_date < date_beg ): continue
                 if ( loa_date > date_end ): continue

            com = transform_com + " " + \
                  nc_file       + " " + \
                  heb_file      + " " + \
                  "lbzip2_1p1"

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


    loa_com = "parallel"  + " " + \
              parallel_opt          + " " + \
              "-a " + tmp_file

    if ( ivrb > 1 ):      
         print ( "command file to be executed: ", tmp_file )
    if ( ivrb == 3 ):      
         print ( "Debuggin mode: stop. Command file:", tmp_file )
         exit ( 1 )

    (ret, out) = exe ( loa_com, ivrb )
    if ( ret != 0 ):
         print ( "Error in par_com: ", "\n".join(out) )
         exit  ( 1 )

    if ( ivrb < 2 ):      
#
# ------ Clean garabe
#
         if ( os.path.isfile(heb_file) ): 
              try:
                 os.unlink  ( tmp_file )
              except:
                 pass

#
# ------------------------------------------------------------------------
#
def main():
    num_arg = len(sys.argv) - 1
    if ( num_arg < 2 ):
         print ( "Usage: nc_to_spl_heb.py dir_in dir_out [date_beg] [date_end] [ivrb]" )
         exit ( 1 )
    dir_in  = sys.argv[1]
    dir_out = sys.argv[2]
    if ( num_arg >= 3 ):
         date_beg = sys.argv[3]
    else:
         date_beg = "19700101_0000"

    if ( num_arg >= 4 ):
         date_end = sys.argv[4]
    else:
         date_end = "20491231_2359"
    if ( num_arg >= 5 ):
         ivrb    = int(sys.argv[5])
    else:
         ivrb    = 0

    nc_to_spl ( dir_in, dir_out, date_beg, date_end, ivrb )


if __name__ == "__main__":
    vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    signal.signal ( signal.SIGINT,  nc_signal_handler )
    signal.signal ( signal.SIGTERM, nc_signal_handler )
    signal.signal ( signal.SIGTTOU, signal.SIG_IGN    )
    os.environ["OMP_NUM_THREADS"] = "1"
    main()
