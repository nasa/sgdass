#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   Program that automatically generates pictures and radplots from    *
# *   FITS images and calibrated uv-data.                                *
# *                                                                      *
# * ### 22-NOV-2023   pictdir.py   v 1.0 (c)  L. Petrov 22-NOV-2023  ### *
# *                                                                      *
# ************************************************************************
"""
import pwd, sys, os, re, shutil, time, subprocess, datetime, operator, signal
sys.path.append("/usr")
import argparse 
import pima_local
from pima_exe import exe, exe_out_log, exe_out_nolog, exe_noout_log, exe_noout_nolog, \
                     pima_print_mes, pima_signal_handler_term
pictdir__label   = "pictdir.py"
pictdir__version = "pictdir.py v 1.0  20231122"

#
# ------------------------------------------------------------------------
#
def pictdir_exe ( ima_dir, verb ):

    map_list = []
    file_list = os.listdir ( ima_dir )
    for file in file_list:
        if ( len(file) < 9 ): continue
        if ( file[len(file)-8:len(file)] == "map.fits" ):
             map_list.append ( ima_dir + "/" + file )
    if ( len(map_list) == 0 ):
         return 0

    map_list.sort()

    num_ima = 0
    num_uva = 0
    for map_fits_file in map_list:
        num_uva = num_uva + 1
        id = map_fits_file.rfind("/")
        band = map_fits_file[id+12:id+13]
        sou_name = map_fits_file[id+1:id+11]
        if ( verb > 0 ):
             print ( "pictdir.py Making picture of source %s band %s  %5d ( %5d )" % \
                      ( sou_name, band, num_uva, len(map_list) ) )


        map_gif_file  = map_fits_file.replace("map.fits","map.gif")
        if ( band == "K" ):
             map_opt = "-box  8.0"
        elif ( band == "X" ):
             map_opt = "-box 25.0"
        elif ( band == "C" ):
             map_opt = "-box 40.0"
        elif ( band == "S" ):
             map_opt = "-box 80.0"
        else:
             map_opt = "-box 25.0"
#
# ----- Generates gif-image
#
        com = "fits_to_map -o " + map_gif_file + " " + map_opt + " " + map_fits_file
        if ( verb > 2 ):
             print ( "pictdir.py fits_to_map command: ", com )
        (ret, out) = exe_noout_nolog ( com, verb )
        if ( ret != 0 ):
             if ( verb > 1 ):             
                  print ( "pictdir.py ERROR: ", "\n".join(out) )

        uvs_fits_file = map_fits_file.replace("map.fits","uvs.fits")
        uvs_gif_file  = map_fits_file.replace("map.fits","uvs.gif")
#
# ----- Generate radplot
#
        com = "fits_to_radplot -auto -o " + uvs_gif_file + " " + uvs_fits_file
        if ( verb > 2 ):
             print ( "pictdir.py fits_to_radplot command: ", com )
        (ret, out) = exe_noout_nolog ( com, verb )
        if ( ret != 0 ):
             if ( verb > 1 ):             
                  print ( "pictdir.py ERROR: ", "\n".join(out) )
        num_ima = num_ima + 1

    return num_ima

#
# ------------------------------------------------------------------------
#
def main():
    """ 
    Parsing arguments
    """
    parser = argparse.ArgumentParser( description=pictdir__label )
    parser.add_argument('--version', action='version', version=pictdir__version )

#
# --- General options:
#
    parser.add_argument ( "-v", "--verbosity", action="store", \
                          dest="verb", \
                          default=1, \
                          metavar="value", \
                          type=int, \
                          help="Verbosity level" )
#
# --- Define positional arguments
#
    parser.add_argument ( "dir", \
                          help="FITS file with calibrated visibilities" )
#
# --- Get and parse options
#
    args = parser.parse_args()

    num_ima = pictdir_exe ( args.dir, args.verb )

    if ( args.verb > 0 ):
         print ( "pictdir.py: generated %d pictures in directory %s " % \
                  (num_ima, args.dir) )

#
# ------------------------------------------------------------------------
#
if __name__ == "__main__":
    global pima_child_pid
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTERM, pima_signal_handler_term )
        pima_child_pid = None
        main()
    except KeyboardInterrupt:
        print ( "pf.py: Interrupted" )
        exit ( 1 )
