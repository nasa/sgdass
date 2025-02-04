#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   Program that autimatically generates images for all files with     *
# *   siffixes _uva.fits and _uvm.fits and generates pictures and        *
# *   radplots.                                                          *
# *                                                                      *
# * ### 15-MAY-2016  imadir.py v 1.2 (c)   L. Petrov   26-DEC-2017  ###  *
# *                                                                      *
# ************************************************************************
"""
import pwd, sys, os, re, shutil, time, subprocess, datetime, operator, signal
sys.path.append("/usr")
import argparse 
import pima_local
from pima_exe import exe, exe_out_log, exe_out_nolog, exe_noout_log, exe_noout_nolog, \
                     pima_print_mes, pima_signal_handler_term
imadir__label   = "imadir.py"
imadir__version = "imadir.py v 1.2  2017.12.26"

#
# ------------------------------------------------------------------------
#
def imadir_exe ( ima_dir, auto_script, pict_only, verb ):

    uva_list = []
    file_list = os.listdir ( ima_dir )
    for file in file_list:
        if ( len(file) < 9 ): continue
        if ( file[len(file)-8:len(file)] == "uva.fits" or \
             file[len(file)-8:len(file)] == "uvm.fits"    ):
             uva_list.append ( ima_dir + "/" + file )
    if ( len(uva_list) == 0 ):
         return 0

    uva_list.sort()

    num_ima = 0
    num_uva = 0
    for uva_file in uva_list:
        num_uva = num_uva + 1
        id = uva_file.rfind("/")
        band = uva_file[id+12:id+13]
        sou_name = uva_file[id+1:id+11]
        if ( verb > 0 ):
             print ( "imadir.py Processes source %s band %s  %5d ( %5d )" % \
                      ( sou_name, band, num_uva, len(uva_list) ) )

        if ( not pict_only ):
#
# ---------- Running auto-image
#
             com = "automap.py -a %s -v %d %s" % (auto_script, verb, uva_file)
             if ( verb > 2 ):
                  print ( "imadir.py automap command: ", com )
             (ret, out) = exe_noout_nolog ( com, verb )
             if ( ret != 0 ):
                  if ( verb > 1 ):             
                       print ( "imadir.py ERROR: ", "\n".join(out) )
                  if ( verb > 0 ):             
                       print ( "imadir.py Error in imaging source %s band %s" %\
                               (sou_name, band) )
                       continue

        map_fits_file = uva_file.replace("uva.fits","map.fits").replace("uvm.fits","map.fits")
        if ( not os.path.isfile ( map_fits_file ) ):
             if ( verb > 2 ):
                  print ( "imadir.py: failed to find image for source %s band %s" %\
                          (sou_name, band) )
             continue

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
             print ( "imadir.py fits_to_map command: ", com )
        (ret, out) = exe_noout_nolog ( com, verb )
        if ( ret != 0 ):
             if ( verb > 1 ):             
                  print ( "imadir.py ERROR: ", "\n".join(out) )

        uvs_fits_file = map_fits_file.replace("map.fits","uvs.fits")
        uvs_gif_file  = map_fits_file.replace("map.fits","uvs.gif")
#
# ----- Generate radplot
#
        com = "fits_to_radplot -o " + uvs_gif_file + " " + uvs_fits_file
        if ( verb > 2 ):
             print ( "imadir.py fits_to_radplot command: ", com )
        (ret, out) = exe_noout_nolog ( com, verb )
        if ( ret != 0 ):
             if ( verb > 1 ):             
                  print ( "imadir.py ERROR: ", "\n".join(out) )
        num_ima = num_ima + 1

    return num_ima

#
# ------------------------------------------------------------------------
#
def main():
    """ 
    Parsing arguments
    """
    parser = argparse.ArgumentParser( description=imadir__label )
    parser.add_argument('--version', action='version', version=imadir__version )

#
# --- General options:
#
    parser.add_argument ( "-v", "--verbosity", action="store", \
                          dest="verb", \
                          default=1, \
                          metavar="value", \
                          type=int, \
                          help="Verbosity level" )

    parser.add_argument ( "-a", "--automap_script", action="store", \
                          dest="auto_script", \
                          default="pima_mupet_01.dfm", \
                          metavar="value", \
                          help="Verbosity level" )

    parser.add_argument ( "-pict", "--picture_only", \
                          action="store_true", \
                          dest="pict", \
                          default=None, \
                          help="Generate only picture, do not make images" )
#
# --- Define positional arguments
#
    parser.add_argument ( "dir", \
                          help="FITS file with calibrated visibilities" )
#
# --- Get and parse options
#
    args = parser.parse_args()

    num_ima = imadir_exe ( args.dir, args.auto_script, args.pict, args.verb )

    if ( args.verb > 0 ):
         print ( "imadir.py: generated %d images in directory %s " % \
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
