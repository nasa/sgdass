#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   Program for gemnerating automatic VLBI images from calibrated      *
# *   visibilities using Difmap.                                         *
# *                                                                      *
# * ### 15-FEB-2016   automap.py   v 1.5  (c) L. Petrov  13-JAN-2019 ### *
# *                                                                      *
# ************************************************************************
"""
import pwd, sys, os, re, shutil, time, subprocess, datetime, operator, signal
sys.path.append("/usr")
import argparse 
import pima_local
from pima_exe import exe, exe_out_log, exe_out_nolog, exe_noout_log, exe_noout_nolog, \
                     pima_print_mes, pima_signal_handler_term

automap__label   = "automap.py"
automap__version = "automap.py v 1.5  2019.01.13"

difmap_exec = pima_local.difmap_exec
scr_dir     = "/tmp"

#
# ------------------------------------------------------------------------
#
def automap_exe ( uva_file, auto_script, verb ):

    if ( auto_script.find("/") > -1 ):
         mupet = auto_script
    else:
#
# ------ Add default path
#
         mupet = pima_local.pima_path + "/bin/" + auto_script

    if ( not os.path.isfile ( mupet ) ):
         print ( "automap.py: cannot find automapping script " + mupet )
         exit  ( 1 )

    if ( uva_file.rfind(".") < 1 ):
         print ( "automap.py: the input file " + uva_file + \
                 " does not have extension" )
         exit  ( 1 )
    uva_trunk = uva_file[0:uva_file.rfind(".")]
    uva_body  = uva_file[uva_file.rfind("/")+1:uva_file.rfind(".")]
    sou_nam  = uva_file[uva_file.rfind("/")+1:uva_file.rfind("/")+11]
    band     = uva_file[uva_file.rfind("/")+12:uva_file.rfind("/")+13]
    if ( len(uva_body) > 16 ):
         uva_suffix = uva_file[uva_file.rfind("/")+13:uva_file.rfind("/")+15]
    else:
         uva_suffix = ""
    scr_file = scr_dir + "/" + sou_nam + "_" + band + ".difmap"
    uv_log   = uva_file.replace("uva.fits","aut.log").replace("uvm.fits","aut.log")
    dfm_log  = uva_file.replace("uva.fits","dfm.log").replace("uvm.fits","dfm.log")
    map_fits = uva_file.replace("uva.fits","map.fits").replace("uvm.fits","map.fits")
    mod_file = uva_file.replace("uva.fits","map.mod").replace("uvm.fits","map.mod")
    obs_fits = uva_file.replace("uva.fits","uvs.fits").replace("uvm.fits","uvs.fits")
    win_file = uva_file.replace("uva.fits","map.win").replace("uvm.fits","map.win")

    if ( os.path.isfile ( uv_log  ) ): os.remove ( uv_log  )
    if ( os.path.isfile ( dfm_log ) ): os.remove ( dfm_log )
    uva_dir = uva_file[0:uva_file.rfind("/")]
    log = open ( uv_log, "w" )
#
    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
    print ( " ".join(sys.argv), file = log ) 
    pima_print_mes ( "Started  imaging source " + sou_nam + \
                     " at " + band + " band by automap PIMA on " + date_str, log )
    pima_print_mes ( "=====================================================" + \
                     "==================================", log )
    print ( " ", file=log )
    log.flush()

#
# --- Parse FITS -file
#
    pima_com = pima_local.fitsh + " " + uva_file
    (ret, uv_dump) = exe_noout_log ( pima_com, verb, log )
    if ( ret != 0 ):
         pima_print_mes ( "Failure in getting dump of visibility file " + \
                           uva_file, log )
         pima_print_mes ( "Failed command: " + pima_com, log )
         exit ( 1 )

    ref_freq = 0.0
    for line in uv_dump:
        if ( line.find("FREQ    =") > 0 ):
             ref_freq = float(line.split()[6].replace("'","").replace("D","E"))
    
    pima_print_mes ( "Reference frequency is %10.2f MHz" % (ref_freq/1.e6), log )

    if ( ref_freq < 2.5e9 ):
#
# ------ L and S bands
#
         automap_fs = 1024
         automap_fc = 0.5
         automap_ts = 20.
    elif ( ref_freq < 5.5e9 ):
         automap_fs = 1024
         automap_fc = 0.3
         automap_ts = 40.
    elif ( ref_freq < 9.5e9 ):
         automap_fs = 1024
         automap_fc = 0.2
         automap_ts = 70.
    elif ( ref_freq < 17.0e9 ):
         automap_fs = 1024
         automap_fc = 0.1
         automap_ts = 120.
    elif ( ref_freq < 25.0e9 ):
         automap_fs = 1024
         automap_fc = 0.05
         automap_ts = 200.
    elif ( ref_freq < 45.0e9 ):
         automap_fs = 1024
         automap_fc = 0.025
         automap_ts = 400.
    elif ( ref_freq < 90.0e9 ):
         automap_fs = 1024
         automap_fc = 0.01
         automap_ts = 800.
         
    com = open ( scr_file, "w" )
    print ( "logfile " + dfm_log , file=com )
    print ( "float field_size; field_size = %5d"    % automap_fs, file=com )
    print ( "float field_cell; field_cell = %8.4f"  % automap_fc, file=com )
    print ( "float taper_size; taper_size = %6.1f"  % automap_ts, file=com )
    print ( "float freq;       freq       = %10.5f" % (ref_freq/1.e9), file=com )
    print ( "obs     " + uva_file, file=com )
    print ( "select rr, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16 ;", file=com )
    print ( 'print "field_size =", field_size', file=com )
    print ( 'print "field_cell =", field_cell', file=com )
    print ( 'print "taper_size =", taper_size', file=com )
    print ( "@" + mupet  + " " + sou_nam + "_" + band + uva_suffix, file=com )
    print ( "wmap   " + map_fits, file=com )
    print ( "wmodel " + mod_file, file=com )
    print ( "wobs   " + obs_fits, file=com )
    print ( "wwins  " + win_file, file=com )
    print ( "float peak_flux; peak_flux = peak(flux,max)", file=com )
    print ( 'print "The peak flux is", peak_flux', file=com )
    print ( "invert", file=com )
    print ( "float image_rms; image_rms = imstat(rms)", file=com )
    print ( 'print "The final image rms is",image_rms', file=com )
    print ( 'loglevs 100*3*image_rms/peak_flux'       , file=com )
    print ( 'print "clev=      ", int(3*image_rms*10000)/10000'      , file=com )
    print ( 'print "peak=      ", int(1000*peak_flux)/1000'          , file=com )
    print ( 'print "dyn_range= ", int(1000*peak_flux/image_rms)/1000', file=com )
    print ( "quit", file=com )
    com.close()
#
    com = open ( scr_file, "r" )
    with open(scr_file) as f:
         com_buf = f.readlines()
    com.close()
    print ( "Beginning of the difmap commnad file:", file=log )
    for line in com_buf:
        print ( line.strip("\n"), file=log )
    print ( "End of the difmap commnad file:", file=log )

    difmap_com = "cd " + uva_dir + "; " + difmap_exec + " < " + scr_file + " > /dev/null 2>&1"
    (ret, out) = exe_noout_log ( difmap_com, verb, log )
    if ( ret != 0 ):
         if ( os.path.isfile ( "difmap.log" ) ): os.remove ( "difmap.log" )
         os.remove ( scr_file )
         pima_print_mes ( "Failure in running difmap for visibility file " + \
                           uva_file, log )
         print ( "difmap command: ", difmap_com )
         for line in out:
             print ( line )
         exit ( 1 )
    if ( os.path.isfile ( "difmap.log" ) ): os.remove ( "difmap.log" )
    os.remove ( scr_file )

#
# - Generate the final success message in the output log file
#
    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
    if ( verb > 0 ):
         pima_print_mes ( "Finished imaging source " + sou_nam + \
                         " at " + band + " band by automap PIMA on " + date_str, log )
    else:
         print ( "Finished imaging source " + sou_nam + \
                 " at " + band + " band by automap PIMA on " + date_str, file=log )

    print ( "=====================================================" + \
             "==================================", file=log )
    print ( " ", file=log )
    log.flush()

    sys.stdout.flush()
    exit ( 0 )

#
# ------------------------------------------------------------------------
#
def main():
    """ 
    Parsing arguments
    """
    parser = argparse.ArgumentParser( description=automap__label )
    parser.add_argument('--version', action='version', version=automap__version )

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

#
# -- Define positional arguments
#
    parser.add_argument ( "uva", \
                          help="FITS file with calibrated visibilities" )
#
# --- Get and parse options
#
    args = parser.parse_args()

    automap_exe ( args.uva, args.auto_script, args.verb )

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
