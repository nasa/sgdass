#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   Program for checking the specicif time series.                     *
# *                                                                      *
# * # 24-MAY-2017 imls_check_series.py v2.0 (c) L. Petrov 14-MAR-2020 ## *
# *                                                                      *
# ************************************************************************
"""
import sys, os, shutil, time, subprocess, datetime, signal
import optparse 
import math
from   Message           import *
from   malo_exe          import *

global malo_child_pid

TMP_DIR   = "/tmp"

check_exe     = os.environ["MALO_DIR"] + "/bin_static/check_listing_gaps"
imls_root_dir = "/imls"

#
# ------------------------------------------------------------------------
#
def main():

    if ( len(sys.argv) <= 4 ):
         print ( "Usage: imls_check_series.py dir typ mod step style report" )
         exit ( 1 )

    dir = sys.argv[1]
    typ = sys.argv[2]
    mod = sys.argv[3]
    stp = sys.argv[4]
    stl = sys.argv[5]
    rep = sys.argv[6]

    com = check_exe + " " + imls_root_dir + "/" + dir + "/" + typ + "/" + mod  + " " + stp
    (ret, out) = exe ( com, 1 )
    if ( ret == 2 ):
         for line in out:
             print ( line )
    
    num_fil   = int(out[0].split()[0])
    first_fil = out[0].split()[1]
    last_fil  = out[0].split()[2]
    
    first_date = first_fil.replace ( typ + "_" + mod + "_","").replace(".eph","").replace(".spl","").replace("aam","").replace(".nc","").replace(".bz2","").replace(".gz","").replace(".heb","")
    last_date  = last_fil.replace  ( typ + "_" + mod + "_","").replace(".eph","").replace(".spl","").replace("aam","").replace(".nc","").replace(".bz2","").replace(".gz","").replace(".heb","")

    if ( stl == "long" ):
         print ( "%-12s %3s %-8s  %-42s  %-42s  %6d" % ( dir, typ, mod, \
                 first_fil, last_fil, num_fil ) )
                 
    else:
         print ( "%-12s %3s %-8s  %s  %s  %6d" % ( dir, typ, mod, \
                 first_date, last_date, num_fil ) )

    if ( len(out) > 1 ):
         for i in range(1,len(out)):
             print ( "  ", out[i] )          
         print ( " " )
         ser_bad = imls_root_dir + "/" + dir + "/" + typ + "/" + mod 
    else:
         ser_bad = ""

    if ( os.path.isfile ( rep ) ):
         with open ( rep ) as f:
              rep_line = f.readline()
         f.close()
         num_dat = int(rep_line.split()[0])
         rem_str = ""
         if ( len(rep_line.split()) > 1 ):
              for i in range(1,len(rep_line.split())):
                  rem_str = rem_str + " " + rep_line.split()[i]
         rem_str = rem_str  + " " + ser_bad
         f = open ( rep, "w" )
         print ( "%8d %s" % ( num_dat + num_fil, rem_str ), file=f )
         f.close()
    else:
         f = open ( rep, "w" )
         print ( "%8d %s" % ( num_fil, ser_bad ), file=f )
         f.close()

if __name__ == "__main__":
    vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    signal.signal ( signal.SIGINT,  malo_signal_handler )
    signal.signal ( signal.SIGTERM, malo_signal_handler )
    signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
    main()
