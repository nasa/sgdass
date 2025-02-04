#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   Program for computing the series of mass loading.                  *
# *                                                                      *
# * ###  18-MAY-2017  grid_to_eph.py  v1.1 (c) L. Petrov 01-JUN-2017 ### *
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

grid_to_eph__label = "grid_to_eph Version of 2017.06.01"

MALO_BIN = "/opt64/bin"

#
# ------------------------------------------------------------------------
#
def main():

    if ( len(sys.argv) <= 3 ):
         print ( "Usage: grid_to_eph.py filgrid filspl fileph" )
         exit ( 1 )

    filgrid = sys.argv[1]
    filspl  = sys.argv[2]
    fileph  = sys.argv[3]

    os.environ["OMP_NUM_THREADS"] = "1"

# loading_nc_to_spl_heb nc-file heb_file
# loading_spl_heb_to_sta splheb_file [splheb_file2] station_file eph_file [ivrb]

    malo_bin_dir    = os.popen(MALO_BIN + "/malo_inq bin_static").read().rstrip()
    malo_share_dir  = os.popen(MALO_BIN + "/malo_inq share"     ).read().rstrip()

    com = malo_bin_dir + "/loading_nc_to_spl_heb" + " " + \
          filgrid + " " + \
          filspl  + " " + \
          "lbzip2_2p1"

    (ret, out) = exe ( com, 4 )
    if ( ret != 0 ):
         print ( "Error in com: ", com )
         print (  "\n".join(out) )
         exit  ( 1 )

    com = malo_bin_dir + "/loading_spl_heb_to_sta" + " " + \
          filspl  + " " + \
          malo_share_dir + "/loading.sta" + " " + \
          fileph + " " \
          "1"

    (ret, out) = exe ( com, 4 )
    if ( ret != 0 ):
         print ( "Error in com: ", com )
         print (  "\n".join(out) )
         exit  ( 1 )

    exit ( 0 )

if __name__ == "__main__":
    vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    signal.signal ( signal.SIGINT,  malo_signal_handler )
    signal.signal ( signal.SIGTERM, malo_signal_handler )
    signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
    main()
