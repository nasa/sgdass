#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Routine validate_vgosda validates a file with results of processed *
# *   VLBI data analysis in VGOSDA format.                               *
# *                                                                      *
# * ## 27-JUL-2019  vgosda_validate.py v1.0 (c) L. Petrov 27-JUL-2019 ## *
# *                                                                      *
# ************************************************************************
import sys, os, signal
from   vsdc_check_vgosda import *

#
# ------------------------------------------------------------------------
#
def main():

    if ( len(sys.argv) < 2 ):
         print ( "Usage: vgosda_validate.py file_name" )
         exit  ( 1 )
    file_name = sys.argv[1]
    ret = vsdc_check_vgosda ( file_name )
    if ( ret == None ):
         exit ( 1 )
#
# --- Pass the test
#
    exit ( 0 )

if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
        main()
    except KeyboardInterrupt:
        print ( "pf.py: Interrupted" )
        exit ( 1 )
