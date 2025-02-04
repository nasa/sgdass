#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Routine validate_difx.py validates a tar-file with difx output.    *
# *                                                                      *
# * ### 16-JUN-2019 validate_difx.py  v1.0 (c) L. Petrov 16-JUN-2019 ###  *
# *                                                                      *
# ************************************************************************
import sys, os, signal
from   vsdc_check_difx        import *

#
# ------------------------------------------------------------------------
#
def main():

    if ( len(sys.argv) < 2 ):
         print ( "Usage: validate_difx.py file_name" )
         exit  ( 1 )
    file_name = sys.argv[1]
    res = vsdc_check_difx ( file_name )
    if ( res == 0 ):
#
# ------ Passed the test
#
         exit ( 0 )
    else:
#
# ------ Failed the test
#
         exit ( 1 )

if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
        main()
    except KeyboardInterrupt:
        print ( "pf.py: Interrupted" )
        exit ( 1 )
