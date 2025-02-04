#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Routine validate_plain_ascii.py validates a file in plain_ascii    *
# *   format. The optional second argument defines a strting the magic.  *
# *   validate_plain_ascii.py checks whether the file starts with magic. *
# *   validate_plain_ascii.py returns nothing if the file passed the     *
# *   test and an error message if it did not.                           *
# *                                                                      *
# * # 16-JUN-2019 validate_plain_ascii.py v1.0 (c) L. Petrov 16-JUN-2019 # *
# *                                                                      *
# ************************************************************************
import sys, os, signal
from   vsdc_check_plain_ascii import *

#
# ------------------------------------------------------------------------
#
def main():

    if ( len(sys.argv) < 3 ):
         print ( "Usage: validate_plain_ascii.py file_name [magic_str]" )
         exit  ( 1 )
    file_name = sys.argv[1]
    magic_str = sys.argv[2]
    vsdc_check_plain_ascii ( file_name, magic_str)

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
