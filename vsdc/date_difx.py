#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Routine date_difx.py returns a strings with two UTC dates          *
# *   extracted from a file with results of correlation of a VLBI        *
# *   experiment in DiFX format. The first date is the scan reference    *
# *   time of the first VLBI observstion, and the second date is the     *
# *   scan reference time of the last VLBI observation.                  *
# *                                                                      *
# * ###  28-JUL-2019  date_difx.py  v1.0 (c) L. Petrov 28-JUL-2019  ###  *
# *                                                                      *
# ************************************************************************
import sys, os, signal, tarfile
from   vsdc_misc        import *
from   vsdc_date_difx   import *

#
# ------------------------------------------------------------------------
#
def main():
    if ( len(sys.argv) < 2 ):
         print ( "Usage: date_difx.py file_name" )
         exit  ( 1 )
    file_name = sys.argv[1]

#
# --- Does the file exist?
#
    if ( not os.path.isfile ( file_name ) ):
         print ( "File %s does not exists" % file_name )
         exit ( 1 )

    (date_first, date_last) = vsdc_date_difx ( file_name )

    if ( date_first == None ):
         exit ( 1 )
    print ( date_first.strftime("%Y.%m.%d_%H:%M:%S"), date_last.strftime("%Y.%m.%d_%H:%M:%S") )
    exit ( 0 )

#
# =============================================================
#

if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
        main()
    except KeyboardInterrupt:
        print ( "pf.py: Interrupted" )
        exit ( 1 )
