#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Routine date_correp.py returns a strings with two UTC dates:       *
# *   start time of the VLBI experiment reported in the correlation      *
# *   report and the stop time.
# *                                                                      *
# * ### 27-AUG-2019  date_correp.py  v1.0 (c) L. Petrov 27-AUG-2019 ###  *
# *                                                                      *
# ************************************************************************
import sys, os, signal
from   vsdc_misc          import *
from   vsdc_date_correp   import *

#
# ------------------------------------------------------------------------
#
def main():

    if ( len(sys.argv) < 2 ):
         print ( "Usage: date_corep.py file_name" )
         exit  ( 1 )
    file_name = sys.argv[1]

#
# --- Does the file exist?
#
    if ( not os.path.isfile ( file_name ) ):
         print ( "File %s does not exists" % file_name )
         exit ( 1 )

    (date_first, date_last) = vsdc_date_correp ( file_name )
    if ( date_first == None ):
         exit ( 1 )
    print ( date_first.strftime("%Y.%m.%d_%H:%M:%S"), date_last.strftime("%Y.%m.%d_%H:%M:%S") )
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
