#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Routine date_fs_log_mod.py returns a strings with two UTC dates.   *
# *   The first date is the last modification date and the second        *
# *   date is one second after that.                                     *
# *                                                                      *
# * ## 16-JUN-2019 date_fs_log_mod.py v1.0 (c) L. Petrov 16-JUN-2019 ### *
# *                                                                      *
# ************************************************************************
import sys, os, signal, time, datetime
from   vsdc_misc        import  *
from   vsdc_date_fslog  import  *

#
# ------------------------------------------------------------------------
#
def main():

    if ( len(sys.argv) < 1 ):
         print ( "Usage: date_fs_log_mod.py file_name" )
         exit  ( 1 )
    file_name = sys.argv[1]

    ( date_start, date_stop ) = vsdc_date_fslog ( file_name )
    if ( date_start == None ):
         exit  ( 1 )
    else:  
         print ( date_start.strftime("%Y.%m.%d_%H:%M:%S"), date_stop.strftime("%Y.%m.%d_%H:%M:%S") )
         exit  ( 0 )

if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
        main()
    except KeyboardInterrupt:
        print ( "pf.py: Interrupted" )
        exit ( 1 )
