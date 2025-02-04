#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Routine date_last_mod.py returns a strings with two UTC dates.     *
# *   The first date is the last modification date and the second        *
# *   date is one second after that.                                     *
# *                                                                      *
# * ### 16-JUN-2019 date_last_mod.py v1.0 (c) L. Petrov 19-JUL-2019 ###  *
# *                                                                      *
# ************************************************************************
import sys, os, signal, time, datetime

#
# ------------------------------------------------------------------------
#
def main():

    if ( len(sys.argv) < 2 ):
         print ( "Usage: date_last_mod.py file_name" )
         exit  ( 1 )
    file_name = sys.argv[1]

#
# --- Does the file exist?
#
    if ( not os.path.isfile ( file_name ) ):
         print ( "File %s does not exists" % file_name )
         exit ( 1 )

#
# --- Get last file modification time tag in UTC
#
    date_last_mod = datetime.datetime.utcfromtimestamp( os.path.getmtime ( file_name ) )
#
# --- ...and convert it to pytnon date format
#
    date_start = date_last_mod
#
# --- Set the stop time as one second after
#
    date_stop  = date_start    + datetime.timedelta ( seconds=1 )
    print ( date_start.strftime("%Y.%m.%d_%H:%M:%S"), date_stop.strftime("%Y.%m.%d_%H:%M:%S") )

if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
        main()
    except KeyboardInterrupt:
        print ( "pf.py: Interrupted" )
        exit ( 1 )
