#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Routine session_prc.py checks whether the filename and the         *
# *   contents of a schedule file in PROC format is valid.               *
# *                                                                      *
# * ## 17-DEC-2021   session_prc.py  v1.0 (c) L. Petrov 18-DEC-2021 ###  *
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
         print ( "Usage: date_session_prc.py file_name" )
         exit  ( 1 )
    file_name = sys.argv[1]

#
# --- Read the proc file in question
#
    buf = read_file ( file_name )
    if ( not buf ):
         print ( "Cannot read snap file ", file_name )
         exit  ( 1 )

#
# --- Extract the basename
#
    id = file_name.rfind("/")
    fil = file_name[id+1:]
#
# --- Check for the minimum length of the basename. It should not be too short
#
    if ( len(fil) < 6):
         print ( "Filename is too short" )
         exit  ( 1 )

#
# --- Check the suffix
#
    if ( fil[-4:] != ".prc" ):
         print ( "Filename does not have suffix .prc" )
         exit  ( 1 )

    proc_name = "??"
    ind_line = 0
    for line in buf:
        ind_line = ind_line + 1
        if ( len(line) < 11   ): continue
        if ( len(line.split()) > 1 ):
             if ( line.split()[0] == "define" ):
                  proc_name = line.split()[1]

    if ( proc_name == "??" ):
         print ( "Malformed proc file -- no procedure definitions were found there" )
         exit  ( 1 )

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
