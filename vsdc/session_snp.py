#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Routine session_snp.py checks whether the filename and the         *
# *   contents of a schedule file in SNAP format is valid.               *
# *                                                                      *
# * ## 17-DEC-2021   session_snp.py  v1.0 (c) L. Petrov 18-DEC-2021 ###  *
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
         print ( "Usage: date_session_sn.py file_name" )
         exit  ( 1 )
    file_name = sys.argv[1]

#
# --- Read the snap file in question
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
# --- Check suffix
#
    if ( fil[-4:] != ".snp" ):
         print ( "Filename does not have suffix .snp" )
         exit  ( 1 )
         
#
# --- Extract experiment name and station name from the filename
#
    exp_name_in_filename = fil[:-6]
    sta_name_in_filename = fil[-6:-4]

#
# --- Scan the snap file, search for mandatory lines that start from scan_name=,
# --- and extract experiment name and station name
#
    sta_name = "??"
    exp_name = "??"
    ind_line = 0
    for line in buf:
        ind_line = ind_line + 1
        if ( len(line) < 11   ): continue
        if ( line[0:1] == '"' ): continue
        if ( line[0:10] == "scan_name=" ):
             if ( len(line.split(",")) < 5 ):
                  print ( "Malformed line %s -- it should have four commas" % ind_line )
                  print ( 1 )
             exp_name = line.split(",")[1]
             sta_name = line.split(",")[2]
#
# --- Check wheather we have found a line with scan_name= there
#
    if ( exp_name == "??" ):
         print ( "Malformed snap file -- no lines scan_name= were found" )
         exit  ( 1 )
         
#
# --- Check the experiment name inside the filename against the name in scan_name= command
#
    if ( exp_name != exp_name_in_filename ):
         print ( "Mismatch between the experiment name in the filename, %s and in the file: %s" % \
                 ( exp_name, exp_name_in_filename ) )
         exit  ( 1 )

#
# --- Check the station name inside the filename against the name in scan_name= command
#
    if ( sta_name != sta_name_in_filename ):
         print ( "Mismatch between the station name in the filename, %s and in the file: %s" % \
                 ( sta_name, sta_name_in_filename ) )
         exit  ( 1 )

#
# --- All check are passed
#
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
