#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Routine session_prc.py checks whether the filename and the         *
# *   contents of a schedule file in VEX format is valid.               *
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
         print ( "Usage: date_session_vex.py file_name" )
         exit  ( 1 )
    file_name = sys.argv[1]

#
# --- Read the vex file in question
#
    buf = read_file ( file_name )
    if ( not buf ):
         print ( "Cannot read vex file ", file_name )
         exit  ( 1 )

#
# --- Extract the basename
#
    id = file_name.rfind("/")
    fil = file_name[id+1:]
#
# --- Check for the minimum length of the basename. It should not be too short
#
    if ( len(fil) < 5 ):
         print ( "Filename is too short" )
         exit  ( 1 )

#
# --- Check suffix
#
    if ( fil[-4:] != ".vex" ):
         print ( "Filename does not have suffix .vex" )
         exit  ( 1 )
         
#
# --- Extract experiment name from the filename 
#
    exp_name_in_filename = fil[:-4]

#
# --- Scan the vex file and search for mandatory keywords
# --- and extract experiment name and station name
#
    exp_name = "??"
    ind_line = 0
    if ( len(buf[0]) < 7 ):
         print ( "Malformed vex file -- the first line is too short" )
         exit  ( 1 )

    if ( buf[0][0:7] != "VEX_rev" ):
         print ( "Malformed vex file -- the first line does start with magiv VEX_rev" )
         exit  ( 1 )

    for line in buf:
        ind_line = ind_line + 1
        if ( len(line) == 0 ): continue
        liner = line.replace("=","" )
        if ( len(liner.split()) >= 3 ):
             if ( liner.split()[0].lstrip() == "ref" and \
                  liner.split()[1].lstrip() == "$EXPER"  ):
                 exp_name = liner.split()[2].lstrip().replace(";","" )
#
# --- Check wheather we have found a line with scan_name= there
#
    if ( exp_name == "??" ):
         print ( "Malformed vex file -- no line ref $EXPER was found" )
         exit  ( 1 )
         
#
# --- Check the experiment name inside the filename against the name in scan_name= command
#
    if ( exp_name != exp_name_in_filename ):
         print ( "Mismatch between the experiment name in the filename, %s and in the file: %s" % \
                 ( exp_name, exp_name_in_filename ) )
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
