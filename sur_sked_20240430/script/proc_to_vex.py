#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   Routine for encoding VLBI field system procedure into the          *
# *   extension block of a VLBI schedule in vex format.                  *
# *                                                                      *
# * ### 12-DEC-2021 vex_to_proc.py v 1.0 (c)  L. Petrov 12-DEC-2021 ###  *
# *                                                                      *
# ************************************************************************
"""
import argparse, signal, sys, os, pwd, math
from   datetime import datetime, timedelta, tzinfo
from   sur_sked_config import * # Import sur_sked confuguration
from   pet_misc        import *

ptv__label   = "proc_to_vex.py"
ptv__version = "2021.12.12"

#
# ------------------------------------------------------------------------
#
def main():
#
# --- Parsing vex_to_snap arguments 
#
    parser = argparse.ArgumentParser( description=ptv__label )
    parser.add_argument('--version', action='version', version=ptv__version )

#
# -- Define positional arguments
#
    parser.add_argument ( "proc_file", \
                          help="Input procedure file" )

    parser.add_argument ( "proc_name", \
                          help="2-letter long station name" )

    parser.add_argument ( "sta_name", \
                          help="2-letter long station name" )

    parser.add_argument ( "vex_file", \
                          help="schedule file in VEX format" )

#
# --- Get and parse options
#
    args = parser.parse_args()

    sta_name = args.sta_name.lower()

    if ( not os.path.isfile ( args.proc_file ) ):
         print ( "proc_to_vex.py: Cannot find file proc_file ", args.proc_file )
         exit  ( 1 )

#
# --- Reading procedure file
#

    with open(args.proc_file,encoding="latin") as f:
         buf = f.read().splitlines()
    f.close()

    out=[]
    out.append ( "  def FS_PROC_%s_%s ;" % ( args.proc_name, args.sta_name ) )
    nl = 0
    for line in buf:
        nl = nl + 1
        out.append ( "      extension NASA : fs_procedure_%s_%03d : %s : '%s' : ;" % \
                     ( args.proc_name, nl, args.sta_name, line ) )
    out.append ( "  endif;" )

    g=open(args.vex_file,"w")
    for line in out:
        print ( line, file=g )
    g.close()


if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        main()
    except KeyboardInterrupt:
        print ( "%s Interrupted" % argv[0] )
        exit  ( 1 )
