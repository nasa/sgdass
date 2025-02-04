#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   Routine for extraction of procedure file for a given station       *
# *   from a VLBI schedule in vex 2.0 extended format.                   *
# *                                                                      *
# * ### 09-DEC-2021 vex_to_proc.py v 1.3 (c)  L. Petrov 09-DEC-2021 ###  *
# *                                                                      *
# ************************************************************************
"""
import argparse, signal, sys, os, pwd, math
from   datetime import datetime, timedelta, tzinfo
from   sur_sked_config import * # Import sur_sked confuguration
from   pet_misc        import *
import ners

vtp__label   = "vex_to_proc.py"
vtp__version = "2021.12.09"

#
# ------------------------------------------------------------------------
#
def main():
#
# --- Parsing vex_to_smap arguments 
#
    parser = argparse.ArgumentParser( description=vtp__label )
    parser.add_argument('--version', action='version', version=vtp__version )

#
# --- General options:
#
    parser.add_argument ( "-v", "--verbosity", \
                          action="store", \
                          dest="verb", \
                          default=0, \
                          metavar="value", \
                          type=int, \
                          help="Verbosity level" )

    parser.add_argument ( "-o", "--output_file", \
                          action="store", \
                          default="-", \
                          dest="output_file", \
                          help="Output file with a procedure file" )

#
# -- Define positional arguments
#
    parser.add_argument ( "vex_file", \
                          help="Input schedule file in VEX format" )

    parser.add_argument ( "sta_name", \
                          help="2-letter long station name" )

#
# --- Get and parse options
#
    args = parser.parse_args()

    if ( not args.vex_file ):
         print ( "Vex file argumnet should be provided" )
         exit ( 1 )

    if ( not os.path.isfile ( args.vex_file ) ):
         print ( "Cannot find vex file %s" % args.vex_file )
         exit ( 1 )

    if ( not args.sta_name ):
         print ( "Station name argumnet should be provided" )
         exit ( 1 )

    if ( not os.path.isfile ( args.vex_file ) ):
         print ( "Cannot find vex file %s" % args.vex_file )
         exit ( 1 )

    if ( not args.output_file ):
         print ( "Output file should be provided" )
         exit ( 1 )

    with open(args.vex_file,encoding="latin") as m:
         buf = m.read().splitlines()
    m.close()

    pref = "*proc_file_" + args.sta_name.lower()

    fl_prc = False
    out = []
    for line in buf:
        if ( "      end_literal(proc" in line and args.sta_name in line ):
             fl_prc = False
        if ( fl_prc ):
             if ( len(line) > 8 ):
                  out.append ( line[8:] )
             else:
                  out.append ( "" )

        if ( "      start_literal(proc" in line and args.sta_name in line ):
             fl_prc = True
                

    if ( len(out) == 0 ):
         print ( "No procedure for station %s was found in vex file %s" % \
                 ( args.sta_name, args.vex_file ) )
         exit ( 1 )

    f=open(args.output_file,"w")
    for line in out:
        print ( line, file=f )
    f.close()

if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        main()
    except KeyboardInterrupt:
        print ( "pf.py: Interrupted" )
        exit  ( 1 )
