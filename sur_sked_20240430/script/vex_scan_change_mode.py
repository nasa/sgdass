#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program vex_scan_change_mode splits ...
# *                                                                      *
# * ## 07-OCT-2024 vex_scan_change_mode v1.0 (c) L. Petrov 19-FEB-2024 # *
# *                                                                      *
# ************************************************************************
import argparse, signal, sys, os, pwd, math
from   datetime import datetime, timedelta, tzinfo
from   sur_sked_config import * # Import sur_sked confuguration
from   pet_misc        import *

vscm__label   = "vex_scan_change_mode 20241007"

def vex_scan_split ( filin, filout, mode_list, gap, ivrb ):
    """
    """

    buf = read_file ( filin )

    num = len(mode_list)
    if ( ivrb > 1 ):
         print ( "vex_scan_split.py -- the number of modes: %d" % num )

    out = []
    scan_num = 0
    ind_mode = 0
    for i in range(0,len(buf)):
        line = buf[i]
        if ( len(line.split())  > 0 ):
             if ( "mode=" in line.split()[0] and buf[i-1][0:5] == "scan " ):
                  scan_name = buf[i-1].split()[1].replace(";","")
                  if ( scan_num > 0 and scan_num%gap == 0 ):
                       ind_mode = ind_mode + 1
                  if ( ind_mode >= len(mode_list) ):
                            ind_mode = 0
                  ip = line.find("=")
                  line = line[0:ip] + "=" + mode_list[ind_mode] + ";"
                  scan_num = scan_num + 1

        out.append ( line )
    (ret,err) = write_file ( out, filout )

    return ( 0 )

#
# ------------------------------------------------------------------------
#
def main():
    """ 
    Main program of the gen_vex_tmpl utility
    """

    parser = argparse.ArgumentParser( description=vscm__label )

    parser.add_argument ( "-i", "--input", \
                          action="store", \
                          dest="input", \
                          metavar="value", \
                          help="Input VLBI schedule in vex format" )

    parser.add_argument ( "-o", "--output", \
                          action="store", \
                          dest="output", \
                          metavar="value", \
                          help="Output VLBI schedule in vex format" )

    parser.add_argument ( "-m", "--mode_list", \
                          action="store", \
                          dest="mode_string", \
                          metavar="value", \
                          help="List of modes that will be used" )

    parser.add_argument ( "-g", "--gap", \
                          action="store", \
                          type=int, \
                          dest="gap", \
                          metavar="value", \
                          help="Gap between scans" )

    parser.add_argument ( "-v", "--verbosity", \
                          action="store", \
                          dest="verb", \
                          default=0, \
                          metavar="value", \
                          type=int, \
                          help="Verbosity level" )

#
# --- Get and parse options
#
    args = parser.parse_args()

    if ( not args.input ):
         print ( "Input schedule file name is not specified" )
         exit ( 1 )

    if ( not args.output ):
         print ( "Output schedule file name is not specified" )
         exit ( 1 )

    if ( not args.mode_string ):
         print ( "The list of new modes is not specified" )
         exit ( 1 )

    if ( ":" in args.mode_string ):
         mode_list = args.mode_string.split(":")[1].split(",")
    else:
         mode_list = args.mode_string.split(",")

    if ( not args.gap ):
         print ( "Gap between subscans is not specified" )
         exit ( 1 )
   
    vex_scan_split ( args.input, args.output, mode_list, args.gap, args.verb )

    if ( args.verb > 0 ):
         print ( "vex_scan_split: Output schedule file %s is ready" % args.output )

if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        main()
    except KeyboardInterrupt:
        print ( "pf.py: Interrupted" )
        exit ( 1 )
