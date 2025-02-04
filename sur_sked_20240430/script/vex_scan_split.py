#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   PRogra vex_scan_split splits ...
# *                                                                      *
# *  ### 19-FEB-2024               v1.0 (c)  L. Petrov  19-FEB-2024 ###  *
# *                                                                      *
# ************************************************************************
import argparse, signal, sys, os, pwd, math
from   datetime import datetime, timedelta, tzinfo
from   sur_sked_config import * # Import sur_sked confuguration
from   pet_misc        import *

vss__label   = "gen_sched.py 20240219"

def vex_scan_split ( filin, filout, mode_list, gap, ivrb ):
    """
    """

    buf = read_file ( filin )

    num = len(mode_list)
    if ( ivrb > 1 ):
         print ( "vex_scan_split.py -- the number of modes: %d" % num )

    out = []
    scan_num = 0
    fl_scan = False
    for i in range(0,len(buf)):
        line = buf[i]
        if ( line[0:5] == "scan " ):
             scan_name = line.split()[1].replace(";","")
             fl_scan = True
             k = 0
             for j in range(i+1,len(buf)):
                 if ( buf[j][0:8] == 'endscan;' ):
                      if ( k == 0 ): 
                           k = j
                      continue
             scan_start = None
             scan_stop  = None
             for j in range(i,k+1):
                 if ( '=' in buf[j] ):
                      if ( buf[j].split('=')[0].replace(" ","") == "start" ):
                           scan_start = datetime.datetime.strptime ( buf[j].split('=')[1].replace(";",""), '%Yy%jd%Hh%Mm%Ss' )
                      if ( buf[j].split('=')[0].replace("*","").replace(" ","") == "stop" ):
                           scan_stop = datetime.datetime.strptime ( buf[j].split('=')[1].replace(";",""), '%Yy%jd%Hh%Mm%Ss' )

             if ( not scan_start or not scan_stop ):
                  print ( "ERROR vex_scan_split: did not find start/stop dates for scan %s" % scan_name )
                  exit  ( 1 )

             dur_scan    = (scan_stop - scan_start).total_seconds()
             dur_subscan = (dur_scan - (len(mode_list)-1)*gap)/len(mode_list)

             for m in range(1,num+1):
                 for j in range(i,k+1):
                     if ( buf[j][0:5] == "scan " ):
                          scan_num = scan_num + 1
                          out.append ( 'scan No%04d;' % scan_num )
                     elif ( buf[j][0:8] == "endscan;" ):
                          out.append ( "endscan;"  )
                     if ( '=' in buf[j] ):
                          if ( buf[j].split('=')[0].replace(" ","") == "mode" ):
                               old_mode = buf[j].split('=')[1].replace(";","")
                               out.append ( buf[j].replace(old_mode,mode_list[m-1]) )
                          elif ( buf[j].split('=')[0].replace(" ","") == "start" ):
                               scan_this_start = scan_start + datetime.timedelta( seconds=(m-1)*(dur_subscan + gap) )
                               old_scan_time = buf[j].split('=')[1].replace(";","")
                               out.append ( buf[j].replace(old_scan_time, datetime.datetime.strftime ( scan_this_start, '%Yy%jd%Hh%Mm%Ss' ) ) )
                          elif ( buf[j].split('=')[0].replace("*","").replace(" ","") == "stop" ):
                               scan_this_stop = scan_this_start + datetime.timedelta( seconds=dur_subscan )
                               old_scan_stop = buf[j].split('=')[1].replace(";","") 
                               out.append ( buf[j].replace(old_scan_stop, datetime.datetime.strftime ( scan_this_stop, '%Yy%jd%Hh%Mm%Ss' ) ) )
                          elif ( buf[j].replace("=","").split()[0] == "station" ):
                               old_dur_str = " " + buf[j].replace("=","").split()[5] + " sec:"
                               lin  = buf[j].replace(old_dur_str," %3d sec:" % dur_subscan )
                               if ( m > 1 ):
                                    old_pre_str = " " + buf[j].replace("=","").split()[3] + " sec:"
                                    lin = lin.replace(old_pre_str," 0 sec:" )
                               out.append ( lin )

                          else:
                               out.append ( buf[j] )
                 if ( m == num ):
                      out.append ( '*' )
                      out.append ( '*** split original scan %s into %s subscans (see above)' % \
                                   ( scan_name, len(mode_list) ) )
                      out.append ( '*' )
                 else:
                      out.append ( '*' )

        if ( not fl_scan ):
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

    parser = argparse.ArgumentParser( description=vss__label )

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
