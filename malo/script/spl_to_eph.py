#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   Program for computing the series of mass loading.                  *
# *                                                                      *
# * ###  18-MAY-2017  spl_to_eph.py  v1.2 (c)  L. Petrov 01-JUN-2017 ### *
# *                                                                      *
# ************************************************************************
"""
import sys, os, shutil, time, subprocess, datetime, signal
import optparse 
import math
import datetime
from   datetime import timedelta
from   Message           import *
from   malo_exe          import *
from   geos_oper_config  import *
from   malo_check_date   import *

global malo_child_pid

TMP_DIR   = "/tmp"

hostname = os.uname()[1]

spl_to_eph__label = "spl_to_eph  Version of 2017.06.01"

MALO_BIN = "/opt64/bin"
loading_exe = "loading_spl_heb_to_sta"

# /progs/malo_20170520/bin_static/loading_spl_heb_to_sta /imls/load_spl/atm/merra2 /imls/ondemand/req/20170518_182433/sta_fil.txt /imls/ondemand/req/20170518_182433/malo_atm_MERRA2_'*'.eph 1  >& /imls/ondemand/req/20170518_182433/log.txt
#
# $MALO_DIR/script/spl_to_eph.py -s /imls/load_spl/nto/omct05 -o /imls/load_list/nto/omct05 -f $MALO_DIR/share/loading.sta -p nto_omct05_ -b 1980.01.01_00:00:00 -e 1995.03.11 -v 2 -j 16
#

def time_interval ( tim_beg ):
    tim_now = datetime.datetime.now()

    tdelta = tim_now - tim_beg
    tot_seconds = tdelta.seconds + tdelta.microseconds/1000000.0
    hours   = int(tot_seconds/3600.0)
    minutes = int((tot_seconds - 3600*hours)/60.0)
    seconds = tot_seconds - 3600*hours - 60*minutes

    return ( "%02d:%02d:%05.2f" % ( hours, minutes, seconds) )

#
# ------------------------------------------------------------------------
#
def main():

    os.environ["OMP_NUM_THREADS"] = "1"
    opts = optparse.OptionParser( version=spl_to_eph__label )

    opts.add_option ( "-b", "--begin_date", action="store", \
                      dest="begin_date", \
                      metavar="DATE", \
                      help="Begin date" )

    opts.add_option ( "-d", "--d1_spl_dir", action="store", \
                      dest="d1_spl_dir", \
                      metavar="DIR", \
                      help="Loading d1 spline directory" )

    opts.add_option ( "-e", "--end_date", action="store", \
                      dest="end_date", \
                      metavar="DATE", \
                      help="End date" )

    opts.add_option ( "-f", "--station_file", action="store", \
                      dest="station_file", \
                      metavar="FILE", \
                      help="Station file" )

    opts.add_option ( "-j", "--num_jobs", action="store", \
                      dest="jobs", \
                      type="int", \
                      default=1, \
                      metavar="VAL", \
                      help="Number of parallel streams. Default=0" )

    opts.add_option ( "-m", "--monthly", action="store_true", \
                      dest="monthly", \
                      metavar="FLAG", \
                      help="If specified, the loading files are gathered into monthly files" )

    opts.add_option ( "-o", "--output_dir", action="store", \
                      dest="output_dir", \
                      metavar="DIR", \
                      help="Output directory" )

    opts.add_option ( "-p", "--output_prefix", action="store", \
                      dest="output_prefix", \
                      metavar="VAL", \
                      default="", \
                      help="Prefix of output files" )

    opts.add_option ( "-s", "--spl_dir", action="store", \
                      dest="spl_dir", \
                      metavar="DIR", \
                      help="Loading spline directory" )

    opts.add_option ( "-v", "--verbosity", action="store", \
                      dest="ivrb", \
                      type="int", \
                      default=0, \
                      metavar="VAL", \
                      help="Verbosity level. Default=0" )
#
# --- Get and parse options
#
    opts, args = opts.parse_args()

    if ( not opts.spl_dir ):
         print ( "spl_to_eph.py: mandatory argument --spl_dir is omitted" )
         exit ( 1 )

    if ( not os.path.isdir ( opts.spl_dir ) ):
         print ( "spl_to_eph.py: loading spline directory %s does not exist" % opts.spl_dir )
         exit ( 1 )
 
    if ( opts.d1_spl_dir ):
         if ( not os.path.isdir ( opts.d1_spl_dir ) ):
              print ( "spl_to_eph.py: loading d1 spline directory %s does not exist" % opts.d1_spl_dir )
              exit ( 1 )
 
    if ( not opts.station_file ):
         print ( "spl_to_eph.py: mandatory argument --station_file is omitted" )
         exit ( 1 )

    if ( not os.path.isfile ( opts.station_file ) ):
         print ( "spl_to_eph.py: station_file %s is not found" % opts.station_file )
         exit ( 1 )

    if ( not opts.begin_date ):
         print ( "spl_to_eph.py: mandatory argument --begin_date is omitted" )
         exit ( 1 )

    begin_date = malo_check_date ( opts.begin_date, "begin_date" ).replace(":","").replace(".","")[0:13]
    if ( not begin_date ): exit ( 1 )

    end_date   = malo_check_date ( opts.end_date,   "end_date"   ).replace(":","").replace(".","")[0:13]
    if ( not begin_date ): exit ( 1 )

    if ( not opts.end_date ):
         print ( "spl_to_eph.py: mandatory argument --end_date is omitted" )
         exit ( 1 )

    if ( not opts.output_dir ):
         print ( "spl_to_eph.py: mandatory argument --output_dir is omitted" )
         exit ( 1 )

    if ( not os.path.isdir ( opts.output_dir ) ):
         print ( "spl_to_eph.py: output directory %s does not exist" % opts.output_dir )
         exit ( 1 )

#
# --- Get MALO share, script, bin directories
#
    malo_script_dir = os.popen(MALO_BIN + "/malo_inq script"    ).read().rstrip()
    malo_share_dir  = os.popen(MALO_BIN + "/malo_inq share"     ).read().rstrip()
    malo_bin_dir    = os.popen(MALO_BIN + "/malo_inq bin_static").read().rstrip()
    tim_beg = datetime.datetime.now()

    spl_list  = []
    for paths, dirs, files in os.walk(opts.spl_dir):
        for file in files: 
            if ( file.find("#") > 0 ): continue
            if ( file.find("~") > 0 ): continue
            if ( file.find(".heb.bz2") > 0 ): 
                 date_str = file[len(file)-21:len(file)-8]
                 if ( date_str >= begin_date and date_str <= end_date ):
                      spl_list.append  ( paths + "/" + file     )


    spl_list.sort(reverse=True)
    if ( len(spl_list) < 1 ):
         print ( "No data were found or loading computation " + \
                 "for time range [%s, %s] in %s" % \
                 ( date_beg, date_end, dir ) ) 
         exit  ( 1 )

    if ( opts.ivrb > 0 ): 
         print ( "Loading for %d dates will be computed" % len(spl_list) )
         sys.stdout.flush()

    com_list = []
    n_proc   = 0
    print ( "%5d Loading displacement fields will be computed" % len(spl_list) )
    for spl_file in spl_list:
        date_str = spl_file[len(spl_file)-21:len(spl_file)-8]
        if ( opts.d1_spl_dir == None ):
             com = malo_bin_dir + "/loading_spl_heb_to_sta" + " " + \
                   spl_file + " " + \
                   opts.station_file + " " + \
                   opts.output_dir + "/" + opts.output_prefix + date_str + ".eph"
             if ( opts.jobs == 1 ):
#
# --------------- Execute command for loading computation
#
                  if ( opts.ivrb > 1 ): 
                       print ( "Executing command", com )
                       sys.stdout.flush()
                  (ret, out) = exe ( com, 1 )
                  if ( ret != 0 ):
                       print ( "Error in com: ", com )
                       print (  "\n".join(out) )
                       exit  ( 1 )
                  n_proc = n_proc + 1
                  if ( opts.ivrb > 0 ): 
                       print ( "%5d (%5d)  %s  Processed loading for epoch %s" % \
                             ( n_proc, len(spl_list), time_interval ( tim_beg ), date_str ) )
                       sys.stdout.flush()
             else:
#
# --------------- Store command for loading computation
#
                  com_list.append ( com )
        else: 
             id = spl_file.rfind("/")
             d1_spl_file = opts.d1_spl_dir + spl_file[id:].replace("_spl_","_d1_spl_")
             com = malo_bin_dir + "/loading_spl_heb_to_sta" + " " + \
                   spl_file          + " " + \
                   d1_spl_file       + " " + \
                   opts.station_file + " " + \
                   opts.output_dir + "/" + opts.output_prefix + date_str + ".eph"
             if ( opts.jobs == 1 ):
#
# --------------- Execute command for loading computation
#
                  if ( opts.ivrb > 1 ): 
                       print ( "Executing command", com )
                       sys.stdout.flush()
                  (ret, out) = exe ( com, 1 )
                  if ( ret != 0 ):
                       print ( "Error in com: ", com )
                       print (  "\n".join(out) )
                       exit  ( 1 )
                  n_proc = n_proc + 1
                  if ( opts.ivrb > 0 ): 
                       print ( "%5d (%5d)  %s  Processed loading for epoch %s" % \
                             ( n_proc, len(spl_list), time_interval ( tim_beg ), date_str ) )
                       sys.stdout.flush()
             else:
#
# --------------- Store command for loading computation
#
                  com_list.append ( com )
               
    if ( opts.jobs > 1 ):

# ------ Parallelized variant. We first write the comands in the file
#
         tmp_file = TMP_DIR + "/spl_to_eph__" + "%05d" % os.getpid() + ".dates"

         w = open(tmp_file,"w")
         for line in com_list:
             print ( line, file=w )
         w.close()

#
# ------ And then execute them using GNU parallel utility
#
         loa_com = "cat " + tmp_file + " | parallel --jobs %d :::" % opts.jobs
         if ( opts.ivrb > 1 ): 
              print ( "Executing command", loa_com )
              sys.stdout.flush()

         (ret, out) = exe ( loa_com, 1 )
         if ( ret != 0 ):
              print ( "Error in com: ", loa_com )
              print (  "\n".join(out) )
              os.remove ( tmp_file )
              exit  ( 1 )

         os.remove ( tmp_file )


    if ( opts.monthly ):
         if ( opts.ivrb > 0 ): 
              print ( "               %s  Combined loadings into monthly files" % \
                      time_interval ( tim_beg ) ) 
         mon_com = malo_bin_dir + "/malo_eph_to_monthly" + " " + \
               opts.output_dir                       + " " + \
               "erase"                               + " " + \
               "1"

         (ret, out) = exe ( mon_com, 1 )
         if ( ret != 0 ):
              print ( "Error in com: ", mon_com )
              print (  "\n".join(out) )
              os.remove ( tmp_file )
              exit  ( 1 )

    if ( opts.ivrb > 0 ): 
         print ( "Loading displacement for %5d epochs have been successfully computed  %s" % \
                 ( len(spl_list), time_interval ( tim_beg ) ) )


    exit  ( 0 )
#
# ------------------------------------------------------------------------
#

if __name__ == "__main__":
    vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    signal.signal ( signal.SIGINT,  malo_signal_handler )
    signal.signal ( signal.SIGTERM, malo_signal_handler )
    signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
    main()
