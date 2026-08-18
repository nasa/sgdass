#!/usr/bin/env python3
import argparse, signal, sys, os, pwd, math, shutil
from   datetime import datetime, timedelta, tzinfo
from   sur_sked_config import * # Import sur_sked confuguration
from   pet_misc        import *

camp_clone_schedule__label = "camp_clone_schedule.py  20260717"
master_tmpl = "/vlbi/@@/@@_master.txt"
stp_dir     = sur_sked_stp

#
# ------------------------------------------------------------------------
#
def camp_clone( from_exper, exper, verb ):
#
# --- Walk over the stp directory file.
#
    sta_name_dict = {}
    for path, dirs, files in os.walk(stp_dir):
        for file in files:
            if ( "#" in file ): continue
            if ( "~" in file ): continue
            stp_file = path + "/" + file
            if ( not os.path.isfile ( stp_file ) ) : continue
            buf = read_file ( stp_file ) 
            for line in buf:
                if ( len(line.split()) == 0 ): continue
                if ( len(line.split()) >= 4 ):
                     if ( line.split()[0] == "SHORT_NAME:" ):
                          short_name = line.split()[3].lower()
                          long_name  = line.split()[1].upper()
                          sta_name_dict[short_name] = {}
                          sta_name_dict[short_name]["short_name"] = short_name
                          sta_name_dict[short_name]["long_name"]  = long_name


    if ( exper[0:1] == "/" ):
         print ( where_error(), "Malformed experiment name %s. It cannot start with /" % exper )
         exit ( 1 )

    if ( from_exper[0:1] == "/" ):
         print ( where_error(), "Malformed experiment from name %s. It cannot start with /" % from_exper )
         exit ( 1 )

    camp = from_exper[0:2]
    master_file = master_tmpl.replace("@@",camp)
    if ( not os.path.isfile ( master_file ) ):
         print ( where_error(), "Did not find master file %s. Please check experiment name" % master_file )
         exit ( 1 )

    if ( from_exper[0:2] != exper[0:2] ):
         print ( where_error(), "Experiments to %s and from %s belong to different campagns" % \
                 ( from_exper, exper ) )
         exit ( 1 )

#
# --- Read the master file
#
    mas = read_file ( master_file )
    if ( mas == None ):
         print ( "%s cannot find master file %s" % ( where_error(), master_file ) )
         exit  ( 1 )
    
#
# --- Parse the master file
#
    start_time   = None
    hds_str      = ""
    long_sta_str = ""
    short_sta_str = ""
    for line in mas:
        if ( len(line.split()) <  5 ): continue
        if ( line[0:1] == "#"       ): continue
        if ( line.split()[3] == exper ):
             start_time = line.split()[0][0:4] + "." + \
                          line.split()[0][4:6] + "." + \
                          line.split()[0][6:8] + "_" + line.split()[1] + ":00"
             start_tim = datetime.datetime.strptime( start_time, "%Y.%m.%d_%H:%M:%S" )
             dur_str = line.split()[2]
             if ( not ":" in dur_str ):
                  print ( "%s failure in parsing line %s in the master template file %s" % \
                          ( where_error(), line, master_file  ) )
                  print ( "There is no ':' delimeter in the duration field" )
                  exit  (  1 )
#
# ---------- Get experiment duration
#
             dur_str = line.split()[2]
             if ( not ":" in dur_str ):
                  print ( "%s in parsing line %s in the master template file %s" % \
                          ( where_error(), line, master_file  ) )
                  print ( "There is no ':' delimeter in the duration field" )
                  exit  (  1 )
             if ( len(dur_str.split(":")) == 2 ):
                  dur_hours = 0
                  dur_mins  = int ( dur_str.split(":")[0] )
                  dur_secs  = int ( dur_str.split(":")[1] )
             elif ( len(dur_str.split(":")) == 3 ):
                  dur_hours = int ( dur_str.split(":")[0] )
                  dur_mins  = int ( dur_str.split(":")[1] )
                  dur_secs  = int ( dur_str.split(":")[2] )
    
             sta_list = line.split()[4].split(",")
             sta_long_list = []
             for sta in sta_list:
                 if ( sta in sta_name_dict.keys() ):
                      long_sta_str  = long_sta_str  + "," + sta_name_dict[sta]["long_name"]
                      short_sta_str = short_sta_str + "," + sta_name_dict[sta]["short_name"]
                      sta_long_list.append ( sta_name_dict[sta]["long_name"] )
                 else:
                      print ( where_error(), "Unknown station %s is specicied in the master file %s" % 
                              ( sta, master_file ) )
                      exit  ( 1 )

             hds_str = line.split()[5] 

    if ( not start_time ):
         print ( "%s: experiment %s is not the master template file %s" % \
                 ( where_error(), exper, master_file ) )
         exit  ( 1 )
#
# ---- Compute stop time
#
    stop_tim = start_tim + datetime.timedelta ( hours=dur_hours, \
                                                minutes=dur_mins, \
                                                seconds=dur_secs )
    stop_time = datetime.datetime.strftime ( stop_tim, '%Y.%m.%d_%H:%M:%S' )
    stop_doy_time = datetime.datetime.strftime ( stop_tim, '%Y.%j_%H:%M:%S' )
    start_doy_time = datetime.datetime.strftime ( start_tim, '%Y.%j_%H:%M:%S' )

    from_exp_dir = sur_sked_exp + "/" + from_exper
    if ( not os.path.isdir(from_exp_dir) ):
         print ( where_error(), "Did not find from experiment directory %s" % from_exp_dir )
         exit  ( 1 )

    from_sched_file = from_exp_dir + "/" + from_exper + ".skf"
    from_descr_file = from_exp_dir + "/" + from_exper + ".txt"

    if ( not os.path.isfile(from_sched_file) ):
         print ( where_error(), "Did not find schedule from %s file" % from_sched_file )
         exit  ( 1 )

    if ( not os.path.isfile(from_descr_file) ):
         print ( where_error(), "Did not find descruption from %s file" % from_descr_file )
         exit  ( 1 )

    exp_dir = sur_sked_exp + "/" + exper
    if ( not os.path.isdir(exp_dir) ):
         os.mkdir ( exp_dir, mode=0o775 )

    sched_file = exp_dir + "/" + exper + ".skf"
    descr_file = exp_dir + "/" + exper + ".txt"

    fl_error = False
    if ( os.path.isfile(sched_file) ):
         print ( where_error(), "Found schedule %s. Please remove it if you really to clone" % sched_file )
         fl_error = True
    if ( os.path.isfile(descr_file) ):
         print ( where_error(), "Found description %s. Please remove it if you really to clone" % descr_file )
         fl_error = True
    if ( fl_error ):
         exit ( 1 )

    skf = read_file ( from_sched_file )
    out = []
    for line in skf:
        if ( "# Control"                 in line or \
             "EXPERIMENT_CODE:"          in line or \
             "HEADER_VEX_TEMPLATE_FILE:" in line or \
             "OUT_PLAN:"                 in line or \
             "OUT_VEX:"                  in line or \
             "OUT_STAT:"                 in line or \
             "OUT_KEY:"                  in line or \
             "OUT_AST:"                  in line or \
             "OUT_SOU_LIST:"             in line    ):
             line = line.replace ( from_exper, exper )
        if ( "START_TIME:" in line ):
             from_start_time = line.split()[1]
             line = line.replace ( from_start_time, start_time )
        if ( "STOP_TIME:" in line ):
             from_stop_time = line.split()[1]
             line = line.replace ( from_stop_time, stop_time )
        if ( "STATIONS:" in line ):
             from_long_sta_str = line.split()[1]
             line = line.replace ( from_long_sta_str, long_sta_str + ":r" )
        if ( "OBSERVING_MODE_NAME:" in line ):
             from_obs_mode = line.split()[1]
             line = line.replace ( from_obs_mode, hds_str )
        if ( "HARDWARE_SETUP_NAME:" in line ):
             from_hds_str = line.split()[1]
             line = line.replace ( from_hds_str, "@DUMMY@" )

        out.append ( line )

    (ret,err) = write_file ( out, sched_file )
    if ( ret != 0 ):
         print ( where_error(), "Error in writing output schedule file %s" % sched_file )
         exit ( 1 )              

    descr = read_file ( from_descr_file )
    out = []
    k = 0    
    kk = -111
    for line in descr:
        k = k + 1
        if ( "VLBI experiment:" == line[0:16] ):
             line = line.replace ( from_exper, exper )
        if ( "Observing stations:" == line[0:19] ):
             line = "Observing stations: " + short_sta_str.replace(","," ")
        if ( "Nominal start time:" == line[0:19] ):
             from_start_time_str = line.split()[3]
             line = line.replace ( from_start_time_str, start_time )

        if ( "Nominal stop  time:" == line[0:19] ):
             from_stop_time_str  = line.split()[3]
             line = line.replace ( from_stop_time_str, stop_time )
        if ( "Start:" == line[0:6] and "End:" == line[25:29] ):
             from_start_doy_time = line.split()[1]
             from_stop_doy_time  = line.split()[3]
             from_sta_list_str   = line.split()[5]
             sta_list_str = str(",".join(sta_list))
             line = line.replace ( from_start_doy_time, start_doy_time )
             line = line.replace ( from_stop_doy_time,  stop_doy_time )
             line = line.replace ( from_sta_list_str,   sta_list_str )
        if ( "Date of experiment:" == line[0:19] ):
             from_start_time_fancy_str = line.split()[3]
             start_time_fancy_str = datetime.datetime.strftime ( start_tim, '%Y,%b,%d' )
             line = line.replace ( from_start_time_fancy_str, start_time_fancy_str )
        if ( "Nominal Start Time:" == line[0:19] ):
             from_start_time_fancy_str = line.split()[3]
             start_time_fancy_str = datetime.datetime.strftime ( start_tim, '%Hh%mm' )
             line = line.replace ( from_start_time_fancy_str, start_time_fancy_str )
        if ( "Nominal End Time:" == line[0:19] ):
             from_start_time_fancy_str = line.split()[3]
             stop_time_fancy_str = datetime.datetime.strftime ( stop_tim, '%Hh%mm' )
             line = line.replace ( from_stop_time_fancy_str, stop_time_fancy_str )
        if ( "Duration:" == line[0:9] ):
             dur_tim = stop_tim - start_tim
             dur_sec = dur_tim.seconds
             from_dur_hour_str = line.split()[1]
             dur_hour_str = "%5.2f" % (dur_sec/3600.0)
             line = line.replace ( from_dur_hour_str, dur_hour_str )
        if ( "Participating stations:" == line[0:23] ):
             from_num_sta_str = line.split()[2]
             num_sta_str = "(%d)" % len(sta_list)
             line = line.replace ( from_num_sta_str, num_sta_str )
             kk = k
        if ( k ==  kk + 1 ):
             for j in range(0,len(sta_list)):
                 out.append ( "%-8s  %2s" % ( sta_long_list[j], sta_list[j] ) )
        if ( k > kk and k < kk + len(sta_list) + 1 ):
             continue
        out.append ( line )

    (ret,err) = write_file ( out, descr_file )
    if ( ret != 0 ):
         print ( where_error(), "Error in writing output description file %s" % descr_file )
         exit ( 1 )              

    return ( 0 )

#
# ------------------------------------------------------------------------
#
def main():
    """
    Parse arguments
    """
    parser = argparse.ArgumentParser( description=camp_clone_schedule__label )
    parser.add_argument ( '--version', action='version', version=camp_clone_schedule__label )

    parser.add_argument ( "-f", "--from_experiment",  \
                          action="store",             \
                          required=True,              \
                          dest="from_exper",          \
                          metavar="from_exper",       \
                          help="Experiment name" )

    parser.add_argument ( "-t", "--to_experiment",  \
                          action="store",        \
                          required=True,         \
                          dest="exper",          \
                          metavar="exper",       \
                          help="Experiment name" )

    parser.add_argument ( "-v", "--verbosity",   \
                          action="store",        \
                          dest="verb",           \
                          default=1,             \
                          metavar="verbosity",   \
                          type=int,              \
                          help="Verbosity level" )

    args = parser.parse_args()

    ret = camp_clone ( args.from_exper, args.exper, args.verb )
#
# ------------------------------------------------------------------------
#
if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
        pima_child_pid = None
        main()
    except KeyboardInterrupt:
        print ( "%s: Interrupted" % sys.argv[0] )
        exit  ( 1 )
