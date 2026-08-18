#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program camp_gen_schedule.py generates a VLBI schedule for         *
# *   the specified experiment of a certain campaign. A campaign has     *
# *   master files that defines experimett name, expriment date,         *
# *   experiment duration, station list, and observing modes. There is   *
# *   also associated input vex template prototype.                      *
# *                                                                      *
# *   The output of camp_gen_schedule.py is a fully functional vex       *
# *   file suitable dfor runnign the observation and its correlation.    *
# *   It embeds station procedure files. gen_camp_schedul.py also        *
# *   generates a set of snap/proc/lst files.                            *
# *                                                                      *
# *   Reqiured input files:                                              *
# *   1) campaign master file                                            *
# *   2) campaign vex input template file (proto-file)                   *
# *   3) template experiment sur_sked configuration file                 *
# *   4) template experiment description file.                           *
# *                                                                      *
# *   The last two files should reside in the vlbi experiment directory. *
# *                                                                      *
# *   Copyright (c) 1975-2025 United States Government as represented by *
# *   the Administrator of the National Aeronautics and Space            *
# *   Administration. All Rights Reserved.                               *
# *   License: NASA Open Source Software Agreement (NOSA).               *
# *                                                                      *
# * ## 21-JAN-2026 camp_gen_schedule.py v1.1 (d) L. Petrov 11-AUG-2026 # *
# *                                                                      *
# ************************************************************************
import argparse, signal, sys, os, pwd, math, shutil
from   datetime import datetime, timedelta, tzinfo
from   sur_sked_config import * # Import sur_sked confuguration
from   pet_misc        import *

camp_gen_schedule__label = "camp_gen_schedule.py  20260817"

gen_vex_template_exe = "gen_vex_template.py"

master_tmpl     = "/vlbi/@@/@@_master.txt"
proto_file_tmpl = "/vlbi/@@/@@_vex.proto"
dir      = "/vlbi"
frq_dir  = "/cont/frq"
stp_dir  = "/cont/stp"
vlbi_sta_info_file = "/apr/sta/vlbi_station.names"
vlbi_sta_desc_file = "/apr/sta/station.desc"
vlbi_sta_pos_file  = "/apr/sta/glo.sit"
vlbi_sta_vel_file  = "/apr/sta/glo.vel"

block_word_list = [                     \
                    "@MODE_BLOCK_BEG@", \
                    "@MODE_BLOCK_END@", \
                    "@MODE_REPEAT@",    \
                    "@STA_BLOCK_BEG@",  \
                    "@STA_BLOCK_END@"   \
                  ]

pub_proc_list = [            \
                    "preses",  \
                    "setscan", \
                    "setmode", \
                    "preob",   \
                    "midob",   \
                    "postob",  \
                    "postses"  \
                ]
hds_default = "h1"

#
# =============================================================
#
def gen_sched ( exper, ivrb ):
    """
    Main routine for VLBI scheduler generation
    """
    camp = exper[0:2]
    master_file = master_tmpl.replace("@@",camp)
    proto_file  = proto_file_tmpl.replace("@@",camp)

#
# --- Remove stale files
#
    com = "rm " + dir + "/" + exper + "/*.prc "  + \
                  dir + "/" + exper + "/*vex* "  + \
                  dir + "/" + exper + "/*.frq "  + \
                  dir + "/" + exper + "/*.skf_log "  + \
                  dir + "/" + exper + "/*.plan " + \
                  dir + "/" + exper + "/*.key "  + \
                  dir + "/" + exper + "/*.ast "  + \
                  dir + "/" + exper + "/*.tmpl "  + \
                  dir + "/" + exper + "/*.stat " + \
                  dir + "/" + exper + "/*.sou_lis"
    (ret,out) = exe ( com )
    
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
    start_time = None
    mode_dict = {}
    hds_list  = []
    seq_list  = []
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
             sta_dict={}
             for sta in sta_list:
                 sta_dict[sta] = {"hds": None, "prc": None, "mode": [] }

             modes_list = line.split()[5].split(",")
             if ( len(line.split()) >= 8 ):
                  if ( line.split()[6] == "proto:" ):
                       proto_file  = line.split()[7] 
                       

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

    if ( ivrb >= 4 ):
         print ( where_info(), "modes_list= ", modes_list  )

#
# --- extract mode and hardware setup names from modes_list
#
    mode_name = modes_list[0].split(":")[0] 
    modes_str = ",".join(modes_list)
    if ( ":" in modes_list[0] ):
         hds_line = modes_list[0].split(":")[1] 
    else:
         hds_line = hds_default
    
    if ( ivrb >= 2 ):
         print ( where_info(), "modes_str= ", modes_str  )
         print ( where_info(), "mode_name= ", mode_name  )
         print ( where_info(), "hds_line=  ", hds_line   )

#
# --- Read the sequence files. We want to extract the hardware setup from there
#
    hds_lst = []
    seq_lst = []
    seq_file_lst = []
    for seq_name in hds_line.split("+"):
#
# ----- Form the sequence file name
#
        seq_file = "/cont/frq/" + mode_name + "_" + seq_name + ".seq"
        if ( not os.path.isfile ( seq_file ) ):
             seq_file_nohds = "/cont/frq/" + mode_name + ".seq"
             if ( os.path.isfile ( seq_file_nohds ) ):
                  seq_file = seq_file_nohds 
             else:
                  print ( "ERROR: cannot find sequence file %s" % seq_file )
                  exit  ( 1 )
#   
# ----- Search for HDS_NAME and STATION keywords in the sequence file
#   
        seq = read_file ( seq_file )
        hds_name = None
        for lin in seq:
            if ( len(lin.split()) > 1 ):
                 if ( lin.split()[0] == "HDS_NAME" ):
#
# ------------------- Extract hardware setup name
#
                      hds_name = lin.split()[1]
                 if ( lin.split()[0] == "STATION" ):
#
# ------------------- Extract the list of station that supports this sequene
#
                      sta = lin.split()[2].lower()
                      if ( sta in sta_dict.keys() ): 
                           sta_dict[sta]["hds"] = hds_name

        if ( hds_name == None ):
             print ( "ERROR: cannot find HDS_MAME in sequence file %s" % seq_file )
             exit  ( 1 )
    
#
# ----- Update the hardware setup list
#
        hds_lst.append ( hds_name )
                 
    for hds in hds_lst:
        if ( not hds in hds_list ):
             hds_list.append ( hds )
#
# ---- Build file names for schedule configation setup and experiment description file
#
    skf_schedule = dir + "/" + exper + "/" + exper + ".skf"
    txt_schedule = dir + "/" + exper + "/" + exper + ".txt"
    
    if ( ivrb >=2 ):
         print ( where_info(), "mode_dist= ", mode_dict )
         print ( where_info(), "hds_list=  ", hds_list  )
         print ( where_info(), "sta_dict=  ", sta_dict  )
    
    if ( not os.path.isfile(skf_schedule) ):
         print ( "%s did not find schedule configuration file %s " % \
                 ( where_error(), skf_schedule ) )
         exit  (  1 )
    
    if ( not os.path.isfile(txt_schedule) ):
         print ( "%s did not find experiment description file %s " % \
                 ( where_error(), skf_schedule ) )
         exit  (  1 )
    
#
# --- Read the schedule control file
#
    skf = read_file ( skf_schedule )
    if ( skf == None ):
         print ( "ERROR: Error in reading the schedule control file %s " % skf_schedule )
         exit  (  1 )
#
# --- Read the schedule configuration template file and update a number of fields
#
    out = []
    for line in skf:
        if ( "# Last updated on" in line ):
             time_now = str(datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S"))       
             line = line.replace(line.split()[4],time_now)
    
        if ( "EXPERIMENT_CODE:" in line ):
             line = line.replace(line.split()[1],exper)
        if ( "START_TIME:" in line ):
             line = line.replace(line.split()[1],start_time)
        if ( "STOP_TIME:" in line ):
             line = line.replace(line.split()[1],stop_time)
        if ( "HEADER_VEX_TEMPLATE_FILE:" in line ):
             vex_tmpl_file = dir + "/" + exper + "/" + exper + "_vex.tmpl"
             line = line.replace(line.split()[1],vex_tmpl_file)
        if ( "HARDWARE_SETUP_NAME:" in line ):
             hds_str = ",".join(hds_list)
             line = line.replace(line.split()[1],hds_str)
        if ( "OBSERVING_MODE_NAME:" in line ):
             line = line.replace ( line.split()[1], modes_str )
        if ( "OUT_PLAN:" in line ):
             line = line.replace(line.split()[1],dir + "/" + exper + "/" + exper + ".plan")
        if ( "OUT_VEX:" in line ):
             line = line.replace(line.split()[1],dir + "/" + exper + "/" + exper + ".vex")
        if ( "OUT_STAT:" in line ):
             line = line.replace(line.split()[1],dir + "/" + exper + "/" + exper + ".stat")
        if ( "OUT_KEY:" in line ):
             line = line.replace(line.split()[1],dir + "/" + exper + "/" + exper + ".key")
        if ( "OUT_AST:" in line ):
             line = line.replace(line.split()[1],dir + "/" + exper + "/" + exper + ".ast")
        if ( "OUT_SOU_LIST:" in line ):
             line = line.replace(line.split()[1],dir + "/" + exper + "/" + exper + ".sou_lis")
        out.append ( line ) 
    
#
# --- Write down updated schedule confiration file
#
    filout = skf_schedule + "__" + "%08d" % os.getpid()
    (ret,out) = write_file ( out, filout )
    check_err_exe ( ret, out, "write_file" )
    os.rename ( filout, skf_schedule )

#
# --- Generate vex template file
#
    com = gen_vex_template_exe        + " " + \
          "-e " + exper               + " " + \
          "-sl " + ",".join(sta_list) + " " + \
          "-m  " + modes_str          + " " + \
          "-p  " + proto_file         + " " + \
          "-d  " + dir                + " " + \
          "-f  " + frq_dir            + " " + \
          "-st " + stp_dir            + " " + \
          "-si " + vlbi_sta_info_file + " " + \
          "-sd " + vlbi_sta_desc_file + " " + \
          "-sp " + vlbi_sta_pos_file  + " " + \
          "-sv " + vlbi_sta_vel_file  + " " + \
          "-v  " + "%d" % ivrb

    if ( ivrb >= 2 ):
         print ( "%s about to execute command %s" % ( where_info(), com ) )
    (ret,err)  = exe ( com )
    if ( ret != 0 ):
         for line in err:
             print ( err )
         print ( "%s in generation vex template file with command %s" % ( where_error(), com ) )
         exit  ( 1 )
#
# --- Generate the schedule file in vex, key, and ast formats
#
    com = "sur_sked " + skf_schedule + " 6 > " + dir + "/" + exper + "/" + exper + "_skf.log"
    if ( ivrb >= 2 ): 
         print ( "%s: about to execute command %s" % ( where_info(), com ) )
    (ret,out) = exe_pipe ( com )
    check_err_exe ( ret, out, com )

#
# --- Generate schedule in stp and prc format for each station
#
    gen_snap_proc ( exper, sta_list, ivrb )

    if ( ivrb > 0 ):
         print ( "Schedule %s is ready" % exper )

#
# =============================================================
#
def gen_snap_proc ( exper, sta_list, ivrb ):
    """
    Take input vex file and generate output proc, snap, and lst files
    for each station
    """
    for sta in sta_list:
#
# ----- Cycle over stations
#
        prc_file  = dir + "/" + exper + "/" + exper + sta + ".prc"
        snap_file = dir + "/" + exper + "/" + exper + sta + ".snp"
        lst_file  = dir + "/" + exper + "/" + exper + sta + ".lst"
#
# ----- Set recorder and recording rate
#
        if ( sta == "kk" ): 
             rec_mode = "mk5_ext"
             rec_rate = 1024
        else:
             rec_mode = "mk6_ext"
             rec_rate = 8192
    
#
# ----- Generate schedule in snap format for stations sta from the schedule in vex format 
#
        com = "python3 " + sur_sked_prefix + "/bin/vex_to_snap.py -m " + rec_mode + " -o " + \
              dir + "/" + exper + "/" + exper + sta + ".snp" + " " + \
              dir + "/" + exper + "/" + exper + ".vex" + " " + \
              sta
        if ( ivrb >= 2 ): 
             print ( "%s: about to execute command %s " % ( where_info(), com ) )
        (ret,out) = exe_pipe ( com )
        check_err_exe ( ret, out, com )
    
#
# ----- Generate Field System procedure file for station sta from the schedule in vex format 
#
        com = "python3 " + sur_sked_prefix + "/bin/vex_to_proc.py -o " + \
              dir + "/" + exper + "/" + exper + sta + ".prc" + " " + \
              dir + "/" + exper + "/" + exper + ".vex" + " " + \
              sta
        if ( ivrb >= 2 ): 
             print ( "%s: about to execute command %s " % ( where_info(), com ) )
        (ret,out) = exe_pipe ( com )
        check_err_exe ( ret, out, com )
    
#
# ----- Generate scheduel listinf for station sta from the schedule in vex format 
#
        com = "python3 " + sur_sked_prefix + "/bin/snap_to_lst.py -o " + \
              dir + "/" + exper + "/" + exper + sta + ".lst" + " " + \
              dir + "/" + exper + "/" + exper + sta + ".snp" + " " + \
              "-s %d" % rec_rate
        if ( ivrb >= 2 ): 
             print ( "%s: about to execute command %s " % ( where_info(), com ) )
        (ret,out) = exe_pipe ( com )
        check_err_exe ( ret, out, com )
    
#
# ----- Run a progfram for verification of schedules in proc and snap formats
#
        com = sur_sked_prefix + "/bin/check_proc_snap " + \
              dir + "/" + exper + "/" + exper + sta + ".prc" + " " + \
              dir + "/" + exper + "/" + exper + sta + ".snp" + " " + \
              "0"
        if ( ivrb >= 2 ): 
             print ( "%s: about to execute command %s " % ( where_info(), com ) )
        (ret,out) = exe_pipe ( com )
        check_err_exe ( ret, out, com )
    
        if ( sta == "kk" and "kt" in prc_file ):
#
# ---------- A special trick for kt experi,ents
#
             com = 'sed -i "s@lo=lod,6600.0@lo=lod,7600.0@g" ' + prc_file
        (ret,out) = exe_pipe ( com )
        check_err_exe ( ret, out, com )
#
# ------------------------------------------------------------------------
#
def main():
    """
    Parse arguments
    """
    parser = argparse.ArgumentParser( description=camp_gen_schedule__label )
    parser.add_argument ( '--version', action='version', version=camp_gen_schedule__label )
    parser.add_argument ( "-e", "--experiment",  \
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

    parser.add_argument ( "-s", "--sta_list",    \
                          action="store",        \
                          dest="sta_list",       \
                          metavar="sta_list",    \
                          help="Station list"    )

    parser.add_argument ( "-p", "--only-proc",   \
                          action="store_true",   \
                          default=False,         \
                          dest="only_proc_snap", \
                          help="Only generate snap and proc files" )

    args = parser.parse_args()
         
    if ( args.only_proc_snap ): 
#
# ------ Only generation station snap and procedure files
#
         if ( not args.sta_list ):
              print ( "ERROR: Station list is not defined" )
              exit  ( 1 )
         gen_snap_proc ( args.exper, args.sta_list )
         exit ( 0 )

    gen_sched ( args.exper, args.verb )
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
