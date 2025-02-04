#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program  gen_sched.py
# *                                                                      *
# *   Example:                                                           *
# *   gen_sched.py -a gen|susbmit                                        *
# *                -c campaign                                           *
# *                -e y4045a                                             *
# *                                                                      *
# * ### 06-FEB-2024   gen_sched.py  v 1.5 (c) L. Petrov 30-JUN-2024 ###  *
# *                                                                      *
# ************************************************************************
import argparse, signal, sys, os, pwd, math
from   datetime import datetime, timedelta, tzinfo
from   sur_sked_config import * # Import sur_sked confuguration
from   pet_misc        import *

gvt__label   = "gen_sched.py 20240630"

seq_dir  = sur_sked_seq
stp_dir  = sur_sked_stp
prc_dir  = sur_sked_prc
exp_dir  = sur_sked_exp
frq_def  = sur_sked_freq_def

vsdc_com = "/opt64/bin/vsdc.py -c /cont/vsdc/cddis.cnf"

nasa_sta_list = [ "gs", "k2", "kk", "mg" ]

default_recording_rate = 8192

def gen_proj_dict ( exp, camp, ivrb ):
    """
    """
    master_file = exp_dir + "/" + camp + "/" + camp + "_master.txt"
    proj_dict = {}

    if ( not os.path.isfile ( master_file ) ):
         print ( "Cannot find master file %s for campaign %s" % \
                 ( master_file, camp ) )
         return ( 1,proj_dict)

    buf = read_file ( master_file )

    proj_dict["camp"] = camp
    for line in buf:
        if ( len(line.split()) < 6 ): continue
        if ( line.split()[3] == exp ):
             proj_dict["exp"] = exp
             proj_dict["sta_list"] = []
             for word in line.split()[4].split(","):
                 proj_dict["sta_list"].append ( word )
             proj_dict["hds"] = line.split()[5]
             date_str = line.split()[0][0:4] + "." + \
                        line.split()[0][4:6] + "." + \
                        line.split()[0][6:8] + "_" + \
                        line.split()[1] + ":00"
             if ( len(line.split()) >= 7 ):
                  proj_dict["filter_prog"] = ""
                  for i in range(6,len(line.split())):
                      proj_dict["filter_prog"] = proj_dict["filter_prog"] + line.split()[i] + " "
             else:
                  proj_dict["filter_prog"] = None

             if ( ivrb > 2 ):
                  print ( "start_time for experiment %s is %s" % ( exp, date_str ) )
             try:
                  proj_dict["start_date"] = datetime.datetime.strptime( date_str, "%Y.%m.%d_%H:%M:%S" )
             except Exception as e:
                  print ( "Error in parsing start date %s -- %s" % ( date_str, e ) )
                  exit  ( 1 )

             try:
                  dur_hours = int(line.split()[2].split(":")[0])
                  dur_mins  = int(line.split()[2].split(":")[1])
                  dur_secs  = int(line.split()[2].split(":")[2])
             except Exception as e:
                  print ( "Error in parsing experiment duration %s -- %s" % ( line.split()[2], e ) )
                  exit  ( 1 )

             dur = timedelta ( hours=dur_hours, minutes=dur_mins, seconds=dur_secs )
             proj_dict["stop_date"] = proj_dict["start_date"] + dur
             proj_dict["dur_secs"]  = dur_hours*3600.0 + dur_mins*60 + dur_secs

    return ( 0,proj_dict )

#
# ------------------------------------------------------------------------
#


def gen_skf ( proj_dict, ivrb ):
    """
    aaa 
    """

    skf_file = exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + ".skf"
    if ( not os.path.isfile ( skf_file ) ):
         print ( "Did not find schedule file %s" % skf_file )
         exit  ( 1 )

    txt_file = exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + ".txt"
    if ( not os.path.isfile ( txt_file ) ):
         print ( "Did not find schedule note file %s" % txt_file )
         exit  ( 1 )

#
# --- Walk over the stp directory file.
#
    sta_dict = {}
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
                          if ( short_name in proj_dict["sta_list"] ):
                               sta_dict[short_name] = {}
                               sta_dict[short_name]["short_name"] = short_name
                               sta_dict[short_name]["long_name"] = long_name

    sta_dict = dict( sorted (sta_dict.items()) )

    if ( ivrb > 1 ):
         print ( "gen_sched-133 ", "sta_dict: ", sta_dict )

    long_sta_str = ""
    for sta in proj_dict["sta_list"]:
        if ( not sta in sta_dict.keys() ):
             print ( "Did not find station %s in the stp directory %s" % \
                      ( sta, stp_dir ) )
             exit  ( 1 )
        long_sta_str = long_sta_str + "," + sta_dict[sta]["long_name"]

    long_sta_str = long_sta_str + ":r" 

    if ( ivrb > 1 ):
         print ( "long_sta_str= ", long_sta_str )

    buf = read_file ( skf_file )
    out = []
    start_time_str = datetime.datetime.strftime( proj_dict["start_date"], "%Y.%m.%d_%H:%M:%S" )
    stop_time_str  = datetime.datetime.strftime( proj_dict["stop_date"],  "%Y.%m.%d_%H:%M:%S" )

    for line in buf:
        if ( line[0:1] == "#" ):
             out.append ( line )
             continue
        if ( line.split()[0] == "EXPERIMENT_CODE:" ):
             line = line.replace ( line.split()[1], proj_dict["exp"] )
        elif ( line.split()[0] == "START_TIME:" ):
             line = line.replace ( line.split()[1], start_time_str )
        elif ( line.split()[0] == "STOP_TIME:" ):
             line = line.replace ( line.split()[1], stop_time_str )
        elif ( line.split()[0] == "STATIONS:" ):
             line = line.replace ( line.split()[1], long_sta_str )
        elif ( line.split()[0] == "HARDWARE_SETUP_NAME:" ):
             line = line.replace ( line.split()[1], proj_dict["hds"].split(":")[0] )
        elif ( line.split()[0] == "HEADER_VEX_TEMPLATE_FILE:" ):
             line = line.replace ( line.split()[1], exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + "_vex.tmpl" )
        elif ( line.split()[0] == "OBSERVING_MODE_NAME:" ):
             ip = line.index ( line.split()[1] )
             line = line[0:ip] + proj_dict["hds"].split(":")[1].replace(","," ")
        elif ( line.split()[0] == "OUT_PLAN:" ):
             line = line.replace ( line.split()[1], exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + ".plan" )
        elif ( line.split()[0] == "OUT_VEX:" ):
             line = line.replace ( line.split()[1], exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + ".vex" )
        elif ( line.split()[0] == "OUT_STAT:" ):
             line = line.replace ( line.split()[1], exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + ".stat" )
        elif ( line.split()[0] == "OUT_KEYS:" ):
             line = line.replace ( line.split()[1], exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + ".key" )
        elif ( line.split()[0] == "OUT_AST:" ):
             line = line.replace ( line.split()[1], exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + ".ast" )
        elif ( line.split()[0] == "OUT_SOU_LIST:" ):
             line = line.replace ( line.split()[1], exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + ".sou_lis" )

        out.append ( line )
             
    (ret,err) = write_file ( out, skf_file )
    check_err_exe ( ret, err, "write_file " + skf_file )
    if ( ivrb > 0 ):
         print ( "gen_sched ", "Updated schedule configuration file %s" % skf_file )

    buf = read_file ( txt_file )
    goal    = []
    fl_goal = False
    corr    = "Unknown"
    contact = ""
    for line in buf:
        if ( line[0:6] == "Start:" ):
             fl_goal = True
             continue
        if ( line[0:19] == "Date of experiment:" ):
             fl_goal = False
        if ( line[0:11] == "Correlator:" ):
             corr = line.split()[1]
        if ( line[0:8] == "Contact:" ):
             contact = line
        if ( fl_goal ):
             goal.append ( line )

    out = []
    out.append ( "VLBI experiment: %s" % proj_dict["exp"] )
    out.append ( "Observing stations: %s" % " ".join(proj_dict["sta_list"]) )
    out.append ( "Nominal start time: %s" % datetime.datetime.strftime( proj_dict["start_date"], "%Y.%m.%d_%H:%M:%S" ) )
    out.append ( "Nominal stop  time: %s" % datetime.datetime.strftime( proj_dict["stop_date"],  "%Y.%m.%d_%H:%M:%S" ) )
    out.append ( "Start: %s End: %s Stations: %s # Needed for CDDIS software" % \
                  ( datetime.datetime.strftime( proj_dict["start_date"], "%Y.%j_%H:%M:%S" ), \
                    datetime.datetime.strftime( proj_dict["stop_date"],  "%Y.%j_%H:%M:%S" ), \
                    ",".join(proj_dict["sta_list"]) ) \
               )

    for line in goal:
        out.append ( line )

    out.append ( "Date of experiment: %s" % datetime.datetime.strftime( proj_dict["start_date"], "%Y,%b,%d" ) )
    out.append ( "Nominal Start Time: %s" % datetime.datetime.strftime( proj_dict["start_date"], "%Hh%Mm UT" ) )
    out.append ( "Nominal End   Time: %s" % datetime.datetime.strftime( proj_dict["stop_date"], "%Hh%Mm UT" ) )
    out.append ( "Duration:           %5.2f  hr" % (proj_dict["dur_secs"]/3600.0) )
    out.append ( "Correlator:         %s" % corr )
    out.append ( "Participating stations: (%d)" % len(proj_dict["sta_list"]) )
    for sta in proj_dict["sta_list"]:
        out.append ( "%-8s  %s" % ( sta_dict[sta]["long_name"], sta_dict[sta]["short_name"] ) )
    out.append ( "" )
    out.append ( contact )
    out.append ( datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S") )

    (ret,err) = write_file ( out, txt_file )
    check_err_exe ( ret, err, "write_file " + txt_file )
    if ( ivrb > 0 ):
         print ( "gen_sched: Updated schedule description   file %s" % skf_file )

    return (0,proj_dict)

#
# ------------------------------------------------------------------------
#
def main():
    """ 
    Main program of the gen_vex_tmpl utility
    """

    parser = argparse.ArgumentParser( description=gvt__label )

    parser.add_argument ( "-e", "--exepriment", \
                          action="store", \
                          dest="exp", \
                          default=0, \
                          metavar="value", \
                          help="Experiment name" )


    parser.add_argument ( "-c", "--campaign", \
                          action="store", \
                          dest="campaign", \
                          metavar="value", \
                          help="VLBI observing campaign name" )


    parser.add_argument ( "-a", "--action", \
                          choices=["gen", "submit", "forced_submit"], 
                          action="store", \
                          dest="action", \
                          metavar="value", \
                          help="Action to perform: to generarate schedule to submit it" )

    parser.add_argument ( "-v", "--verbosity", \
                          action="store", \
                          dest="verb", \
                          default=1, \
                          metavar="value", \
                          type=int, \
                          help="Verbosity level" )

#
# --- Get and parse options
#
    args = parser.parse_args()

    if ( not args.exp ):
         print ( "Experiment name is not specified" )
         exit ( 1 )

    if ( not args.campaign ):
         print ( "Campaign is not specified" )
         exit ( 1 )

    if ( not args.action ):
         print ( "Action is not specified" )
         exit ( 1 )

#
# --- Collect information about the project in proj_dict
#
    (ret,proj_dict) = gen_proj_dict ( args.exp, args.campaign, args.verb )
    if ( ret != 0 ):
         exit ( 1 )

    if ( not "exp" in proj_dict.keys() ):
         print ( "gen_sched-308 ", "Experiment %s was not found in the master file for campaign %s" % \
                 ( args.exp, args.campaign ) )
         if ( args.verb > 2 ):
              print ( "gen_sched-311 ", "proj_dict= ", proj_dict )
         exit  ( 1 ) 

    if ( args.action == "gen" ):
         (ret,proj_dict) = gen_skf ( proj_dict, args.verb )
         if ( args.verb > 1 ):
              print ( "proj_dict=", proj_dict )

         com = "gen_vex_tmpl.py  -e " + proj_dict["exp"]                + " " + \
                                "-s " + ",".join(proj_dict["sta_list"]) + " " + \
                                "-m " + proj_dict["hds"]                + " " + \
                                "-i " + exp_dir + "/" + proj_dict["camp"] + "/" +  proj_dict["camp"] + "_vex.proto" + " " + \
                                "-o " + exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + "_vex.tmpl" + " " + \
                                "-v %d" % args.verb 
         if ( args.verb > 1 ):
              print ( "gen_sched.py: About to execute ", com )

         (ret,err) = exe_pipe ( com )
         if ( ret != 0 ):
              for line in err:
                  print ( line ) 
              print ( "Error in executing command %s" % com ) 
              exit  ( 1 )

         if ( args.verb > 0 ):
              print ( "gen_sched: Generated vex template file %s" % exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + "_vex.tmpl" )
    
         skf_file     = exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + ".skf" 
         skf_log_file = exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + "_skf.log"
         vex_file     = exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + ".vex" 

         com = "sur_sked " + skf_file + " 6 > " + skf_log_file

         if ( args.verb > 1 ):
              print ( "gen_sched.py: About to execute ", com )

         (ret,err) = exe ( com )
         if ( ret != 0 ):
              for line in err:
                  print ( line ) 
              print ( "Error in executing command %s" % com ) 
              exit  ( 1 )

         if ( args.verb > 0 ):
              print ( "gen_sched: Generated schedule file" )
     
         if ( proj_dict["filter_prog"] != None ):
              vex_file_orig = vex_file[0:-4] + ".orig_vex"
              os.rename ( vex_file, vex_file_orig )

              com = proj_dict["filter_prog"]                     + " " + \
                                        "-i " + vex_file_orig    + " " + \
                                        "-o " + vex_file         + " " + \
                                        "-m " + proj_dict["hds"] + " " + \
                                        "-v %d" % args.verb
              if ( args.verb > 1 ):
                   print ( "gen_sched.py: About to execute ", com )
              (ret,err) = exe ( com )
              check_err_exe ( ret, err, com )

              if ( args.verb > 0 ):
                   print ( "gen_sched: ran vex through filter %s " % proj_dict["filter_prog"] )

         vex = read_file ( vex_file )

         for sta in proj_dict["sta_list"]:
             fl_mode = False
             fl_freq = False
             freq_mode = "??"
             num_if = 0
             sampling_rate = 0.0
             fl_this_freq_table = False
             def_freq_counter = 0
             for line in vex:
                 if ( line[0:1] == "*" ): continue
                 if ( len(line.split()) < 1    ): continue
#
# -------------- Check in which section of the vex file we are
#
                 if ( line[0:6] == "$MODE;" ):
                      fl_mode = True
                 elif ( "enddef;" in line ):
                      fl_mode = False
                      fl_this_freq_table = False
                 if ( line[0:6] == "$FREQ;" ):
                      fl_freq = True
                      def_freq_counter = 0
                      continue
                 if ( line[0:1] == "$" and def_freq_counter == 0 ):
                      fl_freq = False
                 if ( fl_mode ):
#
# ------------------- We are in the MODE section. Search for the reference to the 
# ------------------- frequency table
#
                      if ( "$FREQ" in line and "ref" in line ):
#
# ------------------------ Get the station for that reference to the frequency table
# ------------------------ and the associated frequency table name
#
                           freq_sta  = line.split()[3].replace(";","").split(":")[1].lower()
                           if ( sta == freq_sta ):
                                freq_mode = line.split()[3].replace(";","").split(":")[0].lower()
                 if ( fl_freq ):
                      if ( line.split()[0] == "def" ):
                           def_freq_counter = def_freq_counter + 1
                      elif ( "enddef;" in line ) :
                            def_freq_counter = def_freq_counter - 1
                            fl_this_freq_table = False
                 if ( fl_freq and def_freq_counter > 0 and len(line.split()) > 1 ):
#
# ------------------- We are in the FREQ section. Search for the definition
# ------------------- of the frequency table
#
                      if ( line.split()[0] == "def" and \
                           line.split()[1].replace(";","").lower() == freq_mode ):

                           fl_this_freq_table = True
#
# ------------------- We are in the FREQ section. Search for the definition
# ------------------- of the frequency table
#
                      if ( fl_this_freq_table and len(line.split()) > 2 ):
                           if ( line.split()[0].lower() == "chan_def" ):                           
#
# ----------------------------- Well. This line defines the IF. Count it
#
                                num_if = num_if + 1
                           elif ( line.split()[0].lower() == "sample_rate" ):                           
#
# ----------------------------- Well. This line defines the sampling rate.
# ----------------------------- Read it
#
                                sampling_rate = float(line.split()[2])

#
# ---------- Compute the recording rate
#
             if ( sampling_rate > 0.0 and num_if > 0 ):
                  recording_rate = num_if * sampling_rate * 2
             else:
                  recording_rate = default_recording_rate 

             if ( args.verb > 1 ):
                  print ( "gen_sched-444 station ", sta, " sampling_rate: ", sampling_rate, \
                          " num_if: ", num_if, " recording_rate: ", recording_rate )
#
# ---------- Build prc template file name and read it
#
             prc_tmpl_file = sur_sked_prc + "/" + sta + "_template.prc"
             buf_prc_tmpl = read_file ( prc_tmpl_file )
             backend = "??"
#
# ---------- Check whether the prc file defines the backend
# ---------- and send the snap mode accordingly
#
             for line in buf_prc_tmpl:
                 if ( '" Backend:' in line ):
                      backend = line.split()[2]
             if ( backend == "mark5b" ):
                  snap_mode = "mk5_ext"
             else:
                  snap_mode = "mk6_ext"

             snp_file  = exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + sta + ".snp"
             prc_file  = exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + sta + ".prc"
             lst_file  = exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + sta + ".lst"

#
# ---------- Build the command for generation of the snap file
#
             com = "vex_to_snap.py  -m " + snap_mode + " " + \
                                   "-o " + snp_file  + " " + \
                                   vex_file          + " " + \
                                   sta
             if ( args.verb > 1 ):
                  print ( "gen_sched.py: About to execute ", com )
#
# ---------- ... and execute it
#
             (ret,err) = exe_pipe ( com )
             if ( ret != 0 ):
                  for line in err:
                      print ( line ) 
                  print ( "Error in executing command %s" % com ) 
                  exit  ( 1 )
             if ( args.verb > 0 ):
                  print ( "gen_sched: converted vex to snp schedule for station %s" % sta )

#
# ---------- Build the command for generation of the file
# ---------- with precedures related to ths schedule
#
             com = "vex_to_proc.py -o " + prc_file + " " + \
                                   vex_file        + " " + \
                                   sta
             if ( args.verb > 1 ):
                  print ( "gen_sched.py: About to execute ", com )
#
# ---------- ... and execute it
#
             (ret,err) = exe_pipe ( com )
             if ( ret != 0 ):
                  for line in err:
                      print ( line ) 
                  print ( "Error in executing command %s" % com ) 
                  exit  ( 1 )
             if ( args.verb > 0 ):
                  print ( "gen_sched: converted vex to prc schedule for station %s" % sta )

#
# ---------- Build the command for generation of the file
# ---------- with the listing of the scehdule 
#
             com = "snap_to_lst.py -o " + lst_file + " " + \
                                   snp_file        + " " + \
                                   "-s %d" % recording_rate
             if ( args.verb > 1 ):
                  print ( "gen_sched.py: About to execute ", com )
#
# ---------- ... and execute it
#
             (ret,err) = exe_pipe ( com )
             if ( ret != 0 ):
                  for line in err:
                      print ( line ) 
                  print ( "Error in executing command %s" % com ) 
                  exit  ( 1 )
             if ( args.verb > 0 ):
                  print ( "gen_sched: converted snp to lst schedule for station %s" % sta )

#
# ---------- Build the command for checing hte schedule files
#
             com = "check_proc_snap " + prc_file + " " + \
                                        snp_file + " " + \
                                    "0"
             if ( args.verb > 1 ):
                  print ( "gen_sched.py: About to execute ", com )
#
# ---------- ... and execute it
#
             (ret,err) = exe_pipe ( com )
             if ( ret != 0 ):
                  for line in err:
                      print ( line ) 
                  print ( "Error in executing command %s" % com ) 
                  exit  ( 1 )
             if ( args.verb > 0 ):
                  print ( "gen_sched: checked schedule in snap and proc formats for station %s" % sta )
         
         if ( args.verb > 0 ):
              print ( " " )
              print ( "gen_sched: Schedule for %s is READY" % proj_dict["exp"] )

    elif ( args.action == "submit" or args.action == "forced_submit" ):
#
# ------ Build vex and txt filenames
#
         vex_file  = exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + ".vex" 
         txt_file  = exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + ".txt"
         if ( not os.path.isfile ( vex_file ) ):
              print ( "gen_sched: did not find vex file %s" % vex_file )
              exit  ( 1 ) 
         if ( not os.path.isfile ( txt_file ) ):
              print ( "gen_sched: did not find experiment description file %s" % txt_file )
              exit  ( 1 ) 

         for sta in proj_dict["sta_list"]:
#
# ---------- Build file names of schedule files
#
             snp_file  = exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + sta + ".snp"
             prc_file  = exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + sta + ".prc"
#
# ---------- Check schedule files in proc and snap formats
#
             com = "check_proc_snap " + prc_file + " " + \
                                        snp_file + " " + \
                                    "0"
             if ( args.verb > 1 ):
                  print ( "gen_sched.py: About to execute ", com )
             (ret,err) = exe_pipe ( com )
             if ( ret != 0 ):
                  for line in err:
                      print ( line ) 
                  print ( "Error in executing command %s" % com ) 
                  exit  ( 1 )
             if ( args.verb > 0 ):
                  print ( "gen_sched: checked schedule in snap and proc formats for station %s" % sta )

         if ( args.verb > 0 ):
             print ( "gen_sched: schedule has passed the validation test" )

         year = proj_dict["start_date"].year
#
# ------ Submit vex file with schedule to CDDIS
#
         com = vsdc_com + " -t session_aux_vex -f " + vex_file 
         if ( args.verb > 2 ):
              print ( "gen_sched: about to execute %s" % com )
         (ret,err) = exe ( com )
         if ( args.action == "submit" ):
              if ( ret != 0 ):
                   for line in err:
                       print ( line )
                   print ( "Error in executing command %s" % com )
                   print ( "Could not submit vex file %s to CDDIS, but nevertheless, continiue" % vex_file )
              else:
                   if ( args.verb > 0 ):
                        print ( "Suibmitted %s to CDDIS" % vex_file )


#
# ------ Submit vex file with schedule to aw
#
         com = "scp " + vex_file + " aw:/astrogeo/sch/vlbi/vex/"
         if ( args.verb > 2 ):
              print ( "gen_sched: about to execute %s" % com )
         (ret,err) = exe ( com )
         check_err_exe ( ret, err, com )
#
# ------ Submit vex file with schedule to de
#
         com = "scp " + vex_file + " de:/astrogeo/sch/vlbi/vex/"
         if ( args.verb > 2 ):
              print ( "gen_sched: about to execute %s" % com )
         (ret,err) = exe ( com )
         check_err_exe ( ret, err, com )
#
# ------ Submit txt file with schedule description
#
         com = vsdc_com + " -t session_aux_txt -f " + txt_file 
         if ( args.verb > 2 ):
              print ( "gen_sched: about to execute %s" % com )
         (ret,err) = exe ( com )
         if ( args.action == "submit" ):
              if ( ret != 0 ):
                   for line in err:
                       print ( line )
                   print ( "Error in executing command %s" % com )
                   print ( "Could not submit schedule description file %s to CDDIS, but nevertheless, continiue" % txt_file )
              else:
                   if ( args.verb > 0 ):
                        print ( "Suibmitted %s to CDDIS" % txt_file )
#
# ------ Submit vex file with schedule to aw
#
         com = "scp " + txt_file + " aw:/astrogeo/sch/vlbi/txt/"
         if ( args.verb > 2 ):
              print ( "gen_sched: about to execute %s" % com )
         (ret,err) = exe ( com )
         check_err_exe ( ret, err, com )
#
# ------ Submit vex file with schedule to de
#
         com = "scp " + txt_file + " de:/astrogeo/sch/vlbi/txt/"
         if ( args.verb > 2 ):
              print ( "gen_sched: about to execute %s" % com )
         (ret,err) = exe ( com )
         check_err_exe ( ret, err, com )

#
# ------ Now cycle over stations
#
         for sta in proj_dict["sta_list"]:
             snp_file  = exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + sta + ".snp"
             prc_file  = exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + sta + ".prc"
             lst_file  = exp_dir + "/" + proj_dict["exp"] + "/" + proj_dict["exp"] + sta + ".lst"
#
# ---------- Submit the schedule of nasa stations to de
#
             if ( sta in nasa_sta_list ):
                  com = "scp " + snp_file + " de:/astrogeo/sch/vlbi/" + sta + "/" + "%4d" % year
                  if ( args.verb > 2 ):
                       print ( "gen_sched: about to execute %s" % com )
                  (ret,err) = exe ( com )
                  check_err_exe ( ret, err, com )

                  com = "scp " + prc_file + " de:/astrogeo/sch/vlbi/" + sta + "/" + "%4d" % year
                  if ( args.verb > 2 ):
                       print ( "gen_sched: about to execute %s" % com )
                  (ret,err) = exe ( com )
                  check_err_exe ( ret, err, com )

                  com = "scp " + lst_file + " de:/astrogeo/sch/vlbi/" + sta + "/" + "%4d" % year
                  if ( args.verb > 2 ):
                       print ( "gen_sched: about to execute %s" % com )
                  (ret,err) = exe ( com )
                  check_err_exe ( ret, err, com )
#
# ---------- Submit schedule file in snp format to CDDIS
#
             com = vsdc_com + " -t session_aux_snp -f " + snp_file
             if ( args.verb > 2 ):
                  print ( "gen_sched: about to execute %s" % com )
             (ret,err) = exe ( com )
             if ( args.action == "submit" ):
                  if ( ret != 0 ):
                       for line in err:
                           print ( line )
                       print ( "Error in executing command %s" % com )
                       print ( "Could not submit schedule snap file %s to CDDIS, but nevertheless, continiue" % snp_file )
                  else:
                       if ( args.verb > 0 ):
                            print ( "Suibmitted %s to CDDIS" % snp_file )

#
# ---------- Submit schedule file in prc format to CDDIS
#
             com = vsdc_com + " -t session_aux_prc -f " + prc_file
             if ( args.verb > 2 ):
                  print ( "gen_sched: about to execute %s" % com )
             (ret,err) = exe ( com )
             if ( args.action == "submit" ):
                  if ( ret != 0 ):
                       for line in err:
                           print ( line )
                       print ( "Error in executing command %s" % com )
                       print ( "Could not submit schedule procedure file %s to CDDIS, but nevertheless, continiue" % prc_file )
                  else:
                       if ( args.verb > 0 ):
                            print ( "Suibmitted %s to CDDIS" % prc_file )

#
# ---------- Submit the schedule in snp stations to aw
#
             com = "scp " + snp_file + " aw:/astrogeo/sch/vlbi/" + sta + "/" + "%4d" % year
             if ( args.verb > 2 ):
                  print ( "gen_sched: about to execute %s" % com )
             (ret,err) = exe ( com )
             check_err_exe ( ret, err, com )

#
# ---------- Submit the schedule in prc stations to aw
#
             com = "scp " + prc_file + " aw:/astrogeo/sch/vlbi/" + sta + "/" + "%4d" % year
             if ( args.verb > 2 ):
                  print ( "gen_sched: about to execute %s" % com )
             (ret,err) = exe ( com )
             check_err_exe ( ret, err, com )

#
# ---------- Submit the schedule in slst format to aw
#
             com = "scp " + lst_file + " aw:/astrogeo/sch/vlbi/" + sta + "/" + "%4d" % year
             if ( args.verb > 2 ):
                  print ( "gen_sched: about to execute %s" % com )
             (ret,err) = exe ( com )
             check_err_exe ( ret, err, com )

         if ( args.verb > 0 ):
              print ( "gen_sched: Schedule files for %s are submitted" % proj_dict["exp"] )
              
    else:
         print ( "gen_sched: unknown action %s -- only gen or submit are supported" % args.action )


if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        main()
    except KeyboardInterrupt:
        print ( "pf.py: Interrupted" )
        exit ( 1 )
