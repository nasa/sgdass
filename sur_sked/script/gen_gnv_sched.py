#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program gen_gnv_sched.py generates a control file in key format    *
# *   for further processing with NRAO sched program. That control       *
# *   file define the VLBI schedule for GLBA ( stands to GNSS at VLBA    *
# *   sites) and VLBA stations.                                          *
# *                                                                      *
# *   Usage: gen_gnv_sched.py -c generic_definition_file                 *
# *                           -e experiment_name                         *
# *                           -v verbosity                               * 
# *   where                                                              *
# *                           -v verbosity                               * 
# *   generic_definition_file -- defines parameters that are not         *
# *                              experiment secific, such as template    *
# *                              files, directories with ephemerides,    *
# *                              etc.                                    *
# *   experiment_name -- name of the experment.                          *
# *   verbosity -- verbosity level: 0 -- silent, 1 -- normal (default),  *
# *                                 >1 -- debuffing mode.                *
# *                                                                      *
# *   Copyright (c) 1975-2025 United States Government as represented by *
# *   the Administrator of the National Aeronautics and Space            *
# *   Administration. All Rights Reserved.                               *
# *   License: NASA Open Source Software Agreement (NOSA).               *
# *                                                                      *
# * ### 22-SEP-2025 gen_gnv_sched.py v1.0 (d) L. Petrov  06-OCT-2025 ### *
# *                                                                      *
# ************************************************************************
import argparse, signal, sys, os, pwd, math, argparse
from   datetime import datetime, timedelta, tzinfo
from   sur_sked_config import * # Import sur_sked confuguration
from   pet_misc        import *

gen_hrtr_sched__label = "gen_hrtr_sched 20251002"
config_file_magic     = "# HRTR schedule definition file. Version of 2025.09.30"
num_par               = 18

sta_nam_magic = "# STATION-NAMES.  Format version of 2006.01.06"
sou_nam_magic = "# SOURCE-NAMES  v 3.0 2025.09.06"

# Example:
# gen_gnv_sched.py -c /vlbi/gnv/gnv_def.txt -e us008c -v 1


def gen_hrtr_sched ( config_file, exp, ivrb ):
    """
    Main program for generating the schedule for GLBA/VLBA
    """
#
# --- Read the generic configuration file for GLBA/VLBA schedule that
# --- is indepndent from a specific experiment
#
    if ( not os.path.isfile ( config_file ) ):
         print ( "gen_gnv_sched ERROR Cannot find file GLBA/VLBA configurtion file %s" % \
                  config_file )
         exit  ( 1 )

    config = read_file ( config_file )
    if ( not config ):
         print ( "gen_gnv_sched ERROR  Error in reading GLBA/VLBA configuration file file %s" % \
                  config_file )
         exit  ( 1 )
         
#
# --- Check whether the configuiration has the right magic
#
    if ( config[0] != config_file_magic ):
         print ( "gen_gnv_sched ERROR Unrecogned format of hrtr control file %s" % \
                  config_file )
         exit  ( 1 )

#
# --- Parse the configuration file and extract parameters
#
    durs   = [0.0, 0.0]
    n_par = 0
    for line in config:
        if ( line[0:1] == "#"       ): continue
        if ( len(line.split()) >= 2 ):
             if (   line.split()[0] == "vlbi_root:" ):
                    vlbi_root = line.split()[1]
                    n_par = n_par + 1
             elif ( line.split()[0] == "master_file:"   ):
                    master_file = line.split()[1]
                    n_par = n_par + 1
             elif ( line.split()[0] == "skf_tmpl_file:"   ):
                    skf_tmpl_file = line.split()[1]
                    n_par = n_par + 1
             elif ( line.split()[0] == "key_tmpl_file:"   ):
                    key_tmpl_file = line.split()[1]
                    n_par = n_par + 1
             elif ( line.split()[0] == "vex_tmpl_file:"   ):
                    vex_tmpl_file = line.split()[1]
                    n_par = n_par + 1
             elif ( line.split()[0] == "sat_def_file:"   ):
                    sat_def_file = line.split()[1]
                    n_par = n_par + 1
             elif ( line.split()[0] == "sta_def_file:"   ):
                    sta_def_file = line.split()[1]
                    n_par = n_par + 1
             elif ( line.split()[0] == "loc_file:"   ):
                    loc_file = line.split()[1]
                    n_par = n_par + 1
             elif ( line.split()[0] == "freq_file:"   ):
                    freq_file = line.split()[1]
                    n_par = n_par + 1
             elif ( line.split()[0] == "leapsec_file:"   ):
                    leapsec_file = line.split()[1]
                    n_par = n_par + 1
             elif ( line.split()[0] == "tle_dir:"   ):
                    tle_dir = line.split()[1]
                    n_par = n_par + 1
             elif ( line.split()[0] == "margin:"   ):
                    margin = float ( line.split()[1] )
                    n_par = n_par + 1
             elif ( line.split()[0] == "dur_spind_long:"   ):
                    durs[0] = float ( line.split()[1] )
                    n_par = n_par + 1
             elif ( line.split()[0] == "dur_spind_short:"   ):
                    durs[1] = float ( line.split()[1] )
                    n_par = n_par + 1
             elif ( line.split()[0] == "num_sta_exclude:"   ):
                    num_sta_exclude = int ( line.split()[1] )
                    n_par = n_par + 1
             elif ( line.split()[0] == "fl_comp_spind:"   ):
                    if ( line.split()[1].lower() == "false" ):
                         fl_comp_spind = False
                    else:
                         fl_comp_spind = True
                    n_par = n_par + 1
             elif ( line.split()[0] == "sched_exe:" ):
                    sched_exe = line.split()[1]
                    n_par = n_par + 1
             elif ( line.split()[0] == "sched_root:" ):
                    sched_root = line.split()[1]
                    n_par = n_par + 1

    exp_dir = vlbi_root + "/" + exp 
    if ( not os.path.isdir ( exp_dir ) ):
#
# ------ Create experiment directory if it does not exist
#
         try:
            os.mkdir ( exp_dir, mode=0o775 )
         except BaseException as e:
            print ( "gen_gnv_sched ERROR: Attempt to create directory %s -- %s" % \
                    ( exp_dir, e ) )
            exit  ( 1 )
#
# --- Check whether we have defined all parameters
#
    if ( n_par < num_par ):
         print ( "gen_gnv_sched ERROR Only %d out of %d paramters were found in the HRTR configuration file %s" % \
                  ( n_par, num_par, config_file ) )
         exit  ( 1 )

    if ( pyvers >= "3.12" ):
         date_now = datetime.datetime.now(datetime.UTC)
    else:
         date_now = datetime.datetime.utcnow()
    
    mas = read_file ( master_file )
    if ( not mas ):
         print ( "gen_gnv_sched ERROR Cannot found GLBA/VLBA master file %s" % master_file )
         exit  ( 1 )
#
# --- Search for the requested experiment in the master file
#
    exp_name = None
    for line in mas:
        if ( line[0:1] == "#" ): continue
        if ( len(line.split()) < 5 ): continue
        if ( exp == line.split()[0] or exp == line.split()[1] ):
#
# ---------- Extract the experiment name, start and stop time, station and satellite lists
# ---------- as a comma separated string
#
             exp_name       = line.split()[0]
             start_time_str = line.split()[1]
             stop_time_str  = line.split()[2]
             sta_list_str   = line.split()[3]
             sat_list_str   = line.split()[4]

    if ( not exp_name ):
         print ( "gen_gnv_sched ERROR: Experiment %s was not found in the master file %s" % ( exp, master_file ) )
         exit  ( 1 )

#
# --- Transform the comma-separated station string to the station list
#
    sta_list = []
    if ( sta_list_str[0:1] == "@" ):
#
# ------ Special case of @-experesion. That means we need to read this file
# ------ and extrat station names from that file
#
         sta_inc_file = sta_list_str[1:]
         if ( not os.path.isfile ( sta_inc_file ) ):
              print ( "gen_gnv_sched ERROR: Cannot find station file %s" % sta_inc_file )
              exit  ( 1 )
#
# ------ Read the station file
#
         sta_buf = read_file ( sta_inc_file )
         if ( not sta_buf ):
              print ( "gen_gnv_sched ERROR: Cannot read station file %s" % sta_inc_file )
              exit  ( 1 )
         if ( sta_buf[0] != sta_nam_magic ):
              print ( "gen_gnv_sched ERROR: Wrong magic of the station file %s -- expected magic: %s" % \
                      ( sta_inc_file, sta_nam_magic ) )
              exit  ( 1 )
#
# ------ Parse the station definition file
#
         sta_list_str = ""
         for line in sta_buf:
             if ( line[0:1] == "#" ): continue
             if ( len(line.split()) < 2 ): continue
#
# ---------- The station name is taked from the 2nd column
#
             sta_list.append ( line.split()[1] )
             sta_list_str = sta_list_str + "," + line.split()[1]
#
# ------ Create aslo the comma separated station name string. It will be 
# ------ needed in the future
#
         sta_list_str = sta_list_str[1:]
    else:
#
# ------ Just split the comma-separated station name string into words and 
# ------ form the python list
#
         for word in sta_list_str.split(","):
             sta_list.append ( word.upper() )

#
# --- Transform the comma-separated satlellite name string into the satellitelist list
#
    sat_list = []
    if ( sat_list_str[0:1] == "@" ):
#
# ------ Special case of @-experesion. That means we need to read this file
# ------ and extrat satellite names from that file
#
         sat_inc_file = sat_list_str[1:]
         if ( not os.path.isfile ( sat_inc_file ) ):
              print ( "gen_gnv_sched ERROR: Cannot find satellite file %s" % sat_inc_file )
              exit  ( 1 )
         sat_buf = read_file ( sat_inc_file )
         if ( not os.path.isfile ( sat_inc_file ) ):
              print ( "gen_gnv_sched ERROR: Cannot read satellite file %s" % sat_inc_file )
              exit  ( 1 )
         if ( sat_buf[0] != sou_nam_magic ):
              print ( "gen_gnv_sched ERROR: Wrong magic of the satellite file %s -- expected magic: %s" % \
                      ( sta_inc_file, sta_nam_magic ) )
              exit  ( 1 )
#
# ------ Parse the buffer with the satellite file
#
         for line in sat_buf:
             if ( line[0:1] == "#" ): continue
             if ( len(line.split()) < 3 ): continue
             sat_list.append ( line.split()[0] )
         sat_buf = None
    else:
#
# ------ Just split the comma-separated satellite name string into words and 
# ------ form the python list
#
         if ( sat_list_str != "NO" ):
              for word in sat_list_str.split(","):
                  sat_list.append ( word.upper() )
         else:
              sat_list = []

#
# --- Generate names of file related to schedule
#
    sou_sec_file = ["", ""]
    skf_file        = exp_dir + "/" + exp_name + ".skf"  # sur_sked control file
    key_header_file = skf_file.replace(".skf","_key_header.txt")            # a header of a schedule in key format
    vex_header_file = skf_file.replace(".skf","_vex_header.txt")            # a header of a schedule in vex format
    skf_log_file    = skf_file.replace(".skf","_skf.log")                   # sur_sked log file
    sou_sec_file[0] = skf_file.replace(".skf","_%02dsec.spind" % durs[0] )  # pseudo-source file with a coarse time resolution
    sou_sec_file[1] = skf_file.replace(".skf","_%02dsec.spind" % durs[1] )  # pseudo-source file with a fine   time resolution
    orig_key_file   = skf_file.replace(".skf",".key")                       # postprocessed output schedule file in key format
    orig_vex_file   = skf_file.replace(".skf",".vex")                       # postprocessed output schedule file in vex format
    plan_file       = skf_file.replace(".skf",".plan")                      # schedule file in plan format
    ast_file        = skf_file.replace(".skf",".ast")                       # schedule file in ast format
    stat_file       = skf_file.replace(".skf",".stat")                      # statistics of the schedule
    sou_lis_file    = skf_file.replace(".skf",".sou_lis")                   # list of scheduled satellites
    tmp_key_file    = "/tmp/" + exp_name + ".key"                           # original output schedule file in key format
    tmp_vex_file    = "/tmp/" + exp_name + ".vex"                           # original output schedule file in vex format


    if ( sat_list == [] ):
#
# ------ Run sur_sked program that generates the schdule
#
         com = "sur_sked %s 6 > %s" % ( skf_file, skf_log_file )
         if ( ivrb >= 1 ):
              print ( "About to execute command", com, flush=True ) 
    
         (ret,err) = exe ( com )
         if ( ret != 0 ):
              for line in err: 
                  print ( line )
              print ( "gen_gnv_sched ERROR: in generating schedule file with program sur_sked" )
              print ( "gen_gnv_sched ERROR: Failed command: %s" % com )
              exit  ( 1 )
         else:
              print ( "gen_gnv_sched INFO: successfully generted a schedule for AGN observations" )
              exit  ( 0 ) 
              
#
# --- Check output files. Remove them, if they exist
#
    if ( os.path.isfile ( orig_key_file ) ):
         os.unlink ( orig_key_file )
    
    if ( os.path.isfile ( orig_vex_file ) ):
         os.unlink ( orig_vex_file )
    
    if ( os.path.isfile ( tmp_key_file ) ):
         os.unlink ( tmp_key_file )
    
    if ( os.path.isfile ( tmp_vex_file ) ):
         os.unlink ( tmp_vex_file )
    
    if ( os.path.isfile ( skf_file ) ):
         os.unlink ( skf_file )
    
    if ( os.path.isfile ( plan_file ) ):
         os.unlink ( plan_file )
    
    if ( os.path.isfile ( stat_file ) ):
         os.unlink ( stat_file )
    
    if ( os.path.isfile ( ast_file ) ):
         os.unlink ( ast_file )

    if ( os.path.isfile ( skf_log_file ) ):
         os.unlink ( skf_log_file )

    if ( os.path.isfile ( sou_lis_file ) ):
         os.unlink ( sou_lis_file )

#
# --- Create export directory if does not exist, otherwise clean it
#
    export_dir = exp_dir + "/export" 
    if ( not os.path.isdir ( export_dir ) ):
#
# ------ Create export directory
#
         try:
            os.mkdir ( export_dir, mode=0o775 )
         except BaseException as e:
            print ( "gen_gnv_sched ERROR: Attempt to create directory %s -- %s" % \
                    ( export_dir, e ) )
            exit  ( 1 )
    else:
#
# ------ Remove files from the export directory if they exist
#
         com = "rm " + export_dir + "/* "
         (ret,err) = exe ( com )    

#
# --- Create schedile directory if does not exist, otherwise clean it
#
    sched_dir = exp_dir + "/sched" 
    if ( not os.path.isdir ( sched_dir ) ):
#
# ------ Create export directory
#
         try:
            os.mkdir ( sched_dir, mode=0o775 )
         except BaseException as e:
            print ( "gen_gnv_sched ERROR: Attempt to create directory %s -- %s" % \
                    ( sched_dir, e ) )
            exit  ( 1 )
    else:
#
# ------ Remove files from the export directory if they exist
#
         com = "rm " + sched_dir + "/* "
         (ret,err) = exe ( com )    

#
# --- Create the satellite ephemeride directory for this expoeriment if does not exist, 
# --- otherwise clean it
#
    exp_tle_dir = exp_dir + "/tle" 
    if ( not os.path.isdir ( exp_tle_dir ) ):
         try:
            os.mkdir ( exp_tle_dir, mode=0o775 )
         except BaseException as e:
            print ( "gen_gnv_sched ERROR: in an attempt to create directory %s -- %s" % \
                    ( exp_tle_dir, e ) )
            exit  ( 1 )
    else:
         if ( fl_comp_spind ):
#
# ----------- Remove files from the ephemeride directory if they exist
#
              com = "rm " + exp_tle_dir + "/*"
              if ( ivrb >= 2 ):
                   print ( "About to execute command %s" % com, flush=True ) 
              (ret,err) = exe ( com )

    if ( fl_comp_spind ):
#
# ------ Remove source files with both resolutions if we are going
# ------ to (re)created them
#
         if ( os.path.isfile ( sou_sec_file[0] ) ):
              os.unlink ( sou_sec_file[0] )
         if ( os.path.isfile ( sou_sec_file[1] ) ):
              os.unlink ( sou_sec_file[1] )
              
#
# --- Red the satellute definition file
#
    sat_def_buf = read_file (  sat_def_file)
    if ( sat_def_buf == None ):
         print ( "gen_gnv_sched ERROR: failre in reading satellite definition file %s" % sat_def_file )
         exit  ( 1 )

#
# --- Read the schedule template file and generate the actual sur_sked control file
#
    skf = read_file ( skf_tmpl_file )
    if ( not skf ):
         print ( "gen_gnv_sched ERROR: failre in reading a template of the schedule control file %s" % skf_tmpl_file )
         exit  ( 1 )
    out = []
    for line in skf:
#
# ----- Replace some lines in the template of the sur_sked control file
#
        line = line.replace("@vlbi_dir@",vlbi_root). \
                    replace("@exp_name@",exp_name)
        if ( "START_TIME:" in line ):
              line = line.replace("@start_time@",start_time_str)
        if ( "STOP_TIME:" in line ):
              line = line.replace("@stop_time@",stop_time_str)
        if ( "STATIONS:" in line ):
              line = line.replace("@sta_list@",sta_list_str)
              sta_1st = line.split()[1].split(",")[0]
              line = line.replace(sta_1st,sta_1st+":r")
              num_sta = len(line.split()[1].split(","))
        if ( "OUT_VEX:" in line ):
              line = line.replace("@exp_name@",exp_name)
              tmp_vex_file = line.split()[1]
        if ( "OUT_KEY:" in line ):
              line = line.replace("@exp_name@",exp_name)
              tmp_key_file = line.split()[1]
        out.append ( line ) 

#
# --- Write the schedule control file for sur_sked that was made from the template
#
    (ret,err) = write_file ( out, skf_file )
    check_err_exe ( ret, out, "write_file" )

#
# --- Parse experiment start and stop time 
#
    try:
         tim_start = datetime.datetime.strptime ( start_time_str, '%Y.%m.%d_%H:%M:%S' ) 
    except BaseException as e:
         print ( "gen_gnv_sched ERROR: in parsing start time string %s -- %s" % ( start_time_str, e ) )
         exit  ( 1 )
        
    try:
         tim_stop  = datetime.datetime.strptime ( stop_time_str,  '%Y.%m.%d_%H:%M:%S' ) 
    except BaseException as e:
         print ( "gen_gnv_sched ERROR: in parsing stop  time string %s -- %s" % ( stop_time_str, e ) )
         exit  ( 1 )
    
#
# --- Compute start and stop times for the source file by applying margin to
# --- the experiment start and stop times
#
    date_tle_start = datetime.datetime.strftime ( tim_start - datetime.timedelta(seconds=margin), '%Y.%m.%d_%H:%M:%S' ) 
    date_tle_stop  = datetime.datetime.strftime ( tim_stop  + datetime.timedelta(seconds=margin), '%Y.%m.%d_%H:%M:%S' ) 
    date_tle_mid   = datetime.datetime.strftime ( tim_start + (tim_stop - tim_start)/2.0, '%Y%m%d_%H%M%S' ) 
    
#
# --- Learn VTD shared directory
#
    (ret,out) = exe ( "vtd_inq --data" )
    check_err_exe ( ret, out, "vtd_inq --data" )
    vtd_data_dir = out[0]
    
#
# --- Build the GNSS satellite file name
#
    gnss_table_fil = vtd_data_dir + "/gnss_sat_table.txt"
    if ( not os.path.isfile ( gnss_table_fil ) ):
         print ( "gen_gnv_sched ERROR: cannot find the GNSS satellite file %s" % gnss_table_fil )
         exit  ( 1 )
    
#
# --- Read the GNSS satellite table file name
#
    gnss_table_buf = read_file ( gnss_table_fil )
    if ( not gnss_table_buf ):
         print ( "gen_gnv_sched ERROR: in reading the GNSS satellite file %s" % gnss_table_fil )
         exit  ( 1 )
    
#
# --- Check all satellite names againts the GNSS satellite table
# --- and extract 
# --- 1) satellite name (3 characters long for GNSS satellites)
# --- 2) norad id
# --- 3) selestrac satellite ID
#
    sat_name_list  = []
    sat_norad_list = []
    sat_celid_list = []
#
# --- Process the saetllite definition file
#
    for sat in sat_list:
#
# ----- search for the satellite name in the satellite defintion file 
#
        sat_celid = None
        norad_id  = None
        for line in sat_def_buf:
            if ( len(line.split()) == 0 ): continue
            if ( len(line.split()) >= 5 and len(line.split()[0]) == 3 ):
                 if ( line.split()[0] == sat ):
#
# ------------------- Aga! found. Append satellite name and its norad id
#
                      sat_name_list.append  ( sat )
                      norad_id = line.split()[1] 
                      sat_norad_list.append ( norad_id  )
        if ( norad_id == None ):
             print ( "gen_gnv_sched ERROR: Did not find norad id for satellite %s in %s" % \
                     ( sat, sat_def_file ) )
             exit  ( 1 )
#
# ----- Search for the source in the GNSS defitinaion table
#
        for lin in gnss_table_buf:
            if ( lin.split()[0] == sat ):
#
# -------------- Ara: we found the celestrak id for sat!
#
                 sat_celid = lin.split()[2]
                 if ( sat_celid == None ):
                      print ( "gen_gnv_sched ERROR: Did not find celestrak id for satellite %s NORAD id %s in %s" % \
                               ( sat, norad_id, line.split()[1], gnss_table_fil ) )
                      exit  ( 1 )
                 sat_celid_list.append ( sat_celid )

#
# --- We are going to check all GNSS ephemeeride files.
# --- Since we have a lot of these files, we would like to limit
# --- the search to this and two previous months.
# --- Let us create strings for this and two previous months
#
    this_month = "/%4d/%02d" % ( date_now.year, date_now.month )
    if ( date_now.month > 1 ):
         prev_month = "/%4d/%02d" % ( date_now.year, date_now.month - 1)
    else:
         prev_month = "/%4d/%02d" % ( date_now.year - 1, 12 )

    if ( date_now.month > 2 ):
         prev_prev_month = "/%4d/%02d" % ( date_now.year, date_now.month - 2)
    else:
         if ( date_now.month == 2 ):
              prev_prev_month = "/%4d/%02d" % ( date_now.year - 1, 12 )
         else:
              prev_prev_month = "/%4d/%02d" % ( date_now.year - 1, 11 )

#
# --- Copy the freshiest ephemeride to the experiment directory
#
    exp_tle_dir = exp_dir + "/tle"
    
#
# --- Walk through the directry with satellite epehemerid in TLE format
# --- and put the filenames to tle_file_list that correspond to 
# --- ephemerids for this and tow previous months
#
    tle_file_list = []
    for path, dirs, files in os.walk(tle_dir):
        for file in files:
            if ( len(file) < 5 ): continue
            if ( ".tle" == file[len(file)-4:] and 
                 ( this_month      in path or \
                   prev_month      in path or \
                   prev_prev_month in path    ) ): 
                 tle_file_list.append ( path + "/" + file )
    
#
# --- Sort this list
#
    tle_file_list.sort()
    
#
# --- Now check this list and find for each source the latest 
# --- ephemeride file
#
    tle_exp_list = []  # This is the final list of ephemerid file
#
# --- Cycle over satellites (in a form of Celestrak id)
#
    for sat in sat_celid_list:
        age_min = datetime.timedelta ( days=99999 )
        tle_file = None
#
# ----- Cucle over ephemerid files
#
        for fil in tle_file_list:
            id = fil.rfind("/")
            iu = fil[id+1:].find("_")
            il = len(fil)
#
# --------- Get Celestrak satellite id from the file name
#
            sat_cel_id = fil[id+1:id+iu+1]
#
# --------- Get the tle date from the file name 
#
            tle_date   = fil[id+iu+2:il-4] 
            date_fil = datetime.datetime.strptime ( tle_date, '%Y%m%d_%H%M%S' ) 
           
            if ( sat_cel_id == sat ):
                 if ( (tim_start + (tim_stop - tim_start)/2.0 - date_fil) < age_min ):
#
# ------------------- This file has the date closet to the observation date
# ------------------- Put its name to tle_file and store its age with respect
# ------------------- to the experiment midle date
#
                      tle_file = fil
                      age_min = (tim_start + (tim_stop - tim_start)/2.0 - date_fil) 
        if ( not tle_file ):
#
# ---------- That means we did not enounter any ephemeride file for 
# ---------- this satellite
# ---------- Prepare a descriptive error message
#
             sat_name = sat
             for line in gnss_table_buf:
                 if ( line[0:1] == '#' ): continue
                 if ( len(line.split()) < 3 ): continue
                 if ( sat == line.split()[2] ):
                      sat_name = line.split()[0] 
             print ( "gen_gnv_sched ERROR: Did not find ephemeride for satellite %s (aka %s) in %s" % \
                     ( sat_name, sat, tle_dir ) )
             exit  ( 1 )
    
#
# ----- Put this ephemeride file to tle_exp_list
#
        tle_exp_list.append ( tle_file )
    
#
# --- Create pseudo-source files for this experiments
# --- the first  file in a coarse time resolution
# --- the second file in a fine   time resolution
#
    for i in range(0,2):
#
# ----- Build the name of the pseudo-source file
#
        spind_file = exp_dir + "/" + exp + "_%dsec.spind" % int(durs[i])
             
        if ( fl_comp_spind ):
#
# ---------- Run the command that creates the pseud-source file
#
             com = "tle_to_spind " + ",".join(sat_name_list) + " " + \
                    date_tle_start + " " + \
                    date_tle_stop  + " " + \
                    " %4.1f" % durs[i]  + \
                    " %4.1f" % durs[i]  + \
                    " %d " % (len(sta_list) - num_sta_exclude) + \
                    spind_file 
    
             if ( ivrb >= 1 ):
                  print ( "About to execute command %s" % com, flush=True ) 
             (ret,err) = exe_pipe ( com )
             if ( ret != 0 ):
                  for line in err: 
                      print ( line )
                  print ( "gen_gnv_sched ERROR: in computing satellite right ascensions and declinations" )
                  print ( "gen_gnv_sched ERROR: Failed command: %s" % com )
                  exit  ( 1 )
    
#
# ---------- Read the satellite defintion section in the pseudo-source file
#
             spind_buf = read_file ( spind_file )
             for i in range(0,len(spind_buf)):
                 if ( spind_buf[i][0:12] == "# Satellite:" ):
                      id = spind_buf[i].split()[5].rfind("/")
#
# ------------------- Extract the old satellte ephemeride file name and 
# ------------------- make the new satellte ephemeride fiole name by prepending the path
#
                      old_file = spind_buf[i].split()[5]
                      new_file = exp_tle_dir + "/" + spind_buf[i].split()[5][id:]
    
#
# ------------------- Copy the ephemerid file to the experiment-specific 
# ------------------- satellite ephemeride directory
#
                      com = "cp %s %s/" % ( old_file, exp_tle_dir )
                      if ( ivrb >= 3 ):
                           print ( "About to execute command %s" % com, flush=True ) 
                      (ret,err) = exe ( com )
                      if ( ret != 0 ):
                           for line in err: 
                               print ( line )
                               print ( "gen_gnv_sched ERROR: in copying satellite ephemeride" )
                           print ( "gen_gnv_sched ERROR: in executing command %s" % com )
                           exit  ( 1 )
    
#
# ------------------- And replace the satellte ephemeride file name in the 
# ------------------- pseudo-source file
#
                      spind_buf[i] = spind_buf[i].replace ( old_file, new_file )
#
# ---------- Write update pseudo-source file
#
             (ret,err) = write_file ( spind_buf, spind_file )
             if ( ret != 0 ):
                  print ( "gen_gnv_sched ERROR: Error in writing source file %s" % spind_file )
             check_err_exe ( ret, err, "write_file" )
        else:
             if ( ivrb >= 1 ):
                  print ( "Re-use the old source file %s" % spind_file, flush=True )
         
#
# --- Run sur_sked program that generates the schdu;e
#
    com = "sur_sked %s 6 > %s" % ( skf_file, skf_log_file )
    if ( ivrb >= 1 ):
         print ( "About to execute command", com, flush=True ) 
    
    (ret,err) = exe ( com )
    if ( ret != 0 ):
         for line in err: 
             print ( line )
         print ( "gen_gnv_sched ERROR: in generating schedule file with program sur_sked" )
         print ( "gen_gnv_sched ERROR: Failed command: %s" % com )
         exit  ( 1 )
    
#
# --- Run the post-processing program the refine the schedule.
# --- It writes final schedules with names key_file and vex_ffile
#
    spind_1sec_file = exp_dir + "/" + exp + "_%dsec.spind" % int(durs[1])
    tmp_key_file    = "/tmp/" + exp + ".key"
    tmp_vex_file    = "/tmp/" + exp + ".vex"
    key_file        = exp_dir + "/" + exp + ".key"
    vex_file        = exp_dir + "/" + exp + ".vex"
    
    com = "refine_gnss_key %s %s key_tle %s %s %s %s" % \
          (tmp_key_file, tmp_vex_file, sta_list[0], spind_1sec_file, key_file, vex_file)
    if ( ivrb >= 1 ):
         print ( "About to execute command", com, flush=True ) 
    (ret,err) = exe ( com )
    if ( ret != 0 ):
         for line in err: 
             print ( line )
         print ( "gen_gnv_sched ERROR: in refuning the schedule file with program refine_gnss_key" )
         print ( "gen_gnv_sched ERROR: Failed command: %s" % com )
         exit  ( 1 )
    
#
# --- Now we create export key file. It will have not full path names
#
    id = leapsec_file.rfind("/")
    base_leapsec_file = leapsec_file[id+1:]

    id = sta_def_file.rfind("/")
    base_sta_def_file = sta_def_file[id+1:]

    id = loc_file.rfind("/")
    base_loc_file = loc_file[id+1:]

    id = freq_file.rfind("/")
    base_freq_file = freq_file[id+1:]

    id = key_file.rfind("/")
    base_key_file = key_file[id+1:]

    export_key_file = export_dir + "/" + base_key_file
#
# --- Run the program that makes subsitutions in the key file and write it to
# --- export_key_file
#
    com = 'cat ' + orig_key_file + ' | ' + \
          'sed "s@tlefile= ' + vlbi_root + '/' + exp_name + '/tle/@tlefile= @g" | ' + \
          'sed "s@kerfile = ' + leapsec_file + '@kerfile = ' + base_leapsec_file + '@g" | ' + \
          'sed ' + "'" + 's@stafile  = "' + sta_def_file  + '"@stafile  = ' + base_sta_def_file  + '@g' + "' | " + \
          'sed ' + "'" + 's@locfile  = "' + loc_file  + '"@locfile  = ' + base_loc_file  + '@g' + "' | " + \
          'sed ' + "'" + 's@freqfile = "' + freq_file + '"@freqfile = ' + base_freq_file + '@g' + "' > " + \
          export_key_file
    
    if ( ivrb >= 2 ):
         print ( "About to execute command ", com, flush=True )
    (ret,err) = exe ( com )
    if ( ret != 0 ):
         for line in err:
             print ( line )
         print ( "gen_gnv_sched ERROR in an attempt to execute command %s" % com )
         exit ( 1 )
    
#
# --- Copy ephemeride files in TLE format to the export durectory
#
    com = "cp " + exp_tle_dir + "/* " + export_dir + "/"
    (ret,err) = exe ( com )
    if ( ret != 0 ):
         for line in err:
             print ( line )
         print ( "gen_gnv_sched ERROR in an attempt to execute command %s" % com )
         exit ( 1 )
    
#
# --- Copy the station defintion file to the export durectory
#
    com = "cp " + sta_def_file + " " + export_dir + "/"
    (ret,err) = exe ( com )
    if ( ret != 0 ):
         for line in err:
             print ( line )
         print ( "gen_gnv_sched ERROR in an attempt to execute command %s" % com )
         exit ( 1 )

#
# --- Copy the station location file to the export durectory
#
    com = "cp " + loc_file + " " + export_dir + "/"
    (ret,err) = exe ( com )
    if ( ret != 0 ):
         for line in err:
             print ( line )
         print ( "gen_gnv_sched ERROR in an attempt to execute command %s" % com )
         exit ( 1 )

#
# --- Copy the frequency definition file to the export durectory
#
    com = "cp " + freq_file + " " + export_dir + "/"
    (ret,err) = exe ( com )
    if ( ret != 0 ):
         for line in err:
             print ( line )
         print ( "igen_gnv_sched ERROR n an attempt to execute command %s" % com )
         exit ( 1 )

#
# --- Copy the leapsecond file to the export durectory
#
    com = "cp " + leapsec_file + " " + export_dir + "/"
    (ret,err) = exe ( com )
    if ( ret != 0 ):
         for line in err:
             print ( line )
         print ( "gen_gnv_sched ERROR in an attempt to execute command %s" % com )
         exit ( 1 )

#
# --- Create csh command file to run schded as a final test of the schdule
#
    sched_file = exp_dir + "/" + exp + "_run_sched.csh"
    
    csh_buf=[]
    csh_buf.append ( "#!/bin/csh -f" )
    csh_buf.append ( "cd           " + export_dir )
    csh_buf.append ( "setenv SCHED " + sched_root )
    csh_buf.append ( sched_exe + " <<EOF" )
    csh_buf.append ( "SCH=" + export_dir + "/" + base_key_file )
    csh_buf.append ( "/" )
    csh_buf.append ( "EOF" )
    csh_buf.append ( "if ( $status != 0 ) exit ( $status )" )
    csh_buf.append ( "mv " + export_dir + "/sched.runlog "       + sched_dir  )
    csh_buf.append ( "mv " + export_dir + "/" + exp + "*      "  + sched_dir  )
    csh_buf.append ( "cp " + sched_dir  + "/" + exp + ".key    " + export_dir )

    (ret,out) = write_file ( csh_buf, sched_file )
    check_err_exe ( ret, out, "write_file" )
    
#
# --- Run sched program
#
    com = "csh " + sched_file 
    if ( ivrb >= 2 ):
         print ( "About to execute command ", com, flush=True )
    (ret,err) = exe_pipe ( com )
    if ( ret != 0 ):
         for line in err: 
             print ( line )
         print ( "Error in an attempt to process the schedule with sched" )
         print ( "Failed command: %s" % com )
         exit  ( 1 )
    
#
# --- Well, print the success note
#
    date_now = datetime.datetime.now()

    print ( "gen_gnv_sched INFO: successully generated export schedule files %s and %s at %s" % \
             ( key_file, vex_file, date_now.strftime("%Y.%m.%d_%H:%M:%S") ) )

    return ( 0 )
        
#
# ------------------------------------------------------------------------
#
def main():
    """
    Main program for generation of a schdule file for a GLBA/VLBA vlbi experiment
    Parse optints and generate the schedule
    """
    parser = argparse.ArgumentParser ( description=gen_hrtr_sched__label )
    parser.add_argument('--version', action='version', version=gen_hrtr_sched__label )

    parser.add_argument ( "-c", "--config", \
                      action="store", \
                      dest="config_file", \
                      metavar="value", \
                      required=True,  \
                      help="Configuration file" )

    parser.add_argument ( "-e", "--experiment", \
                      action="store", \
                      dest="exp", \
                      metavar="value", \
                      required=True,  \
                      help="Experiment name" )

    parser.add_argument ( "-v", "--verbosity", \
                      action="store", \
                      dest="ivrb", \
                      default=1, \
                      type=int, \
                      metavar="value", \
                      help="Verbosity level" )

#
# --- Get and parse options
#
    args = parser.parse_args()

    ret = gen_hrtr_sched ( args.config_file, args.exp.lower(), args.ivrb )
    exit ( ret )

if __name__ == "__main__":
    pyvers = "%d.%02d" % ( sys.version_info.major, sys.version_info.minor ) # python version
    if ( pyvers < "03.02" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    main()
