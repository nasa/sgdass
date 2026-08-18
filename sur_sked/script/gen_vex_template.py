#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Generation of VLBI schedule consists of two parts:                 *
# *   1) astrometric schedule: defining the sequence of operations       *
# *      (a) slewing to a target source;                                 *
# *      (b) execution of pre-observation procedure(s);                  *
# *      (c) recording level 0 voltage data;                             *
# *      (d) execution of post-observation procedure(s)                  *
# *   2) radio-technical setup that produces commands to VLBI hardware   *
# *      to setup observing mode(s).                                     *
# *                                                                      *
# *   Program gen_vex_template.py generates the radio-technical setup     *
# *   part of the schedule in a form of a an output template file in     *
# *   vex format using a lower input template inf vex format.            *
# *   It is assumed that all stations are operated under the NVI         *
# *   Field System.                                                      *
# *                                                                      *
# *   A file with vex template contains @-expression that are processed  *
# *   and substituted. Both input and output templates have              *
# *   @-expressions. Program gen_vex_template.py processes a number of   *
# *   @-expressions in accordance with input parameters and produces the *
# *   output template file that is ready for processing by programs      *
# *   sur_sked or VieSched++.                                            *
# *                                                                      *
# *   usage: gen_vex_template.py                                         *
# *     [--version]                                                      *
# *     -e exper                                                         *
# *     -sl sta_list                                                     *   
# *     -m modes_str                                                     *
# *     -p proto_file_tmpl                                               *
# *     -d sched_dir                                                     *
# *     -f frq_dir                                                       *
# *     -st stp_dir                                                      *
# *     -si sta_info                                                     *
# *     -sd sta_desc                                                     *
# *     -sp sta_pos                                                      *
# *     -sv sta_vel                                                      *
# *     [-v verbosity]                                                   *
# *     [--version]                                                      *
# *                                                                      *
# *     where                                                            *
# *                                                                      *
# *     exper           -- experiment name                               *
# *     sta_list        -- comma separated list of two-letter ling       *
# *                        station names.                                *
# *     modes_str       -- string with comma-separated modes. Each       *
# *                        mode may follow by the hardware setup         *
# *                        separated by colon. If omitted, default       *
# *                        hardware setup h1 is used.                    *
# *     proto_file_tmpl -- input vex template file.                      *
# *     sched_dir       -- directory where the subdirectory with         *
# *                        schedule files is located. The output         *
# *                        vex template file and other auxiliary files   *
# *                        will be written to that directory.            *
# *     frq_dir         -- directory with the frequency definition and   *
# *                        directory with frequency sequence files.      *
# *     stp_dir         -- directory with station parameter files.       *
# *     sta_info        -- file with station information file            *
# *     sta_desc        -- station description file                      *
# *     sta_pos         -- station position file                         *
# *     sta_vel         -- station velocity file                         *
# *     verbosity       -- verbosity parameter.                          *
# *                        0 -- silent mode.                             *
# *                        1 -- moderate verbosity                       *
# *                       >1 -- debugging verbosity.                      *
# *                                                                      *
# *   Copyright (c) 1975-2025 United States Government as represented by *
# *   the Administrator of the National Aeronautics and Space            *
# *   Administration. All Rights Reserved.                               *
# *   License: NASA Open Source Software Agreement (NOSA).               *
# *                                                                      *
# * ## 21-JAN-2026  gen_vex_template.py v1.0 (d) L. Petrov 27-JAN-2026 # *
# *                                                                      *
# ************************************************************************
import argparse, signal, sys, os, pwd, math, shutil
from   datetime import datetime, timedelta, tzinfo
from   sur_sked_config import * # Import sur_sked confuguration
from   pet_misc        import *

gen_vex_template__label = "gen_camp_schedule.py  20260127"

#
# --- block @-experssions
#
block_word_list = [                     \
                    "@MODE_BLOCK_BEG@", \
                    "@MODE_BLOCK_END@", \
                    "@STA_BLOCK_BEG@",  \
                    "@STA_BLOCK_END@"   \
                  ]

#
# --- list of 7 public field system procedures
#
pub_proc_list = [              \
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
# ------------------------------------------------------------------------
#
def gen_vex_template ( exper,           \
                       sta_list_str,    \
                       modes_str,       \
                       proto_tmpl_file, \
                       sched_dir,       \
                       frq_dir,         \
                       stp_dir,         \
                       sta_info_file,   \
                       sta_desc_file,   \
                       sta_pos_file,    \
                       sta_vel_file,    \
                       ivrb             ):
    """
    Main routine that processes input schedule template and generates
    the output schedule template
    """

#
# --- Check whether input directory exists
#
    if ( not os.path.isdir(sched_dir) ):
         print ( "%s: schedule directory %s does not exist" % \
                 ( where_error(), sched_dir) )
         exit  ( 1 )

    if ( not os.path.isdir(frq_dir) ):
         print ( "%s: frequency directory %s does not exist" % \
                 ( where_error(), frq_dir) )
         exit  ( 1 )
 
    if ( not os.path.isdir(stp_dir) ):
         print ( "%s: station parameter directory %s does not exist" % \
                 ( where_error(), stp_dir) )
         exit  ( 1 )
 
#
# --- Check whether the experiment directiry exists. If does not, create it
#
    exper_dir = sched_dir + "/" + exper
    if ( not os.path.isdir(exper_dir) ):
         try:
             os.mkdir ( exper_dir, mode=0o775 ) 
         except BaseException as e:
             print ( "%s in attempt to create experiment directory %s -- %s" % \
                      ( where_error(), exper_dir, e) )
 
#
# --- Check whether input files exists
#
    if ( not os.path.isfile(sta_info_file) ):
         print ( "%s cannot find station informaiton file %s" % ( where_error(), sta_info_file ) )
         exit  ( 1 )

    if ( not os.path.isfile(sta_desc_file) ):
         print ( "%s cannot find station description file %s" % ( where_error(), sta_desc_file ) )
         exit  ( 1 )

    if ( not os.path.isfile(sta_pos_file) ):
         print ( "%s cannot find station position file %s" % ( where_error(), sta_pos_file ) )
         exit  ( 1 )

    if ( not os.path.isfile(sta_vel_file) ):
         print ( "%s cannot find station velocity file %s" % ( where_error(), sta_vel_file ) )
         exit  ( 1 )

#
# --- make the list of modes, list of stations and determine the hardware setup line
#
    modes_list = modes_str.split(",")
    if ( ":" in modes_list ):
         hds_line = modes_list.split(":")[1] 
    else:
         hds_line = hds_default
    sta_list  = sta_list_str.lower().split(",")

#
# --- Initializations
#
    mode_dict = {}
    sta_dict={}
    hds_list  = []
    seq_list  = []

    for sta in sta_list:
        sta_dict[sta] = {"hds": None, "prc": None, "mode": [] }

#
# --- Cycle over modes
#
    for word in modes_str.split(","):
#
# ----- Extract the mode name form the modes string
# ----- and extract hardware setup lkne
#
        mode_name = word.split(":")[0] 
        if ( ":" in word ):
             hds_line = word.split(":")[1] 
        else:
             hds_line = hds_default
#
        hds_lst = []
        seq_lst = []
        seq_file_lst = []
        for seq_name in hds_line.split("+"):
#
# --------- Determine the frequency sequence file
#
            seq_file = frq_dir + "/" + mode_name + "_" + seq_name + ".seq"
#
# --------- Check whether the frequency sequence file exist
#
            if ( not os.path.isfile ( seq_file ) ):
#
# -------------- Try the version of the sequence file without hardware setup suffix
#
                 seq_file_nohds = frq_dir + "/" + mode_name + ".seq"
                 if ( os.path.isfile ( seq_file_nohds ) ):
                      seq_file = seq_file_nohds 
                 else:
                      print ( "%s: cannot find sequence file %s" % ( where_error(), seq_file) )
                      exit  ( 1 )
#   
# --------- Find HDS_NAME and the list if stations in the sequence file
#   
            seq = read_file ( seq_file )
            hds_name = None
            for lin in seq:
                if ( len(lin.split()) > 1 ):
                     if ( lin.split()[0] == "HDS_NAME" ):
                          hds_name = lin.split()[1]
                     if ( lin.split()[0] == "STATION" ):
                          sta = lin.split()[2].lower()
                          if ( sta in sta_dict.keys() ): 
                               sta_dict[sta]["hds"] = hds_name
            if ( hds_name == None ):
                 print ( "%s: cannot find HDS_MAME in sequence file %s" % ( where_error(), seq_file) )
                 exit  ( 1 )
        
#
# --------- Append the sequence file list, the sequence name list and the harware name lists
#
            seq_file_lst.append ( seq_file )
            seq_lst.append ( seq_name )
            hds_lst.append ( hds_name )

#
# --------- Add this mode to the mode_dict dictionary
#
            mode_dict[mode_name] = {"seq" : seq_lst, "seq_file" : seq_file_lst, "hds" : hds_lst }

#
# --------- Update the list of hardware setups hds_list
#
            for hds in hds_lst:
                if ( not hds in hds_list ):
                     hds_list.append ( hds )

#
# ----- Add this mode ti the station dictionary for all stations
#
        for sta in sta_dict.keys():
             sta_dict[sta]["mode"].append ( mode_name )

#
# --- put the name of output procedure files ijnto the station dictonary
#
    for sta in sta_dict.keys():
        sta_dict[sta]["prc"] = sched_dir + "/" + exper + "/" + "+".join(sta_dict[sta]["mode"]) + "_" + sta + ".prc"

    
    if ( ivrb >=2 ):
         print ( where_info(), "mode_dict= ",  mode_dict )
         print ( where_info(), "hds_list=  ",  hds_list  )
         print ( where_info(), "sta_dist=  ",  sta_dict  )
    
#
# --- Generate the frequency sequences for each mode
#
    for mode in mode_dict.keys():
#
# ----- Generate procedure files and $FREQ section for each hardware setup
#
        ind_hds = -1
        for hds in mode_dict[mode]["hds"]:
            ind_hds = ind_hds + 1
            if ( ivrb >= 2 ):
                 print ( "%s mode %s hds=%s" % ( where_info(), mode, hds) )

            seq_file = mode_dict[mode]["seq_file"][ind_hds]
#
# --------- Generate procedure file and an excerpt for the vex file of $FREQ sectoin
#
            com = "gen_seq_prc.py " + \
                  " -s " + seq_file + " " + \
                  " -o " + sched_dir + "/" + exper + " " + \
                  " -v %d" % ivrb
            if ( ivrb > 1 ): print ( "%s About to execute %s " % ( where_info(), com ) )
            (ret,err) = exe_pipe ( com )
            check_err_exe ( ret, err, com )

#
# --- Process proc-files and merge setmode defintions
#
    for sta in sta_dict.keys():
        if ( len(sta_dict[sta]["mode"]) > 1 ):
#
# ---------- There is more than one modes for this station.
# ---------- We need to merge the definition of the setmode_ procedure
# ---------- in the procedure file
#
             for i in range(0,len(sta_dict[sta]["mode"])):
                 mode_name = sta_dict[sta]["mode"][i] 
#
# -------------- Get the name of the prc file for the i-th mode
#
                 prc_mode_file = sched_dir + "/" + exper + "/" + mode_name + "_" + sta + ".prc"
                 if ( not os.path.isfile(prc_mode_file) ):
                      print ( "%s cannot find procedure file %s" % ( where_error(), prc_mode_file ) )
                      exit  ( 1 )
                 if ( i == 0 ):
#
# ------------------- Copy the procedure file for this mode to the merged procedure file
#
                      shutil.copyfile ( prc_mode_file, sta_dict[sta]["prc"] )
                 else:
#
# ------------------- Read the procedure file for this mode.
# ------------------- We copy the setmode_ section to mode_prc_buf
#
                      prc = read_file ( prc_mode_file )
                      if ( prc == None ):
                           print ( "%s: cannot read procedure file %s" % ( where_error(), prc_mode_file ) )
                           exit  ( 1 )
    
                      fl_setmode   = False
                      mode_prc_buf = []
                      prov_line    = None

                      for line in prc:
                          if ( len(line.split()) > 1 ):
#
# ---------------------------- Check for the setmode_ procedure definition
#
                               if ( line.split()[0] == "define" and \
                                    "setmode_" in line.split()[1]   ):
                                    fl_setmode = True
                               if ( " Provenance: used frequency sequence" in line ):
                                    prov_line = line
    
                          if ( fl_setmode ):
#
# ---------------------------- Since this line belongs to the procedure definition,
# ---------------------------- we copy its contents to mode_prc_buf
#
                               mode_prc_buf.append ( line ) 
                               if ( line.split()[0] == "enddef" ):
#
# ---------------------------- End of setmode_ procedure definition
#
                                    fl_setmode = False
                   
#
# ------------------- Read the merged procedure file
#
                      prc = read_file ( sta_dict[sta]["prc"] )
                      if ( prc == None ):
                           print ( "%s cannot read procedure file %s" % ( where_error(), sta_dict[sta]["prc"] ) )
                           exit  ( 1 )
#
                      fl_setmode  = False
                      fl_inserted = False
                      prc_buf = []
                      for line in prc:
                          if ( len(line.split()) > 1 ):
#
# ---------------------------- Searchy for provenance lines
#
                               if ( " Provenance: used frequency sequence" in line ):
                                    if ( prov_line != None ):
#
# -------------------------------------- Add the provenance line
#
                                         prc_buf.append ( prov_line )
                               if ( line.split()[0] == "define" and \
                                    "setmode_" in line.split()[1]   ):
                                    fl_setmode = True
                          if ( fl_setmode and not fl_inserted ):
                               if ( line.split()[0] == "enddef" ):
                                    prc_buf.append ( line )
#
# --------------------------------- We reached the end of the procedure definition
#
                                    prc_buf.append ( '"' )
                                    prc_buf.append ( '"=================================' )
                                    prc_buf.append ( '"' )
#
# --------------------------------- Insert the new procedure
#
                                    for lin in mode_prc_buf:
                                        prc_buf.append ( lin )
                                    fl_setmode  = False
                                    fl_inserted = True
                                    continue
                          prc_buf.append ( line )
#
# ------------------- Writes down the update procedure file
#
                      (ret,err) = write_file ( prc_buf, sta_dict[sta]["prc"] )
                      check_err_exe ( ret, err, "write_file" )

#
# --- Collect information about stations
#
    sta_info_buf = read_file ( sta_info_file )
    if ( sta_info_buf == None ):
         print ( "%s cannot read station informaiton file %s" % ( where_error(), sta_info_file ) )
         exit  ( 1 )

    sta_desc_buf = read_file ( sta_desc_file )
    if ( sta_desc_buf == None ):
         print ( "%s cannot read station description file %s" % ( where_error(), sta_desc_file ) )
         exit  ( 1 )

    sta_pos_buf = read_file ( sta_pos_file )
    if ( sta_pos_buf == None ):
         print ( "%s cannot read station position file %s" % ( where_error(), sta_pos_file ) )
         exit  ( 1 )

    sta_vel_buf = read_file ( sta_vel_file )
    if ( sta_vel_buf == None ):
         print ( "%s cannot read station velocity file %s" % ( where_error(), sta_vel_file ) )
         exit  ( 1 )

#
# - Cycle over stations
#
    for sta in sta_dict.keys():
        sta_long = None
#
# ----- Search station information file for the station long name
#
        for line in sta_info_buf:
            if ( line[0:1] == "#" or line[0:1] == " " ): 
                 continue
            if ( sta == line[0:2].lower() ):
                 sta_long = line.split()[1].upper()
        if ( sta_long == None ):
             print ( "%s Did not find station %s in the station information file %s" % \
                     ( where_error(), sta, sta_info_file ) )
             exit  ( 1 )

        sta_dict[sta]["long_name"] = sta_long

#
# ----- Bield the name of the station definition file
#
        stp_file = stp_dir + "/" + sta_long.lower() + ".stp"
        if ( not os.path.isfile(stp_file) ):
             print ( "%s cannot find station definition file %s" % ( where_error(), stp_file ) )
             exit  ( 1 )

        if ( not os.path.isfile(stp_file) ):
             print ( "%s cannot find station definition file %s" % ( where_error(), stp_file ) )
             exit  ( 1 )

#
# ----- Read the staqtion definition file
#
        stp_buf = read_file ( stp_file )
        if ( stp_buf == None ):
             print ( "%s cannot read file %s" % ( where_error(), sta_file ) )
             exit  ( 1 )

#
# ----- Parse the station definition file
#
        for line in stp_buf:
            if ( line[0:1] == "#" ): 
                 continue
            if ( len(line.split()) < 1 ): 
                 continue
#
# --------- Exttract various information from the station definition file
#
            if ( line.split()[0] == "RECORDER:" ):
                 if ( "_recorder" in line.split()[3] ):
                      sta_dict[sta]["recorder"] = line.split()[3]
                 else:
                      sta_dict[sta]["recorder"] = line.split()[3] + "_recorder"
            if ( line.split()[0] == "BACKEND:" ):
                 sta_dict[sta]["backend"] = line.split()[3]
            if ( line.split()[0] == "MOUNT:" ):
                 sta_dict[sta]["mount"] = line.split()[3]
                 sta_dict[sta]["diameter"] = None
            if ( line.split()[0] == "HOR_AZIM:" ):
                 sta_dict[sta]["hor_azim"] = []
                 for i in range(3,len(line.split())):
                     sta_dict[sta]["hor_azim"].append ( float ( line.split()[i] ) )
            if ( line.split()[0] == "HOR_ELEV:" ):
                 sta_dict[sta]["hor_elev"] = []
                 for i in range(3,len(line.split())):
                     sta_dict[sta]["hor_elev"].append ( float ( line.split()[i] ) )
            if ( line.split()[0] == "SLEW_AZ:" ):
                 sta_dict[sta]["slew_az"] = float ( line.split()[3] )
            if ( line.split()[0] == "SLEW_EL:" ):
                 sta_dict[sta]["slew_el"] = float ( line.split()[3] )
            if ( line.split()[0] == "ACCL_AZ:" ):
                 sta_dict[sta]["accl_az"] = float ( line.split()[3] )
            if ( line.split()[0] == "ACCL_EL:" ):
                 sta_dict[sta]["accl_el"] = float ( line.split()[3] )
            if ( line.split()[0] == "TSETTLE_AZ:" ):
                 sta_dict[sta]["tsettle_az"] = float ( line.split()[3] )
            if ( line.split()[0] == "TSETTLE_EL:" ):
                 sta_dict[sta]["tsettle_el"] = float ( line.split()[3] )
            if ( line.split()[0] == "EL_MIN:" ):
                 sta_dict[sta]["el_min"] = float ( line.split()[3] )
            if ( line.split()[0] == "EL_MAX:" ):
                 sta_dict[sta]["el_max"] = float ( line.split()[3] )
            if ( line.split()[0] == "AZ_RANGE:" ):
                 sta_dict[sta]["az_range"] = []
                 for i in range(3,len(line.split())):
                     sta_dict[sta]["az_range"].append ( float(line.split()[i]) )

#
# ----- Get station positions for station sta from the station position file
#
        sta_dict[sta]["pos"] = None
        for line in sta_pos_buf:
            if ( line[0:1] == '$' or line[0:1] == '#' ): continue
            if ( len(line.split()) < 8 ): continue
            if ( line.split()[0] == sta_dict[sta]["long_name"] ):
                 sta_dict[sta]["pos"] = ( float(line.split()[1]), float(line.split()[2]), float(line.split()[3]) )
                 sta_dict[sta]["pos_origin"] = line.split()[7]

        if ( sta_dict[sta]["pos"] == None ):
             print ( "%s: did not find positions of %s (long name: %s) in file %s" % \
                     ( where_error(), sta, sta_dict[sta]["long_name"], sta_pos_file ) )
             exit  ( 1 )

#
# ----- Get station velocities for station sta from the station velocity file
#
        sta_dict[sta]["vel"] = None
        for line in sta_vel_buf:
            if ( line[0:1] == '$' or line[0:1] == '#' ): continue
            if ( len(line.split()) < 5 ): continue
            if ( line.split()[0] == sta_dict[sta]["long_name"] ):
                 sta_dict[sta]["vel"] = ( float(line.split()[1]), float(line.split()[2]), float(line.split()[3]) )

        if ( sta_dict[sta]["vel"] == None ):
             print ( "%s: did not find velocities of %s (long name: %s) in file %s" % \
                     ( where_error(), sta, sta_dict[sta]["long_name"], sta_vel_file ) )
             exit  ( 1 )

#
# ----- Get antenna diameter and the axis offset form the station description file
#
        sta_dict[sta]["diameter"] = None
        sta_dict[sta]["axof"]     = None
        for line in sta_desc_buf:
            if ( line[0:1] == '#' ): continue
            if ( line.split()[0] == sta_dict[sta]["long_name"] ):
                 sta_dict[sta]["axof"]     = float(line.split()[2])
                 sta_dict[sta]["diameter"] = float(line.split()[4])

        if ( sta_dict[sta]["axof"] == None ):
             print ( "%s: did not find antenna axis offset of %s (long name: %s) in file %s" % \
                     ( where_error(), sta, sta_dict[sta]["long_name"], sta_desc_file ) )
             exit  ( 1 )

#
# --- Build the name of the output vex template file
#
    vex_tmpl_file = sched_dir + "/" + exper + "/" + exper + "_vex.tmpl"
#
# ---  Read the inout vex template file
#
    proto = read_file ( proto_tmpl_file )
    if ( proto == None ):
         print ( "%s in reading vex proto files %s" % ( where_error(), proto_tmpl_file ) )
         exit  ( 1 )

#
# -- Process @SITE_DEF@ clause in the vex template file and fill $SITE section
#
    vex = []
    for line in proto:
        if ( len(line.split()) == 0 ): continue
        if ( line.split()[0] == "@SITE_DEF@" ):
             for sta in sta_dict.keys():
#
# --------------- For each station put information into the $SITE section
#
                  vex.append ( "  def %s;" % sta_dict[sta]["long_name"] )
                  vex.append ( "    site_ID = %s;" % sta )
                  vex.append ( "    site_position = %12.3f      m:  %12.3f      m:    %12.3f   m;   * origin: %s" % \
                               ( sta_dict[sta]["pos"][0], sta_dict[sta]["pos"][1], sta_dict[sta]["pos"][2], \
                                 sta_dict[sta]["pos_origin"] ) )
                  vex.append ( "    site_velocity = %14.5f m/yr:  %14.5f    m/yr: %14.5f m/yr;" % \
                               ( 0.001*sta_dict[sta]["vel"][0], 0.001*sta_dict[sta]["vel"][1], 0.001*sta_dict[sta]["vel"][2] ) )
                  vex.append ( "    site_position_epoch =  51544;" )
                  vex.append ( "    site_type           =  fixed;" )  
                  hor_az_str = ""
                  hor_el_str = ""
                  for i in range(0,len(sta_dict[sta]["hor_azim"])):
                      hor_az_str = hor_az_str + " %5.1f deg :" % sta_dict[sta]["hor_azim"][i]
                      hor_el_str = hor_el_str + " %5.1f deg :" % sta_dict[sta]["hor_elev"][i]
                  hor_az_str = hor_az_str[0:-1] + ";"
                  hor_el_str = hor_el_str[0:-1] + ";"
                  vex.append ( "    horizon_map_az      = %s" % hor_az_str )
                  vex.append ( "    horizon_map_el      = %s" % hor_el_str )
                  vex.append ( "  enddef;" )
                  if ( sta != list(sta_dict.keys())[-1] ):
                       vex.append ( "*" )
        else:
             vex.append ( line )
    proto = vex

#
# -- Process @STATION_DEF@ clause in the vex template file and fill $STATION section
#
    vex = []
    for line in proto:
        if ( len(line.split()) == 0 ): continue
        if ( line.split()[0] == "@STATION_DEF@" ):
             for sta in sta_dict.keys():
#
# --------------- For each station put information into the $STATION section
#
                  vex.append ( "  def %s;" % sta )
                  vex.append ( "      ref $SITE    = %s;"     % sta_dict[sta]["long_name"] )
                  vex.append ( "      ref $ANTENNA = %s;"     % sta_dict[sta]["long_name"] )
                  vex.append ( "      ref $DAS     = %s;"     % sta_dict[sta]["backend"] )
                  vex.append ( "      ref $DAS     = %s_das;" % sta )
                  vex.append ( "      ref $DAS     = %s;"     % sta_dict[sta]["recorder"] )
                  vex.append ( "  enddef;" )
                  if ( sta != list(sta_dict.keys())[-1] ):
                       vex.append ( "*" )
        else:
             vex.append ( line )
    proto = vex

#
# -- Process @ANTENNA_DEF@ clause in the vex template file and fill $ANTENNA section
#
    vex = []
    for line in proto:
        if ( len(line.split()) == 0 ): continue
        if ( line.split()[0] == "@ANTENNA_DEF@" ):
             for sta in sta_dict.keys():
#
# --------------- For each station put information into the $ANTENNA section
#
                  vex.append ( "  def %s;" % sta_dict[sta]["long_name"] )
                  if ( sta_dict[sta]["diameter"] != None ):
                       vex.append ( "      antenna_diam = %5.1f m;" % sta_dict[sta]["diameter"] )
                  else:
                       vex.append ( "      antenna_diam =    1.0 m; * unknown"  )
                  vex.append ( "      axis_offset  =  %7.4f m;" % sta_dict[sta]["axof"] )
                  if ( sta_dict[sta]["mount"] == "ALTAZ" ):
                       vex.append ( "      axis_type = az : el;" )
                  elif ( sta_dict[sta]["mount"] == "EQUAT" ):
                       vex.append ( "      axis_type = ha : dec;" )
                  elif ( sta_dict[sta]["mount"] == "X-YE" ):
                       vex.append ( "      axis_type = x : yew;" )
                  else: 
                       print ( "%s: unsuported mounting type %s for station %s" % \
                               ( where_error(), sta_dict[sta]["mount"], sta ) )
                       exit  ( 1 )
                  vex.append ( "      antenna_motion  = az : %5.2f deg/sec : %5.2f sec : %5.2f deg/sec^2;" % \
                               ( sta_dict[sta]["slew_az"], sta_dict[sta]["tsettle_az"], sta_dict[sta]["accl_az"] ) )
                  vex.append ( "      antenna_motion  = el : %5.2f deg/sec : %5.2f sec : %5.2f deg/sec^2;" % \
                               ( sta_dict[sta]["slew_el"], sta_dict[sta]["tsettle_el"], sta_dict[sta]["accl_el"] ) )
                  vex.append ( "      pointing_sector = &ccw : az : %6.1f deg : %6.1f deg : el : %6.1f deg  : %6.1f deg : ccw ;" % \
                               ( sta_dict[sta]["az_range"][0], sta_dict[sta]["az_range"][1], \
                                 sta_dict[sta]["el_min"],      sta_dict[sta]["el_max"] ) )
                  vex.append ( "      pointing_sector = &n   : az : %6.1f deg : %6.1f deg : el : %6.1f deg  : %6.1f deg : n   ;" % \
                               ( sta_dict[sta]["az_range"][1], sta_dict[sta]["az_range"][2], \
                                 sta_dict[sta]["el_min"],      sta_dict[sta]["el_max"] ) )
                  vex.append ( "      pointing_sector = &cw  : az : %6.1f deg : %6.1f deg : el : %6.1f deg  : %6.1f deg : cw  ;" % \
                               ( sta_dict[sta]["az_range"][2], sta_dict[sta]["az_range"][3], \
                                 sta_dict[sta]["el_min"],      sta_dict[sta]["el_max"] ) )
                  vex.append ( "  enddef;" )
                  if ( sta != list(sta_dict.keys())[-1] ):
                       vex.append ( "*" )
        else:
             vex.append ( line )
    proto = vex
#
# -- Process @DAS_DEF@ clause in the vex template file and fill $ADAS section
#
    vex = []
    for line in proto:
        if ( len(line.split()) == 0 ): continue
        if ( line.split()[0] == "@DAS_DEF@" ):
             for sta in sta_dict.keys():
#
# --------------- For each station put information into the $DAS section
#
                  vex.append ( "  def %s_das;" % sta )
                  vex.append ( "      recording_system_ID = %s;" % sta_dict[sta]["recorder"] )
                  vex.append ( "  enddef;" )
                  if ( sta != list(sta_dict.keys())[-1] ):
                       vex.append ( "*" )
        else:
             vex.append ( line )
    proto = vex

#
# --- Expand HDS_BLOCK @-expressions
#
    if ( len(hds_list) == 1 ):
#
# ------ Case when there is only one hardware setup 
#
         vex = []
         for i in range(0,len(proto)):
             line = proto[i]
             if ( "@HDS@" in line ):
#
# --------------- Just replace @HDS@ with the actual hadrware setup name
#
                  line = line.replace("@HDS@",hds_list[0])
             vex.append  ( line )
    else:
         vex = []
         ind_skip = []
         for i in range(0,len(proto)):
             line = proto[i]
             if ( len(line.split()) < 1 ): continue
             if ( "@HDS_BLOCK_BEG@" in line ):
#
# --------------- Found beginning of the HDS block.
# --------------- Get the rnage if indices of proto buffer that corresond
# --------------- to the HDS_BLOCK seciont
#
                  ind_end = None
                  for j in range(i,len(proto)):
#
# ------------------- Put the index of line in the skip-list to prevent
# ------------------- writing it again in the unaltered form
#
                      ind_skip.append(j)
                      if ( "@HDS_BLOCK_END@" in proto[j] ):
                           ind_end = j
                           break
#
# --------------- Cycle over hardware setups
#
                  for hds in hds_list:
#
# -------------------- Duplicate @HDS@ word with the hadware setup name witin
# -------------------- the HDS_BLOCK section
#
                      for k in range(i,ind_end+1):
                          lin = proto[k].replace("@HDS@",hds)
                          vex.append ( lin  )
                      if ( hds != hds_list[-1] ):
                           vex.append  ( "*" )

             else:
                 if ( not i in ind_skip ):
                      vex.append  ( line )
         proto = vex

#
# ------ Expand STA_HDS @-expression
#
         vex = []
         for i in range(0,len(proto)):
             line = proto[i]
             if ( "@HDS@" in line ):
#
# --------------- We found  @HDS@ word in the line
# --------------- Cycle over all harware setups
#
                  for hds in hds_list:
                      if ( "@STA_HDS@" in line ):
#
# ------------------------ Aga, we found @STA_HDS@ word
#
                           sta_hds = []
#
# ------------------------ Cyhcle over all station
#
                           for sta in sta_dict.keys():
#
# ---------------------------- Check, whether this station participate
# ---------------------------- in this hardware setup
#
                               if ( hds in sta_dict[sta]["hds"] ):
#
# --------------------------------- Yes? Add the to the list
#
                                    sta_hds.append ( sta )
#
# ------------------------ Replace @STA_HDS@ with the column-separated list
# ------------------------ of stations that participate in this harware setup
#
                           line = line.replace("@STA_HDS@",":".join(sta_hds)) 
#
# ------------------- Replace @HDS@ with the actual harware setup name
#
                      vex.append  ( line.replace("@HDS@",hds) )
                      line = proto[i]
                  continue
#
# ---------- Process HDSx, STAx, and STA_HDS @-expressions
#
             for k in range(0,len(hds_list)):
#
# -------------- Cycle over hardware setups
#
                 HDS_PATTERN = "@HDS%d@" % (k+1)
                 STA_PATTERN = "@STA%d@" % (k+1)
                 if ( "@STA_HDS@" in proto[i] ):
                      STA_PATTERN = "@STA_HDS@"
                 if ( HDS_PATTERN in line ):
                      line = line.replace(HDS_PATTERN,hds_list[k]) 
                 
                 if ( STA_PATTERN in proto[i] ):
#
# ------------------- We found either @STA@_HDS@ or @STAx pattern
# ------------------- Generate list sta_hds of stations that participate
# ------------------- in this setup
#
                      sta_hds = []
                      for sta in sta_dict.keys():
                          if ( hds_list[k] in sta_dict[sta]["hds"] ):
                               sta_hds.append ( sta )
#
# ------------------- Replace @STA@_HDS@ or @STAx pattern with 
# ------------------- that station list 
#
                      line = line.replace(STA_PATTERN,":".join(sta_hds)) 
             vex.append  ( line )

##    (ret,err) = write_file ( vex, vex_tmpl_file )  ; exit ( 66  ) # %%%%%%%%%%% ##########################
#
# --- Expand STA_BLOCK @-expressions
#
    proto = vex
    vex = []
    ind_skip = []
    for i in range(0,len(proto)):
        line = proto[i]
        if ( len(line.split()) < 2 ):
             if ( not i in ind_skip ):
                  vex.append ( line )
             continue
        if ( "@STA_BLOCK_BEG@" in line ):
#
# ---------- We found beginning of STA_BLOCK
#
             ind_end = None
             for j in range(i,len(proto)):
#
# -------------- Put the index of line in the skip-list to prevent
# -------------- writing it again in the unaltered form
#
                 ind_skip.append(j)
                 if ( "@STA_BLOCK_END@" in proto[j] ):
                      ind_end = j
                      break
#
# ---------- Replicate STA_BLOCK n times, where n is the number of station
#
             for sta in sta_list:
                 for k in range(i,ind_end+1):
#
# ------------------ Replace @STA@ with the station name
#
                     lin = proto[k].replace("@STA@",sta)
                     if ( '@HDS_STA@' in lin ):
#
# ----------------------- Replace @HDS_STA@ with the name of the hardware setup 
# ----------------------- which that station used
#
                          lin = lin.replace('@HDS_STA@',sta_dict[sta]["hds"])
                     vex.append ( lin  )
                 if ( sta != sta_list[-1] ):
                      vex.append  ( "*" )
        else:
             if ( not i in ind_skip ):
                  vex.append  ( line )
    proto = vex

#
# --- Expand STA and STA_LIST @-experessions
#
    vex = []
    for i in range(0,len(proto)):
        line = proto[i]
        if ( "@STA_LIST@" in line ):
#
# ---------- Replace @STA_LIST@ with the column-separated station list
#
             vex.append  ( line.replace("@STA_LIST@",":".join(sta_list)) )
        elif ( "@STA@" in line ):
#
# ---------- Replicate lines with @STA@ for each station and replace @STA@ with the station name
#
             for sta in sta_list:
                 vex.append  ( line.replace("@STA@",sta) )
        else:
             vex.append  ( line )
    proto = vex

#
# --- Expand @MODE_BLOCK experssion
#
    vex = []
    ind_skip = []
    for i in range(0,len(proto)):
        line = proto[i]

        if ( len(line.split()) < 2 ):
             if ( not i in ind_skip ):
                  vex.append ( line )
             continue
        if ( "@MODE_BLOCK_BEG@" in line ):
             ind_end = None
             for j in range(i,len(proto)):
                 ind_skip.append(j)
                 if ( "@MODE_BLOCK_END@" in proto[j] ):
                      ind_end = j
                      break
             for mode in mode_dict.keys():
                 for k in range(i,ind_end+1):
                     lin = proto[k].replace("@MODE@",mode)
                     vex.append ( lin  )
                 if ( mode != list(mode_dict.keys())[-1] ):
                      vex.append  ( "*" )
        else:
             if ( not i in ind_skip ):
                  vex.append  ( line )


#
# --- Expand MODE @-expressions
#
    proto = vex
    vex = []
    for i in range(0,len(proto)):
        line = proto[i]
        if ( "@MODE@" in line ):
             if ( len(mode_dict.keys()) == 1 ):
#
# --------------- Only one mode was used. Replace @MODE@ with the mode name
#
                  vex.append ( line.replace("@MODE@",list(mode_dict.keys())[0]) )
             else:
#
# --------------- More than one mode was used. Replicate the line and 
# --------------- replace @MODE@ with each mode name
#
                  for mode in mode_dict.keys():
                      vex.append ( line.replace("@MODE@",mode) )
             continue

        vex.append  ( line )

#
# --- Expand FREQ  @-expressions and replace it with the frequency setup 
# --- files generated by gen_seq_prc.py utility
#
    proto = vex
#
# --- Cycle over modes
#
    for mode in mode_dict.keys():
#
# ----- Cycle over hardware setups of this mode
#
        for hds in mode_dict[mode]["hds"]:
#
# --------- Search for lines in the vex templae with a specific pattern
#
            FREQ_PATTERN = mode + "_" + hds + "_@FREQ@"
            vex   = []
            for i in range(0,len(proto)):
                line = proto[i]
                if ( FREQ_PATTERN in line ):
#
# ------------------ We find this pattern
# ------------------ Build  the name of the frequency setup file 
# ------------------ generated by gen_seq_prc.py utility
#
                     vex_frq_file = sched_dir + "/" + exper + "/" + mode + "_" + hds + "_vex.frq"
                     if ( not os.path.isfile(vex_frq_file)  ):
                          print ( "%s: cannot find file %s" % ( where_error(), vex_frq_file ) )
                          exit  ( 1 )
#
# ------------------ Read this file
#
                     vex_frq_buf = read_file ( vex_frq_file )
                     if ( vex_frq_buf == None ):
                          print ( "%s: cannot read file %s" % ( where_error(), vex_frq_file ) )
                          exit  ( 1 )
#
# ------------------ and copy its conents into the vex template file
#
                     for lin in vex_frq_buf:
                         vex.append ( lin )
                     continue
                else:
                     vex.append ( line )
            proto = vex

#
# --- Insert the contents of Field System procedure file into the vex template
# --- Extract duration time for seven Field System public procedures from
# --- the procedure file for each station. Put durations into sta_dct
#
    for sta in sta_dict.keys():
#
# ----- Cycle over stations. 
# ----- Build the name of the procedure file
#
        prc_file = sched_dir + "/" + exper + "/" + "+".join(sta_dict[sta]["mode"]) + "_" + sta + ".prc"
        if ( not os.path.isfile(prc_file)  ):
             print ( "%s cannot find procedure file %s" % ( where_error(), prc_file ) )
             exit  ( 1 )
#
# ----- Read the procedure file
#
        prc_buf = read_file ( prc_file )
        if ( prc_buf == None ):
             print ( "%s cannot read procedure file %s" % ( where_error(), prc_file ) )
             exit  ( 1 )

        proto = vex
        vex   = []
        for i in range(0,len(proto)):
            line = proto[i]
            if ( len(line.split()) >= 1 ):
#
# -------------- Search for the line that is the marker of the procedure 
# -------------- in the vex template
#
                 if ( line.split()[0] == "start_literal(proc_" + sta + ");" ):
                      vex.append ( line )
#
# ------------------- Copy contents of the procedure file to the section
# ------------------- of the vex templae
#
                      for j in range(0,len(prc_buf)):
                          lin = prc_buf[j]
                          vex.append ( "        " + lin )
                          if  ( len(lin.split()) > 1 ):
#
# ----------------------------- Search for a patter of the definition of a public procedure
#
                                for pub_proc in pub_proc_list:
#
# --------------------------------- dur_pub_proc is the pattern of the prublc porocedure definition
#
                                    dur_pub_proc = "dur_" + pub_proc
                                    if ( lin.split()[0] == "define" and (pub_proc + "_") in lin.split()[1] ):
#
# -------------------------------------- Extract the procedure duration.
# -------------------------------------- By convention it is the 3rd word of the next line
#
                                         sta_dict[sta][dur_pub_proc] = int(prc_buf[j+1].split()[2])
                      continue
            vex.append ( line )

    if ( ivrb >=2 ):
         print ( where_info(), "sta_dist= ", mode_dict )
                     
#
# --- Replace DUR, backend, and schedule-convert @-expressions
#
    proto = vex
    vex   = []
    for i in range(0,len(proto)):
        line = proto[i]
        if ( len(line.split()) < 1 ): continue
        if ( "@DUR@" in line ):
#
# ---------- Found DUR @-expression
#
             if ( line.split()[6] in pub_proc_list ):
#
# --------------- This line defines duration of a public procedure from the list
#
                  sta = line.split()[4]
                  if ( not sta in sta_dict.keys() ):
                       print ( "%s: cannot find station %s in line %s in the station list" % \
                               ( where_error(), sta, line ) )
                       exit ( 1 )
#
# --------------- ind_pub_proc -- index of the public procedure
# --------------- dur_pub_proc -- name of the dictionary key for duration of that procedure
#
                  ind_pub_proc = pub_proc_list.index ( line.split()[6] )
                  dur_pub_proc = "dur_" + pub_proc_list[ind_pub_proc]
                  if ( not dur_pub_proc in sta_dict[sta].keys() ):
                       print ( "%s: %s duratrion was not defined in the procedure file for station %s" % \
                               ( where_error(), pub_proc_list[ind_pub_proc], sta ) )
                       exit ( 1 )
#
# --------------- Replace @DUR@ with the actual duration
#
                  line = line.replace ( line.split()[8], "%2d" % sta_dict[sta][dur_pub_proc] )
        if ( "@backend@" in line ):
             if ( not "backend" in sta_dict[line.split()[6]].keys() ):
                  print ( "%s: backend was not defined in the procedure file for station %s" % \
                          ( where_error(), line.split()[6] ) )
                  exit ( 1 )
             line = line.replace ( line.split()[8], sta_dict[line.split()[6]]["backend"] )
             
        if ( "@schedule_convert@" in line ):
             sta = line.split()[4]
             if ( not sta in sta_dict.keys() ):
                   print ( "%s: cannot find station %s in line %s in the station list" % \
                           ( where_error(), sta, line ) )
                   exit ( 1 )
           
             if ( not "backend" in sta_dict[sta].keys() ):
                  print ( "%s: backend was not defined for station %s" % ( where_error(), sta ) )
                  exit ( 1 )
#
# ---------- Generate a command for convertingf vex schedule to proc/snap
# ---------- It dependes on the backend
#
             if ( "rdbe"  in sta_dict[sta]["backend"] or \
                  "r2dbe" in sta_dict[sta]["backend"] or \
                  "dbbc2" in sta_dict[sta]["backend"] or \
                  "dbbc3" in sta_dict[sta]["backend"] or \
                  "cdas" in sta_dict[sta]["backend"]     ):
                   com = "vex_to_snap.py -m " + "mk6_ext" + " -o " + \
                         sched_dir + "/" + exper + "/" + exper + sta + ".snp" + " " + \
                         sched_dir + "/" + exper + "/" + exper + ".vex" + " " + sta
             elif ( "mark5" in sta_dict[sta]["backend"] ):
                   com = "vex_to_snap.py -m " + "mk5_ext" + " -o " + \
                         sched_dir + "/" + exper + "/" + exper + sta + ".snp" + " " + \
                         sched_dir + "/" + exper + "/" + exper + ".vex" + " " + sta
             else:
                  print ( "%s: unknown backend %s was found for station %s" % \
                          ( where_error(), sta_dict[sta]["backend"], sta ) )
                  exit ( 1 )
             line = line.replace ( "@schedule_convert@", com )

        vex.append ( line )

#
# --- Final cleanup: 
# --- 1) remove @xxx_BLOCK_yyy@ anchor words
# --- 2) put name and version of gen_vex_templ program
#
    proto = vex
    vex   = []
    for i in range(0,len(proto)):
        line = proto[i]
        if ( len(line.split()) < 1 ): continue
        for j in range(0,len(proto[i].split())):
            word = proto[i].split()[j]
            if ( word in block_word_list ):
                 line = line.replace ( word, "" )
            if ( word == '"@GEN_VEX_TEMPLATE@"' ):
                 now_str =  datetime.datetime.now().astimezone().strftime("%Y.%m.%d_%H:%M:%S %z")
                 line = line.replace ( proto[i].split()[j],   gen_vex_template__label.split()[0] )
                 line = line.replace ( proto[i].split()[j+2], gen_vex_template__label.split()[1] )
                 line = line.replace ( proto[i].split()[j+4], now_str )

        vex.append ( line )

#
# --- Write down the schedule template file
#
    (ret,err) = write_file ( vex, vex_tmpl_file )
    check_err_exe ( ret, err, "write_file" )

    if ( ivrb >= 1 ):
         print ( "%s Generated vex template file %s" % ( where_info(), vex_tmpl_file), flush=True ) 

#
# ------------------------------------------------------------------------
#
def main():
    """
    Parse the arguments
    """
    parser = argparse.ArgumentParser( description=gen_vex_template__label )
    parser.add_argument ( '--version', action='version', version=gen_vex_template__label )
    parser.add_argument ( "-e", "--experiment", \
                          action="store", \
                          dest="exper", \
                          metavar="exper", \
                          help="Experiment name" )

    parser.add_argument ( "-sl", "--sta_list", \
                          action="store", \
			  required=True, \
                          dest="sta_list", \
                          metavar="sta_list", \
                          help="Station list" )

    parser.add_argument ( "-m", "--modes_string", \
                          action="store", \
			  required=True, \
                          dest="modes_str", \
                          metavar="modes_str", \
                          help="Mode string" )

    parser.add_argument ( "-p", "--proto", \
                          action="store", \
			  required=True, \
                          dest="proto_file_tmpl", \
                          metavar="proto_file_tmpl", \
                          help="File with vex prototype" )

    parser.add_argument ( "-d", "--schedule_dir", \
                          action="store", \
			  required=True, \
                          dest="sched_dir", \
                          metavar="sched_dir", \
                          help="Directory with VLBI expriment files" )

    parser.add_argument ( "-f", "--frq_dir", \
                          action="store", \
			  required=True, \
                          dest="frq_dir", \
                          metavar="fr4q_dir", \
                          help="Directory with frequency definitions" )

    parser.add_argument ( "-st", "--stp_dir", \
                          action="store", \
			  required=True, \
                          dest="stp_dir", \
                          metavar="stp_dir", \
                          help="Station parameter directory" )

    parser.add_argument ( "-si", "--sta_info", \
                          action="store", \
			  required=True, \
                          dest="sta_info", \
                          metavar="sta_info", \
                          help="File with station information" )

    parser.add_argument ( "-sd", "--sta_desc", \
                          action="store", \
			  required=True, \
                          dest="sta_desc", \
                          metavar="sta_desc", \
                          help="File with station description" )

    parser.add_argument ( "-sp", "--sta_pos", \
                          action="store", \
			  required=True, \
                          dest="sta_pos", \
                          metavar="sta_pos", \
                          help="File with station positions" )

    parser.add_argument ( "-sv", "--sta_vel", \
                          action="store", \
			  required=True, \
                          dest="sta_vel", \
                          metavar="sta_vel", \
                          help="File with station velocities" )

    parser.add_argument ( "-v", "--verbosity", \
                          action="store", \
                          dest="verb", \
                          default=1, \
                          metavar="verbosity", \
                          type=int, \
                          help="Verbosity level" )

    args = parser.parse_args()
         
    if ( not args.exper ):
         print ( "%s: Experiment name is not defined" % where_error() )
         exit  ( 1 )

    gen_vex_template ( args.exper.lower(),   \
                       args.sta_list,        \
                       args.modes_str,       \
                       args.proto_file_tmpl, \
                       args.sched_dir,       \
                       args.frq_dir,         \
                       args.stp_dir,         \
                       args.sta_info,        \
                       args.sta_desc,        \
                       args.sta_pos,         \
                       args.sta_vel,         \
                       args.verb             )

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
