#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program gen_hrtr_files.py generates NRAO style three files         *
# *   used for scheduling VLBA+HRTR observations using templates and     *
# *   SGDASS apriori data. These three files are                         *
# *   1) station definitions,                                            *
# *   2) station locations, and                                          *
# *   3) freqency defintions.                                            *
# *                                                                      *
# *   Copyright (c) 1975-2025 United States Government as represented by *
# *   the Administrator of the National Aeronautics and Space            *
# *   Administration. All Rights Reserved.                               *
# *   License: NASA Open Source Software Agreement (NOSA).               *
# *                                                                      *
# *  ## 02-OCT-2025 gen_hrtr_files v1.0 (d)  L. Petrov  05-OCT-2025 ###  *
# *                                                                      *
# ************************************************************************
import argparse, signal, sys, os, pwd, math
sys.path.append('/auto')
from   pet_misc  import *
gen_hrtr_files__label = "gen_hrtr_files 20251002"

vlba_sta_list = [ \
                  "BR-VLBA", \
                  "FD-VLBA", \
                  "HN-VLBA", \
                  "KP-VLBA", \
                  "LA-VLBA", \
                  "MK-VLBA", \
                  "NL-VLBA", \
                  "OV-VLBA", \
                  "PIETOWN", \
                  "SC-VLBA", \
                 ]

                
fil_sta_pos = "/apr/sta/glo.sit"
fil_sta_vel = "/apr/sta/glo.vel"
fil_sta_des = "/apr/sta/station.desc"
#
fil_sta_inp = "/vlbi/gnv/vlba_stations.txt"
fil_loc_inp = "/vlbi/gnv/vlba_locations.txt"
fil_frq_inp = "/vlbi/gnv/vlba_freq.txt"
#
fil_sta_out = "/vlbi/gnv/gnv_stations.txt"
fil_loc_out = "/vlbi/gnv/gnv_locations.txt"
fil_frq_out = "/vlbi/gnv/gnv_freq.txt"

#
# --- Read intput files
#
buf_sta_pos = read_file ( fil_sta_pos )
if ( buf_sta_pos == None ):
     print ( "Error in reading %s" % fil_sta_pos )
     exit  ( 1 )

buf_sta_vel = read_file ( fil_sta_vel )
if ( buf_sta_vel == None ):
     print ( "Error in reading %s" % fil_sta_vel )
     exit  ( 1 )

buf_sta_des = read_file ( fil_sta_des )
if ( buf_sta_des == None ):
     print ( "Error in reading %s" % fil_sta_des )
     exit  ( 1 )

buf_frq_inp = read_file ( fil_frq_inp )
if ( buf_frq_inp == None ):
     print ( "Error in reading %s" % fil_frq_inp )
     exit  ( 1 )

date_now = datetime.datetime.now().strftime( '%Y.%m.%d_%H:%M:%S' )

#
# --- Extract the list of GNSS stations (sta_list)
#
sta_list = []
for line in buf_sta_des:
    if ( line[0:1] == "#" ): continue
    if ( len(line.split()) < 6 ): continue    
    if ( len(line.split()[0]) == 4 and \
         line.split()[1] == "GNSS"     ):
         sta_list.append ( line.split()[0] )

#
# --- and sort it
#
sta_list = sorted(sta_list)

sta_pos_dict={}
sta_vel_dict={}
sta_vlba_pos_dict={}
sta_vlba_vel_dict={}

#
# --- Process input file with apriori station position 
#
sol_id = "uknown"
for line in buf_sta_pos:
    if ( line[0:1] == "#" ): continue
    if ( "$$  Generated from solution" in line ):
         sol_id = line.split()[4]
    if ( line[0:1] == "$" ): continue
    if ( len(line.split()) < 4 ): continue    
#
# --- Put position of GNSS stations to sta_pos_dict
#
    sta = line.split()[0]
    if ( sta in sta_list ):
         sta_pos_dict[sta] = [ float(line.split()[1]), float(line.split()[2]), float(line.split()[3]) ]
#
# --- Put position of VLBA stations to sta_vlba_pos_dict
#
    if ( sta in vlba_sta_list ):
         sta_vlba_pos_dict[sta] = [ float(line.split()[1]), float(line.split()[2]), float(line.split()[3]) ]
         sol_id = line.split()[7]

#
# --- Process input file with apriori station velocity
#
for line in buf_sta_vel:
    if ( line[0:1] == "#" ): continue
    if ( len(line.split()) < 4 ): continue    
    sta = line.split()[0]
#
# --- Put velocity of GNSS stations to sta_pos_dict
#
    if ( sta in sta_list ):
         sta_vel_dict[sta] = [ float(line.split()[1]), float(line.split()[2]), float(line.split()[3]) ]
#
# --- Put velocity of VLBA stations to sta_vlba_pos_dict
#
    if ( sta in vlba_sta_list ):
         sta_vlba_vel_dict[sta] = [ float(line.split()[1]), float(line.split()[2]), float(line.split()[3]) ]

#
# --- Read the input station defintion file
#
buf_sta = read_file ( fil_sta_inp )

for i in range(0,len(buf_sta)):
#
# --- Process @-experssion in the comment section of the input station file
#
    if ( "__@DATE@___________" in buf_sta[i] ):
          date_now = datetime.datetime.now().strftime( '%Y.%m.%d_%H:%M:%S' )
          buf_sta[i] = buf_sta[i].replace ( "__@DATE@___________", date_now )
    if ( "__@NUMSTA@__" in buf_sta[i] ):
          buf_sta[i] = buf_sta[i].replace ( "__@NUMSTA@__", "%d" % len(sta_list) )
    if ( "__@PROGNAME@__" in buf_sta[i] ):
          buf_sta[i] = buf_sta[i].replace ( "__@PROGNAME@__", gen_hrtr_files__label )

#
# --- Add a section with GNSS stations
#
buf_sta.append ( '!' )
buf_sta.append ( '!  High Rate Trackinbg Receiver (HRTR) GNSS stations' )
buf_sta.append ( '!' )

sta_ind = 0
for sta in sta_list:
#
# --- Add information about GNSS station sta
#
    sta_ind = sta_ind + 1
    if ( sta_ind < 10 ):
         sta_code = "H"+chr(48+sta_ind)
    else:
         sta_code = "H"+chr(55+sta_ind)

    buf_sta.append ( "STATION=%-8s    STCODE=%2s  CONTROL=VLBA" % ( sta, sta_code ) )
    buf_sta.append ( "    DBNAME=%-8s" % sta )
    buf_sta.append ( "    ! Next 9 lines are bogus. In fact, this is a GNSS reciever" )
    buf_sta.append ( "    MOUNT=ALTAZ  AX1LIM=-90,450 AX2LIM=2.00,90 AX1RATE=999.9 AX2RATE=999.9" )
    buf_sta.append ( "    TSETTLE=2 TLEVSET=2 MINSETUP=2  DAR=RDBE2  NBBC=16 " )
    buf_sta.append ( "    DISK=MARK5C   MEDIADEF=DISK    TSCAL=CONT" )
    buf_sta.append ( "    HOR_AZ =   0,  5, 10, 15, 25, 30, 40, 45, 70, 75,120,125,130,135," )
    buf_sta.append ( "             155,160,185,190,195,220,225,235,240,245,250,255,265,270," )
    buf_sta.append ( "             275,300,305,310,315,330,335,340,345,350,360" )
    buf_sta.append ( "    HOR_EL =   5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5," )
    buf_sta.append ( "               5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5," )
    buf_sta.append ( "               5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5" )
    buf_sta.append ( "  / " )

#
# --- Write output station defintion file
#
(ret,err) = write_file ( buf_sta, fil_sta_out )
check_err_exe ( ret, err, "write_file" )

#
# --- Read the input station location file
#
buf_loc = read_file ( fil_loc_inp )

for i in range(0,len(buf_loc)):
#
# --- expand @-expressions
#
    if ( "__@DATE@___________" in buf_loc[i] ):
          buf_loc[i] = buf_loc[i].replace ( "__@DATE@___________", date_now )
    if ( "__@NUMSTA@__" in buf_loc[i] ):
          buf_loc[i] = buf_loc[i].replace ( "__@NUMSTA@__", "%d" % len(sta_list) )
    if ( "__@PROGNAME@__" in buf_loc[i] ):
          buf_loc[i] = buf_loc[i].replace ( "__@PROGNAME@__", gen_hrtr_files__label )
    if ( " X= " in buf_loc[i] ):
#
# ------ Put positions of VLBA stations
#
         sta = buf_loc[i-1][7:14]
         buf_loc[i] = "    X= %12.3f   Y = %12.3f   Z = %12.3f" % \
                            ( sta_vlba_pos_dict[sta][0],  \
                              sta_vlba_pos_dict[sta][1],  \
                              sta_vlba_pos_dict[sta][2] )
         buf_loc[i+1] = "    DXDT= %8.5f    DYDT= %8.5f    DZDT= %8.5f  EPOCH=51544" % \
                        ( 0.001*sta_vlba_vel_dict[sta][0], \
                          0.001*sta_vlba_vel_dict[sta][1], \
                          0.001*sta_vlba_vel_dict[sta][2]  \
                        )
         buf_loc[i+2] = "    FRAME='NASA GSFC, Solution %s'" % sol_id
         

buf_loc.append ( '!' )
buf_loc.append ( '!  High Rate Tracking Receiver (HRTR) GNSS stations' )
buf_loc.append ( '!' )

for sta in sta_list:
#
# ------ Put positions of GNSS stations
#
    buf_loc.append ( "DBNAME=%s      BEGIN=     0.00  END=100000.00  AXISOFF=  0.0" % sta )
    buf_loc.append ( "    X= %12.3f   Y = %12.3f   Z = %12.3f" % \
                     ( sta_pos_dict[sta][0], sta_pos_dict[sta][1], sta_pos_dict[sta][2] )  )
    buf_loc.append ( "    DXDT= %8.5f    DYDT= %8.5f    DZDT= %8.5f  EPOCH=51544" % \
                     ( 0.001*sta_vel_dict[sta][0], 0.001*sta_vel_dict[sta][1], 0.001*sta_vel_dict[sta][2] ) )
    buf_loc.append ( "    FRAME='unknwon                                   '" )
    buf_loc.append ( "/" )

#
# --- Write output station location file
#
(ret,err) = write_file ( buf_loc, fil_loc_out )
check_err_exe ( ret, err, "write_file" )

#
# -- Generate the herader of tthje frequency definition file
#
buf_frq = []
buf_frq.append ( "! Frequency file for an experiment between HRTR and VLBA stations" )
buf_frq.append ( "!" )
buf_frq.append ( "! Generated on %s with %s " % ( date_now, gen_hrtr_files__label ) )
buf_frq.append ( "!" )
buf_frq.append ( "! Definitions for VLBA stations" )
buf_frq.append ( "!" )
#
# --- Add records related to VLBA stations
#
for sta in vlba_sta_list:
    for line in buf_frq_inp:
        if ( "VLBA" in line ):
             buf_frq.append ( line.replace("VLBA",sta) )
        else:
             buf_frq.append ( line )

buf_frq.append ( "!" )
buf_frq.append ( "! Definitions for HRTR stations" )
buf_frq.append ( "!" )

#
# --- Add records related to GNSS stations
#
for sta in sta_list:
    for line in buf_frq_inp:
        if ( "note" in line ): continue
        if ( "VLBA" in line ):
             buf_frq.append ( line.replace("VLBA",sta + "   ") )
        else:
             buf_frq.append ( line )

#
# --- Write the output frequency definition file
#
(ret,err) = write_file ( buf_frq, fil_frq_out )
check_err_exe ( ret, err, "write_file" )
