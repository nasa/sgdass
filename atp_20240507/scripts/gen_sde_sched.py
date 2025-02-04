# -*- coding: utf-8 -*-
"""
Program gen_sde_sched.py generates the .sds file and subsequently
the .snp and .prc files for a single dish experiment.
In the initial iteration of this routine we assume that the 
files will be in stow, and we are editing based on those templates.

18-JAN-2024   gen_sde_sched.py v1.0 (c) N. Habana
"""
#
# Routines to import
#
import  pwd, sys, os, re, shutil, time, subprocess, datetime
import os
import pytz
from pathlib import Path
from pet_misc import *
from get_relative_snp import *
#
# -- Check if the arguments are declared appropriately
#
if ( len(sys.argv)-1 < 3 ):
    print ( "Usage: gen_sde_sched  stn_id  sds_temp_file  master_file ")
    exit ( 1 )
else:
    stn_id        = sys.argv[1];  stn_id        = stn_id.lower()
    sds_fil_templ = sys.argv[2];  sds_fil_templ = sds_fil_templ.lower()
    fil_master    = sys.argv[3];  fil_master    = fil_master.lower()
    #
    # -- Does this station exist?
    #
    if ( stn_id != "gs" and stn_id != "mg" and stn_id != "k2" ):
       print ( "ERROR: For stn id we expected: gs, mg, or k2, not ", stn_id )
       exit ( 1 )
    #
    # -- In case the sds_fil_templ is entered as a path to the file
    #
    fil_templ = sds_fil_templ.split("/")
    ln        = len(fil_templ)
    if ( ln > 1 ):
       print ( "ERROR: Please only enter the template file name, not the path" )
       exit ( 1 )
    stn_templ  = sds_fil_templ[0:2]
    #
    # -- Does the template file station match the input?
    #
    if ( stn_templ != stn_id ):
       print("ERROR: The stn id %s doesn't match that of the template file %s" %(stn_id, stn_temp))
       exit ( 1 )
    #
    # -- Does the template file exist where we expect it?
    #
    sds_dir_templ = "/sde/stow"
    sds_fil_templ = sds_dir_templ + "/" + sds_fil_templ 
    is_templ      = os.path.isfile(  sds_fil_templ )
    if ( not is_templ ):
       print ( "ERROR: We did not find template file ", sds_fil_templ )
       exit ( 1 )
    #
    # -- In case the fil_master is entered as a path to the file
    #
    lnm = len( fil_master.split("/") )
    if ( lnm > 1 ):
       print ( "ERROR: Please only enter the master file name, not the path" )
       exit ( 1 )
    #
    # -- Does the master file exist where we expect it?
    # %%%%%%%%%%%  FIX THIS AFTER TESTING TO /sde/sched
    #dir_master = "/f2/anc/sde"
    dir_master = "/sde/sched"
    fil_master = dir_master + "/" + fil_master
    is_master  =  os.path.isfile( fil_master )
    if ( not is_master ):
       print ( "ERROR: We did not find master file ", fil_master )
       exit ( 1 )
#
# -- What is the current date in UTC
#
cur_tim = datetime.datetime.utcnow()
#
# -- Convert to string with Format YYYY.mm.dd & HH:MM:SS
#
date_now = cur_tim.strftime( "%Y.%m.%d" )
tim_now  = cur_tim.strftime( "%H:%M:%S" )
#
# -- Read the template file to a buffer
#
templ_data = read_file ( sds_fil_templ )
#
# -- Read the master file to a buffer
#
master_data = read_file ( fil_master )
#
# -- Check if this is actually an approapriate Master SDE file
#
if ( master_data[0] != "Master SDE file format version 1.0           2024.01.03 NH"):
   print ( "ERROR: ", fil_master, " is not a Master SDE file" )
   exit ( 1 )
#
# -- Go through the Master file
#
flag    = 0
sde_dir = "/sde"
icnt = 0
#
for master_line in master_data:
    icnt += 1
    line = master_line.split()
#
# -- Look at the lines that begin "SDE"
#
    if ( line[0] == "SDE" ):
       #
       # -- Convert the date given in the master file to datetime format
       #
       date_master = line[2]; sdate = date_master.split(".")
       time_master = line[5]; stime = time_master.split(":")
 ##      date_str    = date_master + "_" +  time_master
 ##      date = datetime.datetime.strptime( date_str, "%y.%m.%d_%H:%M:%S" )
       date = datetime.datetime( int(sdate[0]), int(sdate[1]), int(sdate[2]), \
                                 int(stime[0]), int(stime[1]), int(stime[2])   )
       #
       # -- Get the station on this line
       #
       stn = line[7].lower()
       #
       # -- The first time we encounter a date in the future, we generate an sds for that
       #
       if ( date > cur_tim and stn == stn_id and flag != 1 ):
          flag = 1
          # --
          exp         = line[3].lower()
          exp_dir     = sde_dir + "/" + exp
          exp_sds_fil = exp_dir + "/" + exp + ".sds"
          #
          # -- Create the directory for the experiment if it doesn't exist already
          #
          if ( not os.path.isdir ( exp_dir ) ):
             os.mkdir ( exp_dir )
          #
          # -- Whether or not we have the sds file, we are going to overwrite it
          #
          f_sds = []
          jcnt  = 0
          for templ_line in templ_data:
              jcnt += 1
              #
              # -- Extract the lines that are to be edited.
              #
              if ( templ_line[0:1].strip() == "@" ):
                 #
                 # -- remove the "@" symbol
                 #
                 templ_line = templ_line.replace("@", "", 1)
                 #
                 # -- Split the line
                 #
                 inp   = templ_line.split()
                 #
                 # -- Edit the "First version" line
                 #
                 if ( "# First version:" in templ_line ):
                    inp[3]      = date_now
                    f_sds.append ( inp[0] + " "  + inp[1] + " " +       \
                                   inp[2] + "  " + inp[3]            )
                 #
                 # -- Edit the "Last updated" line
                 #
                 elif ( "# Last updated on" in templ_line ):
                    inp[4]      = date_now + "_" + tim_now
                    f_sds.append ( inp[0] + " " + inp[1] + " " +        \
                                   inp[2] + " " + inp[3] + " " + inp[4] )
                 #
                 # -- Edit the "station" line
                 # -- We are just keeping it the same until a future date
                 #
                 elif ( "station" in templ_line ):
                    f_sds.append( templ_line )
                 #
                 # -- Edit the "exper" line
                 # -- We are just editing the experiment name by taking it from Master file
                 #
                 elif ( "exper" in templ_line ):
                    inp[1] = exp
                    f_sds.append( inp[0] + "    "  + inp[1] + "    " + inp[2] )
                 #
                 # -- Edit the "type" line
                 # -- No changes
                 #
                 elif ( "type" in templ_line ):
                    f_sds.append( templ_line )
                 #
                 # -- Edit the "pi_name" line
                 # -- No changes
                 #
                 elif ( "pi_name" in templ_line ):
                    f_sds.append( templ_line )
                 #
                 # -- Edit the "pi_email" line
                 # -- No changes
                 #
                 elif ( "pi_email" in templ_line ):
                    f_sds.append( templ_line )
                 #
                 # -- Edit the "duration" line
                 # -- No changes
                 #
                 elif ( "duration" in templ_line ):
                    f_sds.append( templ_line )
                 #
                 # -- Edit the "antenna_on" line
                 # -- No changes
                 #
                 elif ( "antenna_on" in templ_line ):
                    f_sds.append( templ_line )
                 #
                 # -- Edit the "start" line
                 #
                 elif ( "start" in templ_line):
                    inp[1] = date_master + "_" + time_master 
                    lni = len(inp)
                    lin_prt = ""
                    for i in range(0,lni):
                        lin_prt = lin_prt + inp[i] + " "
                    f_sds.append( lin_prt )
                 #
                 # -- Edit the "antenna_off" line
                 # -- No changes
                 #
                 elif ( "antenna_off" in templ_line ):
                    f_sds.append( templ_line )
                 #
                 # -- Edit the "logon" line
                 # -- No changes
                 #
                 elif ( "logon" in templ_line ):
                    f_sds.append( templ_line )
                 #
                 # -- Edit the "cycle i" line
                 # -- No changes
                 #
                 elif ( "cycle i" in templ_line ):
                    f_sds.append( templ_line )
                 #
                 # -- Edit the "cycle j" line
                 # -- No changes
                 #
                 elif ( "cycle j" in templ_line ):
                    f_sds.append( templ_line )
                 #
                 # -- Edit the "proc" line
                 # -- No changes
                 #
                 elif ( "proc" in templ_line ):
                    f_sds.append( templ_line )
                 #
                 # -- Edit the "stow" line
                 # -- No changes
                 #
                 elif ( "stow" in templ_line ):
                    f_sds.append( templ_line )
                 #
                 # -- Edit the "First version" line
                 # -- No changes
                 #
                 elif ( "freq_h01" in templ_line ):
                    f_sds.append( templ_line )

                 #
                 # -- Edit the "log=station" line
                 # -- No changes
                 #
                 elif ( "log=station" in templ_line ):
                    f_sds.append( templ_line )
                 #
                 # -- Edit the "logoff" line
                 # -- No changes
                 #
                 elif ( "logoff" in templ_line ):
                    f_sds.append( templ_line )
              #
              # -- Extract the lines that are to be copied as is.
              #
              else:
                 f_sds.append( templ_line )
          #
          # -- update the date of submission on master file
          #
          lin_prt = master_line[0:71].strip()
          master_data[icnt-1] = lin_prt + "          " + date_now + " 0"
#
# -- Write the updated master buffer to the master file
#
(ret,err) = write_file ( master_data, fil_master )
check_err_exe ( ret, err, "write_file" )
#
# -- Write the sds buffer to the file
#
(ret,err) = write_file ( f_sds, exp_sds_fil )
check_err_exe ( ret, err, "write_file" )
#
# -- update file for list of sds that have been generated so far.
#
sds_list_fil = stn_id + "x_sds_list.txt"
sds_list_fil = sds_dir_templ + "/" + sds_list_fil
# --
f_list = open ( sds_list_fil, "a")
f_list.write  ( date_now + "_" + tim_now + " " + exp_sds_fil + "\n")
f_list.close()
#
print ( " " )
print ( " Wrote sds file to:     ", exp_sds_fil  )
print ( " Update master file at: ", fil_master   )
print ( " Updated list at:       ", sds_list_fil )
print ( " " )
#
# -- Generate the snp and prc files only if they don't exist
#
exp_snp_fil = exp_dir +  "/" + exp + ".snp"
exp_prc_fil = exp_dir +  "/" + exp + ".prc"
#
com = "sds_to_snap.py " + exp_sds_fil + " " + \
     exp_snp_fil + " " + exp_prc_fil
#
( ret, out ) = exe ( com )
lno = len(out)
for i in range(0,lno):
    print ( out[i] )
print ( " " )
#
# -- For Gs snap files, put the relative time like Katie asked.
#
if ( stn_id == "gs" ):
   f_snp = get_rel_snp ( exp_snp_fil )
   #
   # -- Write the updated snap buffer to the snap file
   #
   (ret,err) = write_file ( f_snp, exp_snp_fil )
   check_err_exe ( ret, err, "write_file" )
#
# -- Send out the experiments to their respective station folders
#
if ( stn_id == "gs" ):
   com = "cd " + exp_dir + ";" +                                        \
         "scp -p " + exp_snp_fil + " " + exp_prc_fil + " " +            \
         "de:/astrogeo/sch/sde/gs/" + sdate[0] + "/"
#
# --
#
   ( ret, out ) = exe ( com )
else:
   com = "cd " + exp_dir + "; /auto/sde_submit.csh " + exp_snp_fil
   ( ret, out ) = exe ( com )
#
#
#
print ( "Did we send out the file??" )
print ( " " )
#print ( "cd " + exp_dir + ";" +                                      \
 #       "scp -p " + exp_snp_fil + " " + exp_prc_fil + " " +          \
  #      "de:/astrogeo/sch/sde/gs/" + sdate[0] + "/" )
#
lno = len(out)
for i in range(0,lno):
    print ( out[i] )
print ("----------------------------------------------------")
