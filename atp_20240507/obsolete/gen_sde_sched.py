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
#
# -- Check if the arguments are declared appropriately
#
if ( len(sys.argv)-1 < 2 ):
    print ( "Usage: gen_sde_sched master_file_path stn_id")
    exit ( 1 )
else:
    fil_master = sys.argv[1]
    stn_id     = sys.argv[2]
    stn_id = stn_id.lower()
    #
    # -- Does this file exist?
    #
    if not os.path.isfile( fil_master ):
        print ( "Cannot find master file: ", fil_master )
        exit  ( 1 )
    #
    # -- Does this station exist?
    #
    if ( stn_id != "gs" and stn_id != "mg" and stn_id != "k2" ):
        print ( "For stn id we expected: gs, mg, or k2, not ", stn_id )
        exit ( 1 )
#
# -- Open the master file 
#
##NOKH##fmaster = open(fil_master, "r")
#
# -- what is the current date?
#
cur_tim = datetime.datetime.utcnow()
#yyyy.mm.dd
date_now = cur_tim.strftime( "%Y.%m.%d" )
tim_now  = cur_tim.strftime( "%H:%M:%S" )
#
# -- go through master file
#
##NOKH##data = fmaster.readlines()
master_data = read_file ( fil_master )
#
# -- check if this is actually a Master SDE file
#
if ( master_data[0].strip() != "Master SDE file format version 1.0           2024.01.03 NH"):
    print ( "ERROR: ", fil_master, " is not a Master SDE file" )
    exit ( 1 )
#
flag = 0
sde_dir = "/sde/" 
sds_dir_templ = "/sde/stow/"
#
cnt = 0
for line in master_data:
    cnt += 1
    if ( line[0:3].strip() == "SDE" ):
        yr = int( line[15:19].strip() )
        mo = int( line[20:22].strip() )
        dy = int( line[23:25].strip() )
        H  = int( line[39:41].strip() )
        M  = int( line[42:44].strip() )
        S  = int( line[45:47].strip() )
        date = datetime.datetime( yr, mo, dy, H, M, S )
        date_dif = date - cur_tim                            # timedelta
        sec_dif  = date_dif.total_seconds()                  # timedelta in seconds
        int_dif  = int( sec_dif )
        #
        # -- Station id from master file
        #
        stn  = line[60:62].strip()
        stn  = stn.lower()
        #
        # -- The first time that we encounter a date in the future, we generate an sds file for that.
        #
        ############################################
        #######
        #######CHECK FOR IF THE DAY IS OVER A WEEK AWAY, OR IF IT'S TODAY
        ############################################
        if ( int_dif > 0 and stn == stn_id and flag != 1 ):
            flag = 1
            # --
            exp  = line[26:32].strip()
            exp_dir = sde_dir + exp
            exp_sds_fil = exp_dir + "/" + exp + ".sds"
            sds_fil_templ = sds_dir_templ + stn + "x_template.sds"
            #
            # -- Does the template exist?
            #
            if ( not os.path.isfile(sds_fil_templ) ):
                print ( "ERROR: ", sds_fil_templ, " not found")
                exit (1)
            #
            # -- Open the template and edit from it.
            #
            f_templ = open( sds_fil_templ, "r" )
            templ_data = f_templ.readlines()
            #
            # -- Does the directory for this experiment exist? If not, then create it
            #
            if ( os.path.isdir(exp_dir) ):
                #
                # -- Whether or not we have the sds file, we are going to overwrite it.
                #
                f_sds = open( exp_sds_fil, "w")
                # --
                for inp in templ_data:
                    #
                    # -- Extract the lines that are to be edited
                    #
                    if ( inp[0:1].strip() == "@"):
                        ln = len(inp)
                        #
                        # -- Edit the "First version" line
                        #
                        if ( inp[1:16].strip() == "# First version" ):
                            inp = inp[1:17].strip()
                            inp = inp + "  " + date_now.strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "Last updated" line
                        #
                        elif ( inp[1:15].strip() == "# Last updated" ):
                            inp = inp[1:18].strip()
                            inp = inp + " " + date_now.strip() + "_" + tim_now.strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "station" line
                        # -- We are just keeping it the same until a future date
                        #
                        elif ( inp[1:8].strip() == "station" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "exper" line
                        # -- We are just editing the experiment name by taking it from Master file
                        #
                        elif ( inp[1:6].strip() == "exper" ):
                            inp = inp[1:6].strip()
                            inp = inp + "    " + exp + "    1"
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "type" line
                        # -- No changes
                        #
                        elif ( inp[1:5].strip() == "type" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "pi_name" line
                        # -- No changes
                        #
                        elif ( inp[1:8].strip() == "pi_name" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "pi_email" line
                        # -- No changes
                        #
                        elif ( inp[1:9].strip() == "pi_email" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "duration" line
                        # -- No changes
                        #
                        elif ( inp[1:9].strip() == "duration" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "antenna_on" line
                        # -- No changes
                        #
                        elif ( inp[1:11].strip() == "antenna_on" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "start" line
                        #
                        elif ( inp[1:6].strip() == "start" ):
                            syr  = line[15:19].strip()
                            smo  = line[20:22].strip()
                            sdy  = line[23:25].strip()
                            sH   = line[39:41].strip()
                            sM   = line[42:44].strip()
                            sS   = line[45:47].strip()
                            inp  = inp[1:ln].strip()
                            init = inp[0:5]
                            azel = inp[35:45]
                            inp  = init+"         "+syr+"."+smo+"."+sdy+"_"+ sH+":"+sM+":"+sS + "  " + azel
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "antenna_off" line
                        # -- No changes
                        #
                        elif ( inp[1:12].strip() == "antenna_off" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "logon" line
                        # -- No changes
                        #
                        elif ( inp[1:6].strip() == "logon" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "cycle i" line
                        # -- No changes
                        #
                        elif ( inp[1:8].strip() == "cycle i" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "cycle j" line
                        # -- No changes
                        #
                        elif ( inp[4:11].strip() == "cycle j" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "proc" line
                        # -- No changes
                        #
                        elif ( inp[7:11].strip() == "proc" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "end cycle j" line
                        # -- No changes
                        #
                        elif ( inp[4:15].strip() == "end cycle j" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "end cycle i" line
                        # -- No changes
                        #
                        elif ( inp[1:12].strip() == "end cycle i" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "stow" line
                        # -- No changes
                        #
                        elif ( inp[1:5].strip() == "stow" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "First version" line
                        # -- No changes
                        #
                        elif ( inp[1:9].strip() == "freq_h01" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "log=station" line
                        # -- No changes
                        #
                        elif ( inp[1:12].strip() == "log=station" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "logoff" line
                        # -- No changes
                        #
                        elif ( inp[1:7].strip() == "logoff" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")

                    #
                    # -- Extract the lines that are to be copied as is.
                    #
                    else:
                        f_sds.write( inp )

            #
            # --------- The experiment directory does not exist, which implies neither does the file
            #
            else:
                os.mkdir(exp_dir)
                f_sds = open( exp_sds_fil, "w")
                # --
                for inp in templ_data:
                    #
                    # -- Extract the lines that are to be edited
                    #
                    if ( inp[0:1].strip() == "@"):
                        ln = len(inp)
                        #
                        # -- Edit the "First version" line
                        #
                        if ( inp[1:16].strip() == "# First version" ):
                            inp = inp[1:17].strip()
                            inp = inp + "  " + date_now.strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "Last updated" line
                        #
                        elif ( inp[1:15].strip() == "# Last updated" ):
                            inp = inp[1:18].strip()
                            inp = inp + " " + date_now.strip() + "_" + tim_now.strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "station" line
                        # -- We are just keeping it the same until a future date
                        #
                        elif ( inp[1:8].strip() == "station" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "exper" line
                        # -- We are just editing the experiment name by taking it from Master file
                        #
                        elif ( inp[1:6].strip() == "exper" ):
                            inp = inp[1:6].strip()
                            inp = inp + "    " + exp + "    1"
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "type" line
                        # -- No changes
                        #
                        elif ( inp[1:5].strip() == "type" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "pi_name" line
                        # -- No changes
                        #
                        elif ( inp[1:8].strip() == "pi_name" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "pi_email" line
                        # -- No changes
                        #
                        elif ( inp[1:9].strip() == "pi_email" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "duration" line
                        # -- No changes
                        #
                        elif ( inp[1:9].strip() == "duration" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "antenna_on" line
                        # -- No changes
                        #
                        elif ( inp[1:11].strip() == "antenna_on" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "start" line
                        #
                        elif ( inp[1:6].strip() == "start" ):
                            syr  = line[15:19].strip()
                            smo  = line[20:22].strip()
                            sdy  = line[23:25].strip()
                            sH   = line[39:41].strip()
                            sM   = line[42:44].strip()
                            sS   = line[45:47].strip()
                            inp  = inp[1:ln].strip()
                            init = inp[0:5]
                            azel = inp[35:45]
                            inp  = init+"         "+syr+"."+smo+"."+sdy+"_"+ sH+":"+sM+":"+sS + "  " + azel
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "antenna_off" line
                        # -- No changes
                        #
                        elif ( inp[1:12].strip() == "antenna_off" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "logon" line
                        # -- No changes
                        #
                        elif ( inp[1:6].strip() == "logon" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "cycle i" line
                        # -- No changes
                        #
                        elif ( inp[1:8].strip() == "cycle i" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "cycle j" line
                        # -- No changes
                        #
                        elif ( inp[4:11].strip() == "cycle j" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "proc" line
                        # -- No changes
                        #
                        elif ( inp[7:11].strip() == "proc" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "end cycle j" line
                        # -- No changes
                        #
                        elif ( inp[4:15].strip() == "end cycle j" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "end cycle i" line
                        # -- No changes
                        #
                        elif ( inp[1:12].strip() == "end cycle i" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "stow" line
                        # -- No changes
                        #
                        elif ( inp[1:5].strip() == "stow" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "First version" line
                        # -- No changes
                        #
                        elif ( inp[1:9].strip() == "freq_h01" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "log=station" line
                        # -- No changes
                        #
                        elif ( inp[1:12].strip() == "log=station" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")
                        #
                        # -- Edit the "logoff" line
                        # -- No changes
                        #
                        elif ( inp[1:7].strip() == "logoff" ):
                            inp = inp[1:ln].strip()
                            f_sds.write(inp + "\n")

                    #
                    # -- Extract the lines that are to be copied as is.
                    #
                    else:
                        f_sds.write( inp )
            #
            # -- Close the template and sds files
            #
            f_templ.close()
            f_sds.close()
            print ( "Wrote sds file to: ", exp_sds_fil )
            #
            # -- update the date of submission
            #
            lin_prt = line[0:71].strip()
            master_data[cnt-1] = lin_prt + "          " + date_now + " 0"
#
# -- Write the update buffer to the master file
#
(ret,err) = write_file ( master_data, fil_master )
check_err_exe ( ret, err, "write_file" )
#
# -- Get sds file info.
#
sds_fil_info = os.stat(exp_sds_fil)
sds_fil_time_epoch = sds_fil_info.st_mtime
sds_fil_datetime = datetime.datetime.fromtimestamp(sds_fil_time_epoch, pytz.utc)
sds_fil_datetime_str = cur_tim.strftime( "%Y.%m.%d_%H:%M:%S" )
#
# -- update file for list of sds that have been generated so far.
#
##################################################################################
############                                                          ############
############ Need to go through file and update the line for this sds ############
############                                                          ############
##################################################################################
sds_list_fil = stn_id + "x_sds_list.txt"
sds_list_fil = sds_dir_templ + sds_list_fil
# --
f_list = open ( sds_list_fil, "a")
f_list.write( sds_fil_datetime_str + " " + exp_sds_fil + "\n")
f_list.close()




#print( time.gmtime(sec) )                
#print( sds_fil_datetime_str )
