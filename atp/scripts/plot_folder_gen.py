#!/usr/bin/python3
# ********************************************************************************
# *                                                                              *
# *   Routine plot_folder_gen.py                                                 *
# *                                                                              *
# *   Copyright (c) 1975-2025 United States Government as represented by *
# *   the Administrator of the National Aeronautics and Space            *
# *   Administration. All Rights Reserved.                               *
# *   License: NASA Open Source Software Agreement (NOSA).               *
# *                                                                      *
# *  ### 09-FEB-2024  plot_folder_gen.py  v1.0 (d)  N. Habana  09-FEB-2024  ###  *
# *                                                                              *
# ********************************************************************************
#
import pwd, sys, os, re, shutil, time, subprocess, datetime, argparse
import socket
sys.path.append('/home/nhabana/bin')
from   pet_misc  import *
#
# --
#
if ( len(sys.argv)-1 < 3 ):
    print ( "Usage: plot_folder_gen sde|vlbi year log-file" )
    exit ( 1 )
else:
    exp_type = sys.argv[1]
    exp_type = exp_type.lower()
    exp_yr   = sys.argv[2]
    fil_log  = sys.argv[3]
    fil_log  = fil_log.lower()
#
# -- Verify that these are the variables you expect
#
    if ( not exp_type == "sde" and not exp_type == "vlbi" ):
       print ("Error: Initial argument can only be 'sde' or 'vlbi'")
       exit(1)
#
    int_yr = int(exp_yr)
    today  = datetime.date.today()
    cur_yr = int( today.year )
    if ( int_yr < 2000 or int_yr > cur_yr ):
       print ( int_yr, " is out the range [ 2000, ", cur_yr, "]" )
       exit(1)
#
    split_log = re.split(r',|/|;| ', fil_log)
    ln  = len( split_log )
    lnt = len( fil_log   )
    if ( not ln == 1 or not fil_log[lnt-4:lnt] == ".log" ):
       print ( "Error: we expected only a log name ending with .log, not", fil_log )
       exit(1)
#
# -- Get host name
#
sys_host = os.uname()[1]
#
# -- For sagitta
#
if ( sys_host == "gs61a-sagitta.ndc.nasa.gov" or \
     sys_host == "gs61a-sagitta"              or \
     sys_host == "sa" ):
#
   dir_plot     =  "/t0/anc/plots"
   if ( exp_type == "sde" ):
      dir_anc_orig =  "/t0/anc/sde/orig"
      stn_id   = fil_log[0:2]
#      exp      = fil_log[0:6]
      exp      = fil_log[0:lnt-4]
      dir_log  = "/sde/" + exp
      base_nam = exp
      dir_plot_exp = dir_plot + "/" + exp
      dir_plot_stn = dir_plot_exp
# ---      
   else:
      dir_anc_orig =  "/t0/anc/vlbi/orig"
      stn_id   = fil_log[6:8]
      exp      = fil_log[0:6]
      dir_log  = "/s0/fs_logs/" + exp_yr + "/" + exp
      base_nam = fil_log[0:8]
      dir_plot_exp = dir_plot + "/" + exp
      dir_plot_stn = dir_plot_exp   + "/" + stn_id
#
# -- For crux
#
elif ( sys_host == "gs61a-crux.gsfc.nasa.gov" or \
       sys_host == "gs61a-crux"               or \
       sys_host == "cx" ):
# --
   dir_plot     =  "/anc/plots"
   if ( exp_type == "sde" ):
      dir_anc_orig =  "/anc/sde/orig"
      stn_id   = fil_log[0:2]
#      exp      = fil_log[0:6]
      exp      = fil_log[0:lnt-4]
      dir_log  = "/sde/" + exp
      base_nam = exp
      dir_plot_exp = dir_plot + "/" + exp
      dir_plot_stn = dir_plot_exp
# ---
   else:
      dir_anc_orig =  "/anc/vlbi/orig"
      stn_id   = fil_log[6:8]
      exp      = fil_log[0:6]
      dir_log  = "/q0/fs_logs/" + exp_yr + "/" + exp
      base_nam = fil_log[0:8]
      dir_plot_exp = dir_plot + "/" + exp
      dir_plot_stn = dir_plot_exp   + "/" + stn_id
#
# -- For astrogeo-laptop
#
elif ( sys_host == "astrogeo-laptop" or \
       sys_host == "al" ):
# --
   dir_plot     =  "/anc/plots"
   if ( exp_type == "sde" ):
      dir_anc_orig =  "/anc/sde/orig"
      stn_id   = fil_log[0:2]
      exp      = fil_log[0:6]
      dir_log  = "/sde/" + exp
      base_nam = exp
      dir_plot_exp = dir_plot + "/" + exp
      dir_plot_stn = dir_plot_exp
# ---
   else:
      dir_anc_orig =  "/anc/vlbi/orig"
      stn_id   = fil_log[6:8]
      exp      = fil_log[0:6]
      dir_log  = "/q0/fs_logs/" + exp_yr + "/" + exp
      base_nam = fil_log[0:8]
      dir_plot_exp = dir_plot + "/" + exp
      dir_plot_stn = dir_plot_exp   + "/" + stn_id
else:
    print("This procedure was written for gs61a-crux and gs61a-sagitta")
    print("Please edit the file paths to match your own, and run it.")
#
# -- Check whether the log file exists
#
path_log = dir_log + "/" + fil_log
if ( not os.path.isfile( path_log ) ):
   print ( "ERROR: log file ", path_log, " was not found" )
   print ( "       Check if the experiment type and year are correct" )
   exit (1)
#
# -- Check the binary file
#
fil_bnc = dir_anc_orig + "/" + base_nam + "_orig.bnc"
#
if ( not os.path.isfile( fil_bnc ) ):
   print ( "ERROR: Binary file ", fil_bnc, " was not found." )
   print ( "       Ergo, unlikely plots where generated (yet)." )
   exit (1)
#
dir_ampl      = dir_plot_stn + "/ampl_phas"
dir_ampl_time = dir_ampl     + "/time"
dir_ampl_spec = dir_ampl     + "/spectr"
#
dir_phas      = dir_plot_stn + "/phas"
dir_phas_time = dir_phas     + "/time"
dir_phas_spec = dir_phas     + "/spectr"
#
dir_tsys      = dir_plot_stn + "/tsys"
dir_tsys_time = dir_tsys     + "/time"
dir_tsys_spec = dir_tsys     + "/spectr"
dir_tsys_azel = dir_tsys     + "/azel"
dir_tsys_azim = dir_tsys     + "/azim"
dir_tsys_elev = dir_tsys     + "/elev"
#
dir_fmgt      = dir_plot_stn + "/fmgt"
dir_fmgt_time = dir_fmgt     + "/time"
#
dir_fmpt      = dir_plot_stn + "/fmpt"
dir_fmpt_time = dir_fmpt     + "/time"
#
# -- Get the list of files in the plots folder, that are not in a subdirectory
#
fil_lis_tot = [ f for f in os.listdir(dir_plot) \
                if os.path.isfile(os.path.join(dir_plot, f) ) ]
#
# -- Get the specific files related to our quest
#
fil_lis = []
for f in fil_lis_tot:
    if ( base_nam in f ):
        fil_lis.append( f )

ln = len(fil_lis)
if ( ln < 1 ):
   print ( "ERROR: No files to move")
   exit(1)
#
# -- Check if the plot directories we defined exist
#
if ( not os.path.isdir(dir_plot_exp) ):
   os.mkdir( dir_plot_exp ) 
#
if ( not os.path.isdir(dir_plot_stn) ):
   os.mkdir( dir_plot_stn ) 
#
if ( not os.path.isdir( dir_ampl ) ):
   os.mkdir( dir_ampl )
   os.mkdir( dir_ampl_time )
   os.mkdir( dir_ampl_spec )
else:
   if ( not os.path.isdir ( dir_ampl_time ) ):
      os.mkdir( dir_ampl_time )
   if ( not os.path.isdir ( dir_ampl_spec ) ):
      os.mkdir( dir_ampl_spec )
#       
if ( not os.path.isdir( dir_phas ) ):
   os.mkdir( dir_phas )
   os.mkdir( dir_phas_time )
   os.mkdir( dir_phas_spec )
else:
   if ( not os.path.isdir ( dir_phas_time ) ):
      os.mkdir( dir_phas_time )
   if ( not os.path.isdir ( dir_phas_spec ) ):
      os.mkdir( dir_phas_spec )
#       
if ( not os.path.isdir( dir_tsys ) ):
   os.mkdir ( dir_tsys )
   os.mkdir ( dir_tsys_time )
   os.mkdir ( dir_tsys_spec )
   os.mkdir ( dir_tsys_azel )
   os.mkdir ( dir_tsys_azim )
   os.mkdir ( dir_tsys_elev )
else:
   if ( not os.path.isdir ( dir_tsys_time ) ):
      os.mkdir( dir_tsys_time )
   if ( not os.path.isdir ( dir_tsys_spec ) ):
      os.mkdir( dir_tsys_spec )
   if ( not os.path.isdir ( dir_tsys_azel ) ):
       os.mkdir ( dir_tsys_azel )
   if ( not os.path.isdir ( dir_tsys_azim ) ):
       os.mkdir ( dir_tsys_azim )
   if ( not os.path.isdir ( dir_tsys_elev ) ):
       os.mkdir ( dir_tsys_elev )
#
if ( not os.path.isdir( dir_fmgt ) ):
   os.mkdir( dir_fmgt )
   os.mkdir( dir_fmgt_time )
else:
   if ( not os.path.isdir ( dir_fmgt_time ) ):
      os.mkdir( dir_fmgt_time )
#
if ( not os.path.isdir( dir_fmpt ) ):
   os.mkdir( dir_fmpt )
   os.mkdir( dir_fmpt_time )
else:
   if ( not os.path.isdir ( dir_fmpt_time ) ):
      os.mkdir( dir_fmpt_time )
#
# -- move the files around
#
for f in fil_lis:
    if ( "_ampl_time_" in f ):
       shutil.move( os.path.join(dir_plot,f), dir_ampl_time )
    elif ( "_ampl_spectr_" in f ):
       shutil.move( os.path.join(dir_plot,f), dir_ampl_spec )
    elif ( "_phas_time_" in f ):
       shutil.move( os.path.join(dir_plot,f), dir_phas_time )
    elif ( "_phas_spectr_" in f ):
       shutil.move( os.path.join(dir_plot,f), dir_phas_spec )
    elif ( "_tsys_time_" in f ):
       shutil.move( os.path.join(dir_plot,f), dir_tsys_time )
    elif ( "_tsys_spectr_" in f ):
       shutil.move( os.path.join(dir_plot,f), dir_tsys_spec )
    elif ( "_tsys_azel_" in f ):
       shutil.move( os.path.join(dir_plot,f), dir_tsys_azel )
    elif ( "_tsys_azim_" in f ):
       shutil.move( os.path.join(dir_plot,f), dir_tsys_azim )
    elif ( "_tsys_elev_" in f ):
       shutil.move( os.path.join(dir_plot,f), dir_tsys_elev )
    elif ( "_fmgt_time_" in f ):
       shutil.move( os.path.join(dir_plot,f), dir_fmgt_time )
    elif ( "_fmpt_time_" in f ):
       shutil.move( os.path.join(dir_plot,f), dir_fmpt_time )
#
# -- Get content list of each directory
#
ls_ampl_time = os.listdir(dir_ampl_time)
ls_ampl_spec = os.listdir(dir_ampl_spec)
ls_phas_time = os.listdir(dir_phas_time)
ls_phas_spec = os.listdir(dir_phas_spec)
ls_tsys_time = os.listdir(dir_tsys_time)
ls_tsys_spec = os.listdir(dir_tsys_spec)
ls_tsys_elev = os.listdir(dir_tsys_elev)
ls_tsys_azim = os.listdir(dir_tsys_azim)
ls_tsys_azel = os.listdir(dir_tsys_azel)
ls_fmgt_time = os.listdir(dir_fmgt_time)
ls_fmpt_time = os.listdir(dir_fmpt_time)
#
# -- Delete any directories that are empty
#
if len(ls_ampl_time) == 0:
   os.rmdir(dir_ampl_time)
if len(ls_ampl_spec) == 0:
   os.rmdir(dir_ampl_spec)
if len(ls_phas_time) == 0:
   os.rmdir(dir_phas_time)
if len(ls_phas_spec) == 0:
   os.rmdir(dir_phas_spec)
if len(ls_tsys_time) == 0:
   os.rmdir(dir_tsys_time)
if len(ls_tsys_spec) == 0:
   os.rmdir(dir_tsys_spec)
if len(ls_tsys_azel) == 0:
   os.rmdir(dir_tsys_azel)
if len(ls_tsys_azim) == 0:
   os.rmdir(dir_tsys_azim)
if len(ls_tsys_elev) == 0:
   os.rmdir(dir_tsys_elev)
if len(ls_fmgt_time) == 0:
   os.rmdir(dir_fmgt_time)
if len(ls_fmpt_time) == 0:
   os.rmdir(dir_fmpt_time)

ls_ampl = os.listdir(dir_ampl)
ls_phas = os.listdir(dir_phas)
ls_tsys = os.listdir(dir_tsys)
ls_fmgt = os.listdir(dir_fmgt)
ls_fmpt = os.listdir(dir_fmpt)

if len(ls_ampl) == 0:
   os.rmdir(dir_ampl)
if len(ls_phas) == 0:
   os.rmdir(dir_phas)
if len(ls_tsys) == 0:
   os.rmdir(dir_tsys)
if len(ls_fmgt) == 0:
   os.rmdir(dir_fmgt)
if len(ls_fmpt) == 0:
   os.rmdir(dir_fmpt)

