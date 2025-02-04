#!/usr/bin/env python3

import  pwd, sys, os, re, shutil, time, subprocess, datetime
from    pet_misc  import *
from    bnc_scav_update import *
#
def l2b_sub  ( exp_type, tim_delt, fil_log_path, pid ):

    """
    Program logtobnc.py converts an experiment log file to binary
    format as defined in the atp library.
    First the log file is converted to anc, then to binary.
    In this version, you input the path to the log fil, not just 
    the log file name.
    
    This is akin to logtobnc.py as a subroutine

    25-MAR-2024  l2b_sub.py    v2.1 (c) N. Habana
    """
#
# -- Predefine ret and out just in case
#
    ret = 0
    out = []
#
# -- Verify that these are the variables you expect
#
    if ( not exp_type == "sde" and not exp_type == "vlbi" ):
       out = [ "Error: Initial argument can only be 'sde' or 'vlbi'" ]
       ret = -1
       return ( ret, out )
#
# -- Is the file declared even there?
#
    if ( not os.path.isfile(fil_log_path) ):
       lnp = len(fil_log_path)
       print ( "We didn't find %s" %(fil_log_path) )
#
# ---- maybe you declared the file as zipped with .bz2, but it was
#      unzipped in a previous run of this
#
       if ( ".bz2" in fil_log_path):
          fil_log_path_chk = fil_log_path[0:lnp-4]
          if ( os.path.isfile(fil_log_path_chk) ):
             print (" The declared file, %s is not available" %(fil_log_path) )
             print (" We instead have, and will use, %s" %(fil_log_path_chk) )
             fil_log_path = fil_log_path_chk
             ret = 999
#
# ---- maybe you declared the file as unzipped, but it was zipped with
#      bzp2
#
       else:
          fil_log_path_chk = fil_log_path + ".bz2"
          if ( os.path.isfile(fil_log_path_chk) ):
             print (" The declared %s is not available" %(fil_log_path) )
             print (" We instead have (and will use) %s" %(fil_log_path_chk) )
             fil_log_path = fil_log_path_chk
             ret = 999
#
# ---- maybe you declared the file as zipped with .gz, but it was
#      unzipped in a previous run of this
#
       if ( ".gz" in fil_log_path):
          fil_log_path_chk = fil_log_path[0:lnp-3]
          if ( os.path.isfile(fil_log_path_chk) ):
             print (" The declared %s is not available" %(fil_log_path) )
             print (" We instead have (and will use) %s" %(fil_log_path_chk) )
             fil_log_path = fil_log_path_chk
             ret = 999
#
# ---- maybe you declared the file as unzipped, but it was zipped with
#      gzip
#
       else:
          fil_log_path_chk = fil_log_path + ".gz"
          if ( os.path.isfile(fil_log_path_chk) ):
             print (" The declared %s is not available" %(fil_log_path) )
             print (" We instead have (and will use) %s" %(fil_log_path_chk) )
             fil_log_path = fil_log_path_chk
             ret = 999
# ----     
       if ( ret != 999 ):
          out = [ "ERROR: %s (compressed/uncompressed) not found " %(fil_log_path)]
          ret = -1
          return (ret, out)
#
# -- If there, is it the right kind of file?
#
    split_log = fil_log_path.split("/")
    ln        = len ( split_log )
#
# -- We expect the end of the split to be a log file.
#
    fil_log     = split_log[ln-1]
#
# -- What's the character length of what we expect to be a log file?
#
    lnt = len(fil_log)
#
# -- Check if the file is actually a log file
#
    if ( not fil_log[lnt-4:lnt] == ".log"      and \
         not fil_log[lnt-7:lnt] == ".log.gz"   and \
         not fil_log[lnt-8:lnt] == ".log.bz2"       ):
       out = [ "Error: %s is not a log file" %(fil_log_path),
               "File should end with .log or zipped with .gz or .bz2" ]
       ret = -1
       return (ret, out)
#
# -- Get the experiment name
# -- for our file keeping sake, the log file should be in the
#    experiment folder, and if all is well, the experiment name
#    should be in both.
#
    exp = split_log[ln-2]
    if ( not exp in fil_log ):
       ret = -1
       out = [ "Error: Log name and experiment name do not match",   \
               "move log to correct folder path, not ", fil_log_path ]
       return ( ret, out )
#
# -- Get the directory path to the experiment folder.
# -- Recall: it is the folder where the log file lies.
#
    for i in range(ln-1):
        if ( i == 0 ):
           dir_log = "/" + split_log[i]
        else:
           dir_log += split_log[i] + "/"
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
#
# ---- SDE's in sagitta
#
       if ( exp_type == "sde" ):
          dir_sde = "/sde"
          dir_anc_orig =  "/t0/anc/sde/orig"
          dir_anc_scav =  "/t0/anc/sde/scav"
          dir_anc_temp =  "/t0/anc/temp/sde"
#
# ------- Match experiment type to what you input
#
          if ( not "sde" in fil_log_path ):
             out = [ "Error: declared exp_type as SDE", \
                     "but file %s is not in any /sde library" %(fil_log_path) ]
             ret = -1
             return ( ret, out )
#
# ------- The path to SDE logs is /sde/ssSSSS/ssSSSS.log where ss is the
#         station id
#
          stn_id  = fil_log[0:2]
#
# ------- Is the log file zipped, if so, unzip it.
#
          if ( ".log.bz2" in fil_log_path ):
             com = "cd " + dir_log + "; bzip2 -d " + fil_log_path
             (ret, out) = exe ( com )
             if ( ret != 0 ):
                ln_out = len(out)
                for i in range (ln_out):
                    print (out[i])
#
# ---------- rename fil_log to remove the .bz2
#
             else:
                fil_log = fil_log[0:lnt-4]
                lnt = len(fil_log)
# -------
          elif ( ".log.gz" in fil_log_path ):
             com = "cd " + dir_log + "; gzip -d " + fil_log_path
             (ret, out) = exe ( com )
             if ( ret != 0 ):
                ln_out = len(out)
                for i in range (ln_out):
                    print (out[i])
#
# ---------- rename fil_log to remove the .gz
#
             else:
                fil_log = fil_log[0:lnt-3]
                lnt = len(fil_log)
#
# ------- Define some library paths
#
          dir_plot_exp = dir_plot + "/" + exp
          dir_plot_stn = dir_plot_exp
          base_nam     = exp
#
# ---- VLBI in sagitta
#
       else:
          dir_fs = "/s0/fs_logs"
          dir_anc_orig =  "/t0/anc/vlbi/orig"
          dir_anc_scav =  "/t0/anc/vlbi/scav"
          dir_anc_temp = "/t0/anc/temp/vlbi"
#
# ------- Match experiment type to what you input
#
          if ( not "/s0/fs_logs" in fil_log_path ):
             out = [ "Error: declared exp_type as VLBI", \
                     "but file is not in any /s0/fs_logs library" ]
             ret = -1
             return ( ret, out )
#
# ------- If the file is compressed, then decompress it
#
          if ( ".log.bz2" in fil_log_path ):
             com = "cd " + dir_log + "; bzip2 -d " + fil_log_path
             (ret, out) = exe ( com )
             if ( ret != 0 ):
                ln_out = len(out)
                for i in range (ln_out):
                    print (out[i])
                return ( ret, out )
#
# ---------- rename fil_log to remove the .bz2
#
             else:
                fil_log = fil_log[0:lnt-4]
                lnt = len(fil_log)
# -------
          elif ( ".log.gz" in fil_log_path ):
             com = "cd " + dir_log + "; gzip -d " + fil_log_path
             (ret, out) = exe ( com )
             if ( ret != 0 ):
                ln_out = len(out)
                for i in range (ln_out):
                    print (out[i])
                return ( ret, out )
#
# ---------- rename fil_log to remove the .gz
#
             else:
                fil_log = fil_log[0:lnt-3]
                lnt = len(fil_log)
#
# ------- The path to VLBI logs is typically /t0/fs_logs/YYYY/XXXXXX/XXXXXXyy.log 
#         where XXXXXX is the session name, and yy is the station id.
#         N.B: - some log files may be XXXXXXyy_full.log
#
          if ( "_full" in fil_log ):
             stn_id   = fil_log[lnt-11:lnt-9]
             base_nam = fil_log[0:lnt-9]
          else:
             stn_id = fil_log[lnt-6:lnt-4]
             base_nam = fil_log[0:lnt-4]
#
# ------- Define some variables
#
          exp_yr       = split_log[3]
          dir_plot_exp = dir_plot + "/" + exp
          dir_plot_stn = dir_plot_exp + "/" + stn_id
#
# -- For Crux
#
    elif ( sys_host == "gs61a-crux.gsfc.nasa.gov" or \
           sys_host == "gs61a-crux"               or \
           sys_host == "cx" ):
#
# ---- Plot directory
#
       dir_plot = "/anc/plot"
#
# ---- SDE in Crux
#
       if ( exp_type == "sde" ):
          dir_sde = "/sde"
          dir_anc_orig =  "/anc/sde/orig"
          dir_anc_scav =  "/anc/sde/scav"
#
# ------- The value of the temp directory, depends on whether we have
#         access to the NVME fast directories
#
          if ( os.path.isdir("/f2/anc/temp/sde") ):
             dir_anc_temp = "/f2/anc/temp/sde"
          else:
             dir_anc_temp = "/anc/temp/sde"
#
# ------- Match experiment type to what you input
#
          if ( not "sde" in fil_log_path ):
             out = [ "Error: declared exp_type as SDE", \
                     "but file is not in any /sde library" ]
             ret = -1
             return ( ret, out )
#
# ------- The path to SDE logs is /sde/ssSSSS/ssSSSS.log where ss is the
#         station id
#
          stn_id  = fil_log[0:2]
#
# ------- Is the log file zipped, if so, unzip it.
#
          if ( ".log.bz2" in fil_log_path ):
             com = "cd " + dir_log + "; bzip2 -d " + fil_log_path
             (ret, out) = exe ( com )
#
# ---------- failed to unzip file
#
             if ( ret != 0 ):
                ln_out = len(out)
                for i in range (ln_out):
                    print ( out[i] )
#
# ---------- successfully unzipped. rename fil_log to remove the .bz2
#
             else:
                fil_log = fil_log[0:lnt-4]
                lnt = len(fil_log)
# -------
          elif ( ".log.gz" in fil_log_path ):
             com = "cd " + dir_log + "; gzip -d " + fil_log_path
             (ret, out) = exe ( com )
# ----------
             if ( ret != 0 ):
                ln_out = len(out)
                for i in range (ln_out):
                    print (out[i])
#
# ---------- successfully uncompressed; rename fil_log to remove the .gz
#
             else:
                fil_log = fil_log[0:lnt-3]
                lnt = len(fil_log)
#
# ------- Define some library paths
#
          dir_plot_exp = dir_plot + "/" + exp
          dir_plot_stn = dir_plot_exp
          base_nam     = exp
#
# ---- VLBI in crux
#
       else:
          dir_fs = "/q0/fs_logs"
          dir_anc_orig =  "/anc/vlbi/orig"
          dir_anc_scav =  "/anc/vlbi/scav"
#
# ------- The value of the temp directory, depends on whether we have
#         access to the NVME fast directories
#
          if ( os.path.isdir("/f2/anc/temp/vlbi") ):
             dir_anc_temp = "/f2/anc/temp/vlbi"
          else:
             dir_anc_temp = "/anc/temp/vlbi"
#
# ------- Match experiment type to what you input
#
          if ( not "/q0/fs_logs" in fil_log_path ):
             out = [ "Error: declared exp_type as VLBI", \
                     "but file is not in any /q0/fs_logs library" ]
             ret = -1
             return ( ret, out )
#
# ------- If the file is compressed, then decompress it
#
          if ( ".log.bz2" in fil_log_path ):
             print ("%s was compressed with bzip2")
             com = "cd " + dir_log + "; bzip2 -d " + fil_log_path
             (ret, out) = exe ( com )
             if ( ret != 0 ):
                ln_out = len(out)
                for i in range (ln_out):
                    print (out[i])
                return ( ret, out )
#
# ---------- rename fil_log to remove the .bz2
#
             else:
                fil_log = fil_log[0:lnt-4]
                lnt = len(fil_log)
# -------
          elif ( ".log.gz" in fil_log_path ):
             print ("%s was compressed with gzip")
             com = "cd " + dir_log + "; gzip -d " + fil_log_path
             (ret, out) = exe ( com )
             if ( ret != 0 ):
                ln_out = len(out)
                for i in range (ln_out):
                    print (out[i])
                return ( ret, out )
#
# ---------- rename fil_log to remove the .gz
#
             else:
                fil_log = fil_log[0:lnt-3]
                lnt = len(fil_log)
#
# ------- The path to VLBI logs is typically /q0/fs_logs/YYYY/XXXXXX/XXXXXXyy.log 
#         where XXXXXX is the session name, and yy is the station id.
#         N.B: - some log files may be XXXXXXyy_full.log
#
          if ( "_full" in fil_log ):
             stn_id   = fil_log[lnt-11:lnt-9]
             base_nam = fil_log[0:lnt-9]
          else:
             stn_id   = fil_log[lnt-6:lnt-4]
             base_nam = fil_log[0:lnt-4]
#
# ------- Define some variables
#
          exp_yr       = split_log[3]
          dir_plot_exp = dir_plot + "/" + exp
          dir_plot_stn = dir_plot_exp + "/" + stn_id
#
# -- System not yet ready
#
    else:
       out = ["This procedure was written for gs61a-crux and gs61a-sagitta",
              "Please edit the file paths to match your own, and run it." ]
       ret = -1
       return ( ret, out )
#
# -- Define important files
#
    fil_anc     = dir_anc_orig + "/" + base_nam + "_orig.anc"
    fil_anc_bz2 = dir_anc_orig + "/" + base_nam + "_orig.anc.bz2"
    fil_ave     = dir_anc_scav + "/" + base_nam + "_scav.anc"
    fil_bnc     = dir_anc_orig + "/" + base_nam + "_orig.bnc"
    fil_lg2nt   = dir_anc_orig + "/" + base_nam + "_log2ant.log"
# --
    fil_anc_temp   = dir_anc_temp + "/" + base_nam + "_orig.anc_%08d" %(pid)
    fil_ave_temp   = dir_anc_temp + "/" + base_nam + "_scav.anc_%08d" %(pid)
    fil_bnc_temp   = dir_anc_temp + "/" + base_nam + "_orig.bnc_%08d" %(pid)
    fil_lg2nt_temp = dir_anc_temp + "/" + base_nam + "_log2ant.log_%08d" %(pid)
#
# -- generate the anc file
# -- First we do it in the temporary folder, then when it's done,
#    move it to the writeful directory.
#
    print ( "The exp is ", exp, " of type ", exp_type )
    print ( "The stn_id is ", stn_id )
    print ( "pid is ", pid)
    print ( " " )
#
# -- Does the anc file exist?
#
    if ( not os.path.isfile(fil_anc) and not os.path.isfile(fil_anc_bz2) ):
# ----
       cur_tim  = datetime.datetime.utcnow()
       date_iso1 = cur_tim.strftime( "%Y.%m.%d_%H:%M:%S")
       print ( "generating anc file from %s starting: " %(fil_log_path) )
       print ( date_iso1 )
#
       com = "log2ant -t dat -t met -t tsys -t sefd -t phc -t fmt -u -o " + \
             fil_anc_temp + " " + fil_log_path + " 2> " + fil_lg2nt_temp
       ( ret, out ) =  exe ( com )
#
# ---- Did the temporary anc file write successfully?
#
       if ( ret != 0 ):
          ln_out = len(out)
          for i in range (ln_out):
              print (out[i])
          return ( ret, out )
       else:
          com = "mv " + fil_lg2nt_temp + " " +  fil_lg2nt
          ( ret, out ) = exe ( com )
#
# ---- we wrote the temporary anc file.
#      lets check for the binary files and the scav files
#
       cur_tim  = datetime.datetime.utcnow()
       date_iso2 = cur_tim.strftime( "%Y.%m.%d_%H:%M:%S")
       print ( "Succesfully wrote temp anc file to %s " %(fil_anc_temp) )
       print ( date_iso2 )
       print ( " " )
       print ( "checking for binary files")
#
# ---- is the binary file already there?
#
       if ( not os.path.isfile( fil_bnc) ):
          print ( " binary file not found " )
          print ( " generating temp binary file %s" %(fil_bnc_temp) )
#
# ------- Does the reduced data file exist?
#
          if ( not os.path.isfile( fil_ave ) ):
             print ("Also generating %s" %(fil_ave_temp) ) 
             com = "anc_to_bnc_sim " + fil_anc_temp + " " + fil_ave_temp + " " + fil_bnc_temp
             ( ret, out ) = exe ( com )
             if ( ret != 0 ):
                print ( "Failed to generate binary file and base reduced data file")
                ln_out = len(out)
                for i in range (ln_out):
                    print (out[i])
# -------
          else:
             print ( "%s already exists, no temp file generated " %(fil_ave) )
             com = "anc_to_bnc_sim " + fil_anc_temp + " " + fil_bnc_temp
             ( ret, out ) = exe ( com )
             if ( ret != 0 ):
                print ("Failed to generate temp bnc file, but %s is there" %(fil_ave))
                ln_out = len(out)
                for i in range (ln_out):
                    print (out[i])
#
# ------- If we failed to generate the binary file
#
          if ( not os.path.isfile(fil_bnc_temp) ):
#
# ---------- Move temporary anc file to permanent folder
#
             print ( "Failed to generate %s" %(fil_bnc_temp) )
             print ( "Moving %s to %s" %(fil_anc_temp, fil_anc) )
             com = "mv " + fil_anc_temp + " " + fil_anc
             ( ret, out ) = exe ( com )
             if ( ret != 0 ):
                ln_out = len(out)
                for i in range (ln_out):
                    print (out[i])
                ret = 0
#
# ---------- zip the anc file
#
             else:
                print ( "Compress %s with bzip2" %(fil_anc) )
                com = "bzip2 " + fil_anc
                ( ret, out ) = exe ( com )
                if ( ret != 0 ):
                   ln_out = len(out)
                   for i in range (ln_out):
                       print (out[i])
                   ret = 0
# ----------
             if ( ret == 0 ):
                out = [ "ERROR: Failed to write binary file %s" %(fil_bnc_temp) ]
                ret = -1
                return ( ret, out )
#
# ------- if we successfully wrote the binary file
#
          else:
             cur_tim  = datetime.datetime.utcnow()
             date_iso3 = cur_tim.strftime( "%Y.%m.%d_%H:%M:%S")
             print ( "Succesfully wrote temp bnc file to %s " %(fil_bnc_temp) )
             print ( date_iso3 )
             print ( " " )
#
# ---------- Did we need to generate an average file with the binary file?
#
             if ( not os.path.isfile(fil_ave_temp) ):
#
# ------------- Check if the scav file we have has been updated already
#
                print ( "We did not generated temp ave file." )
                print ( "Is %s available: " %(fil_ave) )
                flag_ave = os.path.isfile(fil_ave)
                print ( flag_ave )
                print ( "Is it up to date: " )
                flag_scav = bnc_scav_status ( fil_ave )
                print ( flag_scav )
                if ( flag_ave and not flag_scav ):
                   print ( "Updating %s with %s" %(fil_ave, fil_bnc_temp) )
# ----------------
                   com = "bnc_scav " + fil_bnc_temp + " " + fil_ave + " " + \
                          exp_type + " " + tim_delt + " 1"
                   ( ret, out ) = exe ( com )
                   if ( ret != 0 ):
                      ln_out = len(out)
                      for i in range (ln_out):
                          print (out[i])
                      print ( "Failed to update %s " %(fil_ave) )
                      print ( "but we are still continuing with other tasks" )
# ----------------
                   else:
                      cur_tim  = datetime.datetime.utcnow()
                      date_iso4 = cur_tim.strftime( "%Y.%m.%d_%H:%M:%S")
                      print ( "Updated scan_ave file %s " %(fil_ave) )
                      print ( date_iso4 )
                      print ( " " )
# -------------
                print ( "Moving temp anc file to final destination " )
                com = "mv " + fil_anc_temp + " " + fil_anc
                ( ret, out ) = exe ( com )
                if ( ret != 0 ):
                   print ( " failed to move %s to %s" %(fil_anc_temp, fil_anc) )
                else:
                   print ("Compress %s with bzip2" %(fil_anc))
                   com = "bzip2 " + fil_anc
                   ( ret, out ) = exe ( com )
                   if ( ret != 0 ):
                      print ( " failed to bzip2 %s" %(fil_anc) )
# -------------
                print ( "Moving temp bnc file to final destination " )
                com = "mv " + fil_bnc_temp + " " + fil_bnc
                ( ret, out ) = exe ( com )
                if ( ret != 0 ):
                   print ( " failed to move %s to %s" %(fil_bnc_temp, fil_bnc) )
#
# ---------- We have in hand fil_anc_temp, fil_bnc_temp, and fil_ave_temp (unupdated)
#
             else:
                print ("We generated %s, now let us update it" %(fil_ave_temp) )
#
# ------------- Update fil_ave_temp
#
                com = "bnc_scav " + fil_bnc_temp + " " + fil_ave_temp + " " + \
                      exp_type + " " + tim_delt + " 1"
                ( ret, out ) = exe ( com )
# -------------
                if ( ret == 0 ):
                   cur_tim  = datetime.datetime.utcnow()
                   date_iso5 = cur_tim.strftime( "%Y.%m.%d_%H:%M:%S")
                   print ( "Updated scan_ave file %s " %(fil_ave_temp) )
                   print ( date_iso5 )
                   print ( " " )
                else:
                   print ( "Failed to update %s " %(fil_ave_temp) )
                   print ( "but we are still continuing with other tasks" )
# -------------
                print ( "Moving %s to %s " %(fil_anc_temp, fil_anc) )
                com = "mv " + fil_anc_temp + " " + fil_anc
                ( ret, out ) = exe ( com )
                if ( ret != 0 ):
                   print ( " failed to move %s to %s" %(fil_anc_temp, fil_anc) )
                else:
                   print ("Compress %s with bzip2" %(fil_anc))
                   com = "bzip2 " + fil_anc
                   ( ret, out ) = exe ( com )
                   if ( ret != 0 ):
                      print ( " failed to bzip2 %s" %(fil_anc) )
# -------------
                print ( "Moving %s to %s " %(fil_bnc_temp, fil_bnc) )
                com = "mv " + fil_bnc_temp + " " + fil_bnc
                ( ret, out ) = exe ( com )
                if ( ret != 0 ):
                   print ( " failed to move %s to %s" %(fil_bnc_temp, fil_bnc) )
# -------------
                print ( "Moving %s to %s " %(fil_ave_temp, fil_ave) )
                com = "mv " + fil_ave_temp + " " + fil_ave
                ( ret, out ) = exe ( com )
                if ( ret != 0 ):
                   print ( " failed to move %s to %s" %(fil_ave_temp, fil_ave) )
#
# --- The anc file already exists, but what about the bnc and scav files
#
    else:
#
# ---- If we don't have the compressed file, then we use the regular file
#
       if ( not os.path.isfile( fil_anc_bz2) ):
          print ( "The anc file is already at %s " %(fil_anc) )
          is_bzp = False
       else:
          print ( "The anc file is already at %s " %(fil_anc_bz2) )
          is_bzp = True
#
# ---- If we don't have a bnc file?
#
       print ( "Checking for binary file %s" %(fil_bnc) ) 
       if ( not os.path.isfile(fil_bnc) ):
          print ( "%s not found" %(fil_bnc) ) 
#
# ------- Generate the bnc file from the existing compressed anc file
#
          if ( is_bzp ):
             cur_tim  = datetime.datetime.utcnow()
             date_iso1 = cur_tim.strftime( "%Y.%m.%d_%H:%M:%S")
             print ( "generating temp bnc file from %s starting: " %(fil_anc_bz2) )
             print ( date_iso1 )
#
# ---------- Does the reduced data file exist?
#
             if ( not os.path.isfile( fil_ave) ):
                print ("Also generating %s" %(fil_ave_temp) )
                com = "anc_to_bnc " + fil_anc_bz2 + " " + fil_ave_temp + " " + fil_bnc_temp
                ( ret, out ) = exe ( com )
                if ( ret != 0 ):
                   ln_out = len(out)
                   for i in range (ln_out):
                       print (out[i])
# ----------
             else:
                print ("%s already exists, so no temp reduced file" %(fil_ave) )
                com = "anc_to_bnc " + fil_anc_bz2 + " " + fil_bnc_temp
                ( ret, out ) = exe ( com )
                if ( ret != 0 ):
                   ln_out = len(out)
                   for i in range (ln_out):
                       print (out[i])
#
# ------- Generate the bnc file from the existing anc file
#
          else:
             cur_tim  = datetime.datetime.utcnow()
             date_iso1 = cur_tim.strftime( "%Y.%m.%d_%H:%M:%S")
             print ( "generating temp bnc file from %s starting: " %(fil_anc) )
             print ( date_iso1 )
#
# ---------- Does the reduced data file exist?
#
             if ( not os.path.isfile( fil_ave) ):
                print ("Also generating %s" %(fil_ave_temp) )
                com = "anc_to_bnc_sim " + fil_anc + " " + fil_ave_temp + " " + fil_bnc_temp
                ( ret, out ) = exe ( com )
                if ( ret != 0 ):
                   ln_out = len(out)
                   for i in range (ln_out):
                       print (out[i])
             else:
                print ("%s already exists, so no temp reduced file" %(fil_ave) )
                com = "anc_to_bnc_sim " + fil_anc + " " + fil_bnc_temp
                ( ret, out ) = exe ( com )
                if ( ret != 0 ):
                   ln_out = len(out)
                   for i in range (ln_out):
                       print (out[i])
#
# ------- If we failed to generate the binary file then report an error 
#         message.
#
          if ( not os.path.isfile(fil_bnc_temp) ):
             out = [ "ERROR: Failed to write binary file %s" %(fil_bnc_temp) ]
             ret = -1
             return ( ret, out )
#
# ------- If the binary file wrote successfully
#
          else:
             cur_tim  = datetime.datetime.utcnow()
             date_iso1 = cur_tim.strftime( "%Y.%m.%d_%H:%M:%S")
             print ( "successfully wrote %s: " %(fil_bnc_temp) )
             print ( date_iso1 )
#
# ---------- Did we need to generate an average file with the binary file?
#
             if ( not os.path.isfile(fil_ave_temp) ):
#
# ------------- Check if the scav file we have has been updated already
#
                print ( "We did not generate temp ave file, %s" %(fil_ave_temp) )
                print ( "Is %s available: " %(fil_ave) )
                flag_ave = os.path.isfile(fil_ave)
                print ( flag_ave )
                print ( "Is it up to date: " )
                flag_scav = bnc_scav_status ( fil_ave )
                print ( flag_scav )
# -------------
                if ( flag_ave and not flag_scav ):
# ----------------
                   com = "bnc_scav " + fil_bnc_temp + " " + fil_ave + " " + \
                         exp_type + " " + tim_delt + " 1"
                   ( ret, com ) = exe ( com )
# ----------------
                   if ( ret == 0 ):
                      cur_tim  = datetime.datetime.utcnow()
                      date_iso4 = cur_tim.strftime( "%Y.%m.%d_%H:%M:%S")
                      print ( "Updated scan_ave file %s " %(fil_ave) )
                      print ( date_iso4 )
                      print ( " " )
                   else:
                      print ( "Failed to update %s " %(fil_ave) )
                      print ( "but we are still continuing with other tasks" )
# -------------
                else:
                   print(  "%s is up to date" %(fil_ave) ) 
                   print ( "Moving %s to %s " %(fil_bnc_temp, fil_bnc) )
                   com = "mv " + fil_bnc_temp + " " + fil_bnc
                   ( ret, out ) = exe ( com )
                   if ( ret != 0 ):
                      print ( "failed to move %s to %s" %(fil_bnc_temp, fil_bnc) )
#
# ---------- We had to generate an average file with the binary file.
#
             else:
                print ( "Generated %s" %(fil_ave_temp) )
                print ( "Updating it" )
#
# ------------- Update fil_ave_temp
#
                com = "bnc_scav " + fil_bnc_temp + " " + fil_ave_temp + " " + \
                      exp_type + " " + tim_delt + " 1"
                ( ret, com ) = exe ( com )
# -------------
                if ( ret == 0 ):
                   cur_tim  = datetime.datetime.utcnow()
                   date_iso5 = cur_tim.strftime( "%Y.%m.%d_%H:%M:%S")
                   print ( "Updated scan_ave file %s " %(fil_ave_temp) )
                   print ( date_iso5 )
                   print ( " " )
#
# ---------------- Move the generated reduced file
#
                   com = "mv " + fil_ave_temp + " " + fil_ave
                   ( ret, out ) = exe ( com )
                   if ( ret != 0 ):
                      print ( " failed to move %s to %s" %(fil_ave_temp, fil_ave) )
# -------------
                else:
                   print ( "Failed to update %s " %(fil_ave_temp) )
                   print ( "but we are still continuing with other tasks" )
# -------------
                print ( "Moving  %s to %s " %(fil_bnc_temp, fil_bnc) )
                com = "mv " + fil_bnc_temp + " " + fil_bnc
                ( ret, out ) = exe ( com )
                if ( ret != 0 ):
                   print ( " failed to move %s to %s" %(fil_bnc_temp, fil_bnc) )
# -------------
                print ( "Moving %s to %s" %(fil_ave_temp, fil_ave) )
                com = "mv " + fil_ave_temp + " " + fil_ave
                ( ret, out ) = exe ( com )
                if ( ret != 0 ):
                   print ( " failed to move %s to %s" %(fil_ave_temp, fil_ave) )

       else:
          print ( "The bnc file is already at %s " %(fil_bnc) )
          print ( "Checking for reduced data file %s" %(fil_ave) )
#
# ------- Do we have a scan average file?
#
          if ( not os.path.isfile(fil_ave) ):
             out = [" We have bnc and anc file for %s but no %s" %(base_nam, fil_ave)]
             ret = -1
             print (out)
             return (ret, out)
#
# ------- We have the average file, but is it updated?
#
          else:
             print ( "Found %s" %(fil_ave) )
             print ( "Checking if it is up to date" )
             flag_scav = bnc_scav_status ( fil_ave )
             print ("flag_scav: ", flag_scav)
             if ( not flag_scav ):
                print ( "Updating the reduced data file" )
                com = "bnc_scav " + fil_bnc + " " + fil_ave + " " + \
                exp_type + " " + tim_delt + " 1"
                ( ret, com ) = exe ( com )
                if ( ret != 0 ):
                   print ( "Failed to update %s" %(fil_ave) )
                else:
                    print (" Updated %s" %(fil_ave) )
             else:
                print ("%s is already up to date" %(fil_ave) )
#
# ---- If the anc file is not compressed, then compress it.
#
       if ( not is_bzp ):
          com = "bzip2 " + fil_anc
          (ret, out) = exe ( com )
          if ( ret != 0 ):
             print ( "ERROR: failed to bzip2 %s " %(bzip2) )
# -----
    return ( ret, out )
