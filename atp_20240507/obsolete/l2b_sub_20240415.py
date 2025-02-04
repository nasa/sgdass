#!/usr/bin/env python3

import  pwd, sys, os, re, shutil, time, subprocess, datetime
from    pet_misc  import *
from    bnc_scav_update import *
#
def l2b_sub ( exp_type, tim_delt, fil_log ):

    """
    Program logtobnc.py converts an experiment log file to binary
    format as defined in the atp library.
    First the log file is converted to anc, then to binary.
    In the initial version, we assume a fixed destination for all
    files, and user need only declare log file name.
    
    This is equivalent to logtobnc.py as a subroutine

    29-FEB-2024  l2b_sub.py    v1.3 (c) N. Habana
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
       print ("Error: Initial argument can only be 'sde' or 'vlbi'")
       exit(1)
#
    split_log = re.split(r',|/|;| ', fil_log)
    ln  = len( split_log )
    lnt = len( fil_log   )
    if ( ln == 1 and
         (fil_log[lnt-4:lnt]==".log" or fil_log[lnt-3:lnt]==".gz" or fil_log[lnt-4:lnt]==".bz2") ):
       print ( " " )
    else:
       print ( "Error: we expected only a log name, not ", fil_log )
       print ( "File should end with .log or zipped with .gz or .bz2 ")
       exit(1)
#
# -- In the event that fil_log is defined as zipped, then rename it to
#    unzipped, and we will unzip it if we find the actual file.
#
    if ( fil_log[lnt-3:lnt] == ".gz" ):
       fil_log = fil_log[0:lnt-3]
    elif ( fil_log[lnt-4:lnt] == ".bz2" ):
       fil_log = fil_log[0:lnt-4]
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
#
# ------- Go through the log file binary tree for SDE's
#
          islog  = False
          for path, dirs, files in os.walk( dir_sde ):
              for fil in files:
#
# --------------- Length of fil
#
                  lnf = len(fil)
#
# --------------- Check if the fil_log is in fil.
# --------------- N.B: - We should always have fil_log ending with .log
#                        Therefore, if file is zipped, it should be longer.
#                      - Make sure the directory is not "incoming"
#
                  if ( not "incoming" in dirs and fil_log in fil ):
                     islog = True
                     path_log = path + "/" + fil_log
                     dir_log  = path
#
# ------------------ Is the log file zipped
#
                     if ( fil[lnf-4:lnf] == ".bz2" ):
                        com = "cd " + path + "; bzip2 -d " + fil
                        (ret, out) = exe ( com )
                     elif ( fil[lnf-3:lnf] == ".gz" ):
                        com = "cd " + path + "; gzip -d " + fil
                        (ret, out) = exe ( com )
#
# ------------------ Exit the loop
#
                     continue
# 
# --------- Did we find the log file?
#           /sde/ssSSSS/ssSSSS.log
#
          if ( islog ):
             stn_id   = path_log.split("/")[3][0:2]
             exp      = path_log.split("/")[2]
             dir_log  = "/sde/" + exp
             dir_plot_exp = dir_plot + "/" + exp
             dir_plot_stn = dir_plot_exp
             base_nam = exp
          else:
             print ( "ERROR: %s log file not found on /sde path" %(fil_log) )
#muted#             exit(1)
#
# ------ VLBI in sagitta
#
       else:
          dir_fs = "/s0/fs_logs"
          dir_anc_scav =  "/t0/anc/vlbi/scav"
          dir_anc_orig =  "/t0/anc/vlbi/orig"
#
# ------- go through the log file tree to find the log file
# ------- N.B: The path is expected to be /s0/fs_logs/YYYY/XXXXXX/XXXXXXyy.log
#              where XXXXXX is the session name, and yy is the station id.
#
          islog  = False
          for path, dirs, files in os.walk( dir_fs ):
              for fil in files:
#
# --------------- Length of fil
#
                  lnf = len(fil)
#
# --------------- Check if the fil_log is in fil.
# --------------- N.B: We should always have fil_log ending with .log
#                      Therefore, if file is zipped, it should be longer.
#
                  if ( fil_log in fil ):
                     islog = True
                     path_log = path + "/" + fil_log
                     dir_log  = path
#
# ------------------ Is the log file zipped
#
                     if ( fil[lnf-4:lnf] == ".bz2" ):
                        com = "cd " + path + "; bzip2 -d " + fil
                        (ret, out) = exe ( com )
                     elif ( fil[lnf-3:lnf] == ".gz" ):
                        com = "cd " + path + "; gzip -d " + fil
                        (ret, out) = exe ( com )
#
# ------------------ Exit the loop
#
                     continue
#
# ------- Did we find the log file?
#
          if ( islog ):
             exp_yr    = path_log.split("/")[3]
             exp       = path_log.split("/")[4]
             lt        = len(exp)
             stn_id    = path_log.split("/")[5][lt:lt+2]
             base_nam  = path_log.split("/")[5][0:lt+2]
             dir_plot_exp = dir_plot + "/" + exp
             dir_plot_stn = dir_plot_exp   + "/" + stn_id
          else:
             print ( "ERROR: %s log file not found on /q0/fs_logs path" %(fil_log) )
#muted#             exit(1)
#
# -- For crux
#
    elif ( sys_host == "gs61a-crux.gsfc.nasa.gov" or \
           sys_host == "gs61a-crux"               or \
           sys_host == "cx" ):
# ----
       dir_plot     =  "/anc/plots"
#
# ---- SDE on crux
#
       if ( exp_type == "sde" ):
          dir_sde = "/sde"
          dir_anc_orig =  "/anc/sde/orig"
          dir_anc_scav =  "/anc/sde/scav"
#
# ------- Go through the log file binary tree for SDE's
#
          islog  = False
# -------
          for path, dirs, files in os.walk( dir_sde ):
              for fil in files:
#
# --------------- Length of fil
#
                  lnf = len(fil)
#
# --------------- Check if the fil_log is in fil.
# --------------- N.B: - We should always have fil_log ending with .log.
#                        Therefore, if file is zipped, it should be longer.
#                      - Make sure the directory is not "incoming"
#
                  if ( not "incoming" in dirs and fil_log in fil ):
                     islog = True
                     path_log = path + "/" + fil_log
                     dir_log  = path
#
# ------------------ Is the log file zipped
#
                     if ( fil[lnf-4:lnf] == ".bz2" ):
                        com = "cd " + path + "; bzip2 -d " + fil
                        (ret, out) = exe ( com )
                     elif ( fil[lnf-3:lnf] == ".gz" ):
                        com = "cd " + path + "; gzip -d " + fil
                        (ret, out) = exe ( com )
#
# ------------------ Exit the loop
#
                     continue
# 
# --------- Did we find the log file?
#           /sde/ssSSSS/ssSSSS.log
#
          if ( islog ):
             stn_id   = path_log.split("/")[3][0:2]
             exp      = path_log.split("/")[2]
             dir_log  = "/sde/" + exp
             dir_plot_exp = dir_plot + "/" + exp
             dir_plot_stn = dir_plot_exp
             base_nam = exp
          else:
             print ( "ERROR: %s log file not found on /sde path" %(fil_log) )
#muted#             exit(1)
#
# ------ VLBI in crux
#
       else:
          dir_fs = "/q0/fs_logs"
          dir_anc_scav =  "/anc/vlbi/scav"
          dir_anc_orig =  "/anc/vlbi/orig"
#
# ------- go through the log file tree to find the log file
# ------- N.B: The path is expected to be /q0/fs_logs/YYYY/XXXXXX/XXXXXXyy.log
#              where XXXXXX is the session name, and yy is the station id.
#
          islog  = False
          for path, dirs, files in os.walk( dir_fs ):
              for fil in files:
#
# --------------- Length of fil
#
                  lnf = len(fil)
#
# --------------- Check if the fil_log is in fil.
# --------------- N.B: We should always have fil_log ending with .log
#                      Therefore, if file is zipped, it should be longer.
#
                  if ( fil_log in fil ):
                     islog = True
                     path_log = path + "/" + fil_log
                     dir_log  = path
#
# ------------------ Is the log file zipped
#
                     if ( fil[lnf-4:lnf] == ".bz2" ):
                        com = "cd " + path + "; bzip2 -d " + fil
                        (ret, out) = exe ( com )
                     elif ( fil[lnf-3:lnf] == ".gz" ):
                        com = "cd " + path + "; gzip -d " + fil
                        (ret, out) = exe ( com )
#
# ------------------ Exit the loop
#
                     continue
#
# ------- Did we find the log file?
#
          if ( islog ):
             exp_yr    = path_log.split("/")[3]
             exp       = path_log.split("/")[4]
             lt        = len(exp)
             stn_id    = path_log.split("/")[5][lt:lt+2]
             base_nam  = path_log.split("/")[5][0:lt+2]
             dir_plot_exp = dir_plot + "/" + exp
             dir_plot_stn = dir_plot_exp   + "/" + stn_id
          else:
             print ( "ERROR: %s log file not found on /q0/fs_logs path" %(fil_log) )
#muted#             exit(1)
#
# -- System not yet ready
#
    else:
       print("This procedure was return for gs61a-crux and gs61a-sagitta")
       print("Please edit the file paths to match your own, and run it.")
#muted#       exit(1)
#
# -- Define important files
#
    fil_anc   = dir_anc_orig + "/" + base_nam + "_orig.anc"
    fil_ave   = dir_anc_scav + "/" + base_nam + "_scav.anc"
    fil_bnc   = dir_anc_orig + "/" + base_nam + "_orig.bnc"
    fil_lg2nt = dir_anc_orig + "/" + base_nam + "_log2ant.log"
#
# -- generate the anc file
#
    print ( "The exp is ", exp )
    print ( "The stn_id is ", stn_id )
    print ( " " )
# --
    if ( not os.path.isfile( fil_anc) ):
       cur_tim  = datetime.datetime.utcnow()
       date_iso = cur_tim.strftime( "%Y.%m.%d_%H:%M:%S")
       print ( "generating anc file from %s starting: " %(path_log) )
       print ( date_iso )
#
       com = "log2ant -t dat -t met -t tsys -t sefd -t phc -t fmt -u -o " + \
             fil_anc + " " + path_log + " 2> " + fil_lg2nt
       ( ret, out ) =  exe ( com )
#
# ---- did the anc file write successfully?
#
       if ( not os.path.isfile( fil_anc) ):
          print ( " Failed to write anc file to ", fil_anc )
#muted#          exit ( 1 )
       else:
          cur_tim  = datetime.datetime.utcnow()
          date_iso = cur_tim.strftime( "%Y.%m.%d_%H:%M:%S")

          print ( "Succesfully wrote anc file to %s ending: " %(fil_anc) )
          print ( date_iso )
          print ( " " )
#
# -- if the anc file already exists
#
    else:
       print ( "The anc file is already at ", fil_anc )
#
# -- Generate the binary file
#
    if ( not os.path.isfile( fil_bnc) ):
       print ( "Generating binary file for %s %s " %(exp_type, exp) )
#
       com = "anc_to_bnc_sim " + fil_anc + " " + fil_ave + " " + fil_bnc
       ( ret, out ) = exe ( com )
#
# ---- did the bnc file write successfully?
#
       if ( not os.path.isfile( fil_bnc) ):
          print ( " Failed to write bnc file to ", fil_bnc )
#muted#          exit ( 1 )
       else:
          print ( "Succesfully wrote bnc file to %s" %(fil_bnc) )
#
# ------- Has the scav_fil been updated already?
#
          flag_scav = bnc_scav_status ( fil_ave )

          if ( not flag_scav ):
             com = "bnc_scav " + fil_bnc + " " + fil_ave + " " + \
                   exp_type + " " + tim_delt + " 1"
             ( ret, com ) = exe ( com )
#
# --- if the anc file already exists
#
    else:
       print ( "The bnc file is already at %s " %(fil_bnc) )
       print ( "The ave file is already at %s " %(fil_ave) )
#
# ---- Has the scav_fil been updated already?
#
       flag_scav = bnc_scav_status ( fil_ave )
       if ( not flag_scav ):
          com = "bnc_scav " + fil_bnc + " " + fil_ave + " " + \
                exp_type + " " + tim_delt + " 1"
          ( ret, com ) = exe ( com )

    return ( ret, out )
