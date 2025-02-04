#!/usr/bin/python3
# ********************************************************************************
# *                                                                              *
# *   Routine gen_tot_bnc.py                                                     *
# *                                                                              *
# *   This routine generates any binary files that can't be found.               *
# *                                                                              *
# *  ### 29-FEB-2024  gen_tot_bnc.py     v1.0 (c)  N. Habana  29-FEB-2024  ###   *
# *                                                                              *
# ********************************************************************************
#
import pwd, sys, os, re, shutil, time, subprocess, datetime, argparse
import socket
sys.path.append('/home/nhabana/bin')
from  get_logs  import get_local_fslog_list
from  pet_misc  import *
from  l2b_sub   import *
#
# =========================================================================
# ===========                   MAIN PROGRAM                   ============
# =========================================================================
#
def main():
#
# -- define useful parameters
#
    root_dir_vlbi  = "/q0/fs_logs"
    root_dir_sde   = "/sde"
    dir_sde_anc    = "/anc/sde/orig"
    dir_share      = "/progs/atp_20230928/share"
    date_max  =  datetime.datetime.utcnow()
    pid       = os.getpid()
    tim_delt  = "4.0"
#
# -- Output files
#
    fil_vlbi_logs = dir_share  +  "/vlbi_log_file_paths.txt"
    fil_sde_anc   = dir_share  +  "/sde_anc_file_paths.txt"
    sde_anc_lis   = dir_share  +  "/sde_anc_file_names.txt"
    vlbi_log_lis  = dir_share  +  "/vlbi_log_file_names.txt"
    fil_sde_logs  = dir_share  +  "/sde_log_file_paths.txt"
    sde_log_lis   = dir_share  +  "/sde_log_file_names.txt"

#
# -- Get the file paths to all vlbi logs
#
    print ( "started: ", date_max )
    print ( "gen_tot_bnc: getting vlbi log list")
    buf_vlbi_logs =  get_local_fslog_list ( root_dir_vlbi )
#
# -- Get file paths to all sde anc files.
#
#    print ( "gen_tot_bnc: getting sde anc list")
 #   buf_sde_anc = []
  #  buf_sde_anc_lis = []
   # for path, dirs, files in os.walk ( dir_sde_anc ):
    #    for fil in files:
     #       if ".anc" in fil:
      #          buf_sde_anc_lis.append( fil )
       #         buf_sde_anc.append( path + "/" + fil )
#
# -- Split the logs into just files
#
    print ( "gen_tot_bnc: splitting log file paths")
    buf_vlbi_log_names = []
    for paths in buf_vlbi_logs:
        log_nams = paths.split("/")
        ln       = len(log_nams)
        log      = log_nams[ln-1]
        buf_vlbi_log_names.append(log)
# --
    print ( "gen_tot_bnc: writting buf_vlbi_logs to file")
    (ret,err) = write_file ( buf_vlbi_logs, fil_vlbi_logs )
    check_err_exe ( ret, err, "write_file" )
# --
#    print ( "gen_tot_bnc: writting buf_sde_anc to file")
 #   (ret,err) = write_file ( buf_sde_anc, fil_sde_anc )
  #  check_err_exe ( ret, err, "write_file" )
# --
#    print ( "gen_tot_bnc: writting buf_sde_anc_lis to file")
 #   (ret,err) = write_file ( buf_sde_anc_lis, sde_anc_lis )
  #  check_err_exe ( ret, err, "write_file" )
# --
    print ( "gen_tot_bnc: writting buf_vlbi_log_names to file")
    (ret,err) = write_file ( buf_vlbi_log_names, vlbi_log_lis )
    check_err_exe ( ret, err, "write_file" )
#
# -- Generate the binary files for the vlbi logs
#
    print ( "gen_tot_bnc: Generating VLBI anc and bnc files" )
    exp_type = "vlbi"
    lcnt = len(buf_vlbi_log_names)
    icnt = 0
    for log_fil in buf_vlbi_log_names:
        icnt += 1
        print ( log_fil, " ", icnt, " of ", lcnt )
# -----
        chk = ".log.1"
        if ( not chk in log_fil ):
           (ret1, out1) = l2b_sub( exp_type, tim_delt, log_fil )
# ---
    date_end0 =  datetime.datetime.utcnow()
    print ( "gen_tot_bnc: VLBI files finished at ", date_end0 )
#
#-------------------------------------------------------------------------
# -- Generate binary files for the SDE logs
#
#    print ( "gen_tot_bnc: Generating SDE bnc files from anc" )
 #   dir_orig = "/anc/sde/orig"
  #  dir_scav = "/anc/sde/scav"
   # lcnt = len(buf_sde_anc_lis)
    #icnt = 0
#    for anc_fil in buf_sde_anc_lis:
# -----
 #       icnt += 1
  #      print ( anc_fil, " ", icnt, " of ", lcnt )
# -----
   #     exp = anc_fil.split("_")[0]
    #    bnc_fil  = dir_orig + "/" + exp + "_orig.bnc"
     #   scav_fil = dir_scav + "/" + exp + "_scav.anc"
      #  anc_fil  = dir_orig + "/" + anc_fil
# -----
       # com = "anc_to_bnc_sim " + anc_fil + " " + bnc_fil
        #(ret, out) = exe ( com )
# --
    date_end =  datetime.datetime.utcnow()
    print ( "gen_tot_bnc: finished at ",  date_end )
    print ( "PID = ", pid, " runtime: ", date_end - date_max )    
#
# =========================================================================
# ===========              MAIN PROGRAM ASSERTION              ============
# =========================================================================
#
if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
        main()
    except KeyboardInterrupt:
        print ( "\gen_tot_bnc.py: Interrupted" )
        exit ( 1 )

