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
#    fil_sde_anc   = dir_share  +  "/sde_anc_file_paths.txt"
 #   sde_anc_lis   = dir_share  +  "/sde_anc_file_names.txt"
  #  vlbi_log_lis  = dir_share  +  "/vlbi_log_file_names.txt"
    fil_sde_logs  = dir_share  +  "/sde_log_file_paths.txt"
   # sde_log_lis   = dir_share  +  "/sde_log_file_names.txt"
#
# -- Get the file paths to all vlbi logs
#
    print ( "started: ", date_max )
    print ( "gen_tot_bnc: getting vlbi log list")
    buf_vlbi_logs =  get_local_fslog_list ( root_dir_vlbi )
#
# -- Split the logs into just files
#
#~~#    print ( "gen_tot_bnc: splitting vlbi log file paths")
#~~#    buf_vlbi_log_names = []
#~~#    for paths in buf_vlbi_logs:
#~~#        log_nams = paths.split("/")
#~~#        ln       = len(log_nams)
#~~#        log      = log_nams[ln-1]
#~~#        buf_vlbi_log_names.append(log)
# --
#~~#    print ( "gen_tot_bnc: writting buf_vlbi_logs to file")
#~~#    (ret,err) = write_file ( buf_vlbi_logs, fil_vlbi_logs )
#~~#    check_err_exe ( ret, err, "write_file" )
# --
#~~#    print ( "gen_tot_bnc: writting buf_vlbi_log_names to file")
#~~#    (ret,err) = write_file ( buf_vlbi_log_names, vlbi_log_lis )
#~~#    check_err_exe ( ret, err, "write_file" )
#
# -- Generate the binary files for the vlbi logs
#
#~~#    print ( "gen_tot_bnc: Generating VLBI anc and bnc files" )
#~~#    exp_type = "vlbi"
#~~#    lcnt = len(buf_vlbi_log_names)
#~~#    icnt = 0
#~~#    for log_fil in buf_vlbi_log_names:
#~~#        icnt += 1
#~~#        print ( log_fil, " ", icnt, " of ", lcnt )
# -----
#~~#        chk = ".log.1"
#~~#        if ( not chk in log_fil ):
#~~#           (ret1, out1) = l2b_sub( exp_type, tim_delt, log_fil )
# ---
#~~#    date_end0 =  datetime.datetime.utcnow()
#~~#    print ( "gen_tot_bnc: VLBI files finished at ", date_end0 )
#
#-------------------------------------------------------------------------
#
# -- Get the file paths to all sde logs
#
    print ( "started: ", date_max )
    print ( "gen_tot_bnc: getting sde log list")
    buf_sde_logs =  get_local_fslog_list ( root_dir_sde )
#
# -- Split the logs into just files
#
    print ( "gen_tot_bnc: splitting sde log file paths")
    buf_sde_log_names = []
    for paths in buf_sde_logs:
        log_nams = paths.split("/")
        ln       = len(log_nams)
        log      = log_nams[ln-1]
        buf_sde_log_names.append(log)
# --
    print ( "gen_tot_bnc: writting buf_sde_logs to file: ", fil_sde_logs )
    (ret,err) = write_file ( buf_sde_logs, fil_sde_logs )
    check_err_exe ( ret, err, "write_file" )

# --
    print ( "gen_tot_bnc: writting buf_sde_log_names to file: ", sde_log_lis)
    (ret,err) = write_file ( buf_sde_log_names, sde_log_lis )
    check_err_exe ( ret, err, "write_file" )
#
# -- Generate the binary files for the sde logs
#
    print ( "gen_tot_bnc: Generating SDE anc and bnc files" )
    exp_type = "sde"
    lcnt = len(buf_sde_log_names)
    icnt = 0
    for log_fil in buf_sde_log_names:
        icnt += 1
        print ( log_fil, " ", icnt, " of ", lcnt )
# -----
        chk1 = "log2ant"
        chk2 = "point"
        chk3 = "gain"
        chk4 = "short"
        chk5 = "#"
        chk6 = "old"
        chk7 = "orig_"
        chk8 = "incoming"
# -----        
        if ( not chk1 in log_fil and \
             not chk2 in log_fil and \
             not chk3 in log_fil and \
             not chk4 in log_fil and \
             not chk5 in log_fil and \
             not chk6 in log_fil and \
             not chk7 in log_fil and \
             not chk8 in log_fil        ):
           ( ret1, out1 ) = l2b_sub( exp_type, tim_delt, log_fil )
# --------
           date_end0 =  datetime.datetime.utcnow()
           print ( "gen_tot_bnc: SDE files finished at ", date_end0 )

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

