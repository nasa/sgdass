#!/usr/bin/python3
# ********************************************************************************
# *                                                                              *
# *   Routine write_log_paths_to_file.py                                                     *
# *                                                                              *
# *   This routine generates any binary files that can't be found.               *
# *                                                                              *
# *  ### 29-FEB-2024  write_log_paths_to_file.py     v1.0 (c)  N. Habana  29-FEB-2024  ###   *
# *                                                                              *
# ********************************************************************************
#
import pwd, sys, os, re, shutil, time, subprocess, datetime, argparse
import socket
sys.path.append('/home/nhabana/bin')
from  get_logs  import get_local_fslog_list
from  pet_misc  import *
#
# =========================================================================
# ===========                   MAIN PROGRAM                   ============
# =========================================================================
#
def main():
#
# -- Read the screen prompts for the 
#

    if ( len(sys.argv)-1 < 2 ):
       print ( "Usage: write_log_paths_to_file.py sde|vlbi tim_delt " )
       exit ( 1 )
    else:
       exp_type  = sys.argv[1]; exp_type = exp_type.lower()
       tim_delt  = sys.argv[2]; tim_delt = tim_delt.lower()
#
# -- Verify that these are the variables you expect
#
    if ( not exp_type == "sde" and not exp_type == "vlbi" ):
       print ( "write_log_paths_to_file Error: ", +                                \
               " Initial argument can only be 'sde' or 'vlbi'" )
       exit(1)
#
# -- Define early useful parameters
#
#########################################################
#### LATER ACCOUNT FOR SYSTEM, THIS IS FOR CRUX ONLY ####
#########################################################
    dir_share      = "/progs/atp_20230928/share"
    date_start  =  datetime.datetime.utcnow()
    pid       = os.getpid()
    cpid      = str(pid)
# --
    if ( exp_type == "sde" ):
       root_dir_log = "/sde"
       fil_logs     = dir_share  +  "/sde_log_file_paths.txt"
       
    else:
       root_dir_log = "/q0/fs_logs"
       fil_logs     = dir_share  +  "/vlbi_log_file_paths.txt"
#
# -- Get the file paths to all exp_type logs
#
    print ( "started: ", date_start )
    print ( "write_log_paths_to_file: getting log list")
    buf_logs =  get_local_fslog_list ( root_dir_log )
# --
    print ( "write_log_paths_to_file: writting log list to file")
    (ret,err) = write_file ( buf_logs, fil_logs )
    check_err_exe ( ret, err, "write_file" )

# --
    date_end =  datetime.datetime.utcnow()
    print ( "Wrote log paths to: ", fil_logs )
    print ( "write_log_paths_to_file: finished at ",  date_end )
    print ( "PID = ", pid, " runtime: ", date_end - date_start )    
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
        print ( "\write_log_paths_to_file.py: Interrupted" )
        exit ( 1 )

