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
#from  l2b_sub   import *
#from  l2b_sub_B import *
from  l2b_sub_C import *

#
# =========================================================================
# ===========                   MAIN PROGRAM                   ============
# =========================================================================
#
def main():
#
# -- define useful parameters
#
    pid       = os.getpid()

    exp_type = "sde"
    tim_delt = "4.0"
#    log_path = "/sde/gsx060/gsx060.log"
 #   ( ret, err ) = l2b_sub_B ( exp_type, tim_delt, log_path, pid )
  #  if ( ret != 0 ): print (err)
    
#    print ("-+_+_+__+_+_+_+_+_")
 #   log_path = "/sde/k20001/pointK2.21.083.log.bz2"
  #  ( ret, err ) = l2b_sub_B ( exp_type, tim_delt, log_path, pid )
   # if ( ret != 0 ): print (err)

#    print ("+_+_+_+_+_+_+_+_+_")
 #   exp_type = "vlbi"
  #  log_path = "/q0/fs_logs/2020/v20240/v20240k2_full.log"
   # ( ret, out ) = l2b_sub_B ( exp_type, tim_delt, log_path, pid )
    #if ( ret != 0 ): print (out)

#    print ("+_+_+_+_+_+_+_+_+_")
 #   log_path = "/q0/fs_logs/2020/t2141/t2141ny.log.1"
  #  ( ret, out ) = l2b_sub_B ( exp_type, tim_delt, log_path, pid )
   # if ( ret != 0 ): print (out)

#    print ("+_+_+_+_+_+_+_+_+_")
 #   log_path = "/q0/fs_logs/2020/v20240/v20240wf.log"
  #  ( ret, out ) = l2b_sub_B ( exp_type, tim_delt, log_path, pid )
   # if ( ret != 0 ): print (out)

#    print ("+_+_+_+_+_+_+_+_+_")
#    exp_type = "vlbi"
 #   log_path = "/q0/fs_logs/2024/v24081/v24081k2_full.log.bz2"
  #  ( ret, out ) = l2b_sub_B ( exp_type, tim_delt, log_path, pid )
   # if ( ret != 0 ): print (out)

#######################################
#1
    print ("+_+_+_+_+_+_+_+_+_")
    exp_type = "vlbi"
    log_path = "/q0/fs_logs/2024/vo4066/vo4066gs_full.log.bz2"
    ( ret, out ) = l2b_sub_C ( exp_type, tim_delt, log_path, pid )
    if ( ret != 0 ): print (out)

    print ("+_+_+_+_+_+_+_+_+_")
    exp_type = "vlbi"
    log_path = "/q0/fs_logs/2024/vo4066/vo4066is.log"
    ( ret, out ) = l2b_sub_C ( exp_type, tim_delt, log_path, pid )
    if ( ret != 0 ): print (out)

    print ("+_+_+_+_+_+_+_+_+_")
    exp_type = "vlbi"
    log_path = "/q0/fs_logs/2024/v24082/v24082k2_full.log.bz2"
    ( ret, out ) = l2b_sub_C ( exp_type, tim_delt, log_path, pid )
    if ( ret != 0 ): print (out)


    

   
    #        wget_year_log = dir_wget + "/wget_%s_%08d.log" % ( str(year), pid )
    
#
# =========================================================================
# ===========              MAIN PROGRAM ASSERTION              ============
# =========================================================================
#
if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ):
           print ( "This script cannot run under Python older than 3.2. Please upgrade" )
           exit ( 1 )
        signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
        main()
    except KeyboardInterrupt:
        print ( "\tester.py: Interrupted" )
        exit ( 1 )

