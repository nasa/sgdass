#!/usr/bin/python3
import sys, os, shutil, time, subprocess, datetime, signal
sys.path.append('/opt64/bin')
from pima_exe import exe, exe_out_log, exe_out_nolog, exe_noout_log, exe_noout_nolog, \
                     pima_signal_handler_term

SPD_PATH = os.environ['SPD_DIR']
num_proc = 16

#
# ------------------------------------------------------------------------
#
def exe_nolog ( command ):
#"""
#    Spawn a supborcess, execute command in the context of the suborpess,
#    wait for its completion and return completion code and results.
#"""
    words = command.split()
    time_str = str(datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.") + "%6d" % datetime.datetime.now().microsecond).replace( " ", "0" )
    p = subprocess.Popen( command , shell=True)
    ret = os.waitpid(p.pid, 0)
    return ( ret[1] )
#
# ------------------------------------------------------------------------
#
def main():
    if ( len(sys.argv) < 6 ):
         print ( "Usage: spd_all.py fil_cnf dir_heb date_beg date_end spd_pref [ivrb]" )
         exit ( 1 )
    fil_cnf  = sys.argv[1]
    dir_heb  = sys.argv[2]
    date_beg = sys.argv[3]
    date_end = sys.argv[4]
    spd_pref = sys.argv[5]
    if ( len(sys.argv) > 6 ):
         ivrb = int(sys.argv[6])
    else:
         ivrb = 0
#
    if ( len(date_beg) < 11 ): date_beg = date_beg[0:11] + "_00:00"
    if ( len(date_end) < 11 ): date_end = date_end[0:11] + "_00:00"
#
    finam_list = []
    for paths, dirs, files in os.walk(dir_heb):
        for k in range(0,len(files)):
            name = paths + "/" + files[k]
            if ( name.rfind("/d/d_") > 0 and name.rfind(".heb") > 0 ):
                 finam_list.append(name)

    finam_list.sort(reverse=True)    
  
    files_list = []
    files_string = ""
    for i in range(0,len(finam_list)):
        ih = finam_list[i].rfind(".heb")
        date_file = finam_list[i][ih-13:ih-9] + "." + \
                    finam_list[i][ih-9:ih-7] + "." + \
                    finam_list[i][ih-7:ih-2] + ":" + \
                    finam_list[i][ih-2:ih]
        if ( ivrb > 4 ):
             print ( "date_file= ", date_file )
        if ( date_file >= date_beg  and date_file <= date_end ):
             files_list.append(finam_list[i])
             files_string = files_string + " " + finam_list[i] + " "

    il = len(files_string)
    if ( il > 0 ):
         com = "parallel -P " + str(num_proc) + " " + \
                SPD_PATH + "/bin/spd_3d " + fil_cnf + " {} " + \
                spd_pref + " 1 ::: " + \
                files_string 
         if ( ivrb > 3 ):
              print ( "com= ", com )
         fil = "/tmp/spd_com_%05d.csh" % os.getpid()
         f = open ( fil, "w" )
         print ( "setenv OMP_NUM_THREADS 1", file=f )
         print ( com, file=f )
         f.close()    
         ret = os.system ( "/bin/csh -f " + fil )  
#         ret = os.system ( com )  
         if ( ret != 0 ):
              print  ( "ERROR: %d \n" % ret )
         os.remove ( fil )
    else:
         print  ( "Did not find files to process" )

if __name__ == "__main__":
    vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    main()
