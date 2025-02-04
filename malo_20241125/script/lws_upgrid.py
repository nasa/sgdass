#!/usr/bin/python3
import sys, os, shutil, time, subprocess

if ( sys.version[:3] < "3.0" ): print ( "This script cannot run under Python-2" ); exit ( 1 )

def exe ( command ):
#"""
#    Spawn a supborcess, execute command in the context of the suborpess,
#    wait for its completion and return completion code and results.
#"""
    words = command.split()
    (ret, out) = subprocess.getstatusoutput ( command )
    return ( ret, out.split ( "\n" ) )

#
# ------------------------------------------------------------------------
#
def main():
    if ( len(sys.argv) < 5 ):
         print ( "Usage: filin fills dirout date_beg date_end malo_upgird_exe" )
         exit ( 1 )
    filin = sys.argv[1]
    fills = sys.argv[2]
    dirout = sys.argv[3]
    date_beg = sys.argv[4]
    date_end = sys.argv[5]
    malo_upgrid_exe = sys.argv[6]
#
    id = filin.rfind("/")
    ip = filin[id:].find("_") + id + 1
    file_date = filin[ip:ip+13] 
    file_year = filin[ip:ip+4] 
    idd = filin[1:id].rfind("/") + 2
    sub_dir = filin[idd:id] 
    date_beg_check = date_beg[0:4] + date_beg[5:7] + date_beg[8:10] + date_beg[11:14] 
    date_end_check = date_end[0:4] + date_end[5:7] + date_end[8:10] + date_end[11:14] 
    if ( file_date < date_beg_check  or  file_date > date_end_check ):
         print ( "Skipped " + filin )
         exit  ( 1 )
   
    filout = dirout + "/" + file_year + "/" + sub_dir + "/"  + sub_dir + "_" + \
             file_date + ".heb"

    if ( not os.path.isdir(dirout + "/" + file_year) ):
         os.mkdir ( dirout + "/" + file_year )

    if ( not os.path.isdir(dirout + "/" + file_year + "/" + sub_dir) ):
         os.mkdir ( dirout + "/" + file_year + "/" + sub_dir )

    upgrid_com = malo_upgrid_exe + " " + \
                 filin           + " " + \
                 fills           + " " + \
                 "2"             + " " + \
                 filout          + ".bz2" 

    ( ret, out) = exe ( upgrid_com )
    if ( ret != 0 ):
         print ( "Error in malo_upgrid: ", out )

    print ( "Processed file: ", filout )

if __name__ == "__main__":
    vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    main()
