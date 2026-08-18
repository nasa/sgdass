#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program imls_status allows to examine and/or set status of the     *
# *   International Mass Loading Service at one of hosts.                *
# *                                                                      *
# *   Usage: imls_status [host] [operation] [action]                     *
# *                                                                      *
# *   When called without parameters, it shows the status                *
# *   "running", "exporting", "aws" at all hosts.                        *
# *                                                                      *
# *   Operations: one of "running", "exporting", "aws".                  *
# *                                                                      *
# *   action: one of "on" or "off"                                       *
# *                                                                      *
# *   Copyright (c) 1975-2025 United States Government as represented by *
# *   the Administrator of the National Aeronautics and Space            *
# *   Administration. All Rights Reserved.                               *
# *   License: NASA Open Source Software Agreement (NOSA).               *
# *                                                                      *
# *  ### 20-JUN-2025  imls_status  v1.2 (d)  L. Petrov  09-JUL-2025 ###  *
# *                                                                      *
# ************************************************************************
import sys, os, socket, shutil, time, subprocess, datetime, math
sys.path.append('/auto')
from    pet_misc  import *

imls_dict = { "sa" : { "host"        : "gs61a-sagitta.ndc.nasa.gov",     \
                       "host_name"   : "gs61a-sagitta.ndc.nasa.gov",     \
                       "run_file"    : "/imls/logs/sagitta.stop",        \
                       "run_file"    : "/imls/logs/sagitta.stop",        \
                       "export_file" : "/imls/logs/sagitta_export.stop", \
                       "aws_file"    : "/imls/logs/aws.stop",            \
                       "run"    : None, \
                       "export" : None, \
                       "aws"    : None  \
                     }, \
              "de" : { "host"        : "gs61a-geodev-a",                  \
                       "host_name"   : "gs61a-geodev-a.gsfc.nasa.gov",    \
                       "run_file"    : "/imls/logs/deva.stop",            \
                       "export_file" : "/imls/logs/deva_export.stop",     \
                       "aws_file"    : "/imls/logs/aws.stop",             \
                       "run"    : None, \
                       "export" : None, \
                       "aws"    : None  \
                     }, \
              "ml" : { "host"   : "earthrotation",                        \
                       "host_name"   : "earthrotation.net",               \
                       "run_file" : "/imls/logs/astrogeo.stop",           \
                       "export_file" : "/imls/logs/astrogeo_export.stop", \
                       "aws_file"    : "/imls/logs/aws.stop",             \
                       "run"    : None, \
                       "export" : None, \
                       "aws"    : None  \
                     }
            }

imls_status_temp_file = "/tmp/imls_temp.txt"

#
# ------------------------------------------------------------------------
#
def check_stop_file ( host, stop_file ):
    """
    Check the specified stop file on a specified host
    """
    if ( host == "localhost" ):
         used_stop_file = stop_file 
    else:
#
# ------ Copy the file from another host to the temporary file
#
         com = "scp -p " + host + ":" + stop_file + " " + imls_status_temp_file
         (ret,out) = exe ( com )
         if ( ret != 0 ):
              for line in out:
                  if ( "No such file or directory" in line ):
                       return ( "runs" )
              else:
                   return ( "unreachable" )

         used_stop_file = imls_status_temp_file
              
#
# --- Check whether the stop file exists
#
    if ( os.path.isfile ( used_stop_file ) ):
#
# ------ Yes, it exists. Then read the stop file
#
         buf = read_file ( used_stop_file )
         if ( host != "localhost" ):
#
# ----------- Remove temporary file
#
              os.unlink ( imls_status_temp_file )
         if ( len(buf) == 0 ):
#
# ----------- The stop file is empty
#
              return ( "runs" )
         elif ( len(buf[0].split()) == 0 ):
#
# ----------- The first line of the stop file is empty
#
              return ( "runs" )
         elif ( len(buf[0].split()) == 1 ):
              if ( buf[0] == "stop" ):
                   return ( "stopped" )
              else:
#
# ---------------- The first line of the stop file is not stop
#
                   return ( "runs" )
         elif ( len(buf[0].split()) == 2 ):
#
# ----------- There are two lines. One of them is stop
#
              if ( buf[0].split()[0] == "stop" ):
                   return ( "stopped since " + buf[0].split()[1] )
              else:
                   return ( "runs" )
         else:
              return ( "runs" )
    else:
         return ( "runs" )

#
# ------------------------------------------------------------------------
#
def set_stop_file ( host, stop_file, action ):
    """
    Execute action for the specified stop file on a specified host
    """

    if ( action == "off" ):
#
# ------ Action off. We need to set the stop file
#
# ------ Get the current date
#
         date_str = str(datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S"))
         buf = []; buf.append ( "stop" + " " + date_str )
         if ( host == "localhost" ):
#
# ----------- Write the stop file
#
              (ret,err) = write_file ( buf, stop_file )
              check_err_exe ( ret, err, "write_file" )
         else:
#
# ----------- Write the temporary file
#
              (ret,err) = write_file ( buf, imls_status_temp_file )
              check_err_exe ( ret, err, "write_file" )
#
# ----------- Copy the temporary file into the remote host
#
              com = "scp -p " + imls_status_temp_file + " " + host + ":" + stop_file
              (ret,err) = exe ( com )
#
# ----------- Remove temporary file
#
              os.unlink ( imls_status_temp_file )
#
              if ( ret != 0 ):
                   print ( "Failure in command " + com )
                   for line in err:
                       print ( line )
                   exit ( 1 )

    elif ( action == "on" ):
#
# ------ Action off. We need to remove the stop file
#
         if ( host == "localhost" ):
              if ( os.path.isfile(stop_file) ):
                   os.unlink ( stop_file )
         else:
              com = 'ssh ' + host + ' "if ( -f ' + stop_file + ' )" rm ' + stop_file 
              (ret,err) = exe ( com )
              if ( ret != 0 ):
                   print ( "Failure in command " + com )
                   for line in err:
                       print ( line )
                   exit ( 1 )

#
# ------------------------------------------------------------------------
#
def main():
    narg=len(sys.argv)-1
    do_host = None
#
# --- Check arguments
#
    if ( narg > 0 and narg != 3 ):
         print ( "Wrong number of arguments" )
         exit  ( 1 )

    if ( narg > 0 ):
         if ( not sys.argv[1] in imls_dict.keys() ):
              print ( "Unknown host %s. Uknown hosts are %s" % ( sys.argv[1], imls_dict.keys() ) )
              exit  ( 1 )
          
         if ( not sys.argv[2] in ("running", "exporting", "aws") ):
              print ( "Unknown operation %s. Uknown operations running, exporting, aws" % \
                       sys.argv[2] )
              exit  ( 1 )
          
         if ( not sys.argv[3] in ("on", "off") ):
              print ( "Unknown action %s. Uknown actions are on and off" % \
                       sys.argv[3] )
              exit  ( 1 )
         do_host = sys.argv[1]
     
#
# --- Get host_perfix
#
    my_host = None
    for host_prefix in imls_dict.keys():
        if ( imls_dict[host_prefix]["host"] == socket.gethostname() ):
             my_host = host_prefix
    if ( not do_host ):
         if ( not my_host ):
              print ( "The current host %s is not in the list of known hosts %s" % \
                       ( os.uname().nodename, imls_dict.keys() ) )
              exit  ( 1 )

         do_host = my_host

    if ( not do_host ):
         print ( "Unknown host %s" % socket.gethostname() )
         exit  ( 1 )

    if ( narg > 0 ):
#
# ------ Execute requested action
#
         if ( do_host == my_host ):
              check_host = "localhost"
         else:
              check_host = do_host
   
         if ( sys.argv[2] == "running" ):
              set_stop_file ( check_host, imls_dict[do_host]["run_file"], sys.argv[3] )
         elif ( sys.argv[2] == "exporting" ): 
              set_stop_file ( check_host, imls_dict[do_host]["export_file"], sys.argv[3] )
         elif ( sys.argv[2] == "aws" ):
              set_stop_file ( check_host, imls_dict[do_host]["aws_file"], sys.argv[3] )
    
#
# --- Check the status of stop files at the local host
#
    imls_dict[my_host]["run"]    = check_stop_file ( "localhost", imls_dict[do_host]["run_file"] )
    imls_dict[my_host]["export"] = check_stop_file ( "localhost", imls_dict[do_host]["export_file"] )
    imls_dict[my_host]["aws"]    = check_stop_file ( "localhost", imls_dict[do_host]["aws_file"] )

    print ( " " )
    print ( "%s: running:   %s" % ( my_host, imls_dict[my_host]["run"]    ) )
    print ( "%s: exporting: %s" % ( my_host, imls_dict[my_host]["export"] ) )
    print ( "%s: aws:       %s" % ( my_host, imls_dict[my_host]["aws"]    ) )
    print ( " " )

#
# --- Check the status of stop files at the remote hosts
#
    if ( my_host == "sa" ):
         com = "ping -c 2 -q " + imls_dict["de"]["host_name"] + " > /dev/null 2>&1" 
         (ret,err) = exe ( com )
         if ( ret == 0 ):
              imls_dict[do_host]["run"]    = check_stop_file ( "de", imls_dict["de"]["run_file"] )
              imls_dict[do_host]["export"] = check_stop_file ( "de", imls_dict["de"]["export_file"] )
              imls_dict[do_host]["aws"]    = check_stop_file ( "de", imls_dict["de"]["aws_file"] )
         else:
              imls_dict[do_host]["run"]    = "down"
              imls_dict[do_host]["export"] = "down"
              imls_dict[do_host]["aws"]    = "down"
         
         print ( "%s: running:   %s" % ( "de", imls_dict[do_host]["run"]    ) )
         print ( "%s: exporting: %s" % ( "de", imls_dict[do_host]["export"] ) )
         print ( "%s: aws:       %s" % ( "de", imls_dict[do_host]["aws"]    ) )
         print ( " " )

    elif ( my_host == "de" ):    
         com = "ping -c 2 -q " + imls_dict["sa"]["host_name"] + " > /dev/null 2>&1" 
         (ret,err) = exe ( com )
         if ( ret == 0 ):
              imls_dict[do_host]["run"]    = check_stop_file ( "sa", imls_dict["sa"]["run_file"] )
              imls_dict[do_host]["export"] = check_stop_file ( "sa", imls_dict["sa"]["export_file"] )
              imls_dict[do_host]["aws"]    = check_stop_file ( "sa", imls_dict["sa"]["aws_file"] )

         else:
              imls_dict[do_host]["run"]    = "down"
              imls_dict[do_host]["export"] = "down"
              imls_dict[do_host]["aws"]    = "down"
         
         print ( "%s: running:   %s" % ( "sa", imls_dict[do_host]["run"]    ) )
         print ( "%s: exporting: %s" % ( "sa", imls_dict[do_host]["export"] ) )
         print ( "%s: aws:       %s" % ( "sa", imls_dict[do_host]["aws"]    ) )
         print ( " " )

if __name__ == "__main__":
    try:
        if ( int(sys.version.split()[0].split('.')[0]) < 3 ):
             print ( "This script cannot run under Python-2" )
             exit ( 1 )
        elif ( int(sys.version.split()[0].split('.')[1]) < 2 ):
             print ( "This script cannot run under Python older than 3.2. Please upgrade" )
             exit ( 1 )
        main()
    except KeyboardInterrupt:
        print ( "\nimls_status.py: Interrupted" )
        exit ( 1 )
