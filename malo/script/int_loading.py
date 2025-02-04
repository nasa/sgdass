#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program  computes the integrated loading time      *
# *   series from the series of gridded loadings.                        *
# *                                                                      *
# *   Program int_loading.py takes as the argument configuration       *
# *   file that determines the land-sea mask and input/output            *
# *   directories. int_loading.py.py walks through input directories   *
# *   with gridded loadings and computes the intergated loading for      *
# *   each epoch.                                                        *
# *                                                                      *
# *   The intergrated loading is computed for 4 cases:                   *
# *   1) integrated vertical loading over the entire Earth;              *
# *   2) integrated vertical loading over the land;                      *
# *   3) integrated vertical loading over the ocean;                     *
# *   4) integrated vertical loading over the the ocean at latitiudes    *
# *      by nodule less than 66 deg.                                     *
# *                                                                      *
# * ### 16-APR-2015 int_loading.py v1.1 (c) L. Petrov  19-MAY-2017 ### *
# *                                                                      *
# ************************************************************************
import sys, os, shutil, time, subprocess, signal
import optparse 
import math
import datetime
from   datetime import timedelta
from   Message          import *
from   geos_oper_config import *

#
# --- Get MALO share, script, and bin directories
#

malo_script_dir = os.popen("malo_inq script").read().rstrip()
malo_share_dir  = os.popen("malo_inq share").read().rstrip()
malo_bin_dir    = os.popen("malo_inq bin_static").read().rstrip()

class config_class:
   def __init__ ( self, filename ):
       self.filename          = filename

#
# ------------------------------------------------------------------------
#
def exe ( command ):
#"""
#    Spawn a supborcess, execute command in the context of the suborpess,
#    wait for its completion and return completion code and results.
#"""
    words = command.split()
    (ret, out) = subprocess.getstatusoutput ( command )
    return ( ret, out.split ( "\n" ) )
#
# ---------------------------------------------------------
#
def proc_load ( config, load_date, ivrb ):
#"""
#    Computes loading integrals.
#"""

    filin = config.load_grid_dir + "/" + config.load_grid_pref + load_date + '.nc.bz2' 
    if ( not os.path.isfile(filin) ):
         Message ( "E", "Input file " + filin + " does not exist" )
         exit ( 1 )

    com_int = config.loading_integral_exe + " " + \
              "1 " + \
              filin + " " + \
              config.aam_ls_mask + " > " + \
              config.load_int_dir  + "/" + config.load_grid_pref + load_date + '.txt'

    if ( ivrb > 1 ): print ( "com_int: " + com_int )
    (ret, out) = exe ( com_int )
    if ( ret != 0 ):
         Message ( "E", "Error an attempt to compute loading intergral" )
         print (  "\n".join(out) )
         exit  ( 1 )

    filin = config.load_d1_grid_dir + "/" + config.load_d1_grid_pref + load_date + '.nc.bz2' 
    if ( not os.path.isfile(filin) ):
         Message ( "E", "Input file " + filin + " does not exist" )
         exit ( 1 )

    com_d1_int = config.loading_integral_exe + " " + \
                 "1 " + \
                 filin + " " + \
                 config.aam_ls_mask + " > " + \
                 config.load_d1_int_dir  + "/" + config.load_d1_grid_pref + load_date + '.txt'

    if ( ivrb > 1 ): print ( "com_d1_int: " + com_d1_int )
    (ret, out) = exe ( com_d1_int )
    if ( ret != 0 ):
         Message ( "E", "Error an attempt to compute d1 loading intergral" )
         print (  "\n".join(out) )
         exit ( 1 )


#
# ---------------------------------------------------------
#
def main():
    if ( len(sys.argv) < 3 ):
         print ( "Usage: int_loading.py config_file load_date ivrb" )
         exit ( 1 )

    fil_conf  = sys.argv[1]
    load_date = sys.argv[2]
    ivrb      = int(sys.argv[3])

    config = config_class ( fil_conf ) 

    parse_geos_oper_config ( config )

    if ( len(load_date) != 13 ):
         Message ( "E", "Wrong load_date argument: its length is not 13" )
         exit ( 1 )
    if ( load_date[8:9] != '_' ):
         Message ( "E", "Wrong load_date argument: its 9th character is not _" )
         exit ( 1 )

    try:
         if ( int(load_date[0:8]) < 19700101 or int(load_date[0:8]) > 20481231 ):
              Message ( "E", "Wrong load_date argument: date is not in range [1970101, 20491231]" )
              exit ( 1 )
    except:
         Message ( "E", "Wrong load_date argument" )
         exit ( 1 )

    try:
         if ( int(load_date[9:13]) < 0 or int(load_date[9:13]) > 2359 ):
              Message ( "E", "Wrong load_date argument: time is not in range [0000, 2359]" )
              exit ( 1 )
    except:
         Message ( "E", "Wrong load_date argument" )
         exit ( 1 )

    proc_load ( config, load_date, ivrb )

if __name__ == "__main__":
    vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
    main()
