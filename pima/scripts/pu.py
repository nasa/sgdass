#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   A wrapper program for re-fringing observations that are marked     *
# *   as outliers by VLBI analysis program Solve.                        *
# *                                                                      *
# * ### 11-JAN-2016    pu.py   v 1.0 (c)   L. Petrov   11-JAN-2016  ###  *
# *                                                                      *
# ************************************************************************
"""
import pwd, sys, os, re, shutil, time, subprocess, datetime, operator, signal
sys.path.append("/usr")
import argparse 
import pima_local
from pima_exe import exe, exe_out_log, exe_out_nolog, exe_noout_log, exe_noout_nolog, \
                     pima_signal_handler_term

band_list = [ "l", "s", "c", "x", "u", "k", "q", "w", "d", "b", "e", "f", "m", "y" ]

pf_dir = pima_local.pf_dir

pu__label   = "pu.py (PIMA suppression status update)"
pu__version = "pu.py v 1.0  2016.01.11"

#
# ------------------------------------------------------------------------
#
def pu ( exp, band, dry_run, verb ):
    """
    Routine ps performs re-fringing
    """

    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:22]

    pf_exp_dir = pf_dir + "/" + exp
    pima_cnt   = pf_exp_dir  + "/" + exp + "_" + band + "_pima.cnt"

#
# --- Read pima control file into cnt
#
    with open(pima_cnt) as f:
         cnt = f.readlines()
    f.close()
 
#
# --- Search there for keyword MKDB.DESC_FILE
#
    desc_file     = None
    for line in cnt:
        line.strip("\n")
        if ( line.split()[0] == "MKDB.DESC_FILE:" ):
             desc_file = line.split()[1]

    if ( not desc_file ):
         print ( "pu.py: cannot find the keyword MKDB.DESC_FILE: " \
                 "in control file ", pima_cnt )

#
# --- Read experiment description file
#
    with open(desc_file) as f:
         desc = f.readlines()
    f.close()

#
# --- Search for database name in the experiment description file
#
    db_name = None
    for line in desc:
        line.strip("\n")
        if ( line.split()[0] == "DB_NAME:" ):
             db_name = line.split()[1]

    if ( not db_name ):
         print ( "pu.py: cannot find the keyword DB_NAME: " \
                 "in the description file ", desc_file + \
                 " specified by the keyword MKDB.DESC_FILE: in control file ", \
                 pima_cnt )
         exit ( 1 )

    com = pima_local.gvf_promote_exec + " " + db_name + "_v002.env" 

    if ( dry_run ):
         print ( com )
         exit  ( 0 )

#
# --- Run command for propagation of the suppression status
#
    if ( verb > 1 ):
         print ( com )
         
    (ret, out ) = exe_out_nolog ( com, verb )
    if ( ret == 0 ): 
         print ( "pu.py: suppression status for database", db_name,
                 "has been successfully updated on", date_str )
         exit ( 0 )
    else:
#
# ------ An error has happened
#
         print ( "pu.py: ERROR in an attempt to run command", com )
         exit ( 1 )
#
# ------------------------------------------------------------------------
#
def main():
    """ 
    Parsing arguments
    """
    parser = argparse.ArgumentParser( description=pu__label )
    parser.add_argument('--version', action='version', version=pu__version )

#
# --- General options:
#
    parser.add_argument ( "-v", "--verbosity", action="store", \
                          dest="verb", \
                          default=1, \
                          metavar="value", \
                          type=int, \
                          help="Verbosity level" )

    parser.add_argument ( "-r", "--dry-run", action="store_true", \
                          dest="dry_run", \
                          help="dry run: only shows which actions to be taken" )


    parser.add_argument ( "-H", "--manual", action="store_true", \
                          dest="manual", \
                          help="Prints manual into stdout" )
#
# -- Define positional arguments
#
    parser.add_argument ( "exp", \
                          help="VLBI experiment" )

    parser.add_argument ( "band", \
                          help="Frequency band" )

    parser.add_argument ( "-verb", "--verbosity-level", \
                          dest="verb", \
                          type=int, \
                          default=1, \
                          help="Verbosity level" )

    args = parser.parse_args()
    if ( args.band not in band_list ):
         print ( "ERROR pf.py: band " + args.band + " is not known.\n"  \
                 "The list of known bands: " + ', '.join(band_list) )
         exit ( 1 )

    pf_exp_dir  = pf_dir + "/" + args.exp
    if ( not os.path.isdir(pf_exp_dir) ):
         print ( "ERROR pf.py: directory " + pf_exp_dir + " does not exist.\n" +
                 "Please check experiment, band, and directory " + pf_dir )
         exit ( 1 )

    pima_cnt = pf_exp_dir  + "/" + args.exp + "_" + args.band + "_pima.cnt"
    if ( not os.path.isfile(pima_cnt) ):
         print ( "ERROR pf.py: PIMA control file " + pima_cnt + " does not exist.\n" +
                 "Please check experiment, band, and directory " + pf_exp_dir  )
         exit ( 1 )

    pu ( args.exp, args.band, args.dry_run, args.verb )

if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTERM, pima_signal_handler_term )
        pima_child_pid = None
        main()
    except KeyboardInterrupt:
        print ( "pu.py: Interrupted" )
        exit ( 1 )
