#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   A wrapper program for a trial fringing a specified observation     *
# *   and a specified band of a specified experiment.                    *
# *                                                                      *
# * ### 29-DEC-2015    pt.py   v 1.4 (c)   L. Petrov   27-MAY-2021  ###  *
# *                                                                      *
# ************************************************************************
"""
import pwd, sys, os, re, shutil, time, subprocess, datetime, operator, signal
sys.path.append("/usr")
import argparse 
import pima_local
from pima_exe import exe, exe_out_log, exe_out_nolog, exe_noout_log, exe_noout_nolog, \
                     pima_signal_handler_term

pima_exec  = { \
               "dynamic": pima_local.pima_dynamic_exec, \
               "static":  pima_local.pima_static_exec   \
             }
pf_dir = pima_local.pf_dir

pt__label   = "pt.py (PIMA trial fringe)"
pt__version = "pt.py v 1.4  2021.05.27"

band_list = [ "l", "s", "c", "x", "u", "k", "q", "w", "f", "d", "e", "1", "2", "3", "4", "5", "6", "7", "8" ]

#
# ------------------------------------------------------------------------
#
def pf_resolve_envar ( pima_cnt_file, cnt ):

    cnt_out = []
    for line in cnt:
        line.strip("\n")
        if ( len(line.split()) > 1 ):
             val = line.split()[1]
             if ( val[0:1] == '$' ):
                  if ( val[1:2] != "{" ):
                       print ( "Failure in parsing line ", line, \
                               " of the control file " + pima_cnt_file + \
                               " -- enviroment vairalbe should start wiht ${" )
                       exit ( 1 )
                  ib = 2
                  if ( not "}" in val ):
                       print ( "Failure in parsing line ", line, \
                               " of the control file " + pima_cnt_file + \
                               " -- enviroment vairalbe name should be followed by }" )
                       exit ( 1 )
                  ie = val.index("}")
                  env_var = val[ib:ie]
                  val = os.getenv ( env_var )
                  if ( not val ):
                       print ( "Failure in parsing line ", line, \
                               " of the control file " + pima_cnt_file + \
                               " -- enviroment varialbe " + \
                               env_var + " is not defined" )
                       exit ( 1 )
                  line = line.replace("${"+env_var+"}",val)
        cnt_out.append ( line )
    return ( cnt_out )
#
# ------------------------------------------------------------------------
#
def pt ( exp, band, obs, pima_opts, static, dry_run, verb ):
    """
    Routine pt performs a trial fringe fitting
    """

    pf_exp_dir    = pf_dir + "/" + exp
    pima_cnt_file = pf_exp_dir  + "/" + exp + "_" + band + "_pima.cnt"
    fri_file      = "/tmp/1.fri"
    frr_file      = "/tmp/1.frr"

    if ( static ):
         pima_com = pima_exec["static"]  + " " + pima_cnt_file + " " + "frib"
    else:
         pima_com = pima_exec["dynamic"] + " " + pima_cnt_file + " " + "frib"

#
# --- Read pima control file into cnt
#
    with open(pima_cnt_file) as f:
         cnt = f.readlines()
    f.close()
    cnt = pf_resolve_envar ( pima_cnt_file, cnt )

    pima_com = pima_com + " " + \
              "FRINGE_FILE: " + fri_file + " " + \
              "FRIRES_FILE: " + frr_file + " " + \
              "FRIB.2D_FRINGE_PLOT: NO"  + " " + \
              "FRIB.1D_RESTIM_PLOT: XW"  + " " + \
              "FRIB.1D_RESFRQ_PLOT: XW"  + " " + \
              "FRIB.1D_DRF_PLOT: NO"     + " " + \
              "DEBUG_LEVEL: 6"           + " " + \
              "FRIB.OBS: " + obs

    for line in cnt:
        line.strip("\n")
        if ( len(line.split()) < 2 ): continue
        if ( line.split()[0] == "BANDPASS_FILE:" ):
             if ( line.split()[1] != "NO" ) :
                  if ( not os.path.isfile(line.split()[1]) and \
                       not "BANDPASS_FILE:" in pima_opts ):
                       pima_com = pima_com + " " + "BANDPASS_FILE: NO"
                       if ( verb > 0 ):
                            print ( "pt.py: set BANDPASS_FILE: NO" )
        if ( line.split()[0] == "BANDPASS_MASK_FILE:" ):
             if ( line.split()[1] != "NO" ) :
                  if ( not os.path.isfile(line.split()[1]) and \
                       not "BANDPASS_MASK_FILE:" in pima_opts ):
                       pima_com = pima_com + " " + "BANDPASS_MASK_FILE: NO"
                       if ( verb > 0 ):
                            print ( "pt.py: set BANDPASS_MASK_FILE: NO" )
        if ( line.split()[0] == "PCAL_MASK_FILE:" ):
             if ( line.split()[1] != "NO" ) :
                  if ( not os.path.isfile(line.split()[1]) and \
                       not "PCAL_MASK_FILE:" in pima_opts ):
                       pima_com = pima_com + " " + "PCAL_MASK_FILE: NO"
                       if ( verb > 0 ):
                            print ( "pt.py: set PCAL_MASK_FILE: NO" )
        if ( line.split()[0] == "POLARCAL_FILE:" ):
             if ( line.split()[1] != "NO" ) :
                  if ( not os.path.isfile(line.split()[1]) and \
                       not "POLARCAL_FILE:" in pima_opts ):
                       pima_com = pima_com + " " + "POLARCAL_FILE: NO"
                       if ( verb > 0 ):
                            print ( "pt.py: set POLARCAL_FILE: NO" )
        if ( line.split()[0] == "TIME_FLAG_FILE:" ):
             if ( line.split()[1] != "NO" ) :
                  if ( not os.path.isfile(line.split()[1]) and \
                       not "TIME_FLAG_FILE:" in pima_opts ):
                       pima_com = pima_com + " " + "TIME_FLAG_FILE: NO"
                       if ( verb > 0 ):
                            print ( "pt.py: set TIME_FLAG_FILE: NO" )


    if ( len(pima_opts) > 0 ):
#
# ------ Add options to the command line
#
         for opt in pima_opts:
             pima_com = pima_com + " " + opt

    if ( verb > 1 or dry_run ):
         print ( "pt.py: " + pima_com )
    if ( dry_run ): return 0

    (ret, out ) = exe_out_nolog ( pima_com, verb )
    if ( ret != 0 ): 
         print ( "ERROR in running command: " + pima_com )
         exit ( 1 )
    exit ( 0 )

#
# ------------------------------------------------------------------------
#
def main():
    """ 
    Parsing arguments
    """
    parser = argparse.ArgumentParser( description=pt__label )
    parser.add_argument('--version', action='version', version=pt__version )

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

    parser.add_argument ( "-s", "--static", action="store_true", \
                          dest="static", \
                          help="use statically linked pima" )

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

    parser.add_argument ( "obs", \
                          help="Observation index" )

    parser.add_argument ( "pima_opts", nargs=argparse.REMAINDER )

    args = parser.parse_args()
    if ( args.band not in band_list ):
         print ( "ERROR pt.py: band " + args.band + " is not known.\n"  \
                 "The list of known bands: " + ', '.join(band_list) )
         exit ( 1 )

    pf_exp_dir  = pf_dir + "/" + args.exp
    if ( not os.path.isdir(pf_exp_dir) ):
         print ( "ERROR pt.py: directory " + pf_exp_dir + " does not exist.\n" +
                 "Please check experiment, band, and directory " + pf_dir )
         exit ( 1 )

    pima_cnt = pf_exp_dir  + "/" + args.exp + "_" + args.band + "_pima.cnt"
    if ( not os.path.isfile(pima_cnt) ):
         print ( "ERROR ps.py: PIMA control file " + pima_cnt + " does not exist.\n" +
                 "Please check experiment, band, and directory " + pf_exp_dir  )
         exit ( 1 )

    pt ( args.exp, args.band, args.obs, args.pima_opts, args.static, args.dry_run, args.verb )

if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTERM, pima_signal_handler_term )
        pima_child_pid = None
        ps_dir = os.getenv("PIMA_STABLE_DIR")
        if ( ps_dir ):
             pima_exec["static"] = ps_dir + "/bin/pima_static"
        main()
    except KeyboardInterrupt:
        print ( "ps.py: Interrupted" )
        exit ( 1 )
