#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   Program that 
# *                                                                      *
# * ### 15-MAY-2016  splt_ref.py v 1.1 (c)   L. Petrov   19-MAY-2016  ###  *
# *                                                                      *
# ************************************************************************
"""
import pwd, sys, os, re, shutil, time, subprocess, datetime, operator, signal
sys.path.append("/usr")
import argparse 
import pima_local
from pima_exe import exe, exe_out_log, exe_out_nolog, exe_noout_log, exe_noout_nolog, \
                     pima_print_mes, pima_signal_handler_term
splt_ref__label   = "splt_ref.py"
splt_ref__version = "splt_ref.py v 1.0  2016.05.20"

def splt_ref_exe ( sources, verb ):

    for sou in sources.split(","):
        print ( "sou: ", sou )

    return 0

#
# ------------------------------------------------------------------------
#
def main():
    """ 
    Parsing arguments
    """
    parser = argparse.ArgumentParser( description=splt_ref__label )
    parser.add_argument('--version', action='version', version=splt_ref__version )

#
# --- General options:
#
    parser.add_argument ( "-v", "--verbosity", \
                          action="store", \
                          dest="verb", \
                          default=1, \
                          metavar="value", \
                          type=int, \
                          help="Verbosity level" )

#
# --- Define positional arguments
#
    parser.add_argument ( action="store", \
                          dest="experiment", \
                          metavar="experiment", \
                          help="Experiment name" )
#
# --- Define positional arguments
#
    parser.add_argument ( action="store", \
                          dest="band", \
                          metavar="band", \
                          help="Band" )
#
# --- Define positional arguments
#
    parser.add_argument ( action="store", \
                          dest="sources", \
                          metavar="sources", \
                          help="Source list" )
#
# --- Get and parse options
#
    args = parser.parse_args()


    num_ima = splt_ref_exe ( args.sources, args.verb )

    if ( args.verb > 0 ):
         print ( "imadir.py: generated %d images in directory %s " % \
                  (num_ima, dir) )

#
# ------------------------------------------------------------------------
#
if __name__ == "__main__":
    global pima_child_pid
    try:
        if ( sys.version[:3] < "3.0" ): print ( "This script cannot run under Python-2" ); exit ( 1 )
        if ( sys.version[:3] < "3.2" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTERM, pima_signal_handler_term )
        pima_child_pid = None
        main()
    except KeyboardInterrupt:
        print ( "pf.py: Interrupted" )
        exit ( 1 )
