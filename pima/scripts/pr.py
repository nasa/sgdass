#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   A wrapper program for re-fringing observations that are marked     *
# *   as outliers by VLBI analysis program Solve.                        *
# *                                                                      *
# * ### 29-DEC-2015    pr.py   v 1.4 (c)   L. Petrov   05-JUL-2016  ###  *
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
pf_py_exec = pima_local.pima_dynamic_exec[0:pima_local.pima_dynamic_exec.rfind("pima")] + "pf.py"

pr__label   = "pr.py (PIMA re-fringe)"
pr__version = "pr.py v 1.5  2022.11.29"

#
# --- Default delay window depending on the band
#
delwin_dict = {
               "l": "5.0", \
               "s": "3.0", \
               "c": "2.0", \
               "x": "1.5", \
               "u": "1.0", \
               "k": "1.0", \
               "q": "1.0", \
               "w": "1.0", \
               "d": "2.0", \
               "b": "2.0", \
               "e": "2.0", \
               "f": "2.0", \
               "m": "2.0", \
               "y": "2.0", \
               "1": "3.0", \
               "2": "2.0", \
               "3": "1.5", \
               "4": "1.5"  \
              }

#
# ------------------------------------------------------------------------
#
#  Expand the environment vairable in the name
#
def pima_de_env( finam ):
    if ( len(finam) < 3 ):
         return ( finam )
    elif ( finam[0:2] == "${" ):
         try:
              ie = finam.find("}")
         except:
              return ( finam )
         env_name = finam[2:ie]
         if ( env_name in os.environ ):
              return ( finam.replace("${"+env_name + "}",os.environ[env_name]) )
         else:
              return ( finam )
    else:   
         return ( finam )

#
# ------------------------------------------------------------------------
#
def ps ( exp, band, snr_min, delwin, nodb, nosd, marked_only, static, dry_run, verb ):
    """
    Routine ps performs re-fringing
    """

    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:22]
#
# --- If delwin was not defined explicitely, get default group delay window semi-width
#
    if ( not delwin ): delwin = delwin_dict[band]

    pr_exp_dir = pf_dir + "/" + exp
    pima_cnt   = pr_exp_dir  + "/" + exp + "_" + band + "_pima.cnt"
    log_file   = pr_exp_dir  + "/" + exp + "_" + band + "_samb.log"
    init_spl   = pr_exp_dir  + "/" + exp + "_" + band + "_init.spl"
    pima_refri = pr_exp_dir  + "/" + exp + "_" + band + "_refri.csh"
    fri_file   = pr_exp_dir  + "/" + exp + "_" + band + "_refri.fri"
    frr_file   = pr_exp_dir  + "/" + exp + "_" + band + "_refri.frr"
    mkdb_log   = pr_exp_dir  + "/" + exp + "_mkdb.log"

    if ( not os.path.isfile(init_spl) ):
         print ( "pr.py: Cannot find Solve listing file " + init_spl )

#
# ---  remove stale files with refringe results
#
    if ( os.path.isfile(fri_file) ): os.remove ( fri_file )
    if ( os.path.isfile(frr_file) ): os.remove ( frr_file )

#
# --- Read pima control file into cnt
#
    with open(pima_cnt) as f:
         cnt = f.readlines()
    f.close()
 
#
# --- Search there for keywords MKDB.DESC_FILE, FRINGE_FILE, and FRIRES_FILE
#
    desc_file      = None
    orig_fri_file  = None
    orig_frr_file  = None
    time_flag_file = None
    for line in cnt:
        line = line.strip("\n")
        if ( len(line.split()) < 2 ): continue
        if ( line.split()[0] == "MKDB.DESC_FILE:" ):
             desc_file = line.split()[1]
        if ( line.split()[0] == "FRINGE_FILE:" ):
             orig_fri_file = line.split()[1]
        if ( line.split()[0] == "FRIRES_FILE:" ):
             orig_frr_file = line.split()[1]
        if ( line.split()[0] == "TIME_FLAG_FILE:" ):
             time_flag_file = line.split()[1]

    if ( not desc_file ):
         print ( "pr.py: cannot find the keyword MKDB.DESC_FILE: " \
                 "in control file ", pima_cnt )

    desc_file = pima_de_env( desc_file )
    if ( not os.path.isfile(desc_file) ):
         print ( "pr.py: Cannot find description file " + desc_file + \
                 " specified by the keyword MKDB.DESC_FILE: in control file ", \
                 pima_cnt )

#
# --- Check whether we should setup TIME_FLAG_FILE: NO
#
    notime_flag = False
    if ( time_flag_file == None ):
         notime_flag = True
    else:
         if ( time_flag_file == "NO" or not os.path.isfile ( time_flag_file ) ):
              notime_flag = True
    
    
#
# --- Read experiment description file
#
    desc_file = pima_de_env(desc_file)
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
         print ( "pr.py: cannot find the keyword DB_NAME: " \
                 "in the description file ", desc_file + \
                 " specified by the keyword MKDB.DESC_FILE: in control file ", \
                 pima_cnt )
         print ( "pr.py: cannot find the keyword DB_NAME: " \
                 "in the description file ", desc_file + \
                 " specified by the keyword MKDB.DESC_FILE: in control file ", \
                 pima_cnt, file=log )
         log.flush()
         exit ( 1 )

    if ( not dry_run ):
         log = open ( log_file, "w" )
         print ( " ".join(sys.argv), file = log ) 
         print ( "pr.py: refringing for experiment ", exp, " on ", date_str, file=log )
         print ( "====================================================================", \
                  file=log )
         print ( " ", file=log )
         log.flush()

#
# --- Create command line for SAMB program
#
    if ( marked_only ):
         filter_str = "MARKED_ONLY"
    else:
         filter_str = "ASIS"
    samb_com = pima_local.samb_exec + " " + \
               "-p " + pima_cnt     + " " \
               "-w " + delwin       + " " \
               "-s " + snr_min      + " " \
               "-r " + init_spl     + " " \
               "-f " + filter_str   + " " \
               "-o " + pima_refri
#
# --- Run program samb
#
    if ( dry_run ):
         (ret, out ) = exe_out_log   ( samb_com, verb, log )
    else:
         (ret, out ) = exe_noout_log ( samb_com, verb, log )
    print ( " ", file=log )

    if ( ret != 0  ): 
#
# ------ Error has happened
#
         if ( verb >= 1 ): 
              print ( "pr.py: ERROR in an attempt to process experiment", exp, \
                      "by samb" )
              print ( "pr.py: ERROR in an attempt to process experiment", exp, \
                      "by samb", file=log )
              if ( verb >= 2 ): 
                   print ( "last command:", samb_com )
              print ( "last command:", samb_com, file=log )
              if ( not dry_run ): log.close()
              exit ( 1 )

#
# --- Samb created a file with commands for re-fringing. 
# --- Let us read it. We need to modify these commands a little bit
#
    with open(pima_refri) as f:
         refri = f.readlines()
    f.close()

#
# --- Open re-fringe command file for writing
#
    g = open ( pima_refri, "w" )
    for line in refri:
        line.strip("\n")
#
# ----- Update a) SNR detection limit
# -----        b) Fringe result file
# -----        c) Fringe residual file
# -----        d) pima executable name
#
        if ( static ):
             new_line = line.replace ( "FRIB.SECONDARY_MAX_TRIES: 1",
                                       "FRIB.SNR_DETECTION: " + snr_min + \
                                       " FRINGE_FILE: " + fri_file + \
                                       " FRIRES_FILE: " + frr_file ) \
                            .replace ( "pima ", pima_exec["static"] + " " )
        else:
             new_line = line.replace ( "FRIB.SECONDARY_MAX_TRIES: 1",
                                       "FRIB.SNR_DETECTION: " + snr_min + \
                                       " FRINGE_FILE: " + fri_file + \
                                       " FRIRES_FILE: " + frr_file ) \
                            .replace ( "pima ", pima_exec["dynamic"] + " " )
        if ( notime_flag ):
             new_line = new_line.replace ( "FRIB.SNR_DETECTION:", \
                                           "TIME_FLAG_FILE: NO FRIB.SNR_DETECTION:" )

        g.write ( new_line )
        if ( dry_run ):
             print ( new_line.strip("\n").strip("\r") ) 
            
#
# --- This is a tricky place. For reasons which are beyond me, python
# --- does not executes refri command if it is shorter than 8192 bytes:
# --- it returns core 0 (success) without runnning a subprocess
# --- But when I add this "plug" to the command file, it executes it
#
    g.write ( '#   ' * 2048 )
    g.close 

#
# --- Dry run? Stop here
#
    if ( dry_run ): exit ( 0 )

#
# --- Execute PIMA refringe command file under csh
#
    refri_com = "/bin/csh -f " + pima_refri
    (ret, out ) = exe_noout_log ( refri_com, verb, log )
    print ( " ", file=log )

    if ( ret != 0  ): 
#
# ------ An error has happened
#
         if ( verb >= 1 ): 
              print ( "pr.py: ERROR in an attempt to run refringing command file", \
                      pima_refri, "when experiment", exp, "band", band, \
                      "was re-fringed" )
              print ( "pr.py: ERROR in an attempt to run refringing command file", \
                      pima_refri, "when experiment", exp, "band", band, \
                      "was re-fringed", file=log )
              if ( verb >= 2 ): 
                   print ( "last command:", refri_com )
              print ( "last command:", refri_com, file=log )
              log.close()
              exit ( 1 )

    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:22]
    if ( not os.path.isfile(fri_file) ):
         print ( "pr.py: ERROR -- no results of refringing by command", \
                  pima_refri, "for experiment", exp, "band", band, \
                 "was found" )
         print ( "pr.py: ERROR -- no results of refringing by command", \
                  pima_refri, "for experiment", exp, "band", band, \
                 "was found", file=log )
         log.close()
         exit ( 1 )


#
# --- Read fringe results file
#
    with open(fri_file) as f:
         res_fri = f.readlines()
    f.close()

#
# --- Read fringe residual file
#
    with open(frr_file) as f:
         res_frr = f.readlines()
    f.close()

#
# --- Create a buffer with output new fringe file that will be appended
# --- to the existing fringe file
#
    out_fri = []
    ind_fri = []
#
# --- Just copy 4 lines of the header
#
    for i in range(0,4):
        out_fri.append ( res_fri[i].strip("\n")  )

    for line in res_fri:
        if ( line[0:1] == "#" ): continue
        if ( line.find ( "Sts:               10" ) > 0 ):
#
# ---------- Copy lines that are marked as detections
#
             out_fri.append ( line.strip("\n") )
#
# ---------- Put the index of the detected observation into int_fri list
#
             ind_fri.append ( line.split()[0] )

#
# --- Copy 2 lines of the trailer
#
    for i in range(0,2):
        out_fri.append ( res_fri[len(res_fri)-2+i].strip("\n") )

#
# --- Append new fringe results to the existing results
#
    orig_fri_file = pima_de_env ( orig_fri_file )
    f = open ( orig_fri_file, "a" )
    for line in out_fri:
        print ( line, file=f )
    f.close()

#
# --- Now process fringe residuals
# --- Copy 4 lines of the header
#
    out_frr = []
    for i in range(0,4):
        out_frr.append ( res_frr[i].strip("\n") )

    for line in res_frr:
#
# ----- Copy those residuals from the refringing that have observation
# ----- idices mareked as "detections"
#
        if ( line.split()[0] in ind_fri ):
             out_frr.append ( line.strip("\n") )

#
# --- Copy 2 lines of the trailer
#
    for i in range(0,2):
        out_frr.append ( res_frr[len(res_frr)-2+i].strip("\n") )

#
# --- and appened the new residuals to the end of the existing
# --- fringe residual file
#
    orig_frr_file = pima_de_env(orig_frr_file)
    f = open ( orig_frr_file, "a" )
    for line in out_frr:
        print ( line, file=f )
    f.close()

    if ( nodb ):
#
# ------ No database is needed? Stop here.
#
         if ( verb > 0 ):
              print ( "pr.py Re-fringing is finished for band " + band + \
                      " of experement " + exp + " on " + date_str )
              print ( "pr.py no database is created" )
         print ( "pr.py Re-fringing is finished for band " + band + \
                 " of experement " + exp + " on " + date_str, file=log )
         print ( "pr.py no database is created", file=log )
         print ( "==============================================================", \
                 file=log )
         log.close()
         exit  ( 0 )
    else:
#
# ------ Create a databas anew
#
         if ( static ):
              com_mkdb = pf_py_exec + " " + "-s"
         else:
              com_mkdb = pf_py_exec

         com_mkdb = com_mkdb  + " " + \
                    exp       + " " + \
                    band      + " " + \
                    "mkdb"    + " " + \
                    "-updt"

#
# --- Run command for GVF database creation
#
    (ret, out ) = exe_noout_log ( com_mkdb, verb, log )
    if ( ret == 0 ): 
#
# ------ Read mkdb log file
#
         with open(mkdb_log) as f:
              mkdb_buf = f.readlines()
         f.close()
#
# ------ ... and append it to the current log file
#
         print ( "pr.py Contents of the log file from pima mkdb", file=log )
         print ( " ", file=log )
         for line in mkdb_buf:
             print ( line.strip("\n"), file=log )

         com = pima_local.gvf_promote_exec + " " + db_name + "_v002.env" 
#
# ------ Run command for propagation of the suppression status
#
         (ret, out ) = exe_noout_log ( com, verb, log )
         if ( ret == 0 ): 
              if ( verb > 0 ):
                   print ( "pu.py: suppression status for database", db_name,
                           "has been successfully updated", )
              print ( "pu.py: suppression status for database", db_name,
                      "has been successfully updated", file=log )
         else:
#
# ---------- An error has happened
#
             print ( "pu.py: ERROR in an attempt to run command", com )
             print ( "pu.py: ERROR in an attempt to run command", com, file=log )
             log.close()
             exit ( 1 )

#
# ------ Generate the final success message in the output log file
#
         date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:22]
         if ( verb > 0 ):
              print ( "pr.py: experiment", exp, "band", band, \
                      "has been successfully re-fringed on " + date_str )
              sys.stdout.flush()
         print ( " ", file=log )
         print ( "pr.py: experiment", exp, "band", band, \
                 "has been successfully re-fringed on " + date_str, file=log )
         print ( "==========================================================================================", 
         file=log )
         log.close()
         exit ( 0 )
    else:
#
# ------ An error has happened
#
         if ( verb >= 1 ): 
              print ( "pr.py: ERROR in an attempt to run command", \
                       com_mkdb, "when experiment", exp, "band", band, \
                      "was re-fringed" )
              print ( "pr.py: ERROR in an attempt to run command", \
                       com_mkdb, "when experiment", exp, "band", band, \
                      "was re-fringed", file=log )
              if ( verb >= 2 ): 
                   print ( "last command: ", com_mkdb )
              print ( "last command: ", com_mkdb, file=log )
              log.close()
              exit ( 1 )

#
# ------------------------------------------------------------------------
#
def main():
    """ 
    Parsing arguments
    """
    parser = argparse.ArgumentParser( description=pr__label )
    parser.add_argument('--version', action='version', version=pr__version )

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

    parser.add_argument ( "snr", \
                          help="minimum acceptable SNR" )

    parser.add_argument ( "-delwin", "--delay-window", \
                           dest="delwin", \
                           default=None, \
                           help="Group delay window semi-width" )

    parser.add_argument ( "-nodb", "--do-not-make-database", \
                          dest="nodb", \
                          action="store_true", \
                          default=False, \
                          help="Do not generate the output database" )

    parser.add_argument ( "-nosd", "--no-staging-directory", \
                          dest="nosd", \
                          action="store_true", \
                          default=False, \
                          help="Do not use staging directory" )

    parser.add_argument ( "-mo", "--marked-only", \
                          dest="marked_only", \
                          action="store_true", \
                          default=False, \
                          help="Use only observations marked in the spool file" )

    parser.add_argument ( "-verb", "--verbosity-level", \
                          dest="verb", \
                          type=int, \
                          default=1, \
                          help="Verbosity level" )

    args = parser.parse_args()
    if ( args.band not in delwin_dict.keys() ):
         print ( "ERROR pr.py: band " + args.band + " is not known.\n"  \
                 "The list of known bands: " + ', '.join(delwin_dict.keys()) )
         exit ( 1 )

    pr_exp_dir  = pf_dir + "/" + args.exp
    if ( not os.path.isdir(pr_exp_dir) ):
         print ( "ERROR pr.py: directory " + pr_exp_dir + " does not exist.\n" +
                 "Please check experiment, band, and directory " + pf_dir )
         exit ( 1 )

    pima_cnt = pr_exp_dir  + "/" + args.exp + "_" + args.band + "_pima.cnt"
    if ( not os.path.isfile(pima_cnt) ):
         print ( "ERROR pr.py: PIMA control file " + pima_cnt + " does not exist.\n" +
                 "Please check experiment, band, and directory " + pr_exp_dir  )
         exit ( 1 )

    ps ( args.exp, args.band, args.snr, args.delwin, args.nodb, args.nosd, \
         args.marked_only, args.static, args.dry_run, args.verb )

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
        print ( "pr.py: Interrupted" )
        exit ( 1 )
