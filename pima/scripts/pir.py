#!/usr/bin/python3
# ************************************************************************
# *                                                                      *
# *   Program pir.py executes elements of the VLBI data analysis         *
# *   pipeline. In total there are 16 elements. Elements can be          *
# *   executed separately, or in the group, or all together.             *
# *                                                                      *
# *   usage: pir.py [-h] [--version] [-v verbosity] [-b band]            *
# *                 [-r run-level] [-s] [-o options] experiment          *
# *                                                                      *
# *   where experiment is the experiment code followed either NRAO,      *
# *         or KVN, or IVS, or KaVA, or EAVN notation.                   *
# *                                                                      *
# *   parameter --verbosity controls verbosiy of the output.             *
# *             0 -- silent                                              *
# *             1 -- normal verbosity (defaults)                         *
# *             2 -- debuggig mode.                                      *
# *                                                                      *
# *   parameter --band specifies the 1-character long band name. If the  *
# *             exeriment has two bands, the code for the upper band     *
# *             should be used.                                          *
# *                                                                      *
# *   parameter --run-level controls which elements or a group of        *
# *             elements of the VLBI data analysis pipeline should be    *
# *             executed. See pir documentation for details.             *
# *                                                                      *
# *   If parameter -s was specified, statically linked PIMA will be      *
# *   used.                                                              *
# *                                                                      *
# *   Limitations:                                                       *
# *    1) The full end-to-end pipeline without manual intervenion        *
# *       is not yet feasible. More work needs be done to implement it.  *
# *       As of version 1, there are several breaking points that assume *
# *       manual work.                                                   *
# *                                                                      *
# *  ### 21-NOV-2021      pir.py    v1.6 (c) L. Petrov  12-MAR-2024 ###  *
# *                                                                      *
# ************************************************************************
import pwd, sys, os, re, shutil, time, subprocess, datetime, argparse, signal
sys.path.append("/usr")
import pima_local
from pima_exe import exe, exe_out_log, exe_out_nolog, exe_noout_log, \
              exe_noout_nolog, pima_print_mes, pima_signal_handler_term, \
              exe_pipe, read_file
from pima_local import *
#
cont_dir    = "/cont"
atmo_dir    = "/imls/heb/geosfpit"

psolve_init = "AU"
snr_samb    = "4.8"
vgos_bands  = ["1", "2", "3", "4"]
run_levels  = [ \
                  "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", \
                  "10", "11", "12", "13", "14", "15", "16", \
                  "a", "b", "c", "f", "g", "i", "l", "r", "pi" \
                ]


pir__label = "pir 2024.05.12  1.6"

#
# ------------------------------------------------------------------------
#
def pima_signal_handler_term ( signal, frame ):
    global pima_child_pid
    tmp_file = '/tmp/pir__' + + "%08d" % os.getpid()
    if ( os.path.isfile(tmp_file) ):
         os.remove ( tmp_file )
    if ( pima_child_pid ):
         os.kill ( pima_child_pid, signal )
    print ( 'Terminated by TERM signal' )
    sys.exit(0)
#
# ------------------------------------------------------------------------
#
def main():
#
# --- Define arguments
#

    parser = argparse.ArgumentParser( description=pir__label )
    parser.add_argument('--version', action='version', version=pir__label )
    
    parser.add_argument ( "-v", "--verbosity", action="store", \
                          dest="verb", \
                          default=1, \
                          metavar="verbosity", \
                          type=int, \
                          help="Verbosity level" )
    
    parser.add_argument ( dest="exp", \
                          action="store",  \
                          metavar="experiment", \
                          help="Experiment name" )
    
    parser.add_argument ( "-b", "--band", \
                          dest="band", \
                          action="store",  \
                          metavar="band", \
                          help="Band name" )
                    
    parser.add_argument ( "-r", "--run-level", \
                          dest="run", \
                          action="store",    \
                          metavar="run-level", \
                          help="Run level" )
    
    parser.add_argument ( "-s", "--static", \
                          dest="static", \
                          action="store_true", \
                          help="use statically linked pima" )
    
    parser.add_argument ( "-o", "--options", action="store", \
                          dest="opts", \
                          default="", \
                          metavar="options", \
                          help="Extra options passed to pima" )
    
#
# --- Get and parse options
#
    args = parser.parse_args()
    
    if ( args.static ):
         pima_exe = pima_static_exec
         pf_exe   = pima_path + "/bin/" + "pf.py -s"
         pr_exe   = pima_path + "/bin/" + "pr.py -s"
    else:
         pima_exe = pima_dynamic_exec
         pf_exe   = pima_path + "/bin/" + "pf.py"
         pr_exe   = pima_path + "/bin/" + "pr.py"
    
    if ( not args.run in run_levels ):
         print ( "Unknown run level %s. Supported run levels: %s" % \
                 ( args.run, " ".join(run_levels) ) )
         exit  ( 1 )
         
    args.band = args.band.lower()
    pima_cnt = pf_dir + "/" + args.exp + "/" + args.exp + "_" + args.band + "_pima.cnt"
    
    if ( not os.path.isfile ( pima_cnt ) ):
         print ( "Cannot find PIMA control file %s" % pima_cnt )
         exit  ( 1 ) 
    

    cnt = read_file ( pima_cnt )
    
    if ( args.run == "1" or args.run == "l" or args.run == "a" ):
#
# ====== Load the experiment to PIMA
#
         print ( "pima is running task load for experiment %s ..." % args.exp )
         pima_load_com = pf_exe + " " + args.exp + " " + args.band + " " + "load" + " " + args.opts
    
         if ( args.run == "0" or args.run == "1" or args.run == "l" ):
              (ret,out) = exe_pipe ( pima_load_com )
              if ( ret != 0 ):
                   for line in out:
                        print ( line ) 
                   print ( "Failed command ", pima_load_com, flush=True  )
                   exit  ( 1 )
              print ( "pima has loaded VLBI experiment", args.exp )
    
#
# --- define pcal mask generation file
#
    pima_pcal_mask_cnt = pf_dir + "/" + args.exp + "/" + args.exp + "_pcal_mask.gen"
    
    pcal_mask_file = "??"
    bpas_mask_file = "??"
    mkdb_2nd       = "??"
    for line in cnt:
        if ( len(line.split()) < 1 ):
             continue
        if ( line.split()[0] == "PCAL_MASK_FILE:" ):
             pcal_mask_file  =  line.split()[1] 
             pcal_mask_gen   = pcal_mask_file.replace(".mask","_mask.gen")
        if ( line.split()[0] == "BANDPASS_MASK_FILE:" ):
             bpas_mask_file  =  line.split()[1] 
             bpas_mask_gen   = bpas_mask_file.replace(".mask","_mask.gen")
        if ( line.split()[0] == "MKDB.2ND_BAND:" ):
             mkdb_2nd        =  line.split()[1].upper()
    
    if ( args.run == "2" or args.run == "l" or args.run == "a" ):
#
# ====== Generate bandpass mask and and pcal mask
#
         print ( "pima is generating bandpass and pcal masks for experiment %s" % args.exp )
         if ( pcal_mask_file != "NO" ):
#
# ----------- Pcal mask generation
#
              pima_pcal_com = pima_exe + " " + pima_cnt + " " + "pmge" + " " + \
                                         "mask_gen" + " " + pcal_mask_gen
        
              (ret,out) = exe_pipe ( pima_pcal_com )
              if ( ret != 0 ):
                   for line in out:
                        print ( line ) 
                   print ( "Failed command ", pima_pcal_com, flush=True  )
                   exit  ( 1 )
              print ( "pima has generated pcal mask file for VLBI experiment", args.exp )
         
         if ( bpas_mask_file != "NO" ):
#
# ----------- Bandpass mask generation
#
              pima_bpas_com = pima_exe + " " + pima_cnt + " " + "bmge" + " " + \
                                     "mask_gen" + " " + bpas_mask_gen
         
              (ret,out) = exe_pipe ( pima_bpas_com )
              if ( ret != 0 ):
                   for line in out:
                        print ( line ) 
                   print ( "Failed command ", pima_bpas_com, flush=True  )
                   exit  ( 1 )
              print ( "pima has generated bpas mask file for VLBI experiment", args.exp )
    
    
    if ( args.run == "3" or args.run == "l" or args.run == "a" ):
#
# ====== Parse logs and load informatuion extracted from logs to PIMA
#
         print ( "pima is parsing logs and load this info into PIMA for experiment %s" % args.exp )
#
# ------ Find log file names and put them in log_file_list
#
         log_file_list = []
         for path, dirs, files in os.walk(pf_dir + "/" + args.exp):
             for file in files:
                 if ( len(file) == len(args.exp) + 6 ):
                      if ( file[-4:] == ".log"  ):
                           log_file_list.append( file )
         
         if ( len(log_file_list) > 0 ):
#
# ----------- Run commands for parsing log files
#
              pima_logs_com = pf_exe + " " + args.exp + " " + args.band + " " + "logs" + " " + args.opts
              print ( "pima is running task logs for experiment %s ..." % args.exp )
              (ret,out) = exe_pipe ( pima_logs_com )
              if ( ret != 0 ):
                   for line in out:
                        print ( line ) 
                   print ( "Failed command ", pima_logs_com, flush=True  )
                   exit  ( 1 )
              print ( "pima has parsed logs files for VLBI experiment", args.exp )
         
#
# ----------- Run command for exporting external calibiration information
# ----------- extracted from log files to PIMA internal data structure
#
              print ( "pima is running task gean for experiment %s..." % args.exp )
              pima_gean_com = pf_exe + " " + args.exp + " " + args.band + " " + "gean" + " " + args.opts
              (ret,out) = exe_pipe ( pima_gean_com )
              if ( ret != 0 ):
                   for line in out:
                        print ( line ) 
                   print ( "Failed command ", pima_gean_com, flush=True  )
                   exit  ( 1 )
              print ( "pima has completed task gean for VLBI experiment", args.exp )

#
# ------ Load gain for the 1st band
#
         if ( not ( args.band == "1" or args.band == "2"  or args.band == "2"  or args.band == "4" ) ):
              print ( "pima is loading gain for experiment %s" % args.exp )
              pima_gain_com = pf_exe + " " + args.exp + " " + args.band + " " + "gain" + " " + args.opts
              print ( "pima is running task gain for band %s of VLBI experiment %s ..." % \
                      ( args.band, args.exp ) )
              (ret,out) = exe_out_nolog ( pima_gain_com, 1 )
              if ( ret != 0 ):
                    for line in out:
                        print ( line ) 
                    print ( "Failed command for gain of the 1st band %s -- %s " % \
                            ( args.band, pima_gain_com), flush=True  )
                    exit  ( 1 )
              print ( "pima has completed task gain for band %s of VLBI experiment %s" % \
                      ( args.band, args.exp ) )
              if ( mkdb_2nd != "NO" ):
#
# ---------------- Load gain for the 2nd band
#
                   band_2nd = mkdb_2nd[-10:-9].lower()
                   pima_gain_com = pf_exe + " " + args.exp + " " + band_2nd + " " + "gain" + " " + args.opts
                   print ( "pima is running task gain for band %s of VLBI experiment %s ..." % \
                         ( band_2nd, args.exp ) )
                   (ret,out) = exe_out_nolog ( pima_gain_com, 1 )
                   if ( ret != 0 ):
                        for line in out:
                            print ( line ) 
                            print ( "Failed command for gain of the 1st band %s -- %s " % \
                                    ( band_2nd, pima_gain_com), flush=True  )
                            exit  ( 1 )
                   print ( "pima has completed task gain for band %s of VLBI experiment %s" % \
                           ( band_2nd, args.exp ) )

    if ( args.run == "4" or args.run == "c" or args.run == "a" ):
#
# ====== Run coarse fringe fitting
#        If the experiment has two bands, this command runs in parallel
#
         print ( "pima is running coarse fringe fitting for experiment %s ..." % args.exp )
         pima_1st_coarse_com =  pf_exe + " " + args.exp + " " + args.band + " " + "coarse" + " " + args.opts
         if ( mkdb_2nd != "NO" ):
              if ( args.band == "4" ):
                   pima_coarse_com = "set echo_style = both ; echo " + '"' 
                   nb = 0
                   for band in vgos_bands:
                       nb = nb + 2
                       next_com =  pf_exe + " " + args.exp + " " + band + " " + "coarse" + " " + args.opts
                       pima_coarse_com = pima_coarse_com + " "  + next_com + "\n"
                       if ( nb < len(vgos_bands) ):
                            pima_coarse_com = pima_coarse_com + " sleep %d;"% nb
                   pima_coarse_com = pima_coarse_com + '"' + " | " + "parallel"
              else:
                   pima_coarse_com = pf_exe + " " + args.exp + " " + args.band + " " + "coarse" + " " + args.opts
                   band_2nd = mkdb_2nd[-10:-9].lower()
                   pima_2nd_coarse_com = pf_exe + " " + args.exp + " " + band_2nd + " " + "coarse" + " " + args.opts
                   pima_coarse_com = "set echo_style = both ; echo " + '"' + pima_1st_coarse_com + "\n" + \
                                     "sleep 4;"            + pima_2nd_coarse_com + "\n" + '"' + \
                                     " | " + "parallel"
         else:     
              pima_coarse_com = pima_1st_coarse_com
    
         (ret,out) = exe_pipe ( pima_coarse_com )
         if ( ret != 0 ):
               for line in out:
                   print ( line ) 
               print ( "Failed command ", pima_coarse_com, flush=True  )
               exit  ( 1 )
         print ( "pima has completed task coarse for VLBI experiment", args.exp )
    
    if ( args.run == "5" or args.run == "b" or args.run == "a" ):
#
# ====== Run bandpass generation. 
#        If the experiment has two bands, this command runs in parallel
#
         pima_1st_bpas_com = pf_exe + " " + args.exp + " " + args.band + " " + "bpas" + " " + args.opts
         if ( mkdb_2nd != "NO" ):
              if ( args.band == "4" ):
                   pima_bpas_com = "set echo_style = both ; echo " + '"' 
                   nb = 0
                   for band in vgos_bands:
                       nb = nb + 2
                       next_com =  pf_exe + " " + args.exp + " " + band + " " + "bpas" + " " + args.opts
                       pima_bpas_com = pima_bpas_com + " "  + next_com + "\n"
                       if ( nb < len(vgos_bands) ):
                            pima_bpas_com = pima_bpas_com + " sleep %d;"% nb
                   pima_bpas_com = pima_bpas_com + '"' + " | " + "parallel"
              else:
                   band_2nd = mkdb_2nd[-10:-9].lower()
                   pima_2nd_bpas_com = pf_exe + " " + args.exp + " " + band_2nd + " " + "bpas" + " " + args.opts
                   pima_bpas_com = "set echo_style = both; echo " + '"' + pima_1st_bpas_com + "\n" + \
                                                           pima_2nd_bpas_com + "\n" + '"' + \
                                   " | " + "parallel"
         else:
              pima_bpas_com = pima_1st_bpas_com
     
         print ( "pima is running task bpas for VLBI experiment %s ..." % args.exp )
         (ret,out) = exe_out_nolog ( pima_bpas_com, 1 )
         if ( ret != 0 ):
               for line in out:
                   print ( line ) 
               print ( "Failed command ", pima_bpas_com, flush=True  )
               exit  ( 1 )
         print ( "pima has completed task bpas for VLBI experiment", args.exp )
    
    if ( args.run == "6" or args.run == "f" or args.run == "a" ):
#
# ====== Run fine fringe fitting
#        If the experiment has two bands, this command runs in parallel
#
         print ( "pima is running fine fringe fitting for experiment %s ..." % args.exp )
         pima_1st_fine_com = pf_exe + " " + args.exp + " " + args.band + " " + "fine" + " " + args.opts
         if ( mkdb_2nd != "NO" ):
              if ( args.band == "4" ):
                   pima_fine_com = "set echo_style = both ; echo " + '"' 
                   nb = 0
                   for band in vgos_bands:
                       nb = nb + 1
                       next_com =  pf_exe + " " + args.exp + " " + band + " " + "fine" + " " + args.opts
                       pima_fine_com = pima_fine_com + " "  + next_com + "\n"
                       if ( nb < len(vgos_bands) ):
                            pima_fine_com = pima_fine_com + " sleep %d;"% nb
                   pima_fine_com = pima_fine_com + '"' + " | " + "parallel"
              else:
                   band_2nd = mkdb_2nd[-10:-9].lower()
                   pima_2nd_fine_com = pf_exe + " " + args.exp + " " + band_2nd + " " + "fine" + " " + args.opts
                   pima_fine_com = "set echo_style = both ; echo " + '"' + pima_1st_fine_com + "\n" + \
                                                           pima_2nd_fine_com + "\n" + '"' + \
                                    " | " + "parallel"
         else:
              pima_fine_com = pima_1st_fine_com
     
         (ret,out) = exe_out_nolog ( pima_fine_com, 1 )
         if ( ret != 0 ):
               for line in out:
                   print ( line ) 
               print ( "Failed command ", pima_fine_com, flush=True  )
               exit  ( 1 )
         print ( "pima has completed task fine for VLBI experiment", args.exp )
    if ( args.run == "af" ):
#
# ====== Run fine fringe fitting
#        If the experiment has two bands, this command runs in parallel
#
         print ( "pima is running all-fine fringe fitting for experiment %s ..." % args.exp )
         pima_1st_fine_com = pf_exe + " " + args.exp + " " + args.band + " " + "allfine" + " " + args.opts
         if ( mkdb_2nd != "NO" ):
              if ( args.band == "4" ):
                   pima_fine_com = "set echo_style = both ; echo " + '"' 
                   nb = 0
                   for band in vgos_bands:
                       nb = nb + 2 + " "
                       next_com =  pf_exe + " " + args.exp + " " + band + " " + "allfine" + " " + args.opts
                       pima_fine_com = pima_fine_com + " "  + next_com + "\n"
                       if ( nb < len(vgos_bands) ):
                            pima_fine_com = pima_fine_com + " sleep %d;"% nb
                   pima_fine_com = pima_fine_com + '"' + " | " + "parallel"
              else:
                   band_2nd = mkdb_2nd[-10:-9].lower()
                   pima_2nd_fine_com = pf_exe + " " + args.exp + " " + band_2nd + " " + "allfine" + " " + args.opts
                   pima_fine_com = "set echo_style = both ; echo " + '"' + pima_1st_fine_com + "\n" + \
                                                           pima_2nd_fine_com + "\n" + '"' + \
                                    " | " + "parallel"
         else:
              pima_fine_com = pima_1st_fine_com
     
         (ret,out) = exe_out_nolog ( pima_fine_com, 1 )
         if ( ret != 0 ):
               for line in out:
                   print ( line ) 
               print ( "Failed command ", pima_fine_com, flush=True  )
               exit  ( 1 )
         print ( "pima has completed task fine for VLBI experiment", args.exp )
    
    if ( args.run == "7" or args.run == "f" or args.run == "a" ):
#
# ====== Run creation of output GVF database
#
         print ( "pima is running task mkdb for VLBI experiment %s ..." % args.exp )
         pima_mkdb_com = pf_exe + " " + args.exp + " " + args.band + " " + "mkdb" + " " + args.opts
         (ret,out) = exe_out_nolog ( pima_mkdb_com, 1 )
         if ( ret != 0 ):
               for line in out:
                   print ( line ) 
               print ( "Failed command ", pima_mkdb_com, flush=True  )
               exit  ( 1 )
         print ( "pima has completed task mkdb for VLBI experiment %s " % args.exp )
    
    if ( args.run == "8" or args.run == "r" or args.run == "a" ):
         os.environ["COND_MAX"] = "1.0D14"
#
# ====== Run pSolve solution and generate listings of a solution for each band
#
         desc_file = pf_dir + "/" + args.exp + "/" + args.exp + ".desc"
         if ( not os.path.isfile ( desc_file ) ):
              print ( "Cannot find experiment description file %s" % desc_file )
              exit  ( 1 )
         desc = read_file ( desc_file )
         db_name = "??"
         for line in desc:
             if ( line.split()[0] == "DB_NAME:" ):
                  db_name = line.split()[1] 
    
         if ( db_name == "??" ):
              print ( "Cannot find database name in the description file %s" % desc_file )
              exit  ( 1 )
    
#
# ------ Generarate psolve command file
#
         psolve_cnt_file = '/tmp/pir__' + "%08d" % os.getpid()
#
# ------ Read template psolve control file
#
         pcnt = read_file ( pima_psolve )
         if ( not pcnt ):
              print ( "Cannot find psolve template file %s" % pcnt )
              exit  ( 1 )
#
# ------ Replace keywords VTD_CONF, RESIDUAL and add session command
#
         f=open(psolve_cnt_file,"w")
         for line in pcnt:
             if ( line.split()[0] == "VTD_CONF" ):
                  line = "  VTD_CONF           " + pima_vtd
             if ( line.split()[0] == "RESIDUALS" ):
                  line = "  RESIDUALS          /tmp/SPOOL"
             print ( line, file=f )
         line = "     OBS  " + db_name + " 2 TYPE " + \
                "GX" + " SUPMET META  ! " + args.exp
         print ( line, file=f )
         f.close()
#
# ------ Run pSolve
#
         pima_psolve_com = "psolve " + psolve_init + " " + psolve_cnt_file + " > " + \
                            pf_dir + "/" + args.exp + "/" + args.exp + "_init" + args.band + \
                            ".log"
         print ( "pima is running psolve for band %s of VLBI experiment %s ..." % \
                 (args.band, args.exp ) )
         (ret,out) = exe_out_nolog ( pima_psolve_com, 1 )
         if ( ret != 0 ):
              for line in out:
                  print ( line ) 
              print ( "Failed command ", pima_psolve_com, flush=True  )
              exit  ( 1 )
         print ( "pima has completed psolve run for band %s of VLBI experiment %s" % \
                 (args.band, args.exp ) )
         if ( os.path.isfile ( psolve_cnt_file ) ):
              os.remove ( psolve_cnt_file )
         if ( os.path.isfile ( psolve_cnt_file + ".XPND" ) ):
              os.remove ( psolve_cnt_file + ".XPND" )

#
# ------ Copy spool file to the experiment directory
#
         spool_file = pf_dir + "/" + args.exp + "/" + args.exp + "_" + \
                     args.band + "_init.spl"
         copy_com = "cp" + " " + "$PSOLVE_SPOOL_DIR/SPLF" + psolve_init + " " + \
                     spool_file 
         (ret,out) = exe_out_nolog ( copy_com, 1 )
         if ( ret != 0 ):
              for line in out:
                  print ( line ) 
              print ( "Failed command ", copy_com, flush=True  )
              exit  ( 1 )
         print ( "Listing of the solution file: ", spool_file )
                    
         if ( mkdb_2nd != "NO" ):
#
# ----------- Process the second band
#
              band_2nd = mkdb_2nd[-10:-9].lower()
#
# ----------- Replace keywords VTD_CONF, RESIDUAL and add session command
#
              f=open(psolve_cnt_file,"w")
              for line in pcnt:
                  if ( line.split()[0] == "VTD_CONF" ):
                       line = "  VTD_CONF           " + pima_vtd
                  if ( line.split()[0] == "RESIDUALS" ):
                       line = "  RESIDUALS          SPOOL"
                  print ( line, file=f )
              line = "     OBS  " + db_name + " 2 TYPE " + \
                     "GS" + " SUPMET META  ! " + args.exp
              print ( line, file=f )
              f.close()
    
#
# ----------- Run Psolve solution for the second band
#
              pima_psolve_com = "psolve " + psolve_init + " " + psolve_cnt_file + " > " + \
                            pf_dir + "/" + args.exp + "/" + args.exp + "_init" + band_2nd + \
                            ".log"
              print ( "pima is running psolve for band %s of VLBI experiment %s ..." % \
                      (band_2nd, args.exp ) )
              (ret,out) = exe_out_nolog ( pima_psolve_com, 1 )
              if ( ret != 0 ):
                    for line in out:
                        print ( line ) 
                    print ( "Failed command ", pima_psolve_com, flush=True  )
                    exit  ( 1 )
              print ( "pima has completed psolve run for band %s of VLBI experiment %s " % \
                      (band_2nd, args.exp ) )
    
              if ( os.path.isfile ( psolve_cnt_file ) ):
                   os.remove ( psolve_cnt_file )
              if ( os.path.isfile ( psolve_cnt_file + ".XPND" ) ):
                   os.remove ( psolve_cnt_file + ".XPND" )

#
# ----------- Copy the spool file
#
              spool_file = pf_dir + "/" + args.exp + "/" + args.exp + "_" + \
                           band_2nd + "_init.spl"
              copy_com = "cp" + " " + "$PSOLVE_SPOOL_DIR/SPLF" + psolve_init + " " + \
                          spool_file 
              (ret,out) = exe_out_nolog ( copy_com, 1 )
              if ( ret != 0 ):
                   for line in out:
                       print ( line ) 
                   print ( "Failed command ", copy_com, flush=True  )
                   exit  ( 1 )
              print ( "Listing of the solution file: ", spool_file )
#
# ------ Remove garbage
#
         fil_spool_bin = '/tmp/SPOOL.bin'
         os.remove ( fil_spool_bin )
         fil_spool_ndx = '/tmp/SPOOL.ndx'
         os.remove ( fil_spool_ndx )

    if ( args.run == "9" or args.run == "r" or args.run == "a" ):
#
# ====== Run re-frining of the observations marked as outliers
#        If the experiment has two bands, this command runs in parallel
#
         pima_1st_samb_com = pr_exe + " " + args.exp + " " + args.band + " " + \
                             snr_samb + " " + "-nodb"
# + " -delwin 4.0"
         if ( mkdb_2nd != "NO" ):
              band_2nd = mkdb_2nd[-10:-9].lower()
              pima_2nd_samb_com = pr_exe + " " + args.exp + " " + band_2nd + " " + \
                                  snr_samb + " " + "-nodb"
              pima_fine_com = "set echo_style = both ; echo " + '"' + pima_1st_samb_com + "\n" + \
                                                      pima_2nd_samb_com + "\n" + '"' + \
                               " | " + "parallel"
         else:
              pima_fine_com = pima_1st_samb_com
     
         print ( "pima is running task samb for VLBI experiment %s ..." % args.exp )
         (ret,out) = exe_out_nolog ( pima_fine_com, 1 )
         if ( ret != 0 ):
               for line in out:
                   print ( line ) 
               print ( "Failed command ", pima_fine_com, flush=True  )
               exit  ( 1 )
         print ( "pima has completed task samb for VLBI experiment %s" % args.exp )

    if ( args.run == "10" or args.run == "r" or args.run == "a" ):
#
# ====== Run generation of the output database in the GVF format with re-fringed data
#
         pima_mkdb_com = pf_exe + " " + args.exp + " " + args.band + " " + "mkdb" + " -updt" + " " + args.opts
         print ( "pima is running task mkdb -updt for VLBI experiment %s ..." % args.exp )
         (ret,out) = exe_out_nolog ( pima_mkdb_com, 1 )
         if ( ret != 0 ):
               for line in out:
                   print ( line ) 
               print ( "Failed command ", pima_mkdb_com, flush=True  )
               exit  ( 1 )
         print ( "pima has completed task mkdb -updt for VLBI experiment %s " % args.exp )

    if ( args.run == "11" or args.run == "pi" or args.run == "a" ):
#
# ====== Run task onof to determine segments of data when antenna was not on source
#
         pima_onof_com = pima_exe + " " + pima_cnt + " " + "onof"
         print ( "pima is running task onof for experiment %s ..." % args.exp )
         (ret,out) = exe_pipe ( pima_onof_com )
         if ( ret != 0 ):
              for line in out:
                  print ( line ) 
                  print ( "Failed command ", pima_onof_com, flush=True  )
              exit  ( 1 )
         print ( "pima has task onof for VLBI experiment", args.exp )

    if ( args.run == "12" or args.run == "pi" or args.run == "a" ):
#
# ====== Run task opag: computation of atmopshere brightness temperature and opacity
#
         pima_opag_com = pima_exe + " " + pima_cnt + " " + "opag" + " " + \
                                     "atmo_dir" + " " + atmo_dir
         print ( "pima is running task opag for experiment %s ..." % args.exp )
         (ret,out) = exe_pipe ( pima_opag_com )
         if ( ret != 0 ):
              for line in out:
                  print ( line ) 
                  print ( "Failed command ", pima_opag_com, flush=True  )
              exit  ( 1 )
         print ( "pima has generated atmosphere Tatm and opacity files for VLBI experiment", args.exp )

    if ( args.run == "13" or args.run == "pi" or args.run == "a" ):
#
# ====== Run task opal: loading atmospheric opacity and brightness temperature
#        followed by automatic editing Tsys values
#
         pima_opal_com = pf_exe + " " + args.exp + " " + args.band + " " + "opal" + " " + args.opts
         print ( "pima is running task opal for band %s of VLBI experiment %s ..." % 
                 ( args.band, args.exp ) )
         (ret,out) = exe_out_nolog ( pima_opal_com, 1 )
         if ( ret != 0 ):
               for line in out:
                   print ( line ) 
               print ( "Failed command ", pima_opal_com, flush=True  )
               exit  ( 1 )
         print ( "pima has completed task opal for band %s of VLBI experiment %s " % \
                 ( args.band, args.exp ) )

    if ( args.run == "14" or args.run == "pi" or args.run == "a" ):
#
# ====== Generate images of the sources selected as reference 
#        If the experiment has two bands, this command runs in parallel
#
# ------ Load gains for the upper band
#
         pima_gain_com = pf_exe + " " + args.exp + " " + args.band + " " + "gain" + " " + args.opts
         print ( "pima is running task gain for band %s of VLBI experiment %s ..." % \
                 ( args.band, args.exp ) )
         (ret,out) = exe_out_nolog ( pima_gain_com, 1 )
         if ( ret != 0 ):
               for line in out:
                   print ( line ) 
               print ( "Failed command ", pima_gain_com, flush=True  )
               exit  ( 1 )
         print ( "pima has completed task gain for band %s of VLBI experiment %s" % \
                 ( args.band, args.exp ) )
         if ( mkdb_2nd != "NO" ):
#
# ----------- Load gain for the 2nd band
#
              band_2nd = mkdb_2nd[-10:-9].lower()
              pima_gain_com = pf_exe + " " + args.exp + " " + band_2nd + " " + "gain" + " " + args.opts
              print ( "pima is running task gain for band %s of VLBI experiment %s ..." % \
                    ( args.band, args.exp ) )
              (ret,out) = exe_out_nolog ( pima_gain_com, 1 )
              if ( ret != 0 ):
                   for line in out:
                       print ( line ) 
                   print ( "Failed command ", pima_gain_com, flush=True  )
                   exit  ( 1 )
              print ( "pima has completed task gain for band %s of VLBI experiment %s" % \
                      ( band_2nd, args.exp ) )

#
# ------ Running command sres -- imaging reference sources
#
         pima_1st_sres_com = pf_exe + " " + args.exp + " " + args.band + " " + "sres" + " " + args.opts
         if ( mkdb_2nd != "NO" ):
              band_2nd = mkdb_2nd[-10:-9].lower()
              pima_2nd_sres_com = pf_exe + " " + args.exp + " " + band_2nd + " " + "sres" + " " + args.opts
              pima_sres_com = "set echo_style = both ; echo " + '"' + pima_1st_sres_com + "\n" + \
                                         "sleep 4;" + pima_2nd_sres_com + "\n" + '"' + \
                               " | " + "parallel"
         else:
              pima_sres_com = pima_1st_sres_com
     
         print ( "pima is running task sres for VLBI experiment %s ..." % args.exp )
         (ret,out) = exe_out_nolog ( pima_sres_com, 1 )
         if ( ret != 0 ):
               for line in out:
                   print ( line ) 
               print ( "Failed command ", pima_sres_com, flush=True  )
               exit  ( 1 )
         print ( "pima has completed task sres for VLBI experiment", args.exp )

    if ( args.run == "15" or args.run == "g" or args.run == "a" ):
#
# ====== Run gain correction computation
#        If the experiment has two bands, this command runs in parallel
#
         pima_1st_gaco_com = pf_exe + " " + args.exp + " " + args.band + " " + "gaco" + " " + args.opts
         if ( mkdb_2nd != "NO" ):
              band_2nd = mkdb_2nd[-10:-9].lower()
              pima_2nd_gaco_com = pf_exe + " " + args.exp + " " + band_2nd + " " + "gaco" + " " + args.opts
              pima_gaco_com = "set echo_style = both ; echo " + '"' + pima_1st_gaco_com + "\n" + \
                                         "sleep 4;" + pima_2nd_gaco_com + "\n" + '"' + \
                               " | " + "parallel"
         else:
              pima_gaco_com = pima_1st_gaco_com
     
         print ( "pima is running task gaco for VLBI experiment %s ..." % args.exp )
         (ret,out) = exe_out_nolog ( pima_gaco_com, 1 )
         if ( ret != 0 ):
               for line in out:
                   print ( line ) 
               print ( "Failed command ", pima_gaco_com, flush=True  )
               exit  ( 1 )
         print ( "pima has completed task gaco for VLBI experiment", args.exp )

    if ( args.run == "16" or args.run == "i" or args.run == "a" ):
#
# ====== Run automatic imaging
#        If the experiment has two bands, this command runs in parallel
#
         pima_1st_map_com = pf_exe + " " + args.exp + " " + args.band + " " + "map" + " " + args.opts
         if ( mkdb_2nd != "NO" ):
              pima_map_com = pf_exe + " " + args.exp + " " + args.band + " " + "map" + " " + args.opts
              band_2nd = mkdb_2nd[-10:-9].lower()
              pima_2nd_map_com = pf_exe + " " + args.exp + " " + band_2nd + " " + "map" + " " + args.opts
              pima_map_com = "set echo_style = both ; echo " + '"' + pima_1st_map_com + "\n" + \
                             "sleep 8;"            + pima_2nd_map_com + "\n" + '"' + \
                             " | " + "parallel"
         else:
              pima_map_com = pima_1st_map_com
    
         print ( "pima is running imaging tasks for experiment %s ..." % args.exp )
         (ret,out) = exe_pipe ( pima_map_com )
         if ( ret != 0 ):
               for line in out:
                   print ( line ) 
               print ( "Failed command ", pima_map_com, flush=True  )
               exit  ( 1 )
         print ( "pima has completed task map for VLBI experiment", args.exp )


if __name__ == "__main__":
    global pima_child_pid
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTERM, pima_signal_handler_term )
        signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
        pima_child_pid = None
        main()
    except KeyboardInterrupt:
        print ( "%s: Interrupted" % sys.argv[0] )
        exit  ( 1 )
