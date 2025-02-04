#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *                                                                      *
# * ### 02-APR-2016  map_init.py v 1.6 (c)   L. Petrov   29-MAR-2016 ### *
# *                                                                      *
# ************************************************************************
"""
import pwd, sys, os, re, shutil, time, subprocess, datetime, operator, signal
sys.path.append("/usr")
import argparse 
import pima_local
from pima_exe import exe, exe_out_log, exe_out_nolog, exe_noout_log, exe_noout_nolog, \
                     pima_print_mes, pima_signal_handler_term

pf__label   = "map_init.py (PIMA fringe)"
pf__version = "map_init.py v 1.6  2016.03.29"

band_list = [ "l", "s", "c", "x", "u", "k", "q", "w", "d", "b", "e", "f", "m", "y" ]
pima_exec  = { \
               "dynamic": pima_local.pima_dynamic_exec, \
               "static":  pima_local.pima_static_exec   \
             }
pf_dir = pima_local.pf_dir
pima_bin= pima_local.pima_path + "/bin"

vlba_sta_list = [ \
                    "BR-VLBA", \
                    "FD-VLBA", \
                    "HN-VLBA", \
                    "KP-VLBA", \
                    "LA-VLBA", \
                    "NL-VLBA", \
                    "OV-VLBA", \
                    "MK-VLBA", \
                    "PIETOWN", \
                    "SC-VLBA"  \
                ]

ivs_gain_file  = pima_local.pima_share_dir + "/ivs_gains.key"
vlba_gain_file = pima_local.pima_share_dir + "/vlba_gains.key"

class pf_class:
   def __init__ ( self, task, exp, band, verb, dry_run, static, pima_opts ):
       self.task      = task
       self.exp       = exp
       self.band      = band
       self.verb      = verb
       self.dry_run   = dry_run
       self.static    = static
       self.exp_dir   = None
       self.cnt_file  = None
       self.pima_opts = pima_opts

#
# ------------------------------------------------------------------------
#
def pf_load ( pf, nopcal ):
    """
    Load VLBI experiment in PIMA
    """
    log_file = pf.exp_dir + "/" + pf.exp + "_load.log"
    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
#
# --- Check: if UV exclude file does not exists, create it
#
    uv_exc_file  = pf.exp_dir + "/" + pf.exp + "_uv.exc"
    if ( os.path.isfile ( uv_exc_file ) ):
         f = open ( uv_exc_file, "w" )
         f.write ( "#" )
         f.close()
#
# --- Check: if OBS exclude file does not exists, create it
#
    obs_exc_file = pf.exp_dir + "/" + pf.exp + "_obs.exc"
    if ( not os.path.isfile ( obs_exc_file ) ):
         f = open ( obs_exc_file, "w" )
         f.write ( "#" )
         f.close()
#
# --- Print start message in the log file
#
    if ( not pf.dry_run ):
         log = open ( log_file, "w" )
         print ( " ".join(sys.argv), file = log ) 
         print ( "Started  loading experiment", pf.exp, "in PIMA on", date_str, file=log )
         print ( "=====================================================================", \
                  file=log )
         print ( " ", file=log )
         log.flush()
#
# --- Generate a command for PIMA
#
    if ( pf.static ):
         pima_com = pima_exec["static"] + " " + pf.cnt_file + " " + "load"
    else:
         pima_com = pima_exec["dynamic"] + " " + pf.cnt_file + " " + "load"
    pima_com = pima_com + " " \
               "BANDPASS_FILE: NO" + " " + \
               "POLARCAL_FILE: NO" + " " + \
	       "BANDPASS_MASK_FILE: NO" + " " + \
               "PCAL_MASK_FILE: NO"
    if ( nopcal ): 
         pima_com = pima_com + " " + "PCAL: NO"

    if ( len(pf.pima_opts) > 0 ):
         for opt in pf.pima_opts:
             pima_com = pima_com + " " + opt

    if ( pf.verb > 1 or pf.dry_run ):
         print ( "pf.py: " + pima_com )
    if ( pf.dry_run ): return 0
#
# --- We make three attempts to load the experiment
#
    for i in range(0,3):
        print ( "pf.py: ", pima_com, file=log )
        print ( " ", file=log )
        log.flush()
        (ret, out ) = exe_noout_log ( pima_com, pf.verb, log )
        if ( ret == 0  ): break
        if ( ret == 23 ): 
#
# ---------- Error code 23 means potentially recoverable error:
# ---------- bad visibilitis were found. They were put in the UV exclude file
#
             if ( i+1 < 3 ): 
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  print ( "pf.py: Try ", i+2, "of loading experiment", pf.exp, \
                           "in PIMA on", date_str, file=log )
                  continue
        if ( ret != 0  ): 
#
# ---------- Some other error has happened
#
             if ( pf.verb >= 1 ): 
                  print ( "pf.py: ERROR in an attempt to load experiment", pf.exp, \
                          "in PIMA" )
             if ( pf.verb >= 2 ): 
                  print ( "ret=", ret, "last command:", pima_com )
             print ( "pf.py: ERROR in an attempt to load experiment", pf.exp, \
                     "in PIMA", file=log )
             print ( "ret=", ret, "last command:", pima_com )
             log.close()
             exit ( 1 )
             
#
# --- Generate the final success message in the output log file
#
    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
    if ( pf.verb > 0 ):
         print ( "pf.py: Successfully loaded experiment", pf.exp, "in PIMA on", \
                  date_str )
         sys.stdout.flush()
    
    print ( " ", file=log )
    print ( "pf.py: Successfully loaded experiment", pf.exp, "in PIMA on", date_str, file=log )
    print ( "===============================================================================", \
             file=log )
    log.close()
    exit ( 0 )

#
# ------------------------------------------------------------------------
#
def pf_logs ( pf ):
    """
    Perform task for parsing field system log files
    """

    log_file = pf.exp_dir + "/" + pf.exp + "_log_antab.log"
    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]

    if ( not pf.dry_run ):
         log = open ( log_file, "w" )
         print ( " ".join(sys.argv), file = log ) 
         print ( "Started  parsing log files for", pf.exp, "by PIMA on", date_str, file=log )
         print ( "========================================================================", \
                  file=log )
         print ( " ", file=log )
         log.flush()

    antab_list = []
    vlba_logs = []
    vlba_sta  = []
    ivs_logs  = []
    ivs_sta   = []
    kvn_logs  = []
    kvn_sta   = []
    for path, dirs, files in os.walk(pf.exp_dir):
        files.sort()
        for file in files:
            if ( file.find ( pf.exp + "log." ) == 0 ):
                 if ( len(file) == len(pf.exp) + len("log.") + 2 ):
                      vlba_logs.append ( pf.exp_dir + "/" + file )
                      vlba_sta.append ( file[len(pf.exp) + len("log."):len(file)] )
            elif ( file.find ( pf.exp  ) == 0 and \
                   file.find ( ".log"  ) == len(pf.exp) + 2 and \
                   len(file) == len(pf.exp) + 2 + len(".log") ):
                   ivs_logs.append ( pf.exp_dir + "/" + file )
                   ivs_sta.append  ( file[len(pf.exp):len(pf.exp)+2] )

            elif ( file.find ( pf.exp  ) == 0 and \
                   file.find ( ".log"  ) == len(pf.exp) + 3 and \
                   len(file) == len(pf.exp) + 3 + len(".log") ):
                   if ( file[len(pf.exp):len(pf.exp)+3] == "KTN" ):
                        kvn_logs.append ( pf.exp_dir + "/" + file )
                        kvn_sta.append  ( "tn" )
                   elif ( file[len(pf.exp):len(pf.exp)+3] == "KUS" ):
                        kvn_logs.append ( pf.exp_dir + "/" + file )
                        kvn_sta.append  ( "us" )
                   elif ( file[len(pf.exp):len(pf.exp)+3] == "KYS" ):
                        kvn_logs.append ( pf.exp_dir + "/" + file )
                        kvn_sta.append  ( "ys" )

    if ( pf.verb > 1 ):
         print ( "vlba_logs = ", vlba_logs )
         print ( "vlba_sta  = ", vlba_sta  )
         print ( "ivs_logs  = ", ivs_logs  )
         print ( "ivs_sta   = ", ivs_sta   )
         print ( "kvn_logs  = ", kvn_logs  )
         print ( "kvn_sta   = ", kvn_sta   )

    if ( len(vlba_logs) > 0 ):
         for i in range(0,len(vlba_logs)):
             antab_file = pf.exp_dir + "/" + pf.exp + "_" + vlba_sta[i] + ".ant"
             pima_com =   pima_local.log_to_antab_exec \
                        + " 1 " \
                        + vlba_logs[i] + " " \
                        + antab_file

             if ( len(pf.pima_opts) > 0 ):
                  for opt in pf.pima_opts:
                      pima_com = pima_com + " " + opt

             if ( pf.verb > 1 or pf.dry_run ):
                  print ( "pf.py: " + pima_com )
                  ret = 0
             if ( not pf.dry_run ):
                  (ret, out ) = exe_noout_log ( pima_com, pf.verb, log )
             if ( ret != 0 ):
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  if ( pf.verb > 0 ):
                       print ( "pf.py: parsing station log for station", \
                               vlba_sta[i], "in experiment", pf.exp, \
                               "FAILED due to an error on", date_str )
                       sys.stdout.flush()
                  print ( " ", file=log )
                  print ( "pf.py: parsing station log for station", \
                          vlba_sta[i], "in experiment", pf.exp, \
                          "FAILED due to an error on", date_str, file=log )
                  log.close()
                  exit ( 1 )
             antab_list.append ( antab_file )

    if ( len(ivs_logs) > 0 ):
         for i in range(0,len(ivs_logs)):
             antab_file = pf.exp_dir + "/" + pf.exp + "_" + ivs_sta[i] + ".ant"
             pima_com =   pima_local.log_to_antab_exec \
                        + " 1 " \
                        + ivs_logs[i] + " " \
                        + antab_file

             if ( len(pf.pima_opts) > 0 ):
                   for opt in pf.pima_opts:
                       pima_com = pima_com + " " + opt

             if ( pf.verb > 1 or pf.dry_run ):
                  print ( "pf.py: " + pima_com )
                  ret = 0
             if ( not pf.dry_run ):
                  (ret, out ) = exe_noout_log ( pima_com, pf.verb, log )
             if ( ret != 0 ):
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  if ( pf.verb > 0 ):
                       print ( "pf.py: parsing station log for station", \
                               ivs_sta[i], "in experiment", pf.exp, \
                               "FAILED due to an error on", date_str )
                       sys.stdout.flush()
                  print ( " ", file=log )
                  print ( "pf.py: parsing station log for station", \
                          ivs_sta[i], "in experiment", pf.exp, \
                          "FAILED due to an error on", date_str, file=log )
                  print ( "=================================================", 
                           file=log )
                  log.close()
                  exit ( 1 )
             antab_list.append ( antab_file )

    if ( len(kvn_logs) > 0 ):
         for i in range(0,len(kvn_logs)):
             antab_file = pf.exp_dir + "/" + pf.exp + "_" + kvn_sta[i] + ".ant"
             pima_com =   pima_local.log_to_antab_exec \
                        + " 11 " \
                        + kvn_logs[i] + " " \
                        + antab_file

             if ( len(pf.pima_opts) > 0 ):
                   for opt in pf.pima_opts:
                       pima_com = pima_com + " " + opt

             if ( pf.verb > 1 or pf.dry_run ):
                  print ( "pf.py: " + pima_com )
                  ret = 0
             if ( not pf.dry_run ):
                  (ret, out ) = exe_noout_log ( pima_com, pf.verb, log )
             if ( ret != 0 ):
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  if ( pf.verb > 0 ):
                       print ( "pf.py: parsing station log for station", \
                               kvn_sta[i], "in experiment", pf.exp, \
                               "FAILED due to an error on", date_str )
                       sys.stdout.flush()
                  print ( " ", file=log )
                  print ( "pf.py: parsing station log for station", \
                          kvn_sta[i], "in experiment", pf.exp, \
                          "FAILED due to an error on", date_str, file=log )
                  print ( "=================================================", 
                           file=log )
                  log.close()
                  exit ( 1 )
             antab_list.append ( antab_file )

    if ( pf.dry_run ):
         exit ( 0 )
    for antab_file in antab_list:
        if ( pf.verb > 0 ):
             print ( "Generated", antab_file )

    if ( len(antab_list) == 0 ):
         print ( "No log files were found for exepriment", pf.exp,
                 ". Please check names." )
         print ( "No log files were found for exepriment", pf.exp,
                 ". Please check names.", file=log )
         log.close()
         exit ( 1  )

    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
    if ( pf.verb > 0 ):
         print ( "pf.py: log files for", len(antab_list), \
                 "stations in experiment", pf.exp, "\n", \
                 "      were successfull parsed error on", date_str )
         sys.stdout.flush()
    print ( "pf.py: log files for", len(antab_list), \
             "stations in experiment", pf.exp, "\n", \
             "      were successfull parsed error on", date_str, file=log )
    print ( "===============================================================", \
            file=log )
    log.close()
        

#
# ------------------------------------------------------------------------
#
def pf_gean ( pf ):
    """
    Perform task for collecting antenna calibration and loading then in PIMA
    """
    log_file = pf.exp_dir + "/" + pf.exp + "_gean.log"

    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
#
# --- Print start message in the log file
#
    if ( not pf.dry_run ):
         log = open ( log_file, "w" )
         print ( " ".join(sys.argv), file = log ) 
         print ( "Started  loading calibration data into PIMA", pf.exp, "by PIMA on", date_str, file=log )
         print ( "====================================================================================", \
                  file=log )
         print ( " ", file=log )
         log.flush()

    if ( pf.static ):
         pima_com = pima_exec["static"]  + " " + pf.cnt_file + " " + "gean"
    else:
         pima_com = pima_exec["dynamic"] + " " + pf.cnt_file + " " + "gean"

    mode  = None
    logv  = []
    loga  = []
    nlog  = 0
    for path, dirs, files in os.walk(pf.exp_dir):
        files.sort()
        for file in files:
            if ( file.find("~") == len(file)-1 ): continue
            if ( file.find("#") == len(file)-1 ): continue
            if ( file.find ( pf.exp + "cal.vlba" ) == 0 ):
                 logv.append ( path + "/" + file )
            if ( file.find ( pf.exp ) == 0 and file.find ( ".ant" ) ==  len(pf.exp) + 3 ):
                 loga.append ( path + "/" + file )
            nlog = nlog + 1

    if ( len(logv) + len(loga) == 0 ):
         print ( "pf.py: failed to find log files in", pf.exp_dir )
         exit ( 1 )

    if ( pf.verb > 1 ):
         if ( len(logv) > 0 ):
              pima_print_mes ( "pf.py: found %d log-files of vlba type for %s" % \
                               (len(logv), pf.exp_dir), log  )
         if ( len(loga) > 0 ):
              pima_print_mes ( "pf.py: found %s log-files of ant  type in %s" % \
                               (len(loga), pf.exp_dir), log  )

    if ( len(logv) > 0 ):
         com = pima_com + " " + "vlba_log_file " + logv[0]
         com = com + " " + \
               "BANDPASS_FILE:       NO" + " " +  \
               "BANDPASS_MASK_FILE:  NO" + " " +  \
               "PCAL_MASK_FILE:      NO" + " " +  \
               "POLARCAL_FILE:       NO" + " " +  \
               "TIME_FLAG_FILE:      NO" + " " +  \
               "SPLT.GAIN_CORR_FILE: NO"
         if ( pf.verb > 1 or pf.dry_run ):
              print ( "pf.py: " + com )
         if ( not pf.dry_run ):
              (ret, out ) = exe_noout_log ( com, pf.verb, log )
         else:
              ret = 0
         if ( ret != 0 ):
              if ( pf.verb > 0 ):
                   print ( "pf.py: Importing atnenna calibration into PIMA " + \
                           "for experiment", pf.exp, "FAILED due to an error on", date_str )
                   sys.stdout.flush()
              print ( " ", file=log )
              print ( "pf.py: Importing atnenna calibration into PIMA " + \
                      "for experiment", pf.exp, "FAILED due to an error on", \
                      date_str, file=log )
              print ( "FAILED due to an error on", date_str, file=log )
              print ( "=================================================", 
                       file=log )
              log.close()
              exit ( 1 )

    if ( len(loga) > 0 ):
         for file in loga:
             com = pima_com + " " + "pima_antab_file " + file
             com = com + " " + \
                   "BANDPASS_FILE:       NO" + " " +  \
                   "BANDPASS_MASK_FILE:  NO" + " " +  \
                   "PCAL_MASK_FILE:      NO" + " " +  \
                   "POLARCAL_FILE:       NO" + " " +  \
                   "TIME_FLAG_FILE:      NO" + " " +  \
                   "SPLT.GAIN_CORR_FILE: NO"
             if ( pf.verb > 1 or pf.dry_run ):
                  print ( "pf.py: " + com )
             if ( not pf.dry_run ):
                  (ret, out ) = exe_noout_log ( com, pf.verb, log )
             else:
                  ret = 0
             if ( ret != 0 ):
                  if ( pf.verb > 0 ):
                       print ( "pf.py: Importing atnenna calibration into PIMA " + \
                               "for experiment", pf.exp, "FAILED due to an error on", date_str )
                       sys.stdout.flush()
                  print ( " ", file=log )
                  print ( "pf.py: Importing atnenna calibration into PIMA " + \
                          "for experiment", pf.exp, "FAILED due to an error on", \
                          date_str, file=log )
                  print ( "FAILED due to an error on", date_str, file=log )
                  print ( "=================================================", 
                           file=log )
                  log.close()
                  exit ( 1 )
             
#
# --- Generate the final success message in the output log file
#
    if ( pf.dry_run ): exit ( 0 )
    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
    if ( pf.verb > 0 ):
         print ( "pf.py: Successful completion of importing calibration into PIMA for", 
                       pf.exp, "on", date_str )
         sys.stdout.flush()
    print ( " ", file=log )
    print ( "pf.py: Successful completion of importing calibration into PIMA for", 
                    pf.exp, "on", date_str, file=log )
    print ( "===========================================================" + \
            "=========================================", file=log )
    log.close()
    exit ( 0 )
           

#
# ------------------------------------------------------------------------
#
def pf_coarse ( pf ):
    """
    Perform fringing fitting of the VLBI experiment in coarse mode
    """
    log_file = pf.exp_dir + "/" + pf.exp + "_" + pf.band + "_coarse.log"
    fri_file = pf.exp_dir + "/" + pf.exp + "_" + pf.band + "_nobps.fri"
    frr_file = pf.exp_dir + "/" + pf.exp + "_" + pf.band + "_nobps.frr"

    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]

#
# --- Print start message in the log file
#
    if ( not pf.dry_run ):
         log = open ( log_file, "w" )
         print ( " ".join(sys.argv), file = log ) 
         print ( "Started  coarse fringing for experiment", pf.exp, "by PIMA on", date_str, file=log )
         print ( "=================================================================================", \
                  file=log )
         print ( " ", file=log )
         log.flush()
    if ( not pf.dry_run ):
         if ( os.path.isfile(fri_file) ): os.remove ( fri_file )
         if ( os.path.isfile(frr_file) ): os.remove ( frr_file )

    if ( pf.static ):
         pima_com = pima_exec["static"]  + " " + pf.cnt_file + " " + "frib"
    else:
         pima_com = pima_exec["dynamic"] + " " + pf.cnt_file + " " + "frib"

    with open(pf.cnt_file) as f:
         cnt = f.readlines()
    f.close()

    polar_use = "RR"
    polar_cal_file = "NO"
    bpsm_file = ""
    ppsm_file = ""
    for line in cnt:
        line.strip("\n")
        if ( line[0:6]  == "POLAR:" ):
             polar_use = line.split()[1]
        if ( line[0:19]  == "BANDPASS_MASK_FILE:" ):
             bpsm_file = line.split()[1]
        if ( line[0:15]  == "PCAL_MASK_FILE:" ):
             ppsm_file = line.split()[1]
 
    if ( bpsm_file != "NO" ):
         if ( not os.path.isfile(bpsm_file) ):
              bpsm_file = "NO"
    if ( ppsm_file != "NO" ):
         if ( not os.path.isfile(ppsm_file) ):
              ppsm_file = "NO"

    if ( polar_use == "I" ): polar_use = "RR"

    pima_com = pima_com + " "                              + \
               "FRINGE_FILE: "           + fri_file  + " " + \
               "FRIRES_FILE: "           + frr_file  + " " + \
               "BANDPASS_USE: "          + "NO"      + " " + \
               "BANDPASS_FILE: "         + "NO"      + " " + \
               "BANDPASS_MASK_FILE: "    + bpsm_file + " " + \
               "PCAL_MASK_FILE: "        + ppsm_file + " " + \
               "POLARCAL_FILE: "         + "NO"      + " " + \
               "POLAR: "                 + polar_use + " " + \
               "FRIB.OVERSAMPLE_MD: "    + "1"       + " " + \
               "FRIB.OVERSAMPLE_RT: "    + "1"       + " " + \
               "FRIB.FINE_SEARCH: "      + "PAR"     + " " + \
               "MKDB.FRINGE_ALGORITHM: " +  "DRF"

    if ( len(pf.pima_opts) > 0 ):
#
# ------ Add options to the command line
#
         for opt in pf.pima_opts:
             pima_com = pima_com + " " + opt

    if ( pf.verb > 1 or pf.dry_run ):
         print ( "pf.py: " + pima_com )
    if ( pf.dry_run ): return 0

    (ret, out ) = exe_noout_log ( pima_com, pf.verb, log )

    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
    if ( ret == 0 ): 
#
# ------ Generate the final success message in the output log file
#
         if ( pf.verb > 0 ):
              print ( "pf.py: Successful completion of coarse fringe fitting of", 
                       pf.exp, "band", pf.band, "on", date_str )
              sys.stdout.flush()
         print ( " ", file=log )
         print ( "pf.py: Successful completion of coarse fringe fitting of", 
                 pf.exp, "band", pf.band, "on", date_str, file=log )
         print ( "===========================================================" + \
                 "======================================", file=log )
         log.close()
         exit ( 0 )
    else:
#
# ------ Generate the error message to the output file
#
         if ( pf.verb > 0 ):
              print ( "pf.py: Coarse fringe fitting of experiment", pf.exp, \
                      "FAILED due to an error on", date_str )
              sys.stdout.flush()
         print ( " ", file=log )
         print ( "pf.py: Coarse fringe fitting of experiment", pf.exp, "band", pf.band, file=log )
         print ( "FAILED due to an error on", date_str, file=log )
         print ( "=================================================", 
                 file=log )
         log.close()
         exit ( 1 )
#
# ------------------------------------------------------------------------
#
def pf_bpas ( pf, insp ):
    """
    Compute complex bandpass
    """
    log_file = pf.exp_dir + "/" + pf.exp + "_" + pf.band + "_bps.log"
    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
#
# --- Print start message in the log file
#
    if ( not pf.dry_run ):
         log = open ( log_file, "w" )
         print ( " ".join(sys.argv), file = log ) 
         print ( "pf.py: computing bandpass for experiment", pf.exp, "by PIMA on", date_str, file=log )
         if ( insp ): print ( "pf.py: Evaluation of the band pass in INSPECTION mode", file=log )
         print ( "==================================================================================", \
                  file=log )
         print ( " ", file=log )
         log.flush()
    else:
         if ( insp ): print ( "pf.py: Evaluation of the band pass in INSPECTION mode" ) 
    if ( pf.static ):
         pima_com = pima_exec["static"]  + " " + pf.cnt_file + " " + "bpas"
    else:
         pima_com = pima_exec["dynamic"] + " " + pf.cnt_file + " " + "bpas"

    if ( len(pf.pima_opts) > 0 ):
#
# ------ Add options to the command line
#
         for opt in pf.pima_opts:
             pima_com = pima_com + " " + opt

    with open(pf.cnt_file) as f:
         cnt = f.readlines()
    f.close()

    polar_use = "RR"
    polar_cal_file = "NO"
    bpsm_file = ""
    for line in cnt:
        line.strip("\n")
        if ( line[0:6]  == "POLAR:" ):
             polar_use = line.split()[1]
        if ( line[0:15]  == "POLAR_CAL_FILE:" ):
             polar_cal_file = line.split()[1]
        if ( line[0:19]  == "BANDPASS_MASK_FILE:" ):
             bpsm_file = line.split()[1]

    if ( bpsm_file != "NO" ):
         if ( not os.path.isfile(bpsm_file) ):
              bpsm_file = "NO"

    fringe_file = pf_dir + "/" + pf.exp + "/" + pf.exp + "_" + pf.band + "_nobps.fri"
    if ( not os.path.isfile(fringe_file) ):
         print ( "pf.py: cannot find coarse fringe file ", fringe_file )
         exit ( 1 )
  
    exclude_obs_file = pf_dir + "/" + pf.exp + "/" + pf.exp + "_" + pf.band + "_bpas_obs.exc"

    if ( not os.path.isfile(exclude_obs_file) ):
         f = open ( exclude_obs_file, "w" )
         f.write ( "#" )
         f.close()

    if ( polar_use == "I" ): polar_use = "RR"
    pima_com = pima_com + " POLAR: " + polar_use + \
               " BANDPASS_MASK_FILE: " + bpsm_file + \
               " FRINGE_FILE: " + fringe_file + \
               " EXCLUDE_OBS_FILE: " + exclude_obs_file 

    if ( insp ): 
         pima_com = pima_com + " BPS.MODE: INSP  DEBUG_LEVEL: 3"
    else:
         pima_com = pima_com + " DEBUG_LEVEL: 3"
         if ( polar_cal_file != "NO" ):
              if ( os.path.isfile(polar_cal_file) ):
                   f = open ( polar_cal_file, "w" )
                   f.write ( "" )
                   f.close()
              
    if ( len(pf.pima_opts) > 0 ):
#
# ------ Add options to the command line
#
         for opt in pf.pima_opts:
             pima_com = pima_com + " " + opt

    if ( pf.verb > 1 or pf.dry_run ):
         print ( "pf.py: " + pima_com )
    if ( pf.dry_run ): return 0

    if ( insp ):
         (ret, out ) = exe_out_log   ( pima_com, pf.verb, log )
    else:
         (ret, out ) = exe_noout_log ( pima_com, pf.verb, log )

    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
    if ( ret == 0 ): 
#
# ------ Generate the final success message in the output log file
#
         if ( pf.verb > 0 ):
              print ( "pf.py: Successful completion of bandpass computation for", 
                       pf.exp, "band", pf.band, "on", date_str )
              sys.stdout.flush()
         print ( " ", file=log )
         print ( "pf.py: Successful completion of bandpass computation for", 
                 pf.exp, "band", pf.band, "on", date_str, file=log )
         print ( "===========================================================" + \
                 "======================================", file=log )
         log.close()
         exit ( 0 )
    else:
#
# ------ Generate the eror message to the output file
#
         if ( pf.verb > 0 ):
              print ( "pf.py: FAILURE in computation of bandpass for experiment", pf.exp, \
                      "band", pf.band, "on", date_str )
#              ib = len(out) - 4
#              ie = len(out) - 1
#              if ( ib < 0 ): ib = 0
#              for i in range(ib,ie):
#                  print ( out[i] )
              sys.stdout.flush()
         print ( " ", file=log )
         print ( "pf.py: FAILURE in computation of bandpass for experiment", pf.exp, \
                 "band", pf.band, "on", date_str, file=log )
         print ( "==========================================================================", 
         file=log )
         log.close()
         exit ( 1 )
#
# ------------------------------------------------------------------------
#
def pf_fine ( pf, keep ):
    """
# ************************************************************************
# *                                                                      *
# *  Perform fringing fitting of the VLBI experiment in fine mode
# *                                                                      *
# ************************************************************************
    """
    log_file = pf.exp_dir + "/" + pf.exp + "_" + pf.band + "_fine.log"
    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
#
# --- Print start message in the log file
#
    fri_file = pf.exp_dir + "/" + pf.exp + "_" + pf.band + ".fri"
    frr_file = pf.exp_dir + "/" + pf.exp + "_" + pf.band + ".frr"
    if ( not pf.dry_run ):
         log = open ( log_file, "w" )
         print ( " ".join(sys.argv), file = log ) 
         print ( "Started  fine fringing of experiment", pf.exp, "by PIMA on", date_str, file=log )
         print ( "==============================================================================", \
                  file=log )
         print ( " ", file=log )
         log.flush()
    if ( not keep and not pf.dry_run ):
         if ( os.path.isfile(fri_file) ): os.remove ( fri_file )
         if ( os.path.isfile(frr_file) ): os.remove ( frr_file )
    elif ( not pf.dry_run ):
         print ( "pf.py: keep existing files with fringe results", file=log )
         log.flush()

    if ( pf.static ):
         pima_com = pima_exec["static"]  + " " + pf.cnt_file + " " + "frib"
    else:
         pima_com = pima_exec["dynamic"] + " " + pf.cnt_file + " " + "frib"

    if ( len(pf.pima_opts) > 0 ):
#
# ------ Add options to the command line
#
         for opt in pf.pima_opts:
             pima_com = pima_com + " " + opt

    if ( pf.verb > 1 or pf.dry_run ):
         print ( "pf.py: " + pima_com )
    if ( pf.dry_run ): return 0

    (ret, out ) = exe_noout_log ( pima_com, pf.verb, log )

    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
    if ( ret == 0 ): 
#
# ------ Generate the final success message in the output log file
#
         if ( pf.verb > 0 ):
              print ( "pf.py: Successful completion of fine fringe fitting of", 
                       pf.exp, "band", pf.band, "on", date_str )
              sys.stdout.flush()
         print ( " ", file=log )
         print ( "pf.py: Successful completion of fine fringe fitting of", 
                 pf.exp, "band", pf.band, "on", date_str, file=log )
         print ( "=========================================================" + \
                 "======================================", file=log )
         log.close()
         exit ( 0 )
    else:
#
# ------ Generate the error message to the output file
#
         if ( pf.verb > 0 ):
              print ( "pf.py: Fine fringe fitting of experiment", pf.exp, \
                      "FAILED due to an error on", date_str )
              sys.stdout.flush()
         print ( " ", file=log )
         print ( "pf.py: Fine fringe fitting of experiment", pf.exp, "band", pf.band, file=log )
         print ( "FAILED due to an error on", date_str, file=log )
         for line in out:
             print ( "pf.py: ", line.strip("\n"), file=log )
         print ( "=================================================", 
                 file=log )
         log.close()
         exit ( 1 )

#
# ------------------------------------------------------------------------
#
def pf_mkdb ( pf, task, updt ):
    """
    Create an output database for astrometry/geodesy analysis
    """
    log_file = pf.exp_dir + "/" + pf.exp + "_mkdb.log"
    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
#
# --- Print start message in the log file
#
    if ( not pf.dry_run ):
         log = open ( log_file, "w" )
         print ( " ".join(sys.argv), file = log ) 
         print ( "pf.py: Started  creation of the GVF database for experiment", 
                  pf.exp, "by PIMA on", date_str, file=log )
         print ( "===========================================================" + \
                 "===========================================", \
                  file=log )
         print ( " ", file=log )
         log.flush()

    if ( pf.static ):
         pima_com = pima_exec["static"] + " " + pf.cnt_file + " " + "mkdb"
    else:
         pima_com = pima_exec["dynamic"] + " " + pf.cnt_file + " " + "mkdb"

    if ( len(pf.pima_opts) > 0 ):
#
# ------ Add options to the command line
#
         for opt in pf.pima_opts:
             pima_com = pima_com + " " + opt

    pima_com = pima_com +                     " " + \
               "STAGING_DIR: NO" + " " + \
               "CHECK_SEVERITY: 1" + " " + \
               "MKDB.FRINGE_ALGORITHM: ADD"

    if ( task == "mktxt" ):
         pima_com = pima_com + " " + \
                   "MKDB.OUTPUT_TYPE:  TEXT " + \
                   "MKDB.OUTPUT_NAME:  " + pf.exp_dir + "/" + pf.exp + "_db.txt" 
         fmt = "ascii"
    else:
         fmt = "gvf"
              
    if ( len(pf.pima_opts) > 0 ):
#
# ------ Add options to the command line
#
         for opt in pf.pima_opts:
             pima_com = pima_com + " " + opt

    if ( pf.verb > 1 or pf.dry_run ):
         print ( "pf.py: " + pima_com )
    if ( pf.dry_run ): return 0

    log.flush()
    (ret, out) = exe_noout_log ( pima_com, pf.verb, log )
    if ( ret != 0 ):
#
# ------ Generate the eror message to the output file
#
         if ( pf.verb > 0 ):
              print ( "pf.py: An attempt to create a " + fmt +  \
                      " database for experiment", pf.exp, \
                      "has been abnormally terminated on", date_str )
              sys.stdout.flush()
         print ( " ", file=log )
         print ( "pf.py: An attempt to create a " + fmt + \
                 " database for experiment", pf.exp, \
                 "has been abnormally terminated on", date_str, file=log )
         log.close()
         for line in out:
             print ( "pf.py: ", line.strip("\n") )
         exit ( 1 )

#
# --- Get the database name
#
    db_name = None
    for line in out:
        num_words = len(line.split())
        if ( num_words == 0 ): continue
        db_name = line.split()[num_words-1]

    if ( updt  and task == "mkdb" ):
         if ( db_name ):
              com = pima_local.gvf_promote_exec  + " " + db_name.replace("v001.env","v002.env")
              (ret, out ) = exe_noout_log ( com, pf.verb, log )
              if ( ret != 0 ):
                   print ( "Failed to run promote outlier flags for experiment", pf.exp )
                   print ( "Failed to run promote outlier flags for experiment", pf.exp,
                            file=log )
                   exit ( 1 )
    else:
         ret = 0

    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
    if ( ret == 0 ): 
#
# ------ Generate the final success message in the output log file
#
         if ( pf.verb > 0 ):
              print ( "pf.py: Database in GVF format for experiment", pf.exp, \
                      "has been successfully created on", date_str )
              sys.stdout.flush()
         print ( " ", file=log )
         print ( "pf.py: Finished creation of the GVF database for experiment", \
                  pf.exp, "on", date_str, file=log )
         print ( "===============================================================" + \
                 "===============================", \
                 file=log )
         log.close()
         with open(log_file) as f:
              log_buf = f.readlines()
         f.close()
         db_name = None
         for line in log_buf:
             ind = line.find("v001.env")
             if ( ind > 1 ):
                  db_name = line[ind-11:].strip("\n")
         if ( task == "mkdb" and pf.verb > 0 ):
              print ( "pf.py: Database name:", db_name )
              sys.stdout.flush()
         elif ( task == "mktxt" and pf.verb > 0 ):
              print ( "pf.py: Database file:", pf.exp_dir + "/" + pf.exp + "_db.txt" )
              sys.stdout.flush()
              
         exit ( 0 )
    else:
#
# ------ Generate the eror message to the output file
#
         if ( pf.verb > 0 ):
              print ( "pf.py: An attempt to create a GVF database for experiment ", pf.exp, \
                      " has been abnormally terminated " + date_str )
              sys.stdout.flush()
         print ( " ", file=log )
         print ( "pf.py: An attempt to create a GVF database for experiment ", pf.exp, \
                  " has been abnormally terminated " + date_str, file=log )
         print ( "===========================================================================", \
                 file=log )
         log.close()
         exit ( 1 )
#
# ------------------------------------------------------------------------
#
def pf_splt ( pf ):
    """
    Averge the visibilites, calibrate, and split, one file per source
    """
    log_file      = pf.exp_dir + "/" + pf.exp + "_" + pf.band + "_splt.log"
    splt_exc_file = pf.exp_dir + "/" + pf.exp + "_" + pf.band + "_exc_sol.obs"

    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
#
# --- Print start message in the log file
#
    if ( not pf.dry_run ):
         log = open ( log_file, "w" )
         print ( " ".join(sys.argv), file = log ) 
         print ( "pf.py: Started  splitting the calibrated visibilities for experiment", \
                  pf.exp, "band", pf.band, "on", date_str, file=log )
         print ( "=====================================================================" \
                 "=========================================", \
                  file=log )
         print ( " ", file=log )
         log.flush()

#
# --- Read pima control file into cnt
#
    with open(pf.cnt_file) as f:
         cnt = f.readlines()
    f.close()

#
# --- Search there for keywords MKDB.DESC_FILE, FRINGE_FILE, and FRIRES_FILE
#
    desc_file     = None
    orig_fri_file = None
    orig_frr_file = None
    for line in cnt:
        line.strip("\n")
        if ( line.split()[0] == "MKDB.DESC_FILE:" ):
             desc_file = line.split()[1]

    if ( not desc_file ):
         print ( "pf.py: cannot find the keyword MKDB.DESC_FILE: " \
                 "in control file ", pima_cnt )

    if ( not os.path.isfile(desc_file) ):
         print ( "pf.py: Cannot find description file " + desc_file + \
                 " specified by the keyword MKDB.DESC_FILE: in control file ", \
                 pima_cnt )
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
         print ( "pf.py: cannot find the keyword DB_NAME: " \
                 "in the description file ", desc_file, \
                 "specified by the keyword MKDB.DESC_FILE: in control file ", \
                 pima_cnt )
         exit ( 1 )

    gvf_arg = "10" # This the first (upper) band or maybe the only band
    if ( pf.band == "c" or pf.band == "s" ):
#
# ------ Check whether this is the second band of the dual-band experiment
#
         if ( pf.band == "c" ):
              ind = pf.cnt_file.rfind("_c_")
         if ( pf.band == "s" ):
              ind = pf.cnt_file.rfind("_s_")
         if ( ind < 1 ):
              print ( "pf.py: Trap of internal control: unsupported name of" \
                      " the pima control file", pf.cnt_file )
              exit ( 1 )
#
# ------ Buukld the name of the upper-band control file
#
         up_cnt_file = pf.cnt_file[0:ind] + "_x" + pf.cnt_file[ind+2:] 
         if ( os.path.isfile(up_cnt_file) ):
#
# ----------- Yes! There exists the PIMA control file for the upper band
#
              gvf_arg = "20" # This is the  second (lower) band
              if ( pf.verb > 0 ):
                   print ( "pf.py The second (lower) band will be processed" )
              print ( "pf.py The second (lower) band will be processed", file=log )
              
#
# --- Build command line for generation the list of observations suppressed
# --- by VTD/Post-Solve
#
    gvf_db_com = pima_local.gvf_db_exec + " " + \
                 db_name + " " + \
                 gvf_arg

    (ret, out ) = exe_noout_nolog ( gvf_db_com, pf.verb )
    if ( ret != 0 ):
         for line in out:
             print ( line, file=log )
         if ( pf.verb > 0 ):
              print ( "pf.py Error in pima_local.gvf_db_exec" )
         print ( "pf.py Error in pima_local.gvf_db_exec", file=log )
         log.close()
         exit ( 1 )

    se = open ( splt_exc_file, "w" )
    for i in range(1,len(out)):
        print ( out[i], file=se )
    se.close()

    if ( pf.static ):
         pima_com = pima_exec["static"]  + " " + pf.cnt_file + " " + "splt"
    else:
         pima_com = pima_exec["dynamic"] + " " + pf.cnt_file + " " + "splt"

    pima_com = pima_com                             + " " + \
               "EXCLUDE_OBS_FILE: " + splt_exc_file + " " + \
               "DEBUG_LEVEL: "      + "2"

    if ( len(pf.pima_opts) > 0 ):
#
# ------ Add options to the command line
#
         for opt in pf.pima_opts:
             pima_com = pima_com + " " + opt

    if ( pf.verb > 1 or pf.dry_run ):
         print ( "pf.py: " + pima_com )
    if ( pf.dry_run ): return 0

    log.flush()
    (ret, out) = exe_noout_log ( pima_com, pf.verb, log )
    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
    if ( ret == 0 ): 
#
# ------ Generate the final success message in the output log file
#
         if ( pf.verb > 0 ):
              print ( "pf.py: Finished splitting the calibrated visibilities for experiment", \
                       pf.exp, "band", pf.band, "on", date_str )
              sys.stdout.flush()
         print ( " ", file=log )
         print ( "pf.py: Finished splitting the calibrated visibilities for experiment", \
                  pf.exp, "band", pf.band, "on", date_str, file=log )
         print ( "==============================================================================================================", \
                  file=log )
         log.close()
         exit ( 0 )
    else:
#
# ------ Send the error message to the output file
#
         if ( pf.verb > 0 ):
              print ( "pf.py: ERROR in splitting calibrated visibilities for experiment", pf.exp, \
                      "band", pf.band, date_str )
              sys.stdout.flush()
         print ( " ", file=log )
         print ( "pf.py: ERROR in splitting calibrated visibilities for experiment", pf.exp, \
                 "band", pf.band, date_str, file=log )
         log.close()
         exit ( 1 )

#
# ------------------------------------------------------------------------
#
def pf_autm ( pf ):
    """
    Run automated imaging using split and calibrated visibilities
    """
    log_file      = pf.exp_dir + "/" + pf.exp + "_" + pf.band + "_autm.log"

    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
#
# --- Print start message in the log file
#
    if ( not pf.dry_run ):
         log = open ( log_file, "w" )
         print ( " ".join(sys.argv), file = log ) 
         print ( "pf.py: Started  automated imaging using splitt and calibrated visibilities for experiment", \
                  pf.exp, "band", pf.band, "on", date_str, file=log )
         print ( "=====================================================================" \
                 "=========================================", \
                  file=log )
         print ( " ", file=log )
         log.flush()


#
# --- Read pima control file into cnt
#
    with open(pf.cnt_file) as f:
         cnt = f.readlines()
    f.close()

#
# --- Search there for keywords EXPER_DIR
#
    scr_dir = None
    for line in cnt:
        line.strip("\n")
        if ( line.split()[0] == "EXPER_DIR:" ):
             scr_dir = line.split()[1]

    if ( not os.path.isdir(scr_dir) ):
         print ( "pf.py: Cannot find experiment directory", scr_dir + \
                 "specified by the keyword EXPER_DIR: in control file ", \
                 pima_cnt )
         exit ( 1 )

    uvs_dir = scr_dir + "/" + pf.exp + "_uvs"

    if ( not os.path.isdir(uvs_dir) ):
         print ( "pf.py: Cannot find directory", uvs_dir, \
                 "where calibrated visibilites are supposed to reside"  )
         exit ( 1 )

    uva_files = []
    for path, dirs, files in os.walk(uvs_dir):
        for file in files:
            if ( file.find("uva.fits") > 0 and \
                 file.find("_" + pf.band.upper() + "_") > 0 ):
                 uva_files.append ( uvs_dir + "/" + file )

    if ( len(uva_files) == 0 ):
         print ( "pf.py: Did not find files with calibrated visibilities", \
                 "for band", pf.band, "in directory", uvs_dir )
         exit ( 1 )

    uva_files.sort()
    n_suc = 0
    n_fai = 0
    for uva_file in uva_files:
        if ( pf.verb > 1 ):
             print ( "Imaging", uva_file[uva_file.rfind("/")+1:] )
        pima_com = pima_local.automap_exec + " " + uva_file
        (ret, out) = exe_noout_log ( pima_com, pf.verb, log )
        if ( ret == 0 ):
             n_suc = n_suc + 1
        else:
             n_fai = n_fai + 1


    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
    if ( n_suc > 0 ): 
#
# ------ Generate the final success message in the output log file
#
         if ( pf.verb > 0 ):
              print ( "pf.py: Finished imaging using split and calibrated visibilities for experiment", \
                       pf.exp, "band", pf.band, "on", date_str )
              sys.stdout.flush()
         print ( " ", file=log )
         print ( "pf.py: Finished imaging using split and calibrated visibilities for experiment", \
                         pf.exp, "band", pf.band, "on", date_str, file=log )
         if ( n_fai ==  0 ):
              print ( "pf.py: %d maps were made, no failures" % n_suc )
              print ( "pf.py: %d maps were made, no failures" % n_suc, file=log )
         else:
              print ( "pf.py: %d maps were made, %d failed" % (n_suc, n_fai) )
              print ( "pf.py: %d maps were made, %d failed" % (n_suc, n_fai), file=log )
         print ( "=====================================================================", \
                  file=log )
         log.close()
         exit ( 0 )
    else:
#
# ------ Send the error message to the output file
#
         if ( pf.verb > 0 ):
              print ( "pf.py: ERROR in imaging split and calibrated visibilities for experiment", pf.exp, \
                      "band", pf.band, date_str )
              sys.stdout.flush()
         print ( " ", file=log )
         print ( "pf.py: ERROR in imaging split and calibrated visibilities for experiment", pf.exp, \
                 "band", pf.band, date_str, file=log )
         log.close()
         exit ( 1 )
#
# ------------------------------------------------------------------------
#
def pf_pict ( pf ):
    """
    Generate picture files of maps and visibilities against baseline length
    """
    log_file      = pf.exp_dir + "/" + pf.exp + "_" + pf.band + "_autm.log"

    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
#
# --- Read pima control file into cnt
#
    with open(pf.cnt_file) as f:
         cnt = f.readlines()
    f.close()

#
# --- Search there for keyword EXPER_DIR
#
    scr_dir = None
    for line in cnt:
        line.strip("\n")
        if ( line.split()[0] == "EXPER_DIR:" ):
             scr_dir = line.split()[1]

    if ( not os.path.isdir(scr_dir) ):
         print ( "pf.py: Cannot find experiment directory", scr_dir + \
                 "specified by the keyword EXPER_DIR: in control file ", \
                 pima_cnt )
         exit ( 1 )

    uvs_dir = scr_dir + "/" + pf.exp + "_uvs"

    if ( not os.path.isdir(uvs_dir) ):
         print ( "pf.py: Cannot find directory", uvs_dir, \
                 "where calibrated visibilites are supposed to reside"  )
         exit ( 1 )

    map_files = []
    for path, dirs, files in os.walk(uvs_dir):
        for file in files:
            if ( file.find("map.fits") > 0 and \
                 file.find("_" + pf.band.upper() + "_") > 0 ):
                 map_files.append ( uvs_dir + "/" + file )

    if ( len(map_files) == 0 ):
         print ( "pf.py: Did not find map files in FITS format", \
                 "for band", pf.band, "in directory", uvs_dir )
         exit ( 1 )

    map_files.sort()
    n_map = 0
    for map_file in map_files:
        if ( pf.verb > 1 ):
             print ( "Processing map", map_file[map_file.rfind("/")+1:] )
       
        map_gif  = map_file.replace("map.fits","map.gif")
        uvs_fits = map_file.replace("map.fits","uvs.fits")
        uvs_gif  = map_file.replace("map.fits","uvs.gif")
        n_map = n_map + 1
        if ( pf.band == "k" ):
             map_opt = "-box  8.0"
        elif ( pf.band == "x" ):
             map_opt = "-box 25.0"
        elif ( pf.band == "c" ):
             map_opt = "-box 40.0"
        elif ( pf.band == "s" ):
             map_opt = "-box 80.0"
        else:
             map_opt = "-box 25.0"

        pima_com = pima_local.fits_to_map_exec + " " + map_opt + \
                   " -o " + map_gif + " " + map_file
        (ret, out) = exe_noout_nolog ( pima_com, pf.verb )

        pima_com = pima_local.fits_to_radplot_exec + \
                   " -o " + uvs_gif + " " + uvs_fits
        (ret, out) = exe_noout_nolog ( pima_com, pf.verb )

    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
#
# - Generate the final success message in the output log file
#
    if ( pf.verb > 0 ):
         print ( "pf.py: Finished generating pictures for experiment", \
                  pf.exp, "band", pf.band, "on", date_str )
         print ( "pf.py: Pictures were generated for %d maps" % n_map )
    sys.stdout.flush()
    exit ( 0 )
#
# ------------------------------------------------------------------------
#
def pf_gain ( pf, gain_file ):
    """
    Generate picture files of maps and visibilities against baseline length
    """
    log_file      = pf.exp_dir + "/" + pf.exp + "_" + pf.band + "_gain.log"
    if ( not pf.dry_run ):
         log = open ( log_file, "w" )

    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
#
# --- Read pima control file into cnt
#
    with open(pf.cnt_file) as f:
         cnt = f.readlines()
    f.close()

    uv_fits_file = None
    exper_dir    = None

    for line in cnt:
        if ( line[0:1] == "#" ): continue
        if ( line[0:8] == "UV_FITS:" ):
             uv_fits_file = line.split()[1]
        if ( line[0:10] == "EXPER_DIR:" ):
             exper_dir = line.split()[1]

    if ( exper_dir == None ):
         pima_print_mes ( "Trap of internal control: cannot find " + \
                          "EXPER_DIR keyword in the control file " + pf.cnt_file, log )
         exit ( 1 )

#
# --- Parse FITS -file
#
    pima_com = pima_local.fitsh + " " + uv_fits_file
    if ( pf.dry_run ):
         (ret, uv_dump) = exe_noout_nolog ( pima_com, pf.verb )
    else:
         (ret, uv_dump) = exe_noout_log ( pima_com, pf.verb, log )
    if ( ret != 0 ):
         pima_print_mes ( "Failure in getting dump of visibility file " + \
                           uv_fits_file, log )
         exit ( 1 )

    ref_freq = 0.0
    for line in uv_dump:
        if ( line.find("REF_FREQ=") > 0 ):
             ref_freq = float(line.split()[5])

    pima_print_mes ( "REF_FREQ = %f" % ref_freq, log )
    if ( ref_freq == 0.0 ):
         pima_print_mes ( "Could not find reference frequency in uv file " + \
                           uv_fits_file, log )
         exit ( 1 )

    sta_fil = exper_dir + "/" + pf.exp + ".sta"
    if ( not os.path.isfile(sta_fil) ):
         pima_print_mes ( "Trap of internal control: cannot find " + \
                          "station dump file " + sta_fil, log )
         exit ( 1 )

    with open(sta_fil) as f:
         sta_buf = f.readlines()
    f.close()

    num_vlba = 0
    num_novlba = 0
    for line in sta_buf:
        if ( line.split()[3] in vlba_sta_list ):
             num_vlba = num_vlba + 1
        else:
             num_novlba = num_novlba + 1

    if ( num_vlba   > 0 and pf.verb > 1 ):
         pima_print_mes ( "Found %d VLBA stations in %s" % (num_vlba, pf.exp), log )
    if ( num_novlba > 0 and pf.verb > 1 ):
         pima_print_mes ( "Found %d VLBA stations in %s" % (num_novlba, pf.exp), log )
         
    gain_band = None
#
    if ( pf.band == "l" ):
         gain_band = "18cm"
    elif ( pf.band == "s" ):
         x_cnt_file = pf.cnt_file.replace("_"+pf.band+"_pima.cnt","_x_pima.cnt")
         if ( os.path.isfile(x_cnt_file) ):
              gain_band = "13cmsx"
         else:
              gain_band = "13cm"
    elif ( pf.band == "c" ):
        if ( ref_freq < 7.0e9 ):
             gain_band = "6cm"
        else:
             gain_band = "7ghz"
    elif ( pf.band == "x" ):
         s_cnt_file = pf.cnt_file.replace("_"+pf.band+"_pima.cnt","_s_pima.cnt")
         if ( os.path.isfile(s_cnt_file) ):
              gain_band = "4cmsx"
         else:
              gain_band = "4cm"
         if ( ref_freq > 3.8e9 and ref_freq < 7.0e9 ):
              gain_band = "7ghz"
    elif ( pf.band == "u" ):
         gain_band = "2cm"
    elif ( pf.band == "k" ):
        if ( ref_freq < 23.0e9 ):
             gain_band = "1cm"
        else:
             gain_band = "24ghz"
    elif ( pf.band == "q" ):
         gain_band = "7mm"
    elif ( pf.band == "w" ):
         gain_band = "3mm"
    else:
         pima_print_mes ( "Did not idenfied VLBA band id for band " + pf.band, \
                           log )
         exit ( 1 )           
    pima_print_mes ( "Gain band: " + gain_band, log )
    
#
# --- Try three cases
#
    for i in range(0,3):
        pima_com = None
#
        if ( i == 0 and gain_file != None ):
               pima_com = "vlba_gain" + " " + gain_file + " " + \
                          "gain_band" + " " + gain_band
        elif ( i == 1 and gain_file == None and num_vlba > 0 ): 
               pima_com = "vlba_gain" + " " + vlba_gain_file + " " + \
                          "gain_band" + " " + gain_band
        elif ( i == 2 and gain_file == None and num_novlba > 0 ): 
               pima_com = "vlba_gain" + " " + ivs_gain_file + " " + \
                          "gain_band" + " " + gain_band

        if ( pima_com == None ): continue

        if ( pf.static ):
             pima_com = pima_exec["static"]  + " " + pf.cnt_file + " " + \
                        "gean " + pima_com
        else:
             pima_com = pima_exec["dynamic"] + " " + pf.cnt_file + " " + \
                        "gean " + pima_com

        if ( not pf.dry_run ):
             (ret, out) = exe_noout_log ( pima_com, pf.verb, log )
        else:
             print ( "pima_com = ", pima_com )
             ret = 0
        if ( ret != 0 ):
             pima_print_mes ( "Errors during imporing gain " + \
                              "from file " + vlba_gain_file, log )
             for line in out:
                 print ( "pf.py: ", line.strip("\n") )
             exit ( 1 )
 
#
# --- Now extract gains from PIMA internal data structures and put
# --- it in the log and on the screen
#
    if ( pf.static ):
          pima_com = pima_exec["static"] + " " + pf.cnt_file + " " + "prga"
    else:
          pima_com = pima_exec["dynamic"] + " " + pf.cnt_file + " " + "prga"
    (ret, out) = exe_noout_log ( pima_com, pf.verb, log )
    if ( ret != 0 ):
         pima_print_mes ( "Errors during reading antenna gain " + \
                          "from PIMA internal structures", log )
         exit ( 1 )
#
# - Generate the final success message in the output log file
#
    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
    if ( pf.verb > 0 ):
         print ( "pf.py: Finished importing gain to PIMA for experiment", \
                  pf.exp, "band", pf.band, "on", date_str )
    if ( not pf.dry_run ):
         print ( "pf.py: Finished importing gain to PIMA for experiment", \
                 pf.exp, "band", pf.band, "on", date_str, file=log )

    sys.stdout.flush()
    exit ( 0 )
#
# ------------------------------------------------------------------------
#
def pf_map ( pf ):
    """
    Automatic imaging of all the sources of given exdperiment at a given band
    """
    log_file      = pf.exp_dir + "/" + pf.exp + "_" + pf.band + "_map.log"
    if ( not pf.dry_run ):
         log = open ( log_file, "w" )

    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
    pima_print_mes ( "Started  imaging all the sources " + \
                     " at " + pf.band + " band by PIMA on " + date_str, log )
    pima_print_mes ( "=====================================================" + \
                     "=========================", log )
#
# --- Task gain
#
    if ( pf.verb > 0 ):
         date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
         print ( "pf.py Started importing antenna gain to " + pf.exp + \
                  " at " + pf.band + " on " + date_str )
         
    pima_com = pima_bin + "/pf.py " + pf.exp + " " + pf.band + " gain"
    (ret, out) = exe_noout_log ( pima_com, pf.verb, log )
    if ( ret != 0 ):
         pima_print_mes ( "Failure in importing antenna gain", log )
         for line in out:
             print ( "pf.py: ", line.strip("\n") )
         exit ( 1 )

#
# --- Task splt
#
    if ( pf.verb > 0 ):
         date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
         print ( "pf.py Started task splt for " + pf.exp + \
                  " at " + pf.band + " on " + date_str )
         
    pima_com = pima_bin + "/pf.py " + pf.exp + " " + pf.band + " splt"
    if ( len(pf.pima_opts) > 0 ):
#
# ------ Add options to the command line
#
         for opt in pf.pima_opts:
             pima_com = pima_com + " " + opt

    (ret, out) = exe_noout_log ( pima_com, pf.verb, log )
    if ( ret != 0 ):
         pima_print_mes ( "Failure in running splt task", log )
         for line in out:
             print ( "pf.py: ", line.strip("\n") )
         exit ( 1 )
#
# --- Automatic mapping
#
    if ( pf.verb > 0 ):
         date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
         print ( "pf.py Started automatic imaging for " + pf.exp + \
                  " at " + pf.band + " on " + date_str )

    pima_com = pima_bin + "/pf.py " + pf.exp + " " + pf.band + " autm"
    (ret, out) = exe_noout_log ( pima_com, pf.verb, log )
    if ( ret != 0 ):
         pima_print_mes ( "Failure in automatic mapping", log )
         for line in out:
             print ( "pf.py: ", line.strip("\n") )
         exit ( 1 )
#
# --- Generating pictures
#
    if ( pf.verb > 0 ):
         date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
         print ( "pf.py Started generating pictures for " + pf.exp + \
                  " at " + pf.band + " on " + date_str )

    pima_com = pima_bin + "/pf.py " + pf.exp + " " + pf.band + " pict"
    (ret, out) = exe_noout_log ( pima_com, pf.verb, log )
    if ( ret != 0 ):
         pima_print_mes ( "Failure in an attempt to generate pictures", log )
         for line in out:
             print ( "pf.py: ", line.strip("\n") )
         exit ( 1 )

    pima_print_mes ( "Finished imaging all the sources " + \
                     " at " + pf.band + " band by PIMA on " + date_str, log )
    pima_print_mes ( "=====================================================" + \
                     "=========================", log )
    log.close()

#
# ------------------------------------------------------------------------
#
def main():
    """ 
    Parsing arguments
    """
    parser = argparse.ArgumentParser( description=pf__label )
    parser.add_argument('--version', action='version', version=pf__version )

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

    subparsers = parser.add_subparsers ( help="PIMA tasks" )

#
# --- Task: load
#
    load_parser = subparsers.add_parser ( "load", \
                                          help="Load VLBI experiment." +
                                               "Options: -nopcal" )
    load_parser.set_defaults ( task="load" )
    
    load_parser.add_argument ( "-nopcal", \
                               dest="nopcal", \
                               action="store_true", \
                               default=None, \
                               help="Do not load phase calibration information" )
    load_parser.add_argument ( "pima_opts", nargs=argparse.REMAINDER )

#
# --- Task: logs
#
    logs_parser = subparsers.add_parser ( "logs", \
                                           help="Parse VLBI log files" )
    logs_parser.set_defaults ( task="logs" )
    logs_parser.add_argument ( "pima_opts", nargs=argparse.REMAINDER )
#
# --- Task: gean
#
    gean_parser = subparsers.add_parser ( "gean", \
                                           help="Get antenna calibrations" )
    gean_parser.set_defaults ( task="gean" )
    gean_parser.add_argument ( "pima_opts", nargs=argparse.REMAINDER )
#
# --- Task: coarse
#
    coarse_parser = subparsers.add_parser ( "coarse", \
                                            help="Fringe fit VLBI experiment in the coarse mode" )
    coarse_parser.set_defaults ( task="coarse" )
    coarse_parser.add_argument ( "pima_opts", nargs=argparse.REMAINDER )
#
# --- Task: bpas
#
    bpas_parser = subparsers.add_parser ( "bpas", \
                                           help="Compute complex bandpass" )
    bpas_parser.set_defaults ( task="bpas" )
    bpas_parser.add_argument ( "-insp", "--inspect", \
                               dest="insp", \
                               action="store_true", \
                               default=None, \
                               help="Inspect cross and auto correlation" )
    bpas_parser.add_argument ( "pima_opts", nargs=argparse.REMAINDER )
#
# --- Task: fine
#
    fine_parser = subparsers.add_parser ( "fine", \
                                          help="Perform fine fringe fitting" )
    fine_parser.set_defaults ( task="fine" )
    fine_parser.add_argument ( "-keep", \
                               dest="keep", \
                               action="store_true", \
                               default=None, \
                               help="Append results of fringe fitting to the existing output files" )
    fine_parser.add_argument ( "pima_opts", nargs=argparse.REMAINDER )
#
# --- Task: mkdb
#
    mkdb_parser = subparsers.add_parser ( "mkdb", \
                                           help="Generate output database in GVF format" )
    mkdb_parser.set_defaults ( task="mkdb" )
    mkdb_parser.add_argument ( "-updt", "--update", \
                               dest="updt", \
                               action="store_true", \
                               default=None, \
                               help="Update suppression status" )
    mkdb_parser.add_argument ( "pima_opts", nargs=argparse.REMAINDER )
#
# --- Task: mktxt
#
    mktxt_parser = subparsers.add_parser ( "mktxt", \
                                           help="Generate output database in ascii format" )
    mktxt_parser.set_defaults ( task="mktxt" )
    mktxt_parser.add_argument ( "pima_opts", nargs=argparse.REMAINDER )
#
# --- Task: splt
#
    splt_parser = subparsers.add_parser ( "splt", \
                                           help="Split the calibrated and averged visibilities, one file per source" )
    splt_parser.set_defaults ( task="splt" )
    splt_parser.add_argument ( "pima_opts", nargs=argparse.REMAINDER )
#
# --- Task: gain
#
    gain_parser = subparsers.add_parser ( "gain", \
                                           help="Updata antenna gain using external table(s)" )
    gain_parser.set_defaults ( task="gain" )
    gain_parser.add_argument ( "-gf", "--gain_file", \
                               dest="gf", \
                               action="store", \
                               default=None, \
                               help="Gain file" )
    gain_parser.add_argument ( "pima_opts", nargs=argparse.REMAINDER )
#
# --- Task: automap
#
    autm_parser = subparsers.add_parser ( "autm", \
                                           help="Run autiomated imaging using split calibrated and averged visibilities" )
    autm_parser.set_defaults ( task="autm" )
    autm_parser.add_argument ( "pima_opts", nargs=argparse.REMAINDER )
#
# --- Task: pict
#
    pict_parser = subparsers.add_parser ( "pict", \
                                           help="Generate picture files of maps and visibilities against baseline length" )
    pict_parser.set_defaults ( task="pict" )
    pict_parser.add_argument ( "pima_opts", nargs=argparse.REMAINDER )
#
# --- Task: map
#
    map_parser = subparsers.add_parser ( "map", \
                                          help="Run autiomated imaging using split calibrated and averged visibilities" )
    map_parser.set_defaults ( task="map" )
    map_parser.add_argument ( "pima_opts", nargs=argparse.REMAINDER )


#
# --- Get and parse options
#
    args = parser.parse_args()

    try:
         if ( args.task == None ): args.task = None
    except:
         print ( "pf.py: A task should be specified in the 3rd argument" )
         print ( "       to see the list of supported task, try pf.py -help" )
         exit  ( 1 )

    pf = pf_class ( args.task, args.exp, args.band, args.verb, args.dry_run, args.static, \
                    args.pima_opts ) 

    if ( args.band not in band_list ):
         print ( "ERROR pf.py: band " + args.band + " is not known.\n"  \
                 "The list of known bands: " + ', '.join(band_list) )
         exit ( 1 )

    pf.exp_dir  = pf_dir + "/" + args.exp
    if ( not os.path.isdir(pf.exp_dir) ):
         print ( "ERROR pf.py: directory " + pf.exp_dir + " does not exist.\n" +
                 "Please check experiment, band, and directory " + pf_dir )
         exit ( 1 )

    pf.cnt_file = pf.exp_dir  + "/" + args.exp + "_" + args.band + "_pima.cnt"
    if ( not os.path.isfile(pf.cnt_file) ):
         print ( "ERROR pf.py: PIMA control file " + pf.cnt_file + " does not exist.\n" +
                 "Please check experiment, band, and directory " + pf.exp_dir  )
         exit ( 1 )

    if ( args.task == "load" ):
         pf_load ( pf, args.nopcal )
    elif ( args.task == "logs" ):
         pf_logs ( pf )
    elif ( args.task == "gean" ):
         pf_gean ( pf )
    elif ( args.task == "gain" ):
         pf_gain ( pf, args.gf )
    elif ( args.task == "coarse" ):
         pf_coarse ( pf )
    elif ( args.task == "fine" ):
         pf_fine ( pf, args.keep )
    elif ( args.task == "bpas" ):
         pf_bpas ( pf, args.insp )
    elif ( args.task == "mkdb" ):
         pf_mkdb ( pf, "mkdb", args.updt )
    elif ( args.task == "mktxt" ):
         pf_mkdb ( pf, "mktxt", None )
    elif ( args.task == "splt" ):
         pf_splt ( pf )
    elif ( args.task == "autm" ):
         pf_autm ( pf )
    elif ( args.task == "pict" ):
         pf_pict ( pf )
    elif ( args.task == "map" ):
         pf_map  ( pf )

if __name__ == "__main__":
    global pima_child_pid
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTERM, pima_signal_handler_term )
        signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
        pima_child_pid = None
        ps_dir = os.getenv("PIMA_STABLE_DIR")
        if ( ps_dir ):
             pima_exec["static"] = ps_dir + "/bin/pima_static"
        main()
    except KeyboardInterrupt:
        print ( "pf.py: Interrupted" )
        exit ( 1 )
