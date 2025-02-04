#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   A wrapper program for VLBI processing program PIMA.                *
# *                                                                      *
# * ### 20-DEC-2015    pg.py   v 1.19 (c)  L. Petrov   01-APR-2022  ###  *
# *                                                                      *
# ************************************************************************
"""
import pwd, sys, os, re, shutil, time, subprocess, datetime, operator, signal
sys.path.append("/usr")
import argparse 
import pima_local
from pima_exe import exe, exe_out_log, exe_out_nolog, exe_noout_log, exe_noout_nolog, \
                     pima_print_mes, pima_signal_handler_term, read_file, write_file, \
                     append_file, check_err_exe

pg__label   = "pg.py (PIMA fringe)"
pg__version = "pg.py v 1.19 2022.04.01"

band_list = [ "l", "s", "c", "x", "u", "k", "q", "w", "f", "d", "e", "1", "2", "3", "4" ]
pima_exec  = { \
               "dynamic": pima_local.pima_dynamic_exec, \
               "static":  pima_local.pima_static_exec   \
             }
pg_dir = pima_local.pf_dir
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

class pg_class:
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
def pg_resolve_envar ( pg, cnt ):

    cnt_out = []
    for line in cnt:
        line = line.strip("\n")
        if ( len(line.split()) > 1 ):
             val = line.split()[1]
             if ( val[0:1] == '$' ):
                  if ( val[1:2] != "{" ):
                       print ( "Failure in parsing line ", line, \
                               " of the control file " + pg.cnt_file + \
                               " -- enviroment vairalbe should start wiht ${" )
                       exit ( 1 )
                  ib = 2
                  if ( not "}" in val ):
                       print ( "Failure in parsing line ", line, \
                               " of the control file " + pg.cnt_file + \
                               " -- enviroment vairalbe name should be followed by }" )
                       exit ( 1 )
                  ie = val.index("}")
                  env_var = val[ib:ie]
                  val = os.getenv ( env_var )
                  if ( not val ):
                       print ( "Failure in parsing line ", line, \
                               " of the control file " + pg.cnt_file + \
                               " -- enviroment varialbe " + \
                               env_var + " is not defined" )
                       exit ( 1 )
                  line = line.replace("${"+env_var+"}",val)
        cnt_out.append ( line )
    return ( cnt_out )
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
def pg_gepm ( pg, tim_mseg, overwrite, sta, tim_thresh, diff_thresh, max_count):
    """
    Perform fringe fitting of the VLBI experiment in coarse mode
    """
    
    log_file = pg.exp_dir + "/" + pg.exp + '_'+ pg.band +"_gepm.log"
    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]

#
# --- Print start message in the log file
#
    if ( not pg.dry_run ):
         log = open ( log_file, "w" )
         print ( " ".join(sys.argv), file = log ) 
         print ( "Started automatic phase calibration masking for experiment", pg.exp, "by PIMA on", date_str, file=log )
         print ( "=================================================================================", \
                  file=log )
         print ( " ", file=log )
         log.flush()
    if ( pg.static ):
         pima_com = pima_exec["static"]  + " " + pg.cnt_file + " " + "gepm" + \
             " " + "sta" + " " + sta + " " + "tim_mseg" + " " + str(tim_mseg) + \
             " " + "overwrite" + " " + "yes" + " " + "tim_thresh" + " " + str(tim_thresh) + \
             " " + "diff_thresh" + " " + str(diff_thresh) + " " + "max_count" + " " + str(max_count)
    else:
         pima_com = pima_exec["dynamic"] + " " + pg.cnt_file + " " + "gepm" + \
             " " + "sta" + " " + sta + " " + "tim_mseg" + " " + str(tim_mseg) + \
             " " + "overwrite" + " " + "yes" + " " + "tim_thresh" + " " + str(tim_thresh) + \
             " " + "diff_thresh" + " " + str(diff_thresh) + " " + "max_count" + " " + str(max_count)

    with open(pg.cnt_file) as f:
         cnt = f.readlines()
    f.close()
    cnt = pg_resolve_envar ( pg, cnt )

    for line in cnt:
        line = line.strip("\n")
        if ( len(line) == 0 ): continue
        if ( len(line.split()) == 0 ): continue
        if ( line[0:15]  == "PCAL_MASK_FILE:" ):
             ppsm_file = line.split()[1]
 
    if ( ppsm_file != "NO" ):
         if ( not os.path.isfile(ppsm_file) ):
              ppsm_file = "NO"
         else:
             if overwrite is not True:
                 print ( "pg.py: PCAL Mask file " + ppsm_file + " exists.")
                 print ( " To overwrite this file, specify --overwrite" )
                 exit  ( 1 )


    pima_com = pima_com + " " + \
               "PCAL_MASK_FILE: "        + ppsm_file + " "    

    if ( len(pg.pima_opts) > 0 ):
#
# ------ Add options to the command line
#
         for opt in pg.pima_opts:
             pima_com = pima_com + " " + opt

    if ( pg.verb > 1 or pg.dry_run ):
         print ( "pg.py: " + pima_com )
    if ( pg.dry_run ): return 0

    #(ret, out ) = exe_noout_log ( pima_com, pg.verb, log )
    ret=0
    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
    if ( ret == 0 ): 
#
# -------Accumulate pcal report into a human-readable gen file
#
         report_file  = pg.exp + '_s_pcal_report.gen' 
         write_report(pg, report_file, ppsm_file)
         print ( "pg.py: Wrote condensed pcal gen file, ", ppsm_file.replace('.mask','.gen') )
#
# ------ Generate the final success message in the output log file
#
         if ( pg.verb > 0 ):
              print ( "pg.py: Successful completion of automatic phase calibration masking of", 
                       pg.exp, "band", pg.band, "on", date_str )
              sys.stdout.flush()
         print ( " ", file=log )
         print ( "pg.py: Successful completion of automatic phase calibration masking of", 
                 pg.exp, "band", pg.band, "on", date_str, file=log )
         print ( "===========================================================" + \
                 "======================================", file=log )
         log.close()
         exit ( 0 )
    else:
#
# ------ Generate the error message to the output file
#
         if ( pg.verb > 0 ):
              print ( "pg.py: Automatic phase calibration masking of experiment", pg.exp, \
                      "FAILED due to an error on", date_str )
              sys.stdout.flush()
         print ( " ", file=log )
         print ( "pg.py: Automatic phase calibration masking of experiment", pg.exp, "band", pg.band, file=log )
         print ( "FAILED due to an error on", date_str, file=log )
         print ( "=================================================", 
                 file=log )
         log.close()
         exit ( 1 )
#
# ------------------------------------------------------------------------
#
def write_report(pg, report_file, ppsm_file):
    """
    Write a condensed pcal mask generator file from GEPM results
    """
    # Open the input file for reading
    STA_LIST = []
    IF_LIST = []
    TONE_LIST = []
    unique_stations = []
    with open(report_file, 'r') as file:
        # Open the output file for writing
        with open(ppsm_file.replace('.mask','_mask.gen'), 'w') as out_file:
            # Iterate over each line in the input file
            for line in file:
                # Split the line by whitespace to get the columns
                columns = line.split()
                if columns[0] == 'PCAL':
                    STA_LIST.append(columns[2])
                    IF_LIST.append(int(columns[4][0]))
                    TONE_LIST.append(int(columns[6][0]))
                    if columns[2] not in unique_stations:
                        unique_stations.append(columns[2])
                elif len(columns) >= 2:
                    if columns[2] == 'PCAL_RPT':
                        format_line = line
            # write preamble
            out_file.write(format_line)
            out_file.write('#\n# created by pg.py\n')
            out_file.write('# using control file ' + pg.cnt_file +'\n#')
            
            # process lists
            for station in unique_stations:
                tone_unique = []
                IF_sta = [IF_LIST[i] for i, sta in enumerate(STA_LIST) if sta == station]
                tone_sta = [TONE_LIST[i] for i, sta in enumerate(STA_LIST) if sta == station]

                try: 
                    len(tone_sta)
                except: 
                    tone_sta = [tone_sta] # scalar integer
                for tone in tone_sta:
                    if tone not in tone_unique:
                        tone_unique.append(tone)
                        IF_tone = [IF_sta[i] for i, tone_iter in enumerate(tone_sta)\
                                    if tone_iter == tone]
                        try: if_runs = find_runs(IF_tone)
                        except: if_runs = [(IF_sta,IF_sta)]
                        for (begin,end) in if_runs:
                            out_file.write('\nPCAL  STA:   '+ station + ' '*(10-len(station)) +\
                                'IND_FRQ:   ' + str(begin) + '-' + str(end) + '    IND_TONE:   ' +\
                                str(tone) + '-' + str(tone) +'      OFF')
                
                out_file.write('\n#')
                
            out_file.write('\n'+format_line)
 #
 # ------------------------------------------------------------------------
 #                  
def find_runs(num_list):
    """
    Find runs in list of integers, return tuples
    """
    num_set = sorted( set( num_list ) )
    gaps_list = [[s,e] for s, e in zip( num_set, num_set[1:] ) if s+1 < e]
    edge_list = iter( num_set[:1] + sum( gaps_list, [] ) + num_set[-1:] )
    return list( zip( edge_list, edge_list ) )
    
 #
 # ------------------------------------------------------------------------
 #   
def main():
    """ 
    Parsing arguments
    """
    parser = argparse.ArgumentParser( description=pg__label )
    parser.add_argument('--version', action='version', version=pg__version )

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
# --- Task: gepm
#
    gepm_parser = subparsers.add_parser ( "gepm", \
                                          help="Load VLBI experiment." +
                                               "Options: -sta, -overwrite, \
                                                -tim_mseg, -tim_thresh, \
                                                -diff_thresh,-max_count" )
    gepm_parser.set_defaults ( task="gepm" )
    gepm_parser.add_argument ( "-overwrite", "--overwrite", \
                               dest="overwrite", \
                               action="store_true", \
                               default=None, \
                               help="Overwrite existing mask file if it exists" )
    gepm_parser.add_argument ( "-tim_mseg", "--tim_mseg", \
                               dest="tim_mseg", \
                               action="store", \
                               default=1, \
                               help="Averaging interval for phase calibration data (sec)" )
    gepm_parser.add_argument ( "-tim_thresh", "--tim_thresh", \
                               dest="tim_thresh", \
                               action="store", \
                               default=0.1, \
                               help="Fraction of infected epochs to disable a pcal tone" )
    gepm_parser.add_argument ( "-diff_thresh", "--diff_thresh", \
                               dest="diff_thresh", \
                               action="store", \
                               default=0.15, \
                               help="Complex plane distance to flag an epoch" )
    gepm_parser.add_argument ( "-max_count", "--max_count", \
                               dest="max_count", \
                               action="store", \
                               default=50, \
                               help="Maximum number of allowed statistically "+\
                                   "significant phase jumps per tone" )
    gepm_parser.add_argument ( "-sta", "--sta", \
                               dest="sta", \
                               action="store", \
                               default="all", \
                               help="Which stations' pcal tones to process." )
    gepm_parser.add_argument ( "pima_opts", nargs=argparse.REMAINDER )

#
# --- Get and parse options
#
    args = parser.parse_args()

    try:
         if ( args.task == None ): args.task = None
    except:
         print ( "pg.py: A task should be specified in the 3rd argument" )
         print ( "       to see the list of supported task, try pg.py -help" )
         exit  ( 1 )

    pg = pg_class ( args.task, args.exp, args.band, args.verb, args.dry_run, args.static, \
                    args.pima_opts ) 

    if ( args.band not in band_list ):
         print ( "ERROR pg.py: band " + args.band + " is not known.\n"  \
                 "The list of known bands: " + ', '.join(band_list) )
         exit ( 1 )

    pg.exp_dir  = pg_dir + "/" + args.exp
    if ( not os.path.isdir(pg.exp_dir) ):
         print ( "ERROR pg.py: directory " + pg.exp_dir + " does not exist.\n" +
                 "Please check experiment, band, and directory " + pg_dir )
         exit ( 1 )

    pg.cnt_file = pg.exp_dir  + "/" + args.exp + "_" + args.band + "_pima.cnt"
    if ( not os.path.isfile(pg.cnt_file) ):
         print ( "ERROR pg.py: PIMA control file " + pg.cnt_file + " does not exist.\n" +
                 "Please check experiment, band, and directory " + pg.exp_dir  )
         exit ( 1 )

    if ( args.task == "gepm" ):
         pg_gepm ( pg, args.tim_mseg, args.overwrite, args.sta, args.tim_thresh,\
                  args.diff_thresh, args.max_count )

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
        print ( "pg.py: Interrupted" )
        exit ( 1 )
