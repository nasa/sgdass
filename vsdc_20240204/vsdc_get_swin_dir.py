#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program vsdc_get_swin_dir.py gets a file of swin files from the    *
# *   data center for the specified year. The data center in question    *
# *   is determined by the VSDC control file.                            *
# *                                                                      *
# *   Options:                                                           *
# *   -c CONTROL_FILE -- the control file name.                          *
# *   -y YEAR         -- the year in question.                           *
# *   -l              -- if present, then print the list of swin files   *
# *                      that are in the data center.                    *
# *   -m              -- if present, then print the list of swin files   *
# *                      that are specified in the master file, but      *
# *                      are not found in the data center.               *
# *   -s SESSION_LIST -- if present, then it specified the session list. *
# *                      Session list can have two forms:                *
# *                      a) each line has one word: experiment name;     *
# *                      b) each line has more than one word, and the    *
# *                         experiment name is in the word that follows  *
# *                         ! character.                                 *
# *                      In both cases lines that start with # are       *
# *                      ignored.                                        *
# *                                                                      *
# * ## 31-JAN-2024 vsdc_get_swin_dir.py v1.3 (c) L. Petrov 06-FEB-2024 # *
# *                                                                      *
# ************************************************************************
import sys, os, subprocess, datetime, argparse, signal
vsdc__root_dir = "@@"
sys.path.append(vsdc__root_dir)
from   vsdc_config          import *
from   vsdc_parse           import *
from   vsdc_master          import *

def vsdc_get_swin_dir ( vsdc, url ):

    if ( vsdc.data_center.lower() == "cddis" ):
         netrc_str = read_file ( vsdc.netrc_file )
         if ( len(netrc_str[0]) < 6 ):
              print ( "Wrong  netrc file %s -- it should have six words" % vsdc.netrc_file )
              return 1

         username = netrc_str[0].split()[3]
         password = netrc_str[0].split()[5]

    if ( "cddis" in url ):
          com = "wget -e robots=off -c -nH --cut-dir=8 -O - " + \
                vsdc.wget_extra_opts     + " " + \
                "--auth-no-challenge "         + \
                "--user=" + username     + " " + \
                "--http-password=" + password + " " + \
                "--progress=bar:force"   + " " + \
                url
    else:
          com = "wget -c " + vsdc.wget_extra_opts + " " + url

    if ( vsdc.verb > 1 ): print ( "com = ", com )
    (ret,out) = vsdc_exe ( com )
    if ( ret != 0 ): 
         fl_found = True
         for line in out:
             if ( "404 Not Found" in line ): 
                  fl_found = False

         if ( fl_found  ):
              for line in out:
                  print ( line )
              print ( "ERROR in getting swin file list" )
              exit ( 1 )
         return ( [] )

    exp_list=[]
    for line in out:
        if ( 'MD5SUMS' in line ): continue
        if ( 'SHA512SUMS' in line ): continue
        if ( '<div class="archiveItemTextContainer"><a class="archiveItemText" id' in line ):
             swin_file = line.split()[6].replace('">',"").replace('</a>','')
             swin_date = line.split()[8].replace('class="fileInfo">','').replace(":",'.') + "_" + \
                         line.split()[9]
             swin_exp  = swin_file.split("_")[1]
             exp_list.append ( {"exp": swin_exp, "file": swin_file, "date": swin_date} )

    return ( exp_list )

#
# ------------------------------------------------------------------------
#
def main():
    """ 
    Main routine of vsdc pakage. It parses arguments
    """
    parser = argparse.ArgumentParser( description=vsdc__label )
    parser.add_argument('--version', action='version', version=vsdc__version )
#
# --- Parse General options
#
    parser.add_argument ( "-v", "--verbosity", action="store", \
                          dest="verb", \
                          default=0, \
                          metavar="value", \
                          type=int, \
                          help="Verbosity level" )

    parser.add_argument ( "-y", "--year", action="store", \
                          dest="year", \
                          metavar="value", \
                          type=int, \
                          help="year" )

    parser.add_argument ( "-c", "--control", action="store", \
                          dest="control_file", \
                          default=None, \
                          metavar="value", \
                          help="Name of the control file" )

    parser.add_argument ( "-l", "--list", action="store_true", \
                          dest="list", \
                          help="Display the list of swin files at the data center" )

    parser.add_argument ( "-m", "--missing", action="store_true", \
                          dest="missing", \
                          help="Display the list of swin files that are missing at the data center" )

    parser.add_argument ( "-s", "--sessions", action="store", \
                          dest="sessions", \
                          metavar="value", \
                          help="The list of observed sessions" )
#
# --- Get and parse options
#
    args = parser.parse_args()

    if ( not args.control_file ):
         print ( "Control file is not specified" )
         exit  ( 1 )

    if ( not args.year ):
         print ( "Year is not specified" )
         exit  ( 1 )

    if ( args.year < 2000 or args.year > 2050 ):
         print ( "Wrong year %d -- it should be in range [2000, 2050]" % args.year )
         exit  ( 1 )

    if ( not args.list and not args.missing ):
         print ( "At least one options -l or -m should be specified" )
         exit  ( 1 )

#
# --- Get the list of observed sessions and 
# --- the list of observed experiments
#
    sess_list = []
    exp_obs_list = []
    if ( args.sessions ):
         if ( not os.path.isfile(args.sessions) ):
              print ( "Session file %s does not exist" % args.sessions )
              exit  ( 1 )
#
# ------ Read the session file 
#
         buf = read_file ( args.sessions )
         for line in buf:
#
# ---------- Skip empty lines and lines with comments
#
             if ( len(line) == 0 ): continue
             if ( line.split()[0][0:1] == '#' ): continue
             if ( len(line.split()) == 1 ):
#
# --------------- The line has one word. Threat theat word as a session name
#
                  sess_list.append ( line.split()[0] )
                  exp_obs_list.append ( line.split()[0].lower() )
             elif ( len(line.split()) > 1 ):
#
# --------------- The line has two or more words. If the first word
# --------------- as the session name
#
                  if ( line.split()[0].upper == "OBS" or \
                       line.split()[0].upper == "INT"    ): 
                       sess_list.append ( line.split()[1] )
#
# ---------- Now look for the experiment. This is the first word ater ! sign
#
             for i in range(0,len(line.split())):
#
# -------------- Search for ! sign
#
                 if ( line.split()[i] == "!" ):
                      if ( i < len(line.split()) ):
#
# ------------------------ We found it and we pick up 
# ------------------------ the experiment from the next word
#
                           exp_obs_list.append ( line.split()[i+1].lower() )

              
#
# --- Initialize the class with vsdc variables
#
    vsdc = vsdc_config_class ( args.control_file ) 
#
# --- Parse vsdc control file
#
    vsdc.verb = args.verb
    ret = vsdc_parse_config ( vsdc )
    if ( ret != 0 ): 
         print ( "Failure in parsing control files" )
         exit  ( 1 )

    if ( vsdc.data_center == "CDDIS" ):
         swin_base_url = "https://cddis.nasa.gov/archive/vlbi/ivsdata/swin"
    else:
         print ( "Data center %s is not supported" % vsdc.data_center )
         exit  ( 1 )

#
# --- Build the url for the directory with swin files for 
# --- the request year
#
    url = swin_base_url + "/%4d/index.html"% args.year
#
# --- Get exp_list -- the list of dictionaries with information about exeriments
#
    exp_list = vsdc_get_swin_dir ( vsdc, url )
    if ( args.list ):
#
# ------ Print the list of uploaded swin files
#
         if ( len(exp_list) > 0 ):
              for exp in exp_list:
                  if ( exp["exp"] in exp_obs_list or exp_obs_list == [] ):
                       print ( "PRESENT Exp: %-8s   File: %-34s   Submission_date: %s " % ( exp["exp"],  exp["file"],  exp["date"] ) )

    if ( args.missing ):
#
# ------ Pring the list of missing swin files
#
# ------ Read the master files
#
         vsdc_master_read ( vsdc )
         year_beg_date = datetime.datetime.strptime ( "%04d.01.01_00:00:00" % args.year, '%Y.%m.%d_%H:%M:%S' )
         year_end_date = datetime.datetime.strptime ( "%04d.12.31_23:59:59" % args.year, '%Y.%m.%d_%H:%M:%S' )
         missing_list = []
#
# ------ Search for experiments in the maste files
#
         for exp_mas in vsdc.master.keys():
#
# ---------- Limit the scope to the requested year
#
             if ( vsdc.master[exp_mas]["date"] >= year_beg_date and \
                  vsdc.master[exp_mas]["date"] <= year_end_date ):
                  fl_found = False
#
# --------------- Search experiment exp_mas in the master file in the list
# --------------- of swin files submitted to the Data Center
#
                  for exp_swi in exp_list:
                      if ( exp_swi["exp"] == exp_mas ): fl_found = True
                  if ( not fl_found ):
#
# -------------------- Hmm, dod not find. That expriment either observed,
# -------------------- or the user does not know and, therefore, did not
# -------------------- specify the expriment list
#
                       if ( exp_mas in exp_obs_list or exp_obs_list == [] ):
#
# ------------------------- Update the missing swin list
#
                            missing_list.append ( "MISSING exp: %-8s  Date: %s  Type: %s  Corr: %s" % ( exp_mas, \
                                 datetime.datetime.strftime( vsdc.master[exp_mas]["date"], "%Y.%m.%d_%H:%M" ), \
                                                             vsdc.master[exp_mas]["type"], \
                                                             vsdc.master[exp_mas]["corr"]  ) )

#
# ------ Sort the missing list over the date
#
         sorted( missing_list, key=lambda field: field.split()[4] )

#
# ------ And print the list of missing experiments
#
         for exp_mis in missing_list:
             print ( exp_mis )

#
# cat /apr/psolve/vda_to_gvf_24.csh | sed "s@OBS@ @g" | awk '{print $7}' | sort -u > /tmp/sess_obs.txt
#


if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
        main()
    except KeyboardInterrupt:
        print ( "\nvsdc.py: Interrupted" )
        exit ( 1 )
