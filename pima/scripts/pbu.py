#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program pbu.py generates the control file for refringing           *
# *   an experiment with bandpass break at given station or stations.    *
# *   A case when a break happened simultaneously at two or more         *
# *   stations is supported.                                             *
# *                                                                      *
# *   The list of observations of a given station or stations is split   *
# *   into two parts "before" and "after" the specified observation.     *
# *   (the specified observations belongs to the group before).          *
# *                                                                      *
# *   If the group "before" has more observations, it is set as a main   *
# *   group and group "after" is set as an auxiliary group. Otherwise,   *
# *   group "after" is set as the main and "before" is set as            *
# *   an auxiliary group.                                                *
# *                                                                      *
# *   A user needs to run the command file build by pbu.py               *
# *                                                                      *
# *   Two bandpasses are computed: the main bandpass that excludes       *
# *   observations in the auxiliary group of given stations and the      *
# *   secondary bandpass that excludes observations of the main group.   *
# *                                                                      *
# *   Usage: pbu.pu exp_name sta_name obs_ind bands                      *
# *   where                                                              *
# *          exp_name -- experiment name                                 *
# *          sta_name -- A comma-separated list of station names,        *
# *                      either IVS name or 2-character long name.       *
# *          obs_ind  -- observation index. Observations with that index *
# *                      and before are assigned group "before".         *
# *                      Observations with indices after that are        *
# *                      assigned to group "after".                      *
# *                                                                      *
# *          bands    -- comma separated list of bands for which         *
# *                      refringing will run.                            *
# *                                                                      *
# *   Examples:                                                          *
# *                                                                      *
# *   pbu.py uh007m BR-VLBA 12580 x,s                                    *
# *   pbu.py bt152a HN,MK   10500 c				         *
# *                                                                      *
# *  ### 15-MAY-2024  pbu.py  v1.0 (c)        Niu Liu   15-MAY-2024 ###  *
# *  ###                      v3.5 updated by L. Petrov 30-MAY-2026 ###  *
# *                                                                      *
# ************************************************************************
# -*- coding: utf-8 -*-
# File name: bandpass_break_update.py
import pwd, sys, os, re, shutil, time, subprocess, datetime, operator, signal
import argparse
import pima_local
from pima_exe import exe, exe_out_log, exe_out_nolog, exe_noout_log, exe_noout_nolog, \
                     pima_print_mes, pima_signal_handler_term, read_file, write_file, \
                     append_file, check_err_exe
pbu__label = "pbu.py 20260530"

pf_dir = pima_local.pf_dir
pima_bin= pima_local.pima_path + "/bin"

#
# date "+%d-%b-%Y"
#

def make_csh ( exp, sta_list, action, obs_indx, bands, filout ):

#
# --- Build PIMA control file
#
    pima_cnt = pima_local.pf_dir + "/" + exp + "/" + exp + "_" + bands[0] + "_pima.cnt"

#
# --- Read pima control file
#
    buf = read_file ( pima_cnt )
    if ( not buf ):
         print ( "Cannot read control file %s . Please check the experiment name" % pima_cnt )
         exit  ( 1 )

#
# --- Extract values of keywords EXPER_DIR and POLARCAL_FILE from the
# --- PIMA control file
#
    exper_dir         = None
    polarcal_file     = None
    use_polarcal_file = False
    fringe_file       = None
    for line in buf:
        if ( line[0:10] == "EXPER_DIR:" ):
             exper_dir = line.split()[1]
        if ( line[0:14] == "POLARCAL_FILE:" ):
             polarcal_file = line.split()[1]
        if ( line[0:12] == "FRINGE_FILE:" ):
             fringe_file = line.split()[1]

    if ( not exper_dir ):
         print ( "Cannot read find exper_dir. Please check the control file %s" % pima_cnt )
         exit  ( 1 )

#
# --- Check whethr POLARCAL_FILE was specified in the PIMA conrol file
#
    if ( polarcal_file ):
         if ( polarcal_file != "NO" ):
              use_polarcal_file = True

#
# -- Read the station file that is a result of  FITS-IDI pasing
#
    sta_file = exper_dir + "/" + exp + ".sta"
    buf = read_file ( sta_file )
    if ( not buf ):
         print ( "Cannot read station file %s" % sta_file )
         exit  ( 1 )

    sta_full_name_list = []
    for line in buf:
        for sta in sta_list:
             if ( line.split()[5].lower() == sta or line.split()[3].lower() == sta ):
                  sta_nam = line.split()[3].upper()
                  if ( not sta_nam in sta_full_name_list ):
                       sta_full_name_list.append ( sta_nam )

    if ( len(sta_full_name_list) == 0 ):
         print ( "Cannot find station %s in the station file %s" % (sta, sta_file ) )
         exit  ( 1 )

#
# --- Build and read experiment statistics file
#
    stt_file = exper_dir + "/" + exp + ".stt"
    buf = read_file ( stt_file )
    if ( not buf ):
         print ( "Cannot read statistics file %s" % stt_file )
         exit  ( 1 )

    num_obs = -1
    for line in buf:
        if ( line[0:23] == 'Number of observations:' ):
             num_obs = int(line.split()[3])

    if ( num_obs == -1 ):
         print ( "Cannot find the number of observations in the statistics file %s" % stt_file )
         exit  ( 1 )

    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S")
    if ( len(sta_list) == 1 ):
         sta_list_str = sta_list[0]
         sta_full_list_str = sta_full_name_list[0]
         sta_full_pattern  = sta_full_name_list[0]
    else:
         sta_list_str = (",").join(sta_list)
         sta_full_list_str = (",").join(sta_full_name_list)
         sta_full_pattern  = ("\\|").join(sta_full_name_list)

    if ( filout is None ):
         bands_str = ("").join(bands)
         filout = pf_dir + "/" + exp + "/" + exp + "_" + sta_list_str + "_" + bands_str + ".csh"

    if ( not fringe_file ):
         print ( "Fringe file is not specified in the control file %s" % fringe_file )
         exit  ( 1 )

    if ( not os.path.isfile(fringe_file) ):
         fringe_file_nobps = fringe_file.replace ( ".fri", "_nobps.fri" )
         if ( os.path.isfile(fringe_file_nobps) ):
              fringe_file = fringe_file_nobps
         else:
              print ( "Cannot cannot find fringe file neither %s nor %s" % ( fringe_file, fringe_file_nobps ) )
              exit  ( 1 )
         fringe_file_list = [ fringe_file ]
         if ( len(bands) > 1 ):
              fringe_file_2nd = fringe_file.replpace ( "_"  +bands[0] + "_","_"  +bands[1] + "_"  )
              if ( not os.path.isfile(fringe_file_nobps) ):
                   print ( "Cannot cannot find fringe file for the second band %s" % fringe_file_2nd )
              fringe_file_list.append ( fringe_file )
#
# --- Start writing the output file
#
    with open(filout, "w") as f:
         print ( '#!/bin/csh -f', file=f )
         print ( '# ************************************************************************', file=f )
         print ( '# *                                                                      *', file=f )
         print ( '# *   Control file for refringing station %s in experiment %-8s      *' % ( sta_list_str, exp ), file=f )
         print ( '# *   before and after observation %7d                               *' % obs_indx,   file=f )
         print ( '# *                                                                      *', file=f )
         print ( '# *  ### Control file is generated with pbu.py on %s    *' % date_str, file=f )
         print ( '# *                                                                      *', file=f )
         print ( '# ************************************************************************', file=f )
         print ( '#', file=f )
         print ( '# Created with command ' + " ".join(sys.argv), file=f )
         print ( '# Created by %s' % pbu__label, file=f )
         print ( '#', file=f )
#
         print ( 'set exp = ' + exp, file=f )
         print ( 'set sta = ' + sta_list_str, file=f )
         print ( 'cd ' + pf_dir + "/" + exp, file=f )
         print ( '#', file=f )
         print ( '# --- Get the file with observations at station %s before and after the break' % sta_full_list_str, file=f )
         print ( '#', file=f )
         band_arr = bands.split(",")
         if ( obs_indx < num_obs/2 ):
#
# ----------- The main part of the bandpass is after
#
              print ( '# --- Main part: after', file=f )
              print ( '#', file=f )
              print ( 'cat ' + fringe_file + ' | grep "' + sta_full_pattern + '" | awk ' + "'{ if ( $1 <= " + "%s" % obs_indx + " ) printf " + '"%6s\\n", ' + "$1}' | sort -u > ${exp}_${sta}_before" + "_" + bands_str + ".obs", file=f )
              print ( 'cat ' + fringe_file + ' | grep "' + sta_full_pattern + '" | awk ' + "'{ if ( $1 >  " + "%s" % obs_indx + " ) printf " + '"%6s\\n", ' + "$1}' | sort -u > ${exp}_${sta}_after"  + "_" + bands_str + ".obs", file=f )
              for band in band_arr:
                  print ( '# ', file=f )
                  print ( '# --- Compute bandpass for %s band' %  band.upper(), file=f )
                  print ( '# ', file=f )
                  print ( 'pf.py $exp ' + band + ' bpas EXCLUDE_OBS_FILE: ' + pf_dir + '/${exp}/${exp}_${sta}_after' + '_' + bands_str + '.obs     \\', file=f )
                  if ( use_polarcal_file ):
                       print ( '                  BANDPASS_FILE:    ' + pf_dir + '/${exp}/${exp}_${sta}_' + band + '_before.bps  \\', file=f )
                       print ( '                  POLARCAL_FILE:    ' + pf_dir + '/${exp}/${exp}_${sta}_' + band + '_before_plr.bps', file=f )
                  else:
                       print ( '                  BANDPASS_FILE:    ' + pf_dir + '/${exp}/${exp}_${sta}_' + band + '_before.bps', file=f )
                  print ( 'mv ' + pf_dir + '/${exp}/${exp}_' + band + '_bps.log    ' + pf_dir + '/${exp}/${exp}_${sta}_' + band + '_bps.log', file=f )
                  print ( 'pf.py $exp ' + band + ' bpas EXCLUDE_OBS_FILE: ' + pf_dir + '/${exp}/${exp}_${sta}_before' + '_' + bands_str + '.obs', file=f )
                  print ( '#', file=f )

              print ( '#', file=f )
              print ( '# --- Check whether we need to exit just after bandpass creation', file=f )
              print ( '#', file=f )
              print ( 'if ( $1 == "bpass" ) then', file=f )
              print ( '     exit 0', file=f )
              print ( 'endif', file=f )

              for band in band_arr:
                  print ( '#', file=f )
                  print ( '# --- Run fringe fitting for station %s at %s band' % ( sta_full_list_str, band.upper()), file=f )
                  print ( '#', file=f )
                  print ( 'pf.py $exp ' + band + ' fine -keep INCLUDE_OBS_FILE: ' + pf_dir + '/${exp}/${exp}_${sta}_after'  + '_' + bands_str + '.obs', file=f )
                  print ( 'pf.py $exp ' + band + ' fine -keep INCLUDE_OBS_FILE: ' + pf_dir + '/${exp}/${exp}_${sta}_before' + '_' + bands_str + '.obs \\', file=f )
                  if ( use_polarcal_file ):
                       print ( '                        BANDPASS_FILE:   ' + pf_dir + '/${exp}/${exp}_${sta}_' + band + '_before.bps  \\', file=f )
                       print ( '                        POLARCAL_FILE:   ' + pf_dir + '/${exp}/${exp}_${sta}_' + band + '_before_plr.bps', file=f )
                  else:
                       print ( '                        BANDPASS_FILE:   ' + pf_dir + '/${exp}/${exp}_${sta}_' + band + '_before.bps', file=f )

         else:
#
# ----------- The main part of the bandpass is before
#
              print ( '# --- Main part: before', file=f )
              print ( '#', file=f )
              band = bands.split(",")[-1]
              print ( 'cat ' + fringe_file + ' | grep "' + sta_full_pattern + '" | awk ' + "'{ if ( $1 <= " + "%s" % obs_indx + " ) printf " + '"%6s\\n", ' + "$1}' | sort -u > ${exp}_${sta}_before" + '_' + bands_str + ".obs", file=f )
              print ( 'cat ' + fringe_file + ' | grep "' + sta_full_pattern + '" | awk ' + "'{ if ( $1 >  " + "%s" % obs_indx + " ) printf " + '"%6s\\n", ' + "$1}' | sort -u > ${exp}_${sta}_after"  + '_' + bands_str + ".obs", file=f )
              for band in band_arr:
                  print ( '# ', file=f )
                  print ( '# --- Compute bandpass for %s band' %  band.upper(), file=f )
                  print ( '# ', file=f )
                  print ( 'pf.py $exp ' + band + ' bpas EXCLUDE_OBS_FILE: ' + pf_dir + '/${exp}/${exp}_${sta}_before' + '_' + bands_str + '.obs        \\', file=f )
                  if ( use_polarcal_file ):
                       print ( '                  BANDPASS_FILE:    ' + pf_dir + '/${exp}/${exp}_${sta}_' + band + '_after.bps  \\',     file=f )
                       print ( '                  POLARCAL_FILE:    ' + pf_dir + '/${exp}/${exp}_${sta}_' + band + '_after_plr.bps', file=f )
                  else:
                       print ( '                  BANDPASS_FILE:    ' + pf_dir + '/${exp}/${exp}_${sta}_' + band + '_after.bps', file=f )
                  print ( 'mv ' + pf_dir + '/${exp}/${exp}_' + band + '_bps.log    ' + pf_dir + '/${exp}/${exp}_${sta}_' + band + '_bps.log', file=f )
                  print ( 'pf.py $exp ' + band + ' bpas EXCLUDE_OBS_FILE: ' + pf_dir + '/${exp}/${exp}_${sta}_after' + '_' + bands_str + '.obs', file=f )

              print ( '#', file=f )
              print ( '# --- Check whether we need to exit just after bandpass creation', file=f )
              print ( '#', file=f )
              print ( 'if ( $1 == "bpass" ) then', file=f )
              print ( '     exit 0', file=f )
              print ( 'endif', file=f )

              for band in band_arr:
                  print ( '#', file=f )
                  print ( '# --- Run fringe fitting for station %s at %s band' % ( sta_full_list_str, band.upper()), file=f )
                  print ( '#', file=f )
                  print ( 'pf.py $exp ' + band + ' fine -keep INCLUDE_OBS_FILE: ' + pf_dir + '/${exp}/${exp}_${sta}_before'+ '_' + bands_str + '.obs', file=f )
                  print ( 'pf.py $exp ' + band + ' fine -keep INCLUDE_OBS_FILE: ' + pf_dir + '/${exp}/${exp}_${sta}_after' + '_' + bands_str + '.obs \\', file=f )
                  if ( use_polarcal_file ):
                       print ( '                        BANDPASS_FILE:   ' + pf_dir + '/${exp}/${exp}_${sta}_' + band + '_after.bps  \\', file=f )
                       print ( '                        POLARCAL_FILE:   ' + pf_dir + '/${exp}/${exp}_${sta}_' + band + '_after_plr.bps', file=f )
                  else:
                       print ( '                        BANDPASS_FILE:   ' + pf_dir + '/${exp}/${exp}_${sta}_' + band + '_after.bps', file=f )

         print ( '#', file=f )
         print ( '# --- Update the database', file=f )
         print ( '#', file=f )
         print ( 'pf.py $exp ' + band_arr[0] + ' mkdb -updt', file=f )
         print ( '#', file=f )
         print ( 'echo "Finished re-fringing for experiment %s because of a break at station %s after obs %s"' % \
                 ( exp, sta_full_list_str, obs_indx ), file=f  )

    f.close()
    os.system ( 'chmod o+x,g+rwx ' + filout )
    print ( 'Created command file  ' + filout )
    if ( action == "script" ):
         return ( 0 )
    else:
         com = "python3 " + filout + " " + action
         (ret,err) = pima_exe ( com )
         if ( ret != 0 ):
              for line in err:
                  print ( line )
              print ( "Erorr in executing command %s" % com  )
              exit  ( 1 )


def main():
    parser = argparse.ArgumentParser(
        description="This script is used to generate a csh program for handling the bandpass break")
    parser.add_argument ( "exp", \
                          help="Experiment name" )

    parser.add_argument ( "sta", \
                          help="Short station name")

    parser.add_argument ( "obs_indx", \
                          type=int,   \
                          help="Observation index where the bandpass break happended")

    parser.add_argument ( "bands", \
                          help="Comma separated band names" )

    parser.add_argument ( "-a", \
                          "--action", \
                          action="store", \
                          dest="action", \
                          choices=['script', 'bpass', 'all'], \
                          default="script", \
                          help="Comma separated band names" )

    parser.add_argument ( "-o", \
                          "--filout", \
                          help="Output file name (default: ${exp}_${sta}.csh)")

    args = parser.parse_args()

    sta_list = args.sta.lower().split(",")

    make_csh ( args.exp, sta_list, args.action, \
               args.obs_indx, args.bands, args.filout )

if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTERM, pima_signal_handler_term )
        main()
    except KeyboardInterrupt:
        print ( "pr.py: Interrupted" )
        exit ( 1 )


