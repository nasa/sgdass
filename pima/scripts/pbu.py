#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program pbu.py generates the control file for refringing           *
# *   an experiment with bandpass break at given station and             *
# *   a given observation.                                               *
# *                                                                      *
# *   The list of observations of a given station is split into two      *
# *   parts "before" and "after" the specified observation.              *
# *   (the specified observations belongs to the group before).          *
# *                                                                      *
# *   If the group "before" has more observations, it is set as a main   *
# *   group and group "after" is set as an auxiliary group. Otherwise,   *
# *   group "after" is set as the main and "before" is set as            *
# *   an auxiliary group.
# *                                                                      *
# *   Two bandpasses are computed: the main bandpass that excludes       *
# *   observations in the auxiliary group of a given stations and the    *
# *   secondary bandpass that excludes observations of the main group.   *
# *                                                                      *
# *   Usage: pbu.pu exp_name sta_name obs_ind bands                      *
# *   where                                                              *
# *          exp_name -- experiment name                                 *
# *          sta_name -- station name, either IVS name or 2-character    *
# *                      long name.                                      *
# *          obs_ind  -- observation index. Observations with that index *
# *                      and before are assigned group "before".         *
# *                      Observations with indices after that are        *
# *                      assigned to group "after".                      *
# *                                                                      *
# *          bands    -- comma separated list of bands for which         *
# *                      refringing will run.                            *
# *                                                                      *
# *   Example:                                                           *
# *                                                                      *
# *   pbu.py uh007m BR-VLBA 12580 x,s                                    *                               *
# *                                                                      *
# *  ### 15-MAY-2024  pbu.py  v1.0 (c)        Niu Liu   15-MAY-2024 ###  *
# *  ###                      v3.1 updated by L. Petrov 20-SEP-2024 ###  *
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

pf_dir = pima_local.pf_dir
pima_bin= pima_local.pima_path + "/bin"

#
# date "+%d-%b-%Y"
#

def make_csh ( exp, sta, obs_indx, bands, filout ):

    pima_cnt = pima_local.pf_dir + "/" + exp + "/" + exp + "_" + bands[0] + "_pima.cnt"

    buf = read_file ( pima_cnt )
    if ( not buf ):
         print ( "Cannot read control file %s . Please check the experiment name" % pima_cnt ) 
         exit  ( 1 )

    exper_dir = None
    for line in buf:
        if ( line[0:10] == "EXPER_DIR:" ):
             exper_dir = line.split()[1]

    if ( not exper_dir ):
         print ( "Cannot read find exper_dir. Please check the control file %s" % pima_cnt ) 
         exit  ( 1 )

    sta_file = exper_dir + "/" + exp + ".sta"
    buf = read_file ( sta_file )
    if ( not buf ):
         print ( "Cannot read station file %s" % sta_file )
         exit  ( 1 )

    sta_full_name = 'None'
    for line in buf:
        if ( line.split()[5].lower() == sta or line.split()[3].lower() == sta ):
              sta_full_name = line.split()[3].upper()
    if ( sta_full_name == 'None' ):
         print ( "Cannot find station %s in the station file %s" % (sta, sta_file ) )
         exit  ( 1 )

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

    if ( filout is None ):
        filout = pf_dir + "/" + exp + "/" + exp + "_" + sta + ".csh"

    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S")
    with open(filout, "w") as f:
         print ( '#!/bin/csh -f', file=f )
         print ( '# ************************************************************************', file=f )
         print ( '# *                                                                      *', file=f )
         print ( '# *   Control file for refringing station %s in experiment %-8s      *' % ( sta, exp ), file=f )
         print ( '# *   before and after observation %7d                               *' % obs_indx,   file=f )
         print ( '# *                                                                      *', file=f )
         print ( '# *  ### Control file is generated with pbu.py on %s    *' % date_str, file=f )
         print ( '# *                                                                      *', file=f )
         print ( '# ************************************************************************', file=f )
         print ( '#', file=f )
         print ( '# Created with command ' + " ".join(sys.argv), file=f )
         print ( '#', file=f )
#
         print ( 'set exp = ' + exp, file=f )
         print ( 'set sta = ' + sta, file=f )
         print ( 'cd ' + pf_dir + "/" + exp, file=f )
         print ( '#', file=f )
         print ( '# --- Get the file with observations of station ' + sta_full_name, " before and after the break", file=f )
         print ( '#', file=f )
         if ( obs_indx < num_obs/2 ):
#
# ----------- The main part of the bandpass is after
#
              print ( '# --- Main part: after', file=f )
              print ( '#', file=f )
              band = bands.split(",")[-1]
              print ( 'cat ${exp}_x.fri | grep "' + sta_full_name + '" | awk ' + "'{ if ( $1 <= " + "%s" % obs_indx + " ) printf " + '"%6s\\n", ' + "$1}' | sort -u > ${exp}_${sta}_before.obs", file=f )
              print ( 'cat ${exp}_x.fri | grep "' + sta_full_name + '" | awk ' + "'{ if ( $1 >  " + "%s" % obs_indx + " ) printf " + '"%6s\\n", ' + "$1}' | sort -u > ${exp}_${sta}_after.obs",  file=f )
              for band in bands.split(","):
                  print ( '# ', file=f )
                  print ( '# --- Compute bandpass for %s band' %  band.upper(), file=f )
                  print ( '# ', file=f )
                  print ( 'pf.py $exp ' + band + ' bpas EXCLUDE_OBS_FILE: ' + pf_dir + '/${exp}/${exp}_${sta}_after.obs     \\', file=f )
                  print ( '                  BANDPASS_FILE:    ' + pf_dir + '/${exp}/${exp}_${sta}_' + band + '_before.bps', file=f )
                  print ( 'mv ' + pf_dir + '/${exp}/${exp}_' + band + '_bps.log    ' + pf_dir + '/${exp}/${exp}_${sta}_' + band + '_bps.log', file=f )
                  print ( 'pf.py $exp ' + band + ' bpas EXCLUDE_OBS_FILE: ' + pf_dir + '/${exp}/${exp}_${sta}_before.obs', file=f )

              for band in bands.split(","):
                  print ( '#', file=f )
                  print ( '# --- Run fringe fitting for station %s at %s band' % ( sta_full_name, band.upper()), file=f )
                  print ( '#', file=f )
                  print ( 'pf.py $exp ' + band + ' fine -keep INCLUDE_OBS_FILE: ' + pf_dir + '/${exp}/${exp}_${sta}_after.obs', file=f )
                  print ( 'pf.py $exp ' + band + ' fine -keep INCLUDE_OBS_FILE: ' + pf_dir + '/${exp}/${exp}_${sta}_before.obs \\', file=f )
                  print ( '                        BANDPASS_FILE:   ' + pf_dir + '/${exp}/${exp}_${sta}_' + band + '_before.bps', file=f )

         else:
#
# ----------- The main part of the bandpass is before
#
              print ( '# --- Main part: before', file=f )
              print ( '#', file=f )
              band = bands.split(",")[-1]
              print ( 'cat ${exp}_x.fri | grep "' + sta_full_name + '" | awk ' + "'{ if ( $1 <= " + "%s" % obs_indx + " ) printf " + '"%6s\\n", ' + "$1}' | sort -u > ${exp}_${sta}_before.obs", file=f )
              print ( 'cat ${exp}_x.fri | grep "' + sta_full_name + '" | awk ' + "'{ if ( $1 >  " + "%s" % obs_indx + " ) printf " + '"%6s\\n", ' + "$1}' | sort -u > ${exp}_${sta}_after.obs",  file=f )
              for band in bands.split(","):
                  print ( '# ', file=f )
                  print ( '# --- Compute bandpass for %s band' %  band.upper(), file=f )
                  print ( '# ', file=f )
                  print ( 'pf.py $exp ' + band + ' bpas EXCLUDE_OBS_FILE: ' + pf_dir + '/${exp}/${exp}_${sta}_before.obs     \\', file=f )
                  print ( '                  BANDPASS_FILE:    ' + pf_dir + '/${exp}/${exp}_${sta}_' + band + '_after.bps', file=f )
                  print ( 'mv ' + pf_dir + '/${exp}/${exp}_' + band + '_bps.log    ' + pf_dir + '/${exp}/${exp}_${sta}_' + band + '_bps.log', file=f )
                  print ( 'pf.py $exp ' + band + ' bpas EXCLUDE_OBS_FILE: ' + pf_dir + '/${exp}/${exp}_${sta}_after.obs', file=f )

              for band in bands.split(","):
                  print ( '#', file=f )
                  print ( '# --- Run fringe fitting for station %s at %s band' % ( sta_full_name, band.upper()), file=f )
                  print ( '#', file=f )
                  print ( 'pf.py $exp ' + band + ' fine -keep INCLUDE_OBS_FILE: ' + pf_dir + '/${exp}/${exp}_${sta}_before.obs', file=f )
                  print ( 'pf.py $exp ' + band + ' fine -keep INCLUDE_OBS_FILE: ' + pf_dir + '/${exp}/${exp}_${sta}_after.obs \\', file=f )
                  print ( '                        BANDPASS_FILE:   ' + pf_dir + '/${exp}/${exp}_${sta}_' + band + '_after.bps', file=f )

         print ( '#', file=f )
         print ( '# --- Update the database', file=f )
         print ( '#', file=f )
         print ( 'pf.py $exp x mkdb -updt', file=f )
         print ( '#', file=f )
         print ( 'echo "Finished re-fringing for experiment %s because of a break at station %s after obs %s"' % \
                 ( exp, sta_full_name, obs_indx ), file=f  )

    f.close()
    os.system ( 'chmod o+x,g+rwx ' + filout )
    print ( 'Created command file  ' + filout )

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

    parser.add_argument ( "-o", \
                          "--filout", \
                          help="Output file name (default: ${exp}_${sta}.csh)")

    args = parser.parse_args()

    make_csh ( args.exp, args.sta.lower(), \
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
