#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program camp_schedule_submit.py submits VLBI schedules to CDDIS    *
# *   and the sgp server.                                                *
# *                                                                      *
# * ## 05-APR-2023 camp_schedule_submit.py v1.6 (c) L. Petrov 25-MAR-2026 ##  *
# *                                                                      *
# ************************************************************************
import           pwd, sys, os, re, shutil, time, subprocess, datetime
sys.path.append('/auto')
from   pet_misc  import *

master_tmpl     = "/vlbi/@@/@@_master.txt"
proto_file_tmpl = "/vlbi/@@/@@_vex.proto"
rem_host_list   = [ "de:/astrogeo/sch/vlbi", "sg:/sch/vlbi" ]
vsdc_com        = "/opt64/bin/vsdc.py -c /cont/vsdc/cddis.cnf"
to_astro        = True
to_cddis        = True

dir      = "/vlbi"
ivrb     = 1
rev      = 0

if ( len(sys.argv)-1 < 1 ):
     print ( "Usage: camp_schedule_submit.py exper [rev] [verb]" )
     exit  ( 1 )
else:
     exper = sys.argv[1]
     if ( len(sys.argv)-1 >= 2 ):
          rev = int(sys.argv[2])

     if ( len(sys.argv)-1 >= 3 ):
          ivrb = int(sys.argv[3])

camp = exper[0:2]
master_file = master_tmpl.replace("@@",camp)
proto_file = proto_file_tmpl.replace("@@",camp)


vex_file = dir + "/" + exper + "/" + exper + ".vex" 
if ( not os.path.isfile ( vex_file ) ):
     print ( "Did not found schedule file ", vex_file )
     exit  ( 1 )

txt_file = dir + "/" + exper + "/" + exper + ".txt" 
if ( not os.path.isfile ( txt_file ) ):
     print ( "Did not found schedule file ", txt_file )
     exit  ( 1 )

mas = read_file ( master_file )
if ( mas == None ):
     print ( "Cannot find master file %s" % master_file )
     exit  ( 1 )

sta_list = []
for line in mas:
    if ( len(line.split()) <  5 ): continue
    if ( line[0:1] == "#"       ): continue
    if ( line.split()[3] == exper ):
         sta_list = line.split()[4].split(",")
if ( len(sta_list) == 0 ):
     print ( "Did not find experiment %s in master file %s" % ( exper, master_file ) )
     exit  ( 1 )


fl_cddis_error = False
for sta in sta_list:
    snp_file = dir + "/" + exper + "/" + exper + sta + ".snp" 
    prc_file = dir + "/" + exper + "/" + exper + sta + ".prc" 
    lst_file = dir + "/" + exper + "/" + exper + sta + ".lst" 
    if ( not os.path.isfile ( snp_file ) ):
         print ( "Did not found schedule file ", snp_file )
         exit  ( 1 )
    
    if ( not os.path.isfile ( prc_file ) ):
         print ( "Did not found schedule file ", prc_file )
         exit  ( 1 )
    
    if ( not os.path.isfile ( lst_file ) ):
         print ( "Did not found schedule file ", lst_file )
         exit  ( 1 )
    
    buf = read_file ( snp_file )

    sched_rev  = 0
    sched_year = 0
    for line in buf:
        if ( '" Schedule_revision:' in line ):
              sched_rev = int(line.split()[2])

        if ( '" UTC_experiment_dates:' in line ):
              sched_year = int(line.split()[2].split(".")[0])

    if ( sched_rev == 0 ):
         print ( "Trap of the internal control: schedule revision was not found in ", snp_file )
         exit  ( 1 )

    if ( sched_year == 0 ):
         print ( "Trap of the internal control: epxeriment year was not found in ", snp_file )
         exit  ( 1 )

    if ( rev > 0 and sched_rev != rev ):
         print ( "Schedule file %s has revision %d, while revision %d is requested"  % \
                 ( snp_file, rev, sched_rev ) )
         exit  ( 1 )

    com = "check_proc_snap " + prc_file + " " + snp_file + " 0"

    if ( ivrb > 1 ):
        print ( "About to execute= ", com )
    (ret,out) = exe ( com )
    check_err_exe ( ret, out, com )
    if ( ret == 0 ):
         if ( ivrb > 0 ):
              print ( "Validation test for proc/snap files for %s has passed" % sta )
    else:
         print ( "Validation test for proc/snap files for %s has failed" % sta )
         exit  ( 1 )

    if ( to_astro ):    
#
# ------ Submit prc file to the astrogeo web server
#
         for rem_host in rem_host_list:
             com = "scp " + prc_file + " " + rem_host + "/" + sta + "/" + "%4d" % sched_year + "/"
             if ( ivrb > 1 ):
                  print ( "About to execute= ", com )
             (ret,out) = exe ( com )
             check_err_exe ( ret, out, com )
             if ( ret == 0 ):
                  if ( ivrb > 0 ):
                       print ( "Submitted %s to astrogeo web server %s" % \
                               ( prc_file, rem_host ) )
             else:
                  print ( "Could not submit %s file to astrogeo web server %s" % \
                          ( prc_file, rem_host ) )
                  exit  ( 1 )
    
#
# ---------- Submit snp file to the astrogeo web server
#
             com = "scp " + snp_file + " " + rem_host + "/" + sta + "/" + "%4d" % sched_year + "/"
             if ( ivrb > 1 ):
                  print ( "About to execute= ", com )
             (ret,out) = exe ( com )
             check_err_exe ( ret, out, com )
             if ( ret == 0 ):
                  if ( ivrb > 0 ):
                       print ( "Submitted %s to astrogeo web server %s" % \
                               ( snp_file, rem_host ) )
             else:
                  print ( "Could not submit %s file to astrogeo web server %s" % \
                               ( snp_file, rem_host ) )
                  exit  ( 1 )
    
#
# ---------- Submit lst file to the astrogeo web server
#
             com = "scp " + lst_file + " " + rem_host + "/" + sta + "/" + "%4d" % sched_year + "/"
             if ( ivrb > 1 ):
                  print ( "About to execute= ", com )
             (ret,out) = exe ( com )
             check_err_exe ( ret, out, com )
             if ( ret == 0 ):
                  if ( ivrb > 0 ):
                       print ( "Submitted %s to astrogeo web server %s" % \
                               ( lst_file, rem_host ) )
             else:
                  print ( "Could not submit %s file to astrogeo web server %s" % \
                               ( lst_file, rem_host ) )
                  exit  ( 1 )
    
    if ( to_cddis ):
#
# ------ Submit the snap file to the data center
#
         com = vsdc_com + " -t session_aux_snp -f " + snp_file
         if ( ivrb > 1 ):
              print ( "About to execute= ", com )
         (ret,out) = exe ( com )
         if ( ret == 0 ):
              if ( ivrb > 0 ):
                   print ( "Submitted %s to cddis" % snp_file )
         else:
              for line in out:
                  print ( line )
              print ( "Failed command: %s", com )
              print ( "Could not submit %s file to cddis" % snp_file )
              fl_cddis_error = True
 
#
# ------ Submit the prc file to the data center
#
         com = vsdc_com + " -t session_aux_prc -f " + prc_file
         if ( ivrb > 1 ):
              print ( "About to execute= ", com )
         (ret,out) = exe ( com )
         if ( ret == 0 ):
              if ( ivrb > 0 ):
                   print ( "Submitted %s to cddis" % prc_file )
         else:
              for line in out:
                  print ( line )
              print ( "Failed command: %s", com )
              print ( "Could not submit %s file to cddis" % prc_file )
              fl_cddis_error = True

if ( to_astro ):
     for rem_host in rem_host_list:
         com = "scp " + vex_file + " " + rem_host + "/vex/" 
         if ( ivrb > 1 ):
              print ( "About to execute= ", com )
         (ret,out) = exe ( com )
         check_err_exe ( ret, out, com )
         if ( ret == 0 ):
              if ( ivrb > 0 ):
                   print ( "Submitted %s to astrogeo web server %s" % \
                           ( vex_file, rem_host ) )
         else:
              print ( "Could not submit %s file to astrogeo web server %s" % \
                      ( vex_file, rem_host ) )
              exit  ( 1 )
    
         com = "scp " + txt_file + " " + rem_host + "/txt/" 
         if ( ivrb > 1 ):
              print ( "About to execute= ", com )
         (ret,out) = exe ( com )
         check_err_exe ( ret, out, com )
         if ( ret == 0 ):
              if ( ivrb > 0 ):
                   print ( "Submitted %s to astrogeo web server %s" % \
                           ( txt_file, rem_host ) )
         else:
              print ( "Could not submit %s file to astrogeo web server %s" % \
                      ( txt_file, rem_host ) )
              exit  ( 1 )

if ( to_cddis ):
#
# --- Submit vex file to the data center
#
     com = vsdc_com + " -t session_aux_vex -f " + vex_file
     if ( ivrb > 1 ):
          print ( "About to execute= ", com )
     (ret,out) = exe ( com )
     if ( ret == 0 ):
          if ( ivrb > 0 ):
               print ( "Submitted %s to cddis" % vex_file )
     else:
          for line in out:
              print ( line )
          print ( "Failed command: %s", com )
          print ( "Could not submit %s file to cddis" % vex_file )
          fl_cddis_error = True

#
# --- Submit txt file to the data center
#
     com = vsdc_com + " -t session_aux_txt -f " + txt_file
     if ( ivrb > 1 ):
          print ( "About to execute= ", com )
     (ret,out) = exe ( com )
     if ( ret == 0 ):
          if ( ivrb > 0 ):
               print ( "Submitted %s to cddis" % txt_file )
     else:
          for line in out:
              print ( line )
          print ( "Failed command: %s", com )
          print ( "Could not submit %s file to cddis" % txt_file )
          fl_cddis_error = True

if ( fl_cddis_error ):
     print ( "Could not submit schedule files to CDDIS" )
     exit  ( 1 )
