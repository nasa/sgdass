#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program sds_slew_process.py takes the input VLBI Field System log  *
# *   file from a VLBI schedule that was produced with sds_to_snap.py    *
# *   or sds_to_snap.py, extracts apriori slewing information, as well   *
# *   as predicted and and actual slewing time.                          *
# *                                                                      *
# *   NB: This program will not work with log files from schedules       *
# *       generated other ways, because such log files do not have       *
# *       critical information.                                          *
# *                                                                      *
# *   Copyright (c) 1975-2025 United States Government as represented by *
# *   the Administrator of the National Aeronautics and Space            *
# *   Administration. All Rights Reserved.                               *
# *   License: NASA Open Source Software Agreement (NOSA).               *
# *                                                                      *
# * ## 06-AUG-2025 sds_slew_process.py v1.0 (d) L. Petrov 06-AUG-2025 ## *
# *                                                                      *
# ************************************************************************
import sys, os, shutil, time, subprocess, math, datetime
sys.path.append('/auto')
from   pet_misc  import *

num_args = len(sys.argv) - 1
if ( num_args < 1 ):
     print ( "Usage: sds_slew_process input_log_file [Output_slew_file]" )
     exit ( 1 )
fili = sys.argv[1]
if ( num_args > 1 ):
     filo = sys.argv[2]
else:
     filo = fili.replace(".log",".slew")

bufl = read_file ( fili )

slew = []
head = []
fl_head = False
fl_vlbi = False
for i in range(0,len(bufl)):
    line = bufl[i]
    if ( 'Slewed from az/el' in line ):
          slew.append ( line.replace('"','') )
    if ( " slew ends at " in line ):
          slew.append ( line.replace('"','') )
    if ( " new az/el/ha: " in line ):
          slew.append ( line.replace('"','').replace(',',' ') )
    if ( 'Source acquired' in line or 'antenna,acquired' in line ):
          slew.append ( line[0:20] + " on source" )

    if ( '" Control file for a single dish experiment' in line ):
         fl_head = True

    if ( '" VLBI experiment schedule in snap format' in line ):
         fl_head = True
         fl_vlbi = True
   
    if ( fl_head ):
         head.append ( line[23: ] )

    if ( "< END OF THE EXPERIMENT DESCRIPTION" in line ):
         fl_head = False

    if ( "Recording_rate: " in line ):
         fl_head = False
         
    if ( fl_vlbi ):
         if ( '"'          in line or \
              '!'          in line or \
              '#trakl#'    in line or \
              'scan_name=' in line    ):
              slew.append ( line )
              
print ( "fl_vlbi= ", fl_vlbi ) 


out=[]
nsca     = 0
nsca_acq = 0
nsca_lim = 0
for i in range(1,len(slew)):
    if ( 'scan_name=' in slew[i] ):
         nsca = nsca + 1
    if ( "on source" in slew[i] ):
         if ( "Slewed from az/el" in slew[i-1] ):
              time_before = datetime.datetime.strptime( slew[i-1][0:20], '%Y.%j.%H:%M:%S.%f')
              time_after  = datetime.datetime.strptime( slew[i][0:20], '%Y.%j.%H:%M:%S.%f')
              time_delta  = time_after - time_before
              tim_sec = time_delta.seconds + time_delta.microseconds/1000000.0
              out.append ( slew[i-1][0:73] + "  Expected " + slew[i-1][128:] + "  Actual slew: %6.2f" % tim_sec )
         elif ( " slew ends at " in slew[i-1] ):
#
              time_start  = datetime.datetime.strptime( slew[i-1][0:20], '%Y.%j.%H:%M:%S.%f' )
              time_expect = datetime.datetime.strptime( slew[i-1][35:55], '%Y.%j.%H:%M:%S.%f' )
              time_after  = datetime.datetime.strptime( slew[i][0:20], '%Y.%j.%H:%M:%S.%f' )
#
              time_delta         = time_expect - time_start  
              tim_expected_sec   = time_delta.seconds + time_delta.microseconds/1000000.0
              if ( tim_expected_sec > 80000 ): tim_expected_sec = tim_expected_sec - 86400.0
#
              time_delta     = time_after - time_start
              tim_actual_sec = time_delta.seconds + time_delta.microseconds/1000000.0
              if ( tim_actual_sec > 80000 ): tim_actual_sec = tim_actual_sec - 86400.0
              old_az = float(slew[i-2].split()[8])
              old_el = float(slew[i-2].split()[9])
              new_az = float(slew[i-2].split()[3])
              new_el = float(slew[i-2].split()[4])
              out.append ( slew[i-1][0:20] + "  Slewed from az/el %7.1f, %5.2f to %7.1f, %5.2f" %
                           ( old_az, old_el, new_az, new_el ) + \
                           " Expected slew_time  %6.2f  Actual slew: %6.2f" % \
                           ( tim_expected_sec, tim_actual_sec ) )
    elif ( "#trakl# Source acquired"  in slew[i] or
           "#trakl# Waiting at limit" in slew[i]   ):
           for k in range(0,10):
               if ( slew[i-k][103:116] == "Slewing time:" ): 
                    time_start  = datetime.datetime.strptime( slew[i-k][0:20], '%Y.%j.%H:%M:%S.%f' )
                    time_end    = datetime.datetime.strptime( slew[i][0:20], '%Y.%j.%H:%M:%S.%f' )
                    time_delta  = time_end - time_start  
                    tim_actual_sec = time_delta.seconds
#
                    time_expected_sec = float( slew[i-k][118:123] )
                    old_az = float(slew[i-k].split()[8].replace(",",""))
                    old_el = float(slew[i-k].split()[9].replace(",",""))
                    new_az = float(slew[i-k].split()[3].replace(",",""))
                    new_el = float(slew[i-k].split()[4].replace(",",""))
#
                    out.append ( slew[i][0:20] + "  Slewed from az/el %7.1f, %5.2f to %7.1f, %5.2f" %
                            ( old_az, old_el, new_az, new_el ) + \
                            " Expected slew_time  %6.2f  Actual slew: %6.2f" % \
                            ( time_expected_sec, tim_actual_sec ) )
                    if ( "#trakl# Source acquired"  in slew[i] ):
                         nsca_acq = nsca_acq + 1
                    if ( "#trakl# Waiting at limit" in slew[i] ):
                         nsca_lim = nsca_lim + 1
                 

f=open(filo,"w")
for line in head:
#    print ( "LINE: ", line ) # %%%%%%%%%%%%
    if ( 'Control file for a single dish experiment' in line or \
         'VLBI experiment schedule in snap format' in line    ):
        line = "Slew measurement from a single dish experiment. Format version  1.00 of 2021.06.29"
    print ( "# " + line, file=f )
for line in out:
    print ( line, file=f )
f.close()
print ( '%4d scans                     were found' % nsca     )
print ( '%4d "source acquired"         were found' % nsca_acq )
print ( '%4d "source waiting at limit" were found' % nsca_lim )
print ( '%4d scans lost                          ' % ( nsca - nsca_acq - nsca_lim ) ) 
print ( "output file ", filo   )
