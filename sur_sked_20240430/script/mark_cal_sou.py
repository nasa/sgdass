#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program mark_cal_sou.py marks a list of comma-separated source     *
# *   names as calibrators in a given VLBI schedule in vex1 or vex2      *
# *   format. It writes the updated schedule with field intent. Since    *
# *   vex1 does not support intent field, mark_cal_scans.py  adds line   *
# *   "* intent  ...", which instructs the parser to ignore that line.   *
# *                                                                      *
# *                                                                      *
# *   Example:                                                           *
# *                                                                      *
# *   mark_cal_scans.py /tmp/y1.vex 3C84, 4C39.25 /tmp/y2.vex            *
# *                                                                      *
# * ### 19-JUL-2023 mark_cal_scans.py v1.0 (c) L. Petrov 19-JUL-2023 ### *
# *                                                                      *
# ************************************************************************
import  pwd, sys, os, shutil, signal, time, datetime
from    sur_sked_config import * # Import sur_sked confuguration
from    pet_misc        import *

mark_cal_scans__label = "mark_cal_scans.py 20230606"

def main():
    if ( len(sys.argv)-1 < 3 ):
         print ( "Usage: mark_cal_scans.py vex_input_file cal_sources vex_output_file" )
         exit  ( 1 )
    else:
         vex_input_file  = sys.argv[1]
         cal_sou         = sys.argv[2]
         vex_output_file = sys.argv[3]
    
    if ( not os.path.isfile(vex_input_file) ):
         print ( "Cannot find input file %s "% vex_input_file )
         exit  ( 1 )
    
#
# --- Read input vex file
#
    buf = []
    with open(vex_input_file,encoding="latin") as f:
         for line in f:
             buf.append ( line.strip("\n").strip("\r") )
    f.close()

    if ( len(buf) < 8 ):
         print ( "Input vex file %s is too short" % vex_input_file )
         exit  ( 1 )

#
# --- Check version of the input vex file
#
    if ( buf[0][0:7] == "VEX_rev" ):
         if ( len( buf[0].replace("="," ").replace(";"," ").split() ) < 2 ):
              print ( "Malformed 1st line of the input vex file: cannot get version number" )
              exit ( 1 )
         else:
              vex_vers = buf[0].replace("="," ").replace(";"," ").split()[1]
              if ( vex_vers != "1.5" and vex_vers != "2.0" ):
                   print ( "Input vex file has version %s -- only versions 1.5 and 2.0 are supported" % \
                            vex_vers )
    else:
         print ( "Malformed vex file: the first line should have VEX_rev" )
         exit ( 1 )

    sou_list = cal_sou.split(",")

#
# --- Cycle over lines of the inpupt vex file. Output schdule is written
# --- in the list out.
#
    ind_sou_line = -1
    fl_sched = False
    num_cal = 0
    num_int = 0
    out = []
#    print ( "sou_list= ", sou_list ) # %%%%%%
    for i in range(0,len(buf)):
        line = buf[i]
        if ( line[0:1] == "$"  ): 
             fl_sched = False
        if ( line.upper() == "$SCHED;" ): 
#
# ---------- Set the flag "We are inside of $SCHED block"
#
             fl_sched = True
        if ( fl_sched ):
             if ( line[0:5] == "scan " ):
#
# --------------- We found a scan block. Extract source name
#
                  sca_nam = line.split()[1].replace(";","").split()[0]
                  sou_nam = "??"
                  for j in range(i+1,len(buf)):
                      if ( "source" in buf[j] ):
                           sou_nam = buf[j].replace("="," ").replace(";"," ").split()[1]
                           if ( sou_nam in sou_list ):
#                                print ( "sca ", sca_nam, ' sou= ', sou_nam ) # %%%%%%%%%%%%
#
# ----------------------------- Yes, this source is the calibrator!
#
                                ind_sou_line = j
                           break
                      if ( "endscan;" in buf[j] ):
                           break
                  if ( sou_nam == "??" ):
                       print ( "Trap of internal control: did not find source definition for scan %s" % sca_nam )
                       exit  ( 1 )

        out.append ( line )
        if ( i == ind_sou_line ):
             if ( i < len(buf) ):
                  if ( buf[i+1][0:1] != "*" and  "intent" in buf[i+1] ):
#
# -------------------- This scan is already marked as "calibrator"
#
                       num_int = num_int + 1
                       ind_sou_line = -1
                       continue
#
# ---------- This scan was not marked as a calibrator before,
# ---------- but the scan name is in the list of calibrator scans
#
             if ( vex_vers == "2.0" ):
                  out.append ( "    intent = : CALIBRATE_BANDPASS : TRUE;" )
                  out.append ( "    intent = : FRINGE_FINDER      : TRUE;" )
             elif ( vex_vers == "1.5" ):
                  out.append ( "*vex1 intent = : CALIBRATE_BANDPASS : TRUE;" )
                  out.append ( "*vex1 intent = : FRINGE_FINDER      : TRUE;" )
             num_cal = num_cal + 1
#
# ---------- Lift the flag of the calinrator scan
#
             ind_sou_line = -1

    if ( num_cal > 0 ):
#
# --- Write the output file
#
        f=open(vex_output_file,"w")
        for line in out:
            print ( line, file=f )
        f.close()

        print ( "%d calibrator scans have been marked in the schedule file" % num_cal )
        if ( num_int > 0 ): 
             print ( "%d scans have been already marked as calibrators in the schedule file" % num_int )
        print ( "Output schedule file %s is written" % vex_output_file )
    else:
        if ( num_int > 0 ): 
             print ( "%d scans have been already marked as calibrators in the schedule file" % num_int )
        print ( "No unmarked calibrators were found in the vex file" )
        print ( "No output file is generated" )
    

if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
        main()
    except KeyboardInterrupt:
        print ( "pf.py: Interrupted" )
        exit ( 1 )
