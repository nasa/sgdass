#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program vex2_to_vex1 transforms schedule file from 2.0 format to   *
# *   1.5 format. It does not make a comprehensive lexical analysis, but *
# *   it supposed to transform schedule to a format that the old version *
# *   of vex2difx will be in a position to process the output schedule   *
# *   in vex1 format.                                                    *
# *                                                                      *
# *   If peculiar clock offsets are defined in the vex file, they are    *
# *   remained in the output with the comment prefix *vex1.              *
# *                                                                      *
# *   Contents of EXTENSIONS and literal blocks is removed in order to   *
# *   avoid lines longer 128 characters that old vex parser did not      *
# *   support.                                                           *
# *                                                                      *
# * ### 03-APR-2022  vex2_to_vex1.py  v1.3 (c) L. Petrov 19-JUL-2023 ### *
# *                                                                      *
# ************************************************************************
import  pwd, sys, os, shutil, signal, time, datetime
from    sur_sked_config import * # Import sur_sked confuguration
from    pet_misc        import *

vex2_converter__label = "vex2_to_vex1.py 20230719"

def main():
    if ( len(sys.argv)-1 != 2 ):
         print ( "Usage: vex2_file vex1_file" )
         exit  ( 1 )
    else:
         vex1_file = sys.argv[1]
         vex2_file = sys.argv[2]
    
    if ( not os.path.isfile(vex1_file) ):
         print ( "Cannot find input file %s "% vex1_file )
    
    buf = []
    with open(vex1_file,encoding="latin") as f:
         for line in f:
             buf.append ( line.strip("\n").strip("\r") )
    f.close()

    fl_ds   = False
    fl_ext  = False
    fl_proc = False
    fl_pco  = False
    out = []
    for line in buf:
        if ( len(line.split()) < 1 ):
#
# ---------- Empty line is passed through
#
             out.append ( line )
             continue
    
        if ( line[0:1] == "*" ):
#
# ---------- Comment line is passed through
#
             out.append ( line )
             continue
    
        if ( "VEX_rev" in line.split()[0] ):
#
# ---------- Version is updated
#
             line = line.replace("2.0","1.5")
    
        if ( "scheduling_software" in line and not fl_proc ):
#
# ---------- scheduling_software line is commented out
#
             line = "*vex1 " + line
    
        if ( "ref " in line and " $EXTENSIONS" in line ):
#
# ---------- Reference to EXTENSIONS are commented out
#
             line = "*vex1 " + line
    
        if ( "if_def" in line and not fl_proc ):
             id = line.find(":")
             line = line[0:id+1] + " DUMMY_" + line[id+1:].ljust(16).strip()
             line = line.replace(": H :",": X :").replace(": V :",": Y :")
    
        if ( line[0:1] == "$" ):
             fl_ds  = False
             fl_ext = False
    
        if ( "ref " in line and " $DATASTREAMS" in line ):
#
# ---------- A reference to DATASTREAMS blocks are replaced with TRACKS
#
             line = line.replace ( " $DATASTREAMS"," $TRACKS     ")
    
        if ( "antenna_motion" in line and not fl_proc ):
#
# ---------- The 4th field in pointing_sector is removed
#
             id = line.rfind(":")
             line = line[0:id-1] + ";"
    
        if ( "pointing_sector" in line and not fl_proc ):
#
# ---------- The 8th field in pointing_sector is removed
#
             id = line.rfind(":")
             line = line[0:id-1] + ";"
    
        if ( "$DATASTREAMS;" in line ):
#
# ---------- $DATASTREAMS block is repalced with $TRACKS bock
#
             line = "$TRACKS;"
             fl_ds = True
    
        if ( fl_ds and len(line.split()) >= 2 ):
             if ( "datastream" in line and \
                  "=" in line          and \
                  not fl_proc              ):
#
# --------------- datastream definition is replaced with track_frame_format
#
                  data_fmt = line.replace(":"," ").replace(";"," ").replace("="," ").split()[2]
                  line = "     track_frame_format = " + data_fmt + ";"
        if ( "thread" in line      and \
             "=" in line           and \
             not "channel" in line and \
             not fl_proc               ):
#
# --------------- Thread definition is removed
#
             continue       
        if ( "channel" in line and \
              not fl_proc          ):
#
# ---------- Channel defnition is replaced with a pair of fanout definitoins 
#
             chan_id = line.split(":")[2].split()[0].replace("&","")
             out.append ( "     fanout_def =   : &" + chan_id + " : sign : 1:  1;" )
             line       = "     fanout_def =   : &" + chan_id + " :  mag : 1:  1;"
    
        if ( " intent " in line ):
#
# ---------- Comment out lines with intent
#
             out.append ( "*vex1 " + line )
             continue

        if ( "PECULAIR_CLOCK_OFFSET" in line and "def " in line ):
#
# ---------- Special trick for keeping data with peculiar clock offsets as comments
# ------ Set a flag "peculiar clock offset section"
#
             fl_pco = True
             out.append ( "*vex1 " + line )
             continue


        if ( "enddef;" in line ):
             if ( fl_pco ):
                  out.append ( "*vex1 " + line )
             fl_pco = False
#
# ---------- Lift a flag "peculiar clock offset section"
#

        if ( fl_pco and "extension" in line and "NASA" in line ):
#
# ---------- Special trick for keeping data with peculiar clock offsets as comments
#
             out.append ( "*vex1 " + line )
             continue

        if ( "$EXTENSIONS;" in line ):
#
# ---------- Set the flag "EXTENSIONS" seciton
#
             fl_ext = True

        if ( fl_ext ):
#
# ---------- Contents of $EXTENSIONS block and its defintion is removed
# ------ with an execption of pecular clock offset considered eariler.
# ---------- Peculuar clock offset data are copied as comments
#
             continue

        if ( "start_literal(proc" in line ):
             fl_proc = True
             continue
    
        if ( "end_literal(proc" in line ):
             fl_proc = False
             continue
    
        if ( fl_proc ):
#
# ---------- Contents of the literal block is removed
#
             continue

        out.append ( line )
        if ( "VEX_rev" in line.split()[0] ):
#
# ---------- Add a line with provenance
#
             out.append ( "* converted from vex2 to vex1 using " + vex2_converter__label )

#
# -- Write down the output vex2 file
#
    f=open(vex2_file,"w")
    for line in out:
        print ( line, file=f )
    f.close()

if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
        main()
    except KeyboardInterrupt:
        print ( "pf.py: Interrupted" )
        exit ( 1 )
