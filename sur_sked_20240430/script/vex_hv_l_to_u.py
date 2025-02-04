#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program vex_hv_l_to_u modifies $FREQ section of vex file: it       *
# *   converts LSB frequency channel defition to USB defintion.          *
# *                                                                      *
# *  ### 15-JUL-2023 vex_hv_l_to_u v1.0 (c)  L. Petrov  15-JUL-2023 ###  *
# *                                                                      *
# ************************************************************************
import  pwd, sys, os, shutil, signal, time, datetime
from   sur_sked_config import * # Import sur_sked confuguration
from   pet_misc        import *

prog__label = "vex_hv_l_to_u.py 20230715"

def main():
    if ( len(sys.argv)-1 != 2 ):
         print ( "Usage: vex_hv_l_to_u input_vex output_vex" )
         exit  ( 1 )
    else:
         filin  = sys.argv[1]
         filout = sys.argv[2]

    if ( not os.path.isfile(filin) ):
         print ( "Cannot find input file %s "% filin )

#
# --- Read input vex file.
#
    buf = []
    with open(filin,encoding="latin") as f:
         for line in f:
             buf.append ( line.strip("\n").strip("\r") )
    f.close()

    out = []
    n_rep = 0
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
        if ( ( "&H" in line or "&V" in line) and \
             "&CH" in line and "&BBC" in line    ):
#
# ---------- Well, we found the frequency defition in the fex file
#
             freq_ch  = float(line.split()[4])
             sub_band = line.split()[7]
             freq_wid = float(line.split()[9])
             if ( sub_band == "L" ):
#
# --------------- Reduce the channel frequency by with with chennale width
# --------------- and update the label.
#
                  freq_ch_str = ("%8.2f" % (freq_ch - freq_wid)).strip()
                  line = line.replace(line.split()[4],freq_ch_str).replace(line.split()[7],"U") + \
                         " * converted from LSB to USB"
                  n_rep = n_rep + 1

        out.append ( line )
        if ( "VEX_rev" in line.split()[0] ):
#
# ---------- Add a line with provenance
#
             out.append ( "* converted HV data from LSB to USB using %s" % prog__label )

    if ( n_rep > 0 ):
#
# ------ Write down the output vex file
#
         f=open(filout,"w")
         for line in out:
             print ( line, file=f )
         f.close()
         print ( "%d LSB definitions for H or V polarization were convertd to USB" % n_rep )
         print ( "Output vex file %s is written" % filout )
    else:
#
# ------ Nothing to do: no subsitution were made
#
         print ( "No LSB definition for H or V polarization was found" )

if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
        main()
    except KeyboardInterrupt:
        print ( "pf.py: Interrupted" )
        exit ( 1 )
