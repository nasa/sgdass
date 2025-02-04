#!/usr/bin/env python3

import  pwd, sys, os, re, shutil, time, subprocess, datetime
from    pet_misc  import *
#
def bnc_scav_status ( fil_scav ):

    """
    Routine to check if the scav file has been updated already?

    19-MAR-2024  bnc_scav_upda v1.3 (c) N. Habana
    """
#
# -- Does the scav file exist?
#
    if ( os.path.isfile (fil_scav) ):
#
# ---- read the file to a buffer
#
       buf_scav = read_file ( fil_scav )
#
# ---- Go through file and check if it's been updated
#
       flag_scav = False
       for line in buf_scav:
           lin_splt = line.split()
#
# -------- Go throught the PROVENANCE block
#
           if ( lin_splt[0] == "PROVENANCE:" ):
              if ( lin_splt[2] == "DATA_TYPE:" and lin_splt[3] == "2" ):
                 flag_scav = True 
#
# -------- Once you're done with the PROVENANCE block, leave the loop
#
           if ( lin_splt[0] == "NUM_DATA_ON:" ): break
#
# -- File was not found
#
    else:
       print ( "Error: Could not find scav file " + fil_scav )
       flag_scav = True
       return flag_scav
#
    return flag_scav
