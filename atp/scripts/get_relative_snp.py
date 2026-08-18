# -*- coding: utf-8 -*-
"""
Program get_relative_snap.py generates the relative time snap file 
for an instow SDE.

16-FEB-2024   get_relative_snap.py v1.0 (c) N. Habana
"""
#
# Routines to import
#
import  pwd, sys, os, re, shutil, time, subprocess, datetime
from pet_misc import *
#
def get_rel_snp ( fil_snp ):
    #
    # -- Read the file to buffer, thus checking if it exists
    #
    snp_buf = read_file ( fil_snp )
    #
    # -- Go through file and make the edits of muting any of the
    #    time stamps
    #
    snp_out = []
    for line in snp_buf:
        #
        # -- Mute the times like !2024.047.20:13:00.00
        #
        if ( line[0:1] == "!" ):
           line_split = line.split(".")
           if ( len(line_split) > 1 ):
              lin_prt = "\"" + line
              snp_out.append( lin_prt )
           else:
              snp_out.append( line )
        #
        # -- Add the "log=station" line
        #
        elif ( line == "stop_mlog" ):
           snp_out.append( line )
           snp_out.append( "log=station" )
        else:
           snp_out.append( line )
    #
    return snp_out

              
