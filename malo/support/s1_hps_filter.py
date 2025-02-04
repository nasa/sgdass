#!/usr/bin/env python
# ************************************************************************
# *                                                                      *
# *   Program s1_hps_filter.py filters harpos file keeping only          *
# *   displacements for S1 frequency.                                    *
# *                                                                      *
# *  ### 09-APR-2014               v1.0 (c)  L. Petrov  09-APR-2014 ###  *
# *                                                                      *
# ************************************************************************
import sys, os, shutil, subprocess
import datetime
import optparse 

fil_in  = sys.argv[1]
fil_out = sys.argv[2]

with open(fil_in) as f:
     line_list = f.readlines()

g = open ( fil_out, 'w' )

for i in range(0,len(line_list)):
    
    if ( line_list[i].find("H  PI1")  > -1 ): continue
    if ( line_list[i].find("H  P1")   > -1 ): continue
    if ( line_list[i].find("H  K1")   > -1 ): continue
    if ( line_list[i].find("H  PSI1") > -1 ): continue
    if ( line_list[i].find("H  2T2")  > -1 ): continue
    if ( line_list[i].find("H  T2")   > -1 ): continue
    if ( line_list[i].find("H  S2")   > -1 ): continue

    if ( line_list[i].find("D  PI1")  > -1 ): continue
    if ( line_list[i].find("D  P1")   > -1 ): continue
    if ( line_list[i].find("D  K1")   > -1 ): continue
    if ( line_list[i].find("D  PSI1") > -1 ): continue
    if ( line_list[i].find("D  2T2")  > -1 ): continue
    if ( line_list[i].find("D  T2")   > -1 ): continue
    if ( line_list[i].find("D  S2")   > -1 ): continue
    print ( line_list[i], end="", file=g )
