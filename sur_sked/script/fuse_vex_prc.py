#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Routine fuse_vex_prc.py                                            *
# *                                                                      *
# * ### 22-FEB-2022 fuse_vex_prc.py v2.0 (c)  L. Petrov  01-APR-2022 ### *
# *                                                                      *
# ************************************************************************
import pwd, sys, os, re, shutil, time, subprocess, datetime
from   sur_sked_config import * # Import sur_sked confuguration
from   pet_misc        import *

prc_dict = { 'preses'  : '10', \
             'setmode' : '10', \
             'setscan' : '10', \
             'preob'   : '10', \
             'midob'   : '10', \
             'postob'  : '10', \
             'postses' : '10'  \
           }
ind_setup = 0

fuse_vex_prc__label = "fuse_veq_prc 20230531"

if ( len(sys.argv)-1 < 3 ):
     print ( "Usage: fuse_vex_prc.py vex_in vex_prc vex_out [setup_ind] [exp]"  )
     exit  ( 1 )
else:
     vex_in  = sys.argv[1]
     vex_prc = sys.argv[2]
     vex_out = sys.argv[3]
     if ( len(sys.argv)-1 >= 4 ):
          ind_setup = int(sys.argv[4])
     if ( len(sys.argv)-1 >= 5 ):
          exp = sys.argv[5]

if ( ind_setup == 0 ):
     hds_tmpl = "__@HDS@__"
else:
     hds_tmpl = "__@HDS%d@__" % ind_setup

vex = read_file ( vex_in  )
prc = read_file ( vex_prc )

if ( not vex ):
     print ( "fuse_vex_prc.py: Cannot find file ", vex_in )
     exit  ( 1 )

if ( not prc ):
     print ( "fuse_vex_prc.py: Cannot find file ", vex_prc )
     exit  ( 1 )

sta_nam = None
for i in range(0,len(prc)):
    line = prc[i]
    if ( len(line.split()) == 0 ): continue
    if ( " Station:" in line ):
          sta_nam = line.split()[3].lower()
    if ( line.split()[0] == "define" ):
         if ( len(prc[i+1].split()) < 2 ): continue
         if ( prc[i+1].split()[1] == "Duration:" ):
              for prc_name in prc_dict.keys():
                  prc_name_def = line.split()[1].split("_")[0]
                  if ( prc_name == prc_name_def ):
                       prc_dict[prc_name] = prc[i+1].split()[2].replace("'","").replace('"','')

if ( sta_nam == None ):
     print ( "Station %s was not found in procedire file %s" % \
             ( sta_nam, vex_prc) )
     exit  ( 1 )

#
# --- Replace duration time
#
fl_found = "no"
for i in range(0,len(vex)):
    if ( len(vex[i].split()) >= 12 ):
         if ( "__@DUR@__" in vex[i] and vex[i].split()[4] == sta_nam ):
               prc_name = vex[i].split()[6]
               vex[i] = vex[i].replace("__@DUR@__",prc_dict[prc_name])
     
         if ( "FS_PROC_" + hds_tmpl + sta_nam in vex[i] ):
               fl_found = "yes"

if ( not fl_found ):
     print ( "fuse_vex_prc.py: Did not find a definition for FS_PROC extension for station %s in the input vex file %s" % \
             ( sta_nam, vex_in ) )
     exit ( 1 )

setmode_buf = []
fl_setmode   = False
for lin in prc:
    if ( len(lin.split()) >= 2 ):
         if ( "define" == lin.split()[0] and \
              "setmode" in lin.split()[1] ):
               fl_setmode = True
    if ( fl_setmode ):
         setmode_buf.append ( lin )
         if ( lin[0:6] == "enddef" ):
               fl_setmode = False

fl_prov = False
fl_next_mode = False
out = []
i = -1
for line in vex:
    i = i + 1
    if ( str("      start_literal(proc_" + hds_tmpl + "_" + sta_nam + ")") in line ):
         if ( str("      end_literal(proc_" + hds_tmpl + "_" + sta_nam + ")") in vex[i+1] ):
#
# ----------- This is the first mode for this station. Copy the procedure file as is 
#
              out.append ( line )
              for lin in prc:
                  out.append ( "        " + lin )
              continue
         else:
#
# ----------- This is the not mode for this station. Let us first find 
# ----------- the setmode command in the new procedure file
#
              fl_next_mode = True

    if ( exp != None and                      \
         "@@_schedule_convert_@@" in line and \
         " " + sta_nam + " " in line          ):
         schedule_proc_com = "vex_to_snap.py -m mk6_ext -o " + exp + sta_nam + ".snp" + " " + \
                             exp + ".vex" + " " + sta_nam
         line = line.replace ( "@@_schedule_convert_@@", schedule_proc_com )

    if ( line[0:1] != "*" and " def" in line and "PROVENANCE" in line ):
         fl_prov = True
    if ( fl_prov and line[0:1] != "*" and " enddef" in line ):
         out.append ( "      extension = NASA : fuse_vex_prc.py    : %s : %s : %s : %s ;" % \
                 ( sta_nam, \
                   vex_prc, \
                   fuse_vex_prc__label.split()[-1], \
                   datetime.datetime.strftime( datetime.datetime.now(), "%Y.%m.%d_%Hh%Mm%Ss") ), \
                 ) 
         fl_prov = False
    out.append ( line )

if ( fl_next_mode ):

     fl_proc = False
     fl_setmode = False
     ind_setmode_beg = 0
     ind_setmode_end = 0

     i = -1
     for line in out:
         i = i + 1
         if ( str("      start_literal(proc_" + hds_tmpl + "_" + sta_nam + ")") in line ):
              fl_proc = True
         if ( fl_proc and len(line.split()) >= 2 ):
              if ( "define"  == line.split()[0] and \
                   "setmode" in line.split()[1]     ):
                   fl_setmode = True
                   ind_setmode_beg = i

         if ( fl_proc    and \
              fl_setmode and \
              line.split()[0] == "enddef"  ):
              fl_setmode = False
              ind_setmode_end = i
         if ( str("      end_literal(proc_" + hds_tmpl + "_" + sta_nam + ")") in line ):
              fl_proc = False

     print ( "IIII ", ind_setmode_beg, ind_setmode_end, len(setmode_buf) ) # %%%%%%%%%

     buf = out
     out = []
     i = -1
     for line in buf:
         i = i + 1
         out.append ( line )
         if ( i == ind_setmode_end ):
              out.append ( '        "' )
              out.append ( '        "=================================' )
              out.append ( '        "' )
              for lin in setmode_buf:
                  out.append ( "        " + lin )
              
#
# --- Write down output file
#
f=open(vex_out,"w")
for line in out:
    print ( line, file=f )
f.close()

print ( "fuse_vex_prc.py: Generated the output vex file %s for station %s" % \
         ( vex_out, sta_nam )  )    
