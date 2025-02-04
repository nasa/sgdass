#!/usr/bin/env python3
import pwd, sys, os, re, shutil, time, subprocess, datetime
from   sur_sked_config import * # Import sur_sked confuguration
from   pet_misc        import *

fuse_vex_frq__label = "fuse_veq_frq 20240224"

if ( len(sys.argv)-1 < 5 ):
     print ( "Usage: fuse_vex_frq proto hds frq_vex frq_def vex [ind]"  )
     exit  ( 1 )
else:
     fil_proto    = sys.argv[1]
     hds_str      = sys.argv[2]
     fil_frq_vex  = sys.argv[3]
     fil_frq_def  = sys.argv[4]
     fil_vex      = sys.argv[5]
     if ( len(sys.argv)-1 >= 6 ):
#
# ------- A user specified which bbc definition to process
#
          hds_ind = int(sys.argv[6])
     else:
#
# ------- To process all BBC definitions
#
           hds_ind = 1

proto   = read_file ( fil_proto   )
frq_vex = read_file ( fil_frq_vex )
frq_def = read_file ( fil_frq_def )
hds_pattern = "@HDS%d@" % hds_ind

if ( not proto ):
     print ( "Cannot find file ", fil_proto )
     exit  ( 1 )

if ( not frq_vex ):
     print ( "Cannot find file ", fil_frq_vex )
     exit  ( 1 )

if ( not frq_def ):
     print ( "Cannot find file ", fil_frq_def )
     exit  ( 1 )

lo={}
for line in frq_def:
    if ( len(line) == 0 ): continue
    if ( line.split()[0] == "MOD_NAME" ):
         mode = line.split()[1] 
    if ( line.split()[0] == "BAND" ):
         bnd = line.split()[1] 
         frq_val  = float(line.split()[2])
         bdw_val  = float(line.split()[3])
         if ( bnd in lo.keys() ):
              lo[bnd]["frq_min"] = min ( lo[bnd]["frq_min"], frq_val           )
              lo[bnd]["frq_max"] = max ( lo[bnd]["frq_max"], frq_val + bdw_val )
         else:
              lo[bnd] = {"frq_min": frq_val, "frq_max": frq_val + bdw_val, "lo_offs": 0.0 }
    if ( line.split()[0] == "LO_OFFSET" ):
         bnd = line.split()[1] 
         lo_offs = float(line.split()[2])
         lo[bnd]["lo_offs"] = lo_offs 

hds = "??"
for line in frq_vex:
    if ( "Hardware setup:" in line ):
         hds = line.split()[3]

out = []
fl_lo      = False
fl_lo_def  = False
fl_bbc     = False
fl_bbc_def = False
fl_frq     = False
fl_frq_def = False
fl_prov    = False
fl_mode    = False
fl_freq_done = False
mode_name  = None
mode_range = ()
bbc_def_ind = 0
lo_def_ind  = 0
prev_line = ""
fl_mode_was_already_defined = False
i=-1
for line in proto:
    i = i  + 1
    if ( "$" in line[0:1] ):
         if ( fl_mode ):
#
# ----------- Replcicate mode block. We increment x in __@MODx@__
#
              if ( len(mode_name) == 10 ):
                   mode_id     = mode_name[6:7]
                   new_mode_id = chr ( ord(mode_id) + 1 )
                   new_mode_name = mode_name.replace(mode_id,new_mode_id)
                   for j in range(mode_range[0],mode_range[1]+1):
                       out.append ( proto[j].replace(mode_name,new_mode_name) )
                   out.append ( '*' )
             
         fl_freq     = False
         fl_freq_def = False
         fl_bbc      = False
         fl_lo       = False
         fl_lo_def   = False
         fl_mode     = False

    if ( "$BBC;" in line[0:5] ):
         fl_bbc     = True
         fl_bbc_def = False

    if ( "$MODE;" in line[0:6] ):
         fl_mode     = True

    if ( "$IF;" in line[0:4] ):
         fl_lo = True

    if ( "$FREQ;" in line[0:6] ):
         fl_frq     = True
         fl_frq_def = False

    if ( " def " in line and fl_bbc ):
#
# ------ bbc definition counter
#
         bbc_def_ind = bbc_def_ind + 1
    if ( " def " in line and fl_lo ):
#
# ------ if definition counter
#
         if ( hds_str in line ):
              fl_lo_def = True
              lo_def_ind = lo_def_ind + 1
         else:
              fl_lo_def = False

    if ( " def " in line and fl_mode ):
         mode_name = line.split()[1].replace(";","")
         if ( mode_name == mode ):
              fl_mode_was_already_defined = True
         mode_range = (i,i)

    if ( line[0:9] == "  enddef;" and fl_mode ):
         mode_range = (mode_range[0],i)

    if ( " def " in line and fl_frq ):
         if ( hds_str in line ): fl_frq_def = True


    if ( "BBC_assign = &BBC" in line  and \
         fl_bbc_def                   and \
         ( bbc_def_ind == hds_ind or hds_ind == 0 ) ):
#
# ------ Get bbc ID in vex
#
         bbc_vex_id = line.split()[2]
         bbc_found = False
         for lin in frq_vex:
             if ( len(lin.split()) >= 14 ):
#
# --------------- Get bbc ID in the frequency file
#
                  bbc_frq_id = lin.split()[14]
                  if ( bbc_frq_id == bbc_vex_id ):
                       bbc_found = True
         if ( not bbc_found ): continue

    if ( len(line.split()) > 1 ):
         if ( line.split()[0] == "def" and "__@OMN@__" in line.split()[1] ):
              next_line = proto[i+1]
              if ( "@@_freq_" in next_line   and \
                    fl_frq                   and \
                    fl_frq_def               and \
                    hds_pattern in next_line     ):
                    line = line.replace ( "__@OMN@__", mode )
                    
    if ( "@@_freq_" in line   and \
          fl_frq              and \
          fl_frq_def          and \
          hds_pattern in line and \
          not fl_freq_done        ):
#
          for lin in frq_vex:
              out.append ( lin )
          fl_freq_done = True
#
          out.append ( "  enddef;                        ** %s" % hds_pattern )
          out.append ( '  def __@OMN@_____@HDS%d@___freq; ** @HDS%d@' % ( hds_ind, hds_ind ) )
          out.append ( '  @@_freq___@HDS%d@__             ** @HDS%d@' % ( hds_ind, hds_ind ) )
          continue

    if ( "@_LO_@" in line and fl_lo and fl_lo_def ):
          bnd = line.split()[2].replace("&IF_","" )
          if ( bnd in lo.keys() ):
               lo_val = lo[bnd]["lo_offs"] + lo[bnd]["frq_min"]
               line = line.replace("@_LO_@", "%6.1f" % lo_val )

    if ( "$FREQ" in line     and \
         "__@OMN@__" in line and \
         "__@" in mode_name  and \
         hds_pattern in line     ):
         out.append ( line.replace ( "__@OMN@__", mode ) )

    if ( "extension" in line and \
         "setmode_"  in line and \
         "__@OMN@__" in line     ):
         out.append ( line.replace ( "__@OMN@__", mode ) )
            
    if ( line[0:1] != "*" and " def" in line and "PROVENANCE" in line ):
         fl_prov = True
    if ( fl_prov and line[0:1] != "*" and " enddef" in line ):
         out.append ( "      extension = NASA : fuse_vex_frq.py    : %s : %s : %s ;" % \
                      ( fil_frq_vex, \
                      fuse_vex_frq__label.split()[-1], \
                      datetime.datetime.strftime( datetime.datetime.now(), "%Y.%m.%d_%Hh%Mm%Ss") ) \
                    )
         fl_prov = False
    prev_line = line
    out.append ( line )

#
# --- replace mode name placehodeler with actual mode name
#
if ( not fl_mode_was_already_defined ):
     buf = out
     out = []
     for line in buf:
         line = line.replace(mode_name,mode)
         out.append ( line )
    

(ret,err)=write_file ( out, fil_vex )
check_err_exe ( ret, err, "write_file" )

print ( "Created file ", fil_vex )    
