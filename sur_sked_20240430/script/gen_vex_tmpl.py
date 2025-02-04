#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program  gen_vex_tmpl.py
# *                                                                      *
# *   Example:                                                           *
# *   gen_vex_tmpl.py -e y4045a \                                        *
# *                   -s gs,nn,yj \                                      *
# *                   -m h01:m01,m25+h02:m02,m31,m32 \                   *  
# *                   -i /vlbi/y2/y2_vex.proto \                         *
# *                   -o /vlbi/y4045a/y4045a_vex.tmpl                    *
# *                                                                      *
# * ### 06-FEB-2024 gen_vex_tmpl.py  v 1.0 (c) L. Petrov 24-FEB-2024 ### *
# *                                                                      *
# ************************************************************************
import argparse, signal, sys, os, pwd, math
from   datetime import datetime, timedelta, tzinfo
from   sur_sked_config import * # Import sur_sked confuguration
from   pet_misc        import *

gvt__label   = "gen_vex_tmpl.py 20240224"

seq_dir  = sur_sked_seq
stp_dir  = sur_sked_stp
prc_dir  = sur_sked_prc
exp_dir  = sur_sked_exp
hds_max  = 9

#
# ------------------------------------------------------------------------
#
def gen_vex ( exp, sta_dict, hds_dict, vex_in, vex_out, ivrb ):
    """
    Routine gen_vex
    """
    mode_list = []
    for hds in hds_dict:
        for mode in hds_dict[hds]:
            if ( not mode in mode_list ):
                 mode_list.append ( mode )

    for mode_dict in mode_list:
        com = "gen_seq_prc.py" + " -s " + mode_dict["seq_file"] + " -o " + exp_dir + "/" + exp
        if ( ivrb > 1 ):
             print ( "gen_vex_tmpl-01: about to execute command ", com )

        (ret,err) = exe ( com )
        if ( ret != 0 ):
             for line in err:
                 print ( line )
             print ( "gen_vex_tmpl ERROR in execution command ", com )
             exit ( 1 )
        if ( ivrb > 0 ):
             print ( "gen_vex_tmpl: generated .prc and .frq sequence %s" % mode )
         
    if ( vex_out != vex_in ):
         com = "cp " + vex_in + " " + vex_out
         if ( ivrb > 1 ):
              print ( "gen_vex_tmpl-01: about to execute command ", com )
         (ret,err) = exe ( com )
         if ( ret != 0 ):
              for line in err:
                  print ( line )
              print ( "gen_vex_tmpl ERROR in execution command ", com )
              exit ( 1 )

    hds_ind = 0
    for hds in hds_dict:
        hds_ind = hds_ind + 1
        for mode_dict in hds_dict[hds]:
            frq_file = exp_dir  + "/" + exp + "/" + hds + "_" + mode_dict["mode_name"] + "_vex.frq" 
            com = "fuse_vex_frq.py " + " " + \
                                     vex_out + " " + \
                                     "@HDS%d@" % hds_ind + " " + \
                                     frq_file + " " + \
                                     mode_dict["seq_file"] + " " + \
                                     vex_out + " %d" % hds_ind
                                       
            if ( ivrb > 1 ):
                 print ( "gen_vex_tmpl-02: about to execute command ", com )

            (ret,err) = exe ( com )
            if ( ret != 0 ):
                 for line in err:
                     print ( line )
                 print ( "gen_vex_tmpl ERROR in execution command ", com )
                 exit ( 1 )
            if ( ivrb > 0 ):
                 print ( "gen_vex_tmpl: fused definitions for mode %s" % mode["mode_name"] )

            for sta in sta_dict.keys():
                if ( sta in mode_dict["sta_short_list"]):
                     prc_sta_file = exp_dir + "/" + exp + "/" + mode_dict["mode_name"] + "_" + sta + ".prc"
                     com = "fuse_vex_prc.py" + " " + \
                                              vex_out + " " + \
                                              prc_sta_file + " " + \
                                              vex_out + " " + \
                                              "%d" % hds_ind + " " + \
                                              exp
                     if ( ivrb > 1 ):
                          print ( "gen_vex_tmpl-03: about to execute command ", com )

                     (ret,err) = exe ( com )
                     if ( ret != 0 ):
                          for line in err:
                              print ( line )
                          print ( "gen_vex_tmpl ERROR in execution command ", com )
                          exit ( 1 )
                     if ( ivrb > 0 ):
                          print ( "gen_vex_tmpl: fused definitions for sequence %s into station file for %s" % \
                                  ( mode["seq_name"], sta )  )
    
    if ( ivrb > 0 ):
         print ( "gen_vex_tmpl: sanitizing vex template for unused hardware modes" )
          
    buf = read_file ( vex_out )
    hds_name_list = list(hds_dict.keys())

    for i in range(1,9):
        hds_extended_pattern = "__@HDS%d@__" % i
        hds_pattern          = "@HDS%d@" % i
        out = []
        if ( i <= len(hds_dict.keys()) ):
             j = -1
             for line in buf:
                 j = j + 1
                 if ( "@@_freq_" + hds_extended_pattern in line ):
                       line = line.replace(hds_extended_pattern,"@OOO@")
                       buf[j+1] = "@OOO@"
                 line = line.replace ( 'def __@OMN@___', '@OOO@' )
                 if ( "@OOO@" in line ): continue
                 if ( '__@OMN@__' in line ):
                      continue
                 line = line.replace ( hds_extended_pattern, list(hds_dict.keys())[i-1] )
                 line = line.replace ( hds_pattern,          list(hds_dict.keys())[i-1] )
                 line = line.replace ( "** " + list(hds_dict.keys())[i-1],  "" )
                 line = line.replace ( "__@DUR@__", "0" )
                 out.append ( line )                      
#                 print ( "Line ", line ) # %%%%%%%%%%%%%%%%%%%%%%
        else:
             j = -1
             for line in buf:
                 j = j + 1
                 if ( hds_extended_pattern in line and j < len(buf)-1 ):
                      if ( "enddef;" in buf[j+1] ):
                            buf[j+1] = buf[j+1] + " * " + hds_extended_pattern 
                      if ( "def FS_PROC_DUR_NAMES_" in line ):
                            for k in range(j+1,len(buf)-1):
                                buf[k] = buf[k] + " * " + hds_extended_pattern 
                                if ( "enddef;" in buf[k]  ):
                                     break
                                
                 if ( hds_pattern in line ): 
                      continue
                 out.append ( line )                      
#                 print ( "LINE ", line ) # %%%%%%%%%%%%%%%%%%%%%%
        buf = out

#
# --- Remove a mode block with __$MOD name and unused @@_schedule_convert_@@ commands
#

    out = []
    fl_mode_rem = False
    for line in buf:
#        print ( "LINE: ", line ) # %%%%%%%%%%%%%
        if ( line[0:5] == "  def" and "__@MOD" in line ):
             fl_mode_rem = True
        if ( "@@_schedule_convert_@@" in line ):
              continue

        if ( not fl_mode_rem ):
             out.append ( line )                      
             
        if ( line[0:9] == "  enddef;" and "__@MOD" in line ):
             fl_mode_rem = False


    (ret,err) = write_file ( out, vex_out )
    if ( ret != 0 ):
         for line in err:
             print ( line )
         print ( "gen_vex_tmpl ERROR in writinig the final vex template to file %s" % vex_out )
         exit ( 1 )            
    if ( ivrb > 0 ):
         print ( "Genrated vex template file %s" % vex_out )

    return 0

#
# ------------------------------------------------------------------------
#
def main():
    """ 
    Main program of the gen_vex_tmpl utility
    """

    parser = argparse.ArgumentParser( description=gvt__label )

    parser.add_argument ( "-e", "--exepriment", \
                          action="store", \
                          dest="exp", \
                          default=0, \
                          metavar="value", \
                          help="Experiment name" )

    parser.add_argument ( "-s", "--station_list", \
                          action="store", \
                          dest="stas", \
                          default=0, \
                          metavar="value", \
                          help="Comma separated station list" )

    parser.add_argument ( "-m", "--hardware_mode", \
                          action="store", \
                          dest="hds_mode", \
                          default=0, \
                          metavar="value", \
                          help="Hardware setup mode" )

    parser.add_argument ( "-i", "--input_vex", \
                          action="store", \
                          dest="vex_in", \
                          default=0, \
                          metavar="value", \
                          help="Input vex template file" )


    parser.add_argument ( "-o", "--ouput_vex", \
                          action="store", \
                          dest="vex_out", \
                          default=0, \
                          metavar="value", \
                          help="Output vex template file" )

    parser.add_argument ( "-v", "--verbosity", \
                          action="store", \
                          dest="verb", \
                          default=0, \
                          metavar="value", \
                          type=int, \
                          help="Verbosity level" )

#
# --- Get and parse options
#
    args = parser.parse_args()

    if ( not args.exp ):
         print ( "Experiment name is not specified" )
         exit ( 1 )

    if ( not args.stas ):
         print ( "Station list is not specified" )
         exit ( 1 )

    if ( not args.hds_mode ):
         print ( "Hardware setup was not specified" )
         exit ( 1 )

    sta_dict = {}
    for word in args.stas.split(","):
        sta_dict[word.lower()] = {}
        sta_dict[word.lower()]["long_name"] = ""
        sta_dict[word.lower()]["prc_file"]  = ""

    hds_dict= {}
    for hds in args.hds_mode.split("+"):
        hds_name = hds.split(":")[0]
        hds_dict[hds_name] = []
        for word in hds.split(","):
            seq_name = word.replace(hds_name+":","")
            seq_file = seq_dir + "/" + seq_name + ".seq" 
            sta_short_list = []
            sta_long_list  = []
            if ( not os.path.isfile(seq_file) ):
                 print ( "gen_vex_tmpl ERROR: sequence file %s does not exist" % seq_file )
                 exit  ( 1 )
            buf = read_file ( seq_file )
            for line in buf:
                if ( line[0:1] == "#" ): 
                     continue
                if ( line[0:8] == "MOD_NAME" ):
                     mode_name = line.split()[1]
                if ( line[0:7] == "STATION" ):
                     sta_long  = line.split()[1]
                     sta_short = line.split()[2]
                     if ( not sta_long in sta_long_list ):
                          sta_long_list.append ( sta_long.upper() )
                     if ( not sta_short in sta_short_list ):
                          sta_short_list.append ( sta_short.lower() )
                                 
            buf = read_file ( seq_file )
            mode = {"mode_name": mode_name,           \
                    "seq_name":  seq_name,            \
                    "seq_file": seq_file,             \
                    "sta_short_list": sta_short_list, \
                    "sta_long_list":  sta_long_list   \
                   }
            hds_dict[hds_name].append ( mode )

    if ( not args.vex_in ):
         print ( "Input vex template file is not specified" )
         exit ( 1 )
 
    if ( not args.vex_out ):
         print ( "Output vex template file is not specified" )
         exit ( 1 )

#
# --- Walk over the stp directory file.
#
    for path, dirs, files in os.walk(stp_dir):
        for file in files:
            if ( "#" in file ): continue
            if ( "~" in file ): continue
            stp_file = path + "/" + file
            if ( not os.path.isfile ( stp_file ) ) : continue
            buf = read_file ( stp_file ) 
            for line in buf:
                if ( len(line.split()) == 0 ): continue
                if ( len(line.split()) >= 4 ):
                     if ( line.split()[0] == "SHORT_NAME:" ):
                          short_name = line.split()[3].lower()
                          long_name  = line.split()[1].upper()
                          if ( short_name in sta_dict.keys() ):
                               sta_dict[short_name]["long_name"] = long_name

    for sta in sta_dict.keys():
        prc_file = prc_dir + "/" + sta + "_template.prc"
        if ( not os.path.isfile ( prc_file ) ): 
             print ( "gen_vex_tmpl: no template procedure file for stations %s was found in %s" % \
                     ( sta, prc_dir ) )
             exit ( 1 )
        sta_dict[sta]["prc_file"] = prc_file 

    if ( args.verb > 1 ):
         print ( "exp=      ", args.exp ) # %%%
         print ( "sta_list= ", sta_dict ) # %%%
         print ( "hds_dict= ", hds_dict ) # %%%
         print ( "vex_in=   ", args.vex_in   ) # %%%
         print ( "vex_out=  ", args.vex_out  ) # %%%

    gen_vex ( args.exp, sta_dict, hds_dict, args.vex_in, args.vex_out, args.verb )

if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        main()
    except KeyboardInterrupt:
        print ( "pf.py: Interrupted" )
        exit ( 1 )
