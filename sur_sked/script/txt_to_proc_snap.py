#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *  Program txt_to_proc_snap_to.py extracts two schedule files for      *
# *  the specfied station fro the VLBI experiment description file with  *
# *  extension .txt . The first file is in SNAP format with extention    *
# *  .snp . The second file is in proc format with extension .prc .      *
# *                                                                      *
# * ## 15-DEC-2021 txt_proc_snap_to.py v1.0 (c) L. Petrov 15-DEC-2021 ## *
# *                                                                      *
# ************************************************************************
"""
import signal, sys, os, pwd, math
from   sur_sked_config import * # Import sur_sked confuguration
from   pet_misc        import *

output_dir = "/tmp"

tsp__label   = "txt_proc_snap_to.py"
tsp__version = "2021.12.15"

def main():
   if ( len(sys.argv)-1 < 2 ):
        print ( "Usage: txt_proc_snap_to.py file|url station" )
        exit ( 1 )

   txt_file = sys.argv[1]
   sta_nam  = sys.argv[2]

#
# --- Read txt VLBI schedule description file
#
   with open(txt_file,encoding="latin") as f:
      txt = f.read().splitlines()
   f.close()

#
# --- Scan the VLBI schedule description file
#
   exp_name = "??"
   pp_name  = "??"
   proc = []
   snap = []
   get_snap = False
   get_proc = False
   for line in txt:
#
# ---- Extract the preprocessor name
#
       if ( len(line) < 2 ): continue
       if ( line.split()[0] == "<preprocessor_name" ):
            pp_name = line.split()[1].replace(">","").strip()
#
# ---- Extract expriment name
#
       if ( line.split()[0] == "<exp_name" ):
            exp_name = line.split()[1].replace(">","").strip()
#
# ---- Check whether snap commands correpsond to the requested station
#
       if ( line.split()[0] ==  "<snap_commands" ):
            snap_sta_name = line.split()[1].replace(">","").strip()
            if ( snap_sta_name == sta_nam ):
                 get_snap = True
                 continue
       if ( line.split()[0] ==  "</snap_commands" ):
            get_snap = False
            continue
       if ( get_snap ):
#
# --------- Extact the snap command and put it in snap list
#
            snap.append ( line.ljust(8192).strip().replace("<snap>","").replace("</snap>","") )

#
# ---- Check whether proc commands correpsond to the requested station
#
       if ( line.split()[0] ==  "<proc_commands" ):
            proc_sta_name = line.split()[1].replace(">","").strip()
            if ( proc_sta_name == sta_nam ):
                 get_proc = True
                 continue
       if ( line.split()[0] ==  "</proc_commands" ):
            get_proc = False
            continue
       if ( get_proc ):
#
# --------- Extact the proc command and put it in snap list
#
            proc.append ( line.ljust(8192).strip().replace("<proc>","").replace("</proc>","") )

   if ( not pp_name ):
        print ( "Not snap or proc files were put in file %s" % txt_file )
        exit  ( 1 )

   if ( not exp_name ):
        print ( "No experiment name has been found in file %s" % txt_file )
        exit  ( 1 )

   if ( len(snap) == 0 ):
        print ( "Snap commands for station %s were not found in file %s" % \
                ( sta_nam, txt_file ) )
        exit  ( 1 )

   if ( len(proc) == 0 ):
        print ( "Proc commands for station %s were not found in file %s" % \
                ( sta_nam, txt_file ) )
        exit  ( 1 )
   
#
# --- Write down snap file
#
   snap_file = output_dir + "/" + exp_name + sta_nam + ".snp"

   f=open(snap_file,"w")
   for line in snap:
       print ( line, file=f )
   f.close()

#
# --- Write down proc file
#
   proc_file = output_dir + "/" + exp_name + sta_nam + ".prc"

   f=open(proc_file,"w")
   for line in proc:
       print ( line, file=f )
   f.close()

   print ( "Snap and proc files are written: %s %s" % ( snap_file, proc_file ) )

if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        main()
    except KeyboardInterrupt:
        print ( "%s: Interrupted" % argv[0] )
        exit ( 1 )
