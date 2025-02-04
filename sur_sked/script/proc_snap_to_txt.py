#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *  Program proc_snap_to_txt.py attaches proc and snap files to the     *
# *  bottom of the VLBI experiment desrription file with extension .txt  *
# *                                                                      *
# * ## 15-DEC-2021 proc_snap_to_txt.py v1.0 (c) L. Petrov 15-DEC-2021 ## *
# *                                                                      *
# ************************************************************************
"""
import signal, sys, os, pwd, math
from   sur_sked_config import * # Import sur_sked confuguration
from   pet_misc        import *

pst__label   = "txt_proc_snap_to.py"
pst__version = "2021.12.15"

def main():
   if ( len(sys.argv)-1 < 2 ):
        print ( "Usage: proc_snap_to_txt.py txt_file [proc_file snap_file ...]" )
        exit ( 1 )

#
# --- Experiment description file is the first argument
#
   txt_file = sys.argv[1]

#
# --- Extract snap and proc file name arguments
#
   snap_file_list = []
   proc_file_list = []
   for i in range(2,len(sys.argv)):
       if ( len(sys.argv[i]) < 6 ): continue
       if ( ".snp" == sys.argv[i][-4:] ):
            snap_file_list.append ( sys.argv[i] )
       if ( ".prc" == sys.argv[i][-4:] ):
            proc_file_list.append ( sys.argv[i] )

   proc_file_list.sort()
   snap_file_list.sort()

   ib = snap_file_list[0].rfind("/") + 1
   if ( ib < 1 ):
        exp_name = snap_file_list[0][0:-6]
   else:
        exp_name = snap_file_list[0][ib:-6]

   if ( len(proc_file_list) != len(snap_file_list) ):
        print ( "The number of proc and snap file is different" )
        exit  ( 1 )

   for snap_file in snap_file_list:
       sta_nam = snap_file[-6:-4]
       fl_found = False
       for proc_file in proc_file_list:
           proc_sta_nam = proc_file[-6:-4] 
           if ( proc_sta_nam == sta_nam ): 
               fl_found = True
       if ( not fl_found ):
            print ( "No proc file for station %s is specified" % sta_nam )
            exit  ( 1 )

   with open(txt_file,encoding="latin") as f:
      txt = f.read().splitlines()
   f.close()

   out = []
   fl_snap = False
   fl_proc = False
   for line in txt:
       if ( "<preprocessor_name"  in line ): 
            continue
       if ( "</preprocessor_name" in line ): 
            continue
       if ( "<preprocessor_version"  in line ): 
            continue
       if ( "</preprocessor_version" in line ): 
            continue
       if ( "<exp_name"  in line ): 
            continue
       if ( "</exp_name" in line ): 
            continue
       if ( "</snap_commands" in line ): 
            fl_snap = False
            continue
       if ( "<snap_commands" in line ): 
            fl_snap = True
       if ( "</proc_commands" in line ): 
            fl_proc = False
            continue
       if ( "<proc_commands" in line ): 
            fl_proc = True
       if ( fl_proc or fl_snap ):
            continue
       out.append ( line )

   out.append ( "<preprocessor_name  %s>"    % pst__label )
   out.append ( "</preprocessor_name %s>"    % pst__label )
   out.append ( "<preprocessor_version  %s>"  % pst__version )
   out.append ( "</preprocessor_version %s>" % pst__version )
   out.append ( "<exp_name  %s>" % exp_name )
   out.append ( "</exp_name %s>" % exp_name )

   sta_nam_list = []
   for snap_file in snap_file_list:
       sta_nam = snap_file[-6:-4]
       sta_nam_list.append ( sta_nam )
       out.append( "<snap_commands %s>" % sta_nam )
       with open(snap_file,encoding="latin") as f:
            snap = f.read().splitlines()
       f.close()
       for line in snap:
           out.append ( "   <snap>%s</snap>" % line )
       out.append( "</snap_commands %s>" % sta_nam )
       
   for proc_file in proc_file_list:
       sta_nam = proc_file[-6:-4]
       out.append( "<proc_commands %s>" % sta_nam )
       with open(proc_file,encoding="latin") as f:
            proc = f.read().splitlines()
       f.close()
       for line in proc:
           out.append( "   <proc>%s</proc>" % line )
       out.append( "</proc_commands %s>" % sta_nam )
       
   f=open(txt_file+"__","w")
   for line in out:
       print ( line, file=f )
   f.close()
   os.rename ( txt_file+"__", txt_file )

   print ( "Snap and proc files for stations %s are included in %s" % \
           ( " ".join(sta_nam_list), txt_file ) )

if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        main()
    except KeyboardInterrupt:
        print ( "%s: Interrupted" % argv[0] )
        exit ( 1 )
