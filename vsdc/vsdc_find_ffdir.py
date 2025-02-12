#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Auxilliary program vsdc_find_ffdir.py print name of the Fourfit    *
# *   subditectory if it finds it. It determines the directory was       *
# *   generated by Fourfit if it either has alist file or its has files  *
# *   with .. patter inside filenames.                                   *
# *                                                                      *
# * ## 02-JUN-2021 vsdc_find_ffdir.py v1.3 (c) L. Petrov 18-DEC-2021 ### *
# *                                                                      *
# ************************************************************************
import sys, os

if ( len(sys.argv)-1 < 1 ):
     print ( "Usage: vsdc_find_ffdir.py  dir [verb]" )
     exit  ( 1 )
else:
     dir = sys.argv[1]
     if ( len(sys.argv)-1 > 1 ):
          ivrb = int(sys.argv[2])
     else:
          ivrb = 0

dir_alist_list     = []
dir_parent_dd_list = []
dir_difx_list      = []
dir_input_list     = []

#
# --- Walk over all files in the directory to be tarred
#
for path, dirs, files in os.walk(dir):
    for file in files:
        if ( file == "alist.out" ):
#
# ---------- We found alist.out file! Let us put it in dir_alist list
#
             if ( not path in dir_alist_list ):
                  dir_alist_list.append ( path )
        if ( ".." in file ):
             ib = file.find("..")
             ip = path.rfind("/")
             if ( ip > 1 and ( ( ib == 1 and len(file) ==  9 ) or \
                               ( ib == 2 and len(file) == 10 )    ) ):
#
# --------------- We found a file that contains ".." inside its name
#
                  if ( not path[0:ip] in dir_parent_dd_list ):
                       dir_parent_dd_list.append ( path[0:ip] )
        if ( ".difx" in file and "_" in file ):
#
# ---------- We found a file with .difx
#
             if ( not path in dir_difx_list ):
                  dir_difx_list.append ( path )
        if ( ".input" in file and "_" in file ):
#
# ---------- We found a file with .input
#
             if ( not path in dir_input_list ):
                  dir_input_list.append ( path )

if ( ivrb > 0 ):
     print ( "dir_alist_list=     ", dir_alist_list     )
     print ( "dir_parent_dd_list= ", dir_parent_dd_list )
     print ( "dir_input_list=     ", dir_input_list     )
     print ( "dir_difx_list=      ", dir_difx_list      )


if ( len(dir_alist_list) > 0 or len(dir_parent_dd_list) > 0 ):
#
# -- Preprare the output. Keep in mind: more than one alist_dir or parent_dd 
# -- directories may be found.
#
     dir_list = []
     for dir in dir_alist_list:
         if ( not dir in dir_list ):
              dir_list.append ( dir )

     for dir in dir_parent_dd_list:
         if ( not dir in dir_list ):
              dir_list.append ( dir )

     for dir in dir_difx_list:
         if ( not dir in dir_list ):
              dir_list.append ( dir )

     out_list = []
     for dir_check in dir_list:
         found_in_difx_dir = False
         found_in_input_dir = False
         for difx_dir in dir_difx_list:
             if ( dir_check == difx_dir ):
                  found_in_dirx_dir = True
         for input_dir in dir_input_list:
             if ( dir_check == input_dir ):
                  found_in_input_dir = True

         if ( not found_in_difx_dir and not found_in_input_dir ):
#
# --------------- We consider only those alist files that are located
# --------------- in directories that have a subdirectory with
# --------------- files with ".." inside their names
#
                  out_str = "/" + dir_check.replace(dir+"/","").replace(dir,"") + "/"
                  if ( not out_str in out_list ):
                       out_list.append ( out_str )

     print (  " ".join(out_list) )
     exit  ( 0 )
