#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program del_gvf.py deletes a gvf file within OBS repsository.      *
# *                                                                      *
# *   Usage: del_gvf db_name [execute]                                   *
# *                                                                      *
# *   without option execute del_gvf.py only shows commands for removing *
# *   files.                                                             *
# *                                                                      *
# *                                                                      *
# *  ### 22-FEB-2022  del_gvf.py  v1.1 (c)  L. Petrov  24-JUN-2022 ###   *
# *                                                                      *
# ************************************************************************
import           pwd, sys, os, re, shutil, time, subprocess, datetime

fl_execute = False
if ( len(sys.argv)-1 < 1 ):
     print ( "Usage: del_gvf db_name [execute]" )
     exit  ( 1 )
else:
     db_name = sys.argv[1]
     if ( len(sys.argv)-1 == 2 ):
          if ( sys.argv[2] == "execute" ): 
               fl_execute = True
          else:
               print ( "Unsupported flag ", sys.argv[2] )
               exit  ( 1 )

if ( len(db_name) != 10 ):
     print ( "db_name argument should have length 10 characeters" )
     exit  ( 1 )

if ( "VCAT_CONF" in os.environ ):
     vcat_conf = os.environ["VCAT_CONF"]
else:
     print ( "Environment variable VCAT_CONF is not defined" )

#
# --- Read vcat file
#
vcat = []
with open(vcat_conf,encoding="latin") as f:
     for line in f:
         vcat.append ( line.strip("\n").strip("\r") )
f.close()

#
# --- Parse vcat file and find repository OBS
#
db_dir = None
env_dir = None
for line in vcat:
    if ( len(line.split()) == 3 ):
         if ( line.split()[0] == "GVF_DB_DIR:" and line.split()[1] == "OBS" ):
              db_dir = line.split()[2]
         if ( line.split()[0] == "GVF_ENV_DIR:" and line.split()[1] == "OBS" ):
              env_dir = line.split()[2]

if ( not db_dir or not env_dir ):
     print ( "Did not find GVF DB_DIR and/or GV ENV_DIR" )
     print ( "Please check you %s file " % vcat_conf )
     exit  ( 1 )

#
# --- Form envelop file
#
env_file_name = None
env_files = []
for i in range(1,9):
    db_name_try = env_dir + "/" + db_name + "_v%03d.env" % i
    if ( os.path.isfile(db_name_try) ):
         env_file_name = db_name_try
         env_files.append ( env_file_name )

if ( not env_file_name ):
     print ( "Did not find files for the database ", db_name )

id = env_file_name.rfind("/")
#
# --- Read the envelop file 
#
env_buf = []
with open(env_file_name,encoding="latin") as f:
     for line in f:
         env_buf.append ( line.strip("\n").strip("\r") )
f.close()

#
# --- Generate the list of db_files
#
db_files = []
for line in env_buf:
    db_file = db_dir + "/" + \
                   env_file_name[id+1:id+11] + "_" + \
                   line.split()[6] + "_"   + \
                   line.split()[2] + "_v"  + \
                   "%03d" % int(line.split()[3]) + ".bgv" 

    if ( not os.path.isfile ( db_file ) ):
         print ( "Error: file %s is not found" % db_file )
         exit ( 1 )
    db_files.append ( db_file )

#
# --- Either remove files or generate commands for removal
#
if ( not fl_execute ):
     print ( "# Examine files to be removed for database " + db_name )

for file in env_files:
    if ( fl_execute ):
         print ( "Removing " + file )
         os.remove ( file )
    else:
         print ( "rm " + file )

for file in db_files:
    if ( fl_execute ):
         print ( "Removing " + file )
         os.remove ( file )
    else:
         print ( "rm " + file )
