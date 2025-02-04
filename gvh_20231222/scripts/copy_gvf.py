#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program copy_gvf.py copies a gvf file within OBS resository.       *
# *                                                                      *
# *  ### 11-FEB-2022  copy_gvf.py  v1.0 (c)  L. Petrov  11-FEB-2022 ###  *
# *                                                                      *
# ************************************************************************
import           pwd, sys, os, re, shutil, time, subprocess, datetime

if ( len(sys.argv)-1 < 2 ):
     print ( "Usage: copy_gvf db_from db_to" )
     exit  ( 1 )
else:
     db_from = sys.argv[1]
     db_to   = sys.argv[2]

if ( len(db_to) != 10 ):
     print ( "db_to argument should have length 10 characeters" )
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
# --- Form envelop file from
#
env_from_name = None
if ( len(db_from) == 19 ):
     db_name_try = env_dir + "/" + db_from
     if ( os.path.isfile(db_name_try) ):
          env_from_name = db_name_try
elif ( len(db_from) == 15 ):
     db_name_try = env_dir + "/" + db_from + ".env"
     if ( os.path.isfile(db_name_try) ):
          env_from_name = db_name_try
else:
     for i in range(1,9):
         db_name_try = env_dir + "/" + db_from + "_v%03d.env" % i
         if ( os.path.isfile(db_name_try) ):
              env_from_name = db_name_try

if ( not env_from_name ):
     print ( "Did not find files for the database ", db_from )

id = env_from_name.rfind("/")
#
# --- Read the envelop file from
#
env_from = []
with open(env_from_name,encoding="latin") as f:
     for line in f:
         env_from.append ( line.strip("\n").strip("\r") )
f.close()

#
# --- Generate contents of env_to envelop file
# --- and copy db files
#
env_to = []
for line in env_from:
    db_from_file = db_dir + "/" + \
                   env_from_name[id+1:id+11] + "_" + \
                   line.split()[6] + "_"   + \
                   line.split()[2] + "_v"  + \
                   line.split()[3] + ".bgv" 

    if ( not os.path.isfile ( db_from_file ) ):
         print ( "Error: file %s is not found" % db_from_file )
         exit ( 1 )

    db_to_file = db_dir + "/"    +         \
                 db_to[0:10]    + "_"   + \
                 line.split()[6] + "_"   + \
                 line.split()[2] + "_v"  + \
                 line.split()[3] + ".bgv" 

    env_to.append ( line[0:20] + db_to[9:10] + line[21:] )
#
# --- Copy db dile
#
    shutil.copy ( db_from_file, db_to_file )

#
# --- Write out output envelop file
#
env_to_name = env_from_name.replace( "/"+db_from[0:10]+"_"  , "/"+db_to[0:10]+"_")
f=open(env_to_name,"w")
for line in env_to:
    print ( line, file=f )
f.close()
