#!/usr/bin/env python3
import           pwd, sys, os, re, shutil, time, subprocess, datetime, cmd
sys.path.append('/home/lpetrov/bin')
from   pet_misc  import *

dir_gvf_env     = "/vlbi/gvf/env"
dir_gvf_db      = "/vlbi/gvf/db"
dir_gvf_new_env = "/vlbi/gvf/new_env"
com_file        = "/tmp/com_file.txt"

iverb       = 0

if ( not os.path.isdir(dir_gvf_env) ):
     print ( "Cannot find directory with envelope files ", dir_gvf_env )
     exit  ( 1 )

if ( not os.path.isdir(dir_gvf_db) ):
     print ( "Cannot find directory with bgv files ", dir_gvf_db )
     exit  ( 1 )

print ( "It is vert important you first make a backup of your current" )
print ( "data base directories %s and %s" % ( dir_gvf_env, dir_gvf_db ) )
print ( "and check integrity of the archive" )
print ( " " )

ans = input ( "I have made a copy and checked it (yes/no) " )

if ( ans != "yes" ):
     print ( "Please make a copy" )
     exit  ( 1 )

if ( not os.path.isdir ( dir_gvf_new_env ) ): 
     os.mkdir ( dir_gvf_new_env )

num_ses = 0
db_files = []
com_list = []
for path, dirs, files in os.walk(dir_gvf_db):
    for file in files:
        if ( '#' in file ): confinue
        if ( '~' in file ): confinue
        db_files.append ( path + "/" + file )

env_files = []
for path, dirs, files in os.walk(dir_gvf_env):
    for file in files:
        env_files.append ( path + "/" + file )

db_files.sort()
env_files.sort()

for env_file     in env_files:
    if ( "#"     in env_file ): continue
    if ( "~"     in env_file ): continue
    if ( "__new" in env_file ): continue
    env_buf = read_file ( env_file )
    if ( len( env_buf[0].split() ) != 6 ):
         print ( "Envelope file %s is already converted. Skipping" % env_file )
         continue
    suffix  = env_buf[0][20:21]
    exp_name = env_buf[0][21:]
    sess_name = env_file.replace(dir_gvf_env,"").replace("/","")[0:10]
    db_file  = dir_gvf_db + "/" + sess_name + exp_name + "_fr1_v001.bgv"
    db_file2 = dir_gvf_db + "/" + sess_name[0:-1] + suffix + exp_name + "_fr1_v001.bgv"
    if ( iverb == 1 ): print ( "env_file= ", env_file )
    if ( os.path.isfile ( db_file ) ):
         if ( iverb == 1 ): print ( "path 1" )
         new_env_buf = []
         for line in env_buf:
             old_db_file = dir_gvf_db + "/" + sess_name + exp_name + "_" + line[8:11] + "_v" + line[12:15].replace(" ","0") + ".bgv"
             if ( not os.path.isfile ( old_db_file ) ):
                  print ( "Cannot find file ", old_db_file )
                  exit ( 1 )
             new_line = line[0:21] + " " + line[21:] 
             new_db_file = dir_gvf_db + "/" + sess_name + "_" + exp_name + '_' + line[8:11] + "_v" + line[12:15].replace(" ","0") + ".bgv"
             com_list.append ( "mv " + old_db_file + " " + new_db_file )
#
             built_new_db_file = dir_gvf_db + "/" + sess_name[0:-2] + "_" + \
                                 new_line.split()[5] + "_" + new_line.split()[6] + "_" + \
                                 new_line.split()[2] + "_v%03d.bgv" % int(new_line.split()[3])
             if ( new_db_file != built_new_db_file ):
                  print ( "new_db_file:       ", new_db_file )
                  print ( "built_new_db_file: ", built_new_db_file )
                  print ( "D1 %s  %s  %-8s  %s %s %s" % ( env_file, suffix, exp_name, sess_name, db_file, db_try_name ) ) # %%%%%%%%%%%%%%
                  exit  ( 1 )

             new_env_buf.append ( new_line )

         f=open ( env_file.replace(dir_gvf_env,dir_gvf_new_env), "w" )
         for line in new_env_buf:
             print ( line, file=f )
         f.close()
         com_list.append ( "mv " + env_file.replace(dir_gvf_env,dir_gvf_new_env) + " " + env_file )
             
         num_ses = num_ses + 1         
    elif ( os.path.isfile ( db_file2 ) ):
         if ( iverb == 1 ): print ( "path 2" )
         id = env_file.rfind("/")
         suffix  = env_file[id+10:id+11]
         exp_name = env_buf[0][20:]
         new_env_buf = []
         for line in env_buf:
             old_db_file = dir_gvf_db + "/" + sess_name[0:-1] + exp_name + "_" + line[8:11] + "_v" + line[12:15].replace(" ","0") + ".bgv"
             if ( not os.path.isfile ( old_db_file ) ):
                  print ( "Cannot find file ", old_db_file )
                  exit ( 1 )
             new_line = line[0:19] + " " + suffix + " " + line[20:] 
             new_db_file = dir_gvf_db + "/" + sess_name + "_" + exp_name + '_' + line[8:11] + "_v" + line[12:15].replace(" ","0") + ".bgv"
             com = "mv " + old_db_file + " " + new_db_file 
             built_new_db_file = dir_gvf_db + "/" + sess_name[0:-2] + "_" + \
                                 new_line.split()[5] + "_" + new_line.split()[6] + "_" + \
                                 new_line.split()[2] + "_v%03d.bgv" % int(new_line.split()[3])
             if ( new_db_file != built_new_db_file ):
                  print ( "new_db_file:       ", new_db_file )
                  print ( "built_new_db_file: ", built_new_db_file )
                  print ( "D2 %s  %s  %-8s  %s %s %s" % ( env_file, suffix, exp_name, sess_name, db_file, db_file2 ) ) # %%%%%%%%%%%%%%
                  exit  ( 1 )

##             new_env_buf.append ( line[0:19] + " " + suffix + " " + line[20:] )
             new_env_buf.append ( new_line )

             com_list.append ( "mv " + old_db_file + " " + new_db_file )

         f=open ( env_file.replace(dir_gvf_env,dir_gvf_new_env), "w" )
         for line in new_env_buf:
             print ( line, file=f )
         f.close()
         com_list.append ( "mv " + env_file.replace(dir_gvf_env,dir_gvf_new_env) + " " + env_file )
                 
         num_ses = num_ses + 1         
    else:
         print ( "FAILURE: %s  %s  %-8s  %s || %s %s " % ( env_file, suffix, exp_name, sess_name, db_file, db_file2 ) )
         exit ( 1 )

#
# --- Rremove duplicates
#
com_list = sorted(set(com_list)) 

com_list.append ( "rmdir " + dir_gvf_new_env )

f=open ( com_file, "w" )
for line in com_list:
    print ( line, file=f )
f.close()

print ( "Created command file %s" % com_file )
print ( "Please execute it." )

print ( "%d experiments processed" % num_ses )
