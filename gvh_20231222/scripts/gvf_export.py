#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Routine gvf_export.py exports the VLBI geodetic database in        *
# *   binary GVF format to ascii vda format and compresses the output.   *
# *                                                                      *
# *   Usage: gvf_export.py db_name dir_out [compress|nocompress] [repo]  *
# *                                                                      *
# *      db_name -- 10-character long GVF database name without version  *
# *                 and extension. gvf_export.py will take the last      *
# *                 version.                                             *
# *                                                                      *
# *      dir_out -- Name of the output directory where the output        *
# *                 database will be written. The name of the output     *
# *                 database will be the same as db_name with .vda.bz2   *
# *                 extension appended.                                  *
# *                                                                      *
# *      flag    -- compress (default) or nocompress.                    *
# *                                                                      *
# *      repo    -- repository. If omitted, repository specified with    *
# *                 environment variable VCAT_REPO will be used, or      *
# *                 default repository if VCAT_REPO is not defined.      *
# *                                                                      *
# * ### 24-OCT-2019  gvf_export.py  v1.3 (c) L. Petrov  22-DEC-2023 ###  *
# *                                                                      *
# ************************************************************************
import    sys, os, time, subprocess, datetime

fil_out = ""
ivrb = 0

def exe ( command ):
    """
    Auxilliary routine ese spawns a supborcess, 
    executes a shell command in the context of the subprocess, 
    waits for its completion, 
    and returns completion code and returns the output of the subprocess
    sent to stdout as a list of strings.
    """
    words = command.split()
    time_str = str(datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.") + "%6d" % \
                   datetime.datetime.now().microsecond).replace( " ", "0" )
    (ret, out) = subprocess.getstatusoutput ( command )
    return ( ret, out.split ( "\n" ) )
#
# ------------------------------------------------------------------------
#
def main():

    flag = "compress" # Default
    if ( len(sys.argv) < 3 ):
         print ( "Usage: gvf_export.py db_name dir_out [compress|nocompress] [repo]" )
         exit  ( 1 )
    else:
         db_name = sys.argv[1]
         dir_out = sys.argv[2]
         if ( len(sys.argv)-1 >=  3 ):
              flag = sys.argv[3] 
              if ( len(flag) >= 3 ):
                   if ( flag[0:3] == "com" ): flag = "compress"
                   if ( flag[0:3] == "noc" ): flag = "nocompress"
              if ( not ( flag == "nocompress" or flag == "compress" ) ):
                   print ( "Wrong 3rd argument %s -- only compress or nocompress are supported" % flag  )
                   exit  ( 1 )
         if ( len(sys.argv)-1 >=  4 ):
              repo = sys.argv[4].upper()
         else:
              repo = ""

#
# --- Build the name of the output file
#
    if ( "/" in db_name ):
         id = db_name.rfind("/")
         fil_out = db_name[id+1:id+11]
    else:
         fil_out = db_name[0:10]

#
# --- Generate command for transformation to ascii format
#
    if ( flag == "compress" ):
         fil_out = dir_out + "/" + fil_out + ".vda.bz2"
         com = "gvf_transform -to_ascii" + " " + \
                db_name + " " + \
                dir_out + "/@.bz2" + " " + repo + " ; " \
                "chmod o+r,g+rws " + fil_out 
    else:
         fil_out = dir_out + "/" + fil_out + ".vda"
         com = "gvf_transform -to_ascii" + " " + \
                db_name + " " + \
                dir_out + "/@" + " " + repo + " ; " \
                "chmod o+r,g+rws " + fil_out 

#
# --- Run command for exporting to ascii
#
    if ( ivrb > 0 ):
         print ( "com= ", com )
    (ret, out) = exe ( com )
    for line in out:
        print ( line )

    if ( ret != 0 ):
         print ( "gvf_export.py: Failure to transform gvf data file %s to vda format" % db_name )
         print ( "Failed command: ", com )
         exit ( 1 )
    else:
         print ( out[0] )
         exit  ( 0 )

if __name__ == "__main__":
    global pima_child_pid
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        main()
    except KeyboardInterrupt:
        print ( "gvf_export.py: Interrupted" )
#
# ----- Clean garbage
#
        if ( fil_out != "" ):
             if ( os.path.isfile ( fil_out ) ):
                  os.unlink ( fil_out )
        exit ( 1 )
