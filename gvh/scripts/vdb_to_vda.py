#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program vdb_to_vda.py transforms VLBI data from vgosdb (vdb)       *
# *   format to vgosda (vda) format. It uses nusolve for that.           *
# *                                                                      *
# *  ### 09-OCT-2019  vdb_to_vda.py v1.3 (c)  L. Petrov  18-MAR-2021 ### *
# *                                                                      *
# ************************************************************************
import  sys, os, subprocess, datetime
#
#nusolve_exe = "nuSolve -i -t vgosDxConvertor.js b2a"
nusolve_exe = "vgosDxConvertor db2da"
#
temp_dir    = "/tmp"
#

def exe ( command ):
    """
    Auxilliary routine exe spawns a supborcess, 
    executes a shell command in the context of the subprocess, 
    waits for its completion, 
    and returns completion code and returns the output of the subprocess
    sent to stdout as a list of strings.
    """
    words = command.split()
    time_str = str(datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.") + "%6d" % datetime.datetime.now().microsecond).replace( " ", "0" )
    (ret, out) = subprocess.getstatusoutput ( command )
    return ( ret, out.split ( "\n" ) )

def main():
    debug = 0
    if ( len(sys.argv) < 3 ):
         print ( "Usage: vdb_to_vda.py  VDB-file VDA-file [-debug]" )
         exit  ( 1 )
    else:
         vdb_file = sys.argv[1]
         vda_file = sys.argv[2]
         if ( len(sys.argv) > 3 ):
              debug = int ( sys.argv[3] )
    if ( debug > 0 ):
         print ( "debug= ", debug ) 

#
# --- Extract the file name and compression suffix
#
    ib = vdb_file.rfind("/")
    ie = vdb_file.rfind(".tgz")
    if ( ie < 1 ):
         ie = vdb_file.rfind(".tar")
    elif ( ie < 1 ):
         ie = vdb_file.rfind(".tar.gz")
    elif ( ie < 1 ):
         ie = vdb_file.rfind(".tar.bz2")
    if ( ie < 1 ):
         print ( "Input vdb file should be a tar archive that can be compressed" )
         exit  ( 1 )

#
# --- Build temporary file names
#
    vdb_tar_dir     = temp_dir + "/" + vdb_file[ib+1:ie]
    vdb_dir         = vdb_tar_dir + "/" + vdb_file[ib+1:ie]
    if ( os.path.isdir ( vdb_tar_dir ) ):
         print ( "Temporary directory %s already exists. Please remove it and try again." % vdb_tar_dir )
         exit  ( 1 )

    vda_output_file = temp_dir + "/" + vdb_file[ib+1:ie] + ".vda"
#
# --- Create a temporary directory whether the vdb archive will be put
#
    (ret,out) = exe ( "mkdir "  + vdb_tar_dir )
    if ( ret != 0 ):
         print ( "Cannot create directory " + vdb_tar_dir + " " )
         print ( out )
         exit  ( 1 )
    if ( debug > 0 ):
         print ( "Created temporary directory ", vdb_tar_dir )

#
# --- Untar the input vdb archive
#
    if ( vdb_file.rfind(".tgz") > 0 or vdb_file.rfind(".tar.gz") ):
         com = "tar -C " + vdb_tar_dir + " -zxf " + vdb_file
    elif ( vdb_file.rfind(".tar.bz2") ):
         com = "tar -C " + vdb_tar_dir + " -jxf " + vdb_file
    else:
         com = "tar -C " + vdb_tar_dir + "  -xf " + vdb_file
    (ret,out) = exe ( com )
    if ( ret != 0 ):
         print ( "Cannot uncompress input vdb file " + vdb_file )
         for line in out:
             print ( line )
         print ( "Failed command: ", com )
         exit  ( 1 )
    if ( debug > 0 ):
         print ( "Untarred with command: ", com )
       
#
# --- Search for a wrapper file
#
    wrp_file_list = []
    for path, dirs, files in os.walk(vdb_dir):
        for file in files:
            if ( "ngs" in file ): continue
            if ( ".wrp" in file ): 
                 wrp_file_list.append ( path + "/" + file )

    if ( len(wrp_file_list) == 0 ):
         print ( "No wrp files in %s were found" % vdb_dir )
         exit  ( 1 )

#
# --- Sort the wrapper files
#
    wrp_file_list.sort()

#
# --- Take the wrapper file with the highest version
#
    wrp_file = wrp_file_list[len(wrp_file_list)-1]
    iv = wrp_file.rfind("_V")
    if ( len(wrp_file_list) == 0 ):
         print ( "Cannot find version in the wrp %s " % wrp_file )
         exit  ( 1 )
    if ( debug > 0 ):
         print ( "Took wrapper file ", wrp_file )
            
#
# --- Extract versions, long and short
#
    vers_short_str = wrp_file[iv+2:iv+5] 
    vers_long_str  = wrp_file[iv:iv+6] 

    for line in wrp_file_list:
        if ( vers_long_str in line ):
             if ( "GSFC_kall" in line ):
                   wrp_file = line
    if ( debug > 0 ):
         print ( "vers_short: ", vers_short_str, " vers_long: ", vers_long_str )

#
# --- Run nusolve for transforming VDB to VDA
#
#    com = nusolve_exe + " " + \
#          wrp_file + " " + \
#          vdb_dir  + " db2da"
    com = nusolve_exe + " " + \
          vdb_file + " " + \
          vda_file
    if ( debug > 0 ):
         print ( "About to run command ", com ) 
    (ret,out) = exe ( com )
    if ( ret != 0 ):
         print ( "Failure in conversion from VGOSDB to VGOSDA" )
         print ( out )
         exit  ( 1 )

    if ( not os.path.isfile ( vda_file ) ):
         print ( "Somehow command %s did not generate output file %s\n" % \
                 ( com, vda_output_file ) )
         for line in out:
             print ( line )
         print ( "Failed command: ", com )
         exit  ( 1 )

#@#
#@# --- Move the output to the final dstination
#@#
#@    if ( vda_output_file != vda_file ):
#@         if ( debug == 0 ):
#@              com = "mv " + vda_output_file + " " + vda_file
#@         else:
#@              com = "cp " + vda_output_file + " " + vda_file
#@              print ( "About to run command ", com )
#@         (ret,out) = exe ( com )
#@         if ( ret != 0 ):
#@              print ( "Failure in command %s " % com )
#@              for line in out:
#@                  print ( line )
#@              exit  ( 1 )

    if ( debug == 0 ):
#
# ------ Remove a temporary file
#
         com = "rm -fR " + vdb_tar_dir
         (ret,out) = exe ( com ) 
         if ( ret != 0 ):
              print ( "Failure in command %s " % com )
              for line in out:
                  print ( line )
              exit  ( 1 )

         com = "rm -fR " + temp_dir + "/nuSolve"
         (ret,out) = exe ( com ) 
         if ( ret != 0 ):
              print ( "Failure in command %s " % com )
              for line in out:
                  print ( line )
              exit  ( 1 )
    else:
         print ( "nusolve log is in ", temp_dir + "/nuSolve" )
         print ( "Temporary directory " + vdb_tar_dir + " was created" )

    print ( "Written output file " + vda_file )

if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        main()
    except KeyboardInterrupt:
        print ( "pf.py: Interrupted" )
        exit ( 1 )

