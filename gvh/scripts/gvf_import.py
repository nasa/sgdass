#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Routine gvf_import.py imports the VLBI geodetic database in        *
# *   ascii vda format, transforms it in binary GVF format and place     *
# *   in the appropriate directories binary data files and the envelope  *
# *   file.                                                              *
# *                                                                      *
# *   Usage: gvf_import.py input_file [repo]                             *
# *                                                                      *
# *      input_file -- The full name of the input file in vda format.    *
# *                    The name is supposed to be un a form              *
# *                    yyyymmdd_S.vda, where yyyy is year, mm is month,  *
# *                    dd is day, S is the suffux in a range from        *
# *                    a to z. The file may be compressed with bzip2,    *
# *                    gzip, or lzma and therefore, have secondary       *
# *                    suffix .bz2, .gz, or .xz .                        *
# *                                                                      *
# *      repo    -- repository. If omitted, repository specified with    *
# *                 environment variable VCAT_REPO will be used, or      *
# *                 default repository if VCAT_REPO is not defined.      *
# *                                                                      *
# * ### 24-OCT-2019  gvf_import.py  v1.3 (c) L. Petrov  03-FEB-2024 ###  *
# *                                                                      *
# ************************************************************************
import    sys, os, time, subprocess, datetime

temp_dir = ""
db_name_base = ""

def exe ( command ):
    """
    Auxilliary routine exe spawns a supborcess, 
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
def read_file ( file_name ):
    """"
    Auxilliary program read_file reads a plain ascii file and
    returns its contents as a list of strings. If the file is 
    compressed with either gzip, or bzip2, or lzma, it uncompresses
    it on the fly.
    """

    if ( not os.path.isfile ( file_name ) ):
         print ( "read_file: file %s does not exist" % file_name )
         return None

    buf = []
#
# --- Check whether the file has zero length?
#
    if ( os.stat(file_name).st_size == 0 ):
         return buf

    magic = b'000000' # Default magic

    try:
#
# ----- Unless the file is too short, let us check magic -- 
# ----- it may be compressed
#
        if ( os.stat(file_name).st_size > 8 ):
             with open(file_name,"rb") as f:
                  magic = f.read(6)
             f.close()         
    
        if ( magic[0:2] == b'\x1f\x8b' ):
#
# ---------- This file is compressed with gzip
#
             buf=[]
             with gzip.open(file_name,mode="rt") as f:
                  for line in f:
                      buf.append ( line.strip("\n").strip("\r") )
             f.close()
        elif ( magic[0:2] == b'\x1f\x9d' ):
#
# ---------- This file is compressed with unix utiliy compress. 
# ---------- gzip understands this format and uncompress it.
#
             buf=[]
             with gzip.open(file_name,mode="rt") as f:
                  for line in f:
                      buf.append ( line.strip("\n").strip("\r") )
             f.close()
        elif ( magic[0:3] == b'BZh' ):
#
# ---------- This file is compressed with bzip2
#
             buf=[]
             with bz2.open(file_name,mode="rt") as f:
                  for line in f:
                      buf.append ( line.strip("\n").strip("\r") )
             f.close()
        elif ( magic[0:6] == b'\xfd7zXZ\x00'):
#
# ---------- This file is compressed with xz
#
             buf=[]
             with lzma.open(file_name,mode="rt") as f:
                  for line in f:
                      buf.append ( line.strip("\n").strip("\r") )
             f.close()
        else:
             with open(file_name,encoding="latin") as f:
                  for line in f:
                      buf.append ( line.strip("\n").strip("\r") )
             f.close()
    except BaseException as e: 
        print ( "Error in reading file %s -- %s" % ( file_name, str(e) ) )
        buf = None
        
    return buf
#
# ------------------------------------------------------------------------
#
def main():

    if ( len(sys.argv)-1 < 1 ):
         print ( "Usage: gvf_import.py db_name_file [repo]" )
         exit  ( 1 )
    else:
         db_name = sys.argv[1]
         if ( len(sys.argv)-1 >=  2 ):
              repo = sys.argv[2].upper()
         else:
              repo = ""

    if ( not os.path.isfile ( db_name ) ):
         print ( "Input file %s does not exist" % db_name )
         exit  ( 1 )

    id = db_name.rfind("/") 
    iv = db_name.rfind(".vda")
    if ( iv < 0 ):
         print ( "Malformed database name %s -- suffix .vda is absent" % db_name )
         exit  ( 1 )
        
    if ( iv - id != 11 ):
         print ( "Malformed database name %s -- the basename should be 10 characters long" % db_name )
         exit  ( 1 )

    if ( db_name[iv-2:iv-1] != "_" ):
         print ( "Malformed database name %s -- the 9th ccharacter of " + \
                 "the database name should be underscore" % db_name )
         exit  ( 1 )

    try:
         db_int = int(db_name[id+1:iv-2])
    except:
         print ( "Malformed database name %s -- first 8 character names " + \
                 "of the datqabase shoukld be an integer number" % db_name )
         exit  ( 1 )
 
    sess_name = db_name[id+1:iv] 
    suffix    = db_name[iv-1:iv] 

    if ( suffix < "a" or suffix > "z" ):
         print ( "Malformed database name %s -- suffix should be a low case letter" % db_name )
         exit  ( 1 )

    if ( ".bz2" in db_name or ".xz" in db_name or ".gz" in db_name ):
         fl_noshm = 0
         if ( "GVF_NOSHM" in os.environ ):
              if ( os.environ["GVF_NOSHM"] == "yes" or \
                   os.environ["GVF_NOSHM"] == "YES"    ):
                   fl_noshm = 1

#
# ------ Check for the temporary directory. We prefer /dev/shm, but
# ------ honor enfiroment variable that, if set, results in selection
# ------ of /tmp as a temporary directory
#
         if ( os.path.isdir ( '/dev/shm' ) and fl_noshm == 0 ):
              temp_dir = "/dev/shm"
         else:
              temp_dir = "/tmp"

    if ( ".bz2" in db_name ):
#
# ------ Uncompress with bzip2
#
         db_name_base = db_name[db_name.rfind("/")+1:].replace(".bz2","")
         com_compr = "lbzip2 -n1 -cdf " + db_name + " > " + temp_dir + "/" + db_name_base
         (ret, out) = exe ( com_compr )
         if ( ret != 0 ):
              print ( "gvf_import.py: Failure to decompress file %s" % db_name )
              for line in out:
                  print ( line )
              exit ( 1 )

    elif ( ".xz" in db_name ):
#
# ------ Uncompress with xz
#
         db_name_base = db_name[db_name.rfind("/")+1:].replace(".xz","")
         com_compr = "xz -cdf " + db_name + " > " + temp_dir + "/" + db_name_base
         (ret, out) = exe ( com_compr )
         if ( ret != 0 ):
              print ( "gvf_import.py: Failure to decompress file %s" % db_name )
              for line in out:
                  print ( line )
              exit ( 1 )

    elif ( ".gz" in db_name ):
#
# ------ Uncompress with gzip
#
         db_name_base = db_name[db_name.rfind("/")+1:].replace(".gz","")
         com_compr = "gzip -cdf " + db_name + " > " + temp_dir + "/" + db_name_base
         (ret, out) = exe ( com_compr )
         if ( ret != 0 ):
              print ( "gvf_import.py: Failure to decompress file %s" % db_name )
              for line in out:
                  print ( line )
              exit ( 1 )
    else:
         db_name_base = db_name
         temp_dir     = ""

#
# --- Generate a command for transformation to binary
#
    com_tra = "gvf_transform -to_binary"    + " " + \
               temp_dir + "/" + db_name_base + " " + \
               "@" + suffix + " " + repo

    (ret, out) = exe ( com_tra )
    if ( ret != 0 ):
         print ( "gvf_import.py: Failure to transform vda data file %s to gvf format" % db_name )
         print ( "gvf_import.py: Failed command: %s" % com_tra )
         for line in out:
             print ( line )
         exit ( 1 )
    else:
         print ( out[len(out)-1] )

#
# --- Leartn the full path name of the envelop file
#
    env_file = None
    if ( out[len(out)-1].split()[0] == "Updated" ):
         env_file = out[len(out)-1].split()[4]
    elif ( out[len(out)-1].split()[0] == "Created" ):
         env_file = out[len(out)-1].split()[3]

    if ( not env_file == None ):
#
# ------ Set permission for the envelop file
#
         os.system ( "chmod o+r,g+rws " + env_file )
         buf = []
         with open(env_file,encoding="latin") as f:
              for line in f:
                  buf.append ( line.strip("\n").strip("\r") )
         f.close()
         for line in buf:
#
# ---------- Build a db-file name
#
             db_file = env_file.replace("/env/","/db/")[:-8] + \
                       line.split()[6] + "_" + \
                       line.split()[2] + "_" + \
                       "v%03d." % int(line.split()[3]) + \
                       line.split()[4]
#
# ---------- ... and change its permission
#
             os.system ( "chmod o+r,g+rws " + db_file )

    if ( ".bz2" in db_name or ".xz" in db_name or ".gz" in db_name ):
#
# ------ Removed temporary uncompressed file
#
         os.unlink ( temp_dir + "/" + db_name_base )

    exit  ( 0 )

if __name__ == "__main__":
    global pima_child_pid
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        main()
    except KeyboardInterrupt:
        print ( "gen_sess.py: Interrupted" )
#
# ----- Remove a temporary file if it exists
#
        if ( temp_dir != "" and db_name_base != "" ):
             tmp_file = temp_dir + "/" + db_name_base 
             if ( os.path.isfile ( tmp_file ) ):
                  os.unlink ( tmp_file )
        
        exit ( 1 )

