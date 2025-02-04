#!/usr/bin/env python3
import           pwd, sys, os, re, shutil, time, subprocess, datetime, cmd

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
# ---------- This file is compressed with bzip2
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
    if ( len(sys.argv) < 2 ):
         print ( "Usage: gvf_export.py vcat.conf" )
         exit  ( 1 )
    else:
         vcat_conf_file = sys.argv[1]

    vcat_buf = read_file ( vcat_conf_file )
    if ( vcat_buf == None ):
         print ( "Cannot read vcat_conf configuration file" )
         exit  ( 1 )

    gvf_db_dir  = None
    gvf_env_dir = None
    for line in vcat_buf:
        if ( len(line.split()) < 2 ): continue
        if ( line.split()[0] == "GVF_DB_DIR:"  ): gvf_db_dir  = line.split()[1] 
        if ( line.split()[0] == "GVF_ENV_DIR:" ): gvf_env_dir = line.split()[1] 
  
    if ( gvf_db_dir == None ): 
         print ( "Damaged %s vcat configuration file: GVF_DB_DIR  keyword is undefined" % \
                  gvf_db_dir )
         exit  ( 1 )

    if ( gvf_env_dir == None ): 
         print ( "Damaged %s vcat configuration file: GVF_ENV_DIR keyword is undefined" % \
                  gvf_db_dir )
         exit  ( 1 )

    db_files = []
    for path, dirs, files in os.walk(gvf_db_dir):
        for file in files:
            if ( '#' in file ): confinue
            if ( '~' in file ): confinue
            db_files.append ( path + "/" + file )
    
    env_files = []
    for path, dirs, files in os.walk(gvf_env_dir):
        for file in files:
            env_files.append ( path + "/" + file )

    db_files.sort()
    env_files.sort()

#
# --- First test envelope files
#
    
    num_err = 0
    db_committed_files = []
    for env_file in env_files:
        buf_env   = read_file ( env_file )
        sess_name = env_file[-19:-9]
        suffix    = env_file[-10:-9] 
        for line in buf_env:
            if ( len(line.split()) != 7 ):
                 print ( 'Damaged envelope file %s -- it has a line "%s" with the number of words other than 7' % \
                         ( env_file, line ) )
                 num_err = num_err + 1
                 break
            if ( line.split()[5] != suffix ):
                 print ( "env_file: ", env_file, " hh>>" + env_file[-10:-9] + "<< " )
                 exit ( 2 )
            db_file = gvf_db_dir + "/" + sess_name[0:-2] + "_" + \
                      line.split()[5] + "_" + line.split()[6] + "_" + \
                      line.split()[2] + "_v%03d.bgv" % int(line.split()[3])
           
            if ( not os.path.isfile ( db_file ) ):
                 print ( "Damaged envelope file %s -- cannot file file %s" % \
                         ( env_file, db_file ) )
                 num_err = num_err + 1
                 break
            db_committed_files.append ( db_file )

    if ( num_err == 0 ):
         print ( "Checked %6d envelope files and found no errors" % len(env_files) )
    else:
         print ( "Checked %6d envelope files and found %6d errors" % ( len(env_files), num_err ) )

    num_err = 0
    for db_file in db_files:
        if ( not db_file in db_committed_files ):
             print ( "Orphaned %s file " % db_file )
             num_err = num_err + 1

    if ( num_err == 0 ):
         print ( "Checked %6d bgf files and found no errors" % len(env_files) )
    else:
         print ( "Checked %6d bgf files and found %6d errors" % ( len(env_files), num_err ) )


if __name__ == "__main__":
    global pima_child_pid
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        main()
    except KeyboardInterrupt:
        print ( "gen_sess.py: Interrupted" )
        exit ( 1 )

