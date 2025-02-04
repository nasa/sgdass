#!/usr/bin/env python3
import  pwd, sys, os, re, shutil, time, subprocess, datetime, cmd


#
# ------------------------------------------------------------------------
#
def exe ( command ):
    """
    Auxilliart routine ese spawns a supborcess, 
    executes a shell command in the context of the subprocess, 
    waits for its completion, 
    and returns completion code and returns the output of the subprocess
    sent to stdout as a list of strings.
    """
    words = command.split()
    time_str = str(datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.") + "%6d" % datetime.datetime.now().microsecond).replace( " ", "0" )
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
         print ( "Usage: gvf_fix_exp_code.py vcat.conf" )
         exit  ( 1 )
    else:
         vcat_conf_file = sys.argv[1]

    vcat_buf = read_file ( vcat_conf_file )
    if ( vcat_buf == None ):
         print ( "Cannot read vcat_conf configuration file" )
         exit  ( 1 )

    num_ok  = 0
    num_upd = 0

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

    env_files = []
    for path, dirs, files in os.walk(gvf_env_dir):
        for file in files:
            env_files.append ( path + "/" + file )

    temp_name = "/tmp/gvf_fix.vda"

    for env_file in env_files:
        sess_name = env_file[-19:]
        buf_env   = read_file ( env_file )
        exp_code_env = buf_env[0].split()[6]
        com = "gvf_transform -to_ascii " + sess_name + " " + temp_name 
        (ret, out ) = exe ( com )
        if ( ret != 0 ):
             print ( "Error in executing command ", com )
             for line in out:
                 print ( line )
             exit ( 1)

        buf_db     = read_file ( temp_name )
        buf_db_new = []
        fl_updated = 0
        exp_code_db = None
        for line in buf_db:
            if ( "DATA.1 EXP_CODE" in line ):
                  if ( len(line.split()) < 7 ): 
                      exp_code_db = ""
                      buf_db_new.append ( line + " " + exp_code_env )
                      fl_updated = 1
                  else:
                      exp_code_db = line.split()[6]
                      buf_db_new.append ( line.replace(exp_code_db,exp_code_env) )
            else:
                 if ( "DATA.1 EXP_DESC" in line ):
                       if ( len(line.split()) < 7 ): 
                            buf_db_new.append ( line + " " + "uknown" )
                            fl_updated = 1
                            print ( "Updated EXP_DESC in %s" % env_file )
                       else:
                            buf_db_new.append ( line )
                 elif ( "DATA.1 " in line ):
                       if ( len(line) < 6 ): 
                            print ( "Too short line %s in %s" % ( line, env_file ) )
                            exit  ( 1 )
                       else:
                            buf_db_new.append ( line )
                 else:
                       buf_db_new.append ( line )

        if ( exp_code_env == None ):
             print ( "Error: not EXP_CODE found in %s" % env_file )
             continue

        if ( fl_updated == 1 ):
             f=open(temp_name,"w")
             for line in buf_db_new:
                 print ( line, file=f )
             f.close()
             print ( "%s  EXP_CODE %s is replaced with %s " % ( env_file, exp_code_db, exp_code_env ) )
             exit ( 2 )
             num_upd = num_upd + 1
        else:
             num_ok = num_ok + 1
             print ( "%s  EXP_CODE is ok" % env_file)

if __name__ == "__main__":
    global pima_child_pid
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        main()
    except KeyboardInterrupt:
        print ( "gen_sess.py: Interrupted" )
        exit ( 1 )

