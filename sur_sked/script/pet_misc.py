import  sys, os, subprocess, signal, time, datetime, gzip, bz2, lzma
import  sur_sked_config # Import sur_sked confuguration

def exe ( command ):
    """
    Auxiliary routine exe spawns a subprocess, 
    executes a shell command in the context of the subprocess, 
    waits for its completion, 
    and returns completion code and returns the output of the subprocess
    sent to stdout as a list of strings.
    """
    (ret, out) = subprocess.getstatusoutput ( command )
    return ( ret, out.split ( "\n" ) )

def exe_robust ( command ):
    """
    Auxiliary routine exe spawns a subprocess, 
    executes a shell command in the context of the subprocess, 
    waits for its completion, 
    and returns completion code and returns the output of the subprocess
    sent to stdout as a list of strings.
    """
    ret_file = "/dev/shm/exe_%08d_%s.log" % ( os.getpid(), ("%.9f" % time.time()).replace(".","") )
    com = sur_sked_prefix + "/bin/container.sh " + ret_file + " " + command
    (ret, out) = subprocess.getstatusoutput ( com )
    if ( os.path.isfile(ret_file) ):
         with open(ret_file,encoding="latin") as f:
              buf = []
              for line in f:
                  buf.append ( line.strip("\n").strip("\r") )
              try:
                  ret = int(buf[0])
              except:
                  ret = -1
         f.close()
#         os.unlink( ret_file )
    return ( ret, out.split ( "\n" ) )
#
# ------------------------------------------------------------------------
#
def exe_nowait ( com_arr, filout ):
    """
    Auxiliary routine exe spawns a subprocess, 
    executes a shell command in the context of the subprocess
    and does not waits for its completion.
    """
    f=open(filout,"w")
    p = subprocess.Popen ( com_arr, \
                           shell=False, \
                           universal_newlines=True, \
                           cwd="/", \
                           stdin=subprocess.PIPE, 
                           stdout=f, 
                           stderr=f )
    f.close()
    return ( p.pid )

#
# ------------------------------------------------------------------------
#
def check_err_exe ( ret, out, com ):
    """
    Auxiliary routine checks ret. If ret is not 0, then
    it prints out (error message) and com (failed command)
    and then exits
    """
    if ( ret != 0 ):
         for line in out:
             print ( line )
         print ( "Error in running command ", com )
         exit ( ret )
#
# ------------------------------------------------------------------------
#
def exe_silent ( command ):
    words = command.split()
    (ret, out) = subprocess.getstatusoutput ( command )
    if ( ret != 0 ):
         for line in out.split( "\n" ):
             print ( line )
    return ( ret )
#
# ------------------------------------------------------------------------
#
def exe_pipe ( command ):
    """
    Routine exe_pipe spawns a supborcess, executes command in the context 
    of the suborpess, waits for its completion and return the completion 
    code and results as ( returncode, out ). out is a list of strings
    without trailing "\n". stderr and stdout are mixed. stdout+stderr
    is written immediatly without waiting.
    Requires sys, subprocess, datetime
    """
    proc = subprocess.Popen ( command, \
                              stdout=subprocess.PIPE, \
                              stderr=subprocess.STDOUT,\
                              shell=True )
    out = []
    line = ""
    while True:
       inchar = proc.stdout.read(1)
       if inchar:
          ch = str(inchar, encoding="utf-8", errors="replace")
          print ( ch, end='' ) 
          sys.stdout.flush()
          if ( ch == "\n" ):
               out.append ( line )
               line = ""
          else:
               line = line + ch
       else:
#          print ('')
          break

    if ( line != "" ): 
         out.append ( line )

    proc.wait()
    return ( proc.returncode, out )
#
# ------------------------------------------------------------------------
#
def exe_pipe_noshell ( command ):
    """
    Routine exe_pipe spawns a supborcess, executes command in the context 
    of the suborpess, waits for its completion and return the completion 
    code and results as ( returncode, out ). out is a list of strings
    without trailing "\n". stderr and stdout are mixed. stdout+stderr
    is written immediatly without waiting.
    Requires sys, subprocess, datetime
    """
    com = []
    for word in command.split():
        com.append ( word )

    proc = subprocess.Popen ( com, \
                              stdout=subprocess.PIPE, \
                              stderr=subprocess.STDOUT,\
                              shell=False )
    out = []
    line = ""
    while True:
       inchar = proc.stdout.read(1)
       if inchar:
          ch = str(inchar, encoding="utf-8", errors="replace")
          print ( ch, end='' ) 
          sys.stdout.flush()
          if ( ch == "\n" ):
               out.append ( line )
               line = ""
          else:
               line = line + ch
       else:
          print ('')
          break

    if ( line != "" ): 
         out.append ( line )

    proc.wait()
    return ( proc.returncode, out )

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
def write_file ( buf, file_name ):
    out = []
    file_name_temp = "%s_%08d.temp" % ( file_name, os.getpid() )
    try:
        f=open(file_name_temp,"w")
    except BaseException as e: 
        out.append ( "Failure in openning file %s for writing-- %s" % ( file_name_temp, e) )
        return ( 1, out )
    for line in buf:
        try:
             print ( line,file=f )
        except BaseException as e: 
             out.append ( "Failure in writing in file %s -- %s" % ( file_name_temp, e) )
             return ( 1, out )
    f.close()
    os.rename ( file_name_temp, file_name )
    
    return ( 0, None )
#
# ------------------------------------------------------------------------
#
def append_file ( buf, file_name ):
    out = []
    file_name_temp = "%s_%08d.temp" % ( file_name, os.getpid() )
    try:
        f=open(file_name,"a")
    except BaseException as e: 
        out.append ( "Failure in openning file %s for writing-- %s" % ( file_name_temp, e) )
        return ( 1, out )
    for line in buf:
        try:
             print ( line,file=f )
        except BaseException as e: 
             out.append ( "Failure in writing in file %s -- %s" % ( file_name_temp, e) )
             return ( 1, out )
    f.close()
    os.rename ( file_name_temp, file_name )

    return ( 0, None )

#
# ------------------------------------------------------------------------
#
def is_proc_exists ( pid ):        
    """ Check For the existence of a unix pid. """
    try:
        os.kill(pid, 0)
    except OSError:
        return False
    else:
        return True

#
# ------------------------------------------------------------------------
#   
def mjd_tim_to_time ( mjd, tim ):
    """
    Transform a pair mjd,tim into Python time
    """
    date_j2000 = datetime.datetime.strptime ( "2000", "%Y" )
    if ( mjd < 30000 or mjd > 80000 ):
         print ( "mjd %d is out of range [30000, 80000]" % mjd )
         return None
    if ( tim < -0.000001 or tim > 86400.001 ):
         print ( "tim %f is out of range [-0.001, 86400.001]" % tim )
         return None
    return ( date_j2000 + datetime.timedelta ( days=(mjd - 51544), \
                                               seconds=tim ) )
