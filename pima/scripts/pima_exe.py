import os, sys, subprocess, datetime, signal

def exe ( command, verb, log_handle1, log_handle2 ):
    global pima_child_pid
    """
    Routine exe spawns a supborcess, executes command in the context 
    of the suborpess, waits for its completion and return the completion 
    code and results as ( returncode, out ). out is a list of strings
    without trailing "\n". Results are also immediately written in two 
    files handles if they are not None. If verb >= 3, the command to be 
    executed is printed in stdout.

    Requires sys, subprocess, datetime
    """

    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
    if ( verb >= 2 ): 
         print ( date_str, "execute:", command )
         sys.stdout.flush()
    if ( log_handle2 ):
         print ( date_str, "execute:", command, file=log_handle2 )
         
    proc = subprocess.Popen ( command, \
                              stdout=subprocess.PIPE, \
                              stderr=subprocess.STDOUT, \
                              shell=True )
    pima_child_pid = proc.pid
    out = []
    for line in proc.stdout:
        if ( log_handle1 ):
             print ( str(line, encoding="utf-8", errors="replace"), end="", file=log_handle1 )
             log_handle1.flush()
        if ( log_handle2 ):
             print ( str(line, encoding="utf-8", errors="replace"), end="", file=log_handle2 )
             log_handle2.flush()
        out.append ( str(line, encoding="utf-8", errors="replace").split("\n")[0] )
    proc.wait()
    pima_child_pid = None
    return ( proc.returncode, out )
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
def write_file ( buf, file_name ):
    out = []
    try:
        f=open(file_name,"w")
    except BaseException as e: 
        out.append ( "Failure in openning file %s for writing-- %s" % ( file_name, e) )
        return ( 1, out )
    for line in buf:
        try:
             print ( line,file=f )
        except BaseException as e: 
             out.append ( "Failure in writing in file %s -- %s" % ( file_name, e) )
             return ( 1, out )
    f.close()
    return ( 0, None )
#
# ------------------------------------------------------------------------
#
def append_file ( buf, file_name ):
    out = []
    try:
        f=open(file_name,"a")
    except BaseException as e: 
        out.append ( "Failure in openning file %s for writing-- %s" % ( file_name, e) )
        return ( 1, out )
    for line in buf:
        try:
             print ( line,file=f )
        except BaseException as e: 
             out.append ( "Failure in writing in file %s -- %s" % ( file_name, e) )
             return ( 1, out )
    f.close()
    return ( 0, None )

#
# ------------------------------------------------------------------------
#
def exe_out_log ( command, verb, lh ):
    return ( exe ( command, verb, sys.stdout, lh ) )
#
# ------------------------------------------------------------------------
#
def exe_out_nolog ( command, verb ):
    return ( exe ( command, verb, sys.stdout, None ) )
#
# ------------------------------------------------------------------------
#
def exe_noout_log ( command, verb, lh ):
    return ( exe ( command, verb, None, lh ) )
#
# ------------------------------------------------------------------------
#
def exe_noout_nolog ( command, verb ):
    return ( exe ( command, verb, None, None ) )
#
# ------------------------------------------------------------------------
#
def pima_signal_handler_term ( signal, frame ):
    global pima_child_pid
    if ( pima_child_pid ):
         os.kill ( pima_child_pid, signal )
    print ( 'Terminated by TERM signal' )
    sys.exit(0)
#
# ------------------------------------------------------------------------
#
def pima_print_mes ( str, fh ):
    exe = sys.argv[0]
    if ( exe.rfind("/") > 0 ): exe = exe[exe.rfind("/")+1:]
    print ( exe + ": " + str )
    if ( fh != None ): print ( exe + ": " + str , file=fh )
