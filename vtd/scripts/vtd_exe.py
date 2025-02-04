import os, sys, subprocess, datetime, signal

def exe ( command, verb, log_handle1, log_handle2 ):
    global vtd_child_pid
    """
    Routine exe spawns a supborcess, executes command in the context 
    of the subprocess, waits for its completion and return the completion 
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
    vtd_child_pid = proc.pid
    out = []
    for line in proc.stdout:
        if ( log_handle1 ):
             print ( str(line, encoding="utf-8"), end="", file=log_handle1 )
             log_handle1.flush()
        if ( log_handle2 ):
             print ( str(line, encoding="utf-8"), end="", file=log_handle2 )
             log_handle2.flush()
        out.append ( str(line, encoding="utf-8").split("\n")[0] )
    proc.wait()
    vtd_child_pid = None
    return ( proc.returncode, out )

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
def vtd_signal_handler_term ( signal, frame ):
    global vtd_child_pid
    if ( vtd_child_pid ):
         os.kill ( vtd_child_pid, signal )
    print ( 'Terminated by TERM signal' )
    sys.exit(0)
#
# ------------------------------------------------------------------------
#
def vtd_print_mes ( str, fh ):
    exe = sys.argv[0]
    if ( exe.rfind("/") > 0 ): exe = exe[exe.rfind("/")+1:]
    print ( exe + ": " + str )
    if ( fh != None ): print ( exe + ": " + str , file=fh )
#
# ------------------------------------------------------------------------
#
def read_file_to_buffer ( file_name ):
    """
    Read contents of a text file into a buffer. Prints a message and 
    returns None if error encounted
    """
    buf = []
    if ( not os.path.isfile ( file_name ) ):
         print ( "read_file_to_buffer: file name %s does not exist" % file_name )
         return None
    try:
         with open ( file_name, encoding="latin" ) as f:
              buf = f.read().splitlines()
         f.close ( )
    except Exception as e:
         print ( "read_file_to_buffer: Error in reading input %s -- %s " % \
                 ( str(e), file_name ) )
         return None
    return buf

#
# ------------------------------------------------------------------------
#
def expand_env_vars ( buf, comment_char = None ):
    """
    Expands environment variables in the form ${VAR} and replace then
    with values of the variables. Up to 8 levels of recursion is allowed
    """
    m_lev = 8
#
# --- If the buffer is zero length than nothing to do
#
    if ( len(buf) == 0 ): return buf
    for i in range(0,m_lev):
        fl_envar = 0
        for k in range(0,len(buf)):
            line = buf[k]
#
# --------- If the line empty, skip it
#
            if ( len(line) == 0 ): continue
            if ( comment_char ):
#
# -------------- If the line starts with the comment sign, skip it
#
                 if ( line[0:1] == comment_char ): continue
#
# --------- Search for the patter of environment vriable start
#
            ib = line.find('${')
            if ( ib > 0 ):
#
# -------------- No space to finish the pattern?
#
                 if ( ib == len(line)-1 ):
                      print ( "Error in expanding environment variable at line %s" % line )
                      print ( "Incomplete envirnoment variable definition" )
                      return None
#
# -------------- Search for the end of the environment defition
#
                 ie = line[ib+2:].find('}') + ib+3
                 if ( ie < ib+3 ):
                      print ( "Error in expanding environment variable at line %s" % line )
                      print ( "No closing curly partenthesis" )
                      return None
                 if ( ie == ib+3 ):
                      print ( "Error in expanding environment variable at line %s" % line )
                      print ( "Empty envirnoment variable" )
                      return None
#
# -------------- Extract the envirotment variable name
#
                 envar = line[ib+2:ie-1]
#
# -------------- Get the value of the environment variable
#
                 enval = os.getenv(envar)
                 if ( not enval ):
                      print ( "Error in expanding environment variable at line %s" % line )
                      print ( "Envirnoment variable %s is not defined" % envar )
                      return None
#
# -------------- Finally, substitute the environment variable definition with 
# -------------- its value
#
                 buf[k] = buf[k].replace ( line[ib:ie], enval )
#
# -------------- Set the flag that we found and substiuted the environment variable
#
                 fl_envar = 1

        if ( fl_envar == 0 ): 
             return buf

    print ( "Error in expanding environment variables in the buffer" )
    print ( "Too many levels of recursion. Please check for circular defintions"  )
    return None
