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
             print ( str(line, encoding="utf-8"), end="", file=log_handle1 )
             log_handle1.flush()
        if ( log_handle2 ):
             print ( str(line, encoding="utf-8"), end="", file=log_handle2 )
             log_handle2.flush()
        out.append ( str(line, encoding="utf-8").split("\n")[0] )
    proc.wait()
    pima_child_pid = None
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
