import sys, os, shutil, time, subprocess, datetime, signal
def malo_signal_handler ( signal, frame ):
    global malo_child_pid
    if ( malo_child_pid ):
         os.kill ( malo_child_pid, signal )
    print ( 'Terminated by TERM signal' )

    sys.exit(0)

#
# ------------------------------------------------------------------------
#
def exe ( command, verb ):
    global malo_child_pid
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
         try:
              sys.stdout.flush()
         except:
              print ( "Cannot setup the pipe. Please check the re-directing" )
              exit  ( 1 )
        
    proc = subprocess.Popen ( command, \
                              stdout=subprocess.PIPE, \
                              stderr=subprocess.STDOUT, \
                              shell=True )
    malo_child_pid = proc.pid
    out = []
    line = ""
    while True:
        chr = proc.stdout.read(1)
        if ( chr == b'' and proc.poll() is not None ):
             break
        if ( chr == b'\r' or chr == b'\n' ):
             if ( verb >= 3 ): print ( "" )
             out.append ( line )
             line = ""
        else:
             try:
                str_enc = str(chr, encoding="utf-8")
             except:
                str_enc = "?"
             if ( verb >= 3 ): print ( str_enc, end="" )
             line = line + str_enc
    rc = proc.poll()
    if ( line != "" ): out.append ( line )

    malo_child_pid = None
    return ( proc.returncode, out )
