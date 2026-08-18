import sys, os, shutil, time, calendar, subprocess, datetime

def exe_with_log ( command, log_buf):
#"""
#    Spawn a subprocess, execute command in the context of the subprocess,
#    wait for its completion, and return completion the code and results.
#"""
    words = command.split()
    time_str = str(datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.") + "%6d" % datetime.datetime.now().microsecond).replace( " ", "0" )
    if ( type(log_buf) == "list" ):
         log_buf.append ( "@@@ " )
         log_buf.append ( "@@@ Running command " + command + " @@@" )
         log_buf.append ( "@@@ " + time_str )
         log_buf.append ( "@@@ "            )
    if ( type(log_buf) == "int" ):
         if ( log_buf >= 2 ):
              out_com = []
              out_com.append ( "@@@ " )
              out_com.append ( "@@@ Running command " + command + " @@@" )
              out_com.append ( "@@@ " + time_str )
              out_com.append ( "@@@ "            )
    (ret, out) = subprocess.getstatusoutput ( command )
    if ( type(log_buf) == "list" ):
         log_buf.append ( out )
    if ( type(log_buf) == "int" ):
         if ( log_buf >= 2 ):
              out = out_com + out
    return ( ret, out.split ( "\n" ) )

#
# ------------------------------------------------------------------------
#
def wget_exe ( com, log_buf ):
#"""
#    Routine wget_exe executes a command that includes wget in a secure way.
#    If there  username and/or password are specified in the command
#    line, it is put in the temporary configuration file and removed from 
#    the command line
#"""
#
# --- check for username/password commands
#
    config_list = []    
    for word in com.split():
        if ( "--ftp-password" in word ):
#
# ---------- Extract the ftp password
#
             config_line = word.split("=")[-1].replace('"','')
#
# ---------- Remove it from the command line
#
             com = com.replace(word,"")
#
# ---------- and append to the configuration file
#
             if ( config_line != "" ):
                  config_list.append ( "ftp-pasword = " + config_line )
        if ( "--user" in word ):
#
# ---------- Extract the user name
#
             config_line = word.split("=")[-1].replace('"','')
#
# ---------- Remove it from the command line
#
             com = com.replace(word,"")
#
# ---------- and append to the configuration file
#
             config_list.append ( "user = " + config_line )
        if ( "--http-password" in word ):
#
# ---------- Extract the http password
#
             config_line = word.split("=")[-1].replace('"','')
#
# ---------- Remove it from the command line
#
             com = com.replace(word,"")
#
# ---------- and append to the configuration file
#
             config_list.append ( "http-password = " + config_line )
    
    if ( len(config_list) > 0 ):
#
# ------ Yes! There were username/password in the wget command linie
#
         finam = "/dev/shm/wget__%08d.cnf" % os.getpid()
#
# ------ Write the context of configuration in the temporary file
#
         f=open(finam,"w")
         for line in config_list:
             print ( line, file=f )
         f.close()
#
# ------ Set u=rw,g=,o= permissions
#
         os.chmod ( finam, 0o600 )
#
# ------ Add --config=finam in the wget command
#
#
# ------ Add --config=finam in the wget command
#
         for word in com.split():
             if ( word == "wget" or "/wget" in word ):
                  com = com.replace(word,word + " --config=" + finam)

#
# --- Execute wget command
#
    (ret,err) = exe_with_log ( com, log_buf )
#
# --- Remove temorary file with configuration
#
    if ( len(config_list) > 0 ):
         os.unlink ( finam )
    return    ( ret, err )
