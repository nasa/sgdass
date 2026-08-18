#!/usr/bin/python3 
import os, sys, string, stat, datetime, subprocess

def removeNonAscii(s): 
    return "".join ( i for i in s if \
                       ( ord(i) == 32  or \
                         ord(i) == 35  or \
                         ord(i) == 43  or \
                         ord(i) == 45  or \
                         ord(i) == 46  or \
                         ( ord(i) >= 48  and ord(i) <=  57 ) or \
                         ( ord(i) >= 65  and ord(i) <=  90 ) or \
                           ord(i) == 95                      or \
                         ( ord(i) >= 97  and ord(i) <= 122 )    \
                       )
                   )
#
# ------------------------------------------------------------------------
#
def exe ( command, log_buf):
#"""
#    Spawn a supborcess, execute command in the context of the suborpess,
#    wait for its completion and return completion code and results.
#"""
    words = command.split()
    time_str = str(datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.") + "%6d" % datetime.datetime.now().microsecond).replace( " ", "0" )
    log_buf.append ( "@@@ " )
    log_buf.append ( "@@@ Running command " + command + " @@@" )
    log_buf.append ( "@@@ " + time_str )
    log_buf.append ( "@@@ "            )
    (ret, out) = subprocess.getstatusoutput ( "ulimit -s 8000000; " + command )
    log_buf.append ( out )
    return ( ret, out.split ( "\n" ) )
#
# ------------------------------------------------------------------------
#
def exe_nolog ( command ):
#"""
#    Spawn a supborcess, execute command in the context of the suborpess,
#    wait for its completion and return completion code and results.
#"""
    words = command.split()
    time_str = str(datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.") + "%6d" % datetime.datetime.now().microsecond).replace( " ", "0" )
    (ret, out) = subprocess.getstatusoutput ( "ulimit -s 8000000; " + command )
    return ( ret, out.split ( "\n" ) )
#
# ------------------------------------------------------------------------
#
def check_lock ( lock_file, lock_min_wait, lock_max_wait ):
    pid_str = "%05d" % os.getpid()
    num_tries = int(lock_max_wait/lock_min_wait)
    for k in range(0,num_tries):
        if ( os.path.isfile(lock_file) ):
             f= open ( lock_file, "r" )
             (lock_date, lock_pid, lock_user, lock_process ) = f.readline().split()
             f.close()
             try:
                proc_code = os.kill ( int(lock_pid), 0 )
             except(OSError) as err: 
                proc_code = -1
                break
             else:
                proc_code =  0
             sleep ( lock_min_wait )
        else:
             break
         
             time_now = str(datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.") + \
                       "%3d" % (int(datetime.datetime.now().microsecond)/1000) ).replace( " ", "0" )

             lock_date = datetime ( int(lock_date_str[0:4]),   int(lock_date_str[5:7]),   \
                                    int(lock_date_str[8:10]),  int(lock_date_str[11:13]), \
                                    int(lock_date_str[14:16]), int(lock_date_str[17:19]), \
                                    1000*int(lock_date_str[20:23]) )

        lock_age = (datetime.now() - lock_date).days*86400. + \
                   (datetime.now() - lock_date).seconds

        if ( lock_age > lock_max_wait ):
             break    

    time_now = str(datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.") + \
               "%3d" % (int(datetime.datetime.now().microsecond)/1000) ).replace( " ", "0" )
    f= open ( lock_file, "w" )
    f.write ( "%s %05s apache          malo_ondemand.py\n" % ( time_now, pid_str ) )
    f.close()

    os.chmod ( lock_file, \
               stat.S_IREAD + stat.S_IWRITE + stat.S_IEXEC + \
               stat.S_IRGRP + stat.S_IWGRP  + stat.S_IXGRP + \
               stat.S_IROTH +                 stat.S_IXOTH   )

    return 0

#
# ------------------------------------------------------------------------
#
def html_error ( message ):
    print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
    print ( '<HTML LANG="ru">' )
    print ( '<HEAD>' )
    print ( '<META http-equiv="Content-Type" content="text/html; charset=koi8-r">' )
    print ( '</HEAD' )
    print ( '<BODY>' )
    print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + message )
    print ( '</BODY></HTML>' )
    return 0
#
# ------------------------------------------------------------------------
#
class config_class:
   def __init__ ( self ):
       self.malo_pref       = "/opt64"
       self.malo_bin        = os.popen ( "export LD_LIBRARY_PATH=" + self.malo_pref + "/lib:" + self.malo_pref + "/lib64 ; " + \
                                          self.malo_pref + "/bin/malo_inq bin_static").read().rstrip()
       self.malo_share      = os.popen ( "export LD_LIBRARY_PATH=" + self.malo_pref + "/lib:" + self.malo_pref + "/lib64 ; " + \
                                          self.malo_pref + "/bin/malo_inq share").read().rstrip()
      
       self.ondemand_dir    = "/imls/ondemand"
       self.req_html        = "/ondemand/req"
       if ( "HTTP_HOST" in os.environ ):
            if ( os.environ["HTTP_HOST"] == "massloading.smce.nasa.gov" ):
                 self.ondemand_dir    = "/imls_ondemand"
                 self.req_html        = "/imls_ondemand/req"
                  
       self.max_len         = 1048576
       self.malo_max_sta    = 32768
       self.malo_max_intrv  = 50*366*86400
       self.max_sta         = 16384
       self.max_intrv       = 5*366*86400
       self.lock_min_wait   = 0.01
       self.lock_max_wait   = 32.0
       self.ondemand_queue  = self.ondemand_dir + "/queue.txt"
       self.ondemand_lock   = self.ondemand_dir + "/lock.txt"
       self.req_dir         = self.ondemand_dir + "/req"
       self.catcha_dir      = self.ondemand_dir + "/catcha"
       self.planet_dir      = self.ondemand_dir + "/planets"
       self.animal_dir      = self.ondemand_dir + "/animals"
       self.synonyms        = { 
                                "belka":     "squirrel",  \
                                "chicken":   "rooster",   \
                                "cock":      "rooster",   \
                                "fish":      "shark",     \
                                "giraf":     "giraffe",   \
                                "girafe":    "giraffe",   \
                                "hare":      "rabbit",    \
                                "monkey":    "gorilla",   \
                                "ape":       "gorilla",   \
                                "muha":      "fly",       \
                                "ram":       "sheep",     \
                                "rat":       "mouse",     \
                                "slon":      "elephant",  \
                                "alligator": "crocodile", \
                                "krokodil":  "crocodile", \
                                "lyagushka": "frog",      \
                                "lev":       "lion",      \
                                "leo":       "lion",      \
                                "toad":      "frog"       \
                              }
       self.num_pla         = 8
       self.ivrb            = 0
#
# ------------------------------------------------------------------------
#
def check_ip ( config, ip ):
# ************************************************************************
# *                                                                      *
# *   Routine check_ip checks whether the ip supplied in the second      *
# *   argument is in the unlimited list. It checks ip against the IP     *
# *   address in the IP unlimitred file. If the IP unlimited file        *
# *   contains domain names, it translates the domain name and check     *
# *   it against the IP. If the IP is in the unlimitd list, check_ip     *
# *   returns 1, otherwise 0.                                            *
# *                                                                      *
# *  ### 06-JUL-2015   check_ip    v1.0 (c)  L. Petrov  06-JUL-2015 ###  *
# *                                                                      *
# ************************************************************************
    import socket

#
# --- Build the unlimited ip file name
#
    ip_file_name = config.malo_share + "/ip_unlimited.txt"
  
#
# --- read it
#
    with open(ip_file_name) as f:
         buf = f.readlines()


    ip_name = []
    domain_name = []
    for ip_str in buf:
        ip_str = ip_str.replace("\n","")
        try:
#
# --------- Convert the IP to domain name and back
#
            ip_name.append ( socket.gethostbyaddr(ip_str)[2][0] )
        except:
            try:
                ip_name.append ( socket.gethostbyname(ip_str) )
            except:
                continue 

#
# --- Check ip agaisnt ip_name
#
    for i in range(0,len(ip_name)):
#        print ( "ip_name[i]= }}", ip_name[i], "{{  iipp: }}", ip, "{{ <p>" ) # %%%%%%%%%%%%%%%%%%%%%
        if ( ip == ip_name[i] ):
             return 1
    return 0
#
# ------------------------------------------------------------------------
#
def check_lim ( config, server_name, rem_ip ):
# ************************************************************************
# *                                                                      *
# *   Routine check_lim checks whether the total number of running malo  *
# *   ondemand processs and the number of processes running from a given *
# *   IP is within limits. The file with limits is in                    *
# *   $MALO_DIR/share/malo_lim.txt .
# *                                                                      *
# *  ### 08-JUL-2015   check_lim   v1.0 (c)  L. Petrov  08-JUL-2015 ###  *
# *                                                                      *
# ************************************************************************
    import socket

#
# --- Build the unlimited ip file name
#
    lim_file = config.malo_share + "/malo_lim.txt"
  
#
# --- read it
#
    with open(lim_file) as f:
         buf = f.readlines()

    lim_sim = 11
    lim_tot = 11 
    for line in buf:
        if ( line.split()[0] == server_name ):
              lim_sim = int(line.split()[1])
              lim_tot = int(line.split()[2])

    with open(config.ondemand_queue) as f:
         buf = f.readlines()

    num_proc_tot = 0
    num_proc_ip  = 0
    for line in buf:
        if ( len(line.split()) > 9 ):
             if ( line.split()[9] == 'R' ):
                  num_proc_tot = num_proc_tot + 1
                  if ( line.split()[7] == rem_ip ):
                       num_proc_ip  = num_proc_ip + 1

    if ( num_proc_tot >= lim_tot ):
         print ( '</HTML>' )
         print ( 'The total limit of concurrently running processes for loading', \
                 'computation, %d, is exceeded. Please try later.' % lim_tot )
         print ( '<P>' )
         print ( 'Loading computation is a CPU-intensive procedure and therefore, ' \
                 'is a subject of resource availability.' )
         print ( '<P>' )
         print ( '</BODY>' )
         print ( '</HTML>' )
         return 1

    if ( num_proc_ip >= lim_sim ):
         print ( '</HTML>' )
         print ( 'The limit of concurrently running processes for loading', \
                 'computation from a given IP address %d, %d, is exceeded. ' \
                 'Please try later after your previous process completes.' \
                  % (rem_ip, lim_sim ) )
         print ( '<P>' )
         print ( 'Loading computation is a CPU-intensive procedure and therefore, ' \
                 'is a subject of resource availability.' )
         print ( '</BODY>' )
         print ( '</HTML>' )
         return 1

    return 0
