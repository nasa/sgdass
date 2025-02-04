#!/usr/bin/env python3
import  sys, os, subprocess, signal, time, datetime, gzip, bz2, lzma, argparse
vsdc_swin_gentar__label = "vsdc_swin  1.0  of 2020.10.13"

def vsdc_exe_com ( command ):
    """
    Auxiliary routine vsdc_exe_com spawns a subprocess, 
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
def vsdc_exe_pipe ( command ):
    """
    Auxiliary routine vsdc_exe_pipe spawns a subprocess, 
    executes a shell command as a pipe and returns 
    stdout and stderr as lists of ascii strings.
    NB: if it cannot decode the output as ascii string,
    it returns stdout or stder as binary lists.
    """
    
    (st_out, st_err) = subprocess.Popen( command, shell=True,
                            stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE).communicate()
    out = []
    try:
        for line in st_out.decode("ascii").split("\n"):
            out.append ( line.strip("\n") )
    except:
        out = st_out

    err = []
    try:
         for line in st_err.decode("ascii").split("\n"):
             out.append ( line.strip("\n") )
    except:
        out = st_err

    return ( out, err )

#
# ------------------------------------------------------------------------
#
def mjd_tim_to_time ( mjd, tim ):
    """
    Transform a pair mjd, tim into Python time
    """
    date_j2000 = datetime.datetime.strptime ( "2000", "%Y" )
    if ( mjd < 30000 or mjd > 70000 ):
         print ( "mjd %d is out of range [30000, 70000]" % mjd )
         return None
    if ( tim < -0.000001 or tim > 86400.001 ):
         print ( "tim %f is out of range [-0.001, 86400.001]" % tim )
         return None
    return ( date_j2000 + datetime.timedelta ( days=(mjd - 51544), \
                                               seconds=tim ) )

#
# ------------------------------------------------------------------------
#
def read_file ( file_name ):
    """"
    Auxiliary program read_file reads a plain ascii file and
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
# ---------- This file is compressed with unix utility compress. 
# ---------- gzip understands this format and uncompresses it.
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
def vsdc_anc_check ( anc_magic, anc_file):
    """
    Routine vsdc_anc_check checks a contents of the VLBI antenna calibration
    file.
    """

    exper_name = None
    date_start = None
    date_stop  = None
    sta_name_short = None
    sta_name_long  = None

#
# --- Check whether the antcal file exists
#
    if ( not os.path.isfile ( anc_file ) ):
         return ( "Input file %s does not exist" % anc_file, \
                   exper_name, sta_name_short, sta_name_long, date_start, date_stop )

    anc = read_file ( anc_file )
    if ( not anc ):
         return ( "Error in reading input file %s" % anc_file, \
                   exper_name, sta_name_short, sta_name_long, date_start, date_stop )
        
    if ( not ( anc_magic in anc[0] ) ):
         if ( len(anc[0]) > 80 ):
              anc[0] = anc[0][0:80]
         return ( "Wrong magic in the input file %s -- %s, while %s was expected" % \
                   ( anc_file, anc[0], anc_magic ), \
                   exper_name, sta_name_short, sta_name_long, date_start, date_stop )

    nl = 0
    for line in anc:
        nl = nl + 1
        if ( len(line) < 2    ): continue
        if ( line[0:1] == "#" ): continue
#
        if   ( line.split()[0] == "STATION:" ):
               if ( len(line.split()) < 3 ):
                    return ( "Two few values at line %d line of the input " \
                             "file %s: %d, while %d were expected" % \
                             ( nl, anc_file, len(line.split()), 3 ), \
                             exper_name, sta_name_short, sta_name_long, date_start, date_stop )
               sta_name_short = line.split()[1]
               sta_name_long  = line.split()[2]
        elif ( line.split()[0] == "EXP_CODE:" ):
               if ( len(line.split()) < 2 ):
                    return ( "Two few values at line %d line of the input " \
                             "file %s: %d, while %d were expected" % \
                             ( nl, anc_file, len(line.split()), 2 ), \
                             exper_name, sta_name_short, sta_name_long, date_start, date_stop )
               exper_name = line.split()[1]
        elif ( line.split()[0] == "UTC_MTAI:" ):
               continue
        elif ( line.split()[0] == "FILLERS:" ):
               continue
        elif ( line.split()[0] == "NUM_PROVENANCE:" ):
               continue
        elif ( line.split()[0] == "PROVENANCE:" ):
               continue
        elif ( line.split()[0] == "NUM_DATA_ON:" ):
               continue
        elif ( line.split()[0] == "DATA_ON:" ):
               if ( len(line.split()) < 6 ):
                    return ( "Two few values at line %d line of the input " \
                             "file %s: %d, while %d were expected" % \
                             ( nl, anc_file, len(line.split()), 6 ), \
                             exper_name, sta_name_short, sta_name_long, date_start, date_stop )
               word2 = line.split()[2]
               if ( len(word2) > 19 ): word2= word2[0:19]
               extr_date = datetime.datetime.strptime( word2.replace("-","_").replace("T","_"), \
                                                       "%Y.%m.%d_%H:%M:%S" )
               if ( not date_start ):
                    date_start = extr_date
               else:
                    if ( extr_date < date_start ):
                         date_start = extr_date
               word3 = line.split()[2]
               if ( len(word3) > 19 ): word3= word3[0:19]
               extr_date = datetime.datetime.strptime( word3.replace("-","_").replace("T","_"), \
                                                       "%Y.%m.%d_%H:%M:%S" )
               if ( not date_stop ):
                    date_stop = extr_date
               else:
                    if ( extr_date > date_stop ):
                         date_stop = extr_date 
                         
        elif ( line.split()[0] == "NUM_CABLE_SIGN:" ):
               continue
        elif ( line.split()[0] == "CABLE_SIGN:" ):
               continue
        elif ( line.split()[0] == "NUM_CABLE:" ):
               continue
        elif ( line.split()[0] == "CABLE:" ):
               continue
        elif ( line.split()[0] == "NUM_METEO:" ):
               continue
        elif ( line.split()[0] == "METEO:" ):
               continue
        elif ( line.split()[0] == "NUM_TP_SENSOR:" ):
               continue
        elif ( line.split()[0] == "TP_SENSOR:" ):
               continue
        elif ( line.split()[0] == "NUM_TSYS:" ):
               continue
        elif ( line.split()[0] == "TSYS:" ):
               continue
        elif ( line.split()[0] == "NUM_TPI:" ):
               continue
        elif ( line.split()[0] == "TPI:" ):
               if ( len(line.split()) < 6 ):
                    return ( "Two few values at line %d line of the input " \
                             "file %s: %d, while %d were expected" % \
                             ( nl, anc_file, len(line.split()), 11 ), \
                             exper_name, sta_name_short, sta_name_long, date_start, date_stop )
               word2 = line.split()[2]
               if ( len(word2) > 19 ): word2= word2[0:19]
               extr_date = datetime.datetime.strptime( word2.replace("-","_").replace("T","_"), \
                                                       "%Y.%m.%d_%H:%M:%S" )
               if ( not date_start ):
                    date_start = extr_date
               else:
                    if ( extr_date < date_start ):
                         date_start = extr_date
               word3 = line.split()[2]
               if ( len(word3) > 19 ): word3= word3[0:19]
               extr_date = datetime.datetime.strptime( word3.replace("-","_").replace("T","_"), \
                                                       "%Y.%m.%d_%H:%M:%S" )
               if ( not date_stop ):
                    date_stop = extr_date
               else:
                    if ( extr_date > date_stop ):
                         date_stop = extr_date 
        elif ( line.split()[0] == "NUM_PC_SENSOR:" ):
               continue
        elif ( line.split()[0] == "PC_SENSOR:" ):
               continue
        elif ( line.split()[0] == "NUM_PCAL:" ):
               continue
        elif ( line.split()[0] == "PCAL:" ):
               continue
        elif ( line.split()[0] == "NUM_FMTGPS:" ):
               continue
        elif ( line.split()[0] == "FMTGPS:" ):
               continue
        elif ( line.split()[0] == "NUM_SEFD:" ):
               continue
        elif ( line.split()[0] == "SEFD:" ):
               continue
        elif ( line.split()[0] == "NUM_TCAL:" ):
               continue
        elif ( line.split()[0] == "TCAL:" ):
               continue
        elif ( line.split()[0] == "NUM_OPACITY:" ):
               continue
        elif ( line.split()[0] == "OPACITY:" ):
               continue
        elif ( line.split()[0] == "NUM_TCAL:" ):
               continue
        elif ( line.split()[0] == "NUM_TATM:" ):
               continue
        elif ( line.split()[0] == "TATM:" ):
               continue
        elif ( line.split()[0] == "NUM_TMOD:" ):
               continue
        elif ( line.split()[0] == "TMOD:" ):
               continue
        else:
           return ( "Unsupported keyword %s was founed at %9d line of the input file %s" % \
                   ( line.split()[0], nl, anc_file ), \
                   exper_name, sta_name_short, sta_name_long, date_start, date_stop )

             
    return ( "OK", exper_name, sta_name_short, sta_name_long, date_start, date_stop )


#
# ------------------------------------------------------------------------
#
def vsdc_antcal_check_server ( anc_magic, anc_file ):
    try:
          (ret, exper_name, sta_name_short, sta_name_long, date_start, date_stop ) = \
           vsdc_anc_check ( anc_magic, anc_file  )
    except Exception as e: 
           print ("FATAL: Unknown Error Occurred in vsdc_anc_check: " + str(e) )
           ret = "failed"
    if ( ret == "OK" ):
         return [True, exper_name, sta_name_short, sta_name_long, date_start, date_stop ]
    else: 
         return [False, None, "FATAL: Error Occurred in vsdc_swin_check: " + ret, \
                 None, None, None, None, None ]

