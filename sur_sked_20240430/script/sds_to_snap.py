#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program sds_to_snap translates a single dish schedule sds to snap  *
# *   format.                                                            *
# *                                                                      *
# * ###  12-JAN-2021   sds_to_snap  v1.29 (c) L. Petrov 06-MAY-2024  ### *
# *                                                                      *
# ************************************************************************
import  pwd, sys, os, math, shutil, time, subprocess, datetime, optparse, argparse
from    optparse import OptionParser, Option, IndentedHelpFormatter
from    sur_sked_config import * # Import sur_sked confuguration
from    pet_misc        import *
#
sds_to_snap__label = "sds_to_snap 1.29 2024.05.06"
sds__mlev = 8

stp_dir  = sur_sked_stp

class stp_class:
   def __init__ ( self ):
       self.filename    = None
       self.exp_name    = None
       self.sch_vers    = None
       self.short_name  = None
       self.last_update = None
       self.sta_name    = None
       self.slew_az     = None
       self.slew_el     = None
       self.accl_az     = None
       self.accl_el     = None
       self.tsettle_az  = None
       self.tsettle_el  = None
       self.el_range    = None
       self.preob       = None
       self.postob      = None
       self.recorder    = None
       self.mount       = None
       self.presess     = 120.0
       self.stow_azim   = None
       self.stow_elev   = None
       self.coord       = []
       self.az_range    = []
       self.hor_azim    = []
       self.hor_elev    = []
   def init ( self ):
       __init__ ( self )

sds_funs = { "@add":    2, \
             "@dif":    2, \
             "@mul":    2, \
             "@div":    2, \
             "@int":    1, \
             "@01dint": 1, \
             "@02dint": 1, \
             "@03dint": 1, \
             "@04dint": 1, \
             "@lenarr": 1  \
           }

#
# ------------------------------------------------------------------------
#
def exe ( command ):
    """
    Auxilliary routine exe spawns a supborcess, 
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
# ---------- This file is compressed with lzma
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
def parse_stp ( stp_dir, stp ):
    """
    Parse the station parameter file. The result of parsing is put in 
    data structure stp. Some stp fields, such as station name, is already 
    present.
    """
    stp.filename = stp_dir + "/" + stp.sta_name + ".stp"
 
    if ( not os.path.isfile ( stp.filename ) ):
         print ( "Cannot find station parameter file " + stp.filename )
         exit ( 1 )

#
# --- Read stp file and put resultsd in stp_buf
#
    stp_buf = read_file ( stp.filename )

    na = 0
    ne = 0
    for line in stp_buf:
        if ( line[0:1] == "#" ): continue
        if ( len(line) ==  0  ): continue
        if (   line.split()[0] == "SHORT_NAME:" ):
               stp.short_name = line.split()[3]
        if (   line.split()[0] == "LAST_UPDATE:" ):
               stp.last_update = line.split()[2]
        if (   line.split()[0] == "SLEW_AZ:" ):
               stp.slew_az = float ( line.split()[3] )
        elif ( line.split()[0] == "SLEW_EL:" ):
               stp.slew_el = float ( line.split()[3] )
        if (   line.split()[0] == "ACCL_AZ:" ):
               stp.accl_az = float ( line.split()[3] )
        elif ( line.split()[0] == "ACCL_EL:" ):
               stp.accl_el = float ( line.split()[3] )
        if (   line.split()[0] == "TSETTLE_AZ:" ):
               stp.tsettle_az = float ( line.split()[3] )
        elif ( line.split()[0] == "TSETTLE_EL:" ):
               stp.tsettle_el = float ( line.split()[3] )
        elif ( line.split()[0] == "EL_MIN:" ):
               stp.el_min = float ( line.split()[3] )
        elif ( line.split()[0] == "EL_MAX:" ):
               stp.el_max = float ( line.split()[3] )
        elif ( line.split()[0] == "PREOB:" ):
               stp.preob = float ( line.split()[3] )
        elif ( line.split()[0] == "POSTOB:" ):
               stp.postob = float ( line.split()[3] )
        elif ( line.split()[0] == "RECORDER:" ):
               stp.recorder = line.split()[3]
        elif ( line.split()[0] == "MOUNT:" ):
               stp.mount = line.split()[3]
        elif ( line.split()[0] == "COORD:" ):
               stp.coord = []
               stp.coord.append ( float(line.split()[3]) )
               stp.coord.append ( float(line.split()[4]) )
               stp.coord.append ( float(line.split()[5]) )
        elif ( line.split()[0] == "AZ_RANGE:" ):
               stp.az_range = []
               stp.az_range.append ( float ( line.split()[3] ) )
               stp.az_range.append ( float ( line.split()[4] ) )
               stp.az_range.append ( float ( line.split()[5] ) )
               stp.az_range.append ( float ( line.split()[6] ) )
        elif ( line.split()[0] == "HOR_AZIM:" ):
               stp.hor_azim = []
               na = len(line.split())-3 
               for i in range(0,na):
                   stp.hor_azim.append( float(line.split()[3+i]) )
        elif ( line.split()[0] == "HOR_ELEV:" ):
               stp.hor_elev = []
               ne = len(line.split())-3 
               for i in range(0,ne):
                   stp.hor_elev.append( float(line.split()[3+i]) )
        elif ( line.split()[0] == "STOW_AZIM:" ):
               stp.stow_azim = float(line.split()[3])
        elif ( line.split()[0] == "STOW_ELEV:" ):
               stp.stow_elev = float(line.split()[3])

    if ( ne != na ):
         print ( "Error in parsing stp file %s: the number of nodes in " \
                 "definition of horizontal mask for azimuths is %d, but for " \
                 "elevations is %d" % ( stp.filename, na, ne ) )
    if ( ne == 0 ):
         stp.hor_azim = [0, 360.0]
         stp.hor_elev = [stp.el_min, stp.el_max]
         na = 2
         ne = 2 

    return ( stp )

#
# ------------------------------------------------------------------------
#
def sds_to_snap ( filin, stp, start_time, catchup, ivrb ):
    """
    Main routine of the converter
    """

#
# --- Read the input schedule file. The result is put in a character array sds
#
    sds = read_file ( filin )
    if ( not sds ):
         print ( "Input sds file %s does not exist" % filin )
         exit  ( 1 )
#
# --- The first run: we search for the station name, experiment name,
# --- Start time etc
#
    wait_before_start = ""
    fl_cycle = 0
    macros = {}
    array = {}
    fl_start_time = 0
    nl = 0
    require_stp_version = "??"
    sch_type            = "??"
    pi_name             = "??"
    pi_email            = "??"
    for line in sds:
        nl = nl + 1
        if ( line[0:1] == '#' ): continue
        if ( len(line.split()) == 0 ): continue
#
        if ( line.split()[0] == "exper"  or \
             line.split()[0] == "macros" or \
             line.split()[0] == "array" or \
             line.split()[0] == "require_sds_to_snap_version" or \
             line.split()[0] == "require_stp_version"            ):

             if ( len(line.split()) < 2 ):
                  print ( "Error in processing line %d of the input file %s: " \
                          "keyword %s must have 2 values but has %d" % \
                          ( nl, filin, line.split()[0], len(line.split())+1 ) )
                  exit ( 1 )
             elif ( len(line.split()) < 1 ):
                  print ( "Error in processing line %d of the input file %s: " \
                          "keyword %s must have 2 values but has %d" % \
                          ( nl, filin, line.split()[0], len(line.split())+1 ) )
                  exit ( 1 )

        if ( line.split()[0] == "require_sds_to_snap_version" ):
             req_vers  = line.split()[1]
             used_vers = sds_to_snap__label.split()[2]
             if ( req_vers > used_vers ):
                  print ( "You are trying to use sds_to_snap version %s, but version %s is required. Please upgrade sds_to_snap" %
                          ( used_vers, req_vers) )
                  exit ( 1 )

        elif ( line.split()[0] == "require_stp_version" ):
             require_stp_version  = line.split()[1]

        elif ( line.split()[0] == "type" ):
             sch_type = line.split()[1]

        elif ( line.split()[0] == "pi_name" ):
             pi_name = ""
             for word in line.split():
                 if ( word != "pi_name" ):
                      pi_name = pi_name + word + " " 
    
        elif ( line.split()[0] == "pi_email" ):
             pi_email = line.split()[1]
    
        elif ( line.split()[0] == "duration" ):
             duration = line.split()[1]

        elif ( line.split()[0] == "exper" ):
#
# ---------- Extract experimetn name and schedule version
#
             if ( len(line.split()) < 3 ):
                  print ( "Experiment version is not specified. Please update line with exper" )
                  exit  ( 1 )
             stp.exp_name = line.split()[1] 
             stp.sch_vers = line.split()[2] 
             if ( ivrb > 3 ):
                  print ( "Experiment name:     ", stp.exp_name )
                  print ( "Experiment version:  ", stp.sch_vers )
        elif ( line.split()[0] == "station" ):
#
# ---------- Extract station name
#
             stp.sta_name = line.split()[1].lower()
             if ( ivrb > 3 ):
                  print ( "Station name:        ", stp.sta_name )
        elif ( line.split()[0] == "start" ):
#
# ---------- Extract the stat time
#
             if ( fl_start_time != 0 ):
                  print ( "Error in processing line %d of the input file %s: " \
                          "keyword %s must be defined only once" % ( nl, "start_time", filin ) )
                  exit ( 1 )
             if ( start_time == None ):
#
# --------------- User did not specified start time. We get it from the schedule file
#
                  try:
                      if ( len(line.split()[1]) > 19 ):
                           start_time = datetime.datetime.strptime ( line.split()[1], "%Y.%m.%d_%H:%M:%S.%f" )
                      elif ( len(line.split()[1]) == 19 ):
                           start_time = datetime.datetime.strptime ( line.split()[1], "%Y.%m.%d_%H:%M:%S" )
                      elif ( len(line.split()[1]) == 16 ):
                           start_time = datetime.datetime.strptime ( line.split()[1], "%Y.%m.%d_%H:%M" )
                      elif ( len(line.split()[1]) == 13 ):
                           start_time = datetime.datetime.strptime ( line.split()[1], "%Y.%m.%d_%H" )
                      elif ( "!" in line.split()[1] ):
                           start_time = datetime.datetime.strptime ( "2000.01.01_00", "%Y.%m.%d_%H" )
                           wait_before_start = line.split()[1]
                      else:
                           print ( "Wrong start date format: ", line.split()[1] )
                           exit  ( 1 )
                  except Exception as e:
                      print ( "Error in parsing start date %s -- %s " % ( line.split()[1], str(e) ) )
                      exit  ( 1 )

             fl_start_time = 1
        elif ( line.split()[0] == "macros" ):
            if ( ivrb > 2 ):
                 print ( "Adding macros: ", line.split()[1] )
            macros[line.split()[1]] = line.split()[2]
        elif ( line.split()[0] == "array" ):
            if ( ivrb > 2 ):
                 print ( "Adding array ", line.split()[1] )
            array[line.split()[1]] = line.split()[1]
            line = line.replace("{"," ").replace("}"," ").replace(","," ").replace("}"," ")
            arr = []
            for k in range(0,len(line.split())):
                if ( k > 1 ):
                     arr.append ( line.split()[k] )
            array[line.split()[1]] = arr
        elif ( line.split()[0] == "cycle" ):
            fl_cycle = fl_cycle + 1

    if ( not stp.sta_name ):
         print ( "Input schedule %s does not have keyword station" % filin )
         exit ( 1 )

    if ( not start_time ):
         print ( "Input schedule %s does not have keyword start" % filin )
         exit ( 1 )

    if ( require_stp_version == "??" ):
         print ( "Parameter require_stp_version is not defined in the input file %s" % filin )
         exit ( 1 )

#
# --- Unravel cycles
#
    if ( fl_cycle > 0 ):
         for k in range(1,fl_cycle+1):
             nline = 0
             cycle_var = "??"
             cycle_nline = 0
#
# ---------- First collect information about the cycle
# ---------- NB: If there more than one cycle, the last
# ---------- unprocessed cycle will be processed at this
# ---------- level of iterations
#
             for line in sds:
                 nline = nline + 1
                 if ( len(line.split()) < 3 ): continue
                 if ( line.split()[0] == "cycle" ):
                      (line,fun_val) = sds_proc_fun ( line, array )
                      if ( len(line.split()) < 4 ):
                           print ( "Too few words in line %s" % line )
                           exit  ( 1 )
                      cycle_var = line.split()[1]           
                      cycle_min = int ( line.split()[2] )
                      cycle_max = int ( line.split()[3] )
                      cycle_nline = nline
                 elif ( line.split()[0] == "end"   and \
                        line.split()[1] == "cycle" and \
                        line.split()[2] == cycle_var   ):
                      cycle_nline = ( cycle_nline, nline )

             if ( cycle_var == "??" ):
                  print ( "Syntax error at line %d" % line )
                  exit  ( 1 )
        
             sds_new = []
#
# ---------- Lines before the cycle
#
             try:
                k = cycle_nline[0]
             except Exception as e:
                print ( "Syntax error in parsing schedule file: cycle with variable %s does not have a matching end cycle statement" % cycle_var )
                exit  ( 1 )

             for nl in range(0,cycle_nline[0]-1):
                 sds_new.append ( sds[nl] )
#
# ---------- Lines within the cycle
#
             for ind in range(cycle_min,cycle_max+1):
                 for nl in range(cycle_nline[0],cycle_nline[1]-1):
                     new_line = sds[nl].replace("${%s}"%cycle_var,"%d" % ind).replace("$%s"%cycle_var,"%d" % ind)
                     sds_new.append ( new_line  )
#
# ---------- Lines after the cycle
#
             for nl in range(cycle_nline[1],len(sds)):
                 sds_new.append ( sds[nl] )
             sds = sds_new
          
#
# --- Parse the stat ion parameter file
#
    stp = parse_stp ( sur_sked_stp, stp )

    if ( stp.last_update[0:10] < require_stp_version[0:10] ):
         print ( "You are trying to use station parameter file (stp) %s version %s, but version %s is required. Please your file" % \
                 ( stp.filename, stp.last_update, require_stp_version ) )
         exit ( 1 )
#
# --- The second run
#
# --- Put the header in the snap file. The header is considered comment
#
    com_line = ""
    for word in sys.argv:
        com_line = com_line + " " + word

    snap = []
    snap.append ( '" Control file for a single dish experiment' )
    snap.append ( '" ' )    
    snap.append ( '" Generated by ' + sds_to_snap__label )
    snap.append ( '" Created at %s by %s (%s)' % ( datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23], \
                                                   pwd.getpwuid(os.getuid()).pw_name, \
                                                   pwd.getpwuid(os.getuid()).pw_gecos ) )
    snap.append ( '" Created with ' + com_line )
    snap.append ( '" ' )
    snap.append ( '" ' + "Experiment: %s   Schedule version: %s   Schedule type: %s" % ( stp.exp_name, stp.sch_vers, sch_type ) )
    snap.append ( '" ' + "PI:         %s   email: %s" % ( pi_name, pi_email ) )
    snap.append ( '" ' )
    snap.append ( '" Start date: %s ' % datetime.datetime.strftime( start_time, "%Y.%m.%d_%H:%M:%S" )[:19] )
    snap.append ( '" ' )
    snap.append ( '" The following slewing parameters were used for station ' + stp.sta_name )
    snap.append ( '" ' )
    snap.append ( '" >> BEGINNING OF THE STATIONS DEFINITION FILE ' + stp.filename )
    stp_buf = read_file ( stp.filename )
    for line in stp_buf:
        snap.append ( '" ' + line )
    snap.append ( '" >> END OF THE STATIONS DEFINITION FILE ' + stp.filename )
    snap.append ( '" ' )
    proc = []

#
# --- Initiazation
#
    az_acc = stp.stow_azim 
    el_acc = stp.stow_elev
    obs_time = start_time - datetime.timedelta ( seconds=stp.presess )
    nl = 0
    fl_description = False
    fl_proc        = False
    nsca = 0
#
    for line in sds:
        nl = nl + 1
        if ( ivrb > 3 ):
             if ( "Last updated" in line ):
                  print ( "Line %4d: %s" % (nl,line) )
        if ( line[0:1] == '#'       ): continue
        if ( len(line.split()) == 0 ):
             if ( fl_description ):
                   snap.append ( '"@' )
             if ( fl_proc ):
                   snap.append ( '"%' )
             continue
#
        if ( fl_description ):
             if ( line.split()[0] == "description_end" ):
                  if ( ivrb > 3 ):
                       print ( "Line %4d, found end of the experiment description definition block" % nl )
                  fl_description = False
                  snap.append ( '" ' )
                  snap.append ( '" << END OF THE EXPERIMENT DESCRIPTION' )
                  snap.append ( '" ' )
             else:
                  snap.append ( '"@ ' + line )
             continue
        if ( fl_proc ):
             if ( line.split()[0] == "procedure_end" ):
                  if ( ivrb > 3 ):
                       print ( "Line %4d, found end of the procedure definition block" % nl )
                  fl_proc = False
                  snap.append ( '" ' )
                  snap.append ( '" << END OF THE PROCEDURE FILE' )
                  snap.append ( '" ' )
             else:
                  snap.append ( '"% ' + line )
                  proc.append ( line )
             continue

        if ( "[" in line and "]" in line ):
             for k in range(0,8):
                 for word in line.split():
                     for i in range(0,4):
                         if ( "[" in word ):
                              ib = word.find("[")
                              ie = word.find("]")
                              if ( ib  > 0 ):
                                   if ( "," in word ):
                                        ibc = word[0:ib].find(",") + 1
                                   else:
                                        ibc = 0
                               
                                   if ( "(" in word ):
                                        ibp = word[0:ib].find("(") + 1
                                   else:
                                        ibp = 0
                               
                                   if ( ")" in word ):
                                        ibpp = word[0:ib].find(")") + 1
                                   else:
                                        ibpp = 0
                                   ibu = max ( ibc, ibp, ibpp )
                               
                                   arr_nam = word[ibu:ib] 
                                   arr_sub = word[ibu:ie+1] 
                                   if ( word[ibu:ib] in array.keys() ):
                                        var  = word[ib+1:ie]
                                        word = word.replace(arr_sub,array[arr_nam][int(var)-1])[ibu:]
                                        line = line.replace(arr_sub,array[arr_nam][int(var)-1])

        if ( ivrb > 4 ):
             print ( "Line: ", line )
        if   ( line.split()[0] != "macros" and line.split()[0] != "pi_email" ):
               if ( ivrb > 2 ):
                    print ( "Processing line: ", line )
                    print ( "mac ", macros )
               (line,fun_val) = sds_proc_fun   ( line, array )


        if   ( line.split()[0] == "start" ):
#
# ---------- Start command
#
             if ( len(line.split()) < 4 ):
                  print ( "Error in priocessing line %d -- to few words " % nl,
                          " after keyword start, while 4 words is expected" )
                  exit ( 1 )
             if ( line.split()[2] == "stow" or line.split()[2] == "do_not_move" ):
                  az = stp.stow_azim
                  el = stp.stow_elev
             else:
                  az = float ( line.split()[2] )
                  el = float ( line.split()[3] )

             fl_move = True
             if ( line.split()[2] == "do_not_move" ):
                  fl_move = False

             (ret, out) = sds_check_azel ( filin, stp, nl, az, el )
             if ( ret != 0 ):
                  print ( out )
                  exit  ( 1 )
             obs_time_str = datetime.datetime.strftime( obs_time, "%Y.%j.%H:%M:%S.%f" )[:20]
             if ( not catchup ):
                  snap.append ( '" Move antenna to the first source of the schedule and wait for the schedule start' )
                  snap.append ( '!' + obs_time_str )
             (slew_time, az_acc, el_acc, wrap) = sds_point ( stp, nl, az_acc, el_acc, az, el, ivrb )
             if ( wait_before_start == "" ):
                  obs_time = start_time
                  obs_time_str = datetime.datetime.strftime( obs_time, "%Y.%j.%H:%M:%S.%f" )[:20]
                  if ( catchup ):
#
# -------------------- The catchup was requested. Check whether we can catch up the schedule
#
                       new_start_time = datetime.datetime.utcnow() + datetime.timedelta ( seconds=catchup )
                       if ( obs_time < new_start_time ):
                            if ( ivrb > 0 ):
                                 print ( "At %s skipped command %s " % ( obs_time_str, line ) )
                            continue
                       else:
                            snap.append ( '" Move antenna to the first source of the catchuip schedule and wait for resuming' )
                            catchup = None
             else:
                 if ( wait_before_start[0:1] == "!" ):
                      snap.append ( wait_before_start )
                 else:
                      snap.append ( "!" + wait_before_start )
             if ( fl_move ):
                  snap.append ( str('source=azel,%10.5fd,%8.5fd,,%s' % ( az, el, wrap )).replace(" ","") )
             snap.append ( '!' + obs_time_str )
        elif ( line.split()[0] == "exper" ):
#
# ---------- Exper command
#
             snap.append ( '" Single dish experiment: ' + line.split()[1] )
             snap.append ( '" ' )
        elif ( line.split()[0] == "station" ):
#
# ---------- Station command
#
             snap.append ( '" Station: ' + line.split()[1] )
        elif ( line.split()[0] == "wait_slew" ):
#
# ---------- wait_skew command
#
             if ( len(line.split()) >= 2 ):
                  dur_time = float ( line.split()[1] )
                  obs_time = obs_time + datetime.timedelta ( seconds=dur_time )
             if ( catchup ):
#
# --------------- The catchup was requested. Check whether we can catch up the schedule
#
                  new_start_time = datetime.datetime.utcnow() + datetime.timedelta ( seconds=catchup )
                  if ( obs_time < new_start_time ):
                       if ( ivrb > 0 ):
                            obs_time_str = datetime.datetime.strftime( obs_time, "%Y.%j.%H:%M:%S.%f" )[:20]
                            print ( "At %s skipped command %s " % ( obs_time_str, line ) )
                       continue
             obs_time_str = datetime.datetime.strftime( obs_time, "%Y.%j.%H:%M:%S.%f" )[:20]
             snap.append ( '!' + obs_time_str )
        elif ( line.split()[0] == "!" ):
             obs_inp_time_str = line.replace("!","").split()[0][0:19]
             try:
                 if ( obs_inp_time_str[10:11] == "-" ):
                      obs_time = datetime.datetime.strptime( obs_inp_time_str, "%Y.%m.%d-%H:%M:%S" )
                 else:
                      obs_time = datetime.datetime.strptime( obs_inp_time_str, "%Y.%m.%d_%H:%M:%S" )
                 obs_time_str = datetime.datetime.strftime( obs_time, "%Y.%j.%H:%M:%S.%f" )[:20]
             except Exception as e:
                 print ( "Eror in line %s while parsing the date %s" % \
                         (nl, obs_inp_time_str ) )
             snap.append ( '!' + obs_time_str )
        elif ( line.split()[0] == "proc" ):
             if ( len(line.split()) < 3 ):
                  print ( "Error in processing line %d -- Less than three words. Line %s" % \
                          (nl,line) )
                  exit ( 1 )
#
# ---------- Proc command. A line in the schedule file may contain more than one 
# ---------- procedure call separated with ";" delimiter
#
             fl_skip = False
             if ( catchup ):
#
# --------------- The catchup was requested. Check whether we can catch up the schedule
#
                  new_start_time = datetime.datetime.utcnow() + datetime.timedelta ( seconds=catchup )
                  if ( obs_time < new_start_time ):
                       if ( ivrb > 0 ):
                            obs_time_str = datetime.datetime.strftime( obs_time, "%Y.%j.%H:%M:%S.%f" )[:20]
                            print ( "At %s skipped command %s " % ( obs_time_str, line ) )
                       fl_skip = True
#
# ---------- Concatenate the string of snap commands that may or may not have blanks
# ---------- wit string proc_val that is separated with ";" only.
#
             proc_val = ""
             for i in range(1,len(line.split())-1):
                 proc_val = proc_val + line.split()[i] + ";"
                 proc_val = proc_val.replace(";;",";")

             if ( "(" in proc_val ):
#
# --------------- The procedure value is a macros. Expand the macros
#
                  proc_macros_name = proc_val.replace("("," ").split()[0]
                  if ( not proc_macros_name in macros.keys() ):
                       print ( "Error in processing line %d -- undefined unknown macros %s was found" % \
                                ( nl, macros_name ) )
                       exit ( 1 )
                  num_macros_vals = proc_val.count(",") + 1
                  macros_arg = proc_val.replace(proc_macros_name,"").replace("("," ").replace(")"," ").replace(","," ")
                  proc_val   = macros[proc_macros_name]
                  for i in range(0,num_macros_vals):
                      proc_val = proc_val.replace("{$%d}"%(i+1),macros_arg.split()[i])

                  if ( "scan" in proc_val ):
                       nsca = nsca + 1
                       proc_val = proc_val.replace("${scan}","%04d"%nsca)
                       (proc_val,fun_val) = sds_proc_fun  ( proc_val, array )
                  if ( "$" in proc_val ):
                       print ( "Error in processing line %d -- not all macros arguments are expanded. line %s" % \
                                (nl,line) )
                       print ( "proc_val= ", proc_val )
                       exit ( 1 )
             else:
                  if ( "scan" in proc_val ):
                       nsca = nsca + 1
                       proc_val = proc_val.replace("${scan}","%04d"%nsca)

#
# ---------- Put the snap command in the snap list
#
             for com in proc_val.split(";"):
                 if ( "scan_name=" in com ):
                      com = com + "," + \
                            stp.exp_name.lower() + "," + \
                            line.split()[len(line.split())-1] + "," + \
                            line.split()[len(line.split())-1]
                 if ( not fl_skip and com != " " and com != "" ):
                      snap.append ( '%s' % com )
             if ( len(line.split()) >= 3 ):
                  dur_time = float ( line.split()[len(line.split())-1] )
                  obs_time = obs_time + datetime.timedelta ( seconds=dur_time )
        elif ( line.split()[0] == "logon" ):
#
# ---------- logon command
#
#?             snap.append ( 'log=' + stp.exp_name + '.log' )
             snap.append ( 'tpicd=yes,100' )
             snap.append ( 'tpicd'         )
             snap.append ( 'start_mlog'    )
        elif ( line.split()[0] == "logoff" ):
#
# ---------- logoff command
#
             snap.append ( 'tpicd=stop' )
             snap.append ( 'stop_mlog'  )
        elif ( line.split()[0] == "point_azel" or line.split()[0] == "stow" ):
             if ( line.split()[0] == "point_azel" ):
#
#---------------- point_azel command
#
                  try:
                       az = float ( line.split()[1] )
                  except Exception as e:
                       print ( "Error in processing azimith in line %s -- %s " % \
                               ( line, e ) )
                       exit  ( 1 )
                  try:
                       el = float ( line.split()[2] )
                  except Exception as e:
                       print ( "Error in processing elevation in line %s -- %s " % \
                               ( line, e ) )
                       exit  ( 1 )
             elif ( line.split()[0] == "stow" ):
                  az = stp.stow_azim
                  el = stp.stow_elev
             if ( len(line.split()) > 3):
                  dwell = float ( line.split()[3] )
             else:
                  dwell = None
             (ret, out) = sds_check_azel ( filin, stp, nl, az, el )
             if ( ret != 0 ):
                  print ( out )
                  exit  ( 1 )
             old_az_acc = az_acc
             old_el_acc = el_acc
             (slew_time, az_acc, el_acc, wrap) = sds_point ( stp, nl, az_acc, el_acc, az, el, ivrb )
             old_obs_time_str = datetime.datetime.strftime( obs_time,     "%Y.%j.%H:%M:%S.%f" )[:20]
             obs_time = obs_time + datetime.timedelta ( seconds=slew_time )
             fl_skip = False
             if ( catchup ):
#
# --------------- The catchup was requested. Check whether we can catch up the schedule
#
                  new_start_time = datetime.datetime.utcnow() + datetime.timedelta ( seconds=catchup )
                  if ( obs_time < new_start_time ):
                       if ( ivrb > 0 ):
                            obs_time_str = datetime.datetime.strftime( obs_time,     "%Y.%j.%H:%M:%S.%f" )[:20]
                            print ( "At %s skipped command %s " % ( obs_time_str, line ) )
                       fl_skip = True
             obs_time_str = datetime.datetime.strftime( obs_time,     "%Y.%j.%H:%M:%S.%f" )[:20]
             if ( not fl_skip ):
                  snap.append ( '" Slewed from az/el %7.1f, %5.2f to %7.2f, %6.2f since %s till %s slew_time %6.2f' % \
                              ( old_az_acc, old_el_acc, az, el, old_obs_time_str, obs_time_str, slew_time ) )
                  snap.append ( str('source=azel,%10.5fd,%8.5fd,,%s' % ( az, el, wrap )).replace(" ","") )
             if ( dwell ):
                  if ( len(line.split()) > 4 ):
                       logging = line.split()[4]
                  else:
                       logging = "none"

                  if ( logging == "onof" ):
                       obs_time_str = datetime.datetime.strftime( obs_time,     "%Y.%j.%H:%M:%S.%f" )[:20]
                       if ( not fl_skip ):
                            snap.append ( '!' + obs_time_str )
                            snap.append ( 'tpicd=yes,100' )
                            snap.append ( 'tpicd'         )
                            snap.append ( 'start_mlog'    )
                  elif ( logging != "none" ):
                       print ( "Error in processing line %d -- unsupported 4th argumement onof was expected" % nl )
                       exit ( 1 )
#
                  obs_time = obs_time + datetime.timedelta ( seconds=dwell, microseconds=(1000000 - obs_time.microsecond) )
                  obs_time_str = datetime.datetime.strftime( obs_time,     "%Y.%j.%H:%M:%S.%f" )[:20]
                  if ( not fl_skip ):
                       snap.append ( '!' + obs_time_str )
                  if ( logging == "onof" ):
                       snap.append ( 'tpicd=stop' )
                       snap.append ( 'stop_mlog'  )
             if ( line.split()[0] == "stow" ):
                  snap.append ( "stow" )
        elif ( line.split()[0] == "point_radec" ):
             if  ( len(line.split()) < 4 ):
                   print ( "Error in processing line %d -- three arguments were expected, but got %d" % \
                           ( nl, len(line.split())-1 ) )
                   exit ( 1 )
#
#----------- point_radec command
#
             sou_nam = line.split()[1] 
             ra_str  = line.split()[2].replace(":","").replace("_","")
             dec_str = line.split()[3].replace(":","").replace("_","")
             wrap    = "neutral"
             line = "source=" + sou_nam + "," + ra_str + "," + dec_str + ",2000.0," + wrap
             snap.append ( line )
        elif ( line.split()[0] == "move_azel" ):
#
# ---------- move_azel command
#
             delta_az = float ( line.split()[1] )
             delta_el = float ( line.split()[2] )
             if ( len(line.split()) > 3):
                  dwell = float ( line.split()[3] )
             else:
                  dwell = None
             (ret, out) = sds_check_azel ( filin, stp, nl, az_acc + delta_az, el_acc + delta_el )
             if ( ret != 0 ):
                  print ( out )
                  exit  ( 1 )
             old_az_acc = az_acc
             old_el_acc = el_acc
             (slew_time, az_acc, el_acc, wrap) = sds_move ( stp, nl, az_acc, el_acc, delta_az, delta_el, ivrb )
             az = az_acc
             if ( az <    0.0 ): az = az + 360.0
             if ( az >= 360.0 ): az = az - 360.0
             old_obs_time_str = datetime.datetime.strftime( obs_time,     "%Y.%j.%H:%M:%S.%f" )[:20]
             obs_time = obs_time + datetime.timedelta ( seconds=slew_time )
             obs_time_str = datetime.datetime.strftime( obs_time,     "%Y.%j.%H:%M:%S.%f" )[:20]
             fl_skip = False
             if ( catchup ):
#
# --------------- The catchup was requested. Check whether we can catch up the schedule
#
                  new_start_time = datetime.datetime.utcnow() + datetime.timedelta ( seconds=catchup )
                  if ( obs_time < new_start_time ):
                       if ( ivrb > 0 ):
                            print ( "At %s skipped command %s " % ( obs_time_str, line ) )
                       fl_skip = True
             if ( not fl_skip ):
                  snap.append ( '" Slewed from az/el %7.1f, %5.2f to %7.2f, %6.2f since %s till %s slew_time %6.2f' % \
                                ( old_az_acc, old_el_acc, az_acc, el_acc, old_obs_time_str, obs_time_str, slew_time ) )
                  snap.append ( str('source=azel,%10.5fd,%8.5fd,,%s' % ( az, el_acc, wrap )).replace(" ","") )
             if ( dwell ):
                  obs_time = obs_time + datetime.timedelta ( seconds=dwell, microseconds=(1000000 - obs_time.microsecond) )
                  obs_time_str = datetime.datetime.strftime( obs_time,     "%Y.%j.%H:%M:%S.%f" )[:20]
                  if ( not fl_skip ):
                       snap.append ( '!' + obs_time_str )
        elif ( line.split()[0] == "dwell" ):
#
# ---------- dwell command
#
             dwell = float ( line.split()[1] )
             obs_time = obs_time + datetime.timedelta ( seconds=dwell, microseconds=(1000000 - obs_time.microsecond) )
             obs_time_str = datetime.datetime.strftime( obs_time,     "%Y.%j.%H:%M:%S.%f" )[:20]
             fl_skip = False
             if ( catchup ):
#
# --------------- The catchup was requested. Check whether we can catch up the schedule
#
                  new_start_time = datetime.datetime.utcnow() + datetime.timedelta ( seconds=catchup )
                  if ( obs_time < new_start_time ):
                       if ( ivrb > 0 ):
                            print ( "At %s skipped command %s " % ( obs_time_str, line ) )
                       fl_skip = True
             if ( not fl_skip ):
                  snap.append ( '!' + obs_time_str )
        elif ( line.split()[0] == "antenna_on" ):
               snap.append ( "source=hold" )
               snap.append ( "antenna=operate" )
        elif ( line.split()[0] == "antenna_off" ):
               snap.append ( "source=disable" )
               snap.append ( "antenna=off" )
        elif ( line.split()[0] == "description_begin" ):
             if ( ivrb > 3 ):
                  print ( "Line %4d, found beginning of the description definition block" % nl )
             fl_description = True
             snap.append ( '" >> BEGINNING OF THE EXPERIMENT DESCRIPTION' )
             snap.append ( '" ' )
        elif ( line.split()[0] == "procedure_begin" ):
             if ( ivrb > 3 ):
                  print ( "Line %4d, found beginning of the procedure definition block" % nl )
             fl_proc  = True
             snap.append ( '" >> BEGINNING OF THE PROCEDURE' )
             snap.append ( '" ' )
        elif ( line.split()[0] == "description_end" ):
             fl_description = False
        elif ( line.split()[0] == "procedure_end" ):
             fl_proc = False
        elif ( line.split()[0] == "end" ):
#
# ---------- end command
#
             snap.append ( '" end of schedule ' + stp.exp_name )
             snap.append ( '" Upon submission of the log file, please send email to %s' % pi_email )
    if ( ivrb > 3 ):
         print ( "%4d lines of the input file %s were processed" % (nl, filin) )

    dur = (obs_time - start_time).seconds + 86400*(obs_time - start_time).days

    return (0,snap,proc,dur,pi_email)

#
# ------------------------------------------------------------------------
#
def snap_check ( snap ):
    nl = 0
    for line in snap:
        nl = nl + 1
        if ( line[0:1] == '"' ):
             continue          
        if ( line[0:9] == "scan_name" ):
             scan_name = line.split(",")[0].replace("scan_name=","")
             if ( len(scan_name) > 12 ):
                  print ( "Error in checking snap file: line %d has too long scan name: %s" % \
                          ( nl, scan_name ) )
                  return 1
    return 0

#
# ------------------------------------------------------------------------
#
def sds_check_azel ( filin, stp, nl, az, el ):
    """
    Check validity of azimith and elevations
    """
    if ( az == stp.stow_azim and stp.stow_elev == el ):
         return ( 0, "" )                       

    if ( az < stp.az_range[0] ):
         out = "Error in processing line %d of the input file %s: " \
               "azimuth %f is below the range %f " % \
               ( nl, filin, az, stp.az_range[0] )
         return ( 1, out )
    if ( az > stp.az_range[3] ):
         out = "Error in processing line %d of the input file %s: azimuth %f " \
               "is above the range %f " % ( nl, filin, az, stp.az_range[3] )
         return ( 2, out )
    if ( el < stp.el_min ):
         out = "Error in processing line %d of the input file %s: " \
               "elevation %f is below the range %f " % \
               ( nl, filin, el, stp.el_min )
         return ( 1, out )
    if ( el > stp.el_max ):
         out = "Error in processing line %d of the input file %s: " \
               "elevation %f is above the range %f " % \
               ( nl, filin, el, stp.el_max )
         return ( 1, out )

    for i in range (0, len(stp.hor_azim)-1):
        if ( az >= stp.hor_azim[i] and az < stp.hor_azim[i+1] ):
             if ( el < stp.hor_elev[i] ):
                  out = "Error in processing line %d of the input file %s: " \
                        "elevation %f is below the horizonal mask for azimith %f, " \
                        "which is %s" % \
                        ( nl, filin, el, az, stp.hor_elev[i] )
                  return ( 1, out )


    return ( 0, "" )                       

#
# ------------------------------------------------------------------------
#
def sds_point ( stp, nl, az_acc, el_acc, az, el, ivrb ):
    """
    Point to azimith and elevation az, el from the current az_acc, el_acc.
    Argument nl is the line index in the input schedule file (for formatting 
    error messages).
    sds_point returns a tupple ( slew_time, az_acc, el_acc, wrap ), where
    slew_time is slewing time in second, az_acc, el_acc are accumulated azimuth
    an elevation at the end of slewing, and wrap is a string "ccw", "n", or "cw"
    that defines the wrap sector.
    """
    if ( stp.sta_name == "ggao12m" ):
         ( slew_time, az_acc, el_acc, wrap ) = sds_point_ggao12m ( stp, nl, \
                                                   az_acc, el_acc, az, el, ivrb )
         return ( slew_time, az_acc, el_acc, wrap )
#
# --- dist_az_accel is the distance over azimuth   that antenna travles using only 
# ---               acceleration and deceleration without reaching full slewing speed
# --- dist_el_accel is the distance over elevation that antenna travles using only 
# ---               acceleration and deceleration without reaching full slewing speed
#
    dist_az_accel = stp.slew_az**2/stp.accl_az
    dist_el_accel = stp.slew_el**2/stp.accl_el

    if ( az_acc == None ):
#
# ------ Special case az_acc is undefined. We just set antenna's new azimuth and elevation,
# ------ by we consider antenna has no slewing time
#
         if ( az >= stp.az_range[0] and az < stp.az_range[1] ):
              wrap = "ccw"
         elif ( az >= stp.az_range[1] and az < stp.az_range[2] ):
              wrap = "n"
         else:
              wrap = "cw"

         return ( 0.0, az, el, wrap )
    else:
         dif_az = az - az_acc
         dif_el = el - el_acc

#
# ------ Determine which azimuth arc is shorter and whether the shorter arc is allowed
#
         if ( dif_az < -360 ): dif_az = dif_az + 360
         if ( dif_az >  360 ): dif_az = dif_az - 360
         if ( dif_az > 180 ):
              if ( az_acc + (dif_az - 360) > stp.az_range[0] ):
                   dif_az =  dif_az - 360
         if ( dif_az < -180 ):
              if ( az_acc + (dif_az + 360) < stp.az_range[3] ):
                   dif_az =  dif_az + 360
         if ( az_acc + dif_az >=  stp.az_range[3] ):
              dif_az = dif_az - 360
         if ( az_acc + dif_az <=  stp.az_range[0] ):
              dif_az = dif_az + 360

         if ( abs(dif_az) > dist_az_accel ):
#
# ----------- Slewing time over azimith when antenna reaches full speed
#
              slew_az = abs(dif_az)/stp.slew_az + stp.slew_az/stp.accl_az
         else:
              slew_az = 2*math.sqrt ( abs(dif_az)/stp.accl_az )

         if ( abs(dif_el) > dist_el_accel ):
#
# ----------- Slewing time over elevation when antenna reaches full speed
#
              slew_el = abs(dif_el)/stp.slew_el + stp.slew_el/stp.accl_el
         else:
              slew_el = 2*math.sqrt ( abs(dif_el)/stp.accl_el )
          
         slew_time = max ( slew_az + stp.tsettle_az, slew_el + stp.tsettle_el )

#
# ------ Store new azimuth and elevation
#
         az_acc = az_acc + dif_az
         el_acc = el_acc + dif_el
#
# ------ Determine the wrap sector
#
         if (   az_acc >= stp.az_range[0] and az_acc < stp.az_range[1] ):
              wrap = "ccw"
         elif ( az_acc >= stp.az_range[1] and az_acc < stp.az_range[2] ):
              wrap = "n"
         else:
              wrap = "cw"

    return ( slew_time, az_acc, el_acc, wrap )

#
# ------------------------------------------------------------------------
#
def sds_move ( stp, nl, az_acc, el_acc, delta_az, delta_el, ivrb ):
    ( slew_time, az_acc, el_acc, wrap ) = sds_point ( stp, nl, az_acc, el_acc, \
                                                      az_acc + delta_az, el_acc + delta_el, ivrb )
    return ( slew_time, az_acc, el_acc, wrap )

#
# ------------------------------------------------------------------------
#
def sds_point_ggao12m ( stp, nl, az_acc, el_acc, az, el, ivrb ):
    """
    Special case of GGAO12M to avoid radar mask.
    Point to azimith and elevation az, el from the current az_acc, el_acc.
    Argument nl is the line index in the input schedule file (for formatting 
    error messages).
    sds_point returns a tupple ( slew_time, az_acc, el_acc, wrap ), where
    slew_time is slewing time in second, az_acc, el_acc are accumulated azimuth
    an elevation at the end of slewing, and wrap is a string "ccw", "n", or "cw"
    that defines the wrap sector.

    Here is the azimuth-elevation diagram:



           |------|                    |------|
           |      |                    |      |
           |      |                    |      |
           |      |____________________|      |______
                 am(1)               am(2)   am(3)

    There are four cases:

    1) from and to are both above the elevation mask      "d"
    2) from is above the mask, to is below                "ab"
    3) from is below the mask, from is above              "ba"
    4) both from and to are below the mask                "bb"

    """
    el_mask = 42.0
    az_mask = [ -180.0, -126.0, 150.0, 234.0 ]
    if ( az_acc < az_mask[1] and el_acc < el_mask ):
         print ( "Error 1 in line %5d az_acc/el_acc %5.1f %5.1f for GGAO12M are in the forbidden zone" % ( nl, az_acc, el_acc) )
         exit ( 1 )
    if ( az_acc > az_mask[2] and az_acc < az_mask[3] and el_acc < el_mask ):
         print ( "Error 2 in line %5d az_acc/el_acc %5.1f %5.1f for GGAO12M are in the forbidden zone" % ( nl, az_acc, el_acc) )
         exit ( 1 )
    if ( az < az_mask[1] and el < el_mask ):
         print ( "Error 3 in line %5d az/el %5.1f %5.1f for GGAO12M are in the forbidden zone" % ( nl, az, el) )
         exit ( 1 )
    if ( az > az_mask[2] and az < az_mask[3] and el < el_mask ):
         print ( "Error 4 in line %5d az/el %5.1f %5.1f for GGAO12M are in the forbidden zone" % ( nl, az, el) )
         exit ( 1 )


    if ( az_acc == None ):
#
# ------ Special case az_acc is undefined. We just set antenna's new azimuth and elevation,
# ------ by we consider antenna has no slewing time
#
         if ( az >= stp.az_range[0] and az < stp.az_range[1] ):
              wrap = "ccw"
         elif ( az >= stp.az_range[1] and az < stp.az_range[2] ):
              wrap = "n"
         else:
              wrap = "cw"

         return ( 0.0, az, el, wrap )
    else:
         dif_az = az - az_acc
         dif_el = el - el_acc
         fl_case = "undef"

         if ( ( az_acc < az_mask[1] or ( az_acc > az_mask[2] and az_acc < az_mask[3] ) ) and \
              ( ( az > az_mask[1] and az < az_mask[2] ) or az > az_mask[3] )                 ):
              fl_case = "ab"

         if ( ( ( az_acc > az_mask[1] and az_acc < az_mask[2] ) or az_acc > az_mask[3] ) and \
              (   az < az_mask[1] or ( az > az_mask[2] and az < az_mask[3] )  )              ):
              fl_case = "ba"

         if ( ( ( az_acc > az_mask[1] and az_acc < az_mask[2] ) or az_acc > az_mask[3] ) and \
              ( ( az     > az_mask[1] and az     < az_mask[2] ) or az     > az_mask[3] )     ):
              fl_case = "bb"

         if ( el_acc > el_mask and el > el_mask ): 
              fl_case = "d"
         if ( az_acc >= az_mask[1] and az_acc <= az_mask[2] and
              az     >= az_mask[1] and az     <= az_mask[2]   ):
#
# ----------- Both azimuths are in the same 1st valley
#
              fl_case = "d"
         if ( az_acc >= az_mask[3] and az >= az_mask[3] ):
#
# ----------- Both azimuths are in the same 2nd valley
#
              fl_case = "d"

         if ( ivrb > 2 ): print ( "fl_case= ", fl_case, " acc= ", az_acc,el_acc, ' az/el= ', az, el )
         if ( fl_case == "ab" ):
#
# ----------- We are moving from the point above the mask to the point blowe the mask
#
              dif_el_mask = abs(el_acc-el_mask)
#
# ----------- Find the closest azimuthal wall on azimuthal motion
#
              if ( az_acc < az_mask[1] ):
                   az_wall = az_mask[1]
              else:
                 if ( az < az_mask[2] ):
                      az_wall = az_mask[2]
                      az_from_wall = az_mask[2] - az
                 if ( az > az_mask[3] ):
                      az_wall = az_mask[3]
                      az_from_wall = az - az_mask[3] 
#
# ----------- Compute time to reach the azimutal wall and the elevation wall without stopping at the end
#
              dif_az = min ( abs(az_acc-az_wall), abs(az_acc-az_wall-360), abs(az_acc-az_wall+360) )
              slew_az_to_wall = slew_time_hit ( dif_az, stp.slew_az, stp.accl_az )
              slew_el_to_wall = slew_time_hit ( abs(el_acc-el_mask), stp.slew_el, stp.accl_el )

              if ( slew_az_to_wall < slew_el_to_wall ):
#
# ---------------- We would hit the azimuthal wall first. Then this is a direct slewing
#              
                   dif_az = min ( abs(az_acc-az), abs(az_acc-az-360), abs(az_acc-az+360) )
                   slew_el = slew_time_stop ( abs(dif_el), stp.slew_el, stp.accl_el )
                   slew_az = slew_time_stop ( abs(dif_az), stp.slew_az, stp.accl_az )
              else:
#
# ---------------- We would hit the elevation wall first. 
# ---------------- Elevation motion reaches the elevation wall and waits for azimith 
# ---------------- motion to reach the azimuth wall. 
# ---------------- Then the elevation consinutes the second part of elevation path 
# ---------------- Azimuthal motion does not stop
#              
                   dif_az = min ( abs(az_acc-az), abs(az_acc-az-360), abs(az_acc-az+360) )
                   slew_az = slew_time_stop ( abs(dif_az), stp.slew_az, stp.accl_az )
                   slew_el_to_wall = slew_time_stop ( abs(el_acc-el_mask), stp.slew_el, stp.accl_el )

                   slew_el = max ( slew_az_to_wall, slew_el_to_wall ) + \
                             slew_time_stop ( abs(el_mask - el), stp.slew_el, stp.accl_el )

         elif ( fl_case == "ba" ):
#
# ----------- We are moving from the point below the mask to the point above the mask
#
# ----------- Find the closest azimuthal wall on azimuthal motion
#
              dif_el_mask = abs(el_acc-el_mask)
              if ( az_acc > az_mask[3] ):
                   az_wall = az_mask[3]
              else:
                 if ( az_acc < az_mask[2] and az <= az_mask[1] ):
                      as_wall = az_mask[1]
                 elif ( az_acc < az_mask[2] and az >= az_mask[2] ):
                      as_wall = az_mask[2]
                 elif ( az_acc > az_mask[3] ):
                      as_wall = az_mask[3]
#
# ----------- Compute time to reach the azimutal wall and the elevation wall
#
              dif_az = min ( abs(az_acc-az_wall), abs(az_acc-az_wall-360), abs(az_acc-az_wall+360) )
              slew_az_to_wall = slew_time_hit ( abs(az_acc-az_wall), stp.slew_az, stp.accl_az )
              slew_el_to_wall = slew_time_hit ( abs(el_acc-el_mask), stp.slew_el, stp.accl_el )
              if ( slew_el_to_wall <= slew_az_to_wall ):
#
# ---------------- We would hit the elevation wall first. Then this is direct slewing
#              
                   dif_az = min ( abs(az_acc-az), abs(az_acc-az-360), abs(az_acc-az+360) )
                   slew_el = slew_time_stop ( abs(dif_el), stp.slew_el, stp.accl_el )
                   slew_az = slew_time_stop ( abs(dif_az), stp.slew_az, stp.accl_az )
              else:
#
# ---------------- We would hit the azimuth wall first. We need to stop azimuthal
# ---------------- motion and wait for the elevation motion to reach the elevation wall.
# ---------------- After that azimuthal motion resimes.
# ---------------- Elevation motion does not stop
#              
                   slew_el = slew_time_stop ( abs(dif_el), stp.slew_el, stp.accl_el )
                   dif_az  = min ( abs(az_acc-az_wall), abs(az_acc-az_wall-360), abs(az_acc-az_wall+360) )
                   slew_az = max ( slew_el_to_wall, slew_time_stop ( dif_az, stp.slew_az, stp.accl_az ) )
                   dif_az  = min ( abs(az-az_wall), abs(az-az_wall-360), abs(az-az_wall+360) )
                   slew_az = slew_az + slew_time_stop ( dif_az, stp.slew_az, stp.accl_az )

         elif ( fl_case == "bb" ):
#
# ----------- Case when to and from are both below the elevation limit of the mask 
# ----------- and they are separataed by the azimuthal walls.
# ----------- The first azimuthal wall is the wall closest to the azimuth "from"
#
              dif_el_mask = abs(el_acc-el_mask)
              if ( az_acc > az_mask[3] ):
                   az_wall1 = az_mask[3]
                   az_wall2 = az_mask[2]
              else:
                   if ( az_acc < az_mask[2] and az >= az_mask[3] ):
                        az_wall1 = az_mask[2]
                        az_wall2 = az_mask[3]
                   elif ( az_acc > az_mask[3] ):
                        az_wall1 = az_mask[3]
                        az_wall2 = az_mask[2]
#
# ----------- Compute time to reach both azimutal walls and the elevation wall
#
              dif_az = min ( abs(az_acc-az_wall1), abs(az_acc-az_wall1-360), abs(az_acc-az_wall1+360) )
              slew_az_to_wall1 = slew_time_hit ( abs(az_acc-az_wall1), stp.slew_az, stp.accl_az )
              dif_az = min ( abs(az_acc-az_wall2), abs(az_acc-az_wall2-360), abs(az_acc-az_wall2+360) )
              slew_az_to_wall2 = slew_time_hit ( abs(az_acc-az_wall2), stp.slew_az, stp.accl_az )
              slew_el_to_wall  = slew_time_hit ( abs(el_acc-el_mask), stp.slew_el, stp.accl_el )

              if ( el_acc >= el_mask ):
#
# ---------------- We are moving from the point that is above the mask elevation limit
#
                   if ( slew_el_to_wall >= slew_az_to_wall2 ):
#
# --------------------- We would clear the elevation limit by the time we reached teh second
# --------------------- azimuthal wall. Then this is direct slewing.
#              
                        dif_az = min ( abs(az_acc-az), abs(az_acc-az-360), abs(az_acc-az+360) )
                        slew_el = slew_time_stop ( abs(dif_el), stp.slew_el, stp.accl_el )
                        slew_az = slew_time_stop ( abs(dif_az), stp.slew_az, stp.accl_az )
                   else:
#
# --------------------- We would hit the elevation wall first and the 2nd azimuthal wall.
# --------------------- Then elevation motinon stops at the elevation wall, waits
# --------------------- for antenna clear the second azimuthal wall, and then elevation
# --------------------- motion restarts.
# --------------------- Azimuthal motion does not stop.
#              
                        slew_az = slew_time_stop ( abs(dif_az), stp.slew_az, stp.accl_az )
                        slew_el = max ( slew_az_to_wall2, \
                                        slew_time_stop ( abs(el_acc-el_mask), stp.slew_el, stp.accl_el ) ) + \
                                  slew_time_stop ( abs(el_mask-el), stp.slew_el, stp.accl_el )
              elif ( el >= el_mask ):
#
# ---------------- We are moving from the point that is below the mask elevation limit
# ---------------- to the point above the elevation limit
#
                   if ( slew_az_to_wall1 < slew_el_to_wall ):
#
# --------------------- We reach the 1st azimuthal wall first.
# --------------------- Azimuthal motion stops and waits forr elevation motion to clear
# --------------------- the 1st aziumthal wall. 
# --------------------- Elevation motion does not stop
#
                        dif_az = min ( abs(az_acc-az_wall1), abs(az_acc-az_wall1-360), abs(az_acc-az_wall1+360) )
                        slew_az = max ( slew_time_stop ( abs(dif_az), stp.slew_az, stp.accl_az ), slew_el_to_wall )
                        dif_az = min ( abs(az-az_wall1), abs(az-az_wall1-360), abs(az-az_wall1+360) )
                        slew_az = slew_az + slew_time_stop ( abs(dif_az), stp.slew_az, stp.accl_az ) 
                        slew_el = slew_time_stop ( abs(dif_el), stp.slew_el, stp.accl_el )
                   else:
#
# --------------------- We reach the elevation wall first. 
# --------------------- This is direct slewing
#
                        dif_az = min ( abs(az_acc-az), abs(az_acc-az-360), abs(az_acc-az+360) )
                        slew_el = slew_time_stop ( abs(dif_el), stp.slew_el, stp.accl_el )
                        slew_az = slew_time_stop ( abs(dif_az), stp.slew_az, stp.accl_az )
              else:
#
# ---------------- We are moving from the point that is below the mask elevation limit
# ---------------- to the point below the elvation limit
#
                   if ( slew_az_to_wall1 < slew_el_to_wall ):
#
# --------------------- We reach the 1st azimuthal wall first.
# --------------------- Azimuthal motion stops and waits for elevation motion to clear
# --------------------- the 1st aziumthal wall. Elevation motion reaches the elevation wall
# --------------------- then stops. After that the azimuthal motion starts.
# --------------------- When it reaches the second wall, elevation motion restarts
#
                        dif_az = min ( abs(az_acc-az_wall1), abs(az_acc-az_wall1-360), abs(az_acc-az_wall1+360) )
                        slew_el = slew_time_stop ( abs(el_acc-el_mask), stp.slew_az, stp.accl_az )
                        slew_az = max ( slew_time_stop ( abs(dif_az), stp.slew_az, stp.accl_az ), slew_el )

                        dif_az = min ( abs(az-az_wall1), abs(az-az_wall1-360), abs(az-az_wall1+360) )
                        slew_az = slew_az + slew_el + slew_time_stop ( abs(dif_az), stp.slew_az, stp.accl_az ) 

                        dif_az = min ( abs(az_wall1-az_wall2), abs(az_wall1-az_wall2-360), abs(az_wall1-az_wall2+360) )
                        slew_az_wall1_to_wall2 = slew_time_hit ( dif_az , stp.slew_az, stp.accl_az )
                        slew_el = slew_el + slew_az_wall1_to_wall2 + slew_time_stop ( abs(el-el_mask), stp.slew_el, stp.accl_el )
                   else:
#
# --------------------- We reach the elevation first.
# --------------------- Elevation motion reacges the elevation wall then stops. 
# --------------------- Azimuthal motion does not stop
# --------------------- When azimuthal motion reaches the second wall, elevation motion restarts
#
                        dif_az = min ( abs(az_acc-az), abs(az_acc-az-360), abs(az_acc-az+360) )
                        slew_az = slew_time_stop ( abs(dif_az), stp.slew_az, stp.accl_az )
                        slew_el = slew_az_to_wall2 + slew_time_stop ( abs(el_acc), stp.slew_el, stp.accl_el )

         elif ( fl_case == "d" ):
              slew_el = slew_time_stop ( abs(dif_el), stp.slew_el, stp.accl_el )
              slew_az = slew_time_stop ( abs(dif_az), stp.slew_az, stp.accl_az )

         if ( ivrb > 1 and fl_case != "d" ): 
              print ( "sds_point_ggao12m  fl_case= %4s slew_az=%6.1f, slew_el=%6.1f dif_az=%6.1f \n" % (fl_case, slew_az, slew_el, dif_az ) ) # %
         slew_time = max ( slew_az + stp.tsettle_az, slew_el + stp.tsettle_el )
#
# ------ Store new azimuth and elevation
#
         az_acc = az
         el_acc = el_acc + dif_el
#
# ------ Determine the wrap sector
#
         if (   az_acc >= stp.az_range[0] and az_acc < stp.az_range[1] ):
              wrap = "ccw"
         elif ( az_acc >= stp.az_range[1] and az_acc < stp.az_range[2] ):
              wrap = "n"
         else:
              wrap = "cw"

    return ( slew_time, az_acc, el_acc, wrap )
#
# ------------------------------------------------------------------------
#
def slew_time_hit ( dist, slew_vel, slew_acc ):
    """
    Slewing time for a case when accelerates, slews and reach the target without stoppig
    """
    dist_accel = slew_vel**2/(2.0*slew_acc)
    if ( dist < dist_accel ):
         return ( math.sqrt ( 2*dist/slew_acc ) )
    else:
         return ( dist/slew_vel + slew_vel/(2.0*slew_acc) )
#
# ------------------------------------------------------------------------
#
def slew_time_stop ( dist, slew_vel, slew_acc ):
    """
    Slewing time for a case when accelerates, slews, and decelrate to a full stop
    """
    dist_accel = slew_vel**2/slew_acc
    if ( dist < dist_accel ):
         return ( 2 * math.sqrt ( dist/slew_acc ) )
    else:
         return ( dist/slew_vel + slew_vel/slew_acc )

#
# ------------------------------------------------------------------------
#
def sds_proc_fun ( line, array ):
    val = None
    fl_fun = 0
    if ( not "@" in line ): 
         return ( line, None )

    ibf = line.rfind("@")
    for fun in sds_funs.keys():
        if ( fun in line[ibf:] ):
             if ( fun in line ):
                  ib = line[ibf:].find(fun+"{") + len(fun+"{") + ibf
                  ie = line[ibf:].find("}") + ibf
                  narg = len(line[ib:ie].split(",")) 
                  if ( narg != sds_funs[fun] ):
                       print ( "Error in expanding function %s fun: wrong number of arguments" % fun )
                       return ( line, -1 )
                  if ( fun == "@int" or fun == "@01dint" or fun == "@02dint" or fun == "@03dint" or fun == "@04dint" ): 
                       try:
                           arg1 = float ( line[ib:ie].split(",")[0] )
                       except Exception as e:
                           print  ( "Error in expanding function %s fun: the first argument %s is not a float number" % \
                                     (fun, line[ib:ie].split(",")[0] ) )
                           return ( line, -1 )

                  if ( fun == "@add" or fun == "@sub" or fun == "@mul" or fun == "@div"  ):
                       try:
                           arg1 = float ( line[ib:ie].split(",")[0] )
                       except Exception as e:
                           print  ( "Error in expanding function %s fun: the first argument %s is not a float number" % \
                                     (fun, line[ib:ie].split(",")[0] ) )
                           return ( line, -1 )

                       try:
                           arg2 = float ( line[ib:ie].split(",")[1] )
                       except Exception as e:
                           print  ( "Error in expanding function %s fun: tehsecond argument %s is not a float number" % \
                                     (fun, line[ib:ie].split(",")[1] ) )
                           return ( line, -1 )

                  if ( fun == "@int"   ):
                       val = int(arg1)
                       line = line[0:ibf] + "%d" %val + line[ie+1:]
                  if ( fun == "@01dint"   ):
                       val = int(arg1)
                       line = line[0:ibf] + "%01d" %val + line[ie+1:]
                  if ( fun == "@02dint"   ):
                       val = int(arg1)
                       line = line[0:ibf] + "%02d" %val + line[ie+1:]
                  if ( fun == "@03dint"   ):
                       val = int(arg1)
                       line = line[0:ibf] + "%03d" %val + line[ie+1:]
                  if ( fun == "@04dint"   ):
                       val = int(arg1)
                       line = line[0:ibf] + "%04d" %val + line[ie+1:]
                  if ( fun == "@add"   ):
                       val = arg1 + arg2
                       line = line[0:ibf] + "%f" %val + line[ie+1:]
                  elif ( fun == "@sub" ):
                       val = arg1 - arg2
                       line = line[0:ibf] + "%f" %val + line[ie+1:]
                  elif ( fun == "@mul" ):
                       val = arg1 * arg2
                       line = line[0:ibf] + "%f" %val + line[ie+1:]
                  elif ( fun == "@sub" ):
                       val = arg1 / arg2
                       line = line[0:ibf] + "%f" %val + line[ie+1:]
                  elif ( fun == "@lenarr"   ):
                       arg1 = line[ib:ie]
                       if ( arg1 in array.keys() ):
                            val = len(array[arg1])
                       else:
                            print ( "Wrong argument in function @lenarr: %s -- such an array is not defined" % \
                                     arg1 )
                            exit ( 1 )
                       line = line[0:ibf] + "%d" %val + line[ie+1:]

                  (line, new_val) = sds_proc_fun ( line, array )
                  if ( new_val != None ):
                       val = new_val

    return ( line, val )
    

#
# ------------------------------------------------------------------------
#
def main():
    """
    Main program
    """
#
# - Define command line interface
#
    parser = argparse.ArgumentParser(description=sds_to_snap__label)
    parser.add_argument ( "input_sds_file", 
                          help="Input single dish schedule in SDS format" )

    parser.add_argument ( "output_snap_file", \
                          help="Output fields system VLBI schedule in snap format" )

    parser.add_argument ( "output_proc_file", \
                          help="Output fields system procedure file" )

    parser.add_argument( '-v', '--verbosity', \
                         type=int, \
                         dest="ivrb", \
                         default=1, \
                         help="Verbosity level (0 -- silent mode, " + \
                              "1 -- normal verbosity (default), >1 -- debugging mode)" )

    parser.add_argument ( '--version', '-V', \
                          action='version', \
                          version=sds_to_snap__label )

    parser.add_argument( '-t', '--time-start', \
                         dest='start_time_str', \
                         default=None, \
                         help='Start time in UTC in format YYYY.MM.DD_hh:mm:ss or YYYY.JJJ.hh:mm:ss' )

    parser.add_argument( '-c', '--catchup', \
                         dest='catchup', \
                         type=float, \
                         default=None, \
                         help='Interval time in second requried to catch up the ' + \
                              'schedule with the start date in the past' )

#
# --- Get and parse options
#
    opts = parser.parse_args()  

    if ( opts.start_time_str != None ):
#
# ------ If statt time was given, parse it 
#
         opts.start_time_str = opts.start_time_str.replace("T","_").replace("-","_")
         try:
            if ( len(opts.start_time_str) == 13 ):
                 opts.start_time_str = opts.start_time_str + ":00:00"
            if ( len(opts.start_time_str) == 16 ):
                 opts.start_time_str = opts.start_time_str + ":00"
            start_time = datetime.datetime.strptime ( opts.start_time_str, "%Y.%m.%d_%H:%M:%S" )
         except Exception as e:
            try:
               start_time = datetime.datetime.strptime ( opts.start_time_str, "%Y.%j.%H:%M:%S" )
            except Exception as e: 
               print ("sds_to_snap: Wrong format start time %s it should be YYYY.MM.DD_hh:mm:ss or YYYY.ddd.hh:mm:ss" % \
                       opts.start_time_str )
               exit ( 1 )
    else:
#
# ------ Tell sds_to_snap to use the start time defined in the sds control file
#
         start_time = None

    if ( opts.ivrb > 3 ):
         print ( "Python version used: ", sys.version[:3] )
         print ( "Using program:       ", sds_to_snap__label )
         print ( "Input file:          ", opts.input_sds_file )
         print ( "File size:           ", os.stat( opts.input_sds_file ).st_size, " bytes" )
#
# --- Initialize the Station Parameter object
#
    stp = stp_class ( ) 
#
# --- Parse Input Singe Dish schedule and generatge output snap file 
#
    (ret,snap,proc,dur,pi_email) = sds_to_snap  ( opts.input_sds_file, stp, start_time, opts.catchup, opts.ivrb )

    ret = snap_check ( snap )
    if ( ret != 0 ):
         print ( "Trap of internal control: snap file is invalid" )
         exit  ( 1 )

#
# --- Write down the result of conversion in the output file
#
    f=open ( opts.output_snap_file, "w" )
    for line in snap:
        print ( line, file=f )
    f.close()

#
# --- Write down the procedure file
#
    f=open ( opts.output_proc_file, "w" )
    for line in proc:
        print ( line, file=f )
    f.close()
    print ( "Written snap file    ", opts.output_snap_file )
    print ( "Written proc file    ", opts.output_proc_file )
    print ( "Experiment duration: %7d seconds ( %5.2f hours) " % ( dur, dur/3600.0 ) )
    print ( "Upon submission of the log file, please send email to %s" % pi_email )

if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        main()
    except KeyboardInterrupt:
        print ( "pf.py: Interrupted" )
        exit ( 1 )
