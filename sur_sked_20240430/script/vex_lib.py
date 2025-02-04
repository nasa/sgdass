import sur_sked_config # Import sur_sked confuguration
import ners

def init():
    global vex_slew_accel_default, vex_lib__version
    vex_slew_accel_default = 0.8 # deg/sec/sec
    vex_lib__version = "20240225"

class vex_class:
   def __init__ ( self, filename ):
      self.filename = filename
      self.format_revision = "unknown"
      self.exper_name = None
      self.start_utc_time = None
      self.stop_utc_time  = None
      self.proc           = {}
      self.revision       = 1
      self.sta  = {}
      self.sou  = {}
      self.scan = {}
      self.freq = {}
      self.mode = {}
init()
#
# ==============================================================================
#
def parse_vex ( filename ):
    """ 
    Parsing vex file. The contents of vex file is placed in
    the data structure vex 
    """
    vex = vex_class ( filename ) 
    vex.filename = filename
#
# --- Read the vex file and put its contents in string list buf
#
    with open(vex.filename,encoding="latin") as f:
         buf = f.read().splitlines()
    f.close()

    if ( buf[0].split()[0] != "VEX_rev" ):
         print ( "Error in processing file %s -- the first word is %s, " % ( filename, buf[0].split()[0] ) +
                 "while VEX_rev was expected. Please check whether " +
                 "you are processing a vex file" )
         exit ( 1 )
    if ( len(buf[0].replace("=","").split()) < 2 ):
         print ( "Error in processing file %s " % filename +
                 "vex revision is omitted" )
         exit ( 1 )

    vex.format_revision = buf[0].replace("=","").replace(";","").split()[1]

    buf = sort_vex ( buf )

    sect    = "unknown"
    sta_nam = "unknown"
    mode_scan_name = None
    ext_def = None
    vex_vers = "unknown"
    for line in buf:
        if ( vex_vers == "1.5"   and \
             "*vex1" in line     and \
             "NASA"      in line and \
             "extension" in line and \
             "usec"      in line     ):
#
# ---------- A hack: exteract peculiar clock offset from the comment field
# ---------- of vex2 file
#
             sta_nam = line.split()[5]
             clock_offset = float ( line.split()[7] )
             clock_offset_units = line.split()[9]
             clock_offset_epoch = line.split()[11]
             if ( sta_nam in vex.sta.keys() ):
                  vex.sta[sta_nam]["peculiar_clock_offset"] = 1.0e-6*clock_offset
                  vex.sta[sta_nam]["clock_offset_epoch"]    = clock_offset_epoch
                  vex.sta[sta_name]["clock_offset_units"]   = clock_offset_units 
#
# ----- Bypass comments 
#
        if ( len(line.split())     == 0 ): continue
        if ( line.split()[0][0:1]  == "#" ): continue
#
        if ( line[0:7] == "VEX_rev" ):
             vex_vers = line.split()[2].replace(";","")
#
# ----- Get rid of = : and ; in the line
#
        line_orig = line
        line = line.replace("="," ").replace(";","").replace(":"," ")
        if ( line.split()[0][0:20] == "*   revision number:" ):
#
# ---------- Extract revision number from the comment
#
             vex.revision = line.split()[3].replace(";","")
        if ( "PI revision number:" in line_orig and len(line_orig.split()) > 5 ):
             vex.revision = line_orig.split()[5]
              
        if ( line.split()[0][0:1] == "*" ): continue
#
        if ( line.split()[0][0:1] == "$" ):
#
# ---------- Set the section name
#
             sect = line.split()[0][1:]
             continue
        if ( sect == "GLOBAL" and line.split()[0] == "ref" ): 
             vex.exper_name = line.split()[2].replace(";","")
        if ( sect == "EXPER" and line.split()[0] == "exper_description" ): 
             vex.exper_desc = " ".join(line.split()[1:]).replace('"','')
        if ( sect == "EXPER" and line.split()[0] == "exper_nominal_start" ): 
             vex.exper_utc_start = ners.ydhms_to_time ( line.split()[1] )
        if ( sect == "EXPER" and line.split()[0] == "exper_nominal_stop" ): 
             vex.exper_utc_stop = ners.ydhms_to_time ( line.split()[1] )
        if ( sect == "EXPER" and line.split()[0] == "contact_name" ): 
             vex.contact_name = " ".join(line.split()[1:]).replace('"','')
        if ( sect == "EXPER" and line.split()[0] == "scheduler_email" ): 
             vex.contact_email = " ".join(line.split()[1:]) 
        if ( sect == "EXPER" and line.split()[0] == "PI_name" ): 
             vex.contact_name = " ".join(line.split()[1:]).replace('"','')
        if ( sect == "EXPER" and line.split()[0] == "PI_email" ): 
             vex.contact_email = " ".join(line.split()[1:]) 

#
# ----- Parse STATION section
#
        if ( sect == "STATION" ):
             if ( line.split()[0] == "def" ): 
#
# --------------- Extract station name and initialize the station dictionary
#
                  sta_name = line.split()[1].replace(";","").lower()
                  vex.sta[sta_name] = {}
                  vex.sta[sta_name]["rec_rate"]    = 256.0 # default rate
                  vex.sta[sta_name]["preob_proc"]  =     0 # default
                  vex.sta[sta_name]["postob_proc"] =     0 # default
                  vex.sta[sta_name]["backend"]     = "Unknown"
                  vex.sta[sta_name]["peculiar_clock_offset"] = 0.0
                  vex.sta[sta_name]["specific_clock_offset"] = 0.0
                  vex.sta[sta_name]["fmtgps_clock_offset"]   = 0.0
                  vex.sta[sta_name]["clock_offset_units"]    = "usec"
                  vex.sta[sta_name]["clock_offset_epoch"]    = "Unknown"
             if ( len(line.split()) == 3 ):
                  if ( line.split()[1] == "$SITE" ): 
#
# -------------------- Extract antenna name
#
                       ant_name = line.split()[2].upper()
                       vex.sta[sta_name]["ant_name"] = ant_name
                  if ( line.split()[1] == "$DAS" ): 
#
# -------------------- Extract DAS name
#
                       das_name = line.split()[2].lower()
                       vex.sta[sta_name]["das_name"] = das_name

#
# ----- Parse SITE section
#
        if ( sect == "SITE" ):
             if ( line.split()[0] == "def" ): 
                  ant_name = line.split()[1]
                  this_sta_name = None
                  for sta_name in vex.sta.keys():
                      if ( vex.sta[sta_name]["ant_name"] == ant_name ): 
                           this_sta_name = sta_name
                  if ( this_sta_name == None ):
                       print ( "Antenna %s from SITE section is not defined in STATION section" % ant_name )
                       exit  ( 1 )
                  sta_name = this_sta_name
                  coo = []
                  
             if ( line.split()[0] == "site_position" ): 
                  line = line.replace("m","").replace("=","")
                  coo.append ( float(line.split()[1]) )
                  coo.append ( float(line.split()[2]) )
                  coo.append ( float(line.split()[3]) )
                  if ( not sta_name ):
                       print ( "line: ", line ) 
                       print ( "Trap of internal control: station name is not defined in site_position in SITE section" )
                       exit  ( 1 )
                  vex.sta[sta_name]["coo"] = coo
             if ( line.split()[0] == "horizon_map_az" ): 
                  line = line.replace("deg","").replace(":","").replace("horizon_map_az","")
                  hor_mask_az = []
                  for word in line.split():
                      try:
                           hor_mask_az.append ( float(word) )
                      except:
                           print ( "Error in processing line %s of file %s -- cannot convert a string to a float" % \
                                   ( line_orig, filename ) )
                           exit ( 1 )
                  vex.sta[sta_name]["hor_mask_az"] = hor_mask_az

             if ( line.split()[0] == "horizon_map_el" ): 
                  line = line.replace("deg","").replace(":","").replace("horizon_map_el","")
                  hor_mask_el = []
                  for word in line.split():
                      try:
                           hor_mask_el.append ( float(word) )
                      except:
                           print ( "Error in processing line %s of file %s -- cannot convert a string to a float" % \
                                   ( line_orig, filename ) )
                           exit ( 1 )
                  vex.sta[sta_name]["hor_mask_el"] = hor_mask_el

#
# ----- Parse ANTENNA section
#
        if ( sect == "ANTENNA" ):
             if ( line.split()[0] == "def" ):
                  ant_name = line.split()[1]
                  this_sta_name = None
                  for sta_name in vex.sta.keys():
                      if ( vex.sta[sta_name]["ant_name"] == ant_name ): 
                           this_sta_name = sta_name
                  sta_name = this_sta_name
                  if ( sta_name == None ):
                       print ( "Antenna %s from ANTENNA section is not defined in STATION section" % ant_name )
                       exit  ( 1 )
                  az = {}
                  el = {}
                  slew_rate    = [0.0, 0.0]
                  slew_tsettle = [0.0, 0.0]
                  slew_accel   = [0.0, 0.0]
 
             if ( line.split()[0] == "axis_type" ): 
                  if ( not sta_name in vex.sta.keys() ):
                       print ( "line= ", line )
                       print ( "Station >>%s<< is not defined in STATION section"  % sta_name )
                       print ( "List of defined stations: ", vex.sta.keys() )
                       exit  ( 1 )
                  vex.sta[sta_name]["mount"] = []
                  vex.sta[sta_name]["mount"].append ( line.split()[1] )
                  vex.sta[sta_name]["mount"].append ( line.split()[2] )
                  if ( vex.sta[sta_name]["mount"][0] == "xy" ):
                       vex.sta[sta_name]["mount"][0] = "x" 
                       vex.sta[sta_name]["mount"][1] = "yew" 
             if ( line.split()[0] == "antenna_motion" ): 
                  line = line.replace("deg","")
                  if ( line.split()[1] == "az" or \
                       line.split()[1] == "ha" or \
                       line.split()[1] == "x" ):
                       if ( line.split()[3] == "/sec" ):
                            slew_rate[0]    = float(line.split()[2])/180.0*ners.NERS__PI
                       else:
                            slew_rate[0]    = float(line.split()[2])/60.0/180.0*ners.NERS__PI
                       slew_tsettle[0] = float(line.split()[4])
                       if ( len(line.split()) >= 8 ):
                            slew_accel[0]   = float(line.split()[6])/180.0*ners.NERS__PI
                       else:
                            slew_accel[0]   = vex_slew_accel_default/180.0*ners.NERS__PI

                  elif ( line.split()[1] == "el"  or \
                         line.split()[1] == "dec" or \
                         line.split()[1] == "y"      ):
                       if ( line.split()[3] == "/sec" ):
                            slew_rate[1]    = float(line.split()[2])/180.0*ners.NERS__PI
                       else:
                            slew_rate[1]    = float(line.split()[2])/60.0/180.0*ners.NERS__PI
                       slew_tsettle[1] = float(line.split()[4])
                       if ( len(line.split()) >= 8 ):
                            slew_accel[1]   = float(line.split()[6])/180.0*ners.NERS__PI
                       else:
                            slew_accel[1]   = vex_slew_accel_default/180.0*ners.NERS__PI
             if ( line.split()[0] == "pointing_sector" ): 
                  if ( line.split()[1] == "&ccw" or line.split()[1] == "&n" or line.split()[1] == "&cw" ):
                       az[line.split()[1]] = ( float ( line.split()[3] )/180.0*ners.NERS__PI, \
                                               float ( line.split()[5] )/180.0*ners.NERS__PI  )
                       el["low"]  = float ( line.split()[8]  )/180.0*ners.NERS__PI
                       el["high"] = float ( line.split()[10] )/180.0*ners.NERS__PI
             if ( line.split()[0] == "enddef" ): 
                  if ( len(az.keys()) > 0 ):
                       if ( not "&ccw" in az.keys() or not "&cw" in az.keys() ):
                            if ( not "&n" in az.keys() ):
                                 print ( "Pointing sector is not correctly " + \
                                         "defined for station ", sta_nam )
                                 exit ( 0 )
                            azz = []
                            azz.append ( az["&n"][0] )
                            azz.append ( az["&n"][1] )
                            if ( azz[0] > 0.0 ):
                                 azz[0] = az["&n"][0] -2*ners.NERS__PI
                                 azz[1] = az["&n"][1] -2*ners.NERS__PI
                            az["&ccw"] = [ azz[0], azz[1] - 2*ners.NERS__PI ]
                            az["&cw"]  = [ azz[0] + 2*ners.NERS__PI, azz[1] ]
                            az["&n"]   = [az["&cw"][0], az["&ccw"][1] ]

                       az_lim = ( az["&ccw"][0], az["&ccw"][1], az["&cw"][0], az["&cw"][1] )
                       el_lim = ( el["low"], el["high"] )
                       vex.sta[sta_name]["az_lim"] = az_lim
                       vex.sta[sta_name]["el_lim"] = el_lim
                  else:
                       vex.sta[sta_name]["az_lim"] = [-ners.NERS__PI, -ners.NERS__PI, ners.NERS__PI, ners.NERS__PI ]
                       vex.sta[sta_name]["el_lim"] = [0.0, ners.NERS__PI/2.0]
                  vex.sta[sta_name]["slew_rate"]    = slew_rate
                  vex.sta[sta_name]["slew_tsettle"] = slew_tsettle
                  vex.sta[sta_name]["slew_accel"]   = slew_accel


#
# ----- Parse FREQ section
#
        if ( sect == "FREQ" ):
             if ( line.split()[0] == "def" ):
                  freq_name = line.split()[1]
                  vex.freq[freq_name] = {}
                  vex.freq[freq_name]["aggregated_bits_rate"] = 0.0
                  vex.freq[freq_name]["if"] = []
             if ( line.split()[0] == "sample_rate" ):
                  vex.freq[freq_name]["sample_rate"] = float( line.split()[1] )
                  if ( len(line.split()) >= 5 ):
                       if ( line.split()[4] == "(2bits/sample)" ):
                            vex.freq[freq_name]["bits_per_sample"] = 2
                       else:
                            vex.freq[freq_name]["bits_per_sample"] = 1
                  else:
                       vex.freq[freq_name]["bits_per_sample"] = 2
             if ( line.split()[0] == "chan_def" ):
                  if ( line.split()[3] == "U" or line.split()[3] == "L" ):
                       vex.freq[freq_name]["if"].append ( {"sky_freq":  float(line.split()[1]), \
                                                           "subband":         line.split()[3],  \
                                                           "bandwidth": float(line.split()[4])} )
                  else:
                       vex.freq[freq_name]["if"].append ( {"sky_freq":  float(line.split()[2]), \
                                                           "subband":         line.split()[4],  \
                                                           "bandwidth": float(line.split()[5])} )
             if ( line.split()[0] == "enddef" ):
                  for i in range(0,len(vex.freq[freq_name]["if"])):
                      vex.freq[freq_name]["aggregated_bits_rate"] = \
                          vex.freq[freq_name]["aggregated_bits_rate"] + \
                          vex.freq[freq_name]["if"][i]["bandwidth"]*2*\
                          vex.freq[freq_name]["bits_per_sample"]

#
# ----- Parse MODE section
#
        if ( sect == "MODE" ):
             if ( line.split()[0] == "def" ):
                  mode_name = line.split()[1]
                  vex.mode[mode_name] = {"procedure": {}, "freq": {}}
             if ( line.split()[0] == "ref" and line.split()[1] == "$EXTENSIONS"  ):
                  if ( "FS_PROC_DUR_NAMES_" in line.split()[2] ):
                       proc_name_ref = line.split()[2] 
                       vex.mode[mode_name]["procedure"][proc_name_ref] = {"station": []}
                       for i in range (3,len(line.split())):
                          sta_name = line.split()[i].lower() 
                          if ( sta_name in list(vex.sta.keys()) ):
                               vex.mode[mode_name]["procedure"][proc_name_ref]["station"].append ( sta_name )

             if ( line.split()[0] == "ref" and line.split()[1] == "$FREQ"  ):
                  freq_name_ref = line.split()[2] 
                  vex.mode[mode_name]["freq"][freq_name_ref] = {"station": []}
                  for i in range (3,len(line.split())):
                      sta_name = line.split()[i].lower() 
                      if ( sta_name in list(vex.sta.keys()) ):
                           vex.mode[mode_name]["freq"][freq_name_ref]["station"].append ( sta_name )

#                   
#
# ----- Parse SOURCE section section
#
        if ( sect == "SOURCE" ):
             if ( line.split()[0] == "source_name" ):
                  sou_name = line.split()[1]
                  vex.sou[sou_name] = {}
             if ( line.split()[0] == "ra" ):
                  vex.sou[sou_name]["ra"] = ners.hms_to_rad ( line.split()[1] )
             if ( line.split()[0] == "dec" ):
                  vex.sou[sou_name]["dec"] = ners.dms_to_rad ( line.split()[1] )

             if ( len(line.split()) >= 4 ):
                  if ( line.split()[2] == "dec" ):
                       vex.sou[sou_name]["dec"] = ners.dms_to_rad ( line.split()[3] )

#
# ----- Parse SCHED section
#
        if ( sect == "SCHED" ):
             if ( line.split()[0] == "scan" ):
                  scan_name = line.split()[1]
                  vex.scan[scan_name] = {}
                  vex.scan[scan_name]["station"] = {}
                  vex.scan[scan_name]["intent"] = []

             if ( line.split()[0] == "mode" ):
                  mode_scan_name = line.split()[1]
                  vex.scan[scan_name]["mode"] = mode_scan_name

             if ( line.split()[0] == "intent" and "TRUE" in line ):
                  intent_scan = line.split()[1]
                  vex.scan[scan_name]["intent"].append ( intent_scan )

             if ( line.split()[0] == "start" ):
                  vex.scan[scan_name]["start_utc_time"] = ners.ydhms_to_time ( line.split()[1] )

             if ( line.split()[0] == "source" ):
                  vex.scan[scan_name]["source"] = line.split()[1]

             if ( len(line.split()) >= 4 ):
                  if ( line.split()[2] == "source" ):
                       vex.scan[scan_name]["source"] = line.split()[3]

             if ( len(line.split()) >= 6 ):
                  if ( line.split()[4] == "source" ):
                       vex.scan[scan_name]["source"] = line.split()[5]

             if ( line.split()[0] == "station" ):
                  sta_name = line.split()[1].lower()
                  vex.scan[scan_name]["station"][sta_name] = {}

                  sta_proc_used = None                  
                  proc_name = None
                  if ( mode_scan_name in vex.mode.keys() ):
                       if ( len(vex.mode[mode_scan_name]["procedure"].keys()) > 0 ):
                            for proc_name in vex.mode[mode_scan_name]["procedure"].keys():
                                if ( "station" in vex.mode[mode_scan_name]["procedure"][proc_name] ):
                                     if ( sta_name in vex.mode[mode_scan_name]["procedure"][proc_name]["station"] ):
                                          sta_proc_used = proc_name
#
                  vex.scan[scan_name]["station"][sta_name]["proc_name"] = sta_proc_used

                  if ( sta_proc_used != None ):
                       if ( not "proc_names" in vex.sta[sta_name].keys() ):
                            vex.sta[sta_name]["proc_names"] = []
                       if ( not sta_proc_used in vex.sta[sta_name]["proc_names"] ):
                            vex.sta[sta_name]["proc_names"].append ( sta_proc_used )

                  vex.scan[scan_name]["station"][sta_name]["start_offset"] = \
                                      float ( line_orig.split(":")[1].lstrip().split()[0] )
                  vex.scan[scan_name]["station"][sta_name]["stop_offset"]  = \
                                      float ( line_orig.split(":")[2].lstrip().split()[0] )
                  vex.scan[scan_name]["station"][sta_name]["sector"]  = line_orig.split(":")[5].lstrip().strip()
                  if ( vex.scan[scan_name]["station"][sta_name]["sector"][0:1] != "&" ):
                       vex.scan[scan_name]["station"][sta_name]["sector"] = "&n"
#
# ----- Parse EXTENSION section
#
        if ( sect == "EXTENSIONS" ):
             if ( ext_def ):
                  if ( "FS_PROC_DUR_NAMES_" in ext_def and \
                       "NASA"      in line             and \
                       len(line_orig.split(":")) == 5  and \
                       ( "preses"  in line or \
                         "setscan" in line or \
                         "setmode" in line or \
                         "preob"   in line or \
                         "midob"   in line or \
                         "postob"  in line or \
                         "postses" in line    ) ):
#
                       vex.proc[proc_name]["hds"] = ext_def.replace("FS_PROC_DUR_NAMES_","").split("_")[0]
                       vex.proc[proc_name]["sta"] = ext_def.replace("FS_PROC_DUR_NAMES_","").split("_")[1]
#
# -------------------- Parse definitions of seven procedures
#
#
# -------------------- Extract words
#
                       word_sta  = line_orig.split(":")[1].split()[0]
                       word_proc = line_orig.split(":")[2].split()[0]
                       word_dur  = line_orig.split(":")[3].split()[0]
                       word_name = line_orig.split(":")[4].split()[0].replace(";","")
                       if ( word_proc == "preses" ):
                            vex.proc[proc_name]["preses_proc_dur"]  = float ( word_dur  )
                            vex.proc[proc_name]["preses_proc_name"] = word_name
                       if ( word_proc == "setmode" ):
                            mode_proc_name = word_name.replace("setmode_","")
                            vex.proc[proc_name]["setmode"][mode_proc_name] = \
                                    {"setmode_proc_dur": float(word_dur), "setmode_proc_name": word_name}
                       if ( word_proc == "setscan" ):
                            vex.proc[proc_name]["setscan_proc_dur"]  = float ( word_dur  )
                            vex.proc[proc_name]["setscan_proc_name"] = word_name
                       if ( word_proc == "preob" ): 
                            vex.proc[proc_name]["preob_proc_dur"]    = float ( word_dur  )
                            vex.proc[proc_name]["preob_proc_name"]   = word_name
                       if ( word_proc == "midob" ): 
                            vex.proc[proc_name]["midob_proc_dur"]    = float ( word_dur  )
                            vex.proc[proc_name]["midob_proc_name"]   = word_name
                       if ( word_proc == "postob" ): 
                            vex.proc[proc_name]["postob_proc_dur"]   = float ( word_dur  )
                            vex.proc[proc_name]["postob_proc_name"]  = word_name
                       if ( word_proc == "postses" ):
                            vex.proc[proc_name]["postses_proc_dur"]  = float ( word_dur  )
                            vex.proc[proc_name]["postses_proc_name"] = word_name

                  if ( ext_def == "BACKEND_DESCR" and "NASA" in line ):
                       sta_nam = line.split()[2].lower()
                       backend = line.split()[4]
                       vex.sta[sta_nam]["backend"] = backend

                  if ( ext_def == "PECULAIR_CLOCK_OFFSET" and "NASA" in line ):
                       sta_nam      = line.replace(":","").split()[2]
                       offset       = float ( line.replace(":","").split()[3] )
                       epoch_offset = ners.ydhms_to_time ( line.replace(":","").split()[5] )
                       if ( sta_nam in vex.sta.keys() ):
                            vex.sta[sta_nam]["peculiar_clock_offset"] = 1.0e-6*offset
                            vex.sta[sta_nam]["clock_offset_epoch"] = epoch_offset

             if ( line.split()[0] == "def" ):
                  ext_def = line.split()[1]
                  if ( "FS_PROC_DUR_NAMES_" in ext_def ):
#
# -------------------- Initializtion of proc section
#
                       proc_name = line.split()[1]
                       mode_name = None
                       for mode_name_test in vex.mode.keys():
                           if ( proc_name in vex.mode[mode_name_test]["procedure"].keys() ):
                                mode_name = mode_name_test
                       if ( mode_name == None ):
                            print ( "Error in parsing EXTENSIONS section: procedure %s was not defined in $mode section" % \
                                     proc_name  )
                            exit ( 1 )
                       vex.proc[proc_name] = { "preses_proc_dur":   0.0, "preses_proc_name":   "??", \
                                               "setmode":  {},                                       \
                                               "setscan_proc_dur":  0.0, "setscan_proc_name":  "??", \
                                               "preob_proc_dur":    0.0, "preob_proc_name":    "??", \
                                               "midob_proc_dur":    0.0, "midob_proc_name":    "??", \
                                               "postob_proc_dur":   0.0, "postob_proc_name":   "??", \
                                               "postses_proc_dur":  0.0, "postses_proc_name":  "??"  \
                                             }

             if ( line.split()[0] == " enddef" ):
                  ext_def = None

    return      ( vex )
#
# ==============================================================================
#
def sort_vex ( buf ):
    """
    Auxilliary function sort_vex sorts vex file to force sections to follow
    in the specific order. This allows parse_vex to parse vex file in 
    one run.
    """

#
# --- Split the vex file into sections. Nex lines will create dictionary 
# --- sect_range that will contain the range of lines of the vex portion
# --- in the original file
#
    sect_range  = {}
    sect_name = None
    for i in range(0,len(buf)):
        line = buf[i].replace("="," ").replace(";","").replace(":"," ")
        if ( line[0:1] == '$' ):
             if ( sect_name ):
                  sect_range[sect_name].append ( i- 1 )
             else:
                  sect_name = "null"
                  sect_range[sect_name] = [0, i-1]
               
                  
             sect_name = line[1:]
             sect_range[sect_name] = [i]
    if ( sect_name ):
         sect_range[sect_name].append ( len(buf) )

#
# -- This list defines the order of sections. Although vex specs allow arbitrary 
# -- order of sections, processing vex file requires sections to be in 
# -- a certain orer
#
    sect_list = [ "null",              \
                  "GLOBAL",            \
                  "EXPER",             \
                  "STATION",           \
                  "SITE",              \
                  "ANTENNA",           \
                  "DAS",               \
                  "FREQ",              \
                  "IF",                \
                  "MODE",              \
                  "PROCEDURES",        \
                  "EXTENSIONS",        \
                  "BBC",               \
                  "HEAD_POS",          \
                  "PASS_ORDER",        \
                  "ROLL",              \
                  "PHASE_PROC_DETECT",  \
                  "CLOCK",             \
                  "CORR",              \
                  "SCHEDULING_PARAMS", \
                  "ANTENNA_PROC_OBS",   \
                  "CABLE_PROC_OBS",     \
                  "CLOCK_OBS",         \
                  "SCHED_OBS",         \
                  "PHASE_PROC_OBS",     \
                  "TAPELOG_OBS",       \
                  "TSYS_OBS",          \
                  "WX_OBS",            \
                  "SOURCE",            \
                  "SCHED"              \
                ]
#
# --- Asseble a buffer with sorted vex file, section by section
#
    out_buf = []
    for sect_name in sect_list:
        if ( sect_name in sect_range.keys() ):
             for i in range(sect_range[sect_name][0],sect_range[sect_name][1]):
                 out_buf.append ( buf[i] )

    return ( out_buf )

#
# ==============================================================================
#
def print_vex ( vex, verb ):
    """
    Function for printing the contents of data structure vex
    verb = 1 -- only the summary is printed
    verb > 1 -- Ful; information is printed
    """

    if ( verb >= 1 ):
#
# ------ Printing the summary
#
         print ( "filename        = ", vex.filename )
         print ( "format_revision = ", vex.format_revision )
         if ( hasattr( vex, "vex.revision") ):
              print ( "vex.revision        = ", vex.revision   )
         print ( "exper_desc      = ", vex.exper_desc )
         print ( "exper_utc_start = ", vex.exper_utc_start )
         print ( "exper_utc_stop  = ", vex.exper_utc_stop  )
         if ( hasattr( vex, "contact_name") ):
              print ( "contact_name    = ", vex.contact_name    )
         if ( hasattr( vex, "contact_email") ):
              print ( "contact_email   = ", vex.contact_email   )
         print ( "Number of stations  = ", len(vex.sta)  )
         print ( "Number of sources   = ", len(vex.sou)  )
         print ( "Number of scans     = ", len(vex.scan) )

    if ( verb >= 2 ):
         print ( " " ) 
         freq_list = sorted(list(vex.freq.keys()))
#
# ------ Printing information about the frequency setup
#
         for freq_name in freq_list:
             print ( " " ) 
             print ( " Frequency: %-24s  bits_rate %7.1f Mbps" % \
                       ( freq_name, vex.freq[freq_name]["aggregated_bits_rate"] ) )
             for i in range(0,len(vex.freq[freq_name]["if"])):
                 print ( "      IF %2d  sky_freq: %10.3f MHz, bandwidth: %10.3f MHz  subband: %s" % \
                              ( i+1, \
                                vex.freq[freq_name]["if"][i]["sky_freq"],  \
                                vex.freq[freq_name]["if"][i]["bandwidth"], \
                                vex.freq[freq_name]["if"][i]["subband"]    \
                              ) )

         if ( len(vex.proc) > 0 ):
              print ( " " ) 
              for proc_name in vex.proc.keys():
                  print ( "  Procedure  %s" %  proc_name )
                  if ( "preses_proc_dur" in vex.proc[proc_name].keys() ):
                       print ( "     preses_proc:  %-12s duration %5.1f sec" % 
                               ( vex.proc[proc_name]["preses_proc_name"], \
                                 vex.proc[proc_name]["preses_proc_dur"] ) )
                  if ( "setmode" in vex.proc[proc_name].keys() ):
                       for mode_proc_name in vex.proc[proc_name]["setmode"]:
                           print ( "     setmode_proc: %-12s duration %5.1f sec" % 
                                   ( vex.proc[proc_name]["setmode"][mode_proc_name]["setmode_proc_name"], \
                                     vex.proc[proc_name]["setmode"][mode_proc_name]["setmode_proc_dur"] ) )
                  if ( "setscan_proc_dur" in vex.proc[proc_name].keys() ):
                       print ( "     setscan_proc: %-12s duration %5.1f sec" % 
                               ( vex.proc[proc_name]["setscan_proc_name"], \
                                 vex.proc[proc_name]["setscan_proc_dur"] ) )
                  if ( "preob_proc_dur" in vex.proc[proc_name].keys() ):
                       print ( "     preob_proc:   %-12s duration %5.1f sec" % 
                               ( vex.proc[proc_name]["preob_proc_name"], \
                                 vex.proc[proc_name]["preob_proc_dur"] ) )
                  if ( "midob_proc_dur" in vex.proc[proc_name].keys() ):
                       print ( "     midob_proc:   %-12s duration %5.1f sec" % 
                               ( vex.proc[proc_name]["midob_proc_name"], \
                                 vex.proc[proc_name]["midob_proc_dur"] ) )
                  if ( "postob_proc_dur" in vex.proc[proc_name].keys() ):
                       print ( "     postob_proc:  %-12s duration %5.1f sec" % 
                               ( vex.proc[proc_name]["postob_proc_name"], \
                                 vex.proc[proc_name]["postob_proc_dur"] ) )
                  if ( "postses_proc_dur" in vex.proc[proc_name].keys() ):
                       print ( "     postses_proc: %-12s duration %5.1f sec" % 
                               ( vex.proc[proc_name]["postses_proc_name"], \
                                 vex.proc[proc_name]["postses_proc_dur"] ) )

         if ( len(vex.mode) > 0 ):
              for mode_name in vex.mode.keys():
                  print ( " " ) 
                  if ( len(vex.mode[mode_name]["procedure"].keys()) > 0 ):
                       for proc_name in vex.mode[mode_name]["procedure"].keys():
                           print ( "  Mode       %-16s Procedure: %-16s " % ( mode_name, proc_name ) )
                           for sta_name in vex.mode[mode_name]["procedure"][proc_name]["station"]:
                               print ( "                                        ", sta_name )
                  else:
                       print ( "  Mode       %-16s  " % mode_name )

#
# ------ Printing informatin about participating stations
#
         sta_list = sorted(list(vex.sta.keys())) 
         for sta_name in sta_list:
             print ( " " ) 
             if ( not "coo" in vex.sta[sta_name].keys() ): continue
             (hei, long, lat_gdt ) = ners.cart_to_hlp ( vex.sta[sta_name]["coo"] )
             print ( "  Station: %2s  Site: %-8s  Coo: %12.3f %12.3f %12.3f" % \
                       ( sta_name, \
                         vex.sta[sta_name]["ant_name"], \
                         vex.sta[sta_name]["coo"][0],   \
                         vex.sta[sta_name]["coo"][1],   \
                         vex.sta[sta_name]["coo"][2] )  \
                       )
             print ( "           long: %s lat_gdt: %s" % \
                     ( ners.rad_to_dms ( long,    2 ), \
                       ners.rad_to_dms ( lat_gdt, 2 ) ) )
             print ( "           Mount: %s Slew_rate: %6.3f %6.3f, Slew_accel: %6.3f %6.3f, Slew_tsettle: %4.0f %4.0f" % \
                               ( vex.sta[sta_name]["mount"][0]+vex.sta[sta_name]["mount"][1], \
                                 vex.sta[sta_name]["slew_rate"][0]*180.0/ners.NERS__PI,  \
                                 vex.sta[sta_name]["slew_rate"][1]*180.0/ners.NERS__PI,  \
                                 vex.sta[sta_name]["slew_accel"][0]*180.0/ners.NERS__PI, \
                                 vex.sta[sta_name]["slew_accel"][1]*180.0/ners.NERS__PI, \
                                 vex.sta[sta_name]["slew_tsettle"][0], vex.sta[sta_name]["slew_tsettle"][1] ) )
             print ( "           DAS: %s  Recording rate: %9.3f Mbps " % \
                                    ( vex.sta[sta_name]["das_name"], vex.sta[sta_name]["rec_rate"] ) )
             if ( len(vex.sta[sta_name]["az_lim"]) >= 4 ): 
                  print ( "           AZ_range:  %5.1f %5.1f %5.1f %5.1f  EL_range: %5.1f %5.1f" % \
                                               ( vex.sta[sta_name]["az_lim"][0]*180.0/ners.NERS__PI, \
                                                 vex.sta[sta_name]["az_lim"][1]*180.0/ners.NERS__PI, \
                                                 vex.sta[sta_name]["az_lim"][2]*180.0/ners.NERS__PI, \
                                                 vex.sta[sta_name]["az_lim"][3]*180.0/ners.NERS__PI, \
                                                 vex.sta[sta_name]["el_lim"][0]*180.0/ners.NERS__PI, \
                                                 vex.sta[sta_name]["el_lim"][1]*180.0/ners.NERS__PI  \
                                                ) )
             if ( "hor_mask_az" in vex.sta[sta_name].keys() ):
                  str_hor_mask = ""
                  for azim in vex.sta[sta_name]["hor_mask_az"]:
                      str_hor_mask = str_hor_mask + "%6.2f " % azim
                  print ( "           AZ_hor_mask: %s" % str_hor_mask )

             if ( "hor_mask_el" in vex.sta[sta_name].keys() ):
                  str_hor_mask = ""
                  for elev in vex.sta[sta_name]["hor_mask_el"]:
                      str_hor_mask = str_hor_mask + "%6.2f " % elev
                  print ( "           EL_hor_mask: %s" % str_hor_mask )
             if ( vex.sta[sta_name]["clock_offset_epoch"] == "Unknown" ):
                  print ( "           Peculiar clock offset is not defined" )
             else:
                  print ( "           Peculiar clock offset %12.3f Last epoch: %s" % \
                          ( vex.sta[sta_name]["peculiar_clock_offset"], \
                            vex.sta[sta_name]["clock_offset_epoch"] )  )

#
# ------ Printing information about observed sources
#
         print ( " " ) 
         sou_list = sorted(list(vex.sou.keys()))
         for sou_name in sou_list:
             print ( "  Source: %-8s  Ra: %s  Dec: %s" % \
                        ( sou_name,                                        \
                          ners.rad_to_hms ( vex.sou[sou_name]["ra"], 4 ),  \
                          ners.rad_to_dms ( vex.sou[sou_name]["dec"], 3, "+"  )  \
                        ) )

#
# ------ Printing information about scans
#
         print ( " " ) 
         scan_list = sorted(list(vex.scan.keys()))
         for scan_name in scan_list:
             print ( " " ) 
             print ( "  Scan: %s  Source: %s  UTC_start_time: %s mode: %s" % \
                      ( scan_name, \
                        vex.scan[scan_name]["source"],         \
                        vex.scan[scan_name]["start_utc_time"], \
                        vex.scan[scan_name]["mode"]            \
                      ) \
                   )
             
             sta_list = sorted(list(vex.scan[scan_name]["station"]))
             for sta_name in sta_list:
                 start_time = vex.scan[scan_name]["station"][sta_name]["start_offset"]
                 print ( "        Station: %s  Start_offset: %4d  Stop_offset: %4d  Sector: %s" % \
                           ( sta_name, \
                             vex.scan[scan_name]["station"][sta_name]["start_offset"], \
                             vex.scan[scan_name]["station"][sta_name]["stop_offset"], \
                             vex.scan[scan_name]["station"][sta_name]["sector"] \
                           ) )
                        
