#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program  gen_seq_prc.py takes as input a file with the frequency   *
# *   setup and generates a number of output files. The output files are *
# *   a) procedure files for each station defined in the iunput          *
# *      frequency sequence file;                                        *
# *   b) frequency definition file in vex format. That section is to be  *
# *      included in the schedule in vex format.                         *
# *                                                                      *
# *  /progs/sur_sked_20231118/script/gen_seq_prc.py -s /vlbi/y2/y2_m02.seq -o /vlbi/y4045a
# * 
# * ### 09-FEB-2024  gen_seq_prc.py v 1.7 (c)  L. Petrov 14-JUL-2024 ### *
# *                                                                      *
# ************************************************************************
import argparse, signal, sys, os, pwd, math
from   datetime import datetime, timedelta, tzinfo
from   sur_sked_config import * # Import sur_sked confuguration
from   pet_misc        import *

gsp__label   = "gen_seq_prc.py 20240716"

prc_magic = "# FRQ_SEQ  1.4   Format version 2023.06.03"
frd_magic = "# Frequency definition table. Version 1.0   Format of 2024.02.10"

seq_dir  = sur_sked_seq
stp_dir  = sur_sked_stp
prc_dir  = sur_sked_prc
frq_def  = sur_sked_freq_def

mark5b_geo2 = { \
               1: {"bbc_ind":  9, "sub_band": 'U', "chan": 11, "band_ind":  1 }, \
               2: {"bbc_ind": 10, "sub_band": 'U', "chan": 12, "band_ind":  2 }, \
               3: {"bbc_ind": 11, "sub_band": 'U', "chan": 13, "band_ind":  3 }, \
               4: {"bbc_ind": 12, "sub_band": 'U', "chan": 14, "band_ind":  4 }, \
               5: {"bbc_ind": 13, "sub_band": 'U', "chan": 15, "band_ind":  5 }, \
               6: {"bbc_ind": 14, "sub_band": 'U', "chan": 16, "band_ind":  6 }, \
               7: {"bbc_ind":  1, "sub_band": 'L', "chan":  1, "band_ind":  1 }, \
               8: {"bbc_ind":  1, "sub_band": 'U', "chan":  2, "band_ind":  2 }, \
               9: {"bbc_ind":  2, "sub_band": 'U', "chan":  3, "band_ind":  3 }, \
              10: {"bbc_ind":  3, "sub_band": 'U', "chan":  4, "band_ind":  4 }, \
              11: {"bbc_ind":  4, "sub_band": 'U', "chan":  5, "band_ind":  5 }, \
              12: {"bbc_ind":  5, "sub_band": 'U', "chan":  6, "band_ind":  6 }, \
              13: {"bbc_ind":  6, "sub_band": 'U', "chan":  7, "band_ind":  7 }, \
              14: {"bbc_ind":  7, "sub_band": 'U', "chan":  8, "band_ind":  8 }, \
              15: {"bbc_ind":  8, "sub_band": 'L', "chan":  9, "band_ind":  9 }, \
              16: {"bbc_ind":  8, "sub_band": 'U', "chan": 10, "band_ind": 10 }  \
            }

mark5b_geo3 = { \
               1: {"bbc_ind":  9, "sub_band": 'U', "chan": 11, "band_ind":  1 }, \
               2: {"bbc_ind": 10, "sub_band": 'U', "chan": 12, "band_ind":  2 }, \
               3: {"bbc_ind": 11, "sub_band": 'U', "chan": 13, "band_ind":  3 }, \
               4: {"bbc_ind": 12, "sub_band": 'U', "chan": 14, "band_ind":  4 }, \
               5: {"bbc_ind": 13, "sub_band": 'U', "chan": 15, "band_ind":  5 }, \
               6: {"bbc_ind": 14, "sub_band": 'U', "chan": 16, "band_ind":  6 }, \
               7: {"bbc_ind":  1, "sub_band": 'L', "chan":  1, "band_ind":  1 }, \
               8: {"bbc_ind":  1, "sub_band": 'U', "chan":  2, "band_ind":  2 }, \
               9: {"bbc_ind":  2, "sub_band": 'U', "chan":  3, "band_ind":  3 }, \
              10: {"bbc_ind":  3, "sub_band": 'U', "chan":  4, "band_ind":  4 }, \
              11: {"bbc_ind":  4, "sub_band": 'U', "chan":  5, "band_ind":  5 }, \
              12: {"bbc_ind":  5, "sub_band": 'U', "chan":  6, "band_ind":  1 }, \
              13: {"bbc_ind":  6, "sub_band": 'U', "chan":  7, "band_ind":  2 }, \
              14: {"bbc_ind":  7, "sub_band": 'U', "chan":  8, "band_ind":  3 }, \
              15: {"bbc_ind":  8, "sub_band": 'L', "chan":  9, "band_ind":  4 }, \
              16: {"bbc_ind":  8, "sub_band": 'U', "chan": 10, "band_ind":  5 }  \
            }

mark5b_geo4 = { \
               1: {"bbc_ind":  9, "sub_band": 'U', "chan": 11, "band_ind":  1 }, \
               2: {"bbc_ind": 10, "sub_band": 'U', "chan": 12, "band_ind":  2 }, \
               3: {"bbc_ind": 11, "sub_band": 'U', "chan": 13, "band_ind":  3 }, \
               4: {"bbc_ind": 12, "sub_band": 'U', "chan": 14, "band_ind":  4 }, \
               5: {"bbc_ind": 13, "sub_band": 'U', "chan": 15, "band_ind":  1 }, \
               6: {"bbc_ind": 14, "sub_band": 'U', "chan": 16, "band_ind":  2 }, \
               7: {"bbc_ind":  1, "sub_band": 'L', "chan":  1, "band_ind":  1 }, \
               8: {"bbc_ind":  1, "sub_band": 'U', "chan":  2, "band_ind":  2 }, \
               9: {"bbc_ind":  2, "sub_band": 'U', "chan":  3, "band_ind":  3 }, \
              10: {"bbc_ind":  3, "sub_band": 'U', "chan":  4, "band_ind":  4 }, \
              11: {"bbc_ind":  4, "sub_band": 'U', "chan":  5, "band_ind":  5 }, \
              12: {"bbc_ind":  5, "sub_band": 'U', "chan":  6, "band_ind":  1 }, \
              13: {"bbc_ind":  6, "sub_band": 'U', "chan":  7, "band_ind":  2 }, \
              14: {"bbc_ind":  7, "sub_band": 'U', "chan":  8, "band_ind":  3 }, \
              15: {"bbc_ind":  8, "sub_band": 'L', "chan":  9, "band_ind":  4 }, \
              16: {"bbc_ind":  8, "sub_band": 'U', "chan": 10, "band_ind":  5 }  \
            }

#
#  NB: this may be wrong
#
mark5b_vdif = { \
               1: {"bbc_ind":  9, "sub_band": 'U', "chan": 11, "band_ind":  1  }, \
               2: {"bbc_ind": 10, "sub_band": 'U', "chan": 12, "band_ind":  2  }, \
               3: {"bbc_ind": 11, "sub_band": 'U', "chan": 13, "band_ind":  3  }, \
               4: {"bbc_ind": 12, "sub_band": 'U', "chan": 14, "band_ind":  4  }, \
               5: {"bbc_ind": 13, "sub_band": 'U', "chan": 15, "band_ind":  1  }, \
               6: {"bbc_ind": 14, "sub_band": 'U', "chan": 16, "band_ind":  2  }, \
               7: {"bbc_ind":  1, "sub_band": 'L', "chan":  9, "band_ind":  1  }, \
               8: {"bbc_ind":  1, "sub_band": 'U', "chan":  1, "band_ind":  2  }, \
               9: {"bbc_ind":  2, "sub_band": 'U', "chan":  2, "band_ind":  3  }, \
              10: {"bbc_ind":  3, "sub_band": 'U', "chan":  3, "band_ind":  4  }, \
              11: {"bbc_ind":  4, "sub_band": 'U', "chan":  4, "band_ind":  5  }, \
              12: {"bbc_ind":  5, "sub_band": 'U', "chan":  5, "band_ind":  1  }, \
              13: {"bbc_ind":  6, "sub_band": 'U', "chan":  6, "band_ind":  2  }, \
              14: {"bbc_ind":  7, "sub_band": 'U', "chan":  7, "band_ind":  3  }, \
              15: {"bbc_ind":  8, "sub_band": 'L', "chan": 10, "band_ind":  4  }, \
              16: {"bbc_ind":  8, "sub_band": 'U', "chan":  8, "band_ind":  5  }  \
            }

au1_map = { \
             1: {"bbc": "bbc001", "lo": "a" }, \
             2: {"bbc": "bbc002", "lo": "a" }, \
             3: {"bbc": "bbc003", "lo": "a" }, \
             4: {"bbc": "bbc004", "lo": "a" }, \
             5: {"bbc": "bbc005", "lo": "a" }, \
             6: {"bbc": "bbc006", "lo": "a" }, \
             7: {"bbc": "bbc007", "lo": "a" }, \
             8: {"bbc": "bbc008", "lo": "a" }, \
             9: {"bbc": "bbc009", "lo": "b" }, \
            10: {"bbc": "bbc010", "lo": "b" }, \
            11: {"bbc": "bbc011", "lo": "b" }, \
            12: {"bbc": "bbc012", "lo": "b" }, \
            13: {"bbc": "bbc013", "lo": "b" }, \
            14: {"bbc": "bbc014", "lo": "b" }, \
            15: {"bbc": "bbc015", "lo": "b" }, \
            16: {"bbc": "bbc016", "lo": "b" }, \
            17: {"bbc": "bbc065", "lo": "a" }, \
            18: {"bbc": "bbc066", "lo": "a" }, \
            19: {"bbc": "bbc067", "lo": "a" }, \
            20: {"bbc": "bbc068", "lo": "a" }, \
            21: {"bbc": "bbc069", "lo": "a" }, \
            22: {"bbc": "bbc070", "lo": "a" }, \
            23: {"bbc": "bbc071", "lo": "a" }, \
            24: {"bbc": "bbc072", "lo": "a" }, \
            25: {"bbc": "bbc073", "lo": "b" }, \
            26: {"bbc": "bbc074", "lo": "b" }, \
            27: {"bbc": "bbc075", "lo": "b" }, \
            28: {"bbc": "bbc076", "lo": "b" }, \
            29: {"bbc": "bbc077", "lo": "b" }, \
            30: {"bbc": "bbc078", "lo": "b" }, \
            31: {"bbc": "bbc079", "lo": "b" }, \
            32: {"bbc": "bbc080", "lo": "b" }, \
            33: {"bbc": "bbc025", "lo": "d" }, \
            34: {"bbc": "bbc026", "lo": "d" }, \
            35: {"bbc": "bbc027", "lo": "d" }, \
            36: {"bbc": "bbc028", "lo": "d" }, \
            37: {"bbc": "bbc029", "lo": "d" }, \
            38: {"bbc": "bbc030", "lo": "d" }, \
            39: {"bbc": "bbc031", "lo": "d" }, \
            40: {"bbc": "bbc032", "lo": "d" }, \
            41: {"bbc": "bbc033", "lo": "e" }, \
            42: {"bbc": "bbc034", "lo": "e" }, \
            43: {"bbc": "bbc035", "lo": "e" }, \
            44: {"bbc": "bbc036", "lo": "e" }, \
            45: {"bbc": "bbc037", "lo": "e" }, \
            46: {"bbc": "bbc038", "lo": "e" }, \
            47: {"bbc": "bbc039", "lo": "e" }, \
            48: {"bbc": "bbc040", "lo": "e" }, \
            49: {"bbc": "bbc049", "lo": "g" }, \
            50: {"bbc": "bbc050", "lo": "g" }, \
            51: {"bbc": "bbc051", "lo": "g" }, \
            52: {"bbc": "bbc052", "lo": "g" }, \
            53: {"bbc": "bbc053", "lo": "g" }, \
            54: {"bbc": "bbc054", "lo": "g" }, \
            55: {"bbc": "bbc055", "lo": "g" }, \
            56: {"bbc": "bbc056", "lo": "g" }, \
            57: {"bbc": "bbc057", "lo": "h" }, \
            58: {"bbc": "bbc058", "lo": "h" }, \
            59: {"bbc": "bbc059", "lo": "h" }, \
            60: {"bbc": "bbc060", "lo": "h" }, \
            61: {"bbc": "bbc061", "lo": "h" }, \
            62: {"bbc": "bbc062", "lo": "h" }, \
            63: {"bbc": "bbc063", "lo": "h" }, \
            64: {"bbc": "bbc064", "lo": "h" }  \
          }

def gen_seq_prc ( fil_seq, dir_out, ivrb ):
    """
    aaa
    """ 
#
# --- Read the input sequence file
#
    buf = read_file ( fil_seq )
    if ( not buf ):
         print ( "ERROR get_seq_prc: Cannot read file %s" %  fil_seq )
         exit  ( 1 )
    
    if ( buf[0] != prc_magic ):
         print ( "ERROR get_seq_prc:Wrong format of sequence file %s" %  fil_seq )
         print ( "The first line is %s while %s was expected" % ( buf[0], prc_magic ) )
         exit  ( 1 )

    frd = read_file ( frq_def )
    if ( not frd ):
         print ( "ERROR get_seq_prc: Cannot read frequency defintion file %s" %  frq_def )
         exit  ( 1 )
    
    if ( frd[0] != frd_magic ):
         print ( "ERROR get_seq_prc: Wrong format of the frequency defintion file %s" %  frq_def  )
         print ( "The first line is %s while %s was expected" % ( frd[0], frd_magic) )
         exit  ( 1 )
 
#
# --- Parse the input sequence file
#
    seq = {}
    if_ind = -1
    wir_def = {}
    for line in buf:
        if ( line[0:1] == "#" ): continue
        if ( len(line.split()) < 2): continue
        if ( line.split()[0] == "HDS_NAME" ):
#
# ---------- Get hardware setup mode name
#
             seq["hds_name"] = line.split()[1].lower()
        if ( line.split()[0] == "MOD_NAME" ):
#
# ---------- Get the frequency sequence name
#
             seq["mod_name"] = line.split()[1].lower()
        if ( line.split()[0] == "STATION" ):
#
# ---------- Parse station type
#
             sta_name = line.split()[2].lower()
             if ( not "station" in seq.keys() ):
#
# --------------- Initialize "station" dictionary
#
                  seq["station"] = {}
             if ( sta_name in seq["station"].keys() ):
                  print ( "Stations %s is defined more than once in %s" % \
                          ( sta_name, fil_seq ) )

             wir_name = line.split()[3].lower()
#
# ----------- Get wiring name and check whether the wiring is dynamic or  static
#
             if ( wir_name[0:1] == "*" ):
                  wir_def  = {"dynamic": True,  "name": wir_name}
             else:
                  wir_def  = {"dynamic": False, "name": wir_name}

#
# ---------- Now parse the frequency definition file. We need to get 
# ---------- relevant informatil there
#
             for lin in frd:
                 if ( len(lin) ==  0       ): continue
                 if ( lin[0:1] == "#"      ): continue
                 if ( len(lin.split()) < 9 ): continue
                 frd_sta  = lin.split()[0].lower()
                 frd_wir  = lin.split()[1].lower()
                 frd_band = lin.split()[2]

                 if ( len(lin.split()) > 9 ): 
                      frd_lo = float(lin.split()[9])
                 else:
                      frd_lo = 0.0

                 if ( len(lin.split()) > 10 ): 
                      use = "nop"
                 else:
                      use = "use"

                 if (   frd_wir == wir_name or \
                      ( frd_wir[0:1] == "*" and wir_name[0:1] == "*" ) ):
#
# ------------------- Check for a match of the wiring name
#
# ------------------- Since only on dynamicmode is allowed, we consider 
# ------------------- the match anyway
#
                      fl_wir_match = True
                 else:
                      fl_wir_match = False
              
                 if ( frd_sta == sta_name and \
                      fl_wir_match            ):
#
# ------------------- Define wiring dictionary
#
                      wir_def[frd_band] = {                     \
                             "frq_min":  float(lin.split()[3]), \
                             "frq_max":  float(lin.split()[4]), \
                             "frq_step": float(lin.split()[5]), \
                             "das":      lin.split()[6],        \
                             "lo_typ":   lin.split()[7],        \
                             "if_typ":   lin.split()[8],        \
                             "lo":       frd_lo,                \
                             "use":      use                    \
                              }

             if ( wir_def == {} ):
                  print ( "ERROR get_seq_prc: Did not find wiring %s for station %s in the frequency definition file %s" % \
                          ( wir_name, sta_name, frq_def ) )
                  exit ( 1 )
#
# ---------- Update station dictionary
#
             seq["station"][sta_name]  = {"short_name": sta_name.lower(),        \
                                          "long_name":  line.split()[1].upper(), \
                                          "wiring":     wir_def,                 \
                                         }
#
# ---------- End of parseing the frequency definition file. 
#

        if ( line.split()[0] == "BAND" ):
#
# ---------- Now parse the IF defintion line
#
             bnd_name = line.split()[1].upper()
             if ( not "band" in seq.keys() ):
#
# --------------- Initialize band dictionary
#
                  seq["band"] = {}

             if ( not bnd_name in seq["band"].keys() ):
#
# --------------- Create band dictionary
#
                  seq["band"][bnd_name] = { \
                                            "band_frq_min": 0.0, \
                                            "band_frq_max": 0.0, \
                                            "if_frq": [],        \
                                            "if_bwd": [],        \
                                            "order":  []         \
                                          }
             
#
# ---------- ... and add there IF frequency and IF bandwidth
#
             seq["band"][bnd_name]["if_frq"].append ( float ( line.split()[2] ) )
             seq["band"][bnd_name]["if_bwd"].append ( float ( line.split()[3] ) )
             if ( len(line.split()) >= 5 ):
                  if ( line.split()[4].lower() == "dir" or \
                       line.split()[4].lower() == "inv"    ):
                       continue
                  else:
                      print ( "ERROR get_seq_prc: wrong 5th word in line %s -- dir or inv were expected" % \
                              line )
                  seq["band"][bnd_name]["order"].append ( line.split()[4].lower() )
             else:
                  seq["band"][bnd_name]["order"].append ( "inv"  )
                  

              
    if ( ivrb > 1 ):
         print ( "gen_seq_prc-286 num_band= ", len(seq["band"]) )
    if ( ivrb > 2 ):
         print ( "seq= ", seq )

    luff={} # Iitilize a dictionary of LUFF frequencies
    fl_vex_created = False
#
# --- Now process all the statins
#
    for sta in seq["station"]:
        ind_used_dbbc_band = 0
        luff[sta] = 21500.0
        if ( sta == "gs" or sta == "wf" ):
             luff[sta] = 22500.0
#
# ----- Collect information about bands
#
        frq_band={} # The lowest frequency of a given band for a given station
        lo_band={}  # The lo frequency of a given band for a given station
        sb_band={}  # The lo subband (usb or lsb) of a given band for a given station
        udc_band={} # The UDC frequency of a given band for a given station (if defined)
        dbe_band={} # A set if indice of channels to be recorded if rdbe is used

        tot_nif = 0
        min_nif = 8192
        max_nif = 0

        num_core3h    = 0
        num_core3h_eq = 0

        for band in seq["band"]:
            min_nif = min ( min_nif, len(seq["band"][band]["if_frq"]) )
            max_nif = max ( max_nif, len(seq["band"][band]["if_frq"]) )
            tot_nif = tot_nif + len(seq["band"][band]["if_frq"])
            nif= len(seq["band"][band]["if_frq"])
            band_frq_min = seq["band"][band]["if_frq"][0]
            band_frq_max = seq["band"][band]["if_frq"][nif-1] + seq["band"][band]["if_bwd"][nif-1] 
            fl_match = False
            if ( seq["station"][sta]["wiring"]["dynamic"] and not band in seq["station"][sta]["wiring"] ):
                 frq_band[band] = seq["band"][band]["if_frq"][0]
                 for bnd in seq["station"][sta]["wiring"]: 
                     if ( isinstance( seq["station"][sta]["wiring"][bnd], dict ) ):
#
                          if ( seq["station"][sta]["wiring"][bnd]["frq_min"] <= band_frq_min and \
                               seq["station"][sta]["wiring"][bnd]["frq_max"] >= band_frq_max     ):
#
                               frq_band[band] = seq["band"][band]["if_frq"][0]
                               lo_band[band]  = seq["station"][sta]["wiring"][bnd]["lo"]
                               fl_match = True
                               if ( ivrb > 1 ):
                                    print ( "gen_seq_prc-330: band= ", band, ' lo_band= ', lo_band,  'band_frq_min= ', band_frq_min ) 
                               if ( lo_band[band] < band_frq_min ):
                                    sb_band[band] = "usb"
                               else:
                                    sb_band[band] = "lsb"
                               udc_band[band] = None

                 if ( not fl_match ):
                      print ( "ERROR get_seq_prc:  sta: %s band: %s -- did not find a matching dynamic range" % \
                              ( sta, band ) )
                      exit  ( 1 )
                 if ( ivrb > 1 ):
                      print ( "DYN sta= ", sta, " band= ", band, " frq= ", sb_band[band], " sb= ", sb_band[band] )
                 continue
            elif ( not band in seq["station"][sta]["wiring"] ):
                 print ( "ERROR get_seq_prc: Did not find band %s in wiring %s for station %s in the frequency definition file: %s" % \
                         ( band, seq["station"][sta]["wiring"]["name"], sta, frq_def ) )
                 exit ( 1 )
            mode_dbbc3_v = False
            if ( not seq["station"][sta]["wiring"]["dynamic"] ):
                 if ( seq["station"][sta]["wiring"][band]["das"] == "dbbc3_v" ):
                      mode_dbbc3_v = True

            if   ( seq["station"][sta]["wiring"][band]["lo_typ"] == "a" ):
                   frq_band[band] = seq["band"][band]["if_frq"][0]
                   lo_band[band]  = seq["station"][sta]["wiring"][band]["lo"]
                   fl_match = True
                   if ( frq_band[band] > lo_band[band] ):
                        sb_band[band] = "usb"
                   else:
                        sb_band[band] = "lsb"
                   udc_band[band] = None
                   if ( ivrb > 1 ):
                        print ( "STT sta= ", sta, " band= ", band, " frq= ", sb_band[band], " sb= ", sb_band[band] )
                
            elif ( seq["station"][sta]["wiring"][band]["lo_typ"] == "b" ):
#
# ---------------- rdbe mode
#
                   frq_band[band] = seq["band"][band]["if_frq"][0] - seq["band"][band]["if_bwd"][0] + 512.0 
                   fl_match = True
                   lo_band[band]  = frq_band[band] - 1024.0 + seq["band"][band]["if_bwd"][0]/2.0
                   sb_band[band]  = "usb"
                   udc_band[band] = (lo_band[band] + luff[sta])/4.0
                   
                   dbe_band[band] = ""                
                   for i in range(0,len(seq["band"][band]["if_frq"])):
                       k = len(seq["band"][band]["if_frq"]) - i - 1
                       ind = (frq_band[band] - seq["band"][band]["if_frq"][k])/seq["band"][band]["if_bwd"][k]
                       dbe_band[band] = dbe_band[band] + "%d:" % ind
                   dbe_band[band] = dbe_band[band][0:-1] + ";"

            elif ( seq["station"][sta]["wiring"][band]["lo_typ"] == "c" ):
#
# ---------------- r2dbe mode
#
                   fl_match = True
                   frq_band[band] = seq["band"][band]["if_frq"][0] + 512.0 
                   lo_band[band]  = frq_band[band] - 1024.0 + seq["band"][band]["if_bwd"][0]/2.0
                   sb_band[band]  = "usb"
                   udc_band[band] = (lo_band[band] + luff[sta])/4.0
                   dbe_band[band] = ""                

                   for i in range(0,len(seq["band"][band]["if_frq"])):
                       k = len(seq["band"][band]["if_frq"]) - i - 1
                       ind = 16 + (frq_band[band] - seq["band"][band]["if_frq"][k])/seq["band"][band]["if_bwd"][k]
                       dbe_band[band] = dbe_band[band] + "%d:" % ind
                   dbe_band[band] = dbe_band[band][0:-1] + ";"

            elif ( seq["station"][sta]["wiring"][band]["lo_typ"] == "d" ):
#
# ---------------- rdbe mode
#
                   frq_band[band] = seq["band"][band]["if_frq"][0] - seq["band"][band]["if_bwd"][0] + 512.0 
                   fl_match = True
                   lo_band[band]  = frq_band[band] - 1024.0 + seq["band"][band]["if_bwd"][0]/2.0
                   sb_band[band]  = "lsb"
                   udc_band[band] = (lo_band[band] + luff[sta])/4.0
                   
                   dbe_band[band] = ""                
                   for i in range(0,len(seq["band"][band]["if_frq"])):
                       k = len(seq["band"][band]["if_frq"]) - i - 1
                       ind = (frq_band[band] - seq["band"][band]["if_frq"][k])/seq["band"][band]["if_bwd"][k]
                       dbe_band[band] = dbe_band[band] + "%d:" % ind
                   dbe_band[band] = dbe_band[band][0:-1] + ";"

            else:
                 print ( "ERROR get_seq_prc:  sta: %s band: %s -- unsupported lo type %s" % \
                         ( sta, band, seq["station"][sta]["wiring"]["lo_typ"] ) )
                 exit  ( 1 )
            if ( not fl_match ):
                 print ( "ERROR get_seq_prc:  sta: %s band: %s -- did not find matching lo type" % \
                         ( sta, band ) )
                 exit  ( 1 )

        if ( ivrb > 11 ):
             for band in seq["band"]:
                 print ( "GSP-231 sta: %s band: %s lo= %8.1f lb= %8.1f sb: %s nf=%2d" % \
                         ( sta, band, frq_band[band], lo_band[band], sb_band[band], len(seq["band"][band]["if_frq"]) ) )
             print ( " " )

#
# ----- Read a template for procedure 
#
        prc_tmpl = prc_dir + "/" + sta + "_template.prc"
        if ( not os.path.isfile ( prc_tmpl ) ):
             print ( "ERROR get_seq_prc: Cannot find procedure template file %s" % prc_tmpl )
             exit ( 1 )
        
        buf = read_file ( prc_tmpl )
        if ( not buf ):
             print ( "ERROR get_seq_prc: Error in reading the procedure template file %s"  % prc_tmpl )
             exit ( 1 )

#
# ----- Check style of lo naming: a,b,c,d,e,f,g,h         ( fl_lo_extra_let = True  ) or 
# -----                           a0,a1 b0,b1 c0,c1 d0,d1 ( fl_lo_extra_let = False )
#
        fl_lo_extra_let = True 
        lo_list = []
        for i in range(0,len(buf)):
            line = buf[i]                 
            if ( line[0:5] == "lo=lo" ):
                 if ( line[6:7] == "0" or line[6:7] == "1" ): 
                      fl_lo_extra_let = False

                      if ( not line[5:7] in lo_list ):
                           lo_list.append ( line[5:7] )
                 else:
                      if ( not line[5:6] in lo_list ):
                           lo_list.append ( line[5:6] )

        band_list = []
        for word in seq["band"].keys():
            band_list.append ( word.lower() )

        prc = []
        for i in range(0,len(buf)):
            line = buf[i]                 
            if ( "@update_date@" in line ):        
                 if ( pyvers >= "312000" ):
                      date_utc_now = datetime.datetime.now(datetime.UTC).strftime("%Y.%m.%d_%H:%M:%S %Z")
                      date_now     = datetime.datetime.now().astimezone().strftime("%Y.%m.%d_%H:%M:%S %z")
                 else:
                      date_utc_now = datetime.datetime.utcnow().strftime("%Y.%m.%d_%H:%M:%S UTC")
                      date_now     = datetime.datetime.now().astimezone().strftime("%Y.%m.%d_%H:%M:%S %z")
                 line = line.replace("@update_date@",date_now)
                 prc.append ( line )
                 prc.append ( '" ' )
                 prc.append ( '" Provenance: generated by:                   %s' % gsp__label )
                 prc.append ( '" Provenance: used procedure template   file: %s' % prc_tmpl   )
                 prc.append ( '" Provenance: used frequency definition file: %s' % frq_def    )
                 prc.append ( '" Provenance: used frequency sequence   file: %s' % fil_seq    )
                 continue
            elif ( "@vers@" in line ):        
#
# -------------- obsolete
#
                 continue
            elif ( "@time_stamp@" in line ):        
                 prc.append ( '" ' )
                 prc.append ( '" Hardware  setup name: %s' % seq["hds_name"] )
                 prc.append ( '" Frequency setup name: %s' % seq["mod_name"] )
                 prc.append ( '" ' )
                 kif  = 0
                 kbnd = 0
                 for band in seq["band"]:
                     kbnd = kbnd + 1
                     for i in range(0,len(seq["band"][band]["if_frq"])):
                         kif = kif + 1
                         if ( udc_band[band] ):
                              prc.append  ( '" IF: %3d Band: %1d Band_name: %1s  IF_ind: %2s Sky freq: %8.1f  Bandwidth: %5.1f LO freq: %8.1f UDC freq: %8.1f MHz  Side: %s' % \
                                      ( kif, \
                                        kbnd, \
                                        band.lower(), \
                                        i+1, \
                                        seq["band"][band]["if_frq"][i], \
                                        seq["band"][band]["if_bwd"][i], \
                                        lo_band[band], \
                                        udc_band[band], \
                                        sb_band[band]   \
                                     ) )
                         else:
                              prc.append  ( '" IF: %3d Band: %1d Band_name: %1s  IF_ind: %2s Sky freq: %9.2f Bandwidth: %5.1f LO freq: %8.1f MHz  Side: %s' % \
                                      ( kif, \
                                        kbnd, \
                                        band.lower(), \
                                        i+1, \
                                        seq["band"][band]["if_frq"][i], \
                                        seq["band"][band]["if_bwd"][i], \
                                        lo_band[band], \
                                        sb_band[band]   \
                                     ) )
                     prc.append ( '" ' )
                 continue

#
# --------- Check for patterns
#
            if ( 'add : rdbe' in line and line[0:1] != '"' ): 
                 ipa = line.index( 'add : rdbe'  )
            else:
                 ipa = None

            if ( '=dbe_chsel=' in line and line[0:1] != '"' ):
                 ipd = line.index( '=dbe_chsel=' )
            else:
                 ipd = None

            if ( '-h udc' in line and line[0:1] != '"' ):
                 iph = line.index( '-h udc' )
            else:
                 iph = None

            if ( 'rfd_atten=' in line and line[0:1] != '"' ):
                 ipr = line.index( 'rfd_atten=' )
            else:
                 ipr = None

            if ( 'udceth0 udc' in line and line[0:1] != '"' ):
                 ipu = line.index( 'udceth0 udc' )
            else:
                 ipu = None

            if ( ipa != None ):
#
# -------------- Check whether the band defined in the procedure template 
# -------------- file is present in the sequence. If not, do not propagate
# -------------- that band definition in the output procedure file
#
                 band_def = line[ipa+10:ipa+11].lower()
                 if ( not band_def in band_list ):
                      continue

            if ( iph != None ):
#
# -------------- the same check as above
#
                 band_def = line[iph+6:iph+7].lower() 
                 if ( not band_def in band_list ):
                      continue

            if ( ipr != None ):
#
# -------------- Check whether attenuation is defined in the template for the
# -------------- band that is not used. If yes, skip this line
#
                 ind_bnd_def = ipr + len('rfd_atten=' )
                 try:
                       ind_bnd = int(line[ind_bnd_def:ind_bnd_def+1])
                 except Exception as e:
                       ind_bnd = -1
                 band_name_trial = chr(ord("a") + ind_bnd)
                 if ( not band_name_trial in band_list ):
                      continue

            if ( ipu != None and len(line.split()) >= 3 ):
#
# -------------- the same
#
                 att_udc_def = line.split()[2]
                 if ( "udc" in att_udc_def ):
                      att_udc_band = att_udc_def.replace("udc","").lower()
                      if ( not att_udc_band in band_list ):
                           continue

            if ( "bbc" == line[0:3] and len(line) <= 6 ):
#
# -------------- Remove excessive bbc lines
#
                 try:
                       ind_bbc = int(line[3:6])
                 except Exception as e:
                       ind_bbc = -1

                 if ( ind_bbc > 2*tot_nif and not mode_dbbc3_v ):
                      continue
#
# --------- Replace remaining @-expressions in the template file
#
            if ( '@chs_en' in line ):
#
# -------------- Set the mode of channel definition
#
                 ib = line.index ( '@chs_en' )
                 if ( min_nif == 16 ):
#
# ------------------- All channels are used
#
                      line = line[0:ib] + '4:chsel_disable:psn_enable;'
                 else:
                      if ( max_nif > 8 ):
                           line = line[0:ib] + '4:chsel_enable:psn_enable;'
                      else:                 
#
# ------------------------ Some channels are defined
#
                           line = line[0:ib] + '2:chsel_enable:psn_enable;'
                 prc.append ( line )
                 continue
            elif ( '@chsel' in line ):
                 if ( min_nif < 16 ):
                      ind = line.index('@chsel') + 8 
                      chsel_band = line[ind-1:ind].lower()
                      if ( chsel_band == "@" ):
                           ind = line.index('@chsel') + 7 
                           chsel_band = line[ind-1:ind].lower()
                      if ( not chsel_band.lower() in band_list ):
                           continue

                      ind = line.index('@chsel')
                      line = line[0:ind] + dbe_band[chsel_band.upper()]
                      prc.append ( line )
                 continue                      
            elif ( '@hds@' in line ):
#
# -------------- Replace hardware setup name
#
                 line = line.replace ( '@hds@', seq["hds_name"] )
                 prc.append ( line )
                 continue                      
            elif ( '@mode@' in line ):
#
# -------------- Replace the sequence name
#
                 line = line.replace ( '@mode@', seq["mod_name"] )
                 prc.append ( line )
                 continue                      
            elif ( '@lo@' in line ):
#
# -------------- update lo definition
#
# -------------- get the band name that corresponds to the lo of this line
#

                 if ( line[0:1] == '"' ):
                      lo_band_name = line[15:16].lower()
                 else:
                      lo_band_name = line[5:6].lower()

                 if ( fl_lo_extra_let ):
                      if ( lo_band_name in "abcdefgh" and len(lo_list) > 4 ):
                           ip = "abcdefgh".index(lo_band_name)
                           lo_band_name = "aabbccdd"[ip].lower()
                 if ( not lo_band_name in band_list ):
                      continue

#
# -------------- Replace the lo
#
                 line = line.replace ( "@lo@", "%6.1f" % lo_band[lo_band_name.upper()] )
                 if ( '@sib@' in line ):
#
# ------------------- and it needed, the sub-band defintipm as well
#
                      line = line.replace ( '@sib@', sb_band[lo_band_name.upper()] )
            elif ( '@udc_lo@' in line ):
#
# -------------- update udc lo definition
#
                 lo_band_name = None
                 for word in line.split():
                     if ( "udcc" in word ):
                          lo_band_name = word[-1].lower()

                 if ( lo_band_name == None ):
                      err_msg = "did not find a word that starts with udcc"
                      print ( "ERROR get_seq_prc: Error in parsing line %s of template file %s -- %s" % \
                              ( line, prc_tmpl, err_msg ) )
                      exit ( 1 )

                 if ( not lo_band_name in band_list ):
                      err_msg = "word %s refers to the band that is not defined" % lo_band_name
                      print ( "ERROR get_seq_prc: Error in parsing line %s of template file %s -- %s" % \
                              ( line, prc_tmpl, err_msg ) )
                      exit ( 1 )

                 line = line.replace( '@udc_lo@', "%6.1f" % lo_band[lo_band_name.upper()] )

            elif ( '@udc_luff@' in line ):
#
# -------------- update udc lo definition
#
                 lo_band_name = None
                 for word in line.split():
                     if ( "udcc" in word ):
                          lo_band_name = word[-1].lower()

                 if ( lo_band_name == None ):
                      err_msg = "did not find a word that starts with udcc"
                      print ( "ERROR get_seq_prc: Error in parsing line %s of template file %s -- %s" % \
                              ( line, prc_tmpl, err_msg ) )
                      exit ( 1 )

                 if ( not lo_band_name in band_list ):
                      err_msg = "word %s refers to the band that is not defined" % lo_band_name
                      print ( "ERROR get_seq_prc: Error in parsing line %s of template file %s -- %s" % \
                              ( line, prc_tmpl, err_msg ) )
                      exit ( 1 )

                 line = line.replace( '@udc_luff@', "%6.1f" % udc_band[lo_band_name.upper()] )

            elif ( '@pcal_step_5@' in line ):
#
# -------------- Set the offset of pcal rail to govern the on-board hardware for pcal extraction
#
                 pcal_band_name = line[4:5].lower()
                 if ( pcal_band_name.lower() in band_list ):
                      line = line.replace( '@pcal_step_5@', "%5.3fe6" % math.fmod( lo_band[pcal_band_name.upper()]+1024.0,5.0 ) )
                      prc.append ( line )

                 continue

            elif ( '@pcal_step_10@' in line ):
#
# -------------- Set the offset of pcal rail to govern the on-board hardware for pcal extraction
#
                 pcal_band_name = line[4:5].lower()
                 if ( pcal_band_name.lower() in band_list ):
                      line = line.replace( '@pcal_step_10@', "%5.3fe6" % math.fmod( lo_band[pcal_band_name.upper()]+1024.0,10.0 ) )
                      prc.append ( line )

                 continue

            elif ( '@bit_rate@'  in line ):
                 if ( min_nif < 16 ):
                      line = line.replace('@bit_rate@','8192')
                 else:
                      line = line.replace('@bit_rate@','16384')
                 prc.append ( line )
                 continue

            elif ( '@if_offset@' in line ):
                 if_band_ind = line.index("@if_offset@") + len("@if_offset@") + 1
                 if_band_name = line[if_band_ind:if_band_ind+1].upper()
                 if ( if_band_name.lower() in band_list ):
                      if ( line[0:3] == "bbc" ):
                           bbc_ind = int( line[3:5] )
                      else:
                           bbc_ind = int( line[2:4] )
                 else:
                      err_msg = "Band %s is not defined" % if_band_name
                      print ( "ERROR get_seq_prc: Error in parsing line %s of template file %s -- %s" % \
                              ( line, prc_tmpl, err_msg ) )
                      exit ( 1 )

                 bbc_def_ind = -1
                 if ( seq["station"][sta]["wiring"][if_band_name]["das"] == "mark5b_geo2" ):
                        for bbc in mark5b_geo2:
                            if ( mark5b_geo2[bbc]["bbc_ind"] == bbc_ind ):
                                 bbc_def_ind = bbc
                                 bbc_def_chan = mark5b_geo2[bbc]["chan"]
                                 bbc_sub_band = mark5b_geo2[bbc]["sub_band"]
                                 bbc_band_ind = mark5b_geo2[bbc]["band_ind"]

                 elif ( seq["station"][sta]["wiring"][if_band_name]["das"] == "mark5b_geo3" ):
                        for bbc in mark5b_geo3:
                            if ( mark5b_geo3[bbc]["bbc_ind"] == bbc_ind ):
                                 bbc_def_ind = bbc
                                 bbc_def_chan = mark5b_geo3[bbc]["chan"]
                                 bbc_sub_band = mark5b_geo3[bbc]["sub_band"]
                                 bbc_band_ind = mark5b_geo3[bbc]["band_ind"]

                 elif ( seq["station"][sta]["wiring"][if_band_name]["das"] == "mark5b_geo4" ):
                        for bbc in mark5b_geo4:
                            if ( mark5b_geo4[bbc]["bbc_ind"] == bbc_ind ):
                                 bbc_def_ind = bbc
                                 bbc_def_chan = mark5b_geo4[bbc]["chan"]
                                 bbc_sub_band = mark5b_geo4[bbc]["sub_band"]
                                 bbc_band_ind = mark5b_geo4[bbc]["band_ind"]
                        
                 elif ( seq["station"][sta]["wiring"][if_band_name]["das"] == "mark5b_vdif" ):
                        bbc_def_ind = -1
                        for bbc in mark5b_vdif:
                            if ( mark5b_vdif[bbc]["bbc_ind"] == bbc_ind ):
                                 bbc_def_ind  = bbc
                                 bbc_def_chan = mark5b_vdif[bbc]["chan"]
                                 bbc_sub_band = mark5b_vdif[bbc]["sub_band"]
                                 bbc_band_ind = mark5b_vdif[bbc]["band_ind"]

                 if ( bbc_def_ind == -1 ):
                      err_msg = "Did not find bbc index %d" % bbc_ind
                      print ( "ERROR get_seq_prc: Error in parsing line %s of template file %s -- %s" % \
                              ( line, prc_tmpl, err_msg ) )
                      exit ( 1 )

#
# -------------- NB: bbc_def_chan here is indexed from 1, but python expects indices that start from 0
#
                 if ( bbc_band_ind > len(seq["band"][if_band_name]["if_frq"]) ):
                      err_msg = "Wrong chan_ind %d for bbc_ind %d out of %d ifs" % \
                                ( bbc_def_chan, bbc_ind, len(seq["band"][if_band_name]["if_frq"]) )
                      print ( "ERROR get_seq_prc: Error in parsing line %s of template file %s -- %s" % \
                              ( line, prc_tmpl, err_msg ) )
                      exit ( 1 )
                     
                 if ( bbc_sub_band == "U" ):
                      if_offset_frq = seq["band"][if_band_name]["if_frq"][bbc_band_ind-1] - lo_band[if_band_name]
                 else:
                      if_offset_frq = seq["band"][if_band_name]["if_frq"][bbc_band_ind-1] - lo_band[if_band_name] + \
                                      seq["band"][if_band_name]["if_bwd"][bbc_band_ind-1] 

                 line = line.replace ( '@if_offset@', "%8.2f" % if_offset_frq )
                 line = line.replace ( '@if_width@',  "%8.2f" % seq["band"][if_band_name]["if_bwd"][bbc_band_ind-1] )
                 line = line.replace(" ","")
                 if ( line[0:3] == "vci" ):
                      line = line.replace ( "," + if_band_name.lower(), "" )
                      word2_old = "," + line.split(",")[1] + ","
                      word2_new = word2_old.replace(".00",".0")[0:-1]
                      word2_new = word2_new.replace(".0","")
                      line = line.replace ( word2_old, word2_new )

                 if ( ivrb > 7 ):
                      print ( "bbc_ind %2d if_band_name %s band: %s bbc_band_ind: %d" % \
                              ( bbc_ind, if_band_name, band, bbc_band_ind ) )
                                                                             
                 if ( line[0:2] == "vc" ):
#
# ------------------- Remove lo name
#
                      ip = line.index(",")
                      line = line[0:ip] + line[ip+2:] 

                 prc.append ( line )
                 continue


            elif ( '@sideband@' in line ):
                 ib = None
                 ind_core = None
                 band_core = None
                 sample_rate = 0.0
                 for band in seq["band"]:
#
# ------------------ Find the sample rate
#
                     sample_rate = 2*seq["band"][band]["if_bwd"][0]
                     break

                 if ( "core3h=" in line ):
                      num_core3h_eq = num_core3h_eq + 1
                      if ( num_core3h_eq > 2*len(seq["band"]) ):
#
# ------------------------ Do not put @sideband@ for bands that are not used
#
                           continue
                      ib = line.index("core3h=")
                      try:
                           ind_core = int(line[ib+7:ib+8]) 
                      except Exception as e:
                           err_msg = "1: Cannot parse core index"
                           print ( "ERROR get_seq_prc: Error in parsing line %s of template file %s -- %s" % \
                                   ( line, prc_tmpl, err_msg ) )
                           exit ( 1 )

                 else:
                      num_core3h = num_core3h + 1
                      if ( num_core3h > 2*len(seq["band"]) ):
#
# ------------------------ Do not put @sideband@ for bands that are not used
#
                           continue
                      if ( "mode=" in line ):
                          ib = line.index("mode=")
                          try:
                               ind_core = int(line[ib+5:ib+6])
                          except Exception as e:
                               err_msg = "2: Cannot parse core index"
                               print ( "ERROR get_seq_prc: Error in parsing line %s of template file %s -- %s" % \
                                       ( line, prc_tmpl, err_msg ) )
                               exit ( 1 )
                 band_core = None
                 if ( ind_core != None ):
                      bnd_ind = int((ind_core-1)/2) + 1
                      if ( bnd_ind <= len(seq["band"]) ):
                           band_core = list(seq["band"].keys())[bnd_ind-1]

                 if ( ivrb > 1 ):
                      print ( "GET_SEQ_PRC-867 sta= ", sta, "band_core= ", band_core, "ind_core=", ind_core, "sb_band= ", sb_band )
#                 print ( "GET_SEQ_PRC-868 band_core_name= ", band_core ) # %%%%%%%%
#                 print ( "GET_SEQ_PRC-869 wir=", seq["station"][sta]["wiring"].keys() ) # %%%%%%%%
#                 print ( "GET_SEQ_PRC-870 cor=", seq["station"][sta]["wiring"][band_core] ) # %%%%%%%%
                 if ( band_core != None ):
                      if ( "dynamic" in seq["station"][sta]["wiring"].keys() ):
                           if ( ivrb > 1 ):
                                print ( "GET_SEQ_PRC-879 sta= ", sta, "band_core=", band_core, 'sb_band[band_core]= ', sb_band[band_core] )
#
# ------------------------ Dynamic case: band_core was computed
#
                           if ( sb_band[band_core] == "lsb" ):
                                side_band = "lsb"
                           elif ( sb_band[band_core] == "usb" ):
                                side_band = "usb"
                           else:
                                side_band = "usb"
                      else:
                           if ( ivrb > 1 ):
                                print ( "GET_SEQ_PRC-875 sta= ", sta, "if_typ= ", seq["station"][sta]["wiring"][band_core]["if_typ"], " sb= ", sb_band[band_core] )
#
# ------------------------ band_core was defined in the wiring statement
#
                           if ( seq["station"][sta]["wiring"][band_core]["if_typ"] != "c" ):
                                if ( sb_band[band_core] == "lsb" ):
                                     side_band = "lsb"
                                elif ( sb_band[band_core] == "usb" ):
                                     side_band = "usb"
                                else:
                                     side_band = "usb"
                           else:
#
# ------------------------ Special case: swap usb and lsb
#
                                if ( sb_band[band_core] == "lsb" ):
                                     side_band = "usb"
                                elif ( sb_band[band_core] == "usb" ):
                                     side_band = "lsb"
                                else:
                                     side_band = "usb"
                 else:
#
# ------------------ Default: usb
#
                     if ( ivrb > 1 ):
                          print ( "GET_SEQ_PRC-896 sta= ", sta, "if_typ= ", seq["station"][sta]["wiring"][band_core]["if_typ"], " sb= ", sb_band[band_core] )
                     side_band = "usb"

#
# -------------- This is very tricky part. 
# -------------- '33' means 32MHz channels and setting the usb netside
# -------------- 'cc' means 32MHz channels and setting the lsb netside
# -------------- If the input RF signal is USB, we set the lsb netside (33)
# -------------- If the input RF signal is LSB, we set the usb netside (33)
#
                 if ( ivrb > 1 ):
                      print ( "GET_SEQ_PRC-900 sta= ", sta, "side_band= ", side_band )
                 if ( side_band == "lsb" ):
                      line = line.replace('@sideband@','0x33333333')
                 elif ( side_band == "usb" ):
                      line = line.replace('@sideband@','0xcccccccc')
                 else:
                      line = line.replace('@sideband@','0xcccccccc')

                 if ( line[0:5] == "band@" ):
                      if ( band_core != None ):
                           line = line[7:]
                      else:
                           line = line[7:21] + "stop"

                 if ( '@two_if_width@' in line ):
                      sample_rate_str = "%6.1f" % sample_rate 
                      line = line.replace( "@two_if_width@", sample_rate_str.replace(" ","") )

                 prc.append ( line )
                 continue


            elif ( '@two_if_width@' in line ):
                 for band in seq["band"]:
#
# ------------------ Replace with the sample rate
#
                     sample_rate = 2*seq["band"][band]["if_bwd"][0]
                     sample_rate_str = "%6.1f" % sample_rate 
                     line = line.replace( "@two_if_width@", sample_rate_str.replace(" ","") )
                     break
                 if ( '@if_mask@' in line ):
                      if ( seq["band"][band]["if_bwd"][0] == 16.0 ):
                           line = line.replace( '@if_mask@', '0xffffffff' )
                      else:
                           print ( "gen_seq_prc-966: Trap of internal control -- do not " + 
                                   "know how to set up mask for bandwidth %d" % \
                                   seq["band"][band]["if_bwd"][0] )
                           exit  ( 1 )
                 prc.append ( line )
                 continue
            elif ( '@if_band_used@' in line ):
                 try:
                      if ( len(line.split(",")[0].split("=")) == 3 ):
                           band_num  = int(line.split(",")[0].split("=")[2])
                      elif ( len(line.split(",")[0].split("=")) == 2 ):
                           band_num  = int(line.split(",")[0].split("=")[1])
                      else:
                           err_msg = "cannot extract a numeric band number"
                           print ( "ERROR get_seq_prc: Error in parsing line %s of template file %s -- %s" % \
                                   ( line, prc_tmpl, err_msg ) )
                 except Exception as e:
                      err_msg = "did not find a numeric band number"
                      print ( "ERROR get_seq_prc: Error in parsing line %s of template file %s -- %s" % \
                              ( line, prc_tmpl, err_msg ) )
                      exit ( 1 )
                 band_name_trial = chr(ord("A") + int((band_num - 1)/2)).lower()
                 if ( band_name_trial in band_list ):
                      prc.append ( line.replace( " @if_band_used@","" ) )
                 continue
                      
            elif ( '@start_or_stop_vdif@' in line ):
                 try:
                      if ( len(line.split(",")[0].split("=")) == 3 ):
                           band_num  = int(line.split(",")[0].split("=")[2])
                      elif ( len(line.split(",")[0].split("=")) == 2 ):
                           band_num  = int(line.split(",")[0].split("=")[1])
                      else:
                           err_msg = "cannot extract a numeric band number"
                           print ( "ERROR get_seq_prc: Error in parsing line %s of template file %s -- %s" % \
                                   ( line, prc_tmpl, err_msg ) )
                 except Exception as e:
                      err_msg = "did not find a numeric band number"
                      print ( "ERROR get_seq_prc: Error in parsing line %s of template file %s -- %s" % \
                              ( line, prc_tmpl, err_msg ) )
                      exit ( 1 )
                 band_name_trial = chr(ord("A") + int((band_num - 1)/2)).lower()
                 if ( band_name_trial in band_list ):
                      prc.append ( line.replace( "@start_or_stop_vdif@","start vdif" ) )
                 else:
                      prc.append ( line.replace( "@start_or_stop_vdif@","stop" ) )
                 continue
            elif ( '@core_by_2^16@' in line ):
                 try:
                      if ( len(line.split(",")[0].split("=")) == 3 ):
                           band_num  = int(line.split(",")[0].split("=")[2])
                      elif ( len(line.split(",")[0].split("=")) == 2 ):
                           band_num  = int(line.split(",")[0].split("=")[1])
                      else:
                           err_msg = "cannot extract a numeric band number"
                           print ( "ERROR get_seq_prc: Error in parsing line %s of template file %s -- %s" % \
                                   ( line, prc_tmpl, err_msg ) )
                 except Exception as e:
                      err_msg = "did not find a numeric band number"
                      print ( "ERROR get_seq_prc: Error in parsing line %s of template file %s -- %s" % \
                              ( line, prc_tmpl, err_msg ) )
                      exit ( 1 )
                 band_name_trial = chr(ord("A") + int((band_num - 1)/2)).lower()
                 if ( band_name_trial in band_list ):
                      prc.append ( line.replace( "@core_by_2^16@","%-6d" % (ind_used_dbbc_band*65536) ) )
                      ind_used_dbbc_band = ind_used_dbbc_band + 1
                 continue
                      
            elif ( '@dbbc3_bbc@' in line ):
#
# -------------- Generate a section of IF frequency offsets for dbbc3 procedure file
#
                 polar_dbbc3 = ['h', 'v']
#
# -------------  Find DAS
#
                 for band in seq["station"][sta]["wiring"]:
                     if ( isinstance( seq["station"][sta]["wiring"][band], dict ) ):
                          if ( "das" in seq["station"][sta]["wiring"][band].keys() ):
                               das = seq["station"][sta]["wiring"][band]["das"]
#
# -------------- Run over bands
#
                 for band in seq["band"]:
                     band_dbbc3 = chr( int(ord("a")) + 2*int(ord( band.lower()) - ord("a")) )
#
# ------------------ Run over polarizations
#
                     k_bbc = 8*(ord(band_dbbc3) - ord("a"))
                     for pol in polar_dbbc3:
                         line = '" Band: ' + band_dbbc3.upper() + " Polar: " + pol
                         prc.append ( line )
#
# ---------------------- Run over IFs
#
                         for i in range(0,len(seq["band"][band]["if_frq"])):
                             k = len(seq["band"][band]["if_frq"]) - i - 1
#
# -------------------------- Set the order of IFs: direct when IFs are in the increasing order
# -------------------------- and iverse when IFs ar in the descreasing order of their frequencies
#
                             if ( seq["band"][band]["order"] == "dir" ):
                                  if_ind =  i
                             else:
                                  if_ind =  k
                             if ( sb_band[band] == "usb" ):
#
# ------------------------------- USB: direct order of IFs
#
                                  if_offset = seq["band"][band]["if_frq"][if_ind] - lo_band[band] + \
                                              seq["band"][band]["if_bwd"][if_ind]
                             else:
#
# ------------------------------- Inverted order of IFs
#
                                  if_offset = lo_band[band] - seq["band"][band]["if_frq"][if_ind] - \
                                              seq["band"][band]["if_bwd"][if_ind]
                             k_bbc = k_bbc + 1
#%                             print ( "sta= ", sta, ' band: ' , band, ' k1: ', seq["station"][sta]["wiring"]  ) # %%%%
#%                             print ( "sta= ", sta, ' band: ' , band, ' k2: ', seq["station"][sta]["wiring"]["dynamic"] ) # %%%%

                             
                             if ( not mode_dbbc3_v ):
                                  line = "bbc%03d=%6.1f,%s,%5.1f" % ( k_bbc, if_offset, \
                                                                       band_dbbc3, seq["band"][band]["if_bwd"][k] )
                             else:
                                  k_bbc_au1      = au1_map[k_bbc]["bbc"]
                                  band_dbbc3_au1 = au1_map[k_bbc]["lo"]
                                       
                                  line = "%s=%6.1f,%s,%5.1f" % ( k_bbc_au1, if_offset, \
                                                                 band_dbbc3_au1, seq["band"][band]["if_bwd"][k] )
                             line = line.replace(" ","")
                             prc.append ( line )
                         band_dbbc3 = chr ( ord(band_dbbc3) + 1 ) # update dbbc3 band label, NB: not the same as rdbe label!
                 continue
            elif ( 'if' == line[0:2] and "=" == line[3:4] ):
#
# -------------- Remove those IF that are not used
#
                 fl_if_used = False
                 for band in seq["band"]:
                     band_dbbc3 = chr( int(ord("a")) + 2*int(ord( band.lower()) - ord("a")) )
                     if ( band_dbbc3 == line[2:3] ):
                          fl_if_used = True
                     band_dbbc3 = chr( int(ord("a")) + 2*int(ord( band.lower()) - ord("a")) + 1 )
                     if ( band_dbbc3 == line[2:3] ):
                          fl_if_used = True

                 if ( not fl_if_used ):
                      continue
#
# --------- No changes for this line
#
            prc.append ( line )

        prc_fileout = dir_out + "/" + seq["mod_name"] + "_" + sta + ".prc"
        (ret,err) = write_file ( prc, prc_fileout )
        if ( ret != 0 ):
             print ( "ERROR get_seq_prc: Error in writing output procedure file %s" % prc_filout )
             exit ( 1 )
                 
        if ( fl_vex_created ):
             continue
#
# ----- Generate vex frequency definition file.
# ----- We do it when processing the first station
#
        vex= []
#
# ----- Update comments
#
        vex.append ( '*' )
        vex.append ( '*   Hardware  setup name: %s' % seq["hds_name"] )
        vex.append ( '*   Frequency setup name: %s' % seq["mod_name"] )
        if ( pyvers >= "312000" ):
             date_now     = datetime.datetime.now().astimezone().strftime("%Y.%m.%d_%H:%M:%S %z")
        else:
             date_now     = datetime.datetime.now().astimezone().strftime("%Y.%m.%d_%H:%M:%S %z")
        vex.append ( '*   Generated from        %s by %s on %s' % ( fil_seq, gsp__label, date_now ) )

        ind_chn = 0
        ind_bbc = 0
        sample_rate = 0.0
#
# ----- Cycle over babds
#
        for band in seq["band"]:
            if ( ivrb > 7 ):
                 print ( "GSP-1012 band= ", band )
            vex.append ( '*' )
#
# --------- Getting a band name. 
# --------- NB: it may happen that the station has a dynamic wiring.
# ---------     Then we need to use the band name defined in the station wiring
#
            band_name = band
            if ( seq["station"][sta]["wiring"]["dynamic"] and not band in seq["station"][sta]["wiring"] ):
                 for bnd in seq["station"][sta]["wiring"]:
                     if ( isinstance( seq["station"][sta]["wiring"][bnd], dict ) ):
                          band_name = bnd

            if ( seq["station"][sta]["wiring"][band_name]["das"] == 'rdbe'      or \
                 seq["station"][sta]["wiring"][band_name]["das"] == 'r2dbe'     or \
                 seq["station"][sta]["wiring"][band_name]["das"] == 'dbbc3'     or \
                 seq["station"][sta]["wiring"][band_name]["das"] == 'dbbc3_au1'    ):
#
# -------------- Case of rdb2, r2dbe or dbbc3
#
                 sbd = "L"
                 for pol in ['H', 'V']:
                     for i in range(0,len(seq["band"][band]["if_frq"])):
                         ind_chn = ind_chn + 1
                         ind_bbc = ind_bbc + 1
#
# ---------------------- IF index runs from the top frequency to the lowest frequency.
# ---------------------- The reported frequency is IF + bws
#
                         k = len(seq["band"][band]["if_frq"]) - i - 1
                         vex.append ( '    chan_def = &%s : %9.2f MHz : %s : %6.2f MHz : &CH%03d : &BBC%03d : &L_cal;' % \
                                      ( pol, seq["band"][band]["if_frq"][k] + seq["band"][band]["if_bwd"][k], \
                                        sbd, seq["band"][band]["if_bwd"][k], ind_chn, ind_bbc ) \
                                    )
                         sample_rate = 2*seq["band"][band]["if_bwd"][k] 

            elif ( seq["station"][sta]["wiring"][band]["das"] == 'cdas_rpol'  ):
#
# -------------- CDAS case
#
                 pol = 'R'
                 for i in range(0,len(seq["band"][band]["if_frq"])):
                     ind_chn = ind_chn + 1
                     ind_bbc = ind_bbc + 1
                     k = len(seq["band"][band]["if_frq"]) - i - 1
                     vex.append ( '    chan_def = &%s : %9.2f MHz : %s : %6.2f MHz : &CH%03d : &BBC%03d : &L_cal;' % \
                                  ( pol, seq["band"][band]["if_frq"][k] + seq["band"][band]["if_bwd"][k], \
                                    sbd, seq["band"][band]["if_bwd"][k], ind_chn, ind_bbc ) \
                                )
                     sample_rate = 2*seq["band"][band]["if_bwd"][k] 
            elif ( seq["station"][sta]["wiring"][band]["das"] == 'cdas_dual'  ):
#
# ---------------- Do not know
#
                   continue
            elif ( seq["station"][sta]["wiring"][band]["das"] == 'mark5b_geo2' or \
                   seq["station"][sta]["wiring"][band]["das"] == 'mark5b_geo3' ):
#
# -------------- Mark5b in geodetic wiring mode
#
                 for i in range(0,len(seq["band"][band]["if_frq"])):
                     ind_chn = ind_chn + 1
                     ind_bbc = ind_bbc + 1
#
# ------------------ Set band name
#
                     if ( seq["band"][band]["if_frq"][i] > 3000.0 ):
                          band_name = 'X'
                     else:
                          band_name = 'S'
#
# ------------------ Frequencies are going from  upper to lower
#
                     k = len(seq["band"][band]["if_frq"]) - i - 1
#
# ------------------ FIXME!! We need to check here subband 
#
                     sbd = "?"
                     vex.append ( '    chan_def = &%s : %9.2f MHz : %s : %6.2f MHz : &CH%03d : &BBC%03d : &L_cal;' % \
                                  ( band_name, seq["band"][band]["if_frq"][k] + seq["band"][band]["if_bwd"][k], \
                                    sbd, seq["band"][band]["if_bwd"][k], \
                                    mark5b_geo3[i+1]["bbc_ind"], mark5b_geo3[i+1]["chan"] \
                                  ) \
                                )
                     sample_rate = 2*seq["band"][band]["if_bwd"][k] 
            elif ( seq["station"][sta]["wiring"][band]["das"] == 'mark5b_geo4' ):
#
# -------------- Mark5b in geodetic wiring mode
#
                 for i in range(0,len(seq["band"][band]["if_frq"])):
                     ind_chn = ind_chn + 1
                     ind_bbc = ind_bbc + 1
#
# ------------------ Set band name
#
                     if ( seq["band"][band]["if_frq"][i] > 3000.0 ):
                          band_name = 'X'
                     else:
                          band_name = 'S'
#
# ------------------ Frequencies are going from  upper to lower
#
                     k = len(seq["band"][band]["if_frq"]) - i - 1
#
# ------------------ FIXME!! We need to check here subband 
#
                     sbd = mark5b_geo4[ind_bbc]["sub_band"]
                     if ( sbd == "U" ):
                          vex.append ( '    chan_def = &%s : %9.2f MHz : %s : %6.2f MHz : &CH%03d : &BBC%03d : &L_cal;' % \
                                       ( band_name, seq["band"][band]["if_frq"][i], \
                                         sbd, seq["band"][band]["if_bwd"][k], \
                                         mark5b_geo4[i+1]["bbc_ind"], mark5b_geo4[i+1]["chan"] \
                                       ) \
                                     )
                     elif ( sbd == "L" ):
                          vex.append ( '    chan_def = &%s : %9.2f MHz : %s : %6.2f MHz : &CH%03d : &BBC%03d : &L_cal;' % \
                                       ( band_name, seq["band"][band]["if_frq"][i] + seq["band"][band]["if_bwd"][k], \
                                         sbd, seq["band"][band]["if_bwd"][k], \
                                         mark5b_geo4[i+1]["bbc_ind"], mark5b_geo4[i+1]["chan"] \
                                       ) \
                                     )
                     else:
                          vex.append ( '    chan_def = &%s : %9.2f MHz : %s : %6.2f MHz : &CH%03d : &BBC%03d : &L_cal;' % \
                                       ( band_name, seq["band"][band]["if_frq"][i], \
                                         "?", seq["band"][band]["if_bwd"][k], \
                                         mark5b_geo4[i+1]["bbc_ind"], mark5b_geo4[i+1]["chan"] \
                                       ) \
                                     )
                     sample_rate = 2*seq["band"][band]["if_bwd"][k] 
            elif ( seq["station"][sta]["wiring"][band]["das"] == 'vdif_geo'   ):
                 for i in range(0,len(seq["band"][band]["if_frq"])):
                     ind_chn = ind_chn + 1
                     ind_bbc = ind_bbc + 1
                     if ( seq["band"][band]["if_frq"][k] > 3000.0 ):
                          band_name = 'X'
                     else:
                          band_name = 'S'
                     k = len(seq["band"][band]["if_frq"]) - i - 1
                     vex.append ( '    chan_def = &%s : %9.2f MHz : %s : %6.2f MHz : &CH%03d : &BBC%03d : &L_cal;' % \
                                  ( band_name, seq["band"][band]["if_frq"][k] + seq["band"][band]["if_bwd"][k], \
                                    sbd, seq["band"][band]["if_bwd"][k], 
                                    mark5b_vdif[i]["bbc_ind"], mark5b_vdif[i]["chan"] \
                                  ) \
                                )
                     sample_rate = 2*seq["band"][band]["if_bwd"][k] 
        vex.append ( '*' )
        vex.append ( '    sample_rate = %6.1f Ms/sec;' % sample_rate )

        vex_fileout = dir_out + "/" + seq["hds_name"] + "_" + seq["mod_name"] + "_vex.frq"
        (ret,err) = write_file ( vex, vex_fileout )
        if ( ret != 0 ):
             print ( "ERROR get_seq_prc: Error in writing output vex template file %s" % vex_filout )
             exit ( 1 )

        fl_vex_created = True

    return ( 0,[])
#
# ------------------------------------------------------------------------
#
def main():
    """ 
    Main program of the gen_seq_prc utility
    """

    parser = argparse.ArgumentParser( description=gsp__label )

    parser.add_argument ( "-s", "--seq_file", \
                          action="store", \
                          dest="fil_seq", \
                          metavar="value", \
                          help="Frequency sequence file" )

    parser.add_argument ( "-o", "--dir_out", \
                          action="store", \
                          dest="dir_out", \
                          metavar="value", \
                          help="Oitput directory file" )

    parser.add_argument ( "-v", "--verbosity", \
                          action="store", \
                          dest="verb", \
                          default=0, \
                          metavar="value", \
                          type=int, \
                          help="Verbosity level" )
#
# --- Get and parse options
#
    args = parser.parse_args()

    if ( not args.fil_seq ):
         print ( "Frequency sequence file is not specified" )
         exit ( 1 )

    if ( not args.dir_out ):
         print ( "Output directory is not specified" )
         exit ( 1 )

    if ( not os.path.isfile ( args.fil_seq ) ):
         print ( "Frequency sequence file %s is not found" % args.fil_seq )
         exit ( 1 )

    if ( not os.path.isdir ( args.dir_out ) ):
         print ( "Output directory %s is not found" % args.dir_out )
         exit ( 1 )


    if ( not args.dir_out ):
         print ( "Output directory file is not specified" )
         exit ( 1 )

    (ret,out) = gen_seq_prc ( args.fil_seq, args.dir_out, args.verb )

if __name__ == "__main__":
    try:
        pyvers = "%1d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( pyvers < "302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        main()
    except KeyboardInterrupt:
        print ( "pf.py: Interrupted" )
        exit ( 1 )

