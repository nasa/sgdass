#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   Program for computing the series of mass loading.                  *
# *                                                                      *
# * ### 12-APR-2016   loading.py    v2.2  (c)  L. Petrov 25-MAY-2017 ### *
# *                                                                      *
# ************************************************************************
"""
import sys, os, shutil, time, subprocess, datetime, signal
import optparse 
import math
from   Message           import *
from   malo_exe          import *
from   geos_oper_config  import *
global malo_child_pid

TMP_DIR   = "/tmp"

hostname = os.uname()[1]

if   ( hostname == "astrogeo" or hostname == "earthrotation" ): 
       host_suffix = "astrogeo"
       parallel_opt = "-P 16 --keep-order"  # parallel option
       os.environ["OMP_NUM_THREADS"] = "1"
elif ( hostname == "gs698-geopod.gsfc.nasa.gov" or \
       hostname == "gs698-i2pod.gsfc.nasa.gov"     ):
       host_suffix = "geopod"
       parallel_opt = "-P 30 --keep-order"  # parallel option
       os.environ["OMP_NUM_THREADS"] = "1"
elif ( hostname == "gs61a-geodev-a.gsfc.nasa.gov" or \
       hostname == "gs61a-geodev-a"                  ):
       host_suffix = "deva"
       parallel_opt = "-P 30 --keep-order"  # parallel option
       os.environ["OMP_NUM_THREADS"] = "1"
else:
       host_suffix = "unknown"

class config_class:
   def __init__ ( self, filename ):
       self.filename           = filename

   def init ( self ):
       __init__ ( self )
#
# ------------------------------------------------------------------------
#
def main():
    if ( len(sys.argv) <= 5 ):
         print ( "Usage: loading.py type data date_beg date_end ivrb" )
         exit ( 1 )
    typ  = sys.argv[1]
    data = sys.argv[2]
    date_beg  = sys.argv[3]
    date_end  = sys.argv[4]
    ivrb_str  = sys.argv[5]
    ivrb      = int(ivrb_str)


#
# --- Determine malo_script_dir  and  malo_share_dir
#
    malo_script_dir = os.popen("malo_inq script").read().rstrip()
    malo_share_dir  = os.popen("malo_inq share").read().rstrip()

#
# === Determine control file
#
    merra2_typ_config   = { "lws": malo_share_dir + "/" + host_suffix + "_lws_merra2.conf", \
                            "atm": malo_share_dir + "/" + host_suffix + "_atm_merra2.conf"  \
                          }
    
    geosfpit_typ_config = { "lws": malo_share_dir + "/" + host_suffix + "_lws_geosfpit.conf", \
                            "atm": malo_share_dir + "/" + host_suffix + "_atm_geosfpit.conf"  \
                          }
    
    omct05_typ_config   = {
                            "nto": malo_share_dir + "/" + host_suffix + "_nto_omct05.conf"    \
                          }

    mpiom06_typ_config  = {
                            "nto": malo_share_dir + "/" + host_suffix + "_mpiom06.conf"    \
                          }
    mpiom07_typ_config  = {
                            "nto": malo_share_dir + "/" + host_suffix + "_mpiom07.conf"    \
                          }

    got410c_typ_config  = {
                            "toc": malo_share_dir + "/" + host_suffix + "_toc_got410c.conf"   \
                          }

    fes2014b_typ_config = {
                            "toc": malo_share_dir + "/" + host_suffix + "_toc_fes2014b.conf"  \
                           }

    equil01_typ_config  = {
                            "toc": malo_share_dir + "/" + host_suffix + "_toc_equil01.conf"   \
                          }

    equil02_typ_config  = {
                            "toc": malo_share_dir + "/" + host_suffix + "_toc_equil02.conf"   \
                          }
    psi1_typ_config  = {
                            "toc": malo_share_dir + "/" + host_suffix + "_toc_psi1.conf"   \
                          }
    
    typ_config = {
                    "geosfpit": geosfpit_typ_config, \
                    "merra2":   merra2_typ_config,   \
                    "omct05":   omct05_typ_config,   \
                    "mpiom06":  mpiom06_typ_config,  \
                    "mpiom07":  mpiom07_typ_config,  \
                    "got410c":  got410c_typ_config,  \
                    "fes2014b": fes2014b_typ_config, \
                    "equil01":  equil01_typ_config,  \
                    "equil02":  equil02_typ_config,  \
                    "psi1":     psi1_typ_config      \
                 }
    
    typ_proc  = { "lws": malo_script_dir + "/lws_loading.py", \
                  "atm": malo_script_dir + "/atm_loading.py", \
                  "nto": malo_script_dir + "/nto_loading.py", \
                  "toc": malo_script_dir + "/har_loading.py"  \
                }

    typ_mode = { "lws": "111111111", \
                 "atm": "111111111", \
                 "nto": "111111111", \
                 "toc": "111111"     \
               }
    if ( os.getenv("MALO_SPR_ONLY") == "YES" ):
         typ_mode = { "lws": "100000000", \
                      "atm": "100000000", \
                      "nto": "100000000"  \
                    }
#
# ===
#

    if ( not data in typ_config.keys() ):
         print ( "Loading data", data, "is not supported. Supported data:", \
                  ", ".join(typ_config.keys()) )
         exit  ( 1 )

    if ( not typ in typ_config[data].keys() ):
         print ( "Loading type", typ, "is not supported. Supported types:", \
                  ", ".join(typ_mode.keys()) )
         exit  ( 1 )
         
    if ( typ == "atm" or typ == "lws" or typ == "nto" or typ == "toc" ):
         config_file = typ_config[data][typ]

         config = config_class ( config_file )
         parse_geos_oper_config ( config )

         with open ( config.load_conf ) as f:
            conf_buf = f.readlines()
         f.close ()

         dir = config.geos_heb_dir 

    if ( typ == "toc" ):
         loa_com = typ_proc[typ] + " " + \
                   "toc"         + " " + \
                   data          + " " + \
                   typ_mode[typ] + " " + \
                   ivrb_str

    else:
         if ( date_beg.find(".") > 0 ):
              date_beg = date_beg[0:4] + date_beg[5:7] + date_beg[8:10] + \
                          date_beg[10:13] + date_beg[14:17] 
         if ( len(date_beg) < 8 ):
              print ( "Argument date_beg", date_beg, "is too short" )
              exit ( 1 )
     
         if ( len(date_beg) < 11 ):
              date_beg = date_beg[0:8] + "_0000"
       
         if ( date_end.find(".") > 0 ):
              date_end = date_end[0:4] + date_end[5:7] + date_end[8:10] + \
                          date_end[10:13] + date_end[14:17] 
         if ( len(date_end) < 8 ):
              print ( "Argument date_end", date_end, "is too short" )
              exit ( 1 )
     
         if ( len(date_end) < 11 ):
              date_end = date_end[0:8] + "_0000"

         date_list = []
         for paths, dirs, files in os.walk(dir):
             for file in files: 
                 if ( file.find ( config.pivot_sds[0] + "_" ) == 0  or \
                      file.find ( ".asc" ) > 0 ):
                      if ( file.find ( ".heb" ) > 0 ):
                           if ( typ == "nto" and data == "omct05" ):
                                date_str = file[ len(config.pivot_sds[0])+1 : len(config.pivot_sds[0])+9 ] + \
                                           "_0000"
                           else:
                                date_str = file[ len(config.pivot_sds[0])+1 : len(config.pivot_sds[0])+14 ]
                           
                      elif ( file.find ( ".asc" ) > 0 ):
#
# ------------------------ Case of OMCT05
#
                           date_str = file[len(file)-19:len(file)-9].replace("-","") # + "_0000"

                      if ( date_str >= date_beg and date_str <= date_end ):
                           if ( date_str[9:11] == "00" or \
                                date_str[9:11] == "03" or \
                                date_str[9:11] == "06" or \
                                date_str[9:11] == "09" or \
                                date_str[9:11] == "12" or \
                                date_str[9:11] == "15" or \
                                date_str[9:11] == "18" or \
                                date_str[9:11] == "21"    ):

                                if ( not ( date_str in date_list ) ):
                                     date_list.append ( date_str )

         date_list.sort(reverse=True)
         if ( len(date_list) < 1 ):
              print ( "No data were found or loading computation " + \
                      "for time range [%s, %s] in %s" % \
                      ( date_beg, date_end, dir ) ) 
              exit  ( 1 )

         if ( ivrb > 1 ): 
              print ( "Loading for %d dates will be computed" % len(date_list) )
              sys.stdout.flush()

         fil_dates = TMP_DIR + "/loa__" + "%05d" % os.getpid() + ".dates"

         w = open(fil_dates,"w")
         for line in date_list:
             print ( line, file=w )
         w.close()

         loa_com = "parallel"            + " " + \
                   parallel_opt          + " " + \
                   "-a " + fil_dates     + " " + \
                   typ_proc[typ]         + " " + \
                   typ_config[data][typ] + " " + \
                   typ_mode[typ]         + " " + \
                   "{}"                  + " " + \
                   ivrb_str

    if ( ivrb > 1 ): 
         print ( "Executing command", loa_com )
         sys.stdout.flush()

    (ret, out) = exe ( loa_com, 2 )
    if ( ret != 0 ):
         print ( "Error in loa_com: ", "\n".join(out) )
         exit  ( 1 )

#
# ------------------------------------------------------------------------
#

if __name__ == "__main__":
    vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    signal.signal ( signal.SIGINT,  malo_signal_handler )
    signal.signal ( signal.SIGTERM, malo_signal_handler )
    signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
    main()
