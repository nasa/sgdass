#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   Program for computing loading in harmonic variations of            *
# *   the geopysical fluilds/                                            *
# *                                                                      *
# *   Usage: har_loading.py filcnf mode ivrb                             *
# *   where                                                              *
# *          filcnf -- Control file for computation of loading           *
# *                    displacements of this type.                       *
# *          typ    --                                                   *
# *          model  --                                                   *
# *          mode   -- an ascii string of 9 characters consisting        *
# *                    0 and 1. 1 in the th position turns on            *
# *                    a component of loading computation. Usually,      *
# *                    string 111111111 is used.                         *
# *                                                                      *
# *          ivrb   -- Verbosity level:                                  *
# *                    0 -- silent                                       *
# *                    1 -- normal verbosity                             *
# *                   >1 -- debugging mode                               *
# *                   >3 -- temporary files are not removed.             *
# *                                                                      *
# * ###  25-MAY-2017  har_loading  v1.4 (c) L. Petrov   19-MAR-2020 ###  *
# *                                                                      *
# ************************************************************************
"""
import sys, os, shutil, time, subprocess, datetime, signal
import optparse 
import math
import datetime
from   datetime          import timedelta
from   Message           import *
from   geos_oper_config  import *
from   malo_exe          import *

tmp_dir = "/tmp"

hostname = os.uname()[1]

if   ( hostname == "astrogeo" or hostname == "earthrotation" ): 
       host_suffix = "astrogeo"
       os.environ["OMP_NUM_THREADS"] = "16"
elif ( hostname == "gs698-geopod.gsfc.nasa.gov" or \
       hostname == "gs61a-geopod.gsfc.nasa.gov" or \
       hostname == "gs698-i2pod.gsfc.nasa.gov"     ): 
       host_suffix = "geopod"
       os.environ["OMP_NUM_THREADS"] = "8"
elif ( hostname == "gs61a-geodev-a" or \
       hostname == "gs61a-geodev-a.gsfc.nasa.gov" ):
       host_suffix = "deva"
       os.environ["OMP_NUM_THREADS"] = "30"
else:
       host_suffix = "unknown"

class config_class:
   def __init__ ( self, filename ):
       self.filename           = filename
       self.name               = None
       self.pivot_sds          = []
       self.begin_date         = None
       self.end_date           = None
       self.time_step          = None
       self.url_template       = None
       self.epochs_per_file    = None
       self.merra_temp_dir     = None
       self.merra_heb_dir      = None
       self.merra_dir          = None
       self.to_heb_exe         = None
       self.to_love_exe        = None
       self.malo_exe           = None
       self.gen_spr_exe        = None
       self.gen_bdsp_exe       = None
       self.gmao_gh            = None
       self.malo_elev          = None
       self.load_bdsp_dir      = None
       self.load_list_wc       = None
       self.load_grid_wc       = None
       self.load_grid_conf     = None
       self.load_list_conf     = None
       self.load_list_dir      = None
       self.load_grid_dir      = None
       self.load_d1_conf       = None
       self.load_d1_list_dir   = None
       self.load_d1_grid_dir   = None
       self.load_grid_pref     = None
       self.load_list_pref     = None
       self.load_spl_pref      = None
       self.load_d1_grid_pref  = None
       self.load_d1_list_pref  = None
       self.load_d1_spl_pref   = None
       self.vgep_dir           = None
       self.vgep_pref          = None
       self.vgep_wc            = None
       self.aam_exe            = None
       self.aam_ser_exe        = None
       self.aam_igh            = None
       self.aam_ogh            = None
       self.aam_ls_mask        = None
       self.aam_pref           = None
       self.aam_ser_file       = None
       self.malo_ls_mask       = None
       self.upgrid_ls_mask     = None
       self.sc_file            = None
       self.loa_comm           = None
       self.loa_descr          = None
       self.compress_com       = None
       self.stop_file          = None
       self.log_file           = None
       self.log_file_handle    = None
       self.err_file           = None
       self.suc_file           = None
       self.lock_file          = None
       self.lock_timeout       = None

       self.date_list      = []

   def init ( self ):
       __init__ ( self )

#
#
# $MALO_DIR/bin_static/loading_heb_to_spl_heb /imls/load_har_grid/atm/merra2/atm_merra2_harmod.heb /imls/load_har_spl/atm/merra2/atm_merra2_spl_harmod.heb
#
# $MALO_DIR/bin_static/loading_spl_heb_to_sta /imls/load_har_spl/atm/merra2/atm_merra2_spl_harmod.heb $MALO_DIR/share/loading.sta /imls/load_har_list/atm/merra2/atm_merra2.hps 1
#
# ------------------------------------------------------------------------
#
def main():
    if ( len(sys.argv) <= 4 ):
         print ( "Usage: har_loading typ model mode ivrb" )
         exit ( 1 )
    typ       = sys.argv[1]
    model     = sys.argv[2]
    mode      = sys.argv[3]
    ivrb_str  = sys.argv[4]
    ivrb      = int(ivrb_str)

#
# --- Get MALO share, script, bin directories
#
    malo_script_dir = os.popen("malo_inq script").read().rstrip()
    malo_share_dir  = os.popen("malo_inq share").read().rstrip()
    malo_model_dir  = os.popen("malo_inq model").read().rstrip()
    malo_bin_dir    = os.popen("malo_inq bin_static").read().rstrip()

    filcnf = malo_share_dir + "/" + host_suffix + "_" + typ + "_" + model + ".conf"

    if ( not os.path.isfile(filcnf) ):
         print ( "har_loading.py: control file %s does not exist" % filcnf )
         exit ( 1 )

#
# --- Set the number of processors used
#
    if ( "OMP_NUM_THREADS" in os.environ ): 
          num_thr = int ( os.environ["OMP_NUM_THREADS"] )
    else:
          num_thr = 1


#
# --- Read and parse configuration  file
#
    config = config_class ( filcnf ) 
    parse_geos_oper_config ( config )


#
# --- Extract the name of the model from the name of the surface pressure directory
#
    id = config.geos_heb_dir.rfind("/")
    if ( id == len(config.geos_heb_dir) ):
         id = config.geos_heb_dir[0:id-1].rfind("/")
    load_model = config.geos_heb_dir[id+1:].replace("/","")

#
# --- Read and parse the loading configurationfile
#
    if ( config.load_conf[0:1] == "/" ):
         with open(config.load_conf) as f:
              load_conf = f.readlines()
         f.close()
    else:
         with open(malo_share_dir + "/" + config.load_conf) as f:
              load_conf = f.readlines()
         f.close()
#
# --- Read and parse the d1 loading configurationfile
#
    if ( config.load_d1_conf[0:1] == "/" ):
         with open(config.load_d1_conf) as f:
              load_d1_conf = f.readlines()
         f.close()
    else:
         with open(malo_share_dir + "/" + config.load_d1_conf) as f:
              load_d1_conf = f.readlines()
         f.close()

#
# --- Find station file with names of the stations for loading computation
#
    load_sta_file  = None
    harmod_file    = None
    harmod_d1_file = None

    for line in load_conf:
        if ( line.split()[0] == "STATION_FINAM" ):
             load_sta_file = line.split()[2].rstrip("\n") 
             if ( load_sta_file[0:1] != "/" ):
                  load_sta_file = malo_share_dir + "/" + load_sta_file 

        if ( line.split()[0] == "MALO_FINAM_MODEL" ):
             harmod_file = line.split()[2].rstrip("\n") 
             if ( harmod_file[0:1] != "/" ):
                  harmod_file = malo_model_dir + "/" + harmod_file
                  
    for line in load_d1_conf:
        if ( line.split()[0] == "MALO_FINAM_MODEL" ):
             harmod_d1_file = line.split()[2].rstrip("\n") 
             if ( harmod_d1_file[0:1] != "/" ):
                  harmod_d1_file = malo_model_dir + "/" + harmod_d1_file

    if ( not os.path.isfile ( load_sta_file ) ) :
         print ( "Error: cannot find station file", load_sta_file )
         exit ( 1 )

    if ( not os.path.isfile ( harmod_file ) ) :
         print ( "Error: cannot harmonic model file", harmod_file )
         exit ( 1 )

    if ( not os.path.isfile ( harmod_d1_file ) ) :
         print ( "Error: cannot d1 harmonic model file", harmod_d1_file )
         exit ( 1 )

#
# --- Check output directories
#
    if ( not os.path.isdir ( config.load_har_grid_dir ) ) :
         print ( "Error: output gridded loading directory %s does not exist" % config.load_har_grid_dir )
         exit ( 1 )

    if ( not os.path.isdir ( config.load_har_spl_dir ) ) :
         print ( "Error: output station loading directory %s does not exist" % config.load_har_spl_dir )
         exit ( 1 )

    if ( not os.path.isdir ( config.load_har_list_dir ) ) :
         print ( "Error: output station loading directory %s does not exist" % config.load_har_list_dir )
         exit ( 1 )

    if ( not os.path.isdir ( config.load_har_grid_dir ) ) :
         print ( "Error: output d1 gridded loading directory %s does not exist" % config.load_har_grid_dir )
         exit ( 1 )

    if ( not os.path.isdir ( config.load_har_spl_dir ) ) :
         print ( "Error: output d1 station loading directory %s does not exist" % config.load_har_spl_dir )
         exit ( 1 )

    if ( not os.path.isdir ( config.load_har_list_dir ) ) :
         print ( "Error: output d1 station loading directory %s does not exist" % config.load_har_list_dir )
         exit ( 1 )


    if ( not os.path.isdir ( config.load_d1_har_grid_dir ) ) :
         print ( "Error: output gridded loading directory %s does not exist" % config.load_d1_har_grid_dir )
         exit ( 1 )

    if ( not os.path.isdir ( config.load_d1_har_spl_dir ) ) :
         print ( "Error: output station loading directory %s does not exist" % config.load_d1_har_spl_dir )
         exit ( 1 )

    if ( not os.path.isdir ( config.load_d1_har_list_dir ) ) :
         print ( "Error: output station loading directory %s does not exist" % config.load_d1_har_list_dir )
         exit ( 1 )

    if ( not os.path.isdir ( config.load_d1_har_grid_dir ) ) :
         print ( "Error: output d1 gridded loading directory %s does not exist" % config.load_d1_har_grid_dir )
         exit ( 1 )

    if ( not os.path.isdir ( config.load_d1_har_spl_dir ) ) :
         print ( "Error: output d1 station loading directory %s does not exist" % config.load_d1_har_spl_dir )
         exit ( 1 )

    if ( not os.path.isdir ( config.load_d1_har_list_dir ) ) :
         print ( "Error: output d1 station loading directory %s does not exist" % config.load_d1_har_list_dir )
         exit ( 1 )


    load_grid_file    = config.load_har_grid_dir    + "/" + config.load_grid_pref    + "harmod.heb"
    load_spl_file     = config.load_har_spl_dir     + "/" + config.load_spl_pref     + "harmod.heb"
    load_list_file    = config.load_har_list_dir    + "/" + config.load_list_pref    + "harmod.hps"
    load_d1_grid_file = config.load_d1_har_grid_dir + "/" + config.load_d1_grid_pref + "harmod.heb"
    load_d1_spl_file  = config.load_d1_har_spl_dir  + "/" + config.load_d1_spl_pref  + "harmod.heb"
    load_d1_list_file = config.load_d1_har_list_dir + "/" + config.load_d1_list_pref + "harmod.hps"

    if ( ivrb >= 4 ):
         print ( "load_grid_file              = ", load_grid_file        )
         print ( "load_spl_file               = ", load_spl_file         )
         print ( "load_list_file              = ", load_list_file        )
         print ( "load_d1_grid_file           = ", load_d1_grid_file     )
         print ( "load_d1_spl_file            = ", load_d1_spl_file      )
         print ( "load_d1_list_file           = ", load_d1_list_file     )
         print ( "config.loa_descr            = ", config.loa_descr      )
         print ( "config.loa_comm             = ", config.loa_comm       )

         sys.stdout.flush()

    
    if ( mode[0:1] == "1" ):
#
# ------ Compute the gridded loading using the harmonic model of 
# ------ the surface pressure.
#
         if ( config.pivot_sds[0] == "eqt" ):
              loa_com =  config.malo_load_model_exe            + " " + \
                         config.load_conf                      + " " + \
                         load_grid_file.replace(".heb",".all") + " " + \
                         "3"
         else:
              loa_com =  config.malo_load_model_exe            + " " + \
                         "load "                               + " " + \
                         config.load_conf                      + " " + \
                         harmod_file                           + " " + \
                         load_grid_file.replace(".heb",".all") + " " + \
                         config.loa_descr                      + " " + \
                         config.loa_comm                       + " " + \
                         "3"

         if ( ivrb > 3 ):
              print ( "loa_com= ", loa_com )
         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "load_model_exe             start time: ", date_str )

         ( ret, out) = exe ( loa_com, ivrb )
         if ( ret != 0 ):
              print ( "Error in " + config.malo_load_model_exe + " : ", "\n".join(out) )
              exit  ( 1 )

         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "load_model_exe             stop  time: ", date_str )

    if ( mode[1:2] == "1" ):
#
# ------ Expand the gridded loading into B-spline basis
#
         loa_com = config.loading_heb_to_spl_heb_exe + " " + \
                          load_grid_file             + " " + \
                          load_spl_file

         if ( ivrb > 3 ):
              print ( "loa_com= ", loa_com )
         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "loading_heb_to_spl_heb_exe start time: ", date_str )

         ( ret, out) = exe ( loa_com, ivrb )
         if ( ret != 0 ):
              print ( "Error in " + config.loading_heb_to_spl_heb_exe + " : ", "\n".join(out) )
              exit  ( 1 )

         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "loading_heb_to_spl_heb_exe stop  time: ", date_str )

    if ( mode[2:3] == "1" ):
#
# ------ Compute loading displacement for stations using its expanion into B-spline basis
#
         loa_com = config.loading_spl_heb_to_sta_exe + " " + \
                          load_spl_file              + " " + \
                          load_sta_file              + " " + \
                          load_list_file             + " " + \
                          "1"

         if ( ivrb > 3 ):
              print ( "loa_com= ", loa_com )
         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "loading_spl_heb_to_sta_exe start time: ", date_str )

         ( ret, out) = exe ( loa_com, ivrb )
         if ( ret != 0 ):
              print ( "Error in " + config.loading_spl_heb_to_sta_exe + " : ", "\n".join(out) )
              exit  ( 1 )

         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "loading_spl_heb_to_sta_exe stop  time: ", date_str )
    
    if ( mode[3:4] == "1" ):
#
# ------ Compute the gridded loading of degree 1 using the harmonic model of 
# ------ the surface pressure.
#
         if ( config.pivot_sds[0] == "eqt" ):
              loa_com =  config.malo_load_model_exe               + " " + \
                         config.load_d1_conf                      + " " + \
                         load_d1_grid_file.replace(".heb",".all") + " " + \
                         "3"
         else:
              loa_com =  config.malo_load_model_exe                + " " + \
                         "load_d1"                                 + " " + \
                          config.load_d1_conf                      + " " + \
                          harmod_d1_file                           + " " + \
                          load_d1_grid_file.replace(".heb",".all") + " " + \
                          config.loa_d1_descr                      + " " + \
                          config.loa_d1_comm                       + " " + \
                          "3"

         if ( ivrb > 3 ):
              print ( "loa_com= ", loa_com )
         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "load_model_exe             start time: ", date_str )

         ( ret, out) = exe ( loa_com, ivrb )
         if ( ret != 0 ):
              print ( "Error in " + config.malo_load_model_exe + " : ", "\n".join(out) )
              exit  ( 1 )

         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "load_model_exe             stop  time: ", date_str )

    if ( mode[4:5] == "1" ):
#
# ------ Expand the gridded loading into B-spline basis
#
         loa_com = config.loading_heb_to_spl_heb_exe + " " + \
                          load_d1_grid_file          + " " + \
                          load_d1_spl_file

         if ( ivrb > 3 ):
              print ( "loa_com= ", loa_com )
         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "loading_heb_to_spl_heb_exe start time: ", date_str )

         ( ret, out) = exe ( loa_com, ivrb )
         if ( ret != 0 ):
              print ( "Error in " + config.loading_heb_to_spl_heb_exe + " : ", "\n".join(out) )
              exit  ( 1 )

         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "loading_heb_to_spl_heb_exe stop  time: ", date_str )

    if ( mode[5:6] == "1" ):
#
# ------ Expand the gridded loading into B-spline basis
#
         loa_com = config.loading_spl_heb_to_sta_exe + " " + \
                          load_d1_spl_file           + " " + \
                          load_sta_file              + " " + \
                          load_d1_list_file          + " " + \
                          "1"

         if ( ivrb > 3 ):
              print ( "loa_com= ", loa_com )
         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "loading_spl_heb_to_sta_exe start time: ", date_str )

         ( ret, out) = exe ( loa_com, ivrb )
         if ( ret != 0 ):
              print ( "Error in " + loading_spl_heb_to_sta_exe + " : ", "\n".join(out) )
              exit  ( 1 )

         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "loading_spl_heb_to_sta_exe stop  time: ", date_str )

    if ( mode[6:7] == "1" ):
#
# ------ Compute loading displacement for stations using its expanion into B-spline basis
#
         loa_com = config.loading_spl_heb_to_sta_exe + " " + \
                          load_spl_file              + " " + \
                          load_sta_file              + " " + \
                          load_list_file.replace(".hps","_s1.hps") + " " + \
                          "1"

         if ( ivrb > 3 ):
              print ( "loa_com= ", loa_com )
         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "loading_spl_heb_to_sta_exe start time: ", date_str )

         ( ret, out) = exe ( loa_com, ivrb )
         if ( ret != 0 ):
              print ( "Error in " + config.loading_spl_heb_to_sta_exe + " : ", "\n".join(out) )
              exit  ( 1 )

         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "loading_spl_heb_to_sta_exe stop  time: ", date_str )

    sys.stdout.flush()

if __name__ == "__main__":
    vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    signal.signal ( signal.SIGINT,  malo_signal_handler )
    signal.signal ( signal.SIGTERM, malo_signal_handler )
    signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
    main()
