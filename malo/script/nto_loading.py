#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   Program for computing non-tidal ocean mass loading.                *
# *                                                                      *
# *   Usage: nto_loading.py filcnf mode data_date ivrb                   *
# *   where                                                              *
# *          filcnf -- Control file for computation of loading           *
# *                    displacements of this type.                       *
# *            mode -- an ascii string of 9 characters consisting        *
# *                    0 and 1. 1 in the th position turns on            *
# *                    a component of loading computation. Usually,      *
# *                    string 1111111111 is used.                        *
# *                                                                      *
# *                    1-th: Computation of the bottom pressure from     *
# *                          input Stokes coefficients, dealiasing and   *
# *                          upgridding it to degree/order 2699.         *
# *                    2-th: Expansion of the pressure field into the    *
# *                          spherical harmonics and scaling the         *
# *                          expansion with Love numbers.                *
# *                    3-th: Two tasks. First: computation the loading   *
# *                          displacements at global grid with respect   *
# *                          to the center of mass of the total Earth.   *
# *                          Second: computation of the loading          *
# *                          displacement global field by taking into    *
# *                          account degree 1 only.                      *
# *                    4-th: Apply sampling correction for the loading   *
# *                          displacement field with respect to the      *
# *                          center of mass of the total Earth.          *
# *                    5-th: Two tasks. First: transformation the        *
# *                          loading displacement field with respect to  *
# *                          the center of the total Earth from          *
# *                          heb to netCDF format and compress it.       *
# *                          Second: perform the same operation for the  *
# *                          loading displacement global field by taking *
# *                          into account degree 1 only.                 *
# *                    6-th: Two tasks. First: computation of spline     *
# *                          expansion of the loading displacement field *
# *                          with respect to the center of the total     *
# *                          Earth. Second: perform the same operation   *
# *                          for the loading displacement global field   *
# *                          by taking into account degree 1 only.       *
# *                    7-th: Two tasks: First: computation of loading    *
# *                          for stations with respect to the center of  *
# *                          the total Earth. Second: perform the same   *
# *                          operation for the loading displacement      *
# *                          global field by taking into account degree  *
# *                          1 only.                                     *
# *                    8-th: Compute the Stokes coefficients of the      *
# *                          gravity contribution due to non-tidal       *
# *                          variation of the bottom pressure.           *
# *                    9-th: compression loading intergrals.             *
# *                   10-th: compression of results.                     *
# *                                                                      *
# *                    NB: each step depends on previous steps.          *
# *                        unless ivrb > 3, intermediate results are     *
# *                        purged after completion of nto_loading.py     *
# *                                                                      *
# *       data_date -- Date of the loading in yyyymmdd_HHMM format.      *
# *                    In fact, HHMM field is ignored and the loading    *
# *                    for four epochs with a step of 6 hours is         *
# *                    computed for the given day.                       *
# *            ivrb -- Verbosity level:                                  *
# *                    0 -- silent                                       *
# *                    1 -- normal verbosity                             *
# *                   >1 -- debugging mode                               *
# *                   >3 -- temporary files are not removed.             *
# *                                                                      *
# * ###  13-APR-2016   nto_loading v2.11 (c) L. Petrov   21-OCT-2023 ### *
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

mod_dates_04 = ["0000", "0600", "1200", "1800"]
mod_dates_08 = ["0000", "0300", "0600", "0900", "1200", "1500", "1800", "2100"]

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
       self.load_int_dir       = None
       self.load_d1_conf       = None
       self.load_d1_list_dir   = None
       self.load_d1_grid_dir   = None
       self.load_d1_int_dir    = None
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
# ------------------------------------------------------------------------
#
def main():
    if ( len(sys.argv) <= 4 ):
         print ( "Usage: nto_loading filcnf mode data_date ivrb" )
         exit ( 1 )
    filcnf    = sys.argv[1]
    mode      = sys.argv[2]
    data_date = sys.argv[3]
    ivrb_str  = sys.argv[4]
    ivrb      = int(ivrb_str)

#
# --- Set the number of processors used
#
    if ( "OMP_NUM_THREADS" in os.environ ): 
          num_thr = int ( os.environ["OMP_NUM_THREADS"] )
    else:
          num_thr = 1

#
# --- Convert the date to yyyymmdd_HHDD format
#
    if ( data_date.find(".") > 0 ):
         if ( len(data_date) <= 10 ): data_date = data_date + "_00:00:00.0"
         data_date = data_date[0:4] + data_date[5:7] + data_date[8:10] + \
                     data_date[10:13] + data_date[14:17] 

    if ( len(data_date) <= 10 ):
         date_beg = data_date[0:4] + "." + data_date[4:6] + "." + data_date[6:8] + \
                    "_00:00:00.0"
    else:
         date_beg = data_date[0:4] + "." + data_date[4:6] + "." + data_date[6:8] + \
                    "_" + data_date[9:11] + ":" + data_date[11:13] + ":00.0"

#
# --- Read and parse configuration  file
#
    config = config_class ( filcnf ) 
    parse_geos_oper_config ( config )

#
# --- Get MALO share, script, bin directories
#
    malo_script_dir = os.popen("malo_inq script").read().rstrip()
    malo_share_dir  = os.popen("malo_inq share").read().rstrip()
    malo_bin_dir    = os.popen("malo_inq bin_static").read().rstrip()

#
# --- Extract the name of the model from the name of the bottom pressure directory
#
    id = config.geos_dir.rfind("/")
    if ( id == len(config.geos_dir) ):
         id = config.geos_dir[0:id-1].rfind("/")
    load_model = config.geos_dir[id+1:].replace("/","")

#
# --- Read and parse nto loading configurationfile
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
# --- Find station file with names of the stations for loading computation
#
    load_sta_file = None
    for line in load_conf:
        if ( line.split()[0] == "STATION_FINAM" ):
             load_sta_file = line.split()[2].rstrip("\n") 
             if ( load_sta_file[0:1] != "/" ):
                  load_sta_file = malo_share_dir + "/" + load_sta_file 

    if ( not os.path.isfile ( load_sta_file ) ) :
         print ( "Error: cannot find station file", load_sta_file )
         exit ( 1 )

#
# --- Build the name of the nto original data file with Stokes coefficients
#
    if ( "omct05" in config.geos_dir ):
         nto_orig_dir = config.geos_dir + "/asc/"
         nto_orig_file = config.geos_dir + "/asc/AOD1B_" + data_date[0:4] + "-" + \
                         data_date[4:6] + "-" + data_date[6:8] + "_X_05.asc"
         mod_dates = mod_dates_04
    elif ( "mpiom06" in config.geos_dir ):
         nto_orig_dir = config.geos_dir + "/asc/" + data_date[0:4]
         nto_orig_file = config.geos_dir + "/asc/" + data_date[0:4] + "/AOD1B_" + \
                         data_date[0:4] + "-" + data_date[4:6] + "-" + \
                         data_date[6:8] + "_X_06.asc"
         mod_dates = mod_dates_08
    elif ( "mpiom07" in config.geos_dir ):
         nto_orig_dir = config.geos_dir + "/asc/" + data_date[0:4]
         nto_orig_file = config.geos_dir + "/asc/" + data_date[0:4] + "/AOD1B_" + \
                         data_date[0:4] + "-" + data_date[4:6] + "-" + \
                         data_date[6:8] + "_X_07.asc"
         mod_dates = mod_dates_08
    else:
         print ( "Unknown nto model: mpiom07 or mpiom06 omct05 are supported. Please check config.geos_dir %s " % config.geos_dir )
         exit ( 1 )

#
# -- Define template file names. Actual file names depdend on date and will be 
# -- generated on the fily
#
    if ( "omct" in config.geos_dir ):
         nto_spr_file_templ     = config.geos_heb_dir  + "/" + data_date[0:4] + "/" + \
                                  config.pivot_sds[0]  + "/" +  load_model + "_xxxxxxxx_xxxx" + ".heb"
    else:
         if ( config.geos_heb_dir == "none" ):
              nto_spr_dir       = config.geos_temp_dir
         else:
              nto_spr_dir       = config.geos_heb_dir

         nto_spr_file_templ    = nto_spr_dir + "/" + \
                                 load_model + "_xxxxxxxx_xxxx" + ".heb"

    nto_love_file_templ         = config.geos_temp_dir + "/nto_" + load_model +"_xxxxxxxx_xxxx" + ".shc"
    nto_dspl_nosc_file_templ    = config.geos_temp_dir + "/nto_" + load_model + "_nosc_" + "xxxxxxxx_xxxx" + ".heb"
    nto_dspl_sc_file_templ      = config.geos_temp_dir + "/nto_" + load_model + "_sc_"   + "xxxxxxxx_xxxx" + ".heb"

    nto_dspl_nc_file_templ      = config.load_grid_dir + "/" + config.load_grid_pref + \
                                        "xxxxxxxx_xxxx" + ".nc"
    nto_dspl_spl_file_templ     = config.load_spl_dir + "/" + config.load_spl_pref + \
                                        "xxxxxxxx_xxxx" + ".heb"
    nto_dspl_list_file_templ    = config.load_list_dir + "/" + config.load_list_pref + \
                                        "xxxxxxxx_xxxx" + ".eph"
    nto_dspl_int_file_templ     = config.load_int_dir + "/nto_" + load_model + "xxxxxxxx_xxxx" + ".txt"

    nto_d1_dspl_heb_file_templ  = config.geos_temp_dir + "/" + config.load_d1_grid_pref + \
                                        "xxxxxxxx_xxxx" + ".heb"
    nto_d1_dspl_nc_file_templ   = config.load_d1_grid_dir + "/" + config.load_d1_grid_pref + \
                                        "xxxxxxxx_xxxx" + ".nc"
    nto_d1_dspl_spl_file_templ  = config.load_d1_spl_dir + "/" + config.load_d1_spl_pref + \
                                        "xxxxxxxx_xxxx" + ".heb"
    nto_d1_dspl_list_file_templ = config.load_d1_list_dir + "/" + config.load_d1_list_pref + \
                                        "xxxxxxxx_xxxx" + ".eph"
    nto_d1_dspl_int_file_templ  = config.load_int_dir + "/nto_" + load_model + "xxxxxxxx_xxxx" + ".txt"

    if ( ivrb >= 4 ):
         print ( "config.malo_ls_mask         = ", config.malo_ls_mask   )
         print ( "config.upgrid_ls_mask       = ", config.upgrid_ls_mask )
         print ( "config.sc_file              = ", config.sc_file        )
         print ( "config.loa_descr            = ", config.loa_descr      )
         print ( "config.loa_comm             = ", config.loa_comm       )
         print ( "load_model                  = ", load_model            )
         print ( "data_date                   = ", data_date             )
         print ( "date_beg                    = ", date_beg              )
         print ( "nto_spr_file_templ          = ", nto_spr_file_templ    )
         print ( "nto_love_file_templ         = ", nto_love_file_templ   )
         print ( "nto_dspl_nosc_file_templ    = ", nto_dspl_nosc_file_templ    )
         print ( "nto_dspl_sc_file_templ      = ", nto_dspl_sc_file_templ      )
         print ( "nto_dspl_nc_file_templ      = ", nto_dspl_nc_file_templ      )
         print ( "nto_dspl_spl_file_templ     = ", nto_dspl_spl_file_templ     )
         print ( "nto_dspl_list_file_templ    = ", nto_dspl_list_file_templ    )
         print ( "nto_dspl_int_file_templ     = ", nto_dspl_int_file_templ     )
         print ( "nto_d1_dspl_heb_file_templ  = ", nto_d1_dspl_heb_file_templ  )
         print ( "nto_d1_dspl_nc_file_templ   = ", nto_d1_dspl_nc_file_templ   )
         print ( "nto_d1_dspl_spl_file_templ  = ", nto_d1_dspl_spl_file_templ  )
         print ( "nto_d1_dspl_list_file_templ = ", nto_d1_dspl_list_file_templ )
         print ( "nto_d1_dspl_int_file_templ  = ", nto_d1_dspl_int_file_templ  )

         sys.stdout.flush()

#
# --- Check whether the ouotput directory exits, and if not, create it
#
    if ( not os.path.isdir( nto_orig_dir ) ):
         os.mkdir ( nto_orig_dir )

#
# --- Check whether the input file with Stokes coefficients exists
#
    if ( not os.path.isfile(nto_orig_file) ):
         print ( "Cannot find input nto Stokes coefficient file", nto_orig_file )
         exit ( 1 )
    
    if ( mode[0:1] == "1" ):
#
# ------ Compute the bottom pressure from Stokes coefficients of the input data.
# ------ NB: one input data file has the data for four epochs
#
         nto_to_spr_com = config.malo_exe         + " " + \
                          "grav_sphe_pres_create" + " " + \
                          config.load_conf        + " " + \
                          nto_orig_file           + " " + \
                          date_beg[0:10]          + " " + \
                          date_beg[0:10]          + " " + \
                          nto_spr_dir             + " " + \
                          "3"

         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "malo_spr               start time: ", date_str, flush=True )

         ( ret, out) = exe ( nto_to_spr_com, ivrb )
         if ( ret != 0 ):
              print ( "Error in nto_loading: ", "\n".join(out) )
              exit  ( 1 )

         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "malo_spr               stop  time: ", date_str, flush=True )
    elif ( mode[0:1] == "2" ):
#
# ------ Copy the bottom pressure from Stokes coefficients of the input data.
# ------ NB: one input data file has the data for four epochs
#
         for mod_date in mod_dates:
             spr_file_bz2 = config.spr_dir + "/" + data_date[0:4] + "/" + \
                            config.pivot_sds[0] + "_" + data_date + "_" + \
                            mod_date + ".heb.bz2"
             spr_file     = config.geos_temp_dir + "/" + \
                            config.pivot_sds[0] + "_" + data_date + "_" + \
                            mod_date + ".heb"

#
# ---------- and uncompress it
#
             nto_uncompr_spr_com = "bzip2 -dfc " + spr_file_bz2 + " > " + spr_file
             if ( ivrb > 3 ):
                  print ( "nto_uncompr_spr_com= ", nto_uncompr_spr_com, flush=True )
             ( ret, out) = exe ( nto_uncompr_spr_com, ivrb )
             if ( ret != 0 ):
                  print ( "Error in nto_loading: ", "\n".join(out) )
                  exit  ( 1 )

    if ( mode[1:2] == "1" ):  
#
# ------ Expansion of the pressure field into the spherical harmonics 
# ------ and scaling the expansion with Love numbers
#
         for mod_date in mod_dates:
             file_date = date_beg[0:4] + date_beg[5:7] + date_beg[8:10] + "_" + mod_date 
             nto_spr_file  = nto_spr_file_templ.replace( "xxxxxxxx_xxxx",file_date)
             nto_love_file = nto_love_file_templ.replace("xxxxxxxx_xxxx",file_date)

             malo_com = config.malo_exe    + " " + \
                        "sphe_love_create" + " " + \
                        config.load_conf   + " " + \
                        nto_spr_file       + " " + \
                        date_beg[0:10]     + "_" + mod_date[0:2] + ":" + mod_date[2:4] + ":00.0 " + \
                        date_beg[0:10]     + "_" + mod_date[0:2] + ":" + mod_date[2:4] + ":00.0 " + \
                        nto_love_file      + " " + \
                        ivrb_str

             if ( ivrb > 2 ):
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  print ( "malo_sphe              start time: ", date_str, flush=True )

             ( ret, out) = exe ( malo_com, ivrb )
             if ( ret != 0 ):
                  print ( "Error in malo_com: ", "\n".join(out) )
                  exit  ( 1 )

             if ( ivrb > 2 ):
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  print ( "malo_sphe              stop  time: ", date_str, flush=True )

    if ( mode[2:3] == "1" ):
         for mod_date in mod_dates:
#
# ---------- Compute the loading displacements at 2'x2' grid
# ---------- with respect to the center of mass of the total Earth
#
             file_date = date_beg[0:4] + date_beg[5:7] + date_beg[8:10] + "_" + mod_date 
             nto_love_file        = nto_love_file_templ.replace     ("xxxxxxxx_xxxx",file_date)
             nto_dspl_nosc_file   = nto_dspl_nosc_file_templ.replace("xxxxxxxx_xxxx",file_date)
             nto_d1_dspl_heb_file = nto_d1_dspl_heb_file_templ.replace("xxxxxxxx_xxxx",file_date)

             malo_com = config.malo_exe    + " " + \
                        "load_create"      + " " + \
                        config.load_conf   + " " + \
                        nto_love_file      + " " + \
                        date_beg[0:10]     + "_" + mod_date[0:2] + ":" + mod_date[2:4] + ":00.0 " + \
                        date_beg[0:10]     + "_" + mod_date[0:2] + ":" + mod_date[2:4] + ":00.0 " + \
                        nto_dspl_nosc_file + " " + \
                        ivrb_str 

             if ( ivrb > 2 ):
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  print ( "malo_load              start time: ", date_str, flush=True )

             ( ret, out) = exe ( malo_com, ivrb )
             if ( ret != 0 ):
                  print ( "Error in malo_com: ", "\n".join(out) )
                  exit  ( 1 )

             if ( ivrb > 2 ):
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  print ( "malo_load              stop  time: ", date_str, flush=True )

#
# ---------- Compute the loading displacements at global 2'x2' grid
# ---------- with taking into account degree 1 only
#
             malo_com = config.malo_exe      + " " + \
                        "load_d1_create"     + " " + \
                        config.load_d1_conf  + " " + \
                        nto_love_file        + " " + \
                        date_beg[0:10]       + "_" + mod_date[0:2] + ":" + mod_date[2:4] + ":00.0 " + \
                        date_beg[0:10]       + "_" + mod_date[0:2] + ":" + mod_date[2:4] + ":00.0 " + \
                        nto_d1_dspl_heb_file + " " + \
                        ivrb_str             + " "

             if ( ivrb > 2 ):
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  print ( "malo_d1_load           start time: ", date_str, flush=True )
             ( ret, out) = exe ( malo_com, ivrb )
             if ( ret != 0 ):
                  print ( "Error in malo_com: ", "\n".join(out) )
                  exit  ( 1 )

             if ( ivrb > 2 ):
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  print ( "malo_d1_load           stop  time: ", date_str, flush=True )

    if ( mode[3:4] == "1" ):
         for mod_date in mod_dates:
#
# ---------- Apply sampling correction for the loading displacement field
# ---------- with respect to the center of mass of the total Earth
#
             file_date = date_beg[0:4] + date_beg[5:7] + date_beg[8:10] + "_" + mod_date 
             nto_dspl_nosc_file = nto_dspl_nosc_file_templ.replace("xxxxxxxx_xxxx",file_date)
             nto_spr_file       = nto_spr_file_templ.replace(      "xxxxxxxx_xxxx", file_date )
             nto_dspl_sc_heb    = nto_dspl_sc_file_templ.replace(   "xxxxxxxx_xxxx", file_date )

             malo_com = config.sc_apply_exe  + " " + \
                       "pres"                + " " + \
                        config.load_conf     + " " + \
                        nto_dspl_nosc_file   + " " + \
                        nto_spr_file         + " " + \
                        nto_dspl_sc_heb

             if ( ivrb > 2 ):
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  print ( "sc_apply               start time: ", date_str, flush=True )
             ( ret, out) = exe ( malo_com, ivrb )
             if ( ret != 0 ):
                  print ( "Error in sc_apply: ", "\n".join(out) )
                  exit  ( 1 )

             if ( ivrb > 2 ):
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  print ( "sc_apply               stop  time: ", date_str, flush=True )

    if ( mode[4:5] == "1" ):
         for mod_date in mod_dates:
#
# ---------- Transform the loading displacement field with respect to the center of 
# ---------- the total Earth from heb to nc format and compress it
#
             file_date = date_beg[0:4] + date_beg[5:7] + date_beg[8:10] + "_" + mod_date 
             nto_dspl_sc_heb = nto_dspl_sc_file_templ.replace( "xxxxxxxx_xxxx", file_date )
             nto_dspl_sc_nc  = nto_dspl_nc_file_templ.replace( "xxxxxxxx_xxxx", file_date )
             malo_com = config.loading_heb_to_nc_exe            + " " + \
                        nto_dspl_sc_heb                         + " " + \
                        nto_dspl_sc_nc                          + " " + \
                        malo_share_dir + "/" + config.loa_descr + " " + \
                        malo_share_dir + "/" + config.loa_comm  + " " + \
                        config.compress_com

             if ( ivrb > 2 ):
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  print ( "heb_to_nc              start time: ", date_str, flush=True )
             ( ret, out) = exe ( malo_com, ivrb )
             if ( ret != 0 ):
                  print ( "Error in heb_to_nc: ", "\n".join(out) )
                  exit  ( 1 )

             if ( ivrb > 2 ):
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  print ( "heb_to_nc              stop  time: ", date_str, flush=True )
#
# ---------- Transform the loading displacement field with 
# ---------- taking into account degree 1 only
#
             nto_d1_dspl_heb_file = nto_d1_dspl_heb_file_templ.replace("xxxxxxxx_xxxx",file_date)
             nto_d1_dspl_nc  = nto_d1_dspl_nc_file_templ.replace( "xxxxxxxx_xxxx", file_date )
             malo_com = config.loading_heb_to_nc_exe            + " " + \
                         nto_d1_dspl_heb_file + " " + \
                         nto_d1_dspl_nc       + " " + \
                         malo_share_dir       + "/" + config.loa_d1_descr + " " + \
                         malo_share_dir       + "/" + config.loa_d1_comm + " " + \
                         config.compress_com
             if ( ivrb > 2 ):
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  print ( "heb_to_nc_d1           start time: ", date_str, flush=True )
             ( ret, out) = exe ( malo_com, ivrb )
             if ( ret != 0 ):
                  print ( "Error in heb_to_nc_d1: ", "\n".join(out) )
                  exit  ( 1 )

             if ( ivrb > 2 ):
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  print ( "heb_to_nc_d1           stop  time: ", date_str, flush=True )

    if ( mode[5:6] == "1" ):
         for mod_date in mod_dates:
             file_date = date_beg[0:4] + date_beg[5:7] + date_beg[8:10] + "_" + mod_date 
             nto_dspl_sc_heb      = nto_dspl_sc_file_templ.replace(   "xxxxxxxx_xxxx", file_date )
             nto_spl_heb          = nto_dspl_spl_file_templ.replace( "xxxxxxxx_xxxx",file_date)
             nto_d1_dspl_heb_file = nto_d1_dspl_heb_file_templ.replace("xxxxxxxx_xxxx",file_date)
             nto_d1_spl_heb       = nto_d1_dspl_spl_file_templ.replace( "xxxxxxxx_xxxx",file_date)

#
# ---------- Compute spline expansion of the loading displacement field with respect 
# ---------- to the center of the total Earth
#
             load_com = config.loading_heb_to_spl_heb_exe + " " + \
                        nto_dspl_sc_heb                   + " " + \
                        nto_spl_heb

             if ( ivrb > 2 ):
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  print ( "heb_to_spl_heb_com     start time: ", date_str, flush=True )
             ( ret, out) = exe ( load_com, ivrb )
             if ( ret != 0 ):
                  print ( "Error in heb_to_spl_heb_com: ", "\n".join(out) )
                  exit  ( 1 )

             if ( ivrb > 2 ):
                  print ( "\n".join(out) )
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  print ( "heb_to_spl_heb_com     stop  time: ", date_str, flush=True )
#
# ---------- Compute spline expansion of the loading displacement field with 
# ---------- taking into account degree 1 only
#
             load_com = config.loading_heb_to_spl_heb_exe + " " + \
                        nto_d1_dspl_heb_file              + " " + \
                        nto_d1_spl_heb

             if ( ivrb > 2 ):
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  print ( "d1_heb_to_spl_heb_com  start time: ", date_str, flush=True )
             ( ret, out) = exe ( load_com, ivrb )
             if ( ret != 0 ):
                  print ( "Error in d1_heb_to_spl_heb_com: ", "\n".join(out) )
                  exit  ( 1 )

             if ( ivrb > 2 ):
                  print ( "\n".join(out) )
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  print ( "d1_heb_to_spl_heb_com  stop  time: ", date_str, flush=True )

    if ( mode[6:7] == "1" ):
         for mod_date in mod_dates:
             file_date = date_beg[0:4] + date_beg[5:7] + date_beg[8:10] + "_" + mod_date 
             nto_spl_heb     = nto_dspl_spl_file_templ.replace( "xxxxxxxx_xxxx",file_date)
             nto_dspl_eph    = nto_dspl_list_file_templ.replace( "xxxxxxxx_xxxx",file_date)
             nto_d1_spl_heb  = nto_d1_dspl_spl_file_templ.replace( "xxxxxxxx_xxxx",file_date)
             nto_d1_dspl_eph = nto_d1_dspl_list_file_templ.replace( "xxxxxxxx_xxxx",file_date)
             if ( not os.path.isfile(nto_spl_heb) ):
                  nto_spl_heb = nto_spl_heb + ".bz2"
             if ( not os.path.isfile(nto_d1_spl_heb) ):
                  nto_d1_spl_heb = nto_d1_spl_heb + ".bz2"

# ---------- Compute loading for stations with respect 
# ---------- to the center of the total Earth
#
             load_com = config.loading_spl_heb_to_sta_exe + " " + \
                        nto_spl_heb                       + " " + \
                        load_sta_file                     + " " + \
                        nto_dspl_eph                      + " " + \
                        ivrb_str  

             if ( ivrb > 2 ):
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  print ( "loading_spl_heb_to_sta start time: ", date_str, flush=True )
             ( ret, out) = exe ( load_com, ivrb )
             if ( ret != 0 ):
                  print ( "Error in load_com: ", "\n".join(out) )
                  exit  ( 1 )

             if ( ivrb > 2 ):
                  print ( "\n".join(out) )
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  print ( "loading_spl_heb_to_sta stop  time: ", date_str, flush=True )

#
# ---------- Compute loading for stations taking into account degree 1 only
#
             load_com = config.loading_spl_heb_to_sta_exe + " " + \
                        nto_d1_spl_heb                    + " " + \
                        load_sta_file                     + " " + \
                        nto_d1_dspl_eph                   + " " + \
                        ivrb_str  

             if ( ivrb > 2 ):
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  print ( "loading_spl_heb_to_sta start time: ", date_str, flush=True )
             ( ret, out) = exe ( load_com, ivrb )
             if ( ret != 0 ):
                  print ( "Error in load_com: ", "\n".join(out) )
                  exit  ( 1 )

             if ( ivrb > 2 ):
                  print ( "\n".join(out) )
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  print ( "loading_spl_heb_to_sta stop  time: ", date_str, flush=True )

             if ( ivrb > 2 ):
                  print ( "\n".join(out) )
                  date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                  print ( "spl_heb compression    start time: ", date_str, flush=True )

    if ( mode[7:8] == "1" ):
         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "vgep_com       start time: ", date_str, flush=True )
         for mod_date in mod_dates:
#
# ---------- Compute the Stokes coefficients of the non-tidal bottom pressure vairations
#
             file_date = date_beg[0:4] + date_beg[5:7] + date_beg[8:10] + "_" + mod_date 
             nto_love_file = nto_love_file_templ.replace     ("xxxxxxxx_xxxx",file_date)
             vgep_com = config.malo_exe  + " " + \
                        "vgep_create"    + " " + \
                        config.load_conf + " " + \
                        nto_love_file    + " " + \
                        date_beg[0:10]   + "_" + mod_date[0:2] + ":" + mod_date[2:4] + ":00.0 " + \
                        date_beg[0:10]   + "_" + mod_date[0:2] + ":" + mod_date[2:4] + ":00.0 " + \
                        config.vgep_dir  + "/" + config.vgep_pref + config.vgep_wc + " " + \
                        ivrb_str

             ( ret, out) = exe ( vgep_com, ivrb )
             if ( ret != 0 ):
                  print ( "Error in vgep_com: ", "\n".join(out) )
                  exit  ( 1 )

         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "vgep_com       stop  time: ", date_str, flush=True )

    if ( mode[8:9] == "1" ):
#
# ------- Compute loading integral over the Earth surface, land, water, and zone of -+ 66 deg
#
         for mod_date in mod_dates:
             com_int = config.int_loading_exe + " " + \
                       config.filename  + " " + \
                       date_beg[0:4] + date_beg[5:7] + date_beg[8:10] + "_" + mod_date[0:4] + " " + \
                       "3"
             ( ret, out) = exe ( com_int, ivrb )
             if ( ret != 0 ):
                  print ( "Error in com_int: ", "\n".join(out) )
                  exit  ( 1 )
         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "com_int        stop  time: ", date_str, flush=True )

    if ( mode[9:10] == "1" ):
         if ( ivrb > 2 ):
              print ( "\n".join(out) )
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "compression            start time: ", date_str, flush=True )
         for mod_date in mod_dates:
             file_date = date_beg[0:4] + date_beg[5:7] + date_beg[8:10] + "_" + mod_date 
             nto_spl_heb    = nto_dspl_spl_file_templ.replace( "xxxxxxxx_xxxx",file_date)
             nto_d1_spl_heb = nto_d1_dspl_spl_file_templ.replace( "xxxxxxxx_xxxx",file_date)
             nto_spr_file   = nto_spr_file_templ.replace( "xxxxxxxx_xxxx",file_date)
#
# ---------- Compressing spline expansion of the loading displacement field
#
             if ( config.compress_com == "lpbzip2" ):
                  compr_com = "lbzip2 -f -n%d %s" % ( num_thr, nto_spl_heb )
             elif ( config.compress_com == "lpbzip2_p1" ):
                  compr_com = "lbzip2 -f -n1  %s" %    (       nto_spl_heb )
             elif ( config.compress_com == "lpbzip2_1p1" ):
                  compr_com = "lbzip2 -1 -f -n1  %s" % (       nto_spl_heb )
             elif ( config.compress_com == "lpbzip2_2p1" ):
                  compr_com = "lbzip2 -2 -f -n1  %s" % (       nto_spl_heb )
             else:
                  compr_com = "lbzip2 -f -n%d %s" % ( num_thr, nto_spl_heb )

             ( ret, out) = exe ( compr_com, ivrb )
             if ( ret != 0 ):
                  print ( "Error in compression command", compr_com )
                  print ( "Error in spl compression : ", "\n".join(out) )
                  exit  ( 1 )

#
# ---------- Compressing spline expansion of the d1 loading displacement field
#
             if ( config.compress_com == "lpbzip2" ):
                  compr_com = "lbzip2 -f -n%d %s" % ( num_thr, nto_d1_spl_heb )
             elif ( config.compress_com == "lpbzip2_p1" ):
                  compr_com = "lbzip2 -f -n1  %s" %    (       nto_d1_spl_heb )
             elif ( config.compress_com == "lpbzip2_1p1" ):
                  compr_com = "lbzip2 -1 -f -n1  %s" % (       nto_d1_spl_heb )
             elif ( config.compress_com == "lpbzip2_2p1" ):
                  compr_com = "lbzip2 -2 -f -n1  %s" % (       nto_d1_spl_heb )
             else:
                  compr_com = "lbzip2 -f -n%d %s" % ( num_thr, nto_d1_spl_heb )

             ( ret, out) = exe ( compr_com, ivrb )
             if ( ret != 0 ):
                  print ( "Error in compression command", compr_com )
                  print ( "Error in d1_spl compression : ", "\n".join(out) )
                  exit  ( 1 )
             if ( config.spr_dir == "none" ):
#
# --------------- Remove the bottom pressure file
#
                  if ( os.path.isfile ( nto_spr_file ) ): os.unlink ( nto_spr_file )
             else:
#
# --------------- Compressing nto bottom pressure
#
                   if ( config.compress_com == "lpbzip2" ):
                        compr_com = "lbzip2 -f -n%d %s" % ( num_thr, nto_spr_file )
                   elif ( config.compress_com == "lpbzip2_p1" ):
                        compr_com = "lbzip2 -f -n1  %s" %    (       nto_spr_file )
                   elif ( config.compress_com == "lpbzip2_1p1" ):
                        compr_com = "lbzip2 -1 -f -n1  %s" % (       nto_spr_file )
                   elif ( config.compress_com == "lpbzip2_2p1" ):
                        compr_com = "lbzip2 -2 -f -n1  %s" % (       nto_spr_file )
                   else:
                        compr_com = "lbzip2 -f -n%d %s" % ( num_thr, nto_spr_file )

                   ( ret, out) = exe ( compr_com, ivrb )
                   if ( ret != 0 ):
                        print ( "Error in compression command", compr_com )
                        print ( "Error in spr    compression : ", "\n".join(out) )
                        exit  ( 1 )

         if ( ivrb > 2 ):
              print ( "\n".join(out) )
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "compression            stop  time: ", date_str, flush=True )

    if ( ivrb < 3 ): 
#
# ------ Remove temporary files
#
         for mod_date in mod_dates:
             file_date = date_beg[0:4] + date_beg[5:7] + date_beg[8:10] + "_" + mod_date 
             nto_love_file        = nto_love_file_templ.replace("xxxxxxxx_xxxx",file_date)
             nto_dspl_sc_heb      = nto_dspl_sc_file_templ.replace("xxxxxxxx_xxxx", file_date )
             nto_dspl_nosc_file   = nto_dspl_nosc_file_templ.replace("xxxxxxxx_xxxx",file_date)
             nto_d1_dspl_heb_file = nto_d1_dspl_heb_file_templ.replace("xxxxxxxx_xxxx",file_date)

             if ( os.path.isfile ( nto_love_file        ) ): os.unlink ( nto_love_file        )
             if ( os.path.isfile ( nto_dspl_sc_heb      ) ): os.unlink ( nto_dspl_sc_heb      )
             if ( os.path.isfile ( nto_dspl_nosc_file   ) ): os.unlink ( nto_dspl_nosc_file   )
             if ( os.path.isfile ( nto_d1_dspl_heb_file ) ): os.unlink ( nto_d1_dspl_heb_file )

             if ( mode[9:10] == "9" ):
                  nto_spl_heb    = nto_dspl_spl_file_templ.replace( "xxxxxxxx_xxxx",file_date)
                  nto_d1_spl_heb = nto_d1_dspl_spl_file_templ.replace( "xxxxxxxx_xxxx",file_date)
                  nto_spr_file   = nto_spr_file_templ.replace( "xxxxxxxx_xxxx",file_date)
                  if ( os.path.isfile ( nto_spl_heb    ) ): os.unlink ( nto_spl_heb    )
                  if ( os.path.isfile ( nto_d1_spl_heb ) ): os.unlink ( nto_d1_spl_heb )
                  if ( os.path.isfile ( nto_spr_heb    ) ): os.unlink ( nto_spr_heb    )

             if ( mode[0:1] == "2" ):
                  spr_file = config.geos_temp_dir + "/" + \
                             config.pivot_sds[0] + "_" + data_date + "_" + \
                             mod_date + ".heb"
                  if ( os.path.isfile ( spr_file ) ): os.unlink ( spr_file )

                  spr_file = config.geos_temp_dir + "/" + \
                             config.pivot_sds[0] + "_" + data_date + "_" + \
                             mod_date + ".heb.bz2"
                  if ( os.path.isfile ( spr_file ) ): os.unlink ( spr_file )

                  
    sys.stdout.flush()

if __name__ == "__main__":
    vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( vers < "0302000" ): print ( "TThis script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    signal.signal ( signal.SIGINT,  malo_signal_handler )
    signal.signal ( signal.SIGTERM, malo_signal_handler )
    signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
    main()
