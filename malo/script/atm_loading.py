#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   Program for computing atmospheric presssure loading.               *
# *   Usage: atm_loading.py filcnf mode data_date ivrb                   *
# *   where                                                              *
# *          filcnf -- Control file for computation of loading           *
# *                    displacements of this type.                       *
# *            mode -- an ascii string of 9 characters consisting        *
# *                    0 and 1. 1 in the th position turns on            *
# *                    a component of loading computation. Usually,      *
# *                    string 111111111 is used.                         *
# *                                                                      *
# *                    1-th: Upgridding the pressure field to            *
# *                          degree/order 2699.                          *
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
# *                          gravity contribution due to variations in   *
# *                          the atmospheirc surface pressure field.     *
# *                    9-th: compression of results.                     *
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
# * ### 22-APR-2016  atm_loading.py  v2.3 (c) L. Petrov  01-JUN-2017 ### *
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
global malo_child_pid

tmp_dir = "/tmp"

#
# ------------------------------------------------------------------------
#
def main():
    if ( len(sys.argv) <= 4 ):
         print ( "Usage: atm_loading.py filcnf mode data_date ivrb" )
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
         data_date = data_date[0:4] + data_date[5:7] + data_date[8:10] + \
                     data_date[10:13] + data_date[14:17] 
    date_beg = data_date[0:4] + "." + data_date[4:6] + "." + data_date[6:8] + \
               "_" + data_date[9:11] + ":" + data_date[11:13] + ":00.0"

#
# --- Read and parse configuration  file
#
    config = geos_config_class ( filcnf ) 
    parse_geos_oper_config ( config )

#
# --- Get MALO share, script, bin directories
#
    malo_script_dir = os.popen("malo_inq script").read().rstrip()
    malo_share_dir  = os.popen("malo_inq share").read().rstrip()
    malo_bin_dir    = os.popen("malo_inq bin_static").read().rstrip()

#
# -- Read loading configuration file
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

#
# --- Build file names
#
    spr_file                = config.spr_dir + "/" + config.spr_pref + data_date + ".heb"
    atm_love_pres_file      = config.geos_temp_dir + "/" + config.load_grid_pref + "love_"      + data_date + ".shc"
    atm_dspl_nosc_heb       = config.geos_temp_dir + "/" + config.load_grid_pref + "dspl_nosc_" + data_date + ".heb"
    atm_dspl_sc_heb         = config.geos_temp_dir + "/" + config.load_grid_pref + "dspl_sc_"   + data_date + ".heb"
    atm_d1_dspl_heb         = config.geos_temp_dir + "/" + config.load_grid_pref + "dspl_"      + data_date + ".heb"

    atm_spr_pref            = config.geos_temp_dir + "/" + config.spr_pref 
    atm_spr_file            = atm_spr_pref + data_date + ".heb"
    
    atm_dspl_nc             = config.load_grid_dir    + "/" + config.load_grid_pref    + \
                              data_date + ".nc"
    atm_spl_heb             = config.load_spl_dir     + "/" + config.load_spl_pref     + \
                              data_date + ".heb"
    atm_dspl_eph            = config.load_list_dir    + "/" + config.load_list_pref    + \
                              data_date + ".eph"
    atm_d1_dspl_nc          = config.load_d1_grid_dir + "/" + config.load_d1_grid_pref + \
                              data_date + ".nc"
    atm_d1_spl_heb          = config.load_d1_spl_dir  + "/" + config.load_d1_spl_pref  + \
                              data_date + ".heb"
    atm_d1_dspl_eph         = config.load_d1_list_dir + "/" + config.load_d1_list_pref + \
                              data_date + ".eph"
        
    if ( not os.path.isfile ( load_sta_file ) ) :
         print ( "Error: cannot find station file", load_sta_file )
         exit ( 1 )

    if ( ivrb >= 4 ):
         print ( "config.malo_ls_mask   = ", config.malo_ls_mask   )
         print ( "config.sc_file        = ", config.sc_file        )
         print ( "config.loa_descr      = ", config.loa_descr      )
         print ( "config.loa_comm       = ", config.loa_comm       )
         print ( "config.loa_d1_descr   = ", config.loa_d1_descr   )
         print ( "config.loa_d1_comm    = ", config.loa_d1_comm    )
         print ( "config.load_conf      = ", config.load_conf      )    
         print ( "malo_share_dir        = ", malo_share_dir        )
         print ( "load_sta_file         = ", load_sta_file         )
         print ( "data_date             = ", data_date )
         print ( "date_beg              = ", date_beg  )
         sys.stdout.flush()
#
# --- Check, whether the input pressure file exists
#

    if ( mode[0:1] == "1" ):
#
# ------ Computation of the surface atmospheric pressure at the high resolution grid
#
         malo_com = config.gen_spr_exe    + " " + \
                    config.geos_heb_dir   + " " + \
                    data_date             + " " + \
                    config.gmao_gh        + " " + \
                    config.malo_elev      + " " + \
                    atm_spr_pref
                    
         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "gen_spr                start time: ", date_str )
         ( ret, out) = exe ( malo_com, 3 )
         if ( ret != 0 ):
              print ( "Error %d in malo_com: %s ", (ret, "\n".join(out) ) )
              exit  ( 1 )

         if ( ivrb > 2 ):
              print ( "\n".join(out) )
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "gen_spr                stop  time: ", date_str )

    if ( mode[1:2] == "1" ):
#
# ------ Expansion of the pressure field into the spherical harmonics 
# ------ and scaling the expansion with Love numbers
#
         malo_com = config.malo_exe       + " " + \
                    "sphe_love_create"    + " " + \
                    config.load_conf      + " " + \
                    atm_spr_file          + " " + \
                    date_beg              + " " + \
                    date_beg              + " " + \
                    atm_love_pres_file    + " " + \
                    ivrb_str 
                    
         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "sphe_love              start time: ", date_str )
         ( ret, out) = exe ( malo_com, 3 )
         if ( ret != 0 ):
              print ( "Error in malo_com: ", "\n".join(out) )
              exit  ( 1 )

         if ( ivrb > 2 ):
              print ( "\n".join(out) )
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "sphe_love              stop  time: ", date_str )


    if ( mode[2:3] == "1" ):
#
# ------ Compute the loading displacements at 2'x2' grid
# ------ with respect to the center of mass of the total Earth
#
         malo_com = config.malo_exe    + " " + \
                    "load_create"      + " " + \
                    config.load_conf   + " " + \
                    atm_love_pres_file + " " + \
                    date_beg           + " " + \
                    date_beg           + " " + \
                    atm_dspl_nosc_heb  + " " + \
                    ivrb_str

         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "load_create            start time: ", date_str )
         ( ret, out) = exe ( malo_com, 3 )
         if ( ret != 0 ):
              print ( "Error in malo_com: ", "\n".join(out) )
              exit  ( 1 )

         if ( ivrb > 2 ):
              print ( "\n".join(out) )
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "load_create            stop  time: ", date_str )

#
# ------ Compute the loading displacements at 2'x2' grid
# ------ with taking into account degree 1 only
#
         malo_com = config.malo_exe     + " " + \
                    "load_d1_create"    + " " + \
                    config.load_d1_conf + " " + \
                    atm_love_pres_file  + " " + \
                    date_beg            + " " + \
                    date_beg            + " " + \
                    atm_d1_dspl_heb     + " " + \
                    ivrb_str 

         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "load_d1_create         start time: ", date_str )
         ( ret, out) = exe ( malo_com, 3 )
         if ( ret != 0 ):
              print ( "Error in malo_com: ", "\n".join(out) )
              exit  ( 1 )

#        os.unlink  ( tmp_file )
         if ( ivrb > 2 ):
              print ( "\n".join(out) )
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "load_d1_create         stop  time: ", date_str )
    if ( mode[3:4] == "1" ):
#
# ------ Apply sampling correction for the loading displacement field
# ------ with respect to the center of mass of the total Earth
#
         im = config.malo_exe.rfind("/malo")
         malo_com = config.sc_apply_exe   + " " + \
                    "pres"                + " " + \
                    config.load_conf      + " " + \
                    atm_dspl_nosc_heb     + " " + \
                    atm_spr_file          + " " + \
                    atm_dspl_sc_heb

         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "sc_apply               start time: ", date_str )
         ( ret, out) = exe ( malo_com, 3 )
         if ( ret != 0 ):
              print ( "Error in sc_apply: ", "\n".join(out) )
              exit  ( 1 )

         if ( ivrb > 2 ):
              print ( "\n".join(out) )
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "sc_apply               stop  time: ", date_str )

    if ( mode[4:5] == "1" ):
#
# ------ Transform the loading displacement with respect to the center of 
# ------ the total Earth from heb to nc format and compress it
#
         malo_com = config.loading_heb_to_nc_exe            + " " + \
                    atm_dspl_sc_heb                         + " " + \
                    atm_dspl_nc                             + " " + \
                    malo_share_dir + "/" + config.loa_descr + " " + \
                    malo_share_dir + "/" + config.loa_comm  + " " + \
                    config.compress_com
         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "heb_to_nc              start time: ", date_str )
         ( ret, out) = exe ( malo_com, 3 )
         if ( ret != 0 ):
              print ( "Error in malo_com: ", "\n".join(out) )
              exit  ( 1 )

         if ( ivrb > 2 ):
              print ( "\n".join(out) )
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "heb_to_nc              stop  time: ", date_str )

#
# ------ Transform the loading displacement with respect to the center of 
# ------ the solid Earth from heb to nc format taking into account 
# ------ degree 1 only and compress it
#
         malo_com = config.loading_heb_to_nc_exe                 + " " + \
                    atm_d1_dspl_heb                              + " " + \
                    atm_d1_dspl_nc                               + " " + \
                    malo_share_dir   + "/" + config.loa_d1_descr + " " + \
                    malo_share_dir   + "/" + config.loa_d1_comm  + " " + \
                    config.compress_com
         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "heb_to_nc_d1           start time: ", date_str )
         ( ret, out) = exe ( malo_com, 3 )
         if ( ret != 0 ):
              print ( "Error in malo_com: ", "\n".join(out) )
              exit  ( 1 )

         if ( ivrb > 2 ):
              print ( "\n".join(out) )
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "heb_to_nc_d1           stop  time: ", date_str )

    if ( mode[5:6] == "1" ):
#
# ------ Compute spline expansion of loading
#
         load_com = config.loading_heb_to_spl_heb_exe + " " + \
                    atm_dspl_sc_heb + " " + \
                    atm_spl_heb 

         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "heb_to_spl_heb_com     start time: ", date_str )
         ( ret, out) = exe ( load_com, 3 )
         if ( ret != 0 ):
              print ( "Error in load_com: ", "\n".join(out) )
              exit  ( 1 )

         if ( ivrb > 2 ):
              print ( "\n".join(out) )
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "heb_to_spl_heb_com     stop  time: ", date_str )
#
# ------ Compute spline expansion of loading for d1
#
         load_com = config.loading_heb_to_spl_heb_exe + " " + \
                    atm_d1_dspl_heb + " " + \
                    atm_d1_spl_heb

         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "d1_heb_to_spl_heb_com  start time: ", date_str )
         ( ret, out) = exe ( load_com, 3 )
         if ( ret != 0 ):
              print ( "Error in load_com: ", "\n".join(out) )
              exit  ( 1 )

         if ( ivrb > 2 ):
              print ( "\n".join(out) )
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "d1_heb_to_spl_heb_com  stop  time: ", date_str )

    if ( mode[6:7] == "1" ):
#
# ------ Compute loading for stations
#
         load_com = config.loading_spl_heb_to_sta_exe + " " + \
                    atm_spl_heb                       + " " + \
                    load_sta_file                     + " " + \
                    atm_dspl_eph                      + " " + \
                    ivrb_str  

         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "loading_spl_heb_to_sta start time: ", date_str )
         ( ret, out) = exe ( load_com, 3 )
         if ( ret != 0 ):
              print ( "Error in load_com: ", "\n".join(out) )
              exit  ( 1 )

         if ( ivrb > 2 ):
              print ( "\n".join(out) )
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "loading_spl_heb_to_sta stop  time: ", date_str )

#
# ------ Compute spline expansion of the loading displacement field with 
# ------ taking into account degree 1 only
#
         load_com = config.loading_spl_heb_to_sta_exe + " " + \
                    atm_d1_spl_heb                    + " " + \
                    load_sta_file                     + " " + \
                    atm_d1_dspl_eph                   + " " + \
                    ivrb_str  

         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "loading_spl_heb_to_sta start time: ", date_str )
         ( ret, out) = exe ( load_com, 3 )
         if ( ret != 0 ):
              print ( "Error in load_com: ", "\n".join(out) )
              exit  ( 1 )

         if ( ivrb > 2 ):
              print ( "\n".join(out) )
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "loading_spl_heb_to_sta stop  time: ", date_str )

    if ( mode[7:8] == "1" ):
#
# ------ Compute the Stokes coefficients of the contribution of the 
# ------ atmospheric surface enomaly to the geopotential
#
         vgep_com = config.malo_exe       + " " + \
                    "vgep_create"         + " " + \
                    config.load_conf      + " " + \
                    atm_love_pres_file    + " " + \
                    date_beg              + " " + \
                    date_beg              + " " + \
                    config.vgep_dir + "/" + config.vgep_pref + config.vgep_wc + " " + \
                    ivrb_str

         if ( ivrb > 2 ):
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "vgep_com       start time: ", date_str )
         ( ret, out) = exe ( vgep_com, 3 )
         if ( ret != 0 ):
              print ( "Error in vgep_com: ", "\n".join(out) )
              exit  ( 1 )

         if ( ivrb > 2 ):
              print ( "\n".join(out) )
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "vgep_com       stop  time: ", date_str )


    if ( mode[8:9] == "1" ):
         if ( ivrb > 2 ):
              print ( "\n".join(out) )
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "compression            start time: ", date_str )

         if ( config.spr_dir != "none" and config.spr_dir != "blank" ):
#
# ----------- Copying the spline expaneion to the destination directory and compressing
#
              if ( config.compress_com == "lpbzip2" ):
                   compr_com = "lbzip2 -c -n%d > %s" % ( num_thr, spr_file + ".bz2" )
              elif ( config.compress_com == "lpbzip2_p1" ):
                   compr_com = "lbzip2 -c -n1  > %s" % (          spr_file + ".bz2" )
              elif ( config.compress_com == "lpbzip1_2p1" ):
                   compr_com = "lbzip2 -1 -c -n1  > %s" % (       spr_file + ".bz2" )
              elif ( config.compress_com == "lpbzip2_2p1" ):
                   compr_com = "lbzip2 -2 -c -n1  > %s" % (       spr_file + ".bz2" )
              else:
                   compr_com = "lbzip2 -c -n%d > %s" % ( num_thr, spr_file + ".bz2" )

              compr_com = "cat " + atm_spr_file + " | " + compr_com + " -"
              ( ret, out) = exe ( compr_com, 3 )
              if ( ret != 0 ):
                   print ( "Error in compression command", compr_com )
                   print ( "Error: ", "\n".join(out) )
                   exit  ( 1 )

#
# ------ Compressing spline expansion of the loading displacement field
#
         if ( config.compress_com == "lpbzip2" ):
              compr_com = "lbzip2 -f -n%d %s" % ( num_thr, atm_spl_heb )
         elif ( config.compress_com == "lpbzip2_p1" ):
              compr_com = "lbzip2 -f -n1  %s" % (          atm_spl_heb )
         elif ( config.compress_com == "lpbzip2_1p1" ):
              compr_com = "lbzip2 01 -f -n1  %s" % (       atm_spl_heb )
         elif ( config.compress_com == "lpbzip2_2p1" ):
              compr_com = "lbzip2 -2 -f -n1  %s" % (       atm_spl_heb )
         else:
              compr_com = "lbzip2 -f -n%d %s" % ( num_thr, atm_spl_heb )

         ( ret, out) = exe ( compr_com, 3 )
         if ( ret != 0 ):
              print ( "Error in compression command", compr_com )
              print ( "Error in spl compression : ", "\n".join(out) )
              exit  ( 1 )

#
# ------ Compressing spline expansion of the loading displacement field that
# ------ takes in to account degree/order 1 only
#
         if ( config.compress_com == "lpbzip2" ):
              comnr_com = "lbzip2 -f -n%d %s" % ( num_thr, atm_d1_spl_heb )
         elif ( config.compress_com == "lpbzip2_p1" ):
              compr_com = "lbzip2 -f -n1  %s" % (          atm_d1_spl_heb )
         elif ( config.compress_com == "lpbzip2_1p1" ):
              compr_com = "lbzip2 -1 -f -n1  %s" % (       atm_d1_spl_heb )
         elif ( config.compress_com == "lpbzip2_2p1" ):
              compr_com = "lbzip2 -2 -f -n1  %s" % (       atm_d1_spl_heb )
         else:
              compr_com = "lbzip2 -f -n%d %s" % ( num_thr, atm_d1_spl_heb )

         ( ret, out) = exe ( compr_com, 3 )
         if ( ret != 0 ):
              print ( "Error in compression command", compr_com )
              print ( "Error in d1 spl compression : ", "\n".join(out) )
              exit  ( 1 )

         if ( ivrb > 2 ):
              print ( "\n".join(out) )
              date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
              print ( "compression            stop  time: ", date_str )

    if ( ivrb < 3 ): 
         if ( os.path.isfile ( atm_spr_file       ) ): os.unlink  ( atm_spr_file       )
         if ( os.path.isfile ( atm_love_pres_file ) ): os.unlink  ( atm_love_pres_file )
         if ( os.path.isfile ( atm_dspl_nosc_heb  ) ): os.unlink  ( atm_dspl_nosc_heb  )
         if ( os.path.isfile ( atm_dspl_sc_heb    ) ): os.unlink  ( atm_dspl_sc_heb    )
         if ( os.path.isfile ( atm_d1_dspl_heb    ) ): os.unlink  ( atm_d1_dspl_heb    )

if __name__ == "__main__":
    vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    signal.signal ( signal.SIGINT,  malo_signal_handler )
    signal.signal ( signal.SIGTERM, malo_signal_handler )
    signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
    os.environ["OMP_NUM_THREADS"] = "1"
    main()
