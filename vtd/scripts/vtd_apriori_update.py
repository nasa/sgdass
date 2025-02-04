#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   Program for update of a prioris needed by VTD.                     *
# *                                                                      *
# * # 29-OCT-2018 vtd_apriori_update.py v 3.27 (c) L. Petrov 08-JAN-2025 # *
# *                                                                      *
# ************************************************************************
"""
import pwd, sys, os, re, shutil, time, subprocess, datetime, operator, \
       stat, signal, errno, struct, fcntl, errno
from contextlib import contextmanager
sys.path.append("/usr")
import locale
import optparse 
import argparse 
from   datetime import date, datetime, timedelta, timezone
import vtd_local

from   vtd_exe   import exe, exe_out_log, exe_out_nolog, exe_noout_log, exe_noout_nolog, \
                 read_file_to_buffer, expand_env_vars

vau__label = "vtd_apriori_update.py  v 3.25 of 2025.01.08"
fmt__label = "# Configuration of file vtd_apriori update.  Format version of 2024.11.27"
config__num_par = 30 # The number of configuration parameters
locale.setlocale ( locale.LC_ALL )
iono_year_beg    = 1995
spd_lock_time    = 16.0
spd_wait_time    = 60.0
lock_check_time  =  5.0
lock_check_tries = 50
time_eps         = 180.0

gti_update_exe    = vtd_local.vtd_prefix  + "/bin/gti_update"
curl_exe          = "curl"
bindisp_merge_exe = vtd_local.vtd_prefix  + "/bin/bindisp_merge"
ners_eopser_exe   = vtd_local.ners_prefix + "/bin/ners_eopser"
ners_config_file  = "/cont/ners.config"

#
#  1:  harmonic expansion: 8  order 8;   24 hours
#  2:  harmonic expansion: 8  order 8;    2 hours
#  3:  harmonic expansion: 8  order 8;   24 hours
#  4:  harmonic expansion: 12 order 8;    2 hours
#  5:  harmonic expansion: 15 order 15;   2 hours
#  6:  harmonic expansion: 15 order 15;   1 hours
#

gti_par = [ \
            "codg_01.vio code 1995y001d 1997y032d 4 create", \
            "codg_02.vio code 1997y033d 1997y054d 4 create", \
            "codg_03.vio code 1997y055d 1998y086d 4 create", \
            "codg_04.vio code 1998y087d 2002y306d 4 create", \
            "codg_05.vio code 2002y307d 2014y291d 4 create", \
            "codg_06.vio code 2014y292d 2049      4 create"  \
          ]
            
#
# --- List of ionospheric files that are corrupted at the primary web site
#
codg_files_replacement = [ \
                         "2016_CODG2100.16I.Z",  \
                         "2018_CODG3070.18I.Z",  \
                         "2019_CODG3640.19I.Z"   \
                         ]

codg_files_missing    = [ \

                         "2022_CODG3310.22I.Z",  \
                         "2022_CODG3320.22I.Z",  \
                         "2022_CODG3330.22I.Z",  \
                         "2022_CODG3340.22I.Z",  \
                         "2022_CODG3350.22I.Z",  \
                         "2022_CODG3360.22I.Z",  \
                         "2022_CODG3370.22I.Z",  \
                         "2022_CODG3380.22I.Z",  \
                         "2022_CODG3390.22I.Z",  \
                         "2022_CODG3400.22I.Z",  \
                         "2022_CODG3410.22I.Z",  \
                         "2022_CODG3420.22I.Z",  \
                         "2022_CODG3430.22I.Z",  \
                         "2022_CODG3440.22I.Z",  \
                         "2022_CODG3450.22I.Z",  \
                         "2022_CODG3460.22I.Z",  \
                         "2022_CODG3470.22I.Z",  \
                         "2022_CODG3480.22I.Z",  \
                         "2022_CODG3490.22I.Z",  \
                         "2022_CODG3500.22I.Z",  \
                         "2022_CODG3510.22I.Z",  \
                         "2022_CODG3520.22I.Z",  \
                         "2022_CODG3530.22I.Z",  \
                         "2022_CODG3540.22I.Z",  \
                         "2022_CODG3550.22I.Z",  \
                         "2022_CODG3560.22I.Z",  \
                         "2022_CODG3570.22I.Z",  \
                         "2022_CODG3580.22I.Z",  \
                         "2022_CODG3590.22I.Z",  \
                         "2022_CODG3600.22I.Z",  \
                         "2022_CODG3610.22I.Z",  \
                         "2022_CODG3620.22I.Z",  \
                         "2022_CODG3630.22I.Z",  \
                         "2022_CODG3640.22I.Z",  \
                         "2022_CODG3650.22I.Z"   \
                     ]

spd_dirs = [ "merra",        \
             "merra2",       \
             "opa_geosfpit", \
             "geosit"        \
           ]

bds_dirs = [ "atm/merra2",   \
             "atm/geosfpit", \
             "atm/geosit",   \
             "lws/merra2",   \
             "lws/geosfpit", \
             "lws/geosit",   \
             "nto/mpiom06",  \
             "nto/mpiom07"   \
           ]

#
# --- List of harmonic
#
hps_list = [ "lws/geosfpit/lws_geosfpit_harmod.hps",    \
             "lws/merra2/lws_merra2_harmod.hps",        \
             "lws/geosfpit/lws_geosfpit_harmod.hps",    \
             "atm/merra2/atm_merra2.hps",               \
             "atm/merra2/atm_merra2_harmod.hps",        \
             "atm/geosfpit/atm_geosfpit_harmod.hps",    \
             "toc/got410c/toc_got410c_harmod.hps",      \
             "toc/fes2014b/toc_fes2014b_harmod.hps",    \
             "toc/fes2012/toc_fes2012_harmod.hps",      \
             "toc/fes2012/toc_fes2012_harmod_s1.hps",   \
             "toc/got48/toc_got48_harmod_s1.hps",       \
             "toc/got48/toc_got48_harmod.hps",          \
             "toc/equil01/toc_equil01_harmod.hps",      \
             "toc/equil02/toc_equil02_harmod.hps"      \
           ]

bds_header_len  = 352
bds_datarec_len =   8
bspd__sum_label = "BSPD Summary file.  Format version of 2024.11.24"
bds__sum_label  = "BINDISP Summary file. Format version of 2015.07.24"
trial_header_len = 14
failed_header = b'<!DOCTYPE HTML'

vtd__io_lock_name    = 'dir_read.lck'
vtd__read_lock_name  = 'dir_read.lck'
vtd__write_lock_name = 'dir_write.lck' 
vau_lock_timeout     = 8

vtd_share  = vtd_local.vtd_data

class vau_config_class:
   def __init__ ( self, filename ):
       self.filename             = filename
#
       self.spd_url              = None
       self.bds_url              = None
       self.hps_url              = None
       self.iono_url             = None
#
       self.log_file             = None
       self.log_verbose_file     = None
#
       self.sta_pos              = None
       self.sta_vel              = None
       self.sta_desc             = None
       self.sta_ecc              = None
       self.sou_coo              = None
       self.sou_prl              = None
       self.sou_names            = None
       self.eph_file             = None
#
       self.eop_file             = None
       self.eopz_file            = None
       self.iono_dir             = None
       self.load_bds_dir         = None
       self.load_hps_dir         = None
       self.load_model_list      = None
       self.spd_bin_dir          = None
       self.spd_model_list       = None
#
       self.eop_series_update    = None
       self.eopz_series_update   = None
       self.iono_update          = None
       self.spd_update           = None
       self.load_update          = None
       self.merra2_geosfpit_atm  = None
       self.merra2_geosfpit_lws  = None

       self.verb                 = None
       self.log_file_handle      = None
       self.log_verb_file_handle = None

   def init ( self ):
       __init__ ( self )

class vau_spd_tim_class:
   def __init__ ( self, tim_rec ):
       self.pref = tim_rec[0:8].decode()
       self.nrec = struct.unpack("q",tim_rec[8:16])[0]
       self.mjd_beg  = struct.unpack("i",tim_rec[16:20])[0]
       self.mjd_end  = struct.unpack("i",tim_rec[20:24])[0]
       self.tai_beg  = struct.unpack("d",tim_rec[24:32])[0]
       self.tai_end  = struct.unpack("d",tim_rec[32:40])[0]
       self.tim_step = struct.unpack("d",tim_rec[40:48])[0]

class vau_spd_lab_class:
   def __init__ ( self, lab_rec ):
       self.pref     = lab_rec[0:8].decode()
       self.len      = struct.unpack("q",lab_rec[8:16])[0]
       self.fmt_lab  = lab_rec[16:56].decode()

       self.off_tim  = struct.unpack("q",lab_rec[56:64])[0]
       self.off_sta  = struct.unpack("q",lab_rec[64:72])[0]
       self.off_mod  = struct.unpack("q",lab_rec[72:80])[0]
       self.off_met  = struct.unpack("q",lab_rec[80:88])[0]
       self.off_elv  = struct.unpack("q",lab_rec[88:96])[0]
       self.off_azm  = struct.unpack("q",lab_rec[96:104])[0]
       self.off_del  = struct.unpack("q",lab_rec[104:112])[0]

       self.len_tim  = struct.unpack("q",lab_rec[112:120])[0]
       self.len_sta  = struct.unpack("q",lab_rec[120:128])[0]
       self.len_mod  = struct.unpack("q",lab_rec[128:136])[0]
       self.len_met  = struct.unpack("q",lab_rec[136:144])[0]
       self.len_elv  = struct.unpack("q",lab_rec[144:152])[0]
       self.len_azm  = struct.unpack("q",lab_rec[152:160])[0]
       self.len_del  = struct.unpack("q",lab_rec[160:168])[0]

       self.tot_num_del  = struct.unpack("i",lab_rec[168:172])[0]


   def init ( self ):
       __init__ ( self )

@contextmanager
def timeout(seconds):
#"""
#    Aucilliary routine for setting timeout 
#"""
    def timeout_handler(signum, frame):
        raise InterruptedError

    original_handler = signal.signal(signal.SIGALRM, timeout_handler)

    try:
        signal.alarm(seconds)
        yield
    finally:
        signal.alarm(0)
        signal.signal(signal.SIGALRM, original_handler)

#
# ------------------------------------------------------------------------
#
def vau_info ( config, ivrb ):
#"""
#   Print on stdout information about the current state of apriori
#
#"""

    date_now = datetime.now().astimezone().strftime("%Y.%m.%d_%H:%M:%S %z")
    print ( "# Summary of SGDASS apriori file. Format version 2024.02.14" )
    print ( "# Status report generated on %s" % date_now )
    print ( "#" )
    if ( config.eop_file ):
         if ( not os.path.isfile ( config.eop_file ) ):
              print ( "ERROR vau: stations position file %s is not found" % config.eop_file )
              exit  ( 1 )
         date_mod_str = datetime.fromtimestamp(os.stat(config.eop_file).st_mtime).astimezone().strftime("%Y.%m.%d_%H:%M:%S %z") 
         print ( "EOP_FILE:  %s   Last modified on: %s" % ( config.eop_file, date_mod_str ) )

         buf = read_file_to_buffer ( config.eop_file )
#
# ------ Read the header of the EOP file and extract information form there
#
         eop_gen_date = "unknown"
         eop_fcs_date = "unknown"
         eop_lt_date  = "unknown"
         for i in range(0,min(64,len(buf))):
             line = buf[i]
             if ( "Generated by NERS" in line and len(line.split()) >= 9 ):
                   eop_gen_date = line.split()[8]
         
             if ( "Last used epoch of AAM" in line and len(line.split()) >= 8 ):
                   eop_fcs_date = line.split()[7]

             if ( "# Last used epoch of Long-term" in line and len(line.split()) >= 8 ):
                   eop_lt_date = line.split()[7]

         print ( "EOP_FILE:  %s   Generated on:     %s" % ( config.eop_file, eop_gen_date ) )
         print ( "EOP_FILE:  %s   Forecast till:    %s" % ( config.eop_file, eop_fcs_date ) )
         print ( "EOP_FILE:  %s   Prediction till:  %s" % ( config.eop_file, eop_lt_date  ) )
         print ( "#" )

    if ( config.eopz_file ):
         if ( not os.path.isfile ( config.eopz_file ) ):
              print ( "ERROR vau: stations position file %s is not found" % config.eopz_file )
              exit  ( 1 )
         date_mod_str = datetime.fromtimestamp(os.stat(config.eopz_file).st_mtime).astimezone().strftime("%Y.%m.%d_%H:%M:%S %z") 
         print ( "EOPZ_FILE: %s  Last modified on: %s" % ( config.eopz_file, date_mod_str ) )

#
# ------ Read the header of the EOP file and extract information form there
#
         buf = read_file_to_buffer ( config.eopz_file )

         eopz_gen_date = "unknown"
         eopz_fcs_date = "unknown"
         eopz_lt_date  = "unknown"
         for i in range(0,min(64,len(buf))):
             line = buf[i]
             if ( "Generated by NERS" in line and len(line.split()) >= 9 ):
                   eop_gen_date = line.split()[8]
         
             if ( "Last used epoch of AAM" in line and len(line.split()) >= 8 ):
                   eop_fcs_date = line.split()[7]

             if ( "# Last used epoch of Long-term" in line and len(line.split()) >= 8 ):
                   eop_lt_date = line.split()[7]

         print ( "EOPZ_FILE: %s  Generated on:     %s" % ( config.eopz_file, eop_gen_date ) )
         print ( "EOPZ_FILE: %s  Forecast till:    %s" % ( config.eopz_file, eop_fcs_date ) )
         print ( "EOPZ_FILE: %s  Prediction till:  %s" % ( config.eopz_file, eop_lt_date  ) )
         print ( "#" )

    if ( config.load_bds_dir ):
         if ( not os.path.isdir ( config.load_bds_dir ) ):
              print ( "ERROR vau: loading directory %s is not found" % config.load_bds_dir )
              exit  ( 1 )
         bds_sum_list = []
         for path, dirs, files in os.walk(config.load_bds_dir):
             for file in files:
                 if ( file == "bds_summary.txt" ):
                      bds_sum_list.append ( path + "/" + file )
         bds_sum_list.sort()
         for bds_sum in bds_sum_list:
             buf = read_file_to_buffer ( bds_sum )
             if ( len(buf) > 8 ):
                  bds_last_update = buf[1].split()[1]
                  bds_first_epoch = buf[2].split()[3][0:19]
                  bds_last_epoch  = buf[3].split()[3][0:19]

             bds_name = bds_sum.replace(config.load_bds_dir,"").replace("/"," ").split()[0] + "/" + \
                        bds_sum.replace(config.load_bds_dir,"").replace("/"," ").split()[1]

             print ( "LOADING: %-22s Last update on: %s Range: [%s, %s]" % ( bds_name, \
                     bds_last_update[0:16], bds_first_epoch[0:16], bds_last_epoch[0:16] ) )
         print ( "#" )

    if ( config.spd_bin_dir ):
         if ( not os.path.isdir ( config.spd_bin_dir ) ):
              print ( "ERROR vau: slant path delay directory %s is not found" % config.spd_bin_dir )
              exit  ( 1 )

         spd_sum_list = []
         for path, dirs, files in os.walk(config.spd_bin_dir):
             for file in files:
                 if ( file == "bspd_summary.txt" ):
                      spd_sum_list.append ( path + "/" + file )

         spd_sum_list.sort()
         for spd_sum in spd_sum_list:
             buf = read_file_to_buffer ( spd_sum )
             if ( len(buf) > 8 ):
                  spd_last_update = buf[2].split()[1]
                  spd_first_epoch = buf[3].split()[3][0:19]
                  spd_last_epoch  = buf[4].split()[3][0:19]

             spd_name = spd_sum.replace(config.spd_bin_dir,"").replace("/"," ").split()[0] + "/" + \
                        spd_sum.replace(config.spd_bin_dir,"").replace("/"," ").split()[1]

             print ( "SPD: %-26s Last update on: %s Range: [%s, %s]" % ( spd_name, \
                     spd_last_update[0:16], spd_first_epoch[0:16], spd_last_epoch[0:16] ) )
         print ( "#" )

    if ( config.iono_dir ):

         vio_file_list = []
         for path, dirs, files in os.walk(config.iono_dir):
             for file in files:
                 if ( len(file) > 5 ):
                      if ( file[-4:] == ".vio" ):
                           vio_file_list.append ( path + "/" + file )

         vio_file_list.sort()
         for vio_file in vio_file_list:
             com = "viono_show_header " + vio_file 
             (ret,out) = exe ( config, com )
             if ( ret != 0 ):
                  for line in out:
                      print ( line )
                  print ( "Error in an attempt to execute command %s" % com )
             date_mod_str = datetime.fromtimestamp(os.stat(vio_file).st_mtime).astimezone().strftime("%Y.%m.%d_%H:%M:%S %z") 
             start_time_str = "unknown"
             stop_time_str = "unknown"
             for line in out:
                 if ( line[0:11] == "START_TIME:" ):
                      start_time_str = line.split()[1]
                 if ( line[0:10] == "STOP_TIME:" ):
                      stop_time_str = line.split()[1]
             print ( "IONO: %-26s  Last update on: %s Range: [%s, %s]" % ( vio_file, \
                     date_mod_str, start_time_str[0:16], stop_time_str ) )

    print ( "#" )

    if ( config.sta_vel ):
         if ( not os.path.isfile ( config.sta_vel ) ):
              print ( "ERROR vau: stations position file %s is not found" % config.sta_vel )
              exit  ( 1 )
         date_mod_str = datetime.fromtimestamp(os.stat(config.sta_vel).st_mtime).astimezone().strftime("%Y.%m.%d_%H:%M:%S %z") 
         print ( "STA_VEL:   %-25s  Last modified on %s" % ( config.sta_vel, date_mod_str ) )

    if ( config.sta_desc ):
         if ( not os.path.isfile ( config.sta_desc ) ):
              print ( "ERROR vau: stations position file %s is not found" % config.sta_desc )
              exit  ( 1 )
         date_mod_str = datetime.fromtimestamp(os.stat(config.sta_desc).st_mtime).astimezone().strftime("%Y.%m.%d_%H:%M:%S %z") 
         print ( "STA_DESC:  %-25s  Last modified on %s" % ( config.sta_desc, date_mod_str ) )

    if ( config.sta_ecc ):
         if ( not os.path.isfile ( config.sta_ecc ) ):
              print ( "ERROR vau: stations position file %s is not found" % config.sta_ecc )
              exit  ( 1 )
         date_mod_str = datetime.fromtimestamp(os.stat(config.sta_ecc).st_mtime).astimezone().strftime("%Y.%m.%d_%H:%M:%S %z") 
         print ( "STA_ECC:   %-25s  Last modified on %s" % ( config.sta_ecc, date_mod_str ) )

    if ( config.sou_coo ):
         if ( not os.path.isfile ( config.sou_coo ) ):
              print ( "ERROR vau: stations position file %s is not found" % config.sou_coo )
              exit  ( 1 )
         date_mod_str = datetime.fromtimestamp(os.stat(config.sou_coo).st_mtime).astimezone().strftime("%Y.%m.%d_%H:%M:%S %z") 
         print ( "SOU_COO:   %-25s  Last modified on %s" % ( config.sou_coo, date_mod_str ) )

    if ( config.sou_prl ):
         if ( not os.path.isfile ( config.sou_prl ) ):
              print ( "ERROR vau: stations position file %s is not found" % config.sou_prl )
              exit  ( 1 )
         date_mod_str = datetime.fromtimestamp(os.stat(config.sou_prl).st_mtime).astimezone().strftime("%Y.%m.%d_%H:%M:%S %z") 
         print ( "SOU_PRL:   %-25s  Last modified on %s" % ( config.sou_prl, date_mod_str ) )

    if ( config.sou_names ):
         if ( not os.path.isfile ( config.sou_names ) ):
              print ( "ERROR vau: stations position file %s is not found" % config.sou_names )
              exit  ( 1 )
         date_mod_str = datetime.fromtimestamp(os.stat(config.sou_names).st_mtime).astimezone().strftime("%Y.%m.%d_%H:%M:%S %z") 
         print ( "SOU_NAMES: %-25s  Last modified on %s" % ( config.sou_names, date_mod_str ) )

    if ( config.eph_file ):
         if ( not os.path.isfile ( config.eph_file ) ):
              print ( "ERROR vau: stations position file %s is not found" % config.eph_file )
              exit  ( 1 )
         date_mod_str = datetime.fromtimestamp(os.stat(config.eph_file).st_mtime).astimezone().strftime("%Y.%m.%d_%H:%M:%S %z") 
         print ( "EPH_FILE:  %-25s  Last modified on %s" % ( config.eph_file, date_mod_str ) )

#
# ------------------------------------------------------------------------
#
def parse_vau_config ( config ):
#"""
#    Reads the configuration file which has format KEYWORD: VALUE 
#    and puts parsed information into fields of class config
#"""
   conf_buf = read_file_to_buffer ( config.filename )
   if ( not conf_buf ): exit ( 1 )

#
# --- Expand environment variables
#
   conf_buf = expand_env_vars ( conf_buf, '#' )
   if ( not conf_buf ): exit ( 1 )

   if ( conf_buf[0][0:len(fmt__label)] != fmt__label[0:len(fmt__label)] ):
        print ( "Unsupported format of config file " + config.filename + \
                "\n Format label found:   " + conf_buf[0] + \
                "\n While expected label: " + fmt__label + "\n" )
        exit ( 1 )

   num_par = 0
   for line in conf_buf:
       if ( line == fmt__label ): continue
       if   ( line[0:1]                  == "#" ): continue
       if   ( line.split()[0]            == "SPD_URL:"  ): 
              config.spd_url              = line.split()[1] 
              num_par = num_par + 1

       elif ( line.split()[0]            == "BDS_URL:" ):
              config.bds_url              = line.split()[1]  
              num_par = num_par + 1

       elif ( line.split()[0]            == "HPS_URL:" ):
              config.hps_url              = line.split()[1]  
              num_par = num_par + 1

       elif ( line.split()[0]            == "IONO_URL:" ):
              config.iono_url             = line.split()[1]  
              num_par = num_par + 1

       elif  ( line.split()[0]           == "LOG_FILE:" ):
               config.log_file      =      line.split()[1]
               num_par = num_par + 1

       elif  ( line.split()[0]           == "LOG_VERBOSE_FILE:" ):
               config.log_verbose_file    = line.split()[1]
               num_par = num_par + 1

#
       elif  ( line.split()[0]           == "STA_POS:" ):
               config.sta_pos             = line.split()[1]
               num_par = num_par + 1

       elif  ( line.split()[0]           == "STA_VEL:" ):
               config.sta_vel             = line.split()[1]
               num_par = num_par + 1

       elif  ( line.split()[0]           == "STA_DESC:" ):
               config.sta_desc             = line.split()[1]
               num_par = num_par + 1

       elif  ( line.split()[0]           == "STA_ECC:" ):
               config.sta_ecc             = line.split()[1]
               num_par = num_par + 1

       elif  ( line.split()[0]           == "SOU_COO:" ):
               config.sou_coo             = line.split()[1]
               num_par = num_par + 1

       elif  ( line.split()[0]           == "SOU_PRL:" ):
               config.sou_prl             = line.split()[1]
               num_par = num_par + 1

       elif  ( line.split()[0]           == "SOU_NAMES:" ):
               config.sou_names             = line.split()[1]
               num_par = num_par + 1

       elif  ( line.split()[0]           == "EPH_FILE:" ):
               config.eph_file             = line.split()[1]
               num_par = num_par + 1

       elif  ( line.split()[0]           == "VERBOSITY_LEVEL:" ):
               config.verb                = int(line.split()[1])
               num_par = num_par + 1

       elif  ( line.split()[0]           == "EOP_FILE:" ):
               config.eop_file            = line.split()[1]
               num_par = num_par + 1

       elif  ( line.split()[0]           == "EOPZ_FILE:" ):
               config.eopz_file           = line.split()[1]
               num_par = num_par + 1

       elif  ( line.split()[0]           == "IONO_DIR:" ):
               config.iono_dir            = line.split()[1]
               num_par = num_par + 1

       elif  ( line.split()[0]           == "LOAD_BDS_DIR:" ):
               config.load_bds_dir        = line.split()[1]
               num_par = num_par + 1

       elif  ( line.split()[0]           == "LOAD_HPS_DIR:" ):
               config.load_hps_dir        = line.split()[1]
               num_par = num_par + 1

       elif  ( line.split()[0]           == "SPD_BIN_DIR:" ):
               config.spd_bin_dir         = line.split()[1]
               num_par = num_par + 1

       elif  ( line.split()[0]           == "LOAD_MODEL_LIST:" ):
               temp_list                  = line.replace(line.split()[0],"").split(",")
               config.load_model_list = []
               for item in temp_list:
                   item = item.replace(" ","").lower()
                   if ( item in bds_dirs ):
                        config.load_model_list.append ( item )
                   else:
                        print ( "Unsupported load model %s was found in configuration file %s. The list of supported models: %s" %
                                 ( item, config.filename,  " ".join(bds_dirs) ) )
                        exit  ( 1 )

               num_par = num_par + 1

       elif  ( line.split()[0]           == "SPD_MODEL_LIST:" ):
               temp_list                  = line.replace(line.split()[0],"").split(",")
               config.spd_model_list      = []
               for item in temp_list:
                   item = item.replace(" ","").lower()
                   if ( item in spd_dirs ):
                        config.spd_model_list.append ( item.replace(" ","").lower() )
                   else:
                        print ( "Unsupported slant path model %s was found in configuration file %s. The list of supported models: %s" %
                                 ( item, config.filename,  " ".join(spd_dirs) ) )
                        exit  ( 1 )



               num_par = num_par + 1

       elif  ( line.split()[0]           == "EOP_SERIES_UPDATE:" ):
               config.eop_series_update   = line.split()[1]
               num_par = num_par + 1

       elif  ( line.split()[0]           == "EOPZ_SERIES_UPDATE:" ):
               config.eopz_series_update  = line.split()[1]
               num_par = num_par + 1

       elif  ( line.split()[0]           == "IONO_UPDATE:" ):
               config.iono_update         = line.split()[1].lower()
               num_par = num_par + 1

       elif  ( line.split()[0]           == "SPD_UPDATE:" ):
               config.spd_update          = line.split()[1].lower()
               num_par = num_par + 1

       elif  ( line.split()[0]           == "LOAD_UPDATE:" ):
               config.load_update         = line.split()[1].lower()
               num_par = num_par + 1

       elif  ( line.split()[0]           == "MERRA2_GEOSFPIT_ATM:" ):
               config.merra2_geosfpit_atm = line.split()[1].lower()
               num_par = num_par + 1

       elif  ( line.split()[0]           == "MERRA2_GEOSFPIT_LWS:" ):
               config.merra2_geosfpit_lws = line.split()[1].lower()
               num_par = num_par + 1

       else:
              print ( "Unrecognized keyword " + line.split()[0] + \
                      " in control file " + config.filename )
              exit ( 1 )

   if ( num_par < config__num_par ):
        print ( "Not all keywords were found in in control file " + \
                 config.filename + " -- only %d, though %d were expected" % \
                 ( num_par, config__num_par ) )
        exit ( 1 )

 
   config.log_file_handle = None
   config.log_verb_file_handle = None

#
# ------------------------------------------------------------------------
#
def print_log ( config, str ):
#"""
#   print string str into both log-file specified in config file and to  
#   stdout
#"""
    now = datetime.now() 
    
    sys.stdout.flush()
#
    config.log_file_handle = open ( config.log_file, "a" )
    if ( str == "@start@" ):
         print ( "", file=config.log_file_handle )
         print ( "vtd_apriori_update: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " Started", \
                  file=config.log_file_handle )
         print ( "================================================", file=config.log_file_handle )
    else:
         print ( "vtd_apriori_update: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str, \
                  file=config.log_file_handle )
         print ( "vtd_apriori_update: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str )
    
    config.log_file_handle.flush()
    config.log_file_handle.close()
    print_log_verb ( config, str )

#
# ------------------------------------------------------------------------
#
def print_log_verb ( config, str ):
#"""
#   print string str into both log-file specified in config file and to  
#   stdout
#"""
    now = datetime.now() 
    
    sys.stdout.flush()
#
    config.log_verb_file_handle = open ( config.log_verbose_file, "a" )
    print ( "vtd_apriori_update: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str, \
            file=config.log_verb_file_handle )
    config.log_verb_file_handle.flush()
    config.log_verb_file_handle.close()

#
# ------------------------------------------------------------------------
#
def exe ( config, command ):
#"""
#    Spawn a supborcess, execute command in the context of the suborpess,
#    wait for its completion and return completion code and results.
#"""
    words = command.split()
    time_str = str(datetime.now().strftime("%Y.%m.%d_%H:%M:%S.") + "%6d" % datetime.now().microsecond).replace( " ", "0" )
    if ( config.log_verbose_file != "no" ):
         print_log_verb ( config, "Running command " + command )
    (ret, out) = subprocess.getstatusoutput ( command )
    return ( ret, out.split ( "\n" ) )

#
# ------------------------------------------------------------------------
#
def check_and_wait_remote_lock ( config, lock_url, import_lock_file, \
                                 lock_check_tries, lock_check_interval ):
# """
#   Routine check_and_wait_remote_lock checks for the import_lock_file.
#   If it does not exist, it returns. If it exists, it reads it and
#   extracts the expiration time. Ih it has already expired, it returns.
#   If not, it waits for the shortest among lock_check_interval or
#   lock expiration time. Then it checks again for the external lock
#   file and the procedure repeats. The routine makes lock_check_tries
#   tries and then returns.
# """
    spd_lock_str = None
    for i in range(0,lock_check_tries):
#
# ----- Try to download the remote lock file
#
        comstr = 'wget ' + \
             '-q '   + \
             '--tries=5 ' + \
             '--timeout=1 ' \
             '--retry-connrefused ' + \
             '-O ' + import_lock_file + " " + \
             lock_url 
        ( ret, out ) = exe ( config, comstr )
        if ( ret == 0 ):
#
# ---------- Uuh! You know what? The lock file exists
# ---------- Read it and extract the lock line 
#
             with open(import_lock_file,encoding="latin") as f:
                  lock_buf = f.read().splitlines()
             f.close ( )
             lock_str = lock_buf[0].strip("\n").strip("\r")
             os.unlink ( import_lock_file )
        else:
#
# ---------- We did not get external lock file
#
             lock_str = None

        if ( lock_str ):
#
# ---------- The external directory is locked. Let us know for how long
#
             tim_lock_expire = datetime.strptime ( lock_str.split()[6], "%Y%m%d_%H%M%S" )
             if ( pyvers >= "0312000" ):
                  tim_utc_now = datetime.now(timezone.utc).replace(tzinfo=None)
             else:
                  tim_utc_now = datetime.utcnow().replace(tzinfo=None)

             if ( tim_lock_expire > tim_utc_now ):
#
# --------------- Aga, the lock has not expired. Let is learn for how long to wait
#
                  time_to_wait = (tim_lock_expire - tim_utc_now).seconds
                  if ( ivrb >= 2 ):
                       tim_now = datetime.now().replace(tzinfo=None)
                       print ( "vtd_apriori_update: wait for %4.0f seconds due to lock file" % time_to_wait )
         
#
# --------------- Wait for shortest: till the lock expires or for lock_check_interval
#
                  time.sleep ( min ( time_to_wait, lock_check_interval ) )
             else:
#
# --------------- The lock has expired
#
                  break
        else:
#
# ----------- We did not find the lock file. The remote server has lift the lock.
#
             break
    return ( 0 )
#
# ------------------------------------------------------------------------
#
def vau_eops ( mode, config ):
#"""
#    Update EOP using NERS client
#"""
    if ( os.path.isfile(ners_config_file) ):
         os.environ["NERS_CONFIG"] =  ners_config_file
    if ( mode == "eop" ): 
         filout = config.eop_file
         filout_temp = filout + "_%08d" % os.getpid() 
         if ( filout == None ):
              print_log      ( config, "Configuration error: EOP_FILE has not been defined" )
              exit ( 1 )        
               
         com_str = ners_eopser_exe + " -p polu  -b 1980.01.01 -e future10d -s 86400.0" + " > " + \
                   filout_temp
    elif ( mode == "eopz" ): 
         filout = config.eopz_file
         filout_temp = filout + "_%08d" % os.getpid() 
         if ( filout == None ):
              print_log      ( config, "Configuration error: EOPZ_FILE has not been defined" )
              exit ( 1 )        

         com_str = ners_eopser_exe + " -p poluz -b 1980.01.01 -e future10d -s 86400.0" + " > " + \
                   filout_temp

    if ( mode == "eop" or mode == "eopz" ): 
         ( ret, out ) = exe ( config, com_str )
         if ( ret != 0 ):
              print_log      ( config, "Failure in updating EOP using command " + com_str )
              for line in out:
                  print_log_verb ( config, line )
     
              exit ( 1 )        
         os.rename ( filout_temp, filout )
#
# --- Check whether the EOP seris are correct
#
    eop_ser = read_file_to_buffer ( filout ) 

    if ( eop_ser[0][0:18] != "# NERS EOP series." ):
#
# ------ Fix the problem if the first line is a warning
#
         f = open ( filout, "w" )
         print ( eop_ser[1], file=f )
         print ( eop_ser[2], file=f )
         print ( eop_ser[3], file=f )
         print ( '# ' + eop_ser[0], file=f )
         for i in range(4,len(eop_ser)):
             print ( eop_ser[i], file=f )
         f.close()

#
# ------------------------------------------------------------------------
#
def vau_download_iono ( config, ivrb ):
#"""
#    Update series of the ionospheric contribution
#"""
    if ( os.path.isdir ( config.iono_dir ) ):
         try:
              shutil.rmtree ( config.iono_dir )
         except BaseException as e:
              print_log ( config, "Failure in removing directory " + config.iono_dir + \
                          " -- " + str(e) )
              exit ( 1 )

    vau_iono ( config, ivrb )

#
# ------------------------------------------------------------------------
#
def vau_iono ( config, ivrb ):
#"""
#    Update series of the ionospheric contribution
#"""
    if ( not os.path.isdir ( config.iono_dir ) ):
         try:
             oldmask = os.umask(0) ; os.makedirs ( config.iono_dir, mode=0o775 ) ; oldmask=os.umask(oldmask)
         except:
             print_log ( config, "Failure in creating directory " + config.iono_dir )
             exit ( 1 )
    
    asc_dir = config.iono_dir + "/code_asc"
    if ( not os.path.isdir ( asc_dir ) ):
         try:
             oldmask = os.umask(0) ; os.makedirs ( asc_dir, mode=0o775 ) ; oldmask=os.umask(oldmask)
         except:
             print_log ( config, "Failure in creating directory " + asc_dir )
             exit ( 1 )

    bin_dir = config.iono_dir + "/code_bin"
    if ( not os.path.isdir ( bin_dir ) ):
         try:
             oldmask = os.umask(0) ; os.makedirs ( bin_dir, mode=0o775 ) ; oldmask=os.umask(oldmask)
         except:
             print_log ( config, "Failure in creating directory " + bin_dir )
             exit ( 1 )
                  
    year_now = int(datetime.now().strftime("%Y"))

#
# --- Download ionospheric files (one file per day) for this year and the last year
#
    for year in range(iono_year_beg,year_now+1):
        iono_year_dir = asc_dir + '/%d' % year
        if ( not os.path.isdir ( iono_year_dir ) ):
             fl_new_year = 1
             try:
                 oldmask = os.umask(0) ; os.makedirs ( iono_year_dir, mode=0o775 ) ; oldmask=os.umask(oldmask)
             except:
                 print_log ( config, "Failure in creating directory %s" % iono_year_dir )
                 exit ( 1 )
        else:
             fl_new_year = 0
        year_4str = str("%d" % year)[0:4]
        year_2str = str("%d" % year)[2:4]
        if ( year < 2023 ):
             pat = '-A "CODG*.%sI.Z" ' % year_2str
        else:
             pat = '-A "COD0OPSFIN_%s*_GIM.INX.gz" ' % year_4str
        comstr = 'cd  ' + iono_year_dir + '; ' + \
                 'wget ' + \
                 '-q '   + \
                 '-r '   + \
                 '-l 1 '   + \
                 '--no-parent ' + \
                 '-c '   + \
                 '-N '   + \
                 '-nH '  + \
                 '--cut-dirs=8 '  + \
                 '--timeout=30 ' \
                 '--tries=64 ' \
                 '--retry-connrefused ' \
                 '-e robots=off ' + \
                 '-X robots.txt ' + \
                 '-R "*index*" '  + \
                 '-R "*.gif" '    + \
                 '--max-redirect=1 '   + \
                 '--retr-symlinks=no ' + \
                 '--include-directories=/CODE/%d' % year + ' ' + \
                 pat + " " + \
                 config.iono_url  + '/%d' % year
        if ( fl_new_year == 1 or year >= year_now-1 ):
             if ( config.verb >= 4 ):
                  print ( "About to execute ", comstr ) 
             ( ret, out ) = exe ( config, comstr )
             if ( ret != 0 ):
                  print_log      ( config, "Failure in downloading iono" )
                  print_log      ( config, "Failed command %s" % comstr )
                  for line in out:
                      print_log_verb ( config, line )

                  exit ( 1 )        
             print_log      ( config, "Downloaded iono for %d" % year )

             ( ret, out ) = exe ( config, comstr )

#
# --- Check for files missing from CODE ftp site and copy them from
# --- VTD distribution
#
    for codg_name in codg_files_missing:
        year = codg_name.split("_")[0]
        codg_dir  = asc_dir + "/" + year
        codg_file = asc_dir + "/" + year + "/" + codg_name.split("_")[1]
        if ( not os.path.isdir(codg_file) ):
             comstr = "cp " + vtd_share + "/" + codg_name + " " + codg_file 
             ( ret, out ) = exe ( config, comstr )
             if ( ret != 0 ):
                  print_log ( config, "Failure in copying iono missing file" )
                  print_log ( config, "Failed command: %s" % comstr )
                  for line in out:
                      print_log_verb ( config, line )
                  exit ( 1 )        
             if ( config.verb >=2 ):
                  print_log ( config, "Copied missing file to " + \
                                      vtd_share + "/" + codg_name + " " + codg_file )

#
# --- Check for damaged files that reside at the remote server and 
# --- replace them with corrected versions
#
    for codg_name in codg_files_replacement:
        year = codg_name.split("_")[0]
        codg_dir  = asc_dir + "/" + year
        codg_file = asc_dir + "/" + year + "/" + codg_name.split("_")[1]
        if ( os.path.isdir(codg_file) ):
             comstr = "cp " + vtd_share + "/" + codg_name + " " + codg_file 
             ( ret, out ) = exe ( config, comstr )
             if ( ret != 0 ):
                  print_log ( config, "Failure in copying iono replacement file" )
                  print_log ( config, "Failed command: %s" % comstr )
                  for line in out:
                      print_log_verb ( config, line )
                  exit ( 1 )        
             print_log ( config, "Copied replacement file to " + \
                                  vtd_share + "/" + codg_name + " " + codg_file )


#
# --- Now convert ascii ionospheric files into segments of 
# --- the ionospheric files in binary format. We have to repeat the 
# --- procedure for several segments because of dicontinuites 
# --- in ionospheric series
#
    for gti_par_line in gti_par:
        iono_lock_io_file    = bin_dir  + "/" + vtd__io_lock_name
        iono_lock_read_file  = bin_dir  + "/" + vtd__read_lock_name
        iono_lock_write_file = bin_dir  + "/" + vtd__write_lock_name
        vio_file            = bin_dir  + "/" + gti_par_line.split()[0]
        temp_vio_file       = vio_file.replace    ( ".vio", "_%08d" % os.getpid() + ".temp_vio" )
        temp_gti_par_line   = gti_par_line.replace( ".vio", "_%08d" % os.getpid() + ".temp_vio" )

#
# ----- Generate a temporary ionosphere binary file
#
        comstr = gti_update_exe + " " + \
                 asc_dir        + " " + \
                 bin_dir  + "/" + temp_gti_par_line
        if ( config.verb >= 4 ):
             print ( "About to execute ", comstr ) 
        ( ret, out ) = exe ( config, comstr )
        if ( ret != 0 ):
             print_log      ( config, "Failure in transforming iono TEC maps to binary form" )
             print_log      ( config, "Failed command %s" % comstr )
             for line in out:
                 print_log_verb ( config, line )

             exit ( 1 )        
        if ( os.path.isfile(temp_vio_file) ):
#
# ---------- Set the readlock to prevent other programs from reading 
# ---------- the ionospheric file while it is being renamed 
#
             (fd_rd, fd_wr) = vau_set_readlock ( iono_lock_io_file, \
                                                 iono_lock_read_file, iono_lock_write_file )
             if ( fd_rd == None ):
                  print_log ( config, "Cannot create read lock on the %s directory" % \
                                       loc_iono_dir )
                  exit ( 1 )
#
# ---------- Rename the temporary ionosphere file to a permanent name
#
             try:
                 os.rename ( temp_vio_file, vio_file )
             except BaseException as e: 
                 print_log  ( config, "Error in renaming ionosphere file %s to %s: %s" % \
                                       ( temp_vio_file, vio_file, str(e) ) )
                 exit ( 1 )        
#
# ---------- Lift the readlock
#
             vau_lift_readlock ( fd_rd, fd_wr )
             print_log ( config, "Transformed iono to binary file " + gti_par_line.split()[0] )
           
#
# ------------------------------------------------------------------------
#
def vau_download_spd ( config, ivrb ):
#"""
#    Downnload time series of slant path delay
#"""

    if ( os.path.isdir ( config.spd_bin_dir ) ):
         try:
              shutil.rmtree ( config.spd_bin_dir )
         except BaseException as e:
              print_log ( config, "Failure in removing directory " + config.spd_bin_dir + \
                          " -- " + str(e) )
              exit ( 1 )
    vau_spd ( config, ivrb )

#
# ------------------------------------------------------------------------
#
def vau_spd ( config, ivrb ):
    """
    Updates slant path delay (SPD) a priori files
    """

#
# --- Check whether the directory with path delay in binary format exists
#
    if ( not os.path.isdir ( config.spd_bin_dir ) ):
         try:
             oldmask = os.umask(0) ; os.makedirs ( config.spd_bin_dir, mode=0o775 ) ; oldmask=os.umask(oldmask)
         except:
             print_log ( config, "Failure in creating directory " + config.spd_bin_dir )
             return ( 1 )        
#
# --- Check whether the sandbox directory for path delays in binary format exists
#
    spd_bin_sandbox_dir = config.spd_bin_dir + "/sandbox"
    if ( not os.path.isdir ( spd_bin_sandbox_dir ) ):
         try:
             oldmask = os.umask(0) ; os.makedirs ( spd_bin_sandbox_dir, mode=0o775 ) ; oldmask=os.umask(oldmask)
         except:
             print_log ( config, "Failure in creating directory " + spd_bin_sandbox_dir )
             return ( 1 )        
#
# --- Check and fix bspd if the data at the remote server changed during
# --- downlading
#
    (ret,err) = check_and_fix_bspd ( config, ivrb )
    if ( ret != 0 ):
         print_log ( config, "Found a damaged directory with slant path delays in %s . Please fix it" % \
                     config.spd_bin_dir )
         print_log ( config, "vtd_apriori_udpdate: Failure to update slant path delays" )
         print ( "Found a damaged directory with slant path delays in %s . Please fix it" % \
                  config.spd_bin_dir )
         print ( "vtd_apriori_udpdate: Failure to update slant path delays" )
         return ( 1 )        

    for spd_dir in spd_dirs:
        if ( not spd_dir in config.spd_model_list ): continue
#
# ----- Cycle over SPD directories spefified by the yser 
# ----- to check whether we have to download all the files
#
        if ( ivrb >= 2 ):
             print ( "vau_spd: Phase 1 of processing directory %s" % spd_dir )
             sys.stdout.flush()
        loc_bspd_dir = config.spd_bin_dir + "/" + spd_dir
        if ( not os.path.isdir ( loc_bspd_dir ) ):
#
# ---------- Local directory does not exist? Create it!
#
             try:
                 oldmask = os.umask(0) ; os.makedirs ( loc_bspd_dir, mode=0o775 ) ; oldmask=os.umask(oldmask)
             except:
                 print_log ( config, "Failure in creating directory " + loc_bspd_dir )
                 return ( 1 )        

        bspd_summary_url = loc_bspd_dir + "/bspd_summary.txt"       

#
# ----- Check whether the summary file exists in the local directory
#
        loc_bspd_summary_file = loc_bspd_dir + "/bspd_summary.txt"
        if ( not os.path.isfile ( loc_bspd_summary_file ) ):
             print_log ( config, "Summary file %s was not found. Copy all the binary slant path delay files from the remote server %s" % \
                         ( loc_bspd_summary_file, config.spd_url + "/" + spd_dir + "/" ) )
#
# ---------- Does not? Let us download all the SPD files for a given model 
# ---------- from the remote server
#
             comstr = 'cd  ' + loc_bspd_dir + '; ' + \
                 'wget ' + \
                 '-r '   + \
                 '-q '   + \
                 '-l 1 '   + \
                 '--no-parent ' + \
                 '-c '   + \
                 '-N '   + \
                 '-nH '  + \
                 '--timeout=30 ' \
                 '--tries=64 ' \
                 '--retry-connrefused ' \
                 '--cut-dirs=8 '  + \
                 '-e robots=off ' + \
                 '-X robots.txt ' + \
                 '-R "*index*" '  + \
                 '-R "*.gif" '    + \
                 '-R "*.png" '    + \
                 config.spd_url + "/" + spd_dir + "/"
             ( ret, out ) = exe ( config, comstr )
             if ( ret != 0 ):
                  print_log ( config, "Failure in downloading slant path delays from %s" % config.spd_url )
                  for line in out:
                      print_log_verb ( config, line )
                  return ( 1 )
             print_log      ( config, "Downloaded all binary spd files from %s" % spd_dir )

#
# ---------- Check and fix bspd if the data at the remote server changed during
# ---------- downlading
#
             (ret,err) = check_and_fix_bspd ( config, ivrb )
             if ( ret != 0 ):
                  print_log ( config, "Found a damaged directory with slant path delays in %s . Please fix it" % \
                              config.spd_bin_dir )
                  print_log ( config, "vtd_apriori_udpdate: Failure to update slant path delays" )
                  print ( "Found a damaged directory with slant path delays in %s . Please fix it" % \
                           config.spd_bin_dir )
                  print ( "vtd_apriori_udpdate: Failure to update slant path delays" )
                  return ( 1 )
       
#
# --- Cycle over slant path delay models
#
    for spd_dir in spd_dirs:
        if ( not spd_dir in config.spd_model_list ): continue
#
# ----- Cycle over SPD directories spefified by the yser 
# ----- to check whether we have to download all the files
#
        if ( ivrb >= 2 ):
             print ( "vau_spd: Phase 2 of processing directory %s" % spd_dir )
             sys.stdout.flush()

        spd_sandbox_dir = spd_bin_sandbox_dir + "/" + spd_dir

#
# ----- Check the sandbox directory
#
        if ( not os.path.isdir ( spd_sandbox_dir ) ):
             try:
                 oldmask = os.umask(0) ; os.makedirs ( spd_sandbox_dir, mode=0o775 ) ; oldmask=os.umask(oldmask)
             except:
                 print_log ( config, "Failure in creating directory " + spd_sandbox_dir )
                 return ( 1 )

        loc_bspd_dir = config.spd_bin_dir + "/" + spd_dir 
        bspd_summary_url = config.spd_url + "/" + spd_dir + "/bspd_summary.txt"       
        loc_bspd_summary_file = loc_bspd_dir + "/bspd_summary.txt"
        tem_rem_bspd_summary_file = loc_bspd_summary_file + "_" + "%08d" % os.getpid()

#
# ----- Check for the remote lock and wait if it is set
#
        check_and_wait_remote_lock ( config, config.spd_url + "/" + spd_dir + "/lock", \
                                     "/tmp/spd_lock__" + "%08d" % os.getpid(), \
                                     lock_check_tries, lock_check_time )
#
# ----- Read the remote summary file and put it in the temporary file
#
        comstr = 'cd  ' + loc_bspd_dir + '; ' + \
                 'wget ' + \
                 '-q '   + \
                 '--tries=64 ' + \
                 '--retry-connrefused ' + \
                 '-O ' + tem_rem_bspd_summary_file + " " + \
                 config.spd_url + "/" + spd_dir + "/bspd_summary.txt"

        ( ret, out ) = exe ( config, comstr )
        if ( ret != 0 ):
             print_log ( config, "Failure in downloading bspd_summary.txt file with command %s " % \
                         comstr )
             for line in out:
                 print_log_verb ( config, line )
             return ( 1 )        
#
# ----- and split it into lines
#
        with open(tem_rem_bspd_summary_file ,encoding="latin") as f:
            rem_sum_buf = f.read().splitlines()
        f.close ( )
#
# ----- remove it 
#
        os.unlink ( tem_rem_bspd_summary_file )
#
# ----- and check its label
#
        if ( rem_sum_buf[0] != bspd__sum_label ):
             print_log ( config, "Trap of internal control: bspd summary file %s from the remote server %s not have a supported format label" % \
                         ( tem_rem_bspd_summary_file,  config.spd_url + "/" + spd_dir + "/bspd_summary.txt" ) )
             return ( 1 )

#
# ----- Read a local file with summary of slant path delays
#
        with open(loc_bspd_summary_file,encoding="latin") as f:
             loc_sum_buf = f.read().splitlines()
        f.close ( )

        if ( loc_sum_buf[0] != bspd__sum_label ):
             print_log ( config, "Trap of internal control: bspd summary file %s does not have supported format label" % + loc_bspd_summary_file )
             return ( 1 )
           
        if ( len(loc_sum_buf) != len(rem_sum_buf) ):
#
# ---------- We have detected that the number of stations in the local and remote
# ---------- directories is different. We purge all slant path delays from the 
# ---------- local directory
#
            for path, dirs, files in os.walk(config.spd_bin_dir + "/" + spd_dir):
                for fil in files:
                    if ( ".bspd" in fil        or \
                         "bspd_summary" in fil or \
                         "#" in fil            or \
                         "~" in fil ):
#
                         finam = path + "/" + fil
                         try:
                            os.unlink ( finam )
                         except BaseException as e:
                            print_log ( config, "A trap of internal control: cannot " + \
                                                "remove file %s" % ( finam, str(e) ) )
                            return ( 1 )        
                         print_log_verb ( config, "Remove file " + finam )
                print_log_verb ( config, "The lenght of lcoal and summary files is different for %s" % \
                                 loc_bspd_dir )
                return 1

#
# ----- Extract important field from the remote summary file.
#
        for line in rem_sum_buf:
            if ( line.split()[0] == "Min_epoch:" ):
                 rem_mjd_beg = int(line.split()[1])
                 rem_tai_beg = float(line.split()[2])
                 rem_dat_beg = line.split()[3]
            if ( line.split()[0] == "Max_epoch:" ):
                 rem_mjd_end = int(line.split()[1])
                 rem_tai_end = float(line.split()[2])
                 rem_dat_end = line.split()[3]
            if ( line.split()[0] == "Num_Epoch:" ):
                 rem_num_epochs = int(line.split()[1])
            if ( line.split()[0] == "Num_Stations:" ):
                 rem_num_sta = int(line.split()[1])
            if ( line.split()[0] == "Sample_Interval:" ):
                 rem_tim_stp = float(line.split()[1])
            if ( line.split()[0] == "Prefix:" ):
                 rem_prefix = line.split()[1]
            if ( line.split()[0] == "Delay_offset:" ):
                 rem_del_off = int(line.split()[1])
            if ( line.split()[0] == "Delay_record_len:" ):
                 rem_del_rec_len = int(line.split()[1])

        if ( ivrb >= 3 ):
             print ( "loc_bspd_summmary_file= ", loc_bspd_summary_file )

        if ( loc_sum_buf[0] != bspd__sum_label ):
             print_log ( config, "Trap of internal control: bspd summary file %s " + \
                                 "does not have supported format label" % + \
                                  tem_bspd_summary_file )
#
# ----- Extract important field from the local summary file.
#
        for line in loc_sum_buf:
            if ( line.split()[0] == "Min_epoch:" ):
                 loc_mjd_beg = int(line.split()[1])
                 loc_tai_beg = float(line.split()[2])
                 loc_dat_beg = line.split()[3]
            if ( line.split()[0] == "Max_epoch:" ):
                 loc_mjd_end = int(line.split()[1])
                 loc_tai_end = float(line.split()[2])
                 loc_dat_end = line.split()[3]
            if ( line.split()[0] == "Num_Epoch:" ):
                 loc_num_epochs = int(line.split()[1])
            if ( line.split()[0] == "Num_Stations:" ):
                 loc_num_sta = int(line.split()[1])
            if ( line.split()[0] == "Sample_Interval:" ):
                 loc_tim_stp = float(line.split()[1])
            if ( line.split()[0] == "Prefix:" ):
                 loc_prefix = line.split()[1]
            if ( line.split()[0] == "Delay_offset:" ):
                 loc_del_off = int(line.split()[1])
            if ( line.split()[0] == "Delay_record_len:" ):
                 loc_del_rec_len = int(line.split()[1])

#
# ----- Compare field of local and remote summary files
#
        if ( loc_mjd_beg     == rem_mjd_beg     and \
             loc_tai_beg     == rem_tai_beg     and \
             loc_mjd_end     == rem_mjd_end     and \
             loc_tai_end     == rem_tai_end     and \
             loc_num_epochs  == rem_num_epochs  and \
             loc_num_sta     == rem_num_sta     and \
             loc_tim_stp     == rem_tim_stp     and \
             loc_prefix      == rem_prefix      and \
             loc_del_off     == rem_del_off     and \
             loc_del_rec_len == rem_del_rec_len     ):

             print_log      ( config, "No SPD data to update in %s" % loc_bspd_dir )
             continue

        if ( ivrb >= 2 ):
             print ( "vau_spd REM beg: ", rem_mjd_beg, rem_tai_beg, \
                     " end: ", rem_mjd_end, rem_tai_end, \
                     " num_epc: ", rem_num_epochs, " rem_num_sta: ", rem_num_sta,  \
                     " rem_tim_stp: ", rem_tim_stp, " rem_prefix: ", rem_prefix, \
                     " rem_del_off: ", rem_del_off, " rem_del_rec_len: ", rem_del_rec_len )

             print ( "vau_spd LOC beg: ", loc_mjd_beg, loc_tai_beg, \
                     " end: ", loc_mjd_end, loc_tai_end, \
                     " num_epc: ", loc_num_epochs, " loc_num_sta: ", loc_num_sta,  \
                     " loc_tim_stp: ", loc_tim_stp, " loc_prefix: ", loc_prefix, \
                     " loc_del_off: ", loc_del_off, " loc_del_rec_len: ", loc_del_rec_len )

        if ( abs((rem_mjd_beg - loc_mjd_beg)*86400.0 + (rem_tai_beg - loc_tai_beg) ) > time_eps ):
             print_log ( config, "LOC start date: %s REM start date: %s" % ( loc_dat_beg, rem_dat_beg ) )
             print_log ( config, "Error: start time for remote and local slant path delay data in %s are not the same" % loc_bspd_dir )
             print_log ( config, "Error: You need remove you slant path delay in directory %s and start again" % loc_bspd_dir )
             return ( 1 )

        files_list=[]
#
# ----- Cycle over stations in the summary list 
#
# ----- IIIII_yyyymmdd_hhmmss__YYYYMMDD_HHMMSS.dat
#
        tim_loc_dat_end = datetime.strptime ( loc_dat_end[0:19], "%Y.%m.%d-%H:%M:%S" )
        tim_rem_dat_end = datetime.strptime ( rem_dat_end[0:19], "%Y.%m.%d-%H:%M:%S" )
        tim_new_dat_beg = tim_loc_dat_end + timedelta(seconds=rem_tim_stp)
        update_line = "%s__%s" % ( tim_new_dat_beg.strftime("%Y%m%d_%H%M%S"), \
                                       tim_rem_dat_end.strftime("%Y%m%d_%H%M%S")  )
        if ( ivrb >= 3 ):
             print ( "vtd_apriroi_update: spd update_line= ", update_line )

        for line in loc_sum_buf:
            if ( len(line.split()) < 10 ): continue
            if ( line.split()[0] == "Station_name:" ):
                 sta_nam = line.split()[1].lower()
#
# -------------- Build file names
#
                 old_spd_file = config.spd_bin_dir + "/" + spd_dir + "/" + \
                                loc_prefix + ("%-8s" % sta_nam).replace(" ","_") + ".bspd"
                 loc_spd_file = spd_sandbox_dir + "/" + \
                                loc_prefix + ("%-8s" % sta_nam).replace(" ","_") + ".bspd"
                 rem_spd_file = config.spd_url + "/" + spd_dir + "/" + \
                                rem_prefix + ("%-8s" % sta_nam).replace(" ","_") + ".bspd"
                 extn_file = loc_spd_file + "_" + update_line + ".dat"

                 if ( ivrb >= 3 ):
                      print ( "old_spd_file= ", old_spd_file )
                      print ( "loc_spd_file= ", loc_spd_file )
                      print ( "rem_spd_file= ", rem_spd_file )
                      print ( "extn_file= ", extn_file )

#
# -------------- Copy the sland path delay file in the sandbox direxctory
#
                 comstr = "cp -p " + old_spd_file + " " + loc_spd_file 
                 ( ret, out ) = exe ( config, comstr )
                 if ( ret != 0 ):
                      print_log ( config, "Failure in copying old local file with path delays with command %s" % \
                                  comstr )
                      for line in out:
                          print_log_verb ( config, line )
                      return ( 1 )        

#
# --------------- Offset of the section that has to be downloaded from the
# --------------- remote sereve
#
                 rem_offset = rem_del_off + rem_del_rec_len*loc_num_epochs
#
# -------------- Download the trailer of the remote file
#
                 comstr = curl_exe + " " + \
                          "--silent " + \
                          "--range %d-" % rem_offset + " " + \
                          "--output " + extn_file + " " + \
                          rem_spd_file

                 ( ret, out ) = exe ( config, comstr )
                 if ( ret != 0 ):
                      print_log ( config, "Failure in downloading the trailer of the " + \
                                          "remote file " + rem_spd_file )
                      for line in out:
                          print_log_verb ( config, line )
                      return ( 1 )        

                 comstr = "bspd_util extend_in " + loc_spd_file + " " + extn_file 
                 ( ret, out ) = exe ( config, comstr )
                 if ( ret != 0 ):
                      print_log ( config, "Failure in expanding the local slant path delay file with command %s" % \
                                  comstr )
                      for line in out:
                          print_log_verb ( config, line )
                      return ( 1 )        
                 os.unlink ( extn_file )

#
# ----- Generate the summary file for slant path delays in binary format
# ----- the in main directory
#
        comstr = "spd_3d_toser /tmp/ " + spd_sandbox_dir + "/" + loc_prefix + " summary -v 1"
        ( ret, out ) = exe ( config, comstr )
        if ( ret != 0 ):
             print_log ( config, "Failure in generation of the summary of file with path delays with command %s" % \
                         comstr )
             for line in out:
                 printlog_verb ( config, line )
             return ( 1 )        

#
# ----- Prepared the read lock to the directory with slant path delay in binary format
#
        if ( pyvers >= "0312000" ):
             tim_now = datetime.now().replace(tzinfo=None)
        else:
             tim_now = datetime.now()

        lock_contents = "Spd_update started on %s Expires on %s local time" % \
           (   tim_now.strftime("%Y%m%d_%H%M%S"), \
             ( tim_now + timedelta(seconds=spd_lock_time) ).strftime("%Y%m%d_%H%M%S") )

#
# ----- Write read lock to the directory with slant path delay in binary format
#
        lock_file = config.spd_bin_dir + "/" + spd_dir + "/spd_read_lock"
        lock_file_tmp = lock_file + "__%08d" % os.getpid()
        f = open ( lock_file_tmp, "w" )
        print ( lock_contents, file=f )
        f.close()
        os.rename ( lock_file_tmp, lock_file )
#
# ----- Wait for spd_wait_time in order to provide time for process that
# ----- reads SPD to finish
#
        time.sleep ( spd_wait_time )

        for line in loc_sum_buf:
            if ( len(line.split()) < 10 ): continue
            if ( line.split()[0] == "Station_name:" ):
                 sta_nam = line.split()[1].lower()
                 old_spd_file = config.spd_bin_dir + "/" + spd_dir + "/" + \
                                loc_prefix + ("%-8s" % sta_nam).replace(" ","_") + ".bspd"
                 loc_spd_file = spd_sandbox_dir + "/" + \
                                loc_prefix + ("%-8s" % sta_nam).replace(" ","_") + ".bspd"

#
# -------------- Move the slant path delay file in the sandbox direxctory
#
                 comstr = "mv " + loc_spd_file + " " + old_spd_file 
                 ( ret, out ) = exe ( config, comstr )
                 if ( ret != 0 ):
                      print_log ( config, "Failure in moving local file with path delays with command %s" % \
                                  comstr )
                      for line in out:
                          print_log_verb ( config, line )
                          return ( 1 )        

#
# ----- Move the summary fule
#
        comstr = "mv " + spd_sandbox_dir + "/bspd_summary.txt" + " " + \
                 config.spd_bin_dir + "/" + spd_dir + "/" 
                  
        ( ret, out ) = exe ( config, comstr )
        if ( ret != 0 ):
             print_log ( config, "Failure in moving the sumamry file with command %s" % \
                         comstr )
             for line in out:
                 print_log_verb ( config, line )
                 return ( 1 )        
#
# ----- Remove lock file
#
        if ( os.path.isfile ( lock_file ) ):
             os.unlink ( lock_file )
        print_log ( config, "SPD files in directory %s have been updated" % spd_dir )
    return ( 0 )

#
# ------------------------------------------------------------------------
#
def check_and_fix_bspd ( config, ivrb ):
#   """
#   Routine check_and_fix_bspd checks th directory with slant path
#   delay in binary format, and truncates it if it finds it not
#   damaged
#   """
    fl_error = False
    for spd_dir in spd_dirs:            
#
# ----- Run the check utilit
#
        comstr = "bspd_util check " + config.spd_bin_dir + "/" + spd_dir + "/"
        if ( not os.path.isdir(config.spd_bin_dir + "/" + spd_dir + "/") ): continue
        if ( not os.path.isfile(config.spd_bin_dir + "/" + spd_dir + "/bspd_summary.txt") ): continue
        ( ret, out ) = exe ( config, comstr )
        if ( ret != 0 ):
#
# ---------- An error was detected. Print the error messasge
#
             for line in out:
                 print ( line )

             if ( "Status: Mismatch in the end date" in out[0] ):
#
# --------------- Try to fix the end date mismatch damage
#
                  new_date_end = out[0].split()[16]
                  num_epc_tru  = int(out[0].split()[7])

                  comstr = "bspd_util truncate " + \
                           config.spd_bin_dir + "/" + spd_dir + "/" + " " + \
                           new_date_end
                  ( ret, out ) = exe ( config, comstr )
                  if ( ret == 0 ):
                       print ( "Fixed damaged files in %s by truncating %d epochs with setting the last date to %s" % \
                               ( config.spd_bin_dir + "/" + spd_dir + "/", num_epc_tru, new_date_end ) )
                  else:
                       for line in out:
                           print ( line )
                       print ( "Failed in an attemtp to fix damaged files in %s" % \
                                ( config.spd_bin_dir + "/" + spd_dir + "/" ) )
                       print ( "Failed command: %s" % comstr )
                       fl_error = True
             else:
                  print ( " " ) 
                  print ( "Do not know how to fix damaged files in %s" % \
                             ( config.spd_bin_dir + "/" + spd_dir + "/" ) )
                  print ( "You can try to remove all the files in that directory and rerun the apriori update" )
                  fl_error = True


    if ( fl_error ): 
         return ( 1, "" )
    else:
         return ( 0, "" )
#
# ------------------------------------------------------------------------
#
def vau_download_hps ( config ):
#"""
#    Downnload harmonic variations in site positions due to mass loading
#"""

    if ( not os.path.isdir ( config.load_hps_dir ) ):
         try:
             oldmask = os.umask(0) ; os.makedirs ( config.load_hps_dir, mode=0o775 ) ; oldmask=os.umask(oldmask)
         except:
             print_log ( config, "Failure in creating directory " + config.load_hps_dir )
             return ( 1 )        

    for hps_file in hps_list:
        com_str = 'cd  ' + config.load_hps_dir + '; ' + \
                  'wget ' + \
                  '-q '   + \
                  '-c '   + \
                  config.hps_url + '/' + hps_file

        ( ret, out ) = exe ( config, com_str )
        if ( ret != 0 ):
             print_log      ( config, "Failure in downloading a file with" + \
                                      " harmonic variations caused by mass loading" )
             for line in out:
                 print_log_verb ( config, line ) 

             return ( 1 )        

    return ( 0 )        
#
# ------------------------------------------------------------------------
#
def vau_download_bds ( config ):
#"""
#    Downnload time series of site positions due to mass loading
#"""

    if ( os.path.isdir ( config.load_bds_dir ) ):
         try:
              shutil.rmtree ( config.load_bds_dir )
         except BaseException as e:
              print_log ( config, "Failure in removing directory " + config.load_bds_dir + \
                          " -- " + str(e) )
              exit ( 1 )
    vau_bds ( config )

#
# ------------------------------------------------------------------------
#
def vau_bds ( config ):
    """
    Updates time series of site position variations caused by loadings
    """

    if ( not os.path.isdir ( config.load_bds_dir ) ):
         try:
             oldmask = os.umask(0) ; os.makedirs ( config.load_bds_dir, mode=0o775 ) ; oldmask=os.umask(oldmask)
         except:
             print_log ( config, "Failure in creating directory " + config.load_bds_dir )
             exit ( 1 )

    for bds_dir in bds_dirs:            
        if ( not bds_dir in config.load_model_list ): continue
#
# ----- Check for the remote lock and wait if it is set
#
        check_and_wait_remote_lock ( config, config.spd_url + "/" + bds_dir + "/lock", \
                                     "/tmp/bds_lock__" + "%08d" % os.getpid(), \
                                     lock_check_tries, lock_check_time )

        bds_lock_io_file    = config.load_bds_dir + "/" + bds_dir + "/" + vtd__io_lock_name
        bds_lock_read_file  = config.load_bds_dir + "/" + bds_dir + "/" + vtd__read_lock_name
        bds_lock_write_file = config.load_bds_dir + "/" + bds_dir + "/" + vtd__write_lock_name

#
# ----- Cycle over BDS directories to check whether we have to download all the files
#
        bds_summary_url = config.bds_url + "/" + bds_dir + "/bds_summary.txt"       
        loc_bds_dir = config.load_bds_dir + "/" + bds_dir 
        if ( not os.path.isdir ( loc_bds_dir ) ):
#
# ---------- Directory does not exist? Create it!
#
             try:
                 os.makedirs ( loc_bds_dir, mode=0o775 )        
             except:
                 print_log ( config, "Failure in creating directory " + loc_bin_dir )
                 exit ( 1 )
#
# ----- Check whether the summary file exists in the local directory
#
        loc_bds_summary_file = loc_bds_dir + "/bds_summary.txt"
        if ( not os.path.isfile ( loc_bds_summary_file ) ):
#
# ---------- Does not? Let us download first the BDS summary file and then
# ---------- all the BDS files for a given model from the remote server
#
             bds_url = config.bds_url + "/" + bds_dir
#
# ---------- Download the BDS summary file
#
             comstr = 'wget ' + \
                 '-O ' + loc_bds_dir + "/bds_summary.txt" + "__%08d" % os.getpid() + " " + \
                 '-c '   + \
                 '-N '   + \
                 '-nH '  + \
                 '--timeout=30 '  + \
                 '--tries=64 '    + \
                 '--retry-connrefused ' + \
                 '--cut-dirs=8 '  + \
                 '-e robots=off ' + \
                 '-X robots.txt ' + \
                 bds_url + "/bds_summary.txt"
             ( ret, out ) = exe ( config, comstr )
             if ( ret != 0 ):
                  print_log ( config, "Failure in downloading bds summary file from %s" % 
                              ( bds_url + "/bds_summary.txt" ) )
                  for line in out:
                      print_log_verb ( config, line )
                  exit ( 1 )        

             bds_url = config.bds_url + "/" + bds_dir + "/"
             comstr = 'cd  ' + loc_bds_dir + '; ' + \
                 'wget ' + \
                 '-r '   + \
                 '-R .txt ' + \
                 '-l 1 '   + \
                 '--no-parent ' + \
                 '-c '   + \
                 '-N '   + \
                 '-nH '  + \
                 '--timeout=30 ' \
                 '--tries=64 ' \
                 '--retry-connrefused ' \
                 '--cut-dirs=8 '  + \
                 '-e robots=off ' + \
                 '-X robots.txt ' + \
                 '-R "*index*" '  + \
                 '-R "*.gif" '    + \
                 '-R "*.png" '    + \
                 bds_url
             ( ret, out ) = exe ( config, comstr )
             if ( ret != 0 ):
                  print_log      ( config, "Failure in downloading bds from %s" % bds_url )
                  for line in out:
                      print_log_verb ( config, line )
                  exit ( 1 )        

#
# ---------- Rename the bds summary file
#
             os.rename ( loc_bds_dir + "/bds_summary.txt" + "__%08d" % os.getpid(), \
                         loc_bds_dir + "/bds_summary.txt" )

             print_log ( config, "Downloaded all binary bds files for " + bds_dir )

#
    for bds_dir in bds_dirs:
        if ( not bds_dir in config.load_model_list ): continue
        loc_bds_dir = config.load_bds_dir + "/" + bds_dir 
        bds_summary_url = config.bds_url + "/" + bds_dir + "/bds_summary.txt"       
        loc_bds_summary_file = loc_bds_dir + "/bds_summary.txt"
        tmp_bds_summary_file = loc_bds_summary_file + "_" + "%08d" % os.getpid()

#
# ----- Read the remote summary file and put it in the temporary file
#
        comstr = 'cd  ' + loc_bds_dir + '; ' + \
                 'wget ' + \
                 '--tries=64 ' + \
                 '--retry-connrefused ' + \
                 '-O ' + tmp_bds_summary_file + " " + \
                 config.bds_url + "/" + bds_dir + "/bds_summary.txt"

        ( ret, out ) = exe ( config, comstr )
        if ( ret != 0 ):
             print_log  ( config, "Failure in downloading bds_summary.txt file %s" % \
                          config.bds_url + "/" + bds_dir + "/bds_summary.txt" )
             for line in out:
                 print_log_verb ( config, line )

             exit ( 1 )        

#
# ------ ... and split it into lines
# 
        with open(tmp_bds_summary_file,encoding="latin") as f:
             rem_sum_buf = f.read().splitlines()
        f.close ( )

#
# ------ ... and check its label
#
        if ( rem_sum_buf[0] != bds__sum_label ):
             print_log ( config, "Trap of internal control: bds summary file %s " + \
                             "does not have supported format label" % + \
                              tmp_bds_summary_file )
#
# ----- Read the local file with summary
#
        with open(loc_bds_summary_file ,encoding="latin") as f:
             loc_sum_buf = f.read().splitlines()
        f.close ( )

        if ( loc_sum_buf[0] != bds__sum_label ):
             print_log ( config, "Trap of internal control: bds summary file %s " + \
                                 "does not have supported format label" % + \
                                  loc_bds_summary_file )

        if ( len(loc_sum_buf) != len(rem_sum_buf) ):
#
# ---------- A special return code to ask the calling program to invoke this
# ---------- routine once again if the file was not read to the end
#
             for path, dirs, files in os.walk(config.load_bds_dir + "/" + bds_dir):
                 for fil in files:
                     if ( ".bds" in fil        or \
                          "bds_summary" in fil or \
                          "#" in fil           or \
                          "~" in fil ):
#
                          finam = path + "/" + fil
                          try:
                              os.unlink ( finam )
                          except BaseException as e:
                              print_log ( config, "A trap of internal control: cannot " + \
                                                  "remove file %s" % ( finam, str(e) ) )
                              exit ( 1 )
                     print_log_verb ( config, "Remove file " + finam )
             return 1

#
# ----- Extract important field from the remote summary file.
#
        for line in rem_sum_buf:
            if ( line.split()[0] == "MIN_EPOCH:" ):
                 rem_mjd_beg = int(line.split()[1])
                 rem_tai_beg = float(line.split()[2])
            if ( line.split()[0] == "MAX_EPOCH:" ):
                 rem_mjd_end = int(line.split()[1])
                 rem_tai_end = float(line.split()[2])
                 rem_dat_end = line.split()[3]
            if ( line.split()[0] == "L_EPC:" ):
                 rem_num_epochs = int(line.split()[1])
            if ( line.split()[0] == "L_STA:" ):
                 rem_num_sta = int(line.split()[1])
            if ( line.split()[0] == "L_DSP:" ):
                 rem_num_dsp = int(line.split()[1])
            if ( line.split()[0] == "SMP_INTRV:" ):
                 rem_tim_stp = float(line.split()[1])

        if ( config.verb >= 2 ):
             print ( "loc_bds_summmary_file= ", loc_bds_summary_file )


        if ( loc_sum_buf[0] != bds__sum_label ):
             print_log ( config, "Trap of internal control: bds summary file %s " + \
                                 "does not have supported format label" % + \
                                  tmp_bds_summary_file )
#
# ----- Extract important field from the local summary file.
#
        for line in loc_sum_buf:
            if ( line.split()[0] == "MIN_EPOCH:" ):
                 loc_mjd_beg = int(line.split()[1])
                 loc_tai_beg = float(line.split()[2])
            if ( line.split()[0] == "MAX_EPOCH:" ):
                 loc_mjd_end = int(line.split()[1])
                 loc_tai_end = float(line.split()[2])
                 loc_dat_end = line.split()[3]
            if ( line.split()[0] == "L_EPC:" ):
                 loc_num_epochs = int(line.split()[1])
            if ( line.split()[0] == "L_STA:" ):
                 loc_num_sta = int(line.split()[1])
            if ( line.split()[0] == "L_DSP:" ):
                 loc_num_dsp = int(line.split()[1])
            if ( line.split()[0] == "SMP_INTRV:" ):
                 loc_tim_stp = float(line.split()[1])

#
# ----- Compare field of local and remote summary files
#
        if ( loc_mjd_beg    == rem_mjd_beg    and \
             loc_tai_beg    == rem_tai_beg    and \
             loc_mjd_end    == rem_mjd_end    and \
             loc_tai_end    == rem_tai_end    and \
             loc_num_epochs == rem_num_epochs and \
             loc_num_sta    == rem_num_sta    and \
             loc_tim_stp    == rem_tim_stp        ):
             print_log ( config, bds_dir + " loading was up to date. Last epoch: " + \
                         rem_dat_end )
             continue

        if ( config.verb >= 2 ):
             print ( "vau_bds REM beg: ", rem_mjd_beg, rem_tai_beg, \
                     " end: ", rem_mjd_end, rem_tai_end, \
                     " num_epc: ", rem_num_epochs, " rem_num_sta: ", rem_num_sta,  \
                     " rem_tim_stp: ", rem_tim_stp )

             print ( "vau_bds LOC beg: ", loc_mjd_beg, loc_tai_beg, \
                     " end: ", loc_mjd_end, loc_tai_end, \
                     " num_epc: ", loc_num_epochs, " loc_num_sta: ", loc_num_sta,  \
                     " loc_tim_stp: ", loc_tim_stp, " loc_bds_dir: ", loc_bds_dir )

        files_list=[]
#
# ----- Check for the remote lock and wait if it is set
#
        check_and_wait_remote_lock ( config, config.spd_url + "/" + bds_dir + "/lock", \
                                     "/tmp/bds_lock__" + "%08d" % os.getpid(), \
                                     lock_check_tries, lock_check_time )

#
# ----- Cycle over stations in the summary list 
#
        for line in loc_sum_buf:
            if ( len(line.split()) < 10 ): continue
            if ( line.split()[0] == "STA:" ):
                 sta_nam = line.split()[2]
#
# -------------- Build file names
#
                 loc_bds_file = config.load_bds_dir + "/" + bds_dir + "/" + sta_nam + ".bds"
                 tmp_bds_file = loc_bds_file + "_%08d" % os.getpid()
                 rem_bds_file = config.bds_url + "/" + bds_dir + "/" + sta_nam + ".bds"
                 tmp_header_bds_file  = loc_bds_file + "_rem_header_"  + "%08d" % os.getpid()
                 tmp_trailer_bds_file = loc_bds_file + "_rem_trailer_" + "%08d" % os.getpid()

                 stat_loc_bds = os.stat ( loc_bds_file )
                 if ( config.verb >= 2 ): 
                      print ( "file: ", loc_bds_file, " size= ", stat_loc_bds.st_size )

#
# -------------- Read two sections of the local BDS file: the header and the body
#
                 f=open(loc_bds_file,"rb")
                 loc_hea_rec = f.read ( bds_header_len )    
                 loc_dat_rec = f.read ( stat_loc_bds.st_size - bds_header_len )    
                 f.close()
            
#
# -------------- Download the header of the remote file
#
                 comstr = curl_exe + " " + \
                          "--silent " + \
                          "--range 0-%d" % (bds_header_len - 1) + " " + \
                          "--output " + tmp_header_bds_file + " " + \
                          rem_bds_file

                 ( ret, out ) = exe ( config, comstr )
                 if ( ret != 0 ):
                      print_log ( config, "Failure in downloading header " + \
                                          "of the remote file " + rem_bds_file )
                      for line in out:
                          print_log_verb ( config, line )

                      exit ( 1 )        

#
# -------------- Read the header of the remote file
#
                 f=open(tmp_header_bds_file,"rb")
                 tmp_hea_rec = f.read ( bds_header_len )    
                 f.close

                 if ( tmp_hea_rec[0:trial_header_len] == failed_header ):
                      print_log ( config, "A trap of internal control: remote " + \
                                          "file %s does not exist" % rem_bds_file )
                      exit ( 1 )
                 try:
                      os.unlink( tmp_header_bds_file )
                 except BaseException as e:
                      print_log ( config, "A trap of internal control: cannot " + \
                                          "remove file %s" % \
                                          ( tmp_header_bds_file, str(e) ) )
                      exit ( 1 )
             
                 loc_fil_len = bds_header_len + bds_datarec_len*loc_num_epochs
                 rem_fil_len = bds_header_len + bds_datarec_len*rem_num_epochs
#
# -------------- Download the trailer of the remote file
#
                 comstr = curl_exe + " " + \
                          "--silent " + \
                          "--range %d-%d" % (loc_fil_len, rem_fil_len) + " " + \
                          "--output " + tmp_trailer_bds_file + " " + \
                          rem_bds_file

                 ( ret, out ) = exe ( config, comstr )
                 if ( ret != 0 ):
                      print_log ( config, "Failure in downloading the trailer of the " + \
                                          "remote file " + rem_bds_file )
                      for line in out:
                          print_log_verb ( config, line )

                      exit ( 1 )        
#
# -------------- Read the trailer of the remote file: a portion at the end of the
# -------------- file that the local file does not have
#
                 f=open(tmp_trailer_bds_file,"rb")
                 tmp_trailer = f.read ()    
                 f.close

                 if ( len(tmp_trailer) < trial_header_len ): 
                      tmp_trailer = b'aaaaaaaaaaaaaaaaaaaa'
                 if ( tmp_trailer[0:trial_header_len] == failed_header ):
                      print ( "tmp_trailer_bds_file = ", tmp_trailer_bds_file )
                      print ( "loc_bds_file = ", loc_bds_file )
                      print_log ( config, "A trap of internal control: remote " + \
                                          "file %s does not exist or damaged" % rem_bds_file )
                      print_log ( config, "Failed command: " + comstr )
                      exit ( 1 )

                 try:
                      os.unlink( tmp_trailer_bds_file )
                 except BaseException as e:
                      print_log ( config, "A trap of internal control: cannot " + \
                                          "remove file %s -- %e" % \
                                          ( tmp_trailer_bds_file, str(e) ) )
                      exit ( 1 )

#
# -------------- Write the temporary output file. It consists of theree parts:
# -------------- header                     (taken from the remote file)
# -------------- body that existed          (taken from the local file)
# -------------- trailed that did not exist (taken from the remote file)
#
                 f=open(tmp_bds_file,"wb")
                 f.write(tmp_hea_rec)
                 f.write(loc_dat_rec)
                 f.write(tmp_trailer)
                 f.close()

#
# -------------- Add a tulip of local and remote files
#
                 files_list.append ( ( loc_bds_file, tmp_bds_file ) )

#
# ----- Set readlock to prevent other programs from reading files that
# ----- are beging updated
#
        (fd_rd, fd_wr) = vau_set_readlock ( bds_lock_io_file, \
                                            bds_lock_read_file, bds_lock_write_file )
        if ( fd_rd == None ):
             print_log ( config, "Cannot create read lock on the %s directory" % \
                                  loc_bds_dir )
             exit ( 1 )
#
# ----- Overwrite the BDS old files by remainig temporary files to the old one
#
        for files_tulip in files_list:
            loc_bds_file = files_tulip[0]
            tmp_bds_file = files_tulip[1]
            try:
                os.rename ( tmp_bds_file, loc_bds_file )
            except BaseException as e:
                print_log ( config, "Cannot move bds file from %s to %s -- %s" % \
                            ( tmp_bds_file, loc_bds_file, str(e) ) )
                vau_lift_readlock ( fd_rd, fd_wr )
                exit ( 1 )
#
# ----- Overwrite the old summary file by remaning the temporary summary file
#
        try:
            os.rename ( tmp_bds_summary_file, loc_bds_summary_file )
        except BaseException as e:
            print_log ( config, "Cannot move bds summary file from %s to %s -- %s" % \
                        ( tmp_bds_summary_file, loc_bds_summary_file, str(e) ) )
#
# ----- Lift readlock
#
        vau_lift_readlock ( fd_rd, fd_wr )
        print_log ( config, "Updated " + bds_dir + " loading. Last epoch: " + \
                             rem_dat_end )
    return 3
#
# ------------------------------------------------------------------------
#
def vau_bme ( config, loa_typ ):
    if ( loa_typ == "atm" ):
         outdir = config.load_bds_dir  + "/atm/merra2_geosfpit"
         comstr = bindisp_merge_exe   +                    " " + \
                  config.load_bds_dir  + "/atm/merra2/"   + " " + \
                  config.load_bds_dir  + "/atm/geosfpit/" + " " + \
                  outdir
    elif ( loa_typ == "lws" ):
         outdir = config.load_bds_dir  + "/lws/merra2_geosfpit"
         comstr = bindisp_merge_exe   +                    " " + \
                  config.load_bds_dir  + "/lws/merra2/"   + " " + \
                  config.load_bds_dir  + "/lws/geosfpit/" + " " + \
                  outdir

    if ( not os.path.isdir ( outdir ) ):
#
# ------ Output directory does not exist? Create it!
#
         try:
               os.makedirs ( outdir, mode=0o775 )        
         except:
               print_log ( config, "Failure in creating output directory " + outdir )
               exit ( 1 )

    ( ret, out ) = exe ( config, comstr )
    if ( ret != 0 ):
         print_log ( config, "Failure in an attempt to merge binary loading " + \
                             "time series with command %s" % comstr )
         for line in out:
             print_log_verb ( config, line )
     
         exit ( 1 )        
    return 0

#
# ------------------------------------------------------------------------
#
def vau_set_readlock ( lock_io_file, lock_read_file, lock_write_file ):
    """
    vau_set_readlock waits for clearing bds write lock and sets read lock
    """

    file_mode_open = stat.S_IREAD + stat.S_IWRITE + stat.S_IRGRP + stat.S_IWGRP + \
                     stat.S_IROTH + stat.S_IWOTH  + stat.S_ISGID

#
# --- Exclusively create, or if exists, open the io lock file
#
    oldmask = os.umask(0) 
    try:
        fd_io = os.open ( lock_io_file, os.O_CREAT + os.O_EXCL, file_mode_open )
    except FileExistsError:
        fd_io = os.open ( lock_io_file, os.O_RDONLY, file_mode_open )
    except BaseException as e:
        print ( "vau_set_readlock Error %s in an attempt to create io lock file %s" % \
                ( str(e), lock_io_file ) )
        return  ( None, None )
    oldmask=os.umask(oldmask)

#
# --- Set exlusive lock in the io lock file. The operation will be interrupted 
# --- with timer, if it takes longer than vau_lock_timeout seconds
#
    with timeout(vau_lock_timeout):
         try:
             fcntl.flock ( fd_io, fcntl.LOCK_EX )
         except IOError as e:
             pass
         except BaseException as e:
             print ( "vau_set_readlock Error %s in an attempt to set exclusive io lock file %s" % \
                      ( str(e), lock_io_file ) )
             os.close ( fd_io )
             return   ( None, None )

#
# --- Exclusively create, or if exists, open the write lock file
#
    oldmask = os.umask(0) 
    try:
        fd_wr = os.open ( lock_write_file, os.O_CREAT + os.O_EXCL, file_mode_open )
    except FileExistsError:
        fd_wr = os.open ( lock_write_file, os.O_RDONLY, file_mode_open )
    except BaseException as e:
        print ( "vau_set_readlock Error %s in an attempt to create read lock file %s" % \
                ( str(e), lock_write_file ) )
        fcntl.flock ( fd_io, fcntl.LOCK_UN )
        os.close ( fd_io )
        return   ( None, None )
    oldmask=os.umask(oldmask)

#
# --- Set shared non-blocking lock in the write lock file
#
    try:
         fcntl.flock ( fd_wr, fcntl.LOCK_SH + fcntl.LOCK_NB )
    except IOError as e:
         pass

#
# --- Exclusively create, or if exists, open the read lock file
#
    oldmask = os.umask(0) 
    try:
        fd_rd = os.open ( lock_read_file, os.O_CREAT + os.O_EXCL, file_mode_open )
    except FileExistsError:
        fd_rd = os.open ( lock_read_file, os.O_RDONLY, file_mode_open )
    except BaseException as e:
        print ( "vau_set_readlock Error %s in an attempt to create read lock file %s" % \
                ( str(e), lock_read_file ) )
        fcntl.flock ( fd_wr, fcntl.LOCK_UN )
        fcntl.flock ( fd_io, fcntl.LOCK_UN )
        os.close ( fd_wr )
        os.close ( fd_io )
        return  ( None, None )
    oldmask=os.umask(oldmask)

#
# --- Set exlusive lock in the read lock file. The operation will be interrupted with timer
# --- if it takes longer than vau_lock_timeout
#
    with timeout(vau_lock_timeout):
         try:
             fcntl.flock ( fd_rd, fcntl.LOCK_EX )
         except InterruptedError:
             pass
         except BaseException as e:
             print ( "vau_set_readlock exception %s in an attempt to lock file %s: ", 
                          ( str(e), lock_io_file ) )
             fcntl.flock ( fd_wr, fcntl.LOCK_UN )
             fcntl.flock ( fd_io, fcntl.LOCK_UN )
             os.close ( fd_wr )
             os.close ( fd_io )
             os.close ( fd_rd )
             return   ( None, None )

    fcntl.flock ( fd_io, fcntl.LOCK_UN )
    os.close ( fd_io )

    return ( fd_rd, fd_wr )
#
# ------------------------------------------------------------------------
#
def vau_check_missing ( config ):
    """
    vau_check_missing checks for missing a priori and copies them 
                      from VTD_DATA if it cannot find them.
    """
    num_updated = 0

#
# --- Check for missing files
#
    if ( not  os.path.isfile(config.sta_pos) ):
         id = config.sta_pos.rfind("/") + 1
         vtd_sta_pos = vtd_local.vtd_data + "/" + config.sta_pos[id:]
         try:
              shutil.copyfile ( vtd_sta_pos, config.sta_pos )
         except BaseException as e:
              pass
         num_updated = num_updated + 1

    if ( not  os.path.isfile(config.sta_vel) ):
         id = config.sta_vel.rfind("/") + 1
         vtd_sta_vel = vtd_local.vtd_data + "/" + config.sta_vel[id:]
         try:
              shutil.copyfile ( vtd_sta_vel, config.sta_vel )
         except BaseException as e:
              pass
         num_updated = num_updated + 1

    if ( not  os.path.isfile(config.sta_desc) ):
         id = config.sta_desc.rfind("/") + 1
         vtd_sta_desc = vtd_local.vtd_data + "/" + config.sta_desc[id:]
         try:
              shutil.copyfile ( vtd_sta_desc, config.sta_desc )
         except BaseException as e:
              pass
         num_updated = num_updated + 1

    if ( not  os.path.isfile(config.sta_ecc) ):
         id = config.sta_ecc.rfind("/") + 1
         vtd_sta_ecc = vtd_local.vtd_data + "/" + config.sta_ecc[id:]
         try:
              shutil.copyfile ( vtd_sta_ecc, config.sta_ecc )
         except BaseException as e:
              pass
         num_updated = num_updated + 1

    if ( not  os.path.isfile(config.sou_coo) ):
         id = config.sou_coo.rfind("/") + 1
         vtd_sou_coo = vtd_local.vtd_data + "/" + config.sou_coo[id:]
         try:
              shutil.copyfile ( vtd_sou_coo, config.sou_coo )
         except BaseException as e:
              pass
         num_updated = num_updated + 1

    if ( not  os.path.isfile(config.sou_prl) ):
         id = config.sou_prl.rfind("/") + 1
         vtd_sou_prl = vtd_local.vtd_data + "/" + config.sou_prl[id:]
         try:
              shutil.copyfile ( vtd_sou_prl, config.sou_prl )
         except BaseException as e:
              pass
         num_updated = num_updated + 1

    if ( not  os.path.isfile(config.sou_names) ):
         id = config.sou_names.rfind("/") + 1
         vtd_sou_names = vtd_local.vtd_data + "/" + config.sou_names[id:]
         try:
              shutil.copyfile ( vtd_sou_names, config.sou_names )
         except BaseException as e:
              pass
         num_updated = num_updated + 1

    if ( not  os.path.isfile(config.eph_file) ):
         id = config.eph_file.rfind("/") + 1
         vtd_eph_file = vtd_local.vtd_data + "/" + config.eph_file[id:]
         try:
              shutil.copyfile ( vtd_eph_file, config.eph_file )
         except BaseException as e:
              pass
         num_updated = num_updated + 1

    return ( num_updated )
#
# ------------------------------------------------------------------------
#
def vau_lift_readlock ( fd_rd, fd_wr ):
    """
    vau_lift_readlock lifts the locks
    """
    fcntl.flock ( fd_rd, fcntl.LOCK_UN )
    fcntl.flock ( fd_wr, fcntl.LOCK_UN )
    os.close    ( fd_rd )
    os.close    ( fd_wr )

#
# ------------------------------------------------------------------------
#
def main():
    os.environ["LANG"] = "utf-8"

    opts = optparse.OptionParser( version=vau__label )

    opts.add_option ( "-c", "--config", action="store", \
                      dest="config", \
                      metavar="NAME", \
                      help="Configuration file" )

    opts.add_option ( "-d", "--download", action="store_true", \
                      dest="download", \
                      help="Download everything" )

    opts.add_option ( "-m", "--missing", action="store_true", \
                      dest="missing", \
                      help="Update missing a priori files that are normally not updated" )

    opts.add_option ( "-i", "--info", action="store_true", \
                      dest="info", \
                      help="Do not update apriori data, but only display information about current data" )

    opts.add_option ( "-v", "--verbosity", action="store", \
                      dest="ivrb", \
                      default=0, \
                      type="int", \
                      metavar="VALUE", \
                      help="Verbosity level. 0 -- silent, 1 -- normal " + \
                           "verbosity, 2 -- debugging level of verbosity" )

    opts.add_option ( "-V", "--version", action="store_true", \
                      dest="version", \
                      default=False, \
                      metavar="NAME", \
                      help="See the version" )

#
# --- Get and parse options
#
    opts, args = opts.parse_args()
    if ( opts.version ):
         print ( vau__label )
         exit  ( 0 )
#
# --- Check option values
#
    if ( opts.config == None ):
         print ( "vtd_apriori_update.py: Configuration file is not specified. Try vtd_apriori_update.py -h to see options" )
         exit ( 1 )

    if ( not os.path.isfile(opts.config) ):
         print ( "vtd_apriori_update.py: Configuration file ", opts.config, " does not exist" )
         exit ( 1 )

    config = vau_config_class ( opts.config )

#
# --- Parse configuration file
#
    parse_vau_config ( config ) 

    if ( opts.info ):

         vau_info ( config, opts.ivrb  )
         exit ( 0 )
    if ( not os.path.isfile ( config.log_file ) ):
         try:
             f = open ( config.log_file, "w" )
             f.close()
         except:
             print ( "vtd_apriori_update.py: cannot open for writing " + \
                     "log file " + config.log_file + \
                     " specified in LOG_FILE of the configuration file ", opts.config )
             exit ( 1 )

    if ( not os.path.isfile ( config.log_verbose_file ) ):
         try:
             f = open ( config.log_verbose_file, "w" )
             f.close()
         except:
             print ( "vtd_apriori_update.py: cannot open for writing" + \
                     " log file " + config.log_verbose_file + \
                     " specified in LOG_VERBOSE_FILE of the" + \
                     " configuration file ", opts.config )
             exit ( 1 )

    print_log ( config, "@start@" )

    if ( opts.missing ):
         vau_eops  ( "eop",  config )
         print_log ( config, "EOP  file has been updated" )
#
         vau_eops  ( "eopz", config )
         print_log ( config, "EOPZ file has been updated" )
#
         ret = vau_check_missing ( config )
         print_log ( config, "Checked for missing files" )
         if ( ret == 0 ) :
              print_log ( config, "No missing files were found" )
         else:
              print_log ( config, "%d missing files were updated" % ret )
         exit ( 0 )

    if ( opts.download ):
#
# ------ Download all the apriori files instead of updating
#
         if ( config.load_update == "yes" ):
              ret = vau_download_hps ( config )
              if ( ret == 0 ):
                   print_log ( config, "Downloaded loading harmonic variations in site postions" )
              else:
                   print_log ( config, "Fauiluer in downloading loading harmonic variations in site postions" )
                   exit ( 1 )

              vau_download_bds ( config )
              print_log ( config, "Downloaded loading time series" )

         if ( config.spd_update  == "yes" ):
              vau_download_spd ( config, opts.ivrb )
              print_log ( config, "Downloaded slant path delays" )

         if ( config.iono_update == "yes" ):
              vau_download_iono  ( config )
              print_log ( config, "IONO file has been updated" )

         print_log ( config, "Finished apriori download" )

    check_and_fix_bspd ( config, opts.ivrb )

    if ( config.eop_series_update   == "yes" ):
         vau_eops ( "eop",  config )
         print_log ( config, "EOP  file has been updated" )

    if ( config.eopz_series_update  == "yes" ):
         vau_eops  ( "eopz", config )
         print_log ( config, "EOPZ file has been updated" )

    if ( opts.download ):
         exit ( 0 )

    if ( config.iono_update         == "yes" ):
         vau_iono  ( config, opts.ivrb )
         print_log ( config, "IONO file has been updated" )

    if ( config.spd_update          == "yes" ):
         ret = vau_spd ( config, opts.ivrb )
         if ( ret != 0 ): 
              print_log ( config, "Failed SPD update" )
              exit ( ret )
      
    if ( config.load_update         == "yes" ):
         for i in range(0,len(bds_dirs)+1):
             ret = vau_bds ( config )
             if ( ret != 1 ): break
         print_log ( config, "BDS loading files have been updated" )

    if ( config.merra2_geosfpit_atm == "yes" ):
         ret = vau_bme ( config, "atm" )
         if ( ret != 0 ): 
              print_log ( config, "Error in attempt to update merra2_geosfpit_atm time series" )
         else:
              print_log ( config, "Updated merra2_geosfpit_atm time series" )
      
    if ( config.merra2_geosfpit_lws == "yes" ):
         ret = vau_bme ( config, "lws" )
         if ( ret != 0 ): 
              print_log ( config, "Error in attempt to update merra2_geosfpit_lws time series" )
         else:
              print_log ( config, "Updated merra2_geosfpit_lws time series" )
    print_log ( config, "Finished apriori update" )

if __name__ == "__main__":
    try:
        pyvers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( pyvers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        main()
    except KeyboardInterrupt:
        print ( "pf.py: Interrupted" )
        exit ( 1 )
