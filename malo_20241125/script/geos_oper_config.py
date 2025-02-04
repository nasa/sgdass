#!/usr/bin/env python
# ************************************************************************
# *                                                                      *
# *   Routine for parsing GEOS control file                              *
# *                                                                      *
# * # 12-APR-2014 geos_oper_config.py v2.12 (c) L. Petrov 15-JUN-2017 ## *
# *                                                                      *
# ************************************************************************
import sys, os, shutil, time, subprocess, datetime

from   Message     import *

fmt__label      = "# GEOS_CONFIG file. Format of 2024.11.28"
config__num_par = 75 # The number of configuration parameters

class geos_config_class:
   def __init__ ( self, filename ):
       self.filename                   = filename
       self.name                       = None
       self.username                   = None
       self.password                   = None
       self.pivot_sds                  = []
       self.begin_date                 = None
       self.look_back_days             = None
       self.epochs_per_file            = None
       self.end_date                   = None
       self.time_step                  = None
       self.url_template               = None
       self.geos_temp_dir              = None
       self.geos_heb_dir               = None
       self.geos_dir                   = None
       self.spr_dir                    = None
       self.to_heb_exe                 = None
       self.loading_exe                = None
       self.int_loading_exe            = None
       self.loading_integral_exe       = None
       self.malo_exe                   = None
       self.malo_load_model_exe        = None
       self.gen_spr_exe                = None
       self.gen_bdsp_exe               = None
       self.sc_apply_exe               = None
       self.loading_heb_to_nc_exe      = None
       self.loading_heb_to_spl_heb_exe = None
       self.loading_spl_heb_to_sta_exe = None
       self.compress_com               = None
       self.gmao_gh                    = None
       self.malo_elev                  = None
       self.spr_pref                   = None
       self.load_bdsp_dir              = None
       self.load_grid_wc               = None
       self.load_list_wc               = None
       self.load_conf                  = None
       self.load_grid_dir              = None
       self.load_spl_dir               = None
       self.load_list_dir              = None
       self.load_har_grid_dir          = None
       self.load_har_list_dir          = None
       self.load_har_spl_dir           = None
       self.load_int_dir               = None
       self.load_d1_conf               = None
       self.load_d1_grid_dir           = None
       self.load_d1_spl_dir            = None
       self.load_d1_list_dir           = None
       self.load_d1_har_grid_dir       = None
       self.load_d1_har_list_dir       = None
       self.load_d1_har_spl_dir        = None
       self.load_d1_int_dir            = None
       self.vgep_dir                   = None
       self.load_grid_pref             = None
       self.load_list_pref             = None
       self.load_spl_pref              = None
       self.load_d1_grid_pref          = None
       self.load_d1_list_pref          = None
       self.load_d1_spl_pref           = None
       self.vgep_pref                  = None
       self.vgep_wc                    = None
       self.aam_exe                    = None
       self.aam_ser_exe                = None
       self.aam_igh                    = None
       self.aam_ogh                    = None
       self.aam_ls_mask                = None
       self.aam_pref                   = None
       self.aam_ser_file               = None
       self.acp_height                 = None
       self.acp_dir                    = None
       self.st_dir                     = None
       self.stop_file                  = None
       self.log_file                   = None
       self.log_file_handle            = None
       self.lock_file                  = None
       self.err_file                   = None
       self.suc_file                   = None
       self.lock_timeout               = None

       self.date_list                  = []

def parse_geos_oper_config ( config ):
#"""
#    Reads the configuration file which has format KEYWORD: VALUE 
#    and puts parsed information into fields of class config
#"""
   with open ( config.filename  ) as f:
        conf_buf = f.readlines()
   f.close ( )

   if ( conf_buf[0][0:len(fmt__label)] == fmt__label[0:len(fmt__label)] ):
        ifmt = 0
   else:
        ifmt = -1
        Message ( "F", "Unsupported format of config file " + config.filename + \
                  "\n Format label found:   " + conf_buf[0] + \
                  "\n While expected label: " + fmt__label + "\n" )
        exit ( 1 )

   malo_script_dir = os.popen("malo_inq script").read().rstrip()
   malo_share_dir  = os.popen("malo_inq share").read().rstrip()
   malo_model_dir  = os.popen("malo_inq model").read().rstrip()
   malo_bin_dir    = os.popen("malo_inq bin_static").read().rstrip()

   num_par = 0
   config.pivot_sds = []
   for line in conf_buf:
       if ( line == fmt__label ): continue
       if   ( line[0:1] == "#" ): continue
       if ( line.split()[0]      == "username:"    ):
              config.username     = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "password:"    ):
              config.password       = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "begin_date:"    ):
              config.begin_date     = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "end_date:"      ):
              config.end_date       = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "look_back_days:" ):
              config.look_back_days = int(line.split()[1])
              num_par = num_par + 1
       elif ( line.split()[0]      == "epochs_per_file:" ):
              config.epochs_per_file = int(line.split()[1])
              num_par = num_par + 1
       elif ( line.split()[0]      == "pivot_sds:"     ):
              num_pivots = len(line.split()) - 1
              for i in range(0,num_pivots):
                  config.pivot_sds.append ( line.split()[i+1] )
              num_par = num_par + 1
       elif ( line.split()[0]      == "time_step:"     ):
              config.time_step      = float(line.split()[1])
              num_par = num_par + 1
       elif ( line.split()[0]      == "url_template:"  ):
              config.url_template   = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "geos_temp_dir:" ):
              config.geos_temp_dir  = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "geos_heb_dir:"  ):
              config.geos_heb_dir   = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "geos_dir:"      ):
              config.geos_dir       = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "spr_dir:"      ):
              config.spr_dir        = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "to_heb_exe:"   ):
              config.to_heb_exe     =  line.split()[1].replace("@malo_bin@",malo_bin_dir)
              num_par = num_par + 1
       elif ( line.split()[0]      == "gen_spr_exe:"   ):
              config.gen_spr_exe    =  line.split()[1].replace("@malo_bin@",malo_bin_dir)
              num_par = num_par + 1
       elif ( line.split()[0]      == "loading_exe:"   ):
              config.loading_exe    =  line.split()[1].replace("@malo_script@",malo_script_dir)
              num_par = num_par + 1
       elif ( line.split()[0]      == "int_loading_exe:"   ):
              config.int_loading_exe =  line.split()[1].replace("@malo_script@",malo_script_dir)
              num_par = num_par + 1
       elif ( line.split()[0]      == "loading_integral_exe:"   ):
              config.loading_integral_exe =  line.split()[1].replace("@malo_bin@",malo_bin_dir)
              num_par = num_par + 1
       elif ( line.split()[0]      == "malo_exe:"   ):
              config.malo_exe       =  line.split()[1].replace("@malo_bin@",malo_bin_dir)
              num_par = num_par + 1
       elif ( line.split()[0]      == "malo_load_model_exe:"   ):
              config.malo_load_model_exe =  line.split()[1].replace("@malo_bin@",malo_bin_dir)
              num_par = num_par + 1
       elif ( line.split()[0]      == "gen_bdsp_exe:"  ):
              config.gen_bdsp_exe   =  line.split()[1].replace("@malo_bin@",malo_bin_dir) 
              num_par = num_par + 1
       elif ( line.split()[0]      == "sc_apply_exe:"  ):
              config.sc_apply_exe   =  line.split()[1].replace("@malo_bin@",malo_bin_dir)
              num_par = num_par + 1
       elif ( line.split()[0]      == "loading_heb_to_nc_exe:"  ):
              config.loading_heb_to_nc_exe      = line.split()[1].replace("@malo_bin@",malo_bin_dir)
              num_par = num_par + 1
       elif ( line.split()[0]      == "loading_heb_to_spl_heb_exe:"  ):
              config.loading_heb_to_spl_heb_exe = line.split()[1].replace("@malo_bin@",malo_bin_dir)
              num_par = num_par + 1
       elif ( line.split()[0]      == "loading_spl_heb_to_sta_exe:"  ):
              config.loading_spl_heb_to_sta_exe = line.split()[1].replace("@malo_bin@",malo_bin_dir)
              num_par = num_par + 1
       elif ( line.split()[0]     == "aam_exe:"  ):
              config.aam_exe = line.split()[1].replace("@malo_bin@",malo_bin_dir)
              num_par = num_par + 1
       elif ( line.split()[0]     == "aam_ser_exe:"  ):
              config.aam_ser_exe = line.split()[1].replace("@malo_bin@",malo_bin_dir)
              num_par = num_par + 1
       elif ( line.split()[0]      == "gmao_gh:"   ):
              config.gmao_gh        = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "spr_pref:"  ):
              config.spr_pref       = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "malo_elev:"   ):
              config.malo_elev      = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "load_list_wc:"   ):
              config.load_list_wc  = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "load_grid_wc:"   ):
              config.load_grid_wc  = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "load_bdsp_dir:"   ):
              config.load_bdsp_dir  = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "load_bdsp_export_dir:"   ):
              config.load_bdsp_export_dir  = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "load_bdsp_sandbox_dir:"   ):
              config.load_bdsp_sandbox_dir  = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "run_bdsp_export:"   ):
              par = line.split()[1].lower()
              if ( par == "yes" ):
                   config.run_bdsp_export = True
              elif ( par == "no" or par == "none" ):
                   config.run_bdsp_export = False
              else:
                   print ( "Erro in parsing configuration file %s" + \
                           " -- option run_bdsp_export: " + \
                           " requires yes or no but %s was supplied" % \
                           ( config.filename, par ) )
                   exit ( 1 )
              num_par = num_par + 1
#
       elif ( line.split()[0]      == "load_conf:"   ):
              config.load_conf      = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "load_grid_dir:"   ):
              config.load_grid_dir  = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "load_list_dir:"   ):
              config.load_list_dir  = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "load_spl_dir:"   ):
              config.load_spl_dir   = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "load_har_grid_dir:" ):
              config.load_har_grid_dir  = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "load_har_list_dir:" ):
              config.load_har_list_dir  = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "load_har_spl_dir:"   ):
              config.load_har_spl_dir = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "load_int_dir:"   ):
              config.load_int_dir = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "load_grid_pref:"   ):
              config.load_grid_pref = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "load_list_pref:"   ):
              config.load_list_pref = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "load_spl_pref:"   ):
              config.load_spl_pref  = line.split()[1] 
              num_par = num_par + 1
#
       elif ( line.split()[0]         == "load_d1_conf:"  ):
              config.load_d1_conf      = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]         == "load_d1_grid_dir:"   ):
              config.load_d1_grid_dir  = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]         == "load_d1_list_dir:"   ):
              config.load_d1_list_dir  = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]         == "load_d1_spl_dir:"   ):
              config.load_d1_spl_dir   = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]         == "load_d1_har_grid_dir:" ):
              config.load_d1_har_grid_dir  = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]         == "load_d1_har_list_dir:" ):
              config.load_d1_har_list_dir  = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]         == "load_d1_har_spl_dir:"  ):
              config.load_d1_har_spl_dir   = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]          == "load_d1_int_dir:"   ):
              config.load_d1_int_dir    = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]          == "load_d1_grid_pref:" ):
              config.load_d1_grid_pref  = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]          == "load_d1_list_pref:" ):
              config.load_d1_list_pref  = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]          == "load_d1_spl_pref:" ):
              config.load_d1_spl_pref   = line.split()[1] 
              num_par = num_par + 1
#
       elif ( line.split()[0]          == "vgep_dir:" ):
              config.vgep_dir           = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]          == "vgep_pref:" ):
              config.vgep_pref          = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]          == "vgep_wc:" ):
              config.vgep_wc            = line.split()[1] 
              num_par = num_par + 1
#
       elif ( line.split()[0]      == "compress_com:"  ):
              config.compress_com  = line.split()[1] 
              if ( config.compress_com == "blank" ): config.compress_com = ""
              num_par = num_par + 1
       elif ( line.split()[0]     == "acp_height:" ):
              config.acp_height = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "acp_dir:" ):
              config.acp_dir = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "st_dir:"  ):
              config.st_dir = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "aam_igh:"  ):
              config.aam_igh = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "aam_ogh:"  ):
              config.aam_ogh = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "aam_ls_mask:"  ):
              config.aam_ls_mask = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "aam_pref:" ):
              config.aam_pref = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "aam_ser_file:" ):
              config.aam_ser_file = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "stop_file:"  ):
              config.stop_file     = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "log_file:"  ):
              config.log_file      = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "lock_file:"  ):
              config.lock_file     = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "lock_timeout:"  ):
              config.lock_timeout  = float(line.split()[1])
              num_par = num_par + 1
       elif ( line.split()[0]     == "step_ahead:"  ):
              config.step_ahead     = int(line.split()[1])
              num_par = num_par + 1
       elif ( line.split()[0]     == "err_file:"  ):
              config.err_file      = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "suc_file:"  ):
              config.suc_file      = line.split()[1] 
              num_par = num_par + 1
       else:
              print ( "Unrecognized keyword " + line.split()[0] + \
                      " in control file " + config.filename )
              exit ( 1 )

   config.malo_ls_mask   = None
   config.upgrid_ls_mask = None
   config.sc_file        = None
   config.loa_comm       = None
   config.loa_descr      = None

   if ( config.load_conf ):

        if ( not config.load_conf == "blank" ):
             if ( not os.path.isfile ( config.load_conf ) ):
                  config.load_conf = malo_share_dir + "/" + config.load_conf 

             if ( not os.path.isfile ( config.load_conf ) ):
                  print ( "File config.load_conf ", config.load_conf, "does not exist" )
                  exit ( 1 )

             with open ( config.load_conf ) as f:
                  conf_buf = f.readlines()
             f.close ()
     
             for line in conf_buf:
                 if ( line.split()[0]     == "#" ): continue
                 if ( line.split()[0]     == "MALO_FINAM_LS_MASK"  ):
                      config.malo_ls_mask = line.split()[2] 
                 if ( line.split()[0]     == "MALO_UPGRID_LS_MASK" ):
                      config.upgrid_ls_mask = line.split()[2] 
                 if ( line.split()[0]     == "MALO_SC_FILE"        ):
                      config.sc_file = line.split()[2] 
                 if ( line.split()[0]     == "LOA_FINAM_DESCR"        ):
                      config.loa_descr = line.split()[2] 
                 if ( line.split()[0]     == "LOA_FINAM_COMM"        ):
                      config.loa_comm  = line.split()[2] 

   if ( config.load_d1_conf ):

        if ( not config.load_d1_conf == "blank" ):
             if ( not os.path.isfile ( config.load_d1_conf ) ):
                  config.load_d1_conf = malo_share_dir + "/" + config.load_d1_conf 
     
             if ( not os.path.isfile ( config.load_d1_conf ) ):
                  print ( "File config.load_d1_conf ", config.load_d1_conf, "does not exist" )
                  exit ( 1 )
     
             with open ( config.load_d1_conf ) as f:
                  conf_buf = f.readlines()
             f.close ()

             for line in conf_buf:
                 if ( line.split()[0]     == "#" ): continue
                 if ( line.split()[0]     == "LOA_FINAM_DESCR"        ):
                      config.loa_d1_descr = line.split()[2] 
                 if ( line.split()[0]     == "LOA_FINAM_COMM"        ):
                      config.loa_d1_comm  = line.split()[2] 

   if ( not config.aam_ls_mask == "blank" ):
        if ( not os.path.isfile ( config.aam_ls_mask ) ):
             config.aam_ls_mask = malo_model_dir + "/" + config.aam_ls_mask
        if ( not os.path.isfile ( config.aam_ls_mask ) ):
             print ( "File config.aam_ls_mask ", config.aam_ls_mask, "does not exist" )
             exit ( 1 )

   if ( num_par < config__num_par ):
        print ( "Not all keywords were found in in control file " + \
                 config.filename + " -- only %d, though %d were expected" % \
                 ( num_par, config__num_par ) )
        exit ( 1 )
#
# ------------------------------------------------------------------------
#
def print_suc ( config, str ):
#"""
#   print string str into suc-file specified in the config file
#"""
    now = datetime.datetime.now() 
    
#
    suc_file_handle = open ( config.suc_file, "w" )
    print ( "get_geosfpit_oper: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str, \
            file=suc_file_handle )
    suc_file_handle.flush()
    suc_file_handle.close()
#
# ------------------------------------------------------------------------
#
def print_err ( config, str ):
#"""
#   print string str into err-file specified in the config file
#"""
    now = datetime.datetime.now() 
    
#
    err_file_handle = open ( config.err_file, "w" )
    print ( "get_geosfpit_oper: " + now.strftime("%Y.%m.%d_%H:%M:%S") + " " + str, \
            file=err_file_handle )
    err_file_handle.flush()
    err_file_handle.close()
