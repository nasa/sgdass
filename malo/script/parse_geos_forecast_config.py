import sys, os, shutil, time, subprocess

fmt__label      = "# GEOS_FORECAST_CONFIG file. Format of 2015.11.30"
config__num_par = 24 # The number of configuration parameters

class config_class:
   def __init__ ( self, filename ):
       self.look_back_hours     = None
       self.look_ahead_hours    = None
       self.time_step_mod_hours = None
       self.time_step_fcs_hours = None
       self.url_template        = None
       self.geosfp_heb_dir      = None
       self.geosfcs_heb_dir     = None
       self.pivot_sds           = []
       self.geos_temp_dir       = None
       self.to_heb_exe          = None
       self.aam_exe             = None
       self.aam_ser_exe         = None
       self.aam_igh             = None
       self.aam_ogh             = None
       self.aam_ls_mask         = None
       self.aam_pref            = None
       self.aam_ser_file        = None
       self.compress_com        = None
       self.log_file            = None
       self.suc_file            = None
       self.fcs_file            = None
       self.err_file            = None
       self.lock_file           = None
       self.lock_timeout        = None

       self.filename            = filename
       self.log_file_handle     = None
       self.geosfp_heb_list     = []
       self.geosfcs_heb_list    = []
       self.remote_url_list     = []
       self.date_time_step      = None
#
# ===========================================================================
#

def parse_geos_forecast_config ( config ):
#"""
#    Reads the configuration file which has format KEYWORD: VALUE 
#    and puts parsed information into fields of class config
#"""
   with open ( config.filename  ) as f:
        conf_buf = f.readlines()
   f.close ( )

   if ( conf_buf[0][0:len(fmt__label)] != fmt__label[0:len(fmt__label)] ):
        Message ( "F", "Unsupported format of config file " + config.filename + \
                  "\n Format label found:   " + conf_buf[0] + \
                  "\n While expected label: " + fmt__label + "\n" )
        exit ( 1 )

   malo_script_dir = os.popen("malo_inq script").read().rstrip()
   malo_share_dir  = os.popen("malo_inq share").read().rstrip()
#   malo_bin_dir    = os.popen("malo_inq bin_static").read().rstrip()
   malo_bin_dir    = os.popen("malo_inq bin").read().rstrip()

   num_par = 0
   for line in conf_buf:
       if ( line == fmt__label ): continue
       if   ( line.split()[0]     == "#" ): continue
       if   ( line.split()[0]     == "look_ahead_hours:"  ): 
              config.look_ahead_hours = int(line.split()[1])
              num_par = num_par + 1
       elif   ( line.split()[0]     == "look_back_hours:"  ): 
              config.look_back_hours = int(line.split()[1])
              num_par = num_par + 1
       elif   ( line.split()[0]     == "time_step_mod_hours:"  ): 
              config.time_step_mod_hours = int(line.split()[1])
              num_par = num_par + 1
       elif   ( line.split()[0]     == "time_step_fcs_hours:"  ): 
              config.time_step_fcs_hours = int(line.split()[1])
              num_par = num_par + 1
       elif ( line.split()[0]      == "url_template:"  ):
              config.url_template   = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "pivot_sds:"     ):
              num_pivots = len(line.split()) - 1
              for i in range(0,num_pivots):
                  config.pivot_sds.append ( line.split()[i+1] )
              num_par = num_par + 1
       elif ( line.split()[0]      == "geos_temp_dir:" ):
              config.geos_temp_dir =   line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "geosfp_heb_dir:"  ):
              config.geosfp_heb_dir =  line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "geosfcs_heb_dir:"  ):
              config.geosfcs_heb_dir = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "to_heb_exe:"   ):
              config.to_heb_exe    =   line.split()[1].replace("@malo_bin@",malo_bin_dir)
              num_par = num_par + 1
       elif ( line.split()[0]      == "aam_exe:"   ):
              config.aam_exe       =   line.split()[1].replace("@malo_bin@",malo_bin_dir)
              num_par = num_par + 1
       elif ( line.split()[0]      == "aam_ser_exe:"   ):
              config.aam_ser_exe   =   line.split()[1].replace("@malo_bin@",malo_bin_dir)
              num_par = num_par + 1
       elif ( line.split()[0]      == "aam_igh:"  ):
              config.aam_igh       =   line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]      == "aam_ogh:"  ):
              config.aam_ogh       =   line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]      == "aam_ls_mask:"  ):
              config.aam_ls_mask   =   line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]      == "aam_pref:" ):
              config.aam_pref      =   line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]      == "aam_ser_file:" ):
              config.aam_ser_file  =   line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]      == "compress_com:"  ):
              config.compress_com  =   line.split()[1] 
              if ( config.compress_com == "blank" ): config.compress_com = ""
              num_par = num_par + 1
       elif ( line.split()[0]     == "log_file:"  ):
              config.log_file     =   line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "err_file:"  ):
              config.err_file     =   line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "suc_file:"  ):
              config.suc_file     =   line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "fcs_file:"  ):
              config.fcs_file     =   line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "lock_file:"  ):
              config.lock_file    =   line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "lock_timeout:"  ):
              config.lock_timeout =   float(line.split()[1])
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
