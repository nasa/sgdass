#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program for updaring dates files related to mass loading,          *
# *   atmosperic angular momentum, and path delay services.              *
# *                                                                      *
# * ### 26-FEB-2016  malo_service.py v4.1 (c) L. Petrov  25-MAR-2024 ### *
# *                                                                      *
# ************************************************************************
import sys, os, shutil, subprocess, time
import optparse 
import math
import datetime
from   datetime import timedelta
from   Message   import *

malo_service__label = "malo_service.py  v 4.1 of 2024.03.25"
malo__fmt__label    = "# MALO_SERVICE_CONFIG file.  Format of 2017.05.18"
aam__fmt__label     = "# AAM_SERVICE_CONFIG file.  Format of 2015.08.13"
spd__fmt__label     = "# SPD_SERVICE_CONFIG file.  Format of 2016.02.26"
stat__fmt__label    = "# STAT_SERVICE_CONFIG file.  Format of 2016.02.26"
malo__num_par       =  8 # The number of configuration parameters
aam__num_par        =  3 # The number of configuration parameters
spd__num_par        =  3 # The number of configuration parameters
stat__num_par       =  1 # The number of configuration parameters

class config_class:
   def __init__ ( self, filename ):
       self.filename        = filename
       self.action          = None
       self.name            = None
       self.begin_date      = None
       self.end_date        = None
       self.heb_dir         = None
       self.vgep_dir        = None
       self.loading_dir     = None
       self.loading_type    = None
       self.data_set        = None
       self.extension       = None
       self.aam_dir         = None
       self.aam_ser_file    = None
       self.spd_asc_dir     = None
       self.spd_dir_list    = None

   def init ( self ):
       __init__ ( self )

#
# ===========================================================================
#
def malo_loading_date ( config ):
    
    num_file = 0;
    for paths, dirs, files in os.walk(config.loading_dir):
        for k in range(0,len(files)):
            file = paths + "/" + files[k]
            if ( file.find("#") > 0 ): continue
            ie =  file.find ( config.extension )
            id =  file.find ( "/" + config.data_set )
##            if ( ie >= 13 and id > 0 ):
            if ( ie >= 13 ):
                 num_file = num_file + 1;
                 date_load = file[ie-13:ie]
                 (mode, ino, dev, nlink, uid, gid, size, atime, mtime, ctime) = \
                 os.stat( file )
                 if ( num_file == 1 ):
                      date_min_load = date_load
                      date_max_load = date_load
                      date_min_mod  = time.strftime("%Y%m%d_%H%M",time.gmtime(mtime))
                      date_max_mod  = time.strftime("%Y%m%d_%H%M",time.gmtime(mtime))
                 else:             
                      date_min_load = min ( date_min_load, date_load )
                      date_max_load = max ( date_max_load, date_load )
                      date_min_mod  = min ( date_min_mod, time.strftime("%Y%m%d_%H%M",time.gmtime(mtime)) )
                      date_max_mod  = max ( date_max_mod, time.strftime("%Y%m%d_%H%M",time.gmtime(mtime)) )

    if ( num_file == 0 ):
         date_min_load = "unknown"
         date_max_load = "unknown"
         date_max_mod  = "unknown"
    if ( config.action == "get_loading_first_date" ):
         print ( date_min_load )
    elif ( config.action == "get_loading_last_date" ):
         print ( date_max_load )
    elif ( config.action == "get_loading_last_update" ):
         print ( date_max_mod  )

#
# ===========================================================================
#
def aam_date ( config ):
    (mode, ino, dev, nlink, uid, gid, size, atime, mtime, ctime) = \
    os.stat( config.aam_ser_file )

    date_update = time.strftime("%Y%m%d_%H%M",time.gmtime(mtime))


    with open(config.aam_ser_file ) as f:
         buf = f.readlines()
    f.close()

    date_min_aam = "unknown"
    date_max_aam = "unknown"

    for str in buf:
        try:
            if ( str.index("Assimilation start date:") > 0 ):
                 date_min_aam = str.split()[4][0:19]
        except:
            pass

        try:
            if ( str.index("Data start date:") > 0 ):
                 date_min_aam = str.split()[4][0:19]
        except:
            pass

        try:
            if ( str.index("Forecast end date:") > 0 ):
                 date_max_aam = str.split()[4][0:19]
        except:
            pass

        try:
            if ( str.index("Data end date:") > 0 ):
                 date_max_aam = str.split()[4][0:19]
        except:
            pass
        

    if ( config.action == "get_aam_first_date"  ):
         print ( date_min_aam )
    elif ( config.action == "get_aam_last_date" ):
         print ( date_max_aam )
    elif ( config.action == "get_aam_last_update" ):
         print ( date_update )
    
#
# ===========================================================================
#
def spd_date ( config ):

    num_file = 0;
    extension_list = [ ".spd", ".spd.bz2" ]
    for paths, dirs, files in os.walk(config.spd_asc_dir):
        for file in files:
            file = config.spd_asc_dir + "/" + file
            ie =  file.rfind ( extension_list[0] )
            if ( ie < 1 ):
                 ie =  file.rfind ( extension_list[1] )
            if ( ie > 13 ):
                 if ( not os.path.isfile(file) ): continue
                 num_file = num_file + 1;
                 date_spd = file[ie-13:ie]
                 if ( num_file == 1 ):
                      date_min_spd = date_spd
                      date_max_spd = date_spd
                 else:             
                      date_min_spd = min ( date_min_spd, date_spd )
                      date_max_spd = max ( date_max_spd, date_spd )

                 (mode, ino, dev, nlink, uid, gid, size, atime, mtime, ctime) = \
                 os.stat( file )
                 if ( num_file == 1 ):
                      date_min_spd = date_spd
                      date_max_spd = date_spd
                      date_min_mod = time.strftime("%Y%m%d_%H%M",time.gmtime(mtime))
                      date_max_mod = time.strftime("%Y%m%d_%H%M",time.gmtime(mtime))
                 else:             
                      date_min_spd = min ( date_min_spd, date_spd )
                      date_max_spd = max ( date_max_spd, date_spd )
                      date_min_mod = min ( date_min_mod, time.strftime("%Y%m%d_%H%M",time.gmtime(mtime)) )
                      date_max_mod = max ( date_max_mod, time.strftime("%Y%m%d_%H%M",time.gmtime(mtime)) )


    if ( num_file == 0 ):
         date_min_spd = "unknown"
         date_max_spd = "unknown"
         date_max_mod = "unknown"
    if ( config.action == "get_spd_first_date" ):
         print ( date_min_spd )
    elif ( config.action == "get_spd_last_date" ):
         print ( date_max_spd )
    elif ( config.action == "get_spd_last_update" ):
         print ( date_max_mod  )

#
# ===========================================================================
#
def stat_service ( config ):
    num_file = 0
    for dir in config.dir_list:
        for paths, dirs, files in os.walk(dir):
            num_file = num_file + len(files)

    if ( config.action == "get_num_files" ):
         print ( num_file )

#
# ===========================================================================
#
def malo_vgep_date ( config ):
    
    num_file = 0;
    extension = config.extension
    for paths, dirs, files in os.walk(config.vgep_dir):
        for k in range(0,len(files)):
            ie =  files[k].find ( extension )
            if ( ie > 13 ):
                 file = paths + "/" + files[k]
                 if ( file.find("#") > 0 ): continue
                 num_file = num_file + 1;
                 date_vgep = files[k][ie-13:ie]
                 (mode, ino, dev, nlink, uid, gid, size, atime, mtime, ctime) = \
                 os.stat( file )
                 if ( num_file == 1 ):
                      date_min_vgep = date_vgep
                      date_max_vgep = date_vgep
                      date_min_mod  = time.strftime("%Y%m%d_%H%M",time.gmtime(mtime))
                      date_max_mod  = time.strftime("%Y%m%d_%H%M",time.gmtime(mtime))
                 else:             
                      date_min_vgep = min ( date_min_vgep, date_vgep )
                      date_max_vgep = max ( date_max_vgep, date_vgep )
                      date_min_mod  = min ( date_min_mod, time.strftime("%Y%m%d_%H%M",time.gmtime(mtime)) )
                      date_max_mod  = max ( date_max_mod, time.strftime("%Y%m%d_%H%M",time.gmtime(mtime)) )

    if ( num_file == 0 ):
         date_min_vgep = "unknown"
         date_max_vgep = "unknown"
         date_max_mod  = "unknown"
    if ( config.action == "get_vgep_first_date" ):
         print ( date_min_vgep )
    elif ( config.action == "get_vgep_last_date" ):
         print ( date_max_vgep )
    elif ( config.action == "get_vgep_last_update" ):
         print ( date_max_mod  )

    
#
# ===========================================================================
#
def parse_malo_service_config ( config ):
#"""
#    Reads the configuration file which has format KEYWORD: VALUE 
#    and puts parsed information into fields of class config
#"""
   with open ( config.filename  ) as f:
        conf_buf = f.readlines()
   f.close ( )

   if ( conf_buf[0][0:len(malo__fmt__label)] == malo__fmt__label[0:len(malo__fmt__label)] ):
        conf_type = "malo"
   elif ( conf_buf[0][0:len(aam__fmt__label)] == aam__fmt__label[0:len(aam__fmt__label)] ):
        conf_type = "aam"
   elif ( conf_buf[0][0:len(spd__fmt__label)] == spd__fmt__label[0:len(spd__fmt__label)] ):
        conf_type = "spd"
   elif ( conf_buf[0][0:len(stat__fmt__label)] == stat__fmt__label[0:len(stat__fmt__label)] ):
        conf_type = "stat"
   else:
        Message ( "F", "Unsupported support of config file " + config.filename + \
                  "\n Format label found:   " + conf_buf[0] + \
                  "\n While expected label: " + malo__fmt__label + " or " + \
                  aam__fmt__label + " or " + spd__fmt__label + " or " + stat_fmt__lavel + "\n" )
        exit ( 1 )

   num_par = 0
   config.dir_list  = []
   for line in conf_buf:
       if ( line == malo__fmt__label ): continue
       if ( line == aam__fmt__label ): continue
       if ( line == spd__fmt__label ): continue
       if   ( line.split()[0]     == "#" ): continue
       if ( conf_type == "malo" ) :
            if ( line.split()[0]       == "name:"         ):
                   config.name          = line.split()[1] 
                   num_par = num_par + 1
            elif ( line.split()[0]     == "begin_date:"   ):
                   config.begin_date    = line.split()[1] 
                   num_par = num_par + 1
            elif ( line.split()[0]     == "end_date:"     ):
                   config.end_date      = line.split()[1] 
                   num_par = num_par + 1
            elif ( line.split()[0]     == "loading_dir:"  ):
                   config.loading_dir   = line.split()[1] 
                   num_par = num_par + 1
            elif ( line.split()[0]     == "vgep_dir:"  ):
                   config.vgep_dir      = line.split()[1] 
                   num_par = num_par + 1
            elif ( line.split()[0]     == "heb_dir:"  ):
                   config.heb_dir      = line.split()[1] 
                   num_par = num_par + 1
            elif ( line.split()[0]     == "loading_type:" ):
                   config.loading_type  = line.split()[1] 
                   num_par = num_par + 1
            elif ( line.split()[0]     == "data_set:" ):
                   config.data_set      = line.split()[1] 
                   num_par = num_par + 1
            elif ( line.split()[0]     == "extension:" ):
                   config.extension     = line.split()[1] 
                   num_par = num_par + 1
       elif ( conf_type == "aam" ) :
            if ( line.split()[0]       == "name:"         ):
                 config.name          = line.split()[1] 
                 num_par = num_par + 1
            elif ( line.split()[0]     == "aam_dir:"   ):
                   config.aam_dir       = line.split()[1] 
                   num_par = num_par + 1
            elif ( line.split()[0]     == "aam_ser_file:"   ):
                   config.aam_ser_file  = line.split()[1] 
                   num_par = num_par + 1
       elif ( conf_type == "spd" ) :
            if ( line.split()[0]       == "name:"         ):
                 config.name            = line.split()[1] 
                 num_par = num_par + 1
            elif ( line.split()[0]     == "spd_asc_dir:"   ):
                   config.spd_asc_dir   = line.split()[1] 
                   num_par = num_par + 1
            elif ( line.split()[0]     == "spd_bin_dir:"   ):
                   config.spd_bin_dir   = line.split()[1] 
                   num_par = num_par + 1
       if ( conf_type == "stat" ) :
            if ( line.split()[0]     == "dir_list:"   ):
                 config.dir_list.append ( line.split()[1] )
                 num_par = num_par + 1

   if ( conf_type == "malo" and num_par < malo__num_par ):
        print ( "Not all keywords were found in in control file " + \
                 config.filename + " -- only %d, though %d were expected" % \
                 ( num_par, malo__num_par ) )
        exit ( 1 )

   if ( conf_type == "aam" and num_par < aam__num_par ):
        print ( "NOT all keywords were found in in control file " + \
                 config.filename + " -- only %d, though %d were expected" % \
                 ( num_par, aam__num_par ) )
        exit ( 1 )

   if ( conf_type == "spd" and num_par < spd__num_par ):
        print ( "NOT all keywords were found in in control file " + \
                 config.filename + " -- only %d, though %d were expected" % \
                 ( num_par, spd__num_par ) )
        exit ( 1 )

   if ( conf_type == "stat" and num_par < stat__num_par ):
        print ( "NOT all keywords were found in in control file " + \
                 config.filename + " -- only %d, though %d were expected" % \
                 ( num_par, stat__num_par ) )
        exit ( 1 )

   return 1

#
# ------------------------------------------------------------------------
#
def main():

    opts = optparse.OptionParser( version=malo_service__label )

    opts.add_option ( "-c", "--config", action="store", \
                      dest="config", \
                      metavar="NAME", \
                      help="Configuration file" )

    opts.add_option ( "-a", "--action", action="store", \
                      dest="action", \
                      metavar="NAME", \
                      help="action to exectue" )
#
# --- Get and parse options
#
    opts, args = opts.parse_args()
    
#
# --- Check option values
#
    if ( opts.config == None ):
         print ( "Configuration file is not specied. Try malo_service.py -h to see options" )
         exit ( 1 )

    if ( opts.action == None ):
         print ( "Action is not specified. Try malo_service.py  -h to see options" )
         exit ( 1 )

    if ( not os.path.isfile(opts.config) ):
         print ( "Configuration file ", opts.config, " does not exist" )
         exit ( 1 )
         

    config = config_class ( opts.config ) 

    parse_malo_service_config ( config )
    config.action = opts.action

    if ( config.action == "get_loading_first_date" or \
         config.action == "get_loading_last_date"  or \
         config.action == "get_loading_last_update"   ):
         malo_loading_date ( config )
    elif ( config.action == "get_vgep_first_date" or   \
           config.action == "get_vgep_last_date"  or   \
           config.action == "get_vgep_last_update"     ):
           malo_vgep_date ( config )
    elif ( config.action == "get_aam_first_date"  or   \
           config.action == "get_aam_last_date"   or   \
           config.action == "get_aam_last_update"      ):
           aam_date ( config )
    elif ( config.action == "get_spd_first_date"  or   \
           config.action == "get_spd_last_date"   or   \
           config.action == "get_spd_last_update"      ):
           spd_date ( config )
    elif ( config.action == "get_num_files"       ):
           stat_service ( config )
    else:
         print ( "Unsupported action: ", config.action, \
                 " List of supported actions: get_loading_first_date, " \
                 "get_loading_last_date, get_loading_ast_update, " \
                 "get_aam_first_date, get_aam_last_date, get_aam_last_update, " \
                 "get_spd_first_date, get_spd_last_date, get_spd_last_update, " \
                 "get_vgep_first_date, get_vgep_last_date, get_vgep_last_update, " \
                 "get_num_files" )

if __name__ == "__main__":
    vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    main()
