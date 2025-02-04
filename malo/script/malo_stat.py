#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *   Program for generating current status of MALO products.            *
# *                                                                      *
# * ###  30-JUN-2014  malo_stat.py  v2.3 (c) L. Petrov  11-NOV-2021  ### *
# *                                                                      *
# ************************************************************************
"""
import sys, os, shutil, time, subprocess
import optparse 
import math
import datetime
from   datetime import timedelta
from   Message     import *
from   geos_oper_config import *


malo_stat__label = "malo_stat.py of 2024.01.30"
fmt__label       = "# MALO_STAT Configuration file. Format of 2023.03.03"
config__num_par  = 12 # The number of configuration parameters

date_range = "1978.01.01 2025.01.01"

pyvers = "%d.%02d" % ( sys.version_info.major, sys.version_info.minor ) # python version

class malo_stat_config_class:
   def __init__ ( self, filename ):
       self.filename        = filename
       self.title           = None
       self.host            = "unknown"
       self.log_dir         = None
       self.lat_file        = None
       self.stat_http       = None
       self.stat_dir        = None
       self.plot_dir        = None
       self.ondemand_log    = None
       self.ondemand_queue  = None
       self.ondemand_sleep  = 0.0
       self.num_services    = 0
       self.fcs_heb_dir     = None
       self.fcs_pivot_sds   = None
       self.service_name    = []
       self.service_unit    = []
       self.service_data    = []
       self.service_type    = []


   def init ( self ):
       __init__ ( self )

#
# ===========================================================================
#
def malo_stat_config_parse ( config ):
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

   num_par = 0 
   for line in conf_buf:
       if ( line == fmt__label ): continue
       if ( line[0] == "#"     ): continue
       if   ( line.split()[0]      == "#" ): continue
       if   ( line.split()[0]      == "title:"        ):
              config.title          = line.split()[1] 
              num_par = num_par + 1
       if   ( line.split()[0]      == "host:"        ):
              config.host          = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "log_dir:"      ): 
              config.log_dir        = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "lat_file:"      ): 
              config.lat_file       = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "stat_http:"    ):
              config.stat_http      = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "stat_dir:"    ):
              config.stat_dir       = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "plot_dir:"    ):
              config.plot_dir       = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]      == "num_services:" ):
              config.http_name      = int(line.split()[1] )
              num_par = num_par + 1
       elif ( line.split()[0]      == "ondemand_log:" ):
              config.ondemand_log   = line.split()[1]
              if ( config.ondemand_log  == "NONE" ): 
                   config.ondemand_log = None
              num_par = num_par + 1
       elif ( line.split()[0]      == "ondemand_queue:" ):
              config.ondemand_queue = line.split()[1]
              if ( config.ondemand_queue == "NONE" ): 
                   config.ondemand_queue = None
              num_par = num_par + 1
       elif ( line.split()[0]      == "ondemand_sleep:" ):
              if ( line.split()[1] == "NONE" ): 
                   config.ondemand_sleep = 1000000
              else:
                   config.ondemand_sleep = float(line.split()[1])
              num_par = num_par + 1
       elif ( line.split()[0]      == "fcs_heb_dir:"    ):
              config.fcs_heb_dir = line.split()[1]
       elif ( line.split()[0]      == "fcs_pivot_sds:"  ):
              config.fcs_pivot_sds  = line.split()[1]
       elif ( line.split()[0]      == "service:"      ):
              if ( len(config.service_name) == 0 ):
                   num_par = num_par + 1
              config.service_name.append ( line.split()[1] )
              config.service_unit.append ( line.split()[2] )
              config.service_data.append ( line.split()[3] )
              str = ""
              for i in range(4,len(line.split())):
                  str = str + line.split()[i]
                  if ( i < len(line.split()) - 1 ):
                       str = str + " "
              config.service_type.append ( str )

   if ( num_par < config__num_par ):
        print ( "Not all keywords were found in in control file " + \
                 config.filename + " -- only %d, though %d were expected" % \
                 ( num_par, config__num_par ) )
        exit ( 1 )

#
# ===========================================================================
#
def malo_stat ( config, ivrb ):

    f = open ( config.stat_http, "w" )
#
# --- Write the title
#
    if ( pyvers >= "3.12" ):
         utc_now_str = datetime.datetime.now(datetime.UTC).strftime("%Y.%m.%d_%H:%M:%S")
    else:
         utc_now_str = datetime.datetime.utcnow().strftime("%Y.%m.%d_%H:%M:%S")
    f.write ( "<BIG>Status at " + utc_now_str + \
                 " <B>UTC</B> </BIG> &nbsp; &nbsp; &nbsp; &nbsp; <SMALL>( " + \
              datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S") + \
              " local time ) <SMALL>\n" )
    f.write ( "<P>\n" )
#
# --- Write the table header
#
    f.write ( '<TABLE CELLSPACING="2" CELLPADDING="10" BORDER="1" >\n' )
    f.write ( '<TR><TD><B><I> Data source </I></B></TD>' + \
              '<TD><I> Service type </I></TD>\n' + \
              '<TD ALIGN="CENTER"><I> Latency </I></TD>\n' + \
              '<TD NOWRAP> Last time updated (UTC)</TD>\n' + \
              '<TD ALIGN="RIGHT" NOWRAP> Since last update </TD>\n' + \
              '<TD ALIGN="RIGHT" NOWRAP> Since last error  </TD>\n' + \
              '</TR>\n' )
    last_date = []
    for i in range (0,len(config.service_name)):
#
# ----- Get sucess file, its modification date and its contents
#
        if ( ivrb > 2 ):
             print ( "service_name: ", config.service_name[i] )

        suc_file = config.log_dir + "/" + config.service_name[i] + ".fcs"
        if ( not os.path.isfile(suc_file) ):
             suc_file = config.log_dir + "/" + config.service_name[i] + ".suc"
        try:
             if ( pyvers >= "3.12" ):
                  update_time = datetime.datetime.fromtimestamp(os.path.getmtime(suc_file),datetime.UTC)
                  latency_update = datetime.datetime.now(datetime.UTC) - update_time 
             else:
                  update_time = datetime.datetime.utcfromtimestamp(os.path.getmtime(suc_file))
                  latency_update = datetime.datetime.utcnow() - update_time 
             latency_update_sec = latency_update.days*86400.0 + latency_update.seconds
             with open ( suc_file ) as s:
                  log_buf = s.readlines()
             s.close ( )
        except:
             update_time    = "n/a"
             latency_update = "n/a"
             latency_update_sec = 1024*1024*1024
                          
#
# ----- Get the error file and its modification date
#
        err_file = config.log_dir + "/" + config.service_name[i] + ".err"
        try:
            if ( pyvers >= "3.12" ):
                 err_time = datetime.datetime.fromtimestamp(os.path.getmtime(err_file),datetime.UTC)
                 latency_err = datetime.datetime.now(datetime.UTC).replace(tzinfo=None) - err_time
            else:
                 err_time = datetime.datetime.utcfromtimestamp(os.path.getmtime(err_file))
                 latency_err = datetime.datetime.utcnow() - err_time
        except:
            latency_err = "n/a"

        if ( config.service_name[i] == "geosfcs_fcs" ):
#
# ---------- Special case to the last forecast date. It may not go in the 
# ---------- chronological order
#
             fcs_last_date = "19500101_0000"
             for dir_list, subdir_list, file_list in os.walk(config.fcs_heb_dir):
                 for file in file_list:
                     if ( ".heb.bz2" in file and "u_" in file ):
                           if ( file[len(file)-21:len(file)-8] > fcs_last_date ):
                                fcs_last_date = file[len(file)-21:len(file)-8] 
             last_date.append ( fcs_last_date )
        else:
             try:
                  last_date.append ( log_buf[0].split()[-1] )
             except:
                  pass

        try:
             if ( len(last_date[i]) == 8 ):
                  fidat_last = datetime.datetime.strptime ( last_date[i], "%Y%m%d" )
             else:
                  fidat_last = datetime.datetime.strptime ( last_date[i], "%Y%m%d_%H%M" )
        except:
             print ( "Cannot get last date of data processed from file ", suc_file )
             exit ( 1 )


        if ( pyvers >= "3.12" ):
             latency_data = datetime.datetime.now(datetime.UTC).replace(tzinfo=None) - fidat_last 
        else:
             latency_data = datetime.datetime.utcnow() - fidat_last 
        if ( latency_err == "n/a" ):
             latency_err_str = "n/a"
             latency_err_sec = 512*1024*1024
        else:
             if ( latency_err.days > 0 ):
                  latency_err_str= "%7.2f" % ( latency_err.days + latency_err.seconds/86400.0 ) + \
                                    " days"
             else:
                  latency_err_str= "%7.2f" % ( latency_err.days*24.0 + latency_err.seconds/3600.0 ) + \
                                    " hours"
             latency_err_sec = latency_err.days*86400 + latency_err.seconds
        latency_err_str = latency_err_str.lstrip()
#
# ----- Write columns service name and service type
#
        f.write ( "  <TR><TD> <B>" + config.service_data[i] + " </B></TD>\n" )
        f.write ( "      <TD>" + config.service_type[i] + " </TD>\n" )
        if ( config.service_unit[i] == "hour" ):
             if ( latency_update != "n/a" ):
                  latency_update_str = "%5.2f" % ( latency_update.days*24.0 + latency_update.seconds/3600.0 ) + \
                                       " " + config.service_unit[i] 
             else:
                  latency_update_str = "n/a"
             latency_data_str   = "%5.2f" % ( latency_data.days*24.0   + latency_data.seconds/3600.0   ) +\
                                  " " + config.service_unit[i] 
             latency_update_str = latency_update_str.lstrip()
             if ( ivrb > 3 ):
                  print ( "latency_update_str= ", latency_update_str, " latency_err_str= ", latency_err_str ) # %%%%%%%%%%%%
#
             if ( latency_err_str != "n/a" and latency_update_sec > latency_err_sec ):
#
# --------------- Red color: error file is yonger than success fule
#
                  latency_data_str = '<FONT COLOR="C00000"><B> ' + latency_data_str + '</B> </FONT>'
                  latency_err_str  = '<FONT COLOR="C00000"><B> ' + latency_err_str  + '</B> </FONT>'
             elif ( latency_data.days < 1 ):
#
# --------------- Green color, no errors, latency is OK
#
                  latency_data_str = '<FONT COLOR="008000"><B> ' + latency_data_str + '</B> </FONT>'
             else:
#
# --------------- Yellow color: no errors, bu latency is bad
#
                  latency_data_str = '<FONT COLOR="D8A000"><B> ' + latency_data_str + '</B> </FONT>'
        elif ( config.service_unit[i] == "Hour" ):
             latency_hours = latency_data.days*24.0 + latency_data.seconds/3600.0 
             if ( latency_update != "n/a" ):
                  latency_update_str = "%5.2f" % ( latency_update.days*24.0 + latency_update.seconds/3600.0 ) + \
                                       " " + config.service_unit[i] 
             else:
                  latency_update_str = "n/a"
             latency_data_str   = "%5.2f" % ( latency_data.days*24.0   + latency_data.seconds/3600.0   ) +\
                                  " " + config.service_unit[i] 
             latency_update_str = latency_update_str.lstrip()

             if ( latency_err_str != "n/a" and latency_update_sec > latency_err_sec ):
#
# --------------- Red color: the error file is younger than the success file
#
                  latency_data_str = '<FONT COLOR="C00000"><B> ' + latency_data_str + '</B> </FONT>'
                  latency_err_str  = '<FONT COLOR="C00000"><B> ' + latency_err_str  + '</B> </FONT>'
             elif ( latency_hours < -24.0 ):
#
# --------------- Green color, no errors, latency is OK
#
                  latency_data_str = '<FONT COLOR="008000"><B> ' + latency_data_str + '</B> </FONT>'
             elif ( latency_hours > -0.1 ):
#
# --------------- Red color: latency is not real time
#
                  latency_data_str = '<FONT COLOR="C00000"><B> ' + latency_data_str + '</B> </FONT>'
             else:
#
# --------------- Yellow color: no errors, but latency is bad
#
                  latency_data_str = '<FONT COLOR="D8A000"><B> ' + latency_data_str + '</B> </FONT>'

        elif ( config.service_unit[i] == "day" ):
             if ( latency_update != "n/a" ):
                  latency_update_str = "%5.2f" % ( latency_update.days + latency_update.seconds/86400.0 ) + \
                                       " " + config.service_unit[i] 
             else:
                  latency_update_str = "n/a"
             latency_data_str   = "%5.2f" % ( latency_data.days   + latency_data.seconds/86400.0   ) + \
                                  " " + config.service_unit[i] 
             if ( latency_err == "n/a" ):
                  latency_err_str    = "n/a"
             else:
                  latency_err_str    = "%7.2f" % ( latency_err.days   + latency_err.seconds/86400.0   ) + \
                                       " " + config.service_unit[i] 

             latency_update_str = latency_update_str.lstrip()
             latency_err_str    = latency_err_str.lstrip()

             if ( latency_err_str != "n/a" and latency_update_sec > latency_err_sec ):
#
# -------------------- Red color: error file is yonger than success fule
#
                  latency_data_str = '<FONT COLOR="C00000"><B> ' + latency_data_str + '</B> </FONT>'
                  latency_err_str  = '<FONT COLOR="C00000"><B> ' + latency_err_str  + '</B> </FONT>'
             elif ( latency_data.days < 60 ):
#
# -------------------- Green color, no errors, latency is OK
#
                  latency_data_str = '<FONT COLOR="008000"><B> ' + latency_data_str + '</B> </FONT>'
             elif ( latency_update_str != "n/a" ):
#
# -------------------- Yellow color: no errors, but latency is bad
#
                  latency_data_str = '<FONT COLOR="D8A000"><B> ' + latency_data_str + '</B> </FONT>'
             else:
#
# --------------- Red color: latency update is not known
#
                  latency_data_str = '<FONT COLOR="C00000"><B> ' + latency_data_str + '</B> </FONT>'
                  latency_err_str  = '<FONT COLOR="C00000"><B> ' + latency_err_str  + '</B> </FONT>'

        f.write ( '      <TD ALIGN="RIGHT" NOWRAP>' + latency_data_str   + " </TD>\n" )
        if ( update_time != "n/a" ): 
             f.write ( '      <TD ALIGN="RIGHT" NOWRAP>' + update_time.strftime("%Y.%m.%d_%H:%M") + " </TD>\n" )
        else:
             f.write ( '      <TD ALIGN="RIGHT" NOWRAP>' + "n/a" + " </TD>\n" )
        f.write ( '      <TD ALIGN="CENTER">' + '<A HREF="' + config.plot_dir + '/latency_' + \
                  config.service_name[i] + '.html' + '">' + \
                  latency_update_str + " </A> </TD>\n" )
        f.write ( '      <TD ALIGN="RIGHT" NOWRAP>' + latency_err_str  + " </TD>\n" )
        f.write ( "  </TR>\n" )
#
# --- Write the table footer
#
    f.write ( "</TABLE>\n" )
    if ( config.ondemand_log != None ):

        with open ( config.ondemand_log, encoding="latin" ) as s:
             ond_buf = s.readlines()
        s.close ( )
        if ( pyvers >= "3.12" ):
             now_utc   = datetime.datetime.now(datetime.UTC).replace(tzinfo=None)
        else:
             now_utc   = datetime.datetime.utcnow()
        now_local = datetime.datetime.now()
        try:
#
# ---------- Get the last date of ond activity and time elapsed from that
#
             ond_date_local_str = ond_buf[len(ond_buf)-1].split()[0]
             ond_date_local = datetime.datetime.strptime ( ond_date_local_str, "%Y.%m.%d-%H:%M:%S" )
             ond_date_utc = ond_date_local + (now_utc - now_local)
             lat_ond = (now_utc - ond_date_utc).total_seconds()
        except:
             lat_ond = -999999

        with open ( config.ondemand_queue ) as s:
             queue_buf = s.readlines()
        s.close ( )
#
        try:
#
# ---------- Get the last date of request placed in the queue and time elapsed from that
#
             last_queue_str = queue_buf[len(queue_buf)-1].split()[0]
             last_queue_local = datetime.datetime.strptime ( last_queue_str, "%Y%m%d_%H%M%S" )
             lat_queue_utc   = last_queue_local + datetime.timedelta(seconds=time.altzone)
             lat_queue = (now_utc - lat_queue_utc).total_seconds()
        except:
             lat_queue = -999999
#

        f.write ( ' \n' )
        f.write ( '<P>&nbsp;<P>&nbsp;<P>\n' )
        f.write ( ' \n' )
        f.write ( '<TABLE CELLSPACING="2" CELLPADDING="10" BORDER="1" >\n' )
        f.write ( '  <TR><TD> <B> Service </B></TD>\n' )
        f.write ( '      <TD> <B> Last ping </B></TD> \n' )
        f.write ( '      <TD> <B> Since last processing </B></TD> \n' )
        f.write ( '  </TR>\n' )
        f.write ( '  <TR><TD> <B> On-demand loading </B></TD>\n' )
        if ( lat_ond < -100 ): 
             f.write ( '%s %s %s' % ( '      <TD><FONT COLOR="800000"><B> ', "down", ' </B></FONT></TD> \n' ) )
        elif ( lat_ond < 2.0*config.ondemand_sleep ):
             f.write ( '%s %8.1f %s' % ( '      <TD><FONT COLOR="008000"><B> ', lat_ond, ' sec </B></FONT></TD> \n') )
        elif ( lat_ond  < 1000 ):
             f.write ( '%s %8.1f %s' % ( '      <TD><FONT COLOR="80000"><B> ', lat_ond, ' sec </B></FONT></TD> \n' ) )
        elif ( lat_ond < 10000 ):
             f.write ( '%s %8.2f %s' % ( '      <TD><FONT COLOR="80000"><B> ', lat_ond/60.0, ' min </B></FONT></TD> \n' ) )
        elif ( lat_ond < 4*86400.0 ):
             f.write ( '%s %8.2f %s' % ( '      <TD><FONT COLOR="80000"><B> ', lat_ond/3600.0, ' hours </B></FONT></TD> \n' ) )
        else:
             f.write ( '%s %8.2f %s' % ( '      <TD><FONT COLOR="80000"><B> ', lat_ond/86400, ' days </B></FONT></TD> \n' ) )

        if ( lat_queue < 0 ):
             f.write ( '%s %s' % ( '      <TD ALIGN="RIGHT" NOWRAP> ', \
                       ' down </TD>\n' ) )
        if ( lat_queue < 1000 ):
             f.write ( '%s %8.1f %s' % ( '      <TD ALIGN="RIGHT" NOWRAP> ', \
                       lat_queue, ' sec </TD>\n' ) )
        elif ( lat_queue < 10000 ):
             f.write ( '%s %8.2f %s' % ( '      <TD ALIGN="RIGHT" NOWRAP> ', \
                       lat_queue/60.0, ' min </TD>\n' ) )
        elif ( lat_queue < 4*86400 ):
             f.write ( '%s %8.2f %s' % ( '      <TD ALIGN="RIGHT" NOWRAP> ', \
                       lat_queue/3600.0, ' hours </TD>\n' ) )
        else:
             f.write ( '%s %8.2f %s' % ( '      <TD ALIGN="RIGHT" NOWRAP> ', \
                       lat_queue/86400, ' days </TD>\n' ) )
        f.write ( '  </TR>\n' )
        f.write ( '</TABLE>\n' )
        f.write ( '<P> &nbsp; <P>\n' )
        f.write ( ' \n' )
        imls_stat_file = config.stat_dir + "/" + config.host + "_imls_check.txt"
        f.write ( "<PRE>\n" )
        f.write ( "<!--#include virtual='" + imls_stat_file + "'-->\n" )
        f.write ( "</PRE>\n" )
        f.write ( ' \n' )
        f.write ( '</P>\n' )

    f.close()
#
# --- Now update the statistcs file: append the line with the current UTC time 
# --- and UTC epochs of the latest results update
#
    if ( ivrb > 3 ):
         print ( "About to open config file ", config.lat_file, " len(config.service_name)= ", len(config.service_name) )
    try:
         y = open ( config.lat_file, "a" )
         if ( pyvers >= "3.12" ):
              y.write  ( datetime.datetime.now(datetime.UTC).strftime("%Y.%m.%d_%H:%M") + " " )
         else:
              y.write  ( datetime.datetime.utcnow().strftime("%Y.%m.%d_%H:%M") + " " )
         for i in range (0,len(config.service_name)):
             if ( ivrb > 2 ):
                  print ( "Service_Name: ", config.service_name[i], " last_date: ", last_date[i] )
             y.write  ( last_date[i] + " " )
         y.write  ( "\n" )
         y.close()
    except:
         pass      

#
# ------------------------------------------------------------------------
#
def main():

    os.putenv ( "GOMP_STACKSIZE", "2000000")
    os.putenv ( "LD_LIBRARY_PATH", "/opt64/lib:/opt64/lib/python3.3/lib-dynload:/usr/lib" )

    opts = optparse.OptionParser( version=malo_stat__label  )

    opts.add_option ( "-c", "--config", action="store", \
                      dest="config", \
                      metavar="NAME", \
                      help="Configuration file" )

    opts.add_option ( "-v", "--verbosity", action="store", \
                      dest="ivrb", \
                      default=0, \
                      type="int", \
                      metavar="NAME", \
                      help="Verbosity level" )

#
# --- Get and parse options
#
    opts, args = opts.parse_args()
    
#
# --- Check option values
#
    if ( opts.config == None ):
         print ( "Configuration file is not specied. Try get_geos_oper.py -h to see options" )
         exit ( 1 )

    if ( not os.path.isfile(opts.config) ):
         print ( "Configuration file ", opts.config, " does not exist" )
         exit ( 1 )
         

    config = malo_stat_config_class ( opts.config ) 

    malo_stat_config_parse ( config )

    malo_stat ( config, opts.ivrb )

if __name__ == "__main__":
    vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    main()
