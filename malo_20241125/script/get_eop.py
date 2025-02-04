#!/usr/bin/env python3
import sys, os, shutil, time, calendar, subprocess, datetime
import optparse 
import gzip
import zlib
import datetime
from   subprocess        import Popen, PIPE
from   datetime          import timedelta
from   Message           import *
from   exe               import *

get_eop__label = "get_eop.py v 1.21 of 2024.01.12"

fmt__label      = "# EOP processing control file.  Format version of 2023.03.03"
eop__label      = "# Earth orientation time series.  Format version of 2016.04.01"
config__num_par = 29 # The number of configuration parameters

#wget_opts = "-q --timeout=20 --connect-timeout=20 --tries=64 -nH -N " + \
#            "--cut-dirs=8 --no-remove-listing -c " + \
#            "--retry-connrefused"

wget_opts = "-q --timeout=20 --connect-timeout=20 --retry-connrefused " + \
            "--auth-no-challenge --tries=64 -nH -N " + \
            "--cut-dirs=8 --no-remove-listing -c "

cddis_week_min = "2222"

mjd_j2000_unix_sec = calendar.timegm(time.strptime('2000.01.01_00:00:00', '%Y.%m.%d_%H:%M:%S') )
mjd_j2000          = 51544 # 2000.01.01_00:00:00
PI                 = 3.141592653589793
ARCSEC__TO__RAD    = PI/(180.0*3600.0)
UT1__TO__E3        = -1.00273781191135448e0*PI/43200.0
OM__EAR            =  7.29211514670697e-5
LOD__TO__ER3       =  1.00273781191135448e0*OM__EAR**2/(2*PI)

pyvers = "%d.%02d" % ( sys.version_info.major, sys.version_info.minor ) # python version

class eop_config_class:
   def __init__ ( self, filename, verb ):
       self.filename           = filename
       self.username           = None
       self.password           = None
       self.verb               = verb
       self.eop_url            = []
       self.eop_type           = []
       self.eop_pref           = []
       self.eop_download_dir   = []
       self.eop_dir            = []
       self.aam_ser_file       = []
       self.aam_ser_ref        = []
       self.leapsec_list       = []
       self.leapsec_list       = []
       self.series_code        = []
       self.series_type        = {}
       self.series_units       = {}
       self.series_good        = {}
       self.series_crit        = {}
       self.series_name        = {}
       self.ners_stop_file     = {}  
       self.eop_stat_file      = None
       self.eop_stat_html      = None
       self.eop_fcs_ser        = None
       self.ners_url           = None
       self.max_file_age       = None
       self.eop_fcs_pref       = None
       self.ners_plot_dir      = None
       self.heo_file           = None
       self.prc_apr_mod        = None
       self.nut_apr_mod        = None
       self.e3z_mod            = None
       self.heo_mod            = None
       self.heo_id             = None
       self.ltp_mod            = None
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
def eop_series_transform ( config, lh ):

    for i in range(0,len(config.eop_download_dir)):
        ddir = config.eop_download_dir[i] + "/" + config.eop_pref[i]
        file_list = []
        if ( config.verb > 1 ):
             print ( "get_eop.py Processing directory ", ddir )
             sys.stdout.flush()
        for paths, dirs, files in os.walk(ddir ):
            for k in range(0,len(files)):
                if ( files[k].find('~') > -1 ): continue
                if ( files[k].find('#') > -1 ): continue
                if ( files[k][0:1] == "."    ): continue
                file_list.append ( paths + "/" + files[k] )

        file_list.sort()
        out_file = config.eop_dir + "/" + config.eop_pref[i] + ".eop"
        eop_transform ( file_list, config.eop_type[i], \
                        config.eop_pref[i], config.leapsec_list, \
                        config.verb, out_file )

#
# ------------------------------------------------------------------------
#
def eop_transform ( eop_file_list, eop_type, eop_pref, leapsec_list, \
                    ivrb, eop_out_file ):

    out = []
    tim_dic = {}
    if ( eop_type == "dir" ):
         if ( ivrb  > 0 ):
              print ( "get_eop.py EOP directory ", eop_pref, \
                       "with %d files" % len(eop_file_list) )
              sys.stdout.flush()
         for eop_file in eop_file_list:
             if ( eop_file.find("cod09517.erp.Z") >= 0 ): continue
             if ( eop_file.find("cod06497.erp.Z") >= 0 ): continue
             if ( eop_file.find("usn16870.erp.Z") >= 0 ): continue
             if ( eop_file.find("#")              >= 0 ): continue
             if ( eop_file.find("~")              >= 0 ): continue
             if ( eop_file.find("igu")            >= 0 ): 
                  fl_igu = True
             else:
                  fl_igu = False
#
             if ( ivrb > 3 ):
                  print ( "Processing file: ", eop_file )
             if ( eop_file.find(".gz") > 0 or \
                  eop_file.find(".Z")  > 0    ):
                  f = Popen(["zcat", eop_file], stdout=PIPE).stdout
             else:
                  f = Popen(["cat", eop_file], stdout=PIPE).stdout
             eop_buf = f.readlines()
             f.close()
             e   = [0.0, 0.0, 0.0, 0.0]
             er  = [0.0, 0.0, 0.0, 0.0]
             de  = [0.0, 0.0, 0.0, 0.0]
             der = [0.0, 0.0, 0.0, 0.0]
             for line in eop_buf:
                 line = line.decode("iso-8859-1").strip("\n")
                 if ( line.find("Source: NEOS/IERS Bulletin A") >= 0 ): break
                 if ( line.find("Source: IERS Bulletin A")      >= 0 ): break
                 if ( line.find("UT1-UTC IERS Bulletin A")      >= 0 ): break
                 if ( line.find("CENTER FOR ORBIT DETERM")      >= 0 ): break
                        
                 if ( line[5:6] == '.' ):
                      mjd_r8 = float ( line.split()[0] )
                      tim = (mjd_r8 - mjd_j2000)*86400.0
                      tai_utc = get_leapsec ( tim, leapsec_list )
                      tim = tim + tai_utc 
                      e[1]   = float ( line.split()[2] )/1.0e6*ARCSEC__TO__RAD
                      e[2]   = float ( line.split()[1] )/1.0e6*ARCSEC__TO__RAD
                      e[3]   = (float ( line.split()[3] )/1.0e7 - tai_utc)*UT1__TO__E3
                      er[1]  = float ( line.split()[13])/1.0e6*ARCSEC__TO__RAD/86400.0
                      er[2]  = float ( line.split()[12])/1.0e6*ARCSEC__TO__RAD/86400.0
                      er[3]  = float ( line.split()[4] )/1.0e7*LOD__TO__ER3
#
                      de[1]  = float ( line.split()[6] )/1.0e6*ARCSEC__TO__RAD
                      de[2]  = float ( line.split()[5] )/1.0e6*ARCSEC__TO__RAD
                      de[3]  = float ( line.split()[7] )/1.0e7*UT1__TO__E3
                      der[1] = float ( line.split()[15])/1.0e6*ARCSEC__TO__RAD/86400.0
                      der[2] = float ( line.split()[14])/1.0e6*ARCSEC__TO__RAD/86400.0
                      der[3] = float ( line.split()[8] )/1.0e7*LOD__TO__ER3
#
                      ind_tim = None
                      tim_str = "%12.3f" % tim 
                      ind_tim = None
                      if ( len(out) > 0 ):
                           if ( tim_str in tim_dic ):
                                ind_tim = tim_dic[tim_str]
                      if ( ind_tim ):
                           out[ind_tim] = eop_format ( tim, e, er, de, der )
                      else:
                           out.append (   eop_format ( tim, e, er, de, der ) )
                           tim_dic[tim_str] = len(out)-1

                      if ( fl_igu ): break

    elif ( eop_type == "file" ):
         if ( len(eop_file_list) < 1 ):
              return -1
         if ( ivrb > 0 ):
              print ( "get_eop.py EOP file      ", eop_pref, eop_file_list[0] )

         if ( eop_file_list[0].find(".gz") > 0 or \
              eop_file_list[0].find(".Z")  > 0    ):
              f = Popen(["zcat", eop_file_list[0]], stdout=PIPE).stdout
         else:
              f = Popen(["cat", eop_file_list[0]], stdout=PIPE).stdout
         eop_buf = f.readlines()
         f.close()
         eop_buf = clean_eop_ser ( eop_buf )
         e   = [0.0, 0.0, 0.0, 0.0]
         er  = [0.0, 0.0, 0.0, 0.0]
         de  = [0.0, 0.0, 0.0, 0.0]
         der = [0.0, 0.0, 0.0, 0.0]
         for line in eop_buf:
             if ( line[0:1] == '#' ): continue
             fl_append = 0
             if ( line[6:7] == '.' and line[34:35] == '.' ):
                  mjd_r8 = float ( line.split()[0] )
                  tim = (mjd_r8 - mjd_j2000)*86400.0
                  tai_utc = get_leapsec ( tim, leapsec_list )
                  e[1]   = float ( line.split()[2] )*ARCSEC__TO__RAD
                  e[2]   = float ( line.split()[1] )*ARCSEC__TO__RAD
                  e[3]   = (float ( line.split()[3] ) - tai_utc)*UT1__TO__E3
                  er[1]  = float ( line.split()[20])*ARCSEC__TO__RAD/86400.0
                  er[2]  = float ( line.split()[19])*ARCSEC__TO__RAD/86400.0
                  er[3]  = float ( line.split()[21])*LOD__TO__ER3
#
                  de[1]  = float ( line.split()[7] )*ARCSEC__TO__RAD
                  de[2]  = float ( line.split()[6] )*ARCSEC__TO__RAD
                  de[3]  = -float ( line.split()[8] )*UT1__TO__E3
                  der[1] = float ( line.split()[25])*ARCSEC__TO__RAD/86400.0
                  der[2] = float ( line.split()[24])*ARCSEC__TO__RAD/86400.0
                  der[3] = float ( line.split()[26] )*LOD__TO__ER3
                  fl_append = 1
             elif ( line[34:35] == '.' and line[45:46] == '.' ):
#
# --------------- pre-2023 C04
#
                  mjd_r8 = float ( line.split()[3] )
                  tim = (mjd_r8 - mjd_j2000)*86400.0
                  tai_utc = get_leapsec ( tim, leapsec_list )
                  e[1]   = float ( line.split()[5] )*ARCSEC__TO__RAD
                  e[2]   = float ( line.split()[4] )*ARCSEC__TO__RAD
                  e[3]   = (float ( line.split()[6] ) - tai_utc)*UT1__TO__E3
                  er[1]  = 0.0
                  er[2]  = 0.0
                  er[3]  = float ( line.split()[7])*LOD__TO__ER3
#
                  de[1]  = float ( line.split()[11] )*ARCSEC__TO__RAD
                  de[2]  = float ( line.split()[10] )*ARCSEC__TO__RAD
                  de[3]  = float ( line.split()[12] )*UT1__TO__E3
                  der[1] = 0.0
                  der[2] = 0.0
                  der[3] = float ( line.split()[13] )*LOD__TO__ER3
                  fl_append = 1
             elif ( line[31:32] == '.' and line[43:44] == '.' and line[54:55] == '.' ):
#
# --------------- post-2023 C04
#
                  mjd_r8 = float ( line.split()[4] )
                  tim = (mjd_r8 - mjd_j2000)*86400.0
                  tai_utc = get_leapsec ( tim, leapsec_list )
                  e[1]   = float ( line.split()[6] )*ARCSEC__TO__RAD
                  e[2]   = float ( line.split()[5] )*ARCSEC__TO__RAD
                  e[3]   = (float ( line.split()[7] ) - tai_utc)*UT1__TO__E3
                  er[1]  = float ( line.split()[11] )*ARCSEC__TO__RAD
                  er[2]  = float ( line.split()[10] )*ARCSEC__TO__RAD
                  er[3]  = float ( line.split()[12])*LOD__TO__ER3
#
                  de[1]  = float ( line.split()[14] )*ARCSEC__TO__RAD
                  de[2]  = float ( line.split()[13] )*ARCSEC__TO__RAD
                  de[3]  = float ( line.split()[15] )*UT1__TO__E3
                  der[1] = float ( line.split()[19] )*ARCSEC__TO__RAD
                  der[2] = float ( line.split()[18] )*ARCSEC__TO__RAD
                  der[3] = float ( line.split()[20] )*LOD__TO__ER3
                  fl_append = 1
             elif ( line[5:6] == '.' and line[31:32] == '.' and line[72:73] == '.' ):
#
# --------------- IAA
#
                  mjd_r8 = float ( line.split()[0] )
                  tim = (mjd_r8 - mjd_j2000)*86400.0
                  tai_utc = get_leapsec ( tim, leapsec_list )
                  e[1]   = 0.0
                  e[2]   = 0.0
                  e[3]   = (float ( line.split()[3] ) - tai_utc)*UT1__TO__E3
                  er[1]  = 0.0
                  er[2]  = 0.0
                  er[3]  = 0.0
#
                  de[1]  = 0.0
                  de[2]  = 0.0
                  de[3]  = -float ( line.split()[8] )*UT1__TO__E3
                  der[1] = 0.0
                  der[2] = 0.0
                  der[3] = 0.0
                  fl_append = 1
#
             if ( fl_append == 1 ):
                  tim_str = "%12.3f" % tim 
                  ind_tim = None
                  if ( len(out) > 0 ):
                       if ( tim_str in tim_dic ):
                            ind_tim = tim_dic[tim_str]
                  if ( ind_tim ):
                       out[ind_tim] = eop_format ( tim, e, er, de, der )
                  else:
                       out.append (   eop_format ( tim, e, er, de, der ) )
                       tim_dic[tim_str] = len(out)-1

#
# --- Sort all the lines except the header
#
    out.sort()
    with open ( eop_out_file, 'w' ) as f:
         print ( eop__label, file=f )
         print ( "#", file=f )
         print ( "# Input source: %s" % eop_file_list[0], file=f )
         print ( "# Converted on: %s" % \
                       datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23], file=f )
         print ( "#", file=f )
         for line in out:
             print ( line, file=f )
    f.close() 

#
# ------------------------------------------------------------------------
#
def eop_format ( tim, e, er, de, der ):
    date_str = datetime.datetime.utcfromtimestamp(mjd_j2000_unix_sec + tim).strftime("%Y.%m.%d_%H:%M:%S.%f")
    out = "%s || %13.3f % 14.8e % 14.8e % 14.8e  % 14.8e % 14.8e % 14.8e || % 14.8e % 14.8e % 14.8e  % 14.8e % 14.8e % 14.8e" % \
                 ( datetime.datetime.utcfromtimestamp(mjd_j2000_unix_sec + tim).strftime("%Y.%m.%d_%H:%M:%S.%f")[:-3], \
                   tim, \
                   e[1],   e[2],  e[3],  er[1],  er[2],  er[3], \
                   de[1], de[2], de[3], der[1], der[2], der[3]  )
    return ( out ) 
#
# ------------------------------------------------------------------------
#
def eop_series_download ( config, lh ):

    last_url = None
    for i in range(0,len(config.eop_url)):
        date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
        if ( config.verb > 0 ):
             print ( 'get_eop.py', date_str, "start downloading ", config.eop_pref[i] )
#
# ----- Cycle over eop series
#
        url  = config.eop_url[i]
        typ  = config.eop_type[i]
        pref = config.eop_pref[i]
        ddir = config.eop_download_dir[i]
        loc_file_list = []
#
# ----- Get the list of local files of the same prefix
#
        for paths, dirs, files in os.walk(ddir + "/" + pref):
            for k in range(0,len(files)):
                if ( paths.find("epo") > -1 ): continue
                if ( paths.find("ion") > -1 ): continue
                if ( paths.find("lat") > -1 ): continue
                if ( paths.find("rep") > -1 ): continue
                if ( paths.find("rtp") > -1 ): continue
                if ( paths.find("leo") > -1 ): continue
                if ( paths.find("mge") > -1 ): continue
                if ( paths.find("tro") > -1 ): continue
#
                loc_file_list.append ( paths + "/" + files[k] )
        loc_file_list.sort()
#
        if ( typ == "dir" ):
             date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
             print ( "get_eop.py %s Started  retrieving listing from %s" % \
                      (date_str, url), file=lh )
#
# ---------- Get the listing of the remote directory if the previous URL
# ---------- was different than the URL for the current EOP series
#
             if ( url != last_url ):
                  file_list = []
                  url_list = []
#
# --------------- The time series is presented in a two-level directory tree
# --------------- First get the listing of the first level
#
                  if ( "ftp://" in url ):
                       listing_file = config.eop_temp_dir + "/.listing"
                  else:
                       listing_file = config.eop_temp_dir + "/index.html"
                  if ( os.path.isfile(listing_file) ): os.remove ( listing_file )
#
                  if ( url[0:8] == "https://" ):
                       wget_command = "cd " + config.eop_temp_dir + " ; " + \
                                 "wget " + "--user=" + config.username + \
                                 " --http-password=" + config.password + " " + \
                                  wget_opts + " " + url
                  else:                  
                       wget_command = "cd " + config.eop_temp_dir + " ; " + \
                                      "wget " + wget_opts + " " + url
                  ( ret, out) = exe_out_nolog ( wget_command, config.verb )
                  if ( ret != 0 ):
                       for line in out:
                           print ( line          )
                           print ( line, file=lh )
                       print ( "get_eop.py series_download-1: failure to retrieve " + \
                               "listing of the remote host at " + url + 
                               " with command " + wget_command )
                       print ( "get_eop.py series_download-1: failure to retrieve " + \
                               "listing of the remote host at " + url + 
                               " with command " + wget_command, file=lh )
                       break
#
# --------------- Read the listing
#
                  if ( os.path.isfile(listing_file) ):
                       with open ( listing_file ) as f:
                            listing = f.readlines()
                       f.close ( )
                  else:
                       continue
#
# --------------- ... and remove it
#
                  os.remove ( listing_file )
                  for line in listing:
#
# ------------------- Cycle over subdirectories
#
                      href = None
                      if ( "ftp://" in url ):
                           if ( line.split()[0][0:1] == "d" and \
                                line.split()[8][0:1] != "."     ):
                                href = line.split()[8] 
                      else:
                           if ( "archiveDirText" in line ):
                                href = line.split()[5].replace('"', ' ').split()[1]
                                    
                      if ( href ):
                           if ( "cddis.nasa.gov" in url ):
                                if ( href < cddis_week_min ): continue
#
# ------------------------ Get the listing of a subdirectory
#
                           if ( url[0:8] == "https://" ):
                                     wget_command = "cd " + config.eop_temp_dir + " ; " + \
                                          "wget " + " " + "--user=" + config.username + \
                                          " --http-password=" + config.password + " " + \
                                          wget_opts + " " + url + href + "/"
                           else:
                                     wget_command = "cd " + config.eop_temp_dir + " ; " + \
                                          "wget " + " " + \
                                           wget_opts + " " + url + href + "/"

                           ( ret, out) = exe_out_nolog ( wget_command, config.verb )
                           if ( ret != 0 ):
                                for line in out:
                                    print ( line          )
                                    print ( line, file=lh )
                                print ( "eop_series_download-2: failure to retrieve " + 
                                        "listing of the remote host" )
                                break
                           if ( not os.path.isfile ( listing_file ) ):
                                print ( "get_eop_py: trap of internal control: " + \
                                        "no listing file " + listing_file + " was found " + \
                                        "after command " + wget_command )
                                break
#
# ------------------------ read it the listing file
#
                           with open ( listing_file ) as f:
                                       listing = f.readlines()
                           f.close ( )
#
# ------------------------ ... and remove it
#
                           os.remove ( listing_file )
#
#------------------------- Cycle over files of the subdirectory
#
                           for lin in listing:
                               hhref = None
                               if ( "ftp://" in url ):
                                    if ( lin.split()[0][0:1] == "d" and \
                                         lin.split()[8][0:1] != "."     ):
                                         href = lin.split()[8] 
                               else:
                                    if ( "archiveItemText" in lin ):
                                          hhref = lin.split()[3].replace('"', ' ').split()[1]
                               if ( not hhref ): continue
                               if ( hhref.find("erp.Z")    > 0 or hhref.find("ERP.ERP.gz") > 0 ):
#
# --------------------------------- Select a file that starts with the specified prefix and ends with erp.Z
#
                                    url_list.append  ( url + href + "/" + hhref.strip("\n") )
                                    file_list.append ( hhref.strip("\n") )

             if ( config.verb > 2 ):
                  print ( "Got %d files for type %s pref %s" % ( len(file_list), typ, pref ) )

             for i in range(0,len(file_list)):
                 if ( config.verb > 2 ):
                      print ( "url_list: ", url_list[i] )
                 name_exception = 0
#
# -------------- A special trick for handling CODE dataset that after 2016.06.30 was 
# -------------- renamed from cod to cof
#
                 if ( pref[0:3] == "cod" ):
                      if ( url_list[i].find("/cof") > 0 ):
                           name_exception = 1
                 if ( pref == "igu_u" and "_ERP.ERP.gz" in url_list[i] ):
                      pref_extended = "IGS0OPSULT"
                 elif ( pref == "igr_r" and "_ERP.ERP.gz" in url_list[i] ):
                      pref_extended = "IGS0OPSRAP"
                 elif ( pref == "code_f" and "_ERP.ERP.gz" in url_list[i] ):
                      pref_extended = "COD0MGXFIN"
                 else:
                      pref_extended = pref[0:3]
                 if ( url_list[i].find("/"+pref_extended) > 0 or name_exception == 1 ):
#
# ------------------- Cycle over collected URLs
#
                      loc_fil = ddir + "/" + pref + "/" + file_list[i]
#
# ------------------- Check whether that file aleady exists in the local directory
#
                      if ( config.verb > 2 ):
                           print ( "Check local file", loc_fil, "exist:", os.path.isfile(loc_fil)  ) 
                      age_days = 0.0
                      if ( os.path.isfile(loc_fil) ):
#
# ------------------------ Exist. How old the local file?
#
                           age_days = (time.time() - os.path.getmtime(loc_fil))/86400.0
                           if ( config.verb > 2 ):
                                print ( "Check age of file", loc_fil, "young:", age_days )
                      else:
                           if ( config.verb > 2 ):
                                print ( "Found new file", loc_fil )
#
# ------------------- Check the local file age
#
                      if ( age_days < config.max_file_age ):
#
# ------------------------ If the local file is too young or does not exist , then download it
#
                           filout_tmp = file_list[i]  + "_" + "%08d" % os.getpid()
                           if ( url_list[0][0:8] == "https://" ):
                                wget_command = "cd " + ddir + "/" + pref + " ; " + \
                                               "wget " + " -O " + filout_tmp + " " + \
                                               "--user=" + config.username + \
                                               " --http-password=" + config.password + " " + \
                                               wget_opts + " " + url_list[i]
                           else:
                                wget_command = "cd " + ddir + "/" + pref + " ; " + \
                                               "wget " + " -O " + filout_tmp + " " + \
                                                wget_opts + " " + url_list[i]
                          
                           if ( config.verb > 2 ):
                                print ( "Downloading eop: ", wget_command )
                           ( ret, out) = exe_out_log ( wget_command, config.verb, lh )
                           if ( ret != 0 ):
                                for line in out:
                                     print ( line          )
                                     print ( line, file=lh )
                                print ( "eop_series_download-3: failure to " + \
                                        "retrieve remote file", \
                                         url_list[i], file=lh )
                                print ( "eop_series_download-3: failure to " + \
                                        "retrieve remote file", \
                                         url_list[i] )
                                break
#
# ------------------------ Check bad magic
#
                           with open(ddir + "/" + pref + "/" + filout_tmp,"rb") as f:
                                magic = f.read(15)
                           f.close()         
                           if ( magic == b'<!DOCTYPE html>' ):
                                print ( "eop_series_download-4. Because of CDDIS is down: " + \
                                        "failure to retrieve remote file", \
                                         url_list[i], file=lh )
                                print ( "eop_series_download-4. Because of CDDIS is down: " + \
                                        "failure to retrieve remote file", \
                                         url_list[i] )
                                os.remove ( filout_tmp )
                                break
                           elif ( filout_tmp[-3:] == ".gz" and magic[0:2] != b'\x1f\x8b' ):
                                print ( "eop_series_download-5. Bad gzip magic in retrieved " + \
                                        "remote file", url_list[i], file=lh )
                                print ( "eop_series_download-5. Bad gzip magic in retrieved " + \
                                        "remote file", url_list[i] )
                                os.remove ( filout_tmp )
                                break
                           elif ( filout_tmp[-4:] == ".bz2" and magic[0:3] != b'BZh' ):
                                print ( "eop_series_download-6. Bad bzip2 magic in retrieved " + \
                                        "remote file", url_list[i], file=lh )
                                print ( "eop_series_download-6. Bad bzip2 magic in retrieved " + \
                                        "remote file", url_list[i] )
                                os.remove ( filout_tmp )
                                break
                                
#
# ------------------------ Move the downloaded file to the permanent location
#
                           move_com = "cd " + ddir + "/" + pref + " ; " + \
                                      "mv " + filout_tmp + " " + file_list[i]
                           ( ret, out) = exe_out_log ( move_com, config.verb, lh )
                           date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                           if ( config.verb > 1 ):
                                print ( "get_eop.py %s: Downloaded %s " % ( date_str, file_list[i] ), file=lh )
             date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
             print ( "get_eop.py %s Finished retrieving files from %s" % \
                      (date_str, url), file=lh )

        elif ( typ == "file" ):
#
# ---------- Type: file
#
             fil = ddir + "/" + pref + "/" + url[url.rfind("/")+1:]
             filout_tmp = url[url.rfind("/")+1:] + "_" + "%08d" % os.getpid()
#
# ---------- Download the file to a temporary location first
#
             if ( url[0:8] == "https://" ):
                  wget_command = "cd " + ddir + "/" + pref + " ; " + \
                                 "wget " + " -O " + filout_tmp + " " + \
                                 " --user=" + config.username + \
                                 " --http-password=" + config.password + " " + \
                                 wget_opts + " " + url
             else:
                  wget_command = "cd " + ddir + "/" + pref + " ; " + \
                                 "wget " + " -O " + filout_tmp + " " + wget_opts + " " + url
             ( ret, out) = exe_out_nolog ( wget_command, config.verb )
             if ( ret != 0 ):
                  for line in out:
                      print ( line          )
                      print ( line, file=lh )
                  print ( "eop_series_download-6: failure to retrieve a file " + 
                          "from the remote host with command " + wget_command )
                  print ( "eop_series_download-6: failure to retrieve a file " + 
                          "from the remote host with command " + wget_command, file=lh )
#
# ------------------------ Check bad magic
#
             else:
                  full_filout_tmp = ddir + "/" + pref + "/" + filout_tmp
                  with open(full_filout_tmp,"rb") as f:
                       magic = f.read(15)
                  f.close()         
#
                  if ( magic == b'<!DOCTYPE html>' ):
                       print ( "eop_series_download-7. Because of CDDIS is down: " + \
                               "failure to retrieve remote file", \
                               url_list[i], file=lh )
                       print ( "eop_series_download-7. Because of CDDIS is down: " + \
                               "failure to retrieve remote file", \
                               url_list[i] )
                       os.remove ( filout_tmp )
                          
                  else:
#
# -------------------- and then move the downloaded file to the permanent location
#
                       move_com = "cd " + ddir + "/" + pref + " ; " + \
                                  "mv " + full_filout_tmp + " " + fil
                       ( ret, out) = exe_out_log ( move_com, config.verb, lh )
                       date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
                       print ( "get_eop.py %s: Downloaded %s " % ( date_str, url[url.rfind("/")+1:] ), file=lh )
        sys.stdout.flush()

        last_url = url # Store the last URL
   
    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
    if ( config.verb > 0 ):
         print ( 'get_eop.py', date_str, "Finished downloading" )
    sys.stdout.flush()

#
# =======================================================================
#
def eop_latencies ( config ):
#"""
#    Compute latencies of input EOP series 
#"""
    eop_last_date = []
    suffix_list   = []
    pref_lat={}
    eop_file = None
    buf = []
    for pref in config.eop_pref:
        eop_file = config.eop_dir + "/" + pref + ".eop"
        with open ( eop_file ) as f:
             buf = f.readlines()
        f.close ( )
        suffix_list.append ( pref[-1:] )
        eop_last_date.append ( buf[len(buf)-1][0:19] )
        pref_lat[pref] = buf[len(buf)-1][0:19] 

#
# --- Add AAM forecast
#
    suffix_list.append ( "a" )
    with open ( config.aam_ser_file ) as f:
         buf = f.readlines()
    f.close()
    eop_last_date.append ( buf[len(buf)-1][0:19] )
    pref = "geosfcs_a"
    pref_lat[pref] = buf[len(buf)-1][0:19].replace("-","_")

#
# --- Add NERS forecast
#
    with open ( config.ners_stop_file ) as f:
         buf = f.readlines()
    f.close()
    try:
        eop_last_date.append ( buf[len(buf)-1][0:19] )
    except:
        print ( "get_eop.py: file %s  has %d lines" % ( config.ners_stop_file, len(buf) ) )
        print ( "get_eop.py: Cannot append buf[len(buf)-1][0:19]" )
        exit  ( 1 )
    pref = "ners_n"
    pref_lat[pref] = buf[len(buf)-1][0:19].replace("-","_")
    suffix_list.append ( pref[-1:] )

    if ( pyvers >= "3.12" ):
         date_utc_now = datetime.datetime.now(datetime.UTC).strftime("%Y.%m.%d_%H:%M:%S")
         now_utc      = datetime.datetime.now(datetime.UTC).replace(tzinfo=None)
    else:
         date_utc_now = datetime.datetime.utcnow().strftime("%Y.%m.%d_%H:%M:%S")
         now_utc      = datetime.datetime.utcnow()
    
    str_out = date_utc_now + " " + "".join(suffix_list) + " " + " ".join(eop_last_date)
 
    f=open  ( config.eop_stat_file, "a" )
    print ( str_out, file=f )   
    f.close()

    f = open ( config.eop_stat_html, "w" )
#
# --- Write the title
#
    f.write ( "<BIG>Status at " + date_utc_now + \
                 " <B>UTC</B> </BIG> &nbsp; &nbsp; &nbsp; &nbsp; ( " + \
              date_utc_now + \
              " local time ) \n" )
    f.write ( "<P>\n" )
#
# --- Write the table header
#
    f.write ( '<TABLE CELLSPACING="2" CELLPADDING="10" BORDER="1" >\n' )
    f.write ( '<TR><TD>Data type </TD>' + \
              '<TD><B><I> Series name </I></B></TD>\n' + \
              '<TD ALIGN="CENTER"><I> Latency </I></TD>\n' + \
              '<TD NOWRAP> Last time updated (UTC)</TD>\n' + \
              '<TD ALIGN="RIGHT" NOWRAP> Latency plot (long) </TD>\n' + \
              '<TD ALIGN="RIGHT" NOWRAP> Latency plot (short) </TD>\n' + \
              '</TR>\n' )
    for code in config.series_code:
        config.series_type[code]
        config.series_units[code]
        config.series_name[code]        
        f.write ( '  <TR><TD> ' + config.series_type[code] + ' series </TD>\n' )
        f.write ( '      <TD> <B> ' + config.series_name[code] + ' </B></TD>\n' )
        if ( config.series_type[code] == "EOP" or \
             config.series_type[code] == "AAM" ): 
             try:
                  date_eop = datetime.datetime.strptime ( pref_lat[code], "%Y.%m.%d_%H:%M:%S" )
             except:
                  date_eop = datetime.datetime.strptime ( "2022.01.01_00:00:00", "%Y.%m.%d_%H:%M:%S" )
             lat = (now_utc - date_eop).total_seconds()
             if ( config.series_units[code] == "hour" ):
                  lat = lat/3600.0
             else:
                  lat = lat/86400.0

             if ( lat > config.series_crit[code] ):
                  color_tag = '<FONT COLOR="800000">' # red
             elif ( lat > config.series_good[code] ):
                  color_tag = '<FONT COLOR="E0B848">' # yellow
             else:
                  color_tag = '<FONT COLOR="008000">' # green

             f.write ( '      <TD ALIGN="RIGHT" NOWRAP>' + color_tag + '<B> ' + \
                         '%8.2f %s' % ( lat, config.series_units[code] ) + \
                         ' </B> </FONT> </TD>\n' )
             try:
                  f.write ( '      <TD ALIGN="RIGHT" NOWRAP> ' + pref_lat[code] + ' </TD>\n' )
             except:
                  f.write ( '      <TD ALIGN="RIGHT" NOWRAP> ' + "2022.01.01_00:00:00" + ' </TD>\n' )
        elif ( config.series_type[code] == "NERS" ):
             n = open ( config.ners_stop_file, 'r' )
             ners_date = n.readline()[0:19].replace("-","_")
             n.close()
             try:
                 date_eop = datetime.datetime.strptime ( ners_date, "%Y.%m.%d_%H:%M:%S" )
             except:
                 print ( "Unsupported format of stop date in ", config.ners_stop_file )
                 print ( "ners_date: ", ners_date )
                 exit  ( 1 )
             lat = (now_utc - date_eop).total_seconds()
             if ( config.series_units[code] == "hour" ):
                  lat = lat/3600.0
             else:
                  lat = lat/86400.0

             if ( lat > config.series_crit[code] ):
                  color_tag = '<FONT COLOR="800000">' # red
             elif ( lat > config.series_good[code] ):
                  color_tag = '<FONT COLOR="E0B848">' # yellow
             else:
                  color_tag = '<FONT COLOR="008000">' # green

             f.write ( '      <TD ALIGN="RIGHT" NOWRAP>' + color_tag + '<B>' + \
                       '%8.2f %s' % ( lat, config.series_units[code] ) + \
                       ' </B> </FONT> </TD>\n' )
             f.write ( '      <TD ALIGN="RIGHT" NOWRAP> ' + ners_date + ' </TD>\n' )
   
        f.write ( '      <TD ALIGN="LEFT"><A HREF="' + config.ners_plot_dir + '/latency_' + \
                         code + '_2.gif">' + "latency of " + code + ' </A> </TD>\n' )
        f.write ( '      <TD ALIGN="LEFT"><A HREF="' + config.ners_plot_dir + '/latency_' + \
                         code + '_1.gif">' + "Latency of " + code + ' </A> </TD>\n' )
        f.write ( '  </TR>\n' )

    f.write ( '</TABLE>' )
    f.close()

#
# =======================================================================
#
def parse_eop_config ( config ):
#"""
#    Reads the configuration file which has format KEYWORD: VALUE 
#    and puts parsed information into fields of class config
#"""
   with open ( config.filename  ) as f:
        conf_buf = f.readlines()
   f.close ( )

   if ( conf_buf[0][0:len(fmt__label)] != fmt__label[0:len(fmt__label)] ):
        Message ( "F", "Unsupported config file " + config.filename + \
                  "\n Format label found:   " + conf_buf[0] + \
                  "\n While expected label: " + fmt__label + "\n" )
        exit ( 1 )

   
   num_par = 0
   for line in conf_buf:
       if ( line == fmt__label ): continue
       if   ( line[0:1] == "#" ): continue

       if ( len ( line.split() ) < 2 ): 
            print ( "Unrecognized line " + line + \
                      " in control file " + config.filename + \
                      " -- too few words" )
            exit ( 1 )
            
       elif ( line.split()[0]     == "username:" ):
              config.username = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "password:" ):
              config.password = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "aam_ser_file:" ):
              config.aam_ser_ref  = line.split()[1]
              config.aam_ser_file = line.split()[2]
              num_par = num_par + 1
       elif ( line.split()[0]     == "eop_fcs_pref:" ):
              config.eop_fcs_pref = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "ners_plot_dir:" ):
              config.ners_plot_dir = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "eop_file:" ):
              if ( len ( line.split() ) < 4 ): 
                   print ( "Unrecognized line " + line.split() + \
                           " in control file " + config.filename + \
                           " -- there should be 4 words" )
                   exit ( 1 )
              if ( len(config.eop_url) == 0 ):
                   num_par = num_par + 1
              config.eop_url.append          ( line.split()[1] )
              config.eop_type.append         ( line.split()[2] )
              config.eop_pref.append         ( line.split()[3] )
              config.eop_download_dir.append ( line.split()[4] )
       elif ( line.split()[0]     == "ners_url:"  ):
              config.ners_url      = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "max_file_age:"  ):
              config.max_file_age  = float(line.split()[1])
              num_par = num_par + 1
       elif ( line.split()[0]     == "leapsec_file:" ):
              config.leapsec_file  = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "eop_dir:" ):
              config.eop_dir       = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "heo_file:"  ):
              config.heo_file      = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "prc_apr_mod:"  ):
              config.prc_apr_mod   = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "nut_apr_mod:"  ):
              config.nut_apr_mod   = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "e3z_mod:"  ):
              config.e3z_mod       = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "heo_mod:"  ):
              config.heo_mod       = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "heo_id:"  ):
              config.heo_id        = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "ltp_mod:"  ):
              config.ltp_mod       = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "ners_stop_file:"  ):
              config.ners_stop_file = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "eop_stat_file:" ):
              config.eop_stat_file = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "series:" ):
              code = line.split()[1] 
              config.series_code.append ( code )
              config.series_units[code] = line.split()[2]
              config.series_good[code]  = float(line.split()[3])
              config.series_crit[code]  = float(line.split()[4])
              config.series_type[code]  = line.split()[5]
              config.series_name[code]  = ""
              for ind in range(6,len(line.split())):
                  config.series_name[code] = config.series_name[code]  + \
                                     " " + line.split()[ind] 
              if ( len(config.series_code) == 1 ): num_par = num_par + 1
       elif ( line.split()[0]     == "eop_stat_html:" ):
              config.eop_stat_html = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "eop_fcs_ser:"   ):
              config.eop_fcs_ser   = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "eop_temp_dir:"  ):
              config.eop_temp_dir  = line.split()[1] 
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
       elif ( line.split()[0]     == "err_file:"  ):
              config.err_file      = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "suc_file:"  ):
              config.suc_file      = line.split()[1] 
              num_par = num_par + 1
       elif ( line.split()[0]     == "lock_timeout:"  ):
              config.lock_timeout  = float(line.split()[1])
              num_par = num_par + 1
       else:
              print ( "Unrecognized keyword " + line.split()[0] + \
                      " in control file " + config.filename )
              exit ( 1 )

   if ( num_par < config__num_par ):
        print ( "Not all keywords were found in control file " + \
                 config.filename + " -- only %d, though %d were expected" % \
                 ( num_par, config__num_par ) )
        exit ( 1 )
#
# ------------------------------------------------------------------------
#
def clean_eop_ser ( eop_buf ):

    for i in range(0,len(eop_buf)):
        eop_buf[i] = eop_buf[i].decode("iso-8859-1").strip("\n")

    for i in range(0,len(eop_buf)):
        line = eop_buf[i] 
        if ( line[0:1] == '#' ): continue
        if ( line[6:7] == '.' and line[34:35] == '.' ):
             best = 0
             mul  = 0
             for j in range(1,16):             
                 if ( i+j >= len(eop_buf) ): break
                 lin = eop_buf[i+j]
                 if ( lin[6:7] == '.' and lin[34:35] == '.' ):
#
# ------------------- Check for records with the same epochs
#
                      if ( line.split()[0] == lin.split()[0] ):
                           mul = mul + 1
                           if ( lin.split()[8] < line.split()[8] ):
#
# ------------------------------ If the records with index i+j is has 
# ------------------------------ the smallest uncertainy, it is the best
#
                                 best = j
             if ( mul > 0 ):
                  for j in range(0,16):             
                      if ( i+j >= len(eop_buf) ): break
                      lin = eop_buf[i+j]
                      if ( lin[6:7] == '.' and lin[34:35] == '.' ):
                           if ( line.split()[0] == lin.split()[0] ):
                                if ( j != best ):
#
# ---------------------------------- Comment out not-best records
#
#                                     print ( "commented out ", i+j, eop_buf[i+j], ' best: ', eop_buf[i+best] )
                                     eop_buf[i+j] = '#' + eop_buf[i+j] 

    return eop_buf

#
# ------------------------------------------------------------------------
#
def get_leapsec ( tim, leapsec_list):

    leapsec = None
    if ( tim <= leapsec_list[0][0] ): leapsec = 0.0
    for tim_leap in leapsec_list:
        if ( tim >= tim_leap[0] ): 
             leapsec = tim_leap[1] 
#    print ( "tim= ", tim, ' ls = ', leapsec ) # %%%
    return leapsec

#
# ------------------------------------------------------------------------
#
def load_leapsec ( leapsec_file ):

    with open(leapsec_file) as f:
         leap_buf = f.readlines()
    f.close()
    
    leapsec_list = []
    for line in leap_buf:
        if ( len(line)  <  8  ): continue
        if ( line[0:1] == '#' ): continue
        if ( len(line.split())  <  4  ): continue
        if ( line.split()[0] == "Date:" ):
             tim = calendar.timegm(time.strptime( line.split()[1][:-2], '%Y.%m.%d_%H:%M:%S')) - \
                   calendar.timegm(time.strptime('2000.01.01_00:00:00', '%Y.%m.%d_%H:%M:%S'))
             leapsec_list.append ( [tim, float(line.split()[3])] )
         
    return leapsec_list
#
# ------------------------------------------------------------------------
#
def main():
    os.putenv ( "GOMP_STACKSIZE", "2000000")

    opts = optparse.OptionParser( version=get_eop__label  )

    opts.add_option ( "-c", "--config", action="store", \
                      dest="config", \
                      metavar="NAME", \
                      help="Configuration file" )

    opts.add_option ( "-r", "--run-level", action="store", \
                      dest="run_level", \
                      default=0, \
                      type="int", \
                      metavar="NAME", \
                      help="Run level" )

    opts.add_option ( "-l", "--latencies", \
                      dest="lat", \
                      action="store_true", \
                      help="EOP latencies only" )

    opts.add_option ( "-v", "--verbosity", action="store", \
                      dest="ivrb", \
                      default=0, \
                      type="int", \
                      metavar="NAME", \
                      help="Verbosity level" )

    opts.add_option ( "-f", "--file", action="store", \
                      dest="file", \
                      default=None, \
                      metavar="NAME", \
                      help="File to process" )
#
# --- Get and parse options
#
    opts, args = opts.parse_args()


#
# --- Check option values
#
    if ( opts.config == None ):
         print ( "Configuration file is not specied. Try get_merra.py -h to see options" )
         exit ( 1 )

    if ( not os.path.isfile(opts.config) ):
         print ( "Configuration file ", opts.config, " does not exist" )
         exit ( 1 )
         
    if ( opts.ivrb == None ):
         opts.ivrb = 0
        
    config = eop_config_class ( opts.config, opts.ivrb ) 

    parse_eop_config    ( config )

    if ( opts.lat ):
         eop_latencies ( config )
         exit ( 0 )

    lh = open ( config.log_file, "a" )
    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
    print ( " ".join(sys.argv) )
    print ( " ".join(sys.argv), file=lh )
    print ( "Started  get_eop.py on", date_str, file=lh )
    print ( "=============================================", file=lh )

    config.leapsec_list = load_leapsec ( config.leapsec_file )

    if ( opts.run_level == 1 or opts.run_level == 0 ):
         eop_series_download ( config, lh )

    if ( opts.run_level == 2 or opts.run_level == 0 ):
         eop_series_transform ( config, lh )

    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
    print ( "Finished get_eop.py on", date_str, file=lh )
    print ( "============================================", file=lh )
    print ( " ", file=lh )

if __name__ == "__main__":
    vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
    if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
    main()
