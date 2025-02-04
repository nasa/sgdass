import sys, os, signal, datetime
from   vsdc_config   import *
from   vsdc_misc     import *

#
# ------------------------------------------------------------------------
#
def vsdc_master_download ( vsdc ):
    """
    Download master files from the remote URL specified 
    in the VSDC control file
    """
    this_year = int(datetime.datetime.now().strftime("%Y"))

    if ( vsdc.data_center.lower() == "cddis" ):
         netrc_str = read_file ( vsdc.netrc_file )
         if ( len(netrc_str[0]) < 6 ):
              print ( "Wrong  netrc file %s -- it should have six words" % vsdc.netrc_file )
              return 1

         username = netrc_str[0].split()[3]
         password = netrc_str[0].split()[5]


    for year in range (vsdc__master_year_1st,this_year+2):
#
# ----- Master file have two-digit dates embedded in their names
#
        for suff in vsdc_master_suffix:
            if ( year >= vsdc__master_year_2nd ):
                 master_file = "master%04d%s" % ( year, suff )
            else:
                 if ( year < 2000 ):
                      master_file = "master%02d%s" % ( (year - 1900), suff )
                 else:
                      master_file = "master%02d%s" % ( (year - 2000), suff )
#
# --------- Build the fill URL name
#
            url = vsdc.master_url + "/" + master_file
#
# --------- Build the command for downloading. If the master file exists,
# --------- we rename it and adding suffix ".old"
#
            if ( vsdc.data_center.lower() == "cddis" ):
                 com = "cd " + vsdc.master_dir + "; " + \
                       "mv " + master_file + " " + master_file + ".old; "  + \
                       "wget "                        + \
                       vsdc.wget_extra_opts     + " " + \
                       "--auth-no-challenge "         + \
                       "--user=" + username     + " " + \
                       "--http-password=" + password + " " + \
                       url + "; " + \
                       "chmod g+rw,o+rw,u+rw " + master_file
            else:
                  com = "cd " + vsdc.master_dir + "; " + \
                        "mv " + master_file + " " + master_file + ".old; "  + \
                        "wget "                        + \
                        vsdc.wget_extra_opts   + " "   + \
                        url                    + "; "  + \
                        "chmod g+rw,o+rw,u+rw " + master_file
               
            if ( vsdc.verb > 1 ): print ( "com = ", com )
            (ret,out) = vsdc_exe ( com )
            if ( ret != 0 and suff != ".txt" ): continue
            if ( ret != 0 and year < this_year + 1 ):
                 for line in out:
                     print ( line )
                 print ( "Failure in downloadning ", url )
#
# -------------- Restore the old master file
#
                 com = "cd " + vsdc.master_dir + "; " + \
                       "mv " + master_file + ".old " + master_file
                 (ret,out) = vsdc_exe ( com )
                 return 1
            else:
#
# -------------- Remove the old master file
#
                 com = "cd " + vsdc.master_dir + "; " + \
                       "rm " + master_file + ".old"
                 (ret,out) = vsdc_exe ( com )
    if ( this_year == 2023 ):
#
# ------ Clean up of pre-2023 master files
#
         for path, dirs, files in os.walk(vsdc.master_dir):
             for file in files:
                 if ( file == "master23.txt" or file == "master23-int.txt" ):
#
# ------------------- Remove master files for 2023 in the old format.
#
                      file_to_remove = path + "/" + file 
                      try:
                           os.unlink ( file_to_remove )
                      except BaseException as e: 
                           print ( "Could not remove version 1.0 master file %s because, nevertheless, continue" % \
                                   ( file_to_remove, str(e) ) )
                      print ( "vsdc_master: Removed version 1.0 master file %s" % file_to_remove )
                          
    return 0
#
# ------------------------------------------------------------------------
#
def vsdc_master_read ( vsdc ):
    """
    Read all master files in the directory at the local computer
    specified in the VSDC configuratioin file, parse their context,
    sort and store in vsdc.master dictionary
    """
    vsdc.master = {}
#
# --- Cycle over master files in the master directory
#
    for path, dirs, files in os.walk(vsdc.master_dir):
        for file in files:
#
# --------- Bypoass srtray files
#
            if ( "#" in file ):   continue
            if ( "~" in file ):   continue
            if ( len(file) < 8 ): continue
            if ( file[0:6] == "master" and file[-4:] == ".txt" ):
#
# -------------- Aga, this is a master file
#
                 master_file = path + "/" + file 
                 if ( "int" in file ):
                      master_type = "int"
                 else:
                      master_type = "24"
                 buf = read_file ( master_file )
                 if ( "<!DOCTYPE html>" in buf[0] ): continue
                 master_version = buf[0].split()[5]
                 if ( not ( master_version == "1.0" or master_version == "2.0" ) ):
                      print ( "vsdc_master: unrecognized version of the master schedule file %s " % master_version )
                      print ( "vsdc_master: supported version: 1.0 and 2.0" )
                 for line in buf:
#
# ------------------ Extract the year from the master file
#
                     if ( "MULTI-AGENCY" in line ):
                          year = int ( line.split()[0] )
                     if ( line[0:1] == '|' ):
#
# ----------------------- Extract session, date string and convert it to date in internal Python format
#
                          ib = 0
                          ip = [0]
                          for i in range(0,vsdc__num_mf):
                              try:
                                  ib = line.find("|",ib+1)
                                  ip.append ( ib )
                              except:
                                  continue
                          if ( master_version == "1.0" ):
                               sess_name = line[ip[1]+1:ip[2]].strip(" ").lower()
                               date_str  = "%4d" % year + line[ip[2]+1:ip[3]].upper()
                               time_str  =                line[ip[4]+1:ip[5]].upper()
                               corr_str  =                line[ip[8]+1:ip[9]].upper()
                               sess_suff = line[ip[11]+1:ip[12]].strip(" ").upper()
                               try:
                                   sess_date = datetime.datetime.strptime ( date_str + "_" + time_str, \
                                                                            '%Y%b%d_%H:%M' ) 
                                   vsdc.master[sess_name] = {"date":     sess_date,   \
                                                             "date_str": date_str,    \
                                                             "suffix":   sess_suff,   \
                                                             "type":     master_type, \
                                                             "corr":     corr_str     }
                               except:
                                   continue
                          else:
                               sess_name = line[ip[2]+1:ip[3]].strip(" ").lower()
                               date_str  = line[ip[1]+1:ip[2]].upper()
                               time_str  = line[ip[4]+1:ip[5]].upper()
                               corr_str  = line[ip[8]+1:ip[9]].upper()
                               sess_suff = line[ip[10]+1:ip[11]].strip(" ").upper()
                               try:
                                   sess_date = datetime.datetime.strptime ( date_str + "_" + time_str , '%Y%m%d_%H:%M' ) 
                                   vsdc.master[sess_name] = {"date": sess_date,       \
                                                             "date_str": date_str,    \
                                                             "suffix":   sess_suff,   \
                                                             "type":     master_type, \
                                                             "corr":     corr_str     }
                               except:
                                   continue

    if ( vsdc.verb > 3 ):
         sess_list = []
         sess_list = sorted ( list(vsdc.master.keys()) )
         for sess_name in sess_list:
             print ( "sess_name= ", sess_name )
    return ( 0 )
