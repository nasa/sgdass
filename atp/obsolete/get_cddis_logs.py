#!/usr/bin/python3
# *******************************************************************************
# *                                                                             *
# *   Routine get_cddis_logs.py                                                 *
# *                                                                             *
# *  ### 29-JAN-2024  get_cddis_logs.py  v1.0 (c)  N. Habana  29-JAN-2024  ###  *
# *                                                                             *
# *******************************************************************************
#
import pwd, sys, os, re, shutil, time, subprocess, datetime, argparse
sys.path.append('/home/nhabana/bin')
from   pet_misc  import *
#
get_cddis_logs__label = "get_cddis_logs 2024.01.30 1.0"
#
direxp       = "/home/nhabana/work/temp_f2/vlbi"
vsdc_dir     = "/opt64/bin"
vlbi_sta_nam = "/apr/sta/vlbi_station.names"
year_min = 2022
year_max = 2023
dc_list = [ "https://cddis.nasa.gov/archive/vlbi/ivsdata/aux" ]
stat_cddis = "https://cddis.nasa.gov/archive/vlbi/ivsdata/aux"
master_fil_dir  = "/home/nhabana/work/temp_f2/anc/vlbi/master_files"
master_list_fil = "/home/nhabana/work/temp_f2/anc/vlbi/master_list.txt"
dir_fs = "/q0/fs_logs"
log_list_files = "/home/nhabana/work/temp_f2/anc/vlbi/fs_logs_list.txt"
#
# -- Read master list to buffer
#
master_list  = read_file ( master_list_fil )
#
master__num_mf = 32

print ("!!!!!WE ARE HERE1!!!!!")
print (dir_fs)

#
# ======================================================================
# ==========  FUNCTION TO GET FS DIRECTORY TREE FROM SYSTEM   ==========
# ======================================================================
#
def get_os_fslog_list ( dir_fs ):
    print ("!!!!!WE ARE HERE!!!!!")

#
# -- Read file with list of logs in the current system
#
#    log_list_buf = read_file ( log_list_files )
    log_list_buf = []
    for path, dirs, files in os.walk ( dir_fs ):
        print (dirs)
        for fil in files:
            print (fil)
            log_list_buf.append( path + "/" + fil )
#
    return log_list_buf

#
# ======================================================================
# ==========                   MAIN ROUTINE                   ==========
# ======================================================================
#
def main():
    
    print ("@@@@@@@@@ WHAT ABUT HURR3 @@@@@@")
    get_os_fslog_list ( dir_fs )
    print ("@@@@@@@@@ WHAT ABUT HURR @@@@@@")
#
# -- Write buffer to file
#
    (ret,err) = write_file ( log_list_buf, log_list_files )
    check_err_exe ( ret, err, "write_file" )

#    check_err_exe ( ret, err, "get_os_fslog_list" )

    


exit(1)













#
# -- Fetch only master files in range
#
master_list_use = []
for master in master_list:
    iyr = int( master[6:8] )
    if ( iyr >= 90 ):
         master_year = 1900 + iyr
    else:
         if ( iyr == 20 ):
              if ( master[8] == "." or master[8] == "-" ):
                   iyr = iyr
              else:
                   iyr = int(master[8:10])
         master_year = 2000 + iyr
    if ( master_year >= year_min and master_year <= year_max ): 
         master_list_use.append ( master )

#
# -- Get the full paths to the Masterfiles that are applicable to our range
#
master_list = []
for path, dirs, files in os.walk(master_fil_dir):
    for file in files:
        if ( file in master_list_use ):
             master_list.append ( path + "/" + file )
master_list.sort()
#
# -- Read the files of interest to buffer and extract their information 
#
master_dict = {}
for master_file in master_list:
#
# -- read current file to buffer
#
    buf = read_file ( master_file )
#
# -- Account for change in formatting since 2023
# 
    master_fmt = buf[0].split()[5]
#
# -- Read all lines in the buffer
#
    for line in buf:
#
# ----- Get the year of schedules from file text
#

        if ( "MULTI-AGENCY" in line ):
             year = int ( line.split()[0] )
             year_chr = line.split()[0][2:4]
#
# ----- index of bars, "|" and array of their 
#
        ib = 0
        ip = [0]
#
# ----- 
#
        if ( line[0:1] == '|' ):
#
# ---------- Get an array of the indeces of the "|" up to master__num_mf of them
#
             for i in range(0,master__num_mf):
                  try:
                       ib = line.find("|",ib+1)
                       ip.append ( ib )
                  except:
                       continue
#
# ---------- Get information based on file format type
#
             if ( master_fmt == "2.0" ):
                  sess_name = line[ip[2]+1:ip[3]].strip(" ")
                  sess_date = datetime.datetime.strptime ( line[ip[1]+1:ip[2]], "%Y%m%d" )
                  suff = line[ip[10]:ip[11]].replace("|", " ").ljust(20).strip()
                  dur_str = line[ip[5]+1:ip[6]]
                  sess_dur = int(dur_str[0:2]) + int(dur_str[3:5])/60.0                         # [Hrs]
                  db_name = datetime.datetime.strftime( sess_date, "%Y%b%d" ).upper()[2:] + \
                                suff.replace(" ","").upper()
                  stn_lst = line[ip[6]+1:ip[7]].lower()
             elif ( master_fmt == "1.0" ):
                  if ( line[32:43] == '|     |   |' ):
                       line = line.replace ( '|     |   |', '|12:00|1.0|' )
                       sess_name = line[ip[1]+1:ip[2]].strip(" ")
                       sess_date = datetime.datetime.strptime ( "%4d" % year + line[ip[2]+1:ip[3]], "%Y%b%d" )
                       suff = line[ip[11]:ip[12]].replace("|", " ").replace(" X","").replace(" V","").replace(" ","")
                       dur_str = line[ip[5]+1:ip[6]]
                       db_name = year_chr + line[ip[2]+1:ip[3]].strip(" ") + \
                                 line[ip[11]+1:ip[12]].replace(" ","") 
                       stn_lst = line[ip[5]+1:ip[6]].lower()
#
                       if ( len(dur_str.strip(" ")) > 0 ):
                            sess_dur = float(dur_str)
                       else:
                            sess_dur = 25
             else:
                  print ( "Unsupported format of master file %s" % master_file )
                  exit  ( 1 )
#
# ---------- Generate a unique ditionary name
#
             sess_temp_code = datetime.datetime.strftime ( sess_date, "%Y%m%d" ) + \
                                  "_" + suff
             master_dict[sess_temp_code] = { "db_name"   : db_name.upper(),   \
                                        "sess_name" : sess_name.lower(), \
                                        "sess_date" : sess_date,         \
                                        "sess_dur"  : sess_dur,          \
                                        "stn_list"  : stn_lst              }
#
# -- Go through the master dictionary and extract each station on the station list
#
master_paths = []
#cnt = 0
for key in master:
 #   cnt += 1
  #  if (cnt > 5): break
  #  print(cnt, "db_name:   ", master[key]["db_name"],   \
   #            "sess_name: ", master[key]["sess_name"], \
    #           "sess_date: ", master[key]["sess_date"], \
     #          "sess_dur:  ", master[key]["sess_dur"],  \
      #         "stn_list:  ", master[key]["stn_list"]     )
#
# -- Get the session name
# 
    sess_name = master[key]["sess_name"]
#
# -- Get the year of the experiment
#
    sess_date =  master[key]["sess_date"]
    sess_yr   =  sess_date.strftime("%Y")
#
# --- Get the station list for this session
#
    stn_tot = []
    stn_row = master[key]["stn_list"]
    ind = stn_row.find("-")
#
# -- If there are no stations that were removed from this session
#
    if ( ind == -1 ):
        stn_row_len = len(stn_row.replace(" ", ""))
#
# ----- Get every two characters to define # of stations
#
        no_stns = int(stn_row_len/2)
        for i in range(0,no_stns):
            ilb = 2*i                                   # index of lower boundary
            iub = ilb + 2                               # index of upper boundary
            stn_tot.append( stn_row[ilb:iub] )
    else:
        no_stns = int((ind-1)/2)
        for i in range(0,no_stns):
            ilb = 2*i                                   # index of lower boundary
            iub = ilb + 2                               # index of upper boundary
            stn_tot.append( stn_row[ilb:iub] )
        
 #   print ( "SESS_NAME: ", sess_name, "SESS_DATE: ", sess_date, "YEAR: ", sess_yr, "STNS: ", stn_tot, "\n" )

#
# -- check if a given log file exists
#PATH = https://cddis.nasa.gov/archive/vlbi/ivsdata/aux/YYYY/SESS/filename
#filename = SESSSta.log
#    print( type(stat_cddis) )
#    print( type(sess_yr)    )
#    print( type(sess_name)  )
#    print( type(stn_tot[i]) )

    for i in range(0,no_stns):
        url = stat_cddis + "/" + sess_yr + "/" + sess_name + "/" + \
              sess_name  + stn_tot[i] + "_full.log"
        master_paths.append(url)



fil_url_list = "/home/nhabana/temp/cddis_url_list.txt"
(ret,err) = write_file ( master_paths, fil_url_list )
check_err_exe ( ret, err, "write_file" )


#exit(1)





#
# ======================================================================
# ==========  FUNCTION TO GET FS DIRECTORY TREE FROM SYSTEM   ==========
# ======================================================================
#
def get_os_fslog_list ( dir_fs ):
#
# -- Read file with list of logs in the current system
#
#    log_list_buf = read_file ( log_list_files )
    log_list_buf = []
    for path, dirs, files in os.walk ( dir_fs ):
        for fil in files:
            print (fil)
            log_list_buf.append( path + "/" + fil )
#
# -- Write buffer to file
#
    (ret,err) = write_file ( log_list_buf, fil_log_list )
    check_err_exe ( ret, err, "write_file" )
#
    return (0, None)

#
# ======================================================================
# ==========                   MAIN ROUTINE                   ==========
# ======================================================================
#
def main():
    
    (ret, err) = get_os_fslog_list ( log_fil_dir)
    check_err_exe ( ret, err, "get_os_fslog_list" )

    


exit(1)
    
# ---------------------------------------------------------------------
#
# -- get station list
#
vlbi_sta_buf = read_file ( vlbi_sta_nam )
if ( not vlbi_sta_buf ):
     print ( "Did not find VLBI station name file ", vlbi_sta_nam )
     exit  ( 1 )
#
# -- write stations to buffer
#
sta_nam_list = []
sta_id_list  = []
for lin in vlbi_sta_buf:
    if ( lin[0:1] == '#' ): continue
    sta_id  = lin.split()[0]
    sta_nam = lin.split()[1]
    sta_id_list.append ( sta_id )
    sta_nam_list.append ( sta_nam )
#
# --
#
cnt = 0
for sta in sta_nam_list:
    for dc in dc_list:
#        fs_log_file = args.exp + sta.lower() + ".log"
        print (fs_log_file)
        cnt += 1
        if ( cnt == 10 ): break
         




#for sta in sta_nam_list:
 #   for dc in dc_list:
  #      fs_log_file = args.exp + sta.lower() + ".log"
   #     if ( not os.path.isfile ( fs_log_file ) ):
    #         url = dc + "/%04d/" % date_obs.year + args.exp + "/" + fs_log_file
     #        com = "cd " + direxp + "/" + args.exp + "; vget " + url
      #       (ret,out) = exe ( com )
       #      full_fs_log_file = direxp + "/" + args.exp + "/" + fs_log_file 
        #     if ( not os.path.isfile( full_fs_log_file ) ):
         #         print ( "FS log file for station %s is not found -- no %s file" % ( sta, full_fs_log_file ) )







