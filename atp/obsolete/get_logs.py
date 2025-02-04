#!/usr/bin/python3
# ********************************************************************************
# *                                                                              *
# *   Routine get_logs.py                                                        *
# *                                                                              *
# *   This routine gets *_full.log from cddis and posts them to their respective * 
# *   folders in cx:/q0/fs_logs/ or sa:/t0/fs_logs. For bookkeeping purposes,    *
# *   the following files with lists are then generated/updated:                 *
# *       1) fs_logs_list.txt        : paths to all available log in the         *
# *                                    system's fs_logs folder                   *
# *       2) cddis_full_url_list.txt : urls to all possible full logs at cddis   *
# *                                    on a given range. The urls are generated  *
# *                                    based on master_files.                    *
# *       3) cddis_download_list.txt : urls to files in 2) but not in 1)         *
# *       4) cddis_vget_list.txt     : paths to were the downloaded files were   *
# *                                    sent                                      *
# *       5) cddis_not_found.txt     : urls to files that were not found.        *
# *                                                                              *
# *       N.B: all files stored in the staging folder
# *  
# *  ### 29-JAN-2024  get_cddis_logs.py  v1.0 (c)  N. Habana  29-JAN-2024  ###   *
# *                                                                              *
# ********************************************************************************
#
import pwd, sys, os, re, shutil, time, subprocess, datetime, argparse
import socket
sys.path.append('/home/nhabana/bin')
from   pet_misc  import *
#
# -- Get host name
#
sys_host = os.uname()[1]
#
# -- For sagitta
#
if ( sys_host == "gs61a-sagitta.ndc.nasa.gov" or \
     sys_host == "gs61a-sagitta"              or \
     sys_host == "sa" ):
    fs_dir = "/s0/fs_logs"
    root_dir = "/home/nhabana/work/temp_f2/anc"
    get_cddis_logs__label = "get_cddis_logs 2024.01.30 1.0"

    master_fil_dir  = root_dir + "/vlbi/master_files"
    master_list_fil = root_dir + "/vlbi/master_list.txt"

    log_list_files  = root_dir + "/vlbi/fs_logs_list.txt"
    cddis_tot_full_logs_url = root_dir + "/vlbi/cddis_full_url_list.txt"
    cddis_download_list = root_dir + "/vlbi/cddis_download_list.txt"
    cddis_vget_list = root_dir + "/vlbi/cddis_vget_list.txt"
    cddis_not_found = root_dir + "/vlbi/cddis_not_found.txt"
    temp_dir = root_dir + "/temp"
#
# -- For crux
#
elif ( sys_host == "gs61a-crux.gsfc.nasa.gov" or \
       sys_host == "gs61a-crux"               or \
       sys_host == "cx" ):
    fs_dir = "/q0/fs_logs"
    root_dir = "/f2/anc"
#    root_dir = "/home/nhabana/work/temp_f2/anc"
    get_cddis_logs__label = "get_cddis_logs 2024.01.30 1.0"

    master_fil_dir  = root_dir + "/vlbi/master_files"
    master_list_fil = root_dir + "/vlbi/master_list.txt"

    log_list_files  = root_dir + "/vlbi/fs_logs_list.txt"
    cddis_tot_full_logs_url = root_dir + "/vlbi/cddis_full_url_list.txt"
    cddis_download_list = root_dir + "/vlbi/cddis_download_list.txt"
    cddis_vget_list = root_dir + "/vlbi/cddis_vget_list.txt"
    cddis_not_found = root_dir + "/vlbi/cddis_not_found.txt"
    temp_dir = root_dir + "/temp"
else:
    print("This procedure was written for gs61a-crux and gs61a-sagitta")
    print("Please edit the file paths to match your own, and run it.")
#
# -- Universal variables
#
dc_list = [ "https://cddis.nasa.gov/archive/vlbi/ivsdata/aux" ]
stat_cddis = "https://cddis.nasa.gov/archive/vlbi/ivsdata/aux"
vlbi_sta_nam = "/apr/sta/vlbi_station.names"
#master_fil_dir  = "/cont/master_files"
#master_list_fil = "/cont/master_list.txt"
date_min =  datetime.datetime(2020, 1, 1, 0 , 0, 0)
date_max =  datetime.datetime.utcnow()
#
# -- Read master list to buffer
#
master_list_buf  = read_file ( master_list_fil )
#
master__num_mf = 32
#
# =========================================================================
# ==========  FUNCTION TO GET FS DIRECTORY TREE FROM SYSTEM   =============
# =========================================================================
#
def get_os_fslog_list ( dir_fs ):
#
# -- Read file with list of logs in the current system
#
#    log_list_buf = read_file ( log_list_files )
    log_list_buf = []
    for path, dirs, files in os.walk ( dir_fs ):
        for fil in files:
            if "_full.log" in fil:
                log_list_buf.append( path + "/" + fil )
#
    return log_list_buf
#
# =========================================================================
# ==========     FUNCTION TO GET LIST OF ALL POSSIBLE FULL    =============
# ==========     LOGS ON CDDIS WITHIN RANGE                   =============
# =========================================================================
#
def get_cddis_log_list ( master_list_buf, date_min, date_max, root_url ):
#
# -- Extract year from dates as an integer
#
    str_date_min = date_min.strftime( "%Y%m%d" )
    str_date_max = date_max.strftime( "%Y%m%d" )
#    
    yr_min = int( str_date_min[0:4] )
    yr_max = int( str_date_max[0:4] )
#
# -- Fetch only master files in range
#
    master_list_use = []
    for master in master_list_buf:
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
        if ( master_year >= yr_min and master_year <= yr_max ): 
            master_list_use.append ( master )
#
# -- Get the full paths to the Masterfiles that are applicable to our
#    range
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
# ----- read current file to buffer
#
        buf = read_file ( master_file )
#
# ----- Account for change in formatting since 2023
#
        master_fmt = buf[0].split()[5]
#
# ----- Read all lines in the buffer
#
        for line in buf:
#
# ----- Get the year of schedules from file text
#
            if ( "MULTI-AGENCY" in line ):
                year = int ( line.split()[0] )
                year_chr = line.split()[0][2:4]
#
# --------- index of bars, "|" and array of their 
#
            ib = 0
            ip = [0]
#
# --------- 
#
            if ( line[0:1] == '|' ):
#
# ---------- Get an array of the indeces of the "|" up to master__num_mf
#            of them
#
                for i in range(0,master__num_mf):
                    try:
                        ib = line.find("|",ib+1)
                        ip.append ( ib )
                    except:
                        continue
#
# ------------- Get information based on file format type
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
                    print ( "Unsupported format of master file %s" \
                            % master_file )
                    exit  ( 1 )
#
# ------------- Generate a unique ditionary name, if the session is within
#               the observation range, write it to dictionary
#               N.B: sess_date is already type datetime.datetime
#
                if ( sess_date >= date_min and sess_date <= date_max ):
                    sess_temp_code = datetime.datetime.strftime ( sess_date, "%Y%m%d" ) + \
                                  "_" + suff
                    master_dict[sess_temp_code] = { "db_name"   : db_name.upper(),   \
                                                    "sess_name" : sess_name.lower(), \
                                                    "sess_date" : sess_date,         \
                                                    "sess_dur"  : sess_dur,          \
                                                    "stn_list"  : stn_lst              }
#
# -- Go through the master dictionary and extract each station on the
#    station list
#
    master_paths = []
    for key in master_dict:
#
# ----- Get the session name
# 
        sess_name = master_dict[key]["sess_name"]
#
# ----- Get the year of the experiment
#
        sess_date =  master_dict[key]["sess_date"]
        sess_yr   =  sess_date.strftime("%Y")
#
# ----- Get the station list for this session
#
        stn_tot = []
        stn_row = master_dict[key]["stn_list"]
        ind = stn_row.find("-")
#
# ----- If there are no stations that were removed from this session
#
        if ( ind == -1 ):
            stn_row_len = len(stn_row.replace(" ", ""))
#
# --------- Get every two characters to define # of stations
#
            no_stns = int(stn_row_len/2)
            for i in range(0,no_stns):
                ilb = 2*i                                                  # index of lower boundary
                iub = ilb + 2                                              # index of upper boundary
                stn_tot.append( stn_row[ilb:iub] )
        else:
            no_stns = int((ind-1)/2)
            for i in range(0,no_stns):
                ilb = 2*i                                                  # index of lower boundary
                iub = ilb + 2                                              # index of upper boundary
                stn_tot.append( stn_row[ilb:iub] )
#
# -- check if a given log file exists
#PATH = https://cddis.nasa.gov/archive/vlbi/ivsdata/aux/YYYY/SESS/filename $$ filename = SESSSta.log
#
        for i in range(0,no_stns):
            url = root_url + "/" + sess_yr + "/" + sess_name + "/" + \
                  sess_name  + stn_tot[i] + "_full.log"
            master_paths.append(url)
#
    (ret,err) = write_file ( master_paths, cddis_tot_full_logs_url )
    check_err_exe ( ret, err, "write_file" )
#
    return master_dict 
#
# =========================================================================
# ===========  FUNCTION TO GET LOG LIST IN CDDIS BUT NOT IN SYSTEM ============
# =========================================================================
#
def get_fil_dif ( fil_local_logs_list, fil_cddis_logs_list ):
#
# -- Get both files into buffers
#
    local_list = read_file ( fil_local_logs_list )
    cddis_list = read_file ( fil_cddis_logs_list )
##
## -- Get file names from local list
## -- N.B: Path is /q0/fs_logs/YYYY/sess/filename
##         if we split using "/" the first index will be empty, therefore
##         index of q0 is actually 1, not 0 as you would expect.

    local_fil_nams = []
    for fil_path in local_list:
        fil_nam = fil_path.split("/")[5]
        local_fil_nams.append( fil_nam )        
#
# -- Get just the filenames from the cddis list
# -- N.B: #PATH = https://cddis.nasa.gov/archive/vlbi/ivsdata/aux/YYYY/SESS/filename
#         When spliting using "/", index 0 = https:, index 1 = blank,
#         index 2 = cddis.nasa.gov, etc
#
    cddis_fil_nams = []
    for url_path in cddis_list:
        fil_nam = url_path.split("/")[9]
        cddis_fil_nams.append( fil_nam )
#
# -- Check if the files on cddis are there locally
#
    download_list = []
    cnt  = 0
    for log_fil in cddis_fil_nams:
#
# ----- Counter to get file index
#
        cnt += 1
#
# ----- If the file is there locally
#
        if log_fil in local_fil_nams:
            continue
#
# ----- If the file is not there
#
        else:
#
# --------- Get the index of the string in the cddis list
#
#            ind_fil = cddis_fil.index(log_fil)
            download_list.append( cddis_list[cnt -1] )

    return download_list
#
# =========================================================================
# ===========  FUNCTION TO DOWNLOAD FROM CDDIS AND RECORD ALL   ===========
# ===========  DOWNLOADED FILES, AND THOSE THAT WEREN'T THERE   ===========
# =========================================================================
#
def get_vget_lists ( download_list_buf, fs_dir, temp_dir, \
                     fil_vget_list, fil_cddis_not_found ):
#
# -- Download the available links, and skip the ones that aren't there
#
    vget_list = []
    vnot_list = []
    ivcnt = 0
    for cddis_url in download_list_buf:
# -----
        year_dir = cddis_url.split("/")[7]
        sess_dir = cddis_url.split("/")[8]
        fil_nam  = cddis_url.split("/")[9]
        stn_id   = fil_nam[6:8]
#
# ----- Switch to the temp directory, and download the file (if it's there)
#
        com = "cd " + temp_dir + "; vget " + cddis_url
        (ret, out) = exe ( com )
        full_fs_log_file = temp_dir + "/" + fil_nam
#
# ----- check if the file was generated. If so, then move it to it's
#       expected destination
#
        if ( os.path.isfile ( full_fs_log_file ) ):
# ---------
            ivcnt += 1
#
# --------- move log file to expected folder
#
            year_dir = fs_dir + "/" + year_dir
            sess_dir = year_dir + "/" + sess_dir
#
# --------- Do we already have a directory for this session?
#
            if ( os.path.isdir ( sess_dir ) ):
#
# ------------- move the file to its respective session
#
##@@##            com = "mv " + full_fs_log_file + " " + sess_dir
##@@##            (ret, out) = exe ( com )
                shutil.move(full_fs_log_file, sess_dir )
#
# --------- If we don't have the session directory, then we create it, 
#           but first check if the year dir is there
#
            else:
#
# ------------- If we don't have the year directory, then let's make it
#
                if ( not os.path.isdir( year_dir ) ):
                    os.mkdir( year_dir )
# -------------
                os.mkdir ( sess_dir )
##@@##            com = "mv " + full_fs_log_file + " " + sess_dir
##@@##            (ret, out) = exe ( com )
                shutil.move(full_fs_log_file, sess_dir )
# ---------
            vget_list.append (sess_dir + "/" + fil_nam )
#
# ------ in the event vget was not able to get the file 
#
        else:
#        print( "Full Log file %s not found" %(cddis_url) )
            vnot_list.append ( cddis_url )
#
# -- Write the files that vget downloaded to a file
#
    (ret,err) = write_file ( vget_list, fil_vget_list )
    check_err_exe ( ret, err, "write_file" )
#
# -- 
#
    (ret,err) = write_file ( vnot_list, fil_cddis_not_found )
    check_err_exe ( ret, err, "write_file" )
# --
    return vget_list, vnot_list
#
# =========================================================================
# ===========                   MAIN ROUTINE                   ============
# =========================================================================
#
#
# -- Get the buffer for the local list of files
#
buf = get_os_fslog_list ( fs_dir )
#
# -- Write buffer to file
#
(ret,err) = write_file ( buf, log_list_files )
check_err_exe ( ret, err, "write_file" )
#
# -- Get the dictionary of all CDDIS files based on the master files
#    This routine also writes this range to a file
#
master_dict = get_cddis_log_list ( master_list_buf, date_min, date_max, \
                                   stat_cddis )
#
# -- Get buffer of url links to download from CDDIS, based on
#    missing files
#
download_list = get_fil_dif ( log_list_files, cddis_tot_full_logs_url )
#
# -- Write the download buffer list to a file
#
(ret,err) = write_file ( download_list, cddis_download_list )
check_err_exe ( ret, err, "write_file" )
#
# -- Download the available link, and skip the ones that aren't there
#
vget_list, vnot_list =   get_vget_lists ( download_list, fs_dir, temp_dir, \
                                          cddis_vget_list, cddis_not_found )
