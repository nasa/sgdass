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
# ==========================================================================
# ===== GET THE ACTUALLY AVAILABLE LOG FILES IN CDDIS FOR A GIVEN YEAR =====
# ==========================================================================
#
def cddis_get_log_dir ( cddis_fil, date_min, date_max, cddis_root, dir_wget, pid ):
#
# -- Get log in infomartion
#
    info_buf = read_file ( cddis_fil )
    username = info_buf[0]
    password = info_buf[1]
#
# -- Convert the dates to string
#
    str_date_min = date_min.strftime( "%Y%m%d" )
    str_date_max = date_max.strftime( "%Y%m%d" )
#
# -- Get the year range as integers
#
    yr_min = int( str_date_min[0:4] )
    yr_max = int( str_date_max[0:4] )
#
# -- Loop through the years, don't take any files that were
#    uploaded after date_max
#    N.B: We don't exit when such a file is reached, because on cddis
#         the files are listed alphabetically by year-> sess-> exp
#         not by date like in the master_files
#
    master_dict = {}
    for year in range( yr_min, yr_max + 1):
#
# ----- Expand url to include the year
#
        url = cddis_root + "/" + str(year) + "/"
#
# ----- define a log file for this year
#
        wget_year_log = dir_wget + "/wget_%s_%08d.log" % ( str(year), pid )
#
# ----- Get information on all the data in the current year of CDDIS
# ----- N.B: This will just include the experiment folders, you will
#            need to go into each experiment there after
#            The wget log is return to the log file defined above.
#
        com = "cd " + dir_wget + "; " +                           \
              "wget -o " + wget_year_log + " " +                  \
              "-e robots=off -c -nH --cut-dir=8 -O - " + " " +    \
              "--auth-no-challenge "         +                    \
              "--user=" + username     + " " +                    \
              "--http-password=" + password + " " +               \
              "--progress=bar:force"   + " " +                    \
              url
# -----
        (ret,out) = exe ( com )
#    
# ----- Did this URL actually have anything for us?
#
        fl_found = False
        if ( ret != 0 ):
           fl_found = True
#
# -------- check for errors in the wget
#
           for line in out:
               if ( "404 Not Found" in line ):
                  fl_found = False
# ------------
           if ( fl_found ):
              for line in out:
                  print ( line )
              print ( "ERROR in getting sess diectories for ", year )
              print ( " ")
              print ( "We are skipping this directory" )
              print ( out )
              #exit ( 1 )
           #return ( [] )
#################### COME AND MAKE SURE YOU DO THIS ONLY IF FL IS FALSE
#
# ----- If there were no issues with getting into the year directory, then we can proceed
#
        if ( not fl_found ):
#
# -------- Get the session lists from our current list of url's
#
#@@#        sess_list=[]
           for line in out:
               if ( '<div class="archiveDirTextContainer"><a class="archiveDirText" id=' in line ):
                 #print ( len(line.split()), line.split() )
                  sess_name = line.split()[3]
                  ln   = len( sess_name )
                  sess_name = sess_name[4:ln-1]; ln   = len( sess_name )
#@@#                  sess_list.append ( sess_name )
#
# --------------- Get a unique code to define this session in the dictionary YYYY_sess
#
                  sess_code = str(year) + "_" + sess_name
#
# --------------- define a log file for this session
#
                  wget_sess_log = dir_wget + "/wget_%s%s_%08d.log" % ( str(year), sess_name, pid )
#
# --------------- With session in hand, go into it's directory
#
                  url_sess = url + sess_name + "/"
# ---------------
                  com_sess = "cd " + dir_wget + "; " +                           \
                             "wget -o " + wget_sess_log + " " +                  \
                             "-e robots=off -c -nH --cut-dir=8 -O - " + " " +    \
                             "--auth-no-challenge "         +                    \
                             "--user=" + username     + " " +                    \
                             "--http-password=" + password + " " +               \
                             "--progress=bar:force"   + " " +                    \
                             url_sess
# --------------- 
                  ( ret, out_sess ) = exe ( com_sess )           
#
# --------------- Did this URL actually have anything for us?
#
                  fl_found_sess = False
                  if ( ret != 0 ):
                     fl_found_sess = True
#
# ------------------ check for errors in the wget
#
                     for err in out_sess:
                         if ( "404 Not Found" in err ):
                            fl_found_sess = False
#
                     if ( fl_found_sess ):
                        for err  in out_sess:
                            print ( err )
                        print ( "ERROR in accessing ", url_sess )
                        print ( "See the log file ", wget_sess_log )
                        print ( "So we are skipping this directory" )
                        #exit ( 1 )
                     #return ( {} )
#
# --------------- if there were no issues with getting into this sessions
#                 then we can proceed.
#
                  if ( not fl_found_sess ): 
#
# ------------------ Get the logs from this directory
#
                     stn_list      = []
                     ln_list       = []
                     log_list      = []
                     log_date_list = []
                     for line_sess in out_sess:
                         if ( 'MD5SUMS' in line ): continue
                         if ( 'SHA512SUMS' in line ): continue
                         if ( '<div class="archiveItemTextContainer"><a class="archiveItemText" id' in line_sess and \
                              '.log' in line_sess ):
                            log_file = line_sess.split()[3]
                            ln_log   = len( log_file )
                            log_file = log_file[4:ln_log-1]; ln_log   = len( log_file )                # log file name and it's length after removing residuals
                            stn_name = log_file[ln:ln+2]                                               # extract station name from log file name
                            log_date = line_sess.split()[8]; log_date = log_date.split('>')[1]         # log date YYYY:MM:DD
                            log_yr = log_date[0:4]; log_mon = log_date[5:7]; log_day = log_date[8:10]
                            log_date = datetime.datetime( int(log_yr), int(log_mon), int(log_day) )    # log date in datetime format
#
# ------------------------- Take only logs in our date ranges
#
                            if ( log_date >= date_min and log_date <= date_max ):
#
# ---------------------------- Is there another log file from the same station?
#                              If so, we assume the longer name is the "_full.log",
#                              and that's the one we keep.
#
                               if ( stn_name in stn_list ):
#
# ------------------------------- what is the index of the previous stn
#
                                  ind = stn_list.index(stn_name)
#
# ------------------------------- What was the old length?
#
                                  ln_old = ln_list[ind]
#
# ------------------------------- Keep the longer of the two files
#
                                  if ( ln_log > ln_old ):                    # the new length is longer
                                     del stn_list[ind]
                                     del ln_list[ind]
                                     del log_list[ind]
                                     del log_date_list[ind]
                                     stn_list.append ( stn_name )
                                     ln_list.append  ( ln_log   )
                                     log_list.append ( log_file )
                                     log_date_list.append ( log_date )
#
# ---------------------------- Station is not in list
#
                               else:
                                  stn_list.append ( stn_name )
                                  ln_list.append  ( ln_log   )
                                  log_list.append ( log_file )
                                  log_date_list.append ( log_date )
#
# ------------------ We are done with this session, now let's Append the
#                    master dictionary with it's content
#
                     master_dict[sess_code] = { "sess_year" : year,         \
                                                "sess_name" : sess_name,    \
                                                "stn_list"  : stn_list,     \
                                                "log_list"  : log_list,     \
                                                "log_date"  : log_date_list   }
###               print ( sess_code, "Yr: ", year, "sess: ", sess_name, "stns: ", stn_list, "logs: ", log_list )
# --
    return master_dict
#
# ==========================================================================
# ======            GET THE LOGS AVAILABLE ON THE SYSTEM              ======
# ==========================================================================
#
def get_local_fslog_list ( dir_fs ):
#
# -- Read file with list of logs in the current system
#
    log_list_buf = []
    for path, dirs, files in os.walk ( dir_fs ):
        for fil in files:
            #
            # -- Ignore any folders with "incoming"
            #
            if not "incoming" in path:
               if ".log" in fil:
                  log_list_buf.append( path + "/" + fil )
#
    return log_list_buf
#
# ==========================================================================
# ======   GET THE LIST OF URL's FOR LOGS IN CDDIS BUT  NOT AVAIL     ======
# ======   LOCALLY                                                    ======
# ==========================================================================
#
def get_log_dif ( local_logs_buf, cddis_sess_dict ):
##
## -- Get file names from local list
## -- N.B: Path is /q0/fs_logs/YYYY/sess/filename
##         if we split using "/" the first index will be empty, therefore
##         index of q0 is actually 1, not 0 as you would expect.
##         We are only taking logs that have been assigned to sessions, no strays.

    local_fil_nams = []
    cnt = 0
    for fil_path in local_logs_buf:
        path_split = fil_path.split("/")
        lt = len(path_split)
        fil_nam = path_split[lt-1]
        local_fil_nams.append( fil_nam )
#
# -- Get just the filenames from the cddis dict
#    Elements of the dictionary include {  "sess_year" , \
#                                          "sess_name" , \
#                                          "stn_list"  , \
#                                          "log_list"  , \
#                                          "log_date"      }
#
    cddis_url_list = []
    cddis_fil_name = []
    for key in cddis_sess_dict:
#
# ----- Get the session year
#
        sess_year = cddis_sess_dict[key]["sess_year"]
#
# ----- Get the session name
#
        sess_name = cddis_sess_dict[key]["sess_name"]
        ln_sess   = len(sess_name)
#
# ----- Get the list of log file names
#
        log_list  = []
        log_list  = cddis_sess_dict[key]["log_list"]
        num_log   = len(log_list)
#
# ----- extract experiment name from log list
# ----- N.B: - some of these files might still be zipped.
#            - some files are _full.log, and others are just .log
#
        for log in log_list:
            cddis_url_list.append ( str(sess_year) + "/" + \
                                    str(sess_name) + "/" + \
                                    log )
            if ( "_full.log" in log ):
               cddis_fil_name.append ( log[ 0:ln_sess + 11 ] )
            else:
               cddis_fil_name.append ( log[ 0:ln_sess + 7 ] ) 
#
# -- Check if the files on cddis are there locally
#
    download_url_list = []
    cnt  = 0
    for log in cddis_fil_name:
#
# ----- Counter to get file index
#
        cnt += 1
#
# ----- If the file is there locally
#
        if ( log in local_fil_nams ):
           continue
#
# ----- If the file is not there
#
        else:
#
# --------- Get the index of the string in the cddis list
#
#            ind_fil = cddis_fil.index(log_fil)
           download_url_list.append( cddis_url_list[cnt - 1] )
#
    return download_url_list, cddis_url_list
#
# =========================================================================
# ===========                   MAIN PROGRAM                   ============
# =========================================================================
#
def main():
#
# -- define useful parameters
#fast_dir  = "/home/nhabana/work/temp_f2"
#
    root_url  =  "https://cddis.nasa.gov/archive/vlbi/ivsdata/aux"
    fil_cddis = "/home/nhabana/.cddis"
    root_dir  = "/q0/fs_logs"
    date_min  =  datetime.datetime(2022, 12, 31, 0 , 0, 0)
    date_max  =  datetime.datetime.utcnow()
    fast_dir  = "/f2"
    temp_dir  = fast_dir + "/anc/temp"
    pid       = os.getpid()
    url_fil   = fast_dir + "/anc/vlbi/cddis_download_url_ %08d.txt" %(pid)
    cddis_fil = fast_dir + "/anc/vlbi/cddis_url_list_ %08d.txt" %(pid)
#
# -- Get a dictionary of all log files in cddis that are within range
#
    print ( "started: ", date_max, " PID: ", pid )
    print ( "get_logs: getting logs dictionary from cddis")
    cddis_dict     = cddis_get_log_dir ( fil_cddis, date_min,     \
                                         date_max, root_url, temp_dir, pid   )
#
# -- Get list of all log files available locally
#
    print ( "get_logs: getting local logs list" )
    local_list_buf =  get_local_fslog_list ( root_dir )
#
# -- Get list of urls to download from cddis
# -- N.B: the download list is given as YYYY/SESS/log_fil
#
    print ( "get_logs: getting diff. between cddis and local")
#    download_urls  =  get_log_dif ( local_list_buf, cddis_dict )

    ( download_urls, cddis_urls ) =  get_log_dif ( local_list_buf, cddis_dict )
#
    (ret,err) = write_file ( download_urls, url_fil )
    check_err_exe ( ret, err, "write_file" )
  #  exit(1)
   # download_urls = read_file ( "/home/nhabana/temp/cddis_download_list.txt" )

    (ret,err) = write_file ( cddis_urls, cddis_fil )
    check_err_exe ( ret, err, "write_file" )
  
#
# -- Download these files to their respective dire
#
    cnt = 0
    print ( "get_logs: downloading from cddis")
    for url in download_urls:
        year = url.split("/")[0]
        sess = url.split("/")[1]
        fil  = url.split("/")[2]  # don't forget, it might be zipped
#
# ----- Actual download url
#
        cddis_url = root_url + "/" + url
        fs_log_file_temp = temp_dir + "/" + fil + "_%08d_temp" % ( pid )
#
# ----- switch to the temp directory, and download file there
#
        com = "cd " + temp_dir + "; vget " + cddis_url + " " + fs_log_file_temp
        (ret, out) = exe ( com )
#
# ----- check if the file was generated. If so, then move it to it's
#       expected destination
#
        if ( os.path.isfile ( fs_log_file_temp ) ):
#
# -------- move log file to expected folder, if the file is not done downloading,
#
           year_dir = root_dir + "/" + year
           sess_dir = year_dir + "/" + sess
           fs_log_file = sess_dir + "/" + fil
#
# -------- Do we already have a directory for this session?
#
           if ( os.path.isdir ( sess_dir ) ):
#
# ----------- move the file to its respective session
#
        #      shutil.move( fs_log_file_temp, fs_log_file )
              com = "mv " + fs_log_file_temp + " " + fs_log_file
              (ret, out) = exe ( com )
#
# -------- If we don't have the session directory, then we create it, 
#          but first check if the year dir is there
#
           else:
#
# ----------- If we don't have the year directory, then let's make it
#
              if ( not os.path.isdir( year_dir ) ):
                 os.mkdir( year_dir )
# -----------
              os.mkdir ( sess_dir )
#              shutil.move( fs_log_file_temp, fs_log_file )
              com = "mv " + fs_log_file_temp + " " + fs_log_file
              (ret, out) = exe ( com )
# --
    date_end =  datetime.datetime.utcnow()
    print ( "get_logs: finished at ",  date_end )
    print ( "PID = ", pid, " runtime: ", date_end - date_max )              
#
# =========================================================================
# ===========              MAIN PROGRAM ASSERTION              ============
# =========================================================================
#
if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
        main()
    except KeyboardInterrupt:
        print ( "\get_logs.py: Interrupted" )
        exit ( 1 )
