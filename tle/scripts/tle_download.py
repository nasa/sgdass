#!/usr/bin/env python3
"""
Created on Wed 2024.06.18_14:51:25

Program tle_auto_upload.py loads satellite constellations from Celestrak then
breaks each tle into it's own individual file, and sends these files to 
cx:/q0/tle/

Files are sorted by constellation, then within each constelation we have the 
YYYY/MM as folders. Also each file is appended with a corresponding 
YYYYMMDD_HHMM after the name. 
Full constellation files are appended with when they were downloaded

18-JUN-2024    tle_auto_upload.py    v1.0 (c)  N. Habana
"""
import  pwd, sys, os, re, shutil, time, subprocess, datetime, argparse
import  socket
from    pet_misc  import *
pyvers = "%d.%02d" % ( sys.version_info.major, sys.version_info.minor ) # python version

dName =  "/tle"
fName =  "/cont/celestrak_url_list.txt"


#
# ===============================================================================
# =====      GET THE YYYYMMDD_HHMMSS from  the TLE date                     =====
# ===============================================================================
#
def tledate_to_ymdhms ( tle_date ):
#
# - First get the year from the tle date string
#
    Y_str = tle_date[0:2]
    Y_int = int(Y_str)

    if ( Y_int > 58 ):
        Y_int = Y_int + 1900
    else:
        Y_int = Y_int + 2000

    Y_str = str(Y_int)
#
# - Get the number of days
#
    ln        = len(tle_date)
    Ndays_str = tle_date[2:ln]
    Ndays_r8  = float(Ndays_str)
    Ndays_int = int(Ndays_r8)
#
# - Check if this is a leap year or not
#
    ly_chk = Y_int % 4
    if ( ly_chk == 0 ):
       if ( (Ndays_int > 0) and (Ndays_int <= 31) ):         # january
          M_int = 1
          D_int = Ndays_int
       elif ( (Ndays_int > 31)  and (Ndays_int <= 60) ):   # February
          M_int = 2
          D_int = Ndays_int - 31
       elif ( (Ndays_int > 60)  and (Ndays_int <= 91) ):   # March
          M_int = 3
          D_int = Ndays_int - 60
       elif ( (Ndays_int > 91)  and (Ndays_int <= 121) ):  # April
          M_int = 4
          D_int = Ndays_int - 91
       elif ( (Ndays_int > 121) and (Ndays_int <= 152) ):  # May
          M_int = 5
          D_int = Ndays_int - 121
       elif ( (Ndays_int > 152) and (Ndays_int <= 182) ):  # June
          M_int = 6
          D_int = Ndays_int - 152
       elif ( (Ndays_int > 182) and (Ndays_int <= 213) ):  # July
          M_int = 7
          D_int = Ndays_int - 182
       elif ( (Ndays_int > 213) and (Ndays_int <= 244) ):  # August
          M_int = 8
          D_int = Ndays_int - 213
       elif ( (Ndays_int > 244) and (Ndays_int <= 274) ):  # September
          M_int = 9
          D_int = Ndays_int - 244
       elif ( (Ndays_int > 274) and (Ndays_int <= 305) ):  # October
          M_int = 10
          D_int = Ndays_int - 274
       elif ( (Ndays_int > 305) and (Ndays_int <= 335) ):  # November
          M_int = 11
          D_int = Ndays_int - 305
       elif ( (Ndays_int > 335) and (Ndays_int <= 366) ):  # December
          M_int = 12
          D_int = Ndays_int - 335
    else: 
       if ( (Ndays_int > 0) and (Ndays_int <= 31) ):         # january
          M_int = 1
          D_int = Ndays_int
       elif ( (Ndays_int > 31)  and (Ndays_int <= 59) ):   # February
          M_int = 2
          D_int = Ndays_int - 31
       elif ( (Ndays_int > 59)  and (Ndays_int <= 90) ):   # March
          M_int = 3
          D_int = Ndays_int - 59
       elif ( (Ndays_int > 90)  and (Ndays_int <= 120) ):  # April
          M_int = 4
          D_int = Ndays_int - 90
       elif ( (Ndays_int > 120) and (Ndays_int <= 151) ):  # May
          M_int = 5
          D_int = Ndays_int - 120
       elif ( (Ndays_int > 151) and (Ndays_int <= 181) ):  # June
          M_int = 6
          D_int = Ndays_int - 151
       elif ( (Ndays_int > 181) and (Ndays_int <= 212) ):  # July
          M_int = 7
          D_int = Ndays_int - 181
       elif ( (Ndays_int > 212) and (Ndays_int <= 243) ):  # August
          M_int = 8
          D_int = Ndays_int - 212
       elif ( (Ndays_int > 243) and (Ndays_int <= 273) ):  # September
          M_int = 9
          D_int = Ndays_int - 243
       elif ( (Ndays_int > 273) and (Ndays_int <= 304) ):  # October
          M_int = 10
          D_int = Ndays_int - 273
       elif ( (Ndays_int > 304) and (Ndays_int <= 334) ):  # November
          M_int = 11
          D_int = Ndays_int - 304
       elif ( (Ndays_int > 334) and (Ndays_int <= 365) ):  # December
          M_int = 12
          D_int = Ndays_int - 334
#
# - Now get the hours mins and seconds
#
    dec_day = Ndays_r8 % 1
    h_int   = int( dec_day * 24 )
    mi_int  = int( ( ( dec_day * 24 ) * 60) % 60 )
    s_int   = int( ( ( dec_day * 24 ) * 3600) % 60 )
# 
    ymd_hms  = Y_str + str(M_int).zfill(2) + str(D_int).zfill(2) + "_" + \
               str(h_int).zfill(2) + str(mi_int).zfill(2) + str(s_int).zfill(2)
#    
    return ymd_hms
#
# =========================================================================
# ===========                   MAIN PROGRAM                   ============
# =========================================================================
#
def main():
#
# - Get the current date and time in utc
#
    if ( pyvers >= "3.12" ):
         date_now = datetime.datetime.now(datetime.UTC)
    else:
         date_now = datetime.datetime.utcnow()

    date_now_str =  date_now.strftime( "%Y%m%d_%H%M") # YYYYMMDD_hhmm
#                                                       0123456789012
#
    d8 = date_now_str
    d8 = d8[0:4] + "." + d8[4:6] + "." + d8[6:9] + d8[9:11] + ":" + d8[11:13] 
    print ( "running: /auto/tle_download.py " + d8 ) #date_now_str )   
#
# - open file holding url's for satellite groups
#
    fPath =  fName
    furl  =  open(fPath, "r")                                       # don't forget to close this
#
    fDwnList = "celestrak_downloaded_files.txt"
    fDwnPath = dName + "/" + fDwnList
    fdwn     = open(fDwnPath, "a")                                  # don't forget to close this
#
# - read each line in the url file
#
    for fln in furl:
        if ( fln[0:1] == '#' ): continue
        rln = fln.split()                                           # Full line
        sat_con = rln[0].strip()                                    # 1st word: Satellite Constellation
        sat_url = rln[1].strip()                                    # 2nd word: URL to the latest TLE of that constellation
#
# ----- Check if a directory exists for this constelation, if there isn't then
#       make it.
#
        sat_con_dir = dName + "/" + sat_con
        if ( not os.path.isdir( sat_con_dir ) ):
           os.mkdir(sat_con_dir )
#
# ----- Is there a year folder for the current year?
#
        sat_con_y_dir = sat_con_dir + "/" + date_now_str[0:4]
        if ( not os.path.isdir( sat_con_y_dir ) ):
           os.mkdir(sat_con_y_dir )
#
# ----- Is there a month folder for the current year?
#
        sat_con_ym_dir = sat_con_y_dir + "/" + date_now_str[4:6]
        if ( not os.path.isdir( sat_con_ym_dir ) ):
           os.mkdir(sat_con_ym_dir )
#
# ----- The name of the output file. It will be tagged with the download date
#
        fConName = sat_con_ym_dir + "/" + sat_con + "_" + date_now_str + ".tle"
#
# ----- download content to that file
#
        wget_command = "wget -O " + fConName + " " + sat_url
        (ret, out) = exe(wget_command)
#
# ----- Did we successfully run wget?
#
        if ( ret == 0 ):
           fil_det =  date_now_str + "\t" + fConName + "\n"
           fdwn.write( fil_det )
#
# ----- go through the file and disserminate content into various tle files
#
        fCon = open(fConName, "r")                                  # don't forget to close this
#
# ----- Recall that every 3 lines are a tle
#
        cnt = 0
        for tln in fCon:
            cnt += 1
#
# --------- first line is most likely the satellite name, so name file based 
#           on it.
#
            if cnt == 1:
               ln0 = tln
               sat_nam = tln
               replacements = [('(', ''), (')', ''), ('/', ''), (' ', '')]
               for char, replacement in replacements:
                   if char in sat_nam:
                      sat_nam = sat_nam.replace(char, replacement)
               sat_nam = sat_nam.strip()
#
# --------- Second line is probably tle information.
#           Extract the date and time of the tle from that line
#
            elif cnt == 2:
               ln1 = tln
               ln1_split = ln1.split()
               tle_date = ln1_split[3].strip()
               YMD_hms = tledate_to_ymdhms ( tle_date )                 # YYYYMMDD_hhmmss
#
# ------------ Check if a directory for that year and month exist for 
#              this constellation
#
               sat_con_y_dir = sat_con_dir + "/" + YMD_hms[0:4]
               if ( not os.path.isdir( sat_con_y_dir ) ):
                  os.mkdir(sat_con_y_dir )
#
# ------------ Is there a month folder for the current year?
#
               sat_con_ym_dir = sat_con_y_dir + "/" + YMD_hms[4:6]
               if ( not os.path.isdir( sat_con_ym_dir ) ):
                  os.mkdir(sat_con_ym_dir )
#
# ------------ Specific Satellite TLE name
#
               fTLEName = sat_con_ym_dir + "/" + sat_nam + "_" + \
                          YMD_hms + ".tle"
               fTLE = open(fTLEName, 'w')
#
# ------------ write to file
#
               fTLE.write(ln0)
               fTLE.write(ln1)
#
# --------- Third line
#
            elif cnt == 3:
               ln2 = tln
               fTLE.write(ln2)
               cnt = 0
               fTLE.close()
# -----
        fCon.close()
# -
    furl.close()
    fdwn.close()
    print ()
#
# ==============================================================================
# ===========                   MAIN PROGRAM ASSERTION              ============
# ==============================================================================
#
if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
        main()
    except KeyboardInterrupt:
        print ( "\tle_auto_upload.py: Interrupted" )
        exit ( 1 )
