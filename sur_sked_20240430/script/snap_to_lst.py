#!/usr/bin/env python3
import  pwd, sys, os, math, shutil, time, subprocess, datetime, argparse
from   sur_sked_config import * # Import sur_sked confuguration
from   pet_misc        import *

snap_to_lst__label = "sds_to_lst 1.5  20240226"
sam_rate_min =     64.0 # Msampls/sec
sam_rate_max =  32768.0 # Msampls/sec

def main():

    parser = argparse.ArgumentParser(description=snap_to_lst__label)

    parser.add_argument ( "snap", \
                          help="Input schedule in snap format" )

    parser.add_argument ( "-o", "--output_file", \
                          action="store", \
                          dest="output_file", \
                          help="Output file with a procedure file" )

    parser.add_argument( '-s', '--sampling_rate', \
                         type=float, \
                         dest="sampling_rate", \
                         default=8192, \
                         help="Sampling rate in Mbps" )

    opts = parser.parse_args()  

    if ( opts.sampling_rate < sam_rate_min or opts.sampling_rate > sam_rate_max ):
         print ( "Sampling rate %f is out of range [64, 32768] Mbps" % opts.sampling_rate )
         exit  ( 1 )

    buf = []
    with open(opts.snap,encoding="latin") as f:
         for line in f:
             buf.append ( line.strip("\n").strip("\r") )
    f.close()

    out = []
    i = 0
    acc_rec = 0.0

    exp_code = "Unknown"
    sta_name = "Unknown"
    sta_code = "Unknown"
    exp_vers = "?"
    for line in buf:
        if ( "Experiment_code:" in line ):
             exp_code = line.split()[2]
        if ( "Experiment:" in line ):
             exp_code = line.split()[2]
             exp_vers = line.split()[5]
        if ( "Schedule_revision:" in line ):
             exp_vers = line.split()[2]
        if ( "Station:" in line ):
             sta_name = line.split()[2]
             if ( len(line.split()) > 3 ):
                  sta_code = line.split()[3]

    if ( len(buf[0].split()) == 6 and not "VLBI" in buf[0] ):
         try:
              year = float(buf[0].split()[2])
         except:
              year = -1
         if ( year > 0 ):
              exp_code = buf[0].split()[1].lower()
              sta_name = buf[0].split()[3].upper()
              sta_code = buf[0].split()[5].lower()

    if ( not opts.output_file ):
         opts.output_file = opts.snap.replace(".snp",".lst")
    f=open(opts.output_file,"w")

    if ( "/" in opts.snap ):
         id = opts.snap.rindex("/") + 1
    else:
         id = 0
    print ( " Schedule file:   %s  Experiment: %s  version: %s  Station: %s  %s" % \
            ( opts.snap[id:], exp_code, exp_vers, sta_name, sta_code ), file=f )
    print ( " Procedure file:  %s  " % (opts.snap[id:-4] + ".prc"), file=f )
    print ( " Times are in the format hh:mm:ss", file=f )
    print ( " Scan   = scan_name command in .snp file", file=f  )
    print ( " Line#  = line number in .snp file where this scan starts", file=f  )
    print ( " Dur    = time interval of on-source data (Start Data to  Stop Data) in mm:ss", file=f  )
    print ( " Gbyte  = Gigabytes written at start of scan", file=f  )
    if ( opts.sampling_rate < sam_rate_min ):
         print ( " This is a single dish experiment. No baseband data will be recorded", file=f  )
    else:
         print ( " Sampling rate %7.1f Mbps" % opts.sampling_rate, file=f  )
    
    print ( " ", file=f )
    print ( " Scan        Line# Source    Az El Cable    Date      Date      Dur    Gbyte", file=f )
    az = 0.0
    el = 0.0
    nsca = 0    
    for i in range(0,len(buf)):
        line = buf[i]
        if ( '" New az/el/ha:' in line and opts.sampling_rate >= sam_rate_min ):
              az = float(line.split()[3].replace(",",""))
              el = float(line.split()[4].replace(",",""))
        if ( line[0:1] == '"' ): continue
        words = line.replace("=",",").replace(":",",").split(",")
        if ( words[0] == "scan_name" ):
             sca_lin = i
             sca_nam = line.replace("=",",").split(",")[1]
             exp_nam = line.replace("=",",").split(",")[2]
             if ( buf[i+1][0:7] == "source=" ):
                  sou_nam = buf[i+1].replace("=",",").split(",")[1]
             if ( opts.sampling_rate < sam_rate_min ):
                  if ( buf[i-1][0:1] == "!" ):
                       scan_start_time = datetime.datetime.strptime ( buf[i-1][1:18], '%Y.%j.%H:%M:%S')
                       rec_tim = float(line.replace("scan_name=","").split(",")[2])
                       scan_stop_time = scan_start_time + datetime.timedelta ( seconds=rec_tim )
                       sou_nam = "dummy"
                       mins = int(rec_tim)//int(60)
                       secs = rec_tim - 60*mins
                       rec_tim_str = "%03d:%02d" % (mins, secs)
                       acc_rec = 0.0
                       print ( " %-9s %4d %-8s %3d %2d %-5s  %8s  %8s  %6s %7.1f" % \
                          ( sca_nam, sca_lin, sou_nam, az, el, sca_wrp.upper()[0:5], \
                            scan_start_time.strftime ( "%H:%M:%S" ), \
                            scan_stop_time.strftime ( "%H:%M:%S" ),  \
                            rec_tim_str, acc_rec ), file=f )
                  nsca = nsca + 1
        if ( words[0] == "source" ):
             sou_nam = words[1]
             sca_wrp = words[5]
        if ( "???mk6=record" in line ):
             nsca= nsca + 1
             scan_start_time = datetime.datetime.strptime ( words[2], '%Yy%jd%Hh%Mm%Ss')
             rec_tim = float(words[3])
             scan_stop_time = scan_start_time + datetime.timedelta ( seconds=rec_tim )
             mins = int(rec_tim)//int(60)
             secs = rec_tim - 60*mins
             rec_tim_str = "%03d:%02d" % (mins, secs)
             if ( rec_tim_str[0:2] == "00" ):
                  rec_tim_str = " " + rec_tim_str[1:]
             sca_nam = words[5]
             exp_nam = words[6]
             sta_nam = words[7]

             if ( nsca == 1 ):
                  print ( " Date: %s" % scan_start_time.strftime ( "%Y-%b-%d  DOY= %j" ), file=f )
                  print ( " ", file=f )
             print ( " %-12s %4d %-8s %3d %2d %-5s  %8s  %8s  %6s %7.1f" % \
                     ( sca_nam, sca_lin, sou_nam, az, el, sca_wrp.upper()[0:5], \
                       scan_start_time.strftime ( "%H:%M:%S" ), \
                       scan_stop_time.strftime ( "%H:%M:%S" ),  \
                       rec_tim_str, acc_rec ), file=f )
             acc_rec = acc_rec + rec_tim*opts.sampling_rate/8000.0
        if ( line[0:5] == "midob" ):
             nsca= nsca + 1
             if ( buf[i-1][0:1] == "!" ):
                  scan_start_time = datetime.datetime.strptime ( buf[i-1][1:18], '%Y.%j.%H:%M:%S' )
             elif ( i-5 >= 0 ):
                  if ( buf[i-2][0:1] == "!" ):
                       scan_start_time = datetime.datetime.strptime ( buf[i-2][1:18], '%Y.%j.%H:%M:%S' )
                  elif ( buf[i-5][0:1] == "!" ):
                       scan_start_time = datetime.datetime.strptime ( buf[i-5][1:18], '%Y.%j.%H:%M:%S' )
            
             scan_stop_time  = datetime.datetime.strptime ( buf[i+1][1:18], '%Y.%j.%H:%M:%S' )
             rec_tim         = (scan_stop_time - scan_start_time).seconds
             mins = int(rec_tim)//int(60)
             secs = rec_tim - 60*mins
             rec_tim_str = "%03d:%02d" % (mins, secs)
             if ( rec_tim_str[0:2] == "00" ):
                  rec_tim_str = " " + rec_tim_str[1:]
             if ( nsca == 1 ):
                  print ( " Date: %s" % scan_start_time.strftime ( "%Y-%b-%d  DOY= %j" ), file=f )
                  print ( " ", file=f )
             print ( " %-12s %4d %-8s %3d %2d %-5s  %8s  %8s  %6s %7.1f" % \
                     ( sca_nam, sca_lin, sou_nam, az, el, sca_wrp.upper()[0:5], \
                       scan_start_time.strftime ( "%H:%M:%S" ), \
                       scan_stop_time.strftime ( "%H:%M:%S" ),  \
                       rec_tim_str, acc_rec ), file=f )
             acc_rec = acc_rec + rec_tim*opts.sampling_rate/8000.0

    print ( " ", file=f )
    print ( " Total number of scans: %4d  Total amount of data: %7.3f Tb" % \
              ( nsca, acc_rec/1000.0 ), file=f )
    f.close()
    print ( "Created output file ", opts.output_file )

if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        main()
    except KeyboardInterrupt:
        print ( "pf.py: Interrupted" )
        exit ( 1 )
