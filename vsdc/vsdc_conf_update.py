#!/usr/bin/env python3
import sys, os, subprocess, datetime, argparse, signal
from   vsdc_misc           import *

#
# ------------------------------------------------------------------------
#
def main():

    if ( len(sys.argv) < 2 ):
         print ( "Usage: vsdc_conf_update.py  config_file" )
         exit  ( 1 )
    config_file = sys.argv[1]
        
    buf = read_file ( config_file )
    if ( not buf ):
         print ( "config_file %s does not exist" % config_file )
         exit  ( 1 )

    if ( buf[0] == "# VSDC_CONFIG file. Version 1.03 of 2021.04.20" ):
         ifmt = 1 
    else:
         print ( "You contol file has miagic %s. It is not supported. Plase update your file manually" % buf[0] )
         exit  ( 1 )

    out = []
    for line in buf:
        if ( line == "# VSDC_CONFIG file. Version 1.03 of 2021.04.20" ):
             line = "# VSDC_CONFIG file. Version 1.04 of 2021.05.10"
        if ( line[0:14] == "URL_DDF_FILES:" ):
             out.append ( 'TAR_SWIN_EXCLUDE: ""' )
             out.append ( 'CURL_EXTRA_OPTS:  ""' )
             out.append ( 'WGET_EXTRA_OPTS:  ""' )
        out.append ( line )
   
    output_file = config_file + ".new"
    f=open( output_file, "w" )
    for line in out:
        print ( line, file=f )
    f.close()
 
    print ( "Written updated file %s" % output_file )
 
if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
        main()
    except KeyboardInterrupt:
        print ( "\nvsdc.py: Interrupted" )
        exit ( 1 )
