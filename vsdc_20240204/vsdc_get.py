#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program vsdc_get gets a file from the data center.                 *
# *                                                                      *
# * ### 25-FEB-2021    vsdc_get.py   v2.0 (c) L. Petrov 13-FEB-2024 ###  *
# *                                                                      *
# ************************************************************************
import sys, os, subprocess, datetime, argparse, signal
from   vsdc_config          import *
from   vsdc_parse           import *

def vsdc_get ( vsdc, url, filout ):

    if ( vsdc.data_center.lower() == "cddis" ):
         netrc_str = read_file ( vsdc.netrc_file )
         if ( len(netrc_str[0]) < 6 ):
              print ( "Wrong  netrc file %s -- it should have six words" % vsdc.netrc_file )
              return 1

         username = netrc_str[0].split()[3]
         password = netrc_str[0].split()[5]

    if ( "cddis" in url ):
          if ( filout ):
               com = "wget -c"                      + " " + \
                    "-O " + filout                 + " " + \
                     vsdc.wget_extra_opts           + " " + \
                     "--auth-no-challenge"          + " " + \
                     "--user=" + username           + " " + \
                     "--http-password=" + password  + " " + \
                     "--progress=bar:force"         + " " + \
                     url
          else:
               com = "wget -c"                      + " " + \
                     vsdc.wget_extra_opts           + " " + \
                     "--auth-no-challenge"          + " " + \
                     "--user=" + username           + " " + \
                     "--http-password=" + password  + " " + \
                     "--progress=bar:force"         + " " + \
                     url
    else:
          if ( filout ):
               com = "wget -c"            + " " + \
                    "-O " + filout        + " " + \
                     vsdc.wget_extra_opts + " " + \
                     url
          else:
               com = "wget -c"            + " " + \
                     vsdc.wget_extra_opts + " " + \
                     url

    if ( vsdc.verb > 1 ): print ( "com = ", com )
    (ret,out) = vsdc_exe_pipe ( com )
    if ( ret != 0 ): 
         for line in out:
             print ( line )
         return ( 1 )
    if ( vsdc.verb > 0 ):
         finam = url[url.rfind("/")+1:]
         print ( "Downloaded", finam )
    return ( 0 )

#
# ------------------------------------------------------------------------
#
def main():
    """ 
    Main routine of vsdc pakage. It parses arguments
    """
    parser = argparse.ArgumentParser( description=vsdc__label )
    parser.add_argument('--version', action='version', version=vsdc__version )
#
# --- General options:
#
    parser.add_argument ( "-v", "--verbosity", \
                          action="store",      \
                          dest="verb",         \
                          default=0,           \
                          metavar="value",     \
                          type=int,            \
                          help="Verbosity level" )

    parser.add_argument ( "-c", "--control",   \
                          action="store",      \
                          dest="control_file", \
                          default=None,        \
                          metavar="value",     \
                          help="Name of the control file" )

    parser.add_argument ( "-o", "--output",    \
                          action="store",      \
                          dest="filout",       \
                          default=None,        \
                          metavar="value",     \
                          help="Name of the control file" )

    parser.add_argument ( "-u", "--url",       \
                          action="store",      \
                          dest="url",          \
                          default=None,        \
                          metavar="value",     \
                          help="File name" )
#
# --- Get and parse options
#
    args = parser.parse_args()

    if ( not args.control_file ):
         print ( "Control file is not specified" )
         exit  ( 1 )

    if ( not args.url ):
         print ( "URL is not specified" )
         exit  ( 1 )

#
# --- Initialize the class with vsdc variables
#
    vsdc = vsdc_config_class ( args.control_file ) 
#
# --- Parse vsdc control file
#
    vsdc.verb = args.verb
    ret = vsdc_parse_config ( vsdc )
    if ( ret != 0 ): 
         print ( "Failure in parsing control files" )
         exit  ( 1 )

    ret = vsdc_get ( vsdc, args.url, args.filout )

if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
        main()
    except KeyboardInterrupt:
        print ( "\nvsdc.py: Interrupted" )
        exit ( 1 )
