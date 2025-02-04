#!/usr/bin/env python3
import sys, os, subprocess, datetime, argparse, signal
global vsdc__root_dir
vsdc__root_dir = "@@"
sys.path.append(vsdc__root_dir)
from   vsdc_config          import *
from   vsdc_parse           import *
from   vsdc_check_file      import *
from   vsdc_check_filename  import *
from   vsdc_inq             import *
from   vsdc_master          import *
from   vsdc_nscodes         import *
from   vsdc_submit_file     import *
from   vsdc_ddf_download    import *

#
# ------------------------------------------------------------------------
#
def main():
    """ 
    Main routine of vsdc pakage. It parses arguments
    """

    if ( vsdc__root_dir == "@@" ):
         print ( "An attempt to run vsdc.py that is in the distribution directory." )
         print ( "Please run vsdc.py that is in the installation directory, " )
         print ( "i.e. the directory you used when you ran vsdc_install.py" )
         exit  ( 1 )

    parser = argparse.ArgumentParser( description=vsdc__label )
    parser.add_argument('--version', action='version', version=vsdc__version )
#
# --- General options:
#
    parser.add_argument ( "-v", "--verbosity", action="store", \
                          dest="verb", \
                          default=1, \
                          metavar="value", \
                          type=int, \
                          help="Verbosity level" )

    parser.add_argument ( "-u", "--update", action="store_true", \
                          dest="update", \
                          help="Update master and networking station files" )

    parser.add_argument ( "-i", "--inquire", \
                          action="store_true", \
                          dest="inq", \
                          help="Inquire supported data types and exit" )

    parser.add_argument ( "-c", "--control", action="store", \
                          dest="control_file", \
                          default=None, \
                          metavar="value", \
                          help="Name of the control file" )

    parser.add_argument ( "-m", "--compression", action="store", \
                          dest="compression", \
                          default=None, \
                          metavar="value", \
                          help="Compress type of submitted data" )

    parser.add_argument ( "-t", "--type", action="store", \
                          dest="file_type", \
                          default=None, \
                          metavar="value", \
                          help="File type" )

    parser.add_argument ( "-f", "--file", action="store", \
                          dest="file_name", \
                          default=None, \
                          metavar="value", \
                          help="File name" )
#
# --- Get and parse options
#
    args = parser.parse_args()

    if ( not args.control_file ):
         print ( "Control file is not specified" )
         exit  ( 1 )

#
# --- Check file name and file type
#
    if ( not os.path.isfile ( args.control_file ) ):
         print ( "Cannot find control file " + args.control_file )
         exit  ( 1 )

    if ( args.compression == "lbzip2" ):
         compr_check = ( "lbzip2 --help" )
         (ret,out) = vsdc_exe ( compr_check )
         if ( ret != 0 ):
              print  ( "vsdc: compressor program lbzip2 does not run: ", out[0] )        
              return ( 1, None )
    elif (          args.compression == "bzip2" ):
         compr_check = ( "bzip2 --help" )
         (ret,out) = vsdc_exe ( compr_check )
         if ( ret != 0 ):
              print  ( "vsdc: compressor program bzip2 does not run: ", out[0] )        
              return ( 1, None )
    elif (          args.compression == "gzip"  ):
         compr_check = ( "gzip --help" )
         (ret,out) = vsdc_exe ( compr_check )
         if ( ret != 0 ):
              print  ( "vsdc: compressor program gzip does not run: ", out[0] )        
              return ( 1, None )
    elif (          args.compression == "pigz"  ):
         compr_check = ( "pigz --help" )
         (ret,out) = vsdc_exe ( compr_check )
         if ( ret != 0 ):
              print  ( "vsdc: compressor program pigz does not run: ", out[0] )        
              return ( 1, None )
    elif ( args.compression != None     ):
         print ( "Unknwon compression %s. Supported comporessions: gzip, pigz, bzip2, lbzip2" % args.compression )
         exit  ( 1 )

         
#
# --- Initialize the class with vsdc variables
#
    vsdc = vsdc_config_class ( args.control_file ) 
#
# --- Parse vsdc configuration file
#
    vsdc.verb = args.verb
    ret = vsdc_parse_config ( vsdc )
    if ( ret != 0 ): 
         print ( "Failure in parsing control files" )
         exit  ( 1 )

#
# --- Download master files and nscodes file
#
    if ( args.update and vsdc.client ):
         if ( vsdc.verb > 0 ):
              print ( "Downloading master files..." )
         ret = vsdc_master_download  ( vsdc ) 
         if ( ret != 0 ): 
              print ( "Failure to download master files" )
              exit  ( 1 )

         if ( vsdc.verb > 0 ):
              print ( "Downloading nscode file..." )
         ret = vsdc_nscodes_download ( vsdc ) 
         if ( ret != 0 ): 
              print ( "Failure to download nscodes files" )
              exit  ( 1 )

         if ( vsdc.verb > 0 ):
              print ( "Downloading ddf files..." )
         ret = vsdc_ddf_download ( vsdc ) 
         if ( ret != 0 ): 
              print ( "Failure to download ddf files" )
              exit  ( 1 )

    if ( not os.path.isfile ( vsdc.ns_codes_file ) and vsdc.client ):
         ret = vsdc_nscodes_download ( vsdc ) 
         if ( ret != 0 ): 
              print ( "Failure to download nscodes files" )
              exit  ( 1 )

#
# --- Parse ddf
#
    ret = vsdc_parse_ddf ( vsdc, vsdc__root_dir )
    if ( ret != 0 ): 
         print ( "Failure in parsing data definition files" )
         exit  ( 1 )

#
# --- Read and parse master file 
#
    vsdc_master_read  ( vsdc ) 
#
# --- Read and parse ns_codes file 
#
    ret = vsdc_nscodes_read ( vsdc ) 
    if ( ret != 0 ): 
         print ( "Failure in parsing nscodes file" )
         exit  ( 1 )
    
    if ( args.inq ):
         vsdc_inq ( vsdc ) 
         exit ( 0 )

#
# --- Check arguments
#
    if ( not args.file_type ):
         if ( args.update ):
              if ( vsdc.verb > 0 ): 
                   print ( "Master files and ns-codes files are updated" )
              exit  ( 0 ) 
         print ( "File type is not specified" )
         exit  ( 1 )

    if ( not args.file_name ):
         print ( "Input file name is not specified" )
         exit  ( 1 )

    if ( not os.path.isfile ( args.file_name ) ):
         print ( "Input file %s is not found " % args.file_name )
         exit  ( 1 )

    old_umask = os.umask(2)
#
# --- Check file_name
#
    ( date_str, vers_str, sess_str, sta_str, suff_str, date_val ) = \
         vsdc_check_filename ( vsdc, args.file_type, args.file_name )
    if ( date_str == None ):
         print ( "File name %s of data type %s has not passed check" % \
                 ( args.file_name, args.file_type ) )
         exit ( 1 )
#
# --- Check file contents
#
    ret = vsdc_check_file ( vsdc, args.file_type, args.file_name )
    if ( ret == None ):
         print ( "File %s has not passed contents check" % \
                  args.file_name )
         exit ( 1 )
#
# --- Compress file if requested
#
    if ( args.compression == None     ):
         file_to_submit = args.file_name
    elif ( args.compression == "lbzip2" or args.compression == "bzip2" ):
         file_to_submit = vsdc.tmp_dir + "/" + os.path.basename ( args.file_name ) + ".bz2"
    elif ( args.compression == "pigz" or args.compression == "gzip" ):
         file_to_submit = vsdc.tmp_dir + "/" + os.path.basename ( args.file_name ) + ".gz"
    if ( args.compression != None ):
         com = args.compression + " -cf " + args.file_name + " > " + file_to_submit 
         if ( vsdc.verb > 3 ): 
              print ( "about to execute command ", com )
         (ret,out) = vsdc_exe ( com )
         if ( ret != 0 ): 
              for line in out:
                  print ( line )
              print ( "vsdc.py: Failed command: ", com )
              exit ( 1 )        
#
# --- Submit file
#
    if ( vsdc.verb > 5 ): 
         print ( "Stop just before submission because verbosity > 5" )
         exit ( 1 )
    ret = vsdc_submit_file ( vsdc, args.file_type, file_to_submit, vsdc.verb )
    if ( ret != 0 ):
         print ( "File %s has not been submitted to %s" % \
                  ( file_to_submit, vsdc.data_center ) )
         old_umask = os.umask(old_umask)
         exit ( 1 )
    old_umask = os.umask(old_umask)
    if ( args.compression != None ):
         try:
              os.remove ( file_to_submit )
         except Exception as e:
              print ( e )
              print ( "Failed to remove temporary file ", file_to_submit )
              exit ( 1 )
    exit ( 0 )

if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
        main()
    except KeyboardInterrupt:
        print ( "\nvsdc.py: Interrupted" )
        exit ( 1 )
