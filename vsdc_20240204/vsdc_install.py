#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program vsdc_install.py installs vsdc. See file user_guide.txt     *
# *   for directions how to run it.                                      *
# *                                                                      *
# * ### 31-MAR-2021 vsdc_install.py v2.4 (c)  L. Petrov  14-APR-2023 ### *
# *                                                                      *
# ************************************************************************
import  sys, os, subprocess, signal, time, datetime, gzip, bz2, lzma, argparse
from    vsdc_misc   import *

dir_pat  = "asljzgroakfg"

files_to_install = [ \
                        "swin_up.bash",            \
                        "swin_up.csh",             \
                        "vget",                    \
                        "vsdc_swin_gentar.py",     \
                        "vsdc_print_swin_meta.py", \
                        "vsdc_get_swin_dir.py",    \
                        "vsdc.py",                 \
                        "vsdc.bash",               \
                        "vsdc.csh"                 \
                   ]

def main():
    parser = argparse.ArgumentParser( description="vsdc installation utility" )

    parser.add_argument ( "--prefix", action="store", \
                          dest="prefix", \
                          default=None, \
                          metavar="value", \
                          help="Directory where vsdc is installed" )

    parser.add_argument ( "--config_dir", action="store", \
                          dest="config", \
                          default=None, \
                          metavar="value", \
                          help="Directory where vsdc configuration files will be installed" )

    parser.add_argument ( "--datacenter", action="store", \
                          dest="datacenter", \
                          default=None, \
                          metavar="value", \
                          help="Datacenter where data will be submitted. Once of: cddis, bkg, and opar" )

    parser.add_argument ( "-v", "--verbosity", \
                          action="store", \
                          dest="verb", \
                          type=int, \
                          default=0, \
                          metavar="value", \
                          help="Verbosity level: 0 -- normal vervbosity, 1 -- debugging mode" )
#
# --- Get and parse options
#
    args = parser.parse_args()

    if ( args.prefix == None ):
         print ( "Directory where vsdc is installed is not specified with --prefix" )
         exit  ( 1 )

    if ( args.config == None ):
         print ( "Directory where vsdc configuration is installed is not specified with --config_dir" )
         exit  ( 1 )

    if ( not os.path.isdir ( args.prefix ) ):
         try:
             if ( args.verb > 0 ):
                  print  ( "Creating directory ", args.prefix )
             os.makedirs(args.prefix, 0o775 )
         except Exception as e:
             print ( "Failure in an attempt to create directory %s -- %s" % \
                      ( args.prefix, e ) )
             exit ( 1 )
         try:
             os.makedirs(args.prefix + "/bin", 0o775 )
         except Exception as e:
             print ( "Failure in an attempt to create directory %s -- %s" % \
                      ( args.prefix + "/bin", e ) )
             exit ( 1 )

    if ( not os.path.isdir ( args.config ) ):
         try:
             if ( args.verb > 0 ):
                  print  ( "Creating directory ", args.config )
             os.makedirs(args.config, 0o775 )
         except Exception as e:
             print ( "Failure in an attempt to create directory %s -- %s" % \
                      ( args.config, e ) )
             exit ( 1 )

    if ( args.datacenter == None ):
         print ( "Datacenter is not specifed. Requires cddis, or bkg, or opar" )
         exit  ( 1 )


    args.datacenter = args.datacenter.lower()
    if ( not ( args.datacenter == "cddis" or args.datacenter == "bkg" or args.datacenter == "opar" ) ):
         print ( "Unknwon datacenter %s. Known data centers are cddis, or bkg, or opar" %  args.datacenter )
         exit  ( 1 )

    if ( "/" in sys.argv[0] ):
         dist_dir = sys.argv[0].replace("/vsdc_install.py","")
         if ( dist_dir == "." ):
              dist_dir = os.path.normpath(os.environ['PWD'])
    else:
         dist_dir = os.path.normpath(os.environ['PWD'])

#
# --- Create the name of the temporry directory
#
    cur_dir = os.path.dirname(sys.argv[0])
    if ( cur_dir == "." ):
         cur_dir = os.getcwd()
    temp_dir = cur_dir + "/temp" 
    if ( not os.path.isdir(temp_dir) ):
         os.mkdir ( temp_dir, mode=0o775 )
    conf_file = temp_dir + "/conf.log"
#
# --- Gather the command line
#
    com_line = " ".join(sys.argv) 
    if ( com_line[0:2] == "./" ): 
         com_line = cur_dir + com_line[1:]

#
# --- Write the command file in the a conf.log file in the temporary directory
#
    f=open(conf_file,"w")
    print ( com_line, file=f )
    f.close()
    os.chmod(conf_file, 0o775)

    for path, dirs, files in os.walk(dist_dir):
        for file in files:
            full_file = path + "/" + file
            if ( "/vsdc" + "_" + dir_pat + "/" in full_file ):
#
# -------------- Process configuration directory
#
                 new_dir_name = args.config + "/" + os.path.dirname(full_file.replace(dist_dir+"/",""))
                 new_dir_name = new_dir_name.replace("/vsdc_asljzgroakfg","" )
                 if ( not os.path.isdir(new_dir_name) ):
                      try:
                          if ( args.verb > 0 ):
                               print  ( "Creating directory ", new_dir_name )
                          os.makedirs ( new_dir_name, 0o775 )
                      except Exception as e:
                          print ( "Failure in an attempt to create directory %s -- %s" % \
                                   ( new_dir_name, e ) )
                          exit ( 1 ) 
                 if ( file == "cddis_netrc.template" ):
                      file = "cddis.netrc"
                 if ( file == "bkg_netrc.template" ):
                      file = "bkg.netrc"
                 if ( not os.path.isfile(new_dir_name + "/" + file) ):
                      com = "cp -p " + full_file + "  " + new_dir_name + "/" + file
                      if ( args.verb > 0 ):
                           print  ( "Run command ", com )
                      (ret,out) = vsdc_exe ( com )
                      if ( ret != 0 ):
                           for line in out:
                               print ( line )
                           print ( "Failed command: ", com )
                           exit( 1 )
            if ( file in files_to_install ):
                 buf = read_file ( full_file )
                 out = []
                 for line in buf:
                     if ( 'vsdc_dir=%%' in line ):
                          line = line.replace("%%",args.prefix + "/bin")
                     elif ( "vsdc_dir=" in line ):
                          ib = line.find("vsdc_dir=")
                          embedded_path = line[ib+len("vsdc_dir="):] 
                          new_path = dist_dir
                          line = line.replace("vsdc_dir="+embedded_path,"vsdc_dir="+new_path)
                     if ( "datacenter_cnf=" in line ):
                          ib = line.rfind("/" )
                          embedded_dc = line[ib:] 
                          line = line.replace(embedded_dc,"/"+args.datacenter+".cnf")
                     if ( 'vsdc__root_dir = "@@"' in line ):
                          line = ( 'vsdc__root_dir = "' +  os.getcwd() + '"' )
                     out.append ( line )
                 install_file_name = args.prefix + "/bin/" + file
                 if ( args.verb > 0 ):
                      print  ( "Writing into file ", install_file_name )
                 f=open(install_file_name,"w")
                 for line in out:
                     print ( line, file=f )
                 f.close()
                 com = "chmod o=r,g+wx,u+wx " + install_file_name
                 if ( args.verb > 0 ):
                      print  ( "Run command ", com )
                 (ret,out) = vsdc_exe ( com )
                 print ( "Installed ", install_file_name )
    
    print ( "Installation of vsdc is completed." )

if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
        main()
    except KeyboardInterrupt:
        print ( "\nvsdc.py: Interrupted" )
        exit ( 1 )
