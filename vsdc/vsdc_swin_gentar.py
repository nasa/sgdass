#!/usr/bin/env python3
import  sys, os, subprocess, signal
vsdc__root_dir = "@@"
sys.path.append(vsdc__root_dir)
from    vsdc_swin        import *
from    vsdc_submit_file import *
from    vsdc_parse       import *
from    vsdc_master      import *
from    vsdc_nscodes     import *

def main():

    if ( vsdc__root_dir == "@@" ):
         print ( "An attempt to run vsdc_swin_gentar.py that is in the distribution directory." )
         print ( "Please run vsdc_swin_gentar.py that is in the installation directory, " )
         print ( "i.e. the directory you used when you ran vsdc_install.py" )
         exit  ( 1 )

    parser = argparse.ArgumentParser( description=vsdc_swin_gentar__label )
    parser.add_argument('--version', action='version', version=vsdc_swin_gentar__label )

    parser.add_argument ( "--compressor", action="store", \
                          dest="compr", \
                          default="lbzip2", \
                          metavar="value", \
                          help="Compressor utility. Supported utilities: none, lbipz2, bzip2, gzip" )

    parser.add_argument ( "--corr_vers", action="store", \
                          dest="corr_vers", \
                          default="1", \
                          type=int, \
                          metavar="value", \
                          help="Correlator output version" )

    parser.add_argument ( "--v2d", action="store", \
                          dest="v2d", \
                          metavar="value", \
                          help="DiFX control file in v2d format" )

    parser.add_argument ( "--vex", action="store", \
                          dest="vex", \
                          metavar="value", \
                          help="VLBI schedule file in vex format" )

    parser.add_argument ( "-c", "--control", \
                          action="store", \
                          dest="control_file", \
                          default=None, \
                          metavar="value", \
                          help="Name of the control file" )

    parser.add_argument ( "-d", "--delete", \
                          action="store_true", \
                          dest="delete", \
                          help="Delete after submission to the Data Center" )

    parser.add_argument ( "-i", "--dir_in", \
                          action="store", \
                          dest="dirin", \
                          metavar="value", \
                          help="Directory or a tar file with the input VLBI Level 1A data in swin format" )

    parser.add_argument ( "-o", "--dir_out", action="store", \
                          dest="dirout", \
                          metavar="value", \
                          help="Directory name whether the output file will be put. " + \
                               "That diretory should have enough space to hold " + \
                               "a copy of the difx output." )

    parser.add_argument ( "-s", "--submit", \
                          action="store_true", \
                          dest="submit", \
                          help="Submit to the Data Center" )

    parser.add_argument ( "-t", "--tmp_dir", action="store", \
                          dest="tmp_dir", \
                          default="/tmp", \
                          metavar="value", \
                          help="Directory name for small (less than 1Mb) temporary files" )

    parser.add_argument ( "-v", "--verbosity", action="store", \
                          dest="verb", \
                          default=1, \
                          metavar="value", \
                          type=int, \
                          help="Verbosity level" )

    args = parser.parse_args()

    if ( args.control_file == None ):
         print ( "Please supply the control file" )
         exit  ( 1 )

    if ( args.submit == None and args.delete ):
         print ( "Option delete can be used only with option submit" )
         exit  ( 1 )

    if ( not args.dirin ):
         print ( "Please supply the input file in swin format using option -i" )
         exit  ( 1 )

    if ( not args.dirout ):
         print ( "Please supply the name of the output directory using option -o" )
         exit  ( 1 )

    if ( not args.vex ):
         print ( "Please supply the name of the vex file using option --vex" )
         exit  ( 1 )

    if ( not args.v2d ):
         print ( "Please supply the name of the v2d file using option --v2d" )
         exit  ( 1 )

    if ( not ( os.path.isdir ( args.dirin ) ) ):
         print ( "Input directory %s does not exist" % args.dirin )
         exit  ( 1 )

    if ( not ( os.path.isdir ( args.dirout ) ) ):
         print ( "Input directory %s does not exist" % args.dirout )
         exit  ( 1 )

    if ( not ( os.path.isdir ( args.tmp_dir ) ) ):
         print ( "Temporary directory %s does not exist" % args.tmp_dir )
         exit  ( 1 )

    if ( args.dirin == "." or args.dirin == "./" ):
         args.dirin = os.getcwd()


    if ( not ( os.path.isfile ( args.vex ) ) ):
         if  ( not args.vex[0:1] == "/" ):
               vex_file = args.dirin + "/" + args.vex
               if ( os.path.isfile ( vex_file ) ):
                    args.vex = vex_file.replace("//","/") 
               else:
                    print ( "Input vex file %s does not exist" % args.vex )
                    exit  ( 1 )

    if ( not ( os.path.isfile ( args.v2d ) ) ):
         if  ( not args.v2d[0:1] == "/" ):
               v2d_file = args.dirin + "/" + args.v2d
               if ( os.path.isfile ( v2d_file ) ):
                    args.v2d = v2d_file.replace("//","/")
               else:
                    print ( "Input v2d file %s does not exist" % args.v2d )
                    exit  ( 1 )

#
# --- Store full path of vex and v2d files
#
    if ( args.vex[0:2] == "./" ):
         args.vex = os.getcwd() + args.vex[1:]

    if ( args.vex[0:1] != "/" ):
         args.vex = os.getcwd() + "/" + args.vex

    if ( args.v2d[0:2] == "./" ):
         args.v2d = os.getcwd() + args.v2d[1:]

    if ( args.v2d[0:1] != "/" ):
         args.v2d = os.getcwd() + "/" + args.v2d

    com = "tar --version"
    (ret,out) = vsdc_exe ( com )
    if ( ret != 0 ):
         for line in out:
             print ( line )
         print ( "Command tar is not avaialbe or not functional" )
         exit ( 1 )
    if ( not "GNU tar" in out[0] ):
         print ( "vsdc_swin_gentar.py requries GNU tar. Please upgrade" )
         exit ( 1 )
       
    if ( out[0] == "" or out[0] == None ):
         out[0] = "0.0"
    gnu_version = out[0].split()[len(out[0].split())-1]
    if ( gnu_version < vsdc__gnutar_minvers ):
         print ( "You have too old version of tar: %s while %s is required. Please upgrage" % \
                 (gnu_version, vsdc__gnutar_minvers) )
         exit ( 1 )

    old_umask = os.umask(2)
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
         print ( "Failure in parsing VSDC control file ", args.control_file )
         exit  ( 1 )

    ret = vsdc_parse_ddf ( vsdc, vsdc__root_dir )
    if ( ret != 0 ): 
         print ( "Failure in parsing data definition files" )
         exit  ( 1 )
#
# --- Read and parse master file 
#
    ret = vsdc_master_read  ( vsdc ) 
    if ( ret != 0 ): 
         print ( "Failure in parsing master files. ret=", ret )
         exit  ( 1 )
#
# ----- Read and parse ns_codes file 
#
    ret = vsdc_nscodes_read ( vsdc ) 
    if ( ret != 0 ): 
         print ( "Failure in parsing nscodes file. ret= ", ret )
         exit  ( 1 )

    ( ret, tar_file) = vsdc_swin_gen_tar ( vsdc, args.dirin, args.v2d, args.vex, \
                            args.dirout, args.tmp_dir, args.corr_vers, \
                            args.compr, args.verb )
    if ( ret != 0 ):
         old_umask = os.umask(old_umask)
         print ( "Failed to create tar file" )
         exit ( 1 )

    if ( args.submit ):

         if ( vsdc.verb > 0 ):
              print ( "Submitting the tar file %s to %s " % \
                     ( tar_file, vsdc.data_center ) )
         ret = vsdc_submit_file ( vsdc, "data_swin", tar_file , vsdc.verb )
         if ( ret != 0 ):
              old_umask = os.umask(old_umask)
              print ( "File %s has not been submitted to %s" % \
                    ( tar_file, vsdc.data_center ) )
              if ( args.delete ):
                   print ( "File %s was not deleted." % tar_file )

              exit ( 1 )

         old_umask = os.umask(old_umask)
         if ( args.delete ):
              if ( vsdc.verb > 0 ):
                   print ( "Deleting", tar_file )
              try:
                  os.unlink ( tar_file )
              except Exception as e: 
                  print ( "Failed to delete tar_file %s -- %s", \
                  ( tar_file, str(e) ) )
                  exit ( 1 )
              if ( vsdc.verb > 0 ):
                   print ( "Deleted ", tar_file )

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
