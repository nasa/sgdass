import sys, os, signal, datetime
from   vsdc_config   import *
from   vsdc_misc     import *

#
# ------------------------------------------------------------------------
#
def vsdc_ddf_download ( vsdc ):
    """
    Download Date Definition Files (ddf) from the remote URL and 
    store them in the directory specified in the VSDC control file.
    """
 
    mode = 1 # 1: download only DDFs from the list. 2: download all DDFs
    if ( not os.path.isdir ( vsdc.ddf_dir ) ):
         try:
             os.mkdir ( vsdc.ddf_dir, mode=0o775 ) 
         except Exception as e:
             print ( "Failure in an attempt to create a local ddf dirctory: %s", str(e) )
             exit  ( 1 )
#
# --- Build a command for downloading it using wget
# 
    if ( mode == 1 ):
         for ddf_file in ddf_list:
             if ( ddf_file in ddf_local_list ):
                  from_ddf = os.path.dirname ( sys.argv[0] ) + "/share/" + ddf_file 
                  to_ddf   = vsdc.ddf_dir + "/" + ddf_file
                  com = "cp -pv " +  from_ddf + " " + to_ddf
                  if ( vsdc.verb > 2 ): 
                       com = com.replace("-q "," " )
                  if ( vsdc.verb > 1 ): 
                       print ( "About to execute com = ", com )
     
                  (ret,out) = vsdc_exe ( com )
                  if ( ret != 0 ):
                       for line in out:
                           print ( line )
                       print ( "Failure in copying ddf file with command %s" % com )
                       return 1
                  continue

                  com = "chmod u+rw,g+rw,o+rw " + to_ddf
                  (ret,out) = vsdc_exe ( com )
                  if ( vsdc.verb > 1 ): 
                       print ( "About to execute com = ", com )
                  if ( vsdc.verb > 1 ): 
                       print ( "Failure in setting permission for the ddf file %s %s" % ( to_ddf, com ) )
                       print ( "Nevertheless, continue" )


             com = "cd " + vsdc.ddf_dir + "; " + \
                   "wget " + \
                   vsdc.wget_extra_opts + " " + \
                   "-q " + \
                   "-c " + \
                   "-e robots=off " + \
                   "-X robots.txt " + \
                   "-nH "  + \
                   "-np "  + \
                   "--cut-dirs=8 "  + \
                   "--retr-symlinks " + \
                   "--retry-connrefused " + \
                   "--tries=1440 " + \
                   "--wait=30 "  + \
                   "--connect-timeout=30 " + \
                   vsdc.url_ddf_files + "/" + ddf_file + "; " + \
                   "chmod u+rw,g+rw,o+rw "  + ddf_file

             if ( vsdc.verb > 2 ): 
                  com = com.replace("-q "," " )
             if ( vsdc.verb > 1 ): 
                  print ( "com = ", com )
     
             (ret,out) = vsdc_exe ( com )
             if ( ret != 0 ):
                  for line in out:
                      print ( line )
                  print ( "Failure in downloadning ddf file %s with command %s" % ( ddf_file, com ) )
                  return 1

    if ( mode == 2 ):
         com = "cd " + vsdc.ddf_dir + "; " + \
               "wget " + \
               vsdc.wget_extra_opts + " " + \
               "-q " + \
               "-c " + \
               "-e robots=off " + \
               "-X robots.txt " + \
               "-nH "  + \
               "-np "  + \
               "--cut-dirs=8 "  + \
               "--retr-symlinks " + \
               "--retry-connrefused " + \
               "--tries=1440 " + \
               "--wait=30 "  + \
               "--connect-timeout=30 " + \
               vsdc.url_ddf_files
         
         if ( vsdc.verb > 2 ): 
              com = com.replace("-q "," " )
         if ( vsdc.verb > 1 ): 
              print ( "com = ", com )
     
         (ret,out) = vsdc_exe ( com )
         if ( ret != 0 ):
              for line in out:
                  print ( line )
              print ( "Failure in downloadning ddf files with command ", com )
              return 1
     
         dir_file = read_file ( vsdc.ddf_dir + "/index.html" )
     
         for line in dir_file:
             if ( "ddf</a>" in line ):
                  ddf_name = line.split('"')[1]
     
                  com = "cd " + vsdc.ddf_dir + "; " + \
                        "wget " + \
                        vsdc.wget_extra_opts + " " + \
                        "-q " + \
                        "-c " + \
                        "-e robots=off " + \
                        "-X robots.txt " + \
                        "-nH "  + \
                        "-np "  + \
                        "--cut-dirs=8 "  + \
                        "--retr-symlinks " + \
                        "--retry-connrefused " + \
                        "--tries=1440 " + \
                        "--wait=30 "  + \
                        "--connect-timeout=30 " + \
                        vsdc.url_ddf_files + "/" + ddf_name + "; " + \
                        "chmod u+rw,g+rw,o+rw " + ddf_name
         
                  if ( vsdc.verb > 2 ): 
                       com = com.replace("-q "," " )
                  if ( vsdc.verb > 1 ): 
                       print ( "com = ", com )
                  (ret,out) = vsdc_exe ( com )
                  if ( ret != 0 ):
                       for line in out:
                           print ( line )
                       print ( "Failure in downloadning ddf file %s with command %s", (ddf_name, com ) )
                       return 1
     
         os.unlink ( vsdc.ddf_dir + "/index.html" )


    return 0

#
# ------------------------------------------------------------------------
#
def vsdc_nscodes_read ( vsdc ):
    """
    Parse the  file with the list of the IVS networking stations.
    The result of parsing  is put in vsdc.ns_coded dictionary 
    """

#
# --- Read the file with the IVS networking stations
#
    if ( not os.path.isfile(vsdc.ns_codes_file) ):
         print ( "NS_CODES_FILE %s does not exist" % vsdc.ns_codes_file )
         exit ( 1 )
         
    buf = read_file ( vsdc.ns_codes_file )
    if ( len(buf) == 0 ):
         print ( "Nscodes file %s is emplty" % vsdc.ns_codes_file )
         exit ( 1 )
    
    if ( len(buf[0]) < len(vsdc__nscodes_label) ):
         print ( "Wrong magic of the nscodes file %s -- %s while %s was expected" % \
                  ( vsdc.ns_codes_file, buf[0], vsdc__nscodes_label ) )
         exit ( 1 )
    if ( buf[0][0:len(vsdc__nscodes_label)] != vsdc__nscodes_label ):
         print ( "Wrong magic of the nscodes file %s -- %s while %s was expected" % \
                  ( vsdc.ns_codes_file, buf[0], vsdc__nscodes_label ) )
         return 1

#
# --- parse the file line by line
#
    for line in buf:
#
# ----- Bypass comments
#
        if ( line[0:1] == '*' ): continue
        if ( len(line) < 5    ): continue
#
# ----- Extract the fields and put them in the ditionary
#
        vsdc.nscodes[line.split()[0].lower()] = { "ivs_name":   line.split()[1].upper(), \
                                                  "dome_name":  line.split()[2], \
                                                  "cdp_number": line.split()[3]  \
                                                 }
    return 0
