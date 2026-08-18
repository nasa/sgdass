import sys, os, signal, datetime
from   vsdc_config   import *
from   vsdc_misc     import *

#
# ------------------------------------------------------------------------
#
def vsdc_nscodes_download ( vsdc ):
    """
    Download ns_codes file with the list of networking stations from
    the remote URL and store it in the file with the name specified 
    in the VSDC control file.
    """
 
    if ( os.path.isfile ( vsdc.ns_codes_file ) ):
#
# ------ If the local file exists, let us remove it 
#
         try:
            os.unlink ( vsdc.ns_codes_file )
         except BaseException as e:
            print ( "A trap of internal control: cannot " + \
                    "remove ns_codes file %s" % ( vsdc.ns_codes_file, str(e) ) )
            return 1
#
# --- Build a command for downloading it using wget
# 
    if ( vsdc.data_center.lower() == "cddis" ):
         netrc_str = read_file ( vsdc.netrc_file )
         if ( len(netrc_str[0]) < 6 ):
              print ( "Wrong  netrc file %s -- it should have six words" % vsdc.netrc_file )
              return 1

         username = netrc_str[0].split()[3]
         password = netrc_str[0].split()[5]
         com = "wget " + \
               vsdc.wget_extra_opts       + " " + \
               "--auth-no-challenge"      + " " + \
               "--user="     + username   + " " + \
               "--http-password=" + password   + " " + \
               "-O " + vsdc.ns_codes_file + " " + \
               vsdc.ns_codes_url + "; " + \
               "chmod g+rw,o+rw,u+rw " + vsdc.ns_codes_file
    else:
         com = "wget -O " + \
               vsdc.wget_extra_opts    + " "  + \
               vsdc.ns_codes_file      + " "  + \
               vsdc.ns_codes_url       + "; " + \
               "chmod g+rw,o+rw,u+rw " + vsdc.ns_codes_file
    
    if ( vsdc.verb > 1 ): 
         print ( "com = ", com )
    (ret,out) = vsdc_exe ( com )
    if ( ret != 0 or not os.path.isfile(vsdc.ns_codes_file) ):
         for line in out:
             print ( line )
         print ( "Failure in downloadning with command ", com )
         return 1
    if ( os.stat(vsdc.ns_codes_file).st_size == 0 ):
         for line in out:
             print ( line )
         print ( "Failure in downloadning with command ", com )
         return 1

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
