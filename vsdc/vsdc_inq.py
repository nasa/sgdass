import os, sys
from   vsdc_config          import *

def vsdc_inq ( vsdc ):

    ddf_keys = sorted ( list(vsdc.ddf.keys()) )
    ddf_list = []

    print ( vsdc__label   )
    print ( vsdc__version )
    print ( " " )
    print ( "Supported file type:" )
    print ( "====================" )
    print ( " " )

    print ( "%-24s  %s" % ( "File type", "Short name:" ) )
    print ( "-------------------------------------" )
    print ( " " )
    for ddf_name in ddf_keys:
        print ( "%-24s  %s" % ( ddf_name, vsdc.ddf[ddf_name]["short_description"] ) )

    print ( " " )
    print ( "%-24s  %s" % ( "File type", "File naming scheme:" ) )
    print ( "---------------------------------------------" )
    print ( " " )

    for ddf_name in ddf_keys:
        print ( "%-24s  %s" % ( ddf_name, vsdc.ddf[ddf_name]["filenaming_scheme"] ) )
