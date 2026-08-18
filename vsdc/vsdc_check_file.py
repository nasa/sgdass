import os, sys
from   vsdc_misc import *
from   vsdc_config import *
#
# ------------------------------------------------------------------------
#
def vsdc_check_file ( vsdc, file_type, file_name ):
    """
    Check file contents by runnning an external validation
    procedure specified in the fied Validate_proc of the 
    DDF file
    """
    if ( not file_type in vsdc.ddf ):
         print ( "Unknown data type file_type %s. Please create a ddf file for this data type." % \
                  file_type )
         return None

#
# --- generate command line for calling the validation procedure
#
    com = vsdc.ddf[file_type]["validate_proc"] + " " + \
          file_name + " " + \
          '"' + vsdc.ddf[file_type]["magic"] + '"'
    if ( vsdc.verb > 0 ):
         print ( "Checking file: ", com )
   
#
# --- Execute validation procedure
#
    (ret, out ) = vsdc_exe ( com )
    if ( ret != 0 ):
         for line in out:
             print ( line )
         print ( "Failure to validate input file %s" % file_name )
         print ( "Failed command:", com )
         return None

    return 0
