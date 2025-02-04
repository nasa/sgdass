import sys, os, signal
from   vsdc_misc          import *

def vsdc_date_vgosda ( filename ):
    """
    Extracts the scan reference time of the first and the last
    observation from the VLBI database file in VGOSDA format.
    """
#
# --- Read file file
#
    buf = read_file ( filename )
#
# --- Initializatopm
#
    mjd_first = None
    utc_first = None

#
# --- Parse the file
#
    for line in buf:
        if ( len(line.split()) < 7 ): continue
        if ( line[0:4] != "DATA" ): continue
#
# ----- Search for MJD_OBS keyword
#
        if ( line.split()[0] == "DATA.1"  and \
             line.split()[1] == "MJD_OBS"     ):
#
# ---------- Extract MJD value
#
             try:
                  mjd_val = int(line.split()[6])
             except:
                  print ( "vgosda file is corrupted: line %s has the 7th field that is not integer" % line )
                  return ( None, None )
             if ( mjd_first == None ): mjd_first = mjd_val
        if ( line.split()[0] == "DATA.1"  and \
             line.split()[1] == "UTC_OBS"     ):
#
# ---------- Extract UTC value
#
             try:
                  utc_val = float(line.split()[6].replace("D","e"))
             except:
                  print ( "vgosda file is corrupted: line %s has the 7th field that is not real" % line )
                  return ( None, None )
             if ( utc_first == None ): utc_first = utc_val

    if ( mjd_first == None ):
         print ( "Failure to extract the MJD/UTC dates of VLBI observations" )
         return ( None, None )
#
# --- Transform the start date into internal Python format
#

    date_first  = mjd_tim_to_time ( mjd_first, utc_first ) 

    if ( date_first == None ):
         print ( "Failure to extract the MJD/UTC dates of VLBI observations" )
         return ( None, None )
#
# --- Transform the stop date into internal Python format
#
    date_last = mjd_tim_to_time ( mjd_val, utc_val )

    return ( date_first, date_last )
