import sys, os, signal, tarfile
from   vsdc_misc          import *

def vsdc_date_difx ( file_name ):
    """
    Extract start and stop dates in Pythond internal format from the 
    tar-file with raw VLBI data in DiFX format
    """
#
# --- Does the file exist?
#
    if ( not os.path.isfile ( file_name ) ):
         print ( "File %s does not exists" % file_name )
         return 1
    
#
# --- Is its length zero?
#
    if ( os.stat(file_name).st_size == 0 ):
         print ( "File %s has zero length " % file_name )
         return 1
    
#
# --- Isn't it too short?
#
    if ( os.stat(file_name).st_size < 256 ):
         print ( "File %s is too short" % file_name )
         return 1

    if ( not tarfile.is_tarfile ( file_name ) ):
         print ( "File %s is too a tar archive" % file_name )
         return 1
 
#
# --- Open tar file
#
    f=tarfile.open ( name=file_name, mode='r' )
#
# --- Initization
#

    tim_min = None
    tim_max = None

#
# --- Get the number of files in the tar-archive
#
    num_files = len(f.getmembers())
#
# --- Cycle over files in the tar-archive
#
    for i in range (0,num_files):
#
# ----- Get the name of the file in the archive
#
        finam = f.getmembers()[i].name 

        if ( ".input" in finam ):
#
# ---------- Open a so-called "input" file in the archive. That file contains
# ---------- information about start date of the scan
#
             g=f.extractfile(f.getmembers()[i].name)
#
# ---------- Read the intput file and parse its contebnts
#
             for line in g:
                 line = line.decode("latin").strip("\n").strip("\r")
                 if ( len(line.split()) > 2 ):
#
# ------------------- Search for lines that start with "START MJD:" or "START SECONDS:" 
#
                      if ( line.split()[0] == "START" and line.split()[1] == "MJD:" ):
                           mjd_val = int ( line.split()[2] )
                      if ( line.split()[0] == "START" and line.split()[1] == "SECONDS:" ):
                           utc_val = int ( line.split()[2] )
             g.close()
             if ( tim_min == None ):
#
# --------------- This is the first input file
#
                  tim_min = mjd_tim_to_time ( mjd_val, utc_val )
                  tim_max = mjd_tim_to_time ( mjd_val, utc_val )
             else:
#
# --------------- Update min and max moment of time
#
                  tim_min = min ( tim_min, mjd_tim_to_time ( mjd_val, utc_val ) )
                  tim_max = max ( tim_max, mjd_tim_to_time ( mjd_val, utc_val ) )

    f.close()

    return ( tim_min, tim_max )
