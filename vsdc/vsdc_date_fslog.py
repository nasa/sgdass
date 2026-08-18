import sys, os, signal, time, datetime
from   vsdc_misc import *

def vsdc_date_fslog ( file_name ):
    """
    Extract start and stop file from the field system station log file
    """
#
# --- Does the file exist?
#
    if ( not os.path.isfile ( file_name ) ):
         print ( "File %s does not exists" % file_name )
         exit ( 1 )

#
# --- Read the log file
#
    buf = read_file ( file_name )
    if ( buf == None ):
         return ( None, None )

    date_start = None
    date_stop  = None

    for line in buf:
        if ( line[0:1] == ";" ): continue
        if ( len(line) <  20  ): continue
#
# ----- Extract the date
#
        try:
             date_rec = datetime.datetime.strptime( line[0:20], "%Y.%j.%H:%M:%S.%f")
        except: 
             continue

        if ( not date_start ): date_start = date_rec

        date_stop = date_rec                            
 
    return ( date_start, date_stop )

