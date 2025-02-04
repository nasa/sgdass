import sys, os, signal, time, datetime
from   vsdc_misc import *

def vsdc_date_correp ( file_name ):
    """
    Extract start and stop file from the correlator report file
    """
#
# --- Does the file exist?
#
    if ( not os.path.isfile ( file_name ) ):
         print ( "File %s does not exists" % file_name )
         exit ( 1 )

#
# --- Read the file
#
    buf = read_file ( file_name )
    if ( buf == None ):
         return ( None, None )

    date_start = None
    date_stop  = None

    str_date   = None
    str_time   = None
    str_dur    = None

    for line in buf:
        if ( len(line) <  8  ): continue
#
# ----- Extract the date
#
        if ( line[0:len("OBSTIME")] == "OBSTIME" and len(line.split()) > 1 ):
             str_date = line.split()[1]

        if ( line[0:len("UTSTART")] == "UTSTART" and len(line.split()) > 1 ):
             str_time = line.split()[1]

        if ( line[0:len("DURATION")] == "DURATION" and len(line.split()) > 1 ):
             str_dur = line.split()[1]
 
    if ( str_date and str_time and str_dur ):
         try:
             date_start = datetime.datetime.strptime( str_date + "_" + str_time, "%Y/%m/%d_%H%M")
             date_stop  = date_start + datetime.timedelta ( hours=int(str_dur) )
         except: 
             date_start = None
             date_stop  = None
         

    return ( date_start, date_stop )

