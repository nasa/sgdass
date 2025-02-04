#!/usr/bin/python3 
import sys, os, shutil, subprocess, time
import datetime
#
# ------------------------------------------------------------------------
#
def exe ( command ):
#"""
#    Spawn a supborcess, execute command in the context of the suborpess,
#    wait for its completion and return completion code and results.
#"""
    words = command.split()
    time_str = str(datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.") + "%6d" % datetime.datetime.now().microsecond).replace( " ", "0" )
    (ret, out) = subprocess.getstatusoutput ( "ulimit -s 8000000; " + command )
    return ( ret, out.split ( "\n" ) )

#
# ------------------------------------------------------------------------
#
def check_malo_ond ( fl_html ):
    
    print ( "404: okay" )
    print ( "Content-type: text/html\n" )
    if ( fl_html == 1 ): print ( "" ) 
    if ( fl_html == 1 ): print ( "" ) 
    if ( fl_html == 1 ): print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
    if ( fl_html == 1 ): print ( '<HTML LANG="ru">' )
    if ( fl_html == 1 ): print ( '<HEAD>' )
    if ( fl_html == 1 ): print ( '<META http-equiv="Content-Type" content="text/html; charset=koi8-r">' )
    if ( fl_html == 1 ): print ( '</HEAD' )
    if ( fl_html == 1 ): print ( '<BODY>' )

    com = "ps -eaf | grep malo_ondemand | grep -v grep"
    (ret, out) = exe ( com )
    if ( len(out) < 1 ): 
         print ( "MALO ondemand is not running" )         
    elif ( out[0]  == "" ):
         print ( "MALO ondemand is not running" )         
    else:
         print ( "MALO ondemand is running" )         
    if ( fl_html == 1 ): print ( '</BODY>' )
    if ( fl_html == 1 ): print ( '</HTML>' )
    return 0
#
# ------------------------------------------------------------------------
#
def main ():
    check_malo_ond ( 0 )  
    exit ( 0 )
 
if __name__ == "__main__":
    if ( sys.version[:3] < "3.0" ): 
         print ( "This script cannot run under Python-2. Please use Python-3." ); 
         exit ( 1 )
    main()

