#!/usr/bin/python3 
#
# ************************************************************************
# *                                                                      *
# *   Program check_st_list.py check the station list and returns        *
# *   the status of each station.                                        *
# *                                                                      *
# * ### 05-JUL-2015 check_st_list.py v2.3 (c) L. Petrov  29-SEP-2014 ### *
# *                                                                      *
# ************************************************************************
#
import os, sys, string, stat, datetime
from   malo_check_date   import *
from   malo_check_stafil import *
from   malo_subs         import *
from   malo_set_catcha   import *


#
# ------------------------------------------------------------------------
#
os.umask ( 2 )

config = config_class()

print ( "404: okay" )
print ( "Content-type: text/html\n\n" )
print ( "" ) 
print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
print ( '<HTML LANG="ru">' )
print ( '<HEAD>' )
print ( '<META http-equiv="Content-Type" content="text/html; charset=koi8-r">' )
print ( '</HEAD' )
print ( '<BODY>' )


if ( len(os.environ["CONTENT_TYPE"]) > 128 ):
     print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
             "Trap of internal control: too long line CONTENT_TYPE" )
     print ( '</PRE>' )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit ( 0 )

if ( len(os.environ["CONTENT_LENGTH"]) > 128 ):
     print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
             "Trap of internal control: too long line CONTENT_LENGTH" )
     print ( '</PRE>' )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit ( 0 )

boundary = os.environ["CONTENT_TYPE"][30:]
input_len = int(os.environ["CONTENT_LENGTH"])

if ( input_len > config.max_len):
     print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
             "Your input is too long. Try to reduce the number of " + \
             "stations" )
     print ( 'input_len = ', input_len, ' malo_max_sta= ', config.max_len )
     print ( '</PRE>' )
     print ( '</BODY>' )
     print ( '</HTML>' )
     exit ( 0 )

print ( '<PRE>' )
buf = sys.stdin.readlines()

vars = {}
ind_fil_beg = 999999999
sta_buf = []
for i in range(0,len(buf)):
    if ( len(buf[i]) > 128 ):
         print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                 "Trap of internal control: line %d is too long " % i )
         print ( '</PRE>' )
         print ( '</BODY>' )
         print ( '</HTML>' )
         exit ( 0 )
    word = buf[i].split()
    if ( len(word) > 0 ):   
         if ( word[0] == "Content-Disposition:" ):
              var_name = word[2].split("=")[1].replace('"','') 
              if ( len(word) == 3 ):
                   var_value = buf[i+2].replace("\n",'').replace("\r",'')  
                   vars[var_name] = var_value
              elif ( len(word) == 4 ):
                     ind_fil_beg = i+3

    if ( i >= ind_fil_beg ):
         if ( buf[i].replace( "\n",'').find(boundary) >= 0 ):
              ind_fil_beg = 999999999
         else:
              if ( len(buf[i]) > 1 ): 
                   if ( buf[i] != "\r\n" ):
                        sta_buf.append( removeNonAscii(buf[i].replace("\n",''))  )


pid_str = "%05d" % os.getpid()
sta_fil = '/tmp/sta_fil__' + pid_str
f = open ( sta_fil, 'w' )
for str in sta_buf:
    f.write ( str + '\n' )
f.close()

log_buf = []
com = config.malo_bin + "/check_sta_list" + " " + sta_fil
(ret, out ) = exe ( com, log_buf )

for str in out:
    print ( str )

#ip_unlim = check_ip ( config, os.environ['REMOTE_ADDR']  )

print ( "ip_unlim %d" % ip_unlim )

print ( '</PRE>' )

print ( '</BODY>' )
print ( '</HTML>' )
