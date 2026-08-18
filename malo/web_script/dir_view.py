#!/usr/bin/python3 
import os, sys, string, stat, datetime, time, urllib, shutil
sys.path.append('/auto')
from   bfi_cli           import *

ext = [".agr", ".nc", ".eph", ".heb", ".heb.bz2"]
pref = "/cgi-bin/dir_view.py"
aws_host_name = "massloading.sciencecloud.nasa.gov"

dir = sys.argv[1]
if ( len(sys.argv) > 2 ):
     year_mon = sys.argv[2]
     if ( year_mon == "None" ): year_mon = None
else:
     year_mon = None

id = dir.find ( "&" )
if ( id > 0 ):
     year_mon = dir[id+1:]
     dir = dir[:id-1]

if ( "QUERY_STRING" in os.environ ):
     if ( not url_sanitizer ( os.environ["QUERY_STRING"], "url" ) ):
          print ( "404: okay" )
          print ( "Content-type: text/html\n\n" )
          print ( '</PRE>' )
          print ( '<FONT COLOR="A04030"><B> Trap of internal control: wrong arg </B></FONT> %s' % os.environ["QUERY_STRING"]  )
          print ( '</BODY>' )
          print ( '</HTML>' )
          exit  ( 0 )

if ( "SERVER_NAME" in os.environ ):
     if ( os.environ["SERVER_NAME"] == aws_host_name ):
          com = "/astrogeo/web_exec/dir_view.py" + " " + dir
          if ( year_mon != None ):
               com = com + " " + year_mon

          (ret,out) = bfi_cli ( com, timeout=120.0 )
          for line in out:
              print ( line )
          exit ( 0 )

print ( "404: okay" )
print ( "Content-type: text/html\n\n" )
print ( "" ) 
print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
print ( '<HTML LANG="en">' )
print ( '<HEAD>' )
print ( '<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' )
print ( '</HEAD>' )
print ( '<BODY>' )

# print ( 'My dir: ' +  dir )

header = dir + "/AAHEADER"
if ( os.path.isfile ( header ) ):
     with open( header ) as f:
          buf = f.readlines()
     f.close()
     for i in range(0,len(buf)):
         print ( buf[i] + "<BR>"  )


finam_list = []
for paths, dirs, files in os.walk(dir):
    for k in range(0,len(files)):
        name = paths + "/" + files[k]
        for m in range(0,len(ext)):
            ih = name.rfind ( ext[m] )
            if ( ih > 0 ):
                 finam_list.append(name)
    finam_list.sort()    

if ( year_mon == None ):
     mon_list = []
     for k in range(0,len(finam_list)):
        for m in range(0,len(ext)):
            ih = finam_list[k].rfind ( ext[m] )
            if ( ih > 0 ):
                 if ( finam_list[k][ih-14:ih-7] not in mon_list ):
                      mon_list.append(finam_list[k][ih-14:ih-7])
         
     mon_list.sort()    
     print ( '<UL>' )
     for k in range(0,len(mon_list)):
         mon = mon_list[k][1:] 
         print ( '   <LI> [DIR] &nbsp; <A HREF="' + pref + "?" + dir + "&" + mon + '">' + \
                     mon + '</A><BR></LI>' )
     print ( '</UL>' )
     print ( '<P>' )

else:
     file_list = []
     for k in range(0,len(finam_list)):
        for m in range(0,len(ext)):
            ih = finam_list[k].rfind ( ext[m] )
            if ( ih > 0 ):
                 if ( finam_list[k].rfind("_" + year_mon) > 0 ):
                      file_list.append(finam_list[k])
         
     file_list.sort()    
     print ( 'Files for month ' + year_mon )
     print ( '<UL>' )
     for k in range(0,len(file_list)):
         file = file_list[k] 
         id = file.rfind("/")
         print ( '   <LI> [FILE] &nbsp; <A HREF="' + file + '">' + file[id+1:] + '</A><BR></LI>' )

     print ( '</UL>' )
     print ( '<P>' )
     print ( '<A HREF="' + pref + "?" + dir + '">Back</A>' )

print ( '<HR size="1">' )
print ( '</BODY>' )
print ( '</HTML>' )
