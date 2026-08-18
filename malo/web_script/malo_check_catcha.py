#!/usr/bin/python3 
import os, sys, string, stat, datetime, time, urllib, shutil
from urllib.parse import urlparse
from   malo_check_date   import *
from   malo_check_stafil import *
from   malo_subs         import *

#
# ------------------------------------------------------------------------
#
os.umask ( 2 )

config = config_class()



key_pairs = urllib.parse.parse_qs ( os.environ["QUERY_STRING"] )

#print ( os.environ["QUERY_STRING"] ) ; exit ( 0 ) # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

date_req    = key_pairs["date_req"][0]
ans         = key_pairs["answer"][0]
mode        = key_pairs["mode"][0]
frame       = key_pairs["frame"][0]
model       = key_pairs["model"][0]
service     = key_pairs["service"][0]
n_sta       = key_pairs["n_sta"][0]
start_date  = key_pairs["start_date"][0]
stop_date   = key_pairs["stop_date"][0]
remote_addr = key_pairs["remote_addr"][0]
malo_http   = key_pairs["malo_http"][0]
silent      = key_pairs["silent"][0]
ip_unlim    = key_pairs["ip_unlim"][0]
email       = key_pairs["email"][0]

status_file_name = config.ondemand_dir + "/req/" + date_req + "/status.txt" 
index_file_name  = config.ondemand_dir + "/req/" + date_req + "/index.html" 
url_str = 'http://' + os.environ["SERVER_NAME"] + '/ondemand/req/' + date_req

if ( silent == "no" ):
     print ( "404: okay" )
     print ( "Content-type: text/html\n\n" )
     print ( "" ) 
     print ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' )
     print ( '<HTML LANG="ru">' )
     print ( '<HEAD>' )
     print ( '<META http-equiv="Content-Type" content="text/html; charset=koi8-r">' )

     if ( not os.path.isfile( status_file_name ) ):
          print ( '<META HTTP-EQUIV="refresh" CONTENT="0;' + \
                   "URL='" + malo_http + "#ondemand'" + '">' )
          print ( '</HEAD' )
          print ( '<BODY>' )
          exit  ( 0 )

if ( ip_unlim == "no" ):
     ans_file = config.catcha_dir + "/" + date_req + ".txt"
     with open( ans_file ) as f:
          buf = f.readlines()
    correct_ans = buf[0].replace("\n",'').replace("\r",'')  
     
     if ( ans == correct_ans ):
          fl_ans = 0
     else:
          if ( ans in config.synonyms ):
               if ( config.synonyms[ans] == correct_ans ):
                    fl_ans = 0
               else:
                    fl_ans = 1
          else:
               fl_ans = 1
else:
      fl_ans = 0


if ( fl_ans  == 0 ):

     check_lock ( config.ondemand_lock, config.lock_min_wait, config.lock_max_wait )
     gg  = open ( "/tmp/malo.log", "w" )
     gg.close()

     with open ( index_file_name, "r" ) as q:
          que_buf = f.readlines()
     q.close()
     for line in que_buf:
         if ( date_req in line ):
              f= open ( index_file_name, "w" )
              f.write ( ' <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">\n' )
              f.write ( ' <HTML><HEAD> <META HTTP-EQUIV="Content-Type" content="text/html; charset=iso-8859-1">\n' )
              f.write ( ' </HEAD><BODY>\n' )
              f.write ( ' Your request have been rejected because it generated more than one concurrent connection\n' )
              f.write ( ' Please fix your browser\n' )
              f.write ( '</BODY>\n' )
              f.write ( '</HTML>\n' )
              os.remove ( config.ondemand_lock )
              os.chmod ( index_file_name, \
                 stat.S_IREAD + stat.S_IWRITE + stat.S_IEXEC + \
                 stat.S_IRGRP + stat.S_IWGRP  + stat.S_IXGRP + \
                 stat.S_IROTH + stat.S_IXOTH  )
              exit  ( 0 )
#
# --- Open the queue for writing
#
     f= open ( config.ondemand_queue, "a" )
#
# -- Add there a record for this request
#
     f.write ( "%s %3s_%-16s %-12s %2s %5s %s %s %-15s %-38s %s %s\n" % \
               ( date_req, service, model, mode, frame, n_sta, start_date, stop_date, \
                 remote_addr, email, "U", "        " ) )
     f.close()
     os.remove ( config.ondemand_lock )
     #os.system ( "chmod g+rw,o+rw " + config.ondemand_queue )

     if ( config.ivrb > 1 ): print ( 'index_file_name ' + index_file_name + "<P>" )

    
     f= open ( index_file_name, "w" )
     f.write ( ' <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">\n' )
     f.write ( ' <HTML><HEAD> <META HTTP-EQUIV="Content-Type" content="text/html; charset=iso-8859-1">\n' )
     f.write ( ' <META HTTP-EQUIV="refresh" CONTENT="5">\n' )
     f.write ( ' </HEAD><BODY>\n' )
     if ( mode == "series" ):
          f.write ( date_req + ' &nbsp;&nbsp; Request for mass loading time series computation has been received <BR>\n' )
     else:
          f.write ( date_req + ' &nbsp;&nbsp; Request for mass loading computation has been received <BR>\n' )
     f.write ( 'Waiting for putting the request into the queue. Status is updated every 5 seconds.<BR>\n' )
     f.write ( ' </BODY></HTML>\n' )
     f.close()
     os.chmod ( index_file_name, \
                stat.S_IREAD + stat.S_IWRITE + stat.S_IEXEC + \
                stat.S_IRGRP + stat.S_IWGRP  + stat.S_IXGRP + \
                stat.S_IROTH + stat.S_IXOTH  )
     
     if ( silent == "no" ): print ( '</PRE>' )
     if ( config.ivrb > 1 ): print ( 'index_file_name ' + index_file_name + "<P>" )
     if ( silent == "no" ):
          print ( "Your request to compute mass loading displacement has been received.<P>" )
          print ( "Please, write down ID of your request: <B> " + date_req + "</B><BR>" )
          print ( "You can check the status of your request at " + \
                  '<A HREF="/ondemand/req/' + date_req + '/">' + \
                  '/ondemand/req/' + date_req + '/</A>.<P>' )
     
          if ( email != "n/a" ):
               print ( "Email will be sent to you when request mass loading series " + \
                       "will be ready for downloading." )
     
          print ( '<P>' )
          print ( '<HR size="1">'   )
          print ( 'Back to <A HREF="' + malo_http + '">' + malo_http + '</A>' )
          print ( '</BODY>' )
          print ( '</HTML>' )
     else:
          print ( 'If your request is not redirected auomatically, follow this <A HREF="' + \
                   url_str + '">link</A>' )
          exit ( 0 )

else:
     request_dir = config.req_dir + "/" + date_req 
     if ( os.path.isdir(request_dir) ): shutil.rmtree ( request_dir )

     time.sleep ( 2.0 )
     print ( "<B>WRONG   answer</B>. If in doubt, ask an advisory opinion of a 3+ years-old child. :-)" )
     print ( '<P>' )
     print ( '<HR size="1">'   )
     print ( 'Back to <A HREF="' + malo_http + '">' + malo_http + '</A>' )

print ( '</HEAD' )
print ( '<BODY>' )
