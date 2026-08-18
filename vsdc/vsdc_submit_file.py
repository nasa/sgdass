import os, sys, datetime, socket, urllib.request
from   vsdc_misc import *
from   vsdc_config import *

pyvers = "%d.%02d" % ( sys.version_info.major, sys.version_info.minor ) # python version

#
# ------------------------------------------------------------------------
#
def vsdc_submit_file ( vsdc, file_type, file_name, verb ):
    """
    Submits the data file with name file_name of file_type type to 
    the remote Data Center
    """
    if ( not file_type in vsdc.ddf ):
         print ( "Unknown data type file_type %s. Please create a ddf file for this data type" % \
                  file_type )
         return 1

    temp_file = "/tmp/vsdc__%08d.log" % os.getpid()
    fl_dryrun = False
    if ( "VSDC_DRY_RUN" in os.environ.keys() ):
         fl_dryrun = True
    if ( verb > 2 ):
         full_log_file = vsdc.tmp_dir + "/" + "vsdc_debug_log.txt"
         fl = open ( full_log_file, "a" )
         print ( "", file=fl )
         print ( "=====================================================================", file=fl )
         print ( "%s About to submit file %s of type %s " % \
                 ( str_now_msec_tz(), file_type, file_name ), file=fl )
#
# --- Login to the data center and get cookies if hungry
# 
    curl_verb = " "
    if ( verb <= 0 ):
         curl_verb = "-s"
    elif ( verb == 1 ):
         curl_verb = " "
    else:
         curl_verb = "-v"

    if ( vsdc.url_login != "" ):
#
# ------ CDDIS style uploading: It requires authenticationi first.
#
# ------ Send a curl request for authentication and store coockes
#
         if ( vsdc.ddf[file_type]["product_id"] == "VLBI_DATA_SWIN" ):
              com = "curl " + curl_verb + " " + \
                            vsdc.curl_extra_opts + " " + \
                            " -4  " + \
                            " -k  " + \
                            " -c " + vsdc.cddis_cookies + " " + \
                            " --netrc-file "  + vsdc.netrc_file    + " " + \
                            " -L "            + vsdc.url_swin_login
         else:
              com = "curl " + curl_verb + " " + \
                            vsdc.curl_extra_opts + " " + \
                            " -4  " + \
                            " -c " + vsdc.cddis_cookies + " " + \
                            " --netrc-file "  + vsdc.netrc_file    + " " + \
                            " -L "            + vsdc.url_login

         if ( verb > 2 ):
              print ( "%s About to excute command %s" % ( str_now_msec_tz(), com ), file=fl )
              print ( "%s About to excute command %s" % ( str_now_msec_tz(), com ) )
         elif ( verb > 1 ): 
              print ( "About to execute command ", com )         
         if ( verb > 0 ): 
              sys.stdout.flush()
         (ret, out ) = vsdc_exe ( com )
         if ( verb > 1 ):
              if ( verb > 2 ):
                   print ( "%s Ran command %s" % ( str_now_msec_tz(), com ), file=fl )
                   print ( "ret= ", ret, file=fl )
              print ( "Ran command: " + com )
              print ( "ret= ", ret )

         fl_welcome = False
         if ( len(out) == 0 ):
              if ( verb > 2 ):
                   print ( "No response received", file=fl )
              print ( "No response received" )
         else:
             for line in out:
                 if ( line == "Welcome to CDDIS File Upload" ):
                      fl_welcome = True
                 if ( verb > 2 ):
                      print ( "Response: %s" % line, file=fl )
                      print ( "Response: ", line )

         if ( ret != 0 ):
#
# ----------- Error in running submission command
#
              for line in out:
                  print ( line )
              if ( verb > 2 ):
                   print ( "Tried to execute  ", com, file=fl )
                   print ( "ret= %d", ret, file=fl )
                   print ( "Error in logging in %s. Please check your configuration." % vsdc.data_center, file=fl )
              print ( "Tried to execute  ", com )
              print ( "Error in logging in %s. Please check your configuration." % vsdc.data_center )
              return 1

         elif ( not fl_welcome ):
#
# ----------- Command ran successfully, but did not return a line that 
# ----------- indicates success from the point of view of the server.
# ----------- Perhaps, cookies were inedible :-(
#
              if ( verb > 2 ):
                   print ( "Tried to execute  ", com, file=fl )
                   print ( "Did not find 'Welcome to CDDIS File Upload' in the server response", file=fl )
              print ( "Tried to execute  ", com )
              print ( "Did not find 'Welcome to CDDIS File Upload' in the server response" )
              print ( "Error in logging in %s. Please check your configuration" % vsdc.data_center )
              return 1

         date_start = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]

         if ( verb > 0 ):
              print ( "=====================================================" )
              print ( "@@@ Started  uploading at " + date_start + " @@@" )

    if ( vsdc.uplolad_manifest != "" ):
#
# ------ Generate the manifest
#

         manifest = { 
                      "file_type"          : file_type, \
                      "file_name"          : os.path.basename(file_name), \
                      "file_length"        : "no", \
                      "submission_id"      : "no", \
                      "origin_ip"          : "no", \
                      "checksum_value"     : "no", \
                      "checksum_algorithm" : "no"  \
                    }
         url = 'https://checkip.amazonaws.com'
         manifest["origin_ip"] = urllib.request.urlopen(url).read().decode('utf8').strip()
         manifest["checkum_algorithm"] = "sha256"
         com = "sha256sum " + file_name
         (ret, out ) = vsdc_exe ( com )
         if ( ret != 0 ):
              print ( "error in executing command %s" % com )
              print ( "Your file has not been submitted to %s" % vsdc.data_center )
              return 1
         manifest["checksum_algorithm"] = "sha256"
         manifest["checksum_value"]     = out[0].split()[0]
         manifest["file_length"]        = os.path.getsize(file_name)
         if ( pyvers >= "3.12" ):
              utc_now_str = datetime.datetime.now(datetime.UTC).strftime("%Y%m%d_%H%M%S_%f")
         else:
              utc_now_str = datetime.datetime.utcnow().strftime("%Y%m%d_%H%M%S_%f")
         manifest["submission_id"] = utc_now_str

         out = []
         out.append ( vsdc_manitset__label )
         out.append ( "file_type          : %s" % manifest["file_type"]          )
         out.append ( "file_name          : %s" % manifest["file_name"]          )
         out.append ( "file_length        : %s" % manifest["file_length"]        )
         out.append ( "submission_id      : %s" % manifest["submission_id"]      )
         out.append ( "origin_ip          : %s" % manifest["origin_ip"]          )
         out.append ( "checksum_value     : %s" % manifest["checksum_value"]     )
         out.append ( "checksum_algorithm : %s" % manifest["checksum_algorithm"] )

         file_mnf = vsdc.tmp_dir + "/" + utc_now_str + ".mnf"

         (ret,err) = vsdc_write_file ( out, file_mnf )
         if ( ret != 0 ):
              print ( "error in an attempt to write manifest in file %s" % file_mnf )
              print ( "Your file has not been submitted to %s" % vsdc.data_center )
              return 1
         if ( verb > 1 ):
              print ( "Created manifest file %s" % file_mnf )

         outgoing_ip_str = vsdc.url_swin_submit.split("#")[0]
         outgoing_port_str = vsdc.url_swin_submit.split("#")[1].split(":")[0]
         com = "nc -v -i 0.001 -w %d %s %s" % ( vsdc__nc_w_timeout, outgoing_ip_str, outgoing_port_str )

         (ret, err ) = vsdc_exe ( com )
         fl_connect = false 
         for line in err:
             if ( "Ncat: Connected to" in line ):
                  fl_connect = True
         if ( not fl_connect ):
              print ( "Cannot connect to the outgoing host %s at ip %s" % ( outgoing_ip_str, outgoing_port_str ) )
              exit ( 1 )


         com = vsdc.upload_command + " " + file_name + " " + vsdc.url_swin_submit
         if ( not fl_dryrun ):
              (ret, err ) = vsdc_exe_pipe ( com )
         else:
              print ( "End of dry run" )
              exit  ( 1 )

         if ( ret != 0 ):
              for line in err:
                  print ( "line= ", line )
              print ( "Error in submitting manifest with command %s" % com )
              exit ( 1 )

    if ( vsdc.url_login != "" ):
#
# ------ Post the file using cookes
#
         if ( vsdc.ddf[file_type]["product_id"] == "VLBI_DATA_SWIN" ):
              com = 'curl ' + curl_verb + " " + \
                            vsdc.curl_extra_opts + " " + \
                            ' -k ' + \
                            ' -4 ' + \
                            ' -o ' + temp_file + ' ' + \
                            ' -b ' + vsdc.cddis_cookies  + ' ' + \
                            ' --netrc-file ' + vsdc.netrc_file + ' ' + \
                            ' -F fileType=' + vsdc.ddf[file_type]["data_type"] + ' ' + \
                            ' -F fileContentType=' + vsdc.ddf[file_type]["data_content_type"] + ' ' + \
                            ' -F file[]=@' + file_name + ' ' + \
                            vsdc.url_swin_submit
         else:
              com = 'curl ' + curl_verb + " " + \
                            vsdc.curl_extra_opts + " " + \
                            ' -b ' + vsdc.cddis_cookies  + ' ' + \
                            ' -o ' + temp_file + ' ' + \
                            ' -4 ' + \
                            ' --netrc-file ' + vsdc.netrc_file + ' ' + \
                            ' -F fileType=' + vsdc.ddf[file_type]["data_type"] + ' ' + \
                            ' -F fileContentType=' + vsdc.ddf[file_type]["data_content_type"] + ' ' + \
                            ' -F file[]=@' + file_name + ' ' + \
                            vsdc.url_submit
    else:
#
# ------ Submit a file without coockes
#
         com = 'curl ' + curl_verb + " " + \
                       '--ssl-reqd ' + \
                       ' -4 ' + \
                       ' -w %{errormsg} ' + \
                       ' -o ' + temp_file + ' ' + \
                       ' --netrc-file ' + vsdc.netrc_file + ' ' + \
                       ' --upload-file ' + file_name + ' ' + \
                       vsdc.url_submit

    if ( verb > 0 ): 
         if ( verb > 2 ):
              print ( "%s About to execude command %s " % ( str_now_msec_tz(), com ), file=fl )
         print ( "About to excute command ", com )
         sys.stdout.flush()
    if ( not fl_dryrun ):
         (ret, out ) = vsdc_exe_pipe ( com )
    else:
         print ( "End of dry run" )
         exit  ( 1 )

    log = None
    if ( os.path.isfile ( temp_file ) ):
         log = read_file ( temp_file )
    if ( vsdc.url_login != "" and log == None ):
         if ( verb > 2 ):
              print ( "%s Something went wrong: log file %s was not found" % ( str_now_msec_tz(), temp_file ), file=fl )
         print ( "Something went wrong: log file %s was not found" % temp_file  )
         exit  ( 1 )
 
    if ( vsdc.url_login == "" and ret == 0 ):
         log = ["We have completely uploaded the requested file"]

    success_line = "??"
    if ( ret != 0 ):
         if ( verb > 2 ):
              print ( "%s Tried to execute  %s", ( str_now_msec_tz(), temp_file ), file=fl )
         print ( "Tried to execute  ", com )
         if ( verb == 0 ):
              min_out = 8
         elif ( verb == 1 ):
              min_out = 80
         else:
              min_out = 8192
         if ( verb < 1 ):
              if ( verb > 2 ):
                   print ( "Server said: ", file=fl )
              print ( "Server said: " )
              for i in range(0,min(min_out,len(out))):
                  if ( verb > 2 ):
                       print ( "Response: %s" % out[i], file=fl )
                  print ( out[i] )

         print ( "Error in an attempt to submit in %s. Please check your configuration" % vsdc.data_center )
         if ( os.path.isfile(temp_file) ):
              os.unlink ( temp_file )
         return 1
    else:
         if ( verb > 2 ):
              for line in out:
                  print ( "Response: %s" % line, file=fl )
                  print ( "Response: %s" %line )

    fl_success = False
    for line in log:
        if ( "Successful upload:" in line ):
             fl_success = True
             success_line = line
        if ( "We are completely uploaded and fine" in line ):
             fl_success = True
             success_line = "We are completely uploaded and fine" 
        if ( "We have completely uploaded the requested file" in line ):
             fl_success = True
             success_line = "We have completely uploaded the requested file"

    if ( not fl_success and verb == 0 ):
#
# ------ The server did not say "success". Instead, it told
# ------ something nasty. This is an error. 
#
         print ( "Tried to execute  ", com )
         if ( verb == 0 ):
              min_out = 8
         elif ( verb > 1 ):
              min_out = 80
         else:
              min_out = 8192

         print ( "%s server said: " % vsdc.data_center)
         if ( verb < 1 ):
              for i in range(0,min(min_out,len(out))):
                  print ( out[i] )
         print ( "Error in submitting in %s. Please check your configuration. ret= %d" % \
                 ( vsdc.data_center, ret ) )
         print ( "Your file has not been submitted to %s" % vsdc.data_center )

         os.unlink ( temp_file )
         return 1
    elif ( not fl_success and verb > 0 ):
         print ( "Out: " )
         for i in range(0,len(out)):
             print ( out[i] )
#
         print ( "Log: " )
         if ( verb == 0 ):
              min_out = 8
         elif ( verb > 1 ):
              min_out = 8192
         else:
              min_out = 80
         print ( "%s server said: " % vsdc.data_center)
         for i in range(0,min(min_out,len(log))):
             print ( log[i] )
         print ( "Error in submitting in %s. Please check your configuration. ret= %d" % \
                 ( vsdc.data_center, ret ) )
         print ( "Your file has not been submitted to %s" % vsdc.data_center )
         return 1
    elif ( verb > 2 ):
         print ( "%s server said: " % vsdc.data_center)
         print ( "Beginning of the server output to stdout", file=fl )
         for i in range(0,len(log)):
             print ( log[i], file=fl )
         print ( "End of the server output to stdout", file=fl )

    os.unlink ( temp_file )
    if ( verb > 0 ):
         print ( success_line )

    date_finish = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
    if ( verb > 0 ):
         if ( verb > 2 ):
              print ( "@@@ Finished uploading at " + date_finish + " @@@", file=fl )
         print ( "@@@ Finished uploading at " + date_finish + " @@@" )
         print ( "=====================================================" )
#
# --- Write a record in the log file
#
    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
    f=open(vsdc.submit_log,"a")    
    if ( vsdc.ddf[file_type]["product_id"] == "VLBI_DATA_SWIN" ):
         com = "du -sb " + file_name
         (ret, out) = vsdc_exe ( com )
         print ( "%s  %s  %-16s %-64s %13s " % \
                 ( vsdc.data_center, date_str, file_type, file_name, out[0].split()[0] ), file=f )
    else:
         print ( "%s  %s  %-16s %-64s %13d"  % \
                 ( vsdc.data_center, date_str, file_type, file_name, os.stat(file_name).st_size ), file=f )
    f.close()
    if ( verb > 2 ):
         print ( "%s" % str_now_msec_tz(), file=fl )
         print ( "", file=fl )
         print ( "=====================================================================", file=fl )
         fl.close()
    return 0
