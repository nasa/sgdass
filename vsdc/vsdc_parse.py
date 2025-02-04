import os, sys
from   vsdc_misc import *
from   vsdc_config import *


#
# ------------------------------------------------------------------------
#
def vsdc_parse_config ( vsdc ):
   """
   Parsing the vsdc control file and putting results of parsing
   into vsdc object.
   """

   conf_buf = read_file ( vsdc.config_file )
   if ( not conf_buf ):
        print ( "Error in reading VSDC configuration file ", vsdc.config_file )
        exit  ( 1 )

   if ( conf_buf[0][0:len(vsdc__config_label)] != vsdc__config_label[0:len(vsdc__config_label)] ):
        print ( "Unsupported vsdc file "     + vsdc.config_file   + \
                "\n Format label found:   " + conf_buf[0]       + \
                " While expected label: "   + vsdc__config_label + "\n" )
        print ( "Please update your control file using vsdc_conf_update.py" )
        return 1

   
   vsdc.vsdc_dir_name = os.path.dirname(sys.argv[0])
   if ( vsdc.vsdc_dir_name == os.curdir ): vsdc.vsdc_dir_name = os.getcwd() 

   num_par = 0
   num_tse = 0
#
# --- Preserve the command line showing how vsdc was invoked 
# --- and the current directory for debugging
#
   vsdc.invoked_as  = " ".join(sys.argv)
   vsdc.current_dir = os.getcwd()
   for line in conf_buf:
       if ( line == vsdc__config_label ): continue
       if   ( line[0:1] == "#" ): continue

       if ( len ( line.split() ) < 2 and \
                ( line.split()[0] != "TAR_SWIN_EXCLUDE:" and \
                  line.split()[0] != "CURL_EXTRA_OPTS:"  and \
                  line.split()[0] != "wget_EXTRA_OPTS:"      ) ):
            print ( "Unrecognized line " + line + \
                    " in control file " + vsdc.config_file + \
                    " -- too few words" )
            return 1

       elif ( line.split()[0]     == "DDF_DIR:" ):
              vsdc.ddf_dir         = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "URL_LOGIN:" ):
              vsdc.url_login       = line.split()[1]
              if (  vsdc.url_login == 'n/a' ): vsdc.url_login = ""
              if (  vsdc.url_login == '""' ): vsdc.url_login = ""
              num_par = num_par + 1
       elif ( line.split()[0]     == "URL_SWIN_LOGIN:" ):
              vsdc.url_swin_login  = line.split()[1]
              if (  vsdc.url_swin_login == 'n/a' ): vsdc.url_swin_login = ""
              if (  vsdc.url_swin_login == '""' ): vsdc.url_swin_login = ""
              num_par = num_par + 1
       elif ( line.split()[0]     == "URL_SUBMIT:" ):
              vsdc.url_submit      = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "URL_SWIN_SUBMIT:" ):
              vsdc.url_swin_submit = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "TAR_SWIN_EXCLUDE:" ):
              if ( len(line.split()) == 1 ):
                   vsdc.tar_swin_exclude.append ( " " )
                   num_par = num_par + 1
                   num_tse = num_tse + 1
                   continue

              if ( '""' == line.split()[1] ):
                   continue
              elif ( "''" == line.split()[1] ):
                   continue
              elif ( '"' in line.split()[1] ):
                   print ( 'Character " is not allowed in TAR_SWIN_EXCLUDE option' )
                   return 1
              if ( "'" in line.split()[1] ):
                   print ( "Character ' is not allowed in TAR_SWIN_EXCLUDE option" )
                   return 1
              vsdc.tar_swin_exclude.append ( line.replace("TAR_SWIN_EXCLUDE:","").ljust(128," ").strip(" ") )
              num_par = num_par + 1
              num_tse = num_tse + 1
       elif ( line.split()[0]     == "CURL_EXTRA_OPTS:" ):
              if ( len(line.split()) == 1 ):
                   vsdc.curl_extra_opts = " "
                   num_par = num_par + 1
                   continue
              vsdc.curl_extra_opts = line.replace("CURL_EXTRA_OPTS:","").replace('"','').replace("'","").ljust(128," ").strip(" ")
              if ( vsdc.curl_extra_opts == ""   ): vsdc.curl_extra_opts = " "
              if ( vsdc.curl_extra_opts == '""' ): vsdc.curl_extra_opts = " "
              if ( vsdc.curl_extra_opts == "''" ): vsdc.curl_extra_opts = " "
              num_par = num_par + 1
       elif ( line.split()[0]     == "WGET_EXTRA_OPTS:" ):
              if ( len(line.split()) == 1 ):
                   vsdc.wget_extra_opts = " "
                   num_par = num_par + 1
                   continue
              vsdc.wget_extra_opts = line.replace("WGET_EXTRA_OPTS:","").replace('"','').replace("'","").ljust(128," ").strip(" ")
              if ( vsdc.wget_extra_opts == ""   ): vsdc.wget_extra_opts = " "
              if ( vsdc.wget_extra_opts == '""' ): vsdc.wget_extra_opts = " "
              if ( vsdc.wget_extra_opts == "''" ): vsdc.wget_extra_opts = " "
              num_par = num_par + 1
       elif ( line.split()[0]     == "URL_DDF_FILES:" ):
              vsdc.url_ddf_files = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "CDDIS_COOKIES:" ):
              vsdc.cddis_cookies   = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "NETRC_FILE:" ):
              vsdc.netrc_file      = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "DATA_CENTER:" ):
              vsdc.data_center    = line.split()[1]
              vsdc.client = True
              if ( "server" in vsdc.data_center ):
                   if ( vsdc.data_center[0:len("server")] == "server" ):
                        vsrc.client = False 
              num_par = num_par + 1
       elif ( line.split()[0]     == "SUBMIT_LOG:" ):
              vsdc.submit_log      = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "MASTER_DIR:" ):
              vsdc.master_dir      = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "MASTER_URL:" ):
              vsdc.master_url      = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "NS_CODES_URL:" ):
              vsdc.ns_codes_url    = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "NS_CODES_FILE:" ):
              vsdc.ns_codes_file   = line.split()[1]
              num_par = num_par + 1
       elif ( line.split()[0]     == "LARGE_TMP_DIR:" ):
              vsdc.tmp_dir         = line.split()[1]
              num_par = num_par + 1
            
   if ( num_par < ( vsdc__config_pars + num_tse - 1)  ):
        print ( "Not all keywords were found in control file " + \
                 vsdc.config_file + " -- only %d, though %d were expected" % \
                 ( num_par, vsdc__config_pars ) )
        return 1

   if ( not os.path.isdir ( vsdc.ddf_dir ) ):
        print ( "DDF_DIR %s specified in the control file %s does not exist" % \
                ( vsdc.ddf_dir, vsdc.config_file ) )
        return 1

#   if ( not os.path.isfile ( vsdc.cddis_cookies ) ):
#        print ( "CDDIS_COOKIES %s specified in the control file %s does not exist" % \
#                ( vsdc.cddis_cookies, vsdc.cddis_cookies ) )
#        return 1

   if ( not os.path.isfile ( vsdc.netrc_file ) ):
        print ( "NETRC_FILE %s specified in the control file %s does not exist" % \
                ( vsdc.netrc_file, vsdc.netrc_file ) )
        return 1

   if ( not os.path.isdir ( vsdc.master_dir ) ):
        print ( "MASTER_DIR %s specified in the control file %s does not exist" % \
                ( vsdc.master_dir, vsdc.master_dir ) )
        return 1
   return 0

#   for exec in vsdc_exec_deps:
#       com = exec + " -version"
#       (ret, out ) = exe ( com )
#       if ( ret != 0 ):
##
## --------- Error in checking the executable
##
#            for line in out:
#                print ( line )
#            print ( "Tried to check whether we can execute ", exec )
#            return 1
#
#
# ------------------------------------------------------------------------
#
def vsdc_parse_ddf ( vsdc, vsdc__root_dir ):

    for path, dirs, files in os.walk(vsdc.ddf_dir):
        for file in files:
            if ( "#" in file ): continue
            if ( "~" in file ): continue
            vsdc.ddf_file_list.append ( path + "/" + file )

    if ( len(vsdc.ddf_file_list) < 1 ):
         print ( "No DDF files were found in %s directory" % vsdc.ddf_dir )
         print ( "Please run vsdc with option -u" )
         return 1

    for ddf_file in vsdc.ddf_file_list:
        ext = ddf_file[-4:-1] + ddf_file[-1]
        if ( not ext == ".ddf" ): continue
        ddf_buf = read_file ( ddf_file )
        if ( ddf_buf[0][0:len(vsdc__ddf_label)] != vsdc__ddf_label ):
             print ( "vsdc_parse_ddf: ddf_file %s does not have a valid label: expected %s, but got %s" % \
                      ( ddf_file, vsdc__ddf_label, ddf_buf[0] ) )
             return 1

        num_par = 0
        ddf_dict={}
        ddf_name = os.path.splitext(os.path.basename(ddf_file))[0]
        for line in ddf_buf:
            if ( line[0:1] == '#' ): continue
            if ( len(line) <   1  ): continue
            if ( len(line.split()) < 2 ): continue

            if   ( line.split()[0] == "Short_description:" ):
                   ddf_dict["short_description"] = line.replace("Short_description:","").lstrip()
                   num_par = num_par + 1
                   continue
            elif ( line.split()[0] == "Long_description:" ):
                   ddf_dict["long_description"] = line.replace("Long_description:","").lstrip()
                   num_par = num_par + 1
                   continue
            elif ( line.split()[0] == "Format_file_name:" ):
                   ddf_dict["format_file_name"] = line.replace("Format_file_name:","").lstrip()
                   num_par = num_par + 1
                   continue
            elif ( line.split()[0] == "Reference:" ):
                   ddf_dict["reference"] = line.replace("Reference:","").lstrip()
                   num_par = num_par + 1
                   continue
            elif ( line.split()[0] == "DOI:" ):
                   ddf_dict["doi"] = line.replace("DOI:","").lstrip()
                   num_par = num_par + 1
                   continue
            elif ( line.split()[0] == "Filenaming_scheme:" ):
                   ddf_dict["filenaming_scheme"] = line.replace("Filenaming_scheme:","").lstrip()
                   num_par = num_par + 1
                   continue
            elif ( line.split()[0] == "File_location:" ):
                   ddf_dict["file_location"] = line.replace("File_location:","").lstrip()
                   num_par = num_par + 1
                   continue
            elif ( line.split()[0] == "Product_ID:" ):
                   ddf_dict["product_id"] = line.replace("Product_ID:","").lstrip()
                   num_par = num_par + 1
                   continue
            elif ( line.split()[0] == "Data_type:" ):
                   ddf_dict["data_type"] = line.replace("Data_type:","").lstrip()
                   num_par = num_par + 1
                   continue
            elif ( line.split()[0] == "Data_content_type:" ):
                   ddf_dict["data_content_type"] = line.replace("Data_content_type:","").lstrip()
                   num_par = num_par + 1
                   continue
            elif ( line.split()[0] == "Data_format:" ):
                   ddf_dict["data_format"] = line.replace("Data_format:","").lstrip()
                   num_par = num_par + 1
                   continue
#            elif ( line.split()[0] == "Date_extraction_proc:" ):
#                   ddf_dict["date_extraction_proc"]   = line.replace("Date_extraction_proc:","").lstrip()
#                   if ( not "/" in ddf_dict["date_extraction_proc"] ):
#                        ddf_dict["date_extraction_proc"] = vsdc.vsdc_dir_name + "/" + \
#                                  ddf_dict["date_extraction_proc"] 
#                   num_par = num_par + 1
#                   continue
            elif ( line.split()[0] == "Validate_proc:" ):
                   ddf_dict["validate_proc"] = line.replace("Validate_proc:","").lstrip()
                   if ( not "/" in ddf_dict["validate_proc"] ):
                        ddf_dict["validate_proc"] = vsdc__root_dir + "/" + \
                                  ddf_dict["validate_proc"] 
                   num_par = num_par + 1
                   continue
            elif ( line.split()[0] == "Magic:" ):
                   ddf_dict["magic"] = line.replace("Magic:","").lstrip()
                   num_par = num_par + 1
                   continue
            elif ( line.split()[0] == "Compression_type:" ):
                   ddf_dict["compression_type"] = line.replace("Compression_type:","").lstrip()
                   num_par = num_par + 1
                   continue

        if ( not ( ddf_dict["data_type"] == "MISC"    or \
                   ddf_dict["data_type"] == "VLBI"    or \
                   ddf_dict["data_type"] == "SLR"     or \
                   ddf_dict["data_type"] == "CNSS"       ) ):
             print ( "Error in processing ddf file %s -- unsupported Data_type %s" % \
                     ( ddf_file, ddf_dict["data_type"] ) )
             return 1

        if ( not ( ddf_dict["data_content_type"] == "Misc"    or \
                   ddf_dict["data_content_type"] == "Data"    or \
                   ddf_dict["data_content_type"] == "SWIN"    or \
                   ddf_dict["data_content_type"] == "Product"    ) ):
             print ( "Error in processing ddf file %s -- unsupported Data_content_type %s" % \
                     ( ddf_file, ddf_dict["data_type"] ) )
             return 1

        if ( not ( ddf_dict["data_format"] in vsdc_data_formats ) ):
             print ( "Error in processing ddf file %s -- unsupported Data_format %s" % \
                     ( ddf_file, ddf_dict["data_format"] ) )
             return 1
           
#        if ( not os.path.isfile(ddf_dict["date_extraction_proc"]) ):
#             print ( "Error in processing ddf file %s -- did not find Date_extraction_proc %s" % \
#                     ( ddf_file, ddf_dict["date_extraction_proc"] ) )
#             return 1
#
        if ( not os.path.isfile(ddf_dict["validate_proc"]) ):
             print ( "Error in processing ddf file %s -- did not find Validate_proc %s" % \
                     ( ddf_file, ddf_dict["validate_proc"] ) )
             return 1

        if ( ddf_dict["magic"][0] == "'" and ddf_dict["magic"][-1] == "'" ):
             ddf_dict["magic"] = ddf_dict["magic"][1:-1] 

        if ( ddf_dict["magic"][0] == '"' and ddf_dict["magic"][-1] == '"' ):
             ddf_dict["magic"] = ddf_dict["magic"][1:-1] 

        vsdc.ddf[ddf_name] = ddf_dict
        if ( num_par < vsdc__ddf_pars ):
             print ( "Not all keywords were found in the ddf file " + \
                      ddf_file + " -- only %d, though %d were expected" % \
                      ( num_par, vsdc__ddf_pars ) )
             return 1
    return 0
