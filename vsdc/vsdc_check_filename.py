import os, sys
from   vsdc_misc import *
from   vsdc_config import *
#
# ------------------------------------------------------------------------
#
def vsdc_check_filename ( vsdc, file_type, file_name ):
    """
    Procedure for checking, whether the file name of the dataset
    of the specified type that is being submitted is valid
    The routine exits with code  1 on failure and return 
    a 4-element tulip of ( date_str, vers_str, sess_str, sta_str )
    where 

          date_str -- date of the observing session in YYYYMMDD
                      format extracted from the file name
                      or "" if the file name does not contain
                      date.
 
          vers_str -- version counter or "" if the file name does not
                      contain version.

          sess_str -- lower case session name or "" if the file name
                      conttauns no VLBI observing session name.

          sta_str  -- lower case station name or "" if the file name
                      does not contain a name of the specific observing 
                      station
    """

    date_val = None
    date_str = ""
    vers_str = ""
    sess_str = ""
    suff_str = ""
    sta_str  = ""


    if ( not file_type in vsdc.ddf ):
         print ( "Unknown data type file_type %s. Please create a ddf file for this data type" % \
                  file_type )
         return ( None, None, None, None, None, None)

    if ( not "@" in vsdc.ddf[file_type]["filenaming_scheme"] ):
#
# ------ Just plain name in the pattern. We check whether the supplied 
# ------ file name is the name specified in the DDF
#
         if ( os.path.basename(file_name) != vsdc.ddf[file_type]["filenaming_scheme"] ):
              print ( "Wrong file name %s -- expected name: %s" % ( file_name, vsdc.ddf[file_type]["filenaming_scheme"] ) )
              return ( None, None, None, None, None, None )
    else:
#
# ------ Extract the file name pattern
#
         fs = vsdc.ddf[file_type]["filenaming_scheme"]
#
# ------ get rid of the directory name part in the pattern
#
         if ( "/" in fs ):
              fs = fs[fs.rfind("/")+1:]
#
# ------ We expand @-expressions, and it may require more than one pass
#
         for i in range(0,len(fs)):
#
# ---------- Check whether the pattern has @-expression
#
             if ( "@" in fs ):
                  ir = fs.find("@")
                  if (  "@{date{%y}}" in fs ):
#
# ---------------------- Date as a two digit year.
# ---------------------- Extract the date into date_str
#
                         date_str = os.path.basename(file_name)[ir:ir+2]
#
# ---------------------- Check whether the date field conforms specifications
#
                         try:
                             date_val = datetime.datetime.strptime ( date_str, "%y" )
                         except:
                             print ( "Wrong date file %s in the file name %s" % \
                                      ( date_str, file_name ) )
                             return ( None, None, None, None, None, None )
#
# ---------------------- Replace the @-expresion with the date_str in the pattern
# 
                         fs = fs[0:ir] + date_str + fs[ir+len("@{date{%y}}"):]
                  elif ( "@{date{%Y}}" in fs ):
#
# ---------------------- Date as a four digit year
#
                         date_str = os.path.basename(file_name)[ir:ir+4] 
#
# ---------------------- Check whether the date field conforms specifications
#
                         try:
                             date_val = datetime.datetime.strptime ( date_str, "%Y" )
                         except:
                             print ( "Wrong date file %s in the file name %s" % \
                                      ( date_str, file_name ) )
#
# ---------------------- Replace the @-expresion with the date_str in the pattern
# 
                         fs = fs[0:ir] + date_str + fs[ir+len("@{date{%Y}}"):]
                  elif ( "@{date{%y%b}}" in fs ):
#
# ---------------------- Date as a two digit year and three letter month in the upper case
#
                         date_str = os.path.basename(file_name)[ir:ir+5] 
#
# ---------------------- Check whether the date field conforms specifications
#
                         try:
                             date_val = datetime.datetime.strptime ( date_str, "%y%b" )
                         except:
                             print ( "Wrong date file %s in the file name %s" % \
                                      ( date_str, file_name ) )
#                         date_str = date_str[0:3] + date_str[3:5].lower() # looks like we do not need this
#
# ---------------------- Replace the @-expresion with the date_str in the pattern
# 
                         fs = fs[0:ir] + date_str + fs[ir+len("@{date{%y%b}}"):]
                  elif ( "@{date{%y%b%d}}" in fs ):
#
# ---------------------- Date as a two digit year, three letter month in the upper case
#
                         date_str = os.path.basename(file_name)[ir:ir+7] 
#
# ---------------------- Check whether the date field conforms specifications
#
                         try:
                             date_val = datetime.datetime.strptime ( date_str, "%y%b%d" )
                         except:
                             print ( "Wrong date file %s in the file name %s" % \
                                      ( date_str, file_name ) )
#
# ---------------------- Replace the @-expresion with the date_str in the pattern
# 
                         fs = fs[0:ir] + date_str + fs[ir+len("@{date{%y%b%d}}"):]
                  elif ( "@{date{%Y%m%d}}" in fs ):
#
# ---------------------- Date as a YYYYMMDD (f.e. 20191230 )
#
                         date_str = os.path.basename(file_name)[ir:ir+8]
#
# ---------------------- Check whether the date field conforms specifications
#
                         try:
                             date_val = datetime.datetime.strptime ( date_str, "%Y%m%d")
                         except:
                             print ( "Wrong date file %s in the file name %s" % \
                                      ( date_str, file_name ) )
#
# ---------------------- Replace the @-expresion with the date_str in thppe pattern
# 
                         fs = fs[0:ir] + date_str + fs[ir+len("@{date{%Y%m%d}}"):]
                  elif ( "@{sess}" in fs ):
#
# ---------------------- Session code in lower case
#
                         if ( "__" in os.path.basename(file_name) ):
                              print ( "Two underscores __ are prohibited in file names %s" % \
                                       os.path.basename(file_name) )
                              return ( None, None, None, None, None, None )
#
# ---------------------- Search for the session code in the pattern string. 
# ---------------------- Try several possibilities
#
                         if ( "_@{sess}_" in fs ): 
# 
# --------------------------- The session code may be encompassed with _
#
                              try:
                                 ib = os.path.basename(file_name).index ( "_" )
                                 ie = os.path.basename(file_name).index ( "_", ib+1 )
                              except:
                                 print ( "Did not find two underscores in file name %s" % \
                                          os.path.basename(file_name) )
                                 return ( None, None, None, None, None, None )
                         elif ( "@{sess}." in fs ): 
#
# --------------------------- The session code may preceed a file extension
#
                              ib = -1
                              ie = os.path.basename(file_name).index ( "." )
                         elif ( "@{sess}@{sta}." in fs ):
#
# --------------------------- The session code may preceed the station name 
# --------------------------- followed by the file extension
#
                              ib = -1
                              ie = os.path.basename(file_name).index ( "." ) - 2
                         elif ( "@{sess}@{sta}_full." in fs ):
#
# --------------------------- The session code may preceed the station name 
# --------------------------- followed by the file extension
#
                              ib = -1
                              ie = os.path.basename(file_name).index ( "." ) - 7
                         else:
                              print ( "Trap of internal control: unsupported combination of @{sess} "\
                                      " in the Filenaming_scheme of the type %s -- %s" % \
                                       ( file_type, vsdc.ddf[file_type]["filenaming_scheme"] ) )

                         sess_str = os.path.basename(file_name)[ib+1:ie]
#
# ---------------------- Now we check whether the session code is specified in 
# ---------------------- the IVS master schedule files
#
                         if ( sess_str in vsdc.master.keys() ):
#
# --------------------------- If yes, then replace the name pattern with the session string
#
                              fs = fs[0:ib+1] + sess_str + fs[ib+1+len("@{sess}"):]
                              suff_str = vsdc.master[sess_str]["suffix"]
                              date_val = vsdc.master[sess_str]["date"]
                              date_str = datetime.datetime.strftime ( date_val, "%Y%m%d")
#                              for sess in vsdc.master.keys(): # %%%%%%%%
#                                  print ( "sss: ", sess, vsdc.master[sess] ) # %%%%%%%
                         else:
                              print ( "Session %s is not specified in IVS master files, therefore file %s of type %s is rejected" % \
                                      (  sess_str, file_name, file_type ) )
                              return ( None, None, None, None, None, None )
                  elif ( "@{suff}" in fs ):
                       try:
                           ie = os.path.basename(file_name).find(".")
                       except:
                           print ( "Malformed file name %s -- extension is not found" % \
                               file_name )
                           return ( None, None, None, None, None, None )
                       suff_str = os.path.basename(file_name)[7:ie]

#
# -------------------- Now we check whether the session code is specified in 
# -------------------- the IVS master schedule files
#
                       for sess_name in vsdc.master.keys():
                           exp_name = vsdc.master[sess_name]["date_str"][2:] + vsdc.master[sess_name]["suffix"]
                           if ( exp_name == os.path.basename(file_name)[0:ie] ):
                                sess_str = sess_name
                                suff_str = vsdc.master[sess_str]["suffix"]
                                date_val = vsdc.master[sess_str]["date"]
                                date_str = datetime.datetime.strftime ( date_val, "%Y%m%d")
                                continue

                       if ( sess_str == "" ):
                            print ( "Session %s is not specified in IVS master files, therefore file %s of type %s is rejected" % \
                                    (  os.path.basename(file_name)[0:ie], file_name, file_type ) )
                            return ( None, None, None, None, None, None )

                       fs = fs[0:ir] + suff_str + fs[ir+len("@{suff}"):]

                  elif ( "@{vers{%03d}}" in fs or \
                         "@{vers{%02d}}" in fs or \
                         "@{vers{%01d}}" in fs or \
                         "@{vers{%1d}}"  in fs    ):
#
# ---------------------- Version counter as an integer number with
# ---------------------- leading zeroes
#
                         if ( "__" in os.path.basename(file_name) ):
                              print ( "Two consecutive underscores __ are prohibited in file names %s" % \
                                       os.path.basename(file_name) )
                              return ( None, None, None, None, None, None )
#                         print ( " fs= ", fs ) # %%%%%%%%%%%%
                         try:
                            if ( "_v@{vers{%03d}}_" in fs or \
                                 "_v@{vers{%02d}}_" in fs or \
                                 "_v@{vers{%01d}}_" in fs or \
                                 "_v@{vers{%1d}}_"  in fs    ):
#
# ------------------------------ the field of version counter must be encompassed
# ------------------------------ with "_" and "."
#
                                 ie = os.path.basename(file_name).rindex( "_" ) 
                                 ib = os.path.basename(file_name).rindex( "_", 0, ie )
                            else:
#
# ------------------------------ the field of version counter must be encompassed
# ------------------------------ with "_" and "."
#
                                 ib = os.path.basename(file_name).rindex ( "_" )
                                 ie = os.path.basename(file_name).index  ( ".", ib+1 )
                         except:
                            print ( "Did not find three underscores in file name ", \
                                     os.path.basename(file_name) )
                            return ( None, None, None, None, None, None )
                         vers_str = os.path.basename(file_name)[ib+1:ie]
                         if ( vers_str[0:1] == "v" ) :
                              vers_str = vers_str[1:]
#
# ---------------------- Check whether the version counter is an integer
# ---------------------- number
#
                         try: 
                            vers = int(vers_str)
                         except:
                            print ( "Version field %s is not an integer number" % vers_str )
                            return ( None, None, None, None, None, None )
#
# ---------------------- Check four different formats of @{vers string
#
                         if ( "v@{vers{%03d}}" in fs ):
                              fs = fs[0:ib+1] + "v" + vers_str + fs[ib+1+len("v@{vers{%03d}}"):]
                              if ( len(vers_str) != 3 ):
                                   print ( "Version field %s should be three character long" % vers_str )
                                   return ( None, None, None, None, None, None )
                         if ( "v@{vers{%02d}}" in fs ):
                              fs = fs[0:ib+1] + "v" + vers_str + fs[ib+1+len("v@{vers{%02d}}"):]
                              if ( len(vers_str) != 2 ):
                                   print ( "Version field %s should be two character long" % vers_str )
                                   return ( None, None, None, None, None, None )
                         if ( "v@{vers{%01d}}" in fs ):
                              fs = fs[0:ib+1] + "v" + vers_str + fs[ib+1+len("v@{vers{%02d}}"):]
                              if ( len(vers_str) != 1 ):
                                   print ( "Version field %s should be one character long" % vers_str )
                                   return ( None, None, None, None, None, None )
                         if ( "@{vers{%03d}}" in fs ):
                              fs = fs[0:ib+1] + vers_str + fs[ib+1+len("@{vers{%03d}}"):]
                              if ( len(vers_str) != 3 ):
                                   print ( "Version field %s should be three character long" % vers_str )
                                   return ( None, None, None, None, None, None )
                         if ( "@{vers{%02d}}" in fs and len(vers_str) != 2 ):
                              fs = fs[0:ib+1] + vers_str + fs[ib+1+len("@{vers{%02d}}"):]
                              if ( len(vers_str) != 2 ):
                                   print ( "Version field %s should be two character long" % vers_str )
                                   return ( None, None, None, None, None, None )
                         if ( "@{vers{%01d}}" in fs and len(vers_str) != 1 ):
                              fs = fs[0:ib+1] + vers_str + fs[ib+1+len("@{vers{%01d}}"):]
                              if ( len(vers_str) != 1 ):
                                   print ( "Version field %s should be one character long" % vers_str )
                                   return ( None, None, None, None, None, None )
                         if ( "@{vers{%1d}}" in fs and len(vers_str) != 1 ):
                              fs = fs[0:ib+1] + vers_str + fs[ib+1+len("@{vers{%1d}}"):]
                              if ( len(vers_str) != 1 ):
                                   print ( "Version field %s should be one character long" % vers_str )
                                   return ( None, None, None, None, None, None )
                  elif ( "@{sta}" in fs ):
#
# ---------------------- Two letter long IVS observing station code
#
                         sta_str = os.path.basename(file_name)[ir:ir+2] 
                         if ( "@{sta}." in fs ): 
                              ib = -1
                              ie = os.path.basename(file_name).index ( "." )
                         elif ( "@{sta}_full." in fs ): 
                              ib = -1
                              ie = os.path.basename(file_name).index ( "." ) - 5
                         else:
                              print ( "Trap of internal control: unsupported combination of @{sta} in the Filenaming_scheme of the type %s -- %s" % \
                                       ( file_type, vsdc.ddf[file_type]["filenaming_scheme"] ) )
                              return ( None, None, None, None, None, None )
                         if ( sta_str in vsdc.nscodes.keys() ):
#
# --------------------------- Check whether the station name is in the list
# --------------------------- of IVS station names, and if yes, replace
# --------------------------- @{sta} expression in the file name pattern
#
                              fs = fs[0:ir] + sta_str + fs[ir+len("@{sta}"):]
                         else:
                              print ( "Station %s is not specified in IVS nscodes files, therefore file %s of type %s is rejected" % \
                                      (  sta_str, file_name, file_type ) )
                              return ( None, None, None, None, None, None )
                  else:
                     print ( "Error in parsing filenameing scheme %s in %s " % \
                             ( vsdc.ddf[file_type]["filenaming_scheme"], file_type ) )
                     print ( "An unkwown @-experssion was found" )
                     exit  ( 1 )

         if ( vsdc.verb > 0 ):
              print ( "vsdc_check_filename: date_str = ", date_str )
              print ( "vsdc_check_filename: vers_str = ", vers_str )
              print ( "vsdc_check_filename: sess_str = ", sess_str )
              print ( "vsdc_check_filename: sta_str  = ", sta_str  )
              print ( "vsdc_check_filename: suff_str = ", suff_str )
              print ( "vsdc_check_filename: date_val = ", date_val )

         if ( vsdc.ddf[file_type]["compression_type"] != "none" ):
              compr_suff = "." + vsdc.ddf[file_type]["compression_type"]
              if ( file_name[-len(compr_suff):] == compr_suff ):
                   fs = fs + "." + vsdc.ddf[file_type]["compression_type"] 

         if ( fs != os.path.basename(file_name) ):
              print ( "expected: ", fs )
              print ( "actual:   ", os.path.basename(file_name) )
              print ( "File name %s of type %s does not follow the CDDIS convenion" % \
                      ( os.path.basename(file_name), file_type ) )
              return ( None, None, None, None, None, None )

         
    if ( vsdc.verb > 0 ): print ( "File name check is OK" )
    return ( date_str, vers_str, sess_str, sta_str, suff_str, date_val )
