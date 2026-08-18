import sys, os, signal
from   vsdc_config          import *
from   vsdc_misc            import *

sect_type = [ \
              "CHUN", \
              "DATA", \
              "FILE", \
              "HEAP", \
              "PREA", \
              "TEXT", \
              "TOCS"  \
           ]


def vsdc_check_vgosda ( file_name ):
#
# --- Does the file exist?
#
    if ( not os.path.isfile ( file_name ) ):
         print ( "File %s does not exists" % file_name )
         return None 
    
#
# --- Is its length zero?
#
    if ( os.stat(file_name).st_size == 0 ):
         print ( "File %s has zero length " % file_name )
         return None 
    
#
# --- Isn't it too short?
#
    if ( os.stat(file_name).st_size < len(vsdc__vgosda_magic) ):
         print ( "File %s is too short" % file_name )
         return None 
    
#
# --- Let us read it 
#
    buf = read_file ( file_name )
    if ( buf == None ):
         print ( "Failure in reading input vgosda file ", file_name )
         return None

    if ( buf[0][0:len(vsdc__vgosda_magic)] != vsdc__vgosda_magic ):
         print ( "File %s does not have vgosda magic" % file_name )
         return None

    for line in buf:
        if ( line[0:len(vsdc__vgosda_magic)] == vsdc__vgosda_magic ): continue
        sect_name = line.split()[0].split(".")[0]
        sect_ind_str =  line.split()[0].split(".")[1]
#
# ----- Check, whether the section ID is OK
#
        if ( not sect_name in sect_type ):
             print ( "Unsupported section type in line %s was found" % line )
             return ( None )
#        
# ----- Check whether the section index is an integer number
#
        try:
           sect_ind = int(sect_ind_str)
        except:
           print ( "Unsuppored section counter in line %s was found: a positive integer was expected" % line )
           return ( None )

        if ( sect_name == "DATA" and len(line.split()) < 7 ):
             if ( "@" in line ): continue
#             if ( "QUALCODE" in line ): continue
             print ( "Too few words in the DATA section in line %s -- 7 words were expected" % line )
             return ( None )

    return ( 0 ) 
