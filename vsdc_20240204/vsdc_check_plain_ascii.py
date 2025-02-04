import sys, os, signal
def vsdc_check_plain_ascii ( file_name, magic_str):
    """
    Checkl a plain ascii file
    """
    magic_length = len(magic_str)
#
# --- Does the file exist?
#
    if ( not os.path.isfile ( file_name ) ):
         print ( "File %s does not exists" % file_name )
         return 1
    
#
# --- Is its length zero?
#
    if ( os.stat(file_name).st_size == 0 ):
         print ( "File %s has zero length " % file_name )
         return 1
    
#
# --- Isn't it too short?
#
    if ( os.stat(file_name).st_size < magic_length ):
         print ( "File %s is too short" % file_name )
         return 1
    
#
# --- Let us read it 
#
    
    try:
        with open(file_name,encoding="latin") as f:
             buf = f.read().splitlines()
        f.close()
    except BaseException as e:
        print ( "Failure in reading: %s " % str(e) )
        return 1
    
#
# --- Check the magic
#
    if ( magic_length > 0 ):
         if ( buf[0][0:magic_length] != magic_str ):
              print ( "Wrong first line of file %s -- expected %s but got %s" % \
                      (file_name, magic_str, buf[0][0:magic_length] ) )
              return 1
    return 0
