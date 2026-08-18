import sys, os, signal, tarfile

def vsdc_check_difx ( file_name ):
    """
    Check a tar file in DiFX format
    """
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
    if ( os.stat(file_name).st_size < 256 ):
         print ( "File %s is too short" % file_name )
         return 1

    if ( not tarfile.is_tarfile ( file_name ) ):
         print ( "File %s is too a tar archive" % file_name )
         return 1
 
    f=tarfile.open ( name=file_name, mode='r' )

    num_files = len(f.getmembers())

    finam_vex = None
    finam_v2d = None

    for i in range (0,num_files):
        finam = f.getmembers()[i].name 
        if ( len(finam) < 4 ):
             print ( "The tarfile with difx output %s contains a file %s with too short name" % \
                     ( file_name, finam ) )
             return 1

        if ( not ( ".calc"       in finam or \
                   ".difx"       in finam or \
                   ".difxlog"    in finam or \
                   ".errs"       in finam or \
                   ".flag"       in finam or \
                   ".im"         in finam or \
                   ".input"      in finam or \
                   ".input.orig" in finam or \
                   ".machines"   in finam or \
                   ".threads"    in finam or \
                   ".v2d"        in finam or \
                   ".vex.obs"    in finam    ) ):
             print ( "The tarfile with difx output %s contains a file %s with unrecoginized extension" % \
                     ( file_name, finam ) )
             return 1
        
        if ( not ( ".vex.obs" in finam  or \
                   ".v2d"     in finam     ) ):
             if ( not "_" in finam ):
                  print ( "The tarfile with difx output %s contains a file %s with name that does not contain _" % \
                          ( file_name, finam ) )
                  return 1
             if ( not "." in finam ):
                  print ( "The tarfile with difx output %s contains a file %s with name that does not contain ." % \
                          ( file_name, finam ) )
                  return 1

             ib = finam.index("_")
             ie = finam.index(".")

             if ( ie < ib + 1 ):
                  print ( "The tarfile with difx output %s contains a file %s with does not have extension part" % \
                          ( file_name, finam ) )
                  return 1
     
             try:
                  ext_ind = int ( finam[ib+1:ie] )
             except:
                  print ( "The tarfile with difx output %s contains a file %s with name that does not have an integer extent" % \
                          ( file_name, finam ) )
                  return 1

        if ( ".v2d" in finam ):
             finam_v2d = finam

        if ( ".vex" in finam ):
             finam_vex = finam

#    if ( not finam_v2d ):
#         print ( "The tarfile with difx output %s doesn not contains a v2d file" % \
#                  file_name )
#         return 1
#
#    if ( not finam_vex ):
#         print ( "The tarfile with difx output %s doesn not contains a vex file" % \
#                  file_name )
#         return 1
#            

#    for i in range (0,num_files):
#        print ( "members: ", f.getmembers()[i].name )
#        if ( "h_"     in f.getmembers()[i].name and \
#             ".input" in f.getmembers()[i].name     ):
#             g=f.extractfile(f.getmembers()[i].name)
#             print ( "g=",g )
#             for line in g:
#                 print ( line.decode("latin").strip("\n").strip("\r") )
#             g.close()

    return 0
