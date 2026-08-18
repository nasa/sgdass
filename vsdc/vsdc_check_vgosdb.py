import sys, os, signal, tarfile

vgosdb_dirs = [ \
                   "Apriori", \
                   "CrossReference", \
                   "Head.nc", \
                   "History", \
                   "ObsCalTheo", \
                   "ObsDerived", \
                   "ObsEdit", \
                   "ObsPart", \
                   "ObsTheoretical", \
                   "Observables", \
                   "Scan", \
                   "Session", \
                   "Solve"  \
              ]

def vsdc_check_vgosdb ( file_name ):
    """
    Check a tar file in Vgosdb format
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
    exp_name = os.path.basename(file_name).replace(".tgz","")

    for i in range (0,num_files):
        finam = f.getmembers()[i].name 
        if ( len(finam) < len(exp_name) ):
             print ( "The tarfile with vgosdb %s contains a file %s with too short name" % \
                     ( file_name, finam ) )
             return 1
        if ( finam[0:len(exp_name)] != exp_name ):
             print ( "The tarfile with vgosdb %s contains a file %s that does not start with experiement name" % \
                     ( file_name, finam ) )
             return 1

        if ( finam[0:len(exp_name)+1] == exp_name + "/" ):
             db_dir = finam.split("/")[1]

             if ( db_dir in vgosdb_dirs ):
#
# --------------- OK
#
                  continue
             if ( len(db_dir) > len(exp_name) ):
                  if ( db_dir[0:len(exp_name)] == exp_name and \
                       db_dir[-4:] == ".wrp"                   ):
                       continue
             if ( len(db_dir) < 4 or len(db_dir) > 8 ):
                  print ( "The tarfile with vgosdb %s contains a file %s with subdirectory %s that is is not recognized" % \
                     ( file_name, finam, db_dir ) )

             continue
    return 0
  
