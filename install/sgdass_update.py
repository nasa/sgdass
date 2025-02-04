#!/usr/bin/env python3
import  stat, string, sys, os, subprocess, getpass, datetime
from    pet_misc  import *
# ************************************************************************
# *                                                                      *
# *  Program sgdass_update is for regeneration of the sgdass repository. *
# *                                                                      *
# *  ### 01-MAR-2021 sgdass_update  v2.2 (c) L. Petrov  07-FEB-2024 ###  *
# *                                                                      *
# ************************************************************************


prog_dir         = "/progs"
sgdass_dir       = "/progs/sgdass"
sgdass_tree_dir  = "/progs/sgdass/sgdass"
sgdass_file_list = sgdass_dir + "/sgdass_filelist.txt"

#
# ------------------------------------------------------------------------
#
def main():

    if ( len(sys.argv)-1 < 1 ):
         print ( "Usage: sgdass_update.py tars|distro|git|git-clean|conf|build output_dir|conf" )
         exit  ( 1 )
    else:
         op = sys.argv[1]

    verb = 1

    if ( op == "distro" ):
         if ( len(sys.argv)-1 < 2 ):
              print ( "Distribution filename should be specified in the second argument" )
              exit  ( 1 )
         
         date_string = str(datetime.datetime.now().strftime("%Y%m%d"))
         output_dir = sys.argv[2]
         output_tarball = output_dir + "/sgdass-" + date_string + ".tar"
         output_readme  = output_dir + "/sgdass-" + date_string + "_INSTALL.txt"
    if ( op == "conf" or op == "build" ):
         conf_file = sys.argv[2]
         if ( len(sys.argv)-1 >= 3 ):
              verb = int ( sys.argv[3] )

    package_list = read_file ( sgdass_file_list )

    if ( op == "tars" ):
    
         for line in package_list:
             if ( line[0:1] == "#" or line[0:1] == " " ): continue
             package = line.split()[0]
             version = line.split()[1]
             suffix  = line.split()[2]
             type    = line.split()[3]
             if ( len(line.split()) >= 5 ):
                  origin = line.split()[4]
             else:
                  origin = ""

             if ( type == "sgdass" ):
                  tarball_from = origin + "/" + package + "-" + version + suffix
                  tarball_to   = sgdass_dir + "/" + package + "-" + version + suffix
                   
                  com = "cp -pv " + tarball_from + " " + tarball_to
                  print ( "Updating ", tarball_to )
                  (ret,out) = exe ( com )
                  if ( ret != 0 ):
                       for line in out:
                           print ( line )
                       print ( "ERROR in running command " + com )
                       exit ( 1 )
         print ( "tarballs in %s are updated" % sgdass_dir )
         
    elif ( op == "distro" ):
           tar_com  = "cd " + sgdass_dir + "; chmod o=r,g+rw *; cd ../ ; tar -cvf " + output_tarball + " "
           copy_com = "cd " + sgdass_dir

           for line in package_list:
               if ( line[0:1] == "#" or line[0:1] == " " ): continue
               package = line.split()[0]
               version = line.split()[1]
               suffix  = line.split()[2]
               type    = line.split()[3]
               if ( len(line.split()) >= 5 ):
                    origin = line.split()[4]
               else:
                    origin = ""

               if ( ".tar" in suffix or ".tgz" in suffix or ".jar" == suffix ):
                     tar_file = "sgdass/" + package + "-" + version + suffix
                     if ( not os.path.isfile(sgdass_dir + "/" + tar_file.replace("sgdass/","")) ):
                           tar_file = "sgdass/" + package + version + suffix
                     tar_com  = tar_com + " " + tar_file
                     if ( type == "data" ):
                          tar_file = "sgdass/" + package + "-" + version + suffix
                          copy_com = copy_com + "; cp " + sgdass_dir + "/" + package + "-" + \
                                     version + suffix + " " + output_dir + "/"
                          if ( not os.path.isfile(sgdass_dir + "/" + tar_file.replace("sgdass/","")) ):
                               tar_file = "sgdass/" + package + version + suffix
                               copy_com = copy_com + "; cp " + sgdass_dir + "/" + package + \
                                     version + suffix + " " + output_dir + "/"

               elif ( ".patch" == suffix ):
                     if ( version == "n/a" ):
                          patch_file = "sgdass/" + package + suffix
                     else:
                          patch_file = "sgdass/" + package + "-" + version + suffix
                     tar_com = tar_com + " " + patch_file
               else:
                     tar_com = tar_com + " " + "sgdass/" + package
    

           if ( verb >= 1 ):
                print ( tar_com )
           (ret,out) = exe ( tar_com )
           if ( ret != 0 ):
                for line in out:
                    print ( line )
                print ( "ERROR in running command " + tar_com )
                exit  ( 1 )

           if ( verb >= 1 ):
                print ( copy_com )
           (ret,out) = exe ( copy_com )
           if ( ret != 0 ):
                for line in out:
                    print ( line )
                print ( "ERROR in running command " + copy_com )
                exit  ( 1 )
#
           com = "cp -pv " + sgdass_dir + "/INSTALL " + output_readme
           (ret,out) = exe ( com )
           if ( ret != 0 ):
                for line in out:
                    print ( line )
                print ( "ERROR in running command " + com )
                exit ( 1 )

           print ( "Created distibution tar     file " + output_tarball )
           print ( "Created distibution INSTALL file " + output_readme  )
    elif ( op == "git" ):
           if ( not os.path.isdir ( sgdass_tree_dir ) ):
                try:
                    os.mkdir ( sgdass_tree_dir, mode=0o775 ) 
                except Exception as e:
                    print ( "Error in an attempt to create temporary directory %s" % \
                             sgdass_tree_dir )
                    exit ( 1 )
           
           com = "rm -fR " + sgdass_tree_dir + "/*"
           (ret,out) = exe ( com )

           os.mkdir ( sgdass_tree_dir + "/install", mode=0o775 ) 
           os.mkdir ( sgdass_tree_dir + "/config",  mode=0o775 ) 

           for line in package_list:
               if ( line[0:1] == "#" or line[0:1] == " " ): continue
               if ( len(line.split()) < 4 ): continue
               package = line.split()[0]
               version = line.split()[1]
               suffix  = line.split()[2]
               type    = line.split()[3]
               if ( len(line.split()) >= 5 ):
                    origin = line.split()[3]
               else:
                    origin = ""

               if ( type == "sgdass" ):
#
# ----------------- Extract a package 
#
                    com = "tar -C " + sgdass_tree_dir + " -jxf " + sgdass_dir + "/" + package + "-" + version + suffix
                    print ( "Extracting %s package" % package )
                    (ret,out) = exe ( com )
                    if ( ret != 0 ):
                         for line in out:
                             print ( line )
                         print ( "ERROR in running command " + com )
                         exit  ( 1 )
#
# ----------------- And rename it
#
                    com = "mv " + sgdass_tree_dir + "/" + package + "_" + version + " " + sgdass_tree_dir + "/" + package 
                    (ret,out) = exe ( com )
                    if ( ret != 0 ):
                         for line in out:
                             print ( line )
                         print ( "ERROR in running command " + com )
                         exit  ( 1 )

               elif ( type == "root" ):
                    com = "cp -p " + sgdass_dir + "/" + package + " " + sgdass_tree_dir + "/"
                    print ( "Copying %s file" % package )
                    (ret,out) = exe ( com )
                    if ( ret != 0 ):
                         for line in out:
                             print ( line )
                         print ( "ERROR in running command " + com )
                         exit  ( 1 )

               elif ( type == "configure" ):
                    com = "cp -p " + sgdass_dir + "/" + package + " " + sgdass_tree_dir + "/config/"
                    print ( "Copying %s file" % package )
                    (ret,out) = exe ( com )
                    if ( ret != 0 ):
                         for line in out:
                             print ( line )
                         print ( "ERROR in running command " + com )
                         exit  ( 1 )

               elif ( type == "install" ):
                    com = "cp -p " + sgdass_dir + "/" + package + " " + sgdass_tree_dir + "/install/"
                    print ( "Copying %s file" % package )
                    (ret,out) = exe ( com )
                    if ( ret != 0 ):
                         for line in out:
                             print ( line )
                         print ( "ERROR in running command " + com )
                         exit  ( 1 )

               elif ( type == "documentation" ):
                    com = "cp -p " + sgdass_dir + "/" + package + " " + sgdass_tree_dir + "/"
                    print ( "Copying %s file" % package )
                    (ret,out) = exe ( com )
                    if ( ret != 0 ):
                         for line in out:
                             print ( line )
                         print ( "ERROR in running command " + com )
                         exit  ( 1 )

#
# -------- Remove files  with "~" and "#" at the end 
#
           for path, dirs, files in os.walk(sgdass_tree_dir):
               for file in files:
                   if ( file[-1:] == "~"      or \
                        file[-1:] == "#"      or \
                        file[-2:] == ".o"     or
                        file[-6:] == ".opt_o" or \
                        file[-6:] == ".o_opt" ): 
                        os.unlink ( path + "/" + file )
                   if ( ".#" in file ):
                        os.unlink ( path + "/" + file )
                   if ( "__pycache__" in path ):
                        os.unlink ( path + "/" + file )
                   if ( "obsolete_doc" in path ):
                        os.unlink ( path + "/" + file )

    elif ( op == "git_clean" ):
           if ( os.path.isdir ( sgdass_tree_dir ) ):
                com = "rm -fR " + sgdass_tree_dir
                (ret,out) = exe ( com )
                if ( ret != 0 ):
                     for line in out:
                         print ( line )
                     print ( "ERROR in running command " + com )
                     exit ( 1 )
                print ( "Cleaned git directory tree %s" % sgdass_tree_dir )
           else:
                print ( "There is no directory tree. Nothing to do" )
                
    elif ( op == "conf" or op == "build" ):
           conf = read_file ( conf_file )
           if ( not conf ):
                print ( "Did not find configuration file %s" % conf_file )
                exit  ( 1 )
           out = []
           np = 0
           for line in conf:
               if ( len(line) == 0 ):
                    out.append ( line )
                    continue
               if ( len(line.split()) == 0 ):
                    out.append ( line )
                    continue
                    
               if ( line.split()[0] == "package:" ): 
                    package_name = line.split()[1]
               if ( line.split()[0] == "version:" ): 
                    old_version_name = line.split()[1]
                    new_version_name = None
                    for lin in package_list:
                        if ( lin[0:1] == "#" or lin[0:1] == " " ): continue
                        if ( lin.split()[2] == ".patch"         ): continue
                        if ( package_name == lin.split()[0] ):
                             new_version_name = lin.split()[1]
                    if ( verb >= 2 ):
                         print ( "package %-20s old_version: %-8s new_version: %s" % \
                                 ( package_name, old_version_name, new_version_name ) )
                    if ( new_version_name == None ):
                         print ( "Did not find package of %s in file list %s" % \
                                 (package_name, sgdass_file_list) )
                         exit ( 1 )
                    if ( new_version_name != old_version_name ):
                         print ( "Updated version of package %s" % package_name )
                         line = line.replace(old_version_name,new_version_name)
                         if ( verb >= 2 ):
                              print ( "Updated version for package %s" % package_name )
                         np = np + 1
               out.append ( line )
           if ( np > 0 ):
                new_conf_file = conf_file.replace(".cnf",".cnf.new")
                f=open(new_conf_file,"w")
                for line in out:
                    if ( "sgas_config" in line ):
                         line = line.replace("sgas_config","sgdass_config")
                    print ( line, file=f )
                f.close()
                com = "mv " + new_conf_file + " " + conf_file
                (ret,out) = exe ( com )
                print ( "Updated configuration file ", conf_file )
           else:
                if ( op == "conf" ):
                     print ( "Configiration file %s is up to date" % conf_file )

    if ( op == "build" ):
        change_file = sgdass_dir + "/CHANGES"
        if ( not os.path.isfile(change_file) ):
             print ( "Error: did not find file %s" % change_file )
             exit  ( 1 )
        change_buf = read_file ( change_file )
        if ( len(change_buf) < 1 ):        
             print ( "Error:change file %s is empty" % change_file )
             exit  ( 1 )
        if ( len(change_buf[0].split()) < 2 ):        
             print ( "Error:change file %s is malformed" % change_file )
             exit  ( 1 )
        build_options = change_buf[0].replace(change_buf[0].split()[0],"").ljust(0)
        sgdass_vers = change_buf[0].split()[0].replace(".","")

        comstr = sgdass_dir + "/sgdass_install.py -c " + conf_file + " " + build_options
 
        (ret,err) = exe_pipe ( comstr )
        if ( ret != 0 ):
             print ( "SGDASS upgrade failed" )
             exit  ( 1 )
        else:
             print ( "SGDASS has been successfully upgraded to version %s" % sgdass_vers )
             exit  ( 1 )


#
# ------------------------------------------------------------------------
#
if __name__ == "__main__":
    try:
        pyvers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( pyvers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
        main()
    except KeyboardInterrupt:
        print ( "\nsgdass_update.py: Interrupted" )
        exit ( 1 )
