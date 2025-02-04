#!/usr/bin/env python3
import  stat, string, sys, os, subprocess, getpass, datetime, argparse
from    pet_misc  import *

sgdass_install__label = "sgdass_install 1.19 version of 2025.01.18"
sgdass_cfg__label     = "# sgdass_config  Version 1.4   of 2021.12.13"
cmake_vers__min = "2.8.0"

#
# ------------------------------------------------------------------------
#
class cnf_class:
   def __init__ ( self, config_file  ):
       self.filename      = config_file
       self.dirs          = []
       self.subdirs       = []
       self.gcc           = None
       self.gcx           = None
       self.gfortran      = None
       self.sdk           = None
       self.cla           = None
       self.clap          = None
       self.cmake         = None
       self.cmake_vers    = None
       self.tarball       = None
       self.build         = None
       self.build_aux     = None
       self.prefix        = None
       self.num_proc      = None
       self.center_abr    = None
       self.center_name   = None
       self.control_file  = None
       self.ld_path       = {}
       self.package       = {}
       self.postinstall   = {}
   def init ( self):
       __init__ ( self, config_file  )

#
# ------------------------------------------------------------------------
#
def parse_control_file ( cnf, inplace, verb ):

    buf = read_file ( cnf.filename )
    mode = "??"
    num_pack = 0

    if ( buf[0] != sgdass_cfg__label ):
         print ( "Wrong sgdass_install configuration file %s" % cnf.filename )
         print ( "It has first line %s while %s was expected" % (buf[0], sgdass_cfg__label) )
         return ( 1, None )


    for line in buf:
        if ( len(line.split()) < 1 ): continue
        if ( line[0:1] == "#"      ): continue
        if ( line.find("#") > 0    ):
#
# ---------- Remove comments after tailing #. But "#" may be a part of the command.
# ---------- Then we needto keep it
#
             line = line.replace("\\#","%^%^%^%")
             if ( line.find("#") > 0    ):
                  line = line[0:line.find("#")]
             line = line.replace("%^%^%^%","#")

        if ( cnf.build       ): line = line.replace("${build}",cnf.build)
        if ( cnf.build_aux   ): line = line.replace("${build_aux}",cnf.build_aux)
        if ( cnf.prefix      ): line = line.replace("${prefix}",cnf.prefix)
        if ( cnf.num_proc    ): line = line.replace("${num_proc}",cnf.num_proc)
        if ( cnf.gcc         ): line = line.replace("${gcc}",cnf.gcc)
        if ( cnf.gcx         ): line = line.replace("${gcx}",cnf.gcx)
        if ( cnf.gfortran    ): line = line.replace("${gfortran}",cnf.gfortran)
        if ( cnf.sdk         ): line = line.replace("${sdk}",cnf.sdk)
        if ( cnf.cla         ): line = line.replace("${cla}",cnf.cla)
        if ( cnf.clap        ): line = line.replace("${clap}",cnf.clap)
        if ( cnf.cmake       ): line = line.replace("${cmake}",cnf.cmake)
        if ( cnf.center_abr  ): line = line.replace("${center_abr}",cnf.center_abr)
        if ( cnf.center_name ): line = line.replace("${center_name}",cnf.center_name)


        if ( "[Directories]" in line ):
             mode = "directories"

        if ( "[SubDirectories]" in line ):
             mode = "subdirectories"

        if ( "[Where]" in line ):
             mode = "where"

        if ( "[Compilers]" in line ):
             mode = "compilers"
             continue

        if ( "[Misc]" in line ):
             mode = "misc"
             continue

        if ( "[Tests]" in line ):
             mode = "tests"
             build_dir = cnf.build
             submode = "??"
             continue

        if ( "[AuxPackages]" in line ):
             mode = "packages"
             build_dir = cnf.build_aux
             submode = "??"
             continue

        if ( "[Packages]" in line ):
             mode = "packages"
             build_dir = cnf.build
             submode = "??"
             continue

        if ( "[PostInstall]" in line ):
             mode = "postinstall"
             continue

        if ( mode == "??" ):
             print ( "Error in configuration file %s -- mode was not defined" % \
                      config_file )
             return ( 1, None )

        if ( mode == "directories" ):
             if ( line.split()[0] == "dir:"  ): 
                  cnf.dirs.append ( line.split()[1] )

        if ( mode == "subdirectories" ):
             if ( line.split()[0] == "subdir:"  ): 
                  cnf.subdirs.append ( line.split()[1] )

        if ( mode == "compilers" ):
             if ( line.split()[0] == "gcc"      ): 
                  cnf.gcc= line.split()[1] 
                  if ( not "/" in cnf.gcc and not cnf.gcc.lower() == "none" ):
                       (ret,out) = exe ( "which " + cnf.gcc )
                       cnf.gcc = out[0]

             if ( line.split()[0] == "gcx"      ): 
                  cnf.gcx= line.split()[1] 
                  if ( not "/" in cnf.gcx and not cnf.gcx.lower() == "none" ):
                       (ret,out) = exe ( "which " + cnf.gcx )
                       cnf.gcx = out[0]

             if ( line.split()[0] == "gfortran" ): 
                  cnf.gfortran = line.split()[1] 
                  if ( cnf.gfortran.lower() != "none" ):
                       if ( not "/" in cnf.gfortran ):
                            (ret,out) = exe ( "which " + cnf.gfortran )
                            cnf.gfortran = out[0]

             if ( line.split()[0] == "cla"      ): 
                  cnf.cla= line.split()[1] 
                  if ( not "/" in cnf.cla and not cnf.cla.lower() == "none"  ):
                       (ret,out) = exe ( "which " + cnf.cla )
                       cnf.cla = out[0]

             if ( line.split()[0] == "clap"      ): 
                  cnf.clap= line.split()[1] 
                  if ( not "/" in cnf.clap and not cnf.clap.lower() == "none" ):
                       (ret,out) = exe ( "which " + cnf.clap )
                       cnf.clap = out[0]

             if ( line.split()[0] == "cmake"      ): 
                  cnf.cmake= line.split()[1] 
                  if ( not "/" in cnf.cmake and not cnf.cmake.lower() == "none" ):
                       (ret,out) = exe ( "which " + cnf.cmake )
                       cnf.cmake = out[0]



        elif ( mode == "where" ):
             if ( line.split()[0] == "tarball"       ): cnf.tarball   = line.split()[1] 
             if ( line.split()[0] == "build"         ): cnf.build     = line.split()[1] 
             if ( line.split()[0] == "build_aux"     ): cnf.build_aux = line.split()[1] 
             if ( line.split()[0] == "prefix"        ): cnf.prefix    = line.split()[1] 
             if ( line.split()[0] == "sdk"           ): cnf.sdk       = line.split()[1] 

        elif ( mode == "misc" ):
             if ( line.split()[0] == "num_proc"    ): cnf.num_proc    = line.split()[1]
             if ( line.split()[0] == "center_abr"  ): cnf.center_abr  = line.split()[1]
             if ( line.split()[0] == "center_name" ): cnf.center_name = line.split(None, 1)[1].ljust(8192).strip()
             if ( line.split()[0] == "install_log"         ): 
                  cnf.install_log         = line.split()[1]
                  if ( "@DATE@" in cnf.install_log ):
                        date_string = str(datetime.datetime.now().strftime("%Y%m%d_%H%M%S"))
                        cnf.install_log = cnf.install_log.replace("@DATE@",date_string)
             if ( line.split()[0] == "build_log"         ): 
                  cnf.build_log   = line.split()[1]
                  if ( "@DATE@" in cnf.build_log ):
                        date_string = str(datetime.datetime.now().strftime("%Y%m%d_%H%M%S"))
                        cnf.build_log = cnf.build_log.replace("@DATE@",date_string)
             if ( cnf.num_proc.lower() == "all" ): 
                  if ( sys.platform == "linux" ):
                       (ret,out) = exe ( 'cat /proc/cpuinfo | grep "core id" | wc -l' )
                       try:
                            num_threads = int(out[-1])
                       except:
                            num_threads = 1
                       (ret,out) = exe ( "lscpu | grep '^Thread(s) per core:' | awk '{print $4}'" )
                       try:
                            num_thr_per_cpu = int(out[-1])
                       except:
                            num_thr_per_cpu = 1
                       num_cores = int(num_threads/num_thr_per_cpu)
                       cnf.num_proc = "%d" % num_cores 
                  if ( sys.platform == "darwin" ):
                       (ret,out) = exe ( 'sysctl -n  machdep.cpu.core_count'  )
                       try:
                            num_cores = int(out[-1])
                       except:
                            num_cores = 1
                       cnf.num_proc = "%d" % num_cores 
                       

        elif ( mode == "postinstall" ):
             pi_name = line.split()[0].replace(":","")
             if ( pi_name not in cnf.postinstall ):
                  cnf.postinstall[pi_name] = []
             cnf.postinstall[pi_name].append ( line.split(None, 1)[1].ljust(8192).strip() )

        elif ( mode == "tests" or mode == "packages" ):
             if (   line.split()[0] == "package:" ):
                    num_pack = num_pack + 1
                    cnf.package[num_pack] = {"name":          line.split()[1], \
                                             "version":       "",              \
                                             "mode":          mode,            \
                                             "build_dir":     build_dir,       \
                                             "build_aux_dir": "no",            \
                                             "pre_unpack":    [],              \
                                             "post_unpack":   [],              \
                                             "pre_config":    [],              \
                                             "with_config":   [],              \
                                             "patch":         [],              \
                                             "patch_file":    [],              \
                                             "options":       "",              \
                                             "post_config":   [],              \
                                             "build":         [],              \
                                             "tarball_file":  []               \
                                            }
                    submode = "package"

             elif ( line.split()[0] == "version:" ):
                    cnf.package[num_pack]["version"] = line.split()[1]
                    submode = "version"

             elif ( line.split()[0] == "pre_unpack:" ):
                    cnf.package[num_pack]["pre_unpack"].append ( line.split(None, 1)[1] )
                    submode = "pre_unpack"

             elif ( line.split()[0] == "build_aux_dir:" ):
                    cnf.package[num_pack]["build_aux_dir"] = line.split(None, 1)[1]
                    submode = "build_aux_dir"

             elif ( line.split()[0] == "post_unpack:" ):
                    cnf.package[num_pack]["post_unpack"].append ( line.split(None, 1)[1] )
                    submode = "post_unpack"

             elif ( line.split()[0] == "pre_config:" ):
                    cnf.package[num_pack]["pre_config"].append ( line.split(None, 1)[1].ljust(8192).strip()  )
                    submode = "pre_config"

             elif ( line.split()[0] == "with_config:" ):
                    cnf.package[num_pack]["with_config"].append ( line.split(None, 1)[1].ljust(8192).strip()  )
                    submode = "with_config"

             elif ( line.split()[0] == "options:" ):
                    if ( line[-1:] != "\\" ):
                         cnf.package[num_pack]["options"] = line.split(None, 1)[1].ljust(8192).strip() 
                    else:
                         cnf.package[num_pack]["options"] = line[:-1].split(None, 1)[1].ljust(8192).strip() 
                    submode = "options"

             elif ( line.split()[0] == "patch:" ):
                    cnf.package[num_pack]["patch"].append ( line.split(None, 1)[1].ljust(8192).strip() )
                    submode = "patch"

             elif ( line.split()[0] == "post_config:" ):
                    cnf.package[num_pack]["post_config"].append ( line.split(None, 1)[1].ljust(8192).strip()  )
                    submode = "post_config"

             elif ( line.split()[0] == "build:" ):
                    if ( line[-1:] != "\\" ):
                         cnf.package[num_pack]["build"].append ( line.split(None, 1)[1].ljust(8192).strip() )
                    else:
                         build_str = line[:-1].split(None, 1)[1].ljust(8192).strip() 
                    submode = "build"

             else:
                    if ( submode == "build" ):
                         if ( line[-1:] == "\\" ):
                              build_str = build_str + " " + line[:-1].ljust(8192).strip() 
                         else:
                              build_str = build_str + " " + line.ljust(8192).strip() 
                              cnf.package[num_pack]["build"].append ( build_str )
                    else:
                         if ( line[-1:] == "\\" ):
                              cnf.package[num_pack][submode] = cnf.package[num_pack][submode] + " " + line[:-1].ljust(8192).strip() 
                         else:
                              cnf.package[num_pack][submode] = cnf.package[num_pack][submode] + " " + line.ljust(8192).strip()

    if ( not os.path.isdir ( os.path.dirname(cnf.build_log) ) ):
         print ( "Directory name for installation log %s does not exist. Please create it." % \
                 cnf.build_log )
         exit  (  1 )

    if ( not os.path.isdir ( os.path.dirname(cnf.install_log) ) ):
         print ( "Directory name for installation log %s does not exist. Please create it." % \
                 cnf.install_log )
         exit  (  1 )

#
# --- Check whether all information was defined
#
    if ( cnf.gcc == None ):
         print ( "gcc compiler is not defined" )
         return ( 1, None )

    if ( cnf.gcx == None ):
         print ( "gcx (g++) compiler is not defined" )
         return ( 1, None )

    if ( cnf.gfortran == None ):
         print ( "gfortran compiler is not defined" )
         return ( 1, None )

    if ( sys.platform == "darwin" ):
         if ( cnf.cla == None ):
              print ( "clang C compiler is not defined" )
              return ( 1, None )

         if ( cnf.clap == None ):
              print ( "clang++ C++ compiler is not defined" )
              return ( 1, None )

         if ( cnf.sdk == None ):
              print ( "macos software development kit (sdk) is not defined in Where section" )
              return ( 1, None )

    if ( cnf.tarball == None ):
         print ( "tarball directory is not defined" )
         return ( 1, None, None )

    elif ( not os.path.isdir(cnf.tarball) ):
         print ( "tarball directory %s does not exist" % cnf.tarball )
         return ( 1, None )

    if ( cnf.build == None ):
         print ( "build directory is not defined" )
         return ( 1, None )

    if ( cnf.build_aux == None ):
         print ( "build_aux directory is not defined" )
         return ( 1, None )

    elif ( not os.path.isdir(cnf.build) ):
         print ( "build directory %s does not exist" % cnf.build )
         return ( 1, None )

    elif ( not os.path.isdir(cnf.build_aux) ):
         print ( "build_aux directory %s does not exist" % cnf.build_aux )
         return ( 1, None )

    if ( cnf.prefix == None ):
         print ( "install directory is not defined" )
         return ( 1, None )

    elif ( not os.path.isdir(cnf.prefix) ):
         print ( "install directory %s does not exist" % cnf.prefix )
         return ( 1, None )

    if ( cnf.num_proc == None ):
         print ( "Number of cores is not defined" )
         return ( 1, None )

    if ( cnf.center_abr  == None ):
         print ( "Center abbreviation is not defined" )
         return ( 1, None )

    if ( cnf.center_name == None ):
         print ( "Center name is not defined" )
         return ( 1, None )

    if ( cnf.center_name == None ):
         print ( "Log file name is not defined" )
         return ( 1, None )         

    (ret,out) = exe ( cnf.gcc + " -dumpversion" )
    if ( ret != 0 ):
         print ( "Cannot run gcc compiler. Please check you definition in [Compiler] section" )
         for line in out:
             print ( line )
         return ( 1, None )   

    if ( cnf.gcx.lower() != "none" ):
         (ret,out) = exe ( cnf.gcx + " -dumpversion" )
         if ( ret != 0 ):
              print ( "Cannot run g++ compiler. Please check you definition in [Compiler] section" )
              for line in out:
                  print ( line )
              print ( "Error in running command %s -dumpversion" % cnf.gcx )
              return ( 1, None )   

    if ( cnf.gfortran.lower() != "none" ):
         (ret,out) = exe ( cnf.gfortran + " -dumpversion" )
         if ( ret != 0 ):
              print ( "Cannot run gfortran compiler. Please check you definition in [Compiler] section" )
              for line in out:
                  print ( line )
              return ( 1, None )   
    else:
         print ( "Warning: gfortran has not been defined. Proceed with care!" )

    if ( "LD_LIBRARY_PATH" in os.environ.keys() ):
         cnf.ld_path = os.environ["LD_LIBRARY_PATH"].split(":")
    else:
         cnf.ld_path = []

#
# --- check whether cmake will be built
#
    build_cmake = 0
    for k in range(1,len(cnf.package)+1):
        if ( cnf.package[k]["name"] == "cmake" ): 
             build_cmake = 1

    if ( build_cmake == 0 ):
         if ( not os.path.isfile ( cnf.cmake ) ):
              print ( "Cannot find cmake. Tried %s. Please build cmake using sgdass" % cnf.cmake  )
              exit  ( 1 )
         cmake_com = cnf.cmake + ' --version 2>&1 | grep "cmake version" | grep -v "no version information available" | tail -1'
         (ret,out) = exe ( cmake_com )
         if ( ret != 0 ): 
              print ( "Cannot run cmake. Please build cmake using sgdass" )
              print ( "Failed test: " )
              for line in out:
                  print ( line )
              exit  ( 1 )
         if ( len(out[0].split()) >= 3 ):
              cnf.cmake_vers= out[0].split()[2]
              cmake_vers_arr = []
              try:
                  cmake_vers_arr.append ( int ( cnf.cmake_vers.split(".")[0] ) )
                  cmake_vers_arr.append ( int ( cnf.cmake_vers.split(".")[1] ) )
                  cmake_vers_arr.append ( int ( cnf.cmake_vers.split(".")[2] ) )
              except:
                  print ( "Unsupported version of cmake %s. Please build cmake using sgdass" % cnf.cmake_vers )
                  print ( "Test command: %s" % cmake_com )
                  exit  ( 1 )
              cmake_vers_num = 10000*cmake_vers_arr[0] + \
                                 100*cmake_vers_arr[1] + \
                                     cmake_vers_arr[2] 

              cmake_vers_min_arr = []
              cmake_vers_min_arr.append ( int ( cmake_vers__min.split(".")[0] ) )
              cmake_vers_min_arr.append ( int ( cmake_vers__min.split(".")[1] ) )
              cmake_vers_min_arr.append ( int ( cmake_vers__min.split(".")[2] ) )
              
              cmake_vers_min_num = 10000*cmake_vers_min_arr[0] + \
                                     100*cmake_vers_min_arr[1] + \
                                         cmake_vers_min_arr[2] 
              if ( cmake_vers_num < cmake_vers_min_num ):
                   print ( "Found cmake version %s, while at least %s is required. Please build cmake using sgdass" % \
                           ( cnf.cmake_vers, cmake_vers__min ) )
                   exit  ( 1 )
       
         else:
              print ( "Cannot determine cmake version. Please build cmake." )
              print ( "Failed test: " )
              for line in out:
                  print ( line )
              print ( "Failed command: %s" % cmake_com )
              exit  ( 1 )

    if ( len(cnf.gcc) > 9 ):
         gcc_lib   = cnf.gcc[:-8] + "/lib"
         gcc_lib64 = cnf.gcc[:-8] + "/lib64"
         gcc_lib_path   = "??"
         gcc_lib64_path = "??"
         usr_lib_path   = "??"
         usr_lib64_path = "??"
         for path_element in cnf.ld_path:
             if ( path_element == "/usr/lib"   or path_element == "/lib"   ): usr_lib_path   = "ok"
             if ( path_element == "/usr/lib64" or path_element == "/lib64" ): usr_lib64_path = "ok"
             if ( gcc_lib   == path_element ): 
                  if ( usr_lib_path != "ok" ):
                       gcc_lib_path  = "ok"
             if ( gcc_lib64 == path_element ): 
                  if ( usr_lib64_path != "ok" ):
                       gcc_lib64_path  = "ok"
         if ( sys.platform == "linux" ):
             if ( not gcc_lib_path == "ok" and gcc_lib != "/usr/lib" ): 
                  print ( "Your LD_LIBRARY_PATH should contain %s before /usr/lib" % gcc_lib ) 
                  try:
                       print ( "Your current LD_LIBRARY_PATH: %s" % os.environ["LD_LIBRARY_PATH"] )
                  except:
                       print ( "You do not have LD_LIBRARY_PATH defined" )
                       print ( os.environ.keys() )
                  return ( 1, None )   
             if ( not gcc_lib64_path == "ok" and gcc_lib64 != "/usr/lib64" ): 
                  print ( "Your LD_LIBRARY_PATH should contain %s before /usr/lib64" % gcc_lib64 ) 
                  return ( 1, None )   

    com_libz = '`ls -lrt /usr/lib/ | grep libz.so | grep -v ">" | awk ''{print $9}'' | sed "s@libz.so.@@g"`'
    (ret,out) = exe ( com_libz )
    if ( ret != 0 ):
         print ( "Error in executing command %s" % com_libz )   
         exit ( 1 )

    libz_version = out[0]

    if ( verb > 1 ):
         print ( "cnf.gcc=         ", cnf.gcc         )
         print ( "cnf.gcx=         ", cnf.gcx         )
         print ( "cnf.gfortran=    ", cnf.gfortran    )
         print ( "cnf.cla=         ", cnf.cla         )
         print ( "cnf.clap=        ", cnf.clap        )
         print ( "cnf.sdk=         ", cnf.sdk         )
         print ( "cnf.tarball=     ", cnf.tarball     )
         print ( "cnf.build=       ", cnf.build       )
         print ( "cnf.build_aux=   ", cnf.build_aux   )
         print ( "cnf.prefix=      ", cnf.prefix      )
         print ( "cnf.ld_path=     ", cnf.ld_path     )
         print ( "cnf.num_proc=    ", cnf.num_proc    )
         print ( "cnf.center_abr=  ", cnf.center_abr  )
         print ( "cnf.center_name= ", cnf.center_name )
         print ( "cnf.num_proc=    ", cnf.num_proc    )
         print ( "cnf.install_log= ", cnf.install_log )
         print ( "cnf.build_log=   ", cnf.build_log   )

    for k in range(1,len(cnf.package)+1):
        cnf.package[k]["tar_file"] = cnf.tarball + "/" + cnf.package[k]["name"] + "_" + cnf.package[k]["version"] + ".tar.gz"
        if ( not os.path.isfile( cnf.package[k]["tar_file"] ) ):
             cnf.package[k]["tar_file"] = cnf.tarball + "/" + cnf.package[k]["name"] + "-" + cnf.package[k]["version"] + ".tar.gz"
        if ( not os.path.isfile( cnf.package[k]["tar_file"] ) ):
             cnf.package[k]["tar_file"] = cnf.tarball + "/" + cnf.package[k]["name"] + "-" + cnf.package[k]["version"] + ".tgz"
        if ( not os.path.isfile( cnf.package[k]["tar_file"] ) ):
             cnf.package[k]["tar_file"] = cnf.tarball + "/" + cnf.package[k]["name"] + "_" + cnf.package[k]["version"] + ".tar.bz2"
        if ( not os.path.isfile( cnf.package[k]["tar_file"] ) ):
             cnf.package[k]["tar_file"] = cnf.tarball + "/" + cnf.package[k]["name"] + "-" + cnf.package[k]["version"] + ".tar.bz2"
        if ( not os.path.isfile( cnf.package[k]["tar_file"] ) ):
             cnf.package[k]["tar_file"] = cnf.tarball + "/" + cnf.package[k]["name"] + "_" + cnf.package[k]["version"] + ".tar.xz"
        if ( not os.path.isfile( cnf.package[k]["tar_file"] ) ):
             cnf.package[k]["tar_file"] = cnf.tarball + "/" + cnf.package[k]["name"] + "-" + cnf.package[k]["version"] + ".tar.xz"
        if ( not os.path.isfile( cnf.package[k]["tar_file"] ) ):
             if ( not inplace ):
                  print ( "Cannot find tarball file for package %s version %s in directory %s" % \
                           ( cnf.package[k]["name"], cnf.package[k]["version"], cnf.tarball ) )
                  return ( 1, None )
        if ( len(cnf.package[k]["patch"]) > 0 ):
             for patch in cnf.package[k]["patch"]:
                 patch_file = cnf.tarball + "/" + patch 
                 patch_file = patch_file.replace("${vers}",cnf.package[k]["version"])
                 if ( not os.path.isfile(patch_file) ):
                       print ( "Cannot find patch file for package %s version %s in directory %s" % \
                               ( patch_file, cnf.package[k]["version"], cnf.tarball ) )
                       return ( 1, None )
                 cnf.package[k]["patch_file"].append ( patch_file )
        if ( verb > 1 ):
             print ( " " )
             print ( "Package k= %2d %-16s %s" % (k, cnf.package[k]["name"],  cnf.package[k]["version"] ) )

             if ( len(cnf.package[k]["pre_unpack"]) > 0 ):
                  for line in cnf.package[k]["pre_unpack"]:
                      print ( "              pre_unpack:  %s" % line )

             if ( len(cnf.package[k]["post_unpack"]) > 0 ):
                  for line in cnf.package[k]["post_unpack"]:
                      print ( "              post_unpack: %s" % line )

             if ( len(cnf.package[k]["patch"]) > 0 ):
                  for line in cnf.package[k]["patch"]:
                      print ( "              patch:       %s" % line )

             if ( len(cnf.package[k]["with_config"]) > 0 ):
                  for line in cnf.package[k]["with_config"]:
                      print ( "              with_config: %s" % line )

             if ( len(cnf.package[k]["pre_config"]) > 0 ):
                  for line in cnf.package[k]["pre_config"]:
                      print ( "              pre_config:  %s" % line )

             print ( "              options:     %s" % cnf.package[k]["options"]     )

             if ( len(cnf.package[k]["post_config"]) > 0 ):
                  for line in cnf.package[k]["post_config"]:
                      print ( "              post_config: %s" % line )

             for line in cnf.package[k]["build"]:
                 print ( "              build:       %s" % line )

    if ( verb > 1 ):
         if ( len(cnf.postinstall) > 0 ):
              for key in cnf.postinstall.keys():
                  print ( " " )
                  for line in cnf.postinstall[key]:
                      print ( "Postinstall: %-12s %s" % (key, line) )
         print ( " " )

    date_string = str(datetime.datetime.now().strftime("%Y%m%d_%H%M%S"))
    cnf.control_file = "/tmp/sgdass_install_" + date_string + ".csh"

    return ( 0, cnf )
#
# ------------------------------------------------------------------------
#
def check_dirs ( cnf, verb ):

#
# --- Check directories, their existance and their permissions
#
    for dir in cnf.dirs:
        if ( not os.path.isdir(dir) ):
             print ( "Directory %s specified in the control file %s does not exist" % \
                     (dir, cnf.filename ) )
             return  ( 1 )

        mode = os.stat(dir).st_mode
        if ( not bool(mode & stat.S_IRUSR) ):
             print ( "Directory %s does not provide a user read permission" % dir )
             exit ( 1 )
        if ( not bool(mode & stat.S_IWUSR) ):
             print ( "Directory %s does not provide a user write permission" % dir )
             exit ( 1 )
        if ( not bool(mode & stat.S_IXUSR) ):
             print ( "Directory %s does not provide a user execute permission" % dir )
             exit ( 1 )
        if ( not bool(mode & stat.S_IRGRP) ):
             print ( "Directory %s does not provide a group read permission" % dir )
             exit ( 1 )
        if ( not bool(mode & stat.S_IWGRP) ):
             print ( "Directory %s does not provide a group write permission" % dir )
             exit ( 1 )
        if ( not bool(mode & stat.S_IXGRP) ):
             print ( "Directory %s does not provide a group execute permission" % dir )
             exit ( 1 )
        if ( not bool(mode & stat.S_IROTH) ):
             print ( "Directory %s does not provide other read permission" % dir )
             exit ( 1 )
        if ( not bool(mode & stat.S_IXOTH) ):
             print ( "Directory %s does not provide other execute permission" % dir )
             exit ( 1 )

#
# --- Check subdirectories, their existence and their permissions
#
    for subdir in cnf.subdirs:
        if ( not os.path.isdir(subdir) ):
             os.mkdir ( subdir, 0o775 )
        mode = os.stat(subdir).st_mode

        if ( not bool(mode & stat.S_IRUSR) ):
             print ( "Subdirectory %s does not provide a user read permission" % subdir )
             exit ( 1 )
        if ( not bool(mode & stat.S_IWUSR) ):
             print ( "Subdirectory %s does not provide a user write permission" % subdir )
             exit ( 1 )
        if ( not bool(mode & stat.S_IXUSR) ):
             print ( "Subdirectory %s does not provide a user execute permission" % subdir )
             exit ( 1 )
        if ( not bool(mode & stat.S_IRGRP) ):
             print ( "Subdirectory %s does not provide a group read permission" % subdir )
             exit ( 1 )
        if ( not bool(mode & stat.S_IWGRP) ):
             print ( "Subdirectory %s does not provide a group write permission" % subdir )
             exit ( 1 )
        if ( not bool(mode & stat.S_IXGRP) ):
             print ( "Subdirectory %s does not provide a group execute permission" % subdir )
             exit ( 1 )
        if ( not bool(mode & stat.S_IROTH) ):
             print ( "Subdirectory %s does not provide other read permission" % subdir )
             exit ( 1 )
        if ( not bool(mode & stat.S_IXOTH) ):
             print ( "Subdirectory %s does not provide other execute permission" % subdir )
             exit ( 1 )


    return ( 0 )
#
# ------------------------------------------------------------------------
#

def gen_control_file ( cnf, operation, package, inplace, verb ):
    
    cnt = []
    cnt.append ( "#!/bin/csh -f" )
    cnt.append ( "# " ) 
    cnt.append ( "# Control file for compling Space Geodesy Data Analysis Software Suite" )
    cnt.append ( "# " ) 
    cnt.append ( "# Generated by " + sgdass_install__label )
    cnt.append ( "# with the use of control file " + cnf.filename  )
    cnt.append ( "# on " + str(datetime.datetime.now().strftime("%Y%m%d_%H%M%S")) )
    cnt.append ( "# " ) 
    cnt.append ( "umask 0002" )
    cnt.append ( "setenv CC  " + cnf.gcc )
    cnt.append ( "setenv CXX " + cnf.gcx )
    cnt.append ( "setenv FC  " + cnf.gfortran )
    cnt.append ( "setenv F77 " + cnf.gfortran )
    cnt.append ( "setenv PKG_CONFIG_PATH " + cnf.prefix + "/lib/pkgconfig:/usr/lib/pkgconfig:/usr/local/lib/pkgconfig:/usr/lib64/pkgconfig" )
    cnt.append ( "setenv build_log " + cnf.build_log )
    cnt.append ( 'echo " " > '       + cnf.build_log )
    cnt.append ( "echo SGDASS version: %s >> %s" % ( sgdass_install__label, cnf.build_log ) )
    cnt.append ( "echo sgdass_install.py was invoked with command %s >> %s " % \
                 ( " ".join(sys.argv), cnf.build_log ) ) 
    sgdass_root = os.path.dirname(sys.argv[0])
    if ( sgdass_root == "./" ):
         sgdass_root = "`pwd`"
    cnt.append ( "setenv sgdass_root " + sgdass_root )
    cnt.append ( "# " ) 
    cnt.append ( 'echo "================ BEGINNING OF CONTROL FILE: ========" >> ' + cnf.build_log )
    cnt.append ( "cat " + cnf.filename + " >> " + cnf.build_log )
    cnt.append ( 'echo "================ END OF CONTROL FILE: ========" >> ' + cnf.build_log )
    cnt.append ( "# " ) 
    
    if ( verb == 1 ):
         echo_suf = ' >>& $build_log'
         if ( verb > 1 ):
              if ( cnf.cmake_vers ):
                   cnt.append ( 'echo "Found cmake version %s" >> %s' % ( cnf.cmake_vers, cnf.build_log ) )
              else:
                   cnt.append ( 'echo "cmake was not found" > %s' % cnf.build_log )
    else:
         echo_suf = ''

    num_used_packages = 0
    if ( operation == "configure"         or \
         operation == "build"             or \
         operation == "build_after"       or \
         operation == "build_and_after"   or \
         operation == "rebuild"           or \
         operation == "rebuild_and_after"    ):
         flag_contunue = 0
         for k in range(1,len(cnf.package)+1):
                  
             if ( cnf.package[k]["mode"] != "tests" ):
                  if ( operation == "build_and_after" and package == cnf.package[k]["name"] ):
                       flag_contunue = 1
     
                  if ( operation == "build_after" and package == cnf.package[k]["name"] ):
                       flag_contunue = 1
                       continue
                  
                  if ( operation == "rebuild_and_after" and package == cnf.package[k]["name"] ):
                       flag_contunue = 1
                       continue

             if ( package == cnf.package[k]["name"] or  \
                  package == "use_vtd"    or            \
                  package == "use_psolve" or            \
                  package == "use_pima"   or            \
                  package == "all"        or            \
                  flag_contunue == 1                    ):

                  num_used_packages = num_used_packages + 1
                  cnt.append ( 'echo " " ' + echo_suf )
                  if ( k == 1 and cnf.package[k]["name"] == "petools" ):
                       cnt.append ( 'echo "sgdass:  Running a general test with %-16s %2d(%2d)"' % \
                                    ( cnf.package[k]["name"] + " " + cnf.package[k]["version"], k, len(cnf.package) ) )
                  else:
                       cnt.append ( 'echo "sgdass:  Installing package %-24s  %2d(%2d)"' % \
                                  ( cnf.package[k]["name"] + " " + cnf.package[k]["version"], k, len(cnf.package) ) )
                  cnt.append ( 'echo "Started  installation of ' + cnf.package[k]["name"] + ' at "' + '`date "+%Y%m%d_%H%M%S"`' + echo_suf )
                  cnt.append ( 'echo "====================="'  + echo_suf  )
                  cnt.append ( 'setenv tarball ' + cnf.tarball               )
                  cnt.append ( 'setenv package ' + cnf.package[k]["name"]    )
                  cnt.append ( 'setenv vers    ' + cnf.package[k]["version"] )

                  if ( cnf.package[k]["build_aux_dir"] == "yes" ):
                       cnf.package[k]["build_dir"] = cnf.build_aux

                  cnt.append ( "cd " + cnf.package[k]["build_dir"] )

                  if ( operation == "configure" or       \
                       operation == "build"     or       \
                       operation == "build_and_after" or \
                       operation == "build_after"        ):
                       if ( len (cnf.package[k]["pre_unpack"]) > 0  ):
                            for line in cnf.package[k]["pre_unpack"]:
                                cnt.append ( line )

                       if ( not inplace ):
                            if ( ".tgz" in cnf.package[k]["tar_file"] ):
                                 cnt.append ( "tar -zxf " + cnf.package[k]["tar_file"] )
                            if ( ".gz" in cnf.package[k]["tar_file"] ):
                                 cnt.append ( "tar -zxf " + cnf.package[k]["tar_file"] )
                            elif ( ".bz2" in cnf.package[k]["tar_file"] ):
                                 cnt.append ( "tar -jxf " + cnf.package[k]["tar_file"] )
                            elif ( ".xz" in cnf.package[k]["tar_file"] ):
                                 cnt.append ( "tar -I " + cnf.prefix + "/bin/xz -xf " + cnf.package[k]["tar_file"] )
                            else:
                                 cnt.append ( "tar -xf " + cnf.package[k]["tar_file"] )

                  root1 = cnf.package[k]["build_dir"] + "/" + cnf.package[k]["name"] + "-" + cnf.package[k]["version"]
                  root2 = cnf.package[k]["build_dir"] + "/" + cnf.package[k]["name"] + "_" + cnf.package[k]["version"]
                  cnt.append ( "if ( -d " + root1 + " ) setenv ROOT_" + cnf.package[k]["name"].replace("-","_") + " " + root1 )
                  cnt.append ( "if ( -d " + root2 + " ) setenv ROOT_" + cnf.package[k]["name"].replace("-","_") + " " + root2 )

                  if ( operation == "configure"       or \
                       operation == "build"           or \
                       operation == "build_and_after" or \
                       operation == "build_after"        ):
                       if ( len (cnf.package[k]["post_unpack"]) > 0  ):
                            for line in cnf.package[k]["post_unpack"]:
                                cnt.append ( line )

                  cnt.append ( "cd $ROOT_" + cnf.package[k]["name"].replace("-","_") )

                  if ( operation == "configure" or       \
                       operation == "build"     or       \
                       operation == "build_and_after" or \
                       operation == "build_after"        ):
                       if ( len (cnf.package[k]["patch"]) > 0  ):
                            for patch_file in cnf.package[k]["patch_file"]:
                                cnt.append ( "patch -Np0 -i " + patch_file + echo_suf )

                  if ( len(cnf.package[k]["pre_config"]) > 0 ):
                       for line in cnf.package[k]["pre_config"]:
                           cnt.append ( line )

                  if ( operation == "rebuild" or        \
                       operation == "rebuild_and_after" ):
                       if ( len(cnf.package[k]["options"]) > 0 ):
                            cnt.append ( "if ( -f Makefile ) rm Makefile" )

                  if ( cnf.package[k]["options"] != "" ):
                       if ( len(cnf.package[k]["with_config"]) > 0 ):
                            for line in cnf.package[k]["with_config"]:
                                cnt.append ( line + echo_suf )
                            
                       if ( not cnf.package[k]["options"] == "noconfigure" ): 
                            if ( sys.platform == "linux" ):
                                 cnt.append ( 'set conf_command = ./configure'     )
                                 cnt.append ( 'if ( -f ./Configure     ) set conf_command = ./Configure'     )
                                 cnt.append ( 'if ( -f ./configure.sh  ) set conf_command = ./configure.sh'  )
                                 cnt.append ( 'if ( -f ./configure.csh ) set conf_command = ./configure.csh' )
                                 cnt.append ( 'if ( -f ./config        ) set conf_command = ./config'        )
                                 cnt.append ( "$conf_command " + cnf.package[k]["options"]  + echo_suf )
                                 cnt.append ( "echo $conf_command " + cnf.package[k]["options"]  + echo_suf )
                            else:
                                 cnt.append ( 'set conf_command = ./configure'     )
                                 cnt.append ( 'if ( -f ./Configure     ) set conf_command = ./Configure'     )
                                 cnt.append ( 'if ( -f ./configure.sh  ) set conf_command = ./configure.sh'  )
                                 cnt.append ( 'if ( -f ./configure.csh ) set conf_command = ./configure.csh' )
                                 cnt.append ( 'if ( -f ./config        ) set conf_command = ./config'        )
                                 cnt.append ( 'file $conf_command | grep -q "C shell"' )
                                 cnt.append ( 'if ( $status == 0 ) then' )
                                 cnt.append ( '          $conf_command ' + cnf.package[k]["options"]  + echo_suf )
                                 cnt.append ( '     echo $conf_command ' + cnf.package[k]["options"]  + echo_suf )
                                 cnt.append ( '  else' )
                                 cnt.append ( '     file $conf_command | grep -qi "perl"' )
                                 cnt.append ( '     if ( $status == 0 ) then' )
                                 cnt.append ( '          $conf_command ' + cnf.package[k]["options"]  + echo_suf )
                                 cnt.append ( '          $conf_command ' + cnf.package[k]["options"]  + echo_suf )
                                 cnt.append ( '        else ' )
                                 cnt.append ( '          file $conf_command | grep -qi "python"' )
                                 cnt.append ( '          if ( $status == 0 ) then' )
                                 cnt.append ( '               $conf_command ' + cnf.package[k]["options"]  + echo_suf )
                                 cnt.append ( '               $conf_command ' + cnf.package[k]["options"]  + echo_suf )
                                 cnt.append ( '            else' )
                                 cnt.append ( '                    sh --login $conf_command ' + cnf.package[k]["options"]  + echo_suf )
                                 cnt.append ( '               echo sh --login $conf_command ' + cnf.package[k]["options"]  + echo_suf )
                                 cnt.append ( '          endif' )
                                 cnt.append ( '     endif' )
                                 cnt.append ( 'endif' )
                       else:
                            cnt.append ( "echo no configure for this package >>& $build_log" )

                  if ( len(cnf.package[k]["post_config"]) > 0 ):
                       for line in cnf.package[k]["post_config"]:
                           cnt.append ( line  + echo_suf )

                  if ( operation == "rebuild"           or \
                       operation == "rebuild_and_after"    ):
                       cnt.append ( "make clean" + echo_suf )

                  if ( len (cnf.package[k]["build"]) > 0  ):
                       for build_line in cnf.package[k]["build"]:
                           if ( "make" in build_line and not ">" in build_line ):
                                cnt.append ( build_line + echo_suf )
                           else:
                                if ( not ">" in build_line ):
                                     cnt.append ( build_line + echo_suf )
                                else:
                                     cnt.append ( build_line )
                           if ( "make" in build_line and not "clean" in build_line ):
                                cnt.append ( 'if ( $status != 0 ) then' )
                                cnt.append ( '     echo "Failed building of ' + cnf.package[k]["name"] + ' at "' + '`date "+%Y%m%d_%H%M%S"`' )
                                cnt.append ( '     exit 1' )
                                cnt.append ( 'endif' )

                  cnt.append ( 'if ( $status == 0 ) then' )
                  cnt.append ( 'rehash' )
                  if ( k == 1 and cnf.package[k]["name"] == "petools" ):
                       cnt.append ( '     echo "Finished test with          %-24s  at %s"' % \
                                        ( cnf.package[k]["name"], '`date "+%Y%m%d_%H%M%S"`' )  )
                  else:
                       cnt.append ( '     echo "Finished installation of    %-24s  at %s"' % \
                                        ( cnf.package[k]["name"], '`date "+%Y%m%d_%H%M%S"`' )  )
                  cnt.append ( '     echo "====================="' +  echo_suf )
                  cnt.append ( '     echo " "' + echo_suf  )
                  cnt.append ( 'else' )
                  cnt.append ( '     echo "Failed installation of ' + cnf.package[k]["name"] + ' at "' + '`date "+%Y%m%d_%H%M%S"`' )
                  cnt.append ( '     exit 1' )
                  cnt.append ( 'endif' )
                  cnt.append ( '#' )
                  if ( cnf.package[k]["name"] == "vtd" and package == "use_vtd" ):
                       break
                  if ( cnf.package[k]["name"] == "psolve" and package == "use_psolve" ):
                       break
                  if ( cnf.package[k]["name"] == "pima" and package == "use_pima" ):
                       break

    if ( operation == "postinstall" ):
         if ( len(cnf.postinstall) > 0 ):
              for key in cnf.postinstall.keys():
                  if ( package == "all" or package == key ):
                       cnt.append ( '#' )
                       cnt.append ( 'echo "postinstallation for package ' + key + '"' )
                       cnt.append ( '#' )
                       for line in cnf.postinstall[key]:
                           cnt.append ( line )
                       cnt.append ( 'echo + "Finished installation of ' + key  + '"' )

    if ( operation == "configure"         or \
         operation == "build"             or \
         operation == "build_after"       or \
         operation == "build_and_after"   or \
         operation == "rebuild"           or \
         operation == "rebuild_and_after"    ):
         if ( num_used_packages == 0 ):
              if ( package != "all" and        \
                   package != "use_vtd" and    \
                   package != "use_psolve" and \
                   package != "use_pima"       ):
                   print ( "Pakage %s was not found" % package )
              else:
                   print ( "No packages were found" )
              exit  (  1 )
         
                      
    print ( " " )

    return ( 0, cnt )
#
# ------------------------------------------------------------------------
#
def check_proc_env ( cnf, verb ):
    """
    Run a test to check whether gcc compiler is functional and whether shared
    libraries are loaded.
    """
    date_string = str(datetime.datetime.now().strftime("%Y%m%d_%H%M%S"))

    test_dir = cnf.build + "/test"
    lib_dir  = cnf.prefix + "/lib"
    if ( not os.path.isdir(test_dir) ):
         os.mkdir ( test_dir, 0o775 )
    if ( not os.path.isdir(lib_dir) ):
         os.mkdir ( lib_dir, 0o775 )

    fil_code_lib = test_dir + "/test_shared_lib_" + date_string + ".c"
    fil_code_main = test_dir + "/test_shared_main_" + date_string + ".c"
    fil_cmpl = test_dir + "/test_shared_main_" + date_string + ".csh"
    fil_exe  = fil_code_main.replace(".c",".e") 

#
# -- Clean leftovers from prior unsucessful test if present
#
    com_clean = "rm -f " + lib_dir + "/libtest_shared_lib_* >& /dev/null"
    (ret,out) = exe ( com_clean )

    code_lib = []
    code_lib.append ( '#include <stdio.h>' )
    code_lib.append ( '#include <stdlib.h>' )
    code_lib.append ( 'int shared_library_test_' + date_string + '() {' )
    code_lib.append ( '    printf ( "Hello\\n");' )
    code_lib.append ( '    return ( 0 );' )
    code_lib.append ( '}' )

    code_main = []
    code_main.append ( '#include <stdio.h>' )
    code_main.append ( '#include <stdlib.h>' )
    code_main.append ( 'int shared_library_test_' + date_string + '();' )
    code_main.append ( 'int main(int argc, char *argv[]) {' )
    code_main.append ( '    int ret;' )
    code_main.append ( '    ret = shared_library_test_' + date_string + '();' )
    code_main.append ( '}' )
    
    code_cmpl = []
    code_cmpl.append ( "#!/bin/csh -f" )
    code_cmpl.append ( "cd " + test_dir )
    code_cmpl.append ( cnf.gcc + " -fPIC -c -o " + fil_code_main.replace(".c",".o") + " " + fil_code_main )
    code_cmpl.append ( cnf.gcc + " -fPIC -c -o " + fil_code_lib.replace(".c",".o")  + " " + fil_code_lib )
    code_cmpl.append ( "ar cr " + fil_code_lib.replace(".c",".a") + " " + fil_code_lib.replace(".c",".o") + " >/dev/null" )
    if ( sys.platform == "linux" ):
         code_cmpl.append ( cnf.gcc + " -shared -shared -Wl,--whole-archive -o " + lib_dir + "/lib" + \
                            fil_code_lib.replace(test_dir+"/","").replace(".c",".so") + " " + \
                            fil_code_lib.replace(".c",".a") + " -Wl,-no-whole-archive" )
    else:
         code_cmpl.append ( cnf.gcc + " -dynamiclib " + " -install_name " + \
                            lib_dir + "/lib" + fil_code_lib.replace(test_dir+"/","").replace(".c",".dylib") + " " + \
                            " -o " + \
                            lib_dir + "/lib" + fil_code_lib.replace(test_dir+"/","").replace(".c",".dylib") + " " + \
                            fil_code_lib.replace(".c",".o") )
    code_cmpl.append ( cnf.gcc + " -o " + fil_exe + " " + \
                       fil_code_main.replace(".c",".o") + " " + \
                       "-L" + cnf.prefix + "/lib" + " -l" + \
                       fil_code_lib.replace(test_dir+"/","").replace(".c","") )

    f=open(fil_code_lib,"w")
    for line in code_lib:
        print ( line, file=f )
    f.close()

    f=open(fil_code_main,"w")
    for line in code_main:
        print ( line, file=f )
    f.close()

    f=open(fil_cmpl,"w")
    for line in code_cmpl:
        print ( line, file=f )
    f.close()

    (ret,out) = exe ( "csh " + fil_cmpl )
    if ( ret != 0 ):
         print ( "Error in an attempt to compile a test" )
         for line in out:
             print ( line )
         return ( 1 )

    (ret,out) = exe ( fil_exe )
    if ( ret != 0 ):
         print ( "Error in running a test of shared libraries ret= ", ret )
         for line in out:
             print ( line )
         print ( " " )
         print ( "The most common reason is that directory %s/lib is not included in LD_LIBRARY_PATH" % cnf.prefix )
         print ( "You need define environment variable LD_LIBRARY_PATH that includes %s/lib and %s/lib64. If LD_LIBRARY_PATH has more than one column-separated directories, %s/lib should be the first" % \
                 ( cnf.prefix, cnf.prefix, cnf.prefix ) )
         return ( 1 )
    
#
# --- Clean garbage
#
    os.unlink ( fil_cmpl      )
    os.unlink ( fil_exe       )
    os.unlink ( fil_code_lib  )
    os.unlink ( fil_code_lib.replace(".c",".o")   )
    os.unlink ( fil_code_main.replace(".c",".o")  )
    os.unlink ( fil_code_lib.replace(".c",".a")  )
    os.unlink ( fil_code_main )
    if ( sys.platform == "linux" ):
         os.unlink ( cnf.prefix + "/lib/lib" + fil_code_lib.replace(test_dir+"/","").replace(".c",".so") )
    else:
         os.unlink ( cnf.prefix + "/lib/lib" + fil_code_lib.replace(test_dir+"/","").replace(".c",".dylib") )

    return ( 0 )

#
# ------------------------------------------------------------------------
#
def main():

    parser = argparse.ArgumentParser( description=sgdass_install__label )
    parser.add_argument('--version', action='version', version=sgdass_install__label )

    parser.add_argument ( "-v", "--verbosity", \
                          action="store", \
                          dest="verb", \
                          default=1, \
                          metavar="verbosity", \
                          type=int, \
                          help="Verbosity level" )

    parser.add_argument ( "-i", "--inplace", \
                          action="store_true", \
                          dest="inplace", \
                          default=False, \
                          help="If set, compilation will be done in place without the use of tar files" )

    parser.add_argument ( "-c", "--config", \
                          action="store", \
                          dest="config_file", \
                          metavar="config_file", \
                          help="SGDASS configuration file" )

    parser.add_argument ( dest="task", \
                          action="store", \
                          metavar="task", \
                          help="SGDASS task: configure|build|rebuild|postinstall" )

    parser.add_argument ( dest="package", \
                          action="store", \
                          metavar="package", \
                          help="SGDASS package: package_name|all|use_vtd|use_pima|use_psolve" )

#
# --- Get and parse options
#
    args = parser.parse_args()

    if ( not args.config_file ):
         print ( "configuration file is not defined" ) 
         exit  ( 1 )

    if ( not args.task ):
         print ( "task is not defined" ) 
         exit  ( 1 )

    if ( not args.package ):
         print ( "package is not defined" ) 
         exit  ( 1 )

    if ( not os.path.isfile ( args.config_file ) ):
         print ( "configuration file %s is not found" % args.config_file ) 
         exit  ( 1 )

    if ( not ( args.task == "configure"         or \
               args.task == "build"             or \
               args.task == "build_after"       or \
               args.task == "build_and_after"   or \
               args.task == "rebuild"           or \
               args.task == "rebuild_and_after" or \
               args.task == "postinstall"       or \
               args.task == "uninstall"            ) ):
         print ( "Wrong task %s: configure or build or build_and_after or build_after or rebuild or uninstall or postinstall were expected" % args.task )
         exit  ( 1 )

    if ( getpass.getuser() == "root" ):
         print ( "sgdass should not be installed by root user. " + \
                 "Please login as a normal user and try again" )
         exit  ( 1 )
    umask = os.umask(2)
    if ( umask != 2 ):
         print ( "Wrong umask o'%04o. All users must use umask 2 while installing and using sgdass" % umask )
         exit  ( 1 )


    cnf = cnf_class ( args.config_file )
    (ret,cnf) = parse_control_file ( cnf, args.inplace, args.verb )
    if ( ret != 0 ):
         print ( "Error in parsing control file ", args.config_file )
         exit  ( 1 )

    (ret) = check_dirs ( cnf, args.verb )
    if ( ret != 0 ):
         print ( "Failure in checking directory structure" )
         exit  ( 1 )

    (ret) = check_proc_env ( cnf, args.verb )
    if ( ret != 0 ):
         print ( "Failure in checking the processing environment" )
         print ( "Please check whether LD_LIBRARY_PATH is correctly set" )
         exit  ( 1 )
         
    cnt = []
    (ret,cnt) = gen_control_file ( cnf, args.task, args.package, \
                                   args.inplace, args.verb )
    if ( ret != 0 ):
         print ( "Error in attempt to generate control file for building SGDASS" )
         exit  ( 1 )

    if ( not args.task == "uninstall" ):
         f=open(cnf.control_file,"w")
         for line in cnt:
             print ( line, file=f )
         f.close()
         if ( args.verb > 0 ):
              print ( "Generated control file %s" % cnf.control_file )

#   print ( "Interrupt: ", cnf.control_file ); exit ( 22 ) # For tests

    try:
         f = open(cnf.install_log,"w")
    except BaseException as e:
         print ( "Cannot create installation log %s because of %s" % \
                 ( cnf.install_log, str(e) ) )
         exit  ( 1 )

    if ( args.task == "build" or             \
         args.task == "build_after"       or \
         args.task == "build_and_after"   or \
         args.task == "rebuild_and_after" or \
         args.task == "postinstall"          ):

         com = "csh " + cnf.control_file
         date_string = str(datetime.datetime.now().strftime("%Y%m%d_%H%M%S"))
         print ( "Started  SGDASS " + args.task + " " + args.package + " " + date_string         )
         print ( "Started  SGDASS " + args.task + " " + args.package + " " + date_string, file=f )

         if ( args.verb > 0 ):
              if ( args.verb > 1 ):
                   print ( "com= ", com )
              (ret,out) = exe_pipe ( com )
         else:
              (ret,out) = exe ( com )

         for line in out:
             print ( line, file=f )

         fl_success = 0
         for line in out:
             if ( "Finished installation of" in line ): fl_success = 1
             print ( line, file=f )

         date_string = str(datetime.datetime.now().strftime("%Y%m%d_%H%M%S"))
         if ( ret != 0 or fl_success == 0 ):
              print ( args.task + " of SGDASS HAS FAILED. See install log file " + cnf.install_log )
              print ( args.task + " and a detailed build log file " + cnf.build_log )
              print ( "Failed command: " + com )
              f.close()
              exit  ( 0 )
         else:
              print ( args.task + " of SGDASS sucessfully finished at " + date_string )
              print ( "See install log file " + cnf.install_log )
              print ( "and build   log file " + cnf.build_log )
              print ( args.task + " of SGDASS sucessfully finished at " + date_string, file=f )
              os.remove ( cnf.control_file )
              if ( args.verb < 2 ):
                   exit  ( 0 )
              f.close()

    if ( args.task == "rebuild" ):
         com = "csh " + cnf.control_file
         date_string = str(datetime.datetime.now().strftime("%Y%m%d_%H%M%S"))
         print ( "Started SGDASS " + args.task + " " + args.package + " " + date_string )
         print ( "Started SGDASS " + args.task + " " + args.package + " " + date_string, file=f )

         if ( args.verb > 0 ):
              if ( args.verb > 1 ):
                   print ( "com= ", com )
              (ret,out) = exe_pipe ( com )
         else:
              (ret,out) = exe ( com )

         for line in out:
             print ( line, file=f )
#
# ------ Print error message
#
         if ( ret != 0 ):
              for line in out:
                  print ( line )
              print ( "Failed command ", com )
         else:
              if ( args.verb < 2 ): 
                   os.remove ( cnf.control_file )

         date_string = str(datetime.datetime.now().strftime("%Y%m%d_%H%M%S"))
         print ( "Finshed SGDASS " + args.task + " " + args.package + " " + date_string )
         print ( "Finshed SGDASS " + args.task + " " + args.package + " " + date_string, file=f )
         exit ( 0 )
         f.close()

    if ( args.task == "uninstall" ):
         for k in range(1,len(cnf.package)+1):
             if ( args.package == "all"                  or \
                  args.package == cnf.package[k]["name"]    ):
                  pack_dir = cnf.package[k]["build_dir"] + "/" + cnf.package[k]["name"] + "-" + cnf.package[k]["version"]
                  if ( not os.path.isdir ( pack_dir ) ):
                       pack_dir = cnf.package[k]["build_dir"] + "/" + cnf.package[k]["name"] + "_" + cnf.package[k]["version"]
                  if ( os.path.isdir ( pack_dir ) ):
                       com = "cd " + pack_dir + "; make uninstall"
                       (ret,out) = exe ( com )
                       com = "rm -fR " + pack_dir 
                       (ret,out) = exe ( com )
                       print ( "sgdass_install.py: Uninstalled package " + cnf.package[k]["name"] + "-" + cnf.package[k]["version"] )


if __name__ == "__main__":
    try:
        vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
        if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
        signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
        main()
    except KeyboardInterrupt:
        print ( "\nsgdass_install.py: Interrupted" )
        exit ( 1 )
