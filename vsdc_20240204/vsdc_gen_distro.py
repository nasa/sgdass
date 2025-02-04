#!/usr/bin/env python3
import  sys, os, subprocess, signal, time, datetime, gzip, bz2, lzma, argparse
from    vsdc_misc   import *
from    vsdc_parse  import *
from    vsdc_config import *

files_to_update = [ "vget", "swin_up.csh", "swin_up.bash" ]

dir_pat = "asljzgroakfg"

if ( len(sys.argv)-1 < 1 ):
     print ( "usage: vsdc_gen_distro.py vsdc_contr_file" )
     exit  ( 1 )
else:
     cnt_file = sys.argv[1]

#
# --- Initialize the class with vsdc variables
#
vsdc = vsdc_config_class ( cnt_file ) 
ret = vsdc_parse_config ( vsdc )
if ( ret != 0 ): 
     print ( "Failure in parsing control file ", cnt_file  )
     exit  ( 1 )

vsdc_vers = "/progs/vsdc_" + vsdc__version.split()[2].replace(".","")
#
for fil in files_to_update:
    inp_buf = read_file ( fil )
    if ( not inp_buf ):
         print ( "Cannot find file ", fil )
         exit  ( 1 )

    out_buf = []
    fl_updated = 0
    for line in inp_buf:
        if ( "/progs/vsdc_" in line ):
             ip = line.find("/progs/vsdc_")
             if ( ip > 0 ):
                  line = line[0:ip] + vsdc_vers + line[ip+len(vsdc_vers):]
                  fl_updated = 1
        out_buf.append ( line )
    
    if ( fl_updated == 1 ):
         f=open(fil,"w")
         for line in out_buf:
             print ( line, file=f )
         f.close()
         print ( "Updated file ", fil )

filout = "/tmp/vsdc-" + vsdc__vers + ".tar"

com = "tar -C " + vsdc__master_dir + " --exclude=*__pycache__*" + \
              " --exclude=*~" + ' --exclude=*#*' + " --exclude=*old*" + \
              " -cvf " + filout + " " + "vsdc_" + vsdc__vers
print ( com )

(ret,out) = vsdc_exe ( com )
if ( ret != 0 ):
     for line in out:
         print ( line )
     print ( "Failed command: ", com )

root_cnf_dir  = vsdc.ddf_dir[0:vsdc.ddf_dir[1:].find("/")+1]
cnf_dir  = vsdc.ddf_dir.replace("/ddf","").replace(root_cnf_dir+"/","")

com = "tar -C " + root_cnf_dir + " --exclude=*.netrc*" + " --exclude=*.cookies" + \
      " --exclude=*~" + " --exclude=*#*" + " --exclude=*old*" + " --exclude=vsdc/" \
      " --transform='s@vsdc/@vsdc_" + vsdc__vers + "/vsdc" + "_" + \
      dir_pat + "/@'" + " -rvf " + filout + " " + cnf_dir
print ( com )
(ret,out) = vsdc_exe ( com )
if ( ret != 0 ):
     for line in out:
         print ( line )
     print ( "Failed command: ", com )

com = 'tar --delete vsdc -f ' +  filout
print ( com )
(ret,out) = vsdc_exe ( com )
if ( ret != 0 ):
     for line in out:
         print ( line )
     print ( "Failed command: ", com )

com = "lbzip2 -f " + filout
(ret,out) = vsdc_exe ( com )
if ( ret != 0 ):
     for line in out:
         print ( line )
     print ( "Failed command: ", com )

print ( "Distribution file created: ", filout + ".bz2" )
