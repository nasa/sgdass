#!/usr/bin/python3
import sys, os, shutil, subprocess
sys.path.append('/auto')
from   pet_misc  import *

last_date = "2099.12.31"

if ( len(sys.argv) < 2 ):
     print ( "Usage: eval_gain_result stp" )
     exit  ( 1 )
else:
     fil_gain     = sys.argv[1]
     fil_stp_out  = sys.argv[2]

buf = read_file ( fil_gain )
out = []
fl_tsys = False
fl_gain = False

for i in range(0,len(buf)):
    if ( "Gain computed with" in buf[i]):
          gain_prog = buf[i].split()[3] + " " + buf[i].split()[4] 
    if ( "Experiment date:" in buf[i]):
          exp_date= buf[i].split()[2][0:10]
    if ( "TSYS_FREQS:" in buf[i] ):
         out.append( "#" )
         if ( not fl_tsys ):
              line = buf[i] 
              line = line.replace("GHz ", "char")[0:36] + exp_date + " " + last_date
              out.append( '# Gain computed with ' + gain_prog )
              out.append( '#' )
              out.append( line )
              fl_tsys = True
         out.append( buf[i] )
    if ( "TSYS_ELEVS:" in buf[i] ):
         out.append( buf[i] )
    if ( "TSYS_POLVALS:" in buf[i] ):
         out.append( buf[i] )

for i in range(0,len(buf)):
    if ( "GAIN_FREQS:" in buf[i] ):
         out.append( "#" )
         if ( not fl_gain ):
              line = buf[i] 
              line = line.replace("GHz ", "char")[0:36] + exp_date + " " + last_date
              out.append( line )
              fl_gain = True
         out.append( buf[i] )
    if ( "GAIN_ELEVS:" in buf[i] ):
         out.append( buf[i] )
    if ( "GAIN_POLVALS:" in buf[i] ):
         out.append( buf[i] )


(ret,err) = write_file ( out, fil_stp_out )     
check_err_exe ( ret, out, "write_file" )
