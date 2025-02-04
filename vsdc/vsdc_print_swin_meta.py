#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Program vsdc_print_swin_meta prints meta-file extracted from       *
# *   the archived VLBA Level 1A data in swin format from the DiFX       *
# *   correlator.                                                        *
# *                                                                      *
# * ## 25-FEB-2021 vsdc_print_swin_meta v1.1 (c) L. Petrov 30-MAY-2023 # *
# *                                                                      *
# ************************************************************************
import  sys, os, subprocess, signal, time, datetime, gzip, bz2, lzma, argparse
vsdc__root_dir = "@@"
sys.path.append(vsdc__root_dir)
from    vsdc_misc import *
from    vsdc_swin import *

if ( vsdc__root_dir == "@@" ):
     print ( "An attempt to run vsdc_print_swin_meta.py that is in the distribution directory." )
     print ( "Please run vsdc_print_swin_meta.py that is in the installation directory, " )
     print ( "i.e. the directory you used when you ran vsdc_install.py" )
     exit  ( 1 )


if ( len(sys.argv) != 2 ):
     print ( "Usage: vsdc_print_swin_meta.py swin_tar_file " )
     exit  ( 1 )
else:
     swin_tar_file = sys.argv[1]

(ret, meta) = vsdc_swin_get_meta ( swin_tar_file )
if ( ret == 0 ):
     for line in meta:
         print ( line )
     exit ( 0 )
else:
     exit ( 1 )
