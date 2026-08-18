#!/usr/bin/env python3
import  sys, os, argparse
from    vsdc_swin import *

swin_magic = "# SWIN-ARCHIVE meta data." 

#
# ------------------------------------------------------------------------
#
if __name__ == "__main__":

   swin_tar_file = sys.argv[1]
   check_inp_files = 1
   (ret, exper_name, date_start, date_stop ) = vsdc_swin_check ( swin_magic, swin_tar_file )
   print ( ret )
