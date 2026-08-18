#!/usr/bin/env python3
import  sys, os, argparse
sys.path.append('/home/lpetrov/bin')
from    vsdc_antcal import *

vsdc_antcal_magic = "# ANTCAL Format" 

#
# ------------------------------------------------------------------------
#
if __name__ == "__main__":

   antcal_file = sys.argv[1]
   check_inp_files = 1
   ( ret, exper_name, sta_name_short, sta_name_long, date_start, date_stop  ) = \
          vsdc_anc_check ( vsdc_antcal_magic, antcal_file )
   print ( ret )
   print ( "exper_name= ", exper_name )
   print ( "sta_name_short= ", sta_name_short )
   print ( "sta_name_long=  ", sta_name_long  )
   print ( "date_start=     ", date_start )
   print ( "date_stop=      ", date_stop )
