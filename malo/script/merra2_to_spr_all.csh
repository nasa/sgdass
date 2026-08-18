#!/bin/csh -f
set merra_list = /tmp/merra2_list.txt
set merra_dir  = /s1/imls/heb/merra2 
set spr_dir    = /s1/imls/spr/merra2/atm/
set file_igh   = merra2_height_above_geoid.heb
set file_ogh   = gtopo30_dig_elev_above_geoid_d2699.heb
set num_cpu    = 16
set mode       = $1
#
if ( $mode == 1 ) then
     find $merra_dir -name "d_*.heb.bz2" | sort -k 1r > $merra_list
  else if ( $mode == 2 ) then
     cat $merra_list | \
     sed "s@d/d_@ @" | \
     awk '{printf ("%s\n", substr($2,0,13))}' | \
     parallel -P $num_cpu \
     $MALO_DIR/bin/gen_spr $merra_dir {} $file_igh $file_ogh $spr_dir    
  else
     echo "Usage merra2_to_spr_all.csh mode"
     exit 1
endif
