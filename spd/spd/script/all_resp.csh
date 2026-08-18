#!/bin/csh -f
#spd_resp met_file out_dir fil_geoid fil_oh [verbosity] [compress]
#
set heb_dir   = /imsl/heb/geosfpit
set resp_dir  = /s0/resp/geosfpit
set fil_geoid = EGM2008_geoid_height_bspl_d1023.heb
set fil_oh    = geosfpit_height_above_geoid.heb 
#
set temp_fil = /tmp/all_resp.list
#
if ( $1 == "list" ) then
     find $heb_dir/ -name "*d_*.heb.bz2" | grep /d/d_ | sort > $temp_fil
     echo "heb listing is put in file $temp_fil"
     exit 0
  else if ( $1 == "resp" ) then
     set num_proc = $OMP_NUM_THREADS
     setenv OMP_NUM_THREADS 1
     onintr cleanup
     cat $temp_fil | parallel -P $num_proc \
         spd_resp {} $resp_dir $fil_geoid $fil_oh 70 572 359 1.D-1 1.D-1 2
     cleanup:
#     rm -f $temp_fil
     exit 0
  else
     echo "Usage: all_resp.csh list|resp"
     exit 1
endif
