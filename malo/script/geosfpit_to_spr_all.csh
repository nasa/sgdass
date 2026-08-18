#!/bin/csh -f
#
set typ =   geosfpipt_atm_2699
set mode = "spr"
setenv OMP_NUM_THREADS 1
#
if ( $typ == geosfpipt_atm_1023 ) then
     set geos_dir   = /imls/geosfpit_heb
     set spr_dir    = /imls/spr/geosfpit/
     set file_igh   = geosfpit_height_above_geoid.heb
     set file_ogh   = gtopo30_dig_elev_above_geoid_d1023.heb
     set num_cpu    = 6
endif
#
if ( $typ == geosfpipt_atm_2699 ) then
     set geos_dir   = /imls/heb/geosfpit/
     set spr_dir    = /t0/spr_2699/geosfpit/
     set file_igh   = geosfpit_height_above_geoid.heb
     set file_ogh   = gtopo30_dig_elev_above_geoid_d2699.heb
     set num_cpu    = 16
endif
#
if ( $typ == geosfpipt_atm_10799 ) then
     set geos_dir   = /imls/heb/geosfpit/
     set spr_dir    = /t0/spr_10799/geosfpit/
     set file_igh   = geosfpit_height_above_geoid.heb
     set file_ogh   = gtopo30_dig_elev_above_geoid_d10799.heb
     set num_cpu    = 16
endif
#
if ( $typ == merra_atm_2699 ) then
     set geos_dir   =  /s1/imls/heb/merra2
     set spr_dir    = /s1/imls/spr/merra2/
     set file_igh   = geosfpit_height_above_geoid.heb
     set file_ogh   = gtopo30_dig_elev_above_geoid_d2699.heb
     set num_cpu    = 16
endif
#
#find $geos_dir -name "d_*.heb.bz2" | \
#    sort -k 1r | \
#    sed "s@d/d_@ @" | \
#    awk '{printf ("%s\n", substr($2,0,13))}' | \
#    parallel -P $num_cpu \
#    $MALO_DIR/bin/gen_spr $geos_dir {} $file_igh $file_ogh $spr_dir lbzip2_p1
##
if ( $mode == "list" ) then
     find $geos_dir -name "d_*.heb.bz2" | sort -k 1r > /tmp/spr_list.txt
     exit 0
endif
if ( $mode == "spr" ) then
     cat /tmp/spr_list.txt | \
         sed "s@d/d_@ @" | \
         awk '{printf ("%s\n", substr($2,0,13))}' | \
         parallel -P $num_cpu \
         $MALO_DIR/bin/gen_spr $geos_dir {} $file_igh $file_ogh $spr_dir lbzip2_p1
endif
2019.02.04_11:14:31
