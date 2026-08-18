#!/bin/csh -f
#
set typ =   geosit_lws_2699
#set mode = "atm_spr"
#set mode = "lws_list"
#set mode = "lws_spr"
set mode = "lws_model"
#
setenv MALO_DIR `malo_inq prefix`
setenv MALO_ROOT `malo_inq root`
setenv OMP_NUM_THREADS 1
#
if ( $typ == geosip_atm_1023 ) then
     set geos_dir   = /imls/geosit_heb
     set spr_dir    = /imls/spr/geosit/
     set file_igh   = geosit_height_above_geoid.heb
     set file_ogh   = gtopo30_dig_elev_above_geoid_d1023.heb
     set num_cpu    = 6
endif
#
if ( $typ == geosit_atm_2699 ) then
     set geos_dir   = /imls/heb/geosit/
     set spr_dir    = /imls/spr_2699/geosit/
     set file_igh   = geosit_height_above_geoid.heb
     set file_ogh   = gtopo30_dig_elev_above_geoid_d2699.heb
     set num_cpu    = 96
endif
#
if ( $typ == geosit_lws_2699 ) then
     set geos_dir   = /imls/heb/geosit/
     set spr_dir    = /imls/spr_2699/geosit/
     set file_igh   = geosit_height_above_geoid.heb
     set file_ogh   = gtopo30_dig_elev_above_geoid_d2699.heb
     set num_cpu    = 96
endif
#
if ( $typ == geosip_atm_10799 ) then
     set geos_dir   = /imls/heb/geosit/
     set spr_dir    = /t0/spr_10799/geosit/
     set file_igh   = geosit_height_above_geoid.heb
     set file_ogh   = gtopo30_dig_elev_above_geoid_d10799.heb
     set num_cpu    = 16
endif
#
if ( $typ == merra_atm_2699 ) then
     set geos_dir   =  /s1/imls/heb/merra2
     set spr_dir    = /s1/imls/spr/merra2/
     set file_igh   = geosit_height_above_geoid.heb
     set file_ogh   = gtopo30_dig_elev_above_geoid_d2699.heb
     set num_cpu    = 16
endif
#
if ( $mode == "atm_list" ) then
     find $geos_dir -name "d_*.heb.bz2" | sort -k 1r > /tmp/spr_atm_list.txt
     exit 0
endif
if ( $mode == "lws_list" ) then
     find $geos_dir -name "twland_*.heb.bz2" | sort -k 1r | sed "s@_@ @g" | sed "s@\.@ @g" | awk '{printf "%s_%s\n", $2, $3}' | \
            grep -v "_2330" | \
            grep -v "_2230" | \
            grep -v "_2030" | \
            grep -v "_1930" | \
            grep -v "_1730" | \
            grep -v "_1630" | \
            grep -v "_1430" | \
            grep -v "_1330" | \
            grep -v "_1130" | \
            grep -v "_1030" | \
            grep -v "_0830" | \
            grep -v "_0730" | \
            grep -v "_0530" | \
            grep -v "_0430" | \
            grep -v "_0230" | \
            grep -v "_0130" > \
            /tmp/spr_lws_list.txt
     exit 0
endif
if ( $mode == "atm_spr" ) then
     cat /tmp/spr_atm_list.txt | \
         sed "s@d/d_@ @" | \
         awk '{printf ("%s\n", substr($2,0,13))}' | \
         parallel -P $num_cpu \
         $MALO_DIR/bin/gen_spr $geos_dir {} $file_igh $file_ogh $spr_dir lbzip2_p1
  else if ( $mode == "lws_spr" ) then
     cat /tmp/spr_lws_list.txt | \
         parallel -P $num_cpu \
         $MALO_ROOT/script/lws_loading.py ${MALO_ROOT}/share/deva_lws_geosit.conf 1000000000 {} 1
  else if ( $mode == "lws_model" ) then
     $MALO_DIR/bin/spr_model /imls/spr/geosit lws 1998.01.01_00:00 2025.01.01_00:00 2 /imls/oper_model/spr_lws_geosit_model_1998_2025_d2699.heb
endif

