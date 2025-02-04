#!/bin/csh -f
limit stacksize        8000000
setenv GOMP_STACKSIZE  2000000
set num_cpu = $OMP_NUM_THREADS
setenv OMP_NUM_THREADS 1
setenv SPD_DIR /progs/spd_20150430
set tmp_file = /tmp/spd__$$
if ( $1 == "" ) then
     set mode = 5
  else 
     set mode = $1
endif
#
onintr cleanup
if ( $mode == 1 ) then
     set date_beg   = 20050318
     set date_end   = 20050322
     set heb_dir   = /spd/heb/geosfpit/
     set resp_dir  = /spd/resp/geosfpit
     set fil_geoid = EGM2008_geoid_height_bspl_d1023.heb
     set fil_oh    = geosfpit_height_above_geoid.heb 
#
     set tmp_file = /tmp/resp.files
     if ( -f $tmp_file ) rm $tmp_file
     set files = `find $heb_dir -name "d_*.heb.bz2" | \
         sort -k 1r | \
         awk '{printf ("%s@%s\n", substr($1,length($1)-20,13), $1)}'`
     foreach file ($files)
        set filedate = `echo $file | sed "s/@/ /g" | sed "s/_/ /g" | awk '{printf ("%s\n", $1)}'`
        set filename = `echo $file | sed "s/@/ /g" | awk '{printf ("%s\n", $2)}'`
        set filedate = `echo $filename | awk '{print substr($1,length($filenamwe)-20,13)}'`
        if ( `echo $filedate | gawk '{if ($1 > '$date_beg' && $1 < '$date_end' ){print}}'` != "" ) then
              echo $filename >> $tmp_file
        endif
     end
     cat $tmp_file | parallel -P $num_cpu \
         spd_resp {} $resp_dir $fil_geoid $fil_oh 2
  else if ( $mode == 2 ) then
     set heb_dir = /Volumes/Xsan2/spd/heb/geosfpit
     set spd_cnf = /Volumes/Xsan2/spd/control/geosfpit_spd_vlbi.cnf
     set out_dir = /Volumes/Xsan2/spd/spd
     set out_prefix = "vlbi_"
#
     set tmp_file = /tmp/spd__$$
     find $heb_dir -name "d_*.heb.bz2" | grep "/d/d_" | grep "201" | sort -r > $tmp_file
     cat $tmp_file | parallel -P $num_cpu \
         spd_3d \
         $spd_cnf \
         {} \
         $out_dir/$out_prefix \
         2
  else if ( $mode == 3 ) then
     set heb_dir = /spd/heb/geosfpit/2012
     set spd_cnf = /progs/spd_20141218/spd/share/opt_zen_spd.cnf 
     set out_dir = /spd/spd/opt_532_grid
     set out_prefix = grid_0.25x0.25_
#
     onintr cleanup
     find $heb_dir -name "d_*.heb.bz2" | grep "/d/d_" | grep "201" | sort -r > $tmp_file
     cat $tmp_file | parallel -P $num_cpu \
         spd_3d \
         $spd_cnf \
         {} \
         $out_dir/$out_prefix \
         2
  else if ( $mode == 4 ) then
     set heb_dir = /spd/heb/geosfpit/2012
     set spd_cnf = /progs/spd_20141218/spd/share/radio_spd.cnf 
     set out_dir = /spd/spd/radio_grid
     set out_prefix = grid_0.25x0.25_
#
     find $heb_dir -name "d_*.heb.bz2" | grep "/d/d_" | grep "201" | sort -r > $tmp_file
     cat $tmp_file | parallel -P $num_cpu \
         spd_3d \
         $spd_cnf \
         {} \
         $out_dir/$out_prefix \
         2
  else if ( $mode == 5 ) then
     set num_cpu    = 16
     set date_beg   = 20000101
     set date_end   = 20150918
     set heb_dir    = /imsl/heb/geosfpit
     set spd_cnf    = $SPD_DIR/spd/share/geosfpit_spd_igs.cnf
     set out_dir    = /spd/asc/igs_geosfpit
     set out_prefix = spd_geosfpit_
#
     set tmp_file = /tmp/spd_files.txt
#     if ( -f $tmp_file == 0 ) then
#          find $heb_dir -name "*.heb.bz2" | grep "/d/d_" | sort -k 1r > $tmp_file 
#     endif
     cat $tmp_file | parallel -P $num_cpu \
         spd_3d \
         $spd_cnf \
         {} \
         $out_dir/$out_prefix \
         2
  else if ( $mode == 6 ) then
     set num_cpu    = 18
     set date_beg   = 19790101
     set date_end   = 20150227
     set heb_dir = /imsl/heb/merra
     set spd_cnf = $SPD_DIR/spd/share/merra_spd_vlbi.cnf
     set out_dir = /spd/asc/merra
     set out_prefix = spd_merra_
#
     set tmp_file = /tmp/merra_files.txt
     if ( -f $tmp_file == 0 ) then
          find $heb_dir -name "*.heb.bz2" | grep "/d/d_" | sort -k 1r > $tmp_file 
     endif
     cat $tmp_file | parallel -P $num_cpu \
         spd_3d \
         $spd_cnf \
         {} \
         $out_dir/$out_prefix \
         2
endif
cleanup:
# if ( -f $tmp_file ) rm -f $tmp_file
