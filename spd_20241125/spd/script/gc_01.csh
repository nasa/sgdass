#!/bin/csh -f
set heb_dir_merra    = /imsl/heb/merra/
set heb_dir_geosfpit = /imsl/heb/geosfpit/
set date_beg   = 20180815
set date_end   = 20180825
set num_cpu    = 16
setenv OMP_NUM_THREADS 1
#
set spd_list = /tmp/gc_01.txt
#
if ( $1 == "list_merra" || $1 == "list_geosfpit" ) then
     if ( -f $spd_list ) rm $spd_list
     if ( $1 == "list_merra"   ) set heb_dir = $heb_dir_merra
     if ( $1 == "list_geosfpit" ) set heb_dir = $heb_dir_geosfpit
     set files = `find $heb_dir -name "d_*.heb.bz2" | \
         sort -k 1r | \
         sed "s@d/d_@d/d_ @" | \
         awk '{printf ("%s@%s%s\n", substr($2,0,13), $1, $2)}'`
     foreach file ($files)
        set filedate = `echo $file | sed "s/@/ /g" | sed "s/_/ /g" | awk '{printf ("%s\n", $1)}'`
        set filename = `echo $file | sed "s/@/ /g" | awk '{printf ("%s\n", $2)}'`
        echo $filename >> $spd_list
    end
    echo "Created list file: $spd_list"    
  else if ( $1 == "compute_merra" ) then
    cat $spd_list | \
        parallel -P $num_cpu \
            spd_3d /opt64/spd/share/merra_spd_vlbi.cnf \
                   {} \
                   /spd/asc/merra/spd_merra_ \
                   2 \
                   $date_beg \
                   $date_end
  else if ( $1 == "compute_geosfpit" ) then
    cat $spd_list | \
        parallel -P $num_cpu \
            spd_3d  /opt64/spd/share/spd_geosfpit_vlbi.cnf \
                   {} \
                   /spd/asc/geosfpit/spd_geosfpit_ \
                   2 \
                   $date_beg \
                   $date_end
  else if ( $1 == "goldcreek" ) then
    cat $spd_list | \
        parallel -P $num_cpu \
            spd_3d /progs/spd_20161117/spd/share/goldcreek.cnf \
                   {} \
                   /spd/asc/goldcreek/spd_goldcreek_ \
                   2 \
                   $date_beg \
                   $date_end
  else if ( $1 == "compute_special" ) then
    cat $spd_list | \
        parallel -P $num_cpu \
            spd_3d /progs/spd_20161117/spd/share/ngvla.cnf \
                   {} \
                   /spd/asc/ngvla/spd_geosfpit_ngvla_ \
                   2 \
                   $date_beg \
                   $date_end
  else if ( $1 == "compute_map" ) then
    cat $spd_list | \
        parallel -P $num_cpu \
            spd_3d /progs/spd_20161117/spd/share/ngvla_map.cnf \
                   {} \
                   /spd/asc/ngvla_map/spd_geosfpit_ngvla_ \
                   2 \
                   $date_beg \
                   $date_end
  else if ( $1 == "compute_hr_map" ) then
    setenv OMP_NUM_THREADS $num_cpu
    spd_3d /progs/spd_20161117/spd/share/ngvla_hr_map.cnf \
           /imsl/heb/geosfpit/2018/d/d_20180625_1200.heb.bz2 \
           /spd/asc/ngvla_hr_map/spd_geosfpit_ngvla_ \
           2 \
           $date_beg \
           $date_end
    spd_3d /progs/spd_20161117/spd/share/ngvla_hr_map.cnf \
           /imsl/heb/geosfpit/2018/d/d_20181225_1200.heb.bz2 \
           /spd/asc/ngvla_hr_map/spd_geosfpit_ngvla_ \
           2 \
           $date_beg \
           $date_end
  else 
    echo "usage "spd_all.csh list_merra|list_geosfpit|compute_merra|compute_geosfpit|compute_special"
endif
