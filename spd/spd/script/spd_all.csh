#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Program spd_all.csh computes slant path delay, and optionally,     *
# *   air opaicty and brighness temperature for a number of epochs       *
# *   specified in the list.                                             *
# *                                                                      *
# *   Copyright (c) 1975-2025 United States Government as represented by *
# *   the Administrator of the National Aeronautics and Space            *
# *   Administration. All Rights Reserved.                               *
# *   License: NASA Open Source Software Agreement (NOSA).               *
# *                                                                      *
# *  ### 14-MAR-2016  spd_all.csh  v1.7 (d)  L. Petrov  14-JAN-2024 ###  *
# *                                                                      *
# ************************************************************************
set heb_dir_merra2   = /imls/heb/merra2/
set heb_dir_geosfpit = /imls/heb/geosfpit/
set date_beg   = 20000000
set date_end   = 20991231
set num_cpu    = 10
setenv OMP_NUM_THREADS 1
#
set spd_list = /tmp/spd_list.txt
if ( `uname -n` == "gs61a-cetus.ndc.nasa.gov" ) then
      set num_cpu            = 16
      setenv OMP_NUM_THREADS    6
  else if ( `uname -n` == "astrogeo" ) then
      set num_cpu            = 10
      setenv OMP_NUM_THREADS    6
  else if ( `uname -n` == "gs61a-crux.gsfc.nasa.gov" ) then
      set num_cpu            = 10
      setenv OMP_NUM_THREADS    6
  else if ( `uname -n` == "gs61a-sagitta.ndc.nasa.gov" ) then
      set num_cpu            = 20
      setenv OMP_NUM_THREADS    2
  else if ( `uname -n` == 'gs61a-geodev-a' ) THEN
      set num_cpu            =  6
      setenv OMP_NUM_THREADS    4
endif

#
if ( $1 == "list_merra2" || $1 == "list_geosfpit" ) then
     if ( -f $spd_list ) rm $spd_list
     if ( $1 == "list_merra2"    ) set heb_dir = $heb_dir_merra2
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
  else if ( $1 == "compute_custom" ) then
    cat $spd_list | \
        parallel -P $num_cpu \
            spd_3d /opt64/spd/share/merra2_opa_spd_vlbi.cnf \
                   {} \
                   /spd/asc/bw024_merra2/spd_merra2_ \
                   2 \
                   $date_beg \
                   $date_end
  else if ( $1 == "compute_merra2" ) then
    cat $spd_list | \
        parallel -P $num_cpu \
            spd_3d /opt64/spd/share/merra2_spd_vlbi.cnf \
                   {} \
                   /spd/asc/merra2/spd_merra2_ \
                   2 \
                   $date_beg \
                   $date_end
  else if ( $1 == "compute_geosfpit" ) then
    cat $spd_list | \
        parallel -P $num_cpu \
            spd_3d /opt64/spd/share/spd_geosfpit_vlbi.cnf \
                   {} \
                   /spd/asc/geosfpit/spd_geosfpit_ \
                   2 \
                   $date_beg \
                   $date_end
  else if ( $1 == "opa_geosfpit" ) then
    cat $spd_list | \
        parallel -P $num_cpu \
            spd_3d /progs/spd_20240111/spd/share/geosfpit_opa_spd_vlbi.cnf \
                   {} \
                   /spd/asc/opa_geosfpit/spd_geosfpit_ \
                   2 \
                   $date_beg \
                   $date_end
  else if ( $1 == "compute_add_geosfpit" ) then
    cat $spd_list | \
        parallel -P $num_cpu \
            spd_3d /progs/spd_20161117/spd/share/geosfpit_spd_add.cnf \
                   {} \
                   /spd/asc/add/spd_geosfpit_add_ \
                   2 \
                   $date_beg \
                   $date_end
  else 
    echo "usage spd_all.csh list_merra|list_geosfpit|compute_merra|opa_geosfpit|compute_add_geosfpit"
endif
