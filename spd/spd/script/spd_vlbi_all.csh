#!/bin/csh -f
#
#   Memory use for L576x361 case: 20.0 + 0.22*N_threads
#   4 threads: 19.5 Gib
#   8 threads: 20.3 Gib
#
limit stacksize        8000000
setenv GOMP_STACKSIZE  2000000
set num_cpu = $OMP_NUM_THREADS
setenv OMP_NUM_THREADS 1
setenv SPD_DIR /progs/spd_20241125
#setenv SPD_DIR /progs/spd_20240111
set tmp_file = /tmp/spd__$$
if ( $1 == "" ) then
     set mode = 2
  else 
     set mode = $1
endif
#
onintr cleanup
if ( $mode == 1 ) then
#
# --- sa: jobs: 8, threads: 4
#
     set num_jobs   = 9
     setenv OMP_NUM_THREADS 4
#
     set date_beg   = 20240101
     set date_end   = 20241120
     set heb_dir    = /imls/heb/geosit/2023
     set spd_cnf    = ${SPD_DIR}/spd/share/geosit_opa_spd_vlbi.cnf
     set out_dir    = /spd/asc/geosit
     set out_prefix = opa_spd_geosit_
#
     set tmp_file = /tmp/geosit_files.txt
     if ( -f $tmp_file == 0 ) then
          find $heb_dir -name "*.heb.bz2" | grep "/d/d_" | sort -k 1r > $tmp_file 
     endif
     cat $tmp_file | parallel -P $num_jobs \
         spd_3d \
         $spd_cnf \
         {} \
         $out_dir/$out_prefix \
         2
  else if ( $mode == 2 ) then
#
# --- as and cr:  # as,cr: jobs: 12, threads: 8
#
     set num_jobs   = 8
     setenv OMP_NUM_THREADS 8
#
     set date_beg   = 20230101
     set date_end   = 20240101
     set heb_dir    = /imls/heb/geosit
     set spd_cnf    = ${SPD_DIR}/spd/share/geosit_opa_spd_vlbi.cnf
     set out_dir    = /spd/asc/geosit
     set out_prefix = opa_spd_geosit_
#
     set tmp_file = /tmp/geosit_files.txt
     if ( -f $tmp_file == 0 ) then
          find $heb_dir -name "*.heb.bz2" | grep "/d/d_" | sort -k 1r > $tmp_file 
     endif
     cat $tmp_file | parallel -P $num_jobs \
         spd_3d_static  \
         $spd_cnf \
         {} \
         $out_dir/$out_prefix \
         2
  else if ( $mode == 3 ) then
#
# --- cetus: 
#
     set num_jobs   = 16
     setenv OMP_NUM_THREADS 6
#
     set date_beg   = 20220101
     set date_end   = 20230101
     set heb_dir    = /imls/heb/geosit
     set spd_cnf    = ${SPD_DIR}/spd/share/geosit_opa_spd_vlbi.cnf
     set out_dir    = /spd/asc/geosit
     set out_prefix = opa_spd_geosit_
#
     set tmp_file = /tmp/geosit_files.txt
     if ( -f $tmp_file == 0 ) then
          find $heb_dir -name "*.heb.bz2" | grep "/d/d_" | sort -k 1r > $tmp_file 
     endif
     cat $tmp_file | parallel -P $num_jobs \
         spd_3d \
         $spd_cnf \
         {} \
         $out_dir/$out_prefix \
         2
endif
cleanup:
# if ( -f $tmp_file ) rm -f $tmp_file
