#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Routine compute_mpiom_spr.csh copmutes the bottom pressure from    *
# *   AOD1B products.                                                    *
# *                                                                      *
# * # 30-APR-2015 compute_mpiom07_spr.csh v1.1 (c) L. Petrov 22-SEP-2023 # *
# *                                                                      *
# ************************************************************************
#
setenv OMP_NUM_THREADS 1
if ( `uname -n` == 'astrogeo' ) then
      set num_cpu    = 24
  else
      set num_cpu    = 32
endif
set date_beg   = 1970.01.01
set date_end   = 2049.12.31
set temp_file  = /tmp/spr__$$.csh
set mpiom_stokes_orig = /imls/orig_data/mpiom07/orig
set mpiom_stokes_asc  = /imls/orig_data/mpiom07/asc
set mpiom_pres_dir    = /t0/heb/mpiom07
set lis_file          = /tmp/mpiom07.lis
set cnf_file          = nto_mpiom07_load_d2699_nomodel.cnf
#
# --- First uncompress original datafiles
#
if ( $1 == "uncompress" ) then
     cd $mpiom_stokes_asc  
     set years = `ls -c1 $mpiom_stokes_orig | grep -v TIDES`
     foreach year ($years)
        if ( -d $mpiom_stokes_asc/$year == 0 ) mkdir $mpiom_stokes_asc/$year 
     end
     find  $mpiom_stokes_orig -name '*.gz' | grep -v TIDES | grep AOD1B | awk '{printf "gzip -cd %s > %sA\n", $1, $1}' | \
           sed "s@gz > $mpiom_stokes_orig@gz > $mpiom_stokes_asc@g" | sed "s@asc.gzA@asc@g" | \
           sort > $temp_file
     cat $temp_file | parallel -P $num_cpu
     rm $temp_file
     exit 0
endif
if ( $1 == "list" ) then
     find  $mpiom_stokes_asc -name '*.asc' | grep AOD1B | grep -v '#' | sort -k 1r > $lis_file
     echo "File list is written in $lis_file"
     exit 0 
endif
# --- Then compute bottom pressure
#
if ( $1 == "compute" ) then
     cat $lis_file | \
     parallel -P $num_cpu \
     malo grav_sphe_pres_create \
     $cnf_file \
     {} \
     $date_beg \
     $date_end \
     $mpiom_pres_dir \
     1 \
     lbzip2_p1
     echo "Computed bottom pressure"
     exit 0 
endif
