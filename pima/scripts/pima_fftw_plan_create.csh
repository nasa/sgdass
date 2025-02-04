#!/bin/csh 
# ********************************************************************************
# *                                                                              *
# *   Program pima_fftw_plan_create.csh                                          *
# *                                                                              *
# * ### 18-JUN-2018 pima_fftw_plan_create.csh v2.0 (c) L. Petrov 22-FEB-2022 ### *
# *                                                                              *
# ********************************************************************************
if ( `uname` == "Linux" ) then
     set num_threads     = `cat /proc/cpuinfo | grep "core id" | wc -l`
     set num_thr_per_cpu = `lscpu | grep '^Thread(s) per core:' | awk '{print $4}'`
     set num_cores = `expr $num_threads / $num_thr_per_cpu`
else
     set num_cores = `sysctl -n  machdep.cpu.core_count`
endif
if ($#argv < 2) then 
    echo "Usage pima_fftw_plan_create.csh {small|big} {all|ncores}"
    exit 1
  else
    set use   = $1
    set ncore = $2
endif
setenv PIMA_SHARE_DIR `pima --share`
#
echo "The number of cores $num_cores"
if ( $ncore == "all" ) then
     if ( $num_cores >= 1 ) then
          create_fftw_plan MEASURE  1 $PIMA_SHARE_DIR/pima_wis_${use}.inp $PIMA_SHARE_DIR/pima_${use}_measure_1thr.wis
     endif
     #
     if ( $num_cores >= 2 ) then
          create_fftw_plan MEASURE  2 $PIMA_SHARE_DIR/pima_wis_${use}.inp $PIMA_SHARE_DIR/pima_${use}_measure_2thr.wis
     endif
     if ( $num_cores >= 4 ) then
          create_fftw_plan MEASURE  4 $PIMA_SHARE_DIR/pima_wis_${use}.inp $PIMA_SHARE_DIR/pima_${use}_measure_4thr.wis
     endif
     if ( $num_cores >= 8 ) then
          create_fftw_plan MEASURE  8 $PIMA_SHARE_DIR/pima_wis_${use}.inp $PIMA_SHARE_DIR/pima_${use}_measure_8thr.wis
     endif
     if ( $num_cores >= 16 ) then
          create_fftw_plan MEASURE 16 $PIMA_SHARE_DIR/pima_wis_${use}.inp $PIMA_SHARE_DIR/pima_${use}_measure_16thr.wis
     endif
     if ( $num_cores >= 32 ) then
          create_fftw_plan MEASURE 32 $PIMA_SHARE_DIR/pima_wis_${use}.inp $PIMA_SHARE_DIR/pima_${use}_measure_32thr.wis
     endif
     if ( $num_cores >= 64 ) then
          create_fftw_plan MEASURE 64 $PIMA_SHARE_DIR/pima_wis_${use}.inp $PIMA_SHARE_DIR/pima_${use}_measure_64thr.wis
     endif
     if ( $num_cores >= 96 ) then
          create_fftw_plan MEASURE 48 $PIMA_SHARE_DIR/pima_wis_${use}.inp $PIMA_SHARE_DIR/pima_${use}_measure_48thr.wis
          create_fftw_plan MEASURE 96 $PIMA_SHARE_DIR/pima_wis_${use}.inp $PIMA_SHARE_DIR/pima_${use}_measure_96thr.wis
     endif
     if ( $num_cores == 20 ) then
          create_fftw_plan MEASURE 20 $PIMA_SHARE_DIR/pima_wis_${use}.inp $PIMA_SHARE_DIR/pima_${use}_measure_20thr.wis
     endif
     if ( $num_cores == 40 ) then
          create_fftw_plan MEASURE 40 $PIMA_SHARE_DIR/pima_wis_${use}.inp $PIMA_SHARE_DIR/pima_${use}_measure_40thr.wis
     endif
  else
     create_fftw_plan MEASURE $ncore $PIMA_SHARE_DIR/pima_wis_${use}.inp $PIMA_SHARE_DIR/pima_${use}_measure_${ncore}thr.wis
endif
