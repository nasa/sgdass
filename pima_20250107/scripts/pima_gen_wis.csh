#!/bin/csh 
# ************************************************************************
# *                                                                      *
# *   Program pima_gen_wis.csh
# *                                                                      *
# * ### 18-JUN-2018  pima_gen_wis.csh v1.0 (c) L. Petrov 14-JUN-2021 ### *
# *                                                                      *
# ************************************************************************
if ( `uname` == "Linux" ) then
     set num_threads     = `cat /proc/cpuinfo | grep "core id" | wc -l`
     set num_thr_per_cpu = `lscpu | grep '^Thread(s) per core:' | awk '{print $4}'`
     set num_cores = `expr $num_threads / $num_thr_per_cpu`
else
     set num_cores = `sysctl -n  machdep.cpu.core_count`
endif
if ($#argv == 0) then 
    echo "Usage pima_gen_wis.csh {small|big} {all|ncores}"
    exit 1
  else
    set use   = $1
    set ncore = $2
endif
set dirnam = `dirname $0`
if ( $dirnam == "." ) then
     set dirnam = `pwd`
endif
set dirnam = `echo $dirnam | sed "s@/scripts@@g"`
setenv PIMA_DIR $dirnam
#
echo "The number of cores $num_cores"
if ( $ncore == "all" ) then
     if ( $num_cores == 64 ) then
          create_fftw_plan MEASURE 32 $PIMA_DIR/share/pima/pima_wis_${use}.inp $PIMA_DIR/share/pima/pima_${use}_measure_32thr.wis
          create_fftw_plan MEASURE  1 $PIMA_DIR/share/pima/pima_wis_${use}.inp $PIMA_DIR/share/pima/pima_${use}_measure_1thr.wis
          create_fftw_plan MEASURE  8 $PIMA_DIR/share/pima/pima_wis_${use}.inp $PIMA_DIR/share/pima/pima_${use}_measure_8thr.wis
          create_fftw_plan MEASURE 16 $PIMA_DIR/share/pima/pima_wis_${use}.inp $PIMA_DIR/share/pima/pima_${use}_measure_16thr.wis
          create_fftw_plan MEASURE 64 $PIMA_DIR/share/pima/pima_wis_${use}.inp $PIMA_DIR/share/pima/pima_${use}_measure_64thr.wis
     endif
     #
     if ( $num_cores == 40 ) then
          create_fftw_plan MEASURE  1 $PIMA_DIR/share/pima/pima_wis_${use}.inp $PIMA_DIR/share/pima/pima_${use}_measure_1thr.wis
          create_fftw_plan MEASURE 16 $PIMA_DIR/share/pima/pima_wis_${use}.inp $PIMA_DIR/share/pima/pima_${use}_measure_16thr.wis
          create_fftw_plan MEASURE 32 $PIMA_DIR/share/pima/pima_wis_${use}.inp $PIMA_DIR/share/pima/pima_${use}_measure_32thr.wis
          create_fftw_plan MEASURE 40 $PIMA_DIR/share/pima/pima_wis_${use}.inp $PIMA_DIR/share/pima/pima_${use}_measure_40thr.wis
     endif
     if ( $num_cores == 32 ) then
          create_fftw_plan MEASURE  1 $PIMA_DIR/share/pima/pima_wis_${use}.inp $PIMA_DIR/share/pima/pima_${use}_measure_1thr.wis
          create_fftw_plan MEASURE  8 $PIMA_DIR/share/pima/pima_wis_${use}.inp $PIMA_DIR/share/pima/pima_${use}_measure_8thr.wis
          create_fftw_plan MEASURE 16 $PIMA_DIR/share/pima/pima_wis_${use}.inp $PIMA_DIR/share/pima/pima_${use}_measure_16thr.wis
          create_fftw_plan MEASURE 32 $PIMA_DIR/share/pima/pima_wis_${use}.inp $PIMA_DIR/share/pima/pima_${use}_measure_32thr.wis
     endif
     if ( $num_cores == 20 ) then
          create_fftw_plan MEASURE  1 $PIMA_DIR/share/pima/pima_wis_${use}.inp $PIMA_DIR/share/pima/pima_${use}_measure_1thr.wis
          create_fftw_plan MEASURE  8 $PIMA_DIR/share/pima/pima_wis_${use}.inp $PIMA_DIR/share/pima/pima_${use}_measure_8thr.wis
          create_fftw_plan MEASURE 16 $PIMA_DIR/share/pima/pima_wis_${use}.inp $PIMA_DIR/share/pima/pima_${use}_measure_16thr.wis 
          create_fftw_plan MEASURE 20 $PIMA_DIR/share/pima/pima_wis_${use}.inp $PIMA_DIR/share/pima/pima_${use}_measure_20thr.wis
     endif
     if ( $num_cores == 2 ) then
          create_fftw_plan MEASURE  1 $PIMA_DIR/share/pima/pima_wis_${use}.inp $PIMA_DIR/share/pima/pima_${use}_measure_1thr.wis
          create_fftw_plan MEASURE  2 $PIMA_DIR/share/pima/pima_wis_${use}.inp $PIMA_DIR/share/pima/pima_${use}_measure_2thr.wis
     endif
  else
     create_fftw_plan MEASURE $ncore $PIMA_DIR/share/pima/pima_wis_${use}.inp $PIMA_DIR/share/pima/pima_${use}_measure_${ncore}thr.wis
endif
