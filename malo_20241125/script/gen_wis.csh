#!/bin/csh
if ( `uname -n` == "gs61a-geodev-a" ) then
     create_fftw_plan MEASURE  1 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_1thr.wis
     create_fftw_plan MEASURE  2 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_2thr.wis
     create_fftw_plan MEASURE  8 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_8thr.wis
     create_fftw_plan MEASURE 16 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_16thr.wis
     create_fftw_plan MEASURE 32 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_32thr.wis
else if ( `uname -n` == "gs61a-sagitta" ) then
     create_fftw_plan MEASURE  1 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_1thr.wis
     create_fftw_plan MEASURE  2 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_2thr.wis
     create_fftw_plan MEASURE  8 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_8thr.wis
     create_fftw_plan MEASURE 16 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_10thr.wis
     create_fftw_plan MEASURE 20 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_20thr.wis
     create_fftw_plan MEASURE 32 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_32thr.wis
     create_fftw_plan MEASURE 40 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_40thr.wis
else if ( `uname -n` == "earthrotation" ) then
     create_fftw_plan MEASURE  1 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_1thr.wis
     create_fftw_plan MEASURE  2 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_2thr.wis
     create_fftw_plan MEASURE  8 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_8thr.wis
     create_fftw_plan MEASURE 12 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_12thr.wis
     create_fftw_plan MEASURE 16 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_16thr.wis
     create_fftw_plan MEASURE 20 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_20thr.wis
else if ( `uname -n` == "astrogeo" ) then
     create_fftw_plan MEASURE  1 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_1thr.wis
     create_fftw_plan MEASURE  2 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_2thr.wis
     create_fftw_plan MEASURE  8 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_8thr.wis
     create_fftw_plan MEASURE 10 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_10thr.wis
     create_fftw_plan MEASURE 12 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_12thr.wis
     create_fftw_plan MEASURE 16 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_16thr.wis
     create_fftw_plan MEASURE 20 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_20thr.wis
     create_fftw_plan MEASURE 32 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_32thr.wis
     create_fftw_plan MEASURE 64 $MALO_DIR/share/malo_fftw_plan.inp $MALO_DIR/share/malo_fftw_plan_64thr.wis
endif
