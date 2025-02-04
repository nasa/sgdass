#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Program malo_fftw_plane_create.csh creates wisdom files for FFTW   *
# *                                                                      *
# *   It removes all wisdoms if option remove is specified.              *
# *                                                                      *
# * # 29-APR-2021 malo_fftw_plane_create.csh v1.0 (c) L. Petrov 29-APR-2021 #  *
# *                                                                      *
# ************************************************************************
set MALO_SHARE = `malo_inq share`
if ( $1 == "remove" ) then
     rm -f $MALO_SHARE/malo_fftw_plan_*thr.wis >& /dev/null
endif
#
if ( `uname -n` == "asrtrogeo" ) then
      create_fftw_plan measure  1 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_1thr.wis
      create_fftw_plan measure  2 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_2thr.wis
      create_fftw_plan measure  4 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_4thr.wis
      create_fftw_plan measure  8 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_8thr.wis
      create_fftw_plan measure 16 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_16thr.wis
      create_fftw_plan measure 32 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_32thr.wis
      create_fftw_plan measure 64 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_64thr.wis
   else if ( `uname -n` == "asrtrogeo" ) then
      create_fftw_plan measure  1 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_1thr.wis
      create_fftw_plan measure  2 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_2thr.wis
      create_fftw_plan measure  4 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_4thr.wis
      create_fftw_plan measure  8 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_8thr.wis
      create_fftw_plan measure 16 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_16thr.wis
      create_fftw_plan measure 20 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_20thr.wis
   else if ( `uname -n` == "gs61a-geodev-a" ) then
      create_fftw_plan measure  1 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_1thr.wis
      create_fftw_plan measure  2 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_2thr.wis
      create_fftw_plan measure  4 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_4thr.wis
      create_fftw_plan measure  8 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_8thr.wis
      create_fftw_plan measure 16 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_16thr.wis
      create_fftw_plan measure 32 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_20thr.wis
   else if ( `uname -n` == "gs61a-cetus.ndc.nasa.gov" ) then
      create_fftw_plan measure  1 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_1thr.wis
      create_fftw_plan measure  2 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_2thr.wis
      create_fftw_plan measure  4 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_4thr.wis
      create_fftw_plan measure  8 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_8thr.wis
      create_fftw_plan measure 16 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_16thr.wis
      create_fftw_plan measure 32 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_32thr.wis
      create_fftw_plan measure 48 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_48thr.wis
      create_fftw_plan measure 64 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_64thr.wis
      create_fftw_plan measure 96 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_96thr.wis
   else
      create_fftw_plan measure  1 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_1thr.wis
      create_fftw_plan measure  2 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_2thr.wis
      create_fftw_plan measure  4 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_4thr.wis
      create_fftw_plan measure  8 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_8thr.wis
      create_fftw_plan measure 20 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_16thr.wis
      create_fftw_plan measure 40 $MALO_SHARE/sphe_wis.inp $MALO_SHARE/malo_fftw_plan_32thr.wis
endif
