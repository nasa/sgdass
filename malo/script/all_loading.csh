#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Program all_loading.csh computes loadings or integral ofloadings   *
# *   for the list of dates.
# *                                                                      *
# *   Copyright (c) 1975-2025 United States Government as represented by *
# *   the Administrator of the National Aeronautics and Space            *
# *   Administration. All Rights Reserved.                               *
# *   License: NASA Open Source Software Agreement (NOSA).               *
# *                                                                      *
# * ### 01-MAR-2017 all_loading.csh v1.4 (d)  L. Petrov  26-JAN-2026 ### *
# *                                                                      *
# ************************************************************************
set MALO_SHARE  = `malo_inq share`
set MALO_ROOT   = `malo_inq root`
set MALO_MODEL  = `malo_inq root`
set MALO_BIN    = `malo_inq bin`
setenv OMP_NUM_THREADS 1
#
#
set mode = $1
set model = $2
if ( $model == "" ) then
     echo "Usage all_loading.csh mode model"
     exit 1
endif
#
# set mode = atm_list
# set mode = atm_int
# set mode = atm_loading
# set mode = atm_vgep
# set mode = lws_list
# set mode = lws_loading
# set mode = lws_int
# set mode = nto_list
# set mode = nto_loading
# set mode = nto_int
# set mode = nto_vgep
#
if ( `uname -n` == "gs61a-cetus.ndc.nasa.gov" ) then
      set host    = "deva"
      set num_cpu = 96
  else if ( `uname -n` == "gs61a-sagitta.ndc.nasa.gov" ) then
      set host    = "deva"
      set num_cpu = 32
  else if ( `uname -n` == "gs61a-crux.gsfc.nasa.gov" ) then
      set host    = "deva"
      set num_cpu = 64
  else if ( `uname -n` == "gs61a-geodev-a" ) then
      set host    = "deva"
      set num_cpu = 30
endif
#
# --- Atmosphere pressure loading
#
if ( $mode == "atm_list" ) then
     find /imls/heb/${model}/ -name "*.bz2" | grep "/d_" | sort -r | \
          sed "s@.heb.bz2@@g"   | \
          sed "s@_@ @g"   | \
          awk '{printf "%s_%s\n", $2, $3}' \
          > /tmp/atm_loading.list
endif
if ( $mode == "atm_loading" ) then
     cat  /tmp/atm_loading.list | \
         parallel -P $num_cpu \
         $MALO_ROOT/script/atm_loading.py $MALO_SHARE/${host}_atm_${model}.conf 1111111111 {} 2 \
     >> /tmp/atm_loading.log
endif
if ( $mode == "atm_vgep" ) then
     cat  /tmp/atm_loading.list | \
         parallel -P $num_cpu \
         $MALO_ROOT/script/atm_loading.py $MALO_SHARE/${host}_atm_${model}.conf 1100000100 {} 2 \
     >> /tmp/atm_vgep.log
endif
if ( $mode == "atm_int" ) then
     cat  /tmp/atm_loading.list | \
         parallel -P $num_cpu \
         $MALO_ROOT/script/int_loading.py $MALO_SHARE/${host}_atm_${model}.conf {} 3 \
	>> /tmp/atm_loading_int.log
endif
if ( $mode == "atm_aam" ) then
     cat  /tmp/atm_loading.list | \
         parallel -P $num_cpu \
         $MALO_BIN/gen_aam       \
                /imls/heb/$model \
                {}               \
                ${model}_height_above_ellipsoid.heb        \
                gtopo30_dig_elev_above_ellipsoid_d2699.heb \
                /imls/oper_model/fls_mask_d2699.heb        \
                /imls/aam/${model}/aam_${model}_           \
	>> /tmp/aam.log
endif
if ( $mode == "lws_vgep" ) then
     cat  /tmp/lws_loading.list | \
         parallel -P $num_cpu \
         $MALO_ROOT/script/lws_loading.py $MALO_SHARE/${host}_lws_${model}.conf 1100000100 {} 2 \
     >> /tmp/lws_vgep.log
endif
#
# --- Land water storage loading
#
if ( $mode == "lws_list" ) then
     find /imls/heb/${model}/ -name "*.bz2" | grep twland | sort -r | \
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
          grep -v "_0130" | \
          grep -v "_2300" | \
          grep -v "_2200" | \
          grep -v "_2000" | \
          grep -v "_1900" | \
          grep -v "_1700" | \
          grep -v "_1600" | \
          grep -v "_1400" | \
          grep -v "_1300" | \
          grep -v "_1100" | \
          grep -v "_1000" | \
          grep -v "_0800" | \
          grep -v "_0700" | \
          grep -v "_0500" | \
          grep -v "_0400" | \
          grep -v "_0200" | \
          grep -v "_0100" | \
          sed "s@.heb.bz2@@g"   | \
          sed "s@_@ @g"   | \
          awk '{printf "%s_%s\n", $2, $3}' \
          > /tmp/lws_loading.list
endif
if ( $mode == "lws_loading" ) then
     cat  /tmp/lws_loading.list | \
         parallel -P $num_cpu \
         $MALO_ROOT/script/lws_loading.py $MALO_SHARE/${host}_lws_${model}.conf 1111111111 {} 2 \
     >> /tmp/lws_loading.log
endif
if ( $mode == "lws_int" ) then
     cat  /tmp/lws_loading.list | \
         parallel -P $num_cpu \
         $MALO_ROOT/script/int_loading.py $MALO_SHARE/${host}_lws_${model}.conf {} 3 \
     >> /tmp/lws_loading_int.log
endif
#
# --- Non tidal loading
#
if ( $mode == "nto_list" ) then
     find /imls/orig_data/${model}/orig/ -name "*.asc.gz" | sort -r | \
          sed "s@.asc.gz@@g"   | \
          sed "s@_@ @g"   | \
          sed "s@-@.@g"   | \
          awk '{printf "%s\n", $3}' \
          > /tmp/nto_loading.list
endif
#
if ( $mode == "nto_loading" ) then
#
# -- Requres a list in a form of 2025.01.31
#
     cat /tmp/nto_loading.list | \
         parallel -P $num_cpu \
         $MALO_ROOT/script/nto_loading.py $MALO_SHARE/${host}_nto_${model}.conf 1111111111 {} 2 \
     >> /tmp/nto_loading.log
endif
#
if ( $mode == "nto_vgep" ) then
     cat  /tmp/nto_loading.list | \
         parallel -P $num_cpu \
         $MALO_ROOT/script/nto_loading.py $MALO_SHARE/${host}_nto_${model}.conf 1100000100 {} 2 \
     >> /tmp/nto_vgep.log
endif
#
if ( $mode == "nto_int" ) then
#
# -- Requres a list in a form of 20250131_2100
#
     cat  /tmp/nto_loading.list | \
         parallel -P $num_cpu \
         $MALO_ROOT/script/int_loading.py $MALO_SHARE/${host}_nto_${model}.conf {} 3 \
     >> /tmp/nto_loading_int.log
endif
