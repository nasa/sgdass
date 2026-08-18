#!/bin/csh -f
set MALO_SHARE  = `malo_inq share`
set MALO_ROOT   = `malo_inq root`
set MALO_MODEL  = `malo_inq model`
#
set typ   = "atm"
set model = "geosit" 
#
if ( $typ == "atm" && $model == "geosit" ) then
     malo_loading_model load \
          $MALO_SHARE/atm_geosit_load_d2699.cnf                        \
          $MALO_MODEL/spr_atm_geosit_model_19980101_20241218_d2699.heb \
          /imls/load_har_grid/atm/geosit/atm_geosit_harmod.all         \
          $MALO_SHARE/atm_geosit_data_source.txt                       \
          $MALO_SHARE/atm_description.txt                              \
          3
     loading_heb_to_spl_heb  /imls/load_har_grid/atm/geosit/atm_geosit_harmod.heb    \
                             /imls/load_har_spl/atm/geosit/atm_geosit_spl_harmod.heb
     loading_spl_heb_to_sta  /imls/load_har_spl/atm/geosit/atm_geosit_spl_harmod.heb \
                             $MALO_SHARE/loading.sta                              \
                             /imls/load_har_list/atm/geosit/atm_geosit_harmod.hps
endif
#
if ( $typ == "lws" && $model == "geosit" ) then
     malo_loading_model load \
          $MALO_SHARE/lws_geosit_load_d2699.cnf                        \
          $MALO_MODEL/twland_lws_geosit_model_1998_2025_d2699.heb      \
          /imls/load_har_grid/lws/geosit/lws_geosit_harmod.all         \
          $MALO_SHARE/lws_geosit_data_source.txt                       \
          $MALO_SHARE/lws_description.txt                              \
          3
     loading_heb_to_spl_heb  /imls/load_har_grid/lws/geosit/lws_geosit_harmod.heb    \
                             /imls/load_har_spl/lws/geosit/lws_geosit_spl_harmod.heb
     loading_spl_heb_to_sta  /imls/load_har_spl/lws/geosit/lws_geosit_spl_harmod.heb \
                             $MALO_SHARE/loading.sta                                 \
                             /imls/load_har_list/lws/geosit/lws_geosit_harmod.hps
endif
