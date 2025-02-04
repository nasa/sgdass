#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Compuitation of station-based loadings.                            *
# *                                                                      *
# * ## 01-MAR-2020 all_loading_list_update.csh v1.0 (c) L. Petrov 01-MAR-2020 ##  *
# *                                                                      *
# ************************************************************************
#
$MALO_DIR/script/loading_list_update.csh atm geosfpit harmonics
$MALO_DIR/script/loading_list_update.csh lws geosfpit harmonics
$MALO_DIR/script/loading_list_update.csh atm merra2   harmonics
$MALO_DIR/script/loading_list_update.csh lws merra2   harmonics
$MALO_DIR/script/loading_list_update.csh nto mpiom06  harmonics
$MALO_DIR/script/loading_list_update.csh toc fes2014b harmonics
$MALO_DIR/script/loading_list_update.csh toc equil01  harmonics
$MALO_DIR/script/loading_list_update.csh toc equil01  harmonics
#
#$MALO_DIR/script/loading_list_update.csh atm geosfpit series 20000101_0000 20020131_2359 all
#$MALO_DIR/script/loading_list_update.csh lws geosfpit series 20000101_0000 20020131_2359 all
#$MALO_DIR/script/loading_list_update.csh nto mpiom06  series 20000101_0000 20020131_2359 all
#
#$MALO_DIR/script/loading_list_update.csh atm merra2   series 19800101_0000 20020131_2359 cm
#$MALO_DIR/script/loading_list_update.csh lws merra2   series 19800101_0000 20020131_2359 cm
