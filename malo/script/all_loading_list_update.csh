#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Compuitation of station-based loadings.                            *
# *                                                                      *
# *   Copyright (c) 1975-2025 United States Government as represented by *
# *   the Administrator of the National Aeronautics and Space            *
# *   Administration. All Rights Reserved.                               *
# *   License: NASA Open Source Software Agreement (NOSA).               *
# *                                                                      *
# * ## 01-MAR-2020 all_loading_list_update.csh v1.0 (d) L. Petrov 31-JAN-2025 ##  *
# *                                                                      *
# ************************************************************************
#
set MALO_DIR = `malo_inq root`
#
# $MALO_DIR/script/loading_list_update.csh atm geosfpit harmonics
# $MALO_DIR/script/loading_list_update.csh lws geosfpit harmonics
# $MALO_DIR/script/loading_list_update.csh nto mpiom06  harmonics
#
#
#
# $MALO_DIR/script/loading_list_update.csh atm geosit    harmonics
# $MALO_DIR/script/loading_list_update.csh lws geosit    harmonics
# $MALO_DIR/script/loading_list_update.csh nto mpiom07   harmonics
# #
# $MALO_DIR/script/loading_list_update.csh atm merra2   harmonics
# $MALO_DIR/script/loading_list_update.csh lws merra2   harmonics
# $MALO_DIR/script/loading_list_update.csh toc fes2014b harmonics
# $MALO_DIR/script/loading_list_update.csh toc equil01  harmonics
# $MALO_DIR/script/loading_list_update.csh toc equil01  harmonics
#
# $MALO_DIR/script/loading_list_update.csh atm geosit series 19980101_0000 20250130_2359 all
# $MALO_DIR/script/loading_list_update.csh lws geosit series 19980101_0030 20250130_2359 all
$MALO_DIR/script/loading_list_update.csh nto mpiom07  series 19790101_0000 20250130_2359 all
#
#$MALO_DIR/script/loading_list_update.csh atm merra2   series 19800101_0000 20250131_2359 cm
#$MALO_DIR/script/loading_list_update.csh lws merra2   series 19800101_0000 20250131_2359 cm
