#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Program loading_list_allhar_update.csh updates all loading         *
# *   harmonics variations for the default station list.                 *
# *                                                                      *
# * ## 17-JUN-2017 loading_list_allhar_update.csh v1.0 (c) L. Petrov 17-JUN-2017 ## *
# *                                                                      *
# ************************************************************************
set BIN_DIR = /opt64
setenv PATH  "${PATH}:${BIN_DIR}/bin"
set MALO_DIR = `$BIN_DIR/bin/malo_inq script | sed "s@/script@@g"`
set MALO_SCRIPT = `$BIN_DIR/bin/malo_inq script`
set MALO_SHARE = `$BIN_DIR/bin/malo_inq share`
set MALO_BIN = `$BIN_DIR/bin/malo_inq bin_static`
setenv GOMP_STACKSIZE 2000000
limit stacksize       2000000
#
$MALO_SCRIPT/loading_list_update.csh atm geosfpit harmonics
$MALO_SCRIPT/loading_list_update.csh lws geosfpit harmonics
$MALO_SCRIPT/loading_list_update.csh atm merra2   harmonics
$MALO_SCRIPT/loading_list_update.csh lws merra2   harmonics
$MALO_SCRIPT/loading_list_update.csh nto omct     harmonics
$MALO_SCRIPT/loading_list_update.csh toc got410c  harmonics
$MALO_SCRIPT/loading_list_update.csh toc fes2014b harmonics
$MALO_SCRIPT/loading_list_update.csh toc equil01  harmonics
$MALO_SCRIPT/loading_list_update.csh toc equil02  harmonics
