#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Program loading_list_allser_update.csh updates all loading time    *
# *   series for the default station list. 
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
$MALO_SCRIPT/loading_list_update.csh atm geosfpit series
$MALO_SCRIPT/loading_list_update.csh lws geosfpit series
$MALO_SCRIPT/loading_list_update.csh atm merra2   series
$MALO_SCRIPT/loading_list_update.csh lws merra2   series
$MALO_SCRIPT/loading_list_update.csh nto omct     series
