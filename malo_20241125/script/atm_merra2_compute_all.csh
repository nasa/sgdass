#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Program atm_merra_recompute.csh recomputes atmospheric pressure    *
# *   loading for MERRA2 numerical weather model.                         *
# *                                                                      *
# * # 26-APR-2015 atm_merra2_compute_all.csh v1.0 (c) L. Petrov 08-NOV-2016 # *
# *                                                                      *
# ************************************************************************
#
set date_beg = 20160701_0000
set date_end = 20160930_0000
#
#echo "Compute spherical harmonics scaled with Love numbers"
$MALO_DIR/script/compute_love.csh        atm merra2  $date_beg $date_end
if ( $status != 0 ) exit ( 1 )
#
echo "Compute d1 loading time series for the regular grid"
$MALO_DIR/script/loading_time_series.csh atm merra2  grid all $date_beg $date_end
if ( $status != 0 ) exit ( 1 )
#
echo "Compute d1 loading time series for the regular grid"
$MALO_DIR/script/loading_time_series.csh atm merra2  grid d1  $date_beg $date_end
if ( $status != 0 ) exit ( 1 )
#
echo "Compute loading integral"
$MALO_DIR/script/loading_integral.csh /s1/imls/load_grid/atm/merra2/  
if ( $status != 0 ) exit ( 1 )
#
echo "Compute d1 loading integral"
$MALO_DIR/script/loading_integral.csh /s1/imls/load_d1_grid/atm/merra2/
if ( $status != 0 ) exit ( 1 )
