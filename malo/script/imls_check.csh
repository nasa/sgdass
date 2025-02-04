#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Program imls_check.csh checks the imls massloading results         *
# *                                                                      *
# * ### 24-MAY-2017  imls_check.csh v1.7 (c)  L. Petrov  21-OCT-2023 ### *
# *                                                                      *
# ************************************************************************
#
set rep = "short"
#
set tmp_report = /tmp/malo_check__$$
if ( -f $tmp_report ) rm -f tmp_report 
set date_iso = `date -u "+%Y.%m.%d_%H:%M:%S"`
echo "imls check on `uname -n` at $date_iso UTC"
echo "==============================================================="
#
$MALO_DIR/script/imls_check_series.py load_grid     atm geosfpit  3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_grid     atm merra2    6.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_grid     lws geosfpit  3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_grid     lws merra2    3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_grid     nto mpiom06   3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_grid     nto mpiom07   3.0 $rep $tmp_report
echo " "
set str_num_grid_files = `cat $tmp_report`
#
$MALO_DIR/script/imls_check_series.py load_spl      atm geosfpit  3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_spl      atm merra2    6.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_spl      lws geosfpit  3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_spl      lws merra2    3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_spl      nto mpiom06   3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_spl      nto mpiom07   3.0 $rep $tmp_report
echo " "
#
$MALO_DIR/script/imls_check_series.py load_list     atm geosfpit  3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_list     atm merra2    6.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_list     lws geosfpit  3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_list     lws merra2    3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_list     nto mpiom06   3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_list     nto mpiom07   3.0 $rep $tmp_report
echo " "
#
$MALO_DIR/script/imls_check_series.py load_int      atm geosfpit  3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_int      atm merra2    6.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_int      lws geosfpit  3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_int      lws merra2    3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_int      nto mpiom06   3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_int      nto mpiom07   3.0 $rep $tmp_report
echo " "
#
$MALO_DIR/script/imls_check_series.py load_d1_grid  atm geosfpit  3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_d1_grid  atm merra2    6.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_d1_grid  lws geosfpit  3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_d1_grid  lws merra2    3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_d1_grid  nto mpiom06   3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_d1_grid  nto mpiom07   3.0 $rep $tmp_report
echo " "
#
$MALO_DIR/script/imls_check_series.py load_d1_spl   atm geosfpit  3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_d1_spl   atm merra2    6.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_d1_spl   lws geosfpit  3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_d1_spl   lws merra2    3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_d1_spl   nto mpiom06   3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_d1_spl   nto mpiom07   3.0 $rep $tmp_report
echo " "
#
$MALO_DIR/script/imls_check_series.py load_d1_list  atm geosfpit  3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_d1_list  atm merra2    6.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_d1_list  lws geosfpit  3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_d1_list  lws merra2    3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_d1_list  nto mpiom06   3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_d1_list  nto mpiom07   3.0 $rep $tmp_report
echo " "
#
$MALO_DIR/script/imls_check_series.py load_d1_int   atm geosfpit  3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_d1_int   atm merra2    6.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_d1_int   lws geosfpit  3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_d1_int   lws merra2    3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_d1_int   nto mpiom06   3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py load_d1_int   nto mpiom07   3.0 $rep $tmp_report
echo " "
#
$MALO_DIR/script/imls_check_series.py vgep          atm geosfpit  3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py vgep          atm merra2    6.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py vgep          lws geosfpit  3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py vgep          lws merra2    3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py vgep          nto mpiom06   3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py vgep          nto mpiom07   3.0 $rep $tmp_report
echo " "
#
$MALO_DIR/script/imls_check_series.py aam           ''  geosfp    3.0 $rep $tmp_report
$MALO_DIR/script/imls_check_series.py aam           ''  geosfpit  3.0 $rep $tmp_report
#
set str_num_files = `cat $tmp_report`
rm -f $tmp_report
echo "==============================================================="
echo $str_num_grid_files | $MALO_DIR/script/print_num_grid_points.py
echo $str_num_grid_files | $MALO_DIR/script/print_long_number.py
echo $str_num_files      | $MALO_DIR/script/print_long_number.py
