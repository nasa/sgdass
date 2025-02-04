#!/bin/csh -f
set cnf_file = $SPD_DIR/share/geosfpit_spd_vlbi.cnf 
set inp_dir  = /spd/geosfpit_heb
set out_dir  = /d1/spd/geosfpit
set bspd_dir = /d1/bspd/geosfpit
setenv OMP_NUM_THREADS 6
#
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110913_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110913_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110913_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110913_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110913_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110913_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110913_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110913_2100.heb.bz2 $out_dir/vlbi_ 1
#
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110914_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110914_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110914_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110914_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110914_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110914_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110914_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110914_2100.heb.bz2 $out_dir/vlbi_ 1
#
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110915_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110915_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110915_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110915_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110915_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110915_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110915_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110915_2100.heb.bz2 $out_dir/vlbi_ 1
#
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110916_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110916_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110916_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110916_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110916_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110916_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110916_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110916_2100.heb.bz2 $out_dir/vlbi_ 1
#
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110917_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110917_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110917_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110917_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110917_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110917_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110917_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110917_2100.heb.bz2 $out_dir/vlbi_ 1
#
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110918_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110918_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110918_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110918_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110918_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110918_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110918_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110918_2100.heb.bz2 $out_dir/vlbi_ 1
#
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110919_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110919_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110919_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110919_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110919_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110919_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110919_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110919_2100.heb.bz2 $out_dir/vlbi_ 1
#
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110920_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110920_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110920_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110920_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110920_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110920_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110920_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110920_2100.heb.bz2 $out_dir/vlbi_ 1
#
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110921_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110921_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110921_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110921_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110921_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110921_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110921_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110921_2100.heb.bz2 $out_dir/vlbi_ 1
#
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110922_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110922_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110922_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110922_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110922_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110922_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110922_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110922_2100.heb.bz2 $out_dir/vlbi_ 1
#
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110923_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110923_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110923_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110923_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110923_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110923_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110923_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110923_2100.heb.bz2 $out_dir/vlbi_ 1
#
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110924_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110924_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110924_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110924_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110924_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110924_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110924_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110924_2100.heb.bz2 $out_dir/vlbi_ 1
#
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110925_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110925_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110925_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110925_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110925_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110925_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110925_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110925_2100.heb.bz2 $out_dir/vlbi_ 1
#
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110926_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110926_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110926_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110926_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110926_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110926_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110926_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110926_2100.heb.bz2 $out_dir/vlbi_ 1
#
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110927_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110927_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110927_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110927_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110927_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110927_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110927_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110927_2100.heb.bz2 $out_dir/vlbi_ 1
#
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110928_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110928_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110928_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110928_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110928_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110928_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110928_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110928_2100.heb.bz2 $out_dir/vlbi_ 1
#
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110929_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110929_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110929_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110929_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110929_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110929_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110929_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110929_2100.heb.bz2 $out_dir/vlbi_ 1
#
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110930_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110930_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110930_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110930_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110930_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110930_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110930_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20110930_2100.heb.bz2 $out_dir/vlbi_ 1
#
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20111001_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20111001_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20111001_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20111001_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20111001_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20111001_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20111001_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20111001_2100.heb.bz2 $out_dir/vlbi_ 1
#
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20111002_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20111002_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20111002_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20111002_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20111002_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20111002_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20111002_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file $inp_dir/2011/d/d_20111002_2100.heb.bz2 $out_dir/vlbi_ 1
#
/progs/spd_20131130/bin/spd_3d_to_ser $out_dir $bspd_dir/vlbi_ 3
