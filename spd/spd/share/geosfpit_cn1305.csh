#!/bin/csh -f
set cnf_file = $SPD_DIR/share/geosfpit_spd_vlbi.cnf 
set out_dir  = /g1/geosfpit_spd_vlbi
setenv OMP_NUM_THREADS 6
#
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131120_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131120_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131120_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131120_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131120_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131120_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131120_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131120_2100.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131121_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131121_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131121_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131121_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131121_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131121_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131121_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131121_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131121_2100.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131122_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131122_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131122_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131122_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131122_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131122_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131122_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131122_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131122_2100.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131123_0000.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131123_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131123_0300.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131123_0600.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131123_0900.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131123_1200.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131123_1500.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131123_1800.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131123_2100.heb.bz2 $out_dir/vlbi_ 1
$SPD_DIR/bin/spd_3d $cnf_file /spd/geosfpit_heb/2013/d/d_20131124_0000.heb.bz2 $out_dir/vlbi_ 1
