#!/bin/csh -f
set heb_dir    = /spd/heb/geosfpit/2012
set date_beg   = 20111228
set date_end   = 20120301
set num_cpu    = 6
setenv OMP_NUM_THREADS 1
#
#set tmp_file = /tmp/spd__$$
#find $heb_dir -name "d_*.heb.bz2" | grep "/d/d_" | grep "201207\|201208\|201209\|20121" | sort > $tmp_file
#cat $tmp_file | parallel -P $num_cpu \
#    spd_3d \
#        /progs/spd_20141218/spd/share/met_spd.cnf \
#        {} \
#        /g0/rinex_met/spd/met_ \
#        2
##
set heb_dir    = /spd/heb/geosfpit/2013
set num_cpu    = 6
setenv OMP_NUM_THREADS 1
#
set tmp_file = /tmp/spd__$$
find $heb_dir -name "d_*.heb.bz2" | grep "/d/d_" | grep "20131118\|20131119\|2013112\|2013113\|201312" | sort > $tmp_file
cat $tmp_file | parallel -P $num_cpu \
    spd_3d \
        /progs/spd_20141218/spd/share/met_spd.cnf \
        {} \
        /g0/rinex_met/spd/met_ \
        2
#
set heb_dir    = /spd/heb/geosfpit/2014
set num_cpu    = 6
setenv OMP_NUM_THREADS 1
#
set tmp_file = /tmp/spd__$$
find $heb_dir -name "d_*.heb.bz2" | grep "/d/d_" | grep "20140101\|20140102" | sort > $tmp_file
cat $tmp_file | parallel -P $num_cpu \
    spd_3d \
        /progs/spd_20141218/spd/share/met_spd.cnf \
        {} \
        /g0/rinex_met/spd/met_ \
        2
