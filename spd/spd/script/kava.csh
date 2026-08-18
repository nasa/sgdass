#!/bin/csh -f
setenv OMP_NUM_THREADS 1
spd_3d /progs/spd_20151111/spd/share/geosfpit_spd_kava.cnf \
       /imsl/heb/geosfpit/ \
       /spd/asc/kava_geosfpit/kava_geosfpit_ \
       1 \
       2015.07.19 \
       2015.07.26
#
spd_3d /progs/spd_20151111/spd/share/geosfpit_spd_kava.cnf \
       /imsl/heb/geosfpit/ \
       /spd/asc/kava_geosfpit/kava_geosfpit_ \
       1 \
       2015.12.16 \
       2015.12.23
