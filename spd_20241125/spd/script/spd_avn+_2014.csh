#!/bin/csh -f
setenv OMP_NUM_THREADS 2
spd_3d /progs/spd_20151111/spd/share/geosfpit_spd_avn+.cnf \
       /imsl/heb/geosfpit/2014 \
       /spd/asc/avn+_geosfpit/spd_avn+_geosfpit_ \
       1 \
       2014.11.20 \
       2015.01.01 
