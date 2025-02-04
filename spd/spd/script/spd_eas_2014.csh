#!/bin/csh -f
setenv OMP_NUM_THREADS 16
spd_3d /progs/spd_20151111/spd/share/geosfpit_spd_eas.cnf \
       /imsl/heb/geosfpit/2014 \
       /spd/asc/eas_geosfpit/spd_eas_geosfpit_ \
       1 \
       2014.01.01 \
       2015.01.01 
