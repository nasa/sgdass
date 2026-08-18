#!/bin/csh -f
limit stacksize        100000
setenv GOMP_STACKSIZE  200000
setenv LD_LIBRARY_PATH /opt64/lib:/opt64/lib64
/opt64/bin/aam_fcs_intrp /aam/geos_fcs now etab
