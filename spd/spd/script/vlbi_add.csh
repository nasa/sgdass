#!/bin/csh -f
set year = $1
set date_beg = 2016.01.01
set date_end = 2017.01.01
set year = `echo $date_beg | awk '{printf ( "%4s", substr($1,0,4) )}'`
#
if ( ${?SPD_DIR} == 0 ) then
     echo "vlbi_add.csh: environment variable SPD_DIR is not defined"
     exit 1
endif
if ( -d ${SPD_DIR} ) then
   else 
     echo "merra_year.csh: environment variable SPD_DIR shows to a directory $SPD_DIR that does not exist"
     exit 1
endif
setenv OMP_NUM_THREADS 1
python3 $SPD_DIR/spd/script/spd_all.py               \
        $SPD_DIR/spd/share/spd_geosfpit_add.cnt      \
        /imsl/heb/geosfpit/$year $date_beg $date_end \
        /spd/asc/add/geosfpit_add_ 2
#
