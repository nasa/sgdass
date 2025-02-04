#!/bin/csh -f
set year = $1
if ( ${?SPD_DIR} == 0 ) then
     echo "merra_year.csh: environment variable SPD_DIR is not defined"
     exit 1
endif
if ( -d ${SPD_DIR} ) then
   else 
     echo "merra_year.csh: environment variable SPD_DIR shows to a directory $SPD_DIR that does not exist"
     exit 1
endif
python3 $SPD_DIR/spd/script/spd_all.py \
        $SPD_DIR/spd/share/merra_spd_vlbi.cnf \
        /spd/heb/merra/$year $year.01.01_00:00 $year.12.31_18:00 \
        /spd/spd/merra/spd_merra_
#
