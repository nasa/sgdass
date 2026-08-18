#!/bin/csh -f
setenv OMP_NUM_THREADS 1
setenv SPD_DIR /progs/spd_20140925
#
python3 $SPD_DIR/spd/script/spd_all.py \
        $SPD_DIR/spd/share/merra_spd_vlbi.cnf \
        /spd/heb/merra/2014 2014.01.01_00:00 2014.01.11_00:00 \
        /spd/spd/merra/spd_merra_
#
#$SPD_DIR/spd/script/merra_year.csh 1999
#$SPD_DIR/spd/script/merra_year.csh 1998
#$SPD_DIR/spd/script/merra_year.csh 1997
#$SPD_DIR/spd/script/merra_year.csh 1996
#$SPD_DIR/spd/script/merra_year.csh 1995
#$SPD_DIR/spd/script/merra_year.csh 1994
#$SPD_DIR/spd/script/merra_year.csh 1993
#$SPD_DIR/spd/script/merra_year.csh 1992
#$SPD_DIR/spd/script/merra_year.csh 1991
#$SPD_DIR/spd/script/merra_year.csh 1990
#$SPD_DIR/spd/script/merra_year.csh 1989
#$SPD_DIR/spd/script/merra_year.csh 1988
#$SPD_DIR/spd/script/merra_year.csh 1987
#$SPD_DIR/spd/script/merra_year.csh 1986
#$SPD_DIR/spd/script/merra_year.csh 1985
#$SPD_DIR/spd/script/merra_year.csh 1984
#$SPD_DIR/spd/script/merra_year.csh 1983
#$SPD_DIR/spd/script/merra_year.csh 1982
#$SPD_DIR/spd/script/merra_year.csh 1981
#$SPD_DIR/spd/script/merra_year.csh 1980
#
#$SPD_DIR/spd/script/merra_year.csh 2000
#$SPD_DIR/spd/script/merra_year.csh 2001
#$SPD_DIR/spd/script/merra_year.csh 2002
#$SPD_DIR/spd/script/merra_year.csh 2003
#$SPD_DIR/spd/script/merra_year.csh 2004
#$SPD_DIR/spd/script/merra_year.csh 2005
#$SPD_DIR/spd/script/merra_year.csh 2006
#$SPD_DIR/spd/script/merra_year.csh 2007
#$SPD_DIR/spd/script/merra_year.csh 2008
#$SPD_DIR/spd/script/merra_year.csh 2009
#$SPD_DIR/spd/script/merra_year.csh 2010
#$SPD_DIR/spd/script/merra_year.csh 2011
#$SPD_DIR/spd/script/merra_year.csh 2012
#$SPD_DIR/spd/script/merra_year.csh 2013
#$SPD_DIR/spd/script/merra_year.csh 2014
