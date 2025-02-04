#!/bin/bash -f
# ************************************************************************
# *                                                                      *
# *   Bash-program update_viono.sh downloads GPS TEC map data files,     *
# *   and/or updates a priori ionosphere TEC data in VIONO format.       *
# *                                                                      *
# *   Usage: update_viono.sh mode [year]                                 *
# *                                                                      *
# *          where mode are one of                                       *
# *          1 -- download raw ionosphere TEC data from CDDIS at NASA    *
# *               Goddard Space Flight Center.                           *
# *          2 -- download raw ionosphere TEC data of CODE model for     *
# *               the specified year in 20th century. The year should    *
# *               be in two-digits format.                               *
# *          3 -- download raw ionosphere TEC data of CODE model for     *
# *               the specified year in 21th century. The year should    *
# *               be in two-digits format.                               *
# *          4 -- Create VIONO file from raw ionosphere TEC files from   *
# *               CODE.                                                  *
# *          5 -- update VIONO file from raw ionosphere TEC files from   *
# *               CODE.                                                  *
# *                                                                      *
# * ### 20-JUN-2010 update_viono.sh v1.0 (c) L. Petrov  20-JUN-2010 ###  *
# *                                                                      *
# ************************************************************************
if test "${VTD_ROOT+set}" != set ; then
   echo "Environtment VTD_ROOT is not defined"
   exit 1
fi
#
# ---- You can change this directory name if you like
#
VIONO_DIR=$VTD_ROOT/share
#
mode=$1
year=$2
#
if [ ! -d $VIONO_DIR ] ; then
     echo "Viono directory $VIONO_DIR does not exist. Please create it"
     exit 1
fi
if [ "$mode" = "1" ] ; then
     cd $VIONO_DIR
     wget -r \
     -N \
     -nH \
     --cut-dirs=4 \
     -e robots=off \
     -X robots.txt \
     ftp://cddis.gsfc.nasa.gov/pub/gps/products/ionex
  elif [ "$mode" = 2 ] ; then
     cent=19
     cd $VIONO_DIR
     wget -r \
     -c \
     -N \
     -nH \
     --cut-dirs=4 \
     -e robots=off \
     -X robots.txt \
     -A "CODG*${year}I.Z" \
      ftp://ftp.unibe.ch/aiub/CODE/${cent}${year}
  elif [ "$mode" = 3 ] ; then
     cent=20
     cd $VIONO_DIR
     wget -r \
     -c \
     -N \
     -nH \
     --cut-dirs=4 \
     -e robots=off \
     -X robots.txt \
     -A "CODG*${year}I.Z" \
      ftp://ftp.unibe.ch/aiub/CODE/${cent}${year}
  elif [ "$mode" = 4 ] ; then
#
# -- Starts from 1998.03.28
#
     $VTD_ROOT/bin/gti_update $VIONO_DIR/code/ $VTD_ROOT/share/codg_01.vio code 1998y087d 2002y306d 4 create
     $VTD_ROOT/bin/gti_update $VIONO_DIR/code/ $VTD_ROOT/share/codg_02.vio code 2002y307d 2049      4 create
  elif [ "$mode" = 5 ] ; then
     $VTD_ROOT/bin/gti_update $VIONO_DIR/code/ $VTD_ROOT/share/codg_02.vio code 2002y307d 2049      4 
fi
#
