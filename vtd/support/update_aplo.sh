#!/bin/bash -f
# ************************************************************************
# *                                                                      *
# *   Program for updating  local directory with the atmospheric         *
# *   pressure loading.                                                  *
# *                                                                      *
# * ### 18-JAN-2009  update_aplo.sh  v1.2 (c) L. Petrov  20-JUN-2010 ### *
# *                                                                      *
# ************************************************************************
if test "${VTD_ROOT+set}" != set ; then
   echo "Environtment VTD_ROOT is not defined"
   exit 1
fi
incoming=tmp
aplo_bds=$VTD_ROOT/share/aplo_bds
if [ ! -d aplo_bds ] ; then
     echo "aplo_bds directory $aplo_bds does not exist. Please create it"
     exit 1
fi
cd $incoming
if [ -f /incoming/aplo_bds.tar.bz2 ]; then
     rm /incoming/aplo_bds.tar.bz2 
fi
wget -q http://lacerta.gsfc.nasa.gov/aplo/aplo_bds.tar.bz2
cd $aplo_bds
tar -jxf /incoming/aplo_bds.tar.bz2
if [ -f /incoming/aplo_bds.tar.bz2 ]; then
     rm /incoming/aplo_bds.tar.bz2 
fi
