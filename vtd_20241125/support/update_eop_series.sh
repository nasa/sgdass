#!/bin/bash -f
# ************************************************************************
# *                                                                      *
# *   Bash-program update_eop_series.sh updates the a priori EOP time    *
# *   series file.                                                       *
# *                                                                      *
# *  ### 20-JUN-2010               v1.0 (c)  L. Petrov  20-JUN-2010 ###  *
# *                                                                      *
# ************************************************************************
tmp_file=/tmp/eop__$$
if test "${VTD_ROOT+set}" != set ; then
   echo "Environtment VTD_ROOT is not defined"
   exit 1
fi
#
wget -q -O $tmp_file http://gemini.gsfc.nasa.gov/500/oper/solve_save_files/last.erp
if [ $status != 0]; then
     if [ -f $tmp_file ]; then
          rm $tmp_file 
     fi
fi
mv $tmp_file $VTD_ROOT/share/vlbi_apr.erp
rm $tmp_file 
