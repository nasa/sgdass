#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Program gen_db.csh puts several database in the GVF data direcotry *
# *   for tests.                                                         *
# *                                                                      *
# * ### 22-DEC-2023 psolve_gen_db.csh v1.3 (c) L. Petrov 23-DEC-2023 ### *
# *                                                                      *
# ************************************************************************
set SOLVE_GVF_DATA = $1
set PSOLVE_ROOT    = $2
if ( ${?VCAT_CONF} == 1 ) then
#
# -- unset VCAT_CONF environment variable
#
     setenv VCAT_CONF_OLD $VCAT_CONF
     unsetenv VCAT_CONF
endif
if ( `ls -c1 $SOLVE_GVF_DATA/obs/env | wc -l` == 0 ) then
      echo "Installing OBS databases... "
      gvf_import.py ${PSOLVE_ROOT}/data/20061209_k.vda.bz2 OBS
      gvf_import.py ${PSOLVE_ROOT}/data/20210907_p.vda.bz2 OBS
      gvf_import.py ${PSOLVE_ROOT}/data/20220705_p.vda.bz2 OBS
      echo "Installed three OBS databases"
endif
if ( `ls -c1 $SOLVE_GVF_DATA/int/env | wc -l` == 0 ) then
      echo "Installing INT databases... "
      gvf_import.py ${PSOLVE_ROOT}/data/20230930_j.vda.bz2 INT
      echo "Installed one INT database"
endif
if ( ${?VCAT_CONF_OLD} == 1 ) then
     setenv   VCAT_CONF     $VCAT_CONF_OLD 
     unsetenv VCAT_CONF_OLD
endif
