#!/bin/csh -f
set fil = $1
if ( $fil == "" ) then
     echo "Missing the argument vcat configuration file"
     exit 1 
endif
if ( -f $fil == 0 ) then
     echo "vcat configutation file $fil does not exist"
     exit 1 
endif
set DATE_LONG = `date "+%Y%m%d_%H%M%S" | tr "[a-z]" "[A-Z]"`
set vcat_label = `cat $fil | head -1`
set header_test = `cat $fil | head -1 | grep "# VCAT  Configuration file. Version of 2006.02.18"`
if ( "$header_test" != "" ) then
     set old_fil = ${fil}_${DATE_LONG}.old
     cp $fil $old_fil
     set old_gvf_db_dir    = `cat $fil | grep "^GVF_DB_DIR:"    |  awk '{print $2}'`
     set old_gvf_env_dir   = `cat $fil | grep "^GVF_ENV_DIR:"   |  awk '{print $2}'`
     set old_vcat_conf_fil = `cat $fil | grep "^VTD_CONF_FILE:" |  awk '{print $2}'`
     echo "# VCAT  Configuration file.  Version of 2020.06.08"  > $fil
     echo "#"                                                  >> $fil
     echo "# Upgraded by upgrade_vcat_conf.csh on $DATE_LONG"  >> $fil
     echo "#"                                                  >> $fil
     echo "GVF_REP_NAMES:  OBS SYS"                            >> $fil
     echo "#"                                                  >> $fil
     echo "GVF_ENV_DIR:    OBS $old_gvf_env_dir"               >> $fil
     echo "GVF_DB_DIR:     OBS $old_gvf_db_dir"                >> $fil
     echo "#"                                                  >> $fil
     echo "GVF_ENV_DIR:    SYS $old_gvf_env_dir"               >> $fil
     echo "GVF_DB_DIR:     SYS $old_gvf_db_dir"                >> $fil
     echo "#"                                                  >> $fil
     echo "VTD_CONF_FILE:  $old_vcat_conf_fil"                 >> $fil
     echo "#"                                                  >> $fil
     echo "# VCAT  Configuration file.  Version of 2020.06.08" >> $fil
#
     echo "Upgraded vcat configuration file $fil"
     echo "Old vcat configuraiton file is written in $old_fil"
endif
