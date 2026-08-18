#!/bin/csh -f
set db_agv = /tmp/a1.agv
if ( $#argv < 3 ) then 
     echo "Usage: db_sta_rename.csh db_name old_sta_name new_sta_name [vers]"
     exit 1
endif
#
set db_name  = $1
set old_name = "$2"
set new_name = "$3"
if ( $4 == "" ) then
     gvf_transform -to_ascii ${db_name}_v002.env $db_agv
  else
     gvf_transform -to_ascii ${db_name}_v00${4}.env $db_agv
endif
if ( `grep $old_name $db_agv` == "" ) then
      echo "station $old_name was not observed in VLBI experiment $db_name"
      exit 1
endif
sed -i "s@$old_name@$new_name@g" $db_agv
gvf_transform -to_binary $db_agv  @
if ( $status == 0 ) then
     echo "Station $old_name was successfully renamed to $new_name in $db_name"
     rm $db_agv
endif
