#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Program sou_rename_gvf.csh renames a source. The new source name   *
# *   length shgould not exceed 8 characters
# *                                                                      *
# *   Copyright (c) 1975-2025 United States Government as represented by *
# *   the Administrator of the National Aeronautics and Space            *
# *   Administration. All Rights Reserved.                               *
# *   License: NASA Open Source Software Agreement (NOSA).               *
# *                                                                      *
# * ## 26-APR-2025 sou_rename_gvf.csh v1.0 (d) L. Petrov 26-APR-2025 ### *
# *                                                                      *
# ************************************************************************
set db_agv = /tmp/a1.agv
set vers_def = 0
set repo_def = OBS
if ( $#argv < 3 ) then 
     echo "Usage: sou_rename_gvf.csh db_name old_sou_name new_sou_name [vers] [repo]"
     exit 1
endif
#
set db_name  = $1
set old_name = "$2"
set new_name = "$3"
set vers     = $4
set repo     = $5
if ( $vers == 0 ):
     set vers = $vers_def
endif
if ( $repo == 0 ):
     set vers = $repo_def
endif
#
set repo_lower = `echo $repo | tr "[A-Z]" "[a-z]"`
set repo_upper = `echo $repo | tr "[a-z]" "[A-Z]"`
dir_env = "/l2/gvf/${repo_lower}/env"
if ( $vers == 0 ) then
#
# -- Find the last version of the database
#
     set db_last_vers = `ls -c1 ${dir_env}/ | grep ${db_name} | sort | tail -1`
     if ( $db_last_vers == "" ) then
          echo "Did not find database $db_las_vers in repository $repo"
          exit ( 1 )
     endif
#
# -- Transform the last version to VDA
#
     gvf_transform -to_ascii ${db_last_vers} $db_agv
  else
#
# -- Transform the specified version to VDA
#
     gvf_transform -to_ascii ${db_name}_v00${4}.env $db_agv
endif
if ( `grep $old_name $db_agv` == "" ) then
      echo "source $old_name was not observed in VLBI experiment $db_name"
      exit 1
endif
sed -i "s@$old_name@$new_name@g" $db_agv
#
# --- Transform VDA to GVF
#
gvf_transform -to_binary $db_agv  @
if ( $status == 0 ) then
     echo "Source $old_name was successfully renamed to $new_name in $db_name"
     rm $db_agv
endif
