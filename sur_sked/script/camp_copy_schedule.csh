#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Program camp_copy_schedule.csh copies schdule file and changes all *
# *   the fields related to the new experiment name, except spind.       *
# *                                                                      *
# *   Copyright (c) 1975-2025 United States Government as represented by *
# *   the Administrator of the National Aeronautics and Space            *
# *   Administration. All Rights Reserved.                               *
# *   License: NASA Open Source Software Agreement (NOSA).               *
# *                                                                      *
# * # 16-MAR-2026 camp_copy_schedule.csh v1.0 (d) L. Petrov 17-MAR-2026 # *
# *                                                                      *
# ************************************************************************
if ( $#argv < 2 ) then 
     echo "Usage: exp_from exp_to"
     exit 0
endif
set exp_from = $1
set exp_to   = $2

set dir = "/vlbi"

set sch_in = ${dir}/${exp_from}/${exp_from}.skf
set txt_in = ${dir}/${exp_from}/${exp_from}.txt

if ( ! -f $sch_in ) then
     echo "Did not find input schedule file $sch_in"
     exit 1
endif

if ( ! -f $txt_in ) then
     echo "Did not find input schedule file $sch_in"
     exit 1
endif

set dirout = ${dir}/${exp_to}
if ( ! -d $dirout ) then
     mkdir -m 775 $dirout
endif

set sch_out = ${dir}/${exp_to}/${exp_to}.skf
set txt_out = ${dir}/${exp_to}/${exp_to}.txt

cat $sch_in  | sed "s@$exp_from@$exp_to@g" | sed "s@${exp_to}/${exp_to}.spind@${exp_from}/${exp_from}.spind@g" > $sch_out
cat $txt_in  | sed "s@$exp_from@$exp_to@g" > $txt_out

echo "Created file $sch_out"
echo "Created file $txt_out"
