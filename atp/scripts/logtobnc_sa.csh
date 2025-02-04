#!/bin/csh -f
#
# ************************************************************************
# *                                                                      *
# *   Program logtobnc.csh converts an experiment log file to binary     *
# *   format as defined in the atp library.				 *
# *   First the log file is converted to anc, then to binary.            *
# *   In the initial version, we assume a fixed destination for all      *
# *   files, and user need only declare log file name.			 *
# *                                                                      *
# * ### 06-FEB-2023  logtobnc.csh    v1.0 (c) N. Habana  06-FEB-2023 ### *
# *                                                                      *
# ************************************************************************
#
if ( $#argv == 0 ) then 
     echo "Usage: logtobnc.csh sde|vlbi log-file"
     exit 1
else
#
set exp_type  = $1						# Get experiment type
#
# -- check if the experiment type is viable
#
if ( $1 == "sde" || $1 == "vlbi" ) then
   echo true
else
   echo " Initial argument can only be 'sde' or 'vlbi' "
   exit ( 1 )
endif
#
# -- Declare files and paths
#
set fil_log   = $2
set exp       = $2:t:r
#
if ( $exp_type == "sde" ) then
   set stn_id   = `echo $exp | cut -c1-2`
   set dir_log  = "/sde/$exp/"
   set dir_orig = "/t0/anc/sde/orig/"
   set dir_scav = "/t0/anc/sde/scav/"
   set base_nam =  $exp
endif
#
if ( $exp_type == "vlbi" ) then
   set stn_id   = `echo $exp | cut -c7-8`
   set exp      = `echo $exp | cut -c1-6`
   set dir_log  = "/s0/fs_logs/$exp/"
   set dir_orig = "/t0/anc/vlbi/orig/"
   set dir_scav = "/t0/anc/vlbi/scav/"
   set base_nam = "$exp$stn_id"
endif
#
set path_log  = "$dir_log$fil_log"
#
# -- Check whether the log file exists
#
if ( ! -f $path_log ) then
   echo "log file $path_log was not found"
   exit ( 1 )
endif
#
set fil_anc   = "${dir_orig}${base_nam}_orig.anc"
set fil_ave   = "${dir_scav}${base_nam}_scav.anc"
set fil_bnc   = "${dir_orig}${base_nam}_orig.bnc"
set fil_lg2nt = "${dir_orig}${base_nam}_log2ant.log"
#
# -- generate the anc file
#
echo "The exp is $exp"
echo "The stn_id is $stn_id"
echo " "
if ( ! -f $fil_anc) then
   echo "generating anc file from $path_log"
   echo " "
   log2ant -t dat -t met -t tsys -t sefd -t phc -t fmt -u -o  ${fil_anc} ${path_log} >& ${fil_lg2nt}
#
# -- did the anc file write successfully?
#
   if ( ! -f $fil_anc ) then
      echo "Failed to write anc file to $fil_anc"
      exit ( 1 )
   else 
      echo "Succesfully wrote anc file to $fil_anc"
   endif
else
   echo "The anc file is already at $fil_anc"
endif
#
# -- generate the binary file
#
echo " "
if ( ! -f $fil_bnc ) then
   echo "generating bnc file for $exp_type $exp"
   anc_to_bnc_sim ${fil_anc} ${fil_ave} ${fil_bnc}
#
   if ( ! -f $fil_bnc ) then
      echo "Failed to generate bnc file to $fil_bnc"
      exit ( 1 )
   else
      echo "Succesfully wrote bnc file to $fil_bnc"
   endif
else
   echo "The bnc file is already at $fil_bnc"
   echo "The ave file is already at $fil_ave"
endif
#
