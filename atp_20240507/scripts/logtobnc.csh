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
     echo "Usage: logtobnc.csh sde|vlbi year log-file"
     exit 1
else
#
set exp_type  = $1						# Get experiment type
set exp_yr    = $2						# Get experiment year
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
set fil_log   = $3
set exp       = $3:t:r
#
# -- Host workstation is crux
#
if ( $HOST == "cx" || $HOST == "gs61a-crux.gsfc.nasa.gov" ) then
# ----
   if ( $exp_type == "sde" ) then
      set stn_id   = `echo $exp | cut -c1-2`
      set dir_log  = "/sde/$exp/"
      set dir_orig = "/anc/sde/orig/"
      set dir_scav = "/anc/sde/scav/"
      set base_nam =  $exp
# ----
   else if ( $exp_type == "vlbi" ) then
      set stn_id   = `echo $exp | cut -c7-8`
      set exp      = `echo $exp | cut -c1-6`
#      set dir_log  = "/q0/fs_logs/$exp/"
      set dir_log  = "/q0/fs_logs/$exp_yr/$exp/"
      set dir_orig = "/anc/vlbi/orig/"
      set dir_scav = "/anc/vlbi/scav/"
      set base_nam = "$exp$stn_id"
   endif
#
# -- Define the fast directory 
#
   set dir_fas = "/f2/anc/temp/" 
#
# -- Define log file path
#
   set path_log  = "$dir_log$fil_log"
#
# -- Check whether the log file exists
#
   if ( ! -f $path_log ) then
      echo "log file $path_log was not found: Check the year"
      exit ( 1 )
   endif
# --
   set fil_anc   = "${dir_orig}${base_nam}_orig.anc"
   set fil_ave   = "${dir_scav}${base_nam}_scav.anc"
   set fil_bnc   = "${dir_orig}${base_nam}_orig.bnc"
   set fil_lg2nt = "${dir_orig}${base_nam}_log2ant.log"
#
# -- fast files
#
   set fil_anc_fas   = "${dir_fas}${base_nam}_orig_temp.anc"
   set fil_ave_fas   = "${dir_fas}${base_nam}_scav_temp.anc"
   set fil_bnc_fas   = "${dir_fas}${base_nam}_orig_temp.bnc"
   set fil_lg2nt_fas = "${dir_fas}${base_nam}_log2ant.log"
#
# -- check if the anc file exists
#
   echo "The exp is $exp"
   echo "The stn_id is $stn_id"
   echo " "
# -------------------------------------------------------------------------------------------------------------------
   if ( ! -f $fil_anc) then

############################################################
### ACCOUNT FOR WHEN FAS DIRECTORY IS NOT ACCESSIBLE #######
############################################################
#
# --- Copy the log file to the fast directory.
#
      cp -p ${path_log} ${dir_fas}
      set path_log_fas = "$dir_fas$fil_log"
#
# --- generate the temporary anc file in the fast directory
#
      set DATE_ISO = `date "+%Y.%m.%d_%H:%M:%S.%N"`
      echo "generating fast anc file from $path_log_fas starting: "
      echo "$DATE_ISO"
      echo " "
      log2ant -t dat -t met -t tsys -t sefd -t phc -t fmt -u -o  ${fil_anc_fas} ${path_log_fas} >& ${fil_lg2nt_fas}
#
# --- Did the anc file write successfully?
#
      if ( ! -f $fil_anc_fas ) then
         echo "Failed to write anc file to $fil_anc_fas"
         exit ( 1 )
      else 
         set DATE_ISO = `date "+%Y.%m.%d_%H:%M:%S.%N"`
         echo "Succesfully wrote anc file to $fil_anc_fas ending: "
         echo "$DATE_ISO"
         echo " "
	 echo "cp $fil_anc_fas to $fil_anc"
	 cp -p ${fil_anc_fas} ${fil_anc}
	 mv ${fil_lg2nt_fas} ${fil_lg2nt}
      endif
   else
      echo "The anc file is already at $fil_anc"
   endif
# ---------------------------------------------------------------------------------------------------------------------
#
# -- Generate the binary file
#
   if ( ! -f $fil_bnc ) then
#
# --- The binary file is not there so we need to generate it.
#
      if ( ! -f $fil_bnc_fas ) then
#
# ------ generate the temporary binary file
#

         set DATE_ISO = `date "+%Y.%m.%d_%H:%M:%S.%N"`
         echo "generating temp bnc file for $exp_type $exp"
	 echo "starting: $DATE_ISO"
	 anc_to_bnc_sim ${fil_anc_fas} ${fil_ave_fas} ${fil_bnc_fas}
#         
# ------ copy the binary file to it's respective regular folder
#
         if ( ! -f $fil_bnc_fas ) then
            echo "Failed to generate bnc file to $fil_bnc"
            exit ( 1 )
         else
            set DATE_ISO = `date "+%Y.%m.%d_%H:%M:%S.%N"`
    	    echo "succeffully wrote bnc file to $fil_bnc_fas on: "
            echo "$DATE_ISO"
# ---------
            cp -p ${fil_bnc_fas} ${fil_bnc}
	    cp -p ${fil_ave_fas} ${fil_ave}
#
# --------- Check for the scan average file
#
	    if ( ! -f $fil_ave_fas ) then
               echo "Failed to generate scav file to $fil_ave_fas"
               exit ( 1 )
	    else
#######################################################################
######								#######
######   USE THIS SPACE TO UPDATE THE SCAV FILE WITH bnc_scav	#######
######								#######
#######################################################################
	    endif
	 endif
      else
#
# ------ just copy the fast binary file to the regular folder
#
         cp -p ${fil_bnc_fas} ${fil_bnc}
      endif
   else
      echo "The bnc file is already at $fil_bnc"
      echo "The ave file is already at $fil_ave"
   endif
#######################################
######   MAKE PLOTS... MAYBE	#######
#######################################
#
# -- Host workstation is sagitta
#
else if ( $HOST == "sa" || $HOST == "gs61a-sagitta.ndc.nasa.gov" || $HOST == "gs61a-sagitta" ) then
# ----
   if ( $exp_type == "sde" ) then
      set stn_id   = `echo $exp | cut -c1-2`
      set dir_log  = "/sde/$exp/"
      set dir_orig = "/t0/anc/sde/orig/"
      set dir_scav = "/t0/anc/sde/scav/"
      set base_nam =  $exp
# ----
   else if ( $exp_type == "vlbi" ) then
      set stn_id   = `echo $exp | cut -c7-8`
      set exp      = `echo $exp | cut -c1-6`
#      set dir_log  = "/s0/fs_logs/$exp/"
      set dir_log  = "/s0/fs_logs/$exp_yr/$exp"
      set dir_orig = "/t0/anc/vlbi/orig/"
      set dir_scav = "/t0/anc/vlbi/scav/"
      set base_nam = "$exp$stn_id"
   endif
# --
   set path_log  = "$dir_log$fil_log"
#
# -- Check whether the log file exists
#
   if ( ! -f $path_log ) then
      echo "log file $path_log was not found: check the year"
      exit ( 1 )
   endif
# --
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
# --
   if ( ! -f $fil_anc) then
      set DATE_ISO = `date "+%Y.%m.%d_%H:%M:%S.%N"`
      echo "generating anc file from $path_log starting: "
      echo "$DATE_ISO"
      echo " "
      log2ant -t dat -t met -t tsys -t sefd -t phc -t fmt -u -o  ${fil_anc} ${path_log} >& ${fil_lg2nt}
#
# --- did the anc file write successfully?
#
      if ( ! -f $fil_anc ) then
         echo "Failed to write anc file to $fil_anc"
         exit ( 1 )
      else 
         set DATE_ISO = `date "+%Y.%m.%d_%H:%M:%S.%N"`
         echo "Succesfully wrote anc file to $fil_anc ending: "
         echo "$DATE_ISO"
         echo " "
      endif
   else
      echo "The anc file is already at $fil_anc"
   endif
#
# --- generate the binary file
#
   echo " "
   if ( ! -f $fil_bnc ) then
      echo "generating bnc file for $exp_type $exp"
      anc_to_bnc_sim ${fil_anc} ${fil_ave} ${fil_bnc}
# ---
      if ( ! -f $fil_bnc ) then
         echo "Failed to generate bnc file to $fil_bnc"
         exit ( 1 )
      else
         echo "Succesfully wrote bnc file to $fil_bnc"
#######################################################################
######								#######
######   USE THIS SPACE TO UPDATE THE SCAV FILE WITH bnc_scav	#######
######								#######
#######################################################################
      endif
   else
      echo "The bnc file is already at $fil_bnc"
      echo "The ave file is already at $fil_ave"
   endif
#######################################
######   MAKE PLOTS... MAYBE	#######
#######################################
endif
#
