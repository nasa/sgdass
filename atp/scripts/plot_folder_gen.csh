#!/bin/csh -f
umask 0002

# *****************************************************************************
# *									      *
# *   Program plot_folder_gen.csh generates directories and moves plots	      *
# *									      *
# *   N.B: This routine assume the plots have aleardy been created and are    *
# *        currently in the /anc/plots folder				      *
# *									      *
# * ### 06-FEB-2023  plot_folder_gen.csh  v1.2 (c) N. Habana  08-FEB-2024 ### *
# *									      *
# *****************************************************************************
#
if ( $#argv == 0 ) then 
     echo "Usage: plot_folder_gen.csh sde|vlbi year log-file"
     exit 1
else
#
set exp_type  = $1						# Get experiment type
set exp_yr    = $2
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
if ( $HOST == "sa" || $HOST == "gs61a-sagitta.ndc.nasa.gov" || $HOST == "gs61a-sagitta" ) then
# ---
   set dir_anc_orig  = "/t0/anc/sde/orig"
   set dir_plot = "/t0/anc/plots"
   if ( $exp_type == "sde" ) then
      set stn_id   = `echo $exp | cut -c1-2`
      set dir_log  = "/sde/$exp"
      set base_nam = $exp
      set dir_plot_exp = "$dir_plot/$exp"
      set dir_plot_stn = $dir_plot_exp
   endif
# ---
   if ( $exp_type == "vlbi" ) then
      set stn_id   = `echo $exp | cut -c7-8`
      set exp      = `echo $exp | cut -c1-6`
      set dir_log  = "/s0/fs_logs/$exp_yr/$exp"
      set base_nam = "$exp$stn_id"
      set dir_plot_exp = "$dir_plot/$exp"
      set dir_plot_stn = "$dir_plot_exp/$stn_id"
   endif
#
# -- If using crux
#   
else if ( $HOST == "cx" || $HOST == "gs61a-crux.gsfc.nasa.gov" ) then
# ---
   set dir_anc_orig = "/anc/sde/orig"
   set dir_plot = "/anc/plots"
   if ( $exp_type == "sde" ) then
      set stn_id   = `echo $exp | cut -c1-2`
      set dir_log  = "/sde/$exp"
      set base_nam = $exp
      set dir_plot_exp = "$dir_plot/$exp"
      set dir_plot_stn = $dir_plot_exp
# ---
   else if ( $exp_type == "vlbi" ) then
      set stn_id   = `echo $exp | cut -c7-8`
      set exp      = `echo $exp | cut -c1-6`
      set dir_log  = "/q0/fs_logs/$exp_yr/$exp"
      set base_nam = "$exp$stn_id"
      set dir_plot_exp = "$dir_plot/$exp"
      set dir_plot_stn = "$dir_plot_exp/$stn_id"
   endif
endif
set path_log  = "$dir_log$fil_log"
#
# -- Check whether the log file exists
#
if ( ! -f $path_log ) then
   echo "log file $path_log was not found"
   exit ( 1 )
endif
# ---
set fil_bnc   = "${dir_anc_orig}/${base_nam}_orig.bnc"
# ---
if ( ! -f $fil_bnc ) then
   echo "Binary file $fil_bnc was not found. Ergo, unlikely plots where generated (yet)"
   exit ( 1 )
endif
# ---
set dir_ampl      = "${dir_plot_stn}/ampl_phas"
set dir_ampl_time = "${dir_ampl}/time"
set dir_ampl_spec = "${dir_ampl}/spectr"

set dir_phas      = "${dir_plot_stn}/phas"
set dir_phas_time = "${dir_phas}/time"
set dir_phas_spec = "${dir_phas}/spectr"

set dir_tsys      = "${dir_plot_stn}/tsys"
set dir_tsys_time = "${dir_tsys}/time"
set dir_tsys_spec = "${dir_tsys}/spectr"
set dir_tsys_azel = "${dir_tsys}/azel"
#
# -- set all key words specifics
#
set ampl = "*_ampl_*"
set phas = "*_phas_*"
set tsys = "*_tsys_*"
#
# -- change directories to the plots directory
#
cd ${dir_plots}
#
# -- make all the amplitude phase directories and their subdirectories
#
mkdir ${dir_ampl}
mkdir ${dir_ampl_time}
mkdir ${dir_ampl_spec}

mkdir ${dir_phas}
mkdir ${dir_phas_time}
mkdir ${dir_phas_spec}

mkdir ${dir_tsys}
mkdir ${dir_tsys_time}
mkdir ${dir_tsys_spec}
mkdir ${dir_tsys_azel}
#
# -- move all the plots to the station specific directory
#
mv ${dir_plots}/${ampl} ${dir_ampl}


mv    ./*_ampl_* ampl_phas;
mv    ./ampl_phas/*_time_*    ./ampl_phas/time;
mv    ./ampl_phas/*_spectr_*  ./ampl_phas/spectr;

mv    ./*_phas_* phas;
mv    ./phas/*_time_* ./phas/time;
mv    ./phas/*_spectr_* ./phas/spectr;

mv    ./*_tsys_* tsys;
mv    ./tsys/*_time_* ./tsys/time;
mv    ./tsys/*_spectr_* ./tsys/spectr;
mv    ./tsys/*_azim_* ./tsys/azim;
mv    ./tsys/*_elev_* ./tsys/elev;

