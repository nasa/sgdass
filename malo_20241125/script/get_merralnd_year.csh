#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Prrogram for downloading and transforming MERRA LND data into      *
# *   heb format for the specified year.                                 *
# *                                                                      *
# * ### 12-FEB-2013 get_merralnd_year v1.1 (c) L. Petrov 10-JAN-2016 ### *
# *                                                                      *
# ************************************************************************
#
#set url_prefix   = ftp://goldsmr2.sci.gsfc.nasa.gov/data/opendap/MERRA/MAT1NXLND.5.2.0
set url_prefix   = ftp://goldsmr4.sci.gsfc.nasa.gov/data/opendap/MERRA2/M2T1NXLND.5.12.4
set num_streams  = 1
set merra_keep_flag = "no"
set num_proc = 8
#
set merra_keep_dir = /g4/merra_lnd
set donwload_dir   = /s1/imls/temp_merra_lnd
set merra_dir      = /s1/imls/heb/merra2/ 
if ( `hostname` == "terra" ) then
      set num_streams     = 1
      set merra_keep_flag = "yes"
      set merra_keep_dir  = /d1/merra_flx
      set donwload_dir    = /d1/merra_flx_temp
      set merra_dir  Finished processing the data     = /d1/merra_heb
endif
#
if ( $1 == "" ) then
    echo "Usage get_merra_year.csh year"
    exit 1
endif
set year = $1
#
if ( ${?MALO_DIR} == "0" ) then 
    echo "--- get_merralnd_year: environment variable MALO_DIR was not defined ---"
    stop
endif
if ( $merra_keep_flag == "yes" ) then
     if ( -d $merra_keep_dir == 0 ) then
          echo "--- get_merralnd_year: directory merra_keep_dir $merra_keep_dir is not found"
          stop
     endif
     if ( -d $merra_keep_dir/$year == 0 ) then
          mkdir $merra_keep_dir/$year 
     endif
endif
set install_dir  = `dirname $0`
if ( $install_dir == "." ) set install_dir = `pwd`
#
# --- Create the output directories if they do not exist
#
if ( -d $merra_dir/$year        == 0 ) mkdir $merra_dir/$year 
if ( -d $merra_dir/$year/twland == 0 ) mkdir $merra_dir/$year/twland
cd  $donwload_dir 
if ( $2 == "compress" ) goto out_of_loop
#
if ( $year == "2016" ) then
     set months = `seq 10 -1 1`
  else
     set months = `seq 1 1 12`
#     set months = `seq 3 1 3`
endif
foreach month ($months)
#
# --- Cycle over months of the year
#
# --- Remove old files
#
    rm *.hdf *.nc4 >& /dev/null
#
    if ( `expr length $month` == 1 ) then
         set month = "0"$month
    endif
    set DATE_ISO = `date "+%Y.%m.%d_%H:%M:%S"`
    echo "Started downloading the data   $year/$month on $DATE_ISO"
#
# --- Launch downloading the data for this month
#
    $install_dir/get_merra_lnd_month.csh $url_prefix $year $month $donwload_dir $num_streams
    set download_status = $status
    if ( $download_status != 0 ) then
         echo "Failure in donwloading data from $url_prefix"
         exit $download_status 
    endif
    echo "Check downloaded data"
#    
    set num_files_requested  = `$install_dir/get_ftp_listing.csh  $url_prefix/$year/$month - | grep -v map | grep -v xml | wc -l`
    set num_files_downloaded = `ls -c1 | wc -l`
    if ( $num_files_downloaded < $num_files_requested ) then
#
# ------ The number of downloaded files is not the same as requested
#
         echo "The number of requested  files: $num_files_requested"
         echo "The number of downloaded files: $num_files_downloaded"
         echo "Resume downloading the data   $year/$month on $DATE_ISO"
#
# ------ Download the data. This time we use only one stream
#
         $install_dir/get_merra_lnd_month.csh $url_prefix $year $month $donwload_dir 1
         set download_status = $status
         if ( $download_status != 0 ) then
              echo "Failure in donwloading data from $url_prefix"
              exit $download_status 
         endif
#
# ------ Check the second time whether all the data have been downloaded.
#
         echo "Check downloaded data again"
         set num_files_downloaded = `ls -c1 | wc -l`
         if ( $num_files_downloaded < $num_files_requested ) then
#
# ----------- What?? Not all the data? I give up
#
              echo "The second time the number of downloaded files is less than the number of requested files"
              echo "The number of requested  files: $num_files_requested"
              echo "The number of downloaded files: $num_files_downloaded"
              echo "Giving up"
	      exit 1
         endif
    endif
#
    set DATE_ISO = `date "+%Y.%m.%d_%H:%M:%S"`
    echo "Started processing the data    $year/$month on $DATE_ISO"
#
# --- Now process the data and transform them into heb-format
#
    find $donwload_dir/ -name "*.nc4" | sort | \
         parallel $MALO_DIR/script/merralnd_to_heb.csh {} $merra_dir
    set DATE_ISO = `date "+%Y.%m.%d_%H:%M:%S"`
    echo "Finished processing the data   $year/$month on $DATE_ISO"
    if ( $merra_keep_flag == "yes" ) then
         echo "Moving orignal MERRA data to $merra_keep_dir/$year"
         mv $donwload_dir/*.hdf $merra_keep_dir/$year/twland
    endif
#    goto out_of_loop
end
out_of_loop:
#
set DATE_ISO = `date "+%Y.%m.%d_%H:%M:%S"`
echo "Finished processing the data   $year    on $DATE_ISO"
#
set DATE_ISO = `date "+%Y.%m.%d_%H:%M:%S"`
echo "Launched data compressing for  $year    on $DATE_ISO"
#
# --- Launch compressing the data as a detached process
#
find $merra_dir/$year/twland -name '*.heb' | parallel pbzip2 -p$num_proc -r -m1024 -S4096 -f -p1 {} >& /tmp/heb__compress__$$ &
