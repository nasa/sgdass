#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Prrogram for downloading and transforming MERRA Nv data into       *
# *   heb format for the specified year.
# *                                                                      *
# * ### 11-FEB-2013 get_merranv_year  v2.0 (c) L. Petrov 12-APR-2016 ### *
# *                                                                      *
# ************************************************************************
set url_prefix   = ftp://goldsmr5.sci.gsfc.nasa.gov/data/opendap/MERRA2/M2I6NVANA.5.12.4
set donwload_dir = /s1/imsl/oper_temp
set donwload_dir = /s1/imsl/temp_merra2
set merra_heb    = /s1/imsl/heb/merra2
set num_streams  = 4
#
if ( `hostname` == "terra" ) then
      set num_streams = 2
endif
#
if ( $1 == "" ) then
    echo "Usage get_merra_year year"
    exit 1
endif
if ( ${?MALO_DIR} == "0" ) then 
    echo "--- get_merranv_year: environment variable MALO_DIR was not defined ---"
    stop
endif
set install_dir  = `dirname $0`
set year = $1
#
# --- Create the output directories if they do not exist
#
if ( -d $merra_heb/$year   == 0 ) mkdir $merra_heb/$year 
if ( -d $merra_heb/$year/d == 0 ) mkdir $merra_heb/$year/d
if ( -d $merra_heb/$year/q == 0 ) mkdir $merra_heb/$year/q
if ( -d $merra_heb/$year/t == 0 ) mkdir $merra_heb/$year/t
if ( -d $merra_heb/$year/u == 0 ) mkdir $merra_heb/$year/u
if ( -d $merra_heb/$year/v == 0 ) mkdir $merra_heb/$year/v
cd  $donwload_dir 
#
if ( $year == "2016" ) then
     set months = `seq 10 -1 1`
  else
     set months = `seq 1 1 12`
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
    $install_dir/get_merra_month.csh $url_prefix $year $month $donwload_dir $num_streams
    set download_status = $status
    if ( $download_status != 0 ) then
         echo "Failure in donwloading data from $url_prefix"
         exit $download_status 
    endif
    echo "Check downloaded data"
#    
    set num_files_requested  = `$install_dir/get_ftp_listing.csh  $url_prefix/$year/$month - | grep -v map | grep -v xml | wc -l`
    set num_files_downloaded = `ls -c1 $donwload_dir/ | wc -l`
    if ( $num_files_downloaded < $num_files_requested ) then
#
# ------ The number of downloaded files is not the same as requested
#
         set DATE_ISO = `date "+%Y.%m.%d_%H:%M:%S"`
         echo "The number of requested  files: $num_files_requested"
         echo "The number of downloaded files: $num_files_downloaded"
         echo "Resume downloading the data   $year/$month on $DATE_ISO"
#
# ------ Download the data. This time we use only one stream
#
         $install_dir/get_merra_month.csh $url_prefix $year $month $donwload_dir 1
         set download_status = $status
         if ( $download_status != 0 ) then
              echo "Failure in donwloading data from $url_prefix/$year/$month status: $download_status"
              exit $download_status 
         endif
#
# ------ Check the second time whether all the data have been downloaded.
#
         echo "Check downloaded data again"
         set num_files_downloaded = `ls -c1 $donwload_dir/ | wc -l`
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
# --- Now processes the data and transform them into heb-format
#
    set file_list = `find $donwload_dir/ -name "*.hdf" | sort`
    if ( $#file_list == 0 ) then
         set file_list = `find $donwload_dir/ -name "*.nc4" | sort`
    endif
    echo $file_list | sed "s@ @\n@g" | parallel $MALO_DIR/script/merranv_to_dqtuv.csh {} $merra_heb
    set DATE_ISO = `date "+%Y.%m.%d_%H:%M:%S"`
    echo "Finished processing the data   $year/$month on $DATE_ISO"
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
find $merra_heb/$year/ -name "*.heb" | parallel lbzip2 -n1 {} >& /tmp/heb__compress__$$ &
set DATE_ISO = `date "+%Y.%m.%d_%H:%M:%S"`
echo "Finished data compressing for  $year    on $DATE_ISO"
