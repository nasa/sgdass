#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Program get_merra_month.csh downloads one month of MERRA data      *
# *   from the specifued url porefix for the specified year, specified   *
# *   month in a specified directory. Downloading is done in N streams.  *
# *                                                                      *
# *   Usage: get_merra_month.csh url_prefix year month output_dir num_streams *
# *                                                                      *
# *   where                                                              *
# *         url_prefix  is the part of the URL which ends to the product * 
# *                     name.                                            *
# *         year        -- year to download (1979+).                     *
# *         month       -- month do download in a range [1,12].          *
# *         output_dir  -- directory where to download the data.         *
# *         num_streams -- the number of streams for simultaneous        *
# *                        downloading.                                  *
# *                                                                      *
# * ## 12-FEB-2013 get_merra_month.csh v2.0 (c) L. Petrov 12-APR-2016 ## *
# *                                                                      *
# ************************************************************************
# set url_prefix = ftp://goldsmr5.sci.gsfc.nasa.gov/data/opendap/MERRA2/M2I6NVANA.5.12.4
#
set wget_opts   = "-c -nH --cut-dirs=8 -q -t 256 --timeout=30 --waitretry=8 --retry-connrefused"
set install_dir = `dirname $0`
#
if ($#argv != 5) then 
    echo "Usage: get_merra_month.csh url_prefix year month output_dir num_streams"
    exit 1
endif
set url_prefix = $1
set year  = $2
set month = $3
set output_dir = $4
if ( `expr length $month` == 1 ) then
      set month = "0"$month
endif
set num_streams = $5
if ( -d $output_dir == 0 ) then
     mkdir $output_dir 
endif
#
set listing_file = /tmp/merra__listing__$$
set url_file = /tmp/merra__url__$$
#
# --- Retreive ftp-listing
#
$install_dir/get_ftp_listing.csh \
   ${url_prefix}/${year}/${month} \
   $listing_file
set wget_status = $status
if ( $wget_status != 0 ) then
     if ( -f $listing_file ) rm $listing_file 
     exit $wget_status
endif
#
# --- Create the URL file from the ftp-listing
#
cat $listing_file | grep -v map | grep -v xml | \
    awk '{ printf ( "PODSTANOVKA/%s\n", $1 )}' | \
    sed -e "s@@@g" -e "s@PODSTANOVKA@$url_prefix/$year/$month/@g" \
    > $url_file    
rm $listing_file
cd $output_dir
#
# --- run wget quietly in parallel
#
cat $url_file | parallel -j $num_streams wget $wget_opts {}
rm $url_file
exit 0
