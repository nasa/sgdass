#!/bin/csh -f
#
set mode = $1
set movie_dir    = /tmp/4fun
set fs_rate      = 5 # frames per second
#
if ( $mode == 1 ) set pref = st
if ( $mode == 2 ) set pref = dewt
if ( $mode == 3 ) set pref = rh
if ( $mode == 4 ) set pref = acp
#
set output_mpeg4 = /astrogeo.org/misc/terra_$pref.mpeg4
set output_avi   = /astrogeo.org/misc/terra_$pref.avi
#
set last_file = /tmp/4fun/st_20110331_1800.jpg
#goto movie
rm $movie_dir/*
#
$Ex/plot_4fun.e 20110301_0000 $mode 3 /g1/merra_dewt/dewt_20110301_0000.heb
$Ex/plot_4fun.e 20110301_0600 $mode 3 /g1/merra_dewt/dewt_20110301_0600.heb
$Ex/plot_4fun.e 20110301_1200 $mode 3 /g1/merra_dewt/dewt_20110301_1200.heb
$Ex/plot_4fun.e 20110301_1800 $mode 3 /g1/merra_dewt/dewt_20110301_1800.heb
$Ex/plot_4fun.e 20110302_0000 $mode 3 /g1/merra_dewt/dewt_20110302_0000.heb
$Ex/plot_4fun.e 20110302_0600 $mode 3 /g1/merra_dewt/dewt_20110302_0600.heb
$Ex/plot_4fun.e 20110302_1200 $mode 3 /g1/merra_dewt/dewt_20110302_1200.heb
$Ex/plot_4fun.e 20110302_1800 $mode 3 /g1/merra_dewt/dewt_20110302_1800.heb
$Ex/plot_4fun.e 20110303_0000 $mode 3 /g1/merra_dewt/dewt_20110303_0000.heb
$Ex/plot_4fun.e 20110303_0600 $mode 3 /g1/merra_dewt/dewt_20110303_0600.heb
$Ex/plot_4fun.e 20110303_1200 $mode 3 /g1/merra_dewt/dewt_20110303_1200.heb
$Ex/plot_4fun.e 20110303_1800 $mode 3 /g1/merra_dewt/dewt_20110303_1800.heb
$Ex/plot_4fun.e 20110304_0000 $mode 3 /g1/merra_dewt/dewt_20110304_0000.heb
$Ex/plot_4fun.e 20110304_0600 $mode 3 /g1/merra_dewt/dewt_20110304_0600.heb
$Ex/plot_4fun.e 20110304_1200 $mode 3 /g1/merra_dewt/dewt_20110304_1200.heb
$Ex/plot_4fun.e 20110304_1800 $mode 3 /g1/merra_dewt/dewt_20110304_1800.heb
$Ex/plot_4fun.e 20110305_0000 $mode 3 /g1/merra_dewt/dewt_20110305_0000.heb
$Ex/plot_4fun.e 20110305_0600 $mode 3 /g1/merra_dewt/dewt_20110305_0600.heb
$Ex/plot_4fun.e 20110305_1200 $mode 3 /g1/merra_dewt/dewt_20110305_1200.heb
$Ex/plot_4fun.e 20110305_1800 $mode 3 /g1/merra_dewt/dewt_20110305_1800.heb
$Ex/plot_4fun.e 20110306_0000 $mode 3 /g1/merra_dewt/dewt_20110306_0000.heb
$Ex/plot_4fun.e 20110306_0600 $mode 3 /g1/merra_dewt/dewt_20110306_0600.heb
$Ex/plot_4fun.e 20110306_1200 $mode 3 /g1/merra_dewt/dewt_20110306_1200.heb
$Ex/plot_4fun.e 20110306_1800 $mode 3 /g1/merra_dewt/dewt_20110306_1800.heb
$Ex/plot_4fun.e 20110307_0000 $mode 3 /g1/merra_dewt/dewt_20110307_0000.heb
$Ex/plot_4fun.e 20110307_0600 $mode 3 /g1/merra_dewt/dewt_20110307_0600.heb
$Ex/plot_4fun.e 20110307_1200 $mode 3 /g1/merra_dewt/dewt_20110307_1200.heb
$Ex/plot_4fun.e 20110307_1800 $mode 3 /g1/merra_dewt/dewt_20110307_1800.heb
$Ex/plot_4fun.e 20110308_0000 $mode 3 /g1/merra_dewt/dewt_20110308_0000.heb
$Ex/plot_4fun.e 20110308_0600 $mode 3 /g1/merra_dewt/dewt_20110308_0600.heb
$Ex/plot_4fun.e 20110308_1200 $mode 3 /g1/merra_dewt/dewt_20110308_1200.heb
$Ex/plot_4fun.e 20110308_1800 $mode 3 /g1/merra_dewt/dewt_20110308_1800.heb
$Ex/plot_4fun.e 20110309_0000 $mode 3 /g1/merra_dewt/dewt_20110309_0000.heb
$Ex/plot_4fun.e 20110309_0600 $mode 3 /g1/merra_dewt/dewt_20110309_0600.heb
$Ex/plot_4fun.e 20110309_1200 $mode 3 /g1/merra_dewt/dewt_20110309_1200.heb
$Ex/plot_4fun.e 20110309_1800 $mode 3 /g1/merra_dewt/dewt_20110309_1800.heb
$Ex/plot_4fun.e 20110310_0000 $mode 3 /g1/merra_dewt/dewt_20110310_0000.heb
$Ex/plot_4fun.e 20110310_0600 $mode 3 /g1/merra_dewt/dewt_20110310_0600.heb
$Ex/plot_4fun.e 20110310_1200 $mode 3 /g1/merra_dewt/dewt_20110310_1200.heb
$Ex/plot_4fun.e 20110310_1800 $mode 3 /g1/merra_dewt/dewt_20110310_1800.heb
$Ex/plot_4fun.e 20110311_0000 $mode 3 /g1/merra_dewt/dewt_20110311_0000.heb
$Ex/plot_4fun.e 20110311_0600 $mode 3 /g1/merra_dewt/dewt_20110311_0600.heb
$Ex/plot_4fun.e 20110311_1200 $mode 3 /g1/merra_dewt/dewt_20110311_1200.heb
$Ex/plot_4fun.e 20110311_1800 $mode 3 /g1/merra_dewt/dewt_20110311_1800.heb
$Ex/plot_4fun.e 20110312_0000 $mode 3 /g1/merra_dewt/dewt_20110312_0000.heb
$Ex/plot_4fun.e 20110312_0600 $mode 3 /g1/merra_dewt/dewt_20110312_0600.heb
$Ex/plot_4fun.e 20110312_1200 $mode 3 /g1/merra_dewt/dewt_20110312_1200.heb
$Ex/plot_4fun.e 20110312_1800 $mode 3 /g1/merra_dewt/dewt_20110312_1800.heb
$Ex/plot_4fun.e 20110313_0000 $mode 3 /g1/merra_dewt/dewt_20110313_0000.heb
$Ex/plot_4fun.e 20110313_0600 $mode 3 /g1/merra_dewt/dewt_20110313_0600.heb
$Ex/plot_4fun.e 20110313_1200 $mode 3 /g1/merra_dewt/dewt_20110313_1200.heb
$Ex/plot_4fun.e 20110313_1800 $mode 3 /g1/merra_dewt/dewt_20110313_1800.heb
$Ex/plot_4fun.e 20110314_0000 $mode 3 /g1/merra_dewt/dewt_20110314_0000.heb
$Ex/plot_4fun.e 20110314_0600 $mode 3 /g1/merra_dewt/dewt_20110314_0600.heb
$Ex/plot_4fun.e 20110314_1200 $mode 3 /g1/merra_dewt/dewt_20110314_1200.heb
$Ex/plot_4fun.e 20110314_1800 $mode 3 /g1/merra_dewt/dewt_20110314_1800.heb
$Ex/plot_4fun.e 20110315_0000 $mode 3 /g1/merra_dewt/dewt_20110315_0000.heb
$Ex/plot_4fun.e 20110315_0600 $mode 3 /g1/merra_dewt/dewt_20110315_0600.heb
$Ex/plot_4fun.e 20110315_1200 $mode 3 /g1/merra_dewt/dewt_20110315_1200.heb
$Ex/plot_4fun.e 20110315_1800 $mode 3 /g1/merra_dewt/dewt_20110315_1800.heb
$Ex/plot_4fun.e 20110316_0000 $mode 3 /g1/merra_dewt/dewt_20110316_0000.heb
$Ex/plot_4fun.e 20110316_0600 $mode 3 /g1/merra_dewt/dewt_20110316_0600.heb
$Ex/plot_4fun.e 20110316_1200 $mode 3 /g1/merra_dewt/dewt_20110316_1200.heb
$Ex/plot_4fun.e 20110316_1800 $mode 3 /g1/merra_dewt/dewt_20110316_1800.heb
$Ex/plot_4fun.e 20110317_0000 $mode 3 /g1/merra_dewt/dewt_20110317_0000.heb
$Ex/plot_4fun.e 20110317_0600 $mode 3 /g1/merra_dewt/dewt_20110317_0600.heb
$Ex/plot_4fun.e 20110317_1200 $mode 3 /g1/merra_dewt/dewt_20110317_1200.heb
$Ex/plot_4fun.e 20110317_1800 $mode 3 /g1/merra_dewt/dewt_20110317_1800.heb
$Ex/plot_4fun.e 20110318_0000 $mode 3 /g1/merra_dewt/dewt_20110318_0000.heb
$Ex/plot_4fun.e 20110318_0600 $mode 3 /g1/merra_dewt/dewt_20110318_0600.heb
$Ex/plot_4fun.e 20110318_1200 $mode 3 /g1/merra_dewt/dewt_20110318_1200.heb
$Ex/plot_4fun.e 20110318_1800 $mode 3 /g1/merra_dewt/dewt_20110318_1800.heb
$Ex/plot_4fun.e 20110319_0000 $mode 3 /g1/merra_dewt/dewt_20110319_0000.heb
$Ex/plot_4fun.e 20110319_0600 $mode 3 /g1/merra_dewt/dewt_20110319_0600.heb
$Ex/plot_4fun.e 20110319_1200 $mode 3 /g1/merra_dewt/dewt_20110319_1200.heb
$Ex/plot_4fun.e 20110319_1800 $mode 3 /g1/merra_dewt/dewt_20110319_1800.heb
$Ex/plot_4fun.e 20110320_0000 $mode 3 /g1/merra_dewt/dewt_20110320_0000.heb
$Ex/plot_4fun.e 20110320_0600 $mode 3 /g1/merra_dewt/dewt_20110320_0600.heb
$Ex/plot_4fun.e 20110320_1200 $mode 3 /g1/merra_dewt/dewt_20110320_1200.heb
$Ex/plot_4fun.e 20110320_1800 $mode 3 /g1/merra_dewt/dewt_20110320_1800.heb
$Ex/plot_4fun.e 20110321_0000 $mode 3 /g1/merra_dewt/dewt_20110321_0000.heb
$Ex/plot_4fun.e 20110321_0600 $mode 3 /g1/merra_dewt/dewt_20110321_0600.heb
$Ex/plot_4fun.e 20110321_1200 $mode 3 /g1/merra_dewt/dewt_20110321_1200.heb
$Ex/plot_4fun.e 20110321_1800 $mode 3 /g1/merra_dewt/dewt_20110321_1800.heb
$Ex/plot_4fun.e 20110322_0000 $mode 3 /g1/merra_dewt/dewt_20110322_0000.heb
$Ex/plot_4fun.e 20110322_0600 $mode 3 /g1/merra_dewt/dewt_20110322_0600.heb
$Ex/plot_4fun.e 20110322_1200 $mode 3 /g1/merra_dewt/dewt_20110322_1200.heb
$Ex/plot_4fun.e 20110322_1800 $mode 3 /g1/merra_dewt/dewt_20110322_1800.heb
$Ex/plot_4fun.e 20110323_0000 $mode 3 /g1/merra_dewt/dewt_20110323_0000.heb
$Ex/plot_4fun.e 20110323_0600 $mode 3 /g1/merra_dewt/dewt_20110323_0600.heb
$Ex/plot_4fun.e 20110323_1200 $mode 3 /g1/merra_dewt/dewt_20110323_1200.heb
$Ex/plot_4fun.e 20110323_1800 $mode 3 /g1/merra_dewt/dewt_20110323_1800.heb
$Ex/plot_4fun.e 20110324_0000 $mode 3 /g1/merra_dewt/dewt_20110324_0000.heb
$Ex/plot_4fun.e 20110324_0600 $mode 3 /g1/merra_dewt/dewt_20110324_0600.heb
$Ex/plot_4fun.e 20110324_1200 $mode 3 /g1/merra_dewt/dewt_20110324_1200.heb
$Ex/plot_4fun.e 20110324_1800 $mode 3 /g1/merra_dewt/dewt_20110324_1800.heb
$Ex/plot_4fun.e 20110325_0000 $mode 3 /g1/merra_dewt/dewt_20110325_0000.heb
$Ex/plot_4fun.e 20110325_0600 $mode 3 /g1/merra_dewt/dewt_20110325_0600.heb
$Ex/plot_4fun.e 20110325_1200 $mode 3 /g1/merra_dewt/dewt_20110325_1200.heb
$Ex/plot_4fun.e 20110325_1800 $mode 3 /g1/merra_dewt/dewt_20110325_1800.heb
$Ex/plot_4fun.e 20110326_0000 $mode 3 /g1/merra_dewt/dewt_20110326_0000.heb
$Ex/plot_4fun.e 20110326_0600 $mode 3 /g1/merra_dewt/dewt_20110326_0600.heb
$Ex/plot_4fun.e 20110326_1200 $mode 3 /g1/merra_dewt/dewt_20110326_1200.heb
$Ex/plot_4fun.e 20110326_1800 $mode 3 /g1/merra_dewt/dewt_20110326_1800.heb
$Ex/plot_4fun.e 20110327_0000 $mode 3 /g1/merra_dewt/dewt_20110327_0000.heb
$Ex/plot_4fun.e 20110327_0600 $mode 3 /g1/merra_dewt/dewt_20110327_0600.heb
$Ex/plot_4fun.e 20110327_1200 $mode 3 /g1/merra_dewt/dewt_20110327_1200.heb
$Ex/plot_4fun.e 20110327_1800 $mode 3 /g1/merra_dewt/dewt_20110327_1800.heb
$Ex/plot_4fun.e 20110328_0000 $mode 3 /g1/merra_dewt/dewt_20110328_0000.heb
$Ex/plot_4fun.e 20110328_0600 $mode 3 /g1/merra_dewt/dewt_20110328_0600.heb
$Ex/plot_4fun.e 20110328_1200 $mode 3 /g1/merra_dewt/dewt_20110328_1200.heb
$Ex/plot_4fun.e 20110328_1800 $mode 3 /g1/merra_dewt/dewt_20110328_1800.heb
$Ex/plot_4fun.e 20110329_0000 $mode 3 /g1/merra_dewt/dewt_20110329_0000.heb
$Ex/plot_4fun.e 20110329_0600 $mode 3 /g1/merra_dewt/dewt_20110329_0600.heb
$Ex/plot_4fun.e 20110329_1200 $mode 3 /g1/merra_dewt/dewt_20110329_1200.heb
$Ex/plot_4fun.e 20110329_1800 $mode 3 /g1/merra_dewt/dewt_20110329_1800.heb
$Ex/plot_4fun.e 20110330_0000 $mode 3 /g1/merra_dewt/dewt_20110330_0000.heb
$Ex/plot_4fun.e 20110330_0600 $mode 3 /g1/merra_dewt/dewt_20110330_0600.heb
$Ex/plot_4fun.e 20110330_1200 $mode 3 /g1/merra_dewt/dewt_20110330_1200.heb
$Ex/plot_4fun.e 20110330_1800 $mode 3 /g1/merra_dewt/dewt_20110330_1800.heb
$Ex/plot_4fun.e 20110331_0000 $mode 3 /g1/merra_dewt/dewt_20110331_0000.heb
$Ex/plot_4fun.e 20110331_0600 $mode 3 /g1/merra_dewt/dewt_20110331_0600.heb
$Ex/plot_4fun.e 20110331_1200 $mode 3 /g1/merra_dewt/dewt_20110331_1200.heb
$Ex/plot_4fun.e 20110331_1800 $mode 3 /g1/merra_dewt/dewt_20110331_1800.heb
#
movie:
#
# --- Get sorted lest of gif files
#
set list = `find $movie_dir -name "*.gif" | sort`
set ind = 1
cd $movie_dir
#
# --- Convert all gif files with rames from gif to jpeg format
#
echo "Convert from gif to jpeg..."
loop:
     set ind = `expr $ind + 1`
     if ( $ind > $#list ) goto out_of_loop
#
     convert -quality 85 $list[$ind]:r{.gif} $list[$ind]:r{.jpg}
     rm -f $list[$ind]:r{.gif} 
     set last_file = $list[$ind]:r{.jpg} 
goto loop
out_of_loop:
movie:
#
#  Get the height and width of the last frame
#
set width  = `identify -verbose $last_file | grep Geometry | cut -dx -f1 | awk '{printf $2}'`
set height = `identify -verbose $last_file | grep Geometry | cut -dx -f2 | sed "s@+@ @g" | awk '{print $1}'`
echo "width: $width "
echo "height: $height"
onintr finish
#
# --- First, encode teh set of jpeg files to mpeg4 codec
#
set optimal_bitrate = `expr 50 \* 25 \* $width \* $height / 256`
set opt_mpeg4   = "vbitrate=${optimal_bitrate}:mbd=2:keyint=132:v4mv:vqmin=3:lumi_mask=0.07:dark_mask=0.2:scplx_mask=0.1:tcplx_mask=0.1:naq"
set opt_msmpeg4 = "vbitrate=${optimal_bitrate}:mbd=2:keyint=132:vqblur=1.0:cmp=2:subcmp=2:dia=2:mv0:last_pred=3"
if ( -f divx2pass.log ) rm -f divx2pass.log
mencoder "mf://*.jpg" -mf type=jpg:w=${width}:h=${height}:fps=$fs_rate  \
          -ovc lavc -lavcopts vcodec=mpeg4:${opt_mpeg4}:vpass=1 -ffourcc DX50 \
          -ffourcc DX50 -nosound -of avi -ofps $fs_rate -o $output_mpeg4
mencoder "mf://*.jpg" -mf type=jpg:w=${width}:h=${height}:fps=$fs_rate  \
          -ovc lavc -lavcopts vcodec=mpeg4:${opt_mpeg4}:vpass=2 -ffourcc DX50 \
          -ffourcc DX50 -nosound -of avi -ofps $fs_rate -o $output_mpeg4
#
# Then recode the movie from mpeg4 to msmpeg4v2 codec. This is done for
# facilitating compatibility with M$-windows. The codec msmpeg4v2 is
# a built-in codec in the M$-windows mplay
#
# Re-coding is done in two passes. This ensures better image quality and 
# better compression ratio
#
mencoder  -ovc lavc -lavcopts vcodec=msmpeg4v2:${opt_msmpeg4}:vpass=1 \
          -nosound -o $output_avi $output_mpeg4
mencoder  -ovc lavc -lavcopts vcodec=msmpeg4v2:${opt_msmpeg4}:vpass=2 \
          -nosound -o $output_avi $output_mpeg4
rm -f $output_mpeg4 
#
