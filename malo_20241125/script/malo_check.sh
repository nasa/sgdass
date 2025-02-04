#!/bin/bash
# ************************************************************************
# *                                                                      *
# *   Bash-shell program malo_check.sh checks the first and last         *
# *   dates of files with time series related to mass loading.           *
# *                                                                      *
# * ### 21-MAR-2013  malo_check.sh v3.8  (c) L. Petrov  30-JAN-2024 ###  *
# *                                                                      *
# ************************************************************************
hostname=`uname -n` 
if [ $hostname == "astrogeo" ]; then
      export BIN_DIR=/opt64/bin
      export LIB_DIR=/opt64/lib
      host_prefix="astrogeo"
      imls_dir=/massloading.net
      ners_dir=/earthrotation.net
      malo_query=/astrogeo.org/web_exec/malo_query.py
elif [ `uname -n` == "earthrotation" ] ; then
      export BIN_DIR=/opt64/bin
      export LIB_DIR=/opt64/lib
      host_prefix="astrogeo"
      imls_dir=/massloading.net
      ners_dir=/earthrotation.net
      malo_query=/astrogeo.org/web_exec/malo_query.py
elif [ `uname -n` == "gs61a-geodev-a" ] ; then
      export BIN_DIR=/opt64/bin
      export LIB_DIR=/opt64/lib
      host_prefix="deva"
      export MALO_ROOT=`$BIN_DIR/malo_inq root`
      imls_dir=/massloading
      ners_dir=/earthrotation
      malo_query=${MALO_ROOT}/web_script/malo_query.py
elif [ `uname -n` == "geopod" ] ; then
      export BIN_DIR=/Users/lpetrov/opt/bin
      export LIB_DIR=/Users/lpetrov/opt/lib
      host_prefix="geopod"
      export MALO_ROOT=`$BIN_DIR/malo_inq root`
      imls_dir=/imls/massloading.net
      ners_dir=/imls/earthrotation.net
      malo_query=${MALO_ROOT}/web_script/malo_query.py
else
      echo "malo_check.sh: Unknown host "`uname -n`
fi
export GOMP_STACKSIZE=2000000
export MALO_SHARE=`$BIN_DIR/malo_inq  share`
export MALO_SCRIPT=`$BIN_DIR/malo_inq script`
export MALO_DIR=`$BIN_DIR/malo_inq root`
export MALO_BIN=`$BIN_DIR/malo_inq bin_static`
export SPD_SHARE="/opt64/spd/share"
#
export PGPLOT_FONT=$BIN_DIR/grfont.dat
export PGPLOT_XW_MARGIN=1.0
#
export imls_plot=${imls_dir}/plots
export ners_fcs=${ners_dir}/ners/eop.ners
export ners_ser=${ners_dir}/ners/ners_series.txt
export ners_plot=${ners_dir}/ners/plots
export tmp_ser=${ners_dir}/ners/ners_ser__$$
export ners_stat_conf=$MALO_SHARE/${host_prefix}_eop_stat.conf
if [ -f $MALO_SHARE/${host_prefix}_stat_all.conf ]; then
     export imls_stat_conf=$MALO_SHARE/${host_prefix}_stat_all.conf
   else
     export imls_stat_conf=$MALO_SHARE/${host_prefix}_stat.conf
fi
export ners_stat_file=`grep lat_file $ners_stat_conf | awk '{print $2}'`
export imls_stat_file=`grep lat_file $imls_stat_conf | awk '{print $2}'`
#
pid_ond=`ps -eaf | grep malo_ondemand | grep -v smce | grep -v grep`
#
if [ $hostname == "earthrotation" ] ; then
      ond_host_prefix=$hostname 
   else      
      ond_host_prefix=$host_prefix
fi
#
if [ "$pid_ond" == "" ]; then
     DATE_ISO=`date "+%Y.%m.%d_%H:%M:%S.%N"`
     echo "Launched malo_ondemand on $DATE_ISO" >> /imls/ondemand/launch.log
     if [ $hostname == "astrogeo" ] || [ $hostname == "earthrotation" ] ; then
          $MALO_BIN/malo_ondemand $MALO_SHARE/${ond_host_prefix}_ond.conf 3 2>&1   >> /logs/imls_ond_launch.log       &
     elif [ $hostname == "gs61a-geodev-a" ]; then
          $MALO_BIN/malo_ondemand $MALO_SHARE/${ond_host_prefix}_ond.conf 3 2>&1   >> /logs/imls_ond_local_launch.log &
     elif [ $hostname == "gs698-geopod.gsfc.nasa.gov" ]; then
           echo "malo_ondemand will not be launched" >> /dev/null
     else
           echo "malo_check.sh: Unknown host $hostname"
	   exit 1
     fi
     if [ "$1" == "ond" ]; then
          exit 0
     fi
fi
#
pid_smce_ond=`ps -eaf | grep malo_ondemand | grep smce | grep -v grep`
if [ "$pid_smce_ond" == "" ]; then
     DATE_ISO=`date "+%Y.%m.%d_%H:%M:%S.%N"`
     if [ $hostname == "gs61a-geodev-a" ]; then
          echo "Launched malo_ondemand on $DATE_ISO" >> /imls/ondemand/launch_smce.log
          $MALO_BIN/malo_ondemand $MALO_SHARE/${ond_host_prefix}_smce_ond.conf 3 2>&1  >> /logs/imls_ond_smce_lanuch.log &
     fi
fi
#
if [ $hostname == "${host_prefix}" ] || [ $hostname == "earthrotation" ] || [ $hostname == "gs61a-geodev-a" ]; then
     cp /imls/logs/${host_prefix}.lat /imls/logs/${host_prefix}_mini.lat
     if [ -f $MALO_SHARE/${host_prefix}_stat_all.conf ]; then
          python3 $MALO_SCRIPT/malo_stat.py -c $MALO_SHARE/${host_prefix}_stat_all.conf -v 1
     fi
     python3 $MALO_SCRIPT/malo_stat.py -c $MALO_SHARE/${host_prefix}_stat.conf -v 1
else
     python3 $MALO_SCRIPT/malo_stat.py -c $MALO_SHARE/geopod_stat.conf -v 1
fi
if [ $hostname == "gs61a-geodev-a" ]; then
     if [ "`ps -eaf | grep bfi_server.py | grep -v grep`" == "" ]; then
          /usr/bin/python3 /auto/bfi_server.py 2>&1  > /dev/null &
     fi
     if [ "`ps -eaf | grep sshfs | grep /ex/ | grep -v grep`" == "" ]; then
          sshfs -o ServerAliveInterval=30,reconnect,cache=no,no_readahead,follow_symlinks aw:/ex/ /ex/
     fi
     if [ "`ps -eaf | grep sshfs | grep /s3/ | grep -v grep`" == "" ]; then
          sshfs -o ServerAliveInterval=30,reconnect,cache=no,no_readahead,follow_symlinks aw:/s3/ /s3/
     fi
fi
#
$MALO_BIN/malo_latency_plot $imls_stat_conf $imls_stat_file $imls_plot # >& /imls/logs/imls_latency_plots.log
if [ -f $MALO_SHARE/${host_prefix}_eop.conf ]; then
      $MALO_BIN/malo_latency_plot $ners_stat_conf $ners_stat_file $ners_plot # >& /imls/logs/ners_latency_plots.log
      python3 $MALO_SCRIPT/get_eop.py -c $MALO_SHARE/${host_prefix}_eop.conf -l
      if [ $status == ]; then
           cat $ners_ser > $tmp_ser
           $MALO_BIN/show_eop_fcs $ners_fcs now ser >> $tmp_ser
           mv  $tmp_ser $ners_ser
      fi
fi
#################
#
###  ATM GEOSFPIT 
#
#################
$malo_query -s atm -m GEOSFPIT -a get_loading_first_date >\
             ${imls_dir}/atm/dates/geosfpit_loading_first_date.txt__$$
mv           ${imls_dir}/atm/dates/geosfpit_loading_first_date.txt__$$ \
             ${imls_dir}/atm/dates/geosfpit_loading_first_date.txt
#
$malo_query -s atm -m GEOSFPIT -a get_loading_last_date >\
             ${imls_dir}/atm/dates/geosfpit_loading_last_date.txt__$$
mv           ${imls_dir}/atm/dates/geosfpit_loading_last_date.txt__$$ \
             ${imls_dir}/atm/dates/geosfpit_loading_last_date.txt
#
$malo_query -s atm -m GEOSFPIT -a get_loading_last_update >\
             ${imls_dir}/atm/dates/geosfpit_loading_last_update.txt__$$
mv           ${imls_dir}/atm/dates/geosfpit_loading_last_update.txt__$$ \
             ${imls_dir}/atm/dates/geosfpit_loading_last_update.txt
#
$malo_query -s atm -m GEOSFPIT -a get_vgep_first_date >\
             ${imls_dir}/atm/dates/geosfpit_vgep_first_date.txt__$$
mv           ${imls_dir}/atm/dates/geosfpit_vgep_first_date.txt__$$ \
             ${imls_dir}/atm/dates/geosfpit_vgep_first_date.txt
#
$malo_query -s atm -m GEOSFPIT -a get_vgep_last_date >\
             ${imls_dir}/atm/dates/geosfpit_vgep_last_date.txt__$$
mv           ${imls_dir}/atm/dates/geosfpit_vgep_last_date.txt__$$ \
             ${imls_dir}/atm/dates/geosfpit_vgep_last_date.txt
#
$malo_query -s atm -m GEOSFPIT -a get_vgep_last_update >\
             ${imls_dir}/atm/dates/geosfpit_vgep_last_update.txt__$$
mv           ${imls_dir}/atm/dates/geosfpit_vgep_last_update.txt__$$ \
             ${imls_dir}/atm/dates/geosfpit_vgep_last_update.txt
#################
#
###  ATM MERRA2 
#
#################
$malo_query -s atm -m MERRA2 -a get_loading_first_date >\
             ${imls_dir}/atm/dates/merra2_loading_first_date.txt__$$
mv           ${imls_dir}/atm/dates/merra2_loading_first_date.txt__$$ \
             ${imls_dir}/atm/dates/merra2_loading_first_date.txt
#
$malo_query -s atm -m MERRA2 -a get_loading_last_date >\
             ${imls_dir}/atm/dates/merra2_loading_last_date.txt__$$
mv           ${imls_dir}/atm/dates/merra2_loading_last_date.txt__$$ \
             ${imls_dir}/atm/dates/merra2_loading_last_date.txt
#
$malo_query -s atm -m MERRA2 -a get_loading_last_update >\
             ${imls_dir}/atm/dates/merra2_loading_last_update.txt__$$
mv           ${imls_dir}/atm/dates/merra2_loading_last_update.txt__$$ \
             ${imls_dir}/atm/dates/merra2_loading_last_update.txt
#
$malo_query -s atm -m MERRA2 -a get_vgep_first_date >\
             ${imls_dir}/atm/dates/merra2_vgep_first_date.txt__$$
mv           ${imls_dir}/atm/dates/merra2_vgep_first_date.txt__$$ \
             ${imls_dir}/atm/dates/merra2_vgep_first_date.txt
#
$malo_query -s atm -m MERRA2 -a get_vgep_last_date >\
             ${imls_dir}/atm/dates/merra2_vgep_last_date.txt__$$
mv           ${imls_dir}/atm/dates/merra2_vgep_last_date.txt__$$ \
             ${imls_dir}/atm/dates/merra2_vgep_last_date.txt
#
$malo_query -s atm -m MERRA2 -a get_vgep_last_update >\
             ${imls_dir}/atm/dates/merra2_vgep_last_update.txt__$$
mv           ${imls_dir}/atm/dates/merra2_vgep_last_update.txt__$$ \
             ${imls_dir}/atm/dates/merra2_vgep_last_update.txt
#################
#
###  LWS MERRA2 
#
#################
$malo_query -s lws -m MERRA2 -a get_loading_first_date >\
             ${imls_dir}/lws/dates/merra2_loading_first_date.txt__$$
mv           ${imls_dir}/lws/dates/merra2_loading_first_date.txt__$$ \
             ${imls_dir}/lws/dates/merra2_loading_first_date.txt
#
$malo_query -s lws -m MERRA2 -a get_loading_last_date >\
             ${imls_dir}/lws/dates/merra2_loading_last_date.txt__$$
mv           ${imls_dir}/lws/dates/merra2_loading_last_date.txt__$$ \
             ${imls_dir}/lws/dates/merra2_loading_last_date.txt
#
$malo_query -s lws -m MERRA2 -a get_loading_last_update >\
             ${imls_dir}/lws/dates/merra2_loading_last_update.txt__$$
mv           ${imls_dir}/lws/dates/merra2_loading_last_update.txt__$$ \
             ${imls_dir}/lws/dates/merra2_loading_last_update.txt
#
$malo_query -s lws -m MERRA2 -a get_vgep_first_date >\
             ${imls_dir}/lws/dates/merra2_vgep_first_date.txt__$$
mv           ${imls_dir}/lws/dates/merra2_vgep_first_date.txt__$$ \
             ${imls_dir}/lws/dates/merra2_vgep_first_date.txt
#
$malo_query -s lws -m MERRA2 -a get_vgep_last_date >\
             ${imls_dir}/lws/dates/merra2_vgep_last_date.txt__$$
mv           ${imls_dir}/lws/dates/merra2_vgep_last_date.txt__$$ \
             ${imls_dir}/lws/dates/merra2_vgep_last_date.txt
#
$malo_query -s lws -m MERRA2 -a get_vgep_last_update >\
             ${imls_dir}/lws/dates/merra2_vgep_last_update.txt__$$
mv           ${imls_dir}/lws/dates/merra2_vgep_last_update.txt__$$ \
             ${imls_dir}/lws/dates/merra2_vgep_last_update.txt
#################
#
###  LWS GEOSFPIT 
#
#################
$malo_query -s lws -m GEOSFPIT -a get_loading_first_date >\
             ${imls_dir}/lws/dates/geosfpit_loading_first_date.txt__$$
mv           ${imls_dir}/lws/dates/geosfpit_loading_first_date.txt__$$ \
             ${imls_dir}/lws/dates/geosfpit_loading_first_date.txt
#
$malo_query -s lws -m GEOSFPIT -a get_loading_last_date >\
             ${imls_dir}/lws/dates/geosfpit_loading_last_date.txt__$$
mv           ${imls_dir}/lws/dates/geosfpit_loading_last_date.txt__$$ \
             ${imls_dir}/lws/dates/geosfpit_loading_last_date.txt
#
$malo_query -s lws -m GEOSFPIT -a get_loading_last_update >\
             ${imls_dir}/lws/dates/geosfpit_loading_last_update.txt__$$
mv           ${imls_dir}/lws/dates/geosfpit_loading_last_update.txt__$$ \
             ${imls_dir}/lws/dates/geosfpit_loading_last_update.txt
#
$malo_query -s lws -m GEOSFPIT -a get_vgep_first_date >\
             ${imls_dir}/lws/dates/geosfpit_vgep_first_date.txt__$$
mv           ${imls_dir}/lws/dates/geosfpit_vgep_first_date.txt__$$ \
             ${imls_dir}/lws/dates/geosfpit_vgep_first_date.txt
#
$malo_query -s lws -m GEOSFPIT -a get_vgep_last_date >\
             ${imls_dir}/lws/dates/geosfpit_vgep_last_date.txt__$$
mv           ${imls_dir}/lws/dates/geosfpit_vgep_last_date.txt__$$ \
             ${imls_dir}/lws/dates/geosfpit_vgep_last_date.txt
#
$malo_query -s lws -m GEOSFPIT -a get_vgep_last_update >\
             ${imls_dir}/lws/dates/geosfpit_vgep_last_update.txt__$$
mv           ${imls_dir}/lws/dates/geosfpit_vgep_last_update.txt__$$ \
             ${imls_dir}/lws/dates/geosfpit_vgep_last_update.txt
############
#
# NTO OMCT05
#
############
$malo_query -s nto -m OMCT05 -a get_loading_first_date >\
             ${imls_dir}/nto/dates/omct05_loading_first_date.txt__$$
mv           ${imls_dir}/nto/dates/omct05_loading_first_date.txt__$$ \
             ${imls_dir}/nto/dates/omct05_loading_first_date.txt
#
$malo_query -s nto -m OMCT05 --a get_loading_last_date >\
             ${imls_dir}/nto/dates/omct05_loading_last_date.txt__$$
mv           ${imls_dir}/nto/dates/omct05_loading_last_date.txt__$$ \
             ${imls_dir}/nto/dates/omct05_loading_last_date.txt
#
$malo_query -s nto -m OMCT05 --a get_loading_last_update >\
             ${imls_dir}/nto/dates/omct05_loading_last_update.txt__$$
mv           ${imls_dir}/nto/dates/omct05_loading_last_update.txt__$$ \
             ${imls_dir}/nto/dates/omct05_loading_last_update.txt
#
$malo_query -s nto -m OMCT05 --a get_vgep_first_date >\
             ${imls_dir}/nto/dates/omct05_vgep_first_date.txt__$$
mv           ${imls_dir}/nto/dates/omct05_vgep_first_date.txt__$$ \
             ${imls_dir}/nto/dates/omct05_vgep_first_date.txt
#
$malo_query -s nto -m OMCT05 --a get_vgep_last_date >\
             ${imls_dir}/nto/dates/omct05_vgep_last_date.txt__$$
mv           ${imls_dir}/nto/dates/omct05_vgep_last_date.txt__$$ \
             ${imls_dir}/nto/dates/omct05_vgep_last_date.txt
#
$malo_query -s nto -m OMCT05 --a get_vgep_last_update >\
             ${imls_dir}/nto/dates/omct05_vgep_last_update.txt__$$
mv           ${imls_dir}/nto/dates/omct05_vgep_last_update.txt__$$ \
             ${imls_dir}/nto/dates/omct05_vgep_last_update.txt
############
#
# NTO MPIOM06
#
############
$malo_query -s nto -m MPIOM06 -a get_loading_first_date >\
             ${imls_dir}/nto/dates/mpiom06_loading_first_date.txt__$$
mv           ${imls_dir}/nto/dates/mpiom06_loading_first_date.txt__$$ \
             ${imls_dir}/nto/dates/mpiom06_loading_first_date.txt
#
$malo_query -s nto -m MPIOM06 --a get_loading_last_date >\
             ${imls_dir}/nto/dates/mpiom06_loading_last_date.txt__$$
mv           ${imls_dir}/nto/dates/mpiom06_loading_last_date.txt__$$ \
             ${imls_dir}/nto/dates/mpiom06_loading_last_date.txt
#
$malo_query -s nto -m MPIOM06 --a get_loading_last_update >\
             ${imls_dir}/nto/dates/mpiom06_loading_last_update.txt__$$
mv           ${imls_dir}/nto/dates/mpiom06_loading_last_update.txt__$$ \
             ${imls_dir}/nto/dates/mpiom06_loading_last_update.txt
#
$malo_query -s nto -m MPIOM06 --a get_vgep_first_date >\
             ${imls_dir}/nto/dates/mpiom06_vgep_first_date.txt__$$
mv           ${imls_dir}/nto/dates/mpiom06_vgep_first_date.txt__$$ \
             ${imls_dir}/nto/dates/mpiom06_vgep_first_date.txt
#
$malo_query -s nto -m MPIOM06 --a get_vgep_last_date >\
             ${imls_dir}/nto/dates/mpiom06_vgep_last_date.txt__$$
mv           ${imls_dir}/nto/dates/mpiom06_vgep_last_date.txt__$$ \
             ${imls_dir}/nto/dates/mpiom06_vgep_last_date.txt
#
$malo_query -s nto -m MPIOM06 --a get_vgep_last_update >\
             ${imls_dir}/nto/dates/mpiom06_vgep_last_update.txt__$$
mv           ${imls_dir}/nto/dates/mpiom06_vgep_last_update.txt__$$ \
             ${imls_dir}/nto/dates/mpiom06_vgep_last_update.txt
############
#
# NTO MPIOM07
#
############
$malo_query -s nto -m MPIOM07 -a get_loading_first_date >\
             ${imls_dir}/nto/dates/mpiom07_loading_first_date.txt__$$
mv           ${imls_dir}/nto/dates/mpiom07_loading_first_date.txt__$$ \
             ${imls_dir}/nto/dates/mpiom07_loading_first_date.txt
#
$malo_query -s nto -m MPIOM07 --a get_loading_last_date >\
             ${imls_dir}/nto/dates/mpiom07_loading_last_date.txt__$$
mv           ${imls_dir}/nto/dates/mpiom07_loading_last_date.txt__$$ \
             ${imls_dir}/nto/dates/mpiom07_loading_last_date.txt
#
$malo_query -s nto -m MPIOM07 --a get_loading_last_update >\
             ${imls_dir}/nto/dates/mpiom07_loading_last_update.txt__$$
mv           ${imls_dir}/nto/dates/mpiom07_loading_last_update.txt__$$ \
             ${imls_dir}/nto/dates/mpiom07_loading_last_update.txt
#
$malo_query -s nto -m MPIOM07 --a get_vgep_first_date >\
             ${imls_dir}/nto/dates/mpiom07_vgep_first_date.txt__$$
mv           ${imls_dir}/nto/dates/mpiom07_vgep_first_date.txt__$$ \
             ${imls_dir}/nto/dates/mpiom07_vgep_first_date.txt
#
$malo_query -s nto -m MPIOM07 --a get_vgep_last_date >\
             ${imls_dir}/nto/dates/mpiom07_vgep_last_date.txt__$$
mv           ${imls_dir}/nto/dates/mpiom07_vgep_last_date.txt__$$ \
             ${imls_dir}/nto/dates/mpiom07_vgep_last_date.txt
#
$malo_query -s nto -m MPIOM07 --a get_vgep_last_update >\
             ${imls_dir}/nto/dates/mpiom07_vgep_last_update.txt__$$
mv           ${imls_dir}/nto/dates/mpiom07_vgep_last_update.txt__$$ \
             ${imls_dir}/nto/dates/mpiom07_vgep_last_update.txt
#################
#
#  AAM
#
#################
$malo_query -s aam -m GEOSFCS -a get_aam_last_update >\
             $ners_dir/aam/dates/last_update.txt__$$
$malo_query -s aam -m GEOSFCS -a get_aam_last_date >\
             $ners_dir/aam/dates/last_date.txt__$$
$malo_query -s aam -m GEOSFCS -a get_aam_first_date >\
             $ners_dir/aam/dates/first_date.txt__$$
#
mv ${ners_dir}/aam/dates/last_update.txt__$$ ${ners_dir}/aam/dates/aam_geosfcs_last_update.txt
mv ${ners_dir}/aam/dates/last_date.txt__$$   ${ners_dir}/aam/dates/aam_geosfcs_last_date.txt
mv ${ners_dir}/aam/dates/first_date.txt__$$  ${ners_dir}/aam/dates/aam_geosfcs_first_date.txt
#
$malo_query -s aam -m GEOSFP -a get_aam_last_update >\
             ${ners_dir}/aam/dates/last_update.txt__$$
$malo_query -s aam -m GEOSFP -a get_aam_last_date >\
             ${ners_dir}/aam/dates/last_date.txt__$$
$malo_query -s aam -m GEOSFP -a get_aam_first_date >\
             ${ners_dir}/aam/dates/first_date.txt__$$
#
mv ${ners_dir}/aam/dates/last_update.txt__$$ ${ners_dir}/aam/dates/aam_geosfp_last_update.txt
mv ${ners_dir}/aam/dates/last_date.txt__$$   ${ners_dir}/aam/dates/aam_geosfp_last_date.txt
mv ${ners_dir}/aam/dates/first_date.txt__$$  ${ners_dir}/aam/dates/aam_geosfp_first_date.txt
#
$malo_query -s aam -m GEOSFPIT -a get_aam_last_update >\
             ${ners_dir}/aam/dates/last_update.txt__$$
$malo_query -s aam -m GEOSFPIT -a get_aam_last_date >\
             ${ners_dir}/aam/dates/last_date.txt__$$
$malo_query -s aam -m GEOSFPIT -a get_aam_first_date >\
             ${ners_dir}/aam/dates/first_date.txt__$$
#
mv ${ners_dir}/aam/dates/last_update.txt__$$ ${ners_dir}/aam/dates/aam_geosfpit_last_update.txt
mv ${ners_dir}/aam/dates/last_date.txt__$$   ${ners_dir}/aam/dates/aam_geosfpit_last_date.txt
mv ${ners_dir}/aam/dates/first_date.txt__$$  ${ners_dir}/aam/dates/aam_geosfpit_first_date.txt
#
$malo_query -s aam -m MERRA2 -a get_aam_last_update >\
             ${ners_dir}/aam/dates/last_update.txt__$$
$malo_query -s aam -m MERRA2 -a get_aam_last_date >\
             ${ners_dir}/aam/dates/last_date.txt__$$
$malo_query -s aam -m MERRA2 -a get_aam_first_date >\
             ${ners_dir}/aam/dates/first_date.txt__$$
#
mv ${ners_dir}/aam/dates/last_update.txt__$$ ${ners_dir}/aam/dates/aam_merra2_last_update.txt
mv ${ners_dir}/aam/dates/last_date.txt__$$   ${ners_dir}/aam/dates/aam_merra2_last_date.txt
mv ${ners_dir}/aam/dates/first_date.txt__$$  ${ners_dir}/aam/dates/aam_merra2_first_date.txt
#################
#
#  SPD merra2
#
#################
if [ $hostname == "astrogeo" ] || [ $hostname == "earthrotation" ] || [ $hostname == "gs61a-geodev-a" ]; then
   $malo_query -s spd -m MERRA -a get_spd_last_update >\
                /spd/dates/last_update.txt__$$
   $malo_query -s spd -m MERRA -a get_spd_last_date >\
                /spd/dates/last_date.txt__$$
   $malo_query -s spd -m MERRA -a get_spd_first_date >\
                /spd/dates/first_date.txt__$$
#
   mv /spd/dates/last_update.txt__$$ /spd/dates/spd_merra_last_update.txt
   mv /spd/dates/last_date.txt__$$   /spd/dates/spd_merra_last_date.txt
   mv /spd/dates/first_date.txt__$$  /spd/dates/spd_merra_first_date.txt
#
#   $malo_query -s spd -m MERRA2 -a get_spd_last_update >\
#                /spd/dates/last_update.txt__$$
#   $malo_query -s spd -m MERRA2 -a get_spd_last_date >\
#                /spd/dates/last_date.txt__$$
#   $malo_query -s spd -m MERRA2 -a get_spd_first_date >\
#                /spd/dates/first_date.txt__$$
##
#   mv /spd/dates/last_update.txt__$$ /spd/dates/spd_merra2_last_update.txt
#   mv /spd/dates/last_date.txt__$$   /spd/dates/spd_merra2_last_date.txt
#   mv /spd/dates/first_date.txt__$$  /spd/dates/spd_merra2_first_date.txt
#
   $malo_query -s spd -m GEOSFPIT -a get_spd_last_update >\
                /spd/dates/last_update.txt__$$
   $malo_query -s spd -m GEOSFPIT -a get_spd_last_date >\
                /spd/dates/last_date.txt__$$
   $malo_query -s spd -m GEOSFPIT -a get_spd_first_date >\
                /spd/dates/first_date.txt__$$
#
   mv /spd/dates/last_update.txt__$$ /spd/dates/spd_geosfpit_last_update.txt
   mv /spd/dates/last_date.txt__$$   /spd/dates/spd_geosfpit_last_date.txt
   mv /spd/dates/first_date.txt__$$  /spd/dates/spd_geosfpit_first_date.txt
#
   $malo_query -s aam -m MERRA2 -a get_aam_last_update >\
                ${ners_dir}/aam/dates/last_update.txt__$$
   $malo_query -s aam -m MERRA2 -a get_aam_last_date >\
                ${ners_dir}/aam/dates/last_date.txt__$$
   $malo_query -s aam -m MERRA2 -a get_aam_first_date >\
                ${ners_dir}/aam/dates/first_date.txt__$$
#
   mv ${ners_dir}/aam/dates/last_update.txt__$$ ${ners_dir}/aam/dates/aam_merra2_last_update.txt
   mv ${ners_dir}/aam/dates/last_date.txt__$$   ${ners_dir}/aam/dates/aam_merra2_last_date.txt
   mv ${ners_dir}/aam/dates/first_date.txt__$$  ${ners_dir}/aam/dates/aam_merra2_first_date.txt
#
   $malo_query -s spd -m MERRA2 -a get_spd_last_update >\
                /spd/dates/last_update.txt__$$
   $malo_query -s spd -m MERRA2 -a get_spd_last_date >\
                /spd/dates/last_date.txt__$$
   $malo_query -s spd -m MERRA2 -a get_spd_first_date >\
                /spd/dates/first_date.txt__$$
#
   mv /spd/dates/last_update.txt__$$ /spd/dates/spd_merra2_last_update.txt
   mv /spd/dates/last_date.txt__$$   /spd/dates/spd_merra2_last_date.txt
   mv /spd/dates/first_date.txt__$$  /spd/dates/spd_merra2_first_date.txt
#
#################
#
#  SPD opa_geosfpit
#
#################
#
   $malo_query -s opa_spd -m GEOSFPIT -a get_spd_last_update >\
                /spd/dates/last_update.txt__$$
   $malo_query -s opa_spd -m GEOSFPIT -a get_spd_last_date >\
                /spd/dates/last_date.txt__$$
   $malo_query -s opa_spd -m GEOSFPIT -a get_spd_first_date >\
                /spd/dates/first_date.txt__$$
#
   mv /spd/dates/last_update.txt__$$ /spd/dates/opa_spd_geosfpit_last_update.txt
   mv /spd/dates/last_date.txt__$$   /spd/dates/opa_spd_geosfpit_last_date.txt
   mv /spd/dates/first_date.txt__$$  /spd/dates/opa_spd_geosfpit_first_date.txt
#################
#
#  SPD opa_geosfpit
#
#################
#
   $malo_query -s opa_spd -m GEOSFPIT -a get_spd_last_update >\
                /spd/dates/last_update.txt__$$
   $malo_query -s opa_spd -m GEOSFPIT -a get_spd_last_date >\
                /spd/dates/last_date.txt__$$
   $malo_query -s opa_spd -m GEOSFPIT -a get_spd_first_date >\
                /spd/dates/first_date.txt__$$
#
   mv /spd/dates/last_update.txt__$$ /spd/dates/opa_spd_geosfpit_last_update.txt
   mv /spd/dates/last_date.txt__$$   /spd/dates/opa_spd_geosfpit_last_date.txt
   mv /spd/dates/first_date.txt__$$  /spd/dates/opa_spd_geosfpit_first_date.txt
fi
#
python3 $MALO_SCRIPT/malo_service.py \
       -c $MALO_SHARE/malo_stat_service.conf \
       -a get_num_files >& /imls/logs/malo_num_files.txt
python3 $MALO_SCRIPT/malo_service.py \
       -c $SPD_SHARE/spd_stat_service.conf \
       -a get_num_files >& /imls/logs/spd_num_files.txt
python3 $MALO_SCRIPT/malo_service.py \
       -c $SPD_SHARE/opa_spd_stat_service.conf \
       -a get_num_files >& /imls/logs/opa_spd_num_files.txt
python3 $MALO_SCRIPT/malo_service.py \
       -c $MALO_SHARE/aam_stat_service.conf \
       -a get_num_files >& /imls/logs/aam_num_files.txt
if [ $hostname == "astrogeo" ] || [ $hostname == "earthrotation" ] || [ $hostname == "gs61a-geodev-a" ]; then
     num1=`cat /imls/logs/spd_num_files.txt`
     num2=`cat /imls/logs/opa_spd_num_files.txt`
     if [ ${#num1} -lt  9 -a ${#num2} -lt  9 ] ; then
          num_all_spd=$((${num1}+${num2}))
     else
          num_all_spd="unknown"
     fi
     echo $num_all_spd > /imls/logs/all_spd_num_files.txt
fi
#
# --- Check for gaps and get statistics
#
$MALO_SCRIPT/imls_check.csh > $imls_dir/stat/${host_prefix}_imls_check.txt
cat $imls_dir/stat/${host_prefix}_imls_check.txt | tail -1 | head -1 > $imls_dir/stat/num_files.txt
cat $imls_dir/stat/${host_prefix}_imls_check.txt | tail -2 | head -1 > $imls_dir/stat/num_grid_files.txt
cat $imls_dir/stat/${host_prefix}_imls_check.txt | tail -3 | head -1 > $imls_dir/stat/num_grid_points.txt
