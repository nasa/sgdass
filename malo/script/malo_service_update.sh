#!/bin/bash
# ************************************************************************
# *                                                                      *
# *  Launch various operational service updates related to processing    *
# *  atmosphere and hydrosphere models for mass loading, atmospheric     *
# *  angular moment and the EOP prediction.                              *
# *                                                                      *
# * # 11-NOV-2014 malo_service_update.sh v4.6 (c) L. Petrov 13-JAN-2024# *
# *                                                                      *
# ************************************************************************
if [ `uname -n` == "astrogeo" ]; then
      BIN_DIR=/opt64
      host_prefix="astrogeo"
elif [ `uname -n` == "earthrotation" ]; then
      BIN_DIR=/opt64
      host_prefix="astrogeo"
elif [ `uname -n` == "gs61a-geodev-a" ]; then
      export LD_LIBRARY_PATH="/opt64/lib:/opt64/lib64"
      host_prefix="deva"
      BIN_DIR=/opt64
elif [ `uname -n` == "geopod" ]; then
      host_prefix="geopod"
      BIN_DIR=/Users/lpetrov/opt
else
      echo "malo_service_update.sh: unknown host "`uname -n`
      exit 1
fi
export PATH=$PATH:$BIN_DIR/bin
MALO_DIR=`$BIN_DIR/bin/malo_inq script | sed "s@/script@@g"`
MALO_SCRIPT=`$BIN_DIR/bin/malo_inq script`
MALO_SHARE=`$BIN_DIR/bin/malo_inq share`
MALO_BIN=`$BIN_DIR/bin/malo_inq bin_static`
#
if [ "$#" -lt 2 ] ; then
     echo "Usage: $0 typ model"
     exit 1
else
     typ=$1
     model=$2
fi
#
if [ -f /imls/logs/geos.stop ]; then
     exit 0
fi
#
if [ $typ == "atm" ] && [ $model == "geosfpit" ]; then
#
# -- mass loading and aam due to the atmosphere using GEOS-FPIT model
#
     python3 $MALO_SCRIPT/geos_oper.py -c $MALO_SHARE/${host_prefix}_atm_geosfpit.conf -v 2 >> \
            /imls/logs/atm_geosfpit_fetch_raw.log 2>&1 &
     if [ $? != 0 ]; then
          error_file=`grep err_file $MALO_SHARE/${host_prefix}_atm_geosfpit.conf | awk '{print $2}'`
          dat=`date '+%Y.%m.%d_%H:%M:%S'`
          echo "malo_service_update.sh $dat Script geos_oper.py has crashed" > $error_file
     fi
elif [ $typ == "lws" ] && [ $model == "geosfpit" ]; then
#
# -- mass loading due to the land water storage using GEOS-FPIT model
#
     python3 $MALO_SCRIPT/geos_oper.py -c $MALO_SHARE/${host_prefix}_lws_geosfpit.conf -v 2 >> \
             /imls/logs/lws_geosfpit_fetch_raw.log 2>&1 &
     if [ $? != 0 ]; then
          error_file=`grep err_file $MALO_SHARE/${host_prefix}_lws_geosfpit.conf | awk '{print $2}'`
          dat=`date '+%Y.%m.%d_%H:%M:%S'`
          echo "malo_service_update.sh $dat Script geos_oper.py has crashed" > $error_file
    fi
elif [ $typ == "aam" ] && [ $model == "geosfp" ]; then
#
# -- AAM from GEOSFP model
#
     python3 $MALO_DIR/script/geos_oper.py \
             -c $MALO_DIR/share/${host_prefix}_aam_geosfp.conf \
             -v 2 >> \
             /imls/logs/geosfp_fetch_raw.log 2>&1 &
     if [ $? != 0 ]; then
          error_file=`grep err_file $MALO_DIR/share/${host_prefix}_aam_geosfp.conf | awk '{print $2}'`
          dat=`date '+%Y.%m.%d_%H:%M:%S'`
          echo "malo_service_update.sh $dat Script geos_oper.py has crashed" > $error_file
     fi
elif [ $typ == "aam" ] && [ $model == "geosfcs" ]; then
#
# -- AAM from GEOS forecast
#
     python3 $MALO_DIR/script/geos_fcs_oper.py \
             -c $MALO_DIR/share/${host_prefix}_aam_geosfcs.conf \
             -v 2 >> \
             /imls/logs/geosfcs_fetch_raw.log 2>&1 &
     if [ $? != 0 ]; then
          error_file=`grep err_file $MALO_DIR/share/${host_prefix}_aam_geosfcs.conf | awk '{print $2}'`
          dat=`date '+%Y.%m.%d_%H:%M:%S'`
          echo "malo_service_update.sh $dat Script geos_fcs_oper.py has crashed" > $error_file
     fi
elif [ $typ == "spd" ] && [ $model == "geosfpit" ]; then
#
# -- Slant path delay from the GEOS-FPIT mdoel
#
     if [ -f /imls/logs/spd.stop ]; then
          exit 0
     fi
     python3    $BIN_DIR/spd/script/spd_update.py \
             -c $BIN_DIR/spd/share/${host_prefix}_spd_geosfpit.conf \
             -v 1 >> /imls/logs/spd_fetch_raw.log  2>&1
#
     if [ $? != 0 ]; then
          dat=`date '+%Y.%m.%d_%H:%M:%S'`
          echo "malo_service_update.sh $typ $dat Script spd_update.py has crashed" > /imls/logs/geosfpit_spd.err
     fi

elif [ $typ == "opa_spd" ] && [ $model == "geosfpit" ]; then
#
# -- Slant path delay from the GEOS-FPIT model
#
     if [ -f /imls/logs/spd.stop ]; then
          exit 0
     fi
     python3    $BIN_DIR/spd/script/spd_update.py \
             -c $BIN_DIR/spd/share/${host_prefix}_opa_spd_geosfpit.conf \
             -v 1 >> /imls/logs/opa_spd_geosfpit_fetch_raw.log  2>&1
#
     if [ $? != 0 ]; then
          dat=`date '+%Y.%m.%d_%H:%M:%S'`
          echo "malo_service_update.sh $typ $dat Script spd_update.py has crashed" > /imls/logs/geosfpit_opa_spd.err
     fi
elif [ $typ == "opa_spd" ] && [ $model == "geosit" ]; then
#
# -- Slant path delay from the GEOS-IT model
#
     if [ -f /imls/logs/spd.stop ]; then
          exit 0
     fi
     python3    $BIN_DIR/spd/script/spd_update.py \
             -c $BIN_DIR/spd/share/${host_prefix}_opa_spd_geosit.conf \
             -v 1 >> /imls/logs/opa_spd_geosit_fetch_raw.log  2>&1
#
     if [ $? != 0 ]; then
          dat=`date '+%Y.%m.%d_%H:%M:%S'`
          echo "malo_service_update.sh $typ $dat Script spd_update.py has crashed" > /imls/logs/geosit_opa_spd.err
     fi
elif [ $typ == "eop" ] && [ $model == "geosfcs" ]; then
#
# -- EOP prediction for the Network Earth rotation model using the AAM forecast
#
     if [ ${host_prefix} == "astrogeo" ] ; then
           export ners_fcs=/earthrotation.net/ners/eop.ners
           export ners_save=/earthrotation.net/ners/save
           export ners_start_date=/earthrotation.net/ners/ners_start_date.txt
           export ners_stop_date=/earthrotation.net/ners/ners_stop_date.txt
           export ners_update_date=/earthrotation.net/ners/ners_stop_update.txt
     elif [ ${host_prefix} == "deva" ]; then
           export ners_fcs=/earthrotation/ners/eop.ners
           export ners_save=/earthrotation/ners/save
           export ners_start_date=/earthrotation/ners/ners_start_date.txt
           export ners_stop_date=/earthrotation/ners/ners_stop_date.txt
           export ners_update_date=/earthrotation/ners/ners_stop_update.txt
     else
           export ners_fcs=/imls/earthrotation.net/ners/eop.ners
           export ners_save=/imls/earthrotation.net/ners/save
           export ners_start_date=/imls/earthrotation.net/ners/ners_start_date.txt
           export ners_stop_date=/imls/earthrotation.net/ners/ners_stop_date.txt
           export ners_update_date=/imls/earthrotation.net/ners/ners_stop_update.txt
     fi
#
     export ners_stop_file=/imls/logs/eop.stop
     export raw_eop_log=/imls/logs/eop_raw.log
     date_iso=`date "+%Y.%m.%d_%H:%M:%S.0"`
     echo "malo_service_update.sh Started  on $date_iso"             >> $raw_eop_log
     echo "==========================================="              >> $raw_eop_log
     if [ -f $ners_stop_file ]; then
           echo "malo_service_update.sh Stopped due to stop_file $stop_file"     >> $raw_eop_log
           exit 0
     fi
#
     $MALO_SCRIPT/get_eop.py -v 1 -c $MALO_SHARE/${host_prefix}_eop.conf -r 0 \
               >> $raw_eop_log 2>&1
     date_iso=`date "+%Y.%m.%d_%H:%M:%S.0"`
     echo "malo_service_update.sh Updating ners forecast"            >> $raw_eop_log
     $MALO_BIN/eop_fcs $MALO_SHARE/${host_prefix}_eop.conf ${ners_fcs}__$$ 0 \
               >> $raw_eop_log 2>&1
     if [ $? -eq 0 ]; then
          mv ${ners_fcs}__$$ ${ners_fcs}
          echo "malo_service_update.sh Updated  ners forecast"            >> $raw_eop_log
#
          date_ners=`date "+%Y%m%d_%H%M"`
          cp $ners_fcs $ners_save/eop.ners_$date_ners
          echo "malo_service_update.sh Copied ners forecast into $ners_save/eop.ners_$date_ners" >> $raw_eop_log
     else
          rm ${ners_fcs}__$$
          echo "malo_service_update.sh Failed to updated  ners forecast"  >> $raw_eop_log
     fi
     echo " "                                                        >> $raw_eop_log
#
     date_iso=`date "+%Y.%m.%d_%H:%M:%S.0"`
     echo $date_iso                                       > $ners_update_date
     $MALO_BIN/show_eop_fcs $ners_fcs 2000.01.01 start    > $ners_start_date
     $MALO_BIN/show_eop_fcs $ners_fcs 2000.01.01 stop     > $ners_stop_date
     echo "malo_service_update.sh Finished on $date_iso"             >> $raw_eop_log
     echo "============================================"             >> $raw_eop_log
     echo " "                                                        >> $raw_eop_log
elif [ $typ == "atm" ] && [ $model == "merra2" ]; then
#
# -- mass loading and aam due to the atmosphere using MERRA2 model
#
     python3 $MALO_SCRIPT/geos_oper.py  -c $MALO_SHARE/${host_prefix}_atm_merra2.conf -v 2 >> \
	    /imls/logs/atm_merra2_fetch_raw.log   2>&1
     if [ $? != 0 ]; then
          error_file=`grep err_file $MALO_SHARE/${host_prefix}_atm_merra2.conf | awk '{print $2}'`
          dat=`date '+%Y.%m.%d_%H:%M:%S'`
          echo "malo_service_update.sh $dat Script geos_oper.py has crashed" > $error_file
     fi
elif [ $typ == "lws" ] && [ $model == "merra2" ]; then
#
# -- mass loading due to the land water storage using MERRA2 model
#
     python3 $MALO_SCRIPT/geos_oper.py -c $MALO_SHARE/${host_prefix}_lws_merra2.conf  -v 2 >> \
	     /imls/logs/lws_merra2_fetch_raw.log   2>&1
     if [ $? != 0 ]; then
          error_file=`grep err_file $MALO_DIR/share/${host_prefix}_lws_merra2.conf | awk '{print $2}'`
          dat=`date '+%Y.%m.%d_%H:%M:%S'`
          echo "malo_service_update.sh $dat Script geos_oper.py has crashed" > $error_file
     fi
elif [ $typ == "nto" ] && [ $model == "omct05" ]; then
#
# -- mass loading due to non-tidal ocean bottom pressure changes using OMCT05 model
#
     python3 $MALO_SCRIPT/omct05_oper.py -c $MALO_SHARE/${host_prefix}_nto_omct05.conf    -v 2 >> \
	     /imls/logs/nto_omct05_fetch_raw.log    2>&1
     if [ $? != 0 ]; then
          error_file=`grep err_file $MALO_SHARE/${host_prefix}_nto_omct05.conf | awk '{print $2}'`
          dat=`date '+%Y.%m.%d_%H:%M:%S'`
          echo "malo_service_update.sh $dat Script omct05_oper.py has crashed" > $error_file
     fi
elif [ $typ == "nto" ] && [ $model == "mpiom06" ]; then
#
# -- mass loading due to non-tidal ocean bottom pressure changes using MPIOM06 model
#
     python3 $MALO_SCRIPT/mpiom_oper.py -c $MALO_SHARE/${host_prefix}_nto_mpiom06.conf    -v 2 >> \
	     /imls/logs/nto_mpiom06_fetch_raw.log    2>&1
     if [ $? != 0 ]; then
          error_file=`grep err_file $MALO_SHARE/${host_prefix}_nto_mpiom06.conf | awk '{print $2}'`
          dat=`date '+%Y.%m.%d_%H:%M:%S'`
          echo "malo_service_update.sh $dat Script mpiom06_oper.py has crashed" > $error_file
     fi
elif [ $typ == "nto" ] && [ $model == "mpiom07" ]; then
#
# -- mass loading due to non-tidal ocean bottom pressure changes using MPIOM07 model
#
     python3 $MALO_SCRIPT/mpiom_oper.py -c $MALO_SHARE/${host_prefix}_nto_mpiom07.conf    -v 2 >> \
	     /imls/logs/nto_mpiom07_fetch_raw.log    2>&1
     if [ $? != 0 ]; then
          error_file=`grep err_file $MALO_SHARE/${host_prefix}_nto_mpiom07.conf | awk '{print $2}'`
          dat=`date '+%Y.%m.%d_%H:%M:%S'`
          echo "malo_service_update.sh $dat Script mpiom07_oper.py has crashed" > $error_file
     fi
elif [ $typ == "check" ] && [ $model == "all" ]; then
     $MALO_DIR/script/malo_check.sh
     if [ `uname -n` == "gs61a-geodev-a" ]; then
          curl --silent --form "/massloading.net/deva/stat_dynamic.html=@/imls/logs/deva_stat_dynamic_all.html;type=text/plain;upload_type=stat_dev" http://alt.massloading.net/cgi-bin/load_ascii_file.py
     fi
else
     echo "$0 ERROR: unsupporred combination of $typ $model"
     exit 1
fi
