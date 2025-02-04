#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Program loading_list_update.csh updates the loading time series    *
# *   for the default station list. If works in tow modes: seris and     *
# *   harmonics.                                                         *
# *                                                                      *
# * ## 17-JUN-2017 loading_list_update.csh v1.2 (c) L. Petrov 11-MAR-2020 ## *
# *                                                                      *
# ************************************************************************
set BIN_DIR = /opt64
setenv PATH  "${PATH}:${BIN_DIR}/bin"
set MALO_DIR = `$BIN_DIR/bin/malo_inq script | sed "s@/script@@g"`
set MALO_SCRIPT = `$BIN_DIR/bin/malo_inq script`
set MALO_SHARE = `$BIN_DIR/bin/malo_inq share`
set MALO_BIN = `$BIN_DIR/bin/malo_inq bin_static`
setenv GOMP_STACKSIZE 2000000
limit stacksize       2000000
#
if ( $#argv < 3 ) then 
     echo "Usage: loading_list_update.csh type model series|harmonics date_beg date_end cm|d1|all"
     exit 1
endif
set typ       = $1
set model     = $2
set mode      = $3
set date_beg  = $4
set date_end  = $5
set origin    = $6
set ivrb      = 2
#
if ( $mode == "series" || $mode == "harmonics" || $mode == "s1-harmonics" ) then
  else
     echo "wrong 3rd argumetn $mode while one of series, harmonics, or sa-harmonics were expected"
endif
#
if ( `hostname` == "astrogeo" || `hostname` == "earthrotation" ) then
      set host_name = `hostname`
else if ( `hostname` == "gs698-geopod.gsfc.nasa.gov" || `hostname` == "gs61a-geopod.gsfc.nasa.gov" ) then
      set host_name = geopod
else if ( `hostname` == "gs61a-geodev-a" ) then
      set host_name = deva
else
      set host_name = unknown
endif
#
set beg_date = `date "+%Y.%m.%d_%H:%M:%S"`
echo "load_list_update.csh started on $beg_date"
if ( "$mode" == "harmonics" ) then
     if ( typ == "nto" ) then
          $MALO_SCRIPT/har_loading.py $typ $model 0010011 $ivrb
	  set malo_status = $status
       else
          $MALO_SCRIPT/har_loading.py $typ $model 001001  $ivrb
	  set malo_status = $status
     endif
     set end_date = `date "+%Y.%m.%d_%H:%M:%S"`
     echo "load_list_update.csh finshed on $end_date"
     exit $malo_status
else
     set filcnf = $MALO_SHARE/${host_name}_${typ}_${model}.conf
     if ( -f $filcnf ) then
        else
          echo "Cannot find control file $filcnf"
          exit 1
     endif
     set load_conf        = $MALO_SHARE/`cat $filcnf | grep -v '#' | grep "load_conf:"        | awk '{print $2}'`
     set load_spl_dir     = `cat $filcnf | grep -v '#' | grep "load_spl_dir:"     | awk '{print $2}'`
     set load_list_dir    = `cat $filcnf | grep -v '#' | grep "load_list_dir:"    | awk '{print $2}'`
     set load_d1_spl_dir  = `cat $filcnf | grep -v '#' | grep "load_d1_spl_dir:"  | awk '{print $2}'`
     set load_d1_list_dir = `cat $filcnf | grep -v '#' | grep "load_d1_list_dir:" | awk '{print $2}'`
     if ( -f $load_conf ) then
        else
          echo "Cannot find control file loading configuration file $load_conf"
          exit 1
     endif
     set end_date = `date "+%Y.%m.%d_%H:%M:%S"`
#
     set sta_file = $MALO_SHARE/`cat $load_conf | grep -v '#' | grep "STATION_FINAM" | awk '{print $3}'`
     set malo_status = 1
#
     if ( $origin == "cm" || $origin == "all" ) then
          echo "load_list_update.csh cm started on $end_date"
          set com = "$MALO_SCRIPT/loa_spl_to_eph.py $load_spl_dir    $sta_file $load_list_dir    $date_beg $date_end $ivrb"
          echo "Execute com = $com"
          `$com`
          set malo_status = $status
     endif
#
     if ( $origin == "d1" || $origin == "all" ) then
          echo "load_list_update.csh d1 started on $end_date"
          set com = "$MALO_SCRIPT/loa_spl_to_eph.py $load_d1_spl_dir $sta_file $load_d1_list_dir $date_beg $date_end $ivrb"
          echo "Execute com = $com"
          `$com`
          set end_date = `date "+%Y.%m.%d_%H:%M:%S"`
          set malo_status = $status
     endif
     echo "load_list_update.csh finshed on $end_date"
     exit $malo_status
endif
