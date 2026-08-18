" NASA style of VLBI schedule in proc format.
" Station:      WESTFORD   Wf
"
" Template last modification date: 2025.08.13_11:52:58
" Last update:  @update_date@
"
" Hidden procedures: pcalon
"
@vers@
define  proc_library  00000000000x
enddef
"
" =================================
"
define  sched_initi   00000000000x
preses_@hds@
setmode_@mode@   
enddef
"
"=================================
"
define  ifdmon        00000000000x
sy=popen 's_client -t 2 -h udca -c udc_lo=@udc_lo@ 2>&1' -n udcca
sy=popen 's_client -t 2 -h udcb -c udc_lo=@udc_lo@ 2>&1' -n udccb
sy=popen 's_client -t 2 -h udcc -c udc_lo=@udc_lo@ 2>&1' -n udccc
sy=popen 's_client -t 2 -h udcd -c udc_lo=@udc_lo@ 2>&1' -n udccd
sy=popen 's_client -t 2 -h udca -c udc_atten=0:5 2>&1'   -n udcca
sy=popen 's_client -t 2 -h udca -c udc_atten=1:5 2>&1'   -n udcca
sy=popen 's_client -t 2 -h udcb -c udc_atten=0:5 2>&1'   -n udccb
sy=popen 's_client -t 2 -h udcb -c udc_atten=1:5 2>&1'   -n udccb
sy=popen 's_client -t 2 -h udcc -c udc_atten=0:5 2>&1'   -n udccc
sy=popen 's_client -t 2 -h udcc -c udc_atten=1:5 2>&1'   -n udccc
sy=popen 's_client -t 2 -h udcd -c udc_atten=0:5 2>&1'   -n udccd
sy=popen 's_client -t 2 -h udcd -c udc_atten=1:5 2>&1'   -n udccd
enddef
"
"=================================
"
define  time          00000000000x
rdbe=pps_offset?;
rdbe=dot?;
rdbe=gps_offset?;
enddef
"
"=================================
"
define  dewar         00000000000x
sy=popen -n mcicn 'mcicn mci "g_hrp" 2>&1' &
!+1s
sy=popen -n mcicn 'mcicn mci "g_70ktemp" 2>&1' &
!+1s
sy=popen -n mcicn 'mcicn mci "g_20ktemp" 2>&1' &
enddef
"
"=================================
"=================================
"
define  preses_@hds@   00000000000x
" Duration: 0 sec
"fix for tsys for band d, allows 5point to run
" set the mk6 stream
mk6=input_stream = delete ;
!+4s
mk6=input_stream = add : rdbeA : vdif : 8224 : 42 : 66 : eth2 : 127.0.0.1 : 12000;
mk6=input_stream = add : rdbeB : vdif : 8224 : 42 : 66 : eth3 : 127.0.0.1 : 12000;
mk6=input_stream = add : rdbeC : vdif : 8224 : 42 : 66 : eth4 : 127.0.0.1 : 12000;
mk6=input_stream = add : rdbeD : vdif : 8224 : 42 : 66 : eth5 : 127.0.0.1 : 12000;
mk6=input_stream = commit ;
rdbed=dbe_num_chan=legacy:32:16;
enddef
"
"=================================
"
define  setmode_@mode@       00000000000x
" Duration: 4 sec
@time_stamp@
" set observing mode @mode@
" set the lo stream
"
lo=
lo=loa0,@lo@,usb,lcp,5
lo=loa1,@lo@,usb,rcp,5
lo=lob0,@lo@,usb,lcp,5
lo=lob1,@lo@,usb,rcp,5
lo=loc0,@lo@,usb,lcp,5
lo=loc1,@lo@,usb,rcp,5
lo=lod0,@lo@,usb,lcp,5
lo=lod1,@lo@,usb,rcp,5
ifdmon
enddef
"
"=================================
"
define  setscan_@hds@       00000000000x
" Duration: 0 sec
pcalon
tpicd=stop
rdbe=dbe_data_send=off;
!+1s
rdbe=dbe_chsel_en=@chs_en@;
rdbea=dbe_chsel=0:@chsela@;
rdbea=dbe_chsel=1:@chsela@;
rdbeb=dbe_chsel=0:@chselb@;
rdbeb=dbe_chsel=1:@chselb@;
rdbec=dbe_chsel=0:@chselc@;
rdbec=dbe_chsel=1:@chselc@;
rdbed=dbe_chsel=0:@chseld@;
rdbed=dbe_chsel=1:@chseld@;
rdbea=pcal=@pcal_step_10@;
rdbeb=pcal=@pcal_step_10@;
rdbec=pcal=@pcal_step_10@;
rdbed=pcal=@pcal_step_10@;
!+1s
rdbe=dbe_data_send=on;
tpicd=no,100
tpicd
enddef
"
"=================================
"
define  preob_@hds@        00000000000x
" Duration: 3 sec
onsource
track
rdbe_atten=
rdbe=dbe_quantize=0;
rdbe=dbe_quantize=1;
rdbe=dbe_bstate?0;
rdbe=dbe_bstate?1;
mk6=rtime?@bit_rate@;
enddef
"
"=================================
"
define  midob_@hds@         00000000000x
" Duration: 0 sec
!+4s
onsource
track
mk6=rtime?@bit_rate@;
data_valid=on
rdbe=sw_version?;
mk6=dts_id?;
rdbe=dbe_personality?;
wx
ifdmon
mk6=input_stream?;
rdbe=dbe_chsel_en?;
rdbe=dbe_chsel?0;
rdbe=dbe_chsel?1;
rdbe=pcal?;
rdbe_atten
rdbe=dbe_bstate?0;
rdbe=dbe_bstate?1;
dewar
time
enddef
"
"=================================
"
define  postob_@hds@        00000000000x
" Duration: 2 sec
mk6=msg?;
data_valid=off
mk6=record=off;
mk6=rtime?@bit_rate@;
!+2s
mk6=scan_check?;
enddef
"
"=================================
"
define  postses_@hds@        00000000000x
" Duration: 0 sec
" End of schedule
sched_end
enddef
