" NASA style of VLBI schedule in proc format.
" Station:      GGAO12M    Gs
"
" Template last modification date: 2024.07.08_09:02:55
" Last update:  @update_date@
"
" Hidden procedures: dewar start_mlog stop_mlog
"
@vers@
define  proc_library  00000000000x
enddef
"
" =================================
"
define  sched_initi   00000000000x
preses_@hds@
enddef
"
"=================================
"
define  pcalon        00000000000x
enddef
"
"=================================
"
define  ifdbb         00000000000x
enddef
"
"=================================
"
define  mk6bb         00000000000x
" set the mk6 stream
mk6=input_stream = delete ;
!+4s
mk6=input_stream = add : rdbeA : vdif : 8224 : 42 : 66 : eth2 : 127.0.0.1 : 12000;
mk6=input_stream = add : rdbeB : vdif : 8224 : 42 : 66 : eth3 : 127.0.0.1 : 12000;
mk6=input_stream = add : rdbeC : vdif : 8224 : 42 : 66 : eth4 : 127.0.0.1 : 12000;
mk6=input_stream = add : rdbeD : vdif : 8224 : 42 : 66 : eth5 : 127.0.0.1 : 12000;
mk6=input_stream = commit ;
enddef
"
"=================================
"
define  ifdmon        00000000000x
sy=popen 'udceth0 udca 2>&1' -n udcca &
sy=popen 'udceth0 udcb 2>&1' -n udccb &
sy=popen 'udceth0 udcc 2>&1' -n udccc &
sy=popen 'udceth0 udcd 2>&1' -n udccd &
enddef
"
"=================================
"
define  checkmk6      00000000000x
mk6=record=off;
!+2s
mk6=scan_check?;
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
"=================================
" 
define  preses_@hds@   00000000000x
" Duration: 0 sec
mk6bb
start_mlog
setmode_@mode@
enddef
"
"=================================
"
define  setmode_@mode@   00000000000x
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
" new lo: lo=loa0,@lo@,usb,lcp,5
" new lo: lo=loa1,@lo@,usb,rcp,5
" new lo: lo=lob0,@lo@,usb,lcp,5
" new lo: lo=lob1,@lo@,usb,rcp,5
" new lo: lo=loc0,@lo@,usb,lcp,5
" new lo: lo=loc1,@lo@,usb,rcp,5
" new lo: lo=lod0,@lo@,usb,lcp,5
" new lo: lo=lod1,@lo@,usb,rcp,5
" set the udc
sy=popen 'udceth0 udca @udc_luff@ 20 20 2>&1' -n udcca &
sy=popen 'udceth0 udcb @udc_luff@ 20 20 2>&1' -n udccb &
sy=popen 'udceth0 udcc @udc_luff@ 20 20 2>&1' -n udccc &
sy=popen 'udceth0 udcd @udc_luff@ 20 20 2>&1' -n udccd &
" set rdbe channalization for observing mode @mode@
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
rdbea=pcal=@pcal_step_5@;
rdbeb=pcal=@pcal_step_5@;
rdbec=pcal=@pcal_step_5@;
rdbed=pcal=@pcal_step_5@;
!+1s
rdbe=dbe_data_send=on;
enddef
"
"=================================
"
define  setscan_@hds@   00000000000x
" Duration: 0 sec
pcalon
tpicd=stop
tpicd=no,100
tpicd
enddef
"
"=================================
"
define  preob_@hds@     00000000000x
" Duration: 3 sec
onsource
track
rdbe_atten=
rdbe=dbe_quantize=0;
rdbe=dbe_quantize=1;
mk6=rtime?@bit_rate@;
enddef
"
"=================================
"
define  midob_@hds@     00000000000x
" Duration: 0 sec
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
define  postob_@hds@    00000000000x
" Duration: 2 sec
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
stop_mlog
" End of schedule
sched_end
enddef
