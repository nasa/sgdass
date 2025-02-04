" NASA style of VLBI schedule in proc format.
" Station:      RAEGYEB    Yj
"
" Template last modification date: 2024.02.26_11:35:54
" Last update:  @update_date@
"
" Hidden procedures: phasecal newlo
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
" Turn phase cal on
phasecal=on
enddef
"
"=================================
define  bread         00000000000x
"Yebes RDBEs:
rdbe=dbe_tsys?1;
rdbe=dbe_tsys?0;
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
mk6=input_stream = add : bandA : vdif : 8224 : 42 : 66 : eth2 : 192.168.1.100 : 9000;
mk6=input_stream = add : bandB : vdif : 8224 : 42 : 66 : eth3 : 192.168.1.101 : 9001;
mk6=input_stream = add : bandC : vdif : 8224 : 42 : 66 : eth4 : 192.168.1.102 : 9002;
mk6=input_stream = add : bandD : vdif : 8224 : 42 : 66 : eth5 : 192.168.1.103 : 9003;
mk6=input_stream = commit;
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
define  auto          00000000000x
rdbe_atten=both
rdbe=dbe_quantize=0;
rdbe=dbe_quantize=1;
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
define  logsw_geo     00000000000x
"switch the log file script to ivs
"2014-02-03 pb v1.0
sy=rm /usr2/oper/bin/lgput
sy=ln -s /usr2/oper/bin/lgput_geo /usr2/oper/bin/lgput
"log destination switched to ivs
enddef
"
"=================================
"=================================
" 
define  preses_@hds@   00000000000x
" Duration: 0 sec
" OK
antenna=geo
logsw_geo
proc_library
mk6bb
auto
setmode_@mode@
enddef
"
"=================================
"
define  setmode_@mode@   00000000000x
" Duration: 6 sec
@time_stamp@
" set observing mode @mode@
" set the lo stream
lo=
newlo=0
lo=loa0,@lo@,usb,lcp,10
lo=loa1,@lo@,usb,rcp,10
newlo=a
lo=lob0,@lo@,usb,lcp,10
lo=lob1,@lo@,usb,rcp,10
newlo=b
lo=loc0,@lo@,usb,lcp,10
lo=loc1,@lo@,usb,rcp,10
newlo=c
lo=lod0,@lo@,usb,lcp,10
lo=lod1,@lo@,usb,rcp,10
newlo=d
" new lo: lo=loa0,@lo@,usb,lcp,10
" new lo: lo=loa1,@lo@,usb,rcp,10
" new lo: lo=lob0,@lo@,usb,lcp,10
" new lo: lo=lob1,@lo@,usb,rcp,10
" new lo: lo=loc0,@lo@,usb,lcp,10
" new lo: lo=loc1,@lo@,usb,rcp,10
" new lo: lo=lod0,@lo@,usb,lcp,10
" new lo: lo=lod1,@lo@,usb,rcp,10
rdbe=dbe_data_send=off;
" set rdbe channalization for observing mode @mode@
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
auto
track
enddef
"
"=================================
"
define  midob_@hds@     00000000000x
" Duration: 0 sec
onsource
bread
data_valid=on
mk6=dts_id?;
mk6=input_stream?;
"
"Yebes RDBEs
rdbe=sw_version?;
rdbe=dbe_personality?;
rdbe=dbe_chsel_en?;
rdbe=dbe_chsel?0;
rdbe=dbe_chsel?1;
rdbe=pcal?;
rdbe_atten
time
cable=getmeas
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
enddef
"
"=================================
"
define  postses_@hds@   00000000000x
" Duration: 0 sec
" End of schedule
sched_end
enddef
