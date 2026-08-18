" NASA style of VLBI schedule in proc format.
" Station:      FORTZA12   Fz
"
" Template last modification date: 2025.11.26_13:22:35
" Last update:  @update_date@
"
" Hidden procedures: pcalon setup_local
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
define  ifdmon        00000000000x
sy=popen 's_client -t 2 -h udca -c udc_lo=@udc_lo@ 2>&1' -n udcca
sy=popen 's_client -t 2 -h udcb -c udc_lo=@udc_lo@ 2>&1' -n udccb
sy=popen 's_client -t 2 -h udcc -c udc_lo=@udc_lo@ 2>&1' -n udccc
sy=popen 's_client -t 2 -h udcd -c udc_lo=@udc_lo@ 2>&1' -n udccd
"
sy=popen 's_client -h udca -c udc_atten=0:17 -t 2 2>&1' -n udcca
sy=popen 's_client -h udcb -c udc_atten=0:15 -t 2 2>&1' -n udccb
sy=popen 's_client -h udcc -c udc_atten=0:14 -t 2 2>&1' -n udccc
sy=popen 's_client -h udcd -c udc_atten=0:5  -t 2 2>&1' -n udccd
sy=popen 's_client -h udca -c udc_atten=1:17 -t 2 2>&1' -n udcca
sy=popen 's_client -h udcb -c udc_atten=1:13 -t 2 2>&1' -n udccb
sy=popen 's_client -h udcc -c udc_atten=1:8  -t 2 2>&1' -n udccc
sy=popen 's_client -h udcd -c udc_atten=1:0  -t 2 2>&1' -n udccd
"
sy=popen 's_client -h rfd  -c rfd_atten=0:3  -t 2 2>&1' -n rfdcn
sy=popen 's_client -h rfd  -c rfd_atten=1:3  -t 2 2>&1' -n rfdcn
sy=popen 's_client -h rfd  -c rfd_atten=2:9  -t 2 2>&1' -n rfdcn
sy=popen 's_client -h rfd  -c rfd_atten=3:9  -t 2 2>&1' -n rfdcn
enddef
"
"=================================
"=================================
"
define  preses_@hds@   00000000000x
" Duration: 4 sec
rdbe_status
mk6=dts_id?;
rdbe_version
mk6=mstat?
mk6in
mk6=input_stream = delete ;
!+4s
mk6=input_stream = add : rdbeA : vdif : 8224 : 42 : 66 : eth2 : 127.0.0.1 : 12000;
mk6=input_stream = add : rdbeB : vdif : 8224 : 42 : 66 : eth3 : 127.0.0.1 : 12000;
mk6=input_stream = add : rdbeC : vdif : 8224 : 42 : 66 : eth4 : 127.0.0.1 : 12000;
mk6=input_stream = add : rdbeD : vdif : 8224 : 42 : 66 : eth5 : 127.0.0.1 : 12000;
mk6=input_stream = commit ;
setmode_@mode@   
enddef
"
"=================================
"
define  setmode_@mode@       00000000000x
" Duration: 6 sec
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
data_send=off
!+1s
channels=a,,@chsela@
channels=b,,@chselb@
channels=c,,@chselc@
channels=d,,@chseld@
pcal_offset=a,@pcal_step_5@
pcal_offset=b,@pcal_step_5@
pcal_offset=c,@pcal_step_5@
pcal_offset=d,@pcal_step_5@
!+1s
data_send=on
ifdmon
enddef
"
"=================================
"
define  setscan_@hds@       00000000000x
" Duration: 0 sec
pcalon
tpicd=stop
tpicd=no,100
tpicd
setup_local
enddef
"
"=================================
"
define  preob_@hds@        00000000000x
" Duration: 3 sec
onsource
track
atten=
atten
quantize=
quantize
bstate
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
rdbe_status
channels
pcal_offset
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
