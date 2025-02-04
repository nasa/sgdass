" VLBI schedule in proc format. NASA style.
" Last update: 2022.02.20_22:54:13
"
@vers@
define  proc_library  00000000000x
enddef
"
" =================================
"
define  sched_initi   00000000000x
preses_@mode@
enddef
"
"=================================
"=================================
"
define  preses_@hds@   00000000000x
" Duration: 0 sec
sched_initi
mk5=dts_id?
mk5=os_rev?
mk5=ss_rev?
mk5_status
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
ifdab=0,0,nor,nor
ifdcd=0,0,nor,nor
lo=
lo=loa,@lo@,usb,rcp,1
lo=lob,@lo@,usb,rcp,1
lo=loc,@lo@,usb,rcp,1
bbc01=@if_offst@,a,@if_width@,@if_width@
bbc02=@if_offst@,a,@if_width@,@if_width@
bbc03=@if_offst@,a,@if_width@,@if_width@
bbc04=@if_offst@,a,@if_width@,@if_width@
bbc05=@if_offst@,c,@if_width@,@if_width@
bbc06=@if_offst@,c,@if_width@,@if_width@
bbc07=@if_offst@,c,@if_width@,@if_width@
bbc08=@if_offst@,c,@if_width@,@if_width@
bbc09=@if_offst@,b,@if_width@,@if_width@
bbc10=@if_offst@,b,@if_width@,@if_width@
bbc11=@if_offst@,b,@if_width@,@if_width@
bbc12=@if_offst@,b,@if_width@,@if_width@
bbc13=@if_offst@,b,@if_width@,@if_width@
bbc14=@if_offst@,b,@if_width@,@if_width@
enddef
"
"=================================
"
define  setscan_@hds@   00000000000x
" Duration: 0 sec
pcalon
tpicd=stop
mk5b_mode=ext,0x@mk5_mode@,,@mk5_if_width@
mk5b_mode
vsi4=geo
vsi4
setmode_@mode@
tpicd=no,0
bank_check
mk5=bank_set?
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
stop_mlog
" End of schedule
sched_end
enddef
