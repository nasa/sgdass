" NASA style of VLBI schedule in proc format. 
" Station:      NYALES20   Ny
"
" Template last modification date: 2023.06.29_08:47:31
" Last update:  @update_date@
"
" S/X setup at 1Gbps
"
@vers@
define  proc_library  00000000000x
enddef
"
" ================================
"
define  exper_initi   00000000000x
sched_initi
enddef
"
" ================================
"
define  sched_initi   00000000000x
proc_library
preses_@hds@
enddef
"
" ================================
"
define  lighton       00000000000x
rx2=set,1,on
enddef
"
" ================================
"
define  lightoff      00000000000x
rx2=set,1,off
enddef
"
"=================================
"
define  pcalon        00000000000x
"no phase cal control is implemented here
enddef
"
"=================================
"
define  mott          00000000000x
rx2=get,0
rx2=get,2
rx2=get,3
rx2=get,4
rx2=get,5
rx2=get,6
rx2=get,17
rx2=get,7
rx2=get,8
rx2=get,16
rx2=get,18
rx2=get,19
"wx
enddef
"
" ================================
"
define  checkmk5      22115170027x
scan_check
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5_status
mk5=bank_set?
enddef
"
" ================================
"
define  ready_disk    22115164043
mk5close
xdisp=on
"mount the mark5 discs for this experiment now
"recording will begin at current position
"enter 'mk5relink' when ready or
"if you can't get the mk5 going then
"enter 'cont' to continue without the mk5
xdisp=off
halt
disk_serial
disk_pos
bank_check
mk5=dts_id?
mk5=os_rev?
mk5=ss_rev?
mk5_status
enddef
"
"=================================
"
define  skmon         22115170004
rx2=get,0
rx2=get,2
rx2=get,3
rx2=get,4
rx2=get,24
rx2=get,17
rx2=get,16
rx2=get,7
rx2=get,8
rx2=get,18
enddef
"
" ===============================
"
define  iread         22115170004
if=ifa,ifa
if=ifb,ifb
if=ifc,ifc
if=ifd,ifd
enddef
"
" ================================
"
define  bread         22115170004x
if=bbc01,bbc01
if=bbc02,bbc02
if=bbc03,bbc03
if=bbc04,bbc04
if=bbc05,bbc05
if=bbc06,bbc06
if=bbc07,bbc07
if=bbc08,bbc08
if=bbc09,bbc09
if=bbc10,bbc10
if=bbc11,bbc11
if=bbc12,bbc12
if=bbc13,bbc13
if=bbc14,bbc14
if=bbc15,bbc15
if=bbc16,bbc16
if=pfb,if=core1\,pfb1
if=pfb,if=core2\,pfb2
if=pfb,if=core3\,pfb3
if=pfb,if=core4\,pfb4
enddef
"
" ================================
"
define  clocko        22115170004x
hpib=gp, calc:data?
!+2s
gps-fmout=gp
enddef
"
" =================================
" =================================
"
define  preses_@hds@ 00000000000x
" Duration: 0 sec
sy=touch /tmp/fs_watchdog &
lighton
jive5ab=version?
mk5=bank_set?
mott@
mk5=dts_id?
mk5=os_rev?
mk5=ss_rev?
mk5_status
dbbc=version
ready_disk
enddef
"
" =================================
"
define  setmode_@mode@    00000000000x
" Duration: 2 sec
@time_stamp@
" set observing mode @mode@
" set the lo stream
"
ifa=4,agc,2
ifb=4,agc,1
ifc=4,agc,2
ifd=4,agc,2
lo=
lo=loa,@lo@,usb,rcp,1
lo=lob,@lo@,usb,rcp,1
lo=loc,@lo@,usb,rcp,1
lo=lod,@lo@,usb,rcp,1
bbc09=@if_offset@,c,@if_width@
bbc10=@if_offset@,c,@if_width@
bbc11=@if_offset@,c,@if_width@
bbc12=@if_offset@,c,@if_width@
bbc13=@if_offset@,d,@if_width@
bbc14=@if_offset@,d,@if_width@
bbc01=@if_offset@,a,@if_width@
bbc02=@if_offset@,a,@if_width@
bbc03=@if_offset@,a,@if_width@
bbc04=@if_offset@,a,@if_width@
bbc05=@if_offset@,b,@if_width@
bbc06=@if_offset@,b,@if_width@
bbc07=@if_offset@,b,@if_width@
bbc08=@if_offset@,b,@if_width@
bread
enddef
"
" =================================
"
define  setscan_@hds@    00000000000x
" Duration: 2 sec
pcalon
tpicd=stop
"Recorder may be wired to vsi1 or vsi2
mk5b_mode=ext,0xffffffff,,32.000
mk5b_mode
form=geo
form
setmode_@mode@
cont_cal=off
bbc_gain=all,agc
tpicd=no,500
bank_check
mk5=bank_set?
tpicd
enddef
"
" =================================
"
define  preob_@hds@       00000000000x
" Duration: 6 sec
if=cont_cal,tpicd=tsys,!*
onsource
if=cont_cal,,!*+4s
if=cont_cal,,caltsys_man
enddef
"
" =================================
"
define  midob_@hds@       00000000000x
" Duration: 0 sec
disk_pos
disk_record=on
disk_record
data_valid=on
onsource
track
skmon
cable
iread
bread
clocko
mk5b_mode
!+1s
mk5=dot?
sy=run setcl adapt &
enddef
"
" =================================
"
define  postob_@hds@      00000000000x
" Duration: 2 sec
data_valid=off
disk_record=off
disk_pos
mk5=pointers?
scan_check
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5_status
mk5=bank_set?
enddef
"
" =================================
"
define  postses_@hds@      00000000000x
" Duration: 0 sec
" End of schedule at station ny
sched_end
enddef
