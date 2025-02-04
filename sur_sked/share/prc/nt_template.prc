" NASA style of VLBI schedule in proc format.
" Station:      NOTO       Nt
"
" Template last modification date: 2024.02.26_23:37:26
" Last update:  @update_date@
"
" Hidden procedures: gps-fmout 
" Backend: mark5b
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
define  caltsys_man   00000000000x
ifman
if=ddc,bbc_gain=all\,man
!+2s
tpi=formbbc,formif
calon
!+2s
tpical=formbbc,formif
caloff
tpdiff=formbbc,formif
caltemp=formbbc,formif
tsys=formbbc,formif
ifagc
if=ddc,bbc_gain=all\,agc
enddef
"
"=================================
"
define  checkfb       00000000000x
scan_check
fb_status
!+1s
fb=scan_set=::+20000000
!+1s
fb=disk2file=systest.vdif:::w
!+3s
sy=exec /usr2/oper/bin/checkdata.py `lognm` /usr2/log/systest.vdif &
fb=rtime?
enddef
"
"=================================
"
define  ifman         00000000000x
if=ifa,ifa=*\,man\,*\,*
if=ifb,ifb=*\,man\,*\,*
if=ifc,ifc=*\,man\,*\,*
if=ifd,ifd=*\,man\,*\,*
enddef
"
"=================================
"
define  ifagc         00000000000x
if=ifa,ifa=*\,agc\,*\,*
if=ifb,ifb=*\,agc\,*\,*
if=ifc,ifc=*\,agc\,*\,*
if=ifd,ifd=*\,agc\,*\,*
enddef
"
"=================================
"
define  calon         00000000000x
cal=on
enddef
"
"=================================
"
define  caloff        00000000000x
cal=off
enddef
"
"=================================
"
define  ready_disk    00000000000x
fb=dir_info?
enddef
"
"=================================
"
define  iread         00000000000x
if=ifa,ifa
if=ifb,ifb
if=ifc,ifc
if=ifd,ifd
enddef
"
"=================================
"
define  bread         00000000000x
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
"=================================
"
define  threadsx      00000000000x
fb=datastream=clear
fb=datastream=reset
enddef
"
"=================================
"
define  fbconfig      00000000000x
fb=net_port?
fb=net_protocol?
fb=record?nthread
enddef
"
"=================================
"
define  check_ntp      00000000000x
sy=popen 'uptime 2>&1' -n uptime &
sy=popen 'ntpq -np 2>&1|grep -v "^[- x#]" 2>&1' -n ntpq &
enddef
"
"=================================
"
define  gpsform
gps-fmout=read
enddef
"
" =================================
" =================================
"
define  preses_@hds@   00000000000x
" Duration: 3 sec
fb=dts_id?
fb=os_rev?
fb_status
dbbc=version
fila10g=version
check_ntp
!+3s
setmode_@mode@
enddef
"
"=================================
"
define  setmode_@mode@   00000000000x
" Duration: 0 sec
"
@time_stamp@
pcalon
tpicd=stop
"vsi1-2 input should be used in 'equip.ctl'
"... vsi1 or vsi2 are also supported
fila10g_mode=,0xffffffff,,@two_if_width@
fila10g_mode
fb_mode=vdif,0xffffffff,,@two_if_width@
fb_mode
form=geo
form
fb_config
"
ifa=1,agc,4,42000
ifb=1,agc,3,42000
ifc=2,agc,2,42000
"
lo=
lo=loa,@lo@,usb,rcp,1
lo=lob,@lo@,usb,rcp,1
lo=loc,@lo@,usb,rcp,1
"
bbc01=@if_offset@,a,@if_width@
bbc02=@if_offset@,a,@if_width@
bbc03=@if_offset@,a,@if_width@
bbc04=@if_offset@,a,@if_width@
bbc05=@if_offset@,b,@if_width@
bbc06=@if_offset@,b,@if_width@
bbc07=@if_offset@,b,@if_width@
bbc08=@if_offset@,b,@if_width@
bbc09=@if_offset@,c,@if_width@
bbc10=@if_offset@,c,@if_width@
bbc11=@if_offset@,c,@if_width@
bbc12=@if_offset@,c,@if_width@
bbc13=@if_offset@,c,@if_width@
bbc14=@if_offset@,c,@if_width@
"
cont_cal=off
bbc_gain=all,agc,16000
tpicd=no,1500
tpicd
enddef
"
"=================================
"
define  setscan_@hds@   00000000000x
" Duration: 0 sec
checkfb
enddef
"
"=================================
"
define  preob_@hds@     00000000000x
" Duration: 10 sec
if=cont_cal,cal=cont,cal=off
if=cont_cal,tpicd=tsys,!*
onsource
if=cont_cal,,!*+4s
if=cont_cal,,caltsys_man
enddef
"
"=================================
"
define  midob_@hds@     00000000000x
" Duration: 1 sec
disk_record=on
disk_record
data_valid=on
"
wx
iread
bread
" the shown order of the commands from here to the end of this procedure is
" strongly recommended
" add your station command to measure the gps to fm output clock offset
fb_mode
gpsform
sy=run setcl &
if=ddc,dbbc=pps_delay
track
cont_cal
cal
fb=mode?
fila10g=sysstat
fila10g=time
fb=record?
onsource
enddef
"
"=================================
"
define  postob_@hds@    00000000000x
" Duration: 0 sec
data_valid=off
disk_record=off
onsource
enddef
"
"=================================
"
define  postses_@hds@        00000000000x
" Duration: 0 sec
" End of schedule
sched_end
enddef
