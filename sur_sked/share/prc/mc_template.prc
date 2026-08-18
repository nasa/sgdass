" NASA style of VLBI schedule in proc format.
" Station:      MEDICINA   Mc
"
" Template last modification date: 2024.02.26_23:37:34
" Last update:  @update_date@
"
" Hidden procedures: check_ntp gps-fmout jive5ab medconf scu vertex 
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
cal=on
!+4s
tpical=formbbc,formif
cal=off
!+1s
tpdiff=formbbc,formif
caltemp=formbbc,formif
tsys=formbbc,formif
enddef
"
"=================================
"
define  checkfb       00000000000x
mk5_status
scan_check
mk5c_mode
jive5ab=mode?
"write out some bytes from beginning of the last scan
!+1s
mk5=scan_set=::+200000000
!+1s
mk5=disk2file=/usr2/oper/data/systest.vdif:::w
!+3s
sy=exec /usr2/oper/bin/checkdata.py `lognm` /usr2/oper/data/systest.vdif &
mk5=rtime?
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
define  ready_disk    00000000000x
mk5=dts_id?;
mk5_status
mk5=os_rev?;
enddef
"
"=================================
"
define  iread         00000000000x
ifa
ifb
ifc
ifd
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
cal=connect
jive5ab=version?
check_ntp
!+3s
setmode_@mode@
enddef
"
"=================================
"
define  setmode_@mode@   00000000000x
" Duration: 0 sec
@time_stamp@
pcalon
tpicd=stop
"
"vsi1-2 input should be used in 'equip.ctl'
"... vsi1 or vsi2 are also supported
fila10g_mode=,0xffffffff,,@two_if_width@
fila10g_mode
fb_mode=vdif,0xffffffff,,@two_if_width@
fb_mode
form=geo
form
threadsx
fb_config
"
ifa=2,agc,2,42000
ifb=2,agc,1,49000
ifc=2,agc,2,42000
ifd=2,agc,2,42000
"
lo=
lo=loa,@lo@,usb,rcp,1
lo=lob,@lo@,usb,rcp,1
lo=loc,@lo@,usb,rcp,1
lo=lod,@lo@,usb,rcp,1
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
bbc13=@if_offset@,d,@if_width@
bbc14=@if_offset@,d,@if_width@
"
cont_cal=off 
bbc_gain=all,agc
tpicd=no,0
tpicd
enddef
"
"=================================
"
define  setscan_@hds@   00000000000x
" Duration: 1 sec
checkfb
enddef
"
"=================================
"
define  preob_@hds@     00000000000x
" Duration: 10 sec
onsource
if=cont_cal,cal=cont,cal=off
if=cont_cal,tpicd=tsys,!*
if=cont_cal,,!*+2s
if=cont_cal,,caltsys_man
enddef
"
"=================================
"
define  midob_@hds@     00000000000x
" Duration: 3 sec
disk_record=on
disk_record
data_valid=on
"
fila10g=time
sy=run setcl &
track
medconf
iread
bread
form
fb_mode
dbbc=pps_delay
gps-fmout
vertex=get lofreq
scu
wx
cont_cal
cal
!+3s
onsource
enddef
"
"=================================
"
define  postob_@hds@    00000000000x
" Duration: 1 sec
data_valid=off
disk_record=off
onsource
if=cont_cal,,ifagc
if=cont_cal,,if=ddc\,bbc_gain=all\\\,agc
gps-fmout
enddef
"
"=================================
"
define  postses_@hds@        00000000000x
" Duration: 0 sec
" End of schedule
sched_end
enddef
