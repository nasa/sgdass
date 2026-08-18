" NASA style of VLBI schedule in proc format.
" Station:      MATVGOS Mb
"
" L. Petrov 2025.07.08  Set 37 second for setmode command
" Template last modification date: 2025.07.08_12:52:47
" Last update:  @update_date@
"
" Hidden procedures: checkfb core3hbb fb_config pcaloff pcalon 
"
@vers@
define  proc_library  00000000000x
enddef
"
" ================================
"
define exper_initi    00000000000x
proc_library
sched_initi
enddef
"
 =================================
"
define  sched_initi   00000000000x
preses_@hds@
setmode_@mode@   
enddef
"
"=================================
"
define  sched_end     00000000000x
source=azel,0d,80d
enddef
"
"=================================
"
define  stow          00000000000x
source=azel,0d,80d
antenna=stop
antenna=stow
enddef
"
"=================================
"
define  checkfb       00000000000x
"scan_check
fb_status
fb=net_protocol?
fb=mtu?
fb=rtime?
enddef
"
"=================================
"
define  iread         25125100000x
if=ifa,ifa,
if=ifb,ifb,
if=ifc,ifc,
if=ifd,ifd,
if=ife,ife,
if=iff,iff,
if=ifg,ifg,
if=ifh,ifh,
enddef
"
"=================================
"
define  core3hbb      00000000000x
core3h=1,vsi_bitmask 0x0 0x0 0x0 @sideband@
core3h=2,vsi_bitmask 0x0 0x0 0x0 @sideband@
core3h=3,vsi_bitmask 0x0 0x0 0x0 @sideband@
core3h=4,vsi_bitmask 0x0 0x0 0x0 @sideband@
core3h=5,vsi_bitmask 0x0 0x0 0x0 @sideband@
core3h=6,vsi_bitmask 0x0 0x0 0x0 @sideband@
core3h=7,vsi_bitmask 0x0 0x0 0x0 @sideband@
core3h=8,vsi_bitmask 0x0 0x0 0x0 @sideband@
core3h_mode
core3h_mode=begin,force
core3h_mode=1,,@sideband@,,@two_if_width@,$
core3h_mode=2,,@sideband@,,@two_if_width@,$
core3h_mode=3,,@sideband@,,@two_if_width@,$
core3h_mode=4,,@sideband@,,@two_if_width@,$
core3h_mode=5,,@sideband@,,@two_if_width@,$
core3h_mode=6,,@sideband@,,@two_if_width@,$
core3h_mode=7,,@sideband@,,@two_if_width@,$
core3h_mode=8,,@sideband@,,@two_if_width@,$
core3h_mode=end,force
enddef
"
"=================================
"
define  pcalon        25125093209
phasecal=on
enddef
"
"=================================
"
define  pcaloff       00000000000
phasecal=off
enddef
"
"=================================
"
define  fb_config     25125093236x
" Configure jive5ab for Mv
fb=net_port=2620
fb=net_port?
fb=net_protocol=udpsnor:768M:128M:24
fb=net_protocol?
fb=record=nthread:3:3
fb=record?nthread
fb=datastream=clear
fb=datastream=add:{thread}:*
fb=datastream=reset
enddef
"
"=================================
"=================================
"=================================
" 
define  preses_@hds@   00000000000x
" Duration: 0 sec
" OK
enddef
"
"=================================
"
define  setmode_@mode@   00000000000x
" Duration: 37 sec
@time_stamp@
pcalon
calnoise=80
tpicd=stop
core3hbb=$
fb_mode=vdif,,,@two_if_width@
fb_mode
fb_config
cont_cal=on,1
bbc_gain=all,agc
tpicd=no,100
tpicd
"
fb_config
@dbbc3_bbc@
"
ifa=1,agc,32000
ifb=1,agc,32000
ifc=2,agc,32000
ifd=2,agc,32000
ife=2,agc,32000
iff=2,agc,32000
ifg=2,agc,32000
ifh=2,agc,32000
" set observing mode @mode@
" set the lo stream
lo=
lo=loa,@lo@,@sib@,lcp,5
lo=lob,@lo@,@sib@,rcp,5
lo=loc,@lo@,@sib@,lcp,5
lo=lod,@lo@,@sib@,rcp,5
lo=loe,@lo@,@sib@,lcp,5
lo=lof,@lo@,@sib@,rcp,5
lo=log,@lo@,@sib@,lcp,5
lo=loh,@lo@,@sib@,rcp,5
"
cont_cal=on,1
bbc_gain=all,agc
tpicd=no,100
tpicd
enddef
"
"=================================
"
define  setscan_@hds@   00000000000x
" Duration: 1 sec
pcalon
cont_cal=on,1
bbc_gain=all,agc
enddef
"
"=================================
"
define  preob_@hds@     00000000000x
" Duration: 2 sec
iread
bread
onsource
track
cont_cal=off
cont_cal=on
enddef
"
"=================================
"
define  midob_@hds@     00000000000x
" Duration: 1 sec
onsource
wx
gps-fmout
mk6=mode?
!+1s
sy=rte_go setcl adapt &
sy=rte_go setcl &
%
disk_record=on
disk_record
data_valid=on
enddef
"
"=================================
"
define  postob_@hds@    00000000000x
" Duration: 1 sec
data_valid=off
disk_record=off
CDMS=getmeas
"checkmk6
fb=record?
fb=evlbi?
fb=scan_check?
!+1s
enddef
"
"=================================
"
define  postses_@hds@   00000000000x
" Duration: 0 sec
" End of schedule
sched_end
enddef
