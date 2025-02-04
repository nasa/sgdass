" NASA style of VLBI schedule in proc format. 
" Station:      RAEGSMAR   Sa
"
" Template last modification date: 2024.05.20_12:36:06
" Last update:  @update_date@
"
" Hidden procedures: phasecal gps-fmout CDMS
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
bbc001
bbc002
bbc003
bbc004
bbc005
bbc006
bbc007
bbc008
bbc009
bbc010
bbc011
bbc012
bbc013
bbc014
bbc015
bbc016
bbc017
bbc018
bbc019
bbc020
bbc021
bbc022
bbc023
bbc024
bbc025
bbc026
bbc027
bbc028
bbc029
bbc030
bbc031
bbc032
bbc033
bbc034
bbc035
bbc036
bbc037
bbc038
bbc039
bbc040
bbc041
bbc042
bbc043
bbc044
bbc045
bbc046
bbc047
bbc048
bbc049
bbc050
bbc051
bbc052
bbc053
bbc054
bbc055
bbc056
bbc057
bbc058
bbc059
bbc060
bbc061
bbc062
bbc063
bbc064
enddef
"
"=================================
"
define  flexmk6       00000000000
mk6=set_disks=mk6
mk6=mtu=9000
mk6=mode=VDIF_8192-8192-64-2
mk6=net_protocol=udpsnor:256M:256M
mk6=datastream=clear
mk6=datastream=add:{thread}:*
mk6=record=nthread:4:4
mk6=datastream=reset
enddef
"
"=================================
"
define  core3h_hv
band@a@dbbc3=core3h=1,vsi_bitmask 0x0 0x0 0x0 @sideband@
band@a@dbbc3=core3h=2,vsi_bitmask 0x0 0x0 0x0 @sideband@
band@b@dbbc3=core3h=3,vsi_bitmask 0x0 0x0 0x0 @sideband@
band@b@dbbc3=core3h=4,vsi_bitmask 0x0 0x0 0x0 @sideband@
band@c@dbbc3=core3h=5,vsi_bitmask 0x0 0x0 0x0 @sideband@
band@c@dbbc3=core3h=6,vsi_bitmask 0x0 0x0 0x0 @sideband@
band@d@dbbc3=core3h=7,vsi_bitmask 0x0 0x0 0x0 @sideband@
band@d@dbbc3=core3h=8,vsi_bitmask 0x0 0x0 0x0 @sideband@
core3h_mode
enddef
"
"=================================
"
define  core3hbb      22361151610x
core3h_mode=begin,$
core3h_mode=1,,@sideband@,,64.0,$
core3h_mode=2,,@sideband@,,64.0,$
core3h_mode=3,,@sideband@,,64.0,$
core3h_mode=4,,@sideband@,,64.0,$
core3h_mode=5,,@sideband@,,64.0,$
core3h_mode=6,,@sideband@,,64.0,$
core3h_mode=7,,@sideband@,,64.0,$
core3h_mode=8,,@sideband@,,64.0,$
core3h_mode=end,$
enddef
"
"=================================
"=================================
"=================================
" 
define  preses_@hds@   00000000000x
" Duration: 0 sec
" OK
proc_library
mk6=status?
dbbc3=version
pcalon
cont_cal=off
enddef
"
"=================================
"
define  setmode_@mode@   00000000000x
" Duration: 6 sec
@time_stamp@
tpicd=stop
core3hbb=$
core3h_hv
"
bbc_gain=all,agc,12000
"
ifa=1,agc,32000
ifb=1,agc,32000
ifc=1,agc,32000
ifd=1,agc,32000
ife=1,agc,32000
iff=1,agc,32000
ifg=1,agc,32000
ifh=1,agc,32000
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
lo_config
@dbbc3_bbc@
tpicd=no,100
tpicd
enddef
"
"=================================
"
define  setscan_@hds@   00000000000x
" Duration: 0 sec
pcalon
cont_cal=on,1
bbc_gain=all,agc,12000
enddef
"
"=================================
"
define  preob_@hds@     00000000000x
" Duration: 0 sec
enddef
"
"=================================
"
define  midob_@hds@     00000000000x
" Duration: 0 sec
onsource
wx
iread
bread
gps-fmout
mk6=mode?
data_valid=on
!+1s
sy=rte_go setcl adapt &
sy=rte_go setcl &
enddef
"
"=================================
"
define  postob_@hds@    00000000000x
" Duration: 2 sec
data_valid=off
CDMS=getmeas
mk6=rtime?
mk6=evlbi?
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
