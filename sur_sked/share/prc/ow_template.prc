" NASA style of VLBI schedule in proc format.
" Station:      ONSA13SW   Ow
"
" Template last modification date: 2024.05.17_22:13:12
" Last update:  @update_date@
"
" Hidden procedures: pcalon cont_enable cdms clock checkfb init_skirner
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
define  iread         00000000000x
ifa
ifb
ifc
ifd
ife
iff
ifg
ifh
enddef
"
"=================================
"
define  core3hbb      00000000000x
core3h=1,vsi_bitmask @sideband@
core3h=2,vsi_bitmask @sideband@
core3h=3,vsi_bitmask @sideband@
core3h=4,vsi_bitmask @sideband@
core3h=5,vsi_bitmask @sideband@
core3h=6,vsi_bitmask @sideband@
core3h=7,vsi_bitmask @sideband@
core3h=8,vsi_bitmask @sideband@
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
"=================================
"=================================
" 
define  preses_@hds@   00000000000x
" Duration: 0 sec
" OK
init_skirner
proc_library
fb=dts_id?
fb=os_rev?
fb_status
enddef
"
"=================================
"
define  setmode_@mode@   00000000000x
" Duration: 6 sec
@time_stamp@
pcalon
tpicd=stop
core3hbb=$
fb_mode=vdif,,,64
fb_mode
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
cont_cal=on,2
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
cont_cal=on,2
bbc_gain=all,agc,12000
enddef
"
"=================================
"
define  preob_@hds@     00000000000x
" Duration: 0 sec
iread
bread
cont_cal=off
cont_cal=on
enddef
"
"=================================
"
define  midob_@hds@     00000000000x
" Duration: 0 sec
onsource
wx
sy=cdms_delay
clock
fb=record?
fb=evlbi?
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
fb=evlbi?
sy=bash /usr2/oper/bin/fbspace.sh &
checkfb
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
