" NASA style of VLBI schedule in proc format for station 
" Station:      NYALE13N   Nn
"
" Template last modification date: 2024.06.13_22:30:38
" Generated on  @update_date@
"
" Hidden procedures: phasecal cont_enable cdms clock checkfb dbbc3_check check_ntp  jive5ab  mcast_time
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
define  core3hbb      00000000000x
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
proc_library
"
" Check DBBC3 timing etc
"
dbbc3_check
"
" check timing
"
clock
"
" check ntp
"
check_ntp
"
" Configure fivept and onoff
"
fivept=azel,-2,9,0.4,1,057u
onoff=2,2,,,,,all
"
" Check jive5ab
"
jive5ab=version?
jive5ab=rtime?
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
lo=loa,@lo@,@sib@,lcp
lo=lob,@lo@,@sib@,rcp
lo=loc,@lo@,@sib@,lcp
lo=lod,@lo@,@sib@,rcp
lo=loe,@lo@,@sib@,lcp
lo=lof,@lo@,@sib@,rcp
lo=log,@lo@,@sib@,lcp
lo=loh,@lo@,@sib@,rcp
cont_enable
bbc_gain=all,agc
tpicd=no,100
tpicd
enddef
"
"=================================
"
define  setscan_@hds@   00000000000x
" Duration: 0 sec
pcalon
cont_enable
bbc_gain=all,agc
enddef
"
"=================================
"
define  preob_@hds@     00000000000x
" Duration: 0 sec
iread
bread
enddef
"
"=================================
"
define  midob_@hds@     00000000000x
" Duration: 1 sec
onsource
wx
"measure cable via CDMS
cdms
clock
fb=record?
fb=evlbi?
mcast_time
disk_record=on
disk_record
data_valid=on
!+1s
enddef
"
"=================================
"
define  postob_@hds@    00000000000x
" Duration: 1 sec
data_valid=off
disk_record=off
checkfb
!+1s
enddef
"
"=================================
"
define  postses_@hds@   00000000000x
" Duration: 1 sec
" End of schedule
sched_end
enddef
