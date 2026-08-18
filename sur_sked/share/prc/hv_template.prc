" NASA style of VLBI schedule in proc format.
" Station:      HARTVGS Hv
"
" Template last modification date: 2025.04.23_14:10:56
" Last update:  @update_date@
"
" Hidden procedures: pcalon cont_enable cdms clock checkfb init_gyller 
"
@vers@
define  proc_library  00000000000x
enddef
"
" ================================
"
define  exper_initi   00000000000x
proc_library
sched_initi
fb=dts_id?
fb=os_rev?
fb_status
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
define  checkfb       00000000000x
scan_check
mk5_status
mk5=net_protocol?
mk5=mtu?
mk5=rtime?
"chk_vgos_ddc
enddef
"
"=================================
"
define  sched_end     00000000000x
source=stow
"sy=exec lgput `lognm`.log hv &
enddef
"
"=================================
"
define  cont_enable   00000000000x
cont_enable/cal80hz
cont_enable/cont_cal=on,0,80,0
enddef
"
"=================================
"
define  cal80hz       00000000000x
sy=popen 'send_pcal set att diode 8' -n send_pcal
sy=popen 'send_pcal set diode 80Hz' -n send_pcal
enddef
"
"=================================
"
define  pcalon        00000000000x
sy=popen 'send_pcal set att pulse 7' -n send_pcal
sy=popen 'send_pcal set pulse on' -n send_pcal
enddef
"
"=================================
"
define  iread         00000000000x
if=ifa,ifa
if=ifb,ifb
if=ifc,ifc
if=ifd,ifd
if=ife,ife
if=iff,iff
if=ifg,ifg
if=ifh,ifh
if=ifa,iftpa
if=ifb,iftpb
if=ifc,iftpc
if=ifd,iftpd
if=ife,iftpe
if=iff,iftpf
if=ifg,iftpg
if=ifh,iftph
enddef
"
"=================================
"
define  fb_config     00000000000x
" Configure jive5ab
fb=net_port=192.168.2.1\@2630=ah:192.168.2.1\@2631=av:192.168.3.1\@2630=bh:192.168.3.1\@2631=bv:192.168.4.1\@2630=ch:192.168.4.1\@2631=cv:192.168.5.1\@2630=dh:192.168.5.1\@2631=dv
fb=net_port?
fb=record?nthread
fb=datastream=clear
fb=datastream=reset
"
"=================================
"
define  checksyns     00000000000x
checksyn=1
checksyn=2
checksyn=3
checksyn=4
enddef
"
"=================================
"
define  checksyn      00000000000x
dbbc3=synth=$,s1
dbbc3=synth=$,mod?
dbbc3=synth=$,cw?
dbbc3=synth=$,att?
dbbc3=synth=$,oen?
dbbc3=synth=$,lk1?
dbbc3=synth=$,s2
dbbc3=synth=$,mod?
dbbc3=synth=$,cw?
dbbc3=synth=$,att?
dbbc3=synth=$,oen?
dbbc3=synth=$,lk2?
enddef
"
"=================================
"
define  bread         00000000000x
if=bbc001,bbc001
if=bbc002,bbc002
if=bbc003,bbc003
if=bbc004,bbc004
if=bbc005,bbc005
if=bbc006,bbc006
if=bbc007,bbc007
if=bbc008,bbc008
if=bbc009,bbc009
if=bbc010,bbc010
if=bbc011,bbc011
if=bbc012,bbc012
if=bbc013,bbc013
if=bbc014,bbc014
if=bbc015,bbc015
if=bbc016,bbc016
if=bbc017,bbc017
if=bbc018,bbc018
if=bbc019,bbc019
if=bbc020,bbc020
if=bbc021,bbc021
if=bbc022,bbc022
if=bbc023,bbc023
if=bbc024,bbc024
if=bbc025,bbc025
if=bbc026,bbc026
if=bbc027,bbc027
if=bbc028,bbc028
if=bbc029,bbc029
if=bbc030,bbc030
if=bbc031,bbc031
if=bbc032,bbc032
if=bbc033,bbc033
if=bbc034,bbc034
if=bbc035,bbc035
if=bbc036,bbc036
if=bbc037,bbc037
if=bbc038,bbc038
if=bbc039,bbc039
if=bbc040,bbc040
if=bbc041,bbc041
if=bbc042,bbc042
if=bbc043,bbc043
if=bbc044,bbc044
if=bbc045,bbc045
if=bbc046,bbc046
if=bbc047,bbc047
if=bbc048,bbc048
if=bbc049,bbc049
if=bbc050,bbc050
if=bbc051,bbc051
if=bbc052,bbc052
if=bbc053,bbc053
if=bbc054,bbc054
if=bbc055,bbc055
if=bbc056,bbc056
if=bbc057,bbc057
if=bbc058,bbc058
if=bbc059,bbc059
if=bbc060,bbc060
if=bbc061,bbc061
if=bbc062,bbc062
if=bbc063,bbc063
if=bbc064,bbc064
enddef
"
"=================================
"
define  caltsys       00000000000x
if=cont_cal,tpicd=tsys,caltsys_man
if=cont_cal,,ifagc
if=cont_cal,,if=ddc\,bbc_gain=all\\\,agc
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
"
define  threadids     00000000000x
dbbc3=core3h=1,regupdate vdif_header 3 @core_by_2^16@ 0x03FF0000
dbbc3=core3h=2,regupdate vdif_header 3 @core_by_2^16@ 0x03FF0000
dbbc3=core3h=3,regupdate vdif_header 3 @core_by_2^16@ 0x03FF0000
dbbc3=core3h=4,regupdate vdif_header 3 @core_by_2^16@ 0x03FF0000
dbbc3=core3h=5,regupdate vdif_header 3 @core_by_2^16@ 0x03FF0000
dbbc3=core3h=6,regupdate vdif_header 3 @core_by_2^16@ 0x03FF0000
dbbc3=core3h=7,regupdate vdif_header 3 @core_by_2^16@ 0x03FF0000
dbbc3=core3h=8,regupdate vdif_header 3 @core_by_2^16@ 0x03FF0000
enddef
"
"=================================
"=================================
"=================================
" 
define  preses_@hds@   00000000000x
" Duration: 0 sec
" OK
sy=send_cntr *rst
sy=send_cntr conf:tint
sy=send_cntr inp1:coup dc\; slop pos\; nrej on\; filt off\; imp 10000000\; lev:abs 1.8
sy=send_cntr inp2:coup dc\; slop pos\; nrej on\; filt off\; imp 10000000\; lev:abs 1.8
sy=send_cntr sample:count max
sy=send_cntr init:imm
dbbc3=version
dbbc3=core3h=1,version
dbbc3=core3h=2,version
dbbc3=core3h=3,version
dbbc3=core3h=4,version
dbbc3=core3h=5,version
dbbc3=core3h=6,version
dbbc3=core3h=7,version
dbbc3=core3h=8,version
fb=scan_check=bytes_to_read:256k
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
threadids
fb_mode=vdif,,,@two_if_width@
fb_mode
fb_config
@dbbc3_bbc@
"
ifa=1,agc,16000
ifb=1,agc,16000
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
cont_enable
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
" Duration: 0 sec
onsource
getwx
sample
mcast_time
CDMS=getmeas
fb=record?
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
fb=evlbi?
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
