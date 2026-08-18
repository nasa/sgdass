" NASA style of VLBI schedule in proc format for station 
" Station:      YARRA12M   Yg
"
" Template last modification date: 2024.02.25_15:38:54
" Generated on  @update_date@
"
" Hidden procedures: cont_enable cdms clock clkoff checkfb maserdelay phasecal wth
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
enddef
"
"=================================
"=================================
" 
define  preses_@hds@   00000000000x
" Duration: 0 sec
" OK
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
fb=datastream=reset
fb=datastream=clear
fb=datastream=add:xx.vdif:192.168.1.40/yg.*
fb=datastream=add:xy.vdif:192.168.1.42/yg.*
fb=datastream=add:sx.vdif:192.168.1.32/yg.*
fb=datastream=add:sy.vdif:192.168.1.34/yg.*
fb=mode=vdif_8000-1024-8-2
fb=mtu=9000
fb=record=nthread::4
fb=net_port=46227
fb=net_protocol=udpsnor:536870912:536870912:4
"
dbbc3=core3h=1,inputselect vsi1-2-3-4 @if_band_used@
dbbc3=core3h=2,inputselect vsi1-2-3-4 @if_band_used@
dbbc3=core3h=3,inputselect vsi1-2-3-4 @if_band_used@
dbbc3=core3h=4,inputselect vsi1-2-3-4 @if_band_used@
dbbc3=core3h=5,inputselect vsi1-2-3-4 @if_band_used@
dbbc3=core3h=6,inputselect vsi1-2-3-4 @if_band_used@
dbbc3=core3h=7,inputselect vsi1-2-3-4 @if_band_used@
dbbc3=core3h=8,inputselect vsi1-2-3-4 @if_band_used@
"
dbbc3=core3h=1,vsi_samplerate 128000000 4 @if_band_used@
dbbc3=core3h=2,vsi_samplerate 128000000 4 @if_band_used@
dbbc3=core3h=3,vsi_samplerate 128000000 4 @if_band_used@
dbbc3=core3h=4,vsi_samplerate 128000000 4 @if_band_used@
dbbc3=core3h=5,vsi_samplerate 128000000 4 @if_band_used@
dbbc3=core3h=6,vsi_samplerate 128000000 4 @if_band_used@
dbbc3=core3h=7,vsi_samplerate 128000000 4 @if_band_used@
dbbc3=core3h=8,vsi_samplerate 128000000 4 @if_band_used@
"
dbbc3=core3h=1,splitmode on @if_band_used@
dbbc3=core3h=2,splitmode on @if_band_used@
dbbc3=core3h=3,splitmode on @if_band_used@
dbbc3=core3h=4,splitmode on @if_band_used@
dbbc3=core3h=5,splitmode on @if_band_used@
dbbc3=core3h=6,splitmode on @if_band_used@
dbbc3=core3h=7,splitmode on @if_band_used@
dbbc3=core3h=8,splitmode on @if_band_used@
"
dbbc3=core3h=1,vsi_bitmask @sideband@ @sideband@ @sideband@ @sideband@
dbbc3=core3h=2,vsi_bitmask @sideband@ @sideband@ @sideband@ @sideband@
dbbc3=core3h=3,vsi_bitmask @sideband@ @sideband@ @sideband@ @sideband@
dbbc3=core3h=4,vsi_bitmask @sideband@ @sideband@ @sideband@ @sideband@
dbbc3=core3h=5,vsi_bitmask @sideband@ @sideband@ @sideband@ @sideband@
dbbc3=core3h=6,vsi_bitmask @sideband@ @sideband@ @sideband@ @sideband@
dbbc3=core3h=6,vsi_bitmask @sideband@ @sideband@ @sideband@ @sideband@
dbbc3=core3h=7,vsi_bitmask @sideband@ @sideband@ @sideband@ @sideband@
dbbc3=core3h=8,vsi_bitmask @sideband@ @sideband@ @sideband@ @sideband@
"
dbbc3=core3h=1,reset @if_band_used@
dbbc3=core3h=2,reset @if_band_used@
dbbc3=core3h=3,reset @if_band_used@
dbbc3=core3h=4,reset @if_band_used@
dbbc3=core3h=5,reset @if_band_used@
dbbc3=core3h=6,reset @if_band_used@
dbbc3=core3h=7,reset @if_band_used@
dbbc3=core3h=8,reset @if_band_used@
"
dbbc3=core3h=1,vdif_station yg @if_band_used@
dbbc3=core3h=2,vdif_station yg @if_band_used@
dbbc3=core3h=3,vdif_station yg @if_band_used@
dbbc3=core3h=4,vdif_station yg @if_band_used@
dbbc3=core3h=5,vdif_station yg @if_band_used@
dbbc3=core3h=6,vdif_station yg @if_band_used@
dbbc3=core3h=7,vdif_station yg @if_band_used@
dbbc3=core3h=8,vdif_station yg @if_band_used@
"
dbbc3=core3h=1,vdif_frame 2 8 8000 ct=off @if_band_used@
dbbc3=core3h=2,vdif_frame 2 8 8000 ct=off @if_band_used@
dbbc3=core3h=3,vdif_frame 2 8 8000 ct=off @if_band_used@
dbbc3=core3h=4,vdif_frame 2 8 8000 ct=off @if_band_used@
dbbc3=core3h=5,vdif_frame 2 8 8000 ct=off @if_band_used@
dbbc3=core3h=6,vdif_frame 2 8 8000 ct=off @if_band_used@
dbbc3=core3h=7,vdif_frame 2 8 8000 ct=off @if_band_used@
dbbc3=core3h=8,vdif_frame 2 8 8000 ct=off @if_band_used@
"
dbbc3=core3h=1,regupdate vdif_header 3 @core_by_2^16@ 0x03FF0000
dbbc3=core3h=2,regupdate vdif_header 3 @core_by_2^16@ 0x03FF0000
dbbc3=core3h=3,regupdate vdif_header 3 @core_by_2^16@ 0x03FF0000
dbbc3=core3h=4,regupdate vdif_header 3 @core_by_2^16@ 0x03FF0000
dbbc3=core3h=5,regupdate vdif_header 3 @core_by_2^16@ 0x03FF0000
dbbc3=core3h=6,regupdate vdif_header 3 @core_by_2^16@ 0x03FF0000
dbbc3=core3h=7,regupdate vdif_header 3 @core_by_2^16@ 0x03FF0000
dbbc3=core3h=8,regupdate vdif_header 3 @core_by_2^16@ 0x03FF0000
"
dbbc3=core3h=1,timesync @if_band_used@
"
!+5s
dbbc3=core3h=2,timesync @if_band_used@
!+5s
dbbc3=core3h=5,timesync @if_band_used@
!+5s
dbbc3=core3h=6,timesync @if_band_used@
!+5s
dbbc3=core3h=1,destination 0 192.168.1.53:46227 @if_band_used@
dbbc3=core3h=2,destination 0 192.168.1.54:46227 @if_band_used@
dbbc3=core3h=5,destination 0 192.168.1.53:46227 @if_band_used@
dbbc3=core3h=6,destination 0 192.168.1.54:46227 @if_band_used@
"
dbbc3=core3h=1,destination 1 none @if_band_used@
dbbc3=core3h=2,destination 1 none @if_band_used@
dbbc3=core3h=3,destination 1 none @if_band_used@
dbbc3=core3h=4,destination 1 none @if_band_used@
dbbc3=core3h=5,destination 1 none @if_band_used@
dbbc3=core3h=6,destination 1 none @if_band_used@
dbbc3=core3h=7,destination 1 none @if_band_used@
dbbc3=core3h=8,destination 1 none @if_band_used@
dbbc3=pps_sync
!+2s
dbbc3=core3h=1,@start_or_stop_vdif@
dbbc3=core3h=2,@start_or_stop_vdif@
dbbc3=core3h=3,@start_or_stop_vdif@
dbbc3=core3h=4,@start_or_stop_vdif@
dbbc3=core3h=5,@start_or_stop_vdif@
dbbc3=core3h=6,@start_or_stop_vdif@
dbbc3=core3h=7,@start_or_stop_vdif@
dbbc3=core3h=8,@start_or_stop_vdif@
" pcalon
tpicd=stop
fb_mode
fb_config
@dbbc3_bbc@
"
ifa=1,agc,32000 @if_band_used@
ifb=1,agc,32000 @if_band_used@
ifc=2,agc,32000 @if_band_used@
ifd=2,agc,32000 @if_band_used@
ife=2,agc,32000 @if_band_used@
iff=2,agc,32000 @if_band_used@
ifg=2,agc,32000 @if_band_used@
ifh=2,agc,32000 @if_band_used@
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
bbc_gain=all,agc
tpicd=no,100
tpicd
enddef
"
"=================================
"
define  setscan_@hds@   00000000000x
" Duration: 0 sec
"pcalon
"cont_enable
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
wth
sy=popen -n uptime uptime &
clkoff
maserdelay
dbbc3=pps_delay
fb=record?
disk_record=on
disk_record
data_valid=on
!+1s
fb=record?
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
