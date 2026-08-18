" NASA style of VLBI schedule in proc format for station 
" Station:      KATH12M    Ke
"
" Template last modification date: 2024.07.24_23:13:14
" Generated on  @update_date@
"
" Hidden procedures: caltsys clkoff  cont_enable maserdelay  phasecal wth 
"
@vers@
define  proc_library  00000000000x
enddef
"
" =================================
"
define  exper_initi   00000000000x
preses_@hds@
setmode_@mode@
enddef
"
"=================================
"
define  pcalon        00000000000x
" Turn phase cal on
sy=/home/oper/bin/pcal.py on &
enddef
"
"=================================
"
define  checkmk5      00000000000x
"sy=/home/oper/bin/auto_cor_check_web_2bit_8mhz_ke.sh &
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
"=================================
"=================================
" 
define  preses_@hds@   00000000000x
" Duration: 42 sec
" OK
proc_library
sched_initi
mk5=version?
dbbc=version
pcalon
mk5=datastream=clear
mk5=datastream=add:a.vdif:192.168.1.32/ke.*
mk5=datastream=add:b.vdif:192.168.1.34/ke.*
mk5=datastream=add:c.vdif:192.168.1.36/ke.*
mk5=datastream=add:d.vdif:192.168.1.38/ke.*
mk5=datastream=add:e.vdif:192.168.1.40/ke.*
mk5=datastream=add:f.vdif:192.168.1.42/ke.*
mk5=datastream=add:g.vdif:192.168.1.44/ke.*
mk5=datastream=add:h.vdif:192.168.1.46/ke.*
mk5=mode=vdif_8000-1024-8-2
mk5=mtu=9000
mk5=record=nthread::4
mk5=net_port=46227
mk5=net_protocol=udpsnor:805306368:134217728:4
dbbc3=core3h=1,inputselect vsi1-2-3-4 @if_band_used@
dbbc3=core3h=2,inputselect vsi1-2-3-4 @if_band_used@
dbbc3=core3h=3,inputselect vsi1-2-3-4 @if_band_used@
dbbc3=core3h=4,inputselect vsi1-2-3-4 @if_band_used@
dbbc3=core3h=5,inputselect vsi1-2-3-4 @if_band_used@
dbbc3=core3h=6,inputselect vsi1-2-3-4 @if_band_used@
dbbc3=core3h=7,inputselect vsi1-2-3-4 @if_band_used@
dbbc3=core3h=8,inputselect vsi1-2-3-4 @if_band_used@
dbbc3=core3h=1,vsi_samplerate 128000000 4 @if_band_used@
dbbc3=core3h=2,vsi_samplerate 128000000 4 @if_band_used@
dbbc3=core3h=3,vsi_samplerate 128000000 4 @if_band_used@
dbbc3=core3h=4,vsi_samplerate 128000000 4 @if_band_used@
dbbc3=core3h=5,vsi_samplerate 128000000 4 @if_band_used@
dbbc3=core3h=6,vsi_samplerate 128000000 4 @if_band_used@
dbbc3=core3h=7,vsi_samplerate 128000000 4 @if_band_used@
dbbc3=core3h=8,vsi_samplerate 128000000 4 @if_band_used@
dbbc3=core3h=1,splitmode on @if_band_used@
dbbc3=core3h=2,splitmode on @if_band_used@
dbbc3=core3h=3,splitmode on @if_band_used@ 
dbbc3=core3h=4,splitmode on @if_band_used@
dbbc3=core3h=5,splitmode on @if_band_used@
dbbc3=core3h=6,splitmode on @if_band_used@
dbbc3=core3h=7,splitmode on @if_band_used@
dbbc3=core3h=8,splitmode on @if_band_used@
dbbc3=core3h=1,vsi_bitmask @sideband@ @sideband@ @sideband@ @sideband@
dbbc3=core3h=2,vsi_bitmask @sideband@ @sideband@ @sideband@ @sideband@
dbbc3=core3h=3,vsi_bitmask @sideband@ @sideband@ @sideband@ @sideband@
dbbc3=core3h=4,vsi_bitmask @sideband@ @sideband@ @sideband@ @sideband@
dbbc3=core3h=5,vsi_bitmask @sideband@ @sideband@ @sideband@ @sideband@
dbbc3=core3h=6,vsi_bitmask @sideband@ @sideband@ @sideband@ @sideband@
dbbc3=core3h=7,vsi_bitmask @sideband@ @sideband@ @sideband@ @sideband@
dbbc3=core3h=8,vsi_bitmask @sideband@ @sideband@ @sideband@ @sideband@
dbbc3=core3h=1,reset @if_band_used@
dbbc3=core3h=2,reset @if_band_used@
dbbc3=core3h=3,reset @if_band_used@
dbbc3=core3h=4,reset @if_band_used@
dbbc3=core3h=5,reset @if_band_used@
dbbc3=core3h=6,reset @if_band_used@
dbbc3=core3h=7,reset @if_band_used@
dbbc3=core3h=8,reset @if_band_used@
dbbc3=core3h=1,vdif_station ke @if_band_used@
dbbc3=core3h=2,vdif_station ke @if_band_used@
dbbc3=core3h=3,vdif_station ke @if_band_used@
dbbc3=core3h=4,vdif_station ke @if_band_used@
dbbc3=core3h=5,vdif_station ke @if_band_used@
dbbc3=core3h=6,vdif_station ke @if_band_used@
dbbc3=core3h=7,vdif_station ke @if_band_used@
dbbc3=core3h=8,vdif_station ke @if_band_used@
dbbc3=core3h=1,vdif_frame 2 8 8000 ct=off @if_band_used@
dbbc3=core3h=2,vdif_frame 2 8 8000 ct=off @if_band_used@
dbbc3=core3h=3,vdif_frame 2 8 8000 ct=off @if_band_used@
dbbc3=core3h=4,vdif_frame 2 8 8000 ct=off @if_band_used@
dbbc3=core3h=5,vdif_frame 2 8 8000 ct=off @if_band_used@
dbbc3=core3h=6,vdif_frame 2 8 8000 ct=off @if_band_used@
dbbc3=core3h=7,vdif_frame 2 8 8000 ct=off @if_band_used@
dbbc3=core3h=8,vdif_frame 2 8 8000 ct=off @if_band_used@
dbbc3=core3h=1,regupdate vdif_header 3      0 0x03FF0000
dbbc3=core3h=2,regupdate vdif_header 3  65536 0x03FF0000
dbbc3=core3h=3,regupdate vdif_header 3 131072 0x03FF0000
dbbc3=core3h=4,regupdate vdif_header 3 196608 0x03FF0000
dbbc3=core3h=5,regupdate vdif_header 3 262144 0x03FF0000
dbbc3=core3h=6,regupdate vdif_header 3 327680 0x03FF0000
dbbc3=core3h=7,regupdate vdif_header 3 393216 0x03FF0000
dbbc3=core3h=8,regupdate vdif_header 3 458752 0x03FF0000
dbbc3=core3h=1,timesync @if_band_used@
!+5s
dbbc3=core3h=2,timesync @if_band_used@
!+5s
dbbc3=core3h=3,timesync @if_band_used@
!+5s
dbbc3=core3h=4,timesync @if_band_used@
!+5s
dbbc3=core3h=5,timesync @if_band_used@
!+5s
dbbc3=core3h=6,timesync @if_band_used@
!+5s
dbbc3=core3h=7,timesync @if_band_used@
!+5s
dbbc3=core3h=8,timesync @if_band_used@
!+5s
dbbc3=core3h=1,destination 0 192.168.1.50:46227 @if_band_used@
dbbc3=core3h=2,destination 0 192.168.1.50:46227 @if_band_used@
dbbc3=core3h=3,destination 0 192.168.1.50:46227 @if_band_used@
dbbc3=core3h=4,destination 0 192.168.1.50:46227 @if_band_used@
dbbc3=core3h=5,destination 0 192.168.1.50:46227 @if_band_used@
dbbc3=core3h=6,destination 0 192.168.1.50:46227 @if_band_used@
dbbc3=core3h=7,destination 0 192.168.1.50:46227 @if_band_used@
dbbc3=core3h=8,destination 0 192.168.1.50:46227 @if_band_used@
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
dbbc3=core3h=1,start vdif @if_band_used@
dbbc3=core3h=2,start vdif @if_band_used@
dbbc3=core3h=3,start vdif @if_band_used@
dbbc3=core3h=4,start vdif @if_band_used@
dbbc3=core3h=5,start vdif @if_band_used@
dbbc3=core3h=6,start vdif @if_band_used@
dbbc3=core3h=7,start vdif @if_band_used@
dbbc3=core3h=8,start vdif @if_band_used@
enddef
"
"=================================
"
define  setmode_@mode@   00000000000x
" Duration: 6 sec
@time_stamp@
pcalon
tpicd=stop
mk5=mode=vdif_8000-512-8-2
mk5=mode?
@dbbc3_bbc@
"
ifa=2,agc,32000
ifb=2,agc,32000
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
dbbc=dbbcgain=all,agc
tpi=all
tpicd=no,80
tpicd
cont_cal=on,2,80,0,10
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
caltsys
enddef
"
"=================================
"
define  midob_@hds@     00000000000x
" Duration: 1 sec
onsource
track
wth
sy=popen -n uptime uptime &
iread
bread
clkoff
maserdelay
dbbc3=pps_delay
dbbc3=core3hstats=5
dbbc3=core3hstats=6
dbbc3=core3hstats=7
dbbc3=core3hstats=8
mk5=mode?
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
mk5=evlbi?
checkmk5
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
