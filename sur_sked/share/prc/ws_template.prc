" NASA style of VLBI schedule in proc format.
" Station:      WETTZ13S   Ws
"
" Template last modification date: 2024.04.04_18:19:36
" Last update:  @update_date@
"
" Hidden procedures: dotmon dotmon2 rxall
"
@vers@
define  sched_initi   00000000000x
preses_@hds@
enddef
"
" =================================
"
define  core3hsx      00000000000x
core3h_mode=begin,$
core3h_mode=1,,oxcccccccc,,64.0,$ "" 0xccc: defines lower sub-band
core3h_mode=2,,0xcccccccc,,64.0,$
core3h_mode=3,,0xcccccccc,,64.0,$
core3h_mode=4,,0xcccccccc,,64.0,$
core3h_mode=5,,0xcccccccc,,64.0,$
core3h_mode=6,,0xcccccccc,,64.0,$
core3h_mode=7,,0xcccccccc,,64.0,$
core3h_mode=8,,0xcccccccc,,64.0,$
core3h_mode=end,$
enddef
"
" =================================
"
define  threadsx      00000000000x "" thread defintion of flexbuff
fb=datastream=clear
fb=datastream=add:{thread}:*
fb=datastream=reset
enddef
"
" =================================
"
define  setthread
core3h=1,regupdate vdif_header 3      0 0x03FF0000
core3h=2,regupdate vdif_header 3  65536 0x03FF0000
core3h=3,regupdate vdif_header 3 131072 0x03FF0000
core3h=4,regupdate vdif_header 3 196608 0x03FF0000
core3h=5,regupdate vdif_header 3 262144 0x03FF0000
core3h=6,regupdate vdif_header 3 327680 0x03FF0000
core3h=7,regupdate vdif_header 3 393216 0x03FF0000
core3h=8,regupdate vdif_header 3 458752 0x03FF0000
enddef
"
" =================================
"
define  timesync      00000000000x
core3h=1,timesync
core3h=2,timesync
core3h=3,timesync
core3h=4,timesync
core3h=5,timesync
core3h=6,timesync
core3h=7,timesync
core3h=8,timesync
!+1s
dbbc3=pps_sync
"
" =================================
"
enddef
define  core3_start         00000000000x
core3h=1,start vdif
core3h=2,start vdif
core3h=3,start vdif
core3h=4,start vdif
core3h=5,start vdif
core3h=6,start vdif
core3h=7,start vdif
core3h=8,start vdif
enddef
"
" =================================
"
define  core3_stop          00000000000x
core3h=1,stop
core3h=2,stop
core3h=3,stop
core3h=4,stop
core3h=5,stop
core3h=6,stop
core3h=7,stop
core3h=8,stop
enddef
"
" =================================
"
define  pcalon        00000000000x
enddef
"
" =================================
" =================================
"
define  preses_@hds@ 00000000000x
" Duration: 0 sec
"fb: flexbuff commands
fb=dts_id? "" software revision
fb=os_rev? ""
fb_status  "" status flexbuff
dbbc3=version
pcalon
tpicd=stop
core3hsx=$             "" field system place holder, core3h: VDIF header generator
fb_mode=vdif,,,64.0    "" flexbuff mode setting, 64 MSps
fb_mode                "" mode       monitoring
threadsx               "" setting in the core3h board (8 of them) individual thread ID for VDIF stream
fb_config              "" setting of flexbuff parameters dealing with buffer sizes 
cont_cal=off           "" needs to adapted to cont_cal=on
bbc_gain=all,agc,16000 "" individual 64 digital bbc setting to agc and 16000 power counts
setthread              "" ?
timesync               "" ?
core3_start            "" ?
tpicd=no,0
tpicd
enddef
"
" =================================
"
define  setmode_@mode@    00000000000x
" Duration: 4 sec
ifa=1,agc,32000
ifb=1,agc,32000
ifc=1,agc,32000
ifd=1,agc,32000
ife=1,agc,32000
iff=1,agc,32000
ifg=1,agc,32000
ifh=1,agc,32000
lo=
lo=loa,@lo@,usb,lcp,5,
lo=lob,@lo@,usb,rcp,5,
lo=loc,@lo@,usb,lcp,5,
lo=lod,@lo@,usb,rcp,5,
lo=loe,@lo@,usb,lcp,5,
lo=lof,@lo@,usb,rcp,5,
lo=log,@lo@,usb,lcp,5,
lo=loh,@lo@,usb,rcp,5,
lo_config
"
"  bbcxxx are hard set for 8 Gbps mode
"
@time_stamp@
@dbbc3_bbc@
enddef
"
" =================================
"
define  setscan_@hds@    00000000000x
" Duration: 0 sec
pcalon
enddef
"
" =================================
"
define  preob_@hds@       00000000000x
" Duration: 0 sec
onsource
mk6=rtime?@bit_rate@;
enddef
"
" =================================
"
define  midob_@hds@       00000000000x
" Duration: 0 sec
mk6=rtime?@bit_rate@;
onsource
wx
dotmon
dotmon2
rxall
cable
"
mk6=dts_id?;
antenna=status
bread
iread
mk6=input_stream?;
data_valid=on
disk_record=on
enddef
"
" =================================
"
define  postob_@hds@      00000000000x
" Duration: 2 sec
data_valid=off
disk_record=off
mk6=rtime?@bit_rate@;
"" !+2s
"" mk6=scan_check?;
enddef
"
" =================================
"
define  postses_@hds@      00000000000x
" Duration: 0 sec
" End of schedule
sched_end
enddef
