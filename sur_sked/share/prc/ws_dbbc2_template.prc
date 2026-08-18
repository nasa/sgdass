" VLBI schedule in proc format. NASA style.
" Last update: 2022.03.07_08:19:57
"
@vers@
define  sched_initi   00000000000x
preses_@hds@
enddef
"
" =================================
"
define  pcalon        00000000000x
enddef
"
" =================================
"
define  iread         16158145945x
"Wettzel DBBC#1
ifa
ifb
ifc
ifd
"
"Wettzel DBBC#2
dbbc2=dbbcifa
dbbc2=dbbcifb
dbbc2=dbbcifc
dbbc2=dbbcifd
enddef
"
" =================================
"
define  bread         16158145951x
"Wettzel DBBC#1
dbbc=power=1
!+1s
dbbc=power=2
!+1s
dbbc=power=3
!+1s
dbbc=power=4
"
"Wettzel DBBC#2
dbbc2=power=1
!+1s
dbbc2=power=2
!+1s
dbbc2=power=3
!+1s
dbbc2=power=4
enddef
"
" =================================
"
define  fila10g_cfg   16158144253x
fila10g1_cfg
fila10g2_cfg
enddef
"
" =================================
"
define  fila10g1_cfg  16158144253x
"Wettzel DBBC#1
"customize inputselect, ips, gateways, macs, and timesync
"for your station
fila10g=arp off
fila10g=tengbcfg eth0 ip=192.168.1.30 gateway=192.168.1.1
fila10g=tengbcfg eth0 mac=ba:dc:af:e4:be:e0
fila10g=tengbcfg eth0 nm=27
fila10g=tengbcfg eth1 ip=192.168.1.31 gateway=192.168.1.1
fila10g=tengbcfg eth1 mac=ba:dc:af:e4:be:e1
fila10g=tengbcfg eth1 nm=27
fila10g=destination 0 192.168.1.2:2630
"fila10g=destination 1 none
fila10g=destination 1 192.168.1.3:2630
"xx:xx:xx:xx:xx:xx is the destination MAC?
"fila10g=tengbarp eth0 11 xx:xx:xx:xx:xx:xx
"fila10g=tengbarp eth1 11 xx:xx:xx:xx:xx:xx
fila10g=tengbarp eth0 2 00:60:dd:44:eb:63
fila10g=tengbarp eth1 3 00:60:dd:44:eb:62
fila10g=vdif_station Ws
"
"fila10g=splitmode off
"fila10g=inputselect vsi1
fila10g=splitmode on
fila10g=inputselect vsi1-2
enddef
"
" =================================
"
define  fila10g2_cfg  16158144253x
"Wettzel DBBC#2
"customize inputselect, ips, gateways, macs, and timesync
"for your station
fila10g2=arp off
fila10g2=tengbcfg eth0 ip=192.168.1.32 gateway=192.168.1.1
fila10g2=tengbcfg eth0 mac=ba:dc:af:e4:be:e2
fila10g2=tengbcfg eth0 nm=27
fila10g2=tengbcfg eth1 ip=192.168.1.33 gateway=192.168.1.1
fila10g2=tengbcfg eth1 mac=ba:dc:af:e4:be:e3
fila10g2=tengbcfg eth1 nm=27
fila10g2=destination 0 192.168.1.4:2630
"fila10g2=destination 1 none
fila10g2=destination 1 192.168.1.5:2630
"xx:xx:xx:xx:xx:xx is the destination MAC?
"fila10g2=tengbarp eth0 11 xx:xx:xx:xx:xx:xx
"fila10g2=tengbarp eth1 11 xx:xx:xx:xx:xx:xx
fila10g2=tengbarp eth0 4 00:60:dd:44:eb:5b
fila10g2=tengbarp eth1 5 00:60:dd:44:eb:5a
fila10g2=vdif_station W2
"
"fila10g2=splitmode off
"fila10g2=inputselect vsi1
fila10g2=splitmode on
fila10g2=inputselect vsi1-2
"set thread ID for second FILA10G to ID=2 and ID=3
fila10g2=regupdate vdif_header 3 131072 0x03FF0000
enddef
"
" =================================
"
define  dbbcbb        16158145047x
"equivalent to:
"rdbe=dbe_chsel=0:1:2:4:6:9:13:14:15;
"rdbe=dbe_chsel=1:1:2:4:6:9:13:14:15;
"Wettzell DBBC#1 has:
" band B v-pol on ifd, core4
" band B h-pol on ifc, core3
" band A v-pol on ifb, core2
" band A h-pol on ifa, core1
dbbc=vsi_align=1
dbbc=dbbcform=flex
!+1s
"vsi2
dbbc=dbbctrk=4,2,v2-0,v2-1,v2-2,v2-3,v2-4,v2-5,v2-6,v2-7,p-1,p-2,p-4,p-6,p-9,p-13,p-14,p-15
!+1s
dbbc=dbbctrk=3,2,p-1,p-2,p-4,p-6,p-9,p-13,p-14,p-15,v2-8,v2-9,v2-10,v2-11,v2-12,v2-13,v2-14,v2-15
!+1s
"vsi1
dbbc=dbbctrk=4,1,v1-0,v1-1,v1-2,v1-3,v1-4,v1-5,v1-6,v1-7,v1-8,v1-9,v1-10,v1-11,v1-12,v1-13,v1-14,v1-15
!+1s
dbbc=dbbctrk=3,1,v1-0,v1-1,v1-2,v1-3,v1-4,v1-5,v1-6,v1-7,v1-8,v1-9,v1-10,v1-11,v1-12,v1-13,v1-14,v1-15
!+1s
dbbc=dbbctrk=2,1,v1-0,v1-1,v1-2,v1-3,v1-4,v1-5,v1-6,v1-7,p-1,p-2,p-4,p-6,p-9,p-13,p-14,p-15
!+1s
dbbc=dbbctrk=1,1,p-1,p-2,p-4,p-6,p-9,p-13,p-14,p-15,v1-8,v1-9,v1-10,v1-11,v1-12,v1-13,v1-14,v1-15
"
"equivalent to:
"rdbe=dbe_chsel=0:1:2:4:6:9:13:14:15;
"rdbe=dbe_chsel=1:1:2:4:6:9:13:14:15;
"Wettzell DBBC#2 has:
" band D v-pol on ifd, core4
" band D h-pol on ifc, core3
" band C v-pol on ifb, core2
" band C h-pol on ifa, core1
dbbc2=vsi_align=1
dbbc2=dbbcform=flex
!+1s
"vsi2
dbbc2=dbbctrk=4,2,v2-0,v2-1,v2-2,v2-3,v2-4,v2-5,v2-6,v2-7,p-1,p-2,p-4,p-6,p-9,p-13,p-14,p-15
!+1s
dbbc2=dbbctrk=3,2,p-1,p-2,p-4,p-6,p-9,p-13,p-14,p-15,v2-8,v2-9,v2-10,v2-11,v2-12,v2-13,v2-14,v2-15
!+1s
"vsi1
dbbc2=dbbctrk=4,1,v1-0,v1-1,v1-2,v1-3,v1-4,v1-5,v1-6,v1-7,v1-8,v1-9,v1-10,v1-11,v1-12,v1-13,v1-14,v1-15
!+1s
dbbc2=dbbctrk=3,1,v1-0,v1-1,v1-2,v1-3,v1-4,v1-5,v1-6,v1-7,v1-8,v1-9,v1-10,v1-11,v1-12,v1-13,v1-14,v1-15
!+1s
dbbc2=dbbctrk=2,1,v1-0,v1-1,v1-2,v1-3,v1-4,v1-5,v1-6,v1-7,p-1,p-2,p-4,p-6,p-9,p-13,p-14,p-15
!+1s
dbbc2=dbbctrk=1,1,p-1,p-2,p-4,p-6,p-9,p-13,p-14,p-15,v1-8,v1-9,v1-10,v1-11,v1-12,v1-13,v1-14,v1-15
enddef
"
" =================================
"
define  ifdbb         16158145116x
"Wettzell BBC#1:
" band A h-pol on ifa, input1
" band A v-pol on ifb, input1
" band B h-pol on ifc, input1
" band B v-pol on ifd, input1
lo=
lo=loa,2472.4,usb,lcp,5
lo=lob,2472.4,usb,rcp,5
lo=loc,4712.4,usb,lcp,5
lo=lod,4712.4,usb,rcp,5
ifa=1,agc,1,38000
ifb=1,agc,1,38000
ifc=1,agc,1,38000
ifd=1,agc,1,38000
"
"Wettzell DBBC#2:
" band C h-pol on ifa, input1
" band C v-pol on ifb, input1
" band D h-pol on ifc, input1
" band D v-pol on ifd, input1
lo=lo2a,5832.4,usb,lcp,5
lo=lo2b,5832.4,usb,rcp,5
lo=lo2c,9672.4,usb,lcp,5
lo=lo2d,9672.4,usb,rcp,5
"ifa=1,agc,1,38000
"ifb=1,agc,1,38000
"ifc=1,agc,1,38000
"ifd=1,agc,1,38000
dbbc2=dbbcifa=1,agc,1,38000
dbbc2=dbbcifb=1,agc,1,38000
dbbc2=dbbcifc=1,agc,1,38000
dbbc2=dbbcifd=1,agc,1,38000
enddef
"
" =================================
"
define  mk6bb         00000000000x
"Fila10G#1 eth0 connects to Mark6 eth2
"Fila10G#1 eth1 connects to Mark6 eth3
"Fila10G#2 eth0 connects to Mark6 eth4
"Fila10G#2 eth1 connects to Mark6 eth5
mk6=input_stream = delete ;
!+4s
mk6=input_stream = add : bandA : vdif : 8224 : 50 : 42 : eth2 : 192.168.1.30 : 2630;
mk6=input_stream = add : bandB : vdif : 8224 : 50 : 42 : eth3 : 192.168.1.31 : 2630;
mk6=input_stream = add : bandC : vdif : 8224 : 50 : 42 : eth4 : 192.168.1.32 : 2630;
mk6=input_stream = add : bandD : vdif : 8224 : 50 : 42 : eth5 : 192.168.1.33 : 2630;
mk6=input_stream = commit;
enddef
"
" =================================
"
define  checkmk6      00000000000x
!+2s
mk6=record=off;
!+2s
mk6=scan_check?;
enddef
"
" =================================
"
define  ifman         00000000000x
"Wettzel DBBC#1
ifa=*,man,*
ifb=*,man,*
ifc=*,man,*
ifd=*,man,*
"Wettzel DBBC#2
dbbc2=dbbcifa=1,man,1,38000
dbbc2=dbbcifb=1,man,1,38000
dbbc2=dbbcifc=1,man,1,38000
dbbc2=dbbcifd=1,man,1,38000
enddef
"
" =================================
"
define  ifagc         00000000000x
"Wettzel DBBC#1
ifa=*,agc,*
ifb=*,agc,*
ifc=*,agc,*
ifd=*,agc,*
"Wettzel DBBC#2
dbbc2=dbbcifa=1,agc,1,38000
dbbc2=dbbcifb=1,agc,1,38000
dbbc2=dbbcifc=1,agc,1,38000
dbbc2=dbbcifd=1,agc,1,38000
enddef
"
" =================================
"
define  fila10gbb     16158144311x
" Wettzell FiLa10G#1
fila10g=vsi_bitmask 0xffffffff 0xffffffff
" samplerate: input_clock decimation
fila10g=vsi_samplerate 64000000 1
"frame: bits/chan channel bytes
fila10g=vdif_frame 2 16 8192
fila10g=start vdif+vdif
"
" Wettzell FiLa10G#2
fila10g2=vsi_bitmask 0xffffffff 0xffffffff
" samplerate: input_clock decimation
fila10g2=vsi_samplerate 64000000 1
"frame: bits/chan channel bytes
fila10g2=vdif_frame 2 16 8192
fila10g2=start vdif+vdif
enddef
"
" =================================
" =================================
"
define  preses_@hds@ 00000000000x
" Duration: 0 sec
azeloff=0d,0d
!+2s
antenna=azeloff,0,0,0
setuprx
proc_library
ifdbb
dbbcbb
fila10gbb
mk6bb
!+1s
mk6=mstat?all
enddef
"
" =================================
"
define  setmode_@mode@    00000000000x
" Duration: 4 sec
@time_stamp@
enddef
"
" =================================
"
define  setscan_@hds@    00000000000x
" Duration: 0 sec
pcalon
ifdbb
enddef
"
" =================================
"
define  preob_@hds@       00000000000x
" Duration: 0 sec aaaaaaaaaaaaaaaaaaaaaaaaaaaa temporary should be 3 sec
onsource
ifman
mk6=rtime?@bit_rate@;
enddef
"
" =================================
"
define  midob_@hds@       00000000000x
" Duration: 0 sec
data_valid=on
mk6=rtime?@bit_rate@;
onsource
wx
dotmon
dotmon2
rxall
cable
"Wettzel DBBC#1
dbbc=version
fila10g=version
fila10g=time
"
"Wettzel DBBC#2
dbbc2=version
fila10g2=version
fila10g2=time
"
mk6=dts_id?;
antenna=status
bread
iread
mk6=input_stream?;
enddef
"
" =================================
"
define  postob_@hds@      00000000000x
" Duration: 0 sec aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa temporary, should be 2 sec
data_valid=off
mk6=rtime?@bit_rate@;
mk6=record=off;
" !+2s
" mk6=scan_check?;
enddef
"
" =================================
"
define  postses_@hds@      00000000000x
" Duration: 0 sec
" End of schedule
sched_end
enddef
