" NASA style of VLBI schedule in proc format.
" Station:      WETTZELL   Wz
"
" Template last modification date: 2024.02.26_23:37:15
" Last update:  @update_date@
"
" Hidden procedures: checkfb dotmon
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
sy=/usr2/st_rtw/pcal_rtw/bin/pcalon_rtwc &
enddef
"
"=================================
"
define  fila10g_cfg   00000000000x
fila10g=splitmode      off
fila10g=inputselect    vsi1
fila10g=vsi_inputwidth 32
fila10g=vsi_samplerate 32000000 2
fila10g=vsi_bitmask    0xffffffff
fila10g=vdif_station   wz
fila10g=vdif_frame     2 16 8000
fila10g=arp off
fila10g=tengbcfg eth0  ip=192.168.1.40 gateway=192.168.1.1
fila10g=tengbcfg eth0  mac=ba:dc:af:e4:be:f0
fila10g=tengbcfg eth0  nm=27
fila10g=tengbcfg eth1  ip=192.168.1.41 gateway=192.168.1.1
fila10g=tengbcfg eth1  mac=ba:dc:af:e4:be:f1
fila10g=tengbcfg eth1  nm=27
"
" connection to flexbuff1 (98)
" fila10g=destination 0 192.168.1.101:2637
" connection to flexbuff2 (97)
"
fila10g=destination 0 192.168.1.105:26300
"
" fila10g=destination 0 192.168.1.104:26300
" connection to flexbuff3 (102)
" fila10g=destination 0 192.168.1.98:26300
"
fila10g=destination 1 none
fila10g=vsi_outputselect vsi1-1
"
" fila10g=timesync
"
enddef
"
"=================================
"
define fila10g_mac    00000000000x
fila10g=arp off
" mac of flexbuff1 (98)
fila10g=tengbarp eth0 100 24:6e:96:25:e9:1c
fila10g=tengbarp eth0 101 24:6e:96:25:e9:1e
fila10g=tengbarp eth0 102 24:6e:96:25:e9:20
fila10g=tengbarp eth0 103 24:6e:96:25:e9:22
" mac of flexbuff2 (97)
fila10g=tengbarp eth0 104 24:6e:96:a0:51:54
fila10g=tengbarp eth0 105 24:6e:96:a0:51:56
fila10g=tengbarp eth0 106 24:6e:96:a0:51:58
fila10g=tengbarp eth0 107 24:6e:96:a0:51:5a
" mac of flexbuff3 (102)
fila10g=tengbarp eth0 98 00:e0:ed:7d:f8:68
fila10g=tengbarp eth0 99 00:e0:ed:7d:f8:69
enddef
"
"=================================
"
define fila10gbb      00000000000x
fila10g=start vdif
enddef
"
"=================================
"
define  caltsys_man   00000000000x
ifman
bbc_gain=all,man
!+2s
tpi=formbbc,formif
calon
!+2s
tpical=formbbc,formif
caloff
tpdiff=formbbc,formif
caltemp=formbbc,formif
tsys=formbbc,formif
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
define  calon         00000000000x
"turn cal on
sy=python /usr2/oper/bin/ncal.py -o on &
enddef
"
"=================================
"
define  caloff        00000000000x
"turn cal off
sy=python /usr2/oper/bin/ncal.py -o off &
enddef
"
" =================================
" =================================
"
define  preses_@hds@   00000000000x
" Duration: 3 sec
proc_library
sched_initi
mk5=dts_id?
mk5=os_rev?
mk5_status
dbbc=version
fila10g=version
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
"vsi1-2 input should be used in 'equip.ctl'
"... vsi1 or vsi2 are also supported
fila10g_mode=,0xffffffff,,@two_if_width@
fila10g_mode
mk5c_mode=vdif,0xffffffff,,@two_if_width@
mk5c_mode
sy=cmd2flexbuff.py net_protocol=udpsnor:8388608:131072:8
sy=cmd2flexbuff.py net_port=26300
form=geo
form
"
ifa=1,agc,2,40000
ifb=1,agc,1,40000
ifc=1,agc,2,40000
ifd=1,agc,2,40000
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
bbc_gain=all,agc,16000
tpicd=no,500
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
if=cont_cal,,!*+4s
if=cont_cal,,caltsys_man
onsource
check=*
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
onsource
antenna=status
wx
rx=dewar?
cable
ifa
ifb
ifc
ifd
bbc01
bbc02
bbc03
bbc04
bbc05
bbc09
bbc13
"
" the shown order of the commands from here to the end of this procedure is
" strongly recommended
" add your station command to measure the gps to fm output clock offset
" gps-fmout=c2
"
mk5c_mode
!+1s
sy=run setcl adapt &
enddef
"
"=================================
"
define  postob_@hds@    00000000000x
" Duration: 1 sec
data_valid=off
disk_record=off
dotmon
sy=exec /usr2/st/bin/legacy_spectra.py `lognm` &
enddef
"
"=================================
"
define  postses_@hds@        00000000000x
" Duration: 0 sec
" End of schedule
sched_end
enddef
