" NASA style of VLBI schedule in proc format.
" Station:      NYALE13S   Ns
"
" Template last modification date: 2023.06.29_11:06:50
" Last update:  @update_date@
"
" S/X setup at 1Gbps
"
@vers@
define  proc_library  00000000000x
enddef
"
" ================================
"
define  exper_initi   00000000000x
sched_initi
enddef
"
" ================================
"
define  sched_initi   00000000000x
proc_library
preses_@hds@
enddef
"
" ================================
"
define  ready_disk    00000000000x
mk5=rtime?
""uncomment the following if your station uses in2net transfers
"mk5=net_protocol=tcp:4194304:2097152;
"mk5=net_protocol=udpsudpsnorth:4194304:2097152;
enddef
"
" ================================
"
define  check_ntp     00000000000x
sy=popen 'uptime 2>&1' -n uptime &
sy=popen 'ntpq -np 2>&1|grep -v "^[- x#]" 2>&1' -n ntpq &
enddef
"
" ================================
"
define  caltsys       00000000000x
caltsys_man
ifagc
dbbc=dbbcgain=all,agc
enddef
"
" ================================
"
define  ifman         00000000000x
ifa=*,man,*
ifb=*,man,*
ifc=*,man,*
ifd=*,man,*
enddef
"
" ================================
"
define  caltsys_man   00000000000x
iread
bread
ifman
dbbc=dbbcgain=all,man
!+1s
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
" ================================
"
define  iread         00000000000x
ifa
ifb
ifc
ifd
enddef
"
" ================================
"
define  bread         00000000000x
bbc01
bbc02
bbc03
bbc04
bbc05
bbc06
bbc07
bbc08
bbc09
bbc10
bbc11
bbc12
bbc13
bbc14
bbc15
bbc16
enddef
"
" ================================
"
define  ifagc         00000000000x
ifa=*,agc,*
ifb=*,agc,*
ifc=*,agc,*
ifd=*,agc,*
enddef
"
" ================================
"
define  pcalon        00000000000x
"no phase cal control is implemented here
"phasecal=on
enddef
"
"=================================
"=================================
"
define  preses_@hds@   00000000000x
" Duration: 0 sec
"antenna=boot
check_ntp
azeloff=0d,0d
mk5=mtu?
mk5=version?
mk5=datastream?
sy=popen 'date' -n 'date' &
tacd
fila10g=start vdif
mk5=dts_id?
mk5=os_rev?
mk5_status
dbbc=version
fila10g=version
ready_disk
enddef
"
"=================================
"
define  setmode_@mode@   00000000000x
" Duration: 0 sec
@time_stamp@
" set observing mode @mode@
" set the lo stream
"
"
" Set lo
"
ifa=1,agc,2,42000
ifb=1,agc,1,42000
ifc=1,agc,1,42000
ifd=1,agc,1,42000
lo=
lo=loa,@lo@,usb,rcp,1
lo=lob,@lo@,usb,rcp,1
lo=loc,@lo@,usb,rcp,1
lo=lod,@lo@,usb,rcp,1
bbc09=@if_offset@,c,@if_width@
bbc10=@if_offset@,c,@if_width@
bbc11=@if_offset@,c,@if_width@
bbc12=@if_offset@,c,@if_width@
bbc13=@if_offset@,d,@if_width@
bbc14=@if_offset@,d,@if_width@
bbc01=@if_offset@,a,@if_width@
bbc02=@if_offset@,a,@if_width@
bbc03=@if_offset@,a,@if_width@
bbc04=@if_offset@,a,@if_width@
bbc05=@if_offset@,b,@if_width@
bbc06=@if_offset@,b,@if_width@
bbc07=@if_offset@,b,@if_width@
bbc08=@if_offset@,b,@if_width@
bread
enddef
"
"=================================
"
define  setscan_@hds@   00000000000x
" Duration: 2 sec
tacd
pcalon
tpicd=stop
"vsi1-2 input should be used in 'equip.ctl'
"... vsi1 or vsi2 are also supported
fila10g_mode=,0xffffffff,,32.000
fila10g_mode
mk5c_mode=vdif,0xffffffff,,32.000
mk5c_mode
form=geo
form
cont_cal=off
bbc_gain=all,agc,16000
tpicd=no,0
tpicd
enddef
"
"=================================
"
define  preob_@hds@     00000000000x
" Duration: 6 sec
!*
wx
onsource
caltsys_man
!*
enddef
"
"=================================
"
define  midob_@hds@     00000000000x
" Duration: 0 sec
disk_record=on
disk_record
data_valid=on
onsource
wx
iread
bread
fmout-gps
bit_streams
!+1s
mk5=record?
mk5=evlbi?
sy=run setcl adapt &
enddef
"
"=================================
"
define  postob_@hds@    00000000000x
" Duration: 6 sec
data_valid=off
disk_record=off
wx
caltsys_man
ifagc
dbbc=dbbcgain=all,agc
mk5=evlbi?
mk5=rtime?
jive5ab=mode?
"mk5=mode?
mk5=scan_check?
mk5_status
mk5=net_protocol?
mk5=mtu?
mk5=rtime?
bit_streams
mk5=scan_set=::+50000000
mk5=disk2file=/home/oper/checkmk5/checkmk5.data:::w
!+5s
sy=exec /usr2/oper/bin/checkdata.py `lognm` /home/oper/checkmk5/checkmk5.data &
enddef
"
"=================================
"
define  postses_@hds@        00000000000x
" Duration: 0 sec
" End of schedule at station ns
sched_end
enddef
