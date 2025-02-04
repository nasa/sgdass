" NASA style of VLBI schedule in proc format.
" Station:      KOKEE      Kk
"
" Template last modification date: 2024.06.26_20:00:04
" Last update:  @update_date@
"
" A special setup for using X-band with 14 BBC with RF a and c
" using a commonly used geo wiring.
"
" Hidden procedures: calon caloff maser  pcsample
" Backend: mark5b
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
" =================================
"
define  pcalon         00000000000x
"no phase cal control is implemented here
enddef
"
" =================================
"
define  caltsys        00000000000x
bbcman
tpi=formbbc,formif
tpgain=formbbc,formif
ifdab=20,20,*,*
ifdcd=20,20,*,*
!+2s
tpzero=formbbc,formif
ifdab=0,0,*,*
ifdcd=0,0,*,*
calon
!+2s
tpical=formbbc,formif
tpdiff=formbbc,formif
tpdiffgain=formbbc,formif
caloff
bbcagc
caltemp=formbbc,formif
tsys=formbbc,formif
cab
enddef
"
" =================================
"
define  cab             00000000000x
hpib=vv,meas? apower
hpib=vv
cable
enddef
"
" =================================
"
define  bbcman        00000000000x
bbc01=*,*,*,*,*,man
bbc02=*,*,*,*,*,man
bbc03=*,*,*,*,*,man
bbc04=*,*,*,*,*,man
bbc05=*,*,*,*,*,man
bbc06=*,*,*,*,*,man
bbc07=*,*,*,*,*,man
bbc08=*,*,*,*,*,man
bbc09=*,*,*,*,*,man
bbc10=*,*,*,*,*,man
bbc11=*,*,*,*,*,man
bbc12=*,*,*,*,*,man
bbc13=*,*,*,*,*,man
bbc14=*,*,*,*,*,man
enddef
"
" =================================
"
define  bbcagc          00000000000x
bbc01=*,*,*,*,*,agc
bbc02=*,*,*,*,*,agc
bbc03=*,*,*,*,*,agc
bbc04=*,*,*,*,*,agc
bbc05=*,*,*,*,*,agc
bbc06=*,*,*,*,*,agc
bbc07=*,*,*,*,*,agc
bbc08=*,*,*,*,*,agc
bbc09=*,*,*,*,*,agc
bbc10=*,*,*,*,*,agc
bbc11=*,*,*,*,*,agc
bbc12=*,*,*,*,*,agc
bbc13=*,*,*,*,*,agc
bbc14=*,*,*,*,*,agc
enddef
"
" =================================
"
define  checkmk5        00000000000x
scan_check
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5_status
enddef
"
"=================================
"=================================
"
define  preses_@hds@   00000000000x
" Duration: 0 sec
mk5=dts_id?
mk5=os_rev?
mk5=ss_rev?
mk5_status
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
ifdab=0,0,nor,nor
ifdcd=0,0,nor,nor
lo=
lo=loa,@lo@,usb,rcp,1
lo=lob,@lo@,usb,rcp,1
lo=loc,@lo@,usb,rcp,1
lo=lod,@lo@,usb,rcp,1
bbc01=@if_offset@,a,@if_width@,@if_width@
bbc02=@if_offset@,a,@if_width@,@if_width@
bbc03=@if_offset@,a,@if_width@,@if_width@
bbc04=@if_offset@,a,@if_width@,@if_width@
bbc09=@if_offset@,d,@if_width@,@if_width@
bbc10=@if_offset@,d,@if_width@,@if_width@
bbc11=@if_offset@,d,@if_width@,@if_width@
bbc12=@if_offset@,d,@if_width@,@if_width@
bbc13=@if_offset@,d,@if_width@,@if_width@
bbc14=@if_offset@,d,@if_width@,@if_width@
bbc05=@if_offset@,c,@if_width@,@if_width@
bbc06=@if_offset@,c,@if_width@,@if_width@
bbc07=@if_offset@,c,@if_width@,@if_width@
bbc08=@if_offset@,c,@if_width@,@if_width@
bread
enddef
"
"=================================
"
define  setscan_@hds@   00000000000x
" Duration: 0 sec
pcalon
tpicd=stop
mk5b_mode=ext,@if_mask@,,@two_if_width@
mk5b_mode
vsi4=geo
vsi4
setmode_@mode@
tpicd=no,0
bank_check
mk5=bank_set?
tpicd
enddef
"
"=================================
"
define  preob_@hds@     00000000000x
" Duration: 10 sec
onsource
caltsys
enddef
"
"=================================
"
define  midob_@hds@     00000000000x
" Duration: 1 sec
disk_pos
disk_record=on
disk_record
data_valid=on
onsource
cab
wx
ifdab
ifdcd
bbc01
bbc05
bbc09
track
fmout
maser
mk5b_mode
!+1s
mk5=dot?
mk5=bank_set?
sy=run setcl adapt &
pcsample
azeloff
enddef
"
"=================================
"
define  postob_@hds@    00000000000x
" Duration: 1 sec
vsi4
data_valid=off
disk_record=off
disk_pos
checkmk5
enddef
"
"=================================
"
define  postses_@hds@        00000000000x
" Duration: 0 sec
" End of schedule
sched_end
enddef
