" NASA style of VLBI schedule in proc format.
" Station:      FORTLEZA    Ft
"
" Template last modification date: 2026.08.17_17:16:49
" Last update:  @update_date@
"
" Hidden procedures: caltsys checkfb pcsample fmout-gps check_ntp rxall valarm vread
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
"
"  Not implented as of 2026.07.24
"
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
mk5=os_rev1?
mk5=os_rev2?
mk5=ss_rev1?
mk5=ss_rev2?
mk5_status
check_ntp
rxall
caltsys
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
tpicd=no,0
mk5b_mode=ext,@if_mask@,,@two_if_width@
mk5b_mode
vsi4=geo
vsi4
tpicd
mk5=mode?
"
ifd=26,25,nor,nor@do_not_remove@
if3=12,in,2,1,,,on
"
lo=
lo=lo1,@lo@,usb,rcp,1
lo=lo2,@lo@,usb,rcp,1
lo=lo3,@lo@,usb,rcp,1
"
vc01=@if_offset@,1,@if_width@,ul
vc02=@if_offset@,1,@if_width@,u
vc03=@if_offset@,1,@if_width@,u
vc04=@if_offset@,1,@if_width@,u
vc05=@if_offset@,3,@if_width@,u
vc06=@if_offset@,3,@if_width@,u
vc07=@if_offset@,3,@if_width@,u
vc08=@if_offset@,3,@if_width@,ul
vc09=@if_offset@,2,@if_width@,u
vc10=@if_offset@,2,@if_width@,u
vc11=@if_offset@,2,@if_width@,u
vc12=@if_offset@,2,@if_width@,u
vc13=@if_offset@,2,@if_width@,u
vc14=@if_offset@,2,@if_width@,u
"
patch=
patch=lo1,1l,2l,3h,4h
patch=lo2,9l,10h,11h,12h,13h,14h
patch=lo3,5h,6h,7h,8h
!+1s
valarm
tpicd=no,0
bank_check
mk5=bank_set?
lo
vread
tpicd
enddef
"
"=================================
"
define  setscan_@hds@   00000000000x
" Duration: 1 sec
enddef
"
"=================================
"
define  preob_@hds@     00000000000x
" Duration: 10 sec
lo
vread
onsource
caltsys
enddef
"
"=================================
"
define  midob_@hds@     00000000000x
" Duration: 3 sec
disk_record=on
disk_record
data_valid=on
onsource
cable
ifd
if3
vc01
vc05
vc09
tpi=formvc,formif
caltemp=formvc,formif
tsys=formvc,formif
sy=run setcl adapt &
hpib=cb,fetc?
fmout-gps=cb
enddef
"
"=================================
"
define  postob_@hds@    00000000000x
" Duration: 1 sec
data_valid=off
disk_record=off
enddef
"
"=================================
"
define  postses_@hds@        00000000000x
" Duration: 0 sec
" End of schedule
sched_end
enddef
