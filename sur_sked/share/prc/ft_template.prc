" NASA style of VLBI schedule in proc format.
" Station:      FORTLEZA    Ft
"
" Template last modification date: 2024.02.26_23:37:15
" Last update:  @update_date@
"
" Hidden procedures: caltsys checkfb pcsample fmout-gps
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
define  trkfsx        00000000000x
trackform=
trackform=2,1us,4,1um,6,1ls,8,1lm,10,2us,12,2um,14,3us,16,3um
trackform=18,4us,20,4um,22,5us,24,5um,26,6us,28,6um,30,7us,32,7um
trackform=3,8us,5,8um,7,8ls,9,8lm,11,9us,13,9um,15,10us,17,10um
trackform=19,11us,21,11um,23,12us,25,12um,27,13us,29,13um,31,14us
trackform=33,14um
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
trkfsx
tracks=v0,v1,v2,v3
form=m,16.000
tpicd=no,0
mk5=play_rate=data:16;
mk5=mode=mark4:32;
bank_check
tpicd
mk5=mode?
"
ifd=13,18,nor,nor
if3=0,in,2,2,,,on
"
lo=
lo=lo2,@lo@,usb,rcp,1
lo=lo1,@lo@,usb,rcp,1
lo=lo3,@lo@,usb,rcp,1
"
vc01=@if_offset@,a,@if_width@,ul
vc02=@if_offset@,a,@if_width@,u
vc03=@if_offset@,a,@if_width@,u
vc04=@if_offset@,a,@if_width@,u
vc05=@if_offset@,b,@if_width@,u
vc06=@if_offset@,b,@if_width@,u
vc07=@if_offset@,b,@if_width@,u
vc08=@if_offset@,b,@if_width@,ul
vc09=@if_offset@,c,@if_width@,u
vc10=@if_offset@,c,@if_width@,u
vc11=@if_offset@,c,@if_width@,u
vc12=@if_offset@,c,@if_width@,u
vc13=@if_offset@,d,@if_width@,u
vc14=@if_offset@,d,@if_width@,u
"
patch=
patch=lo1,1l,2l,3h,4h
patch=lo2,9l,10h,11h,12h,13h,14h
patch=lo3,5h,6h,7h,8h
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
pcsample
tpi=formvc,formif
caltemp=formvc,formif
tsys=formvc,formif
sy=run setcl adapt &
form
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
