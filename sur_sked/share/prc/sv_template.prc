" NASA style of VLBI schedule in proc format.
" Station:      SVETLOE    Sv
"
" Template last modification date: 2024.02.24_18:31:21
" Last update:  @update_date@
"
" Hidden procedures: gps-maser gps-fmout subr vci01 vci02 vci03 vci04 vci05 vci06 vci07 vci08 vci01 vci09 vci10 vci11 vci12 vci13 vci14
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
define  pcalon        00000000000x
"no phase cal control is implemented here
enddef
"
" =================================
"
define  ready_disk   00000000000x
" mount the mark5 disks for this experiment now
" recording will begin at current position
" enter 'mk5relink' when ready or
" if you can't get the mk5 going then
" enter 'cont' to continue without the mk5
" xdisp=off
" halt
"
disk_serial
disk_pos
bank_check
mk5=dts_id?
mk5=os_rev?
mk5=ss_rev?
mk5=status?
enddef
"
" =================================
"
define  clocko         00000000000x
hpib=gp,calc:data?
!+2s
gps-fmout=gp
enddef
"
" =================================
"
define  clocko1        00000000000x
hpib=gm,calc:data?
!+2s
gps-maser=gm
enddef
"
" =================================
"
define   checkmk5      00000000000x
scan_check
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5=get_stats?
mk5=status?
enddef
"
" =================================
"
define  spstsys        00000000000x
sy=sendsps.py agcoff
!+1s
sy=sendsps.py tpi
!+1s
sy=sendsps.py sigoff
!+1s
sy=sendsps.py tpzero
!+1s
sy=sendsps.py sigon
calon
!+1s
sy=sendsps.py tpical
!+1s
caloff
sy=sendsps.py agcon
enddef
"
" =================================
"
define  calm           00000000000x
sy=sendcal.py $
enddef
"
" =================================
"
define  calon          00000000000x
calm=on
!+1s
enddef
"
" =================================
"
define  caloff         00000000000x
calm=off
enddef
"
" =================================
" =================================
"
define  preses_@hds@   00000000000x
" Duration: 0 sec
proc_library
mk5=dts_id?
mk5=os_rev?
mk5=ss_rev?
mk5=status?
setmode_@mode@
enddef
"
"=================================
"
define  setmode_@mode@   00000000000x
" Duration: 0 sec
@time_stamp@
pcalon
pcald=stop
" channel configuration resembles "geo"
" Mark5B clock_set 32 MHz
mk5b_mode=ext,0xffffffff,1
mk5b_mode
bank_check
"
vci01=@if_offset@,a,@if_width@,r
vci02=@if_offset@,a,@if_width@,r
vci03=@if_offset@,a,@if_width@,r
vci04=@if_offset@,a,@if_width@,r
vci05=@if_offset@,a,@if_width@,r
vci06=@if_offset@,a,@if_width@,r
vci07=@if_offset@,a,@if_width@,r
vci08=@if_offset@,a,@if_width@,r
vci09=@if_offset@,b,@if_width@,r
vci10=@if_offset@,b,@if_width@,r
vci11=@if_offset@,b,@if_width@,r
vci12=@if_offset@,b,@if_width@,r
vci13=@if_offset@,b,@if_width@,r
vci14=@if_offset@,b,@if_width@,r
"
sy=sendsps.py x
sy=sendsps.py geo
enddef
"
"=================================
"
define  setscan_@hds@   00000000000x
" Duration: 1 sec
checkmk5
enddef
"
"=================================
"
define  preob_@hds@     00000000000x
" Duration: 10 sec
onsource
spstsys
enddef
"
"=================================
"
define  midob_@hds@     00000000000x
" Duration: 1 sec
onsource
disk_record=on
disk_record
data_valid=on
wx
clocko
clocko1
mk5=status?
sy=run setcl &
subr=?
mk5=dot?
enddef
"
"=================================
"
define  postob_@hds@    00000000000x
" Duration: 1 sec
data_valid=off
disk_record=off
subr=?
mk5=dot?
enddef
"
"=================================
"
define  postses_@hds@        00000000000x
" Duration: 0 sec
" End of schedule
sched_end
enddef
