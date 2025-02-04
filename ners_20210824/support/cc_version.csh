#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   C-shell routine cc_version.csh inqures version and vendor of the   *
# *   cc compiler accessible on this system.                             *
# *                                                                      *
# * ### 03-NOV-2003  cc_version.csh v2.5 (c) L. Petrov  26-OCT-2020 ###  *
# *                                                                      *
# ************************************************************************
setenv LANG   C
setenv LC_ALL C
set temp  = /tmp/vers__$$
set tempc = /tmp/vers__$$.c
if ( -f $temp  ) rm -f $temp
if ( -f $tempc ) rm -f $tempc
if ( $2 != "" ) then
     setenv MK5_C $2
endif
if ( $?MK5_C == 0 ) then
     setenv MK5_C cc
endif
set cc_version = ""
set cc_vendor  = ""
#
# ---First check, whether the C compiler understands gcc style --version switch
#
$MK5_C --version >&! $temp 
set is_gcc = `cat $temp | grep -i GCC`
if ( "$is_gcc" == "" ) then
#
# -- Special trick for Ubuntu
#
     set is_gcc = `cat $temp | grep Ubuntu`
endif
#
#cat $temp | grep gcc
#
if ( "$is_gcc" == "" ) then
#
# -- No. it does not. Then, mayby it is a HP-UX compiler?
#
     if ( `uname` == "HP-UX" ) then
          echo "main (int argc, char *const *argv) {}" >! $tempc
          $MK5_C -V $tempc -o /dev/null >&! $temp
          set cc_version = `cat  $temp | grep ccom | awk '{print $2}'`
          set cc_vendor  = `cat  $temp | grep ccom | awk '{print $4}'`
     endif
#
# -- Or, may be it is a SunOS compiler?
#
     if ( `uname` == "SunOS" ) then
          echo "main (int argc, char *const *argv) {}" >! $tempc
          $MK5_C -V $tempc -o /dev/null >&! $temp
          set cc_vendor  = `cat  $temp | grep cc | awk '{print $2}'`
#          set cc_version = `cat  $temp | grep cc | awk '{print $NF}'`
          set cc_version = `cat  $temp | grep cc | awk '{print $4}'`
     endif
   else
#
# -- Aga. It is gcc compiler. Very well
#
     rm -f $temp
      -v >&! $temp 
     set cc_version = `$MK5_C -dumpfullversion -dumpversion`
     set cc_vendor = "gcc"
endif
if ( -f $temp  ) rm -f $temp
if ( -f $tempc ) rm -f $tempc
if ( $cc_version == "" ) set cc_version = "unknown"
if ( $cc_vendor  == "" ) set cc_vendor  = "unknown"
#
switch ( $1 )
   case "version":
     echo "$cc_version"
     breaksw
   case "vendor":
     echo "$cc_vendor"
     breaksw
   default:
     echo "Compiler: cc, vendor: $cc_vendor, version: $cc_version"
endsw
if ( -f $temp  ) rm $temp
if ( -f $tempc ) rm $tempc
