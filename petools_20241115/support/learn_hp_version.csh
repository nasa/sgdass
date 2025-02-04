#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Script  version deterniumes version of HP-UX and sets              *
# *   an environmentr variable SOLVER_HP_VERSION to keep the version     *
# *   number. It will be set as 09 for all realeases of HP-9, will be    *
# *   set 10, for HP-10.20, and will be set 00 for all other releases.   *
# *                                                                      *
# *  ###  17-JUN-98     version   v2.0  (c)  L. Petrov  21-FEB-2001 ###  *
# *                                                                      *
# ************************************************************************
if ( `uname` == "Linux" ) then
      setenv SOLVE_HP_VERSION "00"
      exit 0
endif
set major_version = `uname -r | awk '{FS="."} {printf "%s \n", $2}'` 
set minor_version = `uname -r | awk '{FS="."} {printf "%s \n", $3}'` 
switch ( $major_version ) 
  case "09":
     setenv SOLVE_HP_VERSION "09"
     breaksw
  case "10":
     if ( $minor_version == "20" ) then
          setenv SOLVE_HP_VERSION "10"
        else
          echo HP-UX version `uname -r` is not supported by SOLVE\!
          setenv SOLVE_HP_VERSION "00"
     endif
     breaksw
  case "11":
     setenv SOLVE_HP_VERSION "11"
     breaksw
  default:
     echo HP-UX version `uname -r` is not supported by SOLVE\!
     setenv SOLVE_HP_VERSION "00"
     breaksw
endsw
# echo "SOLVE_HP_VERSION = " $SOLVE_HP_VERSION 
