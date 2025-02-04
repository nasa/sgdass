#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   C-shell routine f95_version.csh inquires version and vendor of the *
# *   Fortran95 compiler accessible on the system.                       *
# *                                                                      *
# * ### 04-AUG-2002 f95_version.csh v1.8 (c) L. Petrov  26-OCT-2020 ###  *
# *                                                                      *
# ************************************************************************
setenv LANG   C
setenv LC_ALL C
set temp = /tmp/vers__$$
if ( -f $temp ) then
     rm -f $temp
endif
if ( $2 != "" ) then
     setenv MK5_FC $2
endif
if ( $?MK5_FC == 0 ) then
     setenv MK5_FC gfortran
endif
if ( `uname` == "Linux" || `uname` == "Darwin" ) then
      $MK5_FC -V -quiet >&! $temp
      set mk5_fc_status = $status
      if ( $mk5_fc_status != 0 || "`grep error $temp | grep -v warning | head -1`" != "" ) then
           $MK5_FC --version -quiet >&! $temp
           set mk5_fc_status = $status
      endif
   else
      $MK5_FC -V >&! $temp
      set mk5_fc_status = $status
endif
#
if ( $mk5_fc_status != 0 ) then
     echo "???"
     echo "Cannot find compiler $MK5_FC"
     echo "Please check environment variable MK5_FC"
     if ( -f $temp ) rm $temp
     exit 1
endif
set str = (`cat $temp | grep -v warning | head -1 | awk '{printf "%s\n", $1}'`)
if ( "$str[1]" == "Intel(R)" ) then
      set str_6 = (`cat $temp | grep -v warning | head -1 | awk '{printf "%s\n", $6}'`)
      set str_7 = (`cat $temp | grep -v warning | head -1 | awk '{printf "%s\n", $7}'`)
      set str_8 = (`cat $temp | grep -v warning | head -1 | awk '{printf "%s\n", $8}'`)
      set str_9 = (`cat $temp | grep -v warning | head -1 | awk '{printf "%s\n", $9}'`)
      set str_10 = (`cat $temp | grep -v warning | head -1 | awk '{printf "%s\n", $10}'`)
      set str_11 = (`cat $temp | grep -v warning | head -1 | awk '{printf "%s\n", $11}'`)
#      if ( $str_6 == "EM64T-based" ) then
#           echo "Intel(R)" 
#           echo "Compiler $MK5_FC is for 64 bit applications only."
#           echo "This compiler is not suitable. Please install 32 bit compiler."
#           echo "If you have 32 bit compiler installed, please check environment variable PATH"
#           echo "and/or environment variable MK5_FC"
#	   exit 1
#      endif
      set vendor = "$str[1]"
      if ( $str_7  == "Version" ) set version = $str_8
      if ( $str_8  == "Version" ) set version = $str_9
      if ( $str_9  == "Version" ) set version = $str_10
      if ( $str_10 == "Version" ) set version = $str_11
      if ( $1 == "vendor" ) then
           echo "$vendor"
	 else
	   if ( $1 == "version" ) then
                echo "$version"
	     else
                echo "Compiler: f95, Vendor: $vendor, version: $version"
	   endif
      endif
      if ( -f $temp ) rm $temp
      exit 0
   else if ( "$str[1]" == "GNU" ) then
     set vendor  = `cat  $temp | grep -v warning | head -n 1 | awk '{print $1}'`
     set version = `$MK5_FC -dumpfullversion -dumpversion`
     if ( $1 == "vendor" ) then
          echo "$vendor"
	else
	  if ( $1 == "version" ) then
               echo "$version"
	    else
               echo "Compiler: gfortran, Vendor: $vendor, version: $version"
	  endif
     endif
     if ( -f $temp ) rm $temp
     exit 0
   else
      set str = (`cat $temp | grep -v warning | head -1 | awk '{printf "%s\n", $2}'`)
      if ( "$str[1]" == "Sun"  ) then
           set vendor = "$str[1]"
	   set NW = 5
           set str = (`cat $temp | grep -v warning | head -1 | awk '{printf "%s\n", $5}'`)
           set version = "$str[1]"
           if ( $1 == "vendor" ) then
                echo "$vendor"
	      else
	        if ( $1 == "version" ) then
                     echo "$version"
	          else
                     echo "Compiler: f95, Vendor: $vendor, version: $version"
	        endif
           endif
           if ( -f $temp ) rm $temp
	   exit 0
         else
           f90 +version >& $temp
           set str = `cat $temp`
           set vendor = "$str[1]"
           set version = "$str[$#str]"
           if ( $1 == "vendor" ) then
                echo "$vendor"
              else
     	        if ( $1 == "version" ) then
                     echo "$version"
     	          else
                     echo "Compiler: f95, vendor: $vendor, version: $version"
      	        endif
           endif
           rm -f $temp
           exit 0
     endif
endif
if ( -f $temp ) rm $temp
exit 1
