#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   C-shell program fortran_complier_guess.csh is for guessing the     *
# *   name of Fortran compiler.                                          *
# *                                                                      *
# *  ### 30-OCT-2005               v1.0 (c)  L. Petrov  30-OCT-2005 ###  *
# *                                                                      *
# ************************************************************************
if ( $?MK5_FC != 0 ) then
     if ( -e $MK5_FC ) then
          echo $MK5_FC
          exit 0
     endif
endif
#
if ( $?FC != 0 ) then
     if ( -e $FC ) then
          echo $FC
          exit 0
     endif
endif
#
set f95_list = `which f95`
if ( $status == 1 ) set f95_list = ""
set f95_name = $f95_list[1]
if ( $f95_name != "" ) then
     if ( -e $f95_name ) then
          echo $f95_name 
          exit 0
     endif
endif
#
set f95_list = `which f90`
if ( $status == 1 ) set f95_list = ""
set f95_name = $f95_list[1]
if ( $f95_name != "" ) then
     if ( -e $f95_name ) then
          echo $f95_name 
          exit 0
     endif
endif
#
set f95_list = `which ifc`
if ( $status == 1 ) set f95_list = ""
set f95_name = $f95_list[1]
if ( $f95_name != "" ) then
     if ( -e $f95_name ) then
          echo $f95_name 
          exit 0
     endif
endif
#
set f95_list = `which ifort`
if ( $status == 1 ) set f95_list = ""
set f95_name = $f95_list[1]
if ( $f95_name != "" ) then
     if ( -e $f95_name ) then
          echo $f95_name 
          exit 0
     endif
endif
exit 1
