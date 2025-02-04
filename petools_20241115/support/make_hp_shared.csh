#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Auxiliary rouine make_hp_shared creates a shared library from      *
# *   arcive object library at HP-UX. It makes the same as Linux command *
# *   ld -shared --whole-archive -o $2 $1                                *
# *                                                                      *
# *   This routine uese a temprary directory for dumping object modules. *
# *                                                                      *
# *   Usage:                                                             *
# *     make_hp_shared.csh <archive-lib> <shared-lib> <temp_dir_name>    *
# *                                                                      *
# *   where <archive-lib>   -- name of the existing archive library;     *
# *         <shared-lib>    -- name of the shared library to be created. *
# *         <temp_dir_name> -- name of the temporary directory, which    *
# *                            should not exist. make_hp_shared will     *
# *                            create this directory, put there object   *
# *                            files andthen remove this directory.      *
# *                                                                      *
# *  ### 26-DEC-2003               v1.0 (c)  L. Petrov  26-DEC-2003 ###  *
# *                                                                      *
# ************************************************************************
set alib     = $1
set slib     = $2
set temp_dir = $3
set temp_fil = /tmp/make_hp_shared__$$
#
if ( $temp_dir == "" ) then
     echo "make_hp_shared: third argument has not been supplied"
     exit 1
endif
#
set pwd_old = `pwd`
#
if ( -d $temp_dir ) then
     echo "make_hp_shared: $temp_dir already exists"
     exit 1
endif
#
mkdir $temp_dir
if ( $status != 0 ) then
     echo "make_hp_shared: Failure to create $temp_dir"
     exit 1
endif
cd  $temp_dir
#
# --- Dump contents of a-library to the temporary directory
#
ar -x $alib
#
# --- Put the listing of the a-library in the temp_fil
#
ar -t $alib >! $temp_fil 
#
# --- Create shared library
#
ld -b -o $slib -c $temp_fil
if ( -f $slib ) then
     echo "make_hp_shared: $slib has been successfully created"
     set success = "true"
  else 
     echo "make_hp_shared: $slib has NOT BEEN created"
     set success = "false"
endif
#
# --- Remove temporary file
#
rm -f >! $temp_fil 
#
# --- go back
#
cd $pwd_old
#
# --- Remove temporary directory
#
rm -fR $temp_dir
if ( $success == "false" ) then
     exit 0
endif
