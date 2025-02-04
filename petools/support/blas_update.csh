#!/bin/csh
# ************************************************************************
# *                                                                      *
# *   C-shell program blas_update.csh updates library blasf77, which is  *
# *   the part of ATLAS software. ATLAS provides interface with          *
# *   g77 Fortran compiler. This compiler has different input/output     *
# *   libraries than Fortran90 compiler. Therefore, code compiled with   *
# *   Fortran95 would require additional Input/Opuput librarires         *
# *   supplied with g77 which conflicts with Fortran90 input/output      *
# *   libraries. Only one program of ATLAS, xerbla.f, makes input/ouput. *
# *   Re-compiling xerbla with Fortran95 compiler and replacing it in    *
# *   blasf77 solves the problem.                                        *
# *                                                                      *
# * ### 24-JUL-2003  blas_update.csh  v1.1 (c) L. Petrov 06-SEP-2003 ### *
# *                                                                      *
# ************************************************************************
#
# --- Define the full path to this libf77 library
#
if ( $1 == "" ) then
     set blasf77 = /usr/local/lib/libf77blas.a
   else
     set blasf77 = $1
end if
#
# --- Define compiler and options
#
set f95_com  = "$MK5_FC -FR  -nbs -w90 -w95 -cm -c "
#
if ( -f $blasf77 ) then
  else
     echo "blas_update: $blasf77 file has not beend found"
     exit 1
endif
#
# --- compile xerbla.f
#
$f95_com -o xerbla.o xerbla.f
if ( $status != 0 ) then
     echo $f95_status
     echo "blas_update: Failed to compiled xerbla"
     exit 2
endif
#
# --- Replace xerbla.o in blasf77 object library
#
ar r $blasf77 xerbla.o
if ( $status == 0 ) then
     rm -f xerbla.o
     echo "blas_update: $blasf77 has been sucessfully updated"
   else
     rm -f xerbla.o
     echo "blas_update: Errors in update of $blasf77"
endif

