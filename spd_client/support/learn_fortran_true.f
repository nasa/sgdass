      PROGRAM    LEARN_FORTRAN_TRUE
! ************************************************************************
! *                                                                      *
! *   A simple program LEARN_FORTRAN_FALSE print in stdout value of      *
! *   .TRUE. as a integer*4 number.                                      *
! *                                                                      *
! * ## 20-NOV-2003 LEARN_FORTRAN_TRUE v1.0 (c) L. Petrov 20-NOV-2003 ##  *
! *                                                                      *
! ************************************************************************
      LOGICAL*4  TRUE_L4
      INTEGER*4  TRUE_I4
      TRUE_L4 = .TRUE.
      CALL MEMCPY ( TRUE_I4, TRUE_L4, %VAL(4) )
      WRITE ( 6, * ) TRUE_I4
      END  !#!  LEARN_FORTRAN_TRUE  #!#
