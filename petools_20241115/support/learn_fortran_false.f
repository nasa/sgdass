      PROGRAM    LEARN_FORTRAN_FALSE
! ************************************************************************
! *                                                                      *
! *   A simple program LEARN_FORTRAN_FALSE print in stdout value of      *
! *   .FALSE. as a integer*4 number.                                     *
! *                                                                      *
! * ## 20-NOV-2003 LEARN_FORTRAN_FALSE v1.0 (c) L. Petrov 20-NOV-2003 ## *
! *                                                                      *
! ************************************************************************
      LOGICAL*4  FALSE_L4
      INTEGER*4  FALSE_I4
      FALSE_L4 = .FALSE.
      CALL MEMCPY ( FALSE_I4, FALSE_L4, %VAL(4) )
      WRITE ( 6, * ) FALSE_I4
      END  !#!  LEARN_FORTRAN_FALSE  #!#
