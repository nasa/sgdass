      SUBROUTINE NOUT_R4 ( N, VEC )
! ************************************************************************
! *                                                                      *
! *   Subroutine  NOUT_R4  fill N elements of the vector  VEC by zeroes. *
! *                                                                      *
! *  ###  12-Dec-96     NOUT_R4    v1.0  (c)  L. Petrov  12-Dec-96  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N
      REAL*4     VEC(N)
      INTEGER*4  J1
      IF ( N .GT. 0 ) THEN
!!           DO 410 J1=1,N
!!              VEC(J1) = 0.0D0
!! 410       CONTINUE 
           CALL BZERO ( VEC, %VAL(4*N) )
      END IF
      RETURN
      END  !#!  NOUT_R4  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NOUT_R8 ( N, VEC )
! ************************************************************************
! *                                                                      *
! *   Subroutine  NOUT_R8  fill N elements of the vector  VEC by zeroes. *
! *                                                                      *
! *  ###  12-Dec-96     NOUT_R8    v1.0  (c)  L. Petrov  12-Dec-96  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N
      REAL*8     VEC(N)
      INTEGER*4  J1
      IF ( N .GT. 0 ) THEN
           CALL BZERO ( VEC, %VAL(8*N) )
      END IF
      RETURN
      END  !#!  NOUT_R4  #!#
