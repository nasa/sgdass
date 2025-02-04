      SUBROUTINE ADD_VV ( N, VEC1, VEC2 )
! ************************************************************************
! *                                                                      *
! *   Subroutine  ADD_VV  adds vector VEC2 to vector VEC1 and puts sum   *
! *   to vector VEC1.                                                    *
! *                                                                      *
! *  ###  12-Dec-96    ADD_VV      v1.0  (c)  L. Petrov   12-Dec-96 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N, J1
      REAL*8     VEC1(N), VEC2(N)
!
      CALL DAXPY ( N, 1.0D0, VEC2, 1, VEC1, 1 )
!
      RETURN
      END  !#!  ADD_VV  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ADD_VV8 ( N, VEC1, VEC2 )
! ************************************************************************
! *                                                                      *
! *   Subroutine  ADD_VV8  adds vector VEC2 from vector VEC2 and puts    *
! *   result to vector VEC1.                                             *
! *                                                                      *
! *  ###  03-NOV-2017   ADD_VV8    v1.0  (c)  L. Petrov  03-NOV-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*8  N
      REAL*8     VEC1(N), VEC2(N)
      INTEGER*4  CHUNK
      PARAMETER  ( CHUNK = 1023*1024*1024 )
      INTEGER*8  J1, REM, IPOS
      IF ( N .LE. CHUNK ) THEN
           CALL DAXPY ( INT(N,KIND=4), 1.0D0, VEC2, 1, VEC1, 1 )
         ELSE
           REM  = N
           IPOS = 1
           DO 410 J1=1,N/CHUNK+1
              IF ( REM .GE. CHUNK ) THEN
                   CALL DAXPY  ( CHUNK, 1.0D0, VEC2(IPOS), 1, VEC1(IPOS), 1 )
                   IPOS = IPOS + CHUNK
                   REM = REM - CHUNK
                 ELSE
                   CALL DAXPY  ( INT(REM,KIND=4), 1.0D0, VEC2(IPOS), 1, VEC1(IPOS), 1 )
                   IPOS = IPOS + REM
                   REM = 0
              END IF
              IF ( REM == 0 ) GOTO 810
 410       CONTINUE 
 810       CONTINUE 
      END IF
      RETURN
      END  SUBROUTINE  ADD_VV8  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ADD_VV_GEN ( N, VEC1, VEC2 )
! ************************************************************************
! *                                                                      *
! *   Subroutine  ADD_VV_GEN adds vector VEC2 to vector VEC1 and puts    *
! *   sum to vector VEC1.                                                *
! *                                                                      *
! * ###  12-DEC-1996  ADD_VV_GEN  v1.0  (c)  L. Petrov   12-DEC-1996 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N, J1
      REAL*8     VEC1(N), VEC2(N)
!
      DO 410 J1=1,N
         VEC1(J1) = VEC1(J1) + VEC2(J1)
 410  CONTINUE
!
      RETURN
      END  !#!  ADD_VV  #!#
