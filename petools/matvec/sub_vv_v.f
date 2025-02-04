      SUBROUTINE SUB_VV_V ( N, VEC1, VEC2, VEC3 )
! ************************************************************************
! *                                                                      *
! *   Subroutine  SUB_VV  subtracts vector VEC2 from vector VEC1 and     *
! *   puts result to vector VEC3.                                        *
! *                                                                      *
! *  ###  12-DEC-96   SUB_VV_V    v1.1  (c)  L. Petrov   10-MAR-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N
      REAL*8     VEC1(N), VEC2(N), VEC3(N)
      INTEGER*4  J1
!
#ifdef HPUX
      CALL VEC_$DSUB_VECTOR ( VEC1, VEC2, N, VEC3 )
#else
      CALL MEMCPY ( VEC3, VEC1, %VAL(8*N) )
      CALL DAXPY  ( N, -1.0D0, VEC2, 1, VEC3, 1 )
#endif
!
      RETURN
      END  !#!  SUB_VV_V  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SUB_VV_V8 ( N, VEC1, VEC2, VEC3 )
! ************************************************************************
! *                                                                      *
! *   Subroutine  SUB_VV_V8  subtracts vector VEC2 from vector VEC2 and  *
! *   puts result to vector VEC3.                                        *
! *                                                                      *
! *  ###  12-DEC-1996  SUB_VV_V8   v1.0  (c)  L. Petrov  12-DEC-1996 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*8  N
      REAL*8     VEC1(N), VEC2(N), VEC3(N)
      INTEGER*4  CHUNK
      PARAMETER  ( CHUNK = 255*1024*1024 )
      INTEGER*8  J1, REM, IPOS
      IF ( N .LE. CHUNK ) THEN
           CALL MEMCPY ( VEC3, VEC1, %VAL(8*N) )
           CALL DAXPY  ( INT(N,KIND=4), -1.0D0, VEC2, 1, VEC3, 1 )
         ELSE
           REM  = N
           IPOS = 1
           DO 410 J1=1,N/CHUNK+1
              IF ( REM .GE. CHUNK ) THEN
                   CALL MEMCPY ( VEC3(IPOS), VEC1(IPOS), %VAL(8*CHUNK) )
                   CALL DAXPY  ( CHUNK, -1.0D0, VEC2(IPOS), 1, VEC3(IPOS), 1 )
                   IPOS = IPOS + CHUNK
                   REM = REM - CHUNK
                 ELSE
                   CALL MEMCPY ( VEC3(IPOS), VEC1(IPOS), %VAL(8*REM) )
                   CALL DAXPY  ( INT(REM,KIND=4), -1.0D0, VEC2(IPOS), 1, VEC3(IPOS), 1 )
                   IPOS = IPOS + REM
                   REM = 0
              END IF
              IF ( REM == 0 ) GOTO 810
 410       CONTINUE 
 810       CONTINUE 
      END IF
      RETURN
      END  SUBROUTINE  SUB_VV_V8  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SUB_VV_V_GEN ( N, VEC1, VEC2, VEC3 )
! ************************************************************************
! *                                                                      *
! *   Subroutine  SUB_VV  subtracts vector VEC2 from vector VEC1 and     *
! *   puts result to vector VEC3.                                        *
! *                                                                      *
! *  ###  12-DEC-1996 SUB_VV_V_GEN v1.1  (c) L. Petrov  10-MAR-1997  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N
      REAL*8     VEC1(N), VEC2(N), VEC3(N)
      INTEGER*4  J1
!
      DO 410 J1=1,N
         VEC3(J1) = VEC1(J1) - VEC2(J1)
 410  CONTINUE 
!
      RETURN
      END  SUBROUTINE  SUB_VV_V_GEN   !#!#
