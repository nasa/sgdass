      SUBROUTINE COPY_V8 ( N, VEC1, VEC2 )
! ************************************************************************
! *                                                                      *
! *   Subroutine  COPY_V8  copies N elements of the vector VEC1 to the   *
! *   vector  VEC2                                                       *
! *                                                                      *
! *  ###  24-OCT-2017   COPY_V8     v1.0 (c) L. Petrov  24-OCT-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*8  N
      REAL*8     VEC1(N), VEC2(N)
      INTEGER*8  CHUNK
      PARAMETER  ( CHUNK = 255*1024*1024 )
      INTEGER*8  J1, REM, IPOS
      IF ( N .GT. 0 ) THEN
#ifdef HPUX
           CALL VEC_$DCOPY ( VEC1, VEC2, N )
#else
      IF ( N .LE. CHUNK ) THEN
           CALL MEMCPY ( VEC2, VEC1, %VAL(8*N) )
         ELSE
           REM  = N
           IPOS = 1
           DO 410 J1=1,N/CHUNK+1
              IF ( REM .GE. CHUNK ) THEN
                   CALL MEMCPY ( VEC2(IPOS), VEC1(IPOS), %VAL(8*CHUNK) )
                   IPOS = IPOS + CHUNK
                   REM = REM - CHUNK
                 ELSE
                   CALL MEMCPY ( VEC2(IPOS), VEC1(IPOS), %VAL(8*REM) )
                   IPOS = IPOS + REM
                   REM = 0
              END IF
              IF ( REM == 0 ) GOTO 810
 410       CONTINUE 
 810       CONTINUE 
      END IF
#endif
      END IF
      RETURN
      END  !#!  COPY_V8  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NOUT8 ( N8, VEC )
! ************************************************************************
! *                                                                      *
! *   Subroutine  NOUT8  fill N elements of the vector  VEC by zeroes.   *
! *                                                                      *
! *  ###  10-OCT-2017   NOUT8    v1.0  (c)  L. Petrov  10-OCT-2017  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*8  N8, VEC(*)
      CALL BZERO ( VEC, %VAL(N8) )
      RETURN
      END  !#!  NOUT_I8  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NOUT8_R8 ( N, VEC )
! ************************************************************************
! *                                                                      *
! *   Subroutine NOUT8_R8 fills N elements of the vector  VEC by zeroes. *
! *                                                                      *
! *  ###  12-DEC-1996  NOUT8_R8    v1.0  (c)  L. Petrov  12-Dec-96  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*8  N
      REAL*8     VEC(N)
      INTEGER*4  J1
      IF ( N .GT. 0 ) THEN
#ifdef HPUX
      CALL VEC_$DINIT ( VEC, N, 0.0D0 )
#else
      CALL BZERO ( VEC, %VAL(8*N) )
#endif
      END IF
      RETURN
      END  !#!  NOUT8_R8  #!#
