      SUBROUTINE TEST_STAOBS ( LUN, NOBS, IUNW, IUNWP, SUPSTAT, UACSUP, &
     &                         LCX, LCS )
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INTEGER*4  NOBS
      INTEGER*2  LUN, IUNW, IUNWP, SUPSTAT(2), UACSUP
      LOGICAL*4  LBT_S(32), LBT_U(4)
      CHARACTER  LCX*2, LCS*2, FL*1, SL*4
      INTEGER*4  J1, J2, N$N
      LOGICAL*4  SUPR_INQ
      LOGICAL*2  KBIT
!
      DO 410 J1=1,32
         LBT_S(J1) = SUPR_INQ ( SUPSTAT, UACSUP, INT2(J1) )
 410  CONTINUE
!
      DO 420 J2=1,4
         LBT_U = KBIT ( UACSUP, INT2(J2) )
 420  CONTINUE
!
      IF ( SUPR_INQ ( SUPSTAT, UACSUP, USED__SPS ) ) THEN
           FL = 'N'
           SL = 'IUNW'
         ELSE
           FL = 'n'
           SL = 'iunw'
      END IF
      WRITE ( LUN, 110 ) FL, NOBS, SL, IUNW, ( LBT_S(N$N), N$N=1,32 ), &
     &                              ( LBT_U(N$N), N$N=1,4 ), LCX(2:2), LCS(2:2)
 110  FORMAT ( A,'=',I5,' ',A,'=',I2,' lbt_s=', 6(5L1,1X),2L1, &
     &            ' lbt_u=',4l1,' qc=',A1,'/',A1 )
      RETURN
      END  !#!  TEST_STAOBS  #!#
