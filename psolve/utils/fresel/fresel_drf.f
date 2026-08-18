      SUBROUTINE FRESEL_DRF ( FRESEL, LUN_LOG, IVRB, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Routine FRESEL_DRF 
! *                                                                      *
! *  ### 15-FEB-2006  FRESEL_DRF   v1.0 (c)  L. Petrov  15-FEB-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'nut.i'
      INCLUDE   'fresel.i'
      TYPE ( FRESEL__STRU ) :: FRESEL
      INTEGER*4  LUN_LOG, IVRB, IUER
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 32*1024 )
      CHARACTER  BUF(MBUF)*256
      INTEGER*4  MUL_FRQ(M_HEO)
      LOGICAL*4  USE_E3(M_HEO)
      INTEGER*4  J1, J2, J3, J4, J5, NBUF, L_FRQ, L_HEO, N_PAR, M, IER
      INTEGER*4  ISGN, IP
      REAL*8     FRQ_MIN, FRQ_VAL, AMP_VAL
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IFIND_PL
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FRESEL%ADH_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3371, IUER, 'FRESEL_DRF', 'Error in '// &
     &         'attempt to read ad hock frequencies from the file: '// &
     &          FRESEL%ADH_FILE )
           RETURN
      END IF
!
      FRQ_MIN = PI2/(FRESEL%TIME_END - FRESEL%TIME_BEG)
      WRITE ( 6, * ) ' FRQ_MIN = ', FRQ_MIN 
!
      L_FRQ = 0
      N_PAR = 0 
      DO 410 J1=1,NBUF
         IF ( BUF(J1)(1:14) == 'Nut:     Term:' ) THEN
              READ ( UNIT=BUF(J1)(29:47), FMT='(F19.12)' ) FRQ_VAL
              READ ( UNIT=BUF(J1)(56:66), FMT='(F19.12)' ) AMP_VAL
              IF ( AMP_VAL > FRESEL%NUTAMP_MIN ) THEN
                   L_FRQ = L_FRQ + 1
                   MUL_FRQ(L_FRQ) = IDNINT ( FRQ_VAL/FRQ_MIN )
                   N_PAR = N_PAR + 2
                   USE_E3(L_FRQ) = .FALSE.
              END IF
         END IF
!
         IF ( BUF(J1)(1:5) == 'Band:' ) THEN
              READ ( UNIT=BUF(J1)(7:7),   FMT='(I1)' ) M
              READ ( UNIT=BUF(J1)(29:47), FMT='(F19.12)' ) FRQ_VAL
              READ ( UNIT=BUF(J1)(56:66), FMT='(F19.12)' ) AMP_VAL
              IF ( M == 0  .AND. AMP_VAL < FRESEL%TIDZON_MIN ) GOTO 410
              IF ( M >  0  .AND. AMP_VAL < FRESEL%TIDAMP_MIN ) GOTO 410
              DO 420 J2=1,2
                 IF ( J2 == 1 ) ISGN = -1
                 IF ( J2 == 2 ) ISGN =  1
                 IP = IFIND_PL ( L_FRQ, MUL_FRQ, IDNINT( ISGN*FRQ_VAL/FRQ_MIN ) )
                 IF ( J2 == 1  .AND.  IP > 0 ) THEN
                      IF ( .NOT. USE_E3(IP) ) THEN
                           USE_E3(IP) = .TRUE.
                           N_PAR = N_PAR + 2
                      END IF
                    ELSE 
                      L_FRQ = L_FRQ + 1
                      MUL_FRQ(L_FRQ) = IDNINT ( ISGN*FRQ_VAL/FRQ_MIN )
                      IF ( J2 == 1 ) THEN
                           N_PAR = N_PAR + 4
                           USE_E3(L_FRQ) = .TRUE.
                         ELSE 
                           N_PAR = N_PAR + 2
                           USE_E3(L_FRQ) = .FALSE.
                      END IF
                 END IF
 420          CONTINUE 
         END IF
 410  CONTINUE 
!
      WRITE ( 6, * ) ' L_FRQ = ', L_FRQ, ' N_PAR = ', N_PAR
!
      DO 430 J3=1,L_FRQ
         FRESEL%DAT(J3)%PHS  = 0.0D0
         FRESEL%DAT(J3)%FRQ  = MUL_FRQ(J3)*FRQ_MIN
         FRESEL%DAT(J3)%ACCL = 0.0D0
         FRESEL%DAT(J3)%PMC  = 1.0D0
         FRESEL%DAT(J3)%PMS  = 1.0D0
         FRESEL%DAT(J3)%PMC_EST = 1.0D0
         FRESEL%DAT(J3)%PMS_EST = 1.0D0
         FRESEL%DAT(J3)%TID_AMPL  = 1.0D0
         FRESEL%DAT(J3)%AC_CHE(1) = 1.0D0
         FRESEL%DAT(J3)%AS_CHE(1) = 1.0D0
         FRESEL%DAT(J3)%PC_CHE(1) = 1.0D0
         FRESEL%DAT(J3)%PS_CHE(1) = 1.0D0
         FRESEL%DAT(J3)%AC_AVR = 1.0D0
         FRESEL%DAT(J3)%AS_AVR = 1.0D0
         FRESEL%DAT(J3)%PC_AVR = 1.0D0
         FRESEL%DAT(J3)%PS_AVR = 1.0D0
         FRESEL%DAT(J3)%IND = J3
         FRESEL%DAT(J3)%TYP = IND__DRF
         FRESEL%DAT(J3)%L_PWR = 0
         CALL CLRCH  ( FRESEL%DAT(J3)%NAM )
         CALL INCH   ( MUL_FRQ(J3), FRESEL%DAT(J3)%NAM(1:7) )
         CALL CHASHR (              FRESEL%DAT(J3)%NAM(1:7) )
         FRESEL%DAT(J3)%USE_PM  = .TRUE.
         FRESEL%DAT(J3)%USE_UT1 = USE_E3(J3)
 430  CONTINUE 
      FRESEL%N_FRQ = L_FRQ
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FRESEL_DRF  !#!#
