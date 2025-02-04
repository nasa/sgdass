      SUBROUTINE IONO_READ_INFO ( FIL_IONO, MODE, MOBS, NOBS, IONO, &
     &                            EXP_NAME, EXP_VERS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine IONO_READ_INFO  
! *                                                                      *
! * ### 06-DEC-2021  IONO_READ_INFO v3.1 (c)  L. Petrov 06-DEC-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'iono_solve.i'
      CHARACTER  FIL_IONO*(*), EXP_NAME*(*)
      INTEGER*4  MODE, MOBS, NOBS, EXP_VERS, IUER 
      TYPE     ( IONO_DEL__TYPE ) :: IONO(MOBS)
      INTEGER*4    MB
      PARAMETER  ( MB = 64*1024 )
      REAL*8     ME
      LOGICAL*1  FL_PASS
      CHARACTER  BUF(MB)*512, STR*128
      INTEGER*4  J1, J2, NB, IND_OBS, IER 
!
! --- Read the file with information about the ionsphere
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FIL_IONO, MB, BUF, NB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6511, IUER, 'IONO_READ_INFO', 'Error '// &
     &         'in reading input file '//FIL_IONO )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(LABEL__IONO)) .NE. LABEL__IONO ) THEN
           CALL CLRCH ( STR )
           CALL TRAN  ( 13, BUF(1), STR )
           CALL ERR_LOG ( 6512, IUER, 'IONO_READ_INFO', 'Trap of '// &
     &         'internal control: the first line of the input '// &
     &         'file with ionospheric delay is '//TRIM(FIL_IONO)// &
     &         ' is '//TRIM(STR)//' while '//LABEL__IONO// &
     &         ' was expected' )
           RETURN 
      END IF
!
      IND_OBS = 0
      DO 410 J1=1,NB
         IF ( BUF(J1)(1:13) == '# Experiment:' ) THEN
              EXP_NAME = BUF(J1)(15:24)
              CALL CHIN ( BUF(J1)(34:36), EXP_VERS )
         END IF
!
         FL_PASS = .FALSE.
         IF ( MODE == IONO__HBND  .AND. BUF(J1)(51:51) == 'T'          ) FL_PASS = .TRUE.
         IF ( MODE == IONO__LBND  .AND. BUF(J1)(53:53) == 'T'          ) FL_PASS = .TRUE.
         IF ( MODE == IONO__HLBND .AND. BUF(J1)(51:53) == 'T T'        ) FL_PASS = .TRUE.
         IF ( MODE == IONO__3BND  .AND. BUF(J1)(51:55) == 'T T T'      ) FL_PASS = .TRUE.
         IF ( MODE == IONO__2BND  .AND. ( BUF(J1)(51:51) == 'T' .OR. &
     &                                    BUF(J1)(53:53) == 'T'      ) ) FL_PASS = .TRUE.
         IF ( MODE == IONO__ABND ) FL_PASS =.TRUE.
!
         IF ( BUF(J1)(1:9) == 'PUT_IONO:' .AND. FL_PASS ) THEN
              IND_OBS = IND_OBS + 1
              CALL NOUT ( SIZEOF(IONO(IND_OBS)), IONO(IND_OBS) )
              READ ( UNIT=BUF(J1)(31:36), FMT='(I6)' ) IONO(IND_OBS)%IND_REC
              READ ( UNIT=BUF(J1)(51:51), FMT='(L1)' ) IONO(IND_OBS)%USED(1)
              READ ( UNIT=BUF(J1)(53:53), FMT='(L1)' ) IONO(IND_OBS)%USED(2)
              READ ( UNIT=BUF(J1)(55:55), FMT='(L1)' ) IONO(IND_OBS)%USED(3)
              IONO(IND_OBS)%SOU    = BUF(J1)(58:65) 
              IONO(IND_OBS)%STA(1) = BUF(J1)(68:75) 
              IONO(IND_OBS)%STA(2) = BUF(J1)(79:86) 
              READ ( UNIT=BUF(J1)(95:99),   FMT='(I5)'     ) IONO(IND_OBS)%MJD
              READ ( UNIT=BUF(J1)(106:114), FMT='(F9.3)'   ) IONO(IND_OBS)%TAI
              READ ( UNIT=BUF(J1)(131:149), FMT='(D19.12)' ) IONO(IND_OBS)%DEL(1)
              READ ( UNIT=BUF(J1)(153:171), FMT='(D19.12)' ) IONO(IND_OBS)%DEL(2)
              READ ( UNIT=BUF(J1)(187:198), FMT='(D12.5)'  ) IONO(IND_OBS)%DEL_ERR(1)
              READ ( UNIT=BUF(J1)(203:214), FMT='(D12.5)'  ) IONO(IND_OBS)%DEL_ERR(2)
              READ ( UNIT=BUF(J1)(223:234), FMT='(D12.5)'  ) IONO(IND_OBS)%FREQ_EFF(1)
              READ ( UNIT=BUF(J1)(239:250), FMT='(D12.5)'  ) IONO(IND_OBS)%FREQ_EFF(2)
              READ ( UNIT=BUF(J1)(263:275), FMT='(D13.5)'  ) IONO(IND_OBS)%IONO_ZEN(1)
              READ ( UNIT=BUF(J1)(279:291), FMT='(D13.5)'  ) IONO(IND_OBS)%IONO_ZEN(2)
              READ ( UNIT=BUF(J1)(303:307), FMT='(F5.3)'   ) IONO(IND_OBS)%IONO_MAP(1)
              READ ( UNIT=BUF(J1)(309:313), FMT='(F5.3)'   ) IONO(IND_OBS)%IONO_MAP(2)
              IF ( IONO(IND_OBS)%FREQ_EFF(1) < IONO__FREQ_MIN ) IONO(IND_OBS)%FREQ_EFF(1) =   IONO__FREQ_MIN
              IF ( IONO(IND_OBS)%FREQ_EFF(2) < IONO__FREQ_MIN ) IONO(IND_OBS)%FREQ_EFF(2) = 2*IONO__FREQ_MIN
!
! ----------- Get ionospheric contrubition at X-band  simultaneously
!
!                            f_s**2
! ----------- Tau_{ix} = --------------- ( tau_s - tau_x )
!                        f_x**2 - f_s**2
!
              ME = (IONO(IND_OBS)%IONO_MAP(1) + IONO(IND_OBS)%IONO_MAP(2))/2.0
              IONO(IND_OBS)%IONO_V = IONO(IND_OBS)%FREQ_EFF(2)**2/ &
     &             (IONO(IND_OBS)%FREQ_EFF(2)**2 - IONO(IND_OBS)%FREQ_EFF(1)**2)* &
     &             (IONO(IND_OBS)%DEL(1)         - IONO(IND_OBS)%DEL(2))
              IONO(IND_OBS)%IONO_G = ( IONO(IND_OBS)%IONO_ZEN(2)*IONO(IND_OBS)%IONO_MAP(2) - &
     &                                 IONO(IND_OBS)%IONO_ZEN(1)*IONO(IND_OBS)%IONO_MAP(1) )
              IONO(IND_OBS)%IONO_VERR = IONO(IND_OBS)%FREQ_EFF(2)**2/ &
     &             ABS(IONO(IND_OBS)%FREQ_EFF(2)**2 - IONO(IND_OBS)%FREQ_EFF(1)**2)* &
     &             DSQRT ( IONO(IND_OBS)%DEL_ERR(1)**2 + IONO(IND_OBS)%DEL_ERR(2)**2 )
              READ ( UNIT=BUF(J1)(357:364), FMT='(F8.4)' ) IONO(IND_OBS)%EL(1)
              READ ( UNIT=BUF(J1)(372:379), FMT='(F8.4)' ) IONO(IND_OBS)%AZ(1)
              READ ( UNIT=BUF(J1)(387:394), FMT='(F8.4)' ) IONO(IND_OBS)%EL(1)
              READ ( UNIT=BUF(J1)(402:409), FMT='(F8.4)' ) IONO(IND_OBS)%AZ(1)
!
              IONO(IND_OBS)%IONO_G = IONO(IND_OBS)%IONO_G * ( IONO(IND_OBS)%FREQ_EFF(1)/IONO__FREQ_REF )**2
              IONO(IND_OBS)%IONO_V = IONO(IND_OBS)%IONO_V * ( IONO(IND_OBS)%FREQ_EFF(1)/IONO__FREQ_REF )**2
              IONO(IND_OBS)%IONO_VERR = IONO(IND_OBS)%IONO_VERR * ( IONO(IND_OBS)%FREQ_EFF(1)/IONO__FREQ_REF )**2
              IONO(IND_OBS)%IONO_ZEN  = IONO(IND_OBS)%IONO_ZEN  * ( IONO(IND_OBS)%FREQ_EFF(1)/IONO__FREQ_REF )**2
         END IF
 410  CONTINUE 
      NOBS = IND_OBS
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  IONO_READ_INFO !#!  
