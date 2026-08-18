      SUBROUTINE LOAD_IONO_SOLVE ( FIL_IONO, MODE, MOBS, NOBS, IONO, IONO_EST, &
     &                             EXP_NAME, EXP_VERS, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine LOAD_IONO_SOLVE 
! *                                                                      *
! * ### 05-DEC-2021  LOAD_IONO_SOLVE  v1.0 (c) L. Petrov 05-DEC-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'iono_from_solve.i'
      INTEGER*4  MODE, MOBS, NOBS, EXP_VERS, IUER 
      TYPE     ( IONO__TYPE ) :: IONO(MOBS)
      TYPE     ( IONO__EST_TYPE ) :: IONO_EST
      CHARACTER  FIL_IONO*(*), EXP_NAME*(*)
      INTEGER*4    MB
      PARAMETER  ( MB = 64*1024 )
      REAL*8     ME
      LOGICAL*1  FL_PASS
      CHARACTER  BUF(MB)*512
      CHARACTER    PUT_IONO__LABEL*51
      PARAMETER  ( PUT_IONO__LABEL = '# Solve Delay Output.  Format version of 2007.02.02' )
      INTEGER*4  J1, J2, NB, IND_OBS, IER 
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( FIL_IONO, MB, BUF, NB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7101, IUER, 'LOAD_IONO_SOLVE', 'Error '// &
     &         'in reading input file '//FIL_IONO )
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
              IF ( IONO(IND_OBS)%FREQ_EFF(1) < IONO__FREQ_MIN ) THEN
                   IF ( MODE == IONO__ABND ) THEN
                        IONO(IND_OBS)%FREQ_EFF(1) = IONO_EST%EFF_FREQ_AVR(1)
                      ELSE
                        IONO(IND_OBS)%FREQ_EFF(1) = 0.9*IONO__FREQ_MIN
                   END IF
              END IF
              IF ( IONO(IND_OBS)%FREQ_EFF(2) < IONO__FREQ_MIN ) THEN
                   IF ( MODE == IONO__ABND ) THEN
                        IONO(IND_OBS)%FREQ_EFF(2) = IONO_EST%EFF_FREQ_AVR(2)
                      ELSE
                        IONO(IND_OBS)%FREQ_EFF(2) = 0.95*IONO__FREQ_MIN
                   END IF
              END IF
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
      END  SUBROUTINE  LOAD_IONO_SOLVE  !#!#
