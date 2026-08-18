      SUBROUTINE FRESEL_TID ( FRESEL, LUN_LOG, IVRB, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Routine FRESEL_TID
! *                                                                      *
! *  ### 07-APR-2006   FRESEL_TID   v1.1 (c) L. Petrov  27-JUN-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'tid_0001.i'
      INCLUDE   'fresel.i'
      TYPE ( FRESEL__STRU ) :: FRESEL
      INTEGER*4  IVRB, LUN_LOG, IUER
      REAL*8     FRQ_LAST, FRQ_MIN, FRQ_MAIN, PHS_MAIN, &
     &           FRQ_LEFT, FRQ_RIGHT, AMP_LEFT, AMP_RIGHT
      REAL*8     TOL_FRQ, AMP_EPS, AMP_MIN
      PARAMETER  ( TOL_FRQ = 0.90D0  )
      PARAMETER  ( AMP_EPS = 0.90D0  )
      PARAMETER  (  AMP_MIN = 1.D-5  )
      LOGICAL*4  TID_USED(M_HEO), NOTID2_USED(M_HEO), NOTID3_USED(M_HEO), &
     &           FL_UT1_USED
      REAL*8     TIM_SDL(M_PWR), PMC_SDL(M_PWR), PMS_SDL(M_PWR), FRQ_DIF_MIN 
      REAL*8     CI_COS, CI_SIN, SI_COS, SI_SIN, AC_INTG, AS_INTG, &
     &           PC_INTG, PS_INTG, AC_SDL(M_PWR), PC_SDL(M_PWR), VAL(M_PWR)
      REAL*8     FRQ_TID(M_HEO), PHS_TID(M_HEO), AMP_TID(M_HEO), &
     &           ARR1(M_HEO), ARR2(M_HEO), ARR3(M_HEO), ARR4(M_HEO)
      INTEGER*4  TID_IND(M_HEO), IND, IND_USED, IND_PREV, IND_NEXT, IND_TID, &
     &           N_SDL, K_SDL, IND_SDL(M_HEO), N_VAR, K_TID, ITURN, ISGN, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, IER
      REAL*8     ATAN_CS
!
      FRESEL%N_TID = 0
      FRQ_LAST = 0.0D0
      FRQ_MIN = PI2/(FRESEL%TIME_END - FRESEL%TIME_BEG)
!
! --- Read arrays of tidal constituents of the 2-nd degree
!
      K_TID = 0
      DO 410 J1=1,2
         IF ( J1 .EQ. 1 ) ISGN = -1
         IF ( J1 .EQ. 2 ) ISGN =  1
         DO 420 J2=1,NTID
            NOTID2_USED(J2) = .FALSE.
            IF ( ( L(J2) == 2  .AND.  TID_FREQ(J2) .GT. FRQ_LOW_ZON ) .OR. &
     &           ( L(J2) == 3  .AND.  TID_FREQ(J2) .GT. 2.5D0*OM__EAR )    ) THEN
                 K_TID = K_TID + 1
                 FRQ_TID(K_TID) = ISGN*TID_FREQ(J2)
                 PHS_TID(K_TID) = ISGN*TID_PHAS(J2)
                 AMP_TID(K_TID) = TID_AMPL(J2)
                 TID_IND(K_TID) = J2
                 ARR1(K_TID) = -AMP_TID(K_TID)
                 ARR2(K_TID) = K_TID + 1.D-5
            END IF
 420     CONTINUE
 410  CONTINUE
!
! --- Sort array of tidal constituents in decreasing their amplitudes
!
      CALL SORT8 ( K_TID, ARR1, ARR2 )
!
      FRQ_LAST = -1.0D11
      N_VAR = 0
!
      DO 440 J4=1,K_TID
         IND = IDNINT(ARR2(J4))
         TID_USED(IND) = .FALSE.
         IF ( DABS(FRQ_TID(IND)) > FRQ_LOW_ZON .AND. &
     &        DABS(FRQ_TID(IND)) < FRQ_LOW ) THEN
              IF ( AMP_TID(IND) < FRESEL%TIDZON_MIN ) GOTO 440
           ELSE IF ( DABS(FRQ_TID(IND)) > FRQ_LOW ) THEN
              IF ( AMP_TID(IND) < FRESEL%TIDAMP_MIN ) GOTO 440
         END IF
!
         IF ( DABS( FRQ_TID(IND) - FRQ_LAST ) .GT. FRQ_MIN*TOL_FRQ ) THEN
!
! ----------- This frequency is beyond the FRQ_MIN interval with respect
! ----------- to the previous tidal frequency
! ----------- Search among nutation frequencies
!
              IND_USED = 0
              FRQ_DIF_MIN = 1.D10
              DO 450 J5=1,FRESEL%N_FRQ
                 IF ( FRESEL%DAT(J5)%TYP == IND__PRC ) GOTO 450
                 IF ( DABS( FRQ_TID(IND) - FRESEL%DAT(J5)%FRQ ) .LT. &
     &                FRQ_DIF_MIN ) THEN
!
                      FRQ_DIF_MIN = DABS( FRQ_TID(IND) - FRESEL%DAT(J5)%FRQ )  
                      IF ( FRQ_DIF_MIN < FRQ_MIN ) THEN
                           IND_USED = J5
                      END IF
                 END IF
 450          CONTINUE
!
              IF ( IND_USED .EQ. 0 ) THEN
!
! ---------------- This frequency has not been previously used
!
                   FRESEL%N_TID = FRESEL%N_TID + 1
                   FRESEL%N_FRQ = FRESEL%N_FRQ + 1
                   FRESEL%DAT(FRESEL%N_FRQ)%PHS  = PHS_TID(IND)
                   FRESEL%DAT(FRESEL%N_FRQ)%FRQ  = FRQ_TID(IND)
                   FRESEL%DAT(FRESEL%N_FRQ)%ACCL = 0.0D0
                   FRESEL%DAT(FRESEL%N_FRQ)%PMC  = AMP_TID(IND)
                   FRESEL%DAT(FRESEL%N_FRQ)%PMS  = 0.0D0
                   FRESEL%DAT(FRESEL%N_FRQ)%IND  = TID_IND(IND)
                   FRESEL%DAT(FRESEL%N_FRQ)%TYP  = IND__TID
                   FRESEL%DAT(FRESEL%N_FRQ)%TID_AMPL = AMP_TID(IND)
                   FRESEL%DAT(FRESEL%N_FRQ)%IND_MAIN = 0
                   FRQ_LAST = FRQ_TID(IND)
!
                   IF ( FRESEL%PM_EST_TID ) THEN
                        FRESEL%DAT(FRESEL%N_FRQ)%USE_PM = .TRUE.
                      ELSE
                        FRESEL%DAT(FRESEL%N_FRQ)%USE_PM = .FALSE.
                   END IF
!
                   IF ( FRESEL%UT1_EST_TID .AND. &
     &                  FRESEL%DAT(FRESEL%N_FRQ)%FRQ .LT. -FRQ_LOW_ZON ) THEN
!
! --------------------- variations in UT1 are allowed only for 
! --------------------- a negative frequency
!
                        FRESEL%DAT(FRESEL%N_FRQ)%USE_UT1 = .TRUE.
                      ELSE
                        FRESEL%DAT(FRESEL%N_FRQ)%USE_UT1 = .FALSE.
                   END IF
!
                   FRESEL%DAT(FRESEL%N_FRQ)%USE_PM  = .TRUE.
                   FRESEL%DAT(FRESEL%N_FRQ)%L_PWR   = 0
                   TID_USED(IND) = .TRUE.
!
! ---------------- Resolve 2 Pi ambiguity
!
                   ITURN = IDNINT ( FRESEL%DAT(FRESEL%N_FRQ)%PHS/PI2 )
                   FRESEL%DAT(FRESEL%N_FRQ)%PHS = &
     &                    FRESEL%DAT(FRESEL%N_FRQ)%PHS - ITURN*PI2
                   IF ( FRESEL%DAT(FRESEL%N_FRQ)%PHS .LT. 0.0D0 ) THEN
                        FRESEL%DAT(FRESEL%N_FRQ)%PHS = &
     &                    FRESEL%DAT(FRESEL%N_FRQ)%PHS + PI2
                   END IF
!
                   ARR3(FRESEL%N_TID) = FRQ_TID(IND)
                   ARR4(FRESEL%N_TID) = FRESEL%N_FRQ + 1.D-6
                 ELSE
!
! ---------------- This frequency has already been used
!
                   IF ( FRESEL%DAT(IND_USED)%TYP .NE. IND__TID ) THEN
                        IF ( FRESEL%UT1_EST_TID                      .AND. &
     &                       FRESEL%DAT(IND_USED)%TYP .NE. IND__OFFS .AND. &
     &                       FRESEL%DAT(IND_USED)%TYP .NE. IND__PRC  .AND. &
     &                       FRESEL%DAT(IND_USED)%FRQ .LT. -FRQ_LOW_ZON ) THEN
!
! -------------------------- Check, does among frequencies in the FRQ_MIN
! -------------------------- visinity from  IND_USED there is a constituent
! -------------------------- with estimation of E3
!
                             FL_UT1_USED = .FALSE.
                             DO 460 J6=1,FRESEL%N_FRQ
                                IF ( FRESEL%DAT(IND_USED)%USE_UT1 .AND. &
     &                               DABS(FRESEL%DAT(J6)%FRQ - &
     &                                    FRESEL%DAT(IND_USED)%FRQ ) < FRQ_MIN ) THEN
                                     FL_UT1_USED = .TRUE.
                                END IF
 460                         CONTINUE 
                             IF ( .NOT. FL_UT1_USED ) THEN
                                  FRESEL%DAT(IND_USED)%USE_UT1 = .TRUE.
                             END IF
                        END IF
                        FRESEL%DAT(IND_USED)%TID_AMPL = AMP_TID(IND)
!
! --------------------- Store information, that the tidal freuquency such
! --------------------- and such was used for no-tidal EOP estimation,
! --------------------- f.e. nutation
!
                        IF ( TID_IND(IND) .LT. 10000 ) THEN
                             NOTID2_USED(TID_IND(IND)) = .TRUE.
                           ELSE
                             NOTID3_USED(TID_IND(IND)-10000) = .TRUE.
                        END IF
                        TID_USED(IND) = .TRUE.
                   END IF
              END IF ! IND_USED .EQ. 0
         END IF
 440  CONTINUE
      IF ( IVRB .GT. 1 ) THEN
           WRITE ( 6, * ) ' FRESEL%N_TID = ', FRESEL%N_TID
           WRITE ( 6, * ) ' FRESEL%N_FRQ = ', FRESEL%N_FRQ
      END IF
!
! --- Sort array of tidal constituents in increasing their frequencies
!
      CALL SORT8 ( FRESEL%N_TID, ARR3, ARR4 )
!
      IF ( IVRB .GE. 3 ) THEN
           DO 470 J7=1,FRESEL%N_TID
              IND = IDNINT(ARR4(J7))
              IF ( J7 .EQ. 1 ) FRQ_LAST = FRESEL%DAT(IND)%FRQ
              WRITE ( 6, 120 ) J7, FRESEL%DAT(IND)%TYP, FRESEL%DAT(IND)%IND, &
     &                             FRESEL%DAT(IND)%PHS, FRESEL%DAT(IND)%FRQ, &
     &                            (FRESEL%DAT(IND)%FRQ - FRQ_LAST)/FRQ_MIN
 120          FORMAT ( I4,') ', I2,' Ind=',I6,' PHS=',F12.10, &
     &                       ' FRQ=',1PD19.12,'  Dif=',0PF8.2 )
              FRQ_LAST = FRESEL%DAT(IND)%FRQ
 470       CONTINUE
      END IF
!
! --- Now search of sidelobes
!
      DO 480 J8=1,FRESEL%N_TID
         N_SDL = 0
         IND = IDNINT(ARR4(J8))
         FRQ_MAIN = FRESEL%DAT(IND)%FRQ
         PHS_MAIN = FRESEL%DAT(IND)%PHS
         IF ( FRQ_MAIN .GT. FRQ_LOW_ZON ) N_VAR = N_VAR + 2
!
         IF ( J8 .GT. 1 ) THEN
              IND_PREV = IDNINT(ARR4(J8-1))
              FRQ_LEFT = FRQ_MAIN + &
     &                    ( FRESEL%DAT(IND_PREV)%FRQ - FRQ_MAIN )/2.0D0
              AMP_LEFT = AMP_TID(IND_PREV)
            ELSE
              IF ( FRQ_MAIN .LT. 0.D0 ) THEN
                   FRQ_LEFT = 2*FRQ_MAIN
                   AMP_LEFT = 0.0D0
                 ELSE
                   FRQ_LEFT = FRQ_LOW_ZON
                   AMP_LEFT = 0.0D0
              END IF
         END IF
!
         IF ( J8 .LT. FRESEL%N_TID ) THEN
              IND_NEXT = IDNINT(ARR4(J8+1))
              FRQ_RIGHT = FRQ_MAIN + &
     &                    ( FRESEL%DAT(IND_NEXT)%FRQ - FRQ_MAIN )/2.0D0
              AMP_RIGHT = AMP_TID(IND_NEXT)
            ELSE
              IF ( FRQ_MAIN .GT. 0.D0 ) THEN
                   FRQ_RIGHT = 2.D0*FRQ_MAIN
                   AMP_RIGHT = 0.0D0
                 ELSE
                   FRQ_RIGHT = -FRQ_LOW_ZON
                   AMP_RIGHT = 0.0D0
              END IF
         END IF
!
         IF ( J8 > 1 ) THEN
              DO 490 J9=1,J8
                 IND_TID = IDNINT(ARR2(J9))
                 IF ( FRQ_TID(IND_TID) .GT. FRQ_LEFT   .AND.  &
     &                FRQ_TID(IND_TID) .LT. FRQ_RIGHT  .AND.  &
     &                .NOT. TID_USED(IND_TID)                 ) THEN
!
! ------------------- This frequency is a sidelobe
!
                      IF ( AMP_LEFT > AMP_RIGHT ) THEN
                           FRESEL%DAT(IND)%IND_MAIN = IND_PREV
                         ELSE 
                           FRESEL%DAT(IND)%IND_MAIN = IND_NEXT
                      END IF
                 END IF
 490          CONTINUE
         END IF
 480  CONTINUE
!
      FRESEL%TID_RMS = 0.0D0
      FRESEL%TID_MAX = 0.0D0
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE   FRESEL_TID !#!#
