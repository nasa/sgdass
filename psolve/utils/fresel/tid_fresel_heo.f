      SUBROUTINE TID_FRESEL_HEO ( FRESEL, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine TID_FRESEL_HEO
! *                                                                      *
! *  ### 03-NOV-2003  TID_FRESEL_HEO v2.0 (c) L. Petrov 28-MAY-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'tid_0001.i'
      INCLUDE   'fresel.i'
      TYPE ( FRESEL__STRU ) :: FRESEL
      INTEGER*4  IVRB, IUER
      REAL*8     FRQ_LAST, FRQ_MIN, FRQ_MAIN, PHS_MAIN, FRQ_LEFT, FRQ_RIGHT
      REAL*8     TOL_FRQ, AMP_EPS, AMP_MIN
      PARAMETER  ( TOL_FRQ = 0.90D0  )
      PARAMETER  ( AMP_EPS = 0.90D0  )
      PARAMETER  (  AMP_MIN = 1.D-5  )
      LOGICAL*4  TID_USED(M_HEO), NOTID2_USED(M_HEO), NOTID3_USED(M_HEO)
      REAL*8     TIM_SDL(M_PWR), PMC_SDL(M_PWR), PMS_SDL(M_PWR)
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
!
! ------ Read arrays of tidal constiuntes of the 3-rd degree
!
!         DO 430 J3=1,NW3
!            NOTID3_USED(J3) = .FALSE.
!            IF ( FREQ3(J3) .GT. FRQ_LOW ) THEN
!                 K_TID = K_TID + 1
!                 FRQ_TID(K_TID) = ISGN*FREQ3(J3)
!                 PHS_TID(K_TID) = ISGN*PHAS3(J3)
!                 AMP_TID(K_TID) = AMPL3(J3)
!                 TID_IND(K_TID) = 10000+J3
!                 ARR1(K_TID) = -AMP_TID(K_TID)
!                 ARR2(K_TID) = K_TID + 1.D-5
!            END IF
! 430     CONTINUE
 410  CONTINUE
!
! --- Sort array of tidal constituents in decreasing their amplitudes
!
      CALL SORT8 ( K_TID, ARR1, ARR2 )
!
      FRQ_LAST = -1.0D11
      N_VAR = 0
!
!@  write ( 6, * ) ' FRESEL%TIDZON_MIN = ', FRESEL%TIDZON_MIN ; call pause ( 'aa' ) ! %%
      DO 440 J4=1,K_TID
         IND = IDNINT(ARR2(J4))
         TID_USED(IND) = .FALSE.
         IF ( DABS(FRQ_TID(IND)) > FRQ_LOW_ZON .AND. &
     &        DABS(FRQ_TID(IND)) < FRQ_LOW ) THEN
              IF ( AMP_TID(IND) < FRESEL%TIDZON_MIN ) GOTO 440
           ELSE IF ( DABS(FRQ_TID(IND)) > FRQ_LOW ) THEN
              IF ( AMP_TID(IND) < FRESEL%TIDAMP_MIN ) GOTO 440
         END IF
!@  write ( 6, * ) ' FRQ_TID(IND) = ', FRQ_TID(IND), ' amp_tid =', amp_tid(ind) ! %%
!@  call pause ( 'A2' )  ! %%%
!
         IF ( DABS( FRQ_TID(IND) - FRQ_LAST ) .GT. FRESEL%NF*TOL_FRQ ) THEN
!
! ----------- This frequency is beyond the FRESEL%NF interval with respect
! ----------- to the previous tidal frequency
! ----------- Search among nutation frequencies
!
              IND_USED = 0
              DO 450 J5=1,FRESEL%N_FRQ
                 IF ( FRESEL%DAT(J5)%TYP == IND__PRC ) GOTO 450
                 IF ( DABS( FRQ_TID(IND) - FRESEL%DAT(J5)%FRQ ) .LT. &
     &                FRESEL%NF ) THEN
!
                      IND_USED = J5
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
     &                       FRESEL%DAT(IND_USED)%TYP .NE. IND__PRC  .AND. &
     &                       FRESEL%DAT(IND_USED)%FRQ .LT. -FRQ_LOW_ZON ) THEN
!
                             FRESEL%DAT(IND_USED)%USE_UT1 = .TRUE.
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
           DO 460 J6=1,FRESEL%N_TID
              IND = IDNINT(ARR4(J6))
              IF ( J6 .EQ. 1 ) FRQ_LAST = FRESEL%DAT(IND)%FRQ
              WRITE ( 6, 120 ) J6, FRESEL%DAT(IND)%TYP, FRESEL%DAT(IND)%IND, &
     &                             FRESEL%DAT(IND)%PHS, FRESEL%DAT(IND)%FRQ, &
     &                            (FRESEL%DAT(IND)%FRQ - FRQ_LAST)/FRESEL%NF
 120          FORMAT ( I4,') ', I2,' Ind=',I6,' PHS=',F12.10, &
     &                       ' FRQ=',1PD19.12,'  Dif=',0PF8.2 )
              FRQ_LAST = FRESEL%DAT(IND)%FRQ
 460       CONTINUE
      END IF
!
! --- Now search of sidelobes
!
      DO 470 J7=1,FRESEL%N_TID
         N_SDL = 0
         IND = IDNINT(ARR4(J7))
         FRQ_MAIN = FRESEL%DAT(IND)%FRQ
         PHS_MAIN = FRESEL%DAT(IND)%PHS
         IF ( FRQ_MAIN .GT. FRQ_LOW_ZON ) N_VAR = N_VAR + 2
!
         IF ( J7 .GT. 1 ) THEN
              IND_PREV = IDNINT(ARR4(J7-1))
              FRQ_LEFT = FRQ_MAIN + &
     &                    ( FRESEL%DAT(IND_PREV)%FRQ - FRQ_MAIN )/2.0D0
            ELSE
              IF ( FRQ_MAIN .LT. 0.D0 ) THEN
                   FRQ_LEFT = 2*FRQ_MAIN
                 ELSE
                   FRQ_LEFT = FRQ_LOW_ZON
              END IF
         END IF
!
         IF ( J7 .LT. FRESEL%N_TID ) THEN
              IND_NEXT = IDNINT(ARR4(J7+1))
              FRQ_RIGHT = FRQ_MAIN + &
     &                    ( FRESEL%DAT(IND_NEXT)%FRQ - FRQ_MAIN )/2.0D0
            ELSE
              IF ( FRQ_MAIN .GT. 0.D0 ) THEN
                   FRQ_RIGHT = 2.D0*FRQ_MAIN
                 ELSE
                   FRQ_RIGHT = -FRQ_LOW_ZON
              END IF
         END IF
!
         DO 480 J8=1,K_TID
            IND_TID = IDNINT(ARR2(J8))
            IF ( FRQ_TID(IND_TID) .GT. FRQ_LEFT   .AND.  &
     &           FRQ_TID(IND_TID) .LT. FRQ_RIGHT  .AND.  &
     &           .NOT. TID_USED(IND_TID)                 ) THEN
!
! -------------- This frequency is a sidelobe
!
                 N_SDL = N_SDL + 1
                 IND_SDL(N_SDL) = IND_TID
                 TID_USED(IND_TID) = .TRUE.
            END IF
 480     CONTINUE
         IF ( N_SDL .GT. 0 ) THEN
              K_SDL = K_SDL + 1
         END IF
         DO 490 J9=1,M_PWR
!
! --------- Compute varying amplitude at the points of Chebyshev alternance
!
            TIM_SDL(J9) = FRESEL%TIME_BEG+(FRESEL%TIME_END - FRESEL%TIME_BEG)* &
     &                   (1.0D0 + DCOS( (M_PWR-J9)*PI__NUM/(M_PWR-1) ) )/2.D0
            CI_SIN = 0.0D0
            CI_COS = FRESEL%DAT(IND)%PMC
!
            IF ( N_SDL .GT. 0 ) THEN
!
! -------------- Cycle over all sidelobes and update CI_COS, CI_SIN
!
                 DO 4100 J10=1,N_SDL
                    CI_COS = CI_COS + AMP_TID(IND_SDL(J10))*  &
     &                  DCOS( (FRQ_TID(IND_SDL(J10))-FRQ_MAIN)*TIM_SDL(J9) + &
     &                        (PHS_TID(IND_SDL(J10))-PHS_MAIN) )
                    CI_SIN = CI_SIN + AMP_TID(IND_SDL(J10))*  &
     &                  DSIN( (FRQ_TID(IND_SDL(J10))-FRQ_MAIN)*TIM_SDL(J9) + &
     &                        (PHS_TID(IND_SDL(J10))-PHS_MAIN) )
 4100            CONTINUE
            END IF
            AC_SDL(J9) = DSQRT ( CI_COS**2 + CI_SIN**2 )
            IF ( AC_SDL(J9) .GT. AMP_MIN ) THEN
                 PC_SDL(J9) = ATAN_CS ( CI_COS, CI_SIN )
              ELSE
                 PC_SDL(J9) = 0.0D0
            END IF
!
            IF ( J9 .GT. 1 ) THEN
!
! -------------- Resolve phase ambiguity. There should not been PI2 jumps in
! -------------- phase; otherwise interpolation will be screwed up
!
                 ITURN = IDNINT ( (PC_SDL(J9) - PC_SDL(J9-1))/PI2 )
                 PC_SDL(J9) = PC_SDL(J9) - ITURN*PI2
            END IF
 490     CONTINUE
!
         FRESEL%DAT(IND)%L_PWR = M_PWR ! Reserve for future needs...
         CALL NOUT_R8 ( M_PWR, FRESEL%DAT(IND)%AS_CHE )
         CALL NOUT_R8 ( M_PWR, FRESEL%DAT(IND)%PS_CHE )
!
! ------ Compute coefficients of expansion of the varying amplitudes in
! ------ Chebyshev polynomials
!
         CALL COPY_R8 ( M_PWR, AC_SDL, VAL ) ! CHCR destroys array of values
         CALL CHCR ( M_PWR, FRESEL%TIME_BEG, &
     &               (FRESEL%TIME_END - FRESEL%TIME_BEG), TIM_SDL, VAL, &
     &                FRESEL%DAT(IND)%AC_CHE )
!
         CALL COPY_R8 ( M_PWR, PC_SDL, VAL )
         CALL CHCR ( M_PWR, FRESEL%TIME_BEG, &
     &               (FRESEL%TIME_END - FRESEL%TIME_BEG), TIM_SDL, VAL, &
     &               FRESEL%DAT(IND)%PC_CHE )
!
! ------ Compute the average value of AC
!
         CALL CHINTI ( M_PWR, FRESEL%TIME_BEG, FRESEL%TIME_BEG, &
     &                 FRESEL%TIME_END, (FRESEL%TIME_END - FRESEL%TIME_BEG), &
     &                 FRESEL%DAT(IND)%AC_CHE, AC_INTG, -3 )
         FRESEL%DAT(IND)%AC_AVR = AC_INTG/(FRESEL%TIME_END - FRESEL%TIME_BEG)
!
!
! ------ Compute the average value of PC
!
         CALL CHINTI ( M_PWR, FRESEL%TIME_BEG, FRESEL%TIME_BEG, &
     &                 FRESEL%TIME_END, (FRESEL%TIME_END - FRESEL%TIME_BEG), &
     &                 FRESEL%DAT(IND)%PC_CHE, PC_INTG, -3 )
         FRESEL%DAT(IND)%PC_AVR = PC_INTG/(FRESEL%TIME_END - FRESEL%TIME_BEG)
!
! ------ and resolve ambiguity in PC_AVR
!
         ITURN = IDNINT ( FRESEL%DAT(IND)%PC_AVR/PI2 )
         FRESEL%DAT(IND)%PC_AVR = FRESEL%DAT(IND)%PC_AVR - ITURN*PI2
!
! ------ Now normalize AC_SDL
!
         DO 4110 J11=1,M_PWR
            IF ( DABS(FRESEL%DAT(IND)%AC_AVR) .GT. AMP_MIN ) THEN
                 AC_SDL(J11) = AC_SDL(J11)/FRESEL%DAT(IND)%AC_AVR - 1.0D0
            END IF
!
            IF ( FRESEL%DAT(IND)%PC_AVR .GT.  3.0D0*PI__NUM/4.0D0 .OR. &
     &           FRESEL%DAT(IND)%PC_AVR .LT. -3.0D0*PI__NUM/4.0D0      ) THEN
                 PC_SDL(J11) = PC_SDL(J11) - PI__NUM
            END IF
 4110    CONTINUE
!
! ------ If the phase is around -PI ( [-3/4*PI, 3/4*PI] ), then we flip the
! ------ sign of the of the amplitude
!
         IF ( FRESEL%DAT(IND)%PC_AVR .GT.  3.0D0*PI__NUM/4.0D0 .OR. &
     &        FRESEL%DAT(IND)%PC_AVR .LT. -3.0D0*PI__NUM/4.0D0      ) THEN
!
              FRESEL%DAT(IND)%AC_AVR = -FRESEL%DAT(IND)%AC_AVR
              FRESEL%DAT(IND)%PC_AVR =  FRESEL%DAT(IND)%PC_AVR - PI__NUM
              IF ( FRESEL%DAT(IND)%PC_AVR .LT. 0.0D0 ) THEN
                   FRESEL%DAT(IND)%PC_AVR = FRESEL%DAT(IND)%PC_AVR + PI2
              END IF
         END IF
!
! ------ Compute Chebyshev polynomials of normalized amplitude function
!
         IF ( DABS(FRESEL%DAT(IND)%AC_AVR) .GT. FRESEL%NUTAMP_MIN ) THEN
              CALL CHCR ( M_PWR, FRESEL%TIME_BEG, &
     &                  (FRESEL%TIME_END - FRESEL%TIME_BEG), TIM_SDL, AC_SDL, &
     &                   FRESEL%DAT(IND)%AC_CHE )
              CALL CHCR ( M_PWR, FRESEL%TIME_BEG, &
     &                  (FRESEL%TIME_END - FRESEL%TIME_BEG), TIM_SDL, PC_SDL, &
     &                   FRESEL%DAT(IND)%PC_CHE )
            ELSE
              CALL NOUT_R8 ( M_PWR, FRESEL%DAT(IND)%AC_CHE )
              CALL NOUT_R8 ( M_PWR, FRESEL%DAT(IND)%PC_CHE )
              FRESEL%DAT(IND)%PC_AVR = 0.0D0
              FRESEL%DAT(IND)%L_PWR  = 0
         END IF
!
! ------ Now look, do we have the situation when the sidelobe phase is close
! ------ to -+ PI/2? If yes, it would cause cross-term singularity
!
         IF ( ( FRESEL%DAT(IND)%PC_AVR .GT.  1.0D0*PI__NUM/3.0D0  .AND.      &
     &          FRESEL%DAT(IND)%PC_AVR .LT.  2.0D0*PI__NUM/3.0D0      ) .OR. &
     &        ( FRESEL%DAT(IND)%PC_AVR .LT. -1.0D0*PI__NUM/3.0D0  .AND.      &
     &          FRESEL%DAT(IND)%PC_AVR .GT. -2.0D0*PI__NUM/3.0D0      )    ) THEN
!
              CALL NOUT_R8 ( M_PWR, FRESEL%DAT(IND)%AC_CHE )
              CALL NOUT_R8 ( M_PWR, FRESEL%DAT(IND)%PC_CHE )
              FRESEL%DAT(IND)%PC_AVR = 0.0D0
              FRESEL%DAT(IND)%L_PWR  = 0
         END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!        write (6, 210) j7, fresel%dat(ind)%ac_avr, fresel%dat(ind)%pc_avr ! %%
! 210    format ( ' j7=',i4,' ac_avr=',1pd11.3,' pc_avr=',0pf7.3 )         ! %%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&
!
         IF ( N_SDL .EQ. 0  .OR.  FRESEL%IGNORE_SIDELOBES ) THEN
              CALL NOUT_R8 ( M_PWR, FRESEL%DAT(IND)%AC_CHE )
              CALL NOUT_R8 ( M_PWR, FRESEL%DAT(IND)%PC_CHE )
              FRESEL%DAT(IND)%PC_AVR = 0.0D0
              FRESEL%DAT(IND)%L_PWR  = 0
         END IF
 470  CONTINUE
!
      IF ( N_VAR .GT. 0 ) THEN
!
! -------- Now, let's make LSQ estimation of the tidal amplitudes
!
           CALL ERR_PASS ( IUER, IER )
           CALL FRESEL_TIDEST ( N_VAR, FRESEL, NOTID2_USED, NOTID3_USED, &
     &                          IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3381, IUER, 'TID_FRESEL_HEO', 'Error in '// &
     &              'FRESEL_TIDEST' )
                RETURN
           END IF
        ELSE
           FRESEL%TID_RMS = 0.0D0
           FRESEL%TID_MAX = 0.0D0
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  TID_FRESEL_HEO  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FRESEL_TIDEST ( N_VAR, FRESEL, NOTID2_USED, NOTID3_USED, &
     &                           IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine FRESEL_TIDEST
! *                                                                      *
! *  ### 05-NOV-2003  FRESEL_TIDEST v2.0 (c)  L. Petrov 29-DEC-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'fresel.i'
      INCLUDE   'tid.i'
      TYPE ( FRESEL__STRU ) :: FRESEL
      INTEGER*4  N_VAR, IVRB, IUER
      LOGICAL*4  NOTID2_USED(*), NOTID3_USED(*)
      REAL*8     TIM(M_EPT), PM_VAL(M_EPT), RES(M_EPT), &
     &           FRQ_ARR(M_EPT), FTC(M_EPT), FTS(M_EPT), POW(M_EPT)
      REAL*8     ARG, TIM_STEP, EQU_CON(M_HEO)
      REAL*8,    ALLOCATABLE :: NOR_MAT(:), NOR_VEC(:), EST_VEC(:)
      REAL*8     AMP_FCT
      PARAMETER  ( AMP_FCT = 0.01D0 )
      CHARACTER  STR*32
      COMPLEX*16, ALLOCATABLE :: ARR_C16(:)
      REAL*8     RCOND, TIME_STEP, RES_MAX, RES_RMS, AC_AMP, PC_PHS, &
     &           AS_AMP, PS_PHS, WIN, POW_MAX, FRQ_NYQUIST
      INTEGER*4  IBEG, IEND
      INTEGER*4  I_VAR, IOS, IND_MAX, J1, J2, J3, J4, J5, J6, J7, J8, &
     &           J9, J10, J11, IER
      REAL*8     CHINT, DP_VV_V
      INTEGER*4  I_LEN
!
      IF ( N_VAR .LE. 0 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, * ) 'FRESEL_TIDEST: N_VAR=',N_VAR
      END IF
!
      ALLOCATE ( NOR_MAT((N_VAR*(N_VAR+1))/2), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( (N_VAR*(N_VAR+1))*4, STR )
           CALL ERR_LOG ( 3361, IUER, 'FRESEL_TIDEST', 'Error in '// &
     &         'allocation of '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory' )
           RETURN
      END IF
      CALL NOUT_R8 ( (N_VAR*(N_VAR+1))/2, NOR_MAT )
!
      ALLOCATE ( NOR_VEC(N_VAR), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( N_VAR*8, STR )
           CALL ERR_LOG ( 3362, IUER, 'FRESEL_TIDEST', 'Error in '// &
     &         'allocation of '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
      CALL NOUT_R8 ( N_VAR, NOR_VEC )
!
      ALLOCATE ( EST_VEC(N_VAR), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( N_VAR*8, STR )
           CALL ERR_LOG ( 3363, IUER, 'FRESEL_TIDEST', 'Error in '// &
     &         'allocation of '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
      CALL NOUT_R8 ( N_VAR, EST_VEC )
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, * ) ' Compute equation of conditions...'
      END IF
      TIME_STEP = (FRESEL%TIME_END - FRESEL%TIME_BEG)/(M_EPT-1)
!
! --- Compute time series
!
      DO 410 J1=1,M_EPT
         IF ( IVRB .GE. 3  .AND.  MOD(J1,1024) .EQ. 0 ) THEN
              WRITE ( 6, 110 ) J1, M_EPT, CHAR(13)
              CALL FLUSH ( 6 )
 110          FORMAT ( '  ',I6,' ( ',I6,' )    ',A,$ )
         END IF
!
         TIM(J1) = FRESEL%TIME_BEG + (J1-1)*TIME_STEP
         CALL NOUT_R8 ( N_VAR, EQU_CON )
!
         I_VAR = 0
         DO 420 J2=1,FRESEL%N_FRQ
            IF ( FRESEL%DAT(J2)%TYP .NE. IND__TID ) GOTO 420
!
! --------- We bypass negative or low frequencies
!
            IF ( FRESEL%DAT(J2)%FRQ .LE. FRQ_LOW_ZON  ) GOTO 420
            ARG = FRESEL%DAT(J2)%PHS + FRESEL%DAT(J2)%FRQ*TIM(J1)
            IF ( FRESEL%IGNORE_SIDELOBES ) THEN
                 AC_AMP = 1.0D0
                 PC_PHS = 0.0D0
               ELSE 
                 AC_AMP = 1.D0 + CHINT (FRESEL%DAT(J2)%L_PWR, FRESEL%TIME_BEG,      &
     &                             FRESEL%TIME_END - FRESEL%TIME_BEG, TIM(J1), &
     &                             FRESEL%DAT(J2)%AC_CHE, -3 )
                 PC_PHS =        CHINT (FRESEL%DAT(J2)%L_PWR, FRESEL%TIME_BEG,      &
     &                             FRESEL%TIME_END - FRESEL%TIME_BEG, TIM(J1), &
     &                             FRESEL%DAT(J2)%PC_CHE, -3 )
            END IF
            I_VAR = I_VAR + 1
            EQU_CON(I_VAR) = AC_AMP*DCOS(ARG+PC_PHS)
!
            I_VAR = I_VAR + 1
            AS_AMP = 1.0D0
            PS_PHS = 0.0D0
            EQU_CON(I_VAR) = AS_AMP*DSIN(ARG+PS_PHS)
 420     CONTINUE
!
! ------ Update normal matrix
!
         CALL DIAD_CVT_S ( 1.0D0, N_VAR, EQU_CON, EQU_CON, NOR_MAT )
!
! ------ Compute right hand side
!
         PM_VAL(J1) = 0.0D0
         DO 430 J3=1,NTID
            IF ( TID_FREQ(J3) .LT. FRQ_LOW_ZON ) GOTO 430 ! Bypass low frequencies
            IF ( ( L(J3) == 2  .AND.  TID_FREQ(J3) .GT. FRQ_LOW_ZON ) .OR. &
     &           ( L(J3) == 3  .AND.  TID_FREQ(J3) .GT. 2.5D0*OM__EAR )    ) THEN
                 IF ( .NOT. NOTID2_USED(J3) ) THEN
                      ARG = -TID_PHAS(J3) - TID_FREQ(J3)*TIM(J1)
                      PM_VAL(J1) = PM_VAL(J1) + TID_AMPL(J3)*DCOS(ARG)
                 END IF
            END IF
 430     CONTINUE
!
! ------ ... and normal vector
!
         DO 450 J5=1,N_VAR
            NOR_VEC(J5) = NOR_VEC(J5) + EQU_CON(J5)*PM_VAL(J1)
 450     CONTINUE
 410  CONTINUE
!
! --- Invert normal matrix
!
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, * ) ' Invert normal matrix                      '
           WRITE ( 6, * ) ' NTID = ',NTID, ' N_VAR = ' , N_VAR
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS ( N_VAR, NOR_MAT, RCOND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3364, IUER, 'FRESEL_TIDEST', 'Error in an '// &
     &         'attempt to invert normal matrix' )
           RETURN
      END IF
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, * ) ' R_COND = ',RCOND
      END IF
!
! --- Get solution
!
      IER = -1
      CALL MUL_MV_SV_V ( N_VAR, NOR_MAT, N_VAR, NOR_VEC, N_VAR, EST_VEC, IER )
!
      RES_RMS = 0.0D0
      RES_MAX = 0.0D0
      IF ( IVRB .GE. 2 ) THEN
           CALL ANAL_COV ( N_VAR, NOR_MAT, 0.80D0 )
      END IF
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, * ) ' Compute residuals'
      END IF
!
! --- Compute residuals
!
      DO 460 J6=1,M_EPT
         IF ( IVRB .GE. 3  .AND.  MOD(J6,1024) .EQ. 0 ) THEN
              WRITE ( 6, 110 ) J6, M_EPT, CHAR(13)
              CALL FLUSH ( 6 )
         END IF
!
! ------ Initialization of the equation of conditions
!
         CALL NOUT_R8 ( N_VAR, EQU_CON )
         I_VAR = 0
         DO 470 J7=1,FRESEL%N_FRQ
            IF ( FRESEL%DAT(J7)%TYP .NE. IND__TID    ) GOTO 470
            IF ( FRESEL%DAT(J7)%FRQ .LE. FRQ_LOW_ZON ) GOTO 470
            ARG = FRESEL%DAT(J7)%PHS + FRESEL%DAT(J7)%FRQ*TIM(J6)
            IF ( FRESEL%IGNORE_SIDELOBES ) THEN
                 AC_AMP = 1.0D0
                 PC_PHS = 0.0D0
               ELSE 
                 AC_AMP = 1.D0 + CHINT (FRESEL%DAT(J7)%L_PWR, FRESEL%TIME_BEG,      &
     &                             FRESEL%TIME_END - FRESEL%TIME_BEG, TIM(J6), &
     &                             FRESEL%DAT(J7)%AC_CHE, -3 )
                 PC_PHS =        CHINT (FRESEL%DAT(J7)%L_PWR, FRESEL%TIME_BEG,      &
     &                             FRESEL%TIME_END - FRESEL%TIME_BEG, TIM(J6), &
     &                             FRESEL%DAT(J7)%PC_CHE, -3 )
            END IF
            I_VAR = I_VAR + 1
            EQU_CON(I_VAR) = AC_AMP*DCOS(ARG+PC_PHS)
!
            I_VAR = I_VAR + 1
            AS_AMP = 1.0D0
            PS_PHS = 0.0D0
            EQU_CON(I_VAR) = AS_AMP*DSIN(ARG+PS_PHS)
 470     CONTINUE
!
         RES(J6) = PM_VAL(J6) - DP_VV_V ( N_VAR, EQU_CON, EST_VEC )
         RES_RMS = RES_RMS + RES(J6)**2
         IF ( DABS(RES(J6)) .GT. RES_MAX ) RES_MAX = DABS(RES(J6))
 460  CONTINUE
      RES_RMS = DSQRT ( RES_RMS/M_EPT )
!
      FRESEL%TID_RMS = RES_RMS
      FRESEL%TID_MAX = RES_MAX
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, 210 ) RES_RMS,  RES_MAX
 210       FORMAT ( ' Residuals: rms= ', F10.6, ' m^2/s^2,  max= ', &
     &                F10.6,' m^2/s^2' )
           WRITE ( 6, * ) 'Tidal solution is done'
      END IF
!
      IF ( IVRB .GE. 4 ) THEN
           CALL DIAGI_1 ( M_EPT, TIM, RES, -3 )
      END IF
      IF ( IVRB .GE. 2 ) THEN
!
! -------- Apply Hann window
!
           DO 480 J8=1,M_EPT
              WIN = 0.5D0 - 0.5D0*DCOS((PI2*J8)/M_EPT)
              RES(J8) = WIN*RES(J8)
 480       CONTINUE
!
! -------- Run fast Fourier transform
!
           ALLOCATE ( ARR_C16(M_EPT) )
           FRQ_NYQUIST = PI2/(TIM(2) - TIM(1))

           DO 490 J9=1,M_EPT
              IF ( J9 .LE. M_EPT/2 ) THEN
                   FRQ_ARR(J9) = (J9-1)*FRQ_NYQUIST/M_EPT
                ELSE 
                   FRQ_ARR(J9) = (J9-1-M_EPT)*FRQ_NYQUIST/M_EPT
              END IF
              FTC(J9) = REAL ( ARR_C16(J9) )
              FTS(J9) = IMAG ( ARR_C16(J9) )
 490       CONTINUE 
           DEALLOCATE   ( ARR_C16 )
!
! -------- Compute power spectrum
!
           POW_MAX = -1.0D0
           IND_MAX = -1
           IBEG = 0
           DO 4100 J10=1,M_EPT/2
              IF ( J10 .GT. 1 ) THEN
                   POW(J10) = 2.0D0*( FTC(J10)**2 + FTS(J10)**2 )
                 ELSE
                   POW(J10) = FTC(J10)**2 + FTS(J10)**2
              END IF
              IF ( POW(J10) .GT. POW_MAX ) THEN
                   POW_MAX = POW(J10)
                   IND_MAX = J10
              END IF
 4100      CONTINUE
           WRITE ( 6, * ) ' FRQ_VAR(IND_MAX)=', FRQ_ARR(IND_MAX), &
     &                    ' POW=',POW_MAX
           IF ( IVRB .GE. 4 ) THEN
                CALL DIAGI_1 ( M_EPT/2, FRQ_ARR, POW, -3 )
           END IF
      END IF
!
! --- Store the estimate of tidal amplitudes
!
      I_VAR = 0
      DO 4110 J11=1,FRESEL%N_FRQ
         IF ( FRESEL%DAT(J11)%FRQ .LE. FRQ_LOW_ZON ) GOTO 4110
         IF ( FRESEL%DAT(J11)%TYP == IND__TID ) THEN
              I_VAR = I_VAR + 1
              FRESEL%DAT(J11)%PMC_EST = EST_VEC(I_VAR)
!
              I_VAR = I_VAR + 1
              FRESEL%DAT(J11)%PMS_EST = EST_VEC(I_VAR)
         END IF
 4110 CONTINUE
!
      DEALLOCATE ( NOR_MAT )
      DEALLOCATE ( NOR_VEC )
      DEALLOCATE ( EST_VEC )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FRESEL_TIDEST  !#!#
