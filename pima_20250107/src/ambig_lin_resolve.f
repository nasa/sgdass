      SUBROUTINE AMBIG_LIN_RESOLVE ( NP, TIM, PHS, AMP, TIM_0, PHS_0, &
     &                               PHS_DR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  AMBIG_LIN_RESOLVE
! *                                                                      *
! * ## 26-AUG-2006  AMBIG_LIN_RESOLVE  v1.0 (c) L. Petrov 27-AUG-2006 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INTEGER*4  AMBIG_ALG, NP, IUER
      REAL*8     TIM(NP), PHS(NP), AMP(NP), TIM_0, PHS_0, PHS_DR
      INTEGER*4  MP, MS
      PARAMETER  ( MP = 16*1024 )
      PARAMETER  ( MS = 1024    )
      REAL*8     RAT_CYCLE
      PARAMETER  ( RAT_CYCLE = 8.0D0 )
      CHARACTER    STR*32, STR1*32
      REAL*8     RATE_BEG, RATE_END, RATE_STEP, RATE, AMP_MAX, &
     &           RATE_AMP_MAX, PHS_AMP_MAX
      REAL*8     MEAN_T, DR_VAL, SH_VAL, DR_SIG, SH_SIG, PHS_INTRP
      REAL*8     T8(MP), X8(MP) ! %%%
      COMPLEX*16 CAV
      INTEGER*4  J1, J2, J3, J4, IV(MP), ITURN, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      REAL*8,    EXTERNAL :: ATAN_CS
!
      IF ( NP > MP ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( NP, STR  )
           CALL INCH  ( MP, STR1 )
           CALL ERR_LOG ( 5711, IUER, 'AMBIG_LIN_RESOLVE', 'Too many '// &
     &         'points: '//STR(1:I_LEN(STR))// &
     &         ' . Please raise the limit MP '//STR1 )
           RETURN
      END IF
!
      IF ( NP == 1 ) THEN
           PHS_0  = PHS(1)
           PHS_DR = 0.0D0
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!      WRITE ( 16, '(A,I4)' ) '#  NP =', NP
!      DO 510 J1=1,NP
!         WRITE ( 16, 114 ) J1, TIM(J1), PHS(J1), AMP(J1), TIM_0
! 114     FORMAT ( I4, 5(2X, 1PD12.5) )
! 510  CONTINUE
!  if ( np .ne. -12312 ) call err_log ( 0, iuer ) ! %%%%%%%%%55
!  if ( np .ne. -12312 ) return ! %%%%%%%%%55
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      RATE_BEG  = -PI2*RAT_CYCLE/(TIM(NP) - TIM(1))
      RATE_END  =  PI2*RAT_CYCLE/(TIM(NP) - TIM(1))
      RATE_STEP =  (RATE_END - RATE_BEG)/(MS-1)
!
! --- Search the date which makes the amplitude of coherent averaging maximal
!
      AMP_MAX = -1.0D0
      DO 410 J1=1,MS  ! cucle over trial single-band delay rate
         CAV = DCMPLX ( 0.0D0, 0.0D0 )
         RATE = RATE_BEG + (J1-1)*RATE_STEP
         DO 420 J2=1,NP
           CAV = CAV + DCMPLX ( AMP(J2)*DCOS(PHS(J2)), &
     &                          AMP(J2)*DSIN(PHS(J2)) )* &
     &                 DCMPLX ( DCOS(RATE*(TIM(J2)-TIM_0)), &
     &                          DSIN(RATE*(TIM(J2)-TIM_0)) )
 420     CONTINUE
         IF ( ABS(CAV) > AMP_MAX ) THEN
!
! ----------- Store the maximuim amplitude, as well as phas and rates which
! ----------- provided the maximum
!
              AMP_MAX = ABS(CAV)
              RATE_AMP_MAX = RATE
              PHS_AMP_MAX  = ATAN_CS ( DREAL(CAV), DIMAG(CAV) )
         END IF
 410  CONTINUE
!
! --- Resolve ambiguities with respect to the rate whcih gives the maximum
! --- of coherentr average
!
      DO 430 J3=1,NP
!
! ------ Find interpolated phase which correspond to the maximum of the
! ------ coherent average. NB: we **add** coherent phase and **subtract**
! ------ the slope, since the maximum rate is the rate which when added
! ------ compensate the slope in data. Therefore, this rate corresponds
! ------ to the rate in the data with opposite sign.
! ------ This is done in order to make the redidual phase
! ------ a) flat; b) close to zero
!
         PHS_INTRP = PHS_AMP_MAX - RATE_AMP_MAX*(TIM(J3)-TIM_0)
!
! ------ Resolve amiguity
!
         ITURN = IDNINT( (PHS(J3) - PHS_INTRP)/PI2 )
         PHS(J3) = PHS(J3) - ITURN*PI2
         IV(J3) = 1
 430  CONTINUE
!
! --- Now find the parameters of the regerssion line and their uncertainties
! --- using LSQ:  PHS(TIM) = SH_VAL + DR_VAL*(TIM - MEAN_T)
!
      CALL ERR_PASS ( IUER, IER )
      CALL RGRW8 ( NP, TIM, PHS, AMP, IV, MEAN_T, DR_VAL, SH_VAL, DR_SIG, &
     &             SH_SIG, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5712, IUER, 'AMBIG_LIN_RESOLVE', 'Error in '// &
     &         'computation of the regression line' )
           RETURN
      END IF
!
! --- Again resolve ambiguities
!
      DO 440 J4=1,NP
!
! ------ Compute interpolated phase
!
         PHS_INTRP = DR_VAL*(TIM(J4)-MEAN_T) + SH_VAL
!
! ------ Resolve phase amibguity
!
         ITURN = IDNINT( (PHS(J4) - PHS_INTRP)/PI2 )
         PHS(J4) = PHS(J4) - ITURN*PI2
 440  CONTINUE
!
! --- Store the phase on moment TIM_0 and the phase rate of change
!
      PHS_0  = SH_VAL - DR_VAL*(MEAN_T - TIM_0)
      PHS_DR = DR_VAL
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  AMBIG_LIN_RESOLVE  !#!#
