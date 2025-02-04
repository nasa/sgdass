      SUBROUTINE PIMA_FRINGE_RES ( PIM, IND_OBS, LCHN, LFRQ, LTIM, &
     &                             FREQ_ARR, FREQ_REF, UV, WEI_1D, AP_LEN, &
     &                             TIME_FRT, PH_RAT, GR_DEL, GR_RAT, &
     &                             PHAS, AMPL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_FRINGE_RES
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   IND_OBS ( INTEGER*4 ) -- Observation index.                        *
! *      LCHN ( INTEGER*4 ) -- The number of spectral channels           *
! *                            in one frequency channels.                *
! *      LFRQ ( INTEGER*4 ) -- Number of frequency channels.             *
! *      LTIM ( INTEGER*4 ) -- Number of accumulation periods.           *
! *  FREQ_ARR ( REAL*8    ) -- Frequency array. Dimension: (LCHN,LFRQ).  *
! *  FREQ_REF ( REAL*8    ) -- Reference frequency.                      *
! *        UV ( COMPLEX*8 ) -- Array of the cross correlation function.  *
! *                            Dimension: (LCHN,LFRQ,LTIM).              *
! *    WEI_1D ( REAL*8    ) -- One-dimensional Array of weights for      *
! *                            each accumulation period in range [0, 1]. *
! *                            Dimension: LTIM.                          *
! *    AP_LEN ( REAL*8    ) -- Length of the accumulation period.        *
! *  TIME_FRT ( REAL*8    ) -- Fringe reference time from the nominal    *
! *                            start of the observation.                 *
! *    PH_RAT ( REAL*8    ) -- Phase delay rate.                         *
! *    GR_DEL ( REAL*8    ) -- Group delay.                              *
! *    GR_RAT ( REAL*8    ) -- Group delay rate.                         *
! *      PHAS ( REAL*8    ) -- Fringe phase at reference moment of time  *
! *                            at reference frequency.                   *
! *      AMPL ( REAL*8    ) -- Amplitude of the cross-correlation        *
! *                            function which corresponds to GR_DEL,     *
! *                            PH_RAT.                                   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *       PIM ( PIMA__TYP ) -- Object with information related to        *
! *                            program PIMA.                             *
! *   TIME_FRT ( REAL*8   ) -- Fringe reference time from the nominal    *
! *                            start of the observation.                 *
! *     SB_DEL ( REAL*8   ) -- Error of the narrow-band group delay.     *
! * SB_DEL_ERR ( REAL*8   ) -- Error of the narrow-band group delay.     *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the vase of errors. *
! *                                                                      *
! * ### 22-AUG-2009  PIMA_FRINGE_RES  v1.1 (c) L. Petrov 20-SEP-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'fftw3.f'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IND_OBS, LCHN, LFRQ, LTIM, IUER
      REAL*8     FREQ_ARR(LCHN,LFRQ), FREQ_REF, AP_LEN
      REAL*4     WEI_1D(LTIM)
      REAL*8     TIME_FRT, PH_RAT, GR_DEL, GR_RAT, PHAS, AMPL, SNR
      COMPLEX*8  UV(LCHN,LFRQ,LTIM)
      CHARACTER  STR*128, STR1*128
      REAL*8     PHAS_ADD, WEI(PIM__MUV)
      REAL*4     WPOI
      COMPLEX*8  DRF
      INTEGER*4  J1, J2, J3, J4, J5, KEQ, LEQ, LPAR, IFRQ, IER
      LOGICAL*4, EXTERNAL :: IS_R4_NAN
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IFRQ = 0
      DO 410 J1=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         IF ( J1 > PIM%NFRQ ) THEN
              CALL CLRCH ( STR  )
              CALL CLRCH ( STR1 )
              CALL INCH  ( J1,  STR )
              CALL INCH  ( PIM%NFRQ, STR1 )
              CALL ERR_LOG ( 7471, IUER, 'PIMA_FRINGE_RES', 'Trap of internal '// &
     &            'control: an attempt to compute residual phase for frequency '// &
     &            'channel '//STR(1:I_LEN(STR))//' while the total number '// &
     &            'of channels used for fringe search is only '//STR1 )
              RETURN 
         END IF
         IFRQ = IFRQ + 1
         DRF = ( 0.0, 0.0 )
         WPOI     = 0.0
         DO 420 J2=1,LCHN
            IF ( PIM%BANDPASS_MASK_STYLE .NE. PIMA__NO ) THEN
                 IF ( PIM%BANDPASS_MASK(J2,J1,PIM%OBS(IND_OBS)%STA_IND(1),PIMA__MASK_FRNG) == 0 ) GOTO 420
                 IF ( PIM%BANDPASS_MASK(J2,J1,PIM%OBS(IND_OBS)%STA_IND(2),PIMA__MASK_FRNG) == 0 ) GOTO 420
            END IF
            DO 430 J3=1,LTIM
               WEI(J3) = WEI_1D(J3)
               IF ( IS_R4_NAN ( REAL ( UV(J2,IFRQ,J3) ) ) .OR. &
     &              IS_R4_NAN ( IMAG ( UV(J2,IFRQ,J3) ) )      ) THEN
                    WEI(J3) = 0.0
                    UV(J2,IFRQ,J3) = CMPLX ( 0.0, 0.0 )
               END IF
!
! ------------ Check for insane UV values
!
               IF ( REAL ( UV(J2,IFRQ,J3) ) >  PIMA__AMP_MAX .OR. &
     &              REAL ( UV(J2,IFRQ,J3) ) < -PIMA__AMP_MAX .OR. &
     &              IMAG ( UV(J2,IFRQ,J3) ) >  PIMA__AMP_MAX .OR. &
     &              IMAG ( UV(J2,IFRQ,J3) ) < -PIMA__AMP_MAX      ) THEN
                    WEI(J3) = 0.0
                    UV(J2,IFRQ,J3) = CMPLX ( 0.0, 0.0 )
               END IF

! ------------ We apply phase rotation due to the phase delay rate and group
! ------------ delay and conter-rotation due to the total phase.
! ------------ As a result we will get the residual phase
!
               PHAS_ADD = - PHAS &
     &                    + PH_RAT*PI2*FREQ_REF*((J3-1)*AP_LEN - TIME_FRT) &
     &                    + GR_DEL*PI2*(FREQ_ARR(J2,IFRQ)-FREQ_REF) &
     &                    + GR_RAT*PI2*(FREQ_ARR(J2,IFRQ)-FREQ_REF)* &
     &                                 ((J3-1)*AP_LEN - TIME_FRT)
               DRF = DRF + WEI(J3)*UV(J2,IFRQ,J3)* &
     &                     CMPLX ( COS(PHAS_ADD), SIN(PHAS_ADD) )
               IF ( WEI(J3)                   > PIMA__AMP_MIN .AND. &
     &              ABS(REAL(UV(J2,IFRQ,J3))) > PIMA__AMP_MIN .AND. &
     &              ABS(IMAG(UV(J2,IFRQ,J3))) > PIMA__AMP_MIN       ) THEN
                    WPOI = WPOI + WEI(J3)
               END IF
 430        CONTINUE
 420     CONTINUE
         IF ( WPOI > 1.D-8 ) THEN
              PIM%OBS(IND_OBS)%RES_FRN(J1,1) = DRF/WPOI
            ELSE 
              PIM%OBS(IND_OBS)%RES_FRN(J1,1) = 0.0
         END IF
 410  CONTINUE
      PIM%OBS(IND_OBS)%EFF_DUR(1) = WPOI*PIM%OBS(IND_OBS)%AP_LEN/LCHN
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FRINGE_RES  !#!#

