      FUNCTION   PIMA_AUTC_AMPL_NRML ( PIM, IND_OBS, IND_STA, IFRQ, LTIM, &
     &                                 AC, WEI_1D, AC_THR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_AUTC_AMPL_NRML  computes the renormalization factor  *
! *   for system temperature due to discarding some spectral channels.   *
! *   The system temperature is measured over entire intermediate        *
! *   frequency. However, in general, the spectrum of noise is not       *
! *   constant over the band. We use autocorrelation function to compute *
! *   the factor of Tsys(used)/Tsys(tot), where Tsys(used) is the        *
! *   system temperature over the used portion of the bandwidth, and     *
! *   Tsys(tot) is the Tsys over the entire IF, i.e. measured Tsys.      *
! *   The portion of the bandwidth for renormalization is defined by     *
! *   [ICHN_1ST, ICHN_LAST] range *and* the band mask.                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       PIM ( PIMA__TYP ) -- Object with information related to        *
! *                            program PIMA.                             *
! *   IND_OBS ( INTEGER*4 ) -- Observation index.                        *
! *   IND_STA ( INTEGER*4 ) -- Station index.                            *
! *      IFRQ ( INTEGER*4 ) -- IF index. Index 1 correpsonds to          *
! *                            PIM%CONF%BEG_FRQ defined in BEG_FRQ:      *
! *                            keyword of the control file.              *
! *        AC ( COMPLEX*8 ) -- Autocorrelation spectrum for this         *
! *                            station. Dimension: PIM%NCHN,LFRQ,LTIM.   *
! *                            where LFRQ is the number of used IFs.     *
! *                            NB: Indexing the second dimension in      *
! *                            AC_OBS starts from PIM%CONF%BEG_FRQ.      *
! *    WEI_ID ( REAL*4    ) -- Autocorrelation bandpass for this station.*
! *                            Dimension: LTIM.                          *
! *    AC_THR ( REAL*8    ) -- The autocorrelation threshold:            *
! *                            autocorrelations below that threshold     *
! *                            are replaced with zeroes.                 *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <PIMA_AUTC_AMPL_NRML> ( REAL*8 ) -- the autocorrelation              *
! *                                     renormalizaiton factor.          *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
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
! * ## 17-AUG-2013 PIMA_AUTC_AMPL_NRML v2.3 (c) L. Petrov 13-APR-2019 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     PIMA_AUTC_AMPL_NRML
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IND_OBS, IND_STA, IFRQ, LTIM, IUER
      COMPLEX*8  AC(PIM%NCHN,PIM%CONF%END_FRQ-PIM%CONF%BEG_FRQ+1,LTIM)
      REAL*4     WEI_1D(LTIM)
      REAL*4     AC_AVR_FULL, AC_MEAN_FULL, SUM_WEI_TOTAL, SUM_WEI_USED, &
     &           AC_MEAN_TOTAL, AC_MEAN_FRINGE_USED, &
     &           AC_TAV(PIM__MCHN), AC_TAV_SMOO(PIM__MCHN), WEI_USED(PIM__MCHN)
      REAL*8     AC_THR
      CHARACTER  STR*128, STR1*128
      INTEGER*4  NC(PIM__MCHN)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      INTEGER*4  J1, J2, J3, J4, KTIM, IER
!
      AC_TAV      = 0.0
      AC_TAV_SMOO = 0.0
      NC          = 0
      DO 410 J1=1,LTIM
!
! ------ Check whether weights are OK
!
         IF ( WEI_1D(J1) < PIMA__AMP_MIN ) GOTO 410
         IF ( WEI_1D(J1) < AC_THR        ) GOTO 410
!
         DO 420 J2=1,PIM%NCHN
            IF ( ABS(AC(J2,IFRQ,J1)) > AC_THR         .AND. &
     &           ABS(AC(J2,IFRQ,J1)) < PIMA__ACC_MAX        ) THEN
                 AC_TAV(J2) = AC_TAV(J2) + ABS(AC(J2,IFRQ,J1))
                 NC(J2) = NC(J2) + 1
            END IF
 420     CONTINUE
 410  CONTINUE 
!
      DO 430 J3=1,PIM%NCHN
         IF ( NC(J3) > 0 ) THEN
              AC_TAV(J3) = AC_TAV(J3)/NC(J3)
              AC_TAV_SMOO(J3) = AC_TAV(J3) 
         END IF
 430  CONTINUE 
!
! --- Apply mask of MASK_TYPE and smooth time avaraged autocorrelation
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_AC_SMOOTH ( PIM, IND_STA, PIM%CONF%BEG_FRQ+IFRQ-1, .TRUE., &
     &                      PIMA__MASK_AUTC, AC_TAV_SMOO, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7593, IUER, 'PIMA_AUTC_AMPL_NRML', 'Error in '// &
     &         'an attempt to to smooth autocorrelation spectrum' )
           RETURN 
      END IF
!
      AC_MEAN_TOTAL       = 0.0
      AC_MEAN_FRINGE_USED = 0.0
      SUM_WEI_TOTAL       = 0.0
      SUM_WEI_USED        = 0.0
      DO 440 J4=1,PIM%NCHN
         IF ( PIM%BANDPASS_MASK_STYLE .NE. PIMA__NO .AND. &
     &        ASSOCIATED ( PIM%BANDPASS_MASK )            ) THEN
              WEI_USED(J4) = PIM%BANDPASS_MASK(J4,PIM%CONF%BEG_FRQ+IFRQ-1,IND_STA,PIMA__MASK_FRNG)
           ELSE
              WEI_USED(J4) = 1.0D0
         END IF
         AC_MEAN_TOTAL       = AC_MEAN_TOTAL       + AC_TAV(J4)
         AC_MEAN_FRINGE_USED = AC_MEAN_FRINGE_USED + WEI_USED(J4)*AC_TAV(J4)
         SUM_WEI_TOTAL       = SUM_WEI_TOTAL       + 1.0D0
         SUM_WEI_USED        = SUM_WEI_USED        + WEI_USED(J4)
 440  CONTINUE 
      AC_MEAN_TOTAL       = AC_MEAN_TOTAL/SUM_WEI_TOTAL
      IF ( SUM_WEI_USED > 1.D-10 ) THEN 
           AC_MEAN_FRINGE_USED = AC_MEAN_FRINGE_USED/SUM_WEI_USED
         ELSE 
           AC_MEAN_FRINGE_USED = 1.0
      END IF
      IF ( SUM_WEI_USED < 0.999 .OR. SUM_WEI_TOTAL < 0.999 ) THEN
!@           IF ( PIM%CONF%CHECK_SEVERITY > 2 ) THEN
!@                CALL CLRCH ( STR )
!@                CALL INCH  ( IND_OBS, STR  )
!@                CALL CLRCH ( STR1 )
!@                CALL INCH  ( IFRQ, STR1 )
!@                CALL ERR_LOG ( 7594, IUER, 'PIMA_AUTC_AMPL_NRML', 'All weights '// &
!@     &                  'are zeroes during computation of autocorrelation '// &
!@     &                  'normalization for observation '//STR(1:I_LEN(STR))// &
!@     &                  ', sta='//PIM%C_STA(IND_STA)//' IF= '//TRIM(STR1)// &
!@     &                  ' Hint: CHECK_SEVERITY < 3 will allow you to proceed' )
!@                RETURN 
!@              ELSE IF ( PIM%CONF%WARNING ) THEN
!@                CALL CLRCH ( STR )
!@                CALL INCH  ( IND_OBS, STR )
!@                CALL CLRCH ( STR1 )
!@                CALL INCH  ( IFRQ, STR1 )
!@                CALL ERR_PASS ( IUER, IER )
!@                CALL ERR_LOG  ( 7595, IER, 'PIMA_AUTC_AMPL_NRML', 'All weights '// &
!@     &                  'are zeroes during computation of autocorrelation '// &
!@     &                  'normalization for observation '//STR(1:I_LEN(STR))// &
!@     &                  ', sta='//PIM%C_STA(IND_STA)//' IF= '//TRIM(STR1)// &
!@     &                  '. Nevertheless, proceed' )
!@           END IF
           PIMA_AUTC_AMPL_NRML = 1.0D0
         ELSE
!
! -------- If we would have limited Tsys measurement only in used portion of
! -------- the IF band, in that case Tsys would be Tsys(obs)*Ac_used/Ac_total.
!
           IF ( AC_MEAN_TOTAL > PIMA__WEI_MIN ) THEN
                PIMA_AUTC_AMPL_NRML = AC_MEAN_FRINGE_USED/AC_MEAN_TOTAL
              ELSE 
                PIMA_AUTC_AMPL_NRML = 0.0D0
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION  PIMA_AUTC_AMPL_NRML  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_AC_SMOOTH ( PIM, IND_STA, IND_FRQ, FL_PC, &
     &                            MASK_TYPE, AC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_AC_SMOOTH smoothes autocorrelation for the IND_STA    *
! *   th station and the IND_FRQ intemediate frequency. It downweights   *
! *   channels that are masked out and optionally the channels with      *
! *   phase calibiration. Smoothed autocorrelation replaces the input    *
! *   one.                                                               *
! *                                                                      *
! *   Smoothing is perfomed either with expansion into Legendre          *
! *   polynomials or B-spline basis. The type of smoothing is specified  *
! *   by PIM%CONF%BPS_INTRP_METHOD ( BPS.INTRP_METHOD ). The degree of   *
! *   the Legendre polynomial expansion or the number of the B-spline    *
! *   expansion is specified by PIM%CONF%BPS_DEG_AMP ( BPS.DEG_AMP ).    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       PIM ( PIMA__TYP ) -- Object with information related to        *
! *                            program PIMA.                             *
! *   IND_STA ( INTEGER*4 ) -- Station index.                            *
! *   IND_FRQ ( INTEGER*4 ) -- IF index. Starts from the first defined   *
! *                            intermediate frequency.                   *
! *     FL_PC ( LOGICAL*1 ) -- Flag of downeighting channels at phase    *
! *                            calibration frequencies.                  *
! *                            If .TRUE., then the channels at phase     *
! *                            calibration frequencies are downweighted  *
! *                            and are effectively excluded.             *
! * MASK_TYPE ( INTEGER*4 ) -- Type of the band mask. List of supported  *
! *                            mask types:                               *
! *                            PIMA__MASK_AUTC = 1,                      *
! *                            PIMA__MASK_BPAS = 2,                      *
! *                            PIMA__MASK_FRNG = 3,                      *
! *                            PIMA__MASK_SPLT = 4.                      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *      AC ( REAL*4      ) -- Autocorrelation bandpass for this         *
! *                            baseline. Dimension: PIM%NCHN.            *
! *                            At the output it is poplaced with the     *
! *                            smoothed one.                             *
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
! * ### 09-MAR-2017   PIMA_AC_SMOOTH  v1.2 (c) L. Petrov 21-SEP-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IND_STA, IND_FRQ, MASK_TYPE, IUER
      REAL*4     AC(PIM%NCHN)
      LOGICAL*1  FL_PC
      INTEGER*4  DEG
      REAL*8       CNS_VAL_SIG, CNS_DER_SIG, CNS_DR2_SIG, FREQ_EPS
      REAL*8       PIM__PCAL_FREQ, PIM__FRQ_TOL, PIM__WEI_DOWN
      PARAMETER  ( CNS_VAL_SIG  = 1.0D2   )
      PARAMETER  ( CNS_DER_SIG  = 1.0D-5  )
      PARAMETER  ( CNS_DR2_SIG  = 5.0D-12 )
      PARAMETER  ( FREQ_EPS = 1.D0  )
      PARAMETER  ( PIM__PCAL_FREQ = 1.0D6  )
      PARAMETER  ( PIM__FRQ_TOL   = 0.2D0  )
      PARAMETER  ( PIM__WEI_DOWN  = 1.0D-6 )
      PARAMETER  ( DEG = 3 )
      REAL*8     AMPL_R8(PIM__MCHN), WEI(PIM__MCHN), POL_COEF(0:PIM__MCHN), &
     &           POL_COEF_ERR(0:PIM__MCHN), FREQ_NOD(PIM__MCHN), &
     &           SPL_VEC(1-DEG:PIM__MCHN), POSTFIT_AMP_WRMS, &
     &           FRQ, FRQ_CW, FRQ_MIN, FRQ_MAX
      INTEGER*4  J1, J2, J3, J4, J5, NUM_POI, IER
      REAL*8,    EXTERNAL :: LEGENDRE_POL, EBSPL_VAL_R8 
!
      NUM_POI = 0
      DO 410 J1=1,PIM%NCHN
         WEI(J1) = 1.0D0
         FRQ     = PIM%FREQ_ARR(J1,IND_FRQ,PIM%CONF%FRQ_GRP)
         FRQ_CW  = PIM%FRQ(1,PIM%CONF%FRQ_GRP)%CHAN_WIDTH    ! Channel bandwth
!
         IF ( FL_PC .AND. &
     &        DABS ( FRQ - PIM__PCAL_FREQ*IDNINT(FRQ/PIM__PCAL_FREQ) ) < PIM__FRQ_TOL*FRQ_CW ) THEN
!
! ----------- Downweight the point at the phase calibration frequency
!
              WEI(J1) = PIM__WEI_DOWN
         END IF
         IF ( PIM%BANDPASS_MASK_STYLE .NE. PIMA__NO .AND. &
     &        ASSOCIATED ( PIM%BANDPASS_MASK )            ) THEN
              IF ( MASK_TYPE == PIMA__MASK_AUTC .OR. MASK_TYPE == PIMA__MASK_BPAS .OR. &
     &             MASK_TYPE == PIMA__MASK_FRNG .OR. MASK_TYPE == PIMA__MASK_SPLT      ) THEN
!
! ---------------- Multily the with by the mask of the given type
!
                   WEI(J1) = PIM%BANDPASS_MASK(J1,IND_FRQ,IND_STA,MASK_TYPE) * WEI(J1)
              END IF
         END IF
         IF ( WEI(J1) > PIM%CONF%FRIB_AUTOCORR_THRESHOLD ) THEN
              NUM_POI = NUM_POI + 1
         END IF
         AMPL_R8(J1) = AC(J1)
 410  CONTINUE 
      IF ( NUM_POI < DEG+1 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
      IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE .OR. &
     &     PIM%CONF%BPS_INTRP_METHOD == PIMA__LINEAR        ) THEN
!
! -------- Smoothing with Legendre polynomials. First compute the polynomial
! -------- coefficients
!
           CALL ERR_PASS ( IUER, IER )
           FRQ_MIN = MINVAL ( PIM%FREQ_ARR(1:PIM%NCHN,IND_FRQ,PIM%CONF%FRQ_GRP), PIM%NCHN )
           FRQ_MAX = MAXVAL ( PIM%FREQ_ARR(1:PIM%NCHN,IND_FRQ,PIM%CONF%FRQ_GRP), PIM%NCHN  )
           IF ( FRQ_MIN > PIMA__MIN_FRQ .AND. FRQ_MAX - FRQ_MIN > 1.0D0 ) THEN
                CALL LEGENDRE_REGR ( PIM%NCHN, PIM%FREQ_ARR(1,IND_FRQ,PIM%CONF%FRQ_GRP), &
     &                               AMPL_R8, WEI, PIM%CONF%BPS_DEG_AMP, &
     &                               POL_COEF, POL_COEF_ERR, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7591, IUER, 'PIMA_AMPL_SMOOTH', 'Failure '// &
     &                   'in LEGENDRE_REGR' )
                     RETURN
                END IF
!
! ------------- Then compute new, smoothed autocorrelation on the place of the input
! ------------- using the Legendere polynomial coefficients 
!
                DO 420 J2=1,PIM%NCHN
                   AC(J2)= 0.0D0
                   DO 430 J3=0,PIM%CONF%BPS_DEG_AMP
                      AC(J2)= AC(J2) + POL_COEF(J3)* &
     &                               LEGENDRE_POL ( J3, PIM%FREQ_ARR(1,IND_FRQ,PIM%CONF%FRQ_GRP), &
     &                                                  PIM%FREQ_ARR(PIM%NCHN,IND_FRQ,PIM%CONF%FRQ_GRP), &
     &                                                  PIM%FREQ_ARR(J2,IND_FRQ,PIM%CONF%FRQ_GRP) )
 430               CONTINUE 
 420            CONTINUE 
              ELSE
                POL_COEF = 0.0D0
                AC       = 1.0D0
           END IF
         ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
           FRQ_MIN = MINVAL ( PIM%FREQ_ARR(1:PIM%NCHN,IND_FRQ,PIM%CONF%FRQ_GRP), PIM%NCHN )
           FRQ_MAX = MAXVAL ( PIM%FREQ_ARR(1:PIM%NCHN,IND_FRQ,PIM%CONF%FRQ_GRP), PIM%NCHN  )
           IF ( FRQ_MIN > PIMA__MIN_FRQ .AND. FRQ_MAX - FRQ_MIN > 1.0D0 ) THEN
!
! ------------- Compute array of B-spline nodes
!
                DO 440 J4=1,PIM%CONF%BPS_DEG_AMP
                   FREQ_NOD(J4) = PIM%FREQ_ARR(1,IND_FRQ,PIM%CONF%FRQ_GRP) + &
     &                            (J4-1)*( PIM%FREQ_ARR(PIM%NCHN,IND_FRQ,PIM%CONF%FRQ_GRP) - &
     &                                     PIM%FREQ_ARR(1,IND_FRQ,PIM%CONF%FRQ_GRP)    )/ &
     &                                    (PIM%CONF%BPS_DEG_AMP-1)
 440            CONTINUE 
!
! ------------- Compute B-spline coefficients
!
                CALL EBSPL_WLSQ_CNS3 ( PIM%NCHN, PIM%FREQ_ARR(1,IND_FRQ,PIM%CONF%FRQ_GRP), &
     &                                 AMPL_R8, WEI, PIM%CONF%BPS_DEG_AMP, DEG, &
     &                                 FREQ_NOD, SPL_VEC, &
     &                                 CNS_VAL_SIG, CNS_DER_SIG, CNS_DR2_SIG, &
     &                                 POSTFIT_AMP_WRMS, IER )
!
! ------------- Then compute new, smoothed autocorrelation on the place of the input
! ------------- using the B-spline expansion coefficients 
!
                DO 450 J5=1,PIM%NCHN
                   IF ( J5 == 1 ) THEN
                        AC(J5)= EBSPL_VAL_R8 ( PIM%CONF%BPS_DEG_AMP, DEG, &
     &                               PIM%FREQ_ARR(J5,IND_FRQ,PIM%CONF%FRQ_GRP) + FREQ_EPS, FREQ_NOD, SPL_VEC )
                      ELSE IF ( J5 == PIM%NCHN ) THEN
                        AC(J5)= EBSPL_VAL_R8 ( PIM%CONF%BPS_DEG_AMP, DEG, &
     &                               PIM%FREQ_ARR(J5,IND_FRQ,PIM%CONF%FRQ_GRP) - FREQ_EPS, FREQ_NOD, SPL_VEC )
                      ELSE
                        AC(J5)= EBSPL_VAL_R8 ( PIM%CONF%BPS_DEG_AMP, DEG, &
     &                               PIM%FREQ_ARR(J5,IND_FRQ,PIM%CONF%FRQ_GRP),            FREQ_NOD, SPL_VEC )
                   END IF
 450            CONTINUE 
              ELSE
                AC = 1.0D0
           END IF
         ELSE
           CALL ERR_LOG ( 7598, IUER, 'PIMA_AMPL_SMOOTH', 'Cannot perform '// &
     &         'smoothing  with BPS_INTRP_METHOD: '//PIM%CONF%BPS_INTRP_METHOD )
           RETURN 
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_AC_SMOOTH  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   PIMA_AUTC_AMPL_NRML_PRE20170303 ( PIM, ICHN_1ST, &
     &                     ICHN_LAST, IFRQ, AC_OBS, STA_IND, MASK_TYPE )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_AUTC_AMPL_NRML  computes the renormalization factor  *
! *   for system temperature due to discarding some spectral channels.   *
! *   The system temperature is measured over entire intermediate        *
! *   frequency. However, in general, the spectrum of noise is not       *
! *   constant over the band. We use autocorrelation function to compute *
! *   the factor of Tsys(used)/Tsys(tot), where Tsys(used) is the        *
! *   system temperature over the used portion of the bandwidth, and     *
! *   Tsys(tot) is the Tsys over the entire IF, i.e. measured Tsys.      *
! *   The portion of the bandwidth for renormalization is defined by     *
! *   [ICHN_1ST, ICHN_LAST] range *and* the band mask.                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       PIM ( PIMA__TYP ) -- Object with information related to        *
! *                            program PIMA.                             *
! *  ICHN_1ST ( INTEGER*4 ) -- First used spectral channel in the IF.    *
! * ICHN_LAST ( INTEGER*4 ) -- Last used spectral channel in the IF.     *
! *      IFRQ ( INTEGER*4 ) -- IF index. Starts from the first defined   *
! *                            intermediate frequency.                   *
! *    AC_OBS ( REAL*4    ) -- Autocorrelation bandpass for this         *
! *                            baseline. It is normalized to unity over  *
! *                            entire IF. Dimension: PIM%NCHN,LFRQ,      *
! *                            where LFRQ is the number of used IFs.     *
! *                            NB: Indexing the second dimension in      *
! *                            AC_OBS starts from PIM%CONF%BEG_FRQ.      *
! *   STA_IND ( INTEGER*2 ) -- Station indices.                          *
! * MASK_TYPE ( INTEGER*4 ) -- Type of the band mask. List of supported  *
! *                            mask types:                               *
! *                            PIMA__MASK_AUTC = 1,                      *
! *                            PIMA__MASK_BPAS = 2,                      *
! *                            PIMA__MASK_FRNG = 3,                      *
! *                            PIMA__MASK_SPLT = 4.                      *
! *                                                                      *
! * ## 17-AUG-2013 PIMA_AUTC_AMPL_NRML_PRE20170303 v1.0 (c) L. Petrov 18-AUG-2013 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     PIMA_AUTC_AMPL_NRML_PRE20170303
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  ICHN_1ST, ICHN_LAST, IFRQ, MASK_TYPE
      INTEGER*2  STA_IND(2)
      REAL*4     AC_OBS(PIM%NCHN,PIM%CONF%END_FRQ-PIM%CONF%BEG_FRQ+1)
      REAL*4     AC_AVR, AC_TOT, SUM_WEI_AVR, SUM_WEI_TOT
      INTEGER*4  J1, J2, J3, J4
!
      AC_AVR = 0.0
      SUM_WEI_AVR = 0.0
!
      DO 410 J1=ICHN_1ST,ICHN_LAST
         IF ( PIM%CONF%BANDPASS_MASK_FILE .NE. PIMA__BPASS_NO ) THEN
              AC_AVR = AC_AVR + PIM%BANDPASS_MASK(J1,IFRQ,STA_IND(1),MASK_TYPE)* &
     &                          PIM%BANDPASS_MASK(J1,IFRQ,STA_IND(2),MASK_TYPE)* &
     &                          AC_OBS(J1,IFRQ-PIM%CONF%BEG_FRQ+1)
              SUM_WEI_AVR = SUM_WEI_AVR + &
     &                      PIM%BANDPASS_MASK(J1,IFRQ,STA_IND(1),MASK_TYPE)* &
     &                      PIM%BANDPASS_MASK(J1,IFRQ,STA_IND(2),MASK_TYPE)
           ELSE  
              AC_AVR = AC_AVR + AC_OBS(J1,IFRQ-PIM%CONF%BEG_FRQ+1)
              SUM_WEI_AVR = SUM_WEI_AVR + 1.0
         END IF
 410  CONTINUE 

! --- Get the averaged bandpass over the used portion of the bandwidth
!
      IF ( SUM_WEI_AVR > 0.5 ) THEN
           PIMA_AUTC_AMPL_NRML_PRE20170303 = AC_AVR/SUM_WEI_AVR
         ELSE 
           PIMA_AUTC_AMPL_NRML_PRE20170303 = 1.0D0
      END IF
!
      RETURN
      END  FUNCTION  PIMA_AUTC_AMPL_NRML_PRE20170303  !#!#
