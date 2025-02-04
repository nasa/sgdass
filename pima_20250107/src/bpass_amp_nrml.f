      SUBROUTINE BPASS_AMP_NRML ( PIM, IND_STA, IND_STA_REF, &
     &                            BPS_CMPL, BPS_REF_CMPL, &
     &                            AMPL_IF_NRML, AMPL_BAND_NRML, AMPL_IF_AVR, &
     &                            AMPL_IF_RMS, AMPL_BAND_RMS, AMPL_INTEGRAL )
! ************************************************************************
! *                                                                      *
! *   Routine  BPASS_AMP_NRML performs bandpass normalization.           *
! *   Normalization is performed in two modes: power mode and voltage    *
! *   mode. Parameter IND_STA_REF (index of the reference station)       *
! *   should be in 0 for power mode and > 0 for voltage mode.            *
! *                                                                      *
! *   Normalization coefficient in the power mode, Np, is computed as    *
! *                                                                      *
! *        \sum M_i(f) |B_i(f)|                                          *
! *   Np = --------------------                                          *
! *        \sum M_i(f)                                                   *
! *                                                                      *
! *   Normalization coefficient in the voltage mode, Nv, is computed as  *
! *                                                                      *
! *        \sum M_i(f) M_r(f) sqrt(|B_i(f)| * |B_r(f)| )                 *                                                                       *
! *   Nv = ---------------------------------------------                 *
! *        \sum M_i(f) M_r(f)                                            *
! *                                                                      *
! *   where M_i(f) is the mask of the i-th station (0 or 1), M_r(f) is   *
! *   the mask of the reference station, B_i(f) bandpass of the i-th     *
! *   station, and B_r(f) is the bandpass of the remote station.         *
! *                                                                      *
! *   BPASS_AMP_NRML divides the bandpass for the i-th station by        *
! *   the normalization coefficient. Depending on PIM%CONF%BPS_NORM,     *
! *   summation is performed either over IF or over band. Only a portion *
! *   of the bandpass with IFs within [PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ]*
! *   range is normalized. If PIM%CONF%BPS_NORM == NO, then no           *
! *   normalization of performed.                                        *
! *                                                                      *
! *   Routine also computes a number of statistics.                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *         PIM ( PIMA__TYP ) -- Object with information related to      *
! *                              program PIMA.                           *
! *     IND_STA ( INTEGER*4 ) -- Index of the remote (i-th) station.     *
! * IND_STA_REF ( INTEGER*4 ) -- Reference station index.                *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   AMPL_IF_NRML ( REAL*8    ) -- Array of normalization factors       *
! *                                 averaged over IF. Dimension: PIM%NFRQ*
! * AMPL_BAND_NRML ( REAL*8    ) -- Band-averaged normalization factor.  *
! *    AMPL_IF_AVR ( REAL*8    ) -- Array of IF-averaged bandpass.       *
! *                                 Dimension: PIM%NFRQ.                 *
! *    AMPL_IF_RMS ( REAL*8    ) -- Array of rms bandpass amplitude for  *
! *                                 individual IFs.                      *
! *  AMPL_BAND_RMS ( REAL*8    ) -- Rms of IF-averaged amplitudes over   *
! *                                 the band.                            *
! *  AMPL_INTEGRAL ( REAL*8    ) -- Integral averaged amplitude.         *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! * BPS_CMPL ( COMPLEX*8 ) -- complex bandpass. Dimension:               *
! *                           PIM%NCHN,PIM%NFRQ.                         *
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
! *                                                                      *
! * ### 27-JAN-2009  BPASS_AMP_NRML  v3.4 (c) L. Petrov  16-NOV-2018 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      COMPLEX*8  BPS_CMPL(PIM%NCHN,PIM%NFRQ), BPS_REF_CMPL(PIM%NCHN,PIM%NFRQ)
      INTEGER*4  IND_STA, IND_STA_REF
      REAL*4     AMPL_IF_NRML(PIM%NFRQ), AMPL_BAND_NRML, &
     &           AMPL_IF_AVR(PIM%NFRQ), AMPL_IF_RMS(PIM%NFRQ), &
     &           AMPL_BAND_RMS, AMPL_INTEGRAL
      REAL*4     AMPL_AVR(PIM__MFRQ), AMPL_BAND_AVR, &
     &           AMPL_MAX_DEV, SIG_LEV_CHN, BAND_WIDTH 
      LOGICAL*1  FL_BMASK
      INTEGER*1  MASK_CHN, MASK_CHN_LAST
      PARAMETER  ( SIG_LEV_CHN = 3.00 )
      INTEGER*1  USED_CHN_FLAG(PIM__MCHN), USED_FRQ_FLAG(PIM__MFRQ)
      INTEGER*4  J1, J2, J3, J4, J5, J6, IND_MAX, KCHN(PIM__MFRQ), &
     &           IFRQ, KFRQ, N_ITER, KP, IER
      LOGICAL*4, EXTERNAL :: IS_R4_NAN
!
      IF ( PIM%CONF%BANDPASS_MASK_FILE .NE. PIMA__BPASS_NO  .AND. &
     &     ASSOCIATED ( PIM%BANDPASS_MASK )                       ) THEN
           FL_BMASK = .TRUE.
         ELSE 
           FL_BMASK = .FALSE.
      END IF
!
      AMPL_BAND_AVR = 0.0
      AMPL_BAND_RMS = 0.0
      IFRQ = 0
      KFRQ = 0
!
! --- Compute normalization factors
!
      DO 410 J1=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         IFRQ = IFRQ + 1
!
! ------ Compute average over spectral channels with weights reciprocal
! ------ to the geometric mean of the autocorrelation
!
         AMPL_AVR(IFRQ) = 0.0
         KCHN(IFRQ) = 0
         DO 420 J2=1,PIM%NCHN ! cycle over spectral channels
            IF ( FL_BMASK ) THEN
                 IF ( IND_STA_REF > 0 ) THEN
                      MASK_CHN = PIM%BANDPASS_MASK(J2,J1,IND_STA,PIMA__MASK_BPAS) * &
     &                           PIM%BANDPASS_MASK(J2,J1,IND_STA_REF,PIMA__MASK_BPAS)
                    ELSE
                      MASK_CHN = PIM%BANDPASS_MASK(J2,J1,IND_STA,PIMA__MASK_BPAS)
                 END IF
               ELSE
                 MASK_CHN = 1
            END IF
            IF ( IS_R4_NAN ( REAL(BPS_CMPL(J2,J1)) ) .OR. &
     &           IS_R4_NAN ( IMAG(BPS_CMPL(J2,J1)) )      ) THEN
                 BPS_CMPL(J2,J1) = (0.0, 0.0)
            END IF
            IF ( IND_STA_REF > 0 ) THEN
                 IF ( IS_R4_NAN ( REAL(BPS_REF_CMPL(J2,J1)) ) .OR. &
     &                IS_R4_NAN ( IMAG(BPS_REF_CMPL(J2,J1)) )      ) THEN
                      BPS_REF_CMPL(J2,J1) = (0.0, 0.0)
                 END IF
            END IF
            IF ( ABS(BPS_CMPL(J2,J1)) > PIMA__WEI_MIN ) THEN
                 IF ( IND_STA_REF > 0 ) THEN
                      AMPL_AVR(IFRQ) = AMPL_AVR(IFRQ) + MASK_CHN*SQRT(ABS(BPS_CMPL(J2,J1))*ABS(BPS_REF_CMPL(J2,J1)))
                    ELSE 
                      AMPL_AVR(IFRQ) = AMPL_AVR(IFRQ) + MASK_CHN*ABS(BPS_CMPL(J2,J1))
                 END IF
                 KCHN(IFRQ) = KCHN(IFRQ) + MASK_CHN
            END IF
 420     CONTINUE 
!
! ------ Compute bandpass amplitude averaged over the IF
!
         IF ( KCHN(IFRQ) > 0 ) THEN
              AMPL_AVR(IFRQ)     = AMPL_AVR(IFRQ)/KCHN(IFRQ)
            ELSE 
              AMPL_AVR(IFRQ)     = 1.0
         END IF
!
! ------ Compute IF normalization factor
!
         IF ( AMPL_AVR(IFRQ) > PIMA__AMP_MIN ) THEN
              AMPL_IF_NRML(IFRQ) = 1.0/AMPL_AVR(IFRQ)
            ELSE 
              AMPL_IF_NRML(IFRQ) = 1.0
         END IF
!
! ------ Update band accumulator
!
         AMPL_BAND_AVR  = AMPL_BAND_AVR + AMPL_AVR(IFRQ) 
 410  CONTINUE 
!
! --- Compute the band renormalization factor
!
      AMPL_BAND_AVR     = AMPL_BAND_AVR/IFRQ
      IF ( AMPL_BAND_AVR > PIMA__AMP_MIN ) THEN
           AMPL_BAND_NRML = 1.D0/AMPL_BAND_AVR
      END IF
!
! --- ... and the rms of the IF-average bandpasses with respect to the band-averaged bandpasses
!
      IFRQ = 0
      KFRQ = 0
      DO 430 J3=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         IFRQ = IFRQ + 1
         AMPL_BAND_RMS = AMPL_BAND_RMS + ( AMPL_AVR(IFRQ) - AMPL_BAND_AVR )**2
  430 CONTINUE 
      AMPL_BAND_RMS = SQRT(AMPL_BAND_RMS/IFRQ)
!
! --- Finally, normalize the complex bandbpass either to the IF average or 
! --- band avergage amplitude and compute statstics
!
      AMPL_BAND_RMS = 0.0D0
      AMPL_INTEGRAL = 0.0D0
      BAND_WIDTH = 0.0D0
      IFRQ = 0
      KP = 0 
      DO 440 J4=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         IFRQ = IFRQ + 1
         AMPL_IF_AVR(J4) = 0.0
         DO 450 J5=1,PIM%NCHN
            IF ( FL_BMASK ) THEN
                 IF ( IND_STA_REF > 0 ) THEN
                      MASK_CHN = PIM%BANDPASS_MASK(J5,J4,IND_STA,PIMA__MASK_BPAS) * &
     &                           PIM%BANDPASS_MASK(J5,J4,IND_STA_REF,PIMA__MASK_BPAS)
                    ELSE
                      MASK_CHN = PIM%BANDPASS_MASK(J5,J4,IND_STA,PIMA__MASK_BPAS)
                 END IF
               ELSE
                 MASK_CHN = 1
            END IF
!
! --------- Perform normalization
!
            KP = KP + 1
            IF ( PIM%CONF%BPS_NORML == PIMA__NORML_BAND ) THEN
                 BPS_CMPL(J5,J4) = BPS_CMPL(J5,J4)*AMPL_BAND_NRML
               ELSE IF ( PIM%CONF%BPS_NORML == PIMA__NORML_IF ) THEN
                 BPS_CMPL(J5,J4) = BPS_CMPL(J5,J4)*AMPL_IF_NRML(IFRQ)
               ELSE IF ( PIM%CONF%BPS_NORML == PIMA__NORML_NO ) THEN
                 CONTINUE  
            END IF
!
! --------- Update the accumulator for the integral averaged
!
            IF ( J5 > 1 ) THEN
                 IF ( IND_STA_REF > 0 ) THEN
                      AMPL_INTEGRAL = AMPL_INTEGRAL + &
     &                      (   MASK_CHN      * SQRT ( ABS(BPS_CMPL(J5,J4))   * ABS(BPS_REF_CMPL(J5,J4)) )  &
     &                        + MASK_CHN_LAST * SQRT ( ABS(BPS_CMPL(J5-1,J4)) * ABS(BPS_REF_CMPL(J5-1,J4))  ) )* &
     &                          PIM%FRQ(J4,PIM%CONF%FRQ_GRP)%CHAN_WIDTH/2.0D0
                    ELSE
                      AMPL_INTEGRAL = AMPL_INTEGRAL + &
     &                      (   MASK_CHN      * ABS(BPS_CMPL(J5,J4))   &
     &                        + MASK_CHN_LAST * ABS(BPS_CMPL(J5-1,J4)) )* &
     &                          PIM%FRQ(J4,PIM%CONF%FRQ_GRP)%CHAN_WIDTH/2.0D0
                 END IF
                 BAND_WIDTH = BAND_WIDTH + &
     &                        MASK_CHN * PIM%FRQ(J4,PIM%CONF%FRQ_GRP)%CHAN_WIDTH
            END IF
            AMPL_IF_AVR(J4) = AMPL_IF_AVR(J4) + MASK_CHN * ABS(BPS_CMPL(J5,J4))
            MASK_CHN_LAST = MASK_CHN
 450    CONTINUE 
        IF ( KCHN(IFRQ) > 0 ) THEN
             AMPL_IF_AVR(J4) = AMPL_IF_AVR(J4)/KCHN(IFRQ)
           ELSE 
             AMPL_IF_AVR(J4) = 1.0
        END IF
!
! ----- Compute the rms over the IFRQ -th IF with respect to the IF average
!
        AMPL_IF_RMS(J4) = 0.0
        DO 460 J6=1,PIM%NCHN
           AMPL_IF_RMS(J4) = AMPL_IF_RMS(J4) + &
     &                        (ABS(BPS_CMPL(J6,J4)) - AMPL_IF_AVR(J4))**2
           AMPL_BAND_RMS    = AMPL_BAND_RMS + &
     &                        (ABS(BPS_CMPL(J6,J4)) - AMPL_BAND_AVR)**2
 460    CONTINUE 
        IF ( KCHN(IFRQ) > 1 ) THEN
             AMPL_IF_RMS(J4) = SQRT ( AMPL_IF_RMS(J4)/(KCHN(IFRQ)-1) )
           ELSE 
             AMPL_IF_RMS(J4) = 0.0
        END IF
 440 CONTINUE 
!
      IF ( BAND_WIDTH > 0.0D0 ) THEN
           AMPL_BAND_RMS = SQRT ( AMPL_BAND_RMS/IFRQ )
           AMPL_INTEGRAL = AMPL_INTEGRAL/BAND_WIDTH 
         ELSE
           AMPL_BAND_RMS = 1.0D0
           AMPL_INTEGRAL = 1.0D0
      END IF
!
      RETURN
      END  SUBROUTINE BPASS_AMP_NRML  !#!#
