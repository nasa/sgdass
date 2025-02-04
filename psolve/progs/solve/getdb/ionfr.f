      SUBROUTINE IONFR ( IOBS, IMODE, N_FRQ, NOAP, FRQ, AMP_PHS, NOSAMPLE_R8, &
     &                   SAMPLE_RATE, REF_FREQ, VERBOSE_FLAG, BATCH_MODE, &
     &                   GREFF_FREQ, PHEFF_FREQ, RATEFF_FREQ )
! ************************************************************************
! *                                                                      *
! *   Routine  IONFR  computes effective ionosphere frequencies for      *
! *   group delay, phase delay and phase delay rate. It may work in two  *
! *   modes:                                                             *
! *                                                                      *
! *   IONFR_MODE__USEAP:   weights equal the number of processed samples *
! *                        at both USB and LSB and the fringe amplitude  *
! *                        of the channel are used. The number of        *
! *                        processed samples is taken from the array     *
! *                        NOSAMPLE_R8. If this array is empty, then     *
! *                        the number of processed samples is considered *
! *                        to be proportional to the number of           *
! *                        accumulation periods. If the arrays of        *
! *                        the number of accumulation periods is zero    *
! *                        or considered to be corrupted, then the       *
! *                        number of processed samples is considered to  *
! *                        be equal at each channel and be the product   *
! *                        of sampling rate and effective integration    *
! *                        time.                                         *
! *   IONFR_MODE__NOUSEAP: equal weights 1 are used for each channel.    *
! *                                                                      *
! *   In the case when the number of accumulation periods and/or fringe  *
! *   amplitudes are faked (f.e. all values are zero), then IONFR slips  *
! *   automatically into INOFR_MODE_NOUSAP.                              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   IOBS      ( INTEGER*4 ) -- The consecutive index of this           *
! *                              observation (for formatting error       *
! *                              messages only).                         *
! *   IMODE     ( INTEGER*4 ) -- Mode of computing the frequencies. One  *
! *                              of IONFR_MODE__USEAP or                 *
! *                              IONFR_MODE__NOUSEAP.                    *
! *   N_FRQ     ( INTEGER*4 ) -- The number of frequencies in the band.  *
! *   NOAP      ( INTEGER*2 ) -- Array of the number of accumulation     *
! *                              periods per USB/LSB and per channel.    *
! *                              Dimension: (2,N_FRQ). The first column  *
! *                              is for USB, the second column is for    *
! *                              LSB.                                    *
! *   FRQ       ( REAL*8    ) -- Array of sky channel frequencies.       *
! *                              Dimension: N_FRQ. Units: MHz.           *
! *   AMP_PHS   ( REAL*8    ) -- Array of the normalized fringe          *
! *                              amplitudes and fringe phases in deg per *
! *                              channel. Dimension: 2,N_FRQ. The first  *
! *                              column is for amplitudes, the second is *
! *                              for phases.                             *
! * NOSAMPLE_R8 ( REAL*8    ) -- Array of the number of processed        *
! *                              samples per sideband and per channel.   *
! *                              NB: databases created before 01-JUL-2001*
! *                              doesn't have lcode "#SAMPLES" and       *
! *                              therefore this array is zero. IONFR     *
! *                              checks whether this array is zero. If   *
! *                              yes, then it assumes that the           *
! *                              information about the number of         *
! *                              processed samples is unavailable.       *
! *                              Dimension: 2,N_FRQ.                     *
! * SAMPLE_RATE ( REAL*8    ) -- Sample rate in number of processed      *
! *                              samples per second.                     *
! *    REF_FREQ ( REAL*8    ) -- Reference frequency. Units: MHz.        *
! *  BATCH_MODE ( LOGICAL*4 )    If .TRUE. then no confirmation request  *
! *                              in the case of error will put on screen,*
! *                              and IONFR will make corrective action   *
! *                              itself. If .FALSE. then in the case of  *
! *                              error a request to continue or quit will*
! *                              be printed on screen.                   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  GREFF_FREQ  ( REAL*8   ) -- Effective ionosphere frequency for      *
! *                              group delay (in MHz).                   *
! *  PHEFF_FREQ  ( REAL*8   ) -- Effective ionosphere frequency for      *
! *                              phase delay (in MHz).                   *
! *  RATEFF_FREQ ( REAL*8   ) -- Effective ionosphere frequency for      *
! *                              phase delay date (in MHz).              *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * VERBOSE_FLAG ( LOGICAL*4 ) - If .TRUE. then a verbose error          *
! *                              message is put on screen and then       *
! *                              VERBOSE_FLAG is set to .FALSE.          *
! *                                                                      *
! *  ### 24-APR-2001      IONFR    v1.4 (c)  L. Petrov  30-DEC-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INTEGER*4  IOBS, IMODE, N_FRQ
      INTEGER*2  NOAP(2,N_FRQ)
      REAL*8     FRQ(N_FRQ), AMP_PHS(2,N_FRQ), NOSAMPLE_R8(2,N_FRQ), &
     &           SAMPLE_RATE, REF_FREQ, GREFF_FREQ, PHEFF_FREQ, RATEFF_FREQ
      LOGICAL*4  VERBOSE_FLAG, BATCH_MODE
!
      CHARACTER  STR*32
      INTEGER*4  NOAP_USE(2,MAX4_FRQ), ICODE, KCH_AMP, KCH_NAP, J1, J2, J3, J4
      REAL*8     FRQ_LIM
      PARAMETER  ( FRQ_LIM = 0.001 ) ! of MHz
      REAL*8     SUM_WEI, SUM_FR1, SUM_FR2, SUM_FQ2, SUM_FRI, SUM_DFI, &
     &           FRQ_MIN, FRQ_MAX, DFR, WEI_SQ
      LOGICAL*4  BAD_FREQ, BAD_CHAN, BAD_NOSAM, SAME_FREQ, FL_SEQBUG, IS_R8_NAN
!
      IF ( IMODE .NE. IONFR_MODE__USEAP   .AND. &
     &     IMODE .NE. IONFR_MODE__NOUSEAP       ) THEN
           WRITE ( 6, * ) 'IONFR  Wrong value of IMODE: ',IMODE
           STOP   'IONFR Abnormal termination'
      END IF
!
      FRQ_MAX   = FRQ(1)
      FRQ_MIN   = FRQ(1)
      BAD_FREQ  = .FALSE.
      BAD_CHAN  = .FALSE.
      BAD_NOSAM = .TRUE.
      SAME_FREQ = .FALSE.
      FL_SEQBUG = .TRUE.
      KCH_AMP = 0
      KCH_NAP = 0
!
! --- Check channels
!
      DO 410 J1=1,N_FRQ
         IF ( IMODE .EQ. IONFR_MODE__USEAP ) THEN
!
! ----------- Put in NOAP_USE actual nunmber of APs
!
              NOAP_USE(1,J1) = NOAP(1,J1)
              NOAP_USE(2,J1) = NOAP(2,J1)
            ELSE
!
! ----------- Put in NOAP_USE fictitious number of APs: 1 for lower sideband
! ----------- and 0 for upper sideband
!
              NOAP_USE(1,J1) = 0
              NOAP_USE(2,J1) = 1
         END IF
         IF ( AMP_PHS(1,J1) .LT. 0.0D0  .OR.  &
     &        AMP_PHS(1,J1) .GT. 1.0D0        ) THEN
              BAD_CHAN = .TRUE.
         END IF
!
! ------ Count: how many channels yields reasonable amplitude
!
         IF ( DFLOAT( NOAP_USE(1,J1)+NOAP_USE(2,J1) )*AMP_PHS(1,J1) .GT. &
     &        1.D-8 ) THEN
              KCH_AMP = KCH_AMP + 1
         END IF
         IF ( DFLOAT( NOAP_USE(1,J1) + NOAP_USE(2,J1) ) .GT. 0 ) THEN
              KCH_NAP = KCH_NAP + 1
         END IF
         IF ( J1 .GT. 1 ) THEN
!
! ----------- Check the bug in Fourfit which existed in 2000.
!
              IF ( NOAP(2,J1)-1 .NE. NOAP(2,J1-1) ) FL_SEQBUG = .FALSE.
         END IF
!
! ------ If there is at least one element of the array NOSAMPLE_R8 which is
! ------ larger than zero, then we lift flag BAD_NOSAM. It means that
! ------ information in NOSAMPLE_R8 array is usable.
!
         IF ( IS_R8_NAN ( NOSAMPLE_R8(1,J1) ) ) NOSAMPLE_R8(1,J1) = 0.0
         IF ( IS_R8_NAN ( NOSAMPLE_R8(2,J1) ) ) NOSAMPLE_R8(2,J1) = 0.0
         IF ( NOSAMPLE_R8(1,J1) .GE. 1.0D0 ) BAD_NOSAM = .FALSE. 
         IF ( NOSAMPLE_R8(2,J1) .GE. 1.0D0 ) BAD_NOSAM = .FALSE.
         IF ( FRQ(J1)  .EQ. 0.0D0 ) THEN
              NOSAMPLE_R8(1,J1) = 0.0D0
              NOSAMPLE_R8(2,J1) = 0.0D0
         END IF
!
! ------ Compute minimal and maximal frequencies
!
         IF ( FRQ(J1) .LT. MIN__FRQ ) BAD_FREQ = .TRUE.
         IF ( FRQ(J1) .GT. MAX__FRQ ) BAD_FREQ = .TRUE.
         IF ( FRQ(J1) .LT. FRQ_MIN  ) FRQ_MIN = FRQ(J1)
         IF ( FRQ(J1) .GT. FRQ_MAX  ) FRQ_MAX = FRQ(J1)
 410  CONTINUE
!
! --- Check frequencies
!
      IF ( ( FRQ_MAX - FRQ_MIN ) .LT. FRQ_LIM ) THEN
           SAME_FREQ = .TRUE.
      END IF
!
      IF ( BAD_FREQ .OR. SAME_FREQ ) THEN
!
! -------- Our best guess what to do for such a pathological case.
!
           GREFF_FREQ = REF_FREQ
           PHEFF_FREQ = REF_FREQ
           RATEFF_FREQ= REF_FREQ
      END IF
!
      IF ( ( BAD_FREQ .OR. SAME_FREQ ) .AND. VERBOSE_FLAG ) THEN
           IF ( BAD_FREQ ) THEN
                WRITE ( 6, 110 ) IOBS
 110            FORMAT ( 'Observation ',I6,'. Wrong frequency setup: some ', &
     &                   'frequencies are beyond the limits' )
           END IF
           IF ( SAME_FREQ ) THEN
                WRITE ( 6, 120 ) IOBS
 120            FORMAT ( 'Observation ',I6,'. Wrong frequency setup: all ', &
     &                   'frequencies are the same' )
           END IF
           DO 420 J2=1,N_FRQ
              WRITE ( 6, 130 ) J2, FRQ(J2)
 130          FORMAT ( 1X,'Chan: ',I2,'  Freq: ',F10.2,' MHz' )
 420       CONTINUE
           IF ( BATCH_MODE ) THEN
                VERBOSE_FLAG = .FALSE.
              ELSE
                WRITE ( 6, 140 )
 140            FORMAT ( 'Hit <A> for suppressing other warnings or any key ', &
     &                   'to proceed >>'$ )
                CALL INSIM ( STR, ICODE )
                IF ( STR(1:1) .EQ. 'A' .OR. STR(1:1) .EQ. 'a' ) THEN
                     VERBOSE_FLAG = .FALSE.
                END IF
                WRITE ( 6, '(A)' ) ' '
           END IF
      END IF
!
      IF ( BAD_FREQ .OR. SAME_FREQ ) THEN
!
! -------- If frequencies are bad we have nothing to do
!
           RETURN
      END IF
!
      IF ( KCH_AMP .LT. 2  .OR.  KCH_NAP .LT. 2  .OR.  FL_SEQBUG .OR. &
     &     BAD_CHAN ) THEN
!
! -------- the number of AP is too small, or amplitudes were zero or
! -------- "sequence bug" or bad amplitude was detected in at least one channel
! -------- then we weight each channel equally
!
           DO 430 J3=1,N_FRQ
              NOAP_USE(1,J3) = 0
              NOAP_USE(2,J3) = 1
              AMP_PHS(1,J3)  = 1.D0
 430       CONTINUE
      END IF
!
! --- Adder
!
      SUM_WEI = 0.0
      SUM_FR1 = 0.0
      SUM_FR2 = 0.0
      SUM_FQ2 = 0.0
      SUM_FRI = 0.0
      SUM_DFI = 0.0
      DO 440 J4=1,N_FRQ
         DFR = FRQ(J4) - REF_FREQ
!
! ------ Compute weights
!
         IF ( IMODE .EQ. IONFR_MODE__USEAP  ) THEN
              IF ( BAD_NOSAM ) THEN
!
! ---------------- Square of weights are proportional to the number
! ---------------- of accumulation periods
!
                   WEI_SQ = DFLOAT(NOAP_USE(1,J4) + NOAP_USE(2,J4))* &
     &                      AMP_PHS(1,J4)
                 ELSE
!
! ---------------- Square of weights are proportional to the number of
! ---------------- processed samples
!
                   WEI_SQ = (NOSAMPLE_R8(1,J4) + &
     &                       NOSAMPLE_R8(2,J4))*AMP_PHS(1,J4)/SAMPLE_RATE
              END IF
            ELSE
              WEI_SQ = 1.0D0
         END IF
!
         SUM_WEI = SUM_WEI + WEI_SQ
         SUM_FR1 = SUM_FR1 + WEI_SQ*DFR
         SUM_FR2 = SUM_FR2 + WEI_SQ*DFR**2
         SUM_FQ2 = SUM_FQ2 + WEI_SQ*FRQ(J4)**2
         SUM_FRI = SUM_FRI + WEI_SQ/FRQ(J4)
         SUM_DFI = SUM_DFI + WEI_SQ*DFR/FRQ(J4)
 440  CONTINUE
!
! --- Finally, compute effective frequencies.
!
      IF ( (SUM_FRI*SUM_FR2 - SUM_DFI*SUM_FR1 ) > 1.D-4 ) THEN
           PHEFF_FREQ = DSQRT ( REF_FREQ*(SUM_WEI*SUM_FR2 - SUM_FR1**2)/ &
     &                              (SUM_FRI*SUM_FR2 - SUM_DFI*SUM_FR1 ) )
           IF ( DABS((SUM_FRI*SUM_FR1 - SUM_DFI*SUM_WEI)) < 1.D-30 ) THEN
                GREFF_FREQ = REF_FREQ
              ELSE 
                GREFF_FREQ = DSQRT ( (SUM_WEI*SUM_FR2 - SUM_FR1**2)/ &
     &                          (SUM_FRI*SUM_FR1 - SUM_DFI*SUM_WEI) )
           END IF
           RATEFF_FREQ= DSQRT (  SUM_FQ2/SUM_WEI )
         ELSE
           PHEFF_FREQ = REF_FREQ
           GREFF_FREQ = REF_FREQ
           RATEFF_FREQ = REF_FREQ
      END IF
!
      END  SUBROUTINE   IONFR  !#!#
