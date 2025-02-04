      SUBROUTINE SUPSTAT_SET ( IUNW, IUNWP, LQUAL, LQUALXS, &
     &                         ICORR, GIONSG, PHIONS, IWVBIT1, ISITE, &
     &                         JSITI, ITT, ISTAR, ELEV, KIONO, &
     &                         SNR_X, SNR_S, SUPSTAT, UACSUP )
! ************************************************************************
! *                                                                      *
! *   Routine  SUPSTAT_SET  analyzes circumstances of the observation    *
! *   and global settings of the solution. Then it sets suppression      *
! *   status of the observation under consideration in according with    *
! *   this status, user action of suppression and suppression method     *
! *   SUPMET. If UACSUP variable has not been initialized, SUPSTAT_SET   *
! *   initializes it in according with IUNW, IUNWP flags.                *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     IUNW ( INTEGER*2 ) -- Downweight flag for group delay solution   *
! *                           used before MAY98.                         *
! *    IUNWP ( INTEGER*2 ) -- Downweight flag for phase delay solution   *
! *                           used before MAY98.                         *
! *    LQUAL ( INTEGER*2 ) -- Quality code for X-band (Holerit variable) *
! *  LQUALXS ( INTEGER*2 ) -- Quality code for S-band (Holerit variable) *
! *    ICORR ( INTEGER*2 ) -- Ionosphere correction status bit field.    *
! *   GIONSG ( REAL*8    ) -- Array (dim=2) for formal errors of         *
! *                           ionosphere calibration for group delay and *
! *                           delay rate.                                *
! *   PHIONS ( REAL*8    ) -- Formal error of ionosphere calibration for *
! *                           phase delay.                               *
! *  IWVBIT1 ( INTEGER*2 ) -- Array (dim=2) of WVR status.               *
! *    ISITE ( INTEGER*2 ) -- Array (dim=2) of station code of the       *
! *                           baseline under consideration.              *
! *    JSITI ( INTEGER*2 ) -- Station ion calibration flags from NAMFIL. *
! *      ITT ( INTEGER*2 ) -- NAMFIL/PARFIL station correspondence table *
! *    ISTAR ( INTEGER*2 ) -- Source code.                               *
! *     ELEV ( REAL*8    ) -- Array (dim=2) of elevation angles for      *
! *                           both stations.                             *
! *    KIONO ( LOGICAL*2 ) -- Global flag of ionospheric calibration.    *
! *    SNR_X ( REAL*8    ) -- SNR at upper band (X-band).                *
! *    SNR_S ( REAL*8    ) -- SNR at lower band (S-band).                *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  SUPSTAT ( INTEGER*2 ) -- 32-bits field of suppression status        *
! *                           which is set automatically in according    *
! *                           with circumstances of the observation and  *
! *                           global settings (such as quality code      *
! *                           limit, applying ionosphere calibration     *
! *                           etc. )                                     *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   UACSUP ( INTEGER*2 ) -- 16-bits field of user action for           *
! *                           suppression.                               *
! *                                                                      *
! *  ###  15-APR-1998  SUPSTAT_SET  v2.0  (c)  L. Petrov 11-JUN-2010 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE    'solve.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'socom.i'
      LOGICAL*2  KIONO
      INTEGER*2  JSITI(MAX_ARC_STA), ITT(MAX_ARC_STA), IUNW, IUNWP, LQUAL, &
     &           LQUALXS, IWVBIT1(2), ICORR, ISITE(2), ISTAR, SUPSTAT(2), &
     &           UACSUP
      REAL*8     GIONSG(2), PHIONS, ELEV(2), SNR_X, SNR_S
      INTEGER*4 IARR(2), IP
!
      CHARACTER  LQUAL_CHR*2, LQUALXS_CHR*2, LQ*2, LQ_OPP*2
      INTEGER*2  IQC_X, IQC_S
!
      LOGICAL*4  DATYP_INQ
      LOGICAL*2  KBIT
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
! --- Initialization
!
      SUPSTAT(1) = 0
      SUPSTAT(2) = 0
!
      IF ( IUNW .EQ. 8 ) THEN
!
! -------- Re-set ICORR bits in according with IUNW value. They should have
! -------- been set to these value already, but it may appear that they had
! -------- not. We do it in a case once more just for fixing such a situation
!
           CALL SBIT ( ICORR, INT2(4),  INT2(0) )
           CALL SBIT ( ICORR, INT2(10), INT2(0) )
           CALL SBIT ( ICORR, INT2(2),  INT2(1) )
           CALL SBIT ( ICORR, INT2(8),  INT2(1) )
      END IF
!
! --- Not to forget: We should check was UACSUP been intitliezed. Databases or
! --- superfiles created before MAY98 may not have UACSUP and they have
! --- archaic downweight codes IUNW, IUNWP
!
      IF ( .NOT. KBIT ( UACSUP, INIT__UAS ) ) THEN
!
! -------- Oh, it was really so. Then we should set bits UACSUP in according
! -------- with IUNW, IUNWP
!
           CALL UNW_SUPSTAT ( IUNW, IUNWP, SUPSTAT, UACSUP )
      END IF
!
      WRITE ( LQUAL_CHR,   FMT='(A2)' ) LQUAL
      WRITE ( LQUALXS_CHR, FMT='(A2)' ) LQUALXS
!
! --- Decoding quality code for the X-band
!
      IQC_X = -1
      LQ    = LQUAL_CHR
      IF ( DATYP_INQ ( IDATYP, SBAND__DTP )   .AND. &
     &     KBIT ( OPP_STATUS, OPP_SET1__BIT  )  ) THEN
!
! -------- Take quality code for the opposite band.
! -------- NB: side-effect: NOFS and NOFS bits are interchanged
!
           LQ = LQUALXS_CHR
      END IF
!
      IF ( INDEX ( LQ, '0' ) .NE. 0 ) IQC_X =  0
      IF ( INDEX ( LQ, '1' ) .NE. 0 ) IQC_X =  1
      IF ( INDEX ( LQ, '2' ) .NE. 0 ) IQC_X =  2
      IF ( INDEX ( LQ, '3' ) .NE. 0 ) IQC_X =  3
      IF ( INDEX ( LQ, '4' ) .NE. 0 ) IQC_X =  4
      IF ( INDEX ( LQ, '5' ) .NE. 0 ) IQC_X =  5
      IF ( INDEX ( LQ, '6' ) .NE. 0 ) IQC_X =  6
      IF ( INDEX ( LQ, '7' ) .NE. 0 ) IQC_X =  7
      IF ( INDEX ( LQ, '8' ) .NE. 0 ) IQC_X =  8
      IF ( INDEX ( LQ, '9' ) .NE. 0 ) IQC_X =  9
!
! --- Decoding quality code for the S-band
!
      IQC_S  = -1
      LQ_OPP = LQUALXS_CHR
!
      IF ( DATYP_INQ ( IDATYP, SBAND__DTP )  .AND.   &
     &     KBIT ( OPP_STATUS, OPP_SET1__BIT  )  ) THEN
!
! -------- Take quality code for the current band
!
           LQ_OPP = LQUAL_CHR
      END IF
!
      IF ( INDEX ( LQ_OPP, '0' ) .NE. 0 ) IQC_S =  0
      IF ( INDEX ( LQ_OPP, '1' ) .NE. 0 ) IQC_S =  1
      IF ( INDEX ( LQ_OPP, '2' ) .NE. 0 ) IQC_S =  2
      IF ( INDEX ( LQ_OPP, '3' ) .NE. 0 ) IQC_S =  3
      IF ( INDEX ( LQ_OPP, '4' ) .NE. 0 ) IQC_S =  4
      IF ( INDEX ( LQ_OPP, '5' ) .NE. 0 ) IQC_S =  5
      IF ( INDEX ( LQ_OPP, '6' ) .NE. 0 ) IQC_S =  6
      IF ( INDEX ( LQ_OPP, '7' ) .NE. 0 ) IQC_S =  7
      IF ( INDEX ( LQ_OPP, '8' ) .NE. 0 ) IQC_S =  8
      IF ( INDEX ( LQ_OPP, '9' ) .NE. 0 ) IQC_S =  9
!
! --- Setting values of SUPSTAT describing status of the observation
!
! --- Test of X-band quality code
!
      IF ( IQC_X .LT. 1                 ) CALL SBIT ( SUPSTAT, NOFX__SPS, INT2(1) )
      IF ( IQC_X .LT. QUALCODE_GOOD_LIM ) CALL SBIT ( SUPSTAT, BQCX__SPS, INT2(1) )
!
! --- Test of S-band quality code (Quality code of the opposite band)
!
      IF ( KBIT ( OPP_STATUS, OPP_SET1__BIT )  .AND.  LQ_OPP .NE. '  ' ) THEN
!
! ------- There is quality code for S-band available
!
          IF ( IQC_S .LT. 1                 ) CALL SBIT ( SUPSTAT, NOFS__SPS, INT2(1) )
          IF ( IQC_S .LT. QUALCODE_GOOD_LIM ) CALL SBIT ( SUPSTAT, BQCS__SPS, INT2(1) )
        ELSE
!
! ------- If there is no quality code for S-band available then we examine
! ------- ICORR. Bit 2 means "No matching group data for GION",
! ------- bit 6 means "Matching obs has quality code of 0 ('no fringes')"
!
          IF ( KBIT ( ICORR, INT2(2)) ) CALL SBIT ( SUPSTAT, NOFS__SPS, INT2(1) )
          IF ( KBIT ( ICORR, INT2(6)) ) CALL SBIT ( SUPSTAT, NOFS__SPS, INT2(1) )
      END IF
!
! --- Test of elevation cutoff limit
!
      IF ( ELEV(1) .LT. ELVCUT(ISITE(1)) ) CALL SBIT ( SUPSTAT, CUEL__SPS, &
     &                                                 INT2(1) )
      IF ( ELEV(2) .LT. ELVCUT(ISITE(2)) ) CALL SBIT ( SUPSTAT, CUEL__SPS, &
     &                                                 INT2(1) )
!
! --- Test the baseline selection flag
!
      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! -------- Phase delay solution type
!
           IF ( .NOT. KBIT ( IBLSEL_P(1,ISITE(1)),ISITE(2) ) ) THEN
                CALL SBIT ( SUPSTAT, DSBS__SPS, INT2(1) )
           END IF
         ELSE
!
! -------- Group delay (or rate only) solution type
!
           IF ( .NOT. KBIT ( IBLSEL_G(1,ISITE(1)),ISITE(2) ) ) THEN
                CALL SBIT ( SUPSTAT, DSBS__SPS, INT2(1) )
           END IF
      END IF
!
! --- Test the source selection flag
!
      IF ( .NOT. KBIT(ISRSEL(1),ISTAR) ) CALL SBIT ( SUPSTAT, DSSO__SPS, &
     &                                               INT2(1) )
!@!
!@! --- Test for WVR data against mask WVMASK
!@!
!@      IF (   WVMASK(ISITE(1))                    .NE. 0  .AND. &
!@     &       WVMASK(ISITE(1)) .AND. ( IWVBIT1(1) .EQ. 0 )      ) THEN
!@!
!@             CALL SBIT ( SUPSTAT, BWVR__SPS, INT2(1) )
!@      END IF
!@      IF (   WVMASK(ISITE(2))                    .NE. 0  .AND. &
!@     &       WVMASK(ISITE(2)) .AND. ( IWVBIT1(2) .EQ. 0  )     ) THEN
!@!
!@             CALL SBIT ( SUPSTAT, BWVR__SPS, INT2(1) )
!@      END IF
!
! --- Test of parangle status
!
      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
           IF ( IUNWP .EQ. 3 ) CALL SBIT ( SUPSTAT, CUEL__SPS, INT2(1) )
         ELSE
           IF ( IUNW  .EQ. 3 ) CALL SBIT ( SUPSTAT, CUEL__SPS, INT2(1) )
      END IF
!
! --- Setting other IUNW-, IUNWP- related bits
!
      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
           IF ( IUNWP .NE.  0 ) CALL SBIT ( SUPSTAT, IUNW__SPS, INT2(1) )
           IF ( IUNWP .EQ.  4 ) CALL SBIT ( SUPSTAT, BPRN__SPS, INT2(1) )
           IF ( IUNWP .EQ. 12 ) CALL SBIT ( SUPSTAT, BWVR__SPS, INT2(1) )
           IF ( IUNWP .EQ. 16 ) CALL SBIT ( SUPSTAT, DSBS__SPS, INT2(1) )
           IF ( IUNWP .EQ. 17 ) CALL SBIT ( SUPSTAT, DSSO__SPS, INT2(1) )
           IF ( IUNWP .EQ. 98 ) CALL SBIT ( SUPSTAT, WPAS__SPS, INT2(1) )
         ELSE
           IF ( IUNW  .NE.  0 ) CALL SBIT ( SUPSTAT, IUNW__SPS, INT2(1) )
           IF ( IUNW  .EQ.  4 ) CALL SBIT ( SUPSTAT, BPRN__SPS, INT2(1) )
           IF ( IUNW  .EQ. 12 ) CALL SBIT ( SUPSTAT, BWVR__SPS, INT2(1) )
           IF ( IUNW  .EQ. 16 ) CALL SBIT ( SUPSTAT, DSBS__SPS, INT2(1) )
           IF ( IUNW  .EQ. 17 ) CALL SBIT ( SUPSTAT, DSSO__SPS, INT2(1) )
      END IF
!
! --- Setting bits describing group delay ionosphere correction status
!
      IF (       KBIT ( JSITI(ITT(ISITE(1))), INT2(4) ) .AND. &
     &     .NOT. KBIT ( JSITI(ITT(ISITE(1))), INT2(5) ) .AND. &
     &           KBIT ( JSITI(ITT(ISITE(2))), INT2(4) ) .AND. &
     &     .NOT. KBIT ( JSITI(ITT(ISITE(2))), INT2(5) )       ) THEN
!
! ------ Test of GION availability
!
         IF ( KBIT( JSITI(ITT(ISITE(1))), INT2(1) ) .AND. &
     &        KBIT( JSITI(ITT(ISITE(2))), INT2(1) )       ) THEN
!
! ----------- GION calibration was available for observations at this baseline.
! ----------- Let's look at ionosphere status of the observation
!
              IF ( .NOT. KBIT ( ICORR, INT2(4) ) ) CALL SBIT ( SUPSTAT, &
     &                                             GION__SPS, INT2(1) )
           ELSE
!
! ----------- GION calibration is not available for any observation of the bas.
!
              CALL SBIT ( SUPSTAT, GION__SPS, INT2(1) )
          END IF
!
! ------- Make certain that the iono correction is good and that the
! ------- matching S-band has non-zero fringe detection.  (Compensates
! ------- for an old CNPLT bug (now fixed.))
!
          IF ( KBIT( ICORR, INT2(5)) .OR. KBIT( ICORR, INT2(6) ) ) THEN
!
! ------------ Group ionosphere calibration is bad: matching observation was
! ------------ downweighted or matching observation didn't have fringes
!
               CALL SBIT ( SUPSTAT, GIO1__SPS, INT2(1) )
          END IF
          IF ( KBIT( ICORR, INT2(6)) ) THEN
!
! ------------ Group ionosphere calibration is severely bad: matching
! ------------ observation didn't have fringes
!
               CALL SBIT ( SUPSTAT, GIO2__SPS, INT2(1) )
          END IF
!
          IF ( KIONO .AND. ( GIONSG(1).EQ.0 .OR. GIONSG(2).EQ.0 ) ) THEN
!
! ------------ Group ionosphere calibration is severely bad: matching
! ------------ observation didn't have fringes
!
               CALL SBIT ( SUPSTAT, GIO3__SPS, INT2(1) )
          END IF
!
          IF ( .NOT. KIONO .AND. KBIT ( ICORR, INT2(2) ) ) THEN
!
! ------------ Group ionosphere calibration is severely bad: matching
! ------------ observation didn't have fringes
!
               CALL SBIT ( SUPSTAT, GIO4__SPS, INT2(1) )
          END IF
      END IF
!
! --- Setting bits describing phase delay inosphere correction status
!
      IF (       KBIT ( JSITI(ITT(ISITE(1))), INT2(5)) .AND. &
     &     .NOT. KBIT ( JSITI(ITT(ISITE(1))), INT2(4)) .AND. &
     &           KBIT ( JSITI(ITT(ISITE(2))), INT2(5)) .AND. &
     &     .NOT. KBIT ( JSITI(ITT(ISITE(2))), INT2(4))       ) THEN ! of the baseline
!
! ------ Test of PION availability
!
         IF ( KBIT( JSITI(ITT(ISITE(1))), INT2(2) )  .AND. &
     &        KBIT( JSITI(ITT(ISITE(2))), INT2(2) )        ) THEN
!
! ----------- PION calibration was available for observations at this baseline.
! ----------- Let's look at ionpshere status of the observation
!
              IF ( .NOT. KBIT ( ICORR, INT2(10))) CALL SBIT ( SUPSTAT, &
     &                                                 PION__SPS, INT2(1) )
           ELSE
!
! ----------- PION calibration is not available for any observation of the bas.
!
              CALL SBIT ( SUPSTAT, PION__SPS, INT2(1) )
          END IF
!
! ------- Make certain that the iono correction is good and that the
! ------- matching S-band has non-zero fringe detection.  (Compensates
! ------- for an old CNPLT bug (now fixed.))
!
          IF ( KBIT( ICORR, INT2(11)) ) THEN
!
! ------------ Phase ionosphere calibration is bad: matching observation was
! ------------ downweighted
!
               CALL SBIT ( SUPSTAT, PIO1__SPS, INT2(1) )
          END IF
          IF ( KBIT( ICORR, INT2(12)) ) THEN
!
! ------------ Phase ionosphere calibration is severely bad: matching
! ------------ observation didn't have fringes
!
               CALL SBIT ( SUPSTAT, PIO2__SPS, INT2(1) )
          END IF
          IF ( KIONO  .AND.  PHIONS .EQ. 0.0D0 ) THEN
!
! ------------ Phase ionosphere calibration is severely bad: matching
! ------------ observation didn't have fringes
!
               CALL SBIT ( SUPSTAT, PIO3__SPS, INT2(1) )
          END IF
          IF ( .NOT. KIONO  .AND.  KBIT( ICORR, INT2(6)) ) THEN
!
! ------------ Phase ionosphere calibration is severely bad: matching
! ------------ observation didn't have fringes
!
               CALL SBIT ( SUPSTAT, PIO3__SPS, INT2(1) )
          END IF
      END IF
!
! --- Set flag "low SNR code". NB: this will be done only if post-Solve
! --- is used, i.e. the database is in GVF format. Pre-2007 scheme
! --- of handling suppression status flags does not support this feature
!
      IF ( DATYP_INQ ( IDATYP, XBAND__DTP ) .OR. &
     &     DATYP_INQ ( IDATYP, COMB__DTP  )      ) THEN
           IF ( ILEN(ENV_FINAM) > 0 ) THEN
                IF ( SNR_MIN_X > 0.0 .AND. SNR_X < SNR_MIN_X ) THEN
                     CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(1) )
                   ELSE 
                     CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(0) )
                END IF 
           END IF 
      END IF 
!
      IF ( DATYP_INQ ( IDATYP, SBAND__DTP ) .OR. &
     &     DATYP_INQ ( IDATYP, COMB__DTP  )      ) THEN
           IF ( ILEN(ENV_FINAM) > 0 ) THEN
                IF ( SNR_MIN_S > 0.0  .AND.  SNR_S < SNR_MIN_S  .AND.  &
     &               KBIT ( OPP_STATUS, OPP_SET1__BIT  )  ) THEN
                     CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(1) )
                   ELSE 
                     CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(0) )
                END IF 
           END IF 
      END IF 
      IF ( DATYP_INQ ( IDATYP, FUSED__DTP ) ) THEN
           IF ( ( SNR_MIN_X > 0.0  .AND.  SNR_X < SNR_MIN_X ) .AND. &
     &          ( SNR_MIN_S > 0.0  .AND.  SNR_S < SNR_MIN_S )       ) THEN
                 CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(1) )
              ELSE               
                 CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(0) )
           END IF
      END IF 
!
! --- Set suppression status bit. Setting this bit on indicates on
! --- completion of setting suppression status
!
      CALL SBIT ( SUPSTAT, SET1__SPS, INT2(1) )
!
! --- Setting usage status bits
!
      CALL SUPUSE_SET ( SUPSTAT )
!
      RETURN
      END  !#!  SUPSTAT_SET  #!#
