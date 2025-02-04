      SUBROUTINE GAMB_PUT ( VER_GAMB, OBS, GAMB, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  GAMB_PUT updates oborg area of scratch files for group  *
! *   delay ambiguity resolution, finding outliers and (possible)        *
! *   ionosphere calibration update made by GAMB and meteo data for      *
! *   S-band which migrated from X-band. If ionosphere calibration       *
! *   calculated then 22 parameters from X band migrated to S-band and   *
! *   vice versus.                                                       *
! *                                                                      *
! *     If observation had good quality GAMB and it had matching         *
! *   observation of good quality, but GAMB rejected this observation as *
! *   a bad one for at least one band then this observation will be      *
! *   marked in UACSUP variable as "suppressed, despite it looks good"   *
! *                                                                      *
! *     CAMB_PUT makes update of S-band group delay observables and      *
! *   their group delay ambiguities as well as ionosphere calibration    *
! *   in the case when the X-band database was read which contained all  *
! *   necessary information about S-band and user set two-band mode of   *
! *   group delay ambiguity resolution.                                  *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  VER_GAMB ( CHARACTER ) -- String which contain name of GAMB routine *
! *                            and date of the current version.          *
! *       OBS ( RECORD    ) -- Data structure which contains             *
! *                            band-independent information: time of     *
! *                            observation, baseline, lists of objects,  *
! *                            status flags etc.                         *
! *      GAMB ( RECORD    ) -- Array of data structures for group delay  *
! *                            ambiguity resolution software, which      *
! *                            contains two elements: the first for      *
! *                            X-band, the second for S-band.            *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                          Input:  switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                          Output: 0 in the case of successful         *
! *                                  completion and non-zero in the case *
! *                                  of error.                           *
! *                                                                      *
! *  ###  13-AUG-97    GAMB_PUT    v3.9  (c)  L. Petrov 08-JUN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'socom.i'
      INCLUDE   'oborg.i'
      INCLUDE   'prfil.i'
      INCLUDE   'precm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'gamb.i'
!
      TYPE ( OBS__STRU ) ::  OBS
      TYPE ( GAMB__STRU ) ::  GAMB(2)
      INTEGER*4  IUER, J1, J2, J3, J4, J5, J6, NUMAMB_NEW, NUMAMB_NEW_S
      INTEGER*4  M_SCP, IVB, IVE, IER, IMATCH(MG_OBS), LMATCH, MIS_MATCH
      PARAMETER  ( M_SCP = 128 )
      REAL*8     FAMB_NEW, FAMB_NEW_S, DOBS_NEW, DOBS_NEW_S
      REAL*8     DOBS_XB, DPH_XB, DERR_XB, DPHER_XB, EFFREQ_XB, PHEFFREQ_XB, &
     &           REFFREQ_XB, ROBS_XB, RERR_XB, FEED_HORN_XB
      REAL*8     DOBS_SB, DPH_SB, DERR_SB, DPHER_SB, EFFREQ_SB, PHEFFREQ_SB, &
     &           REFFREQ_SB, ROBS_SB, RERR_SB
      REAL*8     AMPL_XB, TOTPH_XB, PHAMI8_XB, SNR_XB, DNB_XB, DNBER_XB, &
     &           DOBS_ORIG_XB, DPH_ORIG_XB, FAMB_XB
      REAL*8     AMPL_SB, TOTPH_SB, PHAMI8_SB, SNR_SB, DNB_SB, DNBER_SB, &
     &           DOBS_ORIG_SB, DPH_ORIG_SB, FAMB_SB
      REAL*8     FJD1, FRACTC1, TT_X, TT_S, EPS_TIM
      CHARACTER  FRINGE_FIL_XB*16, FRINGE_FIL_SB*16
      PARAMETER  ( EPS_TIM = 0.001 )
      INTEGER*4  JSTAR_X, JSITE1_X, JSITE2_X, INDG_X, IBX, IEX, &
     &           JSTAR_S, JSITE1_S, JSITE2_S, INDG_S, IBS, IES, IBS_NEW
      INTEGER*4  NPHAM4_XB, NPHAM4_SB
      INTEGER*2  LQUAL_XB, LQUAL_SB, NUMAMB_XB, NUMAMB_SB
      LOGICAL*4  MATCH, BAD_MATCH, FL_USED, FL_NOFS
      LOGICAL*2  KBIT
!
      INTEGER*2    JSITN(4,MAX_ARC_STA),  ITT(MAX_ARC_STA)
      INTEGER*2    JSITI(MAX_ARC_STA),    ITTB(MAX_ARC_BSL)
      INTEGER*2    JCAPPL(MAX_ARC_STA),   JCAVAL(MAX_ARC_STA)
      INTEGER*2    AX_TYPES(MAX_ARC_STA)
      INTEGER*2    JCAFFL(7,MAX_ARC_STA),  NFCAL, NAMSTA, OBCAPL, MCAPL
      REAL*8       ET(2,MAX_ARC_BSL),      AX_OFFS(MAX_ARC_STA)
      REAL*8       SE(MAX_ARC_STA),        SS(MAX_ARC_SRC)
      REAL*8       BARO_CALS(MAX_ARC_STA), BARO_HEIGHTS(MAX_ARC_STA)
      REAL*8       LATS(MAX_ARC_STA),      HEIGHTS(MAX_ARC_STA)
      CHARACTER    FCAL_NAMES(112)*8 ! why 112?
!
      CHARACTER    VER_GAMB*(*)
      LOGICAL*4    GOOD_OBS, BAD_OBS
      INTEGER*2    INT2_ARG
      INTEGER*4    INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4, EXTERNAL :: SUPR_INQ, META_SUPR_INQ
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( OBS%IT .GT. 1 ) THEN
!
! -------- Writing status message
!
           IF ( OBS%IDB_X .GT. 0  .AND.   OBS%IDB_S .GT. 0 ) THEN
                WRITE (  6, FMT='(A)' ) ' &&&  Oborg area of databases '// &
     &                   GAMB(1)%DBASE//' '//GAMB(2)%DBASE// &
     &                   'is being written'
              ELSE
                WRITE (  6, FMT='(A)' ) ' &&&  Oborg area of database '// &
     &                   GAMB(1)%DBASE//' is being written'
           END IF
      END IF
!
      IBS = -1
      IF ( OBS%IDB_X .GT. 0  .AND.   OBS%IDB_S .GT. 0 ) THEN
           IBX = OBS%NB_X
           IEX = OBS%NE_X
           IBS = OBS%NB_S
           IBS_NEW = OBS%NB_S
        ELSE IF ( OBS%IDB_X .GT. 0  .AND.   OBS%IDB_S .LE. 0 ) THEN
           IBX = OBS%NB_X
           IEX = OBS%NE_X
        ELSE IF ( OBS%IDB_X .LE. 0  .AND.   OBS%IDB_S .GT. 0 ) THEN
           IBX = OBS%NB_S
           IEX = OBS%NE_S
      END IF
!
! --- It is esential to update NAMFIL before since ionosphere status is
! --- used to determine whether the observation is good or bad
!
      IF (   OBS%IDB_X .GT. 0   .AND.  OBS%IDB_S .GT. 0  .AND. &
     &     ( GAMB(1)%STATUS_ION .EQ. GAMB__IONO_1 .OR. &
     &       GAMB(1)%STATUS_ION .EQ. GAMB__IONO_2      ) .AND. &
     &     ( GAMB(2)%STATUS_ION .EQ. GAMB__IONO_1 .OR. &
     &       GAMB(2)%STATUS_ION .EQ. GAMB__IONO_2      )       ) THEN
!
! ---------- We will update NAMFIL. Set there flags of ionosphere calibration
! ---------- and meteo data for S-band which migrated form X-band.
!
! ---------- Calculation boundaries for GAMB-version date
!
             IVE = I_LEN(VER_GAMB)
             IVB = IVE-7
             IF ( IVB .LE. 0 ) IVB=1
             IVE=IVB+7
!
! ---------- Updating NAMFIL cards
!
             CALL ERR_PASS ( IUER, IER )
             CALL NAMFILE_UPDATE ( INT2(OBS%IDB_X), INT2(OBS%IDB_S), &
     &            VER_GAMB(IVB:IVE), .TRUE., IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 7741, IUER, 'GAMB_PUT', 'Error during '// &
     &                'updating NAMFIL card occurred' )
                  RETURN
             END IF
      END IF
!
! --- Read station names, status array, eccentricity data, monument
! --- names, and set up a correspondence table between the stations
! --- in NAMFIL (JSIT's) and those in PARFIL (ISIT's).
!
      CALL NCORT ( JSITN, JSITI, JCAPPL, NUMSTA, ITT, INT2(1), &
     &             IDATYP, ITTB, ET, SE, SS, OBCAPL, MCAPL, JCAVAL, &
     &             LATS, HEIGHTS, AX_TYPES, AX_OFFS, BARO_CALS, &
     &             BARO_HEIGHTS, JCAFFL, FCAL_NAMES, NFCAL, NAMSTA, CALCV )
!
      LMATCH = 0
      DO 410 J1=IBX,IEX
!
! ------ Reading the J1-th record of the database X (It may be S-band if
! ------ only S-band database is analyzied)
!
         CALL USE_OBSFIL  ( IOBSFIL, J1, 'R' )
!
! ------ Setting suppression status
!
         IF ( SUPMET .NE. SUPMET__META ) THEN
              CALL SUPSTAT_SET ( IUNW, IUNWP, LQUAL, LQUALXS, &
     &                           ICORR, GIONSG, PHIONS, IWVBIT1, ISITE, &
     &                           JSITI, ITT, ISTAR, ELEV, KIONO, SNR, SNR_S, &
     &                           SUPSTAT, UACSUP )
         END IF
!
         IF ( J1 .EQ. IBX ) THEN
!
! ----------- Storing time and date of the first observation at the
! ----------- band X
!
              FJD1    = FJD
              FRACTC1 = FRACTC
         END IF
!
! ------ Now look: was this observation in GAMB data structure?
!
         INDG_X = 0
         DO 420 J2=1,OBS%NOBS
            IF ( GAMB(1)%OBS_IND(J2) .EQ. INT2(J1) ) THEN
                 INDG_X = J2
            END IF
 420     CONTINUE
         IF ( INDG_X .GT. 0 ) THEN
!
! ----------- Yes! This observation was in the GAMB data structure. We'll
! ----------- update ambiguities and group delay observable for the X-band
!
! ----------- Extraction of new group ambiguity spacing and new
! ----------- number of ambiguities
!
              FAMB_NEW   = GAMB(1)%GAMBC
              IF ( SUPMET == SUPMET__META ) THEN
                   FL_USED = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, &
     &                                       USED__SPS )
                 ELSE
                   FL_USED = SUPR_INQ ( SUPSTAT, UACSUP, USED__SPS )
              END IF
              IF ( FL_USED .AND. &
     &             DABS( GAMB(1)%JMP(INDG_X)/GAMB(1)%GAMBC ) .GT. 32766 ) THEN
!
! ---------------- Observation with very large ambiguity was set "in solution"
!
                   WRITE ( 6, * ) ' GAMB(1)%JMP(INDG_X) = ',GAMB(1)%JMP(INDG_X)
                   WRITE ( 6, * ) ' GAMB(1)%GAMBC       = ',GAMB(1)%GAMBC
                   WRITE ( 6, * ) ' NUMAMB = ',GAMB(1)%JMP(INDG_X)/GAMB(1)%GAMBC
                   IER = -1
                   CALL ERR_LOG ( 7742, IER, 'GAMB_PUT', 'Error of internal '// &
     &                 'control: attempt to write too huge (more than 32767) '// &
     &                 'group delay ambiguity' )
                   IF ( SUPMET == SUPMET__META ) THEN
                        USER_SUP = IBSET ( USER_SUP, INT4(GRPRAT__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4(GRPONL__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4(G_GXS__DTP)  )
                        USER_SUP = IBSET ( USER_SUP, INT4(PX_GXS__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4(PS_GXS__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4(PX_GX__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4(PX_GS__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4(PS_GX__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4(PS_GS__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4(GS__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4(GX__DTP) )
                      ELSE
                        CALL SUPR_OBS ( GRPONL__DTP, SUPSTAT, UACSUP )
                   END IF
!@                   CALL HIT_CONT ( %VAL(0), %VAL(0) )
                 ELSE IF ( DABS( GAMB(1)%JMP(INDG_X)/GAMB(1)%GAMBC ) &
     &                     .GT. 32766 ) THEN
!
! ---------------- Observation with very large ambiguity was marked as bad.
! ---------------- This trick prevents restoring observation with such a huge
! ---------------- jump and prevent integer overflow
!
                   NUMAMB_NEW = 0
                 ELSE
!
! ---------------- Normal case
!
                   NUMAMB_NEW = NINT ( GAMB(1)%JMP(INDG_X)/GAMB(1)%GAMBC )
              END IF
!
! ----------- Correction "observed" group delay
!
              DOBS_NEW = DOBS - NUMAMB*FAMB*1.D6 &
     &                        + NUMAMB_NEW*FAMB_NEW*1.D6
           ELSE
              FAMB_NEW   = FAMB
              NUMAMB_NEW = NUMAMB
              DOBS_NEW   = DOBS
         END IF
!
         MATCH = .FALSE.
         IF ( SUPMET == SUPMET__META ) THEN
              FL_NOFS = BTEST ( AUTO_SUP, INT4(NOFS__SPS) )
            ELSE
              FL_NOFS = SUPR_INQ ( SUPSTAT, UACSUP, NOFS__SPS )
         END IF
         IF ( OBS%IDB_X .GT. 0  .AND.   OBS%IDB_S .GT. 0 ) THEN
!
! ----------- Case when two databases were read
!
! ----------- Save 22 parameters for X-band which will migrate to S-band
! ----------- oborg area
!
              DOBS_XB      = DOBS_NEW
              DPH_XB       = DPH
              DERR_XB      = DERR
              DPHER_XB     = DPHER
              EFFREQ_XB    = EFFREQ
              PHEFFREQ_XB  = PHEFFREQ
              REFFREQ_XB   = REFFREQ
              ROBS_XB      = ROBS
              RERR_XB      = RERR
              LQUAL_XB     = LQUAL
!
              AMPL_XB      = AMPL
              TOTPH_XB     = TOTPH
              PHAMI8_XB    = PHAMI8
              SNR_XB       = SNR
              DNB_XB       = DNB
              DNBER_XB     = DNBER
              DOBS_ORIG_XB = DOBS_ORIG
              DPH_ORIG_XB  = DPH_ORIG
              FAMB_XB      = FAMB_NEW
              NUMAMB_XB    = NUMAMB_NEW
              NPHAM4_XB    = NPHAM4
              FEED_HORN_XB = FEED_HORN
              FRINGE_FIL_XB = FRINGE_X_FINAM
!
! ----------- Calculation of the identifier of the observation:
! ----------- 1) Object to be observed
! ----------- 2) stations participated
! ----------- 3) time of the obseration
!
              JSTAR_X  = INT4(ISTAR)
              JSITE1_X = INT4(ISITE(1))
              JSITE2_X = INT4(ISITE(2))
              TT_X = ( (FJD - FJD1) + (FRACTC - FRACTC1) )*86400.D0
!
              IES = IBS + M_SCP-1
              IF ( IES .GT. OBS%NE_S ) IES = OBS%NE_S
!
              DO 430 J3=IBS,IES
!
! -------------- Reading the J3-th record of the database S
!
                 CALL USE_OBSFIL ( IOBSFIL, J3, 'R' )
!
! -------------- Setting suppression status
!
                 IF ( SUPMET .NE. SUPMET__META ) THEN
                      CALL SUPSTAT_SET ( IUNW, IUNWP, LQUAL, LQUALXS, &
     &                                   ICORR, GIONSG, PHIONS, IWVBIT1, &
     &                                   ISITE, JSITI, ITT, ISTAR, ELEV, &
     &                                   KIONO, SUPSTAT, UACSUP )
                 END IF
!
                 JSTAR_S  = INT4(ISTAR)
                 JSITE1_S = INT4(ISITE(1))
                 JSITE2_S = INT4(ISITE(2))
                 TT_S = ( (FJD - FJD1) + (FRACTC - FRACTC1) )*86400.D0
!
                 IF ( TT_S - TT_X  .LT. -EPS_TIM ) THEN
!
! ------------------- Observation read for the band S appeared before the
! ------------------- observation read at the band X
!
                      IBS_NEW = J3+1
                 END IF
                 IF ( TT_S - TT_X  .GT.  EPS_TIM ) THEN
!
! ------------------- Observation read for the band S appeared after the
! ------------------- observation read at the band X
!
                      GOTO 830 ! There no sense to seek further since both
!                              ! databases assumed to be sorted by time
                 END IF
!
                 IF ( DABS (TT_X - TT_S) .LT. EPS_TIM  .AND. &
     &                JSTAR_X  .EQ. JSTAR_S            .AND. &
     &                JSITE1_X .EQ. JSITE1_S           .AND. &
     &                JSITE2_X .EQ. JSITE2_S                 ) THEN
!
                      MATCH  = .TRUE.
                      LMATCH = LMATCH+1
                      IMATCH(LMATCH) = J3
!
! ------------------- Matching observation was found. Store 22 observables
! ------------------- which will be migrate from S-band oborg to X-band area
!
                      DOBS_SB      = DOBS
                      DPH_SB       = DPH
                      DERR_SB      = DERR
                      DPHER_SB     = DPHER
                      EFFREQ_SB    = EFFREQ
                      PHEFFREQ_SB  = PHEFFREQ
                      REFFREQ_SB   = REFFREQ
                      ROBS_SB      = ROBS
                      RERR_SB      = RERR
                      LQUAL_SB     = LQUAL
!
                      AMPL_SB      = AMPL
                      TOTPH_SB     = TOTPH
                      PHAMI8_SB    = PHAMI8
                      SNR_SB       = SNR
                      DNB_SB       = DNB
                      DNBER_SB     = DNBER
                      DOBS_ORIG_SB = DOBS_ORIG
                      DPH_ORIG_SB  = DPH_ORIG
                      FAMB_SB      = FAMB
                      NUMAMB_SB    = NUMAMB
                      NPHAM4_SB    = NPHAM4
                      FRINGE_FIL_SB = FRINGE_X_FINAM
!
! ------------------- Now look: was this observation in GAMB data structure?
!
                      INDG_S = 0
                      DO 440 J4=1,OBS%NOBS
                         IF ( GAMB(2)%OBS_IND(J4) .EQ. INT2(J3) ) THEN
                              INDG_S = J4
                         END IF
  440                 CONTINUE
!
                      IF ( INDG_S .GT. 0 ) THEN
!
! ---------------------- Yes! This observation was in the GAMB data
! ---------------------- structure. Well. We'll update ambiguities and
! ---------------------- group delay observable for the S-band
!
! ---------------------- Extraction of new group ambiguity spacing and new
! ---------------------- number of ambiguities
!
                         IF ( SUPMET == SUPMET__META ) THEN
                              FL_USED = META_SUPR_INQ ( AUTO_SUP, USER_SUP, &
     &                                                  USER_REC, USED__SPS )
                           ELSE
                              FL_USED = SUPR_INQ ( SUPSTAT, UACSUP, USED__SPS )
                         END IF
                         IF ( FL_USED .AND. &
     &                        DABS( GAMB(2)%JMP(INDG_S)/GAMB(2)%GAMBC ) .GT. &
     &                        32766 ) THEN
!
                              WRITE ( 6, * ) ' GAMB(2)%JMP(INDG_S) = ', &
     &                                 GAMB(2)%JMP(INDG_S)
                              WRITE ( 6, * ) ' GAMB(2)%GAMBC       = ',GAMB(2)%GAMBC
                              WRITE ( 6, * ) ' NUMAMB = ',GAMB(2)%JMP(INDG_S)/ &
     &                                            GAMB(2)%GAMBC
                              CALL ERR_LOG ( 7743, IUER, 'GAMB_PUT', &
     &                            'Error of internal control: attempt to '// &
     &                            'write too huge (more than 32767) '// &
     &                            'group delay ambiguity for S-band' )
                             RETURN
                           ELSE IF ( DABS( GAMB(2)%JMP(INDG_S)/GAMB(2)%GAMBC ) &
     &                               .GT. 32766 ) THEN
!
! -------------------------- Observation with very large ambiguity was marked
! -------------------------- as bad. This trick prevents restoring observation
! -------------------------- with such a huge jump and prevent integer overflow
!
                             NUMAMB_NEW_S = 0
                           ELSE
!
! -------------------------- Normal case
!
                             NUMAMB_NEW_S = &
     &                              NINT ( GAMB(2)%JMP(INDG_S)/GAMB(2)%GAMBC)
                         END IF
!
                         FAMB_NEW_S  = GAMB(2)%GAMBC
!
! ---------------------- Correction "observed" group delay for the S-band
!
                         DOBS_NEW_S = DOBS_SB - NUMAMB_SB*FAMB_SB*1.D6 &
     &                                        + NUMAMB_NEW_S*FAMB_NEW_S*1.D6
!
! ---------------------- Putting new group ambiguity spacing and new number
! ---------------------- of ambiguities at the place of old ones
!
                         DOBS   = DOBS_NEW_S
                         FAMB   = FAMB_NEW_S
                         NUMAMB = NUMAMB_NEW_S
!
! ---------------------- Saving updated ambiguities and observables for
! ---------------------- the S-band
!
                         DOBS_SB      = DOBS_NEW_S
                         FAMB_SB      = FAMB_NEW_S
                         NUMAMB_SB    = NUMAMB_NEW_S
!
                         IF ( GAMB(2)%STATUS_ION .EQ. GAMB__IONO_1  .OR. &
     &                        GAMB(2)%STATUS_ION .EQ. GAMB__IONO_2       ) THEN
!
! ------------------------- Calculation ionosophere calibration for the S-band
!
                            GION(1)=-(DOBS_XB       - DOBS_SB   )*EFFREQ_XB**2/ &
     &                               (EFFREQ_XB**2  - EFFREQ**2 )
                            GION(2)=-(ROBS_XB       - ROBS_SB   )*REFFREQ_XB**2/ &
     &                               (REFFREQ_XB**2 - REFFREQ**2)
!
! ------------------------- Calculation of formal errors of determination of
! ------------------------- group ionosphere calibration
!
                            GIONSG(1) = DSQRT ( DERR_XB**2    + DERR_SB**2   )* &
     &                          EFFREQ_XB**2/ ( EFFREQ_XB**2  - EFFREQ_SB**2 )
                            GIONSG(2) = DSQRT ( RERR_XB**2    + RERR_SB**2   )* &
     &                          REFFREQ_XB**2/( REFFREQ_XB**2 - REFFREQ_SB**2)
!
! ------------------------- Setting bits of ionosphere flags
!
                            CALL SBIT ( ICORR,  INT2(2), INT2(0) )
                            CALL SBIT ( ICORR,  INT2(4), INT2(1) )
                            CALL SBIT ( IONFLG, INT2(1), INT2(1) )
                            CALL SBIT ( IONFLG, INT2(2), INT2(1) )
                         END IF
!
                         IF ( .NOT. GAMB(2)%USE(INDG_S) ) THEN
!
! ---------------------------- Observation has been marked as outlier at
! ---------------------------- S-band
!
                               CALL SUPR_OBS ( GRPONL__DTP, SUPSTAT, UACSUP )
                         END IF
!
                         IF ( .NOT. GAMB(1)%USE(INDG_X) ) THEN
!
! ---------------------------- Observation has been marked as outlier at
! ---------------------------- X-band
!
                               USER_SUP = IBSET ( USER_SUP, INT4(GRPRAT__DTP) )
                               USER_SUP = IBSET ( USER_SUP, INT4(GRPONL__DTP) )
                               USER_SUP = IBSET ( USER_SUP, INT4(G_GXS__DTP)  )
                               USER_SUP = IBSET ( USER_SUP, INT4(PX_GXS__DTP) )
                               USER_SUP = IBSET ( USER_SUP, INT4(PS_GXS__DTP) )
                               USER_SUP = IBSET ( USER_SUP, INT4(PX_GX__DTP) )
                               USER_SUP = IBSET ( USER_SUP, INT4(PS_GX__DTP) )
                               CALL SUPR_OBS ( GRPONL__DTP, SUPSTAT, UACSUP )
                          END IF
!                         IF ( GAMB(1).USE(INDG_X)  .AND.
!     #                        .NOT. GAMB_F_PREUSE         ) THEN
!C
!C ---------------------------- Observation has been marked as GOOD at
!C ---------------------------- S-band
!C
!                               CALL RECV_OBS ( GRPONL__DTP, SUPSTAT, UACSUP )
!                         END IF
                      END IF  ! indg_s
!
! ------------------- Copying 22 observables of X-band which migrate
! ------------------- to S-band oborg area
!
                      DOBSXS      = DOBS_XB
                      DPHXS       = DPH_XB
                      DERRXS      = DERR_XB
                      DPHERXS     = DPHER_XB
                      EFFREQ_XS   = EFFREQ_XB
                      PHEFFREQ_XS = PHEFFREQ_XB
                      REFFREQ_XS  = REFFREQ_XB
                      ROBSXS      = ROBS_XB
                      RERRXS      = RERR_XB
                      LQUALXS     = LQUAL_XB
!
                      AMPL_S      = AMPL_XB
                      TOTPH_S     = TOTPH_XB
                      PHAMI8_S    = PHAMI8_XB
                      SNR_S       = SNR_XB
                      DNB_S       = DNB_XB
                      DNBER_S     = DNBER_XB
                      DOBS_ORIG_S = DOBS_ORIG_XB
                      DPH_ORIG_S  = DPH_ORIG_XB
                      FAMB_S      = FAMB_XB
                      NUMAMB_S    = NUMAMB_XB
                      NPHAM4_S    = NPHAM4_XB
                      FEED_HORN   = FEED_HORN_XB
                      FRINGE_S_FINAM = FRINGE_FIL_XB
!
                      IF ( INDG_S .GT. 0 ) THEN
!
! ------------------------ Resetting IUNW, IUNWP flags for backward
! ------------------------ compatibility with PRE-APR98 versions of SOLVE
!
                           IF ( SUPMET .NE. SUPMET__META ) THEN
                                CALL SUPSTAT_UNW ( SUPSTAT, UACSUP, IUNW, IUNWP )
                           END IF
!
! ------------------------ Lift stupid ICORR flag!
!
                           CALL SBIT ( ICORR, INT2(1),  INT2(0) )
                           CALL SBIT ( ICORR, INT2(5),  INT2(0) )
                           CALL SBIT ( ICORR, INT2(7),  INT2(0) )
                           CALL SBIT ( ICORR, INT2(10), INT2(0) )
!
! ------------------------ And at last writing back to oborg area updated record
! ------------------------ with S-band stuff
!
                           CALL USE_OBSFIL  ( IOBSFIL, J3, 'W' )
                      END IF
!
                      GOTO 830
                 ENDIF  ! mathing observation
 430          CONTINUE
 830          CONTINUE
              IBS = IBS_NEW ! Update of index of the first record for search
!
! ----------- Reread the J1-th record of the database X
!
              CALL USE_OBSFIL ( IOBSFIL, J1, 'R' )
!
! ----------- Setting suppression status
!
              IF ( SUPMET .NE. SUPMET__META ) THEN
                   CALL SUPSTAT_SET ( IUNW, IUNWP, LQUAL, LQUALXS, &
     &                                ICORR, GIONSG, PHIONS, IWVBIT1, ISITE, &
     &                                JSITI, ITT, ISTAR, ELEV, KIONO, SUPSTAT, &
     &                                UACSUP )
              END IF
           ELSE IF ( OBS%IDB_X .GT. 0  .AND. &   ! X-band oborg are was read
     &               OBS%IDB_S .LE. 0  .AND. &   ! but S-band -- not
     &               GAMB_F_S_BAND     .AND. &   ! X-band data were analyzed
     &               GAMB_F_X_BAND     .AND. &   ! S-band data were analyzed
     &               INDG_X .GT. 0     .AND. &   ! the observation was used
!                                             ! and it has match for S-band
     &               .NOT. FL_NOFS  .AND. &   !
     &               KBIT ( OPP_STATUS, OPP_SET1__BIT ) .AND. &  ! database
     &               KBIT ( OPP_STATUS, OPP_SET2__BIT ) &        ! contains info
     &             ) THEN                                     ! about S-band
!
! ------------ Spescial section for the case when only X-band database was
! ------------ loaded to oborg area, but it contained all necessary information
! ------------ about S-band and user wish to resolve group delay ambiguities
! ------------ for the S-band also.
!
! ------------ Extraction of new group ambiguity spacing and new
! ------------ number of ambiguities
!
               FAMB_NEW_S   = GAMB(2)%GAMBC
               NUMAMB_NEW_S = NINT ( GAMB(2)%JMP(INDG_X)/GAMB(2)%GAMBC )
!
! ------------ Correction "observed" group delay for the S-band
!
               DOBS_NEW_S = DOBSXS - NUMAMB_S*FAMB_S*1.D6 &
     &                             + NUMAMB_NEW_S*FAMB_NEW_S*1.D6
!
! ------------ Putting new group delay observable, new constant of group delay
! ------------ ambiguity spacing and new number of ambiguities at the place
! ------------ of old ones
!
               NUMAMB_S = NUMAMB_NEW_S
               FAMB_S   = FAMB_NEW_S
               DOBSXS   = DOBS_NEW_S
!
! ------------ Saving updated ambiguities and observables for the S-band
!
               DOBS_SB    = DOBS_NEW_S
               FAMB_SB    = FAMB_NEW_S
               NUMAMB_SB  = NUMAMB_NEW_S
!
! ------------ Copying ionpshere reference frequencies and other parameters
! ------------ (for ionosphere calibration below)
!
               EFFREQ_SB  = EFFREQ_XS
               REFFREQ_SB = REFFREQ_XS
               ROBS_SB    = ROBSXS
               DERR_SB    = DERRXS
               RERR_SB    = RERRXS
!
               EFFREQ_XB  = EFFREQ
               REFFREQ_XB = REFFREQ
               DOBS_XB    = DOBS_NEW
               ROBS_XB    = ROBS
               DERR_XB    = DERR
               RERR_XB    = RERR
         END IF ! special section for looking S-band data
!
         IF ( INDG_X .GT. 0 ) THEN
!
! ----------- Writing down to X-band oborg area new group delay ambiguity
! ----------- spacing, number of ambiguities and corrected group delay
! ----------- observable
!
              FAMB   = FAMB_NEW
              NUMAMB = NUMAMB_NEW
              DOBS   = DOBS_NEW
         END IF
!
         IF ( MATCH ) THEN
!
! ----------- Putting S-band stuff at its place in X-band oborg area
!
              DOBSXS      = DOBS_SB
              DPHXS       = DPH_SB
              DERRXS      = DERR_SB
              DPHERXS     = DPHER_SB
              EFFREQ_XS   = EFFREQ_SB
              PHEFFREQ_XS = PHEFFREQ_SB
              REFFREQ_XS  = REFFREQ_SB
              ROBSXS      = ROBS_SB
              RERRXS      = RERR_SB
              LQUALXS     = LQUAL_SB
!
              AMPL_S      = AMPL_SB
              TOTPH_S     = TOTPH_SB
              PHAMI8_S    = PHAMI8_SB
              SNR_S       = SNR_SB
              DNB_S       = DNB_SB
              DNBER_S     = DNBER_SB
              DOBS_ORIG_S = DOBS_ORIG_SB
              DPH_ORIG_S  = DPH_ORIG_SB
              FAMB_S      = FAMB_SB
              NPHAM4_S    = NPHAM4_SB
              NUMAMB_S    = NUMAMB_SB
              FRINGE_S_FINAM = FRINGE_FIL_SB
            ELSE
              IF ( OBS%IDB_S .GT. 0 ) THEN
!
! ---------------- To indicate that there is no S-band data available for
! ---------------- this observation
!
                   LQUALXS_CHR = ' 0'
!
! ---------------- And set other fiels to zero
!
                   DOBSXS      = 0
                   DPHXS       = 0
                   DERRXS      = 0
                   DPHERXS     = 0
                   EFFREQ_XS   = 0
                   PHEFFREQ_XS = 0
                   REFFREQ_XS  = 0
                   ROBSXS      = 0
                   RERRXS      = 0
!
                   AMPL_S      = 0
                   TOTPH_S     = 0
                   PHAMI8_S    = 0
                   SNR_S       = 0
                   DNB_S       = 0
                   DNBER_S     = 0
                   DOBS_ORIG_S = 0
                   DPH_ORIG_S  = 0
                   FAMB_S      = 0
                   NPHAM4_S    = 0
                   NUMAMB_S    = 0
                   CALL CLRCH ( FRINGE_S_FINAM )
              END IF
          END IF
!
! ------- Manipulation with quality codes and group delay ionosphere
! ------- correction
!
! ------- Look: do we need gain downweight code?
!
          IF ( INDG_X .GT. 0 ) THEN
               IF ( .NOT. GAMB(1)%USE(INDG_X) ) THEN
!
! ----------------- Observation has been marked as outlier at X-band
!
                    USER_SUP = IBSET ( USER_SUP, INT4(GRPRAT__DTP) )
                    USER_SUP = IBSET ( USER_SUP, INT4(GRPONL__DTP) )
                    USER_SUP = IBSET ( USER_SUP, INT4(G_GXS__DTP)  )
                    USER_SUP = IBSET ( USER_SUP, INT4(PX_GXS__DTP) )
                    USER_SUP = IBSET ( USER_SUP, INT4(PS_GXS__DTP) )
                    USER_SUP = IBSET ( USER_SUP, INT4(PX_GX__DTP) )
                    USER_SUP = IBSET ( USER_SUP, INT4(PS_GX__DTP) )
                    USER_SUP = IBSET ( USER_SUP, INT4(GX__DTP) )
                    CALL SUPR_OBS ( GRPONL__DTP, SUPSTAT, UACSUP )
               END IF
!               IF ( GAMB(1).USE(INDG_X) .AND.  .NOT. GAMB_F_PREUSE ) THEN
!C
!C ----------------- Observation has been marked as good at X-band
!C
!                    CALL RECV_OBS ( GRPONL__DTP, SUPSTAT, UACSUP )
!               END IF
          END IF
!
          IF ( INDG_S .GT. 0  .AND.  MATCH ) THEN
               IF ( .NOT. GAMB(2)%USE(INDG_S) ) THEN
!
! ----------------- Observation has been marked as outlier at S-band
!
                    USER_SUP = IBSET ( USER_SUP, INT4(GRPRAT__DTP) )
                    USER_SUP = IBSET ( USER_SUP, INT4(GRPONL__DTP) )
                    USER_SUP = IBSET ( USER_SUP, INT4(G_GXS__DTP)  )
                    USER_SUP = IBSET ( USER_SUP, INT4(PX_GXS__DTP) )
                    USER_SUP = IBSET ( USER_SUP, INT4(PS_GXS__DTP) )
                    USER_SUP = IBSET ( USER_SUP, INT4(PS_GX__DTP) )
                    USER_SUP = IBSET ( USER_SUP, INT4(PS_GS__DTP) )
                    USER_SUP = IBSET ( USER_SUP, INT4(GS__DTP) )
                    CALL SUPR_OBS ( GRPONL__DTP, SUPSTAT, UACSUP )
               END IF
          END IF
!
! ------- We make update of the ionsphere calibration when it is available
! ------- in the case either we had both band in oborg area or we had the
! ------- S-band database with information about S-band
!
          IF ( SUPMET == SUPMET__META ) THEN
               FL_NOFS = BTEST ( AUTO_SUP, INT4(NOFS__SPS) )
             ELSE
               FL_NOFS = SUPR_INQ ( SUPSTAT, UACSUP, NOFS__SPS )
          END IF
          IF ( ( GAMB(1)%STATUS_ION .EQ. GAMB__IONO_1 .OR. &
     &           GAMB(1)%STATUS_ION .EQ. GAMB__IONO_2     ) .AND. &
     &         ( MATCH .OR. &
     &           ( GAMB_F_S_BAND                      .AND. &
     &             GAMB_F_X_BAND                      .AND. &
     &             INDG_X .GT. 0                      .AND. &
     &             KBIT ( OPP_STATUS, OPP_SET1__BIT ) .AND. &
     &             KBIT ( OPP_STATUS, OPP_SET2__BIT )       )      )  ) THEN
!
             BAD_MATCH = .TRUE.
             IF ( .NOT. BAD_OBS ( LQUALXS_CHR ) ) BAD_MATCH = .FALSE.
!
             IF ( .NOT. FL_NOFS    .AND. &
     &            .NOT. BAD_MATCH        ) THEN
!
! ------------ There is matching observable fot the S-band
!
               GION(1) = - ( DOBS_XB       - DOBS_SB       )*EFFREQ_SB**2/ &
     &                     ( EFFREQ_XB**2  - EFFREQ_SB**2  )
               GION(2) = - ( ROBS_XB       - ROBS_SB       )*REFFREQ_SB**2/ &
     &                     ( REFFREQ_XB**2 - REFFREQ_SB**2 )
!
! ------------ Calculation of formal errors of determination group
! ------------ ionosphere calibration
!
               GIONSG(1) =DSQRT ( DERR_XB**2    + DERR_SB**2   )*EFFREQ_SB**2/ &
     &                          ( EFFREQ_XB**2  - EFFREQ_SB**2 )
               GIONSG(2) =DSQRT ( RERR_XB**2    + RERR_SB**2   )*REFFREQ_SB**2/ &
     &                          ( REFFREQ_XB**2 - REFFREQ_SB**2)
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!          type *,' j1=',j1,' iunw = ',iunw,                          ! %%%%%%%%
!     #           ' dobs_xb = ',dobs_xb,' dobs_sb = ',dobs_sb,        ! %%%%%%%%
!     #           ' gion(1) = ',gion(1),' gion(2) = ',gion(2),        ! %%%%%%%%
!     #           ' gionsg(1) = ',gionsg(1),' gionsg(2) = ',gionsg(2) ! %%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------------ Setting bits of ionosphere flags
!
               CALL SBIT ( ICORR,  INT2(2), INT2(0) )
               CALL SBIT ( ICORR,  INT2(8), INT2(0) )
               CALL SBIT ( ICORR,  INT2(4), INT2(1) )
               CALL SBIT ( IONFLG, INT2(1), INT2(1) )
               CALL SBIT ( IONFLG, INT2(2), INT2(1) )
             ELSE IF ( FL_NOFS  .OR. BAD_MATCH ) THEN
!
! ------------ No match. We set dummy values to avoid problems with CNPLT
!
               GION(1)   = 0.0D0
               GION(2)   = 0.0D0
               GIONSG(1) = 1.D-8  ! ( set 10 nsec formal error )
               GIONSG(2) = 1.D-12 ! ( set 1 psec/sec formal error )
               CALL SBIT ( ICORR,  INT2(2), INT2(1) )
               CALL SBIT ( ICORR,  INT2(4), INT2(0) )
               CALL SBIT ( ICORR,  INT2(8), INT2(1) )
            END IF  ! work with matched observations
!
            IF ( .NOT. GOOD_OBS ( LQUALXS_CHR ) ) THEN
!
! -------------- Matching observation has too bad quality code. We calculated
! -------------- group delay ionopshere correction for such an observation
! -------------- earlier. Now we downweight it and set flag "matching
! -------------- observation has bad quality code"
!
!@                 CALL SBIT ( ICORR,  INT2(3), INT2(1) )
!@                 CALL SBIT ( ICORR,  INT2(9), INT2(1) )
                 IUNW = 1
            END IF
          END IF
!
! ------- Setting flag: no matching observation detected.
!
          IF ( .NOT. MATCH                                .AND. &
     &       ( OBS%IDB_X .GT. 0  .AND.  OBS%IDB_S .GT. 0 )     ) THEN
!
! ------------ No match. We set dummy values not to irritate CNPLT
!
               GION(1)   = 0.0D0
               GION(2)   = 0.0D0
               GIONSG(1) = 1.D-8  ! ( set 10 nsec formal error )
               GIONSG(2) = 1.D-12 ! ( set 1 psec/sec formal error )
               CALL SBIT ( ICORR,  INT2(2), INT2(1) )
               CALL SBIT ( ICORR,  INT2(4), INT2(0) )
               CALL SBIT ( ICORR,  INT2(8), INT2(1) )
          END IF
!
! ------- Setting suppression status onse more
!
          IF ( SUPMET .NE. SUPMET__META ) THEN
              CALL SUPSTAT_SET ( IUNW, IUNWP, LQUAL, LQUALXS, &
     &                           ICORR, GIONSG, PHIONS, IWVBIT1, ISITE, &
     &                           JSITI, ITT, ISTAR, ELEV, KIONO, SUPSTAT, &
     &                           UACSUP )
          END IF
!
          IF ( INDG_X .GT. 0 ) THEN
!
! ------------ Resetting IUNW, IUNWP flags for backward
! ------------ compatibility with PRE-APR98 versions of SOLVE
!
               IF ( SUPMET .NE. SUPMET__META ) THEN
                    CALL SUPSTAT_UNW ( SUPSTAT, UACSUP, IUNW, IUNWP )
               END IF
!
! ------------ Lift stupid ICORR flag!
!
               CALL SBIT ( ICORR, INT2(1),  INT2(0) )
               CALL SBIT ( ICORR, INT2(5),  INT2(0) )
               CALL SBIT ( ICORR, INT2(7),  INT2(0) )
               CALL SBIT ( ICORR, INT2(10), INT2(0) )
!
! ------------ Write back the J1-th record of the database X
!
               CALL USE_OBSFIL ( IOBSFIL, J1, 'W' )
          END IF
 410  CONTINUE
!
! --- It is possible a situation when some observations at S-band don't have
! --- a counterpart at X-band. We should find all such a cases and mark
! --- these observations
!
      IF ( OBS%IDB_X .GT. 0  .AND.   OBS%IDB_S .GT. 0 ) THEN
           MIS_MATCH = 0
           DO 450 J5=OBS%NB_S,OBS%NE_S
!
! ----------- Sech a counterpart in  the array of matching observations
!
              DO 460 J6=1,LMATCH
                 IF ( J5 .EQ. IMATCH(J6) ) GOTO 450 ! Match has been found
 460          CONTINUE
!
! ----------- J5-th observation at S-band doesn't have a counterpart at X-band
!
              MIS_MATCH = MIS_MATCH + 1
!
! ----------- Read it
!
              CALL USE_OBSFIL ( IOBSFIL, J5, 'R' )
!
! ----------- Set dummy values not to irritate CNPLT
!
              GION(1)   = 0.0D0
              GION(2)   = 0.0D0
!
              GIONSG(1) = 1.D-8  ! ( set 10 nsec formal error )
              GIONSG(2) = 1.D-12 ! ( set 1 psec/sec formal error )
              CALL SBIT ( ICORR,  INT2(2), INT2(1) )
              CALL SBIT ( ICORR,  INT2(4), INT2(0) )
              CALL SBIT ( ICORR,  INT2(8), INT2(1) )
!
! ----------- Lift stupid ICORR flag!
!
              CALL SBIT ( ICORR, INT2(1),  INT2(0) )
              CALL SBIT ( ICORR, INT2(5),  INT2(0) )
              CALL SBIT ( ICORR, INT2(7),  INT2(0) )
              CALL SBIT ( ICORR, INT2(10), INT2(0) )
!
! ----------- Write observation back to oborg area
!
              CALL USE_OBSFIL ( IOBSFIL, J5, 'W' )
 450       CONTINUE
      END IF
!
      IF ( OBS%STATUS_GET_X .EQ. GAMB__GET                .AND. &
     &     OBS%STATUS_GET_S .EQ. GAMB__GET                .AND. &
     &     ( GAMB(1)%STATUS_ION .EQ. GAMB__IONO_1  .OR. &
     &       GAMB(1)%STATUS_ION .EQ. GAMB__IONO_2       ) .AND. &
     &     ( GAMB(2)%STATUS_ION .EQ. GAMB__IONO_1  .OR. &
     &       GAMB(2)%STATUS_ION .EQ. GAMB__IONO_2       )       ) THEN
!
! -------- Setting S-band status bit
!
           CALL SOCOM_EXT()
           CALL SBIT ( OPP_STATUS, OPP_SET1__BIT, INT2(1) )
           CALL SBIT ( OPP_STATUS, OPP_SET2__BIT, INT2(1) )
           CALL USE_COMMON ( 'OWC' )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GAMB_PUT  #!#
