      SUBROUTINE PREPES ( VER_GAMB, OBS, F_BATCH, F_XBAND, F_SBAND, &
     &           GAMB_F_PREUSE, GAMB_F_ION, GAMB_CUTOFF, GAMB_MINOBS, &
     &           GAMB_SPACING_CONST, GAMB_IT, QUALCODE_GOOD_LIM, &
     &           CALX, CALS, GAMB, STATUS_GEN, &
     &           STATUS_X_BAS, STATUS_X_NZ, STATUS_X_CLS, STATUS_X_WHL, &
     &           STATUS_S_BAS, STATUS_S_NZ, STATUS_S_CLS, STATUS_S_WHL, &
     &           ION_MOD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PREPES makes pre-estimation of the session of VLBI         *
! *   observations. GAMB solves simultaneously the following problems:   *
! *   1) resolving group delay ambiguity for both X- and S- band         *
! *   observations; 2) finds preliminary adjustments to clock polynomial *
! *   models for clocks of all stations except one stations chosen as a  *
! *   fiducial station: clock shift, clock drift, frequency drift;       *
! *   3) eliminate outliers.                                             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *        VER_GAMB ( CHARACTER ) -- String with GAMB-identifier and     *
! *                                  number of the current version.      *
! *             OBS ( RECORD    ) -- Data structure which contains       *
! *                                  band-independent information: time  *
! *                                  of observation, baseline, lists of  *
! *                                  objects, status flags etc.          *
! *         F_BATCH ( LOGICAL*4 ) -- Flag: if we are in batch mode then  *
! *                                  .TRUE.                              *
! *         F_XBAND ( LOGICAL*4 ) -- Flag: whether to analyze X-band?    *
! *         F_SBAND ( LOGICAL*4 ) -- Flag: whether to analyze S-band?    *
! *   GAMB_F_PREUSE ( LOGICAL*4 ) -- Flag:  .TRUE. means that only       *
! *                                  observations marked as "good" has   *
! *                                  been put in GAMB data structures.   *
! *                                  .FALSE. means that all observations *
! *                                  has been put in GAMB data structures*
! *      GAMB_F_ION ( LOGICAL*4 ) -- Flag: whether to calculate          *
! *                                  ionosphere correction? It has sense *
! *                                  only of F_XBAND and F_SBAND are     *
! *                                  both .TRUE., otherwise it is        *
! *                                  ignored.                            *
! *     GAMB_CUTOFF ( REAL*8    ) -- Cutoff limit (in sec) for outliers  *
! *                                  detection.                          *
! *     GAMB_MINOBS ( INTEGER*4 ) -- Minimal acceptable number of not    *
! *                                  rejected observations at one        *
! *                                  baseline in order to count that     *
! *                                  baseline as "good".                 *
! * GAMB_SPACING_CONST ( REAL*8 ) -- Default for group delay ambiguity   *
! *                                  spacing. If zero then the values    *
! *                                  from database will be used. If non  *
! *                                  zero then it will superseded by     *
! *                                  database constant.                  *
! *         GAMB_IT ( INTEGER*4 ) -- Verbosity level. 0 -- silent mode,  *
! *                                  2 -- recommended level, 5 --        *
! *                                  debugging level.                    *
! * QUALCODE_GOOD_LIM ( INTEGER*4 ) -- Limit for qualcodes of the        *
! *                                  observations used in getting data   *
! *                                  ( this variable used only for making*
! *                                  information message ).              *
! *            CALX ( RECORD    ) -- Calibration/contribution status for *
! *                                  database for X-band.                *
! *            CALS ( RECORD    ) -- Calibration/contribution status for *
! *                                  database for S-band.                *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     STATUS_GEN ( INTEGER*4  ) -- Quality group ambiguity resolution  *
! *                                  flag. If GAMB__STS_OK -- that means *
! *                                  that ambiguity resolution is        *
! *                                  considered good. GAMB_STS_SUS means *
! *                                  that some problems were detected    *
! *                                  and ambiguities resolution          *
! *                                  consodered as suspitious.           *
! *                                  GAMB_STS_BAD means that solution    *
! *                                  considered as unsuccessful.         *
! *    STATUS_X_BAS ( INTEGER*4 ) -- Flag baseline success. GAMB__BAS_OK *
! *                                  means that all baslines were        *
! *                                  analyzed. GAMB__BAS_BAD means that  *
! *                                  at least one baseline at X-band     *
! *                                  were rejected.                      *
! *     STATUS_X_NZ ( INTEGER*4 ) -- Flag rejection tolerance criteria.  *
! *                                  GAMB__NZ_OK means successfull coming*
! *                                  through this test. GAMB__NZ_SUS     *
! *                                  means that a lot of points were     *
! *                                  rejected at X-band and solution     *
! *                                  marked as suspitious. GAMB__NZ_SUS  *
! *                                  means that too many pointes have    *
! *                                  been deleted at X-band and solution *
! *                                  marked as bad.                      *
! *    STATUS_X_CLS ( INTEGER*4 ) -- Flag of goodness redistribution of  *
! *                                  permanent ambiguities for X-band.   *
! *                                  GAMB__CLS_OK means that clock shift *
! *                                  closure residuals looked OK (but it *
! *                                  doesn't mean that are really OK!).  *
! *                                  GAMB__CLS_SUS means residuals for   *
! *                                  at least one triangle exceeded      *
! *                                  limit -- they are suspicously high. *
! *    STATUS_X_WHL ( INTEGER*4 ) -- Flag of goodness r.m.s. of whole    *
! *                                  multistantion solution for X-band.  *
! *                                  GAMB__WHL_OK means that r.m.s. is   *
! *                                  good enough. GAMB__WHL_SUS means    *
! *                                  that r.m.s. is suspicously high.    *
! *                                  GAMB__WHL_BAD means that r.m.s. is  *
! *                                  too high to mark this solution as   *
! *                                  unsuccessfull.                      *
! *         ION_MOD ( INTEGER*4 ) -- The number of observations with     *
! *                                  ambiguities in group delay          *
! *                                  ionosphere corrections which were   *
! *                                  found during the final control run  *
! *                                  of resolving ambiguities.           *
! *                                                                      *
! *   STATION_S_BAS, STATION_S_NZ, STATION_S_CLS, STATION_S_WHL have the *
! *   same meaniing but applicable for S-band observations.              *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *     GAMB ( RECORD    ) -- Array of data structures for group delay   *
! *                           ambiguity resolution software, which       *
! *                           contains two elements: the first for       *
! *                           X-band, the second for S-band.             *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  03-AUG-97     PREPES     v1.7  (c)  L. Petrov 06-DEC-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'cals.i'
      INCLUDE    'gamb.i'
      TYPE ( OBS__STRU ) ::  OBS
      TYPE ( GAMB__STRU ) ::  GAMB(2)
      TYPE ( CALS_STRU ) ::  CALX, CALS
      LOGICAL*4  F_BATCH, F_XBAND, F_SBAND, GAMB_F_PREUSE, GAMB_F_ION
      REAL*8     GAMB_CUTOFF, GAMB_SPACING_CONST
      INTEGER*4  GAMB_MINOBS, GAMB_IT, QUALCODE_GOOD_LIM, IUER
      INTEGER*4  STATUS_GEN, &
     &           STATUS_X_BAS, STATUS_X_NZ, STATUS_X_CLS, STATUS_X_WHL, &
     &           STATUS_S_BAS, STATUS_S_NZ, STATUS_S_CLS, STATUS_S_WHL, &
     &           ION_MOD
      INTEGER*4  IER, NDB4, NDB4_NEXT, IH, IL, IP, J1, J2
      LOGICAL*4  F_SPABAD, F_OK, F_CONT
      CHARACTER  VER_GAMB*(*), DATE_RAW*24, DATE*18, HOSTNAME*32, STR*80
      CHARACTER  CBAST*17
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IONO_AMB, HIT_CONT
!
! --- Getting curtrent date and time
!
      CALL FDATE ( DATE_RAW )
!
! --- ... and convert it to international DEC format
!
      CALL TRAN  ( 11, DATE_RAW, DATE_RAW )
      CALL CLRCH ( DATE )
      IF ( DATE_RAW(9:9) .EQ. ' ' ) DATE_RAW(9:9) = '0'
      DATE = DATE_RAW(12:19)//' '// &
     &       DATE_RAW(9:10)//'-'//DATE_RAW(5:7)//'-'//DATE_RAW(23:24)
!
! --- Getting host name
!
      CALL CLRCH       ( HOSTNAME )
      CALL GETHOSTNAME ( HOSTNAME, %VAL(LEN(HOSTNAME)) )
      CALL CHASHL      ( HOSTNAME )
!
! --- Printing information messages at the screen and in spool file.
!
      CALL PRCH_23 ( GAMB_IT, 1, ' '//VER_GAMB//'   Utility for automatic '// &
     &                           'group delay ambiguity resolution.' )
      CALL PRCH_23 ( GAMB_IT, 1, ' GAMB  ran on '// &
     &                            HOSTNAME(1:I_LEN(HOSTNAME))//'  at '//DATE )
      IF ( GAMB_F_PREUSE ) THEN
           CALL PRCH_23 ( GAMB_IT, 1, ' GAMB  Only observations markled '// &
     &                                'as "good" are in use'  )
         ELSE
           CALL PRCH_23 ( GAMB_IT, 1, ' GAMB  All observations are in use' )
      END IF
!
! --- Printing information message about quality code
!
      CALL CLRCH   ( STR )
      CALL INCH    ( QUALCODE_GOOD_LIM, STR )
      CALL PRCH_23 ( GAMB_IT, 1, ' GAMB  Observations with quality code '// &
     &               STR(1:I_LEN(STR))//' and better are used' )
!
! --- Printing information message about min observations per beaseline
!
      CALL CLRCH   ( STR )
      CALL INCH    ( GAMB_MINOBS, STR )
      CALL PRCH_23 ( GAMB_IT, 1, ' GAMB  Minimal acceptable number of '// &
     &              'observations at one baseline is '//STR(1:I_LEN(STR)) )
!
! --- Printing information message about cutoff limit
!
      CALL CLRCH   ( STR )
      WRITE ( UNIT=STR, FMT='(F10.2)' ) GAMB_CUTOFF*1.D9
      CALL CHASHL ( STR )
      IF ( STR(1:1) .EQ. '.' ) STR='0'//STR
      CALL PRCH_23 ( GAMB_IT, 1, ' GAMB  Cutoff limit for outlier detection '// &
     &                           'is '//STR(1:I_LEN(STR))//' nsec' )
!
! --- Printing ionosphere calibration status
!
      IF ( F_XBAND  .AND.  F_SBAND  .AND. &
     &     GAMB(1)%STATUS_ION .EQ. GAMB__IONO_1 ) THEN
!
! -------- Ionosphere calibration has been calculated by GAMB
!
           CALL PRCH_23 ( GAMB_IT, 1, ' GAMB  Ionosphere calibration has '// &
     &                                'been calculated' )
         ELSE
!
! ------ X-band
!
         IF ( F_XBAND ) THEN
            IF ( GAMB(1)%STATUS_ION .EQ. GAMB__UNF ) THEN
                 CALL PRCH_23 ( GAMB_IT, 1, ' GAMB  Ionosphere calibration '// &
     &                                      'for X-band has not been applied' )
            END IF
!
            IF ( GAMB(1)%STATUS_ION .EQ. GAMB__IONO_0 ) THEN
                 CALL PRCH_23 ( GAMB_IT, 1, ' GAMB  Ionosphere calibration '// &
     &                         'for X-band has been taken from database')
            END IF
         END IF
!
! ------ S-band
!
         IF ( F_SBAND ) THEN
            IF ( GAMB(2)%STATUS_ION .EQ. GAMB__UNF ) THEN
                 CALL PRCH_23 ( GAMB_IT, 1, ' GAMB  Ionosphere calibration '// &
     &                                      'for S-band has not been applied' )
            END IF
!
            IF ( GAMB(2)%STATUS_ION .EQ. GAMB__IONO_0 ) THEN
                 CALL PRCH_23 ( GAMB_IT, 1, ' GAMB  Ionosphere calibration '// &
     &                         'for S-band has been taken from database')
            END IF
         END IF
      END IF
!
! --- Putting constants in OBS structure.
!
      OBS%CUTOFF = GAMB_CUTOFF
      OBS%MINOBS = GAMB_MINOBS
      OBS%IT     = GAMB_IT
!
! --- Setting up GAMBC -- ambiguity spacing
!
      IF ( DABS(GAMB_SPACING_CONST) .GT. 1.D-10 ) THEN
!
! -------- User supplied
!
           GAMB(1)%GAMBC = GAMB_SPACING_CONST
           GAMB(2)%GAMBC = GAMB_SPACING_CONST
        ELSE
!
! -------- SOLVE supplied
!
           GAMB(1)%GAMBC = GAMB(1)%GAMB_SP
           GAMB(2)%GAMBC = GAMB(2)%GAMB_SP
!
! -------- We should check: does datababase have all values of spacings the
! -------- same?
!
           F_SPABAD = .FALSE.
           IF ( F_XBAND  .AND.  OBS%IDB_X .GT. 0 ) THEN
!
! ----------- First check X-band
!
              IP = OBS%IDB_X
              IF ( GAMB(IP)%L_FAM .GT. 1 ) THEN
!
! -------------- Alas! Our bad apprehensions appeared true.
!
                 CALL PRCH_23 ( 0, 0, '!!! WARNING: Different SOLVE '// &
     &               'supplied ambiguity spacings at X-BAND are detected!' )
!
! -------------- Print at the screen the list of different constants
!
                 DO 410 J1=1,GAMB(IP)%L_FAM
                    CALL CLRCH ( STR )
                    CALL INCH  ( J1, STR )
                    STR(ILEN(STR)+1:)=')'
                    IL = ILEN(STR)+3
                    STR(IL:)    =  CBAST ( OBS, GAMB(IP)%FAMBAS_LIS(J1) )
                    STR(IL+20:) = 'GPDLAMBG = '
                    IL = ILEN(STR)+2
                    WRITE ( STR(IL:IL+20), FMT='(F21.7)' ) &
     &              GAMB(IP)%FAM_LIS(J1)*1.D9
!
                    CALL CHASHL ( STR(IL:) )
                    IF ( STR(IL:) .EQ. '.' ) STR(IL:)='0'//STR(IL:)
                    CALL CHASHR ( STR(IL:IL+21) )
                    STR(ILEN(STR)+2:)='nsec'
                    CALL PRCH_23 ( 0, 0, '!!!---!!!  '//STR )
 410             CONTINUE
                 F_SPABAD = .TRUE.
              END IF
           END IF
!C
           IF ( F_SBAND  .AND.  OBS%IDB_S .GT. 0 ) THEN
!
! ----------- Then check S-band
!
              IP = OBS%IDB_S
              IF ( GAMB(IP)%L_FAM .GT. 1 ) THEN
!
! -------------- Alas! Our bad apprehensions appeared true.
!
                 CALL PRCH_23 ( 0, 0, '!!! WARNING: Different SOLVE '// &
     &               'supplied ambiguity spacings at S-BAND are detected!' )
                 DO 420 J2=1,GAMB(IP)%L_FAM
                    CALL CLRCH ( STR )
                    CALL INCH  ( J2, STR )
                    STR(ILEN(STR)+1:)=')'
                    IL = ILEN(STR)+3
                    STR(IL:)    =  CBAST ( OBS, GAMB(IP)%FAMBAS_LIS(J2) )
                    STR(IL+20:) = 'GPDLAMBG = '
                    IL = ILEN(STR)+2
                    WRITE ( STR(IL:IL+20), FMT='(F21.7)' ) &
     &              GAMB(IP)%FAM_LIS(J2)*1.D9
!
                    CALL CHASHL ( STR(IL:) )
                    IF ( STR(IL:) .EQ. '.' ) STR(IL:)='0'//STR(IL:)
                    CALL CHASHR ( STR(IL:IL+21) )
                    STR(ILEN(STR)+2:)='nsec'
                    CALL PRCH_23 ( 0, 0, '!!!---!!!  '//STR )
 420             CONTINUE
                 F_SPABAD = .TRUE.
              END IF
           END IF
!
           IF ( .NOT. F_BATCH  .AND.  F_SPABAD ) THEN
!
! ------------- In interactive mode user has the opportunity not to make
! ------------- ambiguity resolution but to return to GAMB menu
!
                CALL PRCH ( CHAR(10)//CHAR(13) )
                CALL PRCH ( 'It would be wise to try to change '// &
     &                      'group delay ambiguity constant, wouldn''t it? ' )
                CALL PRCH ( CHAR(10)//CHAR(13) )
                IH = HIT_CONT ( 'Hit key "C" to continue, otherwise we '// &
     &                          'are coming back to GAMB menu  '//CHAR(1), 0 )
                IF ( .NOT. ( IH .EQ. ICHAR('C') .OR. IH .EQ. ICHAR('c') ) ) THEN
                     CALL ERR_PASS ( 3301, IUER )
                     RETURN
                END IF
           END IF
      END IF
!
! --- Now check calibration status in order to decide: is calibration status is
! --- compartible with capacity of GAMB
!
      IF ( F_XBAND  .AND.  OBS%IDB_X .GT. 0 ) THEN
           IP = OBS%IDB_X
           CALL GACAL_CHECK ( F_BATCH, CALX, GAMB(IP)%DBASE, F_OK, F_CONT )
           IF ( .NOT. F_CONT ) THEN
                CALL ERR_PASS ( 3302, IUER )
                RETURN
           END IF
      END IF
!
      IF ( F_SBAND  .AND.  OBS%IDB_S .GT. 0 ) THEN
           IP = OBS%IDB_S
           CALL GACAL_CHECK ( F_BATCH, CALX, GAMB(IP)%DBASE, F_OK, F_CONT )
           IF ( .NOT. F_CONT ) THEN
                CALL ERR_PASS ( 3303, IUER )
                RETURN
           END IF
      END IF
!
      IF ( F_SBAND  .AND.  OBS%STATUS_GET_S .EQ. GAMB__GET ) THEN
           IF ( F_XBAND  .AND.  OBS%STATUS_GET_X .EQ. GAMB__GET  .AND. &
     &          GAMB_F_ION                                             ) THEN
!
! ------------- Resolving ambiguities in group ionosphere delays due to the
! ------------- ambiguities in group delay for X-band
!
                CALL ERR_PASS ( IUER, IER )
                ION_MOD = IONO_AMB ( OBS, GAMB(1), GAMB(2), .FALSE., .FALSE., &
     &                               IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7731, IUER, 'PREPES', 'Error during '// &
     &                   'the preliminary attempt to redistribute group '// &
     &                   'delay amiguites in ionosphere contribution' )
                     RETURN
                END IF
           END IF
!
! -------- Resolving group delay ambiguities for S-band
!
           IF ( OBS%STATUS_GET_X .EQ. GAMB__GET ) THEN
                NDB4 = 2  !  Index for S-band in GAMB array
                NDB4_NEXT = 1
             ELSE
                NDB4 = 1  !  Index for S-band in GAMB array
                NDB4_NEXT = 2
           END IF
!
           CALL ERR_PASS  ( IUER, IER )
           CALL PREPES_MB ( OBS, GAMB(NDB4), IER )
           IF ( IER .NE. 0 ) THEN
                GAMB(NDB4)%STATUS_GAMB = GAMB__ERROR
                CALL ERR_LOG ( 7732, IUER, 'PREPES', 'Error during group '// &
     &              'delay ambiguity resolution for observations '// &
     &               GAMB(2)%DBASE )
                RETURN
           END IF
           GAMB(NDB4)%STATUS_GAMB = GAMB__DONE
!
           IF ( OBS%STATUS_GET_X .EQ. GAMB__GET ) THEN
!
! ------------- Propagation of jumps in observed group delay detected at S-band
! ------------- onto X-band.
!
                CALL PROP_AMB ( OBS, GAMB(NDB4), GAMB(NDB4_NEXT) )
           END IF
      END IF
!
      IF ( F_XBAND  .AND.  OBS%STATUS_GET_X .EQ. GAMB__GET ) THEN
!
! -------- Resolving group delay ambiguity for X-band
!
           CALL ERR_PASS  ( IUER, IER )
           CALL PREPES_MB ( OBS, GAMB(1), IER )
           IF ( IER .NE. 0 ) THEN
                GAMB(1)%STATUS_GAMB = GAMB__ERROR
                CALL ERR_LOG ( 7733, IUER, 'PREPES', 'Error during group '// &
     &              'delay ambiguity resolution for observations '// &
     &               GAMB(1)%DBASE )
                RETURN
           END IF
           GAMB(1)%STATUS_GAMB = GAMB__DONE
      END IF
!
      IF ( F_XBAND  .AND.  F_SBAND  .AND.  GAMB_F_ION ) THEN
!
! -------- Resolving ambiguities of group delay ionsophere correction once more
! -------- for control. If some ambiguities remained -- it means that something
! -------- wrong was...
!
           CALL ERR_PASS ( IUER, IER )
           ION_MOD = IONO_AMB ( OBS, GAMB(1), GAMB(2), .TRUE., .TRUE., IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7734, IUER, 'PREPES', 'Error during '// &
     &              'the preliminary attempt to redistribute group '// &
     &              'delay amiguites in ionosphere contribution' )
                RETURN
           END IF
!          IF ( ION_MOD .NE. 0 ) THEN
!               CALL CLRCH ( STR )
!               CALL INCH  ( ION_MOD, STR )
!               CALL ERR_PASS ( IUER, IER )
!               CALL ERR_LOG ( 1135, IER, 'PREPES', STR(1:I_LEN(STR))//
!     #             ' ambiguities were found during final control run of '//
!     #             'resolving ambiguities in group delay ionosphere '//
!     #             'corrections. ')
!               CALL HIT_CONT ( %VAL(0), %VAL(0) )
!          END IF
         ELSE
           ION_MOD = 0
      END IF
!
      IF ( F_SBAND ) THEN
           CALL ERR_PASS  ( IUER, IER )
           IF ( F_XBAND  .AND.  OBS%STATUS_GET_X .EQ. GAMB__GET ) THEN
                CALL CHECK_RES ( GAMB_F_ION, S__BAND, OBS, GAMB(2), GAMB(1), &
     &                           IER )
              ELSE
                CALL CHECK_RES ( GAMB_F_ION, S__BAND, OBS, GAMB(1), GAMB(2), &
     &                           IER )
           END IF
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7735, IUER, 'PREPES', 'Error during '// &
     &              'residuals inspection when S-BAND data was processing' )
                RETURN
           END IF
      END IF
!
      IF ( F_XBAND  ) THEN
           CALL ERR_PASS  ( IUER, IER )
           CALL CHECK_RES ( GAMB_F_ION, X__BAND, OBS, GAMB(1), GAMB(2), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7736, IUER, 'PREPES', 'Error during '// &
     &              'residuals inspection when X-BAND data was processing' )
                RETURN
           END IF
      END IF
!
! --- Setting flags of goodness of the solution.
!
      STATUS_GEN   = GAMB__STS_OK
!
! --- Initilaization
!
      STATUS_X_NZ  = GAMB__UNF
      STATUS_X_BAS = GAMB__UNF
      STATUS_X_CLS = GAMB__UNF
      STATUS_X_WHL = GAMB__UNF
!
      STATUS_S_NZ  = GAMB__UNF
      STATUS_S_BAS = GAMB__UNF
      STATUS_S_CLS = GAMB__UNF
      STATUS_S_WHL = GAMB__UNF
!
! --- Setting certain flag
!
      IF ( F_XBAND ) THEN
           STATUS_X_NZ  = GAMB(1)%STATUS_NZ
           STATUS_X_BAS = GAMB(1)%STATUS_BAS
           STATUS_X_CLS = GAMB(1)%STATUS_CLS
           STATUS_X_WHL = GAMB(1)%STATUS_WHL
      END IF
!
      IF ( F_SBAND .AND. .NOT. F_XBAND ) THEN
           STATUS_S_NZ  = GAMB(1)%STATUS_NZ
           STATUS_S_BAS = GAMB(1)%STATUS_BAS
           STATUS_S_CLS = GAMB(1)%STATUS_CLS
           STATUS_S_WHL = GAMB(1)%STATUS_WHL
      END IF
!
      IF ( F_SBAND .AND. F_XBAND ) THEN
           STATUS_S_NZ  = GAMB(2)%STATUS_NZ
           STATUS_S_BAS = GAMB(2)%STATUS_BAS
           STATUS_S_WHL = GAMB(2)%STATUS_WHL
      END IF
!
! --- Calculation of entire suspicious flag
!
      IF ( STATUS_X_NZ  .EQ. GAMB__NZ_SUS  ) STATUS_GEN = GAMB__STS_SUS
      IF ( STATUS_X_BAS .EQ. GAMB__BAS_SUS ) STATUS_GEN = GAMB__STS_SUS
      IF ( STATUS_X_CLS .EQ. GAMB__CLS_SUS ) STATUS_GEN = GAMB__STS_SUS
      IF ( STATUS_X_WHL .EQ. GAMB__WHL_SUS ) STATUS_GEN = GAMB__STS_SUS
      IF ( ION_MOD      .GT. 0             ) STATUS_GEN = GAMB__STS_SUS
!
      IF ( STATUS_S_NZ  .EQ. GAMB__NZ_SUS  ) STATUS_GEN = GAMB__STS_SUS
      IF ( STATUS_S_BAS .EQ. GAMB__BAS_SUS ) STATUS_GEN = GAMB__STS_SUS
      IF ( STATUS_S_CLS .EQ. GAMB__CLS_SUS ) STATUS_GEN = GAMB__STS_SUS
      IF ( STATUS_S_WHL .EQ. GAMB__WHL_SUS ) STATUS_GEN = GAMB__STS_SUS
!
! --- Calculation of entire bad flag
!
      IF ( STATUS_X_NZ  .EQ. GAMB__NZ_BAD  ) STATUS_GEN = GAMB__STS_BAD
      IF ( STATUS_X_BAS .EQ. GAMB__BAS_BAD ) STATUS_GEN = GAMB__STS_BAD
      IF ( STATUS_X_WHL .EQ. GAMB__WHL_BAD ) STATUS_GEN = GAMB__STS_BAD
!
      IF ( STATUS_S_NZ  .EQ. GAMB__NZ_BAD  ) STATUS_GEN = GAMB__STS_BAD
      IF ( STATUS_S_BAS .EQ. GAMB__BAS_BAD ) STATUS_GEN = GAMB__STS_BAD
      IF ( STATUS_S_WHL .EQ. GAMB__WHL_BAD ) STATUS_GEN = GAMB__STS_BAD
!
! --- Calculation entire failure flag
!
      IF ( STATUS_X_WHL .EQ. GAMB__WHL_FAI ) STATUS_GEN = GAMB__STS_FAI
      IF ( STATUS_S_WHL .EQ. GAMB__WHL_FAI ) STATUS_GEN = GAMB__STS_FAI
!C
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PREPES  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GACAL_CHECK ( F_BATCH, CAL, DBASE, F_OK, F_CONT )
! ************************************************************************
! *                                                                      *
! *   Routine  GACAL_CHECK  checks calibrations setup status for         *
! *   database with name DBASE. If sessions 1) doesn't contain dry       *
! *   troposphere calibration to at least one stations or 2) contains    *
! *   more than one dry troposphere calibration applyed to at least one  *
! *   station this sessions marked as bad. For good session F_OK = .TRUE.*
! *   and F_CONT = .TRUE.  For bad session F_OK = .FALSE. In batch mode  *
! *   F_CONT  (attribute of continuation) is .TRUE. In interactive mode  *
! *   GACAL_CHECK  trys to persuade user not to analyse session, but     *
! *   change calibration setup. If user hit "C" F_CONT = .TRUE., if      *
! *   user hits any other key -- F_CONT = .FALSE.                        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   F_BATCH ( LOGICAL*4 ) -- Flag: if we are in batch mode then .TRUE. *
! *       CAL ( RECORD    ) -- Calibration/contribution status for       *
! *                            database for X-band.                      *
! *     DBASE ( CHARACTER ) -- Database name (used only for printing     *
! *                            message).                                 *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *      F_OK ( LOGICAL*4 ) -- Flag troposphere calibration status.      *
! *                           .TRUE. means that dry troposphere          *
! *                           calibration was applied only one time to   *
! *                           any station.                               *
! *    F_CONT ( LOGICAL*4 ) -- Flag continuation. If .TRUE. then further *
! *                            continuation is desirable. (Always in     *
! *                            batch mode, and depenend on user's taste  *
! *                            in interactive mode).                     *
! *                                                                      *
! *  ###  01-SEP-97   GACAL_CHECK   v1.2 (c)  L. Petrov  18-NOV-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'cals.i'
      INCLUDE   'socom.i'
      TYPE ( CALS_STRU ) ::  CAL
      LOGICAL*4  F_BATCH, F_OK, F_CONT
      CHARACTER  DBASE*(*)
      INTEGER*4  ICAL(MAXC_STA), ICAL_MIN, ICAL_MAX, J1, J2, J3, IH, IP
      INTEGER*4  HIT_CONT, LTM_DIF
      INTEGER*4  L_SCAL, L_ZENC
      PARAMETER  ( L_SCAL = 2 )
!
! --- Lists of SCALy calibrations and mapping function names appropiate for
! --- dry troposphere calibration
!
      CHARACTER  LIS_SCAL(L_SCAL)*8
      DATA       LIS_SCAL / &
     &                       'CHAO    ', &
     &                       'NIELTDRY' &
     &                    /
      PARAMETER  ( L_ZENC = 8 )
      CHARACTER  LIS_ZENC(L_ZENC)*8
      DATA       LIS_ZENC / &
     &                       'CFAKBDRY', &
     &                       'CFAKBWET', &
     &                       'CFAJJDRY', &
     &                       'IFADRFLY', &
     &                       'IFADRYSS', &
     &                       'MTTDRFLY', &
     &                       'MTTDRFSS', &
     &                       'NMFDRFLY' &
     &                    /
!
! --- Initialization
!
      ICAL_MIN = 99999
      ICAL_MAX = 0
!
      DO 410 J1=1,CAL%L_STA
!
! ------ We scan all stations and count how many times dry troposphere
! ------ calibration has been applied to this station
!
         ICAL(J1) = 0
         DO 420 J2=1,L_SCAL
!
! --------- First check station dependent calibrations
!
            IP = LTM_DIF ( 0, CAL%L_SCAL, CAL%SCAL, LIS_SCAL(J2) )
            IF ( IP .GT. 0 ) THEN
                 IF ( CAL%SCAL_APL(IP,J1) ) ICAL(J1) = ICAL(J1)+1
            END IF
 420     CONTINUE
!
         DO 430 J3=1,L_ZENC
!
! --------- Then check mapping calibrations
!
            IP = LTM_DIF ( 0, CAL%L_ZENC, CAL%ZENC, LIS_ZENC(J3) )
            IF ( IP .GT. 0 ) THEN
                 IF ( CAL%ZENC_APL(IP,J1) ) ICAL(J1) = ICAL(J1)+1
            END IF
 430     CONTINUE
!
! ------ Update storage for minimal and maximal times dry calibration has
! ------ been applied
!
         IF ( ICAL(J1) .GT. ICAL_MAX ) ICAL_MAX = ICAL(J1)
         IF ( ICAL(J1) .LT. ICAL_MIN ) ICAL_MIN = ICAL(J1)
 410  CONTINUE
!
      IF ( ( CALCV .GE. -200.0D0  .AND.  CALCV .LE. -100.0D0 ) .OR. &
     &     ( ICAL_MIN .EQ. 1  .AND.  ICAL_MAX .EQ. 1 )             ) THEN
!
! -------- OK!
!
           F_OK   = .TRUE.
           F_CONT = .TRUE.
           RETURN
        ELSE
!
! -------- Not OK. Generate error message
!
           CALL PRCH_23 ( 0, 0, '!!! Wrong calibration setup for the '// &
     &                    'database '//DBASE//' !!!' )
           IF ( ICAL_MIN .EQ. 0  .AND. ICAL_MAX .EQ. 0 ) THEN
                CALL PRCH_23 ( 0, 0, '!!! Dry troposphere calibration '// &
     &                         'is not applied to any station. !!!' )
              ELSE IF ( ICAL_MIN .EQ. 0  .AND. ICAL_MAX .GT. 0 ) THEN
                CALL PRCH_23 ( 0, 0, '!!! Dry troposphere calibration '// &
     &                         'is not applied to some stations. !!!' )
              ELSE IF ( ICAL_MIN .EQ. 1  .AND. ICAL_MAX .GT. 1 ) THEN
                CALL PRCH_23 ( 0, 0, '!!! Dry troposphere calibration '// &
     &                       'is applied to some stations more then once. !!!' )
              ELSE IF ( ICAL_MIN .GT. 1  .AND. ICAL_MAX .GT. 1 ) THEN
                CALL PRCH_23 ( 0, 0, '!!! Dry troposphere calibration '// &
     &                         'is applied to any station more then once. !!!' )
           END IF
           CALL PRCH_23 ( 0, 0, '!!! It is practicaly senseless to try to '// &
     &                    'resolve group delay ambiguity. !!!' )
           F_OK   = .FALSE.
!
           IF ( F_BATCH ) THEN
!
! ------------- In batch mode -- the end of work
!
                F_CONT = .TRUE.
                RETURN
             ELSE
!
! ------------- In interactive mode user has the opportunity not to make
! ------------- ambiguity resolution but to return to GAMB menu
!
                CALL PRCH ( CHAR(10)//CHAR(13) )
                CALL PRCH ( 'It would be wise to try to change '// &
     &                      'calibration srtatus: ' )
                CALL PRCH ( CHAR(10)//CHAR(13) )
                CALL PRCH ( 'to turn on NIELTDRY or NMFDRFLY in a database '// &
     &                      'calibration section' )
                CALL PRCH ( CHAR(10)//CHAR(13) )
                IH = HIT_CONT ( 'Hit key "C" to continue, otherwise we '// &
     &                          'are coming back to GAMB menu  '//CHAR(1), 0 )
                IF ( IH .EQ. ICHAR('C') .OR. IH .EQ. ICHAR('c') ) THEN
                     F_CONT = .TRUE.
                  ELSE
                     F_CONT = .FALSE.
                END IF
           END IF
      END IF
      RETURN
      END  !#!  GACAL_CHECK  #!#
