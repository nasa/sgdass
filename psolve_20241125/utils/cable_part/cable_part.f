      PROGRAM    CABLE_PART
! ************************************************************************
! *                                                                      *
! *   Program  CABLE_PART  runs in SOLVE user partials mode. It looks    *
! *   at cable calibration status. If the cable calibration is available *
! *   but not set up for the certain station(s) then CABLE_PART puts     *
! *   cable calibration for that(those) stations as partial derivatives  *
! *   in local mode (for this arc only).                                 *
! *                                                                      *
! *   Parameters "Cable_admit xxxxxxxx" where xxxxxxxxx is a 8-letters   *
! *   name of the station are being estimated.                           *
! *                                                                      *
! *   Purpose of estimation of cable parameters is to evaluate           *
! *   reliability of cable calibration. IF the cable calibration is      *
! *   valid we should expect the adjustment about 1.0 . If the cable     *
! *   calibration is valid but analyst made an error in determination of *
! *   its sign that the adjustment to cable will be about -1.0 . Values  *
! *   far from -1.0 and 1.0 indicates on unreliability of cable          *
! *   calibration measurements.                                          *
! *                                                                      *
! *   Cable admittance will not be estimated for the stations which      *
! *   had difference between maximum value of cable calibration and      *
! *   minimal value of cable calibration less then the specified         *
! *   threshold for all (used or not used) observations. This trick is   *
! *   done to prevent normal matrix singularity.                         *
! *                                                                      *
! *  ### 26-DEC-1998   CABLE_PART  v3.0  (c)  L. Petrov 03-MAR-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'oborg.i'
      INCLUDE   'prfil.i'
      INCLUDE   'socom.i'
      INCLUDE   'precm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'cals.i'
      TYPE ( CALS_STRU ) ::  CAL
      INTEGER*2   LDBNAM(5,15), IDBV(15)
      CHARACTER   CDBNAM(15)*10, DBNAME*16
      EQUIVALENCE (CDBNAM,LDBNAM(1,1))
      CHARACTER   LNAME_PT(MAX_ARC_STA)*22, PT_FILE*128
      INTEGER*4   IDBE(15), J0, J1, J2, J3, J4, J5, IND_CAB, L_STA, &
     &            L_CAB,  LIS_CAB(MAX_ARC_STA), &
     &            LN_CAB, LISN_CAB(MAX_ARC_STA), &
     &            PT_DESC, IND_PT(MAX_ARC_STA), JBYTES, JBLOCKS, IUER
      LOGICAL*4   F_CAB(MAX_ARC_STA)
      REAL*8      ARR_PT(MAX_ARC_STA), ARR2_PT(2,MAX_ARC_STA), &
     &            CAB_MIN(MAX_ARC_STA), CAB_MAX(MAX_ARC_STA), CAB_EPS
      PARAMETER   ( CAB_EPS = 1.D-13 )
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4,  EXTERNAL :: I_LEN
!
      CALL PRE_PROG()
!
! --- Reading common areas
!
      CALL USE_GLBFIL_4 ( 'ORC' )
      CALL USE_PARFIL   ( 'ORC' )  ! Reading  prfil.i
      CALL USE_COMMON   ( 'ORC' )  ! Reading  socom.i
      CALL SOCOM_EXT()
!
! --- Getting database name
!
      CALL DBPOX  ( NUMDB, LDBNAM, IDBV, IDBE )
!
      CALL CLRCH ( DBNAME )
      DBNAME = CDBNAM(1)
!
! --- Forming field DBNAME_MES
!
      DBNAME(12:) = '<'
      CALL INCH ( INT4(IDBV(1)), DBNAME(13:) )
      DBNAME( I_LEN(DBNAME)+1: ) = '>'
!
! --- Learn the number of stations, the number of stations with available
! --- but not applied cable, form a station list of such stations
!
      L_STA = INT4(NUMSTA)
      IUER = -1
      CALL LEARN_CABLE    ( DBNAME, L_STA, CAL, IND_CAB, L_CAB, LIS_CAB, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7101, -1, 'CABLE_PART', 'Error in LEARN_CABLE' )
           CALL EXIT ( 1 )
      END IF
 910  CONTINUE
!
! --- Write down the list of user partials
!
      IUER = -1
      CALL PARTLIST_CALBE ( L_CAB, LIS_CAB, LNAME_PT, PT_FILE, PT_DESC, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7102, -1, 'CABLE_PART', 'Error in PARTLIST_CALBE' )
           CALL EXIT ( 1 )
      END IF
!
! --- Initialization of staus flags
!
      DO 400 J0=1,MAX_ARC_STA
         F_CAB(J0) = .FALSE.
 400  CONTINUE
!
      IF ( L_CAB .GT. 0 ) THEN
!
! ------ Opening OBS-file
!
         CALL ACS_OBSFIL ( 'O' )
!
         DO 410 J1=1,NUMOBS ! cycle on observations
            IND_PT(1) = J1
            IND_PT(2) = L_CAB
            IUER = -1
            CALL WRBIN_ARRAY ( PT_DESC, 'I4', 2, IND_PT, IUER )
!
! --------- Reading oborg
!
            CALL USE_OBSFIL ( IOBSFIL, J1, 'R' )
!
! --------- Zeroing array of partials
!
            CALL NOUT_R8 ( INT4(MAX_ARC_STA), ARR_PT )
!
! --------- Scan stations from the list and set partials
!
            DO 420 J2=1,L_CAB
               IND_PT(J2) = J2
               IF ( LIS_CAB(J2) .EQ. INT4(ISITE(1))  ) THEN
                    ARR_PT(J2) = CALIBS(1,1,IND_CAB)
!
! ----------------- Check: whether we have to update minimal or maximal
! ----------------- cable calibration for the station under consideration
! ----------------- (NB: cable calibration enters with minus to the total
! ----------------- delay here)
!
                    IF ( .NOT. F_CAB(J2) ) THEN
                         CAB_MIN(J2) = -ARR_PT(J2)
                         CAB_MAX(J2) = -ARR_PT(J2)
                         F_CAB(J2) = .TRUE.
                      ELSE
                         IF ( -ARR_PT(J2) .LT. CAB_MIN(J2) ) &
     &                      CAB_MIN(J2) = -ARR_PT(J2)
                         IF ( -ARR_PT(J2) .GT. CAB_MAX(J2) ) &
     &                      CAB_MAX(J2) = -ARR_PT(J2)
                    END IF
               END IF
!
               IF ( LIS_CAB(J2) .EQ. INT4(ISITE(2))  ) THEN
                    ARR_PT(J2) = CALIBS(2,1,IND_CAB)
!
! ----------------- Check: whether we have to update minimal or maximal
! ----------------- cable calibration for the station under consideration
! ----------------- (NB: cable calibration enters with plus to the total
! ----------------- delay here)
!
                    IF ( .NOT. F_CAB(J2) ) THEN
                         CAB_MIN(J2) = ARR_PT(J2)
                         CAB_MAX(J2) = ARR_PT(J2)
                         F_CAB(J2) = .TRUE.
                      ELSE
                         IF ( ARR_PT(J2) .LT. CAB_MIN(J2) ) &
     &                      CAB_MIN(J2) = ARR_PT(J2)
                         IF ( ARR_PT(J2) .GT. CAB_MAX(J2) ) &
     &                      CAB_MAX(J2) = ARR_PT(J2)
                    END IF
               END IF
 420        CONTINUE
!
! --------- Writing partil derivatives
!
            IF ( NORATE_FLAG ) THEN
!                 JBYTES  = 8*L_CAB
!                 JBLOCKS = (JBYTES+256-1)/256
!                 CALL BIN_WRITE ( PT_FILE, PT_DESC, ARR_PT, JBLOCKS )
!!
                  IUER = -1
                  CALL WRBIN_ARRAY ( PT_DESC, 'I4', L_CAB, IND_PT, IUER )
                  IUER = -1
                  CALL WRBIN_ARRAY ( PT_DESC, 'R8', L_CAB, ARR_PT, IUER )
               ELSE
!
! -------------- Adding field for delay rate and put there zero
!
                 JBYTES  = 8*2*L_CAB
                 JBLOCKS = (JBYTES+256-1)/256
                 DO 430 J3=1,L_CAB
                    ARR2_PT(1,J3) = ARR_PT(J3)
                    ARR2_PT(2,J3) = 0.0
 430             CONTINUE
                 CALL BIN_WRITE ( PT_FILE, PT_DESC, ARR2_PT, JBLOCKS )
            END IF
 410     CONTINUE
!
! ------ Closing OBS-file
!
         CALL ACS_OBSFIL ( 'C' )
!
! ------ Closing PT-file
!
         CALL BIN_CLOSE ( PT_FILE, PT_DESC )
      END IF
!
! --- Now check stratus flags. We create a new list of stations with estiamted
! --- cable admittance. But only stationbs with difference max_cable minus
! --- min_cable larger than the specified limit are included in the new list
!
      LN_CAB = 0
      DO 440 J4=1,L_CAB
         IF ( (CAB_MAX(J4) - CAB_MIN(J4)) .GT. CAB_EPS ) THEN
              LN_CAB = LN_CAB + 1             ! create a new
              LISN_CAB(LN_CAB) = LIS_CAB(J4)  ! list
         END IF
 440  CONTINUE
!
! --- And check: whether we had such a situation when new list is shorter then
! --- the old. YEs means that there were stations with constant cable
! --- calibration. We have to remove such stations from the list
!
      IF ( LN_CAB .NE. L_CAB ) THEN
!
! -------- Yes we had. Update lists...
!
           L_CAB = LN_CAB                 ! replace and old list
           DO 450 J5=1,L_CAB              ! with new list
              LIS_CAB(J5) = LISN_CAB(J5)
 450       CONTINUE
!
! -------- ... and repeat the entire process once more
!
           GOTO 910
      END IF
!
      CALL END_PROG()
      END  !#!  CABLE_PART  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LEARN_CABLE ( DBNAME, L_STA, CAL, IND_CAB, L_CAB, LIS_CAB, &
     &                         IUER )
! ************************************************************************
! *                                                                      *
! *   Auxillary routine  LEARN_CABLE  learns status the list of stations *
! *   for which cable calibration is available. It also learns the index *
! *   of the cable calibration in the list of calibrations.              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  DBNAME ( CHARACTER ) -- Database name.                              *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   L_STA ( INTEGER*4 ) -- Number of stations in the database.         *
! *     CAL ( RECORD    ) -- Data structure which keeps calibration      *
! *                          status.                                     *
! * IND_CAB ( INTEGER*4 ) -- Index of the cable calibration in the list  *
! *                          of station dependent calibrations.          *
! *   L_CAB ( INTEGER*4 ) -- Number of stations for which the cable      *
! *                          calibration is available.                   *
! * LIS_CAB ( INTEGER*4 ) -- Cross reference table list of station codes *
! *                          for the stations for whch cable cable       *
! *                          calibration is available. Dimension: L_CAB. *
! *                          if ( LIS_CAB(k) = j where j is the index of *
! *                          the station in the database ).              *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *          IUER ( INTEGER*4, OPT ) -- Universal error handler.         *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  26-DEC-98   LEARN_CABLE  v1.0  (c)  L. Petrov  26-DEC-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'cals.i'
      CHARACTER  DBNAME*(*)
      TYPE ( CALS_STRU ) ::  CAL
      INTEGER*4  IVRB, L_STA, L_CAB, LIS_CAB(L_STA), J1, J2, IND_CAB, IUER, IER
!
      IVRB = 0
!
! --- Reading calibration status
!
      CALL ERR_PASS ( IUER, IER )
      CALL CALS_R ( INT2(1), IVRB, 0, CAL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3231, IUER, 'LEARN_CABLE', 'Error in reading '// &
     &         'calibration information while database '//DBNAME// &
     &         ' was processed' )
           RETURN
      END IF
!
! --- Learn the index of "cable calibration"
!
      IND_CAB = 0
      DO 410 J1=1,CAL%L_SCAL
         IF ( CAL%SCAL(J1) == 'cable   ' .OR. &
     &        CAL%SCAL(J1) == 'CABL_DEL'      ) IND_CAB = J1
410  CONTINUE
      IF ( IND_CAB .EQ. 0 ) THEN
           CALL ERR_LOG ( 3232, IUER, 'LEARN_CABLE', 'Cable calibration slot '// &
     &         'was not found in calibrations slots. Error detected while '// &
     &         'database '//DBNAME//' was processed' )
           RETURN
      END IF
!
! --- Setting status "available" and "applied" for user calibration
!
      L_CAB = 0
      DO 420 J2=1,CAL%L_STA
         IF (       CAL%SCAL_AVL(IND_CAB,J2)  .AND. &
     &        .NOT. CAL%SCAL_APL(IND_CAB,J2)        ) THEN
              L_CAB = L_CAB + 1
              LIS_CAB(L_CAB) = J2
         END IF
 420  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  LEARN_CABLE  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PARTLIST_CALBE ( L_CAB, LIS_CAB, LNAME_PT, PT_FILE, PT_DESC, &
     &                            IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PARTLIST_CALBE  creates the list os cable admittance      *
! *   partial derivatives and writes it down.                            *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   L_CAB ( INTEGER*4 ) -- Number of stations for which the cable      *
! *                          calibration is available.                   *
! * LIS_CAB ( INTEGER*4 ) -- Cross reference table list of station codes *
! *                          for the stations for whch cable cable       *
! *                          calibration is available. Dimension: L_CAB. *
! *                          if ( LIS_CAB(k) = j where j is the index of *
! *                          the station in the database ).              *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * LNAME_PT ( CHARACTER ) -- The list of names of user cable admittance *
! *                           paramters. Dimension: L_CAB.               *
! *  PT_FILE ( CHARACTER ) -- File name for the list of user partials.   *
! *  PT_DESC ( INTEGER*4 ) -- File descriptor for the list of user       *
! *                           partials.                                  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *          IUER ( INTEGER*4, OPT ) -- Universal error handler.         *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  26-DEC-98  PARTLIST_CALBE  v1.0 (c) L. Petrov  26-DEC-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'prfil.i'
      INCLUDE   'precm.i'
      INTEGER*4  L_CAB, LIS_CAB(L_CAB), PT_DESC, IUER
      CHARACTER  LNAME_PT(L_CAB)*(*), PT_FILE*(*)
      CHARACTER  PL_FILE*128
      INTEGER*4  J1, I66
      LOGICAL*4  PT_EXIST
      INTEGER*4, EXTERNAL ::  I_LEN
!
! --- Forming the name of PL-file
!
      CALL CLRCH ( PL_FILE )
      PL_FILE = PRE_SCR_DIR(1:PRE_SD_LEN)//'USRP'//PRE_LETRS
!
! --- Openning PL-FILE
!
      OPEN ( UNIT=66, FILE=PL_FILE, STATUS='UNKNOWN', IOSTAT=I66 )
      IF ( I66 .NE. 0 ) THEN
           CALL ERR_LOG ( 3241, IUER, 'CP_START', 'Error openning '// &
     &          'PL-file '//PL_FILE(1:I_LEN(PL_FILE)) )
           RETURN
      END IF
!
! --- Writing information into PL-file
!
      WRITE ( 66, * )  L_CAB
!
      IF ( L_CAB .GT. 0 ) THEN
           DO 410 J1=1,L_CAB
              IF ( LIS_CAB(J1) .GT. 0 ) THEN
                   LNAME_PT(J1) = 'Cable_admit '//ISITN_CHR(LIS_CAB(J1))//' A'
                   WRITE ( 66, FMT='(A)' ) LNAME_PT(J1)
              END IF
 410       CONTINUE
!
! -------- Forming the name of PT-file (user-partials in SOLVE format)
!
           CALL CLRCH ( PT_FILE )
           PT_FILE = PRE_SCR_DIR(1:PRE_SD_LEN)//'PART'//PRE_LETRS
           INQUIRE ( FILE = PT_FILE, EXIST = PT_EXIST )
           IF ( PT_EXIST ) CALL UNLINK ( PT_FILE(1:I_LEN(PT_FILE))//CHAR(0) )
!
! -------- Creating PT-file
!
           CALL BIN_CREATE8 ( PT_FILE, PT_DESC, INT8(0) )
      END IF
      CLOSE ( UNIT=66 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PARTLIST_CALBE  #!#
