      PROGRAM     USE_DELTRAN
! ************************************************************************
! *                                                                      *
! *   Program USE_DELTRAN  works as a user program in Solve. It is used  *
! *   for comparison of two two databases which correspond to the same   *
! *   experiment but were produced by different fringing. It is assumed  *
! *   1) databases has at least 80% of the same observations.            *
! *   2) Program SET_DELTRAN has run with the alternative database.      *
! *                                                                      *
! *   Program SET_DELTRAN created an intermediate file which keeps       *
! *   delays phases, suppression status and some other information about *
! *   alternative session.                                               *
! *                                                                      *
! *   USE_DELTRAN reads this intermediate file and tries to match each   *
! *   observation from the intermediate file (and therefore from the     *
! *   alternative database) with the observation from the current        *
! *   database. All observations in the current database which were not  *
! *   matched with alternative database are marked as outliers.          *
! *   USE_DELTRAN changes suppression status for matched observations    *
! *   in current database to the values from the alternative database.   *
! *   It sets mode calibration USERMCAL which would keep for 6           *
! *   observables: group delay at both bands, phase delay at both bands, *
! *   phase delay rate at both bands to the value: CURRENT_DATABASE      *
! *   minus ALTERNATIVE_DATABASE. Program USE_DELTRAN also sets status   *
! *   flag: to use USERMCAL calibration.                                 *
! *                                                                      *
! *   When USERMCAL mode calibration is on, then Solve will effectively  *
! *   use delays, phases, phase rates from alternative database. When    *
! *   USERMCAL mode calibrations off, then Solve will use delays, rates  *
! *   and phases from the current database.                              *
! *                                                                      *
! *   Caveat: After using USE_DELTRAN the number of suppressed           *
! *   observations may increase. Observations will be suppressed if      *
! *   1) they were suppressed in the current database;                   *
! *   2) they were suppressed in the alternative database;               *
! *   3) they were not matched between the current and alternative       *
! *      database;                                                       *
! *                                                                      *
! *  ### 11-DEC-2001  USE_DELTRAN  v1.2 (c)  L. Petrov  08-JUN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT    NONE
      INCLUDE    'solve.i'
      INCLUDE    'prfil.i'
      INCLUDE    'socom.i'
      INCLUDE    'oborg.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'delay_transfer.i'
      CHARACTER   DBNAME*16
      INTEGER*4   UMC_SLOT, IDBE(15), NOBS, LUN, NBT, LEN_DTR, IND, MJD, &
     &            LOBS, J1, J2, J3, IS, NP, IUER
      REAL*8      UTC_TAG, EPS_SEC
      PARAMETER  ( EPS_SEC = 0.1D0 ) !
      INTEGER*2   LDBNAM(5,15), IDBV(15)
      CHARACTER   CDBNAM(15)*10, FILIN*128, SCR_DIR*128
      INTEGER*4  MOBS_DTR
      PARAMETER  ( MOBS_DTR = 10*1024 )
      TYPE ( DT__STRU ) ::  DTR(MOBS_DTR)
      INTEGER*4,   EXTERNAL :: IARGC, ILEN, I_LEN
!
! --- Learn the name of the scatch directory
!
      CALL CLRCH ( SCR_DIR )
      CALL GETENVAR ( 'SCRATCH_DIR', SCR_DIR )
      IF ( ILEN(SCR_DIR) .EQ. 0 ) THEN
           SCR_DIR = SCRATCH_DIR
      END IF
      IS = I_LEN(SCR_DIR)
      IF ( SCR_DIR(IS:IS) .EQ. '/' ) SCR_DIR(IS:IS) = ' '
      IS = I_LEN(SCR_DIR)
!
! --- Set filename
!
      FILIN = SCR_DIR(1:IS)//'/delay_transfer.bin'
!
! --- LEarn the length of DT-record
!
      LEN_DTR = LOC(DTR%LAST_FIELD) - LOC(DTR%GRDEL_X) + 2
!
      CALL PRE_PROG()
      CALL CLRCH ( DBNAME )
!
! --- Reading common area
!
      CALL USE_COMMON   ( 'ORC' )  ! Reading  socom.i
      CALL SOCOM_EXT()
      CALL USE_PARFIL   ( 'ORC' )  ! Reading  parfil.i
      CALL USE_GLBFIL_4 ( 'ORC' )  ! Reading  glbc4.i
!
! --- Getting database name
!
      CALL DBPOX  ( NUMDB, LDBNAM, IDBV, IDBE )
      DBNAME = CDBNAM(1)
      NOBS = IDBE(1)
!
! --- Getting a slot for User Mode Calibrations and modify NAMFIL if necessary
!
      IUER = -1
      CALL GET_UMC_SLOT ( DBNAME, UMC_SLOT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6501, -1, 'USE_DELTRAN', 'Error in getting a slot '// &
     &         'for user mode calibration when superfile '//DBNAME// &
     &         ' was processed' )
           STOP 'USE_DELTRAN'
      END IF
!
! --- Reading delay transfer file
!
      CALL BINF_OPEN  ( FILIN, 'OLD', LUN, -3 )
      CALL RDBIN_RECORD ( LUN, 4, LOBS, NBT, -3 )
      DO 410 J1=1,LOBS ! cycle on observations
         CALL RDBIN_RECORD ( LUN, LEN_DTR, DTR(J1), NBT, -3 )
 410  CONTINUE
      CALL BINF_CLOSE ( LUN, -3 )
!
! --- Open OBS-file
!
      CALL ACS_OBSFIL ( 'O' )
!
      NP = 0
      DO 420 J2=1,NOBS ! cycle on observations
!
! ------ Reading oborg
!
         CALL USE_OBSFIL ( IOBSFIL, J2, 'R' )
!
         MJD = ( FJD - 2400000.5D0 + 0.001 )
         UTC_TAG = FRACT*86400.0D0
!
         IND = 0
         DO 430 J3=1,LOBS
            IF ( DTR(J3)%MJD .EQ. MJD                            .AND. &
     &           DABS( DTR(J3)%UTC_TAG - UTC_TAG ) .LT. EPS_SEC  .AND. &
     &           DTR(J3)%BAS_NAME(1:8)  .EQ. ISITN_CHR(ISITE(1)) .AND. &
     &           DTR(J3)%BAS_NAME(9:16) .EQ. ISITN_CHR(ISITE(2)) .AND. &
     &           DTR(J3)%SOU_NAME       .EQ. ISTRN_CHR(ISTAR)          ) THEN
!
                 IND = J3
                 GOTO 830
            END IF
 430     CONTINUE
 830     CONTINUE
!
! ------ Check whether we can use this observation
!
         IF ( IND .EQ. 0 ) THEN
              IUNW = 1
              CALL SUPR_OBS ( IDATYP, SUPSTAT, UACSUP )
              USER_SUP = IBSET ( USER_SUP, INT4(IDATYP) )
!
              CALIBM( MCL__GRX, UMC_SLOT) = 0.0D0
              CALIBM( MCL__GRS, UMC_SLOT) = 0.0D0
            ELSE
              IF ( DTR(IND)%QC_X .GE. QUALCODE_GOOD_LIM  .AND. &
     &             DTR(IND)%QC_X .LE. 9                  .AND. &
     &             DTR(IND)%QC_S .GE. QUALCODE_GOOD_LIM  .AND. &
     &             DTR(IND)%QC_S .LE. 9                        ) THEN
!
                   CALIBM( MCL__GRX, UMC_SLOT ) = DOBS*1.D-6 - &
     &                DTR(IND)%GRDEL_X - DTR(IND)%NAMBSP_X*DTR(IND)%GRDAMBSP_X
                   CALIBM( MCL__GRS, UMC_SLOT ) = DOBS_S*1.D-6 - &
     &                DTR(IND)%GRDEL_S - DTR(IND)%NAMBSP_S*DTR(IND)%GRDAMBSP_S
                   UACSUP = DTR(IND)%UACSUP
                   NP = NP + 1
              END IF
         END IF
!
! ------ Writing oborg
!
         CALL USE_OBSFIL ( IOBSFIL, J2, 'W' )
 420  CONTINUE
      WRITE ( 6, 110 ) NP, NOBS
 110  FORMAT ( 'USE_DELTRAN: ', I6, ' delays out of ', I6, ' were replaced' )
      CALL HIT_CONT ( %VAL(0), %VAL(0) )
!
! --- Closing OBS-file
!
      CALL ACS_OBSFIL ( 'C' )
!
      CALL END_PROG()
      END  !#!  USE_DELTRAN  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_UMC_SLOT ( DBNAME, UMC_SLOT, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiallry routine  GET_UMC_SLOT  examines calibration status,      *
! *   return a slot where user mode calibrations can be put. It set bit  *
! *   avialable and applied to user mode calibrations and then write     *
! *   calibration status back to NAMFIL.                                 *
! *                                                                      *
! *  ###  19-NOV-99  GET_UMC_SLOT  v1.0  (c)  L. Petrov  19-NOV-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'cals.i'
      INTEGER*4  UMC_SLOT, IUER
      CHARACTER  DBNAME*(*)
!
      TYPE ( CALS_STRU ) ::  CAL
      INTEGER*4  IVRB, J1, IER
!
      IVRB = 0
!
! --- Reading calibration status
!
      CALL ERR_PASS ( IUER, IER )
      CALL CALS_R ( INT2(1), IVRB, 0, CAL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6511, IUER, 'GET_UMC_SLOT ', 'Error in reading '// &
     &         'calibration information while database '//DBNAME// &
     &         ' was processed' )
           RETURN
      END IF
!
! --- Learn the index of "User mode Calibration"
!
      UMC_SLOT = 0
      IF ( CAL%L_MCAL .GT. 0 ) THEN
           DO 410 J1=1,CAL%L_MCAL
              IF ( CAL%MCAL(J1) .EQ. 'UserMcal' ) UMC_SLOT = J1
 410       CONTINUE
      END IF
!
      IF ( UMC_SLOT .EQ. 0 ) THEN
!
! -------- User Mode Calibration has not been found
!
           IF ( CAL%L_MCAL .EQ. M_CLM ) THEN
                CALL ERR_LOG ( 6512, IUER, 'GET_UMC_SLOT ', 'There is no '// &
     &              'free slot for User Mode Calibrations for the database '// &
     &              DBNAME )
                RETURN
           END IF
!
! -------- Create it
!
           CAL%L_MCAL = CAL%L_MCAL + 1
           UMC_SLOT = CAL%L_MCAL
           CAL%MCAL(UMC_SLOT) = 'UserMcal'
           CAL%MCAL_LCODE(UMC_SLOT) = '        '
      END IF
!
! --- Setting status "available" and "applied" for user mode calibration
!
      CAL%MCAL_AVL(UMC_SLOT) = .TRUE.
      CAL%MCAL_APL(UMC_SLOT) = .TRUE.
!
! --- Writing calibration status
!
      CALL ERR_PASS ( IUER, IER )
      CALL CALS_W ( INT2(1), CAL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6513, IUER, 'GET_UMC_SLOT ', 'Error in writing '// &
     &         'calibration information while database '//DBNAME// &
     &         ' was processed' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_UMC_SLOT  #!#
