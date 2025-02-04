      PROGRAM    USERMCAL
! ************************************************************************
! *                                                                      *
! *   Program  USERMCAL runs in SOLVE in user program mode. This program *
! *   is to procure an external interface to SOLVE for mode calibrations.*
! *   Mode calibrations are calibrations which depend on band and mode:  *
! *   group delay or phase delay. Proper linear combination of mode      *
! *   calibrations is added to theoretical delays and delay rates.       *
! *                                                                      *
! *   USERMCAL  reads user-supplied mode calibrations from the external  *
! *   file and puts them on scratch obs-file. It also modifies NAMFIL    *
! *   in order to set flags which indicate that user mode calibration is *
! *   available and applied to solution. Name of the user mode           *
! *   calibration is UserMcal. If the database had LCODE which is        *
! *   associated with UserMcal calibration in CORFIL then the database   *
! *   update will save a new User Mode calibration which will replace    *
! *   with the old one, otherwise user mode calibration will not be      *
! *   saved.                                                             *
! *                                                                      *
! *   Filename of user mode calibration is expected in the form          *
! *   YYMMMDDXS_Vxxx.umc  were YYMMDDXS is a database name and xxx is    *
! *   a version name (YY - year, MMM - month, DD -day, S - suffix).      *
! *   Suffix blank is replaced with _, suffix * is replaced with -.      *
! *   Example:                                                           *
! *      99SEP20XH_V007.umc                                              *
! *      93DEC29X__V021.umc                                              *
! *      89SEP01X-_V028.umc                                              *
! *                                                                      *
! *   If usermcal doen't find file like  YYMMMDDXS_Vxxx.umc  it would    *
! *   try to find the file YYMMMDDXS.umc  (without version).             *
! *                                                                      *
! *   USERMCAL first tries to find the file in the directory pointed by  *
! *   environment variable UMC_DIR, then it looks in working area        *
! *   (WORK_DIR). Environment variable WORK_DIR overrides a system-wide  *
! *   default.                                                           *
! *                                                                      *
! *   User mode calibration file is a binary file with records of fixed  *
! *   length. The number of records is the total number of observations  *
! *   in the X-band database. Each record has length 48 bytes and        *
! *   contains 6 REAL*8 values:                                          *
! *     1) calibration for group delay at X-band (sec);                  *
! *     2) calibration for phase delay at X-band (sec);                  *
! *     3) calibration for delay rate  at X-band (dimensionless);        *
! *     4) calibration for group delay at S-band (sec);                  *
! *     5) calibration for phase delay at S-band (sec);                  *
! *     6) calibration for delay rate  at S-band (dimensionless).        *
! *                                                                      *
! *  It is assumed that calibrations are added to theoretical values.    *
! *                                                                      *
! *  To use USERMCAL we should                                           *
! *   a) first create User Mode Calibration file with appropriate        *
! *      content and name and put it in WORK_DIR directory.              *
! *   b) load the database in Interactive SOLVE;                         *
! *   c) hit option & in OPTIN (User Program);                           *
! *   d) enter the name of the program: USERMCAL and hit a key           *
! *      <Enter> on the request to enter user buffer (it should be       *
! *      empty);                                                         *
! *   e) run solution.                                                   *
! *                                                                      *
! *   It is enought to do steps c) and d) only once unless the file with *
! *   user mode calibrations has been changed.                           *
! *                                                                      *
! *   To get rid from User Mode Calibrations, call ACCOR (+ from OPTIN)  *
! *   and then hit a key M. Then deselect UserMcal calibration in the    *
! *   menu there and then hit O. User Mode Calibration will not be       *
! *   applied.                                                           *
! *                                                                      *
! *  ###  19-NOV-99     USERMCAL   v1.1  (c)  L. Petrov  29-DEC-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT    NONE
      INCLUDE    'solve.i'
      INCLUDE    'socom.i'
      INCLUDE    'oborg.i'
      CHARACTER   DBNAME*10, FINAM*128, FINAM_OLD*128, DIRNAM*128, STR*80
!!      CHARACTER  MODE_STRING*80
      INTEGER*2   LDBNAM(5,15), IDBV(15)
      CHARACTER   CDBNAM(15)*10
      EQUIVALENCE (CDBNAM,LDBNAM(1,1))
      LOGICAL*4   LEX
      INTEGER*4   UMC_SLOT, IDBE(15), I11, NN, NOBS, J1, IUER
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4,  EXTERNAL :: ILEN, I_LEN
!
      CALL PRE_PROG()
!!      CALL CLRCH ( MODE_STRING )
!!      CALL USE_BUFFER  ( %REF(MODE_STRING), INT2(40), 'ORC' )
      CALL SET_VERSION ( 'U-CAL', '1999.12.29', 'usermcal' )
!
! --- Reading common area
!
      CALL USE_COMMON   ( 'ORC' )  ! Reading  socom.i
      CALL SOCOM_EXT()
!
! --- Getting database name
!
      CALL DBPOX  ( NUMDB, LDBNAM, IDBV, IDBE )
      DBNAME = CDBNAM(1)
      NOBS = IDBE(1)
!
! --- Form file name
!
      FINAM  = CDBNAM(1)(2:10)//'_V000.umc'
      IF ( FINAM(9:9) .EQ. ' ' ) FINAM(9:9) = '_'
      IF ( FINAM(9:9) .EQ. '*' ) FINAM(9:9) = '-'
      CALL INCH ( INT4(IDBV(1)), FINAM(12:14) )
      CALL CHASHR (              FINAM(12:14) )
      CALL BLANK_TO_ZERO (       FINAM(12:14) )
!
! --- Determine directory where User Mode Calibration file whoul be located
!
      CALL CLRCH ( DIRNAM )
      CALL GETENVAR ( 'UMC_DIR',  DIRNAM )
      IF ( ILEN(DIRNAM) .EQ. 0 ) THEN
           CALL GETENVAR ( 'PSOLVE_WORK_DIR', DIRNAM )
      END IF
      IF ( ILEN(DIRNAM) .EQ. 0 ) THEN
           DIRNAM = SOLVE_WORK_DIR
      END IF
!
! --- Prepend this directory before the name of User Mode Calibration file
!
      IF ( DIRNAM(I_LEN(DIRNAM):I_LEN(DIRNAM)) .NE. '/' ) THEN
           DIRNAM = DIRNAM(1:I_LEN(DIRNAM))//'/'
      END IF
!
      FINAM = DIRNAM(1:I_LEN(DIRNAM))//FINAM
!
! --- Check whether the file with user mode calibrations exist
!
      INQUIRE ( FILE=FINAM, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
!
! -------- We didn't find. Make the second attempt. Remove version from the
! -------- name.
!
           CALL CLRCH ( FINAM_OLD )
           FINAM_OLD = FINAM
!
           FINAM  = CDBNAM(1)(2:10)//'.umc'
           IF ( FINAM(9:9) .EQ. ' ' ) FINAM(9:9) = '_'
           IF ( FINAM(9:9) .EQ. '*' ) FINAM(9:9) = '-'
           FINAM = DIRNAM(1:I_LEN(DIRNAM))//FINAM
!
! -------- ... and again check wether the file exists
!
           INQUIRE ( FILE=FINAM, EXIST=LEX )
      END IF
!
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 6501, -1, 'USERMCAL', 'File for user mode '// &
     &         'calibration nether '//FINAM_OLD(1:I_LEN(FINAM_OLD))//' nor '// &
     &          FINAM(1:I_LEN(FINAM))//' is not found' )
           STOP 'USERMCAL'
      END IF
!
! --- Open User Mode Calibration file
!
      OPEN ( UNIT=11, FILE=FINAM, STATUS='OLD', ACCESS='DIRECT', RECL=48, &
     &       IOSTAT=I11 )
      IF ( I11 .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( I11, STR )
           CALL ERR_LOG ( 6502, -1, 'USERMCAL', 'Error in attempt to open '// &
     &         'file with User Mode Calibrations '//FINAM(1:I_LEN(FINAM))// &
     &         '  IOSTAT='//STR )
           STOP 'USERMCAL'
      END IF
!
! --- Getting a slot for User Mode Calibrations and modify NAMFIL if necessary
!
      IUER = -1
      CALL GET_UMC_SLOT ( DBNAME, UMC_SLOT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6503, -1, 'USERMCAL', 'Error in getting a slot for '// &
     &         'user mode calibration when superfile '//DBNAME// &
     &         ' was processed' )
           CLOSE ( UNIT=11 )
           STOP 'USERMCAL'
      END IF
!
! --- Open OBS-file
!
      CALL ACS_OBSFIL ( 'O' )
      DO 410 J1=1,NOBS ! cycle on observations
!
! ------ Reading oborg
!
         CALL USE_OBSFIL ( IOBSFIL, J1, 'R' )
!
! ------ Reading User file directly to the OBORG area
!
         READ ( 11 ) ( CALIBM(NN,UMC_SLOT), NN=1,6 )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!           type *,' j1=',j1,' calibm(1,umc_slot)=',calibm(1,umc_slot), ! %%%
!     #                      ' calibm(4,umc_slot)=',calibm(4,umc_slot)  ! %%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------ Writing oborg
!
         CALL USE_OBSFIL ( IOBSFIL, J1, 'W' )
 410  CONTINUE
!
! --- Closing OBS-file
!
      CALL ACS_OBSFIL ( 'C' )
      CLOSE ( UNIT=11 )
!
      CALL END_PROG()
      END  !#!  USERMCAL  #!#
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
      CALL CALS_R ( INT2(1), IVRB, 0, CAL, IER)
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
      CALL CALS_W ( INT2(1), CAL, IER)
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
