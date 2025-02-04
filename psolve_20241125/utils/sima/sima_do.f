      SUBROUTINE SIMA_DO ( SUPNAM, SIMA, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SIMA_DO  makes main work of the utility sima:             *
! *   a) Creates the control file for batch SOLVE by using template      *
! *      control file defined in configuration file under the field      *
! *      BATCH_CONTROL_FILE by adding the superfile in the arcfile.      *
! *   b) Run Batch Solve with the control file which has been created.   *
! *   c) Parses spool file and correlation file.                         *
! *      It extracts the estimates of the formal errors of EOP           *
! *      It finds the maximum correlations between each component of EOP *
! *      and station coordinates.                                        *
! *   d) Generates the table of results and writes it down in the output *
! *      file.                                                           *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  SUPNAM ( CHARACTER ) -- Superfile name -- name of the experiment    *
! *                          which will be processed by Sovle in         *
! *                          simulation BATCH run.                       *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *   SIMA  ( RECORD    ) -- Object with data structure for keeping      *
! *                          configuration of sima.                      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 02-AUG-2000    SIMA_DO    v1.2 (c)  L. Petrov  10-SEP-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      INCLUDE   'sima.i'
      TYPE ( SIMA__STRU ) ::  SIMA
      INTEGER*4  IUER, MBUF
      PARAMETER  ( MBUF = 16384 )
      CHARACTER  SUPNAM*(*), BUF(MBUF)*256, COMSTR*128, SPOOL_DIR_FILE*128, &
     &           SPOOL_FILE*128, CORL_FILE*128, LOCK_FILE*128
      CHARACTER  XPL_STR*7, YPL_STR*7, UT1_STR*7, UTR_STR*7, &
     &           PSI_STR*7, EPS_STR*7, STR*32, SHELL_COM*16
      LOGICAL*4  LEX, LSUI, FL_A, CHECK_SOLVE_COMPLETE, CHECK_SOLVE_INITIALS
      REAL*8     VAL, XPL_COR(MBUF,2), YPL_COR(MBUF,2), UT1_COR(MBUF,2), &
     &           UTR_COR(MBUF,2), PSI_COR(MBUF,2), EPS_COR(MBUF,2), &
     &           XPL_MAX, YPL_MAX, UT1_MAX, UTR_MAX, PSI_MAX, EPS_MAX
      INTEGER*4  XPL_NCR, YPL_NCR, UT1_NCR, UTR_NCR, PSI_NCR, EPS_NCR
      INTEGER*4  XPL_IND, YPL_IND, UT1_IND, UTR_IND, PSI_IND, EPS_IND
      INTEGER*4  MIN_SPOOL_LEN, NBUF, IS, J1, J2, J3, ISTR, IOUT, IPA, IER
      PARAMETER  ( MIN_SPOOL_LEN = 128 )
      INTEGER*4  ILEN, I_LEN, SYSTEM, UNLINK
!
      SHELL_COM = '/bin/csh -f '
!
! --- Reading template control file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( SIMA%BATCH_CONTROL_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1711, IUER, 'SIMA_DO', 'Error in attempt to read '// &
     &         'template batch control file' )
           RETURN
      END IF
!
! --- Scan template conotrol file
!
      DO 410 J1=1,NBUF
         IPA = INDEX ( BUF(J1), '@arc_line@' )
         IF ( IPA .GT. 0 ) THEN
!
! ----------- If found, replace with actual arc-line
!
              BUF(J1) = '   '//SUPNAM//'  1 !'
              FL_A = .TRUE.
         END IF
 410  CONTINUE
!
      IF ( .NOT. FL_A ) THEN
           CALL ERR_LOG ( 1712, IUER, 'SIMA_DO', 'Signature @arc_line@ '// &
     &         'was not found in a template control file '// &
     &          SIMA%BATCH_CONTROL_FILE )
           RETURN
      END IF
!
! --- Write down the actual control file
!
      CALL ERR_PASS ( IUER, IER )
      CALL WR_TEXT  ( NBUF, BUF, SIMA%TEMP_CONTROL_FILE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1713, IUER, 'SIMA_DO', 'Error in attempt to write '// &
     &         'batch control file' )
           RETURN
      END IF
!
! --- Block Solve user initials
!
      CALL ERR_PASS ( IUER, IER )
      LSUI = CHECK_SOLVE_INITIALS ( 'W', SIMA%SOLVE_INITIALS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1714, -1, 'SIMA_DO', 'Trap of internal control: '// &
     &         'error in checking solve initials '//SIMA%SOLVE_INITIALS )
           RETURN
      END IF
      IF ( .NOT. LSUI ) THEN
           CALL ERR_LOG ( 1715, IUER, 'SIMA_DO', 'Solve initials '// &
     &          SIMA%SOLVE_INITIALS//' are already in use' )
           RETURN
      ENDIF
!
! --- Remove lock of Solve user initials
!
      LOCK_FILE = PRE_SCR_DIR(1:PRE_SD_LEN)//'LOCK'//SIMA%SOLVE_INITIALS
      IS = UNLINK ( LOCK_FILE(1:I_LEN(LOCK_FILE))//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IS, STR )
           CALL ERR_LOG ( 1716, IUER, 'OPA_STW', 'Trap of internal control: '// &
     &         'error '//STR(1:I_LEN(STR))//' during removing lock file' )
           RETURN
      END IF
!
! --- Make the run string for batch  in COMSTR
!
      CALL CLRCH ( COMSTR  )
      COMSTR = SHELL_COM(1:I_LEN(SHELL_COM))//' '// &
     &         PRE_SOL_DIR(1:PRE_SOL_LEN)//'solve '//SIMA%SOLVE_INITIALS//' '// &
     &         SIMA%TEMP_CONTROL_FILE(1:I_LEN(SIMA%TEMP_CONTROL_FILE))// &
     &         ' silent'
!
! --- Launch BATCH
!
      IS = SYSTEM (COMSTR(1:I_LEN(COMSTR))//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IS, STR )
           CALL ERR_LOG ( 1717, IUER, 'SIMA_DO', 'Error '//STR(1:I_LEN(STR))// &
     &         ' in executing command line '//COMSTR )
           RETURN
      END IF
!
! --- Check whether solve run is completed
!
      IF ( .NOT. CHECK_SOLVE_COMPLETE ( SIMA%SOLVE_INITIALS ) ) THEN
           CALL ERR_LOG ( 1718, IUER, 'SIMA_DO', 'Solve run was not '// &
     &                    'successfull' )
           RETURN
      END IF
!
! --- Buuild the name of the spool file
!
      CALL GETENVAR ( 'PSOLVE_SPOOL_DIR', SPOOL_DIR_FILE )
      IF ( ILEN(SPOOL_DIR_FILE) .EQ. 0 ) THEN
           SPOOL_DIR_FILE = SPOOL_DIR
      END IF
      IS = I_LEN(SPOOL_DIR_FILE)
      IF ( SPOOL_DIR_FILE(IS:IS) .EQ. '/' ) SPOOL_DIR_FILE(IS:IS) = ' '
      IS = I_LEN(SPOOL_DIR_FILE)
!
      SPOOL_FILE = SPOOL_DIR_FILE(1:IS)//'/SPLF'//SIMA%SOLVE_INITIALS
!
      INQUIRE ( EXIST=LEX, FILE=SPOOL_FILE )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 1719, IUER, 'SIMA_DO', 'Hm! Spool file '// &
     &          SPOOL_FILE(1:I_LEN(SPOOL_FILE))//' was not '// &
     &         'found. Very strange... ' )
           RETURN
      END IF
!
! --- Read spoll file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( SPOOL_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1720, IUER, 'SIMA_DO', 'Error in attempt to read '// &
     &         'template batch control file' )
           RETURN
      END IF
!
      IF ( NBUF .LT. MIN_SPOOL_LEN ) THEN
           CALL ERR_LOG ( 1721, IUER, 'SIMA_DO', 'Spool file '// &
     &          SPOOL_FILE(1:I_LEN(SPOOL_FILE))//' is almosty empty. '// &
     &         'Probably SOLVE failed to to a good solution' )
           RETURN
      END IF
!
! --- Initialization
!
      CALL CLRCH ( XPL_STR   )
      CALL CLRCH ( YPL_STR   )
      CALL CLRCH ( UT1_STR     )
      CALL CLRCH ( UTR_STR )
      CALL CLRCH ( PSI_STR     )
      CALL CLRCH ( EPS_STR     )
!
! --- Parse spool file. The purposee of this run is to extracte the estimates
! --- of formal uncertainties for pole coordinates, UT1, nutation angles
!
      DO 420 J2=1,NBUF
!
! ------ Extract extimates of EOP sigmas
!
         IF ( BUF(J2)(8:18) .EQ. 'X Wobble  0' ) THEN
              READ  ( UNIT=BUF(J2)(77:87), FMT='(F)' ) VAL
              WRITE ( UNIT=XPL_STR, FMT='(F7.1)'  ) VAL
              CALL CHASHR ( XPL_STR )
           ELSE IF ( BUF(J2)(8:18) .EQ. 'Y Wobble  0' ) THEN
              READ  ( UNIT=BUF(J2)(77:87), FMT='(F)' ) VAL
              WRITE ( UNIT=YPL_STR, FMT='(F7.1)'  ) VAL
              CALL CHASHR ( YPL_STR )
           ELSE IF ( BUF(J2)(8:18) .EQ. 'UT1-TAI   0' ) THEN
              READ  ( UNIT=BUF(J2)(77:87), FMT='(F)' ) VAL
              WRITE ( UNIT=UT1_STR, FMT='(F7.1)'    ) VAL
              CALL CHASHR ( UT1_STR )
           ELSE IF ( BUF(J2)(8:18) .EQ. 'UT1-TAI   1' ) THEN
              READ  ( UNIT=BUF(J2)(77:87), FMT='(F)'  ) VAL
              WRITE ( UNIT=UTR_STR, FMT='(F7.1)' ) VAL
              CALL CHASHR ( UTR_STR )
           ELSE IF ( BUF(J2)(8:35) .EQ. 'Nutation offset in longitude' ) THEN
              READ  ( UNIT=BUF(J2)(67:77), FMT='(F)' ) VAL
              WRITE ( UNIT=PSI_STR, FMT='(F7.1)'     ) VAL
              CALL CHASHR ( PSI_STR )
           ELSE IF ( BUF(J2)(8:35) .EQ. 'Nutation offset in obliquity' ) THEN
              READ  ( UNIT=BUF(J2)(67:77), FMT='(F)' ) VAL
              WRITE ( UNIT=EPS_STR, FMT='(F7.1)'     ) VAL
              CALL CHASHR ( EPS_STR )
         END IF
 420  CONTINUE
!
! --- Build correlation file name
!
      CORL_FILE = PRE_SCR_DIR(1:PRE_SD_LEN)//'CORL'//SIMA%SOLVE_INITIALS
!
      INQUIRE ( EXIST=LEX, FILE=CORL_FILE )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1722, IUER, 'SIMA_DO', 'Hm! Correlations file '// &
     &          CORL_FILE(1:I_LEN(CORL_FILE))//' was not '// &
     &         'found. Very strange... ' )
           RETURN
      END IF
!
! --- Read correlation file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( CORL_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1723, IUER, 'SIMA_DO', 'Error in attempt to read '// &
     &         'correlations file' )
           RETURN
      END IF
!
      XPL_NCR = 0
      YPL_NCR = 0
      UT1_NCR = 0
      UTR_NCR = 0
      PSI_NCR = 0
      EPS_NCR = 0
!
      XPL_IND = 0
      YPL_IND = 0
      UT1_IND = 0
      UTR_IND = 0
      PSI_IND = 0
      EPS_IND = 0
!
      XPL_MAX = 0.0
      YPL_MAX = 0.0
      UT1_MAX = 0.0
      UTR_MAX = 0.0
      PSI_MAX = 0.0
      EPS_MAX = 0.0
!
! --- The purpose of this run us to extract maximal correlation between
! --- the EOPs and something else
!
      DO 430 J3=1,NBUF
         IF ( BUF(J3)(14:14) .EQ. '"' .AND. &
     &        BUF(J3)(35:35) .EQ. '"' .AND. &
     &        BUF(J3)(38:38) .EQ. '"' .AND. &
     &        BUF(J3)(59:59) .EQ. '"' .AND. &
     &        INDEX ( BUF(J3), 'COMPONENT' ) .NE. 0 ) THEN
!
              IF ( INDEX ( BUF(J3), 'X WOBBLE 0' ) .GT. 0 ) THEN
                   XPL_NCR = XPL_NCR + 1
                   READ ( UNIT=BUF(J3)(62:73), FMT='(F)' ) XPL_COR(XPL_NCR,1)
                   XPL_COR(XPL_NCR,2) = J3 + 0.0001
                   IF ( DABS(XPL_COR(XPL_NCR,1)) .GT. XPL_MAX ) THEN
                        XPL_MAX = XPL_COR(XPL_NCR,1)
                        XPL_IND = XPL_NCR
                   END IF
                 ELSE IF ( INDEX ( BUF(J3), 'Y WOBBLE 0' ) .GT. 0 ) THEN
                   YPL_NCR = YPL_NCR + 1
                   READ ( UNIT=BUF(J3)(62:73), FMT='(F)' ) YPL_COR(YPL_NCR,1)
                   YPL_COR(YPL_NCR,2) = J3 + 0.0001
                   IF ( DABS(YPL_COR(YPL_NCR,1)) .GT. YPL_MAX ) THEN
                        YPL_MAX = YPL_COR(YPL_NCR,1)
                        YPL_IND = XPL_NCR
                   END IF
                 ELSE IF ( INDEX ( BUF(J3), 'UT1-TAI  0' ) .GT. 0 ) THEN
                   UT1_NCR = UT1_NCR + 1
                   READ ( UNIT=BUF(J3)(62:73), FMT='(F)' ) UT1_COR(UT1_NCR,1)
                   UT1_COR(UT1_NCR,2) = J3 + 0.0001
                   IF ( DABS(UT1_COR(UT1_NCR,1)) .GT. UT1_MAX ) THEN
                        UT1_MAX = UT1_COR(UT1_NCR,1)
                        UT1_IND = YPL_NCR
                   END IF
                 ELSE IF ( INDEX ( BUF(J3), 'UT1-TAI  1' ) .GT. 0 ) THEN
                   UTR_NCR = UTR_NCR + 1
                   READ ( UNIT=BUF(J3)(62:73), FMT='(F)' ) UTR_COR(UTR_NCR,1)
                   UTR_COR(UTR_NCR,2) = J3 + 0.0001
                   IF ( DABS(UTR_COR(UTR_NCR,1)) .GT. UTR_MAX ) THEN
                        UTR_MAX = UTR_COR(UTR_NCR,1)
                        UTR_IND = UT1_NCR
                   END IF
                 ELSE IF ( INDEX ( BUF(J3), 'LONGITUDE NUTATION' ) .GT. 0 ) THEN
                   PSI_NCR = PSI_NCR + 1
                   READ ( UNIT=BUF(J3)(62:73), FMT='(F)' ) PSI_COR(PSI_NCR,1)
                   PSI_COR(PSI_NCR,2) = J3 + 0.0001
                   IF ( DABS(PSI_COR(PSI_NCR,1)) .GT. PSI_MAX ) THEN
                        PSI_MAX = PSI_COR(PSI_NCR,1)
                        PSI_IND = PSI_NCR
                   END IF
                 ELSE IF ( INDEX ( BUF(J3), 'OBLIQUITY NUTATION' ) .GT. 0 ) THEN
                   EPS_NCR = EPS_NCR + 1
                   READ ( UNIT=BUF(J3)(62:73), FMT='(F)' ) EPS_COR(EPS_NCR,1)
                   EPS_COR(EPS_NCR,2) = J3 + 0.0001
                   IF ( DABS(EPS_COR(EPS_NCR,1)) .GT. EPS_MAX ) THEN
                        EPS_MAX = EPS_COR(EPS_NCR,1)
                        EPS_IND = EPS_NCR
                   END IF
              END IF
         END IF
 430  CONTINUE
!
! === Now time came to start formatiing results.
!
! --- Making header
!
      IOUT = 1
      CALL CLRCH ( BUF(IOUT) )
      BUF(IOUT) = '              Simulation for experiment '//SUPNAM
!
      IOUT = IOUT + 1
      CALL CLRCH ( BUF(IOUT) )
      BUF(IOUT) = &
     & '-------------------------------------------------------------------|'
!
      IOUT = IOUT + 1
      CALL CLRCH ( BUF(IOUT) )
      BUF(IOUT) = &
     & '|            |                |  Maximum    |     Correlating      |'
!
      IOUT = IOUT + 1
      CALL CLRCH ( BUF(IOUT) )
      BUF(IOUT) = &
     & '|   EOP      |     Formal     | Correlation |     Parameter        |'
!
      IOUT = IOUT + 1
      CALL CLRCH ( BUF(IOUT) )
      BUF(IOUT) = &
     & '| Parameters |     Error      |             |                      |'
!
      IOUT = IOUT + 1
      CALL CLRCH ( BUF(IOUT) )
      BUF(IOUT) = &
     & '|------------+----------------+-------------+----------------------|'
!
      IOUT = IOUT + 1
      CALL CLRCH ( BUF(IOUT) )
      BUF(IOUT) = &
     & '|            |                |             |                      |'
!
! --- Making the table of formal uncetainties and the maximal correlations
!
      IOUT = IOUT + 1
      CALL CLRCH ( BUF(IOUT) )
      BUF(IOUT) = '| X-Wobble   |'//XPL_STR//' muas    |'
!
      IF ( XPL_IND .GT. 0 ) THEN
           WRITE ( UNIT=BUF(IOUT)(32:39), FMT='(F5.2)' ) XPL_COR(XPL_IND,1)
           CALL CHASHR ( BUF(IOUT)(32:39) )
           IF ( BUF(IOUT)(19:20) .EQ. ' .' ) BUF(IOUT)(19:20) = '0.'
           IF ( BUF(IOUT)(36:37) .EQ. ' .' ) BUF(IOUT)(36:37) = '0.'
           IF ( BUF(IOUT)(36:37) .EQ. '-.' ) BUF(IOUT)(35:37) = '-0.'
           BUF(IOUT)(45:45) = '|'
           ISTR = XPL_COR(XPL_IND,2)
           IF ( BUF(ISTR)(39:48) .EQ. 'X WOBBLE 0' ) THEN
                BUF(IOUT)(47:66) = BUF(ISTR)(15:34)
              ELSE
                BUF(IOUT)(47:66) = BUF(ISTR)(39:58)
           END IF
           BUF(IOUT)(68:68) = '|'
      END IF
!
      IOUT = IOUT + 1
      CALL CLRCH ( BUF(IOUT) )
      BUF(IOUT) = '| Y-Wobble   |'//YPL_STR//' muas    |'
!
      IF ( YPL_IND .GT. 0 ) THEN
           WRITE ( UNIT=BUF(IOUT)(32:39), FMT='(F5.2)' ) YPL_COR(YPL_IND,1)
           CALL CHASHR ( BUF(IOUT)(32:39) )
           IF ( BUF(IOUT)(19:20) .EQ. ' .' ) BUF(IOUT)(19:20) = '0.'
           IF ( BUF(IOUT)(36:37) .EQ. ' .' ) BUF(IOUT)(36:37) = '0.'
           IF ( BUF(IOUT)(36:37) .EQ. '-.' ) BUF(IOUT)(35:37) = '-0.'
           BUF(IOUT)(45:45) = '|'
           ISTR = YPL_COR(YPL_IND,2)
           IF ( BUF(ISTR)(39:48) .EQ. 'Y WOBBLE 0' ) THEN
                BUF(IOUT)(47:66) = BUF(ISTR)(15:34)
              ELSE
                BUF(IOUT)(47:66) = BUF(ISTR)(39:58)
           END IF
           BUF(IOUT)(68:68) = '|'
      END IF
!
      IOUT = IOUT + 1
      CALL CLRCH ( BUF(IOUT) )
      BUF(IOUT) = '| UT1-TAI    |'//UT1_STR//' musec   |'
!
      IF ( UT1_IND .GT. 0 ) THEN
           WRITE ( UNIT=BUF(IOUT)(32:39), FMT='(F5.2)' ) UT1_COR(UT1_IND,1)
           CALL CHASHR ( BUF(IOUT)(32:39) )
           IF ( BUF(IOUT)(19:20) .EQ. ' .' ) BUF(IOUT)(19:20) = '0.'
           IF ( BUF(IOUT)(36:37) .EQ. ' .' ) BUF(IOUT)(36:37) = '0.'
           IF ( BUF(IOUT)(36:37) .EQ. '-.' ) BUF(IOUT)(35:37) = '-0.'
           BUF(IOUT)(45:45) = '|'
           ISTR = UT1_COR(UT1_IND,2)
           IF ( BUF(ISTR)(39:48) .EQ. 'UT1-TAI  0' ) THEN
                BUF(IOUT)(47:66) = BUF(ISTR)(15:34)
              ELSE
                BUF(IOUT)(47:66) = BUF(ISTR)(39:58)
           END IF
           BUF(IOUT)(68:68) = '|'
      END IF
!
      IOUT = IOUT + 1
      CALL CLRCH ( BUF(IOUT) )
      BUF(IOUT) = '| UT1-Rate   |'//UTR_STR//' musec/d |'
!
      IF ( UTR_IND .GT. 0 ) THEN
           WRITE ( UNIT=BUF(IOUT)(32:39), FMT='(F5.2)' ) UTR_COR(UTR_IND,1)
           CALL CHASHR ( BUF(IOUT)(32:39) )
           IF ( BUF(IOUT)(19:20) .EQ. ' .' ) BUF(IOUT)(19:20) = '0.'
           IF ( BUF(IOUT)(36:37) .EQ. ' .' ) BUF(IOUT)(36:37) = '0.'
           IF ( BUF(IOUT)(36:37) .EQ. '-.' ) BUF(IOUT)(35:37) = '-0.'
           BUF(IOUT)(45:45) = '|'
           ISTR = UTR_COR(UTR_IND,2)
           IF ( BUF(ISTR)(39:48) .EQ. 'UT1-TAI  1' ) THEN
                BUF(IOUT)(47:66) = BUF(ISTR)(15:34)
              ELSE
                BUF(IOUT)(47:66) = BUF(ISTR)(39:58)
           END IF
           BUF(IOUT)(68:68) = '|'
      END IF
!
      IOUT = IOUT + 1
      CALL CLRCH ( BUF(IOUT) )
      BUF(IOUT) = '| Psi-nutat  |'//PSI_STR//' muas    |'
!
      IF ( PSI_IND .GT. 0 ) THEN
      END IF
!
      IOUT = IOUT + 1
      CALL CLRCH ( BUF(IOUT) )
      BUF(IOUT) = '| Eps-nutat  |'//EPS_STR//' muas    |'
!
      IF ( EPS_IND .GT. 0 ) THEN
      END IF
!CCCC
      IOUT = IOUT + 1
      CALL CLRCH ( BUF(IOUT) )
      BUF(IOUT) = &
     & '|------------|----------------|-------------|----------------------|'
!
! --- Write down the table into the file
!
      CALL ERR_PASS ( IUER, IER )
      CALL WR_TEXT  ( IOUT, BUF, SIMA%SIMA_OUTPUT_FILE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1724, IUER, 'SIMA_DO', 'Error in attempt to write '// &
     &         'in the ouput file' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SIMA_DO  #!#
