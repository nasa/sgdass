      PROGRAM    SIMA_MAIN
! ************************************************************************
! *                                                                      *
! *   Program SIMA is a tool for making simulation of VLBI experiment    *
! *   for evaluation of predicted formal errors of determination of      *
! *   EOP and maximal correlations between EOP and station coordinates.  *
! *                                                                      *
! *   Usage: sima -i <solve_initials> -c <configuration_file>            *
! *               -s <superfile_name>                                    *
! *                                                                      *
! *   where superfile_name is the name of the experiment in the format   *
! *   YYMMMDDSS where YY -- year, MMM - month, DD -- day, SS -- suffix.  *
! *   It is recommended that the first letter of the suffix  be Z --     *
! *   this letter is reserved for simulations only.                      *
! *                                                                      *
! *   It is assumed that the simulation superfile has been created by    *
! *   sskedh BEFORE calling sima.                                        *
! *                                                                      *
! *   sima parses input line, then                                       *
! *                                                                      *
! *   1) Reads configuration file.                                       *
! *   2) Checks whether superfile is in SUPCAT;                          *
! *   3) Creates the control file for batch SOLVE by using template      *
! *      control file defined in configuration file under the field      *
! *      BATCH_CONTROL_FILE by adding the superfile in the arcfile.      *
! *   4) Run Batch Solve with the control file which has been created.   *
! *   5) Parses spool file and correlation file.                         *
! *      It extracts the estimates of the formal errors of EOP           *
! *      It finds the maximum correlations between each component of EOP *
! *      and station coordinates.                                        *
! *   6) Generates the table of results and writes it down in the output *
! *      file.                                                           *
! *                                                                      *
! *  ### 02-AUG-2000   SIMA_MAIN   v1.0 (c)  L. Petrov  31-OCT-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'sima.i'
      INCLUDE   'precm.i'
      TYPE ( SIMA__STRU ) ::  SIMA
      CHARACTER  GET_VERSION*54, STR*54, SUPNAM*10, CONFIG_FILE*128, &
     &           SOLVE_INIT*2
      LOGICAL*4  LEX, LSUI, CHECK_SOLVE_INITIALS
      INTEGER*4  M_ARG, M_PAIR
      PARAMETER  ( M_ARG  = 6 )
      PARAMETER  ( M_PAIR = 3 )
      INTEGER*2  IP_I2(5)
      CHARACTER  ARG_ARR(M_ARG)*128
      INTEGER*4  NUMARG, N_ARG, IC, J1, J2, IUER
      INTEGER*4  IARGC, I_LEN, ILEN
!
      INCLUDE   'sima_version.i'
!
! --- Set RMPAR bits and names of some Solve directories
!
      CALL NOUT ( 10, IP_I2 )
      CALL SBIT ( IP_I2(1), INT2(1), INT2(0) )  ! no test version
      CALL SBIT ( IP_I2(2), INT2(1), INT2(1) )  ! spooling on
      CALL SBIT ( IP_I2(3), INT2(1), INT2(1) )  ! Batch
      CALL SBIT ( IP_I2(3), INT2(1), INT2(2) )  ! Minout on
      CALL UNPACK_RMPAR ( IP_I2 )
!
      NUMARG = IARGC ()
!
! --- Get version
!
      STR = GET_VERSION ()
!
! --- Get argument's list
!
      IF ( NUMARG .EQ. 0 ) THEN
           WRITE ( 6, 110 )  STR(1:I_LEN(STR))
 110       FORMAT ( 1X,A/1X,'Usage:  sima -i <Solve_initials> ', &
     &                   '[-c <control file>] -s <superfile_name>' )
           CALL EXIT()
         ELSE
!
! -------- Get arguments from the command line
!
           N_ARG = MIN(M_ARG,NUMARG)
           DO 410 J1=1,N_ARG
              CALL CLRCH  (     ARG_ARR(J1) )
              CALL GETARG ( J1, ARG_ARR(J1) )
 410       CONTINUE
      END IF
!
! --- Parse arguments from the command line
!
      CALL CLRCH ( CONFIG_FILE )
      CALL CLRCH ( SOLVE_INIT  )
      CALL CLRCH ( SUPNAM      )
      IC=1
      DO 420 J2=1,M_PAIR
         IF ( IC .GT. N_ARG ) GOTO 820
         IF ( ARG_ARR(IC)(1:2) .EQ. '-c' ) THEN
!
! ----------- Configuration file is specified
!
              IF ( N_ARG .LT. IC+1 ) THEN
                   CALL ERR_LOG ( 1701, -1, 'SIMA_MAIN', 'No arguments in '// &
     &                 'command line after -c')
                   CALL EXIT ( 3 )
                ELSE
                   CONFIG_FILE = ARG_ARR(IC+1)
                   IC = IC+2
              END IF
           ELSE IF ( ARG_ARR(IC)(1:2) .EQ. '-s' ) THEN
!
! ----------- Superfile name is specified
!
              IF ( N_ARG .LT. IC+1 ) THEN
                   CALL ERR_LOG ( 1702, -1, 'SIMA_MAIN', 'No arguments in '// &
     &                 'command line after -d')
                   CALL EXIT ( 3 )
                ELSE
                   SUPNAM = ARG_ARR(IC+1)
                   IC = IC+2
              END IF
           ELSE IF ( ARG_ARR(IC)(1:2) .EQ. '-i' ) THEN
!
! ----------- Solve user initials are specified
!
              IF ( N_ARG .LT. IC+1 ) THEN
                   CALL ERR_LOG ( 1703, -1, 'SIMA_MAIN', 'No arguments in '// &
     &                 'command line after -i')
                   CALL EXIT ( 3 )
                ELSE
                   SOLVE_INIT = ARG_ARR(IC+1)
                   CALL TRAN ( 11, SOLVE_INIT, SOLVE_INIT ) ! to upper register
                   IC = IC+2
              END IF
           ELSE
!
! ----------- Unrecognized argument
!
              CALL CLRCH ( STR )
              CALL INCH  ( IC, STR )
              CALL ERR_LOG ( 1704, -1, 'SIMA_MAIN', 'Unrecognized '// &
     &             STR(1:I_LEN(STR))//'-th argument: '// &
     &             ARG_ARR(IC)(1:I_LEN(ARG_ARR(IC)))// &
     &            '. -c, -s, -i, -v were expected' )
              CALL EXIT ( 4 )
         END IF
 420  CONTINUE
 820  CONTINUE
!
! --- Check validity of the arguments
!
      IF ( ILEN(SOLVE_INIT) .EQ. 0 ) THEN
           CALL ERR_LOG ( 1705, -1, 'SIMA_MAIN', 'Solve user initials must '// &
     &         'be specified' )
           CALL EXIT ( 3 )
      END IF
      IF ( ILEN(SUPNAM) .EQ. 0 ) THEN
           CALL ERR_LOG ( 1706, -1, 'SIMA_MAIN', 'Superfile name must be '// &
     &         'specified' )
           CALL EXIT ( 3 )
      END IF
      IF ( SUPNAM(1:1) .NE. '$' ) SUPNAM = '$'//SUPNAM
!
      IF ( ILEN(CONFIG_FILE) .EQ. 0 ) THEN
           CONFIG_FILE = PRE_ROOT_DIR(1:PRE_ROOT_LEN)//'/local/'// &
     &                   CENTER_LABEL//'.sim'
      END IF
      INQUIRE ( EXIST=LEX, FILE=CONFIG_FILE )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 1707, -1, 'SIMA_MAIN', 'Configuration file for '// &
     &          'sima: '//CONFIG_FILE(1:I_LEN(CONFIG_FILE))//' was not found' )
           CALL EXIT ( 1 )
      END IF
!
! --- Read configuration file and store configuration parameters in SIMA
! --- object
!
      IUER = -1
      CALL SIMA_CONFIG ( CONFIG_FILE, SIMA, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1708, -1, 'SIMA_MAIN', 'Error in parsing '// &
     &         'configuration file for sima: '// &
     &          CONFIG_FILE(1:I_LEN(CONFIG_FILE)) )
           CALL EXIT ( 1 )
      END IF
!
! --- Check Solve user initials
!
      IUER = -1
      LSUI = CHECK_SOLVE_INITIALS ( 'W', SOLVE_INIT(1:I_LEN(SOLVE_INIT)), IUER )
      IF ( .NOT. LSUI ) THEN
           CALL ERR_LOG ( 1709, -1, 'SIMA_MAIN', 'Solve user initials '// &
     &          SOLVE_INIT(1:I_LEN(SOLVE_INIT))//' are wrong or in use' )
           CALL EXIT ( 4 )
      END IF
      SIMA%SOLVE_INITIALS = SOLVE_INIT
!
! --- Check: whether control file exists?
!
      INQUIRE ( EXIST=LEX, FILE=SIMA%BATCH_CONTROL_FILE )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 1710, -1, 'SIMA_MAIN', 'Template batch control '// &
     &          'file '// &
     &           SIMA%BATCH_CONTROL_FILE(1:I_LEN(SIMA%BATCH_CONTROL_FILE))// &
     &          ' was not found' )
           CALL EXIT ( 1 )
      END IF
!
! --- Remove the output file and temporary files
!
      INQUIRE ( EXIST=LEX, FILE=SIMA%SIMA_OUTPUT_FILE )
      IF ( LEX ) THEN
           CALL UNLINK ( SIMA%SIMA_OUTPUT_FILE(1:I_LEN(SIMA%SIMA_OUTPUT_FILE))// &
     &                   CHAR(0) )
      END IF
!
      INQUIRE ( EXIST=LEX, FILE=SIMA%TEMP_CONTROL_FILE )
      IF ( LEX ) THEN
           CALL UNLINK ( SIMA%TEMP_CONTROL_FILE(1:I_LEN(SIMA%TEMP_CONTROL_FILE))//CHAR(0) )
      END IF
!
! --- Check wether SUPNAM is in the superfile list
!
      IUER = -1
      CALL CHECK_SUPCAT ( SUPNAM, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1711, -1, 'SIMA_MAIN', 'Error in checking '// &
     &         'supferfile name '//SUPNAM(1:I_LEN(SUPNAM)) )
           CALL EXIT ( 1 )
      END IF
!
! --- Execute SIMA
!
      IUER = -1
      CALL SIMA_DO ( SUPNAM, SIMA, IUER )
      IF ( IUER .EQ. 0 ) THEN
           WRITE ( 6, * ) 'Results are in file '// &
     &             SIMA%SIMA_OUTPUT_FILE(1:I_LEN(SIMA%SIMA_OUTPUT_FILE))
           CALL EXIT ( 0 )
        ELSE IF ( IUER .NE. 0 ) THEN
           CALL EXIT ( 1 )
      END IF
!
      END  !#!  SIMA_MAIN  #!#
