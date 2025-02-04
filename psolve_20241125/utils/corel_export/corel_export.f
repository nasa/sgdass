      PROGRAM    COREL_EXPORT
! ************************************************************************
! *                                                                      *
! *   Usage: corel_epoxrt -d <directory> -c <config_file>] [-master]     *
! *                       [-quiet] [-help]                               *
! *                                                                      *
! *  ### 07-APR-2005  COREL_EXPORT  v1.1 (c) L. Petrov  09-JUN-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'corel_export_help.i'
      INCLUDE   'corel_export.i'
      TYPE      ( CEX__TYPE ) :: CEX
      INTEGER*4  M_ARG
      PARAMETER  ( M_ARG  = 5 )
      LOGICAL*4  HELP_FL, MASTER_FL, QUIET_FL  
      CHARACTER  DATA_DIR*128, CONF_FIL*128
      CHARACTER  USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128
      CHARACTER  ARG_STR*128
      INTEGER*4  IARG, IVRB, J1, IL, N_ARG, IUER
      CHARACTER, EXTERNAL :: GET_VERSION*54
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LTM_DIF
!
      N_ARG= IARGC ()
      IF ( N_ARG .LT. 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: corel_export -d <directory> '// &
     &                        ' -c <config_file>] [-help] [-master] [-quiet]'
           CALL EXIT ( 1 )
      END IF
!
! --- Initialization
!
      HELP_FL   = .FALSE.
      MASTER_FL = .FALSE.
      QUIET_FL  = .FALSE.
      CALL CLRCH ( DATA_DIR   )
      CALL CLRCH ( CONF_FIL   )
!
! --- Parse arguments from the command line
!
      IARG = 0
      IVRB = 1
      DO 410 J1=1,M_ARG
         IARG = IARG+1
         IF ( IARG .GT. N_ARG ) GOTO 810 ! No arguments any more
!
! ------ Get the IARG-th argument
!
         CALL CLRCH  ( ARG_STR )
         CALL GETARG ( IARG, ARG_STR )
         IF ( ARG_STR(1:2) .EQ. '-c' ) THEN
!
! ----------- Schedule name
!
              IARG = IARG+1
              IF ( IARG .GT. N_ARG ) THEN
                   CALL ERR_LOG ( 4801, -2, 'COREL_EXPORT', 'No arguments '// &
     &                 'in command line after -c')
                   CALL EXIT ( 2 )
                ELSE
                   CALL GETARG ( IARG, CONF_FIL )
              END IF
           ELSE IF ( ARG_STR(1:2) .EQ. '-d' ) THEN
!
! ----------- Directory name
!
              IARG = IARG+1
              IF ( IARG .GT. N_ARG ) THEN
                   CALL ERR_LOG ( 4802, -2, 'COREL_EXPORT', 'No arguments '// &
     &                 'in command line after -d')
                   CALL EXIT ( 2 )
                ELSE
                   CALL GETARG ( IARG, DATA_DIR )
                   IL = I_LEN(DATA_DIR)
                   IF ( DATA_DIR(IL:IL) .NE. '/' ) THEN
                        DATA_DIR = DATA_DIR(1:IL)//'/'
                   END IF
              END IF
           ELSE IF ( ARG_STR(1:2) .EQ. '-h' .OR. &
     &               ARG_STR(1:2) .EQ. '-?'      ) THEN
              HELP_FL = .TRUE.
           ELSE IF ( ARG_STR(1:2) .EQ. '-m' ) THEN
              MASTER_FL   = .TRUE.
           ELSE IF ( ARG_STR(1:2) .EQ. '-q' ) THEN
              IVRB = 0
           ELSE
              CALL ERR_LOG ( 4803, -2, 'COREL_EXPORT', 'Unrecognized option '// &
     &             ARG_STR(1:I_LEN(ARG_STR))//' was found. Some of -d, '// &
     &            '-c, -help, -master, -quiet were expected' )
              CALL EXIT ( 2 )
         END IF
 410  CONTINUE
 810  CONTINUE
!
! --- Get information about a user
!
      CALL GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
      IF ( ILEN(USER_REALNAME) .EQ. 0 ) USER_REALNAME = 'User '//USER_NAME
      IF ( HELP_FL ) THEN
!
! -------- Show help file
!
           IUER = -1
           CALL SHOW_TEXT_FILE_COL ( COREL_EXPORT_HELP_FILE, 'corel_export', &
     &                               0, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 4804, -2, 'COREL_EXPORT', 'Error in attempt '// &
     &              'to display help file '//COREL_EXPORT_HELP_FILE )
                CALL EXIT ( 2 )
           END IF
           CALL EXIT ( 0 )
      END IF
      IF ( ILEN(CONF_FIL) == 0 ) THEN
           WRITE ( 6, * ) 'Configuration file should be provided'
           CALL EXIT ( 1 ) 
      END IF
!
! --- Parse configuration file and put configuration parameters in record
! --- CEX
!
      IUER = -1
      CALL CEX_CONFIG ( CONF_FIL, CEX, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4805, -2, 'COREL_EXPORT', 'Error in parsing '// &
     &         'configuration file '//CONF_FIL )
           CALL EXIT ( 2 )
      END IF
!
      IF ( MASTER_FL ) THEN
!
! -------- In "master-mode" we read the set of master files from the IVS
! -------- Data Center
!
           IF ( IVRB .GE. 1 ) WRITE ( 6, '(A)' ) 'COREL_EXPORT: Retrieving '// &
     &                                           'master files... '
           IUER = -1
           CALL GET_MASTER ( CEX%WGET_EXE, CEX%URL_IVSCONTROL, CEX%MASTER_DIR, &
     &                       IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 4806, -2, 'COREL_EXPORT', 'Error in attempt to '// &
     &              'retrieve the set of master files from the IVS Data Center')
                CALL EXIT ( 2 )
           END IF
      END IF
      IF ( ILEN(DATA_DIR) == 0 ) THEN
           WRITE ( 6, * ) 'Data directory should be provided' 
           CALL EXIT ( 1 ) 
      END IF
!
      IUER = -1
      CALL CEX_CHECK ( CEX, DATA_DIR, IVRB, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4807, -2, 'COREL_EXPORT', 'Error in checking '// &
     &         'the directory tree '//CONF_FIL )
           CALL EXIT ( 2 )
      END IF
!
      IUER = -1
      CALL CEX_BUILD_COMMAND ( CEX, DATA_DIR, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4808, -2, 'COREL_EXPORT', 'Error in an attempt '// &
     &         'to build command for correlator output submission' )
           CALL EXIT ( 2 ) 
      END IF
!
      IUER = -1
      CALL CEX_SUBMIT ( CEX, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4809, -2, 'COREL_EXPORT', 'Error in an attempt '// &
     &         'to execute command for correlator output submission' )
           CALL EXIT ( 2 ) 
      END IF
!
      END  PROGRAM  COREL_EXPORT
