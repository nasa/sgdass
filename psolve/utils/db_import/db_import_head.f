      PROGRAM    DB_IMPORT_HEAD
! ************************************************************************
! *                                                                      *
! *   Program  DB_IMPORT  downloads database files from the IVS Data     *
! *   Center to the incoming directory in according with specifications  *
! *   from the configuration file.                                       *
! *                                                                      *
! *   DB_IMPORT makes the following steps:                               *
! *     1) reads configuration file and checks its parameters            *
! *     2) retrieves list of the database files in the IVS Data Center   *
! *        for the specified period;                                     *
! *     3) gets the list of database files which are in the current      *
! *        catalogue system;                                             *
! *     4) gets the list of database files which are in the incoming     *
! *        directory and may necessarily in the catalogue system;        *
! *     5) reads get_lists and notget_lists which specifies which        *
! *        databases should be downloaded and which databases should not *
! *        be downloaded. Both lists may contains (and usually contains) *
! *        wildcard symbols;                                             *
! *     6) generates lists of the databases which are to be downloaded:  *
! *        -- in the IVS Data Center database list  AND                  *
! *        -- not in the current catalogue system   AND                  *
! *        -- not in the incoming directory         AND                  *
! *        -- in the get_list                       AND                  *
! *        -- not in the noget_list.                                     *
! *     7) if this list is empty then end of work. If not then URL list  *
! *        of the databases to be downloaded is build and written in the *
! *        temporary file. These files are downloaded by using wget      *
! *        program. If database files are compressed by gzip they are    *
! *        uncompressed.                                                 *
! *     8) e-mail message is sent about successful downloading database  *
! *        files to the user specified in the configuration file. End of *
! *        work.                                                         *
! *                                                                      *
! *        Usage: db_import [-h] -c <config_file> [-v <verbosity>]       *
! *                                                                      *
! *  Options:                                                            *
! *                                                                      *
! *  -h -- if specified then a short help file is printed at the screen. *
! *  -c -- file name of the configuration file. Refer to                 *
! *        db_import_02.txt for specifications of the configuration file.*
! *  -v -- (optional) specifies verbosity level.                         *
! *        0 -- silent mode -- only error messages will be printed.      *
! *        1 -- informational messages about progress appear at the      *
! *             screen.                                                  *
! *        2 -- debugging mode: the lists are printed in the output      *
! *             files.                                                   *
! *        3 -- debugging mode: db_import in addition to printing the    *
! *             output file prints the progress of retrieving            *
! *             information from the local VLBI catalogue.               *
! *                                                                      *
! *  DB_IMPORT returns error code 0 if it successfully reach the end of  *
! *  work: successfully downloads files and send e-mail message or it    *
! *  finds that no files should be downloaded. Otherwise it returns      *
! *  non-zero error code and prints the error message at the screen.     *
! *                                                                      *
! * ### 17-OCT-2000  DB_IMPORT_HEAD  v1.3 (c) L. Petrov 03-SEP-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'db_import.i'
      TYPE ( DBI__STRU ) ::  DBI
      INTEGER*4    M_ARG, M_PAIR
      PARAMETER  ( M_ARG  = 5 )
      PARAMETER  ( M_PAIR = 2 )
      CHARACTER  ARG_ARR(M_ARG)*128, STR*256, SOLVE_HELP_DIR*256, HELP_FILE*256, &
     &           CONFIG_FILE*256
      LOGICAL*4  LEX
      CHARACTER  CH_DBS(M_DBS)*128, CT_DBS(M_DBS)*20, CI_DBS(M_DBS)*20, &
     &           CG_DBS(M_DBS)*20,  CN_DBS(M_DBS)*20, C_DBS(M_DBS)*128, &
     &           C_URL(M_DBS)*128
      INTEGER*4  NUMARG, LH_DBS, LT_DBS, LI_DBS, LG_DBS, LN_DBS, L_DBS, &
     &           IC, N_ARG, IVRB, J1, J2, IUER
      ADDRESS__TYPE :: DIR_DESC
      INTEGER*4, EXTERNAL ::  IARGC, I_LEN, ILEN
      ADDRESS__TYPE, EXTERNAL :: OPENDIR
!
      INCLUDE   'db_import_version.i' ! Set revision date of the current version
!
! --- Get numbers of command-string arguments
!
      NUMARG = IARGC ()
!
      IVRB = 0
      CALL CLRCH ( CONFIG_FILE )
!
      IF ( NUMARG .EQ. 0 ) THEN
           WRITE ( 6, 110 )
 110       FORMAT ( 'Usage: db_import [-h] -c <config_file> [-v <verbosity>]' )
           CALL EXIT ( 1 )
         ELSE
!
! -------- Set values of
! -------- a) SOLVE_HELP_DIR
! -------- We first check environment variables.. If they are not set up we
! -------- get system-wide default
!
           CALL CLRCH  ( SOLVE_HELP_DIR )
           CALL GETENVAR ( 'PSOLVE_HELP_DIR', SOLVE_HELP_DIR )
!
           IF ( ILEN(SOLVE_HELP_DIR) .EQ. 0 ) THEN
                SOLVE_HELP_DIR = SOLVE_HELP_DIR ! get system default
           END IF
           IF ( SOLVE_HELP_DIR(ILEN(SOLVE_HELP_DIR):ILEN(SOLVE_HELP_DIR)) .NE. '/' ) THEN
                SOLVE_HELP_DIR = SOLVE_HELP_DIR(1:ILEN(SOLVE_HELP_DIR))//'/'
           END IF
!
! -------- Check: whether the SOLVE_HELP_DIR directory exists
!
           DIR_DESC = OPENDIR ( SOLVE_HELP_DIR(1:I_LEN(SOLVE_HELP_DIR))//CHAR(0) )
           IF ( DIR_DESC .EQ. 0 ) THEN
                CALL ERR_LOG ( 5701, -1, 'DB_IMPORT_HEAD', 'Wrong value of '// &
     &              'environment variable SOLVE_HELP_DIR (or system-wide default '// &
     &              'SOLVE_HELP_DIR: '//SOLVE_HELP_DIR(1:I_LEN(SOLVE_HELP_DIR))// &
     &              ' -- directory does not exist' )
                CALL EXIT ( 2 )
              ELSE 
                CALL CLOSEDIR ( %VAL(DIR_DESC) )
           END IF
!
! -------- Get argeuments from the command line
!
           N_ARG = MIN(M_ARG,NUMARG)
           DO 410 J1=1,N_ARG
              CALL CLRCH  (     ARG_ARR(J1) )
              CALL GETARG ( J1, ARG_ARR(J1) )
!
              IF ( ARG_ARR(J1)(1:2) .EQ. '-h'  .OR. &
     &             ARG_ARR(J1)(1:2) .EQ. '-?'        ) THEN
!
! ---------------- Help option was requested. Get help file name
!
                   CALL CLRCH ( HELP_FILE )
                   HELP_FILE = SOLVE_HELP_DIR(1:ILEN(SOLVE_HELP_DIR))//HELP01__DBI
!
! ---------------- Show help file
!
                   IUER = -1
                   CALL SHOW_TEXT_FILE_COL ( HELP_FILE, 'DB_IMPORT', 0, IUER )
                   IF ( IUER .NE. 0 ) THEN
                        CALL ERR_LOG ( 5702, -1, 'DBI_IMPORT_HEAD', 'Error '// &
     &                      'in attempt to display help file' )
                        CALL EXIT ( 2 )
                   END IF
                   CALL EXIT()
              END IF
 410       CONTINUE
      END IF
!
! --- Parse arguments from the command line
!
      IC=1
      DO 420 J2=1,M_PAIR
         IF ( IC .GT. N_ARG ) GOTO 820
         IF ( ARG_ARR(IC)(1:2) .EQ. '-c' ) THEN
!
! ----------- Configuration file name was suppplied
!
              IF ( N_ARG .LT. IC+1 ) THEN
                   CALL ERR_LOG ( 5703, -1, 'DBI_IMPORT_HEAD', &
     &                 'No arguments in command line after -c' )
                   CALL EXIT ( 2 )
                ELSE
                   CONFIG_FILE = ARG_ARR(IC+1)
                   IC = IC+2
              END IF
           ELSE IF ( ARG_ARR(IC)(1:2) .EQ. '-v' ) THEN
!
! ----------- Verbosity level parameter is specified
!
              IF ( N_ARG .LT. IC+1 ) THEN
                   CALL ERR_LOG ( 5704, -1, 'DBI_IMPORT_HEAD', 'No arguments '// &
     &                 'in command line after -v' )
                   CALL EXIT ( 2 )
                ELSE
                   CALL CHIN ( ARG_ARR(IC+1), IVRB )
                   IF ( IVRB .LT. IVRB__MIN  .OR.  IVRB .GT. IVRB__MAX ) THEN
                        CALL ERR_LOG ( 5705, -1, 'DBI_IMPORT_HEAD', 'Wrong '// &
     &                      'value of -v argument: '// &
     &                       ARG_ARR(IC+1)(1:I_LEN(ARG_ARR(IC+1)))// &
     &                      '. An integer in the range '//CVRB_RANGE// &
     &                      ' was expected' )
                        CALL EXIT ( 2 )
                   END IF
                   IC = IC+2
              END IF
           ELSE
!
! ----------- Unrecognized argument
!
              CALL CLRCH ( STR )
              CALL INCH  ( IC, STR )
              CALL ERR_LOG ( 5706, -1, 'DBI_IMPORT_HEAD', 'Unrecognized '// &
     &             STR(1:I_LEN(STR))//'-th argument: '// &
     &             ARG_ARR(IC)(1:I_LEN(ARG_ARR(IC)))// &
     &            '. -h, -c, -v were expected' )
              CALL EXIT ( 2 )
         END IF
 420  CONTINUE
 820  CONTINUE
!
      IF ( ILEN(CONFIG_FILE) .EQ. 0 ) THEN
           CALL ERR_LOG ( 5707, -1, 'DBI_IMPORT_HEAD', 'Configuration file '// &
     &         'was not supplied in the command line' )
           CALL EXIT ( 2 )
      END IF
!
! --- 0) Get configuration parameters of DB_IMPORT utility
!
      IUER = -1
      CALL DBI_CONFIG ( CONFIG_FILE, DBI, IUER )
      IF ( IUER .NE. 0 ) THEN
            CALL ERR_LOG ( 5708, -1, 'DB_IMPORT_HEAD', 'Error in attempt '// &
     &          'to set configuration for DB_IMPORT' )
            CALL EXIT ( 8 )
      END IF
      DBI%VRB = IVRB
!
! --- 1) Get list of the databases from the IVS Data Center
!
      IF ( DBI%VRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) 'DB_IMPORT: 1) getting directory list from the '// &
     &                        'IVS Data Center ... '
      END IF
      IUER = -1
      CALL GET_IVS_DBLIST ( DBI, M_DBS, LT_DBS, CT_DBS, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5709, -1, 'DB_IMPORT_HEAD', 'Error in attempt '// &
     &         'to read databases list from the Data Center '//DBI%IVS_DB_URL )
           CALL EXIT ( 9 )
      END IF
      IF ( DBI%VRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) '           1) done'
           IF ( DBI%VRB .GE. 2 ) THEN
                CALL WRITE_LIST ( '/tmp/dbi_dc.lis', '#', LT_DBS, CT_DBS, 0 )
                WRITE ( 6, '(A)' ) 'DC database list is written in '// &
     &                              '/tmp/dbi_dc.lis'
           END IF
      END IF
!
! --- 2) Get the database list from the local catalogue
!
      IF ( DBI%VRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) 'DB_IMPORT: 2) getting the database list from '// &
     &                        'the local catalogue ...'
      END IF
      IUER = -1
      CALL READ_DBCAT ( DBI, M_DBS, LH_DBS, CH_DBS, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5710, -1, 'DB_IMPORT_HEAD', 'Error in attempt '// &
     &         'to get read database list from the local catalogue system' )
           CALL EXIT ( 10 )
      END IF
      IF ( DBI%VRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) '           2) done'
           IF ( DBI%VRB .GE. 2 ) THEN
                CALL WRITE_LIST ( '/tmp/dbi_cat.lis', '#', LH_DBS, CH_DBS, 0 )
                WRITE ( 6, '(A)' ) 'Local catalogue database list is written '// &
     &                             'in /tmp/dbi_cat.lis'
           END IF
      END IF
!
! --- 3) Get the database list in the incoming area
!
      IF ( DBI%VRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) 'DB_IMPORT: 3) getting list of databases in '// &
     &                        'the incoming directory ...'
      END IF
      IUER = -1
      CALL READ_INCOMING ( DBI, M_DBS, LI_DBS, CI_DBS, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5711, -1, 'DB_IMPORT_HEAD', 'Error in attempt '// &
     &         'to get read database list from the local catalogue system' )
           CALL EXIT ( 11 )
      END IF
      IF ( DBI%VRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) '           3) done'
           IF ( DBI%VRB .GE. 2 ) THEN
                CALL WRITE_LIST ( '/tmp/dbi_inc.lis', '#', LI_DBS, CI_DBS, 0 )
                WRITE ( 6, '(A)' ) 'Local incoming list is written '// &
     &                             'in /tmp/dbi_inc.lis'
           END IF
      END IF
!
! --- 4) Read get_file and noget_file
!
      IF ( DBI%VRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) 'DB_IMPORT: 4) reading get_file and noget_file '// &
     &                        'lists ...'
      END IF
      IUER = -1
      CALL READ_GETFILE ( DBI, M_DBS, LG_DBS, CG_DBS, &
     &                         M_DBS, LN_DBS, CN_DBS, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5712, -1, 'DB_IMPORT_HEAD', 'Error in attempt '// &
     &         'to read get_list and no_getlist of the database files '// &
     &         'to be downloaded from the IVS Data Center' )
           CALL EXIT ( 12 )
      END IF
      IF ( DBI%VRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) '           4) done'
           IF ( DBI%VRB .GE. 2 ) THEN
                CALL WRITE_LIST ( '/tmp/dbi_get.lis', '#', LG_DBS, CG_DBS, 0 )
                WRITE ( 6, '(A)' ) 'Get database list is written '// &
     &                             'in /tmp/dbi_get.lis'
                CALL WRITE_LIST ( '/tmp/dbi_noget.lis', '#', LN_DBS, CN_DBS, 0 )
                WRITE ( 6, '(A)' ) 'Noget database list is written '// &
     &                             'in /tmp/dbi_noget.lis'
           END IF
      END IF
!
! --- 5) Build the list of the file names to be browsed from the IVS data center
!
      IF ( DBI%VRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) 'DB_IMPORT: 5) build the database '// &
     &                        'file list to be downloaded'
      END IF
      IUER = -1
      CALL DBI_COMBLIST ( DBI, LT_DBS, CT_DBS, LH_DBS, CH_DBS, LI_DBS, CI_DBS, &
     &                         LG_DBS, CG_DBS, LN_DBS, CN_DBS, L_DBS,  C_DBS, &
     &                         IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5713, -1, 'DB_IMPORT_HEAD', 'Error in attempt '// &
     &         'to build the list of files to be downloaded from the IVS '// &
     &         'Data Center' )
           CALL EXIT ( 13 )
      END IF
      IF ( L_DBS .EQ. 0 ) THEN
!
! -------- No databases to be dowloaded. Nothing to do.
!
           IF ( DBI%VRB .GE. 1 ) THEN
                WRITE ( 6, '(A)' ) 'DB_IMPORT: No database files for '// &
     &                             'downloading were found'
                WRITE ( 6, '(A)' ) 'DB_IMPORT: Successfull termination'
           END IF
           CALL EXIT ( 0 )
      END IF
      IF ( DBI%VRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) '           5) done'
      END IF
!
! --- 6) Build the URL list of the database files to be downloaded
!
      IF ( DBI%VRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) 'DB_IMPORT: 6) building the URL list to be '// &
     &                        'downloaded'
      END IF
      IUER = -1
      CALL DBI_URLLIST ( DBI, L_DBS, C_DBS, C_URL, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5714, -1, 'DB_IMPORT_HEAD', 'Error in attempt '// &
     &         'to build the URL list of files to be downloaded from the IVS '// &
     &         'Data Center' )
           CALL EXIT ( 14 )
      END IF
      IF ( DBI%VRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) '           6) done'
           IF ( DBI%VRB .GE. 2 ) THEN
                CALL WRITE_LIST ( '/tmp/dbi_url.lis', '#', L_DBS, C_DBS, 0 )
                WRITE ( 6, '(A)' ) 'URL list of the databases is written '// &
     &                             'in /tmp/dbi_url.lis'
           END IF
      END IF
!
! --- Downloaded the database files by using wget
!
      IF ( DBI%VRB .GE. 1 ) THEN
           WRITE ( 6, '(A,I5,A)' ) 'DB_IMPORT: 7) scheduling wget for '// &
     &                             'downloading ', L_DBS, ' databases'
      END IF
      IUER = -1
      CALL DBI_DOWNLOAD ( DBI, L_DBS, C_DBS, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5715, -1, 'DB_IMPORT_HEAD', 'Error in attempt '// &
     &         'to download a set of files with databases from the IVS Data '// &
     &         'Center' )
           CALL EXIT ( 15 )
      END IF
      IF ( DBI%VRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) '           7) done'
      END IF
!
! --- Send an e-mail message to a user about successful downloading database
! --- files
!
      IF ( DBI%VRB .GE. 1 ) THEN
           WRITE ( 6, '(A,I5,A)' ) 'DB_IMPORT: 8) sending the mail message '// &
     &                             'about downloading ', L_DBS, ' databases'
      END IF
      IUER = -1
      CALL DBI_SUCCESS ( DBI, L_DBS, C_DBS, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5716, -1, 'DB_IMPORT_HEAD', 'Error in attempt '// &
     &         'to make the final cleanup and send e-mail messages after '// &
     &         'successfull downloading files with databases from the IVS '// &
     &         'Data Center' )
           CALL EXIT ( 16 )
      END IF
      IF ( DBI%VRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) '           8) done'
           WRITE ( 6, '(A)' ) 'DB_IMPORT: successfull termination'
      END IF
!
      END  !#!  DB_IMPORT_HEAD  #!#
