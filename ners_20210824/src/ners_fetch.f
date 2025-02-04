      SUBROUTINE NERS_FETCH ( NERS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  NERS_FETCH
! *                                                                      *
! *  ### 16-JUN-2016   NERS_FETCH   v2.6 (c) L. Petrov  17-JUL-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      INCLUDE   'ners_local.i'
      INCLUDE   'astro_constants.i'
      TYPE     ( NERS__TYPE ) :: NERS
      INTEGER*4  IUER 
      CHARACTER  STR*128, COM_STR*512, TEMP_NERS_FCS*128, TEMP_NERS_LOG*128, &
     &           CONN_TIMEOUT_STR*12, READ_TIMEOUT_STR*12, &
     &           NERS_IO_LOCK_FILE*128, NERS_READ_LOCK_FILE*128, &
     &           NERS_WRITE_LOCK_FILE*128, COM*128
      INTEGER*4  IS, J1, J2, J3, ID, UNIX_DATE, IER
      INTEGER*8  SIZE_I8
      LOGICAL*4  LEX
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, KILL, FILE_INFO, LINDEX
      REAL*8,    EXTERNAL :: TIME
      INTEGER*4, EXTERNAL :: SYSTEM, GETPID
!
      WRITE ( UNIT=STR(1:8), FMT='(I8.8)' ) GETPID()
      TEMP_NERS_FCS  = TRIM(NERS%CNF%FCS_FILE)//'_'//STR(1:8)
      TEMP_NERS_LOG  = '/tmp/ners__'//STR(1:8)//'.log'
!
      ID = LINDEX ( NERS%CNF%FCS_FILE, '/' )
      IF ( ID == 0 ) ID = ILEN(NERS%CNF%FCS_FILE)
      NERS_IO_LOCK_FILE    = NERS%CNF%FCS_FILE(1:ID)//NERS__IO_LOCK
      NERS_READ_LOCK_FILE  = NERS%CNF%FCS_FILE(1:ID)//NERS__READ_LOCK
      NERS_WRITE_LOCK_FILE = NERS%CNF%FCS_FILE(1:ID)//NERS__WRITE_LOCK
!
      WRITE ( UNIT=CONN_TIMEOUT_STR, FMT='(F12.1)' ) NERS%CNF%CONN_TIMEOUT 
      WRITE ( UNIT=READ_TIMEOUT_STR, FMT='(F12.1)' ) NERS%CNF%READ_TIMEOUT 
      CONN_TIMEOUT_STR = ADJUSTL ( CONN_TIMEOUT_STR )
      READ_TIMEOUT_STR = ADJUSTL ( READ_TIMEOUT_STR )
!
      DO 420 J2=1,NERS%CNF%N_TRIES
         DO 430 J3=1,NERS%CNF%N_URL
            COM_STR = 'wget'// &
     &                ' -c'// &
     &                ' --no-use-server-timestamps'// &
     &                ' --dns-timeout='//TRIM(CONN_TIMEOUT_STR)// &
     &                ' --connect-timeout='//TRIM(CONN_TIMEOUT_STR)// &
     &                ' --read-timeout='//TRIM(READ_TIMEOUT_STR)// &
     &                ' -o '//TRIM(TEMP_NERS_LOG)// &
     &                ' -O '//TRIM(TEMP_NERS_FCS)// &
     &                ' '//TRIM(NERS%CNF%URL(J3))//' > '// &
     &                ' '//TRIM(TEMP_NERS_LOG)//' 2>&1'
            IS = SYSTEM ( TRIM(COM_STR)//CHAR(0) )
            IF ( IS == 0 ) THEN
!
! -------------- We have finished downloading. Let us check file size.
!
                 IS = FILE_INFO ( TEMP_NERS_FCS(1:I_LEN(TEMP_NERS_FCS))//CHAR(0), &
     &                            UNIX_DATE, SIZE_I8 )
                 IF ( IS .NE. 0 ) THEN
                      CALL UNLINK ( TEMP_NERS_FCS(1:I_LEN(TEMP_NERS_FCS))//CHAR(0) )
                      GOTO 430
                 END IF
!                 
                 IF ( SIZE_I8 < NERS__FCS_FIL_LEN_MIN ) THEN
!
! ------------------- Oh, no! The file is too short
!
                      CALL UNLINK ( TEMP_NERS_FCS(1:I_LEN(TEMP_NERS_FCS))//CHAR(0) )
                      GOTO 430
                 END IF
!
! -------------- Fine! The file looks OK. Set the read lock file
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL SET_READ_LOCK ( NERS_IO_LOCK_FILE, NERS_READ_LOCK_FILE, &
     &                                NERS_WRITE_LOCK_FILE, NERS%CNF%LOCK_TIMEOUT, &
     &                                NERS%CNF%FD_READ_LOCK, NERS%CNF%FD_WRITE_LOCK, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 4341, IUER, 'NERS_FETCH', 'Error in an attempt to '// &
     &                    'set the read lock file '//NERS_READ_LOCK_FILE )
                      RETURN 
                 END IF
!
! -------------- Rename the temporary file
!
                 CALL RENAME ( TRIM(TEMP_NERS_FCS)//CHAR(0), TRIM(NERS%CNF%FCS_FILE)//CHAR(0) )
!
! -------------- Make NERS message file writeable for all
!
                 COM = 'chmod u+rw,o+rw,g+rw '//TRIM(NERS%CNF%FCS_FILE)//'> /dev/null 2>&1'
                 IS  =  SYSTEM ( TRIM(COM)//CHAR(0) )
!
! -------------- Lift lock file
!
                 CALL LIFT_READ_WRITE_LOCKS ( NERS%CNF%FD_READ_LOCK, NERS%CNF%FD_WRITE_LOCK )
!
! -------------- Remove log files
!
                 CALL UNLINK ( TRIM(TEMP_NERS_LOG)//CHAR(0) )
!
                 CALL ERR_LOG ( 0, IUER )
                 RETURN 
            END IF
 430     CONTINUE 
         IF ( J2 .NE. NERS%CNF%N_TRIES ) THEN
              CALL SLEEP ( IDNINT ( NERS%CNF%CONN_TIMEOUT ) )
         END IF
 420  CONTINUE 
 820  CONTINUE 
!
      IF ( NERS%CNF%ON_FAIL_TO_READ == NERS__IGNORE ) THEN
!
! -------- Did not retrieve the message file. Update the last modification 
! -------- of the existing NERS file
!
           IS = SYSTEM ( '/usr/bin/touch '//TRIM(NERS%CNF%FCS_FILE)//CHAR(0) )
           CALL ERR_LOG ( 0, IUER )
         ELSE IF ( NERS%CNF%ON_FAIL_TO_READ == NERS__WARNING ) THEN
!
! -------- Did not retrieve the message file. Update the last modification 
! -------- of the existing NERS file
!
           IS = SYSTEM ( '/usr/bin/touch '//TRIM(NERS%CNF%FCS_FILE)//CHAR(0) )
!
! -------- Issue the warning
!
           WRITE ( 6, '(A)' ) 'NERS Warning: Cannot retrieve the NERS message file from '// &
     &                        'remote servers. Continue to use the old copy'
           CALL ERR_LOG ( 0, IUER )
         ELSE IF ( NERS%CNF%ON_FAIL_TO_READ == NERS__STOP ) THEN
!
! -------- Did not retrieve the message file. Generate the error message
!
           CALL ERR_LOG ( 4342, IUER, 'NERS_FETCH', 'Error in an attempt to '// &
     &         'download EOP from the remote server. Exit NERS because of '// &
     &         'configuration parameter ON_FAIL_TO_READ: stop. Last download '// &
     &         'command: '//COM_STR )
           IS = SYSTEM ( 'cat '//TRIM(TEMP_NERS_LOG)//CHAR(0) )
      END IF
!
! --- Remove log and lock files
!
      CALL UNLINK ( TRIM(TEMP_NERS_LOG)//CHAR(0) )
!
      RETURN
      END  SUBROUTINE  NERS_FETCH  !#!#
