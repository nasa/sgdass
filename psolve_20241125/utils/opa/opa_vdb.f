      SUBROUTINE OPA_VDB ( SOLVE_INIT, OPA, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Program  OPA_VDB launches  process for inserting solution listing  *
! *   into a database of listings.                                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * SOLVE_INIT ( CHARACTER ) -- Solve user initials.                     *
! *        OPA ( RECORD    ) -- Data structure which keeps internal      *
! *                             information of OPA: configuration        *
! *                             parameters, session name, status codes,  *
! *                             action codes.                            *
! *       IVRB ( INTEGER*4 ) -- Verbosity level. 0 means suppress all    *
! *                             information messages except error        *
! *                             messages.                                *
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
! *  ### 20-DEC-2005     OPA_VDB   v1.2 (c) L. Petrov  11-JAN-2006  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'precm.i'
      INCLUDE    'opa.i'
      TYPE ( OPA__STRU ) ::  OPA
      CHARACTER  SOLVE_INIT*2
      INTEGER*4  IVRB, IUER
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 1024 )
!
      CHARACTER  COMSTR*256, BUF(MBUF)*256, PID_STR*5, STR*512, DIR_NAME*128, &
     &           LISTING_FILE*128, VDB_LOG_FILE*128
      LOGICAL*4  LEX
      INTEGER*4  NBUF, PID, ISIG, ICOD, IS, IYEAR, IER
      INTEGER*4, EXTERNAL :: SYSTEM, UNLINK, GETPID, I_LEN
!
      PID = GETPID () ! get process identification of the current process
      CALL CLRCH   (       PID_STR )
      CALL INCH    ( PID,  PID_STR )
      CALL CHASHR  (       PID_STR )
      CALL BLANK_TO_ZERO ( PID_STR )
!
! --- Build the line for launching VDB
!
      CALL CHIN ( OPA%DB_NAME(1:2), IYEAR )
      IF ( IYEAR .GT. 70 ) THEN
           DIR_NAME = OPA%SESSION_DIR(1:I_LEN(OPA%SESSION_DIR))//'19'// &
     &                OPA%DB_NAME(1:2)//'/'// &
     &                OPA%SESS_CODE(1:I_LEN(OPA%SESS_CODE))//'/'
         ELSE
           DIR_NAME = OPA%SESSION_DIR(1:I_LEN(OPA%SESSION_DIR))//'20'// &
     &                OPA%DB_NAME(1:2)//'/'// &
     &                OPA%SESS_CODE(1:I_LEN(OPA%SESS_CODE))//'/'
      END IF
!
! --- Build filenames
!
      CALL CLRCH ( VDB_LOG_FILE )
      CALL CLRCH ( LISTING_FILE )
!
      LISTING_FILE = DIR_NAME(1:I_LEN(DIR_NAME))// &
     &               OPA%SESS_CODE(1:I_LEN(OPA%SESS_CODE))//'.spl'
!
      VDB_LOG_FILE = OPA%TMP_DIR(1:I_LEN(OPA%TMP_DIR))//'/vdb_log_'// &
     &               OPA%SESS_CODE(1:I_LEN(OPA%SESS_CODE))//'_'//     &
     &               PID_STR(1:I_LEN(PID_STR))//'.err'
!
! --- Check whether the listing file exists
!
      INQUIRE ( FILE=LISTING_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 4421, IUER, 'OPA_VDB', 'Solution Listing file '// &
     &         LISTING_FILE(1:I_LEN(LISTING_FILE))// &
     &         ' was not found. Please, execute action "Make a standalone '// &
     &         'solution and keep listings" first' )
           RETURN
      END IF
!
! --- Check, whether the vdb program exists
!
      INQUIRE ( FILE=OPA%VDB_UPDATE_EXE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 4422, IUER, 'OPA_VDB', 'Executable file '// &
     &         ' for the action VDB update: '// &
     &         OPA%VDB_UPDATE_EXE(1:I_LEN(OPA%VDB_UPDATE_EXE))// &
     &         ' was not found' )
           RETURN
      END IF
!
      CALL CLRCH ( COMSTR  )
      COMSTR = OPA%VDB_UPDATE_EXE(1:I_LEN(OPA%VDB_UPDATE_EXE))//' '// &
     &         LISTING_FILE(1:I_LEN(LISTING_FILE))//' '// &
     &         VDB_LOG_FILE(1:I_LEN(VDB_LOG_FILE))
      IF ( IVRB .GE. 1 ) WRITE ( 6, '(A)' ) ' Running VDB_update...'
!
! --- Launch VDB and wait
!
      IS = SYSTEM ( COMSTR(1:I_LEN(COMSTR))//CHAR(0) )
!
      IF ( IS .NE. 0 ) THEN
!
! -------- Completion code is not 0. Extract ISIG -- signal number which
! -------- caused termination of the command and ICOD -- completion code
!
           ISIG = 0
           ICOD = 0
           CALL MVBITS ( IS, 0, 8, ISIG, 0 )
           CALL MVBITS ( IS, 8, 8, ICOD, 0 )
           IF ( ICOD .GE. 128 ) ICOD = ICOD-256
!
           CALL CLRCH ( STR )
           CALL INCH  ( ICOD, STR )
           CALL ERR_LOG ( 4423, IUER, 'OPA_VDB', 'Error '// &
     &          STR(1:I_LEN(STR))//' in executing command line '// &
     &          COMSTR(1:I_LEN(COMSTR))//' Log file: '//VDB_LOG_FILE )
           RETURN
      END IF
!
! --- Remove temporary VDB file
!
      IS = UNLINK ( VDB_LOG_FILE(1:I_LEN(VDB_LOG_FILE))//CHAR(0) )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  OPA_VDB  !#!#
