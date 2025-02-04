      SUBROUTINE OPA_EOPK ( SOLVE_INIT, OPA, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Program  OPA_EOPK  launches GSFC EOP Kalmna fileter. The input     *
! *   file for Kalman filter is OPA.EOPB_FILE, the output file is        *
! *   OPA.EOPK_FILE                                                      *
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
! *  ### 06-OCT-2000    OPA_EOPK   v1.0 (c)  L. Petrov  06-OCT-2000 ###  *
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
      CHARACTER  COMSTR*256, BUF(MBUF)*256, PID_STR*5, STR*512, &
     &           NEW_EOPK_FILE*128, EOPK_BACKUP*128, &
     &           EOPK_STS_FILE*128, EOPK_PRG_FILE*128
      LOGICAL*4  LEX
      INTEGER*4  NBUF, PID, ISIG, ICOD, IS, IER
      INTEGER*4  SYSTEM, UNLINK, GETPID, RENAME, I_LEN
!
! --- Check EOPS file
!
      INQUIRE ( FILE=OPA%EOPS_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 4341, IUER, 'OPA_EOPK', 'The old EOPS file '// &
     &          OPA%EOPS_FILE(1:I_LEN(OPA%EOPS_FILE))//' was not found. '// &
     &          'It should exist before OPA_EOPK will try to update it ' )
           RETURN
      END IF
!
      PID = GETPID () ! get process identification of the current process
      CALL CLRCH   (       PID_STR )
      CALL INCH    ( PID,  PID_STR )
      CALL CHASHR  (       PID_STR )
      CALL BLANK_TO_ZERO ( PID_STR )
!
! --- Build filenames
!
      CALL CLRCH (   EOPK_BACKUP )
      CALL CLRCH ( NEW_EOPK_FILE )
      CALL CLRCH ( EOPK_STS_FILE )
      CALL CLRCH ( EOPK_PRG_FILE )
!
      EOPK_BACKUP = OPA%EOPK_FILE(1:I_LEN(OPA%EOPK_FILE))// &
     &              '.opa_eopk_bac_'//PID_STR
      NEW_EOPK_FILE = OPA%EOPK_FILE(1:I_LEN(OPA%EOPK_FILE))// &
     &                  '.opa_new_eopk_'//PID_STR
      EOPK_STS_FILE = OPA%TMP_DIR(1:I_LEN(OPA%TMP_DIR))//'eopk_'//PID_STR// &
     &                '.sts'
      EOPK_PRG_FILE = OPA%TMP_DIR(1:I_LEN(OPA%TMP_DIR))//'eopk_'//PID_STR// &
     &                '.prg'
!
! --- Build the line for launching eopkal
!
      CALL CLRCH ( COMSTR  )
      COMSTR = PRE_SOL_DIR(1:PRE_SOL_LEN)//'eopkal '// &
     &        ' -i '//OPA%EOPB_FILE(1:I_LEN(OPA%EOPB_FILE))// &
     &        ' -o '//NEW_EOPK_FILE(1:I_LEN(NEW_EOPK_FILE))// &
     &        ' -s '//EOPK_STS_FILE(1:I_LEN(EOPK_STS_FILE))// &
     &        ' >  '//EOPK_PRG_FILE(1:I_LEN(EOPK_PRG_FILE))
      IF ( IVRB .GE. 1 ) WRITE ( 6, '(A)' ) ' Running eopkal ...'
!
! --- Launch eopkal and wait
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
           CALL ERR_LOG ( 4342, IUER, 'OPA_EOPK', 'Error '//STR(1:I_LEN(STR))// &
     &                   ' in executing command line '//COMSTR )
           RETURN
      END IF
!
! --- Read eopkal status file
!
      CALL RD_TEXT ( EOPK_STS_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4343, IUER, 'OPA_EOPK', 'Error in attempt '// &
     &         'to read the eopkal status file: '//EOPK_STS_FILE )
           RETURN
      END IF
!
! --- ... and check whether it terminated successfully
!
      IF ( BUF(1)(1:16) .NE. 'EOPKAL: finished' ) THEN
           CALL ERR_LOG ( 4344, IUER, 'OPA_EOPK', 'Program eopkal '// &
     &         'terminated abnormally. Please investigate file '// &
     &          EOPK_PRG_FILE )
           RETURN
      END IF
      IF ( IVRB .GE. 1 ) WRITE ( 6, '(A)' ) ' Copying EOPK-file ...'
!
! --- Copy  OPA.EOPK_FILE --> EOPK_BACKUP
!
      CALL ERR_PASS ( IUER, IER )
      CALL COPY_ASCII_FILE ( OPA%EOPK_FILE, EOPK_BACKUP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4345, IUER, 'OPA_EOPK', 'Error in attempt '// &
     &         'to make a backup copy of the old EOPK file' )
           RETURN
      END IF
!
! --- Rename NEW_EOPK_FILE --> OPA.EOPK_FILE
!
      IS = RENAME ( NEW_EOPK_FILE, OPA%EOPK_FILE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 4346, IUER, 'OPA_EOPK', 'Serious error '// &
     &         'in attempt to move the new EOPK file to the old place. '// &
     &         'The old file '// &
     &          OPA%EOPK_FILE(1:I_LEN(OPA%EOPS_FILE))// &
     &         ' is probably spoiled!!! Check the backup copy '// &
     &         EOPK_BACKUP )
           RETURN
      END IF
!
! --- Remove temporary EOPK files
!
      IS = UNLINK ( EOPK_BACKUP(1:I_LEN(EOPK_BACKUP))//CHAR(0) )
      IS = UNLINK ( NEW_EOPK_FILE(1:I_LEN(NEW_EOPK_FILE))//CHAR(0) )
      IS = UNLINK ( EOPK_STS_FILE(1:I_LEN(EOPK_STS_FILE))//CHAR(0) )
      IS = UNLINK ( EOPK_PRG_FILE(1:I_LEN(EOPK_PRG_FILE))//CHAR(0) )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  OPA_EOPK  #!#
