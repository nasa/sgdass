      SUBROUTINE OPA_SNRANAL ( SOLVE_INIT, OPA, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Program  OPA_SNRANAL  launches  SNR analysis software for          *
! *   determination actual SNR as well as SEFD and comparison it with    *
! *   predicted SNR and SEFD.                                            *
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
! *  ### 10-JAN-2001  OPA_SNRANAL  v1.1 (c) L. Petrov  16-JAN-2001  ###  *
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
     &           SNR_STS_FILE*128, SNR_PRG_FILE*128, SNR_ERR_FILE*128
      LOGICAL*4  LEX
      INTEGER*4  NBUF, PID, ISIG, ICOD, IS, IYEAR, IER
      INTEGER*4  SYSTEM, UNLINK, GETPID, I_LEN
!
      PID = GETPID () ! get process identification of the current process
      CALL CLRCH   (       PID_STR )
      CALL INCH    ( PID,  PID_STR )
      CALL CHASHR  (       PID_STR )
      CALL BLANK_TO_ZERO ( PID_STR )
!
! --- Build the line for launching snranal
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
      CALL CLRCH ( SNR_STS_FILE )
      CALL CLRCH ( SNR_PRG_FILE )
      CALL CLRCH ( SNR_ERR_FILE )
!
      SNR_STS_FILE = OPA%TMP_DIR(1:I_LEN(OPA%TMP_DIR))//'snr_'//PID_STR// &
     &               '.sts'
      SNR_PRG_FILE = OPA%TMP_DIR(1:I_LEN(OPA%TMP_DIR))//'snr_'//PID_STR// &
     &               '.prg'
      SNR_ERR_FILE = DIR_NAME(1:I_LEN(DIR_NAME))// &
     &               OPA%SESS_CODE(1:I_LEN(OPA%SESS_CODE))//'.err'
!
! --- Whether the sced program exists?
!
      INQUIRE ( FILE=OPA%SKED_EXE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 4391, IUER, 'OPA_SNRANAL', 'Executable file with'// &
     &         ' program SKED : '//OPA%SKED_EXE(1:I_LEN(OPA%SKED_EXE))// &
     &         ' was not found' )
           RETURN
      END IF
!
      CALL CLRCH ( COMSTR  )
      COMSTR = PRE_SOL_DIR(1:PRE_SOL_LEN)//'snranal '// &
     &        ' -sched '//OPA%SESSION_DIR(1:I_LEN(OPA%SESSION_DIR))// &
     &        ' -master '//OPA%MASTER_DIR(1:I_LEN(OPA%MASTER_DIR))// &
     &        ' -database '//OPA%DB_NAME// &
     &        ' -sked '//OPA%SKED_EXE(1:I_LEN(OPA%SKED_EXE))// &
     &        ' -outdir '//DIR_NAME(1:I_LEN(DIR_NAME))// &
     &        ' -status '//SNR_STS_FILE(1:I_LEN(SNR_STS_FILE))// &
     &        ' > '//SNR_PRG_FILE(1:I_LEN(SNR_PRG_FILE))
      IF ( IVRB .GE. 1 ) WRITE ( 6, '(A)' ) ' Running SNRANAL ...'
!
! --- Launch snranal and wait
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
           CALL ERR_LOG ( 4392, IUER, 'OPA_SNRANAL', 'Error '// &
     &          STR(1:I_LEN(STR))//' in executing command line '// &
     &          COMSTR(1:I_LEN(COMSTR))// &
     &          ' Look at files 1) '//SNR_STS_FILE(1:I_LEN(SNR_STS_FILE))// &
     &          ' Look at files 2) '//SNR_PRG_FILE(1:I_LEN(SNR_PRG_FILE))// &
     &          ' Look at files 3) '//SNR_ERR_FILE(1:I_LEN(SNR_ERR_FILE))  )
           RETURN
      END IF
!
! --- Read snranal status file
!
      CALL RD_TEXT ( SNR_STS_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4393, IUER, 'OPA_SNRANAL', 'Error in attempt '// &
     &         'to read the snranal status file: '//SNR_STS_FILE )
           RETURN
      END IF
!
! --- ... and check whether it terminated successfully
!
      IF ( BUF(1)(1:31) .NE. '# SNRANAL successful completion' ) THEN
           CALL ERR_LOG ( 4394, IUER, 'OPA_SNRANAL', 'Program snranal '// &
     &         'terminated abnormally. Please investigate files '// &
     &         ' 1) '//SNR_STS_FILE(1:I_LEN(SNR_STS_FILE))// &
     &         ' 2) '//SNR_PRG_FILE(1:I_LEN(SNR_PRG_FILE))// &
     &         ' 3) '//SNR_ERR_FILE(1:I_LEN(SNR_ERR_FILE))  )
           RETURN
      END IF
!
! --- Remove temporary SNR anal file
!
      IS = UNLINK ( SNR_STS_FILE(1:I_LEN(SNR_STS_FILE))//CHAR(0) )
      IS = UNLINK ( SNR_PRG_FILE(1:I_LEN(SNR_PRG_FILE))//CHAR(0) )
      IS = UNLINK ( SNR_ERR_FILE(1:I_LEN(SNR_ERR_FILE))//CHAR(0) )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  OPA_SNRANAL  #!#
