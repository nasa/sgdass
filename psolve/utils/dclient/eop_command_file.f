      SUBROUTINE EOP_COMMAND_FILE ( DCLIENT, EOP_TYPE, F_CONF, EOPSNAME, &
     &                              COMMAND_FILE, MESSAGE_FILE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  EOP_COMMAND_FILE  is a part of dclient software.          *
! *   It creates                                                         *
! *   a) a body of e-mail message and write it in a temporary file;      *
! *   b) a command file in the form of C-Shell program which will be     *
! *      executed for processing a request for EOP file(s)               *
! *      transferring to Data Server using dserver. It writes this       *
! *      program in temporary file.                                      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      DCLIENT ( RECORD    ) -- Object with data structure for keeping *
! *                               configuration of dclient.              *
! *     EOP_TYPE ( CHARACTER ) -- EOP series type. One of                *
! *                               "EOPS" or "EOPI".                      *
! *       F_CONF ( LOGICAL*4 ) -- Flag of a confirmation of the request. *
! *                               If .TRUE. then the subroutine asks     *
! *                               user for confirmation of a request.    *
! *     EOPSNAME ( CHARACTER ) -- Input filename with the EOP series.    *
! * COMMAND_FILE ( CHARACTER ) -- File name with a C-Shell program for   *
! *                               processing a request for the EOP files *
! *                               submission.                            *
! * MESSAGE_FILE ( CHARACTER ) -- File name with a message to be sent to *
! *                               dserver.                               *
! *                                                                      *
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
! * ### 01-OCT-1999 EOP_COMMAND_FILE v1.5 (c) L. Petrov 23-AUG-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'dclient.i'
      TYPE ( DCLIENT__STRU ) ::  DCLIENT
      CHARACTER  EOP_TYPE*(*), EOPSNAME*(*), COMMAND_FILE*(*), MESSAGE_FILE*(*)
      LOGICAL*4  F_CONF
      INTEGER*4  IUER
      INTEGER*4  M_MAI, MIND
      PARAMETER  ( M_MAI = 16, MIND=16 )
      INTEGER*4  L_MAI, LIND, IND(2,MIND), I11, IP, PID, J1, J2
      CHARACTER  CIS_MAI(M_MAI)*256, OUT*256, DELIM*4, USER_NAME*256, &
     &           USER_REALNAME*256, USER_E_ADDRESS*256, FILOUT*256, &
     &           PID_STR*5, ANS*3
      CHARACTER  GET_CDATE*19
      LOGICAL*4  LEX
      PARAMETER  ( DELIM = CHAR(0)//CHAR(32)//CHAR(9)//',' )
      INTEGER*4, EXTERNAL :: LINDEX, GETPID, ILEN, I_LEN
!
      IF ( EOP_TYPE .NE. 'EOPS'  .AND.  EOP_TYPE .NE. 'EOPI' ) THEN
           CALL ERR_LOG ( 5261, IUER, 'EOP_COMMAND_FILE', 'EOP_TYPE '// &
     &          EOP_TYPE(1:I_LEN(EOP_TYPE))//' is not supported. One of '// &
     &         '"EOPS" or "EOPI" was expected' )
           RETURN
      END IF
!
! --- Check whether the file with EOP exists
!
      INQUIRE ( FILE = EOPSNAME, EXIST = LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5262, IUER, 'EOP_COMMAND_FILE', 'EOP file '// &
     &          EOPSNAME(1:I_LEN(EOPSNAME))//' was not found' )
           RETURN
      END IF
!
! --- Get information about user name
!
      CALL GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
!
! --- Get PID (Process IDentifier)
!
      PID = GETPID()
!
! --- Transform PID to a text string
!
      CALL CLRCH  ( PID_STR )
      CALL INCH   ( PID,   PID_STR )
      CALL CHASHR (        PID_STR )
      CALL BLANK_TO_ZERO ( PID_STR )
!
! --- Form a message file names. They contain PID in order to avoid
! --- conflicts with simultaneous work of two or more users
!
      MESSAGE_FILE = DCLIENT%TMP_DIR(1:I_LEN(DCLIENT%TMP_DIR))//'dclient_'// &
     &               PID_STR(1:I_LEN(PID_STR))//'.mes'
      COMMAND_FILE = DCLIENT%TMP_DIR(1:I_LEN(DCLIENT%TMP_DIR))//'dclient_'// &
     &               PID_STR(1:I_LEN(PID_STR))//'.cnt'
!
! --- Build output filename
!
      CALL CLRCH ( FILOUT )
      IF ( EOP_TYPE .EQ. 'EOPS' ) THEN
           FILOUT = DCLIENT%FTP_DIR(1:I_LEN(DCLIENT%FTP_DIR))// &
     &              DCLIENT%EOP_SUFFIX(1:I_LEN(DCLIENT%EOP_SUFFIX))//'.eops'
         ELSE IF ( EOP_TYPE .EQ. 'EOPI' ) THEN
           FILOUT = DCLIENT%FTP_DIR(1:I_LEN(DCLIENT%FTP_DIR))// &
     &              DCLIENT%EOP_SUFFIX(1:I_LEN(DCLIENT%EOP_SUFFIX))//'.eopi'
      END IF
!
      IF ( F_CONF ) THEN
!
! -------- Ask user a confirmation of the request
!
           WRITE ( 6, '(A)'  ) 'Dear '// &
     &                          USER_REALNAME(1:I_LEN(USER_REALNAME))//', '// &
     &                         'are you really going to submit '
           WRITE ( 6, '(A)'  ) '     '//EOP_TYPE//' series '// &
     &                         EOPSNAME(1:I_LEN(EOPSNAME))
           WRITE ( 6, '(A$)' ) '     to the Data Center '// &
     &                    DCLIENT%DATA_CENTER(1:I_LEN(DCLIENT%DATA_CENTER))// &
     &                    ' (Y/N) <N>  ? '
!
! -------- Read users' answer
!
           READ ( 5, '(A)' ) ANS
           CALL TRAN ( 11, ANS, ANS )
!
! -------- ... and parse it
!
           IF ( ANS .NE. 'Y' ) THEN
                WRITE ( 6, '(A)' ) 'Your request is canceled'
                CALL EXIT ( -64 )
           END IF
      END IF
!
! --- Open the output file for a message to be sent to dserver
!
      OPEN ( UNIT=11, FILE=MESSAGE_FILE, STATUS='UNKNOWN', IOSTAT=I11 )
      IF ( I11 .NE. 0 ) THEN
           CALL ERR_LOG ( 5263, IUER, 'EOP_COMMAND_FILE', 'Error in '// &
     &         'openning output file for a message '//MESSAGE_FILE )
           RETURN
      END IF
!
! --- Data type
!
      WRITE ( 11, '(A)' ) 'DATA_TYPE:     '//EOP_TYPE
!
! --- URL(s)
!
      WRITE ( 11, '(A)' ) 'URL:           '// &
     &                     DCLIENT%URL_PREFIX(1:I_LEN(DCLIENT%URL_PREFIX))// &
     &                     FILOUT(LINDEX(FILOUT,'/')+1:I_LEN(FILOUT))//'.gz'
!
! --- Deal with a confirm mail field. It may contain more than one address
!
      IF ( ILEN(DCLIENT%CONFIRM_EMAIL) .GT. 0 ) THEN
!
! -------- Split the field onto words
!
           CALL EXWORD ( DCLIENT%CONFIRM_EMAIL, MIND, LIND, IND, DELIM, -3 )
!
! -------- Now put e-addresses in character list. It will exclude duplicates
!
           L_MAI = 0
           DO 410 J1=1,LIND
              IF ( DCLIENT%CONFIRM_EMAIL(IND(1,J1):IND(2,J1)) .EQ. '.' ) THEN
!
! ---------------- Special case "." -- add user e-address instead of it
!
                   CALL ADD_CLIST ( M_MAI, L_MAI, CIS_MAI, USER_E_ADDRESS, -3 )
                 ELSE IF ( DCLIENT%CONFIRM_EMAIL(IND(1,J1):IND(2,J1)) .EQ. 'NO' &
     &                .OR. DCLIENT%CONFIRM_EMAIL(IND(1,J1):IND(2,J1)) .EQ. 'no') &
     &                THEN
                   CONTINUE
                 ELSE
!
! ---------------- Check validity of the a-address
!
                   IP = INDEX ( DCLIENT%CONFIRM_EMAIL(IND(1,J1):IND(2,J1)), '@')
                   IF ( IP .LE. 0 ) THEN
                        CALL ERR_LOG ( 5264, IUER, 'EOP_COMMAND_FILE', &
     &                      'Error in generating a message: field '// &
     &                      'CONFIRM_EMAIL in configuration file '// &
     &                       DCLIENT%CONFIG_FILE(1:I_LEN(DCLIENT%CONFIG_FILE))// &
     &                      ' contains a e-address '// &
     &                       DCLIENT%CONFIRM_EMAIL(IND(1,1):IND(2,1))// &
     &                      ' which does not contain symbol @ and therefore '// &
     &                      ' cannot be considered as a vaild e-address' )
                        CLOSE ( UNIT=11, STATUS='DELETE' )
                        RETURN
                   END IF
                   CALL ADD_CLIST ( M_MAI, L_MAI, CIS_MAI, &
     &                  DCLIENT%CONFIRM_EMAIL(IND(1,J1):IND(2,J1)), -3 )
              END IF
 410       CONTINUE
!
! -------- Transform a list to a line
!
           CALL LIST_TO_LINE ( L_MAI, CIS_MAI, ' ', OUT )
           WRITE ( 11, '(A)' ) 'CONFIRM_EMAIL: '//OUT(1:I_LEN(OUT))
         ELSE
           WRITE ( 11, '(A)' ) 'CONFIRM_EMAIL: '
      ENDIF
!
! --- Deal with a success mail field. It may contain more than one address
!
      IF ( ILEN(DCLIENT%SUCCESS_EMAIL) .GT. 0 ) THEN
!
! -------- Split the field onto words
!
           CALL EXWORD ( DCLIENT%SUCCESS_EMAIL, MIND, LIND, IND, DELIM, -3 )
           L_MAI = 0
           DO 420 J2=1,LIND
              IF ( DCLIENT%SUCCESS_EMAIL(IND(1,J2):IND(2,J2)) .EQ. '.' ) THEN
                   CALL ADD_CLIST ( M_MAI, L_MAI, CIS_MAI, USER_E_ADDRESS, -3 )
                 ELSE IF ( DCLIENT%SUCCESS_EMAIL(IND(1,J2):IND(2,J2)) .EQ. 'NO' &
     &                .OR. DCLIENT%SUCCESS_EMAIL(IND(1,J2):IND(2,J2)) .EQ. 'no') &
     &                THEN
                   CONTINUE
                 ELSE
                   IP = INDEX ( DCLIENT%SUCCESS_EMAIL(IND(1,J2):IND(2,J2)), '@')
                   IF ( IP .LE. 0 ) THEN
                        CALL ERR_LOG ( 5265, IUER, 'EOP_COMMAND_FILE', &
     &                      'Error in generating a message: field '// &
     &                      'SUCCESS_EMAIL in configuration file '// &
     &                       DCLIENT%CONFIG_FILE(1:I_LEN(DCLIENT%CONFIG_FILE))// &
     &                      ' contains a e-address '// &
     &                       DCLIENT%SUCCESS_EMAIL(IND(1,1):IND(2,1))// &
     &                      ' which does not contain symbol @ and therefore '// &
     &                      ' cannot be considered as a vaild e-address' )
                        CLOSE ( UNIT=11, STATUS='DELETE' )
                        RETURN
                   END IF
                   CALL ADD_CLIST ( M_MAI, L_MAI, CIS_MAI, &
     &                  DCLIENT%SUCCESS_EMAIL(IND(1,J2):IND(2,J2)), -3 )
              END IF
 420       CONTINUE
!
           CALL LIST_TO_LINE ( L_MAI, CIS_MAI, ' ', OUT )
           WRITE ( 11, '(A)' ) 'SUCCESS_EMAIL: '//OUT(1:I_LEN(OUT))
         ELSE
           WRITE ( 11, '(A)' ) 'SUCCESS_EMAIL: '
      ENDIF
      CLOSE ( UNIT=11 )
!
! --- Open the output file for a C-Shell command file
!
      OPEN ( UNIT=11, FILE=COMMAND_FILE, STATUS='UNKNOWN', IOSTAT=I11 )
      IF ( I11 .NE. 0 ) THEN
           CALL ERR_LOG ( 5266, IUER, 'EOP_COMMAND_FILE', 'Error in '// &
     &         'openning output file for a message '//COMMAND_FILE )
           RETURN
      END IF
!
! --- Write down it
!
      WRITE ( 11, '(A)' ) '#!/bin/csh'
      WRITE ( 11, '(A)' ) '##################################################'
      WRITE ( 11, '(A)' ) '#                                                #'
      WRITE ( 11, '(A)' ) '#  This file is generated automatically by       #'
      WRITE ( 11, '(A)' ) '#  '//DCLIENT__LABEL     //'                     #'
      WRITE ( 11, '(A)' ) '#  on '//GET_CDATE()  //'                        #'
      WRITE ( 11, '(A)' ) '#                                                #'
      WRITE ( 11, '(A)' ) '##################################################'
      WRITE ( 11, '(A)' ) '#'
      WRITE ( 11, '(A)' ) 'set infile = '//EOPSNAME(1:I_LEN(EOPSNAME))
      WRITE ( 11, '(A)' ) 'set outfile = '// &
     &                     DCLIENT%FTP_DIR(1:I_LEN(DCLIENT%FTP_DIR))// &
     &                     FILOUT(LINDEX(FILOUT,'/')+1:I_LEN(FILOUT))//'.gz'
      WRITE ( 11, '(A)' ) '/bin/echo -e "gzipping $infile  ...  \c"'
      WRITE ( 11, '(A)' ) 'cat $infile | gzip -c > $outfile'
      WRITE ( 11, '(A)' ) 'if ( $status != 0 ) then'
      WRITE ( 11, '(A)' ) '     echo "Error in gzipping " $infile'
      WRITE ( 11, '(A)' ) '     exit ( 0 ) -1'
      WRITE ( 11, '(A)' ) 'endif'
      WRITE ( 11, '(A)' ) 'chmod g+rw $outfile >& /dev/null'
      WRITE ( 11, '(A)' ) 'echo "Done "'
      WRITE ( 11, '(A)' ) '#'
!
      WRITE ( 11, '(A)' ) DCLIENT%MAIL_COMMAND(1:I_LEN(DCLIENT%MAIL_COMMAND))// &
     &                    ' -s "Submission of '// &
     &                    EOPSNAME(1:I_LEN(EOPSNAME))//' by '//DCLIENT__LABEL// &
     &                    '" '// &
     &             DCLIENT%DSERVER_EMAIL(1:I_LEN(DCLIENT%DSERVER_EMAIL))// &
     &             ' < '//MESSAGE_FILE(1:I_LEN(MESSAGE_FILE))
      WRITE ( 11, '(A)' ) 'if ( $status != 0 ) then'
      WRITE ( 11, '(A)' ) '     exit ( 0 ) -3'
      WRITE ( 11, '(A)' ) 'endif'
      WRITE ( 11, '(A)' ) '#'
      WRITE ( 11, '(A)' ) 'echo "=== Request for submitting the '//EOP_TYPE// &
     &                    ' series '// &
     &                    EOPSNAME(1:I_LEN(EOPSNAME))//' is sent to '// &
     &                    DCLIENT%DATA_CENTER(1:I_LEN(DCLIENT%DATA_CENTER))//'"'
      WRITE ( 11, '(A)' ) 'exit 0'
!
      CLOSE ( UNIT=11 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  EOP_COMMAND_FILE  #!#
