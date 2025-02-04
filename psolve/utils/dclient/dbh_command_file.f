      SUBROUTINE DBH_COMMAND_FILE ( DCLIENT, F_CONF, DBNAME, FINAM1, FINAM2, &
     &                              COMMAND_FILE, MESSAGE_FILE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DBH_COMMAND_FILE  is a part of dclient software.         *
! *   It creates                                                         *
! *   a) a body of e-mail message and write it in a temporary file;      *
! *   b) a command file in the form of C-Shell program which will be     *
! *      executed for processing a request for database file(s)          *
! *      transferring to Data Server using dserver. It writes this       *
! *      program in temporary file.                                      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      DCLIENT ( RECORD    ) -- Object with data structure for keeping *
! *                               configuration of dclient.              *
! *       F_CONF ( LOGICAL*4 ) -- Flag of a confirmation of the request. *
! *                               IF .TRUE. then the subroutine asks     *
! *                               user for confirmation of a request.    *
! *       DBNAME ( CHARACTER ) -- database_name argument. May be one of  *
! *                               a) database filename with a full path; *
! *                               b) database filename of the specific   *
! *                                  version without path.               *
! *                               c) database name without version       *
! *                                  number and path. It corresponds to  *
! *                                  a X/S pair of databases.            *
! *       FINAM1 ( CHARACTER ) -- Full path name which corresponds to    *
! *                               the database.                          *
! *       FINAM2 ( CHARACTER ) -- Full path name which corresponds to    *
! *                               the database at the opposite band.     *
! *                               It is an empty line in modes "a" and   *
! *                               "b".                                   *
! * MESSAGE_FILE ( CHARACTER ) -- File name with a message to be sent to *
! *                               dserver.                               *
! * COMMAND_FILE ( CHARACTER ) -- File name with a C-Shell program for   *
! *                               processing a request for a database    *
! *                               submission.                            *
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
! * ### 01-OCT-1999 DBH_COMMAND_FILE v1.4 (c) L. Petrov 23-AUG-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'dclient.i'
      TYPE ( DCLIENT__STRU ) ::  DCLIENT
      CHARACTER  DBNAME*(*), FINAM1*(*), FINAM2*(*), &
     &           COMMAND_FILE*(*), MESSAGE_FILE*(*)
      LOGICAL*4  F_CONF
      INTEGER*4  IUER
      INTEGER*4  M_MAI, MIND
      PARAMETER  ( M_MAI = 16, MIND=16 )
      INTEGER*4  L_MAI, LIND, IND(2,MIND), I11, IP, J1, J2
      CHARACTER  CIS_MAI(M_MAI)*256, OUT*256, DELIM*4, USER_NAME*256, &
     &           USER_REALNAME*256, USER_E_ADDRESS*256, ANS*3
      CHARACTER  GET_CDATE*19
      PARAMETER  ( DELIM = CHAR(0)//CHAR(32)//CHAR(9)//',' )
      INTEGER*4, EXTERNAL :: LINDEX, ILEN, I_LEN
!
! --- Get information about user name
!
      CALL GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
      IF ( F_CONF ) THEN
!
! -------- Ask user a confirmation of the request
!
           WRITE ( 6, '(A)'  ) 'Dear '// &
     &                          USER_REALNAME(1:I_LEN(USER_REALNAME))//', '// &
     &                         'are you really going to submit '
           WRITE ( 6, '(A$)' ) '     '//DBNAME(1:I_LEN(DBNAME))// &
     &                         ' to the Data Center '// &
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
           CALL ERR_LOG ( 5161, IUER, 'DBH_COMMAND_FILE', 'Error in '// &
     &         'openning output file for a message '//MESSAGE_FILE )
           RETURN
      END IF
!
! --- Data type
!
      WRITE ( 11, '(A)' ) 'DATA_TYPE:     DBH'
!
! --- URL(s)
!
      WRITE ( 11, '(A)' ) 'URL:           '// &
     &                     DCLIENT%URL_PREFIX(1:I_LEN(DCLIENT%URL_PREFIX))// &
     &                     FINAM1(LINDEX(FINAM1,'/')+1:I_LEN(FINAM1))//'.gz'
      IF ( ILEN(FINAM2) .GT. 0 ) THEN
           WRITE ( 11, '(A)' ) 'URL:           '// &
     &                         DCLIENT%URL_PREFIX(1:I_LEN(DCLIENT%URL_PREFIX))// &
     &                         FINAM2(LINDEX(FINAM2,'/')+1:I_LEN(FINAM2))//'.gz'
      END IF
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
                        CALL ERR_LOG ( 5162, IUER, 'DBH_COMMAND_FILE', &
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
                        CALL ERR_LOG ( 5163, IUER, 'DBH_COMMAND_FILE', &
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
           CALL ERR_LOG ( 5164, IUER, 'DBH_COMMAND_FILE', 'Error in '// &
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
      WRITE ( 11, '(A)' ) 'set infile = '//FINAM1(1:I_LEN(FINAM1))
      WRITE ( 11, '(A)' ) 'set outfile = '// &
     &                     DCLIENT%FTP_DIR(1:I_LEN(DCLIENT%FTP_DIR))// &
     &                     FINAM1(LINDEX(FINAM1,'/')+1:I_LEN(FINAM1))//'.gz'
      WRITE ( 11, '(A)' ) 'set linkfile = '// &
     &                     DCLIENT%FTP_DIR(1:I_LEN(DCLIENT%FTP_DIR))// &
     &                     FINAM1(LINDEX(FINAM1,'/')+1:I_LEN(FINAM1))//'.gzp'
      WRITE ( 11, '(A)' ) '/bin/echo -e "gzipping $infile  ...  \c"'
      WRITE ( 11, '(A)' ) 'cat $infile | gzip -c > $outfile'
      WRITE ( 11, '(A)' ) 'if ( $status != 0 ) then'
      WRITE ( 11, '(A)' ) '     echo "Error in gzipping " $infile'
      WRITE ( 11, '(A)' ) '     exit ( 0 ) -1'
      WRITE ( 11, '(A)' ) 'endif'
      WRITE ( 11, '(A)' ) 'chmod g+rw $outfile >& /dev/null'
      IF ( ILEN(DCLIENT%CUSTOM_SCRIPT) > 0 ) THEN
           WRITE ( 11, '(A)' ) DCLIENT%CUSTOM_SCRIPT(1:I_LEN(DCLIENT%CUSTOM_SCRIPT))// &
     &                        ' '//DCLIENT%FTP_DIR(1:I_LEN(DCLIENT%FTP_DIR))// &
     &                        FINAM1(LINDEX(FINAM1,'/')+1:I_LEN(FINAM1))//'.gz'
      END IF
      WRITE ( 11, '(A)' ) 'echo "Done "'
      IF ( DCLIENT%DO_LINK(1:3) .EQ. 'YES' ) THEN
           WRITE ( 11, '(A)' ) 'ln -s $outfile $linkfile'
      END IF
      WRITE ( 11, '(A)' ) '#'
!
      IF ( ILEN(FINAM2) .GT. 0 ) THEN
           WRITE ( 11, '(A)' ) 'set infile = '//FINAM2(1:I_LEN(FINAM2))
           WRITE ( 11, '(A)' ) 'set outfile = '// &
     &                          DCLIENT%FTP_DIR(1:I_LEN(DCLIENT%FTP_DIR))// &
     &                         FINAM2(LINDEX(FINAM2,'/')+1:I_LEN(FINAM2))//'.gz'
           WRITE ( 11, '(A)' ) 'set linkfile = '// &
     &                          DCLIENT%FTP_DIR(1:I_LEN(DCLIENT%FTP_DIR))// &
     &                        FINAM2(LINDEX(FINAM2,'/')+1:I_LEN(FINAM2))//'.gzp'
           WRITE ( 11, '(A)' ) '/bin/echo -e "gzipping $infile  ...  \c"'
           WRITE ( 11, '(A)' ) 'cat $infile | gzip -c > $outfile'
           WRITE ( 11, '(A)' ) 'if ( $status != 0 ) then'
           WRITE ( 11, '(A)' ) '     echo "Error in gzipping " $infile'
           WRITE ( 11, '(A)' ) '     exit ( 0 ) -2'
           WRITE ( 11, '(A)' ) 'endif'
           WRITE ( 11, '(A)' ) 'chmod g+rw $outfile >& /dev/null'
           IF ( ILEN(DCLIENT%CUSTOM_SCRIPT) > 0 ) THEN
                WRITE ( 11, '(A)' ) DCLIENT%CUSTOM_SCRIPT(1:I_LEN(DCLIENT%CUSTOM_SCRIPT))// &
     &                              ' '//DCLIENT%FTP_DIR(1:I_LEN(DCLIENT%FTP_DIR))// &
     &                              FINAM2(LINDEX(FINAM1,'/')+1:I_LEN(FINAM2))//'.gz'
           END IF
           WRITE ( 11, '(A)' ) 'echo "Done "'
           IF ( DCLIENT%DO_LINK(1:3) .EQ. 'YES' ) THEN
                WRITE ( 11, '(A)' ) 'ln -s $outfile $linkfile'
           END IF
           WRITE ( 11, '(A)' ) '#'
      END IF
!
      WRITE ( 11, '(A)' ) DCLIENT%MAIL_COMMAND(1:I_LEN(DCLIENT%MAIL_COMMAND))// &
     &                    ' -s "Submission of '// &
     &                    DBNAME(1:I_LEN(DBNAME))//' by '//DCLIENT__LABEL// &
     &                    '" '// &
     &             DCLIENT%DSERVER_EMAIL(1:I_LEN(DCLIENT%DSERVER_EMAIL))// &
     &             ' < '//MESSAGE_FILE(1:I_LEN(MESSAGE_FILE))
      WRITE ( 11, '(A)' ) 'if ( $status != 0 ) then'
      WRITE ( 11, '(A)' ) '     exit ( 0 ) -3'
      WRITE ( 11, '(A)' ) 'endif'
      WRITE ( 11, '(A)' ) '#'
      WRITE ( 11, '(A)' ) 'echo "=== Request for submitting the database '// &
     &                     DBNAME(1:I_LEN(DBNAME))//' is sent to '// &
     &                    DCLIENT%DATA_CENTER(1:I_LEN(DCLIENT%DATA_CENTER))//'"'
      WRITE ( 11, '(A)' ) 'exit 0'
!
      CLOSE ( UNIT=11 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DBH_COMMAND_FILE  #!#
