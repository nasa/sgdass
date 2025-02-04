      SUBROUTINE RESOLVE_DBNAME ( MASTER_DIR, DB_NAME, SESS_CODE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  RESOLVE_DBNAME  resolves database name supplied as the    *
! *   input argument and returns the session code as the output argument.*
! *                                                                      *
! *   RESOLVE_DBNAME extracts the year field from the datbases name,     *
! *   builds the names of the master files which should contain this     *
! *   experiment. Then it first looks in the master file of the          *
! *   24-hour experiments, then it looks in the master file of           *
! *   intensive experiments. It finds the line in the master file which  *
! *   matches the database name and reads the session code.              *
! *                                                                      *
! *   The current version supports master file format 1.0 of 2001.08.21  *
! *   and 2.0 of 2022.11.01 .                                            *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  MASTER_DIR ( CHARACTER ) -- Directory name (with trailing / ) where *
! *                              master fiels are located.               *
! *     DB_NAME ( CHARACTER ) -- Database name of the experiment.        *
! *                              It may start or not start from the      *
! *                              $ character.                            *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *   SESS_CODE ( CHARACTER ) -- Session code as it was found in the     *
! *                              master file. RESOLVE_DBNAME returns     *
! *                              empty SESS_CODE in the case of failure. *
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
! *  ### 14-AUG-2000 RESOLVE_DBNAME v4.0 (c) L. Petrov  28-DEC-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      CHARACTER  MASTER_DIR*(*), DB_NAME*(*), SESS_CODE*(*)
      INTEGER*4  IUER
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 1024 )
      CHARACTER  DBN*9, BUF(MBUF)*128, MASTER_FILE*128, SUFFIX*32, MON_DAY*32, &
     &           DBN_MASTER*9, STR*128, SESS_FIELD*32
      CHARACTER  VER1_SIGNATURE*64, VER2_SIGNATURE*80, YEAR_4D_CHR*4
      PARAMETER  ( VER1_SIGNATURE = &
     &  '## Master file format version 1.0           2001.08.21 CCT&NRV  ' )
      PARAMETER  ( VER2_SIGNATURE = &
     &  '## Master file format version 2.0                             2022.11.01 CAD&CCT' )
      LOGICAL*4  LEX, DBG_ON
      PARAMETER  ( DBG_ON = .FALSE. )
      INTEGER*4  SES__FLD1, DAT__FLD1, CAT__FLD1, SES__FLD2, DAT__FLD2, CAT__FLD2, M__MOU, M__SUF
      PARAMETER  ( SES__FLD1 =  2 )
      PARAMETER  ( DAT__FLD1 =  3 )
      PARAMETER  ( CAT__FLD1 = 12 )
      PARAMETER  ( SES__FLD2 =  3 )
      PARAMETER  ( DAT__FLD2 =  2 )
      PARAMETER  ( CAT__FLD2 = 11 )
      PARAMETER  ( M__MOU   = 12 )
      PARAMETER  ( M__SUF   =  5 )
      CHARACTER    C_MOU(M__MOU)*3, SUF_LET(M__SUF)*1
!
      DATA C_MOU  / 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', &
     &              'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'  /
      DATA SUF_LET / 'S', 'C', 'X', 'K', 'Q' /
      INTEGER*4  NBUF, IYEAR, IDAY, IMON, ICB, ICE, IMB, IME, ISB, ISE, J1, J2, J3, &
     &           IVER_FMT, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF, MULTI_INDEX
!
      CALL CLRCH ( DBN       )
      CALL CLRCH ( SESS_CODE )
!
! --- Remove a dollar sign (RESOLVE_DBNAME doesn't take bribes!)
!
      IF ( DB_NAME(1:1) .EQ. '$' ) THEN
           DBN = DB_NAME(2:)
         ELSE
           DBN = DB_NAME
      END IF
      IF ( ILEN(DBN) .LE. 0 ) THEN
           CALL ERR_LOG ( 5221, IUER, 'RESOLVE_DBNAME', 'Empty argument '// &
     &         'database name' )
           RETURN
      END IF
!
      IF ( ILEN(DBN) .LT. 8 ) THEN
           CALL ERR_LOG ( 5223, IUER, 'RESOLVE_DBNAME', 'The database name '// &
     &          DB_NAME(1:I_LEN(DB_NAME))//' is too short: presumably, some '// &
     &         'fields are missed' )
           RETURN
      END IF
!
! --- Check whether year field is correct
!
      CALL CHIN ( DBN(1:2), IYEAR )
      IF ( IYEAR .LT. 0  .OR.  IYEAR .GT. 100 ) THEN
           CALL ERR_LOG ( 5224, IUER, 'RESOLVE_DBNAME', 'Wrong year field '// &
     &         'in the database name '//DB_NAME )
           RETURN
      END IF
!
! --- Check whether the month
!
      IF ( LTM_DIF ( 0, M__MOU, C_MOU, DBN(3:5) ) .LE. 0 ) THEN
           CALL ERR_LOG ( 5225, IUER, 'RESOLVE_DBNAME', 'Wrong month field '// &
     &         'in the database name '//DB_NAME )
           RETURN
      END IF
!
! --- Check whether the day field
!
      CALL CHIN ( DBN(6:7), IDAY )
      IF ( IDAY .LE. 0  .OR.  IDAY .GT. 31 ) THEN
           CALL ERR_LOG ( 5226, IUER, 'RESOLVE_DBNAME', 'Wrong day field '// &
     &         'in the database name '//DB_NAME )
           RETURN
      END IF
!
! --- Loop over two master files:
!
      DO 410 J1=1,2
         CALL CLRCH ( MASTER_FILE )
         IF ( IYEAR < 50 ) THEN
              YEAR_4D_CHR = '20'//DBN(1:2)
            ELSE
              YEAR_4D_CHR = '19'//DBN(1:2)
         END IF
         IF ( J1 .EQ. 1 ) THEN
!
! ----------- ... first for 24-hours experiments ...
!
              MASTER_FILE = MASTER_DIR(1:I_LEN(MASTER_DIR))//'/master'// &
     &                      YEAR_4D_CHR//'.txt'
            ELSE IF ( J1 .EQ. 2 ) THEN
!
! ----------- ... then for intensives.
!
              MASTER_FILE = MASTER_DIR(1:I_LEN(MASTER_DIR))//'/master'// &
     &                      YEAR_4D_CHR//'-int.txt'
         END IF
!
! ------ Check: whether master file exist
!
         INQUIRE ( FILE=MASTER_FILE, EXIST=LEX )
         IF ( .NOT. LEX ) THEN
              IF ( J1 .EQ. 1 ) THEN
                   CALL ERR_LOG ( 5227, IUER, 'RESOLVE_DBNAME', 'Master file '// &
     &                  MASTER_FILE(1:I_LEN(MASTER_FILE))//' for database '// &
     &                  DB_NAME(1:I_LEN(DB_NAME))//' was not found' )
                 ELSE IF ( J1 .EQ. 2 ) THEN
                   CALL ERR_LOG ( 5228, IUER, 'RESOLVE_DBNAME', 'Database '// &
     &                  'name for '//DB_NAME(1:I_LEN(DB_NAME))//' was not '// &
     &                  'resolved in 24 hour master file and the master '// &
     &                  'file for intensive sessions '// &
     &                  MASTER_FILE(1:I_LEN(MASTER_FILE))//' was not found' )
              END IF
              RETURN
         END IF
!
! ------ Read configuration file into the buffer BUF
!
         CALL ERR_PASS ( IUER, IER )
         CALL RD_TEXT  ( MASTER_FILE, MBUF, BUF, NBUF, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5229, IUER, 'RESOLVE_DBNAME', 'Error in attempt '// &
     &            'to read master file '//MASTER_FILE(1:I_LEN(MASTER_FILE))// &
     &            ' for database '//DB_NAME(1:I_LEN(DB_NAME))// &
     &            ' was not found' )
              RETURN
         END IF
!
         IF ( BUF(1)(1:64) == VER1_SIGNATURE ) THEN
              IVER_FMT = 1
          ELSE IF ( BUF(1)(1:80) .NE. VER2_SIGNATURE ) THEN
              IVER_FMT = 2
          ELSE 
              CALL TRAN  ( 13, BUF(1)(1:80), STR(1:80) )
              CALL ERR_LOG ( 5230, IUER, 'RESOLVE_DBNAME', 'Master file '// &
     &             MASTER_FILE(1:I_LEN(MASTER_FILE))//' has an '// &
     &            'unrecognizable format. The first line has an unknown '// &
     &            'signature: '//STR(1:80) )
              RETURN
         END IF
!
! ------ Parse the buffer with contents of master file
!
         DO 420 J2=1,NBUF
            IF ( BUF(J2)(1:1) .NE. '|' ) GOTO 420
!
! --------- Extract the day and month field and convert to the upper register
!
            IF ( IVER_FMT == 1 ) THEN
                 IMB = MULTI_INDEX ( DAT__FLD1,   BUF(J2), '|' ) + 1
                 IME = MULTI_INDEX ( DAT__FLD1+1, BUF(J2), '|' ) - 1
                 IF ( IME .LT. IMB ) IMB = IME
                 IF ( IME .LE. 0   ) GOTO 420
                 CALL CLRCH  (                       MON_DAY )
                 CALL TRAN   ( 11, BUF(J2)(IMB:IME), MON_DAY )
                 CALL CHASHL (                       MON_DAY )
               ELSE IF ( IVER_FMT == 2 ) THEN
                 IMB = MULTI_INDEX ( DAT__FLD2,   BUF(J2), '|' ) + 1
                 IME = MULTI_INDEX ( DAT__FLD2+1, BUF(J2), '|' ) - 1
                 IF ( IME .LT. IMB ) IMB = IME
                 IF ( IME .LE. 0   ) GOTO 420
                 CALL CHIN ( BUF(J2)(IMB+4:IMB+5), IMON )
                 MON_DAY = C_MOU(IMON)//BUF(J2)(IMB+6:IMB+7)
            END IF
!
! --------- Extract the suffix field and convert to the upper register
!
            IF ( IVER_FMT == 1 ) THEN
                 ICB = MULTI_INDEX ( CAT__FLD1,   BUF(J2), '|' ) +1
                 ICE = MULTI_INDEX ( CAT__FLD1+1, BUF(J2), '|' ) -1
               ELSE IF ( IVER_FMT == 2 ) THEN
                 ICB = MULTI_INDEX ( CAT__FLD2,   BUF(J2), '|' ) +1
                 ICE = MULTI_INDEX ( CAT__FLD2+1, BUF(J2), '|' ) -1
            END IF
            IF ( ICE .LT. ICB ) ICB = ICE
            IF ( ICE .LE. 0   ) GOTO 420
            CALL CLRCH  (                       SUFFIX )
            CALL TRAN   ( 11, BUF(J2)(ICB:ICE), SUFFIX )
            CALL CHASHL (                       SUFFIX )
!
! --------- Cycle over M__SUF possible Band identifier in the suffix
!
            DO 430 J3=0,M__SUF
               IF ( J3 == 0 ) THEN
                    DBN_MASTER = DBN(1:2)//MON_DAY(1:5)//SUFFIX(1:2)
                  ELSE
                    DBN_MASTER = DBN(1:2)//MON_DAY(1:5)//SUF_LET(J3)(1:1)//SUFFIX(2:2)
               END IF
               IF ( DBG_ON ) THEN
                    WRITE ( 6, * ) ' dbn  >>',dbn,'<<   |   >>',dbn_master,'<< '
               END IF
!
! ------------ Does this line match to the database name?
!
               IF ( DBN .EQ. DBN_MASTER ) THEN
!
! ----------------- Yeah! It matches, matches!!!
!
! ----------------- Extract the session field and convert it to the lower
! ----------------- register
!
                    IF ( IVER_FMT == 1 ) THEN
                         ISB = MULTI_INDEX ( SES__FLD1,   BUF(J2), '|' ) +1
                         ISE = MULTI_INDEX ( SES__FLD1+1, BUF(J2), '|' ) -1
                       ELSE 
                         ISB = MULTI_INDEX ( SES__FLD2,   BUF(J2), '|' ) +1
                         ISE = MULTI_INDEX ( SES__FLD2+1, BUF(J2), '|' ) -1
                    END IF 
                    IF ( ISE .LT. ISB ) ISB = ISE
                    IF ( ISE .LE. 0   ) GOTO 420
                    CALL CLRCH  (                       SESS_FIELD )
                    CALL TRAN   ( 12, BUF(J2)(ISB:ISE), SESS_FIELD )
                    CALL CHASHL (                       SESS_FIELD )
                    SESS_CODE = SESS_FIELD
                    GOTO 810
               END IF
 430        CONTINUE
 420     CONTINUE
         IF ( BUF(NBUF) .NE. BUF(1) ) THEN
              CALL ERR_LOG ( 5231, IUER, 'RESOLVE_DBNAME', 'Footer line '// &
     &            'of the master file '//MASTER_FILE(1:I_LEN(MASTER_FILE))// &
     &            'was was not found. Presumably, the file has not been '// &
     &            'read to the end' )
              RETURN
         END IF
 410  CONTINUE
 810  CONTINUE
      IF ( ILEN(SESS_CODE) .EQ. 0 ) THEN
           CALL ERR_LOG ( 5232, IUER, 'RESOLVE_DBNAME', 'Failure to resolve '// &
     &         'database name '//DB_NAME(1:I_LEN(DB_NAME))//' -- the '// &
     &         'database name was not found in the master file' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  RESOLVE_DBNAME  !#!#
