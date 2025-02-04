      SUBROUTINE PARU_COMPILE ( PARU_FIL, PAR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PARU_COMPILE  compiles PAR-program from source file       *
! *   PARU_FIL  and forms the record with object code PAR.               *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * PARU_FIL ( CHARACTER ) -- File name of the PARU-program which        *
! *                           assumed to be used further.                *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *      PAR ( RECORD    ) -- Data structure which contains "object      *
! *                           code" of PARU-program: stream of functions *
! *                           to be executed and their parameters record.*
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  17-MAR-98  PARU_COMPILE  v1.1  (c)  L. Petrov  23-DEC-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'pamb.i'
      CHARACTER  PARU_FIL*(*)
      TYPE ( PAR__STRU ) ::  PAR
      INTEGER*4  IUER
!
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 2048 )
      PARAMETER  ( MIND = 64   )
      CHARACTER  BUF(MBUF)*132, STR*132, STR1*132, OUT*2048, ARG*2048
      INTEGER*4  NBUF, IER, LIND, IND(2,MIND), J1, KEND, IFUN, IARG, IP
      LOGICAL*4  LEX, F_HEADER, F_CONT, F_SEARCH_OP, F_SEARCH_CP
!
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF, SEARCH_OP, SEARCH_CP
!
! --- Does the file name for PARUY program is specified?
!
      IF ( ILEN(PARU_FIL) .EQ. 0 ) THEN
           CALL ERR_LOG ( 6911, IUER, 'PARU_COMPILE', 'File name with PARU '// &
     &          'programm is empty' )
           RETURN
      END IF
!
! --- Does the file name with PARU-program exist?
!
      INQUIRE ( FILE=PARU_FIL, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 6912, IUER, 'PARU_COMPILE', 'File '// &
     &          PARU_FIL(1:I_LEN(PARU_FIL))//' with PARU-prigram is not '// &
     &         'found' )
           RETURN
      END IF
!
! --- Reading the text of PARU-pgramm and putting it in the text buffer
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( PARU_FIL, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6913, IUER, 'PARU_COMPILE', 'Error in reading of '// &
     &          'source code of PARU-program from file '//PARU_FIL )
           RETURN
      END IF
!
! --- Initialization
!
      F_HEADER    = .FALSE.
      F_CONT      = .FALSE.
      F_SEARCH_OP = .FALSE.
      F_SEARCH_CP = .FALSE.
      PAR%N_PRC   = 0
      CALL CLRCH ( PAR%FINAM )
      PAR%FINAM   = PARU_FIL
      PAR%STATUS  = PAR__INCOMPLETE
!
! --- Reading PARU-file. Cycle on the line of PARU-file
!
      DO 410 J1=1,NBUF
!
! ------ If the first symbol of the line was comment -- bypass this line
!
         IF ( BUF(J1)(1:1) .EQ. '!' ) GOTO 410
!
! ------ Transformation to the symbols of upper registr
!
         CALL TRAN ( 11, BUF(J1), BUF(J1) )
!
! ------ KEND is the position of the last analysed symbol of the parsed line
!
         KEND = INDEX ( BUF(J1), '!' ) - 1
         IF ( KEND .LE. 0 ) KEND = ILEN(BUF(J1))
!
         IF ( .NOT. F_HEADER ) THEN
!
! ----------- We check the first not commented out line: it should contain word
! ----------- PARU in the first 4 symbols. It is indication what we parse just
! ----------- PARU program, but not, say fortran program.
!
              IF ( BUF(J1)(1:4) .EQ. 'PARU' ) THEN
                   F_HEADER = .TRUE.
                ELSE
                   CALL TRAN ( 13, BUF(J1)(1:4), STR )
                   CALL ERR_LOG ( 6914, IUER, 'PARU_COMPILE', 'Header '// &
     &                 'statement has not been found: "'//STR(1:4)// &
     &                 '" has been read at the line '//STR1(1:I_LEN(STR1))// &
     &                 '. Expected: "PARU"  .' )
                   RETURN
              END IF
              GOTO 410
         END IF
!
         IF ( .NOT. F_CONT ) THEN
!
! ----------- This line is not continuation of the previous line
!
! ----------- Setting off flags of necessity to search openning or closing
! ----------- parenthsesis at the next line
!
              F_SEARCH_OP = .FALSE.
              F_SEARCH_CP = .FALSE.
!
              CALL CLRCH ( STR1 )
              CALL INCH  ( J1, STR1 )
!
! ----------- Parse the line on words
!
              CALL EXWORD ( BUF(J1)(1:KEND), MIND, LIND, IND, ' (', -3 )
!
! ----------- Search the first words in the table of function names
!
              IFUN = LTM_DIF ( 1, MPAR_FUN, PAR_FUN, BUF(J1)(IND(1,1):IND(2,1)))
              IF ( IFUN .LE. 0 ) THEN
                   CALL LIST_TO_LINE ( MPAR_FUN, PAR_FUN, ',', OUT )
                   CALL ERR_LOG ( 6915, IUER, 'PARU_COMPILE', 'Unrecgnized '// &
     &                 'function at the line '//STR1(1:I_LEN(STR1))// &
     &                 ': "'//BUF(J1)(IND(1,1):IND(2,1))//'". Expected: one '// &
     &                 'of '//OUT )
                   RETURN
              END IF
!
! ----------- Search for the openning parenthesis.
!
              IP = IND(2,1)+1
              IF ( IP .LE. KEND ) THEN
!
! ---------------- Pointer IP points
! ----------------   BEFORE operation: to the symbols just after functuon name
! ----------------   AFTER operation:  to the symbols just after openning
! ----------------                     parenthsesis
!
                   CALL ERR_PASS  ( IUER, IER )
                   IP = SEARCH_OP ( '(', BUF(J1)(IP:KEND), F_CONT, F_SEARCH_OP, &
     &                              STR1, IER ) + IP
                   IF ( IER .NE. 0 ) THEN
!
! --------------------- Error occured in search
!
                        CALL ERR_LOG ( 6916, IUER, 'PARU_COMPILE' )
                        RETURN
                   END IF
                 ELSE
!
! ---------------- There were no one symbol to the end of this line starting
! ---------------- from the current position. Therefore it is usefull and
! ---------------- harmfull to try to find the parenthesis at this line.
! ---------------- We should look at the next line
!
                   F_CONT = .TRUE.
                   F_SEARCH_OP = .TRUE.
              END IF
              IF ( F_CONT ) GOTO 410
!
              IARG = 0
              CALL CLRCH ( ARG )
!
              IF ( IP .LE. KEND ) THEN
!
! ---------------- Search for the closing parenthsesis
!
                   CALL ERR_PASS  ( IUER, IER )
!
! ---------------- Pointer IP points
! ----------------   BEFORE operation: to the symbols just after functuon name
! ----------------   AFTER operation:  to the symbols just after closing
! ----------------                     parenthsesis
!
                   IP = SEARCH_CP ( '(', ')', BUF(J1)(IP:KEND), F_CONT, &
     &                              F_SEARCH_CP, IARG, ARG, STR1, IER ) + IP
                   IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 6917, IUER, 'PARU_COMPILE' )
                         RETURN
                   END IF
              END IF
              IF ( F_CONT ) GOTO 410
!
! ----------- Well we detected openning and closing parentheses. Now we should
! ----------- parse the content wat was found between them. Content was glued
! ----------- togeater (if it was split at severeal lines) and put into
! ----------- the string ARG.
!
! ----------- Parsing arguments list and putting object code to the PAR record
!
              IF ( IARG .EQ. 0 ) IARG =1
              CALL ERR_PASS  ( IUER , IER )
              CALL PARSE_ARG ( IFUN, ARG(1:IARG), PAR, STR1, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6918, IUER, 'PARU_COMPILE', 'Error of '// &
     &                 'parsing argument list for function '// &
     &                  PAR_FUN(IFUN)(1:I_LEN(PAR_FUN(IFUN)))// &
     &                 ' at the line '//STR1 )
                   RETURN
              END IF
           ELSE IF ( F_CONT .AND. F_SEARCH_OP ) THEN
!
! ----------- This line was continuation line.
! ----------- Search of the openning parenthesis.
!
              CALL ERR_PASS  ( IUER, IER )
              IP = SEARCH_OP ( '(', BUF(J1)(1:KEND), F_CONT, F_SEARCH_OP, STR1, &
     &                         IER )
              IF ( IER .NE. 0 ) THEN
!
! ---------------- Error occured in search
!
                   CALL ERR_LOG ( 6919, IUER, 'PARU_COMPILE' )
                   RETURN
              END IF
              IARG = 0
              CALL CLRCH ( ARG )
           ELSE IF ( F_CONT .AND. F_SEARCH_CP ) THEN
!
! ----------- This line was continuation line.
! ----------- Search of the closing parenthesis.
!
              CALL ERR_PASS ( IUER, IER )
              IP = SEARCH_CP ( '(', ')', BUF(J1)(1:KEND), F_CONT, F_SEARCH_CP, &
     &                         IARG, ARG, STR1, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6920, IUER, 'PARU_COMPILE' )
                   RETURN
              END IF
!
              IF ( F_CONT ) GOTO 410
!
! ----------- Parsing arguments list and putting object code to the PAR record
!
              IF ( IARG .EQ. 0 ) IARG =1
              CALL ERR_PASS  ( IUER , IER )
              CALL PARSE_ARG ( IFUN, ARG(1:IARG), PAR, STR1, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6921, IUER, 'PARU_COMPILE', 'Error of '// &
     &                 'parsing argument list for function '// &
     &                  PAR_FUN(IFUN)(1:I_LEN(PAR_FUN(IFUN)))// &
     &                 ' at the line '//STR1 )
                   RETURN
              END IF
         ENDIF
 410  CONTINUE
!
! --- Checks of status of compilation
!
      IF ( .NOT. F_HEADER ) THEN
           CALL ERR_LOG ( 6922, IUER, 'PARU_COMPILE', 'Header statement has '// &
     &         'not been found. Program has only comments' )
           RETURN
      END IF
!
      IF ( F_CONT .AND. F_SEARCH_OP ) THEN
           CALL ERR_LOG ( 6923, IUER, 'PARU_COMPILE', 'Line '// &
     &          STR1(1:I_LEN(STR1))//': openning parenthsis has not been '// &
     &         'found before the end of source file' )
           RETURN
      END IF
!
      IF ( F_CONT .AND. F_SEARCH_CP ) THEN
           CALL ERR_LOG ( 6924, IUER, 'PARU_COMPILE', 'Line '// &
     &          STR1(1:I_LEN(STR1))//': closing parenthsis has not been '// &
     &         'found before the end of source file' )
           RETURN
      END IF
!
! --- Everething was OK? Set satatus "complete". Uhh!
!
      PAR%STATUS  = PAR__COMPLETE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PARU_COMPILE  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   SEARCH_OP ( OP, LINE, F_CONT, F_SEARCH_OP, STR1, IUER )
! ************************************************************************
! *                                                                      *
! *   Rutine  SEARCH_OP  searchs for openning parenthesis in the line    *
! *   LINE.                                                              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *          OP ( CHARACTER ) -- Symbol of the open parenthesis.         *
! *        LINE ( CHARACTER ) -- The line to be analyzed.                *
! *        STR1 ( CHARACTER ) -- Line with the index of parsing string   *
! *                              in the text to refer to it in the       *
! *                              error message if needed.                *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * F_SEARCH_OP ( LOGICAL*4 ) -- Flag. .TRUE. means that parenthesis was *
! *                              not found and parsing of the next line  *
! *                              is needed.                              *
! *      F_CONT ( LOGICAL*4 ) -- It has the same value as F_SEARCH_OP.   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  17-MAR-98    SEARCH_OP   v1.0  (c)  L. Petrov  23-DEC-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  SEARCH_OP, IUER
      LOGICAL*4  F_CONT, F_SEARCH_OP
      CHARACTER  LINE*(*), STR1*(*), OP*1
      INTEGER*4  IE, LIB$SKPC, I_LEN
!
! --- Search the openning parenthesis. To do it firstly find the first
! --- non-blank symbol
!
      IE = LIB$SKPC ( ' ', LINE )
      IF ( IE .LE. 0 ) THEN
!
! -------- Line contains only blanks. Go to the next line
!
           F_CONT      = .TRUE.
           F_SEARCH_OP = .TRUE.
           SEARCH_OP   = 0
           CALL ERR_LOG ( 0, IUER )
           RETURN
         ELSE
           IF ( LINE(IE:IE) .NE. OP ) THEN
                CALL ERR_LOG ( 6931, IUER, 'SEARCH_OP', 'Error at '// &
     &              'the line '//STR1(1:I_LEN(STR1))//': symbol "'// &
     &               LINE(IE:IE)//'" has been read. Openning parenthesis '// &
     &              'was expected' )
                RETURN
          END IF
      END IF
!
      F_SEARCH_OP = .FALSE.
      F_CONT      = .FALSE.
      SEARCH_OP   = IE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SEARCH_OP  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   SEARCH_CP ( OP, CP, LINE, F_CONT, F_SEARCH_CP, IARG, ARG, &
     &                       STR1, IUER )
! ************************************************************************
! *                                                                      *
! *   Rutine  SEARCH_CP  searches closing parentesis in the line LINE    *
! *   and adds content of the text before the parenteses to the line     *
! *   ARG.                                                               *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *          OP ( CHARACTER ) -- Symbol for the opening parenthesis.     *
! *          CP ( CHARACTER ) -- Symbol for the closing parenthesis.     *
! *        LINE ( CHARACTER ) -- The line to be analyzed.                *
! *        STR1 ( CHARACTER ) -- Line with the index of parsing string   *
! *                              in the text to refer to it in the       *
! *                              error message if needed.                *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      F_CONT ( LOGICAL*4 ) -- It has the same value as F_SEARCH_OP.   *
! * F_SEARCH_CP ( LOGICAL*4 ) -- Flag. .TRUE. means that parenthesis was *
! *                              not found and parsing of the next line  *
! *                              is needed.                              *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * IARG ( INTEGER*4      ) -- Number of symbols in the line ARG.        *
! *  ARG ( CHARACTER      ) -- String with content between the openning  *
! *                            parenthesis and the closing one.          *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  17-MAR-98    SEARCH_CP   v1.1  (c)  L. Petrov  23-DEC-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  SEARCH_CP, IARG, IUER
      LOGICAL*4  F_CONT, F_SEARCH_CP
      CHARACTER  LINE*(*), ARG*(*), STR1*(*), CP*1, OP*1
      INTEGER*4  IP
      INTEGER*4, EXTERNAL :: ILEN
!
! --- Search for closing parenthesis
!
      IP = INDEX ( LINE, CP )
      IF ( IP .GT. 0 ) THEN
!
! -------- We found closing parenthesis
!
           IF ( IP-1 .GT. 0 ) THEN
                IF ( INDEX ( LINE(1:IP-1), OP ) .GT. 0 ) THEN
                     CALL ERR_LOG ( 6932, IUER, 'SEARCH_CP', 'The second '// &
     &                   'openning parenthesis before closing parenthesis '// &
     &                   'encountered at the line '//STR1 )
                     RETURN
                END IF
!
! ------------- Add context of the line before closing parenthseis to ARG
!
                IF ( ILEN(ARG) .EQ. 0 ) THEN
                     ARG = LINE(1:IP-1)
                  ELSE
                     ARG = ARG(1:IARG)//LINE(1:IP-1)
                END IF
                IARG = IARG + IP-1
           END IF
!
           F_CONT      = .FALSE.
           F_SEARCH_CP = .FALSE.
           SEARCH_CP    = IP
         ELSE
!
! -------- We failed to
!
           IF ( ILEN(LINE) .GT. 0 ) THEN
                IF ( INDEX ( LINE, OP ) .GT. 0 ) THEN
                     CALL ERR_LOG ( 6933, IUER, 'SEARCH_CP', 'The second '// &
     &                   'openning parenthesis before closing parenthesis '// &
     &                   'encountered at the line '//STR1 )
                     RETURN
                END IF
!
! ------------- Add context of the line to ARG and one extra blank as a mark
! ------------- of end of line
!
                IF ( IARG .EQ. 0 ) THEN
                     ARG = LINE(1:ILEN(LINE))//' '
                  ELSE
                     ARG = ARG(1:IARG)//LINE(1:ILEN(LINE))//' '
                END IF
                IARG = IARG + ILEN(LINE) + 1
           END IF
!
           F_CONT      = .TRUE.
           F_SEARCH_CP = .TRUE.
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SEARCH_CP  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PARSE_ARG ( IFUN, ARG, PAR, STR1, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PARSE_ARG  parses the argument list and added the next    *
! *   PAR-record to the PAR-array.                                       *
! *                                                                      *
! * ________________________ Intput parameters: ________________________ *
! *                                                                      *
! * IFUN ( INTEGER*4  ) -- Function code.                                *
! *  ARG ( CHARACTER  ) -- Line of arguments.                            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  PAR ( RECORD    ) -- Data structure which contains "object code"    *
! *                       of PARU-program: stream of functions to be     *
! *                       executed and their parameters record.          *
! * STR1 ( CHARACTER ) -- Line with the index of parsing string in the   *
! *                       text to refer to it in the error message if    *
! *                        needed.                                       *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  17-MAR-98    PARSE_ARG   v1.1  (c)  L. Petrov  23-DEC-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'pamb.i'
      TYPE ( PAR__STRU ) ::  PAR
      INTEGER*4  IFUN, IUER
      CHARACTER  ARG*(*)
      INTEGER*4  MIND, M_LST
      PARAMETER  ( MIND = M_KWD*2 )
      PARAMETER  ( M_LST = MAX ( L_VAL, M_LKW ) )
      CHARACTER  KEYWORD(M_KWD)*32, VALUE(M_KWD)*512, STR*32, STR1*32, &
     &           C_LST(M_LST)*32, OUT*2048
      LOGICAL*4  F_CONT, F_SEARCH_CP
      INTEGER*4  LIND, LIND2, IND(2,MIND), IND2(2,MIND), IW, IOS, IER, IARG, &
     &           J1, J2, J3, J4, J5, J6, IS, IKEY, IL, IND_KEY, IVAL, L_LST, &
     &           L_KWD, N_PRC, IPOI_MIN, IPOI_MAX
      REAL*8     RVAL
      INTEGER*4, EXTERNAL :: I_LEN, LTM_DIF, SEARCH_CP
!
! --- Parsing arguments list onto words. Blank, comma and "=" sign are used as
! --- delimiters
!
      CALL ERR_PASS ( IUER, IER )
      CALL EXWORD ( ARG, MIND, LIND, IND, ' ,=', IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6941, IUER, 'PARSE_ARG', 'Error in parsing '// &
     &         'argument list on words' )
           RETURN
      END IF
!
! --- Update of the function counter and making some initializations
!
      PAR%N_PRC       = PAR%N_PRC+1
      N_PRC           = PAR%N_PRC
      PAR%ISTS(N_PRC) = PAR__INCOMPLETE
      PAR%IFUN(N_PRC) = IFUN
!
      IF ( LIND .EQ. 0 ) THEN
!
! -------- No words has been found in argument list
!
           IF ( PAR_NUK(IFUN) .EQ. 0 ) THEN
!
! ------------- It is OK. This function shouldn't have keywords
!
                PAR%NKWD(N_PRC) = 0
                PAR%ISTS(N_PRC) = PAR__COMPLETE
!
! ------------- Work is completed
!
                CALL ERR_LOG ( 0, IUER )
                RETURN
              ELSE
                CALL CLRCH ( STR )
                CALL INCH  ( PAR_NUK(IFUN), STR )
                CALL ERR_LOG ( 6942, IUER, 'PARSE_ARG', 'Argument list is '// &
     &              'empty. '//STR(1:I_LEN(STR))//' keywords were expected' )
                RETURN
           END IF
      END IF
!
      L_KWD = 0
      IW=0
!
! --- Cycle on expected keywords
!
      DO 410 J1=1,M_KWD
         CALL CLRCH (     STR )
         CALL INCH  ( J1, STR )
!
! ------ Firstly, extraction of the J1-th keyword
!
         IW = IW+1
         CALL CLRCH ( KEYWORD(J1) )
         KEYWORD(J1) = ARG(IND(1,IW):IND(2,IW))
!
! ------ Extraction of the value of J1-th keyword
!
         IW = IW+1
         IF ( IW .GT. LIND ) THEN
              CALL ERR_LOG ( 6943, IUER, 'PARSE_ARG', 'Argument list is not '// &
     &            'complete: it ended just after the '//STR(1:I_LEN(STR))// &
     &             '-th keyword and delimiter or value is lost' )
              RETURN
         END IF
!
! ------ Check of a delimiter
!
         IF ( INDEX ( ARG(IND(2,IW-1):IND(1,IW)), '=' ) .EQ. 0 ) THEN
              CALL CLRCH (     STR )
              CALL INCH  ( J1, STR )
              WRITE ( 6, * ) ' IW=',IW,' Excerpt: '//'>>'// &
     &                                     ARG(IND(2,IW-1):IND(1,IW))//'<<'
              CALL ERR_LOG ( 6944, IUER, 'PARSE_ARG', 'Delimiter between the '// &
     &             STR(1:I_LEN(STR))//'-th word and its value is absent. '// &
     &            '"=" was expected.' )
              RETURN
         END IF
!
! ------ Get value
!
         CALL CLRCH ( VALUE(J1) )
         VALUE(J1) = ARG(IND(1,IW):IND(2,IW))
         IF ( VALUE(J1)(1:1) .EQ. '{' ) THEN
!
! ----------- Value is a list of words. It starts from parenthesis.
! ----------- Search for the closing parenthesis
!
              IARG = 0
              CALL CLRCH ( VALUE(J1) )
              CALL ERR_PASS ( IUER, IER )
              IS = SEARCH_CP ( '{', '}', ARG(IND(1,IW)+1:), F_CONT, &
     &                         F_SEARCH_CP, IARG, VALUE(J1), STR1, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6945, IUER, 'PARSE_ARG', 'Error in parsing '// &
     &                  'keywords in the '//STR1(1:I_LEN(STR1))//'-th string' )
                   RETURN
              END IF
              CALL CHASHL ( VALUE(J1) )
!
! ----------- Counting words in the KEYWORD list
!
              CALL EXWORD ( VALUE(J1), MIND, LIND2, IND2, ' ,=', IER )
!
              IW = IW + LIND2 + 1
         END IF
!
         IF ( IW .EQ. LIND ) THEN
!
! ----------- Getting out from the cycle
!
              L_KWD = J1
              GOTO 810
         END IF
 410  CONTINUE
!
      CALL CLRCH ( STR )
      CALL INCH  ( M_KWD, STR )
      IF ( IW .LT. LIND ) THEN
           CALL ERR_LOG ( 6946, IUER, 'PARSE_ARG', 'Argument list contains '// &
     &         'too much keywords: more than '//STR(1:I_LEN(STR)) )
           RETURN
      END IF
 810  CONTINUE
!
      PAR%NKWD(N_PRC) = L_KWD
!
! --- Check the number of keywords
!
      IF ( L_KWD .NE. PAR_NUK(IFUN) ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( L_KWD, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( PAR_NUK(IFUN), STR1 )
!
           L_LST = PAR_NUK(IFUN)
           DO 420 J2=1,L_LST
              CALL CLRCH ( C_LST(J2) )
              C_LST(J2) = PAR_KWD( PAR_SHK(J2,IFUN) )
 420       CONTINUE
           CALL LIST_TO_LINE ( L_LST, C_LST, ',', OUT )
           CALL ERR_LOG ( 6947, IUER, 'PARSE_ARG', 'Argument list contains '// &
     &          STR(1:I_LEN(STR))//' keywords. '//STR1(1:I_LEN(STR1))// &
     &         ' keywords were expected: '//OUT )
           RETURN
      END IF
!
! --- Initialization: setting status of values: undefined
!
      DO 430 J3=1,M_KWD
         PAR%ITYP(J3,N_PRC) = PAR__UTYP
 430  CONTINUE
!
! --- Cycle on keywords
!
      DO 440 J4=1,L_KWD
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!         write ( 6, * ) ' J4=',J4,' keyword=',keyword(J4)(1:i_len(keyword(J4))), &  ! %%%
!     &                    ' value=',value(J4)(1:i_len(value(J4)))         ! %%%%%%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------ Build the list of supported keywords for parsed function
!
         L_LST = PAR_NUK(IFUN)
         DO 450 J5=1,L_LST
            CALL CLRCH ( C_LST(J5) )
            C_LST(J5) = PAR_KWD( PAR_SHK(J5,IFUN) )
 450     CONTINUE
!
! ------ Search of the keyword in the table
!
         IL = LTM_DIF ( 0, L_LST, C_LST, KEYWORD(J4) )
         IF ( IL .LE. 0 ) THEN
              CALL LIST_TO_LINE ( L_LST, C_LST, ',', OUT )
              CALL ERR_LOG ( 6948, IUER, 'PARSE_ARG', 'Unrecognized keyword '// &
     &            'found in the argument list: "'// &
     &             KEYWORD(J4)(1:I_LEN(KEYWORD(J4)))// &
     &            '" one of '//OUT(1:I_LEN(OUT))//' was expected' )
              RETURN
         END IF
!
! ------ Putting the index of found key to PAR
!
         IKEY = PAR_SHK(IL,IFUN)
         IND_KEY = IL
!
         IF ( PAR%ITYP(IND_KEY,N_PRC) .NE. PAR__UTYP ) THEN
              CALL ERR_LOG ( 6949, IUER, 'PARSE_ARG', 'Keyword '// &
     &             KEYWORD(J4)(1:I_LEN(KEYWORD(J4)))//' used twice' )
              RETURN
         END IF
!
! ------ Assign the type of the J4-th keyword
!
         IF ( PAR_NUV(IKEY) .GE. 1 ) THEN
              PAR%ITYP(IND_KEY,N_PRC) = PAR__CTYP
            ELSE IF ( PAR_NUV(IKEY) .EQ. -1 ) THEN
              PAR%ITYP(IND_KEY,N_PRC) = PAR__ITYP
            ELSE IF ( PAR_NUV(IKEY) .EQ. -2 ) THEN
              PAR%ITYP(IND_KEY,N_PRC) = PAR__RTYP
            ELSE IF ( PAR_NUV(IKEY) .EQ. -3 ) THEN
              PAR%ITYP(IND_KEY,N_PRC) = PAR__CTYP
            ELSE
              PAR%ITYP(IND_KEY,N_PRC) = PAR__UTYP
         END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!         write ( 6,  * ) ' J4=',J4,' keyword=',keyword(J4)(1:i_len(keyword(J4))), & ! %%%
!     &                    ' value=',value(J4)(1:i_len(value(J4))), &       ! %%%
!     &          ' ikey=',ikey,' ind_key=',ind_key,' ityp=', &              ! %%%
!     &            par%ityp(ikey,n_cal), ' n_cal = ',n_cal                 ! %%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------ Build the list of supported values for this keyword
!
         IF ( PAR%ITYP(IND_KEY,N_PRC) .EQ. PAR__CTYP ) THEN
              L_LST = PAR_NUV(IKEY)
              IF ( L_LST .GT. 0 ) THEN
!
! ---------------- Value in according with the list
!
                   DO 460 J6=1,L_LST
                      CALL CLRCH ( C_LST(J6) )
                      C_LST(J6) = PAR_VAL( PAR_SHV(J6,IKEY) )
 460               CONTINUE
!
! ---------------- Search for the value of keyword in this list
!
                   IL = LTM_DIF ( 1, L_LST, C_LST, VALUE(J4) )
                   IF ( IL .LE. 0 ) THEN
                        CALL LIST_TO_LINE ( L_LST, C_LST, ',', OUT )
                        CALL ERR_LOG ( 6950, IUER, 'PARSE_ARG', &
     &                      'Unrecognized value "'// &
     &                       VALUE(J4)(1:I_LEN(VALUE(J4)))// &
     &                      '" of the keyword '// &
     &                       KEYWORD(J4)(1:I_LEN(KEYWORD(J4)))// &
     &                      ' was found in the argument list: one of "'// &
     &                       OUT(1:I_LEN(OUT))//'" was expected' )
                       RETURN
                   END IF
                   IVAL = PAR_SHV(IL,IKEY)
                   PAR%CKWD(IND_KEY,N_PRC) = PAR_VAL(IVAL)
                 ELSE
!
! ---------------- Free value
!
                   PAR%CKWD(IND_KEY,N_PRC) = VALUE(J4)
              END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!         write ( 6, * ) ' J4=',J4,' PAR%CKWD(IND_KEY,N_PRC)= ', PAR%CKWD(IND_KEY,N_PRC)  ! %%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           ELSE IF ( PAR%ITYP(IND_KEY,N_PRC) .EQ. PAR__ITYP ) THEN
!
! ----------- INTEGER*4  type
!
              READ ( UNIT=VALUE(J4), FMT='(I8)', IOSTAT=IOS ) IVAL
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 6951, IUER, 'PARSE_ARG', 'Error conversion '// &
     &                 'CHARACTER --> INTEGER*4 for the value ='// &
     &                  VALUE(J4)(1:I_LEN(VALUE(J4)))//' of the keyword '// &
     &                  KEYWORD(J4) )
                   RETURN
              END IF
!
! ----------- Getting pointers to the restrictions
!
              IPOI_MIN = PAR_SHV(1,IKEY)
              IPOI_MAX = PAR_SHV(2,IKEY)
!
! ----------- Check: are pointers valid?
!
              IF ( IPOI_MIN .GT. L_IVL ) THEN
                   WRITE ( 6, * ) ' ifun=',ifun,' ikey=',ikey
                   WRITE ( 6, * ) ' ipoi_min = ',ipoi_min,' l_ivl = ',l_ivl
                   CALL ERR_LOG ( 6952, IUER, 'PARSE_ARG', 'Internal error '// &
     &                 'of PAR-compiler' )
                   RETURN
              END IF
!
              IF ( IPOI_MAX .GT. L_IVL ) THEN
                   WRITE ( 6, * ) ' ifun=',ifun,' ikey=',ikey
                   WRITE ( 6, * ) ' ipoi_max = ',ipoi_max,' l_ivl = ',l_ivl
                   CALL ERR_LOG ( 6953, IUER, 'PARSE_ARG', 'Internal error '// &
     &                 'of PAR-compiler' )
                   RETURN
              END IF
!
! ----------- Check for the low limit if necessary
!
              IF ( IPOI_MIN .GT. 0 ) THEN
                   IF ( IVAL .LT. PAR_IVL(IPOI_MIN) ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( PAR_IVL(IPOI_MIN), STR )
                        CALL ERR_LOG ( 6954, IUER, 'PARU_COMPILE', 'Value '// &
     &                       VALUE(J4)(1:I_LEN(VALUE(J4)))//' of the keyword '// &
     &                       KEYWORD(J4)(1:I_LEN(KEYWORD(J4)))// &
     &                      ' is less than low limit '//STR )
                        RETURN
                   END IF
              END IF
!
! ----------- Check for the upper limit if necessary
!
              IF ( IPOI_MAX .GT. 0 ) THEN
                   IF ( IVAL .GT. PAR_IVL(IPOI_MAX) ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( PAR_IVL(IPOI_MAX), STR )
                        CALL ERR_LOG ( 6955, IUER, 'PARU_COMPILE', 'Value '// &
     &                       VALUE(J4)(1:I_LEN(VALUE(J4)))//' of the keyword '// &
     &                       KEYWORD(J4)(1:I_LEN(KEYWORD(J4)))// &
     &                      ' is greater than upper limit '//STR )
                        RETURN
                   END IF
              END IF
!
              PAR%IKWD(IND_KEY,N_PRC) = IVAL
           ELSE IF ( PAR%ITYP(IND_KEY,N_PRC) .EQ. PAR__RTYP ) THEN
!
! ----------- REAL*8 type
!
              READ ( UNIT=VALUE(J4), FMT='(F8.4)', IOSTAT=IOS ) RVAL
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 6956, IUER, 'PARSE_ARG', 'Error conversion '// &
     &                 'CHARACTER --> REAL*8 for the value ='// &
     &                  VALUE(J4)(1:I_LEN(VALUE(J4)))//' of the keyword '// &
     &                  KEYWORD(J4) )
                   RETURN
              END IF
!
! ----------- Getting pointers to the restrictions
!
              IPOI_MIN = PAR_SHV(1,IKEY)
              IPOI_MAX = PAR_SHV(2,IKEY)
!
! ----------- Check: are pointers valid?
!
              IF ( IPOI_MIN .GT. L_RVL ) THEN
                   WRITE ( 6, * ) ' ifun=',ifun,' ikey=',ikey
                   WRITE ( 6, * ) ' ipoi_min = ',ipoi_min,' l_rvl = ',l_rvl
                   CALL ERR_LOG ( 6957, IUER, 'PARSE_ARG', 'Internal error '// &
     &                 'of PAR-compiler' )
                   RETURN
              END IF
!
              IF ( IPOI_MAX .GT. L_RVL ) THEN
                   WRITE ( 6, * ) ' ifun=',ifun,' ikey=',ikey
                   WRITE ( 6, * ) ' ipoi_max = ',ipoi_max,' l_rvl = ',l_rvl
                   CALL ERR_LOG ( 6958, IUER, 'PARSE_ARG', 'Internal error '// &
     &                 'of PAR-compiler' )
                   RETURN
              END IF
!
! ----------- Check for the low limit if necessary
!
              IF ( IPOI_MIN .GT. 0 ) THEN
                   IF ( RVAL .LT. PAR_RVL(IPOI_MIN) ) THEN
                        CALL CLRCH ( STR )
                        WRITE ( UNIT=STR, FMT='(1PE15.7)' ) PAR_RVL(IPOI_MIN)
                        CALL CHASHL ( STR )
                        IF ( STR(1:1) .EQ. '.' ) STR='0'//STR
                        CALL ERR_LOG ( 6959, IUER, 'PARU_COMPILE', 'Value '// &
     &                       VALUE(J4)(1:I_LEN(VALUE(J4)))//' of the keyword '// &
     &                       KEYWORD(J4)(1:I_LEN(KEYWORD(J4)))// &
     &                      ' is less than low limit '//STR )
                        RETURN
                   END IF
              END IF
!
! ----------- Check for the upper limit if necessary
!
              IF ( IPOI_MAX .GT. 0 ) THEN
                   IF ( RVAL .GT. PAR_RVL(IPOI_MAX) ) THEN
                        CALL CLRCH ( STR )
                        WRITE ( UNIT=STR, FMT='(1PE15.7)' ) PAR_RVL(IPOI_MAX)
                        CALL CHASHL ( STR )
                        IF ( STR(1:1) .EQ. '.' ) STR='0'//STR
                        CALL ERR_LOG ( 6960, IUER, 'PARU_COMPILE', 'Value '// &
     &                       VALUE(J4)(1:I_LEN(VALUE(J4)))//' of the keyword '// &
     &                       KEYWORD(J4)(1:I_LEN(KEYWORD(J4)))// &
     &                      ' is greater than upper limit '//STR )
                        RETURN
                   END IF
              END IF
!
              PAR%RKWD(IND_KEY,N_PRC) = RVAL
         END IF
         CALL CLRCH ( PAR%KWD(IND_KEY,N_PRC) )
         PAR%KWD(IND_KEY,N_PRC) = KEYWORD(J4)
 440  CONTINUE
      PAR%ISTS(N_PRC) = PAR__COMPLETE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PARSE_ARG  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PARU_DUMP ( PAR )
! ************************************************************************
! *                                                                      *
! *   Prcedure  PARU_DUMP  prints dump of the object code for            *
! *   PAR-program. Dunm is written to the screen and in SPOOL-file.      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  PAR ( RECORD    ) -- Data structure which contains "object code" of *
! *                       PARU-program: stream of functions to be        *
! *                       executed and their parameters record.          *
! *                                                                      *
! *  ###  17-MAR-98    PARU_DUMP   v1.1  (c)  L. Petrov  23-DEC-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'pamb.i'
      TYPE ( PAR__STRU ) ::  PAR
      CHARACTER  STATUS*12
      INTEGER*4  J1, J2
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Status
!
      IF ( PAR%STATUS .EQ. PAR__COMPLETE ) THEN
           STATUS = 'complete'
        ELSE IF ( PAR%STATUS .EQ. PAR__INCOMPLETE ) THEN
           STATUS = 'incomplete'
        ELSE
           STATUS = 'undefined'
      END IF
!
! --- Header
!
      WRITE (  6, 110 ) PAR%FINAM(1:I_LEN(PAR%FINAM)), STATUS
      WRITE ( 23, 110 ) PAR%FINAM(1:I_LEN(PAR%FINAM)), STATUS
 110  FORMAT ( 1X,'Dump of object code of PAR-program'/ &
     &         1X,'Source file: ',A/ &
     &         1X,'Compilation status: ',A )
      WRITE  (  6, 120 ) PAR%N_PRC
      WRITE  ( 23, 120 ) PAR%N_PRC
 120  FORMAT ( 1X,'Number of function calls : ',I3 )
      IF ( PAR%N_PRC .EQ. 0  .OR.  PAR%STATUS .NE. PAR__COMPLETE ) THEN
           RETURN
      END IF
!
! --- Cycle on procedues
!
      DO 410 J1=1,PAR%N_PRC
         IF ( PAR%ISTS(J1) .EQ. PAR__COMPLETE ) THEN
              WRITE  (  6, 130 ) J1, PAR_FUN( PAR%IFUN(J1) ), PAR%NKWD(J1)
              WRITE  ( 23, 130 ) J1, PAR_FUN( PAR%IFUN(J1) ), PAR%NKWD(J1)
 130          FORMAT ( 4X,I3,') ',A,'   Number of keywords: ',I3 )
              IF ( PAR%NKWD(J1) .GT. 0 ) THEN
!
! -------------- Cycle on keywords
!
                 DO 420 J2=1,PAR%NKWD(J1)
                    IF ( PAR%ITYP(J2,J1) .EQ. PAR__CTYP ) THEN
                         WRITE  (  6, 140 ) J2, PAR%KWD(J2,J1), &
     &                                PAR%CKWD(J2,J1)(1:I_LEN(PAR%CKWD(J2,J1)))
                         WRITE  ( 23, 140 ) J2, PAR%KWD(J2,J1), &
     &                                PAR%CKWD(J2,J1)(1:I_LEN(PAR%CKWD(J2,J1)))
 140                     FORMAT ( 7X,I3,'. keyword: ',A,' value: ',A, &
     &                            '     type: CHARACTER ' )
                       ELSE IF ( PAR%ITYP(J2,J1) .EQ. PAR__ITYP ) THEN
                         WRITE  (  6, 150 ) J2, PAR%KWD(J2,J1), PAR%IKWD(J2,J1)
                         WRITE  ( 23, 150 ) J2, PAR%KWD(J2,J1), PAR%IKWD(J2,J1)
 150                     FORMAT ( 7X,I3,'. keyword: ',A,' value: ',I4, &
     &                            '     type: INTEGER*4 ' )
                       ELSE IF ( PAR%ITYP(J2,J1) .EQ. PAR__RTYP ) THEN
                         WRITE  (  6, 160 ) J2, PAR%KWD(J2,J1), PAR%RKWD(J2,J1)
                         WRITE  ( 23, 160 ) J2, PAR%KWD(J2,J1), PAR%RKWD(J2,J1)
 160                     FORMAT ( 7X,I3,'. keyword: ',A,' value: ',1PE15.7, &
     &                            '  type: REAL*8    ' )
                       ELSE
                         WRITE  (  6, 170 ) J2, PAR%KWD(J2,J1)
                         WRITE  ( 23, 170 ) J2, PAR%KWD(J2,J1)
 170                     FORMAT ( 7X,I3,'. keyword: ',A,' value: ', &
     &                            '  type: undefined' )
                    END IF
 420             CONTINUE
              END IF
           ELSE IF ( PAR%ISTS(J1) .EQ. PAR__INCOMPLETE ) THEN
              WRITE  (  6, 180 ) J1, PAR%IFUN(J1), PAR%NKWD(J1), 'incomplete'
              WRITE  ( 23, 180 ) J1, PAR%IFUN(J1), PAR%NKWD(J1), 'incomplete'
 180          FORMAT ( 4X,I3,') ','Function ',I3,'  Number of keywords: ',I3, &
     &                 ' status = ',A )
           ELSE
              WRITE  (  6, 180 ) J1, PAR%IFUN(J1), PAR%NKWD(J1), 'undefined'
              WRITE  ( 23, 180 ) J1, PAR%IFUN(J1), PAR%NKWD(J1), 'undefined'
         END IF
 410  CONTINUE
      WRITE  (  6, 190 )
      WRITE  ( 23, 190 )
 190  FORMAT ( 1X,'End of dump' )
!
      RETURN
      END  !#!  PARU_DUMP  #!#
