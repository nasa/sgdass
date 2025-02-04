      SUBROUTINE SIMA_CONFIG ( CONFIG_FILE, SIMA, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SIMA_CONFIG  reads CONFIG_FILE with configuration of       *
! *   a sima utility, parses its content and fills field of a record     *
! *   SIMA.                                                              *
! *   Syntax of configuration file:                                      *
! *   1) Lines starting with ##  ( Two symbols: <DIES> <DIES>) are       *
! *      recognized as comments and ignored.                             *
! *   2) Lines starting with #  ( Two symbols: <DIES> <BLANK>) are       *
! *      considered as a specifications of SIMA configuration.           *
! *      Format: keyword, one or more delimiters, value.                 *
! *      Keywords are case-insensitive, but values are case-sensitive.   *
! *      Delimiter is one of <BLANK> <TAB> <BINARY_ZERO>.                *
! *   No defaults can be used. All keywords are to be specified.         *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  CONFIG_FILE ( CHARACTER ) -- File name of the dserver configuration *
! *                               file.                                  *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *        SIMA  ( RECORD    ) -- Object with data structure for keeping *
! *                               configuration of sima.                 *
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
! * ###  02-AUG-2000   SIMA_CONFIG   v1.2 (c) L. Petrov 18-JAN-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'sima.i'
      TYPE ( SIMA__STRU ) ::  SIMA
      CHARACTER  CONFIG_FILE*(*)
      INTEGER*4  IUER
      INTEGER*4  M_CNF, MIND
      PARAMETER  ( M_CNF = 64, MIND = 32 )
      INTEGER*4  LEN_DSR, LIND, IND(2,MIND), N_BUF, I_PAR, J1, IER
      CHARACTER  BUF(M_CNF)*512, DELIM*3, STR*20, STR1*20
      CHARACTER  USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128
      PARAMETER  ( DELIM = CHAR(0)//CHAR(32)//CHAR(9) )
      INTEGER*4  ILEN, I_LEN
!
! --- Clean record SIMA
!
      LEN_DSR = LOC(SIMA%LAST_FIELD) - LOC(SIMA%FIRST_FIELD) + 4
      CALL NOUT ( LEN_DSR, SIMA )
!
! --- Save name of the configuration file
!
      SIMA%CONFIG_FILE = CONFIG_FILE
!
! --- Read configuration file into the buffer BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( CONFIG_FILE, M_CNF, BUF, N_BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5121, IUER, 'SIMA_CONFIG', 'Error in attempt '// &
     &         'to open sima configuration file '//CONFIG_FILE )
           RETURN
      END IF
!
! --- Scan the buffer with copy of configuration file
!
      I_PAR = 0
      DO 410 J1=1,N_BUF
!
! ------ Skip comments
!
         IF ( ILEN(BUF(J1)) .EQ.  0   ) GOTO 410
         IF ( BUF(J1)(1:1)  .NE. '#'  ) GOTO 410
         IF ( BUF(J1)(1:2)  .EQ. '#!' ) GOTO 410
         IF ( BUF(J1)(1:2)  .EQ. '##' ) GOTO 410
!
! ------ Split the line onto words
!
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, -3 )
         CALL CLRCH ( STR )
         CALL INCH  ( J1, STR )
!
! ------ It should be exactly three words. Let's check it
!
         IF ( LIND .LT. 3 ) THEN
              CALL ERR_LOG ( 5122, IUER, 'SIMA_CONFIG', 'Error in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &            'configuration file '//CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &            ': too few words' )
              RETURN
         END IF
!
! ------ Well. Three words. Parse them
!
! ------ Transform the keyword to the letters of upper case
!
         CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                   BUF(J1)(IND(1,2):IND(2,2))  )
!
         IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'BATCH_CONTROL_FILE:' ) THEN
              SIMA%BATCH_CONTROL_FILE  = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'TEMP_CONTROL_FILE:' )THEN
              SIMA%TEMP_CONTROL_FILE  = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'SIMA_OUTPUT_FILE:' ) THEN
              SIMA%SIMA_OUTPUT_FILE = BUF(J1)(IND(1,3):IND(2,3))
            ELSE
              CALL ERR_LOG ( 5123, IUER, 'SIMA_CONFIG', 'Error in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &            'configuration file '//CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &            ': unsupported keyword '//BUF(J1)(IND(1,2):IND(2,2)) )
              RETURN
         END IF
         I_PAR = I_PAR + 1
 410  CONTINUE
!
! --- Check: did we define all fields of dserver?
!
      IF ( I_PAR .LT. M_PAR ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( I_PAR, STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( M_PAR, STR1 )
           CALL ERR_LOG ( 5124, IUER, 'SIMA_CONFIG', 'Not all parameters '// &
     &         'were found in dserver configuration file '// &
     &          CONFIG_FILE(1:I_LEN(CONFIG_FILE))//' --  '//STR(1:I_LEN(STR))// &
     &         ' instead of '//STR1 )
           RETURN
      END IF
!
      CALL GETINFO_USER  ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
      CALL TRAN ( 12, USER_NAME, USER_NAME )
!
      SIMA%TEMP_CONTROL_FILE = &
     &          SIMA%TEMP_CONTROL_FILE(1:I_LEN(SIMA%TEMP_CONTROL_FILE))// &
     &          '_'//USER_NAME
      SIMA%SIMA_OUTPUT_FILE = &
     &          SIMA%SIMA_OUTPUT_FILE(1:I_LEN(SIMA%SIMA_OUTPUT_FILE))// &
     &          '_'//USER_NAME
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!   SIMA_CONFIG  #!#
