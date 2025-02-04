      SUBROUTINE GET_SUPERFILE_NAME ( STRING, SUPFILE_NAME, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxillary routine  GET_SUPERFILE_NAME  parses the string STRING    *
! *   from XPND file (expanded batopt arc file) and builds the superfile *
! *   name.                                                              *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *       STRING ( CHARACTER ) -- The line of the expanded arc file file *
! *                               to be parsed.                          *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * SUPFILE_NAME ( CHARACTER ) -- Full superfile name including path.    *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 14-JAN-99 GET_SUPERFILE_NAME  v2.0 (c) L. Petrov 08-FEB-2006 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*4  IUER
      CHARACTER  STRING*(*), SUPFILE_NAME*(*)
      INTEGER*4  MIND
      PARAMETER  ( MIND = 128 )
      INTEGER*4  LIND, IND(2,MIND), INUM, IVER, IL, IP, MODE, IDAT
      CHARACTER  DBNAME*10, PATH*80, VER*3, STR*20
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LINDEX
!
! --- Spliting the line onto words
!
      CALL CLRCH  ( SUPFILE_NAME )
      CALL EXWORD ( STRING, MIND, LIND, IND, ' '//CHAR(0)//'$', -3 )
!
      IF ( LIND .LT. 4 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( LIND, STR )
           CALL ERR_LOG ( 4701, IUER, 'GET_SUPERFILE_NAME', 'Error in '// &
     &         'parsing string '//STRING(1:I_LEN(STRING))//' -- too few '// &
     &         'words: '//STR )
           RETURN
      END IF
!
! --- Extract the database name
!
      CALL CLRCH ( DBNAME )
      DBNAME = STRING(IND(1,1):IND(2,1))
!
      CALL CHIN ( DBNAME(1:8), IDAT )
      IF ( IDAT > 19700000 .AND. IDAT < 21000000 ) THEN
!
! -------- Ppost-2006 style
!
           MODE = 2
         ELSE 
!
! -------- Check correctness of the database name
!
           CALL CHIN ( DBNAME(1:2), INUM )
           IF ( INUM .LT. 0  .OR.  INUM .GT. 99 ) THEN
                CALL ERR_LOG ( 4702, IUER, 'GET_SUPERFILE_NAME', 'Error in '// &
     &              'parsing string '//STRING(1:I_LEN(STRING))// &
     &              ' -- year letters of the database name are not '// &
     &              'integer: in range [0,99] '//DBNAME(1:2) )
                RETURN
           END IF
!
           IF ( DBNAME(3:5) .NE. 'JAN'  .AND.  DBNAME(3:5) .NE. 'FEB'  .AND. &
     &          DBNAME(3:5) .NE. 'MAR'  .AND.  DBNAME(3:5) .NE. 'APR'  .AND. &
     &          DBNAME(3:5) .NE. 'MAY'  .AND.  DBNAME(3:5) .NE. 'JUN'  .AND. &
     &          DBNAME(3:5) .NE. 'JUL'  .AND.  DBNAME(3:5) .NE. 'AUG'  .AND. &
     &          DBNAME(3:5) .NE. 'SEP'  .AND.  DBNAME(3:5) .NE. 'OCT'  .AND. &
     &          DBNAME(3:5) .NE. 'NOV'  .AND.  DBNAME(3:5) .NE. 'DEC'        ) THEN
!
                CALL ERR_LOG ( 4703, IUER, 'GET_SUPERFILE_NAME', 'Error in '// &
     &              'parsing string '//STRING(1:I_LEN(STRING))//' -- month '// &
     &              'letters of the database name are not a month '// &
     &              'abbreviation '//DBNAME(3:5) )
                RETURN
           END IF
!
           CALL CHIN ( DBNAME(6:7), INUM )
           IF ( INUM .LT. 1  .OR.  INUM .GT. 31 ) THEN
               CALL ERR_LOG ( 4704, IUER, 'GET_SUPERFILE_NAME', 'Error in '// &
     &             'parsing string '//STRING(1:I_LEN(STRING))//' -- day '// &
     &             'letters of the database name are not integer '// &
     &             'in range [1,31] '//DBNAME(6:7) )
               RETURN
           END IF
           MODE = 1
      END IF
!
      IF ( DBNAME(9:9) .EQ. ' ' ) DBNAME(9:9) = '_'
!
! --- Extract the version of the database
!
      CALL CHIN ( STRING(IND(1,2):IND(2,2)), IVER )
!
! --- .. and check its correctness
!
      IF ( IVER .LE. 0  .OR. IVER .GT. 999 ) THEN
           CALL ERR_LOG ( 4705, IUER, 'GET_SUPERFILE_NAME', 'Error in '// &
     &         'parsing string '//STRING(1:I_LEN(STRING))//' -- version '// &
     &         'number is not an integer in the range [1, 999]: '// &
     &          STRING(IND(1,2):IND(2,2)) )
           RETURN
      END IF
!
      CALL INCH   ( IVER,  VER )
      CALL CHASHR (        VER )
      CALL BLANK_TO_ZERO ( VER )
!
! --- Extract the path
!
      CALL CLRCH ( PATH )
      IP = LINDEX ( STRING, '!' ) ! Search the last occurence of "!"
      IF ( IP .LE. 0 ) THEN
           CALL ERR_LOG ( 4706, IUER, 'GET_SUPERFILE_NAME', 'Error in '// &
     &         'parsing string '//STRING(1:I_LEN(STRING))//' -- ! '// &
     &         'was not found' )
           RETURN
      END IF
!
! --- Parse onto words the last part of the string starting from the last
! --- occurence of "!"
!
      CALL EXWORD ( STRING(IP:), MIND, LIND, IND, ' '//CHAR(0)//'$', -3 )
      IF ( IP .LT. 2 ) THEN
           CALL ERR_LOG ( 4706, IUER, 'GET_SUPERFILE_NAME', 'Error in '// &
     &         'parsing string '//STRING(1:I_LEN(STRING))//' There is no '// &
     &         ' more words after ! ' )
           RETURN
      END IF
!
! --- Get at last the path
!
      PATH = STRING(IP-1+IND(1,2):IP-1+IND(2,2))
!
! --- Exclude trailing slash if needed
!
      IF ( PATH(I_LEN(PATH):I_LEN(PATH)) .EQ. '/' ) THEN
           CALL CLRCH ( PATH(I_LEN(PATH):) )
      END IF
!
! --- Check: does the string SUPFILE_NAME is long enough
!
      IL = 15 + ILEN(PATH)
      IF ( IL .GT. LEN(SUPFILE_NAME) ) THEN
           CALL CLRCH ( IL )
           CALL INCH  ( IL, STR )
           CALL ERR_LOG ( 4706, IUER, 'GET_SUPERFILE_NAME', 'String '// &
     &         'SUPFILE_NAME is too short: less than '//STR )
           RETURN
      END IF
!
! --- Finally create the superfile name
!
      IF ( MODE == 1 ) THEN
           SUPFILE_NAME = PATH(1:I_LEN(PATH)) // &
     &                    '/'                 // &
     &                    DBNAME(1:9)         // &
     &                    '_S'                // &
     &                    VER
        ELSE IF ( MODE == 2 ) THEN
           SUPFILE_NAME = PATH(1:I_LEN(PATH)) // &
     &                    '/'                 // &
     &                    DBNAME(1:8)//DBNAME(10:10)// &
     &                    '_S'                // &
     &                    VER
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_SUPERFILE_NAME  #!#
