      SUBROUTINE PARSE_DBNAME ( DBNAME, CDATE, SUFFIX, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DBNAME  parses database name and returns date of the      *
! *   session in SOLVE internal format and database suffix.              *
! *   SOLVE internal format is  yyyy.mm.dd                               *
! *   Examples:                                                          *
! *     DBNAME="$00SEP02XK" then CDATE="2000.09.02", SUFFIX = "XK"       *
! *     DBNAME="94OCT31S  " then CDATE="1994.10.31", SUFFIX = "S "       *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  DBNAME ( CHARACTER ) -- Database name which is being parsed         *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *   CDATE ( CHARACTER ) -- Date of the database in Solve date format   *
! *                          extracted from the database name. Length:   *
! *                          10 symbols.                                 *
! *  SUFFIX ( CHARACTER ) -- two-letters database suffix.                *
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
! *                                                                      *
! *  ### 08-SEP-2000  PARSE_DBAME  v2.0 (c)  L. Petrov  02-DEC-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      CHARACTER  DBNAME*(*)
      INTEGER*4  IUER
      CHARACTER  CDATE*10, SUFFIX*2, MON(12)*3, DBN*9
      DATA       MON /  'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', &
     &                  'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'  /
      INTEGER*4  IDAT, IYEAR, IMON, IDAY
      INTEGER*4  LTM_DIF, I_LEN
!
! --- Transform database name rom DBNAME whcih might have a leading dollar
! --- sign to DBN without dollar sign
!
      CALL CLRCH ( DBN )
      IF ( DBNAME(1:1) .EQ. '$' ) THEN
           DBN = DBNAME(2:)
         ELSE
           DBN = DBNAME
      END IF
!
      CALL CHIN ( DBNAME(1:8), IDAT )
      IF ( IDAT > 19700000  .AND.  IDAT < 20500000 ) THEN
           CDATE = DBNAME(1:4)//'.'//DBNAME(5:6)//'.'//DBNAME(7:8)
           IF ( DBNAME(9:9) == '_' ) THEN
                SUFFIX = DBNAME(10:)
              ELSE IF ( DBNAME(10:10) == '_' ) THEN
                SUFFIX = DBNAME(9:9)//' '
              ELSE 
                SUFFIX = DBNAME(9:10)
           END IF
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Extract year field
!
      CALL CHIN ( DBN(1:2), IYEAR )
      IF ( IYEAR .GT. 70 ) THEN
           IYEAR = IYEAR + 1900
         ELSE
           IYEAR = IYEAR + 2000
      END IF
!
! --- Extract month field
!
      IMON = LTM_DIF ( 0, 12, MON, DBN(3:5) )
!
! --- Extract day field
!
      CALL CHIN ( DBN(6:7), IDAY )
!
! --- Extract suffix
!
      SUFFIX = DBN(8:9)
!
      IF ( IYEAR .LT. 1970  .OR. IYEAR .GT. 2070 ) THEN
           CALL ERR_LOG ( 5291, IUER, 'PARSE_DBNAME', 'Wrong database name '// &
     &         'format: the name "'//DBNAME(1:I_LEN(DBNAME))//'" has wrong '// &
     &         'year field' )
           RETURN
      END IF
!
      IF ( IMON .LT. 1  .OR.  IMON .GT. 12 ) THEN
           CALL ERR_LOG ( 5292, IUER, 'PARSE_DBNAME', 'Wrong database name '// &
     &         'format: '//DBNAME(1:I_LEN(DBNAME))//' -- wrong month field' )
           RETURN
      END IF
!
      IF ( IDAY .LT. 1  .OR.  IDAY .GT. 31 ) THEN
           CALL ERR_LOG ( 5293, IUER, 'PARSE_DBNAME', 'Wrong database name '// &
     &         'format: '//DBNAME(1:I_LEN(DBNAME))//' -- wrong day field' )
           RETURN
      END IF
!
! --- Format output string CDATE
!
      CALL CLRCH ( CDATE )
      CALL INCH  ( IYEAR, CDATE(1:4)  )
      CDATE(5:5) = '.'
      CALL INCH  ( IMON,  CDATE(6:7)  )
      CALL CHASHR(        CDATE(6:7)  )
      CDATE(8:8) = '.'
      CALL INCH  ( IDAY,  CDATE(9:10) )
      CALL CHASHR(        CDATE(9:10) )
      CALL BLANK_TO_ZERO ( CDATE )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END   SUBROUTINE  PARSE_DBNAME  !#!#
