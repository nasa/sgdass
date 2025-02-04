      SUBROUTINE SORT_PARSER ( STRING, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SORT_PARSER  parses the line STRING with "SORT" control   *
! *   codes in the GSETUP section. It reads the next line(s) of the      *
! *   control file if stuff related to SORT keywrod is continued at the  *
! *   next line.                                                         *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * STRING ( CHARACTER ) -- the line of the control file to be parsed.   *
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
! *  ###  01-JAN-99   SORT_PARSER   v1.0 (c)  L. Petrov  03-JAN-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      CHARACTER  STRING*(*)
      INTEGER*4  IUER
      INTEGER*4  MQUALS, MVALS1, MVALS2, NDIF_QUALS
      PARAMETER  ( MQUALS     = 4 )
      PARAMETER  ( MVALS1     = 3 )
      PARAMETER  ( MVALS2     = 3 )
      PARAMETER  ( NDIF_QUALS = 5 )
      CHARACTER  QUALS(MQUALS)*16, QUALS_LINE*160, &
     &           VALS1(MVALS1)*16, VALS1_LINE*160, &
     &           VALS2(MVALS2)*16, VALS2_LINE*160, &
     &           TOKEN*32
      LOGICAL*4  LQUALS(MQUALS)
      DATA       QUALS / 'NO              ', &
     &                   'YES             ', &
     &                   'SOURCES         ', &
     &                   'STATIONS        '  /
      DATA       VALS1 / 'NO              ', &
     &                   'ALPHABET        ', &
     &                   'RIGHT_ASCENSION '  /
      DATA       VALS2 / 'NO              ', &
     &                   'ALPHABET        ', &
     &                   'LONGITUDE       '  /
!
      INTEGER*4  J1, J2, K1, K2, K3, IP, ILQ
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
! --- Grouping all acceptable values of qualificators in in line
!
      CALL CLRCH ( QUALS_LINE )
      DO 510 K1=1,MQUALS
         QUALS_LINE = QUALS_LINE(1:I_LEN(QUALS_LINE)+2)//QUALS(K1)
         LQUALS(K1) = .FALSE.
 510  CONTINUE
      CALL CHASHL ( QUALS_LINE )
!
      CALL CLRCH ( VALS1_LINE )
      DO 520 K2=1,MVALS1
         VALS1_LINE = VALS1_LINE(1:I_LEN(VALS1_LINE)+2)//VALS1(K2)
         LQUALS(K2) = .FALSE.
 520  CONTINUE
      CALL CHASHL ( VALS1_LINE )
!
      CALL CLRCH ( VALS2_LINE )
      DO 530 K3=1,MVALS2
         VALS2_LINE = VALS2_LINE(1:I_LEN(VALS2_LINE)+2)//VALS2(K3)
         LQUALS(K3) = .FALSE.
 530  CONTINUE
      CALL CHASHL ( VALS2_LINE )
!
      DO 410 J1=1,NDIF_QUALS
!
! ------ Get a new token
!
         CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
         IF ( ILEN(TOKEN) .EQ. 0 ) THEN
!
! ----------- No tokens more...
!
              GOTO 810
         END IF
!
! ------ Parsing qualfier
!
         ILQ = LTM_DIF ( 0, MQUALS, QUALS, TOKEN )
         IF ( ILQ .LE. 0 ) THEN
              CALL ERR_LOG ( 3431, IUER, 'SORT_PARSER', 'Unrecoginzed '// &
     &             'qualifier '//TOKEN(1:I_LEN(TOKEN))//' was met when '// &
     &             'keyword SORT was parsed. One of '// &
     &             QUALS_LINE(1:I_LEN(QUALS_LINE))//' was expected' )
              RETURN
           ELSE IF ( ILQ .EQ. 1 ) THEN
!
! ----------- Qualifier NO
!
              IF ( LQUALS(ILQ) ) THEN
                   CALL ERR_LOG ( 3432, IUER, 'SORT_PARSER', 'Qualifier '// &
     &                  TOKEN(1:I_LEN(TOKEN))//' was met twice when '// &
     &                  'keyword SORT was parsed. Check syntax, please.' )
                   RETURN
              END IF
!
              IF ( J1 .NE. 1 ) THEN
                   CALL ERR_LOG ( 3433, IUER, 'SORT_PARSER', 'Qualifier '// &
     &                  TOKEN(1:I_LEN(TOKEN))//' is not the first and the '// &
     &                 'only qualifier of the keyword SORT. Check syntax, '// &
     &                 'please.' )
                   RETURN
              END IF
              LQUALS(ILQ)    = .TRUE.
!
              SORT_SOU = NO__SRT
              SORT_STA = NO__SRT
!
              CALL ERR_LOG ( 0, IUER )
              RETURN
           ELSE IF ( ILQ .EQ. 2 ) THEN
!
! ----------- Qualifier YES
!
              IF ( J1 .NE. 1 ) THEN
                   CALL ERR_LOG ( 3434, IUER, 'SORT_PARSER', 'Qualifier '// &
     &                  TOKEN(1:I_LEN(TOKEN))//' is not the first '// &
     &                 'qualifier of the keyword SORT. Check syntax, '// &
     &                 'please.' )
                   RETURN
              END IF
              LQUALS(ILQ) = .TRUE.
           ELSE IF ( ILQ .EQ. 3  ) THEN
!
! ----------- Qualifier SOURCES
!
              LQUALS(ILQ)    = .TRUE.
!
! ----------- Get the value of the qualifier
!
              CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
              IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                   CALL ERR_LOG ( 3435, IUER, 'SORT_PARSER', 'No value '// &
     &                 'was supplied for qualifier '// &
     &                  QUALS(ILQ)(1:I_LEN(QUALS(ILQ)))// &
     &                 ' when keyword SORT was parsed. One of '// &
     &                  VALS1_LINE(1:I_LEN(VALS1_LINE))//' was expected' )
                   RETURN
              END IF
!
! ----------- Parsing the value of the qualifier
!
              IP = LTM_DIF ( 1, MVALS1, VALS1, TOKEN(1:I_LEN(TOKEN)) )
              IF ( IP .LE. 0 ) THEN
                   CALL ERR_LOG ( 3436, IUER, 'SORT_PARSER', 'Wrong value '// &
     &                 'was supplied for qualifier '// &
     &                  QUALS(ILQ)(1:I_LEN(QUALS(ILQ)))//' : '// &
     &                  TOKEN(1:I_LEN(TOKEN))// &
     &                 ' when keyword SORT was parsed. One of '// &
     &                  VALS1_LINE(1:I_LEN(VALS1_LINE))//' was expected' )
                   RETURN
                 ELSE IF ( IP .EQ. 1 ) THEN
                   SORT_SOU = NO__SRT
                 ELSE IF ( IP .EQ. 2 ) THEN
                   SORT_SOU = ALPHB__SRT
                 ELSE IF ( IP .EQ. 3 ) THEN
                   SORT_SOU = ASCEN__SRT
              END IF
           ELSE IF ( ILQ .EQ. 4  ) THEN
!
! ----------- Qualifier STATIONS
!
              LQUALS(ILQ)    = .TRUE.
!
! ----------- Get the value of the qualifier
!
              CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
              IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                   CALL ERR_LOG ( 3437, IUER, 'SORT_PARSER', 'No value '// &
     &                 'was supplied for qualifier '// &
     &                  QUALS(ILQ)(1:I_LEN(QUALS(ILQ)))// &
     &                 ' when keyword SORT was parsed. One of '// &
     &                  VALS1_LINE(1:I_LEN(VALS1_LINE))//' was expected' )
                   RETURN
              END IF
!
! ----------- Parsing the value of the qualifier
!
              IP = LTM_DIF ( 1, MVALS2, VALS2, TOKEN(1:I_LEN(TOKEN)) )
              IF ( IP .LE. 0 ) THEN
                   CALL ERR_LOG ( 3438, IUER, 'SORT_PARSER', 'Wrong value '// &
     &                 'was supplied for qualifier '// &
     &                  QUALS(ILQ)(1:I_LEN(QUALS(ILQ)))//' : '// &
     &                  TOKEN(1:I_LEN(TOKEN))// &
     &                 ' when keyword SORT was parsed. One of '// &
     &                  VALS2_LINE(1:I_LEN(VALS2_LINE))//' was expected' )
                   RETURN
                 ELSE IF ( IP .EQ. 1 ) THEN
                   SORT_STA = NO__SRT
                 ELSE IF ( IP .EQ. 2 ) THEN
                   SORT_STA = ALPHB__SRT
                 ELSE IF ( IP .EQ. 3 ) THEN
                   SORT_STA = LONG__SRT
              END IF
         END IF
 410  CONTINUE
 810  CONTINUE
!
! --- Create a line with missed qualifiers
!
      CALL CLRCH ( QUALS_LINE )
      DO 420 J2=2,MQUALS
         IF ( .NOT. LQUALS(J2) ) THEN
              QUALS_LINE = QUALS_LINE(1:I_LEN(QUALS_LINE)+2)//QUALS(J2)
         END IF
 420  CONTINUE
      CALL CHASHL ( QUALS_LINE )
!
! --- Check: were some qualifiers missed?
!
      IF ( ILEN(QUALS_LINE) .GT. 0 ) THEN
           CALL ERR_LOG ( 3439, IUER, 'SORT_PARSER', 'Error during parsing '// &
     &         'keyword SORT: qualifiers '// &
     &          QUALS_LINE(1:I_LEN(QUALS_LINE))//' were not supplied. '// &
     &         'Please, check syntax' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SORT_PARSER  #!#
