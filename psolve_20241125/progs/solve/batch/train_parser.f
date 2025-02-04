      SUBROUTINE TRAIN_PARSER ( STRING, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  TRAIN_PARSER  parses the line STRING with "TRAIN" control *
! *   codes. It reads the next line(s) of the control file if            *
! *   stuff related to TRAIN keywrod is continued at the next line.      *
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
! *  ###  01-JAN-99   TRAIN_PARSER  v1.2 (c)  L. Petrov  13-MAY-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'glbcm.i'
      CHARACTER  STRING*(*)
      INTEGER*4  IUER
      INTEGER*4  MQUALS, MVALS, NDIF_QUALS
      PARAMETER  ( MQUALS = 4     )
      PARAMETER  ( MVALS  = 0     )
      PARAMETER  ( NDIF_QUALS = 4 )
      CHARACTER  QUALS(MQUALS)*12, QUALS_LINE*160, TOKEN*32, STR*20
      LOGICAL*4  LQUALS(MQUALS)
      DATA       QUALS / 'YES         ', &
     &                   'NO          ', &
     &                   'GLO_PARLIM  ', &
     &                   'INC_PARLIM  '  /
!!@     #                   'MEMFAULT_REP'   /
      INTEGER*4  J1, J2, K1, ILQ, IVAL
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
! --- Parsing quilifiers
!
      DO 410 J1=1,NDIF_QUALS
!
! ------ Get the new token
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
              CALL ERR_LOG ( 3411, IUER, 'TRAIN_PARSER', 'Unrecoginzed '// &
     &             'qualifier '//TOKEN(1:I_LEN(TOKEN))//' was met when '// &
     &             'keyword TRAIN was parsed. One of '// &
     &             QUALS_LINE(1:I_LEN(QUALS_LINE))//' was expected' )
              RETURN
           ELSE IF ( ILQ .EQ. 1 ) THEN
!
! ----------- Qualifier YES
!
              IF ( LQUALS(ILQ) ) THEN
                   CALL ERR_LOG ( 3412, IUER, 'TRAIN_PARSER', 'Qualifier '// &
     &                  TOKEN(1:I_LEN(TOKEN))//' was met twice when '// &
     &                  'keyword TRAIN was parsed. Check syntax, please.' )
                   RETURN
              END IF
!
              IF ( J1 .NE. 1 ) THEN
                   CALL ERR_LOG ( 3413, IUER, 'TRAIN_PARSER', 'Qualifier '// &
     &                  TOKEN(1:I_LEN(TOKEN))//' is not the first and the '// &
     &                 'only qualifier of the keyword TRAIN. Check syntax, '// &
     &                 'please.' )
                   RETURN
              END IF
              LQUALS(ILQ)    = .TRUE.
!
              TRAIN = .TRUE.
!
              CALL ERR_LOG ( 0, IUER )
              RETURN
           ELSE IF ( ILQ .EQ. 2 ) THEN
!
! ----------- Qualifier NO
!
              IF ( J1 .NE. 1 ) THEN
                   CALL ERR_LOG ( 3414, IUER, 'TRAIN_PARSER', 'Qualifier '// &
     &                  TOKEN(1:I_LEN(TOKEN))//' is not the first '// &
     &                 'qualifier of the keyword TRAIN. Check syntax, '// &
     &                 'please.' )
                   RETURN
              END IF
              LQUALS(ILQ) = .TRUE.
!
              TRAIN = .FALSE.
           ELSE IF ( ILQ .EQ. 3  ) THEN
!
! ----------- Qualifier GLO_PARLIM
!
              LQUALS(ILQ)    = .TRUE.
!
! ----------- Get the value of the qualifier
!
              CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
              IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                   CALL ERR_LOG ( 3415, IUER, 'TRAIN_PARSER', 'No value '// &
     &                 'was supplied for qualifier '// &
     &                  QUALS(ILQ)(1:I_LEN(QUALS(ILQ)))//' when '// &
     &                 'keyword TRAIN was parsed. Integer positive value '// &
     &                 'was expected' )
                   RETURN
              END IF
!
! ----------- Parsing the value of the qualifier
!
              CALL CHIN ( TOKEN, IVAL )
              IF ( IVAL .LE. 0 ) THEN
                   CALL ERR_LOG ( 3416, IUER, 'TRAIN_PARSER', 'Wrong '// &
     &                 'value of the qualifier '// &
     &                  QUALS(ILQ)(1:I_LEN(QUALS(ILQ)))//': '// &
     &                  TOKEN(1:I_LEN(TOKEN))//' was read when keyword '// &
     &                  'TRAIN was parsed. Positive '// &
     &                  'integer value was expected' )
                   RETURN
              END IF
!
              IF ( IVAL .GT. NRMFL_PARMS ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( NRMFL_PARMS, STR )
                   CALL ERR_LOG ( 3417, IUER, 'TRAIN_PARSER', 'Wrong '// &
     &                 'value of the qualifier '// &
     &                  QUALS(ILQ)(1:I_LEN(QUALS(ILQ)))//': '// &
     &                  TOKEN(1:I_LEN(TOKEN))//' -- it exceeds the maximal '// &
     &                 'number of parameters for whch scratch files were '// &
     &                 'sized: '//STR(1:I_LEN(STR))//'. Either decrese the '// &
     &                 'value of '//QUALS(ILQ)(1:I_LEN(QUALS(ILQ)))//' or '// &
     &                 're-create scratch files' )
                   RETURN
              END IF
              IF ( IVAL .LT. GLO_PARLIM__LIM ) THEN
                   CALL CLRCH   ( STR )
                   CALL INCH    ( GLO_PARLIM__LIM, STR )
                   CALL ERR_LOG ( 3418, IUER, 'TRAIN_PARSER', 'Wrong '// &
     &                 'value of the qualifier '// &
     &                  QUALS(ILQ)(1:I_LEN(QUALS(ILQ)))//': '// &
     &                  TOKEN(1:I_LEN(TOKEN))//' -- it is less than the '// &
     &                 'minimal acceptable value: '//STR(1:I_LEN(STR))// &
     &                 '. Please, increase the value of '// &
     &                  QUALS(ILQ)(1:I_LEN(QUALS(ILQ))) )
                   RETURN
              END IF
!
              GLO_PARLIM = IVAL
           ELSE IF ( ILQ .EQ. 4  ) THEN
!
! ----------- Qualifier INC_PARLIM
!
              LQUALS(ILQ)    = .TRUE.
!
! ----------- Get the value of the qualifier
!
              CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
              IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                   CALL ERR_LOG ( 3419, IUER, 'TRAIN_PARSER', 'No value '// &
     &                 'was supplied for qualifier '// &
     &                  QUALS(ILQ)(1:I_LEN(QUALS(ILQ)))//' when '// &
     &                 'keyword TRAIN was parsed. Integer positive value '// &
     &                 'was expected' )
                   RETURN
              END IF
!
! ----------- Parsing the value of the qualifier
!
              CALL CHIN ( TOKEN, IVAL )
              IF ( IVAL .LE. 0 ) THEN
                   CALL ERR_LOG ( 3420, IUER, 'TRAIN_PARSER', 'Wrong '// &
     &                 'value of the qualifier '// &
     &                  QUALS(ILQ)(1:I_LEN(QUALS(ILQ)))//': '// &
     &                  TOKEN(1:I_LEN(TOKEN))//' was read when keyword '// &
     &                  'TRAIN was parsed. Positive integer value was '// &
     &                  'expected' )
                   RETURN
               END IF
!
               INC_PARLIM = IVAL
!!@           ELSE IF ( ILQ .EQ. 5 ) THEN
!!@C
!!@C ----------- Qualifier MEMFAULT_REP
!!@C
!!@              LQUALS(ILQ)    = .TRUE.
!!@C
!!@C ----------- Get the value of the qualifier
!!@C
!!@              CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
!!@              IF ( ILEN(TOKEN) .EQ. 0 ) THEN
!!@                   CALL ERR_LOG ( 3421, IUER, 'TRAIN_PARSER', 'No value '//
!!@     #                 'was supplied for qualifier '//
!!@     #                  QUALS(ILQ)(1:I_LEN(QUALS(ILQ)))//' when '//
!!@     #                 'keyword TRAIN was parsed. Integer positive value '//
!!@     #                 'was expected' )
!!@                   RETURN
!!@              END IF
!!@C
!!@              IF ( TOKEN(1:3) .EQ. 'YES' ) THEN
!!@                   MEMFAULT_REP = .TRUE.
!!@                 ELSE IF ( TOKEN(1:2) .EQ. 'NO' ) THEN
!!@                   MEMFAULT_REP = .FALSE.
!!@                 ELSE
!!@                   CALL ERR_LOG ( 3422, IUER, 'TRAIN_PARSER', 'Wrong '//
!!@     #                 'value of the qualifier '//
!!@     #                  QUALS(ILQ)(1:I_LEN(QUALS(ILQ)))//': '//
!!@     #                  TOKEN(1:I_LEN(TOKEN))//' was read when keyword '//
!!@     #                  'TRAIN was parsed. YES or NO was expected' )
!!@                   RETURN
!!@              END IF
         END IF
 410  CONTINUE
 810  CONTINUE
!
! --- Create the line with missed qualifiers
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
           CALL ERR_LOG ( 3423, IUER, 'TRAIN_PARSER', 'Error during parsing '// &
     &         'keyword TRAIN: qualifiers '// &
     &          QUALS_LINE(1:I_LEN(QUALS_LINE))//' were not supplied. '// &
     &         'Please, check syntax once more.' )
           RETURN
      END IF
!
! --- We set it always true
!
      MEMFAULT_REP = .TRUE.
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  TRAIN_PARSER  #!#
