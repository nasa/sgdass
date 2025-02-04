      SUBROUTINE SNGCHK_PARSER ( STRING, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SNGCHK_PARSER  parses the line with singularity check     *
! *   control codes. It reads the next line(s) of the control file if    *
! *   stuff related to SNGCHK keywrod continues at the nest line.        *
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
! *  ###  08-JUL-98  SNGCHK_PARSER  v1.1 (c)  L. Petrov  23-JUL-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      CHARACTER  STRING*(*)
      INTEGER*4  IUER
      INTEGER*4  MQUALS, NDIF_QUALS, MVALS1
      PARAMETER  ( MQUALS     = 5 )
      PARAMETER  ( NDIF_QUALS = 4 )
      PARAMETER  ( MVALS1     = SNGCHK_ACT__LSAL )
      CHARACTER  QUALS(MQUALS)*16, VALS1(MVALS1)*16, QUALS_LINE*160, &
     &           VALS1_LINE*160, TOKEN*32
      LOGICAL*4  LQUALS(MQUALS)
      DATA       QUALS / 'NONE            ', &
     &                   'ACTION          ', &
     &                   'SOUMIN          ', &
     &                   'STAMIN          ', &
     &                   'BASMIN          '   /
      DATA       VALS1 / 'NONE            ', &
     &                   'WARNING         ', &
     &                   'REPARAMETERIZE  ', &
     &                   'STOP            ', &
     &                   'SKIP            '   /
      INTEGER*4  J1, J2, K1, K2, ILQ, ILV, IVAL
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
! --- Grouping all acceptable values of the first qualificator in in line
!
      CALL CLRCH ( VALS1_LINE )
      DO 520 K2=1,MVALS1
         VALS1_LINE = VALS1_LINE(1:I_LEN(VALS1_LINE)+2)//VALS1(K2)
 520  CONTINUE
      CALL CHASHL ( VALS1_LINE )
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
              CALL ERR_LOG ( 3401, IUER, 'SNGCHK_PARSER', 'Unrecoginzed '// &
     &             'qualifier '//TOKEN(1:I_LEN(TOKEN))//' was met when '// &
     &             'keyword SINGULARITY_CHECK was parsed. One of '// &
     &             QUALS_LINE(1:I_LEN(QUALS_LINE))//' was expected' )
              RETURN
           ELSE IF ( ILQ .EQ. 1 ) THEN
!
! ----------- Qualifier NONE
!
              IF ( LQUALS(ILQ) ) THEN
                   CALL ERR_LOG ( 3402, IUER, 'SNGCHK_PARSER', 'Qualifier '// &
     &                  TOKEN(1:I_LEN(TOKEN))//' was met twice when '// &
     &                  'keyword SINGULARITY_CHECK was parsed. Check syntax. ' )
                   RETURN
              END IF
              LQUALS(ILQ)    = .TRUE.
              SNGCHK_ACTION = SNGCHK_ACT__NONE
              SNGCHK_SOUMIN = 0
              SNGCHK_STAMIN = 0
              SNGCHK_BASMIN = 0
!
              CALL ERR_LOG ( 0, IUER )
              RETURN
           ELSE IF ( ILQ .EQ. 2 ) THEN
!
! ----------- Qualifier ACTION
!
              LQUALS(ILQ)    = .TRUE.
!
! ----------- Get the value of the qualifier
!
              CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
              IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                   CALL ERR_LOG ( 3403, IUER, 'SNGCHK_PARSER', 'No value '// &
     &                 'was supplied for qualifier '// &
     &                  QUALS(ILQ)(1:I_LEN(QUALS(ILQ)))//' when keyword '// &
     &                 'SINGULARITY_CHECK was parsed. One of '// &
     &                  VALS1_LINE(1:I_LEN(VALS1_LINE))//' was expected' )
                   RETURN
              END IF
!
! ----------- Parsing the value
!
              ILV = LTM_DIF ( 0, MVALS1, VALS1, TOKEN )
              IF ( ILV .LE. 0 ) THEN
                   CALL ERR_LOG ( 3404, IUER, 'SNGCHK_PARSER', 'Unrecoginzed '// &
     &                 'value of the qualifier '// &
     &                  QUALS(ILQ)(1:I_LEN(QUALS(ILQ)))//': '// &
     &                  TOKEN(1:I_LEN(TOKEN))//' was met when keyword '// &
     &                 'SINGULARITY_CHECK was parsed. One of '// &
     &                  VALS1_LINE(1:I_LEN(VALS1_LINE))//' was expected' )
                   RETURN
                ELSE
                   SNGCHK_ACTION = ILV
              END IF
           ELSE IF ( ILQ .EQ. 3  .OR.  ILQ .EQ. 4  .OR. ILQ .EQ. 5 ) THEN
!
! ----------- Qualifier SOU_MIN, STA_MIN or BAS_MIN
!
              LQUALS(ILQ)    = .TRUE.
!
! ----------- Get the value of the qualifier
!
              CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
              IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                   CALL ERR_LOG ( 3405, IUER, 'SNGCHK_PARSER', 'No value '// &
     &                 'was supplied for qualifier '// &
     &                  QUALS(ILQ)(1:I_LEN(QUALS(ILQ)))//' when '// &
     &                 'keyword SINGULARITY_CHECK was parsed. One of '// &
     &                  VALS1_LINE(1:I_LEN(VALS1_LINE))//' was expected' )
                   RETURN
              END IF
!
! ----------- Parsing tyhe value of the qualifier
!
              IF ( TOKEN(1:2) .EQ. 'NO' ) THEN
                   IVAL = 0
                ELSE
                   CALL CHIN ( TOKEN, IVAL )
                   IF ( IVAL .LT. 0 ) THEN
                        CALL ERR_LOG ( 3406, IUER, 'SNGCHK_PARSER', 'Wrong '// &
     &                      'value of the qualifier '// &
     &                       QUALS(ILQ)(1:I_LEN(QUALS(ILQ)))//': '// &
     &                       TOKEN(1:I_LEN(TOKEN))//' was met when keyword '// &
     &                      'SINGULARITY_CHECK was parsed. Non-negative '// &
     &                      'integer value was expected' )
                        RETURN
                   END IF
              END IF
!
! ----------- Assign the value to SNGCHK_xxx variables from glbc4.i
!
              IF ( ILQ .EQ. 3 ) THEN
                   SNGCHK_SOUMIN = IVAL
                ELSE IF ( ILQ .EQ. 4 ) THEN
                   SNGCHK_STAMIN = IVAL
                ELSE IF ( ILQ .EQ. 5 ) THEN
                   SNGCHK_BASMIN = IVAL
              END IF
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
           CALL ERR_LOG ( 3403, IUER, 'SNGCHK_PARSER', 'Error during parsing '// &
     &         'keyword SINGULARITY_CHECK: qualifiers '// &
     &          QUALS_LINE(1:I_LEN(QUALS_LINE))//' were not supplied. '// &
     &         'Please, check syntax' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SNGCHK_PARSER  #!#
