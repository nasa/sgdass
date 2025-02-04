      FUNCTION   CORRELATIONS_PARSER ( STRING, IUER )
! ************************************************************************
! *                                                                      *
! *   Finction  CORRELATIONS_PARSER  parses the line STRING with         *
! *   "CORRELATIONS" control codes of the section $OUTPUT opf BATOPT     *
! *   control language. It reads the next line(s) of the control file if *
! *   stuff related to CORRELATIONS keywrod is continued at the next     *
! *   line.                                                              *
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
! * ###  29-SEP-99 CORRELATIONS_PARSER v1.1 (c) L. Petrov 05-OCT-99  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      LOGICAL*4  CORRELATIONS_PARSER
      CHARACTER  STRING*(*)
      CHARACTER  TOKEN*128
      INTEGER*4  IUER
      LOGICAL*4  FL_SUC, CP_CHECK
      INTEGER*4  J1
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
      IF ( TOKEN(1:I_LEN(TOKEN)) .EQ. 'YES' ) THEN
           CORRELATIONS_PARSER = .TRUE.
           COR_FLAG = .TRUE.
!
! -------- Get the next token
!
           DO 410 J1=1,4
              CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
 910          CONTINUE
              IF ( TOKEN(1:I_LEN(TOKEN)) .EQ. 'FORMAT' ) THEN
                   CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                   IF ( TOKEN(1:I_LEN(TOKEN)) .EQ. 'ASCII' ) THEN
                        COROUT_FORM       =  CRL__ASC
                      ELSE IF ( TOKEN(1:I_LEN(TOKEN)) .EQ. 'BINARY' ) THEN
                        COROUT_FORM       =  CRL__BIN
                      ELSE IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                        CALL ERR_LOG ( 3811, IUER, 'CORRELATIONS_PARSER', &
     &                      'Error in parsing the value of qualifier '// &
     &                      'FORMAT of the keyword CORRELATIONS: value '// &
     &                      'ASCII or BINARY was expected' )
                        RETURN
                      ELSE
                        CALL ERR_LOG ( 3812, IUER, 'CORRELATIONS_PARSER', &
     &                      'Error in parsing the value of qualifier '// &
     &                      'FORMAT of the keyword CORRELATIONS: value '// &
     &                       TOKEN(1:I_LEN(TOKEN))//' is not supported. '// &
     &                      'ASCII or BINARY was expected' )
                        RETURN
                   END IF
                ELSE IF ( TOKEN(1:I_LEN(TOKEN)) .EQ. 'GLO_GLO' ) THEN
                   COR_GG_FLAG = .TRUE.
!
! ---------------- Parse global-global parameters
!
                   CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                   IF ( TOKEN(1:I_LEN(TOKEN)) .EQ. 'PARAM_INCLUDE' ) THEN
!
! --------------------- Get the file name and check
!
                        CALL SPLIT_STRING ( STRING, TOKEN, STRING )
                        IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                             CALL ERR_LOG ( 3821, IUER, 'CORRELATIONS_PARSER', &
     &                           'Error in parsing the value of qualifier '// &
     &                           'GLO_GLO PARAM_INCLUDE of the keyword '// &
     &                           'CORRELATIONS: file name was not supplied ' )
                             RETURN
                        END IF
!
                        FL_SUC = CP_CHECK ( TOKEN, COR_GG_INCFIL )
                        IF ( .NOT. FL_SUC ) THEN
                             CALL ERR_LOG ( 3822, IUER, 'CORRELATIONS_PARSER', &
     &                           'Error in parsing the value of qualifier '// &
     &                           'GLO_GLO PARAM_INCLUDE of the keyword '// &
     &                           'CORRELATIONS: file '// &
     &                            COR_GG_INCFIL(1:I_LEN(COR_GG_INCFIL))// &
     &                           ' was not found' )
                             RETURN
                        END IF
                        CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
!
                        IF ( TOKEN(1:I_LEN(TOKEN)) .EQ. 'PARAM_EXCLUDE' ) THEN
!
! -------------------------- Get the file name and check
!
                             CALL SPLIT_STRING ( STRING, TOKEN, STRING )
                             IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                                  CALL ERR_LOG ( 3823, IUER, &
     &                                'CORRELATIONS_PARSER', 'Error in '// &
     &                                'parsing the value of qualifier '// &
     &                                'GLO_GLO PARAM_EXCLUDE of the keyword '// &
     &                                'CORRELATIONS: file name was not '// &
     &                                'supplied ' )
                                  RETURN
                             END IF
!
                             FL_SUC = CP_CHECK ( TOKEN, COR_GG_EXCFIL )
                             IF ( .NOT. FL_SUC ) THEN
                                  CALL ERR_LOG ( 3824, IUER, &
     &                                'CORRELATIONS_PARSER', 'Error in '// &
     &                                'parsing the value of qualifier '// &
     &                                'GLO_GLO PARAM_EXCLUDE of the keyword '// &
     &                                'CORRELATIONS: file '// &
     &                                 COR_GG_EXCFIL(1:I_LEN(COR_GG_EXCFIL))// &
     &                                ' was not found' )
                                  RETURN
                             END IF
                           ELSE IF ( ILEN(TOKEN) .EQ. 0 ) THEN
!
! -------------------------- No tokens any more
!
                             GOTO 810
                           ELSE
!
! -------------------------- Not PARAM_EXCLUDE. We consider that values for
! -------------------------- qualifer GLO_GLO are over
!
                             GOTO 910
                        END IF
                      ELSE IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                        CALL ERR_LOG ( 3825, IUER, 'CORRELATIONS_PARSER', &
     &                      'Error in parsing the value of qualifier '// &
     &                      'GLO_GLO of the keyword CORRELATIONS. '// &
     &                      'No value was supplied. PARAM_INCLUDE was'// &
     &                      'expected' )
                        RETURN
                      ELSE
                        CALL ERR_LOG ( 3826, IUER, 'CORRELATIONS_PARSER', &
     &                      'Error in parsing the value of qualifier '// &
     &                      'GLO_GLO of the keyword CORRELATIONS: '// &
     &                      'value '//TOKEN(1:I_LEN(TOKEN))//' is not '// &
     &                      'supported. PARAM_INCLUDE was expected' )
                        RETURN
                   END IF ! params in glo_glo
                ELSE IF ( TOKEN(1:I_LEN(TOKEN)) .EQ. 'GLO_LOC' ) THEN
                   COR_GL_FLAG = .TRUE.
!
! ---------------- Parse global-local parameters
!
                   CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                   IF ( TOKEN(1:I_LEN(TOKEN)) .EQ. 'PARAM_INCLUDE' ) THEN
!
! --------------------- Get the file name and check
!
                        CALL SPLIT_STRING ( STRING, TOKEN, STRING )
                        IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                             CALL ERR_LOG ( 3831, IUER, 'CORRELATIONS_PARSER', &
     &                           'Error in parsing the value of qualifier '// &
     &                           'GLO_LOC PARAM_INCLUDE of the keyword '// &
     &                           'CORRELATIONS: file name was not supplied ' )
                             RETURN
                        END IF
!
                        FL_SUC = CP_CHECK ( TOKEN, COR_GL_INCFIL )
                        IF ( .NOT. FL_SUC ) THEN
                             CALL ERR_LOG ( 3832, IUER, 'CORRELATIONS_PARSER', &
     &                           'Error in parsing the value of qualifier '// &
     &                           'GLO_LOC PARAM_INCLUDE of the keyword '// &
     &                           'CORRELATIONS: file '// &
     &                            COR_GL_INCFIL(1:I_LEN(COR_GL_INCFIL))// &
     &                           ' was not found' )
                             RETURN
                        END IF
                        CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
!
                        IF ( TOKEN(1:I_LEN(TOKEN)) .EQ. 'PARAM_EXCLUDE' ) THEN
!
! -------------------------- Get the file name and check
!
                             CALL SPLIT_STRING ( STRING, TOKEN, STRING )
                             IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                                  CALL ERR_LOG ( 3833, IUER, &
     &                                'CORRELATIONS_PARSER', 'Error in '// &
     &                                'parsing the value of qualifier '// &
     &                                'GLO_LOC PARAM_EXCLUDE of the keyword '// &
     &                                'CORRELATIONS: file name was not '// &
     &                                'supplied ' )
                                  RETURN
                             END IF
!
                             FL_SUC = CP_CHECK ( TOKEN, COR_GL_EXCFIL )
                             IF ( .NOT. FL_SUC ) THEN
                                  CALL ERR_LOG ( 3834, IUER, &
     &                                'CORRELATIONS_PARSER', 'Error in '// &
     &                                'parsing the value of qualifier '// &
     &                                'GLO_LOC PARAM_EXCLUDE of the keyword '// &
     &                                'CORRELATIONS: file '// &
     &                                 COR_GL_EXCFIL(1:I_LEN(COR_GL_EXCFIL))// &
     &                                ' was not found' )
                                  RETURN
                             END IF
                           ELSE IF ( ILEN(TOKEN) .EQ. 0 ) THEN
!
! -------------------------- No tokens any more
!
                             GOTO 810
                           ELSE
!
! -------------------------- Not PARAM_EXCLUDE. We consider that values for
! -------------------------- qualifer GLO_LOC are over
!
                             GOTO 910
                        END IF
                      ELSE IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                        CALL ERR_LOG ( 3835, IUER, 'CORRELATIONS_PARSER', &
     &                      'Error in parsing the value of qualifier '// &
     &                      'GLO_LOC of the keyword CORRELATIONS. '// &
     &                      'No value was supplied. PARAM_INCLUDE was'// &
     &                      'expected' )
                        RETURN
                      ELSE
                        CALL ERR_LOG ( 3836, IUER, 'CORRELATIONS_PARSER', &
     &                      'Error in parsing the value of qualifier '// &
     &                      'GLO_LOC of the keyword CORRELATIONS: '// &
     &                      'value '//TOKEN(1:I_LEN(TOKEN))//' is not '// &
     &                      'supported. PARAM_INCLUDE was expected' )
                        RETURN
                   END IF ! params in glo_log
                ELSE IF ( TOKEN(1:I_LEN(TOKEN)) .EQ. 'LOC_LOC' ) THEN
                   COR_LL_FLAG = .TRUE.
!
! ---------------- Parse local-local parameters
!
                   CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                   IF ( TOKEN(1:I_LEN(TOKEN)) .EQ. 'PARAM_INCLUDE' ) THEN
!
! --------------------- Get the file name and check
!
                        CALL SPLIT_STRING ( STRING, TOKEN, STRING )
                        IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                             CALL ERR_LOG ( 3841, IUER, 'CORRELATIONS_PARSER', &
     &                           'Error in parsing the value of qualifier '// &
     &                           'LOC_LOC PARAM_INCLUDE of the keyword '// &
     &                           'CORRELATIONS: file name was not supplied ' )
                             RETURN
                        END IF
!
                        FL_SUC = CP_CHECK ( TOKEN, COR_LL_INCFIL )
                        IF ( .NOT. FL_SUC ) THEN
                             CALL ERR_LOG ( 3842, IUER, 'CORRELATIONS_PARSER', &
     &                           'Error in parsing the value of qualifier '// &
     &                           'LOC_LOC PARAM_INCLUDE of the keyword '// &
     &                           'CORRELATIONS: file '// &
     &                            COR_LL_INCFIL(1:I_LEN(COR_LL_INCFIL))// &
     &                           ' was not found' )
                             RETURN
                        END IF
                        CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
!
                        IF ( TOKEN(1:I_LEN(TOKEN)) .EQ. 'PARAM_EXCLUDE' ) THEN
!
! -------------------------- Get the file name and check
!
                             CALL SPLIT_STRING ( STRING, TOKEN, STRING )
                             IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                                  CALL ERR_LOG ( 3843, IUER, &
     &                                'CORRELATIONS_PARSER', 'Error in '// &
     &                                'parsing the value of qualifier '// &
     &                                'LOC_LOC PARAM_EXCLUDE of the keyword '// &
     &                                'CORRELATIONS: file name was not '// &
     &                                'supplied ' )
                                  RETURN
                             END IF
!
                             FL_SUC = CP_CHECK ( TOKEN, COR_LL_EXCFIL )
                             IF ( .NOT. FL_SUC ) THEN
                                  CALL ERR_LOG ( 3844, IUER, &
     &                                'CORRELATIONS_PARSER', 'Error in '// &
     &                                'parsing the value of qualifier '// &
     &                                'LOC_LOC PARAM_EXCLUDE of the keyword '// &
     &                                'CORRELATIONS: file '// &
     &                                 COR_LL_EXCFIL(1:I_LEN(COR_LL_EXCFIL))// &
     &                                ' was not found' )
                                  RETURN
                             END IF
                           ELSE IF ( ILEN(TOKEN) .EQ. 0 ) THEN
!
! -------------------------- No tokens any more
!
                             GOTO 810
                           ELSE
!
! -------------------------- Not PARAM_EXCLUDE. We consider that values for
! -------------------------- qualifer LOC_LOC are over
!
                             GOTO 910
                        END IF
                      ELSE IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                        CALL ERR_LOG ( 3845, IUER, 'CORRELATIONS_PARSER', &
     &                      'Error in parsing the value of qualifier '// &
     &                      'LOC_LOC of the keyword CORRELATIONS. '// &
     &                      'No value was supplied. PARAM_INCLUDE was'// &
     &                      'expected' )
                        RETURN
                      ELSE
                        CALL ERR_LOG ( 3846, IUER, 'CORRELATIONS_PARSER', &
     &                      'Error in parsing the value of qualifier '// &
     &                      'LOC_LOC of the keyword CORRELATIONS: '// &
     &                      'value '//TOKEN(1:I_LEN(TOKEN))//' is not '// &
     &                      'supported. PARAM_INCLUDE was expected' )
                        RETURN
                   END IF ! params in loc_loc
                ELSE IF ( TOKEN(1:I_LEN(TOKEN)) .EQ. 'CROSS_LOC' ) THEN
                   COR_CL_FLAG = .TRUE.
!
! ---------------- Parse cross-local parameters
!
                   CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                   IF ( TOKEN(1:I_LEN(TOKEN)) .EQ. 'PARAM_INCLUDE' ) THEN
!
! --------------------- Get the file name and check
!
                        CALL SPLIT_STRING ( STRING, TOKEN, STRING )
                        IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                             CALL ERR_LOG ( 3851, IUER, 'CORRELATIONS_PARSER', &
     &                           'Error in parsing the value of qualifier '// &
     &                           'CROSS_LOC PARAM_INCLUDE of the keyword '// &
     &                           'CORRELATIONS: file name was not supplied ' )
                             RETURN
                        END IF
!
                        FL_SUC = CP_CHECK ( TOKEN, COR_CL_INCFIL )
                        IF ( .NOT. FL_SUC ) THEN
                             CALL ERR_LOG ( 3852, IUER, 'CORRELATIONS_PARSER', &
     &                           'Error in parsing the value of qualifier '// &
     &                           'CROSS_LOC PARAM_INCLUDE of the keyword '// &
     &                           'CORRELATIONS: file '// &
     &                            COR_CL_INCFIL(1:I_LEN(COR_CL_INCFIL))// &
     &                           ' was not found' )
                             RETURN
                        END IF
                        CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
!
                        IF ( TOKEN(1:I_LEN(TOKEN)) .EQ. 'PARAM_EXCLUDE' ) THEN
!
! -------------------------- Get the file name and check
!
                             CALL SPLIT_STRING ( STRING, TOKEN, STRING )
                             IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                                  CALL ERR_LOG ( 3853, IUER, &
     &                                'CORRELATIONS_PARSER', 'Error in '// &
     &                                'parsing the value of qualifier '// &
     &                                'CROSS_LOC PARAM_EXCLUDE of the keyword '// &
     &                                'CORRELATIONS: file name was not '// &
     &                                'supplied ' )
                                  RETURN
                             END IF
!
                             FL_SUC = CP_CHECK ( TOKEN, COR_CL_EXCFIL )
                             IF ( .NOT. FL_SUC ) THEN
                                  CALL ERR_LOG ( 3854, IUER, &
     &                                'CORRELATIONS_PARSER', 'Error in '// &
     &                                'parsing the value of qualifier '// &
     &                                'CROSS_LOC PARAM_EXCLUDE of the keyword '// &
     &                                'CORRELATIONS: file '// &
     &                                 COR_CL_EXCFIL(1:I_LEN(COR_CL_EXCFIL))// &
     &                                ' was not found' )
                                  RETURN
                             END IF
                             CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                           ELSE IF ( TOKEN .EQ. 'SES_INCLUDE' ) THEN
                             CONTINUE
                           ELSE IF ( ILEN(TOKEN) .EQ. 0 ) THEN
!
! -------------------------- No token
!
                             CALL ERR_LOG ( 3855, IUER, 'CORRELATIONS_PARSER', &
     &                           'Error in parsing the value of qualifier '// &
     &                           'CROSS_LOC of the keyword CORRELATIONS: '// &
     &                           'value '//TOKEN(1:I_LEN(TOKEN))//' is not '// &
     &                           'supported. PARAM_EXCLUDE or SES_INCLUDE '// &
     &                           'was expected' )
                             RETURN
                           ELSE
                             CALL ERR_LOG ( 3856, IUER, 'CORRELATIONS_PARSER', &
     &                           'Error in parsing the value of qualifier '// &
     &                           'CROSS_LOC of the keyword CORRELATIONS: '// &
     &                           'value '//TOKEN(1:I_LEN(TOKEN))//' is not '// &
     &                           'supported. PARAM_EXLUDED or SES_INCLUDE '// &
     &                           'was expected' )
                             RETURN
                        END IF
!
                        IF ( TOKEN .EQ. 'SES_INCLUDE' ) THEN
                             CALL SPLIT_STRING ( STRING, TOKEN, STRING )
                             IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                                  CALL ERR_LOG ( 3857, IUER, &
     &                                'CORRELATIONS_PARSER', 'Error in '// &
     &                                'parsing the value of qualifier '// &
     &                                'CROSS_LOC SES_INCLUDE of the '// &
     &                                'keyword CORRELATIONS: file name was '// &
     &                                'not supplied ' )
                                  RETURN
                             END IF
!
                             FL_SUC = CP_CHECK ( TOKEN, COR_CL_INCSES )
                             IF ( .NOT. FL_SUC ) THEN
                                  CALL ERR_LOG ( 3858, IUER, &
     &                                'CORRELATIONS_PARSER', 'Error in '// &
     &                                'parsing the value of qualifier '// &
     &                                'CROSS_LOC SES_INCLUDE of the keyword '// &
     &                                'CORRELATIONS: file '// &
     &                                 COR_CL_INCSES(1:I_LEN(COR_CL_INCSES))// &
     &                                ' was not found' )
                                  RETURN
                             END IF
!
                             CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                             IF ( TOKEN(1:I_LEN(TOKEN)) .EQ. 'SES_EXCLUDE') THEN
                                  CALL SPLIT_STRING ( STRING, TOKEN, STRING )
                                  IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                                       CALL ERR_LOG ( 3859, IUER, &
     &                                     'CORRELATIONS_PARSER', 'Error in '// &
     &                                     'parsing the value of qualifier '// &
     &                                     'CROSS_LOC SES_EXCLUDE of the '// &
     &                                     'keyword CORRELATIONS: file name '// &
     &                                     'was not supplied ' )
                                       RETURN
                                  END IF
!
                                  FL_SUC = CP_CHECK ( TOKEN, COR_CL_EXCSES )
                                  IF ( .NOT. FL_SUC ) THEN
                                       CALL ERR_LOG ( 3860, IUER, &
     &                                     'CORRELATIONS_PARSER', 'Error in '// &
     &                                     'parsing the value of qualifier '// &
     &                                     'CROSS_LOC SES_EXCLUDE of the '// &
     &                                     'keyword CORRELATIONS: file '// &
     &                                      COR_CL_EXCSES(1:I_LEN(COR_CL_EXCSES))// &
     &                                     ' was not found' )
                                       RETURN
                                  END IF
                               ELSE IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                                  GOTO 810
                               ELSE
                                  GOTO 910
                             END IF
                           ELSE IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                             CALL ERR_LOG ( 3861, IUER, 'CORRELATIONS_PARSER', &
     &                           'Error in parsing the value of qualifier '// &
     &                           'CROSS_LOC of the keyword CORRELATIONS. '// &
     &                           'SES_INCLUDE value was not supplied.' )
                             RETURN
                           ELSE
                             CALL ERR_LOG ( 3862, IUER, 'CORRELATIONS_PARSER', &
     &                           'Error in parsing the value of qualifier '// &
     &                           'CROSS_LOC of the keyword CORRELATIONS: '// &
     &                           'value '//TOKEN(1:I_LEN(TOKEN))//' is not '// &
     &                           'supported. SES_INCLUDE '// &
     &                           'was expected' )
                             RETURN
                        END IF
                      ELSE IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                        CALL ERR_LOG ( 3863, IUER, 'CORRELATIONS_PARSER', &
     &                      'Error in parsing the value of qualifier '// &
     &                      'CROSS_LOC of the keyword CORRELATIONS. '// &
     &                      'No value was supplied. SES_INCLUDE was'// &
     &                      'expected' )
                        RETURN
                      ELSE
                        CALL ERR_LOG ( 3864, IUER, 'CORRELATIONS_PARSER', &
     &                      'Error in parsing the value of qualifier '// &
     &                      'CROSS_LOC of the keyword CORRELATIONS: '// &
     &                      'value '//TOKEN(1:I_LEN(TOKEN))//' is not '// &
     &                      'supported. PARAM_INCLUDE was expected' )
                        RETURN
                   END IF ! params in cross_loc
                ELSE IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                   GOTO 810
              END IF
 410       CONTINUE
!
 810       CONTINUE
           IF ( COROUT_FORM .EQ. CRL__UND ) THEN
                CALL ERR_LOG ( 3891, IUER, 'CORRELATIONS_PARSER', &
     &              'Error in parsing qualifiers of a keyword '// &
     &              'CORRELATIONS in $OUTPUT section: qualifier FORMAT '// &
     &              'has not been found' )
                RETURN
           END IF
!
           IF ( ILEN(COR_GG_INCFIL) .EQ. 0  .AND. &
     &          ILEN(COR_GL_INCFIL) .EQ. 0  .AND. &
     &          ILEN(COR_LL_INCFIL) .EQ. 0  .AND. &
     &          ILEN(COR_CL_INCFIL) .EQ. 0         ) THEN
!
                CALL ERR_LOG ( 3892, IUER, 'CORRELATIONS_PARSER', &
     &              'Error in parsing qualifier YES of a keyword '// &
     &              'CORRELATIONS in $OUTPUT section: no one values '// &
     &              'followed the value YES, except FORMAT' )
                RETURN
           END IF
         ELSE IF ( TOKEN(1:I_LEN(TOKEN)) .EQ. 'NO' ) THEN
           CORRELATIONS_PARSER = .FALSE.
           COR_FLAG = .FALSE.
         ELSE
           CALL ERR_LOG ( 3892, IUER, 'CORRELATIONS_PARSER', &
     &         'Unsupported qualifier of a keyword CORRELATIONS '// &
     &         'in $OUTPUT section: '//TOKEN(1:I_LEN(TOKEN))// &
     &         ' -- one of NO, YES was expected' )
           RETURN
      END IF
!
      IF ( COR_CL_FLAG ) THEN
           CALL ERR_LOG ( 3893, IUER, 'CORRELATIONS_PARSER', 'Support of '// &
     &         'computation of cross-local correltaions is not yet '// &
     &         'implemented' )
           RETURN
      END IF
!
      CALL USE_GLBFIL_4 ( 'OWC' )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END   !#!  CORRELATIONS_PARSER  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   CP_CHECK ( TOKEN, FINAM )
! ************************************************************************
! *                                                                      *
! *   Auxiliary  function  CH_CHECK  copies the input variable TOKEN     *
! *   to the output variable FINAM, and then checks does it is a valid   *
! *   file name. If TOKEN has  the first symbol "/" then it consider it  *
! *   as a full path name. If not, then it prepends the prefix taken     *
! *   from PRE_SAVE_DIR defined in ../precm .                            *
! *                                                                      *
! *   CH_CHECK returns .TRUE. if the output variable FINAM keeps a valid *
! *   file name and .FALSE. in all other cases.                          *
! *                                                                      *
! *  ###  29-SEP-99    CP_CHECK    v1.0  (c)  L. Petrov  30-SEP-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      LOGICAL*4  CP_CHECK
      CHARACTER  TOKEN*(*), FINAM*(*)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      FINAM = TOKEN
      CP_CHECK = .FALSE.
      IF ( ILEN(FINAM) .EQ. 0 ) RETURN
!
      IF ( FINAM(1:1) .EQ. '/' ) THEN
           INQUIRE ( FILE=FINAM, EXIST=CP_CHECK )
           RETURN
        ELSE
           FINAM = PRE_SAV_DIR(1:PRE_SV_LEN)//FINAM
           INQUIRE ( FILE=FINAM, EXIST=CP_CHECK )
           RETURN
      END IF
!
      RETURN
      END  !#!   CP_CHECK  #!#
