      SUBROUTINE REPA_PARSE_CNF ( REP, CONF_FILE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine REPA_CNF  parses configuration file for program REPA.      *
! *   Results of parsing are put into the object REPA. In fact, two      *
! *   configuration files are parsed: CONF_FILE and the status file.     *
! *   If the database name field in the status file coincides with the   *
! *   of the current database, then it is parsed as well and its         *
! *   contents overrides conents of CONF_FILE. Otherwise, the status     *
! *   file is ignored.                                                   *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  CONF_FILE (CHARACTER  ) -- Name of the input configuration file.    *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *        REP ( RECORD    ) -- Object which keeps internal parameters   *
! *                             for program REPA (REsiduals Plots and    *
! *                             Ambiguities).                            *
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
! * ### 01-DEC-2004  REPA_PARSE_CNF  v1.2 (c) L. Petrov  23-SEP-2019 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'diagi.i'
      INCLUDE   'repa.i'
      TYPE     ( REP__TYPE ) :: REP
      CHARACTER  CONF_FILE*(*)
      INTEGER*4  IUER
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 256, MIND = 32 )
      CHARACTER  BUF(MBUF)*128, STR*80, STR1*80, OUT*512, REG*5
      PARAMETER  ( REG = CHAR(0)//CHAR(9)//CHAR(32)//',:' )
      INTEGER*4  NBUF, I_CNF, IP, IL, LIND, IND(2,MIND), IOS, &
     &           J1, J2, J3, J4, J5, J6, IND_INQ, IER
      LOGICAL*4  LEX
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN, LINDEX, LTM_DIF
!
! --- Read configuration file into the buffer
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( CONF_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG  ( 7711, IUER, 'REPA_PARSE_CNF', 'Error in an '// &
     &         'attempt to read a control file for REPA: '//CONF_FILE )
           RETURN
      END IF
!
! --- Check the first line. It should contain the format label
!
      IF ( BUF(1)(1:ILEN(REPA__CNF_LABEL)) .EQ. REPA__CNF_2004 ) THEN
           CALL TRAN ( 13, BUF(1), BUF(1) )
           CALL ERR_LOG  ( 7712, IUER, 'REPA_PARSE_CNF', 'Obsolete '// &
     &         'format of the REPA control file: '// &
     &          CONF_FILE(1:I_LEN(CONF_FILE))//' --  the first line: '// &
     &          BUF(1)//' Suggestion: remove your '// &
     &          CONF_FILE(1:I_LEN(CONF_FILE))//' and run solve_reset' )
           RETURN
      END IF
      IF ( BUF(1)(1:ILEN(REPA__CNF_LABEL)) .NE. REPA__CNF_LABEL ) THEN
           CALL TRAN ( 13, BUF(1), BUF(1) )
           CALL ERR_LOG  ( 7712, IUER, 'REPA_PARSE_CNF', 'Unsupported '// &
     &         'format of the REPA control file: '// &
     &          CONF_FILE(1:I_LEN(CONF_FILE))//' --  the first line: '// &
     &          BUF(1)//' Suggestion: remove your '// &
     &          CONF_FILE(1:I_LEN(CONF_FILE))//' and run solve_reset' )
           RETURN
      END IF
!
! --- Initialization
!
      I_CNF = 0
      DO 410 J1=2,NBUF
         IF ( ILEN(BUF(J1)) .EQ. 0  ) GOTO 410
         IF ( BUF(J1)(1:1) .EQ. '*' ) GOTO 410
         IF ( BUF(J1)(1:1) .EQ. '#' ) GOTO 410
         IP = INDEX  ( BUF(J1), '!' )
         IF ( IP .LE. 0 ) IP = ILEN(BUF(J1))
!
! ------ Parse the line onto words
!
         CALL EXWORD ( BUF(J1)(1:IP), MIND, LIND, IND, REG, -3 )
         IF ( LIND .LT. 2 ) GOTO 410
!
! ------ ... and convert the first word into letters to the upper registr
!
         CALL TRAN ( 11, BUF(J1)(IND(1,1):IND(2,1)), BUF(J1)(IND(1,1):IND(2,1)))
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'GOOD_COLOR' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), REP%CNF%GOOD_CLR )
              IF ( REP%CNF%GOOD_CLR .LE. 0 .OR. REP%CNF%GOOD_CLR .GT. MCLR ) THEN
                   CALL ERR_LOG ( 7713, IUER, 'REPA_PARSE_CNF', 'Bad value '// &
     &                 'of GOOD_CLR: an integer value in the range '// &
     &                 '[1, MCLR] was expected' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'BAD_COLOR' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), REP%CNF%BAD_CLR )
              IF ( REP%CNF%BAD_CLR .LE. 0 .OR. REP%CNF%BAD_CLR .GT. MCLR ) THEN
                   CALL ERR_LOG ( 7714, IUER, 'REPA_PARSE_CNF', 'Bad value '// &
     &                 'of BAD_CLR: an integer value in the range '// &
     &                 '[1, MCLR] was expected' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'UNRC_COLOR' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), REP%CNF%UNRC_CLR )
              IF ( REP%CNF%UNRC_CLR .LE. 0 .OR. REP%CNF%UNRC_CLR .GT. MCLR ) THEN
                   CALL ERR_LOG ( 7715, IUER, 'REPA_PARSE_CNF', 'Bad value '// &
     &                 'of UNRC_CLR: an integer value in the range '// &
     &                 '[1, MCLR] was expected' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'MARKED_COLOR' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), REP%CNF%MARKED_CLR )
              IF ( REP%CNF%MARKED_CLR .LE. 0 .OR. REP%CNF%MARKED_CLR .GT. MCLR ) THEN
                   CALL ERR_LOG ( 7715, IUER, 'REPA_PARSE_CNF', 'Bad value '// &
     &                 'of MARKED_CLR: an integer value in the range '// &
     &                 '[1, MCLR] was expected' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'BOX_PLOT' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                        BUF(J1)(IND(1,2):IND(2,2))  )
              REP%CNF%BOU_BOX = '?'
              DO 420 J2=1,REPA__M_BOX
                 IL = LEN(REPA__NAME_BOX(J2))
                 CALL TRAN ( 11, REPA__NAME_BOX(J2), STR )
                 IF ( BUF(J1)(IND(1,2):IND(1,2)+IL-1) .EQ. STR(1:IL) ) THEN
                      REP%CNF%BOU_IND = J2
                      REP%CNF%BOU_BOX = REPA__LET_BOX(REP%CNF%BOU_IND)
                 END IF
 420          CONTINUE
              IF ( REP%CNF%BOU_BOX .EQ. '?' ) THEN
                   CALL ERR_LOG ( 7716, IUER, 'REPA_PARSE_CNF', &
     &                 'Unsupported value of BOX_PLOT: '// &
     &                  BUF(J4)(IND(1,2):IND(2,2))//' -- Good, Bad&Good '// &
     &                 'or All were expected' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'DEFAULT_BASELINE' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                        BUF(J1)(IND(1,2):IND(2,2))  )
              REP%CNF%BASELINE = BUF(J1)(IND(1,2):IND(2,2))
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'DEFAULT_ARGUMENT' ) THEN
              REP%CNF%ARG_IND = LTM_DIF ( 0, REP__M_ARG, REP__CH_ARG, &
     &                                    BUF(J1)(IND(1,2):IND(2,2)) )
              IF ( REP%CNF%ARG_IND .LE. 0 ) THEN
                   CALL ERR_LOG ( 7717, IUER, 'REPA_PARSE_CNF', &
     &                 'Unsupported value of DEFAULT_ARGUMENT: '// &
     &                 BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'DEFAULT_VALUE' ) THEN
              REP%CNF%VAL_IND = LTM_DIF ( 0, REP__M_VAL, REP__CH_VAL, &
     &                                    BUF(J1)(IND(1,2):IND(2,2)) )
              IF ( REP%CNF%VAL_IND .LE. 0 ) THEN
                   CALL ERR_LOG ( 7718, IUER, 'REPA_PARSE_CNF', &
     &                 'Unsupported value of DEFAULT_VALUE: '// &
     &                 BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'DEFAULT_MODE' ) THEN
              REP%CNF%MOD_IND = LTM_DIF ( 0, REP__M_MOD, REP__CH_MOD, &
     &                                    BUF(J1)(IND(1,2):IND(2,2)) )
              IF ( REP%CNF%VAL_IND .LE. 0 ) THEN
                   CALL ERR_LOG ( 7719, IUER, 'REPA_PARSE_CNF', &
     &                 'Unsupported value of DEFAULT_MODE: '// &
     &                 BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'BOX_SYMMETRIC' ) THEN
              CALL CLRCH ( REP%CNF%BOX_SYMMETRIC )
              CALL TRAN  ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                         REP%CNF%BOX_SYMMETRIC )
              IF ( REP%CNF%BOX_SYMMETRIC .EQ. 'YES ' ) THEN
                   CONTINUE
                 ELSE IF ( REP%CNF%BOX_SYMMETRIC .EQ. 'NO  ' ) THEN
                   CONTINUE
                 ELSE
                   CALL ERR_LOG ( 7719, IUER, 'REPA_PARSE_CNF', &
     &                 'Unsupported value of BOX_SYMMETRIC: '// &
     &                 BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'INQUIRY_DATA' ) THEN
              REP%CNF%INQUIRY_DATA = BUF(J1)(IND(1,2):IND(2,LIND))
              DO 430 J3=2,LIND
                 IND_INQ = LTM_DIF ( 0, REPA__LINQ, REPA__CINQ, &
     &                               BUF(J1)(IND(1,J3):IND(2,J3)) )
                 IF ( IND_INQ .LE. 0 ) THEN
                      CALL LIST_TO_LINE ( REPA__LINQ, REPA__CINQ, ',', OUT )
                      CALL ERR_LOG ( 7719, IUER, 'REPA_PARSE_CNF', &
     &                    'Unsupported value in INQUIRY_DATA: '// &
     &                     BUF(J1)(IND(1,J3):IND(2,J3))//' -- one of  '// &
     &                     OUT(1:I_LEN(OUT))//'  was expected' )
                      RETURN
                 END IF
 430          CONTINUE
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'MARKED_SOURCE' ) THEN
              REP%CNF%MARKED_SOURCE = BUF(J1)(IND(1,2):IND(2,2))
              I_CNF = I_CNF + 1
            ELSE
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 7720, IUER, 'REPA_PARSE_CNF', 'Failure to '// &
     &            'parse the line '//STR(1:I_LEN(STR))//' of the control '// &
     &            'file: '//BUF(J1) )
              RETURN
         END IF
 410  CONTINUE
!
! --- Check whether all parameters were read
!
      IF ( I_CNF .NE. REPA__M_CNF  ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( I_CNF, STR )
           CALL INCH  ( REPA__M_CNF , STR1 )
           CALL ERR_LOG ( 7721, IUER, 'REPA_PARSE_CNF', 'Wrong number of '// &
     &         'keywords in the control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &         ' -- '//STR(1:I_LEN(STR))//' while '//STR1(1:I_LEN(STR1))// &
     &         ' were expected' )
           RETURN
      END IF
!
! --- Store configuration file
!
      REP%CNF%CONF_FILE = CONF_FILE
!
! --- Create the name of the status file
!
      REP%CNF%STAT_FILE = CONF_FILE(1:ILEN(CONF_FILE)-6)//'RPST'// &
     &                    CONF_FILE(ILEN(CONF_FILE)-1:ILEN(CONF_FILE))
!
      INQUIRE ( FILE=REP%CNF%STAT_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
!
! -------- If the personalized status file does not exist, take
! -------- the system default file
!
           REP%CNF%STAT_FILE = PRE_SAV_DIR(1:PRE_SV_LEN)//'repa_stat_template'
           INQUIRE ( FILE=REP%CNF%STAT_FILE, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                STR = CONF_FILE(1:ILEN(CONF_FILE)-6)//'RPST'// &
     &                CONF_FILE(ILEN(CONF_FILE)-1:ILEN(CONF_FILE))
                CALL ERR_LOG ( 7722, IUER, 'REPA_PARSE_CNF', 'No REPA '// &
     &              'status files were found: neither user file '// &
     &               STR(1:ILEN(STR))//' nor system default '// &
     &              'file '//REP%CNF%STAT_FILE )
                RETURN
           END IF
!
! -------- And then write it back as the personalized status file
!
           REP%CNF%STAT_FILE = CONF_FILE(1:ILEN(CONF_FILE)-6)//'RPST'// &
     &                         CONF_FILE(ILEN(CONF_FILE)-1:ILEN(CONF_FILE))
!
           CALL ERR_PASS ( IUER, IER )
           CALL REPA_WRISTAT ( REP, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7723, IUER, 'REPA_PARSE_CNF', 'Error in '// &
     &              'an attempt to write REPA status into the file' )
                RETURN
           END IF
      END IF
!
! --- Read the status file into the buffer
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( REP%CNF%STAT_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG  ( 7723, IUER, 'REPA_PARSE_CNF', 'Error in reading '// &
     &         'REPA status file: '//REP%CNF%STAT_FILE )
           RETURN
      END IF
!
      IF ( BUF(1)(1:ILEN(REPA__STS_LABEL)) .EQ. REPA__STS_2004 ) THEN
           CALL TRAN ( 13, BUF(1), BUF(1) )
           CALL ERR_LOG  ( 7724, IUER, 'REPA_PARSE_CNF', 'Obsolete '// &
     &         'format of the REPA status file: '// &
     &          REP%CNF%STAT_FILE(1:I_LEN(REP%CNF%STAT_FILE))//' --  the '// &
     &         'first line: '//BUF(1)(1:I_LEN(BUF(1)))// &
     &         ' Please use system default file '// &
     &         PRE_SAV_DIR(1:PRE_SV_LEN)//'REPA_STAT.tmpl' )
           RETURN
      END IF
!
      IF ( BUF(1)(1:ILEN(REPA__STS_LABEL)) .NE. REPA__STS_LABEL ) THEN
           CALL TRAN ( 13, BUF(1), BUF(1) )
           CALL ERR_LOG  ( 7724, IUER, 'REPA_PARSE_CNF', 'Unsupported '// &
     &         'format of the REPA status file: '// &
     &          REP%CNF%STAT_FILE(1:I_LEN(REP%CNF%STAT_FILE))//' --  the '// &
     &         'first line: '//BUF(1) )
           RETURN
      END IF
!
! --- First search for the keyword DB_NAME
!
      DO 440 J4=2,NBUF
         IF ( ILEN(BUF(J4)) .EQ. 0  ) GOTO 440
         IF ( BUF(J4)(1:1) .EQ. '*' ) GOTO 440
         IF ( BUF(J4)(1:1) .EQ. '#' ) GOTO 440
         IP = INDEX  ( BUF(J4), '!' )
         IF ( IP .LE. 0 ) IP = ILEN(BUF(J4))
!
! ------ Parse the line onto words
!
         CALL EXWORD ( BUF(J4)(1:IP), MIND, LIND, IND, REG, -3 )
         IF ( LIND .LT. 2 ) GOTO 440
!
         CALL TRAN ( 11, BUF(J4)(IND(1,1):IND(2,1)), BUF(J4)(IND(1,1):IND(2,1)))
         IF ( BUF(J4)(IND(1,1):IND(2,1)) .EQ. 'DB_NAME' ) THEN
              IL = I_LEN(REP%DBNAME_STR)
              IF ( REP%DBNAME_STR .NE. BUF(J4)(IND(1,2):IND(1,2)+IL-1) ) THEN
                   REP%CNF%PAGE = 1
!
! ---------------- If the status file had not the same database name as the
! ---------------- current database skip further reading. Then default modes
! ---------------- stored in the configuration file will be used
!
                   GOTO 860
              END IF
         END IF
 440  CONTINUE
!
! --- If we reach this place, this means that the status file corresponds
! --- to the current database. We have to read it further to the end. Modes
! --- from the status file will replace default values from the
! --- configuration file
!
      I_CNF = 0
      DO 450 J5=2,NBUF
         IF ( ILEN(BUF(J5)) .EQ. 0  ) GOTO 450
         IF ( BUF(J5)(1:1) .EQ. '*' ) GOTO 450
         IF ( BUF(J5)(1:1) .EQ. '#' ) GOTO 450
         IP = INDEX  ( BUF(J5), '!' )
         IF ( IP .LE. 0 ) IP = ILEN(BUF(J5))
!
! ------ Parse the line onto words
!
         CALL EXWORD ( BUF(J5)(1:IP), MIND, LIND, IND, REG, -3 )
         IF ( LIND .LT. 2 ) GOTO 450
!
         CALL TRAN ( 11, BUF(J5)(IND(1,1):IND(2,1)), BUF(J5)(IND(1,1):IND(2,1)))
         IF ( BUF(J5)(IND(1,1):IND(2,1)) .EQ. 'DB_NAME' ) THEN
              CONTINUE
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J5)(IND(1,1):IND(2,1)) .EQ. 'BASELINE' ) THEN
              REP%CNF%BASELINE = BUF(J5)(IND(1,2):IND(1,2)+15)
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J5)(IND(1,1):IND(2,1)) .EQ. 'BOX_PLOT' ) THEN
              CALL TRAN ( 11, BUF(J5)(IND(1,2):IND(2,2)), &
     &                        BUF(J5)(IND(1,2):IND(2,2))  )
              REP%CNF%BOU_BOX = '?'
              DO 460 J6=1,REPA__M_BOX
                 IL = LEN(REPA__NAME_BOX(J6))
                 CALL TRAN ( 11, REPA__NAME_BOX(J6), STR )
                 IF ( BUF(J5)(IND(1,2):IND(1,2)+IL-1) .EQ. STR(1:IL) ) THEN
                      REP%CNF%BOU_IND = J6
                      REP%CNF%BOU_BOX = REPA__LET_BOX(REP%CNF%BOU_IND)
                 END IF
 460          CONTINUE
              IF ( REP%CNF%BOU_BOX .EQ. '?' ) THEN
                   CALL ERR_LOG ( 7725, IUER, 'REPA_PARSE_CNF', &
     &                 'Unsupported value of BOX_PLOT: '// &
     &                  BUF(J5)(IND(1,2):IND(2,2))//' -- Good, Bad&Good '// &
     &                 'or All were expected' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J5)(IND(1,1):IND(2,1)) .EQ. 'ARGUMENT' ) THEN
              REP%CNF%ARG_IND = LTM_DIF ( 0, REP__M_ARG, REP__CH_ARG, &
     &                                    BUF(J5)(IND(1,2):IND(2,2)) )
              IF ( REP%CNF%ARG_IND .LE. 0 ) THEN
                   CALL ERR_LOG ( 7726, IUER, 'REPA_PARSE_CNF', &
     &                 'Unsupported value of ARGUMENT in REPA status file: '// &
     &                 BUF(J5)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J5)(IND(1,1):IND(2,1)) .EQ. 'VALUE' ) THEN
              REP%CNF%VAL_IND = LTM_DIF ( 0, REP__M_VAL, REP__CH_VAL, &
     &                                    BUF(J5)(IND(1,2):IND(2,2)) )
              IF ( REP%CNF%VAL_IND .LE. 0 ) THEN
                   CALL ERR_LOG ( 7727, IUER, 'REPA_PARSE_CNF', &
     &                 'Unsupported value of VALUE in REPA status file: '// &
     &                 BUF(J5)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J5)(IND(1,1):IND(2,1)) .EQ. 'MODE' ) THEN
              REP%CNF%MOD_IND = LTM_DIF ( 0, REP__M_MOD, REP__CH_MOD, &
     &                                    BUF(J5)(IND(1,2):IND(2,2)) )
              IF ( REP%CNF%VAL_IND .LE. 0 ) THEN
                   CALL ERR_LOG ( 7728, IUER, 'REPA_PARSE_CNF', &
     &                 'Unsupported value of MODE in REPA status file: '// &
     &                 BUF(J5)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J5)(IND(1,1):IND(2,1)) .EQ. 'BOX_SYMMETRIC' ) THEN
              CALL CLRCH ( REP%CNF%BOX_SYMMETRIC )
              CALL TRAN  ( 11, BUF(J5)(IND(1,2):IND(2,2)), &
     &                         REP%CNF%BOX_SYMMETRIC )
              IF ( REP%CNF%BOX_SYMMETRIC .EQ. 'YES ' ) THEN
                   CONTINUE
                 ELSE IF ( REP%CNF%BOX_SYMMETRIC .EQ. 'NO  ' ) THEN
                   CONTINUE
                 ELSE
                   CALL ERR_LOG ( 7729, IUER, 'REPA_PARSE_CNF', &
     &                 'Unsupported value of BOX_SYMMETRIC: '// &
     &                 BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J5)(IND(1,1):IND(2,1)) .EQ. 'MARKED_SOURCE' ) THEN
              REP%CNF%MARKED_SOURCE = BUF(J5)(IND(1,2):IND(2,2))
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J5)(IND(1,1):IND(2,1)) .EQ. 'PAGE' ) THEN
              CALL CHIN  ( BUF(J5)(IND(1,2):IND(2,2)), REP%CNF%PAGE )
              IF ( REP%CNF%PAGE .LE. 0           .OR. &
     &             REP%CNF%PAGE .GT. REPA__M_BAS      ) THEN
                   REP%CNF%PAGE = 1
              END IF
              I_CNF = I_CNF + 1
            ELSE
              CALL CLRCH ( STR )
              CALL INCH  ( J5, STR )
              CALL ERR_LOG ( 7730, IUER, 'REPA_PARSE_CNF', 'Failure to '// &
     &            'parse the line '//STR(1:I_LEN(STR))//' of the REPA '// &
     &            'status file '// &
     &             REP%CNF%STAT_FILE(1:I_LEN(REP%CNF%STAT_FILE))//' -- '// &
     &             BUF(J5) )
              RETURN
         END IF
 450  CONTINUE
!
! --- Check whether all parameters were read
!
      IF ( I_CNF .NE. REPA__M_STS ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( I_CNF, STR )
           CALL INCH  ( REPA__M_STS , STR1 )
           CALL ERR_LOG ( 7731, IUER, 'REPA_PARSE_CNF', 'Wrong number of '// &
     &         'keywords in the status file '// &
     &          REP%CNF%STAT_FILE(1:I_LEN(REP%CNF%STAT_FILE))// &
     &         ' -- '//STR(1:I_LEN(STR))//' while '//STR1(1:I_LEN(STR1))// &
     &         ' were expected' )
           RETURN
      END IF
 860  CONTINUE
!
! --- Open files for REPA information
!
      REP%CNF%REPI_FILE = CONF_FILE(1:ILEN(CONF_FILE)-6)//'REPI'// &
     &                    CONF_FILE(ILEN(CONF_FILE)-1:ILEN(CONF_FILE))
      REP%CNF%LUN_REPI = GET_UNIT()
      OPEN ( UNIT=REP%CNF%LUN_REPI, FILE=REP%CNF%REPI_FILE, STATUS='UNKNOWN', &
     &       IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           WRITE ( 6, * ) ' IOS=',IOS
           CALL ERR_LOG ( 7732, IUER, 'REPA_PARSE_CNF', 'Error in an '// &
     &         'attempt to open for writing REPA incormation file '// &
     &          REP%CNF%REPI_FILE )
           RETURN
      END IF
!
! --- Store flags whether to show bad points and/or unrecoverable points
!
      REP%CNF%SHOW_CBAD = CNPLT_SHOW_CBAD
      REP%CNF%SHOW_UNRC = CNPLT_SHOW_UNRC
!
! --- Store the name of the status file
!
      REP%CNF%STAT_FILE = CONF_FILE(1:ILEN(CONF_FILE)-6)//'RPST'// &
     &                    CONF_FILE(ILEN(CONF_FILE)-1:ILEN(CONF_FILE))
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  REPA_PARSE_CNF  #!#
