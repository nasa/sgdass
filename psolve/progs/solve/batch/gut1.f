      SUBROUTINE GUT1 ( UT1FLG, OFFLG, RATFLG, ACCEOP_FLG, TOKEN_OUT, &
     &                  STRING, IEOP_FLG, REOP_FLG, IEOPLL, EOPMID, &
     &                  EOP_EPOCH_MJD, EOP_EPOCH_SEC, EOP_BEFORE_SEC_TAI, &
     &                  EOP_AFTER_SEC_TAI )
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
!
!     Parse UT1/PM line
!
!     INPUT Variables:
      CHARACTER*(*) STRING !STRING - String to be parsed
!
!     OUTPUT Variables:
!
      CHARACTER    UT1FLG*1
      CHARACTER    TOKEN_OUT*(*)
      INTEGER*2    OFFLG, RATFLG, ACCEOP_FLG, IEOP_FLG(6), IEOPLL
      INTEGER*4    IOS, MJD, EOP_EPOCH_MJD, IER
      REAL*8       REOP_FLG(4), EOP_EPOCH_SEC, EOP_BEFORE_SEC_TAI, EOP_AFTER_SEC_TAI 
      INTEGER*2    EOPMID
!
!     UT1FLG - Earth orientation flag.
!     TOKEN_out - The last token picked up from string.
!     OFFLG - Earth orientation offset flag.
!     RATFLG - Earth orientation rate flag.
!     ACCEOP_FLG - Earth orientation acceleration flag.
!     IEOP_FLG - Local earth orientation flags.
!     IEOPLL - Earth orientation plot flag.
!     REOP_FLG - Earth orientation intervals, constraints.
!
!     LOCAL VARIABLES
      REAL*8    REAL8_DECODE
      LOGICAL*2 CFEOF, OKAY
      CHARACTER PERIODS(11)*11, POSSIB(12)*9
      INTEGER*2 I, LENGTH, CFREAD, IDUM, J, IPOS, ITYPE, IPER, TRIMLEN
      INTEGER*2 ITERM, IERR, IL(11), ILIMIT, TEMP, DECIMALTOINT
      REAL*8    RTEMP1, RTEMP2, DEFAULT_INTERVAL, DEFAULT_PM_CONSTRAINT, &
     &          DEFAULT_UT_CONSTRAINT, SEC, TIME_RAD
      INTEGER*4, EXTERNAL :: I_LEN
      CHARACTER TOKEN*128, TEN_TOKENS(10)*20
      DATA TEN_TOKENS  / &
     &    'OFFSET    ', &
     &    'XY        ', &
     &    'RATE      ', &
     &    '--        ', &
     &    'UT1       ', &
     &    'OFFSET    ', &
     &    'U         ', &
     &    'RATE      ', &
     &    'U         ', &
     &    'MIDDLE    ' &
     &                 /
!CCCC
!
! --- This routine was virtually rewritten at the time the 'segmented-offset'
! --- style of hf EOP was introduced. ( So that hfeop could be used in
! --- conjuncition with B3D.)  At that time the syntax for the UT1PM control
! --- line was changed.
!
! --- Historically this routine parsed the UT1PM command line and stored the
! --- UT1PM estimation configuration in the variables:  ut1flg, offlg, ratflg,
! --- reop_flg, ieop_flg, and ieopll.  These variables were passed out of this
! --- routine via  the subroutine agruement list, but ultimately landed in the
! --- label common ba2cm.i. The BATCH routine sflags and ut1pm then uses them
! --- to configure the various flags and arrays in SOCOM.i so that the solution
! --- is done correctly.  In order not to change any of the variables in ba2cm,
! --- the meanings of these variables has be changes somewhat to accomodate
! --- the new EOP configuration.
!
! --- Note that in the old EOP scheme it was possible to configure PM and UT1
! --- differently. Specifically in the hfeop modes the interval could be
! --- different. It was even possible to estimate pm with polynomial
! --- parameterization and UT1 with a segmented parameterizaiton. That is no
! --- longer possible.  Thus, some variables with dimension 2 (such as
! --- EOP_STYLE and NROT_A1) could be simple variables.  That change was not
! --- made, but whereever needed  the (1) position is copied into
! --- the (2) position.
!
!CCCC
!
!     HISTORY
!     WHO  WHEN     WHAT
!     jwr  97.04.15 Almost completely rewritten by jwr for new (April 97)
!                   sytle EOP segentation.
!     pet  97.04.18 Made error messages more comprehensive
!     pet  97.04.21 Added capacity to change order: 'XYU' = 'XUY' etc
!     pet  97.09.02 Added 2ND_ODER keyword support.
!     pet  97.09.03 Added keyword START in POLYNOMIAL case. Added capasity to
!                   put tokens in arbitrary order in POLYNOMIAL case. Petified
!                   comments
!     pet  98.09.23 Added keyword END, NOON in POLYNOMIAL case. Fixed a bug:
!                   previous version estimated polynomial EOP at 12:00 TAI when
!                   MIDNIGHT was speciifed instead of 0 TAI
!     pet  2021.07.11  Added support of keywords BEFORE and AFTER 
!     pet  2021.08.28  Updated initial (default) value of EOP_BEFORE_SEC_TAI and EOP_AFTER_SEC_TAI     
!
!CCCC
      UT1FLG = 'N'
      OFFLG  = 0
      RATFLG = 0
      ACCEOP_FLG = 0
      DO I=1,4
         REOP_FLG(I) = 0.0
      ENDDO
      DO I=1,6
         IEOP_FLG(I) = 0
      ENDDO
      IEOPLL = 0
      DEFAULT_INTERVAL      =  1.D0/24D0 ! One hour in units of days
      DEFAULT_PM_CONSTRAINT = 10.D0      ! mas/day
      DEFAULT_UT_CONSTRAINT =  0.67D0    ! ms/day
      EOP_EPOCH_MJD         =  0
      EOP_BEFORE_SEC_TAI    = -1.0001D30
      EOP_AFTER_SEC_TAI     = -1.0001D30
!
      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
!
      IF ( TOKEN(1:2) .EQ. 'NO' ) THEN
           RETURN
      ENDIF
!
      UT1FLG = 'Y'
!
! --- Check for the grandfathered case for old TRF commmand files.
! --- Only the following strng is supported here.
!
!     'WOBBLE OFFSET XY RATE -- UT1 OFFSET U RATE U MIDDLE'
!
      IF ( TOKEN .EQ. 'WOBBLE' ) THEN ! The grandfather case.
           OKAY = .TRUE.
           I = 1
           DO WHILE ( OKAY .AND.  I .LE. 10 )
              CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
              IF ( TOKEN .EQ. TEN_TOKENS(I) ) THEN ! Looking good
                   I = I+1
                ELSE
                   OKAY = .FALSE.
                   WRITE ( 6, * ) 'BATCH/gut1: unsupported token was met: '// &
     &             TOKEN
              ENDIF
           ENDDO
           IF ( .NOT. OKAY ) STOP &
     &         'BATCH/gut1: grandfather1 (Error parsing old parametrization scheme)'
!
! -------- This is equalivant to 'DEFAULT MIDDLE', so load that in and let the
! -------- rest to the   code do its thing.
!
           STRING = 'MIDDLE'
           TOKEN  = 'DEFAULT'
      ENDIF ! The grandfather case.
!
! --- Look for the default case.
!
      IF ( TOKEN .EQ. 'DEFAULT' ) THEN
!
! -------- turn this into a POLYNOMIAL case and let the logic go.
!
           CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
           STRING = 'OFFSET XYU RATE --U         '
           IF ( TOKEN .EQ. 'MIDDLE'  ) STRING = 'OFFSET XYU RATE --U MIDDLE  '
           IF ( TOKEN .eq. 'MIDNIGHT') STRING = 'OFFSET XYU RATE --U MIDNIGHT'
           TOKEN =  'POLYNOMIAL'
      ENDIF ! turn this into a POLYNOMIAL case and let the logic go.
!
! --- Handle the POLYNOMIAL case.
!
      IF ( TOKEN .eq. 'POLYNOMIAL' ) THEN ! Polynomial style
           IEOP_FLG(1) = 0 ! Set for polynomial style
           EOPMID      = 0 ! EOP epoch at start of session unless MIDNIGHT,
!                          ! MIDDLE, START, END or NOON are specified
 910       CONTINUE
           CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
           IF ( TOKEN .NE. 'OFFSET'          .AND. &
     &          TOKEN .NE. 'RATE'            .AND. &
     &          TOKEN .NE. '2ND_ORDER'       .AND. &
     &          TOKEN .NE. 'MIDDLE'          .AND. &
     &          TOKEN .NE. 'MIDNIGHT'        .AND. &
     &          TOKEN .NE. 'NOON'            .AND. &
     &          TOKEN .NE. 'END'             .AND. &
     &          TOKEN .NE. 'DAYOFTIME_EPOCH' .AND. &
     &          TOKEN .NE. 'EPOCH'           .AND. &
     &          TOKEN .NE. 'BEFORE'          .AND. &
     &          TOKEN .NE. 'AFTER'           .AND. &
     &          TOKEN .NE. ' '                     ) THEN
                WRITE ( 6, '(A)' ) 'Token "'//TOKEN(1:I_LEN(TOKEN))//'" is '// &
     &                 'not legitimate in UT1/PM POLYNOMIAL syntax. '
                WRITE ( 6, '(A)' ) 'One of "OFFSET", "RATE", "2ND_ORDER", '// &
     &               '"MIDDLE", "MIDNIGHT", "START", "END", "NOON", '// &
     &               '"DAYOFTIME_EPOCH", "EPOCH", "BEFORE", "AFTER"'
                WRITE ( 6, '(A)' ) 'tokens was expected'
                STOP 'BATCH/gut1: unrecognized token'
           END IF
           IF ( TOKEN .EQ. 'OFFSET' ) THEN
                CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                IF ( INDEX ( 'XYU-', TOKEN(1:1) ) .LE. 0  .OR. &
     &               INDEX ( 'XYU-', TOKEN(2:2) ) .LE. 0  .OR. &
     &               INDEX ( 'XYU-', TOKEN(3:3) ) .LE. 0       ) THEN
                     WRITE ( 6, '(A)' ) 'Token '//TOKEN(1:I_LEN(TOKEN))// &
     &                     ' is not a legitimate specificator of keyword '// &
     &                     'OFFSET'
                     STOP 'BATCH/gut1: unrecognized token'
                END IF
                IF ( INDEX ( TOKEN, 'X' ) .GT. 0 )  CALL SBIT ( OFFLG, &
     &               INT2(1), INT2(1) )
                IF ( INDEX ( TOKEN, 'Y' ) .GT. 0 )  CALL SBIT ( OFFLG, &
     &               INT2(2), INT2(1) )
                IF ( INDEX ( TOKEN, 'U' ) .GT. 0 )  CALL SBIT ( OFFLG, &
     &               INT2(3), INT2(1) )
             ELSE IF ( TOKEN .EQ. 'RATE' ) THEN
                CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                IF ( INDEX ( 'XYU-', TOKEN(1:1) ) .LE. 0  .OR. &
     &               INDEX ( 'XYU-', TOKEN(2:2) ) .LE. 0  .OR. &
     &               INDEX ( 'XYU-', TOKEN(3:3) ) .LE. 0       ) THEN
                     WRITE ( 6, '(A)' ) 'Token '//TOKEN(1:I_LEN(TOKEN))// &
     &                     ' is not a legitimate specificator of keyword '// &
     &                     'RATE'
                     STOP 'BATCH/gut1: unrecognized token'
                END IF
                IF ( INDEX ( TOKEN, 'X' ) .GT. 0 )  CALL SBIT ( RATFLG, &
     &               INT2(1), INT2(1) )
                IF ( INDEX ( TOKEN, 'Y' ) .GT. 0 )  CALL SBIT ( RATFLG, &
     &               INT2(2), INT2(1) )
                IF ( INDEX ( TOKEN, 'U' ) .GT. 0 )  CALL SBIT ( RATFLG, &
     &               INT2(3), INT2(1) )
!
             ELSE IF ( TOKEN .EQ. '2ND_ORDER' ) THEN
                CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                IF ( INDEX ( 'XYU-', TOKEN(1:1) ) .LE. 0  .OR. &
     &               INDEX ( 'XYU-', TOKEN(2:2) ) .LE. 0  .OR. &
     &               INDEX ( 'XYU-', TOKEN(3:3) ) .LE. 0       ) THEN
                     WRITE ( 6, '(A)' ) 'Token '//TOKEN(1:I_LEN(TOKEN))// &
     &                     ' is not a legitimate specificator of keyword '// &
     &                     '2ND_ORDER'
                     STOP 'BATCH/gut1: unrecognized token'
                END IF
                IF ( INDEX ( TOKEN, 'X' ) .GT. 0 ) CALL SBIT ( ACCEOP_FLG, &
     &               INT2(1), INT2(1) )
                IF ( INDEX ( TOKEN, 'Y' ) .GT. 0 ) CALL SBIT ( ACCEOP_FLG, &
     &               INT2(2), INT2(1) )
                IF ( INDEX ( TOKEN, 'U' ) .GT. 0 ) CALL SBIT ( ACCEOP_FLG, &
     &               INT2(3), INT2(1) )
             ELSE IF ( TOKEN .EQ. ' '         ) THEN
                IF ( OFFLG .EQ. 0 .AND. RATFLG .EQ. 0 .AND. &
     &               ACCEOP_FLG .EQ. 0 ) THEN
                     WRITE ( 6, '(A)' ) 'Neither offset not rate nor second '// &
     &                      'order polynomials for UT1/PM were specified'
                     STOP 'BATCH/gut1: syntax error'
                   ELSE
!
! ------------------ OK. Good bye.
!
                     RETURN
                END IF
             ELSE IF ( TOKEN .EQ. 'START'     ) THEN
                EOPMID = 0 ! EOP epoch at start of session
             ELSE IF ( TOKEN .EQ. 'MIDDLE'    ) THEN
                EOPMID = 1 ! EOP epoch at middle of session
             ELSE IF ( TOKEN .EQ. 'NOON'      ) THEN
                EOPMID = 2 ! EOP epoch at 1st noon after start.
             ELSE IF ( TOKEN .EQ. 'MIDNIGHT'  ) THEN
                EOPMID = 3 ! EOP epoch at 1st midnight after start.
             ELSE IF ( TOKEN .EQ. 'END'       ) THEN
                EOPMID = 4 ! EOP epoch at end of session
             ELSE IF ( TOKEN .EQ. 'DAYOFTIME_EPOCH'       ) THEN
                EOPMID = 5 ! EOP epoch as a day of time
                CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                IER = -1
                CALL HR_TAT ( TOKEN, TIME_RAD, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 1281, -2, '(BATCH)GUT1', 'Error in '// &
     &                   'processing the value of the qualifier '// &
     &                   'DAYOFTIME_EPOCH '//TOKEN(1:I_LEN(TOKEN)) )
                     CALL EXIT ( 1 )
                END IF
                CALL RS_TAT ( TIME_RAD, EOP_EPOCH_SEC )
             ELSE IF ( TOKEN .EQ. 'EPOCH'       ) THEN
                EOPMID = 6 ! EOP epoch at end of session
                CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                IER = -1
                CALL DATE_TO_TIME ( TOKEN, MJD, SEC, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 1282, -2, '(BATCH)GUT1', 'Error in '// &
     &                   'processing the value of the qualifier '// &
     &                   'EPOCH '//TOKEN(1:I_LEN(TOKEN)) )
                     CALL EXIT ( 1 )
                END IF
                EOP_EPOCH_SEC = (MJD - J2000__MJD)*86400.0D0 + &
     &                          (SEC - 43200.0D0)
             ELSE IF ( TOKEN .EQ. 'BEFORE'     ) THEN
                CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                CALL DATE_TO_TIME ( TOKEN, MJD, SEC, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 1283, -2, '(BATCH)GUT1', 'Error in '// &
     &                   'processing the value of the qualifier '// &
     &                   'BEFORE '//TOKEN(1:I_LEN(TOKEN)) )
                     CALL EXIT ( 1 )
                END IF
                EOP_BEFORE_SEC_TAI = (MJD - J2000__MJD)*86400.0D0 + SEC
             ELSE IF ( TOKEN .EQ. 'AFTER'     ) THEN
                CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                CALL DATE_TO_TIME ( TOKEN, MJD, SEC, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 1284, -2, '(BATCH)GUT1', 'Error in '// &
     &                   'processing the value of the qualifier '// &
     &                   'AFTER '//TOKEN(1:I_LEN(TOKEN)) )
                     CALL EXIT ( 1 )
                END IF
                EOP_AFTER_SEC_TAI = (MJD - J2000__MJD)*86400.0D0 + SEC
           END IF
           GOTO 910
         ELSE IF ( TOKEN .EQ. 'SEGMENTS_G.RATE' .OR. &
     &             TOKEN .EQ. 'SEGMENTS_ONLY  ' .OR. &
     &             TOKEN .EQ. 'SINE_STYLE     '       ) THEN
!
! -------- Segmented or sine wave style
!
           IF        ( TOKEN .EQ. 'SEGMENTS_G.RATE') THEN
               IEOP_FLG(1) = 1
             ELSE IF ( TOKEN .EQ. 'SEGMENTS_ONLY  ') THEN
               IEOP_FLG(1) = 2
             ELSE
               IEOP_FLG(1) = 3
           ENDIF
           IEOP_FLG(2) = IEOP_FLG(1)
!
           REOP_FLG(1) = DEFAULT_INTERVAL
           REOP_FLG(3) = DEFAULT_PM_CONSTRAINT
           REOP_FLG(4) = DEFAULT_UT_CONSTRAINT
!
           TOKEN = 'X'
           DO WHILE ( TOKEN .NE. ' ' )
              CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
              IF ( TOKEN .EQ. 'INTERVAL' ) THEN
                   CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                   READ ( TOKEN, *, IOSTAT=IOS ) REOP_FLG(1)
                   IF ( IOS .NE. 0 ) THEN
                        WRITE ( 6, * ) 'BATCH/gut1: Error reading INTERVAL '// &
     &                         'constraint: '//token
                        STOP 'BATCH: Error in parsing batch-file'
                   END IF
                   REOP_FLG(1) = REOP_FLG(1)/(60.D0*24.D0) ! Minutes to days.
                ELSE IF ( TOKEN .EQ. 'PM_RATE_CONSTRAINT' ) THEN
                   CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                   READ ( TOKEN, *, IOSTAT=IOS ) REOP_FLG(3)
                   IF ( IOS.NE.0 ) THEN
                         WRITE ( 6, * ) 'BATCH/gut1: Error reading '// &
     &                          'PM_RATE_CONSTRAINT constraint: '//TOKEN
                         STOP 'BATCH: Error in parsing batch-file'
                   END IF
                ELSE IF ( TOKEN .EQ. 'UT_RATE_CONSTRAINT' ) THEN
                   CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                   READ ( TOKEN, *, IOSTAT=IOS ) REOP_FLG(4)
                   IF ( IOS.NE.0 ) THEN
                        WRITE ( 6, * ) 'BATCH/gut1: Error reading  '// &
     &                         'UT_RATE_CONSTRAINT constraint: '//token
                        STOP 'BATCH: Error in parsing batch-file'
                   END IF
                ELSE IF ( TOKEN .EQ. 'EOP_FILE'          ) THEN
                   CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                   IF ( TOKEN .EQ. 'NONE'   ) IEOPLL = 0
                   IF ( TOKEN .EQ. 'RESET'  ) IEOPLL = 1
                   IF ( TOKEN .EQ. 'APPEND' ) IEOPLL = 2
              ENDIF
           ENDDO
           RETURN
        ELSE
           WRITE ( 6, * ) 'BATCH/gut1: Bad EOP parameterization style '// &
     &            'specified: '//TOKEN
           STOP   'BATCH: Error in parsing batch-file'
      ENDIF
!
      RETURN
      END  !#!  GUT1  #!#
