      SUBROUTINE GCALIB  (IONCTL, IONFLG )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GCALIB PROGRAM SPECIFICATION
!
! 1.1 Parse calibrations section.
!
! 1.2 REFERENCES:
!
! 2.  GCALIB INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) IONCTL,ionflg
!
! IONCTL - Ion control variable (ON, OFF or DEFAULT)
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'solve.i'
      INCLUDE 'calcm.i'
      INCLUDE 'dcali.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: ctrlfl
!       CALLED SUBROUTINES: addstr,gtcalst,newfrm,newsec
!
! 3.  LOCAL VARIABLES
!
      CHARACTER  STRING*256, TOKEN*256, TOKEN_OLD*256
      INTEGER*2  LENGTH, CFREAD, TRIMLEN, IDUM, CNTCAL
      LOGICAL*2  KDEF, CFEOF, KGRP, KION, KAVAL, KUSE, KSET, KIONC, KRES
      INTEGER*4  M_KEE, M_DIS, M_ENB, J1, J2, L_CAL, IUER
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: IFIND_PL, I_LEN
!
      DATA KDEF /.FALSE./, KGRP/.FALSE./, KION/.FALSE./,  KAVAL/.FALSE./
      DATA KUSE /.FALSE./, KSET/.FALSE./, KIONC/.FALSE./, KRES /.FALSE./
!
! 4.  HISTORY
!   WHO   WHEN        WHAT
!   PET   2000.05.12  Added support of the keyword IN.
!                     Added support of post-MAY2000 syntax: new keywords:
!                     KEEP <calibration_name>, ENABLE <calibration_name>,
!                     DISABLE <calibration_name>
!
!   PET   2000.08.03  Fixed the bug: the previous version didn't allow to
!                     specify calibration with blank in the middle of the name
!                     as a value of keywords SET KEPP, SET ENABLE, SET DIABLE
!
! 5.  GCALIB PROGRAM STRUCTURE
!
! Initialize some things
!
      IAVAIL = 0
      ICLUSE = 0
      L_CAL  = 0
      IONCTL = 'DEFAULT'
      IONFLG = 'N'
      M_KEE  = INT4( MAX_CAL )
      M_DIS  = INT4( MAX_CAL )
      M_ENB  = INT4( MAX_CAL )
      L_KEE  = 0
      L_DIS  = 0
      L_ENB  = 0
!
      CNTCAL = 0
      CALL NEWFRM ( CNTCAL )
!
! --- Read the first record in the CALIBRATIONS section
!
      LENGTH=CFREAD(STRING)
      DO WHILE ( STRING(1:1) .EQ.' ' .AND. .NOT.CFEOF(IDUM) )
         DO WHILE ( TRIMLEN(STRING) .GT. 0 )
            CALL SPLITSTRING ( STRING, TOKEN, STRING )
!
            IF ( TOKEN .EQ. 'ION' ) THEN
!
! --------------- Handle ION; must be ON, OFF or DEFAULT
!
                 IF ( KION ) THEN
                      CALL FERR ( INT2(11005), &
     &                    'GCALIB(BATCH) Keword ION used twice', INT2(0), &
     &                     INT2(0) )
                 END IF
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( TOKEN .NE. 'DEFAULT'  .AND.  TOKEN .NE. 'ON'  .AND. &
     &                TOKEN .NE. 'OFF' ) THEN
                      CALL FERR ( INT2(11006), 'GACLIB(BATCH) Illegal '// &
     &                    'qualifier after the keyword ION :'//TOKEN, INT2(0), &
     &                     INT2(0) )
                 ENDIF
                 IONCTL = TOKEN
                 KION = .TRUE.
               ELSE IF ( TOKEN .EQ. 'OLD_IONOSPHERE' ) THEN
!
! -------------- 'OLD_IONOSPHERE' KEYWORD
!
                 CALL FERR ( INT2(4058), &
     &               'GCALIB(BATCH) Keyword OLD_IONOSPHERE '// &
     &               'is not supported any more', INT2(0), INT2(0) )
!                     IF(KIONC) CALL FERR(4050,'ION_CORRECTION USED TWICE',0,0)
!                     CALL SPLITSTRING(STRING,TOKEN,STRING)
!                     IF(TOKEN.EQ.'YES') THEN
!                        IONFLG='Y'
!                      ELSE IF(TOKEN.EQ.'NO') THEN
!                        IONFLG='N'
!                      ELSE
!                        CALL FERR(4052,
!     &                    'ILLEGAL ION_CORRECTION PARAMETER '//TOKEN(1:16),
!     &                     0,0)
!                      ENDIF
!                      KIONC=.TRUE.
!
               ELSE IF ( TOKEN .EQ. 'AVAILABLE' ) THEN
!
! -------------- Handle AVAILABLE keyword
!
                 IF ( KAVAL ) CALL FERR ( INT2(1109), &
     &               'GCALIB(BATCH) Keyword '//'AVALAIBLE used twice', INT2(0), &
     &                INT2(0) )
                 CALL GETAVL ( STRING )
                 KAVAL=.TRUE.
               ELSE IF ( TOKEN .EQ. 'USE' ) THEN
!
! -------------- Handle USE keyword
!
                 CALL GETUSE ( STRING )
                 KUSE =.TRUE.
               ELSE IF ( TOKEN .EQ. 'GROUP' ) THEN
!
! -------------- Handle GROUP keyword
!
                 CALL GETGRP ( CNTCAL, STRING )
                 CALL NEWSEC ( CNTCAL )
                 KGRP=.TRUE.
              ELSE IF ( TOKEN .EQ. 'DEFAULT' .OR. TOKEN .EQ. 'IN' ) THEN
!
! -------------- Handle IN (or DEFAULT) keyword
!
                 IF ( KDEF ) CALL FERR ( INT2(11010), &
     &               'GCALIB(BATCH) Keyword '//'IN or DEFAULT used twice', &
     &                INT2(0), INT2(0) )
                 KDEF=.TRUE.
                 CALL ADDSTR ( CNTCAL, TOKEN )
                 CALL NEWFRM ( CNTCAL )
                 CALL ADDSTR ( CNTCAL, TOKEN )
                 CALL NEWFRM ( CNTCAL )
                 CALL ADDSTR ( CNTCAL, 'ALL     ' )
                 CALL NEWSEC ( CNTCAL )
               ELSE IF ( TOKEN .EQ. 'RESET' ) THEN
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( KRES ) THEN
                      CALL FERR ( INT2(11060), &
     &                    'GCALIB(BATCH) Keyword RESET used '//'twice', INT2(0), &
     &                     INT2(0) )
                      STOP 'GCALIB(BATCH) Abnormal termination'
                 END IF
!
                 IF ( TOKEN .EQ. 'NO' .OR. TOKEN .EQ. 'no' ) THEN
                      FL_RESET = .FALSE.
                    ELSE IF ( TOKEN .EQ. 'YES'  .OR.  TOKEN .EQ. 'yes' ) THEN
                      FL_RESET = .TRUE.
                    ELSE
                      CALL FERR ( INT2(11062), &
     &                    'GCALIB(BATCH) Wrong qualifier '// &
     &                    'after the keyword RESET: '//TOKEN(1:16)// &
     &                    ' one of NO or YES were expected', INT2(0), INT2(0) )
                      STOP 'GCALIB(BATCH) Abnormal termination'
                 END IF
                 KRES = .TRUE.
               ELSE IF ( TOKEN .EQ. 'KEEP' ) THEN
!
! -------------- Keep keyword
!
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( TOKEN .EQ. 'NO' .OR. TOKEN .EQ. 'no' ) THEN
!
! ------------------- NO? Then ignore
!
                      CONTINUE
                    ELSE
                      IUER = -1
                      CALL UNDSCR ( TOKEN )
                      CALL ADD_CLIST ( M_KEE, L_KEE, KEECAL, TOKEN, IUER )
                      IF ( IUER .NE. 0 ) THEN
                           CALL ERR_LOG ( 1801, -3, 'GALIB', &
     &                         'Error in processing keyword KEEP '// &
     &                          TOKEN(1:I_LEN(TOKEN))//' in $CALIBRATIONS section' )
                           STOP 'BATCH(gcalib) Abnormal termination'
                      END IF
                 END IF
               ELSE IF ( TOKEN .EQ. 'ENABLE' ) THEN
!
! -------------- Enable keyword
!
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( TOKEN .EQ. 'NO' .OR. TOKEN .EQ. 'no' ) THEN
!
! ------------------- NO? Then ignore
!
                      CONTINUE
                    ELSE
                      IUER = -1
                      CALL UNDSCR ( TOKEN )
                      CALL ADD_CLIST ( M_ENB, L_ENB, ENBCAL, TOKEN, IUER )
                      IF ( IUER .NE. 0 ) THEN
                           CALL ERR_LOG ( 1802, -3, 'GALIB', &
     &                         'Error in processing keyword ENABLE '// &
     &                          TOKEN(1:I_LEN(TOKEN))//' in $CALIBRATIONS section' )
                           STOP 'BATCH(gcalib) Abnormal termination'
                      END IF
                 END IF
               ELSE IF ( TOKEN .EQ. 'DISABLE' ) THEN
!
! -------------- Enable keyword
!
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( TOKEN .EQ. 'NO' .OR. TOKEN .EQ. 'no' ) THEN
!
! ------------------- NO? Then ignore
!
                      CONTINUE
                    ELSE
                      IUER = -1
                      CALL UNDSCR ( TOKEN )
                      CALL ADD_CLIST ( M_DIS, L_DIS, DISCAL, TOKEN, IUER )
                      IF ( IUER .NE. 0 ) THEN
                           CALL ERR_LOG ( 1803, -3, 'GALIB', &
     &                         'Error in processing keyword DISBLE '// &
     &                          TOKEN(1:I_LEN(TOKEN))//' in $CALIBRATIONS section' )
                           STOP 'BATCH(gcalib) Abnormal termination'
                      END IF
                 END IF
              ELSE
                 CALL ADDSTR ( CNTCAL, TOKEN )
                 CALL NEWFRM ( CNTCAL )
!
                 TOKEN_OLD = TOKEN
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( TOKEN .EQ. ' ' ) THEN
                      CALL FERR ( INT2(11020), 'GCALIB(BATCH) Missing '// &
     &                    'qualifier control after '//TOKEN_OLD(1:16), INT2(0), &
     &                     INT2(0) )
                    ELSE IF ( TOKEN .NE. 'DEFAULT' .AND. &
     &                        TOKEN .NE. 'ON'      .AND. &
     &                        TOKEN .NE. 'OFF'           ) THEN
                      CALL FERR ( INT2(11050), 'GCALIB(BATCH) illegal '// &
     &                    'qualifier control after '//TOKEN_OLD(1:16)// &
     &                    ' one of ON or OFF was expected', INT2(0), INT2(0) )
                 ENDIF
                 CALL ADDSTR  ( CNTCAL, TOKEN )
                 CALL NEWFRM  ( CNTCAL )
                 CALL GTCALST ( CNTCAL, STRING )
                 CALL NEWSEC  ( CNTCAL )
                 L_CAL = L_CAL + 1
                 KSET = .TRUE.
            ENDIF ! token
         ENDDO
         LENGTH = CFREAD ( STRING )
      ENDDO
!
! --- Now that this section is finished, what now?
!
      IF ( KDEF .AND. &
     &     ( KGRP .OR. KION .OR. KAVAL .OR. KUSE .OR. KSET .OR. KIONC ) ) THEN
           CALL FERR ( INT2(11030), &
     &         'GCALIB(BATCH) Keyword IN (or DEFAULT) must '// &
     &         'be the only keyword in the section $CALIBRATONS', INT2(0), &
     &          INT2(0) )
         ELSE IF (      ( KION .OR. KUSE .OR. KIONC ) .AND. &
     &            .NOT. ( KGRP .OR. KDEF .OR. KSET  )       ) THEN
           CALL ADDSTR ( CNTCAL, 'DEFAULT'  )
           CALL NEWFRM ( CNTCAL             )
           CALL ADDSTR ( CNTCAL, 'DEFAULT'  )
           CALL NEWFRM ( CNTCAL             )
           CALL ADDSTR ( CNTCAL, 'ALL     ' )
           CALL NEWSEC ( CNTCAL             )
         ELSE IF ( .NOT. ( KDEF .OR. KGRP .OR. KION .OR. KUSE .OR. KSET .OR. &
     &                     KIONC ) ) THEN
           CALL FERR ( INT2(11040), 'GCALIB(BATCH) no calibrations control', &
     &          INT2(0), INT2(0) )
      ENDIF
      CALL CFUNRD ( LENGTH, STRING )
      IF ( .NOT. KAVAL ) CALL GETAVL ( 'DEFAULT' )
!
      IF ( L_KEE .NE. 0  .OR.  L_ENB .NE. 0  .OR.  L_DIS .NE. 0 ) THEN
           IF ( .NOT. KRES ) THEN
                CALL ERR_LOG ( 1805, -3, 'GCALIB', 'Keyword '// &
     &              'RESET missed in $CALIBRATIONS section' )
                STOP 'GCALIB(BATCH) Abnormal termination'
           END IF
!
! -------- Post MAY-2000 syntax has been applied
!
           IF ( L_KEE .GT. 0 ) THEN
                DO 410 J1=1,L_KEE
                   IF ( IFIND_PL ( L_ENB, ENBCAL, KEECAL(J1) ) .GT. 0 .OR. &
     &                  IFIND_PL ( L_ENB, DISCAL, KEECAL(J1) ) .GT. 0 ) THEN
                        CALL ERR_LOG ( 1806, -3, 'GCALIB','Calibration '// &
     &                       KEECAL(J1)//' was defined in '// &
     &                      'the KEEP keyword and also in ENABLE or DISABLE. '// &
     &                      'Please, specify calibration '//KEECAL(J1)// &
     &                      ' only in one list: KEEP, ENABLE or DISABLE' )
                        STOP 'GCALIB(BATCH) Abnormal termination'
                   END IF
 410            CONTINUE
           END IF
!
           IF ( L_ENB .GT. 0 ) THEN
                DO 420 J2=1,L_ENB
                   IF ( IFIND_PL ( L_DIS, DISCAL, ENBCAL(J2) ) .GT. 0 ) THEN
                        CALL ERR_LOG ( 1807, -3, 'GCALIB','Calibration '// &
     &                       ENBCAL(J2)//' was defined in '// &
     &                      'the ENABLE keyword and also in DISABLE. '// &
     &                      'Please, specify calibration '//ENBCAL(J2)// &
     &                      ' only in one list: ENABLE or DISABLE' )
                        STOP 'GCALIB(BATCH) Abnormal termination'
                   END IF
 420            CONTINUE
           END IF
!
           IF ( L_CAL .GT. 0 ) THEN
                CALL ERR_LOG ( 1808, -3, 'GCALIB', 'Keywords '// &
     &              'ENABLE or DISABLE or KEEP were used together with '// &
     &              '<calibration_name> [ON  or OFF]. This combination '// &
     &              'is not supported. Please, either use KEEP or '// &
     &              'DISABLE/ENABLE or <corfile_display_name [ON or OFF] '// &
     &              'syntax, but not mix them together' )
                STOP 'GCALIB(BATCH) Abnormal termination'
           END IF
      END IF
!
      RETURN
      END  !#!  GCALIB  #!#
