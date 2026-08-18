      SUBROUTINE GSTAVELCNST ( KEYWORD, KCONST, &
     &                         XYZ_CNFL, XYZ_CNST, XYZ_CNSB, &
     &                         UEN_CNFL, UEN_CNST, UEN_CNSB, &
     &                         STASUP_CHR, ISTASP, TOKEN, STRING, IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GSTAVELCNST PROGRAM SPECIFICATION
!
! 1.1 Get VELOCITY info from CONSTRAINTS section of control file.
!
! 1.2 REFERENCES:
!
! 2.  GSTAVELCNST INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER  STRING*(*), KEYWORD*(*), STASUP_CHR(MAX_STA)*8
!
! STASUP - Station components for stations in exception list
! STRING - String to be parsed
!
! 2.3 OUTPUT Variables:
!
      LOGICAL*2  KCONST
      REAL*8     XYZ_CNST(3), UEN_CNST(3)
      INTEGER*2  XYZ_CNSB(STA_BIT_WORDS), UEN_CNSB(STA_BIT_WORDS)
      LOGICAL*2  XYZ_CNFL, UEN_CNFL
      INTEGER*4  IOS, IUER
!
      INTEGER*2 ISTASP
      CHARACTER TOKEN*(*)
!
! ISTASP - Number of stations in exception list
! TOKEN - Individual token from STRING
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: gconst
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2  CFEOF, FL_XYZ, FL_UEN
      REAL*8     VAL
      CHARACTER  BSLASH*1, STR*80, TOKEN_SYS*3, TOKEN_PREV*3
      INTEGER*2  J1, J2, J3, J4, J5, LENGTH, CFREAD, IDUM, IPOS
      PARAMETER  ( BSLASH = '\' )
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: I_LEN
!
! 4.  HISTORY
!   WHO   WHEN    WHAT
!   JLR   921215  replace '\' with BSLASH
!   PET   990312  updated comments. Improved error messages
!   PET   2000.11.24  Totally re-wrote on the basis of the old gvelcnst
!   PET   2001.01.22  corrected a bug related with parsing XYZ NO, UEN NO
!                     clause
!
! 5.  GSTAVELCNST PROGRAM STRUCTURE
!
! Get first token from input string; must be YES, NO or UEN
!
      KCONST = .FALSE.
      DO 410 J1=1,STA_BIT_WORDS
         XYZ_CNSB(J1) = 0
         UEN_CNSB(J1) = 0
 410  CONTINUE
      XYZ_CNST(1) = 0.0D0
      XYZ_CNST(2) = 0.0D0
      XYZ_CNST(3) = 0.0D0
      UEN_CNST(1) = 0.0D0
      UEN_CNST(2) = 0.0D0
      UEN_CNST(3) = 0.0D0
      XYZ_CNFL    = .FALSE.
      UEN_CNFL    = .FALSE.
!
      FL_XYZ = .FALSE.
      FL_UEN = .FALSE.
!
      CALL SPLITSTRING ( STRING, TOKEN, STRING )
      IF ( TOKEN(1:2) .EQ. 'NO' ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
        ELSE IF ( TOKEN(1:1) .EQ. BSLASH ) THEN
           LENGTH = CFREAD ( STRING  )
           CALL SPLITSTRING ( STRING, TOKEN, STRING )
      END IF
      DO 420 J2=1,2
         IF ( TOKEN(1:3) .EQ. 'XYZ' ) THEN
              IF ( FL_XYZ ) THEN
                   CALL ERR_LOG ( 8611, IUER, 'GSTAVELCNST', &
     &                 'Error in parsing keyword '//KEYWORD//' in the '// &
     &                 '$CONSTRAINTS section: qualifier XYZ used twice' )
                   RETURN
              END IF
              FL_XYZ = .TRUE.
            ELSE IF ( TOKEN(1:3) .EQ. 'UEN' ) THEN
              IF ( FL_UEN ) THEN
                   CALL ERR_LOG ( 8612, IUER, 'GSTAVELCNST', &
     &                 'Error in parsing keyword '//KEYWORD//' in the '// &
     &                 '$CONSTRAINTS section: qualifier UEN used twice' )
                   RETURN
              END IF
              FL_UEN = .TRUE.
            ELSE
              CALL INCH ( INT4(J2), STR )
              CALL ERR_LOG ( 8613, IUER, 'GSTAVELCNST', 'Error in '// &
     &            'parsing keyword '//KEYWORD//' in the $CONSTRAINTS '// &
     &            'section during the '//STR(1:I_LEN(STR))//' pass: unknown '// &
     &            'qualifier '//TOKEN(1:I_LEN(TOKEN))//' was found. One of '// &
     &            'UEN or XYZ was expected. Perhaps you are trying to use '// &
     &            'pre 2000.11.22 syntax. Keep in mind that syntax of this '// &
     &            'keyword was changed! Refer to the '// &
     &            '$SOLVE_HELP_DIR/solve_batch_03 documentation relased after '// &
     &            '2000.11.22' )
              RETURN
         END IF
         TOKEN_SYS = TOKEN(1:3)
!
         CALL SPLITSTRING ( STRING, TOKEN, STRING )
         IF ( TOKEN(1:1) .EQ. BSLASH ) THEN
              LENGTH = CFREAD ( STRING )
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
         END IF
!
         IF ( TOKEN(1:3) .EQ. 'YES' ) THEN
              IF ( TOKEN_SYS .EQ. 'XYZ' ) XYZ_CNFL = .TRUE.
              IF ( TOKEN_SYS .EQ. 'UEN' ) UEN_CNFL = .TRUE.
            ELSE IF ( TOKEN(1:2) .EQ. 'NO' ) THEN
              IF ( TOKEN_SYS .EQ. 'XYZ' ) XYZ_CNFL = .FALSE.
              IF ( TOKEN_SYS .EQ. 'UEN' ) UEN_CNFL = .FALSE.
            ELSE
              CALL ERR_LOG ( 8614, IUER, 'GSTAVELCNST', 'Error in '// &
     &            'parsing keyword '//KEYWORD//' in the $CONSTRAINTS '// &
     &            'section: unknown qualifier '//TOKEN(1:I_LEN(TOKEN))// &
     &            ' one of YES or NO was expected' )
              RETURN
         END IF
         TOKEN_PREV = TOKEN(1:3) ! Keep the previous token for better
!                                ! diagnostic
!
         CALL SPLITSTRING ( STRING, TOKEN, STRING )
         IF ( TOKEN(1:1) .EQ. BSLASH ) THEN
!
! ----------- Read the next line
!
              LENGTH = CFREAD ( STRING )
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
         END IF
!
         IF ( TOKEN(1:1) .EQ. ' ' ) THEN
              IF ( TOKEN_PREV .EQ. 'YES' ) THEN
                   CALL ERR_LOG ( 8615, IUER, 'GSTAVELCNST', 'Error in '// &
     &                 'parsing keyword '//KEYWORD//' in the $CONSTRAINTS '// &
     &                 'section: qualifier SIGMA must follow qualifier YES' )
                   RETURN
              END IF
            ELSE
              IF ( TOKEN(1:5) .NE. 'SIGMA' ) THEN
                   IF ( TOKEN .EQ. 'XYZ' .AND. TOKEN_PREV(1:2) .EQ. 'NO' ) THEN
!
! --------------------- NO clause
!
                        GOTO 420
                      ELSE IF ( TOKEN .EQ. 'UEN' .AND. &
     &                          TOKEN_PREV(1:2) .EQ. 'NO' ) THEN
!
! --------------------- NO clause
!
                        GOTO 420
                      ELSE
                        CALL ERR_LOG ( 8616, IUER, 'GSTAVELCNST', &
     &                      'Error in parsing keyword '//KEYWORD//' in the '// &
     &                      '$CONSTRAINTS section: unknown qualifier '// &
     &                       TOKEN(1:I_LEN(TOKEN))//' was found. Qualifier '// &
     &                      'SIGMA was expected' )
                        RETURN
                   END IF
              END IF
!
! ----------- Set flag idincating that sigmas of constrains have been specified
!
              KCONST = .TRUE.
!
              DO 430 J3=1,3
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( TOKEN(1:1) .EQ. BSLASH ) THEN
                      LENGTH = CFREAD ( STRING )
                      CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 END IF
!
                 READ ( TOKEN, *, IOSTAT=IOS ) VAL
                 IF ( IOS .NE. 0 ) THEN
                      CALL ERR_LOG ( 8617, IUER, 'GSTAVELCNST', &
     &                    'Error in parsing keyword '//KEYWORD//' in the '// &
     &                    '$CONSTRAINTS: REAL*8 decoding failure of '// &
     &                     TOKEN(1:I_LEN(TOKEN)) )
                      RETURN
                 END IF
!
! -------------- Transform sigmas from mm/year to m/year
!
                 IF ( KEYWORD .EQ. 'VELOCITIES' ) VAL = VAL/1000.0D0
                 IF ( TOKEN_SYS .EQ. 'XYZ' ) THEN
                      XYZ_CNST(J3) = VAL
                 END IF
                 IF ( TOKEN_SYS .EQ. 'UEN' ) THEN
                      UEN_CNST(J3) = VAL
                 END IF
 430          CONTINUE
         END IF
!
! ------ Get next token; if not EXCEPT, then we're finished
!
         CALL SPLITSTRING ( STRING, TOKEN, STRING )
         IF ( TOKEN(1:1) .EQ. BSLASH ) THEN
              LENGTH = CFREAD ( STRING )
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
         END IF
         IF ( TOKEN .EQ. 'EXCEPT' ) THEN
!
! ----------- Get next token and then loop until no more tokens
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              DO 440 J4=1,MAX_STA
                 IF ( TOKEN(1:1) .EQ. ' '   ) GOTO 840
                 IF ( TOKEN(1:3) .EQ. 'XYZ' ) GOTO 840
                 IF ( TOKEN(1:3) .EQ. 'UEN' ) GOTO 840
!
! -------------- Handle continuation line by reading next line of control file
!
                 IF ( TOKEN .EQ. BSLASH ) THEN
                      LENGTH = CFREAD(STRING)
                      IF ( STRING(1:1).EQ.'$' .OR. CFEOF(IDUM) ) THEN
                           CALL ERR_LOG ( 8618, IUER, 'GSTAVELCNST', &
     &                         'Error in parsing keyword '//KEYWORD// &
     &                         ' in the $CONSTRAINTS: illegal continuation '// &
     &                         'line '//STRING(1:16) )
                           RETURN
                      END IF
                    ELSE
!
! ------------------- Remove undescore from names
!
!@U                      CALL UNDSCR ( TOKEN )
!
! ------------------- Search for this token iin the station list
!
                      IPOS = 0
                      IF ( ISTASP .GT. 0 ) THEN
                           DO 450 J5=1,ISTASP
                              IF ( TOKEN .EQ. STASUP_CHR(J5) ) THEN
                                   IPOS=J5
                              END IF
 450                       CONTINUE
                      END IF
!
                      IF ( IPOS .EQ. 0 ) THEN
                           ISTASP = ISTASP + 1
                           IF ( ISTASP .GT. MAX_STA ) THEN
                                CALL CLRCH ( STR )
                                CALL  INCH ( INT4(MAX_STA), STR )
                                CALL ERR_LOG ( 8619, IUER, 'GSTAVELCNST', &
     &                              'Error in parsing keyword '//KEYWORD// &
     &                              ' in the $CONSTRAINTS section: too many '// &
     &                              'stations in exception list. The '// &
     &                              'STASUP_CHR list length '//STR(1:I_LEN(STR))// &
     &                              ' is exceeded' )
                                RETURN
                            END IF
!
                            STASUP_CHR(ISTASP) = TOKEN
                            IPOS=ISTASP
                      END IF
!
                      IF ( TOKEN_SYS .EQ. 'XYZ' ) THEN
                           CALL SBIT ( XYZ_CNSB(1), IPOS, INT2(1) )
                      END IF
                      IF ( TOKEN_SYS .EQ. 'UEN' ) THEN
                           CALL SBIT ( UEN_CNSB(1), IPOS, INT2(1) )
                      END IF
                 ENDIF
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
  440         CONTINUE ! cycle over tokens
  840         CONTINUE
            ELSE IF ( TOKEN(1:1) .EQ. ' ' ) THEN
              CONTINUE
            ELSE IF ( TOKEN(1:3) .EQ. 'XYZ' ) THEN
              CONTINUE
            ELSE IF ( TOKEN(1:3) .EQ. 'UEN' ) THEN
              CONTINUE
            ELSE
              CALL ERR_LOG ( 8620, IUER, 'GSTAVELCNST', 'Error in '// &
     &            'parsing keyword '//KEYWORD//' in the $CONSTRAINTS '// &
     &            'section: unrecognized token '//TOKEN(1:I_LEN(TOKEN))// &
     &            ' after sigmas of constraints' )
              RETURN
         END IF  ! except
 420  CONTINUE ! 1,2 over XYZ and UEN
!
      IF ( .NOT. FL_XYZ ) THEN
           CALL ERR_LOG ( 8621, IUER, 'GSTAVELCNST', 'Error in '// &
     &         'parsing keyword '//KEYWORD//' in the $CONSTRAINTS '// &
     &         'section: mandatory qualifier XYZ was not found' )
           RETURN
      END IF
!
      IF ( .NOT. FL_UEN ) THEN
           CALL ERR_LOG ( 8622, IUER, 'GSTAVELCNST', 'Error in '// &
     &         'parsing keyword '//KEYWORD//' in the $CONSTRAINTS '// &
     &         'section: mandatory qualifier XYZ was not found' )
           RETURN
      END IF
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!        type *,'GS: keyword =',keyword, ' kconst=',kconst  ! %%%%%%%%%%%%
!        type *,'GS: xyz_cnfl = ',xyz_cnfl, ' uen_cnfl = ',uen_cnfl  ! %%%
!        type *,'GS: xyz_cnst = ',xyz_cnst  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%
!        type *,'GS: uen_cnst = ',uen_cnst  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%
!        type *,'GS: xyz_cnsb = ',xyz_cnsb  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%
!        type *,'GS: uen_cnsb = ',uen_cnsb  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%
!        type *,'GS: -----------'           ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GSTAVELCNST  #!#
