      SUBROUTINE GEXP ( PARAM, COMP_ALL, STRING, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GEXP  parses exception list and fills the fields of       *
! *   common block exceptions.i                                          *
! *   It supports four different keywords: STATIONS, VELOCITIES,         *
! *   SOURCES, PROPER_MOTIONS                                            *
! *                                                                      *
! *  ### 10-AUG-2001       GEXP    v1.2 (c)  L. Petrov  31-DEC-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
      INCLUDE 'exceptions.i'
      CHARACTER PARAM*(*), COMP_ALL*(*), STRING *(*)
!
      INTEGER*4    M_XYZCMP, M_UENCMP, M_RDCMP, M_BWO, IUER
      PARAMETER  ( M_XYZCMP = 8  )
      PARAMETER  ( M_UENCMP = 8  )
      PARAMETER  ( M_RDCMP  = 4  )
      PARAMETER  ( M_BWO    = 20 )
      CHARACTER  XYZCMP_CHR(M_XYZCMP)*8, UENCMP_CHR(M_UENCMP)*8, &
     &           RDCMP_CHR(M_RDCMP)*2, BAD_WORDS(M_BWO)*8, &
     &           STYLE_ALLCMP*1, STYLE_CMP*1, STYLE_OLDCMP*1
      DATA XYZCMP_CHR / 'XYZ', '-YZ', 'X-Z', 'XY-', '--Z', '-Y-', 'X--', '---' /
      DATA UENCMP_CHR / 'UEN', '-EN', 'U-N', 'UE-', '--N', '-E-', 'U--', '---' /
      DATA RDCMP_CHR  / 'RD', 'R-', '-D', '--' /
!
! --- Bad words: they should not appear in exception list.
!
      DATA BAD_WORDS / &
     &                 'STATIONS', &
     &                 'VELOCITI', &
     &                 'ATMOSPHE', &
     &                 'CLOCKS  ', &
     &                 'UT1/PM  ', &
     &                 'NUTATION', &
     &                 'PRECESSI', &
     &                 'GRADIENT', &
     &                 'SOURCES ', &
     &                 'PROPER_M', &
     &                 'HI_FREQ_', &
     &                 'RELATIVI', &
     &                 'AXIS   ', &
     &                 'BASELINE', &
     &                 'EXCEPT  ', &
     &                 'YES     ', &
     &                 'NO      ', &
     &                 'NONE    ', &
     &                 'PICK    ', &
     &                 'FUCK    ' &
     &                /
      CHARACTER  TOKEN*128, CMP_THIS*3, STR*32
      LOGICAL*2  CFEOF, FL_GETNEXT
      INTEGER*2  IDUM_I2
      INTEGER*4  IP_XYZ, IP_UEN, IP_RD, IL_XYZ, IL_UEN, IL_RD, N_OBJ, IB, J1
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: I_LEN, LTM_DIF
!
! --- Try to find COMP_ALL token in complnent lists
!
      IP_XYZ = LTM_DIF ( 0, M_XYZCMP, XYZCMP_CHR, COMP_ALL )
      IP_UEN = LTM_DIF ( 0, M_UENCMP, UENCMP_CHR, COMP_ALL )
!
! --- Determine sytle of station components
!
      STYLE_ALLCMP = ' '
      IF ( IP_XYZ .EQ. M_XYZCMP ) STYLE_ALLCMP = '-'
      IF ( IP_XYZ .GT. 0  .AND.  IP_XYZ .LT. M_XYZCMP ) STYLE_ALLCMP = 'X'
      IF ( IP_UEN .GT. 0  .AND.  IP_UEN .LT. M_XYZCMP ) STYLE_ALLCMP = 'U'
!
      IP_RD  = LTM_DIF ( 0, M_RDCMP, RDCMP_CHR, COMP_ALL )
!
! --- Check whether the first token is a valid station component flag
!
      IF ( PARAM .EQ. 'STATIONS'  .OR.  PARAM .EQ. 'VELOCITIES' ) THEN
           IF ( IP_XYZ .LE. 0  .AND.  IP_UEN .LE. 0 ) THEN
                CALL ERR_LOG ( 8351, IUER, 'GEXP', 'Wrong code for initial '// &
     &              'station component: '//COMP_ALL(1:I_LEN(COMP_ALL))// &
     &              ' in parsing '//PARAM(1:I_LEN(PARAM))//' keyword' )
                RETURN
           END IF
         ELSE IF ( PARAM .EQ. 'SOURCES' .OR. PARAM .EQ. 'PROPER_MOTIONS' ) THEN
           IF ( IP_RD .LE. 0  ) THEN
                CALL ERR_LOG ( 8352, IUER, 'GEXP', 'Wrong code for initial '// &
     &              'source component: '//COMP_ALL(1:I_LEN(COMP_ALL))// &
     &              ' in parsing '//PARAM(1:I_LEN(PARAM))//' keyword' )
                RETURN
           END IF
      END IF
!
      STYLE_OLDCMP = STYLE_ALLCMP
!
! --- Now, cycle over all tokens
!
      N_OBJ = 0
      DO 410 J1=1,MAX(MAX_SRC,MAX_STA)+1
!
! ------ Get new token
!
         CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
         IF ( TOKEN .EQ. ' ' ) THEN
!
! ----------- No tokens any more? We've done.
!
              GOTO 810
           ELSE IF ( TOKEN .EQ. '/' ) THEN
              CALL CFREAD ( STRING )
              IF ( STRING(1:1) .EQ. '$' ) THEN
                   CALL ERR_LOG ( 8353, IUER, 'GEXP', 'Error in parsing '// &
     &                 'keyword '//PARAM(1:I_LEN(PARAM))//': exception list '// &
     &                 'was not completed or extra \' )
                   RETURN
              END IF
!
              IF ( CFEOF(IDUM_I2) ) THEN
                   CALL ERR_LOG ( 8354, IUER, 'GEXP', 'Error in parsing '// &
     &                 'keyword '//PARAM(1:I_LEN(PARAM))//': control file '// &
     &                 'ended, but exception list was not completed. '// &
     &                 'Extra \ ?' )
                   RETURN
              END IF
              CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
         END IF
!
         FL_GETNEXT = .FALSE.
!
! ------ First we try to interpete the token as a components flag
!
         IF ( PARAM .EQ. 'STATIONS'  .OR.  PARAM .EQ. 'VELOCITIES' ) THEN
              IL_XYZ = LTM_DIF ( 0, M_XYZCMP, XYZCMP_CHR, TOKEN )
              IL_UEN = LTM_DIF ( 0, M_UENCMP, UENCMP_CHR, TOKEN )
!
! ----------- Try to determine parameterization style: XYZ or UEN.
! ----------- WE should prevent user's attept to mix them up.
!
              STYLE_CMP = ' '
              IF ( IL_XYZ .EQ. M_XYZCMP ) STYLE_CMP = '-'
              IF ( IL_XYZ .GT. 0  .AND.  IL_XYZ .LT. M_XYZCMP ) STYLE_CMP = 'X'
              IF ( IL_UEN .GT. 0  .AND.  IL_UEN .LT. M_XYZCMP ) STYLE_CMP = 'U'
              IF ( STYLE_CMP .EQ. ' ' ) STYLE_CMP = STYLE_ALLCMP
              IF ( STYLE_ALLCMP .NE. '-'          .AND. &
     &             STYLE_CMP    .NE. STYLE_ALLCMP       ) THEN
                   CALL ERR_LOG ( 8355, IUER, 'GEXP', 'Error in parsing '// &
     &                 'keyword '//PARAM(1:I_LEN(PARAM))//': mixed style of '// &
     &                 'station components -- UEN and XYZ styles cannot be '// &
     &                 'mixed' )
                   RETURN
              END IF
!
              IF ( STYLE_OLDCMP .NE. '-'          .AND. &
     &             STYLE_CMP    .NE. STYLE_OLDCMP       ) THEN
                   CALL ERR_LOG ( 8356, IUER, 'GEXP', 'Error in parsing '// &
     &                 'keyword '//PARAM(1:I_LEN(PARAM))//': mixed style of '// &
     &                 'station components -- UEN and XYZ styles cannot be '// &
     &                 'mixed' )
                   RETURN
              END IF
              STYLE_OLDCMP = STYLE_CMP
!
! ----------- Increment stations counter
!
              N_OBJ = N_OBJ + 1
              IF ( N_OBJ .GT. MAX_STA ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( INT4(MAX_STA), STR )
                   CALL ERR_LOG ( 8357, IUER, 'GEXP', 'Error in parsing '// &
     &                 'keyword '//PARAM(1:I_LEN(PARAM))//': the number of '// &
     &                 'stations exceeded the limit MAX_STA: '//STR )
                   RETURN
              END IF
!
              IF ( IL_XYZ .GT. 0 ) THEN
                   CMP_THIS = XYZCMP_CHR(IL_XYZ)
                   FL_GETNEXT = .TRUE.  ! Flag: we need to read the next token
                 ELSE IF ( IL_UEN .GT. 0 ) THEN
                   CMP_THIS = UENCMP_CHR(IL_UEN)
                   FL_GETNEXT = .TRUE.  ! Flag: we need to read the next token
                 ELSE
                   CMP_THIS = COMP_ALL
              END IF
           ELSE IF ( PARAM .EQ. 'SOURCES' .OR. PARAM .EQ. 'PROPER_MOTIONS' )THEN
              IL_RD = LTM_DIF ( 0, M_RDCMP, RDCMP_CHR, TOKEN )
!
              N_OBJ = N_OBJ + 1
              IF ( N_OBJ .GT. MAX_SRC ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( INT4(MAX_SRC), STR )
                   CALL ERR_LOG ( 8358, IUER, 'GEXP', 'Error in parsing '// &
     &                 'keyword '//PARAM(1:I_LEN(PARAM))//': the number of '// &
     &                 'sources exceeded the limit MAX_SRC: '//STR )
                   RETURN
              END IF
!
              IF ( IL_RD .GT. 0 ) THEN
                   CMP_THIS = RDCMP_CHR(IL_RD)
                   FL_GETNEXT = .TRUE.
                 ELSE
                   CMP_THIS = COMP_ALL
              END IF
         END IF
!
         IF ( FL_GETNEXT ) THEN
              CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
              IF ( TOKEN .EQ. '/' ) THEN
                   CALL CFREAD ( STRING )
                   IF ( STRING(1:1) .EQ. '$' ) THEN
                        CALL ERR_LOG ( 8359, IUER, 'GEXP', 'Error in parsing '// &
     &                      'keyword '//PARAM(1:I_LEN(PARAM))//': exception '// &
     &                      'list was not completed or extra \' )
                       RETURN
                   END IF
!
                   IF ( CFEOF(IDUM_I2) ) THEN
                        CALL ERR_LOG ( 8360, IUER, 'GEXP', 'Error in parsing '// &
     &                      'keyword '//PARAM(1:I_LEN(PARAM))//': control '// &
     &                      'file ended, but exception list was not '// &
     &                      'completed. Extra \ ?' )
                        RETURN
                   END IF
                   CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
              END IF
              IF ( TOKEN .EQ. ' ' ) THEN
                   CALL ERR_LOG ( 8361, IUER, 'GEXP', 'Error in parsing '// &
     &                 'keyword '//PARAM(1:I_LEN(PARAM))//': no object '// &
     &                 'was found after component ' )
                   RETURN
              END IF
         END IF
!
! ------ Search the token among bad words
!
         IB = LTM_DIF ( 0, M_BWO, BAD_WORDS, TOKEN(1:8) )
         IF ( TOKEN(1:1) .EQ. '$'  .OR.  IB .GT. 0 ) THEN
              CALL ERR_LOG ( 8362, IUER, 'GEXP', 'Bad token '// &
     &             TOKEN(1:I_LEN(TOKEN))//' was found in processing '// &
     &            'keyword '//PARAM(1:I_LEN(PARAM))//': it cannot be '// &
     &            'the object name' )
              RETURN
         END IF
!
! ------ Remove underscore in the name
!
!@U         CALL UNDSCR ( TOKEN )
!
! ------ Put object name and the object's components to the appropriate slots
!
         IF ( PARAM .EQ. 'STATIONS' ) THEN
              NUMEXC_STA = N_OBJ
              STA_CMP(NUMEXC_STA) = CMP_THIS
              STA_NAM(NUMEXC_STA) = TOKEN
              STA_CMP_ALL = COMP_ALL
            ELSE IF ( PARAM .EQ. 'VELOCITIES'     ) THEN
              NUMEXC_VEL = N_OBJ
              VEL_CMP(NUMEXC_VEL) = CMP_THIS
              VEL_NAM(NUMEXC_VEL) = TOKEN
              VEL_CMP_ALL = COMP_ALL
            ELSE IF ( PARAM .EQ. 'SOURCES'        ) THEN
              NUMEXC_SOU = N_OBJ
              SOU_CMP(NUMEXC_SOU) = CMP_THIS
              SOU_NAM(NUMEXC_SOU) = TOKEN
              SOU_CMP_ALL = COMP_ALL
            ELSE IF ( PARAM .EQ. 'PROPER_MOTIONS' ) THEN
              NUMEXC_PRO = N_OBJ
              PRO_CMP(NUMEXC_PRO) = CMP_THIS
              PRO_NAM(NUMEXC_PRO) = TOKEN
              PRO_CMP_ALL = COMP_ALL
         END IF
 410  CONTINUE
 810  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GEXP  #!#
