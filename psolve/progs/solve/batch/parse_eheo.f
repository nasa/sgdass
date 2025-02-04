      SUBROUTINE PARSE_EHEO ( STRING, L_EHEO, ADR_EHEO, L_EHEC, ADR_EHEC, &
     &                        MJD_EHEO_REF, TAI_EHEO_REF, ADR_EHES, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PARSE_EHEO parses the portion of the batch control file    *
! *   which is related to HEO keyword. The routine parses the            *
! *   portion of the file according to specifications, allocates         *
! *   memory for EHEO object and loads with the fields determined from   *
! *   parsing the control file.                                          *
! *                                                                      *
! *   PARSE_EHEO supports @-expansion inside the keyword HEO.            *
! *   It reads @-files and inserts their contents into the buffer.       *
! *                                                                      *
! *   The internal buffers are freed after completion HEO.               *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *       L_EHEO ( INTEGER*4 ) -- The size of EHEO object in bytes,      *
! *                               or zero if no Harmonic Earth           *
! *                               Orienatnation parameters are set to be *
! *                               adjusted.                              *
! *     ADR_EHEO ( INTEGER*8 ) -- The address of the data structure      *
! *                               associated with EHEO object.           *
! *                               It conatins parameters related to      *
! *                               estimation of the Harmonic Earth       *
! *                               Orienation parameters.                 *
! *       L_EHEC ( INTEGER*4 ) -- The size of EHEC object in bytes,      *
! *                               or zero if no Harmonic Earth           *
! *                               Orienatnation constraints are set to   *
! *                               be imposed.                            *
! *     ADR_EHEC ( INTEGER*8 ) -- The address of the data structure      *
! *                               associated with EHEC object.           *
! *                               It conatins parameters related to      *
! *                               imposing constaints on the Harmonic    *
! *                               Earth Orienation parameters.           *
! * MJD_EHEO_REF ( INTEGER*4 ) -- Reference modified Julian date for     *
! *                               estimation of the empirical harmonic   *
! *                               Earth orientation model.               *
! * TAI_EHEO_REF ( REAL*8    ) -- Seconds part of the reference modified *
! *                               Julian date for estimation of the      *
! *                               empirical Harmonic Earth Orientation   *
! *                               model.                                 *
! *     ADR_EHES ( INTEGER*8 ) -- The address of the data structure      *
! *                               for reciprocal weights of constraints  *
! *                               imposed on hermonic variations in the  *
! *                               Earth's rotation and its rates.        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   STRING ( CHARACTER ) -- The portion of the line of the control     *
! *                           line which starts from the first           *
! *                           non-whiteblank character following the     *
! *                           keyword SPLINE_POS. STRING is blanked      *
! *                           at the end of PARSE_SPE work.              *
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
! *  ### 26-MAY-2006  PARSE_EHEO  v2.3 (c)  L. Petrov   01-NOV-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      CHARACTER  STRING*(*)
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      ADDRESS__TYPE :: ADR_EHEO, ADR_EHEC, ADR_EHES
      INTEGER*4  L_EHEO, L_EHEC, MJD_EHEO_REF, IUER
      CHARACTER  STR*512, STR1*512, NAME_EXPAND*128, FILOUT*128, &
     &           TOKEN*128, NAME*10, REG*5
      REAL*8     TIM_INT, TAI_EHEO_REF
      INTEGER*4    MBUF, MIND
      PARAMETER  ( MBUF = 4096 )
      PARAMETER  ( MIND =   32 )
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9)//';\' )
      TYPE ( EHEO__TYPE ), ALLOCATABLE :: EHEO(:)
      TYPE ( EHEC__TYPE ), ALLOCATABLE :: EHEC(:)
      TYPE ( EHES__TYPE ), ALLOCATABLE :: EHES(:)
      SAVE   EHEO, EHEC, EHES
      LOGICAL*1  FL_REF
      INTEGER*2  LN
      INTEGER*8        MEM_LEN
      ADDRESS__TYPE :: MEM_ADR
      INTEGER*4  IOS, J1, J2, J3, J4, J5, J6, J7, J8, J9, IDEG, IAT, &
     &           IP, IL, NBUF, LIND, IND(2,MIND), &
     &           LUN, SIZE_EHEO, SIZE_EHEC, SIZE_EHES, IND_EHEO, IER
      REAL*8     SIG
      CHARACTER  NAM_EHEO_CNS(EHEO__NUM_CNS)*20
      LOGICAL*4  LEX, FL_FOUND, FL_EHEO_CNS(EHEO__NUM_CNS)
      DATA       NAM_EHEO_CNS &
     &           / &
     &                'HEO_VAL_E1E2_HAR    ', &
     &                'HEO_VAL_E1E2_CROSS  ', &
     &                'HEO_VAL_E1E2_SHIFT  ', &
     &                'HEO_VAL_E1E2_DRIFT  ', &
     &                'HEO_VAL_E3_HAR      ', &
     &                'HEO_VAL_E3_CROSS    ', &
     &                'HEO_VAL_E3_SHIFT    ', &
     &                'HEO_VAL_E3_DRIFT    ', &
     &                'HEO_ERM_E1E2_HAR    ', &
     &                'HEO_ERM_E1E2_CROSS  ', &
     &                'HEO_ERM_E1E2_SHIFT  ', &
     &                'HEO_ERM_E1E2_DRIFT  ', &
     &                'HEO_ERM_E3_HAR      ', &
     &                'HEO_ERM_E3_CROSS    ', &
     &                'HEO_ERM_E3_SHIFT    ', &
     &                'HEO_ERM_E3_DRIFT    '  &
     &           /
      INTEGER*2, EXTERNAL :: CFREAD
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      CALL CHASHL  ( STRING )
      L_EHEO   = 0
      ADR_EHEO = 0
      L_EHEC   = 0
      ADR_EHEC = 0
      ADR_EHES = 0
      FL_REF   = .FALSE.
      IF ( STRING(1:3) .EQ. 'NO '   .OR. &
     &     STRING(1:5) .EQ. 'NONE '      ) THEN
!
! -------- Nothing to do
!
           CALL SPLITSTRING ( STRING, TOKEN, STRING )
           CALL ERR_LOG ( 0, IUER )
           RETURN
         ELSE IF ( STRING(1:3) .EQ. 'YES' ) THEN
!
! -------- There should be REF_EPOCH <value> pair after YES. Let us check 
! -------- and parse the value
!
           CALL SPLITSTRING ( STRING, TOKEN, STRING )
           CALL SPLITSTRING ( STRING, TOKEN, STRING )
           CALL TRAN ( 11, TOKEN, TOKEN )
           IF ( TOKEN == 'REF_EPOCH' ) THEN
                CALL SPLITSTRING ( STRING, TOKEN, STRING )
                IF ( TOKEN(1:1) == ' '  .OR.  TOKEN(1:1) == '\' ) THEN
                     CALL ERR_LOG ( 8731, IUER, 'PARSE_EHEO', 'Error in '// &
     &                   'parsing keyword HEO in $FLAGS section: '// &
     &                   'value of qualifier REF_EPOCH was not found :-(' ) 
                     RETURN 
                  ELSE
!
! ------------------ Parse the reference date
!
                     CALL ERR_PASS ( IUER, IER )
                     CALL DATE_TO_TIME ( TOKEN, MJD_EHEO_REF, TAI_EHEO_REF, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 8732, IUER, 'PARSE_EHEO', 'Error '// &
     &                        'in parsing keyword HEO in $FLAGS section: '// &
     &                        'failure to decode value of qualifier '// &
     &                        'REF_EPOCH ' )
                          RETURN 
                     END IF
                     FL_REF = .TRUE.
                END IF
              ELSE 
                CALL ERR_LOG ( 8733, IUER, 'PARSE_EHEO', 'Error in parsing '// &
     &              'keyword EHEO in $FLAGS section: qualifier REF_EPOCH '// &
     &              'should follow value YES, but it was not found :-(' )
                RETURN 
           END IF
!
! -------- Read the next line from the control file
!
           LN = CFREAD ( STRING )
      END IF
!
      IF ( .NOT. FL_REF ) THEN
           CALL ERR_LOG ( 8733, IUER, 'PARSE_EHEO', 'HEO reference epoch was '// &
     &         'not specified. Keyword REF_EPOCH should follow HEO YES' )
           RETURN 
      END IF
!
! --- Allocate the buffer for the portion of the control file
!
      ALLOCATE ( BUF(MBUF), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL IINCH ( IOS, STR )
           CALL ERR_LOG ( 8734, IUER, 'PARSE_EHEO', 'Error in an attempt to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes' )
           RETURN
      END IF
!
! --- Allocate EHEO object
!
      ALLOCATE ( EHEO(M__EHEO), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 8735, IUER, 'PARSE_EHEO', 'Failure to '// &
     &         'allocate in memory object EHEO' )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
! --- Allocate EHEC object
!
      ALLOCATE ( EHEC(M__EHEO), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 8736, IUER, 'PARSE_EHEO', 'Failure to '// &
     &         'allocate in memory object EHEC' )
           DEALLOCATE ( BUF )
           DEALLOCATE ( EHEO )
           RETURN
      END IF
!
! --- Allocate EHES object
!
      ALLOCATE ( EHES(1), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 8737, IUER, 'PARSE_EHEO', 'Failure to '// &
     &         'allocate in memory object EHES' )
           DEALLOCATE ( BUF )
           DEALLOCATE ( EHEO )
           DEALLOCATE ( EHEC )
           RETURN
      END IF
!
! --- Initialization
!
      CALL NOUT ( SIZEOF(EHEO(1)), EHEO )
      CALL NOUT ( SIZEOF(EHES(1)), EHES )
!
      NBUF = 1
      BUF(NBUF) = STRING
      IF ( BUF(NBUF)(I_LEN(BUF(NBUF)):I_LEN(BUF(NBUF))) .NE. '\' ) GOTO 810
!
! --- Read the control file line by line till we end of continuation. Expand
! --- @-names and read their context into the buffer
!
      DO 410 J1=2,MBUF
!
! ------ Read the next line from the control file
!
         LN = CFREAD ( STRING )
         IF ( ILEN(STRING) ==  0  ) GOTO 410
         IF ( STRING(1:1)  == '*' ) GOTO 410
         NBUF = NBUF + 1
         BUF(NBUF) = STRING
         IF ( STRING(ILEN(STRING):ILEN(STRING)) .NE.  '\' ) THEN
!
! ----------- Aga. The last character is not continuation. This means that
! ----------- we reached the end of the sequence of continuation lines. Good!
!
              GOTO 810
         END IF
         IAT = INDEX ( BUF(NBUF), '@' )
         IF ( IAT .GT. 0 ) THEN
!
! ----------- Extract the file name and expand the buffer
!
              NAME_EXPAND = BUF(NBUF)(IAT+1:)
              IP = INDEX ( NAME_EXPAND, ' ' )
              CALL CLRCH ( NAME_EXPAND(IP:) )
!
! ----------- Read the file into the buffer
!
              CALL ERR_PASS ( IUER, IER )
              CALL BUFFER_EXPAND ( MBUF, NBUF, BUF, IAT, NAME_EXPAND, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8738, IUER, 'PARSE_EHEO', 'Failure to'// &
     &                 ' expand line @'//NAME_EXPAND(1:I_LEN(NAME_EXPAND))// &
     &                 ' in processing keyword HEO' )
                   DEALLOCATE ( BUF )
                   DEALLOCATE ( EHEO )
                   DEALLOCATE ( EHEC )
                   DEALLOCATE ( EHES )
                   RETURN
              END IF
         END IF
 410  CONTINUE
 810  CONTINUE
!
      DO 420 J2=1,EHEO__NUM_CNS
         FL_EHEO_CNS = .FALSE.
 420  CONTINUE 
!
      CALL CLRCH ( STRING )
!
      DO 430 J3=1,NBUF
         IL = I_LEN(BUF(J3))
         IF ( BUF(J3)(IL:IL) .EQ. '\' ) THEN
              CALL CLRCH ( BUF(J3)(IL:IL) )
         END IF
         IL = ILEN(BUF(J3))
         IF ( IL .LE. 0 ) GOTO 430
!
! ------ Split the command into words
!
         CALL EXWORD ( BUF(J3), MIND, LIND, IND, REG, -3 )
         IF ( BUF(J3)(IND(1,1):IND(1,1)+1) == 'W '  .AND.  LIND .NE. 13 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( LIND, STR )
              CALL ERR_LOG ( 8739, IUER, 'PARSE_EHEO', 'Wrong number of '// &
     &            'words in line "'//BUF(J3)(1:I_LEN(BUF(J3)))//'" when '// &
     &            'the value HEO keyword in $FLAGS section was parsed: '// &
     &             STR(1:I_LEN(STR))//' while 13 was expected' )
              DEALLOCATE ( BUF )
              DEALLOCATE ( EHEO )
              DEALLOCATE ( EHEC )
              DEALLOCATE ( EHES )
              RETURN 
           ELSE IF ( BUF(J3)(IND(1,1):IND(1,1)+1) == 'C '  .AND.  &
     &               LIND .NE. 7 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( LIND, STR )
              CALL ERR_LOG ( 8740, IUER, 'PARSE_EHEO', 'Wrong number of '// &
     &            'words in line "'//BUF(J3)(1:I_LEN(BUF(J3)))//'" when '// &
     &            'the value HEO keyword in $FLAGS section was parsed: '// &
     &             STR(1:I_LEN(STR))//' while 7 was expected' )
              DEALLOCATE ( BUF )
              DEALLOCATE ( EHEO )
              DEALLOCATE ( EHEC )
              DEALLOCATE ( EHES )
              RETURN 
           ELSE IF ( BUF(J3)(IND(1,1):IND(1,1)+1) == 'A '  .AND.  &
     &               LIND .NE. 4 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( LIND, STR )
              CALL ERR_LOG ( 8741, IUER, 'PARSE_EHEO', 'Wrong number of '// &
     &            'words in line "'//BUF(J3)(1:I_LEN(BUF(J3)))//'" when '// &
     &            'the value HEO keyword in $FLAGS section was parsed: '// &
     &             STR(1:I_LEN(STR))//' while 4 was expected' )
              DEALLOCATE ( BUF )
              DEALLOCATE ( EHEO )
              DEALLOCATE ( EHEC )
              DEALLOCATE ( EHES )
              RETURN 
           ELSE IF ( BUF(J3)(IND(1,1):IND(1,1)+1) == 'U '  .AND.  &
     &               LIND .NE. 7 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( LIND, STR )
              CALL ERR_LOG ( 8742, IUER, 'PARSE_EHEO', 'Wrong number of '// &
     &            'words in line "'//BUF(J3)(1:I_LEN(BUF(J3)))//'" when '// &
     &            'the value HEO keyword in $FLAGS section was parsed: '// &
     &             STR(1:I_LEN(STR))//' while 7 was expected' )
              DEALLOCATE ( BUF )
              DEALLOCATE ( EHEO )
              DEALLOCATE ( EHEC )
              DEALLOCATE ( EHES )
              RETURN 
           ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'CNS'  .AND.  &
     &               LIND .NE. 4 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( LIND, STR )
              CALL ERR_LOG ( 8743, IUER, 'PARSE_EHEO', 'Wrong number of '// &
     &            'words in line "'//BUF(J3)(1:I_LEN(BUF(J3)))//'" when '// &
     &            'the value HEO keyword in $FLAGS section was parsed: '// &
     &             STR(1:I_LEN(STR))//' while 4 was expected' )
              DEALLOCATE ( BUF )
              DEALLOCATE ( EHEO )
              DEALLOCATE ( EHEC )
              DEALLOCATE ( EHES )
              RETURN 
         END IF
!
         IF ( BUF(J3)(IND(1,1):IND(1,1)+1) == 'W ' ) THEN
!
! --------- Get the name of the component
!
            IF ( L_EHEO > 0 ) THEN
                 CALL CLRCH ( NAME )
                 NAME = BUF(J3)(IND(1,2):IND(2,2))
                 DO 440 J4=1,L_EHEO
                    IF ( NAME == EHEO(J4)%NAME(1:10) ) THEN
                         CALL CLRCH ( STR  )
                         CALL CLRCH ( STR1 )
                         CALL INCH  ( J4, STR )
                         CALL INCH  ( L_EHEO+1, STR1 )
                         CALL ERR_LOG ( 8744, IUER, 'PARSE_EHEO', 'Error in '// &
     &                       'processing HEO keyword: the name of the '// &
     &                       'constituent, '//BUF(J3)(IND(1,2):IND(1,2)+9)// &
     &                       ' was found more than once: as a name of '// &
     &                       'constituent '//STR(1:I_LEN(STR))//' and a '// &
     &                       'name of constituent '//STR1 )
                         RETURN 
                    END IF
 440             CONTINUE 
            END IF
!
            L_EHEO = L_EHEO + 1
            IF ( L_EHEO > M__EHEO ) THEN
                 CALL CLRCH   ( STR )
                 CALL INCH    ( M__EHEO, STR )
                 CALL ERR_LOG ( 8745, IUER, 'PARSE_EHEO', 'Too many EHEO '// &
     &               'parameters: more than teh limit M__EHEO: '//STR )
                 DEALLOCATE ( BUF )
                 DEALLOCATE ( EHEO )
                 DEALLOCATE ( EHEC )
                 DEALLOCATE ( EHES )
                 RETURN 
            END IF
!
            CALL NOUT ( SIZEOF(EHEO(L_EHEO)), EHEO(L_EHEO) )
            CALL CLRCH ( EHEO(L_EHEO)%NAME ) 
            EHEO(L_EHEO)%NAME = BUF(J3)(IND(1,2):IND(2,2)) 
!
! --------- Get the phase of the component
!
            READ ( UNIT=BUF(J3)(IND(1,3):IND(2,3)), FMT='(D22.12)', IOSTAT=IER ) &
     &             EHEO(L_EHEO)%PHAS
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8746, IUER, 'PARSE_EHEO', 'Error in '// &
     &               'decoding phase of the HEO wave '//EHEO(L_EHEO)%NAME// &
     &               'in line "'//BUF(J3)(1:I_LEN(BUF(J3)))//'" when '// &
     &               'the value HEO keyword in $FLAGS section was parsed' )
                 DEALLOCATE ( BUF )
                 DEALLOCATE ( EHEO )
                 DEALLOCATE ( EHEC )
                 DEALLOCATE ( EHES )
                 RETURN 
            END IF
!
! --------- Get the frequency of the component
!
            READ ( UNIT=BUF(J3)(IND(1,4):IND(2,4)), FMT='(D22.12)', IOSTAT=IER ) &
     &             EHEO(L_EHEO)%FREQ
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8747, IUER, 'PARSE_EHEO', 'Error in decoding '// &
     &               'frequency of the HEO wave '//EHEO(L_EHEO)%NAME// &
     &               'in line "'//BUF(J3)(1:I_LEN(BUF(J3)))//'" when '// &
     &               'the value HEO keyword in $FLAGS section was parsed' )
                 DEALLOCATE ( BUF )
                 DEALLOCATE ( EHEO )
                 DEALLOCATE ( EHEC )
                 DEALLOCATE ( EHES )
                 RETURN 
            END IF
!
! --------- Get the acceleration of the component
!
            READ ( UNIT=BUF(J3)(IND(1,5):IND(2,5)), FMT='(D22.12)', IOSTAT=IER ) &
     &             EHEO(L_EHEO)%ACCL
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8748, IUER, 'PARSE_EHEO', 'Error in decoding '// &
     &               'acceleration of the HEO wave '//EHEO(L_EHEO)%NAME// &
     &               'in line "'//BUF(J3)(1:I_LEN(BUF(J3)))//'" when '// &
     &               'the value HEO keyword in $FLAGS section was parsed' )
                 DEALLOCATE ( BUF )
                 DEALLOCATE ( EHEO )
                 DEALLOCATE ( EHEC )
                 DEALLOCATE ( EHES )
                 RETURN 
            END IF
!
! --------- Estimation status for E1/E2 components for HEO amplitudes
!
            IF ( BUF(J3)(IND(1,6):IND(2,6)) == 'T' ) THEN
                 EHEO(L_EHEO)%FL_EST(HEO__E1E2) = .TRUE.
               ELSE IF ( BUF(J3)(IND(1,6):IND(2,6)) == 'F' ) THEN
                 EHEO(L_EHEO)%FL_EST(HEO__E1E2) = .FALSE.
               ELSE 
                 CALL ERR_LOG ( 8749, IUER, 'PARSE_EHEO', 'Error in decoding '// &
     &               'estimation status of E12 components of the HEO wave '// &
     &                EHEO(L_EHEO)%NAME//'in line "'// &
     &                BUF(J3)(1:I_LEN(BUF(J3)))//'" when the value HEO '// &
     &               'keyword in $FLAGS section was parsed: only T or F are '// &
     &               'allowed' )
                 DEALLOCATE ( BUF )
                 DEALLOCATE ( EHEO )
                 DEALLOCATE ( EHEC )
                 DEALLOCATE ( EHES )
                 RETURN 
            END IF
!
! --------- Estimation status for E3 components for HEO amplitudes
!
            IF ( BUF(J3)(IND(1,7):IND(2,7)) == 'T' ) THEN
                 EHEO(L_EHEO)%FL_EST(HEO__E3) = .TRUE.
               ELSE IF ( BUF(J3)(IND(1,7):IND(2,7)) == 'F' ) THEN
                 EHEO(L_EHEO)%FL_EST(HEO__E3) = .FALSE.
               ELSE 
                 CALL ERR_LOG ( 8750, IUER, 'PARSE_EHEO', 'Error in decoding '// &
     &               'estimation status of E3 components of the HEO wave '// &
     &                EHEO(L_EHEO)%NAME//'in line "'// &
     &                BUF(J3)(1:I_LEN(BUF(J3)))//'" when the value HEO '// &
     &               'keyword in $FLAGS section was parsed: only T or F are '// &
     &               'allowed' )
                 DEALLOCATE ( BUF )
                 DEALLOCATE ( EHEO )
                 DEALLOCATE ( EHEC )
                 DEALLOCATE ( EHES )
                 RETURN 
            END IF
!
! --------- Constraints status for E1/E2 components for HEO amplitudes
!
            IF ( BUF(J3)(IND(1,8):IND(2,8)) == 'T' ) THEN
                 EHEO(L_EHEO)%FL_CNS(HEO__E1E2) = .TRUE.
               ELSE IF ( BUF(J3)(IND(1,8):IND(2,8)) == 'F' ) THEN
                 EHEO(L_EHEO)%FL_CNS(HEO__E1E2) = .FALSE.
               ELSE 
                 CALL ERR_LOG ( 8751, IUER, 'PARSE_EHEO', 'Error in decoding '// &
     &               'constraint status of E12 components of the HEO wave '// &
     &                EHEO(L_EHEO)%NAME//'in line "'// &
     &                BUF(J3)(1:I_LEN(BUF(J3)))//'" when the value HEO '// &
     &               'keyword in $FLAGS section was parsed: only T or F are '// &
     &               'allowed' )
                 DEALLOCATE ( BUF  )
                 DEALLOCATE ( EHEO )
                 DEALLOCATE ( EHEC )
                 DEALLOCATE ( EHES )
                 RETURN 
            END IF
!
! --------- Constraints status for E3 components for HEO amplitudes
!
            IF ( BUF(J3)(IND(1,9):IND(2,9)) == 'T' ) THEN
                 EHEO(L_EHEO)%FL_CNS(HEO__E3) = .TRUE.
               ELSE IF ( BUF(J3)(IND(1,9):IND(2,9)) == 'F' ) THEN
                 EHEO(L_EHEO)%FL_CNS(HEO__E3) = .FALSE.
               ELSE 
                 CALL ERR_LOG ( 8752, IUER, 'PARSE_EHEO', 'Error in decoding '// &
     &               'estimation status of E3 components of the HEO wave '// &
     &                EHEO(L_EHEO)%NAME//'in line "'// &
     &                BUF(J3)(1:I_LEN(BUF(J3)))//'" when the value HEO '// &
     &               'keyword in $FLAGS section was parsed: only T or F are '// &
     &               'allowed' )
                 DEALLOCATE ( BUF  )
                 DEALLOCATE ( EHEO )
                 DEALLOCATE ( EHEC )
                 DEALLOCATE ( EHES )
                 RETURN 
            END IF
!
! --------- Estimation status for E1/E2 components for HEO rate of changes
!
            IF ( BUF(J3)(IND(1,10):IND(2,10)) == 'T' ) THEN
                 EHEO(L_EHEO)%FL_EST_VEL(HEO__E1E2) = .TRUE.
               ELSE IF ( BUF(J3)(IND(1,10):IND(2,10)) == 'F' ) THEN
                 EHEO(L_EHEO)%FL_EST_VEL(HEO__E1E2) = .FALSE.
               ELSE 
                 CALL ERR_LOG ( 8753, IUER, 'PARSE_EHEO', 'Error in decoding '// &
     &               'estimation status of E12 rate components of the HEO '// &
     &               'wave '//EHEO(L_EHEO)%NAME//'in line "'// &
     &                BUF(J3)(1:I_LEN(BUF(J3)))//'" when the value HEO '// &
     &               'keyword in $FLAGS section was parsed: only T or F are '// &
     &               'allowed' )
                 DEALLOCATE ( BUF  )
                 DEALLOCATE ( EHEO )
                 DEALLOCATE ( EHEC )
                 DEALLOCATE ( EHES )
                 RETURN 
            END IF
!
! --------- Estimation status for E3 components for HEO rate of changes
!
            IF ( BUF(J3)(IND(1,11):IND(2,11)) == 'T' ) THEN
                 EHEO(L_EHEO)%FL_EST_VEL(HEO__E3) = .TRUE.
               ELSE IF ( BUF(J3)(IND(1,11):IND(2,11)) == 'F' ) THEN
                 EHEO(L_EHEO)%FL_EST_VEL(HEO__E3) = .FALSE.
               ELSE 
                 CALL ERR_LOG ( 8754, IUER, 'PARSE_EHEO', 'Error in decoding '// &
     &               'estimation status of E3 rate components of the HEO wave '// &
     &                EHEO(L_EHEO)%NAME//'in line "'// &
     &                BUF(J3)(1:I_LEN(BUF(J3)))//'" when the value HEO '// &
     &               'keyword in $FLAGS section was parsed: only T or F are '// &
     &               'allowed' )
                 DEALLOCATE ( BUF  )
                 DEALLOCATE ( EHEO )
                 DEALLOCATE ( EHEC )
                 DEALLOCATE ( EHES )
                 RETURN 
            END IF
!
! --------- Constraints status for E1/E2 components for HEO rate of changes
!
            IF ( BUF(J3)(IND(1,12):IND(2,12)) == 'T' ) THEN
                 EHEO(L_EHEO)%FL_CNS_VEL(HEO__E1E2) = .TRUE.
               ELSE IF ( BUF(J3)(IND(1,12):IND(2,12)) == 'F' ) THEN
                 EHEO(L_EHEO)%FL_CNS_VEL(HEO__E1E2) = .FALSE.
               ELSE 
                 CALL ERR_LOG ( 8755, IUER, 'PARSE_EHEO', 'Error in decoding '// &
     &               'constraint status of E12 rate components of the HEO wave '// &
     &                EHEO(L_EHEO)%NAME//'in line "'// &
     &                BUF(J3)(1:I_LEN(BUF(J3)))//'" when the value HEO '// &
     &               'keyword in $FLAGS section was parsed: only T or F are '// &
     &               'allowed' )
                 DEALLOCATE ( BUF  )
                 DEALLOCATE ( EHEO )
                 DEALLOCATE ( EHEC )
                 DEALLOCATE ( EHES )
                 RETURN 
            END IF
!
! --------- Constraints status for E3 components for HEO rate of changes
!
            IF ( BUF(J3)(IND(1,13):IND(2,13)) == 'T' ) THEN
                 EHEO(L_EHEO)%FL_CNS_VEL(HEO__E3) = .TRUE.
               ELSE IF ( BUF(J3)(IND(1,13):IND(2,13)) == 'F' ) THEN
                 EHEO(L_EHEO)%FL_CNS_VEL(HEO__E3) = .FALSE.
               ELSE 
                 CALL ERR_LOG ( 8756, IUER, 'PARSE_EHEO', 'Error in decoding '// &
     &               'estimation status of E3 rate components of the HEO wave '// &
     &                EHEO(L_EHEO)%NAME//'in line "'// &
     &                BUF(J3)(1:I_LEN(BUF(J3)))//'" when the value HEO '// &
     &               'keyword in $FLAGS section was parsed: only T or F are '// &
     &               'allowed' )
                 DEALLOCATE ( BUF  )
                 DEALLOCATE ( EHEO )
                 DEALLOCATE ( EHEC )
                 DEALLOCATE ( EHES )
                 RETURN 
            END IF
!
            IF ( DABS(EHEO(L_EHEO)%FREQ) < EHEO__FRQ_MIN .AND. &
     &           EHEO(L_EHEO)%FL_EST_VEL(HEO__E3)              ) THEN
                 CALL ERR_LOG ( 8757, IUER, 'PARSE_EHEO', 'Trap of internal '// &
     &               'control in parsing the estimation status of the E3 '// &
     &               'rate component of the HEO wave '// &
     &                EHEO(L_EHEO)%NAME//'in line "'// &
     &                BUF(J3)(1:I_LEN(BUF(J3)))//'" -- estimatation of the '// &
     &               'rate of change for the component with the zero '// &
     &               'frequency is not supported' )
                 DEALLOCATE ( BUF  )
                 DEALLOCATE ( EHEO )
                 DEALLOCATE ( EHEC )
                 DEALLOCATE ( EHES )
                 RETURN 
            END IF
          ELSE IF ( BUF(J3)(IND(1,1):IND(1,1)+1) == 'C ' .OR. &
     &              BUF(J3)(IND(1,1):IND(1,1)+1) == 'U '      ) THEN
            L_EHEC = L_EHEC + 1
            IF ( L_EHEC > M__EHEO ) THEN
                 CALL CLRCH   ( STR )
                 CALL INCH    ( M__EHEO, STR )
                 CALL ERR_LOG ( 8758, IUER, 'PARSE_EHEO', 'Too many EHEC '// &
     &               'parameters: more than teh limit M__EHEO: '//STR )
                 DEALLOCATE ( BUF  )
                 DEALLOCATE ( EHEO )
                 DEALLOCATE ( EHEC )
                 DEALLOCATE ( EHES )
                 RETURN 
            END IF
!
            CALL NOUT ( SIZEOF(EHEC(L_EHEC)), EHEC(L_EHEC) )
!
            IF ( BUF(J3)(IND(1,1):IND(1,1)+1) == 'C ' ) THEN
                 EHEC(L_EHEC)%HEO_TYPE = HEO__E1E2 
               ELSE 
                 EHEC(L_EHEC)%HEO_TYPE = HEO__E3   
            END IF
            CALL CLRCH ( NAME )
            NAME = BUF(J3)(IND(1,2):IND(2,2))
            EHEC(L_EHEC)%IND(1) = 0
            DO 450 J5=1,L_EHEO
               IF ( NAME == EHEO(J5)%NAME(1:10) ) THEN
                    EHEC(L_EHEC)%IND(1) = J5
               END IF
 450        CONTINUE 
            IF ( EHEC(L_EHEC)%IND(1) == 0  ) THEN
                 CALL ERR_LOG ( 8759, IUER, 'PARSE_EHEO', 'Error in '// &
     &               'processing HEO keyword: the name of the '// &
     &               'constituent, '//NAME//' was not previously defined' )
                 RETURN 
            END IF
!
            READ ( UNIT=BUF(J3)(IND(1,3):IND(2,3)), FMT='(D22.12)', IOSTAT=IER ) &
     &             EHEC(L_EHEC)%AMP(HEO__COS,1)
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8760, IUER, 'PARSE_EHEO', 'Error in decoding '// &
     &               'cosine amplitude of the first constituents for '// &
     &               'constaints in line "'//BUF(J3)(1:I_LEN(BUF(J3)))// &
     &               '" when the value HEO keyword in $FLAGS section was '// &
     &               'parsed' )
                 DEALLOCATE ( BUF  )
                 DEALLOCATE ( EHEO )
                 DEALLOCATE ( EHEC )
                 DEALLOCATE ( EHES )
                 RETURN 
            END IF
!
            READ ( UNIT=BUF(J3)(IND(1,4):IND(2,4)), FMT='(D22.12)', IOSTAT=IER ) &
     &             EHEC(L_EHEC)%AMP(HEO__SIN,1)
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8761, IUER, 'PARSE_EHEO', 'Error in decoding '// &
     &               'sine amplitude of the first constituents for '// &
     &               'constaints in line "'//BUF(J3)(1:I_LEN(BUF(J3)))// &
     &               '" when the value HEO keyword in $FLAGS section was '// &
     &               'parsed' )
                 DEALLOCATE ( BUF  )
                 DEALLOCATE ( EHEO )
                 DEALLOCATE ( EHEC )
                 DEALLOCATE ( EHES )
                 RETURN 
            END IF
!
            CALL CLRCH ( NAME )
            NAME = BUF(J3)(IND(1,5):IND(2,5))
            EHEC(L_EHEC)%IND(2) = 0
            DO 460 J6=1,L_EHEO
               IF ( NAME == EHEO(J6)%NAME(1:10) ) THEN
                    EHEC(L_EHEC)%IND(2) = J6
               END IF
 460        CONTINUE 
            IF ( EHEC(L_EHEC)%IND(2) == 0  ) THEN
                 CALL ERR_LOG ( 8762, IUER, 'PARSE_EHEO', 'Error in '// &
     &               'processing HEO keyword: the name of the '// &
     &               'constituent, '//NAME//', was not previously defined' )
                 RETURN 
            END IF
!
            READ ( UNIT=BUF(J3)(IND(1,6):IND(2,6)), FMT='(D22.12)', IOSTAT=IER ) &
     &             EHEC(L_EHEC)%AMP(HEO__COS,2)
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8763, IUER, 'PARSE_EHEO', 'Error in decoding '// &
     &               'cosine amplitude of the second constituents for '// &
     &               'constaints in line "'//BUF(J3)(1:I_LEN(BUF(J3)))// &
     &               '" when the value HEO keyword in $FLAGS section was '// &
     &               'parsed' )
                 DEALLOCATE ( BUF  )
                 DEALLOCATE ( EHEO )
                 DEALLOCATE ( EHEC )
                 DEALLOCATE ( EHES )
                 RETURN 
            END IF
!
            READ ( UNIT=BUF(J3)(IND(1,7):IND(2,7)), FMT='(D22.12)', IOSTAT=IER ) &
     &             EHEC(L_EHEC)%AMP(HEO__SIN,2)
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8764, IUER, 'PARSE_EHEO', 'Error in decoding '// &
     &               'sine amplitude of the second constituents for '// &
     &               'constaints in line "'//BUF(J3)(1:I_LEN(BUF(J3)))// &
     &               '" when the value HEO keyword in $FLAGS section was '// &
     &               'parsed' )
                 DEALLOCATE ( BUF  )
                 DEALLOCATE ( EHEO )
                 DEALLOCATE ( EHEC )
                 DEALLOCATE ( EHES )
                 RETURN 
            END IF
           ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'CNS' .AND. LIND .GE. 4 ) THEN
            FL_FOUND = .FALSE.
            DO 470 J7=1,EHEO__NUM_CNS
               IF ( INDEX ( NAM_EHEO_CNS(J7), BUF(J3)(IND(1,2):IND(2,2)) ) > 0 ) THEN
                    FL_FOUND = .TRUE.
                    IF ( BUF(J3)(IND(1,3):IND(2,3)) .NE. 'SIGMA' ) THEN
                         CALL ERR_LOG ( 8765, IUER, 'PARSE_EHEO', 'Error in '// &
     &                       'decoding line "'//BUF(J3)(1:I_LEN(BUF(J3)))// &
     &                       ' -- qualifier SIGMA was expected but got '// &
     &                       BUF(J3)(IND(1,3):IND(2,3)) )
                         DEALLOCATE ( BUF  )
                         DEALLOCATE ( EHEO )
                         DEALLOCATE ( EHEC )
                         DEALLOCATE ( EHES )
                         RETURN 
                    END IF
!
                    READ ( UNIT=BUF(J3)(IND(1,4):IND(2,4)), FMT='(F22.12)', &
     &                     IOSTAT=IER ) SIG
                    IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 8766, IUER, 'PARSE_EHEO', 'Error in '// &
     &                       'decoding constraint '//NAM_EHEO_CNS(J7)// &
     &                       ' at line "'//BUF(J3)(1:I_LEN(BUF(J3)))// &
     &                       '" when the value HEO keyword in $FLAGS '// &
     &                       'section was parsed' )
                         DEALLOCATE ( BUF  )
                         DEALLOCATE ( EHEO )
                         DEALLOCATE ( EHEC )
                         DEALLOCATE ( EHES )
                         RETURN 
                    END IF
!
! ----------------- ... and put it in an appropriate place in EHES
!
                    CALL LIB$MOVC3 ( 8, SIG, %VAL(LOC(EHES(1))+(J7-1)*8) )
!
                    FL_EHEO_CNS(J7) = .TRUE.
               END IF
 470        CONTINUE 
!
            IF ( .NOT. FL_FOUND ) THEN
                 CALL ERR_LOG ( 8767, IUER, 'PARSE_EHEO', 'Unsupported '// &
     &               'constraint name '//BUF(J3)(IND(1,2):IND(2,2))//' at '// &
     &               'line "'//BUF(J3)(1:I_LEN(BUF(J3)))// &
     &               ' -- qualifier SIGMA was expected but got '// &
     &                BUF(J3)(IND(1,3):IND(2,3)) )
                 DEALLOCATE ( BUF  )
                 DEALLOCATE ( EHEO )
                 DEALLOCATE ( EHEC )
                 DEALLOCATE ( EHES )
                 RETURN 
            END IF
          ELSE IF ( BUF(J3)(IND(1,1):IND(1,1)+1) == 'A ' ) THEN
            IND_EHEO = 0
            DO 480 J8=1,L_EHEO
               IL = I_LEN(EHEO(J8)%NAME)
               IF ( BUF(J3)(IND(1,2):IND(1,2)+IL-1) == EHEO(J8)%NAME(1:IL) ) THEN
                    IND_EHEO = J8
               END IF
 480        CONTINUE 
            IF ( IND_EHEO == 0 ) THEN
                 CALL ERR_LOG ( 8768, IUER, 'PARSE_EHEO', 'Error in parsing '// &
     &               'line "'//BUF(J3)(1:I_LEN(BUF(J3)))//'" -- the WAVE '// &
     &               'was not defined earlier. This error was detected in '// &
     &               'parsing the list of HEO keyword in $FLAGS section' )
                 RETURN 
            END IF
!
            READ ( UNIT=BUF(J3)(IND(1,3):IND(2,3)), FMT='(F22.12)', IOSTAT=IER ) &
     &             EHEO(IND_EHEO)%APR(HEO__COS,HEO__E1E2,1)
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8769, IUER, 'PARSE_EHEO', 'Error in parsing '// &
     &               'line "'//BUF(J3)(1:I_LEN(BUF(J3)))//'" -- cannot '// &
     &               'transform the 3rd word' )
                 RETURN 
            END IF
!
            READ ( UNIT=BUF(J3)(IND(1,4):IND(2,4)), FMT='(F22.12)', IOSTAT=IER ) &
     &             EHEO(IND_EHEO)%APR(HEO__SIN,HEO__E1E2,1)
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8770, IUER, 'PARSE_EHEO', 'Error in parsing '// &
     &               'line "'//BUF(J3)(1:I_LEN(BUF(J3)))//'" -- cannot '// &
     &               'transform the 4th word' )
                 RETURN 
            END IF
          ELSE IF ( BUF(J3)(IND(1,1):IND(1,1)+1) .NE. 'W  '  .AND.  &
     &              BUF(J3)(IND(1,1):IND(1,1)+1) .NE. 'C  '  .AND.  &
     &              BUF(J3)(IND(1,1):IND(2,1))   .NE. 'CNS'         ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( LIND, STR )
              CALL ERR_LOG ( 8771, IUER, 'PARSE_EHEO', 'The first word in '// &
     &            'line "'//BUF(J3)(1:I_LEN(BUF(J3)))//'" is not WAVE or '// &
     &            'CONSTRAINT.' )
              DEALLOCATE ( BUF )
              DEALLOCATE ( EHEO )
              DEALLOCATE ( EHEC )
              DEALLOCATE ( EHES )
              RETURN 
         END IF
 430  CONTINUE
!
! --- Checke whether all constraints have been defined
!
      FL_FOUND = .FALSE.
      DO 490 J9=1,EHEO__NUM_CNS
         IF ( .NOT. FL_EHEO_CNS(J9) ) THEN
              CALL ERR_LOG( 8768, 8772, 'PARSE_EHEO', 'Error in parsing '// &
     &            'keyword HEO in $FLAGS section: reciprocal weight for '// &
     &            'constraint '//NAM_EHEO_CNS(J9)//' was not defined' )
              FL_FOUND = .TRUE.
         END IF
 490  CONTINUE 
!
      IF ( FL_FOUND ) THEN
           CALL ERR_LOG ( 8773, IUER, 'PARSE_EHEO', 'Reciprocal weights '// &
     &          'for some constraints were not defined in HEO keyword ' )
           DEALLOCATE ( BUF  )
           DEALLOCATE ( EHEO )
           DEALLOCATE ( EHEC )
           DEALLOCATE ( EHES )
           RETURN 
      END IF
!
      SIZE_EHEO = MIN(1,L_EHEO)*SIZEOF(EHEO(1))
      SIZE_EHEC = MIN(1,L_EHEC)*SIZEOF(EHEC(1))
      SIZE_EHES = SIZEOF(EHES(1))
#ifdef HPUX
!
! --- Buggy compiler HPUX ignores operator SAVE and deallocates EHEO object
! --- upon return. Damn it!
!
      CALL ERR_PASS  ( IUER, IER )
      CALL GRAB_MEM  ( IER, MEM_LEN, MEM_ADR, 1, INT8(SIZE_EHEO), ADR_EHEO )
      CALL LIB$MOVC3 ( SIZE_EHEO, EHEO, %VAL(ADR_EHEO) )
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) ' SIZE_EHEO=',SIZE_EHEO
           CALL ERR_LOG ( 8774, IUER, 'PARSE_EHEO', 'Error in an '// &
     &         'an attempt to allocate dynamic memory' )
           DEALLOCATE ( BUF  )
           DEALLOCATE ( EHEO )
           DEALLOCATE ( EHEC )
           DEALLOCATE ( EHES )
           RETURN
      END IF
!
! --- Buggy compiler HPUX ignores operator SAVE and deallocates EHEC object
! --- upon return. Damn it!
!
      CALL ERR_PASS  ( IUER, IER )
      CALL GRAB_MEM  ( IER, MEM_LEN, MEM_ADR, 1, INT8(SIZE_EHEC), ADR_EHEC )
      CALL LIB$MOVC3 ( SIZE_EHEC, EHEC, %VAL(ADR_EHEC) )
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) ' SIZE_EHEC=',SIZE_EHEC
           CALL ERR_LOG ( 8775, IUER, 'PARSE_EHEO', 'Error in an '// &
     &         'an attempt to allocate dynamic memory' )
           DEALLOCATE ( BUF  )
           DEALLOCATE ( EHEO )
           DEALLOCATE ( EHEC )
           DEALLOCATE ( EHES )
           RETURN
      END IF
!
! --- Buggy compiler HPUX ignores operator SAVE and deallocates EHEC object
! --- upon return. Damn it!
!
      CALL ERR_PASS  ( IUER, IER )
      CALL GRAB_MEM  ( IER, MEM_LEN, MEM_ADR, 1, INT8(INT8(SIZE)_EHES), ADR_EHESS )
      CALL LIB$MOVC3 ( SIZE_EHES, EHES, %VAL(ADR_EHES) )
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) ' SIZE_EHES=',SIZE_EHES
           CALL ERR_LOG ( 8776, IUER, 'PARSE_EHEO', 'Error in an '// &
     &         'an attempt to allocate dynamic memory' )
           DEALLOCATE ( BUF  )
           DEALLOCATE ( EHEO )
           DEALLOCATE ( EHEC )
           DEALLOCATE ( EHES )
           RETURN
      END IF
#else
      ADR_EHEO  = LOC(EHEO)
      ADR_EHEC  = LOC(EHEC)
      ADR_EHES  = LOC(EHES)
#endif
!
      IF ( L_EHEO > 0 ) THEN
!
! -------- Write down object EHEO into the scratch file.
!
           FILOUT = PRE_SCR_DIR(1:PRE_SD_LEN)//'EHEO'//PRE_LETRS
           INQUIRE ( FILE=FILOUT, EXIST=LEX ) 
           IF ( .NOT. LEX ) THEN
                CALL UNLINK ( FILOUT(1:I_LEN(FILOUT))//CHAR(0) )
           END IF
!
           CALL ERR_PASS  ( IUER, IER )
           CALL BINF_OPEN ( FILOUT, 'NEW', LUN, IER  )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8777, IUER, 'PARSE_EHEO', 'Failure in an attempt '// &
     &              'to open output file '//FILOUT )
                RETURN 
           END IF
!
           CALL ERR_PASS     ( IUER, IER )
           CALL WRBIN_ARRAY  ( LUN, 'I4', 1, L_EHEO, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8778, IUER, 'PARSE_EHEO', 'Failure in '// &
     &              'writing into the output file '//FILOUT )
                RETURN 
           END IF
!
           CALL ERR_PASS     ( IUER, IER )
           CALL WRBIN_ARRAY  ( LUN, 'I4', 1, SIZEOF(EHEO(1)), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8779, IUER, 'PARSE_EHEO', 'Failure in '// &
     &              'writing into the output file '//FILOUT )
                RETURN 
           END IF
!
           CALL ERR_PASS     ( IUER, IER )
           CALL WRBIN_ARRAY  ( LUN, 'B1', SIZE_EHEO, %VAL(ADR_EHEO), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8780, IUER, 'PARSE_EHEO', 'Failure in '// &
     &              'writing into the output file '//FILOUT )
                RETURN 
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL BINF_CLOSE ( LUN, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8781, IUER, 'PARSE_EHEO', 'Failure in '// &
     &              'closing the output file '//FILOUT )
                RETURN 
           END IF
      END IF
!
      IF ( L_EHEC > 0 ) THEN
!
! -------- Write down object EHEC into the scratch file.
!
           FILOUT = PRE_SCR_DIR(1:PRE_SD_LEN)//'EHEC'//PRE_LETRS
           INQUIRE ( FILE=FILOUT, EXIST=LEX ) 
           IF ( .NOT. LEX ) THEN
                CALL UNLINK ( FILOUT(1:I_LEN(FILOUT))//CHAR(0) )
           END IF
!
           CALL ERR_PASS  ( IUER, IER )
           CALL BINF_OPEN ( FILOUT, 'NEW', LUN, IER  )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8782, IUER, 'PARSE_EHEO', 'Failure in an attempt '// &
     &              'to open output file '//FILOUT )
                RETURN 
           END IF
!
           CALL ERR_PASS     ( IUER, IER )
           CALL WRBIN_ARRAY  ( LUN, 'I4', 1, L_EHEC, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8783, IUER, 'PARSE_EHEO', 'Failure in '// &
     &              'writing into the output file '//FILOUT )
                RETURN 
           END IF
!
           CALL ERR_PASS     ( IUER, IER )
           CALL WRBIN_ARRAY  ( LUN, 'I4', 1, SIZEOF(EHEC(1)), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8784, IUER, 'PARSE_EHEO', 'Failure in '// &
     &              'writing into the output file '//FILOUT )
                RETURN 
           END IF
!
           CALL ERR_PASS     ( IUER, IER )
           CALL WRBIN_ARRAY  ( LUN, 'B1', SIZE_EHEC, %VAL(ADR_EHEC), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8785, IUER, 'PARSE_EHEO', 'Failure in '// &
     &              'writing into the output file '//FILOUT )
                RETURN 
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL BINF_CLOSE ( LUN, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8786, IUER, 'PARSE_EHEO', 'Failure in '// &
     &              'closing the output file '//FILOUT )
                RETURN 
           END IF
      END IF
!
! --- Write down object EHES into the scratch file.
!
      FILOUT = PRE_SCR_DIR(1:PRE_SD_LEN)//'EHES'//PRE_LETRS
      INQUIRE ( FILE=FILOUT, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL UNLINK ( FILOUT(1:I_LEN(FILOUT))//CHAR(0) )
      END IF
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FILOUT, 'NEW', LUN, IER  )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8787, IUER, 'PARSE_EHEO', 'Failure in an attempt '// &
     &         'to open output file '//FILOUT )
           RETURN 
      END IF
!
      CALL ERR_PASS     ( IUER, IER )
      CALL WRBIN_ARRAY  ( LUN, 'B1', SIZE_EHES, %VAL(ADR_EHES), IER )
      CALL WRBIN_ARRAY  ( LUN, 'B1', 1, L_EHEC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8788, IUER, 'PARSE_EHEO', 'Failure in '// &
     &         'writing into the output file '//FILOUT )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8789, IUER, 'PARSE_EHEO', 'Failure in '// &
     &         'closing the output file '//FILOUT )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PARSE_EHEO  !#!#     
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PRINT_EHEO ( LUN, L_EHEO, EHEO, L_EHEC, EHEC, EHES )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PRINT_EHEO prints contents of HPE object in     *
! *   logical unit LUN.                                                  *
! *                                                                      *
! * ### 31-MAY-2006   PRINT_EHEO    v1.0 (c) L. Petrov  31-MAY-2006  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INTEGER*4  LUN, L_EHEO, L_EHEC
      TYPE     ( EHEO__TYPE ) EHEO(L_EHEO)
      TYPE     ( EHEC__TYPE ) EHEC(L_EHEC)
      TYPE     ( EHES__TYPE ) EHES
      CHARACTER  NAM_EHEO_CNS(EHEO__NUM_CNS)*20
      DATA       NAM_EHEO_CNS &
     &           / &
     &                'HEO_VAL_E1E2_HAR    ', &
     &                'HEO_VAL_E1E2_CROSS  ', &
     &                'HEO_VAL_E1E2_SHIFT  ', &
     &                'HEO_VAL_E1E2_DRIFT  ', &
     &                'HEO_VAL_E3_HAR      ', &
     &                'HEO_VAL_E3_CROSS    ', &
     &                'HEO_VAL_E3_SHIFT    ', &
     &                'HEO_VAL_E3_DRIFT    ', &
     &                'HEO_ERM_E1E2_HAR    ', &
     &                'HEO_ERM_E1E2_CROSS  ', &
     &                'HEO_ERM_E1E2_SHIFT  ', &
     &                'HEO_ERM_E1E2_DRIFT  ', &
     &                'HEO_ERM_E3_HAR      ', &
     &                'HEO_ERM_E3_CROSS    ', &
     &                'HEO_ERM_E3_SHIFT    ', &
     &                'HEO_ERM_E3_DRIFT    '  &
     &           /
      REAL*8     VAL, SIG
      INTEGER*4  J1, J2, J3, J4
!
      WRITE  ( LUN, '(A)' ) 'Estimation of harmonic variations in EOP'
      WRITE  ( LUN, '(A,I3)' ) 'L_EHEO: ', L_EHEO
      DO 410 J1=1,L_EHEO
         WRITE  ( LUN, '(A)' ) '------------------------------------------'
         WRITE  ( LUN, 110 ) J1, '    NAME:      ', EHEO(J1)%NAME
 110     FORMAT ( 'EHEO(',I3,')%',A,': ', A )
         WRITE  ( LUN, 130 ) J1, '    Phas:      ', EHEO(J1)%PHAS
 120     FORMAT ( 'EHEO(',I3,')%',A,': ', F12.8 )
         WRITE  ( LUN, 130 ) J1, '    Freq:      ', EHEO(J1)%FREQ
 130     FORMAT ( 'EHEO(',I3,')%',A,': ', 0PD18.12 )
         WRITE  ( LUN, 130 ) J1, '    Freq: ', EHEO(J1)%ACCL
         WRITE  ( LUN, 140 ) J1, '    Est_E12/E3 :     ', EHEO(J1)%FL_EST
         WRITE  ( LUN, 140 ) J1, '    Cns_E12/E3 :     ', EHEO(J1)%FL_CNS
         WRITE  ( LUN, 140 ) J1, '    Vel_Est_E12/E3 : ', EHEO(J1)%FL_EST_VEL
         WRITE  ( LUN, 140 ) J1, '    VEL_Cns_E12/E3 : ', EHEO(J1)%FL_CNS_VEL
 140     FORMAT ( 'EHEO(',I3,')%',A,': ', L1, 1X, L1 )
 410  CONTINUE 
      WRITE  ( LUN, '(A)' ) '------------------------------------------'
      WRITE  ( LUN, '(A,I3)' ) 'L_EHEC: ', L_EHEC
      WRITE  ( LUN, '(A)' ) '------------------------------------------'
!
      DO 420 J2=1,L_EHEC
         WRITE  ( LUN, 150 ) J2, 1, EHEC(J2)%HEO_TYPE, EHEC(J2)%IND(1), &
     &                              EHEC(J2)%AMP(HEO__COS,1), &
     &                              EHEC(J2)%AMP(HEO__SIN,1) 
         WRITE  ( LUN, 150 ) J2, 2, EHEC(J2)%HEO_TYPE, EHEC(J2)%IND(2), &
     &                              EHEC(J2)%AMP(HEO__COS,2), &
     &                              EHEC(J2)%AMP(HEO__SIN,2)
 150     FORMAT ( 'EHEC(',I3,') Wave: ',I1,' Typ: ',I1, ' Ind: ',I3, &
     &            ' Amp_cos: ',1PD15.7, ' Amp_sin: ',1PD15.7 )
 420  CONTINUE 
      WRITE  ( LUN, '(A)' ) '------------------------------------------'
!
      DO 430 J3=1,EHEO__NUM_CNS
         CALL LIB$MOVC3 ( 8, %VAL(LOC(EHES)+(J3-1)*8), SIG )
         WRITE ( LUN, 160 ) NAM_EHEO_CNS(J2), SIG
 160     FORMAT ( 'EHES:  ',A, 2X, 1PD15.7 )
 430  CONTINUE 
      WRITE  ( LUN, '(A)' ) '------------------------------------------'
!
      RETURN
      END  SUBROUTINE PRINT_EHEO
