      SUBROUTINE PARSE_HPE ( STRING, L_HPE, ADR_HPE, SIZE_HPE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PARSE_HPE parses the portion of the batch control file     *
! *   whip is related to HARMONIC_POS keyword. The routine parses the    *
! *   portion of the file according to specifications, allocates         *
! *   memory for HPE object and loads with the fields determined from    *
! *   parsing the control file.                                          *
! *                                                                      *
! *   The routine PARSE_HPE first reads the portion of control file      *
! *   which starts immediately after the keyword HARMONIC_POS and puts   *
! *   into internal buffer all continuation lines including the lines    *
! *   with HARMONIC_POS. Upon completion of PARSE_HPE the control file   *
! *   is position to first keyword which follows HARMONIC_POS.           *
! *                                                                      *
! *   PARSE_HPE supports @-expansion inside the keyword HARMONIC_POS.    *
! *   It reads @-files and inserts their contents into the buffer.       *
! *                                                                      *
! *   The internal buffers are freed after completion PARSE_HPE          *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    L_HPE ( INTEGER*4 ) -- The number of harmonics.                   *
! *  ADR_HPE ( INTEGER*8 ) -- The address of the first element of the    *
! *                           array of objects which keep information    *
! *                           about harmonics site position estimation   *
! *                           parameterization.                          *
! * SIZE_HPE ( INTEGER*4 ) -- The size in bytes of the array of objects  *
! *                           which keeps information about harmonic     *
! *                           site position estimation parameterization. *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   STRING ( CHARACTER ) -- The portion of the line of the control     *
! *                           line which starts from the first           *
! *                           non-whiteblank character following the     *
! *                           keyword HARMONIC_POS. STRING is blanked    *
! *                           at the end of PARSE_HPE work.              *
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
! *                                                                      *
! *  ### 21-FEB-2005   PARSE_HPE   v2.2 (c)  L. Petrov  10-MAR-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      INTEGER*4  L_HPE, SIZE_HPE, IUER
      ADDRESS__TYPE :: ADR_HPE
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 1024 )
      PARAMETER  ( MIND =   32 )
      CHARACTER  STRING*(*)
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      CHARACTER  STR*128, NEXT*16, NAME_EXPAND*128, REG*4, FILOUT*128
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9)//'\' )
      INTEGER*2  LN
      INTEGER*4  IOS, J1, J2, J3, J4, IAT, IP, IL, NBUF, LIND, IND(2,MIND), &
     &           LUN, IER
      TYPE ( HPE__TYPE ), ALLOCATABLE :: HPE(:)
      SAVE       HPE
      INTEGER*8        MEM_LEN
      ADDRESS__TYPE :: MEM_ADR
      LOGICAL*4  FL_NAME, FL_STATION, FL_PHASE, FL_FREQ, FL_NNT, FL_NNR, FL_END, LEX
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      INTEGER*2, EXTERNAL :: CFREAD
!
      STR = STRING
      CALL CHASHL  ( STR ) 
      IF ( STR(1:3) .EQ. 'NO '   .OR. &
     &     STR(1:5) .EQ. 'NONE '      ) THEN
!
! -------- Nothing to do
!
           L_HPE = 0
           ADR_HPE = 0
           SIZE_HPE = 0
           CALL SPLITSTRING ( STRING, NEXT, STRING )
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Aloocate memory for the command buffer
!
      ALLOCATE ( BUF(MBUF), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL IINCH ( IOS, STR ) 
           CALL ERR_LOG ( 8711, IUER, 'PARSE_HPE', 'Error in an attempt to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes' )
           RETURN 
      END IF
!
! --- Read the control file line by line till we end of continuation. Expand
! --- @-names and read their context into the buffer
!
      NBUF = 1
      BUF(NBUF) = STRING
      DO 410 J1=2,MBUF
!
! ------ Read the next line from the control file
!
         LN = CFREAD ( STRING )
         IF ( ILEN(STRING) ==  0  ) GOTO 410
         IF ( STRING(1:1)  == '*' ) GOTO 410
         NBUF = NBUF + 1
         BUF(NBUF) = STRING
         IAT = INDEX ( BUF(NBUF), '@' )
         IF ( IAT .GT. 0 ) THEN
!
! ----------- Expand the name and expand the buffer
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
                   CALL ERR_LOG ( 8712, IUER, 'PARSE_HPE', 'Failure to'// &
     &                 ' expand line @'//NAME_EXPAND(1:I_LEN(NAME_EXPAND))// &
     &                 ' in processing keyword HARMOINC_POS' ) 
                   DEALLOCATE ( BUF ) 
                   RETURN 
              END IF
         END IF
         IF ( STRING(ILEN(STRING):ILEN(STRING)) .NE.  '\' ) THEN
!
! ----------- Aga. The last character is not continuation. This means that
! ----------- we reached the end of the sequence of continuation lines. Good!
!
              GOTO 810
         END IF
 410  CONTINUE 
 810  CONTINUE 
      CALL CLRCH ( STRING )
!
! --- First pass: deterine the number of harmonics
!
      L_HPE = 0
      DO 420 J2=1,NBUF
         IF ( INDEX ( BUF(J2), 'NAME' ) > 0 ) THEN
              L_HPE = L_HPE + 1
         END IF
 420  CONTINUE 
      IF ( L_HPE .EQ. 0 ) THEN
           CALL ERR_LOG ( 8713, IUER, 'PARSE_HPE', 'No NAME qualifiers '// &
     &         ' were found in processing the keyword HARMOINC_POS' ) 
           DEALLOCATE ( BUF ) 
           RETURN 
      END IF
      IF ( L_HPE .GT. M__HPE ) THEN
           CALL ERR_LOG ( 8714, IUER, 'PARSE_HPE', 'To many NAME '// &
     &         'qualifiers were found in processing the keyword '// &
     &         'HARMOINC_POS. Check parameter M__HPE in '// &
     &         '$PSOLVE_ROOT/include/solve.i' )
           DEALLOCATE ( BUF ) 
           RETURN 
      END IF
!
! --- Allocate memory for HPE object
!
      ALLOCATE ( HPE(L_HPE), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 8715, IUER, 'PARSE_HPE', 'Failure to '// &
     &         'allocate in memory object HPE' )
           DEALLOCATE ( BUF ) 
           RETURN 
      END IF
      CALL NOUT ( L_HPE*SIZEOF(HPE(1)), HPE )
!
! --- The second pass. Parse the keywords
!
      L_HPE = 0
      NEXT = 'NAME'
      FL_NAME    = .FALSE.
      FL_STATION = .FALSE.
      FL_PHASE   = .FALSE.
      FL_FREQ    = .FALSE.
      DO 430 J3=1,NBUF
         IL = I_LEN(BUF(J3))
         IF ( BUF(J3)(IL:IL) .EQ. '\' ) THEN
              CALL CLRCH ( BUF(J3)(IL:IL) )
              FL_END = .FALSE.
            ELSE 
              FL_END = .TRUE.
         END IF
!
         IL = ILEN(BUF(J3))
         IF ( IL .LE. 0 ) GOTO 430
         IF ( BUF(J3)(1:1) == '*' ) GOTO 430
!
! ------ Parse the line into words
!
         CALL EXWORD ( BUF(J3), MIND, LIND, IND, REG, -3 )
         DO 440 J4=1,LIND
            IF ( NEXT == 'NAME' ) THEN
!
! ===================
!
                 IF ( .NOT. FL_NAME ) THEN
                      IF ( BUF(J3)(IND(1,J4):IND(2,J4)) == 'NAME' ) THEN
                           NEXT = 'NAME' 
                           FL_NAME = .TRUE.
                         ELSE 
                           CALL ERR_LOG ( 8716, IUER, 'PARSE_HPE', 'Error '// &
     &                         'in parsing KEYWORD HARMONIC_POS: the '// &
     &                         'qualifier NAME was expected, but "'// &
     &                          BUF(J3)(IND(1,J4):IND(2,J4))//'" was found' )
                           DEALLOCATE ( HPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
                      L_HPE = L_HPE + 1
                    ELSE 
                      HPE(L_HPE)%NAME = BUF(J3)(IND(1,J4):IND(2,J4)) 
                      NEXT = 'STATION' 
                      FL_NAME    = .FALSE.
                      FL_STATION = .FALSE.
                 END IF
               ELSE IF ( NEXT == 'STATION' ) THEN
!
! ===========================
!
                 IF ( .NOT. FL_STATION ) THEN
                      IF ( BUF(J3)(IND(1,J4):IND(2,J4)) == 'STATION' ) THEN
                           FL_NAME    = .FALSE.
                           FL_STATION = .TRUE.
                           NEXT = 'STATION' 
                         ELSE 
                           CALL ERR_LOG ( 8717, IUER, 'PARSE_HPE', 'Error '// &
     &                         'in parsing KEYWORD HARMONIC_POS: the '// &
     &                         'qualifier STATION was expected, but "'// &
     &                          BUF(J3)(IND(1,J4):IND(2,J4))//'" was found' )
                           DEALLOCATE ( HPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                       END IF
                       HPE(L_HPE)%L_STA = 0
                     ELSE 
                       HPE(L_HPE)%L_STA = HPE(L_HPE)%L_STA + 1
                       IF ( HPE(L_HPE)%L_STA > MAX_STA ) THEN
                            CALL ERR_LOG ( 8718, IUER, 'PARSE_HPE', 'Error '// &
     &                         'in parsing KEYWORD HARMONIC_POS: too '// &
     &                         'many stations  in the list. More than '// &
     &                         'MAX_STA in $PSOLVE_ROOT/include/solve.i' )
                            DEALLOCATE ( HPE ) 
                            DEALLOCATE ( BUF ) 
                            RETURN 
                       END IF
                       HPE(L_HPE)%C_STA(HPE(L_HPE)%L_STA) = BUF(J3)(IND(1,J4):IND(2,J4)) 
                       IF ( HPE(L_HPE)%C_STA(HPE(L_HPE)%L_STA) == 'PHASE' ) THEN
                            HPE(L_HPE)%L_STA = HPE(L_HPE)%L_STA - 1
                            FL_STATION = .FALSE.
                            FL_PHASE = .TRUE.
                            NEXT = 'PHASE'
                       END IF
                       IF ( HPE(L_HPE)%C_STA(HPE(L_HPE)%L_STA) == 'FREQ' ) THEN
                            CALL ERR_LOG ( 8719, IUER, 'PARSE_HPE', 'Error '// &
     &                         'in parsing KEYWORD HARMONIC_POS: qualifier '// &
     &                         'phase was omitted' ) 
                            DEALLOCATE ( HPE ) 
                            DEALLOCATE ( BUF ) 
                            RETURN 
                       END IF
                 END IF
               ELSE IF ( NEXT == 'PHASE' ) THEN
!
! ===========================
!
                 READ ( UNIT=BUF(J3)(IND(1,J4):IND(2,J4)), FMT='(F22.12)', &
     &                  IOSTAT=IOS ) HPE(L_HPE)%PHASE
                 IF ( IOS .NE. 0 ) THEN
                      CALL ERR_LOG ( 8720, IUER, 'PARSE_HPE', 'Error '// &
     &                    'in parsing KEYWORD HARMONIC_POS: error in '// &
     &                    'the value of PHASE qualifier ' )
                      DEALLOCATE ( HPE ) 
                      DEALLOCATE ( BUF ) 
                      RETURN 
                 END IF
                 NEXT = 'FREQ'
                 FL_PHASE = .FALSE.
                 FL_FREQ = .FALSE.
               ELSE IF ( NEXT == 'FREQ' ) THEN
                 IF ( .NOT. FL_FREQ ) THEN
                      IF ( BUF(J3)(IND(1,J4):IND(2,J4)) == 'FREQ' ) THEN
                           FL_FREQ = .TRUE.
                           NEXT = 'FREQ'
                         ELSE 
                           CALL ERR_LOG ( 8721, IUER, 'PARSE_HPE', 'Error '// &
     &                         'in parsing KEYWORD HARMONIC_POS: the '// &
     &                         'qualifier FREQ was expected, but "'// &
     &                          BUF(J3)(IND(1,J4):IND(2,J4))//'" was found' )
                           DEALLOCATE ( HPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
                    ELSE 
                      READ ( UNIT=BUF(J3)(IND(1,J4):IND(2,J4)), FMT='(F22.12)', &
     &                       IOSTAT=IOS ) HPE(L_HPE)%FREQ
                      IF ( IOS .NE. 0 ) THEN
                           CALL ERR_LOG ( 8722, IUER, 'PARSE_HPE', 'Error '// &
     &                         'in parsing KEYWORD HARMONIC_POS: error in '// &
     &                         'reading the value of FREQ qualifier ' )
                           DEALLOCATE ( HPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
                      NEXT    = 'NNT'
                      FL_FREQ = .FALSE.
                      FL_NNT  = .FALSE.
                 END IF
               ELSE IF ( NEXT == 'NNT' ) THEN
!
! ===========================
!
                 IF ( .NOT. FL_NNT ) THEN
                      IF ( BUF(J3)(IND(1,J4):IND(2,J4)) == 'NNT_CNS_SIGMA' ) THEN
                           FL_NNT = .TRUE.
                           NEXT = 'NNT'
                         ELSE 
                           CALL ERR_LOG ( 8723, IUER, 'PARSE_HPE', 'Error '// &
     &                         'in parsing KEYWORD HARMONIC_POS: the '// &
     &                         'qualifier NNT_CNS_SIGMA was expected, but "'// &
     &                          BUF(J3)(IND(1,J4):IND(2,J4))//'" was found' )
                           DEALLOCATE ( HPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
                    ELSE 
                      READ ( UNIT=BUF(J3)(IND(1,J4):IND(2,J4)), FMT='(F22.12)', &
     &                       IOSTAT=IOS ) HPE(L_HPE)%NNT_CNS_SIGMA
                      IF ( IOS .NE. 0 ) THEN
                           CALL ERR_LOG ( 8724, IUER, 'PARSE_HPE', 'Error '// &
     &                         'in parsing KEYWORD HARMONIC_POS: error in '// &
     &                         'reading the value of NNT_CNS_SIGMA '// &
     &                         'qualifier: '//BUF(J3)(IND(1,J4):IND(2,J4)) )
                           DEALLOCATE ( HPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
                      NEXT    = 'NNR'
                      FL_NNT  = .FALSE.
                      FL_NNR  = .FALSE.
                 END IF
               ELSE IF ( NEXT == 'NNR' ) THEN
!
! ===========================
!
                 IF ( .NOT. FL_NNR ) THEN
                      IF ( BUF(J3)(IND(1,J4):IND(2,J4)) == 'NNR_CNS_SIGMA' ) THEN
                           FL_NNR = .TRUE.
                           NEXT   = 'NNR'
                         ELSE 
                           CALL ERR_LOG ( 8725, IUER, 'PARSE_HPE', 'Error '// &
     &                         'in parsing KEYWORD HARMONIC_POS: the '// &
     &                         'qualifier NNR_CNS_SIGMA was expected, but "'// &
     &                          BUF(J3)(IND(1,J4):IND(2,J4))//'" was found' )
                           DEALLOCATE ( HPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
                    ELSE 
                      READ ( UNIT=BUF(J3)(IND(1,J4):IND(2,J4)), FMT='(F22.12)', &
     &                       IOSTAT=IOS ) HPE(L_HPE)%NNR_CNS_SIGMA
                      IF ( IOS .NE. 0 ) THEN
                           CALL ERR_LOG ( 8726, IUER, 'PARSE_HPE', 'Error '// &
     &                         'in parsing KEYWORD HARMONIC_POS: error in '// &
     &                         'reading the value of NNR_CNS_SIGMA '// &
     &                         'qualifier: '//BUF(J3)(IND(1,J4):IND(2,J4)) )
                           DEALLOCATE ( HPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
                      NEXT    = 'NAME'
                      FL_NNR  = .FALSE.
                      FL_NAME = .FALSE.
                 END IF
            END IF
 440     CONTINUE 
         IF ( FL_END ) GOTO 830
 430  CONTINUE 
 830  CONTINUE 
!
      SIZE_HPE = SIZEOF(HPE)
      DEALLOCATE ( BUF ) 
!
#ifdef HPUX
!
! --- Buggy compiler HPUX ignores operator SAVE and deallocates HPE object
! --- upon return. Damn it!
!
      CALL ERR_PASS  ( IUER, IER )
      CALL GRAB_MEM  ( IER, MEM_LEN, MEM_ADR, 1, INT8(SIZE_HPE), ADR_HPE )
      CALL LIB$MOVC3 ( SIZE_HPE, HPE, %VAL(ADR_HPE) )
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) ' SIZE_HPE=',SIZE_HPE
           CALL ERR_LOG ( 8727, IUER, 'PARSE_HPE', 'Error in an '// &
     &         'an attempt to allocate dynamic memory' )
           RETURN 
      END IF
#else
      ADR_HPE  = LOC(HPE)
#endif
!
! --- Write down object HPE into the scratch file.
!
      FILOUT = PRE_SCR_DIR(1:PRE_SD_LEN)//'EHPE'//PRE_LETRS
      INQUIRE ( FILE=FILOUT, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL UNLINK ( FILOUT(1:I_LEN(FILOUT))//CHAR(0) )
      END IF
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FILOUT, 'NEW', LUN, IER  )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8728, IUER, 'PARSE_HPE', 'Failure in an attempt '// &
     &         'to open output file '//FILOUT )
           RETURN 
      END IF
!
      CALL ERR_PASS     ( IUER, IER )
      CALL WRBIN_ARRAY  ( LUN, 'I4', 1, L_HPE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8729, IUER, 'PARSE_HPE', 'Failure in '// &
     &         'writing into the output file '//FILOUT )
           RETURN 
      END IF
!
      CALL ERR_PASS     ( IUER, IER )
      CALL WRBIN_ARRAY  ( LUN, 'I4', 1, SIZE_HPE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8729, IUER, 'PARSE_HPE', 'Failure in '// &
     &         'writing into the output file '//FILOUT )
           RETURN 
      END IF
!
      CALL ERR_PASS     ( IUER, IER )
      CALL WRBIN_ARRAY  ( LUN, 'B1', SIZE_HPE, %VAL(ADR_HPE), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8730, IUER, 'PARSE_HPE', 'Failure in '// &
     &         'writing into the output file '//FILOUT )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8731, IUER, 'PARSE_HPE', 'Failure in '// &
     &         'closing the output file '//FILOUT )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PARSE_HPE
!
! ------------------------------------------------------------------------
!
      SUBROUTINE BUFFER_EXPAND ( MBUF, NBUF, BUF, IAT, FILE_EXPAND, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine BUFFER_EXPAND reads the file FILE_EXPAND and    *
! *   appends its contents to the buffer.                                *
! *                                                                      *
! * ### 22-FEB-2005  BUFFER_EXPAND  v1.0 (c)  L. Petrov  22-FEB-2005 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MBUF, NBUF, IAT, IUER 
      CHARACTER  BUF(MBUF)*(*), FILE_EXPAND*(*)
      CHARACTER  STR*128
      INTEGER*4  NEX, IL, J1, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      STR = BUF(NBUF)
      CALL ERR_PASS ( IUER, IER )
      CALL  RD_TEXT ( FILE_EXPAND, MBUF-NBUF+1, BUF(NBUF), NEX, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8791, IUER, 'BUFFER_EXPAND', 'Error in an '// &
     &         'attempt to read file '//FILE_EXPAND )
           RETURN 
      END IF
!
      IF ( IAT .GT. 0 ) THEN
           BUF(NBUF) = STR(1:IAT-1)//BUF(NBUF)
      END IF
      DO 410 J1=NBUF,NEX+NBUF-1
         IL = I_LEN(BUF(J1))
         IF ( J1 < NEX+NBUF-1 .AND. BUF(J1)(IL:IL) .NE. '\' ) THEN
              BUF(J1)(IL+1:IL+1) = '\'
         END IF
 410  CONTINUE 
!
      NBUF = NEX+NBUF-1
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  BUFFER_EXPAND 
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PRINT_HPE ( LUN, L_HPE, HPE )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PRINT_HPE prints contents of HPE object in      *
! *   logical unit LUN.                                                  *
! *                                                                      *
! *  ### 24-FEB-2005   PRINT_HPE   v1.0 (c)  L. Petrov  24-FEB-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INTEGER*4  LUN, L_HPE
      TYPE ( HPE__TYPE ) HPE(L_HPE)
      INTEGER*4  J1, J2, J3, J4
!
      WRITE  ( LUN, '(A)' ) 'Estimation of harmonic site position variations'
      DO 410 J1=1,L_HPE
         WRITE  ( LUN, '(A)' ) '------------------------------------------'
         WRITE  ( LUN, 110 ) J1, '    NAME', HPE(J1)%NAME
 110     FORMAT ( 'HPE(',I3,')%',A,': ', A )
         WRITE  ( LUN, 120 ) J1, '   L_STA', HPE(J1)%L_STA
 120     FORMAT ( 'HPE(',I3,')%',A,': ', I4 )
         DO 420 J2=1,HPE(J1)%L_STA
            WRITE  ( LUN, 130 ) J1, ' STATION', J2, HPE(J1)%C_STA(J2)
 130        FORMAT ( 'HPE(',I3,')%',A,': ', I4,' Station: ',A )
 420     CONTINUE 
         WRITE  ( LUN, 140 ) J1, '   PHASE', HPE(J1)%PHASE
         WRITE  ( LUN, 140 ) J1, '    FREQ', HPE(J1)%FREQ 
         WRITE  ( LUN, 140 ) J1, ' NNT_CNS', HPE(J1)%NNT_CNS_SIGMA
         WRITE  ( LUN, 140 ) J1, ' NNR_CNS', HPE(J1)%NNR_CNS_SIGMA
 140     FORMAT ( 'HPE(',I3,')%',A,': ', 1PD23.16 )
 410  CONTINUE 
      WRITE  ( LUN, '(A)' ) '------------------------------------------'
!
      RETURN
      END  SUBROUTINE PRINT_HPE
