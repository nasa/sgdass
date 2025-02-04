      SUBROUTINE PARSE_SPE ( STRING, L_SPE, ADR_SPE, SIZE_SPE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PARSE_SPE parses the portion of the batch control file     *
! *   which is related to SPLINE_POS keyword. The routine parses the     *
! *   portion of the file according to specifications, allocates         *
! *   memory for SPE object and loads with the fields determined from    *
! *   parsing the control file.                                          *
! *                                                                      *
! *   The routine PARSE_SPE first reads the portion of control file      *
! *   which starts immediately after the keyword SPLINE_POS and puts     *
! *   into internal buffer all continuation lines including the lines    *
! *   with SPLINE_POS. Upon completion of PARSE_SPE the control file     *
! *   is position to first keyword which follows SPLINE_POS.             *
! *                                                                      *
! *   PARSE_SPE supports @-expansion inside the keyword SPLINE_POS.      *
! *   It reads @-files and inserts their contents into the buffer.       *
! *                                                                      *
! *   The internal buffers are freed after completion PARSE_SPE          *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    L_SPE ( INTEGER*4 ) -- The number of stations which non-linear    *
! *                           motion is parameterized with expansion     *
! *                           with the B-spline basis.                   *
! *  ADR_SPE ( INTEGER*8 ) -- The address of the first element of the    *
! *                           array of objects which keep information    *
! *                           about spline parameterization of           *
! *                           non-linear motion of the specified         *
! *                           stations.                                  *
! * SIZE_SPE ( INTEGER*4 ) -- The size in bytes of the array of objects  *
! *                           which keeps information about spline       *
! *                           parameterization of non-linear motion of   *
! *                           the specified stations.                    *
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
! *  ### 21-FEB-2005   PARSE_SPE   v2.2 (c)  L. Petrov  10-MAR-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      INTEGER*4  L_SPE, SIZE_SPE, IUER
      ADDRESS__TYPE :: ADR_SPE
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 1024 )
      PARAMETER  ( MIND =   32 )
      CHARACTER  STRING*(*)
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      CHARACTER  STR*128, NEXT*16, NAME_EXPAND*128, FILOUT*128, REG*4
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9)//'\' )
      INTEGER*2  LN
      INTEGER*4  IOS, J1, J2, J3, J4, J5, J6, J7, J8, IAT, IP, IL, NBUF, &
     &           LIND, IND(2,MIND), KDEG, IWORD, LUN, IER
      INTEGER*8        MEM_LEN
      ADDRESS__TYPE :: MEM_ADR
      TYPE ( SPE__TYPE ), ALLOCATABLE :: SPE(:)
      SAVE   SPE
      LOGICAL*4  FL_STATION, FL_DEGREE, FL_NODE, FL_MULT, FL_CNS_STA, &
     &           FL_CNS_VEL, FL_CNS_DER, FL_END, LEX
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
           L_SPE = 0
           ADR_SPE = 0
           SIZE_SPE = 0
           CALL SPLITSTRING ( STRING, NEXT, STRING )
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Allocate the buffer for the portion of the control file
!
      ALLOCATE ( BUF(MBUF), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL IINCH ( IOS, STR ) 
           CALL ERR_LOG ( 8731, IUER, 'PARSE_SPE', 'Error in an attempt to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes' )
           RETURN 
      END IF
!
      NBUF = 1
      BUF(NBUF) = STRING
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
                   CALL ERR_LOG ( 8732, IUER, 'PARSE_SPE', 'Failure to'// &
     &                 ' expand line @'//NAME_EXPAND(1:I_LEN(NAME_EXPAND))// &
     &                 ' in processing keyword SPLINE_POS' ) 
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
! --- Determine the number of stations
!
      L_SPE = 0
      DO 420 J2=1,NBUF
         IF ( INDEX ( BUF(J2), 'STATION' ) > 0 ) THEN
              L_SPE = L_SPE + 1
         END IF
 420  CONTINUE 
      IF ( L_SPE .EQ. 0 ) THEN
           CALL ERR_LOG ( 8733, IUER, 'PARSE_SPE', 'No STATION qualifiers '// &
     &         ' were found in processing the keyword SPLINE_POS' ) 
           DEALLOCATE ( BUF ) 
           RETURN 
      END IF
!
      IF ( L_SPE .GT. M__SPE ) THEN
           WRITE ( 6, * ) ' NBUF=', NBUF, ' L_SPE = ', L_SPE, &
     &                    ' M__SPE = ', M__SPE
           CALL ERR_LOG ( 8734, IUER, 'PARSE_SPE', 'To many NAME '// &
     &         'qualifiers were found in processing the keyword '// &
     &         'SPLINE_POS. Check parameter M__SPE in '// &
     &         '$PSOLVE_ROOT/include/solve.i' )
           DEALLOCATE ( BUF ) 
           RETURN 
      END IF
!
! --- Allocate SPE object
!
      ALLOCATE ( SPE(L_SPE), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 8735, IUER, 'PARSE_SPE', 'Failure to '// &
     &         'allocate in memory object SPE' )
           DEALLOCATE ( BUF ) 
           RETURN 
      END IF
!
! --- Initialization
!
      CALL NOUT ( L_SPE*SIZEOF(SPE(1)), SPE )
!
! --- The second pass of the command buffer
!
      L_SPE = 0
      NEXT = 'STATION'
      FL_STATION = .FALSE.
      FL_DEGREE  = .FALSE.
      FL_NODE    = .FALSE.
      FL_MULT    = .FALSE.
      FL_CNS_STA = .FALSE.
      FL_CNS_VEL = .FALSE.
      FL_CNS_DER = .FALSE.
!
      DO 430 J3=1,NBUF
         IL = I_LEN(BUF(J3))
         IF ( BUF(J3)(IL:IL) .EQ. '\' ) THEN
              CALL CLRCH ( BUF(J3)(IL:IL) )
              FL_END = .FALSE.
            ELSE 
              FL_END = .TRUE.
         END IF
         IL = ILEN(BUF(J3))
         IF ( IL .LE. 0 ) GOTO 430
         IF ( BUF(J3)(1:1) == '*' ) GOTO 430
!
! ------ Split the commaind into words
!
         CALL EXWORD ( BUF(J3), MIND, LIND, IND, REG, -3 )
         DO 440 J4=1,LIND
            IF ( NEXT == 'STATION' ) THEN
!
! =======================
!
                 IF ( .NOT. FL_STATION ) THEN
                      IF ( BUF(J3)(IND(1,J4):IND(2,J4)) == 'STATION' ) THEN
                           FL_STATION = .TRUE.
                         ELSE IF ( BUF(J3)(IND(1,J4):IND(2,J4)) == 'CNS_DER_SIGMA' ) THEN
                           NEXT = 'CNS_DER'
                           IWORD = 1
                           FL_STATION = .FALSE.
                           FL_CNS_DER = .TRUE.
                         ELSE 
                           CALL ERR_LOG ( 8736, IUER, 'PARSE_SPE', 'Error '// &
     &                         'in parsing keyword SPLINE_POS: the '// &
     &                         'qualifier STATION was expected, but "'// &
     &                          BUF(J3)(IND(1,J4):IND(2,J4))//'" was found' )
                           DEALLOCATE ( SPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
                    ELSE 
                      L_SPE = L_SPE + 1
                      SPE(L_SPE)%STATION = BUF(J3)(IND(1,J4):IND(2,J4)) 
!
                      IF ( L_SPE > 1 ) THEN
                           DO 450 J5=1,L_SPE-1
                              IF ( SPE(J5)%STATION == SPE(L_SPE)%STATION ) THEN
                                   CALL ERR_LOG ( 8737, IUER, 'PARSE_SPE',   &
     &                                 'An attempt to define '//             &
     &                                 'parameterization of non-linear '//   &
     &                                 'motion of '//SPE(L_SPE)%STATION//    &
     &                                 ' through expantion with B-spline '// &
     &                                 'basis more than once' )
                                    DEALLOCATE ( SPE ) 
                                    DEALLOCATE ( BUF ) 
                                    RETURN 
                              END IF
 450                       CONTINUE 
                      END IF
                      NEXT = 'DEGREE' 
                      FL_DEGREE  = .FALSE.
                      FL_STATION = .FALSE.
                 END IF
               ELSE IF ( NEXT == 'DEGREE' ) THEN
!
! =======================
!
                 IF ( .NOT. FL_DEGREE ) THEN
                      IF ( BUF(J3)(IND(1,J4):IND(2,J4)) == 'DEGREE' ) THEN
                           FL_STATION = .FALSE.
                           FL_DEGREE  = .TRUE.
                         ELSE 
                           CALL ERR_LOG ( 8738, IUER, 'PARSE_SPE', 'Error '// &
     &                         'in parsing keyword SPLINE_POS: the '// &
     &                         'qualifier DEGREE was expected, but "'// &
     &                          BUF(J3)(IND(1,J4):IND(2,J4))//'" was found' )
                           DEALLOCATE ( SPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
                    ELSE 
                      READ ( UNIT=BUF(J3)(IND(1,J4):IND(2,J4)), FMT='(I9)', &
     &                       IOSTAT=IOS ) SPE(L_SPE)%DEGREE
                      IF ( IOS .NE. 0 ) THEN
                           CALL ERR_LOG ( 8739, IUER, 'PARSE_SPE', 'Error '// &
     &                         'in parsing keyword SPLINE_POS: the '// &
     &                         'value of the qualifier DEGFEE is '// &
     &                         'ill-formated: '//BUF(J3)(IND(1,J4):IND(2,J4)) )
                           DEALLOCATE ( SPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
                      IF ( SPE(L_SPE)%DEGREE > M__SPD  .OR.  &
     &                     SPE(L_SPE)%DEGREE < 0 ) THEN
                           CALL ERR_LOG ( 8740, IUER, 'PARSE_SPE', 'Error '// &
     &                         'in parsing keyword SPLINE_POS: the '// &
     &                         'value of the qualifier DEGREE '// &
     &                          BUF(J3)(IND(1,J4):IND(2,J4))//' is out '// &
     &                         'of range [0, M__SPD]' )
                           DEALLOCATE ( SPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
                      SPE(L_SPE)%L_NOD = 0
                      SPE(L_SPE)%K_NOD = -SPE(L_SPE)%DEGREE 
                      NEXT = 'NODE' 
                      FL_DEGREE  = .FALSE.
                      FL_NODE  = .FALSE.
                 END IF
               ELSE IF ( NEXT == 'NODE' ) THEN
!
! =======================
!
                 IF ( .NOT. FL_NODE ) THEN
                      IF ( BUF(J3)(IND(1,J4):IND(2,J4)) == 'NODE' ) THEN
                           FL_NODE = .TRUE.
                         ELSE 
                           CALL ERR_LOG ( 8741, IUER, 'PARSE_SPE', 'Error '// &
     &                         'in parsing keyword SPLINE_POS: the '// &
     &                         'qualifier NODE was expected, but "'// &
     &                          BUF(J3)(IND(1,J4):IND(2,J4))//'" was found' )
                           DEALLOCATE ( SPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
                    ELSE 
                      SPE(L_SPE)%L_NOD = SPE(L_SPE)%L_NOD + 1
!
                      SPE(L_SPE)%USED(SPE(L_SPE)%L_NOD) = .FALSE.
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL DATE_TO_TIME ( BUF(J3)(IND(1,J4):IND(2,J4)), &
     &                     SPE(L_SPE)%MJD(SPE(L_SPE)%L_NOD), &
     &                     SPE(L_SPE)%TAI(SPE(L_SPE)%L_NOD), IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8742, IUER, 'PARSE_SPE', 'Error '// &
     &                         'in parsing keyword SPLINE_POS: the '// &
     &                         'value of the qualifier NODE keeps the '// &
     &                         'ill-formated date '// &
     &                          BUF(J3)(IND(1,J4):IND(2,J4)) )
                           DEALLOCATE ( SPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
                      IF ( SPE(L_SPE)%L_NOD > 1 ) THEN
                           IF ( (SPE(L_SPE)%MJD(SPE(L_SPE)%L_NOD) - &
     &                           SPE(L_SPE)%MJD(SPE(L_SPE)%L_NOD-1))*86400.0D0 + &
     &                          (SPE(L_SPE)%TAI(SPE(L_SPE)%L_NOD) - &
     &                           SPE(L_SPE)%TAI(SPE(L_SPE)%L_NOD-1) ) < 0.0D0 ) THEN
                                 CALL ERR_LOG ( 8743, IUER, 'PARSE_SPE', &
     &                               'Error in parsing keyword SPLINE_POS: '//&
     &                               'the dates of the nodes for station '// &
     &                                SPE(L_SPE)%STATION//' are out '// &
     &                               'of order: '// &
     &                               BUF(J3)(IND(1,J4):IND(2,J4)) )
                                  DEALLOCATE ( SPE ) 
                                  DEALLOCATE ( BUF ) 
                                  RETURN 
                           END IF
                      END IF
                      SPE(L_SPE)%MULT(SPE(L_SPE)%L_NOD) = -1
                      NEXT = 'MULT'
                      FL_NODE = .FALSE.
                      FL_MULT = .FALSE.
                 END IF
               ELSE IF ( NEXT == 'MULT' ) THEN
!
! =======================
!
                 IF ( .NOT. FL_MULT ) THEN
                      IF ( BUF(J3)(IND(1,J4):IND(2,J4)) == 'MULT' ) THEN
                           FL_MULT = .TRUE.
                         ELSE 
                           CALL ERR_LOG ( 8744, IUER, 'PARSE_SPE', 'Error '// &
     &                         'in parsing keyword SPLINE_POS: the '// &
     &                         'qualifier MULT was expected, but "'// &
     &                          BUF(J3)(IND(1,J4):IND(2,J4))//'" was found' )
                           DEALLOCATE ( SPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
                    ELSE 
                      IOS = 0
                      READ ( UNIT=BUF(J3)(IND(1,J4):IND(2,J4)), FMT='(I9)', &
     &                       IOSTAT=IOS ) SPE(L_SPE)%MULT(SPE(L_SPE)%L_NOD)
                      IF ( IOS .NE. 0 ) THEN
                           CALL ERR_LOG ( 8745, IUER, 'PARSE_SPE', 'Error '// &
     &                         'in parsing keyword SPLINE_POS: the '// &
     &                         'value of the qualifier MULT keeps the '// &
     &                         'ill-formated value '// &
     &                          BUF(J3)(IND(1,J4):IND(2,J4)) )
                           DEALLOCATE ( SPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
                      IF ( SPE(L_SPE)%MULT(SPE(L_SPE)%L_NOD) > &
     &                     SPE(L_SPE)%DEGREE ) THEN
                           CALL ERR_LOG ( 8746, IUER, 'PARSE_SPE', 'Error '// &
     &                         'in parsing keyword SPLINE_POS: the '// &
     &                         'value of the qualifier MULT '// &
     &                          BUF(J3)(IND(1,J4):IND(2,J4))//' is out '// &
     &                         'of range [0, DEGREE]' )
                           DEALLOCATE ( SPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
                      IF ( SPE(L_SPE)%L_NOD == 1 .AND. &
     &                     SPE(L_SPE)%MULT(SPE(L_SPE)%L_NOD) .NE. SPE(L_SPE)%DEGREE ) THEN
                           CALL ERR_LOG ( 8747, IUER, 'PARSE_SPE', 'Error '// &
     &                         'in parsing keyword SPLINE_POS: the '// &
     &                         'value of the qualifier MULT is '// &
     &                          BUF(J3)(IND(1,J4):IND(2,J4))// &
     &                         ' -- the multiplicity of the first node '// &
     &                         'should be the same as degree' )
                           DEALLOCATE ( SPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
                      DO 460 J6=0,SPE(L_SPE)%MULT(SPE(L_SPE)%L_NOD)
                         SPE(L_SPE)%K_NOD = SPE(L_SPE)%K_NOD + 1
                         SPE(L_SPE)%TIM(SPE(L_SPE)%K_NOD) = &
     &                          (SPE(L_SPE)%MJD(SPE(L_SPE)%L_NOD)-J2000__MJD)*86400.0D0 + &
     &                          (SPE(L_SPE)%TAI(SPE(L_SPE)%L_NOD)-43200.0D0) + &
     &                          (SPE(L_SPE)%K_NOD + SPE(L_SPE)%DEGREE)*1.D-3
                         SPE(L_SPE)%USED(SPE(L_SPE)%K_NOD) = .FALSE.
 460                  CONTINUE 
                      NEXT = 'CNS_STA'
                      FL_MULT    = .FALSE.
                      FL_CNS_STA = .FALSE.
                 END IF
               ELSE IF ( NEXT == 'CNS_STA' ) THEN
!
! =======================
!
                 IF ( .NOT. FL_CNS_STA ) THEN
                        IF ( BUF(J3)(IND(1,J4):IND(2,J4)) == 'CNS_STA_SIGMA' ) THEN
                           FL_CNS_STA = .TRUE.
                         ELSE IF ( BUF(J3)(IND(1,J4):IND(2,J4)) == 'NODE' ) THEN
                           NEXT = 'NODE'
                           FL_NODE = .TRUE.
                         ELSE 
                           CALL ERR_LOG ( 8748, IUER, 'PARSE_SPE', 'Error '// &
     &                         'in parsing keyword SPLINE_POS: the '// &
     &                         'qualifier CNS_STA_SIGMA was expected, but "'// &
     &                          BUF(J3)(IND(1,J4):IND(2,J4))//'" was found' )
                           DEALLOCATE ( SPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
                    ELSE
                      IOS = 0
                      READ ( UNIT=BUF(J3)(IND(1,J4):IND(2,J4)), FMT='(F22.12)', &
     &                       IOSTAT=IOS ) SPE(L_SPE)%CNS_STA_SIGMA
                      IF ( IOS .NE. 0 ) THEN
                           CALL ERR_LOG ( 8749, IUER, 'PARSE_SPE', 'Error '// &
     &                         'in parsing keyword SPLINE_POS: the '// &
     &                         'value of the qualifier CNS_STA_SIGMA is '// &
     &                         'ill-formated: '//BUF(J3)(IND(1,J4):IND(2,J4)) )
                           DEALLOCATE ( SPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
!
                      NEXT = 'CNS_VEL'
                      FL_CNS_STA = .FALSE.
                      FL_CNS_VEL = .FALSE.
                 END IF
               ELSE IF ( NEXT == 'CNS_VEL' ) THEN
!
! =======================
!
                 IF ( .NOT. FL_CNS_VEL ) THEN
                      IF ( BUF(J3)(IND(1,J4):IND(2,J4)) == 'CNS_VEL_SIGMA' ) THEN
                           FL_CNS_VEL = .TRUE.
                         ELSE 
                           CALL ERR_LOG ( 8750, IUER, 'PARSE_SPE', 'Error '// &
     &                         'in parsing keyword SPLINE_POS: the '// &
     &                         'qualifier CNS_VEL_SIGMA was expected, but "'// &
     &                          BUF(J3)(IND(1,J4):IND(2,J4))//'" was found' )
                           DEALLOCATE ( SPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
                    ELSE
                      IOS = 0
                      READ ( UNIT=BUF(J3)(IND(1,J4):IND(2,J4)), FMT='(F22.12)', &
     &                       IOSTAT=IOS ) SPE(L_SPE)%CNS_VEL_SIGMA
                      IF ( IOS .NE. 0 ) THEN
                           CALL ERR_LOG ( 8751, IUER, 'PARSE_SPE', 'Error '// &
     &                         'in parsing keyword SPLINE_POS: the '// &
     &                         'value of the qualifier CNS_VEL_SIGMA is '// &
     &                         'ill-formated: '//BUF(J3)(IND(1,J4):IND(2,J4)) )
                           DEALLOCATE ( SPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
!
                      NEXT = 'CNS_DER'
                      FL_CNS_VEL = .FALSE.
                      FL_CNS_DER = .FALSE.
                 END IF
               ELSE IF ( NEXT == 'CNS_DER' ) THEN
!
! =======================
!
                 IF ( .NOT. FL_CNS_DER ) THEN 
                      IF ( BUF(J3)(IND(1,J4):IND(2,J4)) == 'CNS_DER_SIGMA' ) THEN
                           FL_CNS_DER = .TRUE.
                           IWORD = 1
                         ELSE 
                           CALL ERR_LOG ( 8752, IUER, 'PARSE_SPE', 'Error '// &
     &                         'in parsing keyword SPLINE_POS: the '// &
     &                         'qualifier CNS_DER_SIGMA was expected, but "'// &
     &                          BUF(J3)(IND(1,J4):IND(2,J4))//'" was found' )
                           DEALLOCATE ( SPE ) 
                           DEALLOCATE ( BUF ) 
                           RETURN 
                      END IF
                    ELSE 
                      IF ( IWORD == 1 ) THEN
                           IOS = 0
                           READ ( UNIT=BUF(J3)(IND(1,J4):IND(2,J4)), FMT='(I9)', &
     &                            IOSTAT=IOS ) KDEG
                           IF ( KDEG < 0 .OR. KDEG > SPE(L_SPE)%DEGREE ) THEN
                                CALL ERR_LOG ( 8753, IUER, 'PARSE_SPE', &
     &                              'Error in parsing keyword SPLINE_POS: '// &
     &                              'the degree of the constraints on '// &
     &                              'spline derivateive '// &
     &                               BUF(J3)(IND(1,J4):IND(2,J4))// &
     &                              ' out of range' )
                                RETURN 
                           END IF
                           IWORD  = 2
                         ELSE IF ( IWORD == 2 ) THEN
                           READ ( UNIT=BUF(J3)(IND(1,J4):IND(2,J4)), FMT='(F22.12)', &
     &                            IOSTAT=IOS ) SPE(L_SPE)%CNS_DER_SIGMA(KDEG)
                           IF ( IOS .NE. 0 ) THEN
                                CALL ERR_LOG ( 8754, IUER, 'PARSE_SPE', 'Error '// &
     &                              'in parsing keyword SPLINE_POS: the '// &
     &                              'value of the qualifier CNS_DER_SIGMA '// &
     &                              'is ill-formated: '// &
     &                              BUF(J3)(IND(1,J4):IND(2,J4)) )
                                DEALLOCATE ( SPE ) 
                                DEALLOCATE ( BUF ) 
                                RETURN 
                           END IF
!
                           NEXT = 'STATION'
                           FL_CNS_DER = .FALSE.
                           FL_STATION = .FALSE.
                      END IF
                 END IF
            END IF
 440     CONTINUE 
         IF ( FL_END ) GOTO 830
 430  CONTINUE 
 830  CONTINUE 
!
      IF ( L_SPE .GT. 0 ) THEN
           DO 470 J7=1,L_SPE
              DO 480 J8=J7+1,L_SPE
                 IF ( SPE(J7)%STATION == SPE(J8)%STATION ) THEN
                      CALL ERR_LOG ( 8756, IUER, 'PARSE_SPE', 'Error '// &
     &                    'in parsing keyword SPLINE_POS: statsion '// &
     &                     SPE(J7)%STATION//' is defined more than once' )
                      DEALLOCATE ( SPE ) 
                      DEALLOCATE ( BUF ) 
                      RETURN 
                 END IF
 480          CONTINUE 
 470       CONTINUE 
      END IF
!
      SIZE_SPE = SIZEOF(SPE)
      DEALLOCATE ( BUF ) 
!
#ifdef HPUX
!
! --- Buggy compiler HPUX ignores operator SAVE and deallocates HPE object
! --- upon return. Damn it!
!
      CALL ERR_PASS  ( IUER, IER )
      CALL GRAB_MEM  ( IER, MEM_LEN, MEM_ADR, 1, INT8(SIZE_SPE), ADR_SPE )
      CALL LIB$MOVC3 ( SIZE_SPE, SPE, %VAL(ADR_SPE) )
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) ' SIZE_SPE=',SIZE_SPE
           CALL ERR_LOG ( 8757, IUER, 'PARSE_SPE', 'Error in an '// &
     &         'an attempt to allocate dynamic memory' )
           RETURN 
      END IF
#else
      ADR_SPE  = LOC(SPE)
#endif
!
! --- Write down object SPE into the scratch file.
!
      FILOUT = PRE_SCR_DIR(1:PRE_SD_LEN)//'ESPE'//PRE_LETRS
      INQUIRE ( FILE=FILOUT, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL UNLINK ( FILOUT(1:I_LEN(FILOUT))//CHAR(0) )
      END IF
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FILOUT, 'NEW', LUN, IER  )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8757, IUER, 'PARSE_SPE', 'Failure in an attempt '// &
     &         'to open output file '//FILOUT )
           RETURN 
      END IF
!
      CALL ERR_PASS     ( IUER, IER )
      CALL WRBIN_ARRAY  ( LUN, 'I4', 1, L_SPE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8758, IUER, 'PARSE_SPE', 'Failure in '// &
     &         'writing into the output file '//FILOUT )
           RETURN 
      END IF
!
      CALL ERR_PASS     ( IUER, IER )
      CALL WRBIN_ARRAY  ( LUN, 'I4', 1, SIZE_SPE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8758, IUER, 'PARSE_SPE', 'Failure in '// &
     &         'writing into the output file '//FILOUT )
           RETURN 
      END IF
!
      CALL ERR_PASS     ( IUER, IER )
      CALL WRBIN_ARRAY  ( LUN, 'B1', SIZE_SPE, %VAL(ADR_SPE), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8759, IUER, 'PARSE_SPE', 'Failure in '// &
     &         'writing into the output file '//FILOUT )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8760, IUER, 'PARSE_SPE', 'Failure in '// &
     &         'closing the output file '//FILOUT )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PARSE_SPE
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PRINT_SPE ( LUN, L_SPE, SPE )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PRINT_SPE prints contents of HPE object in      *
! *   logical unit LUN.                                                  *
! *                                                                      *
! *  ### 24-FEB-2005   PRINT_SPE   v1.0 (c)  L. Petrov  24-FEB-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INTEGER*4  LUN, L_SPE
      TYPE ( SPE__TYPE ) SPE(L_SPE)
      INTEGER*4  J1, J2, J3, J4
!
      WRITE  ( LUN, '(A)' ) 'Estimation of site position using splines'
      DO 410 J1=1,L_SPE
         WRITE  ( LUN, '(A)' ) '------------------------------------------'
         WRITE  ( LUN, 110 ) J1, SPE(J1)%STATION, '  DEGREE', SPE(J1)%DEGREE
         WRITE  ( LUN, 110 ) J1, SPE(J1)%STATION, '   L_NOD', SPE(J1)%L_NOD
 110     FORMAT ( 'SPE(',I3,') ',A, 2X,A, ': ', I4 )
         DO 420 J2=1,SPE(J1)%L_NOD
            WRITE  ( LUN, 120 ) J1, SPE(J1)%STATION, J2, '          MJD', &
     &                              SPE(J1)%MJD(J2)
            WRITE  ( LUN, 130 ) J1, SPE(J1)%STATION, J2, '          TAI', &
     &                              SPE(J1)%TAI(J2)
            WRITE  ( LUN, 120 ) J1, SPE(J1)%STATION, J2, '         MULT', &
     &                              SPE(J1)%MULT(J2)
 120        FORMAT ( 'SPE(',I3,') ',A,' Node: ', I4, 2X, A,': ', I5  )
 130        FORMAT ( 'SPE(',I3,') ',A,' Node: ', I4, 2X, A,': ', 1PD23.16 )
 420     CONTINUE 
         WRITE  ( LUN, 140 ) J1, SPE(J1)%STATION, 'CNS_STA_SIGMA', &
     &                           SPE(J1)%CNS_STA_SIGMA
         WRITE  ( LUN, 140 ) J1, SPE(J1)%STATION, 'CNS_VEL_SIGMA', &
     &                           SPE(J1)%CNS_VEL_SIGMA
 140     FORMAT ( 'SPE(', I3, ') ', A, 2X, A,': ', 1PD23.16 )
         DO 430 J3=0,SPE(J1)%DEGREE
            WRITE  ( LUN, 150 ) J1, SPE(J1)%STATION, 'CNS_DER', J3,  &
     &                              SPE(J1)%CNS_DER_SIGMA(J3)
 150        FORMAT ( 'SPE(',I3,') ',A, 2X, A,I1,': ', 1PD23.16 )
 430     CONTINUE 
         WRITE  ( LUN, 160 ) J1, SPE(J1)%STATION, 'Used: ', SPE(J1)%USED(1-M__SPD:SPE(J1)%L_NOD)
 160     FORMAT ( 'SPE(',I3,') ',A, 2X, A, 16(L1,1X) )
 410  CONTINUE 
      WRITE  ( LUN, '(A)' ) '------------------------------------------'
!
      RETURN
      END  SUBROUTINE PRINT_SPE
