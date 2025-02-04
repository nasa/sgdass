      SUBROUTINE PARSE_ERM_CNT ( STRING, L_ERM, ADR_ERM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PARSE_ERM_CNT parses the portion of the batch control      *
! *   file which is related to ERM keyword. The routine parses the       *
! *   portion of the file according to specifications, allocates         *
! *   memory for ERM object and loads with the fields determined from    *
! *   parsing the control file.                                          *
! *                                                                      *
! *   PARSE_ERM_CNT supports @-expansion inside the keyword ERM.         *
! *   It reads @-files and inserts their contents into the buffer.       *
! *                                                                      *
! *   The internal buffers are freed after completion ERM.               *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    L_ERM ( INTEGER*4 ) -- The size of ERM object in bytes, or zero   *
! *                           if no ERM estimation is requrested.        *
! *  ADR_ERM ( INTEGER*4 ) -- The address of the data structure          *
! *                           associated with ERM object. It conatins    *
! *                           parameters related to estimation of the    *
! *                           Earth rotation model.                      *
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
! * ### 18-JAN-2006 PARSE_ERM_CNT  v2.4 (c)  L. Petrov  04-OCT-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'precm.i'
      INCLUDE   'erm.i'
      CHARACTER  STRING*(*)
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      INTEGER*4  L_ERM, IUER
      ADDRESS__TYPE :: ADR_ERM
      REAL*8       SPAN_MIN, SPAN_OVD, TIM_EPS
      PARAMETER  ( SPAN_MIN = 1000.0D0 )
      PARAMETER  ( SPAN_OVD = 0.02D0   )
      PARAMETER  ( TIM_EPS  = 1.0D-3   )
      CHARACTER  STR*512, STR1*128, STR2*128, NEXT*32, &
     &           NAME_EXPAND*128, REG*4, FILOUT*128
      REAL*8     TIM_INT, TIM_BEG, TIM_END, TIM_TRY, &
     &           TIM_CNS_BEG, TIM_CNS_END, TAI_ORIG
      INTEGER*4    MBUF, MIND
      PARAMETER  ( MBUF = 1024 )
      PARAMETER  ( MIND =   32 )
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9)//'\' )
      TYPE ( ERM__TYPE ), POINTER :: ERM
      SAVE   ERM
      INTEGER*2  LN
      INTEGER*8        MEM_LEN
      ADDRESS__TYPE :: MEM_ADR
      INTEGER*4  IOS, J1, J2, J3, J4, IDEG, ICMP, IAT, IP, IL, IDAY, NBUF, &
     &           LIND, IND(2,MIND), SIZE_ERM, LUN, KNOT_CNS_BEG, KNOT_CNS_END, &
     &           MJD_ORIG, IER
      LOGICAL*4  FL_DEG(3), FL_EST_SPAN(3), FL_CNS_SPAN(3), LEX, &
     &           FL_MEAN_RATE_CNS_ADJ_BEG, FL_MEAN_RATE_CNS_ADJ_END
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      INTEGER*2, EXTERNAL :: CFREAD
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      FL_MEAN_RATE_CNS_ADJ_BEG = .FALSE.
      FL_MEAN_RATE_CNS_ADJ_END = .FALSE.
      STR = STRING
      CALL CHASHL  ( STR )
      IF ( STR(1:3) .EQ. 'NO '   .OR. &
     &     STR(1:5) .EQ. 'NONE '      ) THEN
!
! -------- Nothing to do
!
           L_ERM   = 0
           ADR_ERM = 0
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
           CALL ERR_LOG ( 8331, IUER, 'PARSE_ERM_CNT', 'Error in an attempt to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes' )
           RETURN
      END IF
!
! --- Allocate ERM object
!
      ALLOCATE ( ERM, STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 8332, IUER, 'PARSE_ERM_CNT', 'Failure to '// &
     &         'allocate in memory object ERM' )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
! --- Initialization
!
      CALL NOUT ( SIZEOF(ERM), ERM )
      ERM%FL_EST = .TRUE.
      ERM%FL_APL = .FALSE.
      ERM%FL_EQUIDISTANT = .TRUE.
      ERM%CNS_DER_SIGMA  = 0.0D0
      ERM%CNS_MEAN_SIGMA = 0.0D0
      ERM%CNS_RATE_SIGMA = 0.0D0
      ERM%CNS_MEAN_RTP   = 0.0D0
      ERM%CNS_RATE_RTP   = 0.0D0
!
      NBUF = 1
      BUF(NBUF) = STRING
      ICMP = 0
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
                   CALL ERR_LOG ( 8333, IUER, 'PARSE_ERM_CNT', 'Failure to'// &
     &                 ' expand line @'//NAME_EXPAND(1:I_LEN(NAME_EXPAND))// &
     &                 ' in processing keyword SPLINE_POS' )
                   DEALLOCATE ( BUF )
                   DEALLOCATE ( ERM )
                   RETURN
              END IF
         END IF
 410  CONTINUE
 810  CONTINUE
!
      CALL CLRCH ( STRING )
!
      FL_DEG(1) = .FALSE.
      FL_DEG(2) = .FALSE.
      FL_DEG(3) = .FALSE.
!
      FL_EST_SPAN(1) = .FALSE.
      FL_EST_SPAN(2) = .FALSE.
      FL_EST_SPAN(3) = .FALSE.
!
      FL_CNS_SPAN(1) = .FALSE.
      FL_CNS_SPAN(2) = .FALSE.
      FL_CNS_SPAN(3) = .FALSE.
!
      DO 420 J2=1,NBUF
         IL = I_LEN(BUF(J2))
         IF ( BUF(J2)(IL:IL) .EQ. '\' ) THEN
              CALL CLRCH ( BUF(J2)(IL:IL) )
         END IF
         IL = ILEN(BUF(J2))
         IF ( IL .LE. 0 ) GOTO 420
!
! ------ Split the command into words
!
         CALL EXWORD ( BUF(J2), MIND, LIND, IND, REG, -3 )
            IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'YES' ) THEN
                 CONTINUE
               ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'DEGREE' ) THEN
!
! ============== DEGREE
!
                 IF ( LIND < 3 ) THEN
                      CALL ERR_LOG ( 8334, IUER, 'PARSE_ERM_CNT', 'No value '// &
     &                    'of qualifier DEGREE of the keyword ERM was found' )
                      RETURN
                 END IF
!
                 IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E1' ) THEN
                      ICMP = 1
                   ELSE IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E2' ) THEN
                      ICMP = 2
                   ELSE IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E3' ) THEN
                      ICMP = 3
                   ELSE IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8335, IUER, 'PARSE_ERM_CNT', 'Unsupported '// &
     &                    'first value '//BUF(J2)(IND(1,2):IND(2,2))// &
     &                    ' of the qualifier CNS_DER_SIGMA of the keyword '// &
     &                    'ERM was found: E1 or E2 or E3 was expected' )
                      RETURN
                 END IF
!
                 READ ( UNIT=BUF(J2)(IND(1,3):IND(2,3)), FMT='(I8)', &
     &                  IOSTAT=IER ) ERM%DEGREE(ICMP)
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8336, IUER, 'PARSE_ERM_CNT', 'Failure '// &
     &                    'to decode value '//BUF(J2)(IND(1,2):IND(2,2))// &
     &                    ' of the qualifier DEGREE of the keyword ERM' )
                      RETURN
                 END IF
                 FL_DEG(ICMP) = .TRUE.
               ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'SPAN_EST_DAYS' ) THEN
!
! ============== SPAN_EST_DAYS
!
                 IF ( LIND < 2 ) THEN
                      CALL ERR_LOG ( 8337, IUER, 'PARSE_ERM_CNT', 'No value '// &
     &                    'of qualifier DEGREE of the keyword ERM was found' )
                      RETURN
                 END IF
!
                 IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E1' ) THEN
                      ICMP = 1
                   ELSE IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E2' ) THEN
                      ICMP = 2
                   ELSE IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E3' ) THEN
                      ICMP = 3
                   ELSE IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8338, IUER, 'PARSE_ERM_CNT', 'Unsupported '// &
     &                    'first value '//BUF(J2)(IND(1,2):IND(2,2))// &
     &                    ' of the qualifier CNS_DER_SIGMA of the keyword '// &
     &                    'ERM was found: E1 or E2 or E3 was expected' )
                      RETURN
                 END IF
!
                 READ ( UNIT=BUF(J2)(IND(1,3):IND(2,3)), FMT='(F12.6)', &
     &                  IOSTAT=IER ) ERM%TIME_EST_SPAN(ICMP)
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8339, IUER, 'PARSE_ERM_CNT', 'Failure '// &
     &                    'to decode value '//BUF(J2)(IND(1,2):IND(2,2))// &
     &                    ' of the qualifier SPAN_DAYS of the keyword ERM' )
                      RETURN
                 END IF
                 ERM%TIME_EST_SPAN(ICMP) = 86400.0D0*ERM%TIME_EST_SPAN(ICMP)
                 FL_EST_SPAN(ICMP) = .TRUE.
               ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'SPAN_CNS_DAYS' ) THEN
!
! ============== SPAN_CNS_DAYS
!
                 IF ( LIND < 2 ) THEN
                      CALL ERR_LOG ( 8337, IUER, 'PARSE_ERM_CNT', 'No value '// &
     &                    'of qualifier DEGREE of the keyword ERM was found' )
                      RETURN
                 END IF
!
                 IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E1' ) THEN
                      ICMP = 1
                   ELSE IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E2' ) THEN
                      ICMP = 2
                   ELSE IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E3' ) THEN
                      ICMP = 3
                   ELSE IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8338, IUER, 'PARSE_ERM_CNT', 'Unsupported '// &
     &                    'first value '//BUF(J2)(IND(1,2):IND(2,2))// &
     &                    ' of the qualifier CNS_DER_SIGMA of the keyword '// &
     &                    'ERM was found: E1 or E2 or E3 was expected' )
                      RETURN
                 END IF
!
                 READ ( UNIT=BUF(J2)(IND(1,3):IND(2,3)), FMT='(F12.6)', &
     &                  IOSTAT=IER ) ERM%TIME_CNS_SPAN(ICMP)
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8339, IUER, 'PARSE_ERM_CNT', 'Failure '// &
     &                    'to decode value '//BUF(J2)(IND(1,2):IND(2,2))// &
     &                    ' of the qualifier SPAN_DAYS of the keyword ERM' )
                      RETURN
                 END IF
                 ERM%TIME_CNS_SPAN(ICMP) = 86400.0D0*ERM%TIME_CNS_SPAN(ICMP)
                 FL_CNS_SPAN(ICMP) = .TRUE.
               ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'DATE_BEG' ) THEN
!
! ============== DATE_BEG
!
                 IF ( LIND < 2 ) THEN
                      CALL ERR_LOG ( 8340, IUER, 'PARSE_ERM_CNT', 'No value '// &
     &                    'of qualifier DATE_BEG of the keyword ERM was found' )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL DATE_TO_TIME ( BUF(J2)(IND(1,2):IND(2,2)), &
     &                               ERM%MJD_BEG, ERM%TAI_BEG, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8341, IUER, 'PARSE_ERM_CNT', 'Failure '// &
     &                    'to decode value '//BUF(J2)(IND(1,2):IND(2,2))// &
     &                    ' of the qualifier DATE_BEG of the keyword ERM' )
                      RETURN
                 END IF
               ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'DATE_END' ) THEN
!
! ============== DATE_END
!
                 IF ( LIND < 2 ) THEN
                      CALL ERR_LOG ( 8342, IUER, 'PARSE_ERM_CNT', 'No value '// &
     &                    'of qualifier DATE_END of the keyword ERM was found' )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL DATE_TO_TIME ( BUF(J2)(IND(1,2):IND(2,2)), &
     &                               ERM%MJD_END, ERM%TAI_END, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8343, IUER, 'PARSE_ERM_CNT', 'Failure '// &
     &                    'to decode value '//BUF(J2)(IND(1,2):IND(2,2))// &
     &                    ' of the qualifier DATE_END of the keyword ERM' )
                      RETURN
                 END IF
               ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'CNS_DER_SIGMA' ) THEN
!
! ============== CNS_DER_SIGMA
!
                 IF ( ICMP == 0 ) THEN
                      CALL ERR_LOG ( 8344, IUER, 'PARSE_ERM_CNT', 'Degree '// &
     &                    'of the B-spline expansion was not specified' )
                      RETURN
                 END IF
                 IF ( .NOT. FL_DEG(ICMP) ) THEN
                      CALL ERR_LOG ( 8345, IUER, 'PARSE_ERM_CNT', 'Wrong order '// &
     &                    'of qualifiers: the qualifier DEGREE was not '// &
     &                    'supplied before the qualifier CNS_DER_SIGMA' )
                      RETURN
                 END IF
                 IF ( LIND < 2 ) THEN
                      CALL ERR_LOG ( 8346, IUER, 'PARSE_ERM_CNT', 'No value '// &
     &                    'of qualifier CNS_DER_SIGMA of the keyword ERM '// &
     &                    'was found' )
                      RETURN
                 END IF
                 IF ( LIND < 4 ) THEN
                      CALL ERR_LOG ( 8347, IUER, 'PARSE_ERM_CNT', 'Not enough '// &
     &                    'values of the qualifier CNS_DER_SIGMA of the '// &
     &                    'keyword ERM was found: three values were expected' )
                      RETURN
                 END IF
!
                 IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E1' ) THEN
                      ICMP = 1
                   ELSE IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E2' ) THEN
                      ICMP = 2
                   ELSE IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E3' ) THEN
                      ICMP = 3
                   ELSE IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8348, IUER, 'PARSE_ERM_CNT', 'Unsupported '// &
     &                    'first value '//BUF(J2)(IND(1,2):IND(2,2))// &
     &                    ' of the qualifier CNS_DER_SIGMA of the keyword '// &
     &                    'ERM was found: E1 or E2 or E3 was expected' )
                      RETURN
                 END IF
!
                 READ ( UNIT=BUF(J2)(IND(1,3):IND(2,3)), FMT='(I8)', &
     &                  IOSTAT=IER ) IDEG
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8349, IUER, 'PARSE_ERM_CNT', 'Failure '// &
     &                    'to decode the second value '// &
     &                     BUF(J2)(IND(1,3):IND(2,3))// &
     &                    ' of the qualifier CNS_DER_SIGMA of the keyword ERM' )
                      RETURN
                 END IF
                 IF ( IDEG < 0 .OR. IDEG > ERM%DEGREE(ICMP) ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( IDEG, STR )
                      CALL ERR_LOG ( 8350, IUER, 'PARSE_ERM_CNT', 'Wrong '// &
     &                    'the second value of valufuer CNS_DER_SIGMA '// &
     &                    'of keyword ERM: '//BUF(J2)(IND(1,3):IND(2,3))// &
     &                    ' a value in the range 0, '//STR(1:I_LEN(STR))// &
     &                    ' was expected' )
                      RETURN
                 END IF
!
                 READ ( UNIT=BUF(J2)(IND(1,4):IND(2,4)), FMT='(F12.6)', &
     &                  IOSTAT=IER ) ERM%CNS_DER_SIGMA(IDEG,ICMP)
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8351, IUER, 'PARSE_ERM_CNT', 'Failure '// &
     &                    'to decode the third value '// &
     &                     BUF(J2)(IND(1,4):IND(2,4))// &
     &                    ' of the qualifier DEGREE of the keyword ERM' )
                      RETURN
                 END IF
               ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'CNS_MEAN_SIGMA' ) THEN
                 IF ( LIND < 3 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( LIND, STR )
                      CALL ERR_LOG ( 8352, IUER, 'PARSE_ERM_CNT', 'Too few words '// &
     &                    'for CNS_MEAN_SIGMA definition: at least 3 words were expected, '// &
     &                    'but only '//TRIM(STR)//' found' )
                      RETURN
                 END IF
!
                 IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E1' ) THEN
                      ICMP = 1
                   ELSE IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E2' ) THEN
                      ICMP = 2
                   ELSE IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E3' ) THEN
                      ICMP = 3
                   ELSE IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8353, IUER, 'PARSE_ERM_CNT', 'Unsupported '// &
     &                    'first value '//BUF(J2)(IND(1,2):IND(2,2))// &
     &                    ' of the qualifier CNS_MEAN_SIGMA of the keyword '// &
     &                    'ERM was found: E1 or E2 or E3 was expected' )
                      RETURN
                 END IF
                 READ ( UNIT=BUF(J2)(IND(1,LIND):IND(2,LIND)), FMT='(D20.12)' ) ERM%CNS_MEAN_SIGMA(ICMP)
               ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'CNS_RATE_SIGMA' ) THEN
                 IF ( LIND < 3 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( LIND, STR )
                      CALL ERR_LOG ( 8354, IUER, 'PARSE_ERM_CNT', 'Too few words '// &
     &                    'for CNS_RATE_SIGMA definition: at least 3 words were expected, '// &
     &                    'but only '//TRIM(STR)//' found' )
                      RETURN
                 END IF
!
                 IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E1' ) THEN
                      ICMP = 1
                   ELSE IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E2' ) THEN
                      ICMP = 2
                   ELSE IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E3' ) THEN
                      ICMP = 3
                   ELSE IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8355, IUER, 'PARSE_ERM_CNT', 'Unsupported '// &
     &                    'first value '//BUF(J2)(IND(1,2):IND(2,2))// &
     &                    ' of the qualifier CNS_RATE_SIGMA of the keyword '// &
     &                    'ERM was found: E1 or E2 or E3 was expected' )
                      RETURN
                 END IF
                 READ ( UNIT=BUF(J2)(IND(1,LIND):IND(2,LIND)), FMT='(D20.12)' ) ERM%CNS_RATE_SIGMA(ICMP)
               ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'CNS_TREND_RANGE'   ) THEN
                 IF ( LIND < 3 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( LIND, STR )
                      CALL ERR_LOG ( 8356, IUER, 'PARSE_ERM_CNT', 'Too few words '// &
     &                    'for CNS_TREND_RANGE definition: 3 words were expected, '// &
     &                    'but only '//TRIM(STR)//' found' )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL DATE_TO_TIME ( BUF(J2)(IND(1,2):IND(2,2)), ERM%MJD_BEG_RANGE_CNS, &
     &                               ERM%TAI_BEG_RANGE_CNS, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8357, IUER, 'PARSE_ERM_CNT', 'Error in parsing '// &
     &                    'the second word of the CNS_TREND_RANGE keyword: wrong date '// &
     &                    'format' )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL DATE_TO_TIME ( BUF(J2)(IND(1,3):IND(2,3)), ERM%MJD_END_RANGE_CNS, &
     &                               ERM%TAI_END_RANGE_CNS, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8358, IUER, 'PARSE_ERM_CNT', 'Error in parsing '// &
     &                    'the third word of the CNS_TREND_RANGE keyword: wrong date '// &
     &                    'format' )
                      RETURN
                 END IF
               ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'CNS_TREND_REF'   ) THEN
                 IF ( LIND < 2 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( LIND, STR )
                      CALL ERR_LOG ( 8359, IUER, 'PARSE_ERM_CNT', 'Too few words '// &
     &                    'for CNS_TREND_REF definition:  2 words were expected, '// &
     &                    'but only '//TRIM(STR)//' found' )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL DATE_TO_TIME ( BUF(J2)(IND(1,2):IND(2,2)), ERM%MJD_REF_CNS, &
     &                               ERM%TAI_REF_CNS, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8360, IUER, 'PARSE_ERM_CNT', 'Error in parsing '// &
     &                    'the second word of the CNS_TREND_REF keyword: wrong date '// &
     &                    'format' )
                      RETURN
                 END IF
               ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'CNS_MEAN_RTP'   ) THEN
                 IF ( LIND < 3 ) THEN
                      CALL ERR_LOG ( 8361, IUER, 'PARSE_ERM_CNT', 'No value '// &
     &                    'of qualifier CNS_MEAN_RTP of the keyword ERM was found' )
                      RETURN
                 END IF
!
                 IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E1' ) THEN
                      ICMP = 1
                   ELSE IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E2' ) THEN
                      ICMP = 2
                   ELSE IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E3' ) THEN
                      ICMP = 3
                   ELSE IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8362, IUER, 'PARSE_ERM_CNT', 'Unsupported '// &
     &                    'first value '//BUF(J2)(IND(1,2):IND(2,2))// &
     &                    ' of the qualifier CNS_MEAN_RTP of the keyword '// &
     &                    'ERM was found: E1 or E2 or E3 was expected' )
                      RETURN
                 END IF
                 READ ( UNIT=BUF(J2)(IND(1,LIND):IND(2,LIND)), FMT='(D20.12)' ) ERM%CNS_MEAN_RTP(ICMP)
               ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'CNS_RATE_RTP'   ) THEN
                 IF ( LIND < 3 ) THEN
                      CALL ERR_LOG ( 8363, IUER, 'PARSE_ERM_CNT', 'No value '// &
     &                    'of qualifier CNS_RATE_RTP of the keyword ERM was found' )
                      RETURN
                 END IF
!
                 IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E1' ) THEN
                      ICMP = 1
                   ELSE IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E2' ) THEN
                      ICMP = 2
                   ELSE IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'E3' ) THEN
                      ICMP = 3
                   ELSE IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8364, IUER, 'PARSE_ERM_CNT', 'Unsupported '// &
     &                    'first value '//BUF(J2)(IND(1,2):IND(2,2))// &
     &                    ' of the qualifier CNS_RATE_RTP of the keyword '// &
     &                    'ERM was found: E1 or E2 or E3 was expected' )
                      RETURN
                 END IF
                 READ ( UNIT=BUF(J2)(IND(1,LIND):IND(2,LIND)), FMT='(D20.12)' ) ERM%CNS_RATE_RTP(ICMP)
               ELSE
                 CALL ERR_LOG ( 8365, IUER, 'PARSE_ERM_CNT', 'Unsupported '// &
     &               'qualifier '//BUF(J2)(IND(1,1):IND(2,1))// &
     &               ' of the keyword ERM' )
                 RETURN
            END IF
 420  CONTINUE
!
      TIM_INT = (ERM%MJD_END - ERM%MJD_BEG)*86400.0D0 + &
     &          (ERM%TAI_END - ERM%TAI_BEG)
!
      DO 440 J4=1,3
         FL_MEAN_RATE_CNS_ADJ_BEG = .FALSE.
         FL_MEAN_RATE_CNS_ADJ_END = .FALSE.
         IF ( .NOT. FL_DEG(J4) ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J4, STR )
              CALL ERR_LOG ( 8366, IUER, 'PARSE_ERM_CNT', 'Degree was not '// &
     &            'specified for component E'//STR(1:1) )
              RETURN
         END IF
!
         IF ( .NOT. FL_EST_SPAN(J4) ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J4, STR )
              CALL ERR_LOG ( 8367, IUER, 'PARSE_ERM_CNT', 'Estimation time span '// &
     &            'was not specified for component E'//STR(1:1) )
              RETURN
         END IF
!
         IF ( .NOT. FL_CNS_SPAN(J4) ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J4, STR )
              CALL ERR_LOG ( 8367, IUER, 'PARSE_ERM_CNT', 'Constraint time span '// &
     &            'was not specified for component E'//STR(1:1) )
              RETURN
         END IF
!
         IF ( ERM%TIME_EST_SPAN(J4) < SPAN_MIN ) THEN
              CALL CLRCH ( STR )
              WRITE ( UNIT=STR(1:15),  FMT='(F15.2)' ) ERM%TIME_EST_SPAN(J4)
              WRITE ( UNIT=STR(21:27), FMT='(F7.1)'  ) SPAN_MIN
              CALL ERR_LOG ( 8368, IUER, 'PARSE_ERM_CNT', 'Wrong duration of '// &
     &            'the estimation span for ERM: '//STR(1:15)//' sec, while '// &
     &            'the minimum span is '//STR(21:27) )
              RETURN
         END IF
!
         IF ( ERM%TIME_CNS_SPAN(J4) < SPAN_MIN ) THEN
              CALL CLRCH ( STR )
              WRITE ( UNIT=STR(1:15),  FMT='(F15.2)' ) ERM%TIME_CNS_SPAN(J4)
              WRITE ( UNIT=STR(21:27), FMT='(F7.1)'  ) SPAN_MIN
              CALL ERR_LOG ( 8368, IUER, 'PARSE_ERM_CNT', 'Wrong duration of '// &
     &            'the constraint span for ERM: '//STR(1:15)//' sec, while '// &
     &            'the minimum span is '//STR(21:27) )
              RETURN
         END IF
!
         IF ( TIM_INT < ERM%TIME_EST_SPAN(J4) ) THEN
              CALL CLRCH ( STR )
              WRITE ( UNIT=STR(1:15),  FMT='(F15.2)' ) ERM%TIME_EST_SPAN(J4)
              CALL ERR_LOG ( 8369, IUER, 'PARSE_ERM_CNT', 'Too short interval '// &
     &            'between the first and the last epoch of ERM: less than '// &
     &            ' a time span '//STR(1:15)//' seconds' )
              RETURN
         END IF
!
! ------ Compute the number of knots. NB: rounding!
!
         ERM%NKNOTS(J4) = IDINT ( TIM_INT/ERM%TIME_EST_SPAN(J4) ) + 1
         TIM_INT = (ERM%MJD_END - ERM%MJD_BEG)*86400.0D0 + &
     &             (ERM%TAI_END - ERM%TAI_BEG)
!
         IF ( ERM%TIME_EST_SPAN(J4)*ERM%NKNOTS(J4) < &
     &        TIM_INT - SPAN_OVD*(ERM%TIME_EST_SPAN(J4)-1) ) THEN
!
! ----------- Put an extra knot, if the last knot goes a over 
! ----------- the end epoch of the EERM interval.
!
              ERM%NKNOTS(J4) = ERM%NKNOTS(J4) + 1
              ERM%TAI_END = ERM%TAI_END + ERM%TIME_EST_SPAN(J4)
              IDAY = IDINT ( ERM%TAI_END/86400.0D0 )
              ERM%MJD_END = ERM%MJD_END + IDAY
              ERM%TAI_END = ERM%TAI_END - 86400.0D0*IDAY
              IF ( G_WARNING ) THEN
                   STR = MJDSEC_TO_DATE ( ERM%MJD_END, ERM%TAI_END, IER )
                   WRITE ( 6, '(A,I1,A)' ) 'WARNING: Adjusted end epoch for component ',J4, &
     &                                      ' of the empirical Earth rotation model to '// &
     &                                      STR(1:19)
              END IF
              TIM_INT = (ERM%MJD_END - ERM%MJD_BEG)*86400.0D0 + &
     &                  (ERM%TAI_END - ERM%TAI_BEG)
         END IF
!
         IF ( ERM%NKNOTS(J4) > ERM__MKNOT ) THEN
              CALL CLRCH ( STR  )
              CALL CLRCH ( STR1 )
              CALL CLRCH ( STR2 )
              CALL INCH  ( ERM%NKNOTS(J4), STR  )
              CALL INCH  ( J4,             STR1 )
              CALL INCH  ( ERM__MKNOT,     STR2 )
              CALL ERR_LOG ( 8370, IUER, 'PARSE_ERM_CNT', 'Too many knots: '// &
     &             STR(1:I_LEN(STR))//' for component '//STR1(1:1)// &
     &            ' while the maximum ERM__MKNOT '// &
     &            'defined in erm.i is '//STR2 )
              RETURN
         END IF        
!
         IF ( ERM%CNS_MEAN_SIGMA(J4) > 0.0D0 .OR. ERM%CNS_RATE_SIGMA(J4) > 0.0D0 ) THEN
!
! ----------- Check the beg epoch of the MEAN or RATE constaint. It should not be
! ----------- earlier than the beg epoch of the ERM
!
              TIM_BEG = ERM%MJD_BEG*86400.0D0 + ERM%TAI_BEG*86400.0D0 
              TIM_TRY = ERM%MJD_BEG_RANGE_CNS*86400.0D0 + ERM%TAI_BEG_RANGE_CNS
              IF ( TIM_TRY < TIM_BEG - TIM_EPS ) THEN
                   ERM%MJD_BEG_RANGE_CNS = ERM%MJD_BEG
                   ERM%TAI_BEG_RANGE_CNS = ERM%TAI_BEG
                   FL_MEAN_RATE_CNS_ADJ_BEG = .TRUE.
              END IF
         END IF 
!
! ------ Check the end epoch of the MEAN or RATE constaint. It should not be
! ------ later than the last epoch of the ERM minus DEG*TIM_SPAN
!
         IF ( ERM%CNS_MEAN_SIGMA(J4) > 0.0D0 ) THEN
              IF ( (ERM%MJD_END_RANGE_CNS*86400.0D0 + ERM%TAI_END_RANGE_CNS) > &
     &             (ERM%MJD_END*86400.0D0 + ERM%TAI_END) - ERM%TIME_EST_SPAN(J4) + TIM_EPS ) THEN


                   ERM%MJD_END_RANGE_CNS = ERM%MJD_END
                   ERM%TAI_END_RANGE_CNS = ERM%TAI_END - ERM%TIME_EST_SPAN(J4)
                   IF ( ERM%TAI_END_RANGE_CNS < 0.0D0 ) THEN
                        IDAY = IDINT(-ERM%TAI_END_RANGE_CNS/86400.0D0 )
                        ERM%MJD_END_RANGE_CNS = ERM%MJD_END_RANGE_CNS - IDAY
                        ERM%TAI_END_RANGE_CNS = ERM%TAI_END_RANGE_CNS + IDAY*86400.D0
                   END IF
                   FL_MEAN_RATE_CNS_ADJ_END = .TRUE.
              END IF
         END IF
!
         IF ( FL_MEAN_RATE_CNS_ADJ_BEG .AND. G_WARNING ) THEN
              STR = MJDSEC_TO_DATE ( ERM%MJD_BEG_RANGE_CNS, ERM%TAI_BEG_RANGE_CNS, IER )
              WRITE ( 6, '(A)' ) 'WARNING: Adjusted start epoch for constraints'// &
     &                           ' on ERM mean and rate to '//STR(1:19)
         END IF
         IF ( FL_MEAN_RATE_CNS_ADJ_END .AND. G_WARNING ) THEN
              STR = MJDSEC_TO_DATE ( ERM%MJD_END_RANGE_CNS, ERM%TAI_END_RANGE_CNS, IER )
              WRITE ( 6, '(A)' ) 'WARNING: Adjusted end   epoch for constraints'// &
     &                           ' on ERM mean and rate to '//STR(1:19)
         END IF
 440  CONTINUE
!
#ifdef HPUX
!
! --- Buggy HPUX Fortran compiler ignores operator SAVE and deallocates HPE object
! --- upon return. Damn it!
!
      CALL ERR_PASS  ( IUER, IER )
      CALL GRAB_MEM  ( IER, MEM_LEN, MEM_ADR, 1, INT8(SIZE_ERM), ADR_ERM )
      CALL LIB$MOVC3 ( SIZE_ERM, ERM, %VAL(ADR_ERM) )
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) ' SIZE_ERM=',SIZE_ERM
           CALL ERR_LOG ( 8371, IUER, 'PARSE_ERM_CNT', 'Error in an '// &
     &         'an attempt to allocate dynamic memory' )
           DEALLOCATE ( BUF )
           DEALLOCATE ( ERM )
           RETURN
      END IF
#else
      ADR_ERM  = LOC(ERM)
#endif
      L_ERM = SIZEOF(ERM)
!
! --- Write down object ERM into the scratch file.
!
      FILOUT = PRE_SCR_DIR(1:PRE_SD_LEN)//'EERM'//PRE_LETRS
      INQUIRE ( FILE=FILOUT, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL UNLINK ( FILOUT(1:I_LEN(FILOUT))//CHAR(0) )
      END IF
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FILOUT, 'NEW', LUN, IER  )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8372, IUER, 'PARSE_ERM_CNT', 'Failure in an attempt '// &
     &         'to open output file '//FILOUT )
           RETURN 
      END IF
!
      CALL ERR_PASS     ( IUER, IER )
      CALL WRBIN_ARRAY  ( LUN, 'I4', 1, L_ERM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8373, IUER, 'PARSE_ERM_CNT', 'Failure in '// &
     &         'writing into the output file '//FILOUT )
           RETURN 
      END IF
!
      CALL ERR_PASS     ( IUER, IER )
      CALL WRBIN_ARRAY  ( LUN, 'B1', L_ERM, %VAL(ADR_ERM), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8374, IUER, 'PARSE_ERM_CNT', 'Failure in '// &
     &         'writing into the output file '//FILOUT )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8375, IUER, 'PARSE_ERM_CNT', 'Failure in '// &
     &         'closing the output file '//FILOUT )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PARSE_ERM_CNT  !#!#
