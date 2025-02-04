      SUBROUTINE PIMA_READ_GACO ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_READ_GACO
! *                                                                      *
! *  ### 25-FEB-2016  PIMA_READ_GACO  v1.0 (c) L. Petrov 25-FEB-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IUER
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 128 + 2*PIM__MSTA*PIM__MFRQ )
      PARAMETER  ( MIND =  32 )
      CHARACTER  BUF(MBUF)*128, STR*128, STR1*128
      INTEGER*8  FRQ_I8(PIM__MFRQ)
      REAL*8     GAIN_VAL, GACO_ERR, FRQ
      INTEGER*4  LIND, ISTA, IND_FRQ, IND_IF, NVIS, IND(2,MIND), NBUF, &
     &           J1, J2, J3, J4, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF 
      INTEGER*8, EXTERNAL :: IFIND_PL8
!
      IF ( ILEN(PIM%CONF%SPLT_GAIN_CORR_FILE) == 0 ) THEN
           CALL ERR_LOG ( 5851, IUER, 'PIMA_READ_GACO', 'Trap of internal '// &
     &         'control: gain correction file has not been specified' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL RD_TEXT  ( PIM%CONF%SPLT_GAIN_CORR_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5852, IUER, 'PIMA_READ_GACO', 'Error in reading '// &
     &         'input file with gain control '//PIM%CONF%SPLT_GAIN_CORR_FILE  )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(PIMA__GACO_LABEL)) == PIMA__GACO_LABEL ) THEN
           CONTINUE 
         ELSE IF ( BUF(1)(1:60) == '# PIMA Gain correction  v 1.0  Format verrsion of 2016.02.24' ) THEN
           CONTINUE 
         ELSE
           CALL CLRCH ( STR )
           CALL TRAN  ( 13, BUF(1), STR )
           CALL ERR_LOG ( 5853, IUER, 'PIMA_READ_GACO', 'Cannot recognised format '// &
     &         'of the gain control '// &
     &          PIM%CONF%SPLT_GAIN_CORR_FILE(1:I_LEN(PIM%CONF%SPLT_GAIN_CORR_FILE))// &
     &          ' -- the first line is '//STR(1:I_LEN(STR))//' while the format '// &
     &          PIMA__GACO_LABEL//' was expected' )
           RETURN 
      END IF
!
      CALL PIMA_GACO_INIT ( PIM, 1.0D0, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5854, IUER, 'PIMA_READ_GACO', 'Failure in an attempt '// &
     &         'to initialize GACO object' ) 
           RETURN 
      END IF
      FRQ_I8(1:PIM%NFRQ) = PIM%FRQ(1:PIM%NFRQ,PIM%CONF%FRQ_GRP)%FREQ_I8
!
      DO 410 J1=2,MBUF
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
         IF ( LIND < 12 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 5855, IUER, 'PIMA_READ_GACO', 'Failure in parsing '// &
     &            'the '//STR(1:I_LEN(STR))//'th line of the gain control file '// &
     &             PIM%CONF%SPLT_GAIN_CORR_FILE(1:I_LEN(PIM%CONF%SPLT_GAIN_CORR_FILE))// &
     &            ' -- it contains less than 12 words' ) 
              RETURN 
         END IF
!
         ISTA = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, BUF(J1)(IND(1,2):IND(2,2)) )
         IF ( ISTA < 1 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 5855, IUER, 'PIMA_READ_GACO', 'Failure in parsing '// &
     &            'the '//STR(1:I_LEN(STR))//'th line of the gain control file '// &
     &             PIM%CONF%SPLT_GAIN_CORR_FILE(1:I_LEN(PIM%CONF%SPLT_GAIN_CORR_FILE))// &
     &            ' station '//BUF(J1)(IND(1,2):IND(2,2))//' did not observe'// &
     &            ' in VLBI experient '//PIM%CONF%SESS_CODE )
              RETURN 
         END IF
!
         READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(I4)', IOSTAT=IER  ) IND_IF
         IF ( IND_IF < 1  .OR. IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 5856, IUER, 'PIMA_READ_GACO', 'Failure in parsing '// &
     &            'the '//STR(1:I_LEN(STR))//'th line of the gain control file '// &
     &             PIM%CONF%SPLT_GAIN_CORR_FILE(1:I_LEN(PIM%CONF%SPLT_GAIN_CORR_FILE))// &
     &            ' -- IF index '//BUF(J1)(IND(1,4):IND(2,4))// &
     &            ' was not used in experiment '// &
     &              PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//' '//&
     &            'band '//PIM%CONF%BAND )
              RETURN
         END IF
!
         READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F14.1)', IOSTAT=IER  ) FRQ
         IND_FRQ = IFIND_PL8 ( INT8(PIM%NFRQ), FRQ_I8, NINT ( FRQ, KIND=8 ) )
         IF ( IND_FRQ < 1 ) THEN
!@              CALL CLRCH ( STR )
!@              CALL INCH  ( J1, STR )
!@              CALL CLRCH ( STR1 )
!@              WRITE ( UNIT=STR1(1:15), FMT='(F15.1)' ) FRQ
!@              CALL ERR_LOG ( 5857, IUER, 'PIMA_READ_GACO', 'Failure in parsing '// &
!@     &            'the '//STR(1:I_LEN(STR))//'th line of the gain control file '// &
!@     &             PIM%CONF%SPLT_GAIN_CORR_FILE(1:I_LEN(PIM%CONF%SPLT_GAIN_CORR_FILE))// &
!@     &            ' -- frequency '//STR1(1:I_LEN(STR1))//' was not used in experiment '// &
!@     &              PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//' '//&
!@     &            'band '//PIM%CONF%BAND )
!@              RETURN
         END IF
!
         READ ( UNIT=BUF(J1)(IND(1,8):IND(2,8)),   FMT='(F8.4)', IOSTAT=IER  ) GAIN_VAL
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 5858, IUER, 'PIMA_READ_GACO', 'Failure in parsing '// &
     &            'the '//STR(1:I_LEN(STR))//'th line of the gain control file '// &
     &             PIM%CONF%SPLT_GAIN_CORR_FILE(1:I_LEN(PIM%CONF%SPLT_GAIN_CORR_FILE))// &
     &            ' -- the 8th word value should a non-negative float number' )
              RETURN
         END IF
!
         READ ( UNIT=BUF(J1)(IND(1,10):IND(2,10)), FMT='(F7.4)', IOSTAT=IER  ) GACO_ERR
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 5859, IUER, 'PIMA_READ_GACO', 'Failure in parsing '// &
     &            'the '//STR(1:I_LEN(STR))//'th line of the gain control file '// &
     &             PIM%CONF%SPLT_GAIN_CORR_FILE(1:I_LEN(PIM%CONF%SPLT_GAIN_CORR_FILE))// &
     &            ' -- the 10th word should a float number' )
              RETURN
         END IF
         READ ( UNIT=BUF(J1)(IND(1,12):IND(2,12)), FMT='(I8)',   IOSTAT=IER  ) NVIS
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 5860, IUER, 'PIMA_READ_GACO', 'Failure in parsing '// &
     &            'the '//STR(1:I_LEN(STR))//'th line of the gain control file '// &
     &             PIM%CONF%SPLT_GAIN_CORR_FILE(1:I_LEN(PIM%CONF%SPLT_GAIN_CORR_FILE))// &
     &            ' -- the 10th word should an integer number' )
              RETURN
         END IF
         PIM%GACO(ISTA)%IND_FREQ(IND_IF)  = IND_FRQ
         PIM%GACO(ISTA)%GAIN_CORR(IND_IF) = GAIN_VAL 
         PIM%GACO(ISTA)%GACO_ERR(IND_IF)  = GACO_ERR
         PIM%GACO(ISTA)%GACO_FRQ(IND_IF)  = FRQ
         PIM%GACO(ISTA)%NVIS(IND_IF)      = NVIS
 410  CONTINUE 
      PIM%GACO_STATUS = PIMA__LOADED
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_READ_GACO  !#!#
