      SUBROUTINE READ_EOPS ( FINAM, MASTER_DIR, IVRB, MSES, NSES, EOP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  READ_EOPS  reads input file in EOPS format, parses the    *
! *   records and puts the estiamtes of Earth orientation parameters and *
! *   their formal uncertainties into the fields of EOP object.          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       FINAM ( CHARACTER ) -- Input file name. It is assumed that the *
! *                              input file is in EOPS format.           *
! *  MASTER_DIR ( CHARACTER ) -- Directory name which contains IVS       *
! *                              sessions master files. The master files *
! *                              are needed for resolving session codes. *
! *        IVRB ( INTEGER*4 ) -- Verbosity level parameter.              *
! *                              IVRB = 0 -- silent mode.                *
! *                              IVRB = 1 -- progress counter is         *
! *                                          displayed during work.      *
! *        MSES ( INTEGER*4 ) -- Maximal number of data records expected *
! *                              in the file.                            *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *        NSES ( INTEGER*4 ) -- Actual number of data records read from *
! *                              the file.                               *
! *         EOP ( RECORD    ) -- Array of objects which keep information *
! *                              extracted from the EOPS file. Data      *
! *                              structure EOP is defied in              *
! *                           $PSOOLVE_ROOT/progs/solve/incldue/getpar.i *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
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
! *  ### 17-JUN-2002   READ_EOPS   v1.3 (c)  L. Petrov  14-OCT-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'ners.i'
      INCLUDE   'getpar.i'
      CHARACTER  FINAM*(*), MASTER_DIR*(*)
      INTEGER*4  MSES, NSES, IVRB, IUER
      TYPE ( EOP__STRU  ) :: EOP(MSES)
      TYPE ( NERS__TYPE ) :: NERS
      INTEGER*4  MIND
      PARAMETER  ( MIND = 64 )
      CHARACTER  STR*512, STR1*32, STR2*32, DATE_TAG*19, BUF_LS(M_LS)*128, &
     &           FMT_DATE*10, REG*3
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9) )
      REAL*8     MJD, UTC_VAL, UTC_M_TAI, VAL, TIM, EOP_VEC(3)
      LOGICAL*4  LEX
      INTEGER*4  LUN, LIND, IND(2,MIND), IOS(M_FLD_EOPS), MJD_VAL, J1, J2, IER
      CHARACTER, EXTERNAL :: JD_TO_DATE*23
      INTEGER*4, EXTERNAL :: FSTREAM, GET_UNIT, ILEN, I_LEN
!
! --- Inquire: whether the input file exists?
!
      NSES = 0
      INQUIRE ( FILE=FINAM, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 8611, IUER, 'READ_EOPS', 'Input file '// &
     &          FINAM(1:I_LEN(FINAM))//' is not found' )
           RETURN
      END IF
!
! --- Open input file
!
      LUN = GET_UNIT()
!
      OPEN ( UNIT=LUN, FILE=FINAM, STATUS='OLD', IOSTAT=IOS(1) )
      IF ( IOS(1) .NE. 0 ) THEN
           CALL CLRCH ( IOS(1), STR )
           CALL ERR_LOG ( 8612, IUER, 'READ_EOPS', 'Error '// &
     &                    STR(1:I_LEN(STR))//' in an attempt to open '// &
     &                   'input file '//FINAM )
           RETURN
      END IF
!
      READ ( UNIT=LUN, FMT='(A)', IOSTAT=IOS(1) ) STR
      IF ( STR(1:46) .EQ. SIG_EOP ) THEN
           FMT_DATE = STR(37:46)
         ELSE IF ( STR(1:46) .EQ. SIG_EOP1 ) THEN
           FMT_DATE = STR(37:46)
         ELSE IF ( STR(1:48) .EQ. SIG_EOP2 ) THEN
           FMT_DATE = STR(39:48)
         ELSE
           CALL ERR_LOG ( 8613, IUER, 'READ_EOPS', 'File '// &
     &          FINAM(1:I_LEN(FINAM))//' is not in the EOPS format. The '// &
     &          SIG_EOP//' or '//SIG_EOP1//' or '//SIG_EOP2//' were '// &
     &         'expected at the header' )
           CLOSE ( UNIT=LUN )
           RETURN
      END IF
!
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, '(A,A$)' ) '  READ_EOPS: Reading eops file ', CHAR(13)
      END IF
!
! --- Read the input EOPS file
!
      DO 410 J1=1,1024*1024*1024
!
! ------ Read the line
!
         READ ( UNIT=LUN, FMT='(A)', IOSTAT=IOS(1) ) STR
         IF ( IOS(1) .EQ. -1 ) THEN
              GOTO 810  ! That's it. File has been read to the end
            ELSE IF ( IOS(1) .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IOS(1), STR )
              CALL CLRCH ( STR1 )
              CALL INCH ( J1, STR1 )
              CALL ERR_LOG ( 8613, IUER, 'READ_EOPS', 'Failure to read '// &
     &            'input file '//FINAM(1:I_LEN(FINAM))//' -- error '// &
     &            STR(1:I_LEN(STR))//' during reading line '//STR1 )
              RETURN
         END IF
!
! ------ Clearing error status
!
         CALL NOUT_I4 ( M_FLD_EOPS, IOS )
!
         IF ( STR(1:1)  .EQ. '*' ) GOTO 410
         IF ( STR(1:1)  .EQ. '#' ) GOTO 410
         IF ( ILEN(STR) .EQ.  0  ) GOTO 410
!
! ------ Split the line onto words
!
         CALL EXWORD ( STR, MIND, LIND, IND, REG, -3 )
!
         IF ( FMT_DATE == '2016.06.22' ) THEN
!
! ----------- NERS style of the EOP format
!
              NSES = NSES+1
              CALL NOUT ( SIZEOF(EOP(NSES)), EOP(NSES) )
!
              READ ( UNIT=STR(IND(1,2):IND(2,2)), FMT=*, IOSTAT=IOS(01) ) TIM
              READ ( UNIT=STR(IND(1,5):IND(2,5)), FMT=*, IOSTAT=IOS(02) ) EOP_VEC(1)
              READ ( UNIT=STR(IND(1,6):IND(2,6)), FMT=*, IOSTAT=IOS(03) ) EOP_VEC(2)
              READ ( UNIT=STR(IND(1,7):IND(2,7)), FMT=*, IOSTAT=IOS(04) ) EOP_VEC(3)
              EOP(NSES)%MJD_EOP = TIM/86400.0D0 + J2000__MJD
              EOP(NSES)%XPL_V = ARCSEC__TO__RAD*EOP_VEC(2)
              EOP(NSES)%YPL_V = ARCSEC__TO__RAD*EOP_VEC(1)
              EOP(NSES)%U1_V  = SEC__TO__RAD*EOP_VEC(3)
              CALL CLRCH ( EOP(NSES)%SCODE   ) 
              CALL CLRCH ( EOP(NSES)%DBNAME )
              EOP(NSES)%DBNAME = STR(22:25)//STR(27:28)//STR(30:31)//'_?'
              CALL CLRCH ( EOP(NSES)%C_NET  )
!
              EOP(NSES)%FLAG = ' '
              EOP(NSES)%STATUS = 0
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, XPR__GTP )
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, YPR__GTP )
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, U1__GTP  )
!
              EOP(NSES)%XPL_E = GTP__EPS
              EOP(NSES)%YPL_E = GTP__EPS
              EOP(NSES)%U1_E  = GTP__EPS
              GOTO 410
         END IF
         IF ( LIND .LT. M_FLD_EOPS ) GOTO 410 ! Too few words
!
! ------ Extract MJD
!
         READ ( UNIT=STR(IND(1,1):IND(2,1)), FMT=*, IOSTAT=IOS(01) ) MJD
!
! ------ Check: is it reasonable?
!
         IF ( MJD .GT. 40000.0  .AND.  MJD .LT. 60000.0 ) THEN
              NSES = NSES+1
            ELSE
             GOTO 410
         END IF
         IF ( IVRB .GE. 1  .AND.  MOD(J1,10) .EQ. 0 ) THEN
              WRITE ( 6, '(A,I5,A$)' ) '  READ_EOPS: Reading line   ',J1, &
     &                                 '   '//CHAR(13)
         END IF
!
! ------ Set status: data are OK
!
         EOP(NSES)%FLAG = ' '
         EOP(NSES)%STATUS = 0
!
         READ ( UNIT=STR(IND(1,1):IND(2,1)), FMT=*, IOSTAT=IOS(01) ) &
     &          EOP(NSES)%MJD_EOP
         EOP(NSES)%MJD_NUT = EOP(NSES)%MJD_EOP
!
! ------ Get date tag
!
         DATE_TAG = JD_TO_DATE ( 2400000.5D0+MJD, IER )
         CALL CLRCH ( EOP(NSES)%DBNAME )
!
! ------ Resolve session code and get the database name
!
         EOP(NSES)%SCODE = STR(IND(1,18):IND(2,18))
         IER = 0
         CALL RESOLVE_SESSCODE ( EOP(NSES)%SCODE, DATE_TAG, MASTER_DIR, &
     &                           EOP(NSES)%DBNAME, IER )
         IF ( IER .NE. 0  .AND.  DATE_TAG(6:10) .EQ. '01.01' ) THEN
!
! ----------- If the experiment occured at New Year, thenit might occur,
! ----------- that the database nominal start date occured in the previous
! ----------- year
!
              DATE_TAG = JD_TO_DATE ( 2400000.5D0+MJD-1.0D0, IER )
              IER = 0
              CALL RESOLVE_SESSCODE ( EOP(NSES)%SCODE, DATE_TAG, MASTER_DIR, &
     &                                EOP(NSES)%DBNAME, IER )
         END IF
         IF ( ILEN(EOP(NSES)%DBNAME) .EQ. 0 ) THEN
              EOP(NSES)%DBNAME = '??????????'
         END IF
!
! ------ Get leap seconds values: velues of function UTC minus TAI
!
         MJD_VAL = IDINT ( EOP(NSES)%MJD_EOP )
         UTC_VAL = EOP(NSES)%MJD_EOP - MJD_VAL
         CALL ERR_PASS ( IUER, IER )
         CALL GET_UTC_M_TAI ( NERS, MJD_VAL, UTC_VAL, UTC_M_TAI, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8614, IUER, 'READ_EOPS', 'Error in an '// &
     &            'attempt to learn the value of leap second for '//DATE_TAG )
              CLOSE ( UNIT = LUN )
              RETURN
         END IF
!
! ------ Get information from other fields
!
         IF ( STR(IND(1,2):IND(1,2)+2) .EQ. '-0 ' .OR. &
     &        STR(IND(1,2):IND(1,2)+2) .EQ. '***'      ) THEN
!
!@              CALL SBIT ( EOP(NSES)%STATUS, XPL__GTP, INT2(1) )
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, XPL__GTP )
              EOP(NSES)%XPL_V = 0.0
            ELSE
              READ ( UNIT=STR(IND(1,2):IND(2,2)), FMT=*, &
     &               IOSTAT=IOS(02) ) VAL
              EOP(NSES)%XPL_V = VAL*1.D3/RAD__TO__MAS
         END IF
!
         IF ( STR(IND(1,3):IND(1,3)+2) .EQ. '-0 ' .OR. &
     &        STR(IND(1,3):IND(1,3)+2) .EQ. '***'      ) THEN
!
!@              CALL SBIT ( EOP(NSES)%STATUS, YPL__GTP, INT2(1) )
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, YPL__GTP )
              EOP(NSES)%YPL_V = 0.0
            ELSE
              READ ( UNIT=STR(IND(1,3):IND(2,3)), FMT=*, &
     &               IOSTAT=IOS(03) ) VAL
              EOP(NSES)%YPL_V = VAL*1.D3/RAD__TO__MAS
         END IF
!
         IF ( STR(IND(1,4):IND(1,4)+2) .EQ. '-0 ' .OR. &
     &        STR(IND(1,4):IND(1,4)+2) .EQ. '***'      ) THEN
!
!@              CALL SBIT ( EOP(NSES)%STATUS, U1__GTP, INT2(1) )
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, U1__GTP )
              EOP(NSES)%U1_V = 0.0
            ELSE
              READ ( UNIT=STR(IND(1,4):IND(2,4)), FMT=*, &
     &               IOSTAT=IOS(04) ) VAL
              EOP(NSES)%U1_V = (VAL + UTC_M_TAI)*1.D3/RAD__TO__MSEC
         END IF
!
         IF ( STR(IND(1,5):IND(1,5)+2) .EQ. '-0 ' .OR. &
     &        STR(IND(1,5):IND(1,5)+2) .EQ. '***'      ) THEN
!
!@              CALL SBIT ( EOP(NSES)%STATUS, DPSI__GTP, INT2(1) )
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, DPSI__GTP )
              EOP(NSES)%DPSI_V = 0.0
            ELSE
              READ ( UNIT=STR(IND(1,5):IND(2,5)), FMT=*, &
     &               IOSTAT=IOS(05) ) VAL
              EOP(NSES)%DPSI_V = VAL/RAD__TO__MAS
         END IF
!
         IF ( STR(IND(1,6):IND(1,6)+2) .EQ. '-0 ' .OR. &
     &        STR(IND(1,6):IND(1,6)+2) .EQ. '***'      ) THEN
!
!@              CALL SBIT ( EOP(NSES)%STATUS, DEPS__GTP, INT2(1) )
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, DEPS__GTP )
              EOP(NSES)%DEPS_V = 0.0
            ELSE
              READ ( UNIT=STR(IND(1,6):IND(2,6)), FMT=*, &
     &               IOSTAT=IOS(06) ) VAL
              EOP(NSES)%DEPS_V = VAL/RAD__TO__MAS
         END IF
!
         IF ( STR(IND(1,7):IND(1,7)+2) .EQ. '-0 ' .OR. &
     &        STR(IND(1,7):IND(1,7)+2) .EQ. '***'      ) THEN
!
!@              CALL SBIT ( EOP(NSES)%STATUS, XPL__GTP, INT2(1) )
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, XPL__GTP )
              EOP(NSES)%XPL_V = 0.0
              EOP(NSES)%XPL_E = 0.0
            ELSE
              READ ( UNIT=STR(IND(1,7):IND(2,7)), FMT=*, &
     &               IOSTAT=IOS(07) ) VAL
              EOP(NSES)%XPL_E = VAL*1.D3/RAD__TO__MAS
         END IF
!
         IF ( STR(IND(1,8):IND(1,8)+2) .EQ. '-0 ' .OR. &
     &        STR(IND(1,8):IND(1,8)+2) .EQ. '***'      ) THEN
!
!@              CALL SBIT ( EOP(NSES)%STATUS, YPL__GTP, INT2(1) )
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, YPL__GTP )
              EOP(NSES)%YPL_V = 0.0
              EOP(NSES)%YPL_E = 0.0
            ELSE
              READ ( UNIT=STR(IND(1,8):IND(2,8)), FMT=*, &
     &               IOSTAT=IOS(08) ) VAL
              EOP(NSES)%YPL_E = VAL*1.D3/RAD__TO__MAS
         END IF
!
         IF ( STR(IND(1,9):IND(1,9)+2) .EQ. '-0 ' .OR. &
     &        STR(IND(1,9):IND(1,9)+2) .EQ. '***'      ) THEN
!
!@              CALL SBIT ( EOP(NSES)%STATUS, U1__GTP, INT2(1) )
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, U1__GTP )
              EOP(NSES)%U1_V = 0.0
              EOP(NSES)%U1_E = 0.0
            ELSE
              READ ( UNIT=STR(IND(1,9):IND(2,9)), FMT=*, &
     &               IOSTAT=IOS(09) ) VAL
              EOP(NSES)%U1_E = VAL*1.D3/RAD__TO__MSEC
         END IF
!
         IF ( STR(IND(1,10):IND(1,10)+2) .EQ. '-0 ' .OR. &
     &        STR(IND(1,10):IND(1,10)+2) .EQ. '***'      ) THEN
!
!@              CALL SBIT ( EOP(NSES)%STATUS, DPSI__GTP, INT2(1) )
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, DPSI__GTP )
              EOP(NSES)%DPSI_V = 0.0
              EOP(NSES)%DPSI_E = 0.0
            ELSE
              READ ( UNIT=STR(IND(1,10):IND(2,10)), FMT=*, &
     &               IOSTAT=IOS(10) ) VAL
              EOP(NSES)%DPSI_E = VAL/RAD__TO__MAS
         END IF
!
         IF ( STR(IND(1,11):IND(1,11)+2) .EQ. '-0 ' .OR. &
     &        STR(IND(1,11):IND(1,11)+2) .EQ. '***'      ) THEN
!
!@              CALL SBIT ( EOP(NSES)%STATUS, DEPS__GTP, INT2(1) )
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, DEPS__GTP )
              EOP(NSES)%DEPS_V = 0.0
              EOP(NSES)%DEPS_V = 0.0
            ELSE
              READ ( UNIT=STR(IND(1,11):IND(2,11)), FMT=*, &
     &               IOSTAT=IOS(11) ) VAL
              EOP(NSES)%DEPS_E = VAL/RAD__TO__MAS
         END IF
!
         IF ( INDEX ( STR(IND(1,12):IND(2,12)), '.' ) .GT. 0 ) THEN
              READ ( UNIT=STR(IND(1,12):IND(2,12)), FMT=*, &
     &               IOSTAT=IOS(12) ) EOP(NSES)%WRMS
           ELSE
              STR1 = STR(IND(1,12):IND(2,12))//'.0'
              READ ( UNIT=STR1, FMT=* ) EOP(NSES)%WRMS
         END IF
         EOP(NSES)%WRMS = EOP(NSES)%WRMS*1.D-12
!
         READ ( UNIT=STR(IND(1,13):IND(2,13)), FMT=*, IOSTAT=IOS(13) ) &
     &          EOP(NSES)%C_XY
         READ ( UNIT=STR(IND(1,14):IND(2,14)), FMT=*, IOSTAT=IOS(14) ) &
     &          EOP(NSES)%C_XU
         READ ( UNIT=STR(IND(1,15):IND(2,15)), FMT=*, IOSTAT=IOS(15) ) &
     &          EOP(NSES)%C_YU
         READ ( UNIT=STR(IND(1,16):IND(2,16)), FMT=*, IOSTAT=IOS(16) ) &
     &          EOP(NSES)%C_PE
!
         READ ( UNIT=STR(IND(1,17):IND(2,17)), FMT=*, IOSTAT=IOS(17) ) &
     &          EOP(NSES)%NOBS
!
         IF ( INDEX ( STR(IND(1,19):IND(2,19)), '.' ) .GT. 0 ) THEN
              READ  ( UNIT=STR(IND(1,19):IND(2,19)), FMT=*, &
     &                IOSTAT=IOS(19) ) EOP(NSES)%DURA
           ELSE
              STR1 = STR(IND(1,19):IND(2,19))//'.0'
              READ ( UNIT=STR1, FMT=*, IOSTAT=IOS(19) ) &
     &        EOP(NSES)%DURA
         END IF
         EOP(NSES)%DURA = EOP(NSES)%DURA*3600.0D0
!
         IF ( STR(IND(1,20):IND(1,20)+2) .EQ. '-0 ' .OR. &
     &        STR(IND(1,20):IND(1,20)+2) .EQ. '***'      ) THEN
!
!@              CALL SBIT ( EOP(NSES)%STATUS, XPR__GTP, INT2(1) )
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, XPR__GTP )
              EOP(NSES)%XPR_V = 0.0
            ELSE
              READ ( UNIT=STR(IND(1,20):IND(2,20)), FMT=*, &
     &               IOSTAT=IOS(20) ) VAL
              EOP(NSES)%XPR_V = VAL*1.D3/(RAD__TO__MAS*86400.0D0)
         END IF
!
         IF ( STR(IND(1,21):IND(1,21)+2) .EQ. '-0 ' .OR. &
     &        STR(IND(1,21):IND(1,21)+2) .EQ. '***'      ) THEN
!
!@              CALL SBIT ( EOP(NSES)%STATUS, YPR__GTP, INT2(1) )
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, YPR__GTP )
              EOP(NSES)%YPR_V = 0.0
            ELSE
              READ ( UNIT=STR(IND(1,21):IND(2,21)), FMT=*, &
     &               IOSTAT=IOS(21) ) VAL
              EOP(NSES)%YPR_V = VAL*1.D3/(RAD__TO__MAS*86400.0D0)
         END IF
!
         IF ( STR(IND(1,22):IND(1,22)+2) .EQ. '-0 ' .OR. &
     &        STR(IND(1,22):IND(1,22)+2) .EQ. '***'      ) THEN
!
!@              CALL SBIT ( EOP(NSES)%STATUS, UTR__GTP, INT2(1) )
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, UTR__GTP )
              EOP(NSES)%UTR_V = 0.0
            ELSE
              READ ( UNIT=STR(IND(1,22):IND(2,22)), FMT=*, &
     &               IOSTAT=IOS(22) ) VAL
              EOP(NSES)%UTR_V = -VAL*OM__EAR*1.D3/RAD__TO__MSEC
         END IF
!
         IF ( STR(IND(1,25):IND(1,25)+2) .EQ. '-0 ' .OR. &
     &        STR(IND(1,25):IND(1,25)+2) .EQ. '***'      ) THEN
!
!@              CALL SBIT ( EOP(NSES)%STATUS, XPR__GTP, INT2(1) )
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, XPR__GTP )
              EOP(NSES)%XPR_V = 0.0
              EOP(NSES)%XPR_E = 0.0
            ELSE
              READ ( UNIT=STR(IND(1,25):IND(2,25)), FMT=*, &
     &               IOSTAT=IOS(25) ) VAL
              EOP(NSES)%XPR_E = VAL*1.D3/(RAD__TO__MAS*86400.0D0)
         END IF
!
         IF ( STR(IND(1,26):IND(1,26)+2) .EQ. '-0 ' .OR. &
     &        STR(IND(1,26):IND(1,26)+2) .EQ. '***'      ) THEN
!
!@              CALL SBIT ( EOP(NSES)%STATUS, YPR__GTP, INT2(1) )
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, YPR__GTP )
              EOP(NSES)%YPR_V = 0.0
              EOP(NSES)%YPR_E = 0.0
            ELSE
              READ ( UNIT=STR(IND(1,26):IND(2,26)), FMT=*, &
     &               IOSTAT=IOS(26) ) VAL
              EOP(NSES)%YPR_E = VAL*1.D3/(RAD__TO__MAS*86400.0D0)
         END IF
!
         IF ( STR(IND(1,27):IND(1,27)+2) .EQ. '-0 ' .OR. &
     &        STR(IND(1,27):IND(1,27)+2) .EQ. '***'      ) THEN
!
!@              CALL SBIT ( EOP(NSES)%STATUS, UTR__GTP, INT2(1) )
              EOP(NSES)%STATUS = IBSET ( EOP(NSES)%STATUS, UTR__GTP )
              EOP(NSES)%UTR_V = 0.0
              EOP(NSES)%UTR_E = 0.0
            ELSE
              READ ( UNIT=STR(IND(1,27):IND(2,27)), FMT=*, &
     &               IOSTAT=IOS(27) ) VAL
              EOP(NSES)%UTR_E = VAL*OM__EAR*1.D3/RAD__TO__MSEC
         END IF
!
! ------ No information about these correlations is available
!
          EOP(NSES)%C_URX = 0.0D0
         EOP(NSES)%C_URY = 0.0D0
         EOP(NSES)%C_URU = 0.0D0
         IF ( FMT_DATE .GE. '2007.08.30' ) THEN
              EOP(NSES)%C_NET = STR(237:300)
            ELSE 
              CALL CLRCH ( EOP(NSES)%C_NET )
         END IF
!
! ------ Checlking for reading errors
!
         DO 420 J2=1,M_FLD_EOPS
            IF ( IOS(J2) .NE. 0 ) THEN
                 CALL CLRCH ( STR1 )
                 CALL CLRCH ( STR2 )
                 CALL INCH  ( IOS(J2), STR1 )
                 CALL INCH  ( J1,      STR2 )
!
                 CALL ERR_LOG ( 8615, IUER, 'READ_EOPS', 'Error '// &
     &                STR1(1:I_LEN(STR1))//' in reading line '// &
     &                STR2(1:I_LEN(STR2))//' of the file '// &
     &                FINAM(1:I_LEN(FINAM))//' -- '//STR )
                 RETURN
            END IF
 420     CONTINUE
 410  CONTINUE
 810  CONTINUE
      CLOSE ( UNIT=LUN )
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, '(A,I5,A)' ) '  READ_EOPS: ',NSES, &
     &                ' experiments have been found in the eops file'
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END   !#!  READ_EOPS  #!#
