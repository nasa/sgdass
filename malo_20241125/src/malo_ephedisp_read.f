      SUBROUTINE MALO_EPHEDISP_READ ( FILIN, MALO, DSP_ARR, MODE, &
     &                                L_STA, L_EPC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MALO_EPHEDISP_READ reads the file with the time series of *
! *   displacements due to atmosphere pressure loading for a set of      *
! *   sites. It parses the file and writes results of parsing in fields  *
! *   of the object STA. It is assumed that the input file is in  APLO   *
! *   format.                                                            *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   FILIN ( CHARACTER ) -- Name of the file with displacements due to  *
! *                          atmosphere pressure loading.                *
! *    MALO (                
! *   L_STA ( 
! *   L_EPC ( 
! *                                                                      *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * DSP_ARR ( 
! *    MODE ( 
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ## 06-DEC-2002 MALO_EPHEDISP_READ v3.0 (c) L. Petrov 13-MAY-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'malo.i'
      INCLUDE   'ephedisp.i'
      TYPE      ( MALO__TYPE         ) :: MALO
      TYPE      ( EPHEDISP__P_RECORD        ) ::  P_REC
      TYPE      ( EPHEDISP__T_RECORD_SAMPLE ) ::  TS_REC
      CHARACTER  FILIN*(*)
      INTEGER*4  L_STA, L_EPC, MODE, IUER
      CHARACTER  STR*128, STR1*128, STR_FIRST*128, FMT_EPHEDISP*10
      REAL*8     DSP_ARR(3,L_STA,L_EPC)
      REAL*8     RD_AREA, TIM_INT
      REAL*8     HEI_MIN, HEI_MAX
      PARAMETER  ( HEI_MIN = -1000.0D0 )
      PARAMETER  ( HEI_MAX =  9500.0D0 )
      REAL*8     LAT_GCN, RD, G_ACC
      CHARACTER  STA_NAM(MALO__MSTA)*8
      LOGICAL*4  FL_P, FL_A, FL_T_BEG, FL_T_END, FL_T_SAM
      INTEGER*4  LUN, K_STA, K_DSP, L_DSP, IND_STA, IND_EPC, &
     &           IND_EPC_LAST, IOS, J1, J2, J3, IER
      INTEGER*4, EXTERNAL :: GET_UNIT, I_LEN, LTM_DIF, LOC__SUN$$_STR
!
! --- Open the input file
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILIN, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 3441, IUER, 'MALO_EPHEDISP_READ', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to open the input file '// &
     &          FILIN )
           RETURN
      END IF
!
! --- Read the first record. This record should be a format identifier
!
      READ ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR_FIRST
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR_FIRST )
           CALL INCH  ( IOS, STR_FIRST )
           CALL ERR_LOG ( 3442, IUER, 'MALO_EPHEDISP_READ', 'Error '// &
     &          STR_FIRST(1:I_LEN(STR_FIRST))//' in an attempt to read '// &
     &         'in the input file '//FILIN )
           CLOSE ( UNIT=LUN )
           RETURN
      END IF
!
! --- Check whether the first record is really the valid format identifier
!
      IF ( STR_FIRST(1:LEN(EPHEDISP__LABEL)) .EQ. EPHEDISP__LABEL ) THEN
           FMT_EPHEDISP = '2005.06.30'
        ELSE IF ( STR_FIRST(1:LEN(EPHEDISP__LABEL)) .EQ. &
     &            'EPHEDISP  Format version of 2002.12.12' ) THEN
           FMT_EPHEDISP = '2002.12.12'
           RD_AREA = MALO__RD_AREA ! Default
        ELSE
           CALL TRAN  ( 13, STR_FIRST, STR_FIRST )
           CALL ERR_LOG ( 3443, IUER, 'MALO_EPHEDISP_READ', 'Input file '// &
     &          FILIN(1:I_LEN(FILIN))//'" has wrong format. Line "'// &
     &          EPHEDISP__LABEL//' was expected, but the first line is: "'// &
     &          STR_FIRST )
           CLOSE ( UNIT=LUN )
           RETURN
      END IF
!
      FL_P     = .FALSE.
      FL_A     = .FALSE.
      FL_T_BEG = .FALSE.
      FL_T_END = .FALSE.
      FL_T_SAM = .FALSE.
      K_STA = 0
      K_DSP = 0
      IND_EPC_LAST = 0
!
      DO 430 J3=2,1024*1024*1024
!
! ------ Read the next line
!
         READ ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR
         IF ( IOS .EQ. -1 ) THEN
              CALL ERR_LOG ( 3444, IUER, 'MALO_EPHEDISP_READ', 'Input file '// &
     &             FILIN(1:I_LEN(FILIN))//' ended prematurely: trailing line '// &
     &            'was not found' )
              CLOSE ( UNIT=LUN )
              RETURN
         END IF
         IF ( IOS .NE. 0 ) THEN
              CALL ERR_LOG ( 3445, IUER, 'MALO_EPHEDISP_READ', 'Error '// &
     &             STR(1:I_LEN(STR))//' in reading input file '//FILIN )
              CLOSE ( UNIT=LUN )
              RETURN
         END IF
!
! ------ Bypass comments
!
         IF ( STR(1:1) .EQ. '#' ) GOTO 430
!
! ------ Check whether this line is a trailer record?
!
         IF ( STR(1:LEN(EPHEDISP__LABEL)) .EQ. &
     &        STR_FIRST(1:LEN(EPHEDISP__LABEL))   ) GOTO 810
!
! ------ Read the P record and parse it
!
         IF ( STR(1:1) .EQ. 'P' ) THEN
!
! ----------- P-record
!
              IF ( FMT_EPHEDISP == '2005.06.30' ) THEN
                   CALL LIB$MOVC3 ( LEN(EPHEDISP__P_RECORD_TEMPLATE), &
     &                              STR(1:40), P_REC )
                   READ ( UNIT=P_REC%NUMB_S_REC,  FMT='(I10)', IOSTAT=IOS ) &
     &                    MALO%NSTA
                   READ ( UNIT=P_REC%NUMB_EPOCHS, FMT='(I6)',  IOSTAT=IOS ) &
     &                    MALO%NTIM
                   READ ( UNIT=P_REC%NUMB_D_REC,  FMT='(I10)', IOSTAT=IOS ) &
     &                    L_DSP
                 ELSE IF ( FMT_EPHEDISP == '2002.12.12' ) THEN
                   IF ( STR(13:13) .EQ. ' ' ) THEN
                        READ ( UNIT=STR(9:12),  FMT='(I4)' ) MALO%NSTA
                     ELSE 
                        READ ( UNIT=STR(9:13),  FMT='(I5)' ) MALO%NSTA
                   END IF
                   READ ( UNIT=STR(16:20), FMT='(I5)' ) MALO%NTIM
                   READ ( UNIT=STR(24:30), FMT='(I7)' ) L_DSP
              END IF
!
              IF ( MALO%NSTA .LT. 1  .OR.  MALO%NSTA .GT. MALO__MSTA ) THEN
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( MALO__MSTA, STR1 )
                   WRITE ( 6, * ) ' MALO%NSTA =', MALO%NSTA
                   CALL ERR_LOG ( 3446, IUER, 'MALO_EPHEDISP_READ', 'EPHEDISP '// &
     &                 'file '//FILIN(1:I_LEN(FILIN))//' the total number '// &
     &                 'of sites '//STR(9:12)//' is out of range '// &
     &                 '[1, MALO_MSTA]: '//STR1 )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
!
              IF ( MALO%NTIM .LT. 1  .OR. MALO%NTIM .GT. MALO__FIL ) THEN
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( MALO__FIL, STR1 )
                   CALL ERR_LOG ( 3447, IUER, 'MALO_EPHEDISP_READ', 'EPHEDISP '// &
     &                 'file '//FILIN(1:I_LEN(FILIN))//' the total number of '// &
     &                 'epochs: '//STR(16:20)//' is out of range '// &
     &                 '[1, MALO__FIL]  MALO__FIL: '//STR1 )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
!
              IF ( L_DSP .GT. MALO%NSTA*MALO%NTIM ) THEN
                   WRITE ( 6, * ) '     L_DSP = ', L_DSP 
                   WRITE ( 6, * ) ' MALO%NSTA = ', MALO%NSTA
                   WRITE ( 6, * ) ' MALO%NTIM = ', MALO%NTIM
                   CALL ERR_LOG ( 3448, IUER, 'MALO_EPHEDISP_READ', 'EPHEDISP '// &
     &                 'file '//FILIN(1:I_LEN(FILIN))//' the total number of '// &
     &                 'points exceeded the product of total number of '// &
     &                 'sites and total number of epochs' )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
!
              ALLOCATE ( MALO%STA(MALO%NSTA), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH  ( STR ) 
                   CALL IINCH  ( MALO%NSTA*SIZEOF(MALO%STA(1)), STR )
                   CALL ERR_LOG ( 3449, IUER, 'MALO_EPHEDISP_READ', 'Failure '// &
     &                 'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                 ' bytes of dynamic memory for arraay MALO%STA' )
                   RETURN 
              END IF
!
              FL_P = .TRUE.
            ELSE IF ( STR(1:1) .EQ. 'A' ) THEN
              IF ( .NOT. FL_P ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( J3, STR )
                    CALL ERR_LOG ( 3449, IUER, 'MALO_EPHEDISP_READ', 'Format '// &
     &                   'violation at line '//STR(1:I_LEN(STR))// &
     &                   ' of the input file '//FILIN(1:I_LEN(FILIN))// &
     &                   ' -- P_record should preceed A record ' )
                        CLOSE ( UNIT=LUN )
                    RETURN
              END IF
!
              READ ( UNIT=STR(3:14), FMT='(F14.6)', IOSTAT=IOS ) RD_AREA
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 3450, IUER, 'MALO_EPHEDISP_READ', 'Error '// &
     &                   'in reading line '//STR(1:I_LEN(STR))// &
     &                   ' of the input file '//FILIN )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
              FL_A = .TRUE.
            ELSE IF ( STR(1:1) .EQ. 'T' ) THEN
!
! ----------- T-record
!
              IF ( STR(1:8) .EQ. 'T begin ' ) THEN
                   IF ( .NOT. FL_P ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J3, STR )
                        CALL ERR_LOG ( 3451, IUER, 'MALO_EPHEDISP_READ', 'Format '// &
     &                      'violation at line '//STR(1:I_LEN(STR))// &
     &                      ' of the input file '//FILIN(1:I_LEN(FILIN))// &
     &                      ' -- P_record should preceed T_beg record ' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
!
! ---------------- Read the first T-record and extract the beginning date
!
                   READ ( UNIT=STR(11:15), FMT='(I5)'   ) MALO%MJD_BEG
                   READ ( UNIT=STR(17:23), FMT='(F7.1)' ) MALO%TAI_BEG
                   IF ( MALO%MJD_BEG .LT. MALO__MJD_MIN .OR. &
     &                  MALO%MJD_BEG .GT. MALO__MJD_MAX      ) THEN
                        CALL ERR_LOG ( 3452, IUER, 'MALO_EPHEDISP_READ', &
     &                      'Incorrect MJD in T begin record. Out of range?' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
                   IF ( MALO%TAI_BEG .LT. -1.0D0    .OR. &
     &                  MALO%TAI_BEG .GT. 86400.0D0      ) THEN
                        CALL ERR_LOG ( 3453, IUER, 'MALO_EPHEDISP_READ', &
     &                      'Incorrect SEC in T begin record' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
                   FL_T_BEG = .TRUE.
                 ELSE IF ( STR(1:8) .EQ. 'T end   ' ) THEN
                   IF ( .NOT. FL_T_BEG ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J3, STR )
                        CALL ERR_LOG ( 3454, IUER, 'MALO_EPHEDISP_READ', 'Format '// &
     &                      'violation at line '//STR(1:I_LEN(STR))// &
     &                      ' of the input file '//FILIN(1:I_LEN(FILIN))// &
     &                      ' -- T begin  record should preceed T_end record' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
!
! ---------------- Read the second T-record and extract the end date
!
                   READ ( UNIT=STR(11:15), FMT='(I5)'   ) MALO%MJD_END
                   READ ( UNIT=STR(17:23), FMT='(F7.1)' ) MALO%TAI_END
                   IF ( MALO%MJD_END .LT. MALO__MJD_MIN .OR. &
     &                  MALO%MJD_END .GT. MALO__MJD_MAX      ) THEN
                        CALL ERR_LOG ( 3455, IUER, 'MALO_EPHEDISP_READ', &
     &                      'Incorrect MJD in T begin record. Out of range?' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
                   IF ( MALO%TAI_END .LT. -1.0D0    .OR. &
     &                  MALO%TAI_END .GT. 86400.0D0      ) THEN
!
                        CALL ERR_LOG ( 3456, IUER, 'MALO_EPHEDISP_READ', &
     &                      'Incorrect SEC in T end record' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
                   FL_T_END = .TRUE.
                 ELSE IF ( STR(1:8) .EQ. 'T sample' ) THEN
                   IF ( .NOT. FL_T_END ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J3, STR )
                        CALL ERR_LOG ( 3456, IUER, 'MALO_EPHEDISP_READ', 'Format '// &
     &                      'violation at line '//STR(1:I_LEN(STR))// &
     &                      ' of the input file '//FILIN(1:I_LEN(FILIN))// &
     &                      ' -- T_sample record should preceed T_end record ' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
!
! ---------------- Read the third T-record and extract the samping interval
!
#ifdef SUN
                   CALL LIB$MOVC3 ( LEN(EPHEDISP__T_RECORD_TEMPLATE), &
     &                              %VAL(LOC__SUN$$_STR(STR)), TS_REC )
#else
                   CALL LIB$MOVC3 ( SIZEOF(EPHEDISP__T_RECORD_TEMPLATE), &
     &                              %REF(STR), TS_REC )
#endif
                   READ ( UNIT=TS_REC%SAMPLE_INTERVAL, FMT='(F13.11)', IOSTAT=IER ) TIM_INT
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J3, STR )
                        CALL ERR_LOG ( 3457, IUER, 'MALO_EPHEDISP_READ', 'Error '// &
     &                      'in readling line '//STR(1:I_LEN(STR))// &
     &                      ' of the input file '//FILIN )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
                   TIM_INT = TIM_INT*86400.0D0
                   MALO%TIM_STEP = TIM_INT
                   FL_T_SAM = .TRUE.
              END IF
            ELSE IF ( STR(1:1) .EQ. 'S' ) THEN
!
! ----------- S-record
!
              K_STA = K_STA + 1
              IF ( K_STA .GT. MALO%NSTA ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3457, IUER, 'MALO_EPHEDISP_READ', 'Format '// &
     &                 'violation at line '//STR(1:I_LEN(STR))// &
     &                 ' The number of S-records exceeded parameter '// &
     &                 'specified in P-record' )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
              STA_NAM(K_STA) = STR(4:11)
              MALO%STA(K_STA)%NAME = STR(4:11)
              READ ( UNIT=STR(14:26), FMT='(F13.3)' ) MALO%STA(K_STA)%COO(1)
              READ ( UNIT=STR(28:40), FMT='(F13.3)' ) MALO%STA(K_STA)%COO(2)
              READ ( UNIT=STR(42:54), FMT='(F13.3)' ) MALO%STA(K_STA)%COO(3)
!
! ----------- Computation of station longitude, geocentric latitude, height
! ----------- above ellipsoid
!
              CALL REF_ELL ( 0, MALO%STA(K_STA)%COO(1), LAT_GCN, &
     &                       MALO%STA(K_STA)%LAT_GDT, MALO%STA(K_STA)%LON, &
     &                       MALO%STA(K_STA)%HEI_ELL, RD, G_ACC )
              CALL GETENVAR ( 'MALO_HEIGHT_CHECK_DISABLE', STR )
              CALL TRAN ( 11, STR, STR )
              IF ( STR .NE. 'YES' ) THEN
                   IF ( MALO%STA(K_STA)%HEI_ELL .LT. MALO__HEIGHT_MIN .OR. &
     &                  MALO%STA(K_STA)%HEI_ELL .GT. MALO__HEIGHT_MAX      ) THEN
!
                        CALL CLRCH ( STR )
                        WRITE ( UNIT=STR, FMT='(1PE12.5)' ) MALO%STA(K_STA)%HEI_ELL
                        CALL ERR_LOG ( 3458, IUER, 'MALO_EPHEDISP_READ', 'Site '// &
     &                       STA_NAM(K_STA)//' defined in the S-record of '// &
     &                      'the file '//FILIN(1:I_LEN(FILIN))//' did not pass '// &
     &                      'sanity check: its height above the referenced '// &
     &                      'ellipsoid is '//STR(1:I_LEN(STR))//' -- a little '// &
     &                      'bit strange, isn''t it? ' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
              END IF
!
            ELSE IF ( STR(1:1) .EQ. 'D' .AND. MODE == MALO__REA ) THEN
!
! ----------- D-record
!
              IF ( K_STA .NE. MALO%NSTA ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3459, IUER, 'MALO_EPHEDISP_READ', 'Format '// &
     &                 'violation at line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'file '//FILIN(1:I_LEN(FILIN))//' -- '// &
     &                 ' -- the number of S-records is less than the number '// &
     &                 'declared in the P-record' )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
!
              K_DSP = K_DSP + 1
              IF ( K_DSP .GT. L_DSP ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3460, IUER, 'MALO_EPHEDISP_READ', 'Format '// &
     &                 'violation at line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'file '//FILIN(1:I_LEN(FILIN))//' -- '// &
     &                 ' The number of D-records exceeded parameter '// &
     &                 'specified in P-record' )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
              READ ( UNIT=STR(3:7), FMT='(I5)' ) IND_EPC
              IF ( IND_EPC .NE. IND_EPC_LAST    .AND. &
     &             IND_EPC .NE. IND_EPC_LAST+1  .AND. &
     &             IND_EPC_LAST .NE. 0                ) THEN
!
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3461, IUER, 'MALO_EPHEDISP_READ', 'Format '// &
     &                 'violation at line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'file '//FILIN(1:I_LEN(FILIN))//' -- date epochs '// &
     &                 'increment is more than 1' )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
!
              IF ( IND_EPC .LT. 1  .OR.  IND_EPC .GT. L_EPC ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3462, IUER, 'MALO_EPHEDISP_READ', 'Format '// &
     &                 'violation at line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'file '//FILIN(1:I_LEN(FILIN))//' -- date epochs '// &
     &                 'index is out of range [1, the value defined in '// &
     &                 'the P-record' )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
!
! ----------- Get station index
!
              IND_STA = LTM_DIF ( 1, MALO%NSTA, STA_NAM, STR(46:53) )
              IF ( IND_STA .LE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3463, IUER, 'MALO_EPHEDISP_READ', 'Format '// &
     &                 'violation at line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'file '//FILIN(1:I_LEN(FILIN))//' -- site '// &
     &                  STR(46:53)//' was not defined in the S-record' )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
!
! ----------- Extract station displacements
!
              READ ( UNIT=STR(55:62), FMT='(F8.5)', IOSTAT=IOS ) DSP_ARR(1,IND_STA,IND_EPC)
              IF ( IOS .NE. 0 ) THEN
                   STR1 = STR
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3464, IUER, 'MALO_EPHEDISP_READ', 'Format '// &
     &                 'violation at line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'file '//FILIN(1:I_LEN(FILIN))//' -- cannot read '// &
     &                 ' the station displacement '//STR1(55:62) )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
!
              READ ( UNIT=STR(64:71), FMT='(F8.5)', IOSTAT=IOS ) DSP_ARR(2,IND_STA,IND_EPC)
              IF ( IOS .NE. 0 ) THEN
                   STR1 = STR
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3465, IUER, 'MALO_EPHEDISP_READ', 'Format '// &
     &                 'violation at line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'file '//FILIN(1:I_LEN(FILIN))//' -- cannot read '// &
     &                 ' the station displacement '//STR1(64:71) )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
!
              READ ( UNIT=STR(73:80), FMT='(F8.5)', IOSTAT=IOS ) DSP_ARR(3,IND_STA,IND_EPC)
              IF ( IOS .NE. 0 ) THEN
                   STR1 = STR
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3466, IUER, 'MALO_EPHEDISP_READ', 'Format '// &
     &                 'violation at line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'file '//FILIN(1:I_LEN(FILIN))//' -- cannot read '// &
     &                 ' the station displacement '//STR1(73:80) )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
!
              IND_EPC_LAST = IND_EPC
         END IF
 430  CONTINUE
 810  CONTINUE
      CLOSE ( UNIT=LUN )
      IF ( MODE == MALO__REA ) THEN
           IF ( K_STA .NE. MALO%NSTA ) THEN
                CALL CLRCH ( STR )
                CALL INCH  (     K_STA, STR )
                CALL INCH  ( MALO%NSTA, STR1 )
                CALL ERR_LOG ( 3464, IUER, 'MALO_EPHEDISP_READ', 'The total '// &
     &              'number of S-records defined in file '// &
     &               FILIN(1:I_LEN(FILIN))//' is '//STR(1:I_LEN(STR))// &
     &               ' while the number of sites defined in '// &
     &              'the P-record is '//STR1 )
                RETURN
           END IF
!
           IF ( K_DSP .NE. L_DSP ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( K_DSP, STR )
                CALL INCH  ( L_DSP, STR1 )
                CALL ERR_LOG ( 3465, IUER, 'MALO_EPHEDISP_READ', 'The total '// &
     &              'number of D-records defined in file '// &
     &               FILIN(1:I_LEN(FILIN))//' is '//STR(1:I_LEN(STR))// &
     &              ' while the number of points defined in '// &
     &              'the P-record is '//STR1 )
                RETURN
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE   MALO_EPHEDISP_READ  !#!#
!
! ------------------------------------------------------------------------
!
     SUBROUTINE MALO_EPHEDISP_LEARN ( FILIN, MJD_BEG, TAI_BEG, MJD_END, &
     &                                TAI_END, TIM_STEP, L_EPC, L_STA, &
     &                                IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MALO_EPHEDISP_LEARN reads the preamble section of the     *
! *   the file with site displacements in EPHEDISP format. It parses     *
! *   the header of the file and extracts certain information.           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   FILIN ( CHARACTER ) -- Name of the file with displacements due to  *
! *                          atmosphere pressure loading.                *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * MJD_BEG   ( INTEGER*4 ) -- 
! * TAI_BEG   ( REAL*8    ) -- 
! * MJD_END   ( INTEGER*4 ) -- 
! * TAI_END   ( REAL*8    ) -- 
! * TIM_STEP  ( REAL*8    ) -- 
! * L_EPD     ( INTEGER*4 ) -- 
! * L_STA     ( INTEGER*4 ) -- 
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ## 22-JUL-2015 MALO_EPHEDISP_LEARN v1.0 (c) L. Petrov 22-JUL-2015 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'malo.i'
      INCLUDE   'ephedisp.i'
      TYPE      ( MALO__TYPE         ) :: MALO
      TYPE      ( EPHEDISP__P_RECORD        ) ::  P_REC
      TYPE      ( EPHEDISP__T_RECORD_SAMPLE ) ::  TS_REC
      CHARACTER  FILIN*(*)
      INTEGER*4  MJD_BEG, MJD_END, L_STA, L_EPC, MODE, IUER
      REAL*8     TAI_BEG, TAI_END, TIM_STEP
      CHARACTER  STR*128, STR1*128, STR_FIRST*128, FMT_EPHEDISP*10
      REAL*8     RD_AREA, TIM_INT
      REAL*8     HEI_MIN, HEI_MAX
      PARAMETER  ( HEI_MIN = -1000.0D0 )
      PARAMETER  ( HEI_MAX =  9500.0D0 )
      REAL*8     LAT_GCN, RD, G_ACC
      CHARACTER  STA_NAM(MALO__MSTA)*8
      LOGICAL*4  FL_P, FL_A, FL_T_BEG, FL_T_END, FL_T_SAM
      INTEGER*4  LUN, K_STA, K_DSP, L_DSP, IND_STA, IND_EPC, &
     &           IND_EPC_LAST, IOS, J1, J2, J3, IER
      INTEGER*4, EXTERNAL :: GET_UNIT, I_LEN, LTM_DIF, LOC__SUN$$_STR
!
! --- Open the input file
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILIN, STATUS='OLD', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 3481, IUER, 'MALO_EPHEDISP_LEARN', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to open the input file '// &
     &          FILIN )
           RETURN
      END IF
!
! --- Read the first record. This record should be a format identifier
!
      READ ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR_FIRST
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR_FIRST )
           CALL INCH  ( IOS, STR_FIRST )
           CALL ERR_LOG ( 3482, IUER, 'MALO_EPHEDISP_LEARN', 'Error '// &
     &          STR_FIRST(1:I_LEN(STR_FIRST))//' in an attempt to read '// &
     &         'in the input file '//FILIN )
           CLOSE ( UNIT=LUN )
           RETURN
      END IF
!
! --- Check whether the first record is really the valid format identifier
!
      IF ( STR_FIRST(1:LEN(EPHEDISP__LABEL)) .EQ. EPHEDISP__LABEL ) THEN
           FMT_EPHEDISP = '2005.06.30'
        ELSE IF ( STR_FIRST(1:LEN(EPHEDISP__LABEL)) .EQ. &
     &            'EPHEDISP  Format version of 2002.12.12' ) THEN
           FMT_EPHEDISP = '2002.12.12'
           RD_AREA = MALO__RD_AREA ! Default
        ELSE
           CALL TRAN  ( 13, STR_FIRST, STR_FIRST )
           CALL ERR_LOG ( 3483, IUER, 'MALO_EPHEDISP_LEARN', 'Input file '// &
     &          FILIN(1:I_LEN(FILIN))//'" has wrong format. Line "'// &
     &          EPHEDISP__LABEL//' was expected, but the first line is: "'// &
     &          STR_FIRST )
           CLOSE ( UNIT=LUN )
           RETURN
      END IF
      FL_P     = .FALSE.
      FL_A     = .FALSE.
      FL_T_BEG = .FALSE.
      FL_T_END = .FALSE.
      FL_T_SAM = .FALSE.
!
      DO 430 J3=2,1024*1024*1024
!
! ------ Read the next line
!
         READ ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR
         IF ( IOS .EQ. -1 ) THEN
              CALL ERR_LOG ( 3484, IUER, 'MALO_EPHEDISP_LEARN', 'Input file '// &
     &             FILIN(1:I_LEN(FILIN))//' ended prematurely: trailing line '// &
     &            'was not found' )
              CLOSE ( UNIT=LUN )
              RETURN
         END IF
         IF ( IOS .NE. 0 ) THEN
              CALL ERR_LOG ( 3485, IUER, 'MALO_EPHEDISP_LEARN', 'Error '// &
     &             STR(1:I_LEN(STR))//' in reading input file '//FILIN )
              CLOSE ( UNIT=LUN )
              RETURN
         END IF
!
! ------ Bypass comments
!
         IF ( STR(1:1) .EQ. '#' ) GOTO 430
!
! ------ Check whether this line is a trailer record?
!
         IF ( STR(1:LEN(EPHEDISP__LABEL)) .EQ. &
     &        STR_FIRST(1:LEN(EPHEDISP__LABEL))   ) GOTO 830
!
! ------ Read the P record and parse it
!
         IF ( STR(1:1) .EQ. 'P' ) THEN
!
! ----------- P-record
!
              IF ( FMT_EPHEDISP == '2005.06.30' ) THEN
                   CALL LIB$MOVC3 ( LEN(EPHEDISP__P_RECORD_TEMPLATE), &
     &                              STR(1:40), P_REC )
                   READ ( UNIT=P_REC%NUMB_S_REC,  FMT='(I10)', IOSTAT=IOS ) &
     &                    L_STA
                   READ ( UNIT=P_REC%NUMB_EPOCHS, FMT='(I6)',  IOSTAT=IOS ) &
     &                    L_EPC
                 ELSE IF ( FMT_EPHEDISP == '2002.12.12' ) THEN
                   IF ( STR(13:13) .EQ. ' ' ) THEN
                        READ ( UNIT=STR(9:12),  FMT='(I4)' ) L_STA
                     ELSE 
                        READ ( UNIT=STR(9:13),  FMT='(I5)' ) L_STA
                   END IF
                   READ ( UNIT=STR(16:20), FMT='(I5)' ) L_EPC
              END IF
!
              IF ( L_STA .LT. 1  .OR.  L_STA .GT. MALO__MSTA ) THEN
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( MALO__MSTA, STR1 )
                   WRITE ( 6, * ) ' L_STA= ', L_STA
                   CALL ERR_LOG ( 3486, IUER, 'MALO_EPHEDISP_LEARN', 'EPHEDISP '// &
     &                 'file '//FILIN(1:I_LEN(FILIN))//' the total number '// &
     &                 'of sites '//STR(9:12)//' is out of range '// &
     &                 '[1, MALO_MSTA]: '//STR1 )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
!
              IF ( L_EPC .LT. 1  .OR. L_EPC .GT. MALO__FIL ) THEN
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( MALO__FIL, STR1 )
                   CALL ERR_LOG ( 3487, IUER, 'MALO_EPHEDISP_LEARN', 'EPHEDISP '// &
     &                 'file '//FILIN(1:I_LEN(FILIN))//' the total number of '// &
     &                 'epochs: '//STR(16:20)//' is out of range '// &
     &                 '[1, MALO__FIL]  MALO__FIL: '//STR1 )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
              FL_P = .TRUE.
            ELSE IF ( STR(1:1) .EQ. 'T' ) THEN
!
! ----------- T-record
!
              IF ( STR(1:8) .EQ. 'T begin ' ) THEN
                   IF ( .NOT. FL_P ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J3, STR )
                        CALL ERR_LOG ( 3488, IUER, 'MALO_EPHEDISP_LEARN', 'Format '// &
     &                      'violation at line '//STR(1:I_LEN(STR))// &
     &                      ' of the input file '//FILIN(1:I_LEN(FILIN))// &
     &                      ' -- P_record should preceed T_beg record ' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
!
! ---------------- Read the first T-record and extract the beginning date
!
                   READ ( UNIT=STR(11:15), FMT='(I5)'   ) MJD_BEG
                   READ ( UNIT=STR(17:23), FMT='(F7.1)' ) TAI_BEG
                   IF ( MJD_BEG .LT. MALO__MJD_MIN .OR. &
     &                  MJD_BEG .GT. MALO__MJD_MAX      ) THEN
                        CALL ERR_LOG ( 3489, IUER, 'MALO_EPHEDISP_LEARN', &
     &                      'Incorrect MJD in T begin record. Out of range?' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
                   IF ( TAI_BEG .LT. -1.0D0    .OR. &
     &                  TAI_BEG .GT. 86400.0D0      ) THEN
                        CALL ERR_LOG ( 3490, IUER, 'MALO_EPHEDISP_LEARN', &
     &                      'Incorrect SEC in T begin record' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
                   FL_T_BEG = .TRUE.
                 ELSE IF ( STR(1:8) .EQ. 'T end   ' ) THEN
                   IF ( .NOT. FL_T_BEG ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J3, STR )
                        CALL ERR_LOG ( 3491, IUER, 'MALO_EPHEDISP_LEARN', 'Format '// &
     &                      'violation at line '//STR(1:I_LEN(STR))// &
     &                      ' of the input file '//FILIN(1:I_LEN(FILIN))// &
     &                      ' -- T begin  record should preceed T_end record' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
!
! ---------------- Read the second T-record and extract the end date
!
                   READ ( UNIT=STR(11:15), FMT='(I5)'   ) MJD_END
                   READ ( UNIT=STR(17:23), FMT='(F7.1)' ) TAI_END
                   IF ( MJD_END .LT. MALO__MJD_MIN .OR. &
     &                  MJD_END .GT. MALO__MJD_MAX      ) THEN
                        CALL ERR_LOG ( 3492, IUER, 'MALO_EPHEDISP_LEARN', &
     &                      'Incorrect MJD in T begin record. Out of range?' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
                   IF ( TAI_END .LT. -1.0D0    .OR. &
     &                  TAI_END .GT. 86400.0D0      ) THEN
                        CALL ERR_LOG ( 3493, IUER, 'MALO_EPHEDISP_LEARN', &
     &                      'Incorrect SEC in T end record' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
                   FL_T_END = .TRUE.
                 ELSE IF ( STR(1:8) .EQ. 'T sample' ) THEN
                   IF ( .NOT. FL_T_END ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J3, STR )
                        CALL ERR_LOG ( 3494, IUER, 'MALO_EPHEDISP_LEARN', 'Format '// &
     &                      'violation at line '//STR(1:I_LEN(STR))// &
     &                      ' of the input file '//FILIN(1:I_LEN(FILIN))// &
     &                      ' -- T_sample record should preceed T_end record ' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
!
! ---------------- Read the third T-record and extract the samping interval
!
#ifdef SUN
                   CALL LIB$MOVC3 ( LEN(EPHEDISP__T_RECORD_TEMPLATE), &
     &                              %VAL(LOC__SUN$$_STR(STR)), TS_REC )
#else
                   CALL LIB$MOVC3 ( SIZEOF(EPHEDISP__T_RECORD_TEMPLATE), &
     &                              %REF(STR), TS_REC )
#endif
                   READ ( UNIT=TS_REC%SAMPLE_INTERVAL, FMT='(F13.11)', IOSTAT=IER ) TIM_STEP
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J3, STR )
                        CALL ERR_LOG ( 3495, IUER, 'MALO_EPHEDISP_LEARN', 'Error '// &
     &                      'in readling line '//STR(1:I_LEN(STR))// &
     &                      ' of the input file '//FILIN )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
                   TIM_STEP = TIM_STEP*86400.0D0
                   FL_T_SAM = .TRUE.
                   GOTO 430
              END IF
         END IF
 430  CONTINUE
 830  CONTINUE
      CLOSE ( UNIT=LUN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_EPHEDISP_LEARN  !#!#
