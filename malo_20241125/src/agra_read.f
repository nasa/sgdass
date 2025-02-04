      SUBROUTINE AGRA_READ ( FILIN, AGRA, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  AGRA_READ  reads the file with the time series of Stokes  *
! *   coefficients of the expansion of the atmosphere contribution to    *
! *   the geopotential into a series of spherical harmonics.             *
! *                                                                      *
! *   It is assumed the input file is in AGRA format.                    *
! *                                                                      *
! *   Results of parsing the input file are put in in fields of AGRA     *
! *   object.                                                            *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  FILIN ( CHARACTER ) -- Name of the file with displacements due to   *
! *                         atmosphere pressure loading.                 *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   AGRA ( RECORD    ) -- Data structure which keeps site related      *
! *                         information: maximal degree, time range,     *
! *                         time epochs of Stokes coefficients, and      *
! *                         the Stokes coefficients themselves.          *
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
! * ###  07-JAN-2005    AGRA_READ   v1.1  (c) L. Petrov  16-FEB-2005 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'agra.i'
      TYPE     ( AGRA__T_RECORD_SAMPLE ) ::  TS_REC
      TYPE     ( AGRA__TYPE ) :: AGRA
      CHARACTER  FILIN*(*)
      INTEGER*4  IUER
      CHARACTER  STR*128, STR1*128
      INTEGER*4  M_EPC 
      PARAMETER  ( M_EPC = 1 )
      LOGICAL*4  FL_P, FL_T_BEG, FL_T_END, FL_T_SAM
      INTEGER*4  LUN, K_COE, IDEG, IORD, IND_EPC, IND_EPC_LAST, &
     &           IOS, IOS1, IOS2, IOS3, J1, J2, J3
      INTEGER*4  GET_UNIT, I_LEN, LTM_DIF
!
! --- Open the input file
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILIN, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 2341, IUER, 'AGRA_READ', 'Error '// &
     &          STR(1:I_LEN(STR))//'in an attempt to open the input file '// &
     &          FILIN )
           RETURN
      END IF
!
! --- Read the first record. This record should be a format identifier
!
      READ ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 2342, IUER, 'AGRA_READ', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to read the input '// &
     &         'file '//FILIN )
           CLOSE ( UNIT=LUN )
           RETURN
      END IF
!
! --- Check whether the first record is really the valid format identifier
!
      IF ( STR(1:LEN(AGRA__LABEL)) .NE. AGRA__LABEL ) THEN
           CALL CLRCH ( STR )
           CALL TRAN  ( 13, STR, STR )
           CALL ERR_LOG ( 2343, IUER, 'AGRA_READ', 'Input file '// &
     &          FILIN(1:I_LEN(FILIN))//'" has wrong format. Line "'// &
     &          AGRA__LABEL//' was expected, but the first line is: "'// &
     &          STR )
           CLOSE ( UNIT=LUN )
           RETURN
      END IF
!
      FL_P     = .FALSE.
      FL_T_BEG = .FALSE.
      FL_T_END = .FALSE.
      FL_T_SAM = .FALSE.
      K_COE = 0
      IND_EPC_LAST = 0
!
      DO 430 J3=2,1024*1024
!
! ------ Read the next line
!
         READ ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR
         IF ( IOS .EQ. -1 ) THEN
              CALL ERR_LOG ( 2344, IUER, 'AGRA_READ', 'Input file '// &
     &             FILIN(1:I_LEN(FILIN))//' ended prematurely: trailing '// &
     &            'line was not found' )
              CLOSE ( UNIT=LUN )
              RETURN
         END IF
         IF ( IOS .NE. 0 ) THEN
              CALL ERR_LOG ( 2345, IUER, 'AGRA_READ', 'Error '// &
     &             STR(1:I_LEN(STR))//' in reading the input file '//FILIN )
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
         IF ( STR(1:LEN(AGRA__LABEL)) .EQ. AGRA__LABEL ) GOTO 810
!
! ------ Read the P record and parse it
!
         IF ( STR(1:1) .EQ. 'P' ) THEN
!
! ----------- P-record
!
              READ ( UNIT=STR(9:12),  FMT='(I4)', IOSTAT=IOS1 ) AGRA%L_DEG
              READ ( UNIT=STR(16:20), FMT='(I5)', IOSTAT=IOS2 ) AGRA%L_EPC
              READ ( UNIT=STR(24:30), FMT='(I7)', IOSTAT=IOS3 ) AGRA%L_COE
              IF ( IOS1 .NE. 0 .OR. IOS2 .NE. 0  .OR. IOS3 .NE. 0 ) THEN
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( J3, STR1 )
                   CALL ERR_LOG ( 2346, IUER, 'AGRA_READ', 'Error parsing '// &
     &                 'line '//STR1(1:I_LEN(STR1))//' in the AGRA '// &
     &                 'file '//FILIN )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
              IF ( AGRA%L_DEG .LT. 1  .OR.  AGRA%L_DEG .GT. MDEG__AGRA ) THEN
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( MDEG__AGRA, STR1 )
                   CALL ERR_LOG ( 2347, IUER, 'AGRA_READ', 'in the AGRA '// &
     &                 'file '//FILIN(1:I_LEN(FILIN))//' the maximal '// &
     &                 'degree of the harmonic development '//STR(9:12)// &
     &                 ' is out of range [1, MDEG_AGRA]: '//STR1 )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
!
! ----------- Check whether the total number of time epochs is correct
!
              IF ( AGRA%L_EPC .LT. 1  .OR. AGRA%L_EPC .GT. M_EPC ) THEN
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( M_EPC, STR1 )
                   CALL ERR_LOG ( 2348, IUER, 'AGRA_READ', 'AGRA '// &
     &                 'file '//FILIN(1:I_LEN(FILIN))//' the total number '// &
     &                 'of epochs: '//STR(16:20)//' is out of range '// &
     &                 '[1, M_EPC]  M_EPC: '//STR1 )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
!
! ----------- Check whether the total number of coefficients is correct
!
              IF ( AGRA%L_COE .NE. AGRA%L_EPC*(AGRA%L_DEG+1)*(AGRA%L_DEG+2)/2 ) THEN
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( M_EPC, STR1 )
                   WRITE ( 6, * ) ' AGRA%L_COE = ', AGRA%L_COE 
                   WRITE ( 6, * ) ' AGRA%L_EPC = ', AGRA%L_EPC
                   WRITE ( 6, * ) ' AGRA%L_DEG = ', AGRA%L_DEG
                   CALL ERR_LOG ( 2349, IUER, 'AGRA_READ', 'AGRA '// &
     &                 'file '//FILIN(1:I_LEN(FILIN))//' the total number of '// &
     &                 'coefficients does not correspond to the number '// &
     &                 'of epohcs and the maximal degree of the development' )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
!
              FL_P = .TRUE.
            ELSE IF ( STR(1:1) .EQ. 'T' ) THEN
!
! ----------- T-record
!
              IF ( STR(1:8) .EQ. 'T begin ' ) THEN
                   IF ( .NOT. FL_P ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J3, STR )
                        CALL ERR_LOG ( 2350, IUER, 'AGRA_READ', 'Format '// &
     &                      'violation at line '//STR(1:I_LEN(STR))// &
     &                      ' of the input file '//FILIN(1:I_LEN(FILIN))// &
     &                      ' -- P_record should preceed T_beg record ' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
!
! ---------------- Read the first T-record and extract the beginning date
!
                   READ ( UNIT=STR(11:15), FMT='(I5)'   ) AGRA%MJD_BEG 
                   READ ( UNIT=STR(17:23), FMT='(F7.1)' ) AGRA%SEC_BEG
                   IF ( AGRA%MJD_BEG .LT. 40000 .OR. &
     &                  AGRA%MJD_BEG .GT. 60000      ) THEN
                        CALL ERR_LOG ( 2351, IUER, 'AGRA_READ', &
     &                      'Incorrect MJD in T begin record. Out of range?' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
                   IF ( AGRA%SEC_BEG .LT. -1.0D0    .OR. &
     &                  AGRA%SEC_BEG .GT. 86400.0D0      ) THEN
                        CALL ERR_LOG ( 2352, IUER, 'AGRA_READ', &
     &                      'Incorrect SEC in T begin record' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
                   FL_T_BEG = .TRUE.
                 ELSE IF ( STR(1:8) .EQ. 'T end   ' ) THEN
                   IF ( .NOT. FL_T_BEG ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J3, STR )
                        CALL ERR_LOG ( 2353, IUER, 'AGRA_READ', 'Format '// &
     &                      'violation at line '//STR(1:I_LEN(STR))// &
     &                      ' of the input file '//FILIN(1:I_LEN(FILIN))// &
     &                      ' -- T begin  record should preceed T_end record' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
!
! ---------------- Read the second T-record and extract the end date
!
                   READ ( UNIT=STR(11:15), FMT='(I5)'   ) AGRA%MJD_END
                   READ ( UNIT=STR(17:23), FMT='(F7.1)' ) AGRA%SEC_END
                   IF ( AGRA%SEC_END .LT. -1.0D0    .OR. &
     &                  AGRA%SEC_END .GT. 86400.0D0      ) THEN
!
                        CALL ERR_LOG ( 2354, IUER, 'AGRA_READ', &
     &                      'Incorrect SEC in T end record' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
                   FL_T_END = .TRUE.
                 ELSE IF ( STR(1:8) .EQ. 'T sample' ) THEN
                   IF ( .NOT. FL_T_END ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J3, STR )
                        CALL ERR_LOG ( 2355, IUER, 'AGRA_READ', 'Format '// &
     &                      'violation at line '//STR(1:I_LEN(STR))// &
     &                      ' of the input file '//FILIN(1:I_LEN(FILIN))// &
     &                      ' -- T_sample record should preceed T_end record ' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
!
! ---------------- Read the third T-record and extract the samping interval
!
                   CALL LIB$MOVC3 ( LEN(AGRA__T_RECORD_SAMPLE_TEMPL), &
     &                              %REF(STR), TS_REC )
                   READ ( UNIT=TS_REC%SAMPLE_INTERVAL, FMT='(F7.1)' ) AGRA%INTERVAL
                   AGRA%INTERVAL = AGRA%INTERVAL*86400.0D0
                   FL_T_SAM = .TRUE.
              END IF
            ELSE IF ( STR(1:1) .EQ. 'D' ) THEN
!
! ----------- D-record
!
              K_COE = K_COE + 1
              IF ( K_COE .EQ. 1 ) THEN
!
! ---------------- Deallocate dynamic memory for Stokes coefficients if 
! ---------------- it was allocated
!
                   IF ( ASSOCIATED ( AGRA%STOKES ) ) THEN
                        DEALLOCATE ( AGRA%STOKES )
                   END IF
!
! ---------------- Allocate dynamic memory for Stokes coefficients
!
                   ALLOCATE ( AGRA%STOKES(2,0:AGRA%L_DEG,0:AGRA%L_DEG,AGRA%L_EPC), &
     &                        STAT=IOS )
                   IF ( IOS .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( AGRA%L_DEG*AGRA%L_DEG*AGRA%L_EPC*2, STR )
                        CALL ERR_LOG ( 2356, IUER, 'AGRA_READ', 'Failure '// &
     &                      'to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &                      'of dynamic memory' )
                        CLOSE ( UNIT=LUN )
                        RETURN
                   END IF
!
! ---------------- Initialization the dynamic memory with zeroes
!
                   CALL NOUT_R8 ( 2*AGRA%L_DEG*AGRA%L_DEG*AGRA%L_EPC, &
     &                            AGRA%STOKES )
              END IF
!
              IF ( K_COE .GT. AGRA%L_COE ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 2357, IUER, 'AGRA_READ', 'Format '// &
     &                 'violation at line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'file '//FILIN(1:I_LEN(FILIN))//' -- '// &
     &                 ' The number of D-records exceeded parameter '// &
     &                 'specified in P-record' )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
!
! ----------- Get the epoch index
!
              READ ( UNIT=STR(3:7), FMT='(I5)' ) IND_EPC
              IF ( IND_EPC .NE. IND_EPC_LAST    .AND. &
     &             IND_EPC .NE. IND_EPC_LAST+1  .AND. &
     &             IND_EPC_LAST .NE. 0                ) THEN
!
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 2358, IUER, 'AGRA_READ', 'Format '// &
     &                 'violation at line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'file '//FILIN(1:I_LEN(FILIN))//' -- date epochs '// &
     &                 'increment is more than 1' )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
!
              IF ( IND_EPC .LT. 1  .OR.  IND_EPC .GT. AGRA%L_EPC ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 2359, IUER, 'AGRA_READ', 'Format '// &
     &                 'violation at line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'file '//FILIN(1:I_LEN(FILIN))//' -- date epochs '// &
     &                 'index is out of range [1, the value defined in '// &
     &                 'the P-record' )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
!
! ----------- Get the degere
!
              CALL CHIN ( STR(46:48), IDEG )
              IF ( IDEG .LT. 0  .OR. IDEG .GT. AGRA%L_DEG ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 2360, IUER, 'AGRA_READ', 'Format '// &
     &                 'violation at line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'file '//FILIN(1:I_LEN(FILIN))//' -- degree '// &
     &                  STR(46:48)//' is out of range' )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
!
! ----------- Get the order
!
              CALL CHIN ( STR(50:52), IORD )
              IF ( IORD .LT. 0  .OR. IORD .GT. AGRA%L_DEG ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 2361, IUER, 'AGRA_READ', 'Format '// &
     &                 'violation at line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'file '//FILIN(1:I_LEN(FILIN))//' -- order '// &
     &                  STR(50:52)//' is out of range' )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
!
              IF ( IDEG .LT. IORD ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 2362, IUER, 'AGRA_READ', 'Format '// &
     &                 'violation at line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'file '//FILIN(1:I_LEN(FILIN))//' -- order '// &
     &                  STR(50:52)//' is less than degree '//STR(46:48) )
                   CLOSE ( UNIT=LUN )
                   RETURN
              END IF
!
! ----------- Extract Stokes coefficents
!
              READ ( UNIT=STR(55:66), FMT='(D12.5)' ) AGRA%STOKES(C__COEF,IDEG,IORD,IND_EPC)
              READ ( UNIT=STR(68:79), FMT='(D12.5)' ) AGRA%STOKES(S__COEF,IDEG,IORD,IND_EPC)
!
              IND_EPC_LAST = IND_EPC
         END IF
 430  CONTINUE
 810  CONTINUE
      CLOSE ( UNIT=LUN )
!
      IF ( K_COE .NE. AGRA%L_COE ) THEN
           CALL CLRCH ( STR )
           CALL INCH  (      K_COE, STR )
           CALL INCH  ( AGRA%L_COE, STR1 )
           CALL ERR_LOG ( 2363, IUER, 'AGRA_READ', 'The total number of '// &
     &         'D-records defined in file '//FILIN(1:I_LEN(FILIN))//' is '// &
     &          STR(1:I_LEN(STR))//' while the number of points defined in '// &
     &         'the P-record is '//STR1 )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  AGRA_READ  #!#
!
