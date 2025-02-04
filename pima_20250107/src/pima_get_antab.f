      SUBROUTINE PIMA_GET_ANTAB ( PIM, VTD, ANTAB_MODE, ANTAB_FILE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_GET_ANTAB  reads the file in antab format, extracts   *
! *   (if defiend) flag on-off time, cable calibration, Tsys, weather    *
! *   information, antenna gain, and puts them in PIMA internal          *
! *   structures.                                                        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *        PIM ( PIMA__TYP ) -- Object with information related to       *
! *                             program PIMA.                            *
! *        VTD ( VTD__TYP  ) -- Object with information related to       *
! *                             package VTD for computed apriori path    *
! *                             delay.                                   *
! * ANTAB_MODE ( CHARACTER ) -- Mode that defines the flavour of antab   *
! *                             file:
! *                             evn_antab_file  EVN  antab file generated*
! *                                             by Cormac's program.     *
! *                             pima_antab_file PIMA antab file generated*
! *                                             by PIMA program log_antab*
! * ANTAB_FILE ( CHARACTER ) -- Name of the ANTAB file.                  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the vase of errors. *
! *                                                                      *
! * ### 26-DEC-2008  PIMA_GET_ANTAB  v3.5 (c) L. Petrov 27-DEC-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      TYPE     ( VTD__TYPE  ) :: VTD
      CHARACTER  ANTAB_MODE*(*), ANTAB_FILE*(*)
      INTEGER*4  IUER
      INTEGER*4  MBUF, M_CHN, M_TIM, MIND, M_POLY, M_FRQ
      PARAMETER  ( MBUF   =  32*1024 )
      PARAMETER  ( M_CHN  =       64 )
      PARAMETER  ( M_TIM  =  32*1024 )
      PARAMETER  ( MIND   =      300 )
      PARAMETER  ( M_POLY =       16 )
      PARAMETER  ( M_FRQ  =  4*PIM__MFRQ )
      CHARACTER, ALLOCATABLE :: BUF(:)*2048
      REAL*8,    ALLOCATABLE :: TIM_TSYS_ARR(:,:,:), TSYS_ARR(:,:,:,:)
      REAL*8     TIM_ONS_ARR(2,M_TIM), TIM_CAB_ARR(M_TIM), &
     &           TIM_MET_ARR(M_TIM), TSYS_VALS(M_FRQ)
      INTEGER*4, ALLOCATABLE :: NUM_TSYS(:,:,:), IND_SOU_ARR(:)
      CHARACTER  STR*128, STR1*128, STR2*128, STA_NAM*8, SOU_NAM*8, REG*5, &
     &           SOU_ONS(M_TIM)*8, POL_STR*1
      REAL*8     TIM_EPS, TSYS__MIN, TSYS__MAX, FRQ_DIF__MIN, TSYS__TIM_TOL, &
     &           M__CAB_SHARE, PIM__SPAN_DEF
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9)//',=' )
      PARAMETER  ( TIM_EPS = 1.0D0 )
      PARAMETER  ( TSYS__MIN = 2.0D0 )
      PARAMETER  ( TSYS__MAX = 30000.D0 )
      PARAMETER  ( TSYS__TIM_TOL  = 700.0D0 )
      PARAMETER  ( FRQ_DIF__MIN = 1.0D7 )
      PARAMETER  ( M__CAB_SHARE = 0.20D0 )
      PARAMETER  ( PIM__SPAN_DEF = 600.0D0 ) ! Default time span of phase-cal and Tsys
!
      LOGICAL*1  FL_GAIN, FL_TSYS
      REAL*8     DPFU(2), FRQ_DPFU(2), GAIN_POLY(0:M_POLY-1), SEC, UTC, TAI, &
     &           MIN_R8, TIM_TSYS, TIM_BEG, TIM_END, TSYS_VAL, POLY_VAL, &
     &           UTC_TAG, AVR_CAB, MAX_DEV, SKY_FRQ, REC_FRQ
      LOGICAL*4  FL_USE, FL_FOUND
!
      INTEGER*4  NBUF, J1,  J2,  J3,  J4 , J5 , J6,  J7,  J8,  J9, J10, &
     &           J11, J12, J13, J14, J15, J16, J17, J18, J19, J20, J21, &
     &           J22, J23, J24, J25, J26, J27, J28, J29, J30, J31, J32, &
     &           J33, J34, J35, J36, J37, J38, NUMB_FRQ, IND_POL, &
     &           L_NOD, L_DEG, NZO_REF, &
     &           FRG_CROSS(2,M_FRQ), FRQ_CROSS(2,M_FRQ), POL_CROSS(2,M_FRQ), &
     &           CROSS_COL(M_CHN), LIND, IND(2,MIND), IND_STA, IND_FRG, &
     &           IND_SOU, IND_SCA, IND_SCA2, IND_FRQ, L_POLY, MJD_NEW_YEAR, MJD, DOY, &
     &           IHR, IND_TSYS, IND_STA_BAS, N_ONS, &
     &           N_CAB, N_MET, MJD_TAG, K_MET, K_CAB, SIGN_CAB, N_ITER, &
     &           IV(MBUF), NP, IND_MAX_DEV, N_TSYS, NUM_TSYS_FRG(PIM__MFRG), &
     &           IND_OBS, UV_IND, UV_IND_BEG, UV_IND_END, N_MIS, &
     &           PIM_MIN_FRG, PIM_MAX_FRG, IER
      REAL*8     CROSS_SCA_TSYS(PIM__MSCA,PIM__MFRG), TIM_DIF
      INTEGER*4  PIMA__RR, PIMA__LL ; PARAMETER  ( PIMA__RR = 1 ) ; PARAMETER  ( PIMA__LL = 2 ) ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      CHARACTER  DATE_STR*30, WORD4*64, WORD5*64
      CHARACTER, EXTERNAL ::  MJDSEC_TO_DATE*30, GET_CDATE*19
      INTEGER*4, EXTERNAL ::  ADD_CLIST, ILEN, I_LEN, LTM_DIF
!      
      CALL GETENVAR ( 'PIMAVAR_MIN_FRG', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, PIM_MIN_FRG )
        ELSE 
           PIM_MIN_FRG = 1
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_MAX_FRG', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, PIM_MAX_FRG )
        ELSE 
           PIM_MAX_FRG = PIM%NFRG
      END IF
!
      IND_STA = -1
      ALLOCATE ( BUF(MBUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( LEN(BUF(1))*MBUF, STR )
           CALL ERR_LOG ( 8231, IUER, 'PIMA_GET_ANTAB', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array BUF' )
           RETURN
      END IF
!
      ALLOCATE ( TIM_TSYS_ARR(M_CHN,M_TIM,PIM%NFRG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*M_CHN*M_TIM*PIM%NFRG*PIM%NPOL, STR )
           CALL ERR_LOG ( 8232, IUER, 'PIMA_GET_ANTAB', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array TIM_TSYS_ARR' )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
      ALLOCATE ( TSYS_ARR(M_CHN,M_TIM,PIM%NFRG,PIM%NPOL), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*M_TIM*M_CHN*PIM%NFRG, STR )
           CALL ERR_LOG ( 8233, IUER, 'PIMA_GET_ANTAB', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array TSYS_ARR' )
           DEALLOCATE ( TIM_TSYS_ARR )
           DEALLOCATE ( BUF     )
           RETURN
      END IF
!
      ALLOCATE ( NUM_TSYS(M_FRQ,M_TIM,PIM%NFRG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*M_TIM*M_FRQ*PIM%NFRG, STR )
           CALL ERR_LOG ( 8234, IUER, 'PIMA_GET_ANTAB', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array NUM_TSYS' )
           DEALLOCATE ( TSYS_ARR )
           DEALLOCATE ( TIM_TSYS_ARR )
           DEALLOCATE ( BUF     )
           RETURN
      END IF
!
      ALLOCATE ( IND_SOU_ARR(M_TIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*M_TIM*M_CHN, STR )
           CALL ERR_LOG ( 8235, IUER, 'PIMA_GET_ANTAB', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array IND_SOU_ARR' )
           DEALLOCATE ( NUM_TSYS )
           DEALLOCATE ( TSYS_ARR )
           DEALLOCATE ( TIM_TSYS_ARR )
           DEALLOCATE ( BUF     )
           RETURN
      END IF
!
! --- Read antab file and out its contents in BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( ANTAB_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8236, IUER, 'PIMA_GET_ANTAB', 'Failure in '// &
     &         'an attempt to read antab file '//ANTAB_FILE )
           GOTO 810
      END IF
!
      IF ( ANTAB_MODE == 'evn_antab_file' ) THEN
!
! ======== EVN flavor of antab format
!
           CALL EXWORD ( BUF(1), MIND, LIND, IND, REG, -2 )
           IF ( BUF(1)(1:27) == '! Amplitude calibration for' .OR. &
                BUF(1)(1:32) == '! Amplitude calibration data for' ) THEN
!
! ------------- Get station name
!
                IF ( BUF(1)(1:27) == '! Amplitude calibration for' ) THEN
                     STA_NAM = BUF(1)(IND(1,5):IND(2,5))
                   ELSE IF ( BUF(1)(1:32) == '! Amplitude calibration data for' ) THEN
                     STA_NAM = BUF(1)(IND(1,6):IND(2,6))
                END IF
!
! ------------- Determine station index
!
                IND_STA = 0
                CALL CLRCH ( STR )
                DO 410 J1=1,PIM%NSTA
                   IF ( STA_NAM == PIM%STA(J1)%NAME ) IND_STA = J1
                   STR(I_LEN(STR)+2:) = PIM%STA(J1)%NAME 
 410            CONTINUE
                IF ( IND_STA == 0 ) THEN
                     CALL ERR_LOG ( 8237, IUER, 'PIMA_GET_ANTAB', 'Failure '// &
     &                   'in an attempt to parse antab file '// &
     &                    ANTAB_FILE(1:I_LEN(ANTAB_FILE))//' -- station '// &
     &                    STA_NAM//' did not observe in experiment '// &
     &                    PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))// &
     &                    ' Observing stations: '//STR )
                     GOTO 810
                END IF
              ELSE
                CALL ERR_LOG ( 8238, IUER, 'PIMA_GET_ANTAB', 'Failure in '// &
     &              'an attempt to parse evn_antab_file '// &
     &               ANTAB_FILE(1:I_LEN(ANTAB_FILE))//' the fist line does '// &
     &              'NOT have magic Amplitude calibration for' )
                GOTO 810
           END IF
         ELSE IF ( ANTAB_MODE == 'pima_antab_file' ) THEN
!
! ======== PIMA flavor of antab format
!
!
! -------- Check format label
!
           IF ( BUF(1)(1:50) == '# LOG-ANTAB Format  Version of 2009.08.07' ) THEN
                CONTINUE
              ELSE
                CALL ERR_LOG ( 8239, IUER, 'PIMA_GET_ANTAB', 'Failure in '// &
     &              'an attempt to parse LOG-ANTAB file '// &
     &               ANTAB_FILE(1:I_LEN(ANTAB_FILE))//' the fist line does '// &
     &              'NOT have magic amplitude calibration for' )
                GOTO 810
           END IF
!
! -------- Extract station name and its index
!
           IND_STA = 0
           DO 420 J2=1,NBUF
              IF ( BUF(J2)(1:8) == 'STATION:' ) THEN
                   STA_NAM = BUF(J2)(11:18)
                   DO 430 J3=1,PIM%NSTA
                      IF ( STA_NAM == PIM%STA(J3)%IVS_NAME ) IND_STA = J3
                      IF ( STA_NAM == PIM%STA(J3)%NAME     ) IND_STA = J3
 430               CONTINUE
                   GOTO 820
              END IF
 420       CONTINUE
 820       CONTINUE
           IF ( IND_STA == 0 ) THEN
                CALL ERR_LOG ( 8240, IUER, 'PIMA_GET_ANTAB', 'Failure '// &
     &              'in an attempt to parse antab file '// &
     &               ANTAB_FILE(1:I_LEN(ANTAB_FILE))//' -- station '// &
     &              STA_NAM //' did not observe in experiment '// &
     &               PIM%CONF%SESS_CODE )
                GOTO 810
           END IF
      END IF
!
! --- Initlialize VTD object
!
      IF ( VTD%STATUS .NE. VTD__INIT ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_INIT ( VTD,  IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8241, IUER, 'PIMA_GET_ANTAB', 'Error in an '// &
     &              'attempt to initialize VTD oibject' )
                GOTO 810
           END IF
      END IF
!
      IF ( VTD%STATUS .NE. VTD__LOAD ) THEN
!
! -------- Read and parse VTD configuration file
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_CONF ( PIM%CONF%VTD_CONFIG_FILE, VTD, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8242, IUER, 'PIMA_GET_ANTAB', 'Error in '// &
     &              'an attempt to read configuration file '// &
     &               PIM%CONF%VTD_CONFIG_FILE )
                GOTO 810
          END IF
!
! ------- Load catalogues, ephemerides, EOP series and other data files
!
          CALL ERR_PASS ( IUER, IER )
          CALL VTD_LOAD ( VTD, PIM%NSTA, PIM%C_STA, PIM%NSOU, PIM%C_SOU, &
     &                    PIM%MJD_0, PIM%TAI_0, PIM%MJD_0, &
     &                    PIM%TAI_0 + PIM%TIM_R8(PIM%NEPC), IER )
          IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 8243, IUER, 'PIMA_GET_ANTAB', 'Error in an '// &
     &             'attempt to load the data into VTD data structure' )
               GOTO 810
          END IF
!
! ------- Disable automatic NERS update during run
!
          VTD%NERS%CNF%AGE_FCS = 1.D15
          VTD%NERS%CNF%AGE_SPL = 1.D15
!
          IF ( ILEN(PIM%CONF%EPHEMERIDES_FILE) > 0 ) THEN
!
! ------------ Read the ephemeride of the orbiting station
!
               PIM%NZO%FILNZO = PIM%CONF%EPHEMERIDES_FILE           
               CALL ERR_PASS ( IUER, IER )
               CALL VTD_READ_NZO ( PIM%NZO%FILNZO, PIM%NZO%NZO_NAME, PIM__MNZO, &
     &                             PIM%NZO%L_NZO, PIM%NZO%MJD_ARR, &
     &                             PIM%NZO%TIM_ARR, PIM%NZO%POS_ARR, PIM%NZO%VEL_ARR, &
     &                             PIM%NZO%CENTER_NAME, PIM%NZO%REF_NAME, &
     &                             PIM%NZO%TIM_CODE, PIM%NZO%COO_CODE, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 6315, IUER, 'PIMA_GET_ANTAB', 'Error in an '// &
     &                  'attempt to read NZO data into VTD data structure' )
                    RETURN 
               END IF
               IF ( PIM%NZO%CENTER_NAME == 'EARTH BARYCENTER' ) THEN
                    NZO_REF = VTD__EME
                  ELSE 
                    CALL ERR_LOG ( 6316, IUER, 'PIMA_GET_ANTAB', 'Unsupported '// &
     &                  'coordinate center name: '//PIM%NZO%CENTER_NAME )
                    RETURN 
               END IF
!
! ------------ Expand the orbiting station ephemeride into the B-spline basis
!
               L_NOD = MIN ( PIM%NZO%L_NZO/PIMA__NZO_NOT_SCAL, VTD__M_NOD )
               L_DEG = 3
               CALL ERR_PASS ( IUER, IER )
               CALL VTD_LOAD_OBJ_NZO ( PIM%NZO%NZO_NAME, VTD__ES, VTD__OR, &
     &                                 NZO_REF, PIM%NZO%TIM_CODE, &
     &                                 VTD, PIM%NZO%L_NZO, PIM%NZO%MJD_ARR, &
     &                                 PIM%NZO%TIM_ARR, PIM%NZO%POS_ARR, &
     &                                  L_NOD, L_DEG, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 6317, IUER, 'PIMA_GET_ANTAB', 'Error in an '// &
     &                  'attempt to read NZO data into VTD data structure' )
                    RETURN 
               END IF
          END IF
      END IF
!
! --- Initialization
!
      IND_SCA  = 0
      IND_SOU  = 0
      N_ONS    = 0
      N_CAB    = 0
      N_MET    = 0
      N_TSYS   = 0
      SIGN_CAB = 1
      CALL NOUT_R8 ( M_POLY, GAIN_POLY )
      CALL NOUT_I4 ( M_CHN,  CROSS_COL )
      CALL NOUT_I4 ( M_CHN*M_TIM*PIM%NFRG, NUM_TSYS    )
      CALL NOUT_I4 ( M_TIM,                IND_SOU_ARR )
      CALL NOUT_R8 ( M_CHN*M_TIM*PIM%NFRG, TIM_TSYS_ARR    )
      CALL NOUT_R8 ( M_CHN*M_TIM*PIM%NFRG*PIM%NPOL, TSYS_ARR )
      DATE_STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0, -2 )
      DATE_STR(6:) = '01.01_00:00:00.0'
      CALL DATE_TO_TIME ( DATE_STR, MJD_NEW_YEAR, SEC, -2 )
      L_POLY = 0
      FL_GAIN = .FALSE.
      FL_TSYS = .FALSE.
      FRQ_CROSS = 0
      FRG_CROSS = 0
      NUM_TSYS_FRG = 0
!
! --- Scan the buffer with constants of the antab file
!
      DO 440 J4=2,NBUF
         CALL EXWORD ( BUF(J4), MIND, LIND, IND, REG, -2 )
         IF ( LIND .GE. 4 ) THEN
              WORD4 = BUF(J4)(IND(1,4):IND(2,4))
            ELSE
              CALL CLRCH ( WORD4 )
         END IF
!
         IF ( LIND .GE. 5 ) THEN
              WORD5 = BUF(J4)(IND(1,5):IND(2,5))
            ELSE
              CALL CLRCH ( WORD5 )
         END IF
!
         CALL CLRCH ( STR )
         CALL INCH  ( J4, STR )
         IF (  BUF(J4)(IND(1,1):IND(2,1))== 'NUMB_METEO:' ) THEN
              CALL CHIN ( BUF(J4)(IND(1,2):IND(2,2)), N_MET )
           ELSE IF ( BUF(J4)(IND(1,1):IND(2,1)) == 'NUMB_CAB:' ) THEN
              CALL CHIN ( BUF(J4)(IND(1,2):IND(2,2)), N_CAB )
           ELSE IF ( BUF(J4)(IND(1,1):IND(2,1)) == 'SIGN_CAB:' ) THEN
              CALL CHIN ( BUF(J4)(IND(1,2):IND(2,2)), SIGN_CAB )
           ELSE IF ( BUF(J4)(IND(1,1):IND(2,1)) == 'DATA_ON:' ) THEN
              N_ONS = N_ONS + 1
!
! ----------- Parse the data_on date
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( BUF(J4)(IND(1,2):IND(2,2)), MJD_TAG, &
     &                            UTC_TAG, IER  )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J4, STR )
                   CALL ERR_LOG ( 8244, IUER, 'PIMA_GET_ANTAB', 'Error '// &
     &                 'in reading line '//STR(1:I_LEN(STR))// &
     &                 ' of the antab file '//ANTAB_FILE(1:I_LEN(ANTAB_FILE))// &
     &                 ' -- cannot parse the date '//BUF(J4)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
              TIM_ONS_ARR(1,N_ONS) = ( MJD_TAG - PIM%MJD_0 )*86400.0D0 + &
     &                               ( UTC_TAG - PIM%UTC_MTAI - PIM%TAI_0 )
!
! ----------- Parse the data_off date
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( BUF(J4)(IND(1,3):IND(2,3)), MJD_TAG, &
     &                            UTC_TAG, IER  )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J4, STR )
                   CALL ERR_LOG ( 8245, IUER, 'PIMA_GET_ANTAB', 'Error '// &
     &                 'in reading line '//STR(1:I_LEN(STR))// &
     &                 ' of the antab file '//ANTAB_FILE(1:I_LEN(ANTAB_FILE))// &
     &                 ' -- cannot parse the date '//BUF(J4)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
              TIM_ONS_ARR(2,N_ONS) = ( MJD_TAG - PIM%MJD_0 )*86400.0D0 + &
     &                               ( UTC_TAG - PIM%UTC_MTAI - PIM%TAI_0 )
              SOU_ONS(N_ONS) = BUF(J4)(IND(1,4):IND(2,4))
            ELSE IF ( BUF(J4)(IND(1,1):IND(2,1)) == 'GAIN' ) THEN
              IF ( BUF(J4)(IND(1,4):IND(1,4)+4) == 'DPFU=' ) THEN
                   READ ( UNIT=BUF(J4)(IND(1,5):IND(2,5)), FMT=*, IOSTAT=IER ) DPFU(1)
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8246, IUER, 'PIMA_GET_ANTAB', 'Error '// &
     &                      'in reading DPFU from line '//STR(1:I_LEN(STR))// &
     &                      ' of the antab file '//ANTAB_FILE )
                        GOTO 810
                   END IF
!
                   READ ( UNIT=BUF(J4)(IND(1,6):IND(2,6)), FMT=*, IOSTAT=IER ) DPFU(2)
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8247, IUER, 'PIMA_GET_ANTAB', 'Error '// &
     &                      'in reading DPFU from line '//STR(1:I_LEN(STR))// &
     &                      ' of the antab file '//ANTAB_FILE )
                        GOTO 810
                   END IF
!
                   READ ( UNIT=BUF(J4)(IND(1,8):IND(2,8)), FMT=*, IOSTAT=IER ) FRQ_DPFU(1)
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8248, IUER, 'PIMA_GET_ANTAB', 'Error '// &
     &                      'in reading DPFU from line '//STR(1:I_LEN(STR))// &
     &                      ' of the antab file '//ANTAB_FILE )
                        GOTO 810
                   END IF
!
                   READ ( UNIT=BUF(J4)(IND(1,9):IND(2,9)), FMT=*, IOSTAT=IER ) FRQ_DPFU(2)
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8249, IUER, 'PIMA_GET_ANTAB', 'Error '// &
     &                      'in reading DPFU from line '//STR(1:I_LEN(STR))// &
     &                      ' of the antab file '//ANTAB_FILE )
                        GOTO 810
                   END IF
                   FRQ_DPFU(1) = FRQ_DPFU(1)*1.D6
                   FRQ_DPFU(2) = FRQ_DPFU(2)*1.D6
                 ELSE
                   CALL ERR_LOG ( 8250, IUER, 'PIMA_GET_ANTAB', 'Cannot '// &
     &                 'parse GAIN entry in the antabl file '// &
     &                  ANTAB_FILE(1:I_LEN(ANTAB_FILE)) )
                   GOTO 810
              END IF
            ELSE IF ( BUF(J4)(IND(1,1):IND(1,1)+4) == 'POLY=' ) THEN
              DO 460 J6=2,LIND
                 READ ( UNIT=BUF(J4)(IND(1,J6):IND(2,J6)), FMT=*, IOSTAT=IER ) GAIN_POLY(J6-2)
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8251, IUER, 'PIMA_GET_ANTAB', 'Error '// &
     &                    'in reading POLY from line '//STR(1:I_LEN(STR))// &
     &                    ' of the antab file '//ANTAB_FILE )
                      GOTO 810
                 END IF
 460          CONTINUE
              FL_GAIN = .TRUE.
              L_POLY = LIND - 2
            ELSE IF ( BUF(J4)(IND(1,1):IND(2,1)) == '!Column' ) THEN
              IF ( ILEN(BUF(J4)(IND(1,3):IND(2,3))) < 3 ) THEN
                   CALL ERR_LOG ( 8252, IUER, 'PIMA_GET_ANTAB', 'Error '// &
     &                 'in processing !Column line '//STR(1:I_LEN(STR))// &
     &                 ' of the antab file '// &
     &                 ANTAB_FILE(1:I_LEN(ANTAB_FILE))//' too short 4th word' )
                   GOTO 810
              END IF
!
              CALL CHIN ( BUF(J4)(IND(1,3)+1:IND(2,3)-1), IND_FRQ )
              IF ( IND_FRQ < 1 .OR. IND_FRQ > PIM%NFRQ ) THEN
                   CALL ERR_LOG ( 8253, IUER, 'PIMA_GET_ANTAB', 'Error '// &
     &                 'in parsing !Column line '//STR(1:I_LEN(STR))// &
     &                 ' of the antab file '// &
     &                 ANTAB_FILE(1:I_LEN(ANTAB_FILE))//' wrong 4th word' )
                   GOTO 810
              END IF
!
              CALL CHIN ( BUF(J4)(IND(1,2):IND(2,2)), CROSS_COL(IND_FRQ) )
              IF ( CROSS_COL(IND_FRQ)  < 1       .OR. &
     &             CROSS_COL(IND_FRQ) > PIM%NFRQ ) THEN
                   CALL ERR_LOG ( 8254, IUER, 'PIMA_GET_ANTAB', 'Error '// &
     &                 'in parsing !Column line '//STR(1:I_LEN(STR))// &
     &                 ' of the antab file '// &
     &                 ANTAB_FILE(1:I_LEN(ANTAB_FILE))//' wrong 2nd word' )
                   GOTO 810
              END IF
            ELSE IF ( WORD4(1:5) == 'scan='   .AND. &
     &                WORD5(1:7) == 'source='       ) THEN
              SOU_NAM = BUF(J4)(IND(1,5)+7:IND(2,5))
              IND_SOU = 0
              DO 470 J7=1,PIM%NSOU
                 IF ( SOU_NAM == PIM%SOU(J7)%IVS_NAME ) IND_SOU = J7
 470          CONTINUE
              IF ( IND_SOU == 0 ) THEN
                   CALL ERR_LOG ( 8255, IUER, 'PIMA_GET_ANTAB', 'Error '// &
     &                 'in parsing source= line '//STR(1:I_LEN(STR))// &
     &                 ' of the antab file '// &
     &                 ANTAB_FILE(1:I_LEN(ANTAB_FILE))//' source '// &
     &                 SOU_NAM//' was not observed in experiment '// &
     &                 PIM%CONF%SESS_CODE  )
                   GOTO 810
              END IF
            ELSE IF ( BUF(J4)(IND(1,1):IND(2,1)) == 'NUMB_FRQ:' ) THEN
              CALL CHIN ( BUF(J4)(IND(1,2):IND(2,2)), NUMB_FRQ )
              IF ( NUMB_FRQ > M_FRQ )   THEN
                   CALL CLRCH ( STR  )
                   CALL CLRCH ( STR1 ) 
                   CALL INCH  ( NUMB_FRQ,  STR  )
                   CALL INCH  ( PIM__MFRQ, STR1 )
                   CALL ERR_LOG ( 8255, IUER, 'PIMA_GET_ANTAB', 'Antenna '// &
     &                 'calibration file '//TRIM(ANTAB_FILE)//' has too '// &
     &                 'many frequencies: '//TRIM(STR)//' -- more than '// &
     &                 '4 times of PIMA__MFRQ = '//STR1 )
                   RETURN 
              END IF
            ELSE IF ( BUF(J4)(IND(1,1):IND(2,1)) == 'FRQ:' ) THEN
              CALL CHIN ( BUF(J4)(IND(1,2):IND(2,2)), IND_FRQ )
              READ ( UNIT=BUF(J4)(IND(1,5):IND(2,5)), FMT='(F9.2)' ) SKY_FRQ
              SKY_FRQ = SKY_FRQ*1.D6
              IF ( LIND .GE. 6 ) THEN
                   POL_STR = BUF(J4)(IND(1,6):IND(2,6))
                 ELSE 
                   POL_STR = 'R'
              END IF
!
! ----------- Build the cross-table from the frequency index in PIMA
! ----------- against the frequency index in the antab file
!
!!              DO 480 J8=1,PIM%NFRG
              DO 480 J8=PIM_MIN_FRG,PIM_MAX_FRG
                 DO 490 J9=1,PIM%NFRQ
                    IF ( PIM%FRQ(J9,J8)%SIDE_BAND == 1 ) THEN
                         REC_FRQ = PIM%FRQ(J9,J8)%FREQ
                       ELSE
                         REC_FRQ = PIM%FRQ(J9,J8)%FREQ + &
     &                             PIM%FRQ(J9,J8)%BAND_WIDTH
                    END IF
                    IF ( DABS ( SKY_FRQ - REC_FRQ ) < &
     &                   PIM%FRQ(J9,J8)%BAND_WIDTH/2.0D0 ) THEN
                         IF ( FRQ_CROSS(1,IND_FRQ) == 0 ) THEN
                              FRQ_CROSS(1,IND_FRQ) = J9
                              FRG_CROSS(1,IND_FRQ) = J8
                              IF ( PIM%NPOL == 1 ) THEN
                                   POL_CROSS(1,IND_FRQ) = PIMA__RR
                                 ELSE 
                                   IF ( POL_STR == 'R' ) THEN
                                        POL_CROSS(1,IND_FRQ) = PIMA__RR
                                      ELSE  
                                        POL_CROSS(1,IND_FRQ) = PIMA__LL
                                   END IF
                              END IF
                            ELSE
                              FRQ_CROSS(2,IND_FRQ) = J9
                              FRG_CROSS(2,IND_FRQ) = J8
                              IF ( PIM%NPOL == 1 ) THEN
                                   POL_CROSS(2,IND_FRQ) = PIMA__RR
                                 ELSE 
                                   IF ( POL_STR == 'R' ) THEN
                                        POL_CROSS(2,IND_FRQ) = PIMA__RR
                                      ELSE  
                                        POL_CROSS(2,IND_FRQ) = PIMA__LL
                                   END IF
                              END IF
                         END IF
                    END IF
 490             CONTINUE
 480          CONTINUE
            ELSE IF ( BUF(J4)(IND(1,1):IND(2,1)) == 'TSYS:' ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( BUF(J4)(IND(1,5):IND(2,5)), MJD, UTC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J4, STR )
                   CALL ERR_LOG ( 8256, IUER, 'PIMA_GET_ANTAB', 'Error '// &
     &                 'in parsing date field in line '//STR(1:I_LEN(STR))// &
     &                 ' of the antab file '//ANTAB_FILE )
                   GOTO 810
              END IF
!
              N_TSYS = N_TSYS + 1
              CALL ERR_PASS ( IUER, IER )
              CALL VTD_UTC_TO_TAI ( VTD, MJD, UTC, TAI, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8257, IUER, 'PIMA_GET_ANTAB', 'Error '// &
     &                 'in converting UTC to TAI' )
                   GOTO 810
              END IF
              IND_SOU = 0
              DO 4100 J10=1,PIM%NSOU
                 IF ( BUF(J4)(IND(1,4):IND(2,4)) == PIM%SOU(J10)%NAME ) THEN
                      IND_SOU = J10
                    ELSE IF ( BUF(J4)(IND(1,4):IND(2,4)) == TRIM(PIM%SOU(J10)%IVS_NAME)   ) THEN
                      IND_SOU = J10
                    ELSE IF ( BUF(J4)(IND(1,4):IND(2,4)) == TRIM(PIM%SOU(J10)%J2000_NAME) ) THEN
                      IND_SOU = J10
                    ELSE IF ( BUF(J4)(IND(1,4):IND(2,4)) == TRIM(PIM%SOU(J10)%B1950_NAME) ) THEN
                      IND_SOU = J10
                    ELSE IF ( BUF(J4)(IND(1,4):IND(2,4)) == TRIM(PIM%SOU(J10)%DB_NAME)    ) THEN
                      IND_SOU = J10
                 END IF
 4100         CONTINUE
              IF ( IND_SOU == 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J4, STR )
                   CALL ERR_PASS ( IUER, IER )
                   CALL ERR_LOG ( 8258, IER, 'PIMA_GET_ANTAB', 'Unknown source '// &
     &                  BUF(J4)(IND(1,4):IND(2,4))//' was found at line '// &
     &                  STR(1:I_LEN(STR))//' of the antab file '// &
     &                  ANTAB_FILE(1:I_LEN(ANTAB_FILE)) )
                   WRITE ( 6, * ) 'Nevertheless, continue'
                   GOTO 440
              END IF
              TIM_TSYS = (MJD - PIM%MJD_0)*86400.0D0 + (TAI - PIM%TAI_0)
              IND_SCA = 0
              TIM_DIF = 1.D30
              DO 4110 J11=1,PIM%NSCA
                 TIM_BEG =  1.D30
                 TIM_END = -1.D30
                 DO 4120 J12=1,PIM%SCA(J11)%NBAS
                    IND_OBS = PIM%SCA(J11)%OBS_IND(J12)
                    IND_STA_BAS = 0
                    IF ( PIM%OBS(IND_OBS)%STA_IND(1) == IND_STA ) IND_STA_BAS = 1
                    IF ( PIM%OBS(IND_OBS)%STA_IND(2) == IND_STA ) IND_STA_BAS = 2
                    IF ( IND_STA_BAS == 0 ) GOTO 4120
                    IF ( PIM%OBS(IND_OBS)%SOU_IND .NE. IND_SOU ) GOTO 4120
                    TIM_BEG = MIN ( TIM_BEG, PIM%OBS(IND_OBS)%TIM_BEG )
                    TIM_END = MAX ( TIM_END, PIM%OBS(IND_OBS)%TIM_END )
 4120            CONTINUE
!
                 IF ( DABS(TIM_TSYS - (TIM_BEG + TIM_END)/2.0D0) < TIM_DIF ) THEN
                      TIM_DIF = DABS(TIM_TSYS - (TIM_BEG + TIM_END)/2.0D0) 
                      IND_SCA = J11
                 END IF
 4110         CONTINUE
              IF ( TIM_DIF > TSYS__TIM_TOL ) IND_SCA = 0
!
              IF ( IND_SCA > 0 ) THEN
                   DO 4130 J13=1,NUMB_FRQ
                      IF ( INDEX ( BUF(J4)(IND(1,5+J13):IND(2,5+J13)), '*' ) > 0 ) THEN
                           TSYS_VALS(J13) = 2.0D0*TSYS_VALS(J13) 
                         ELSE 
                           READ ( UNIT=BUF(J4)(IND(1,5+J13):IND(2,5+J13)), FMT='(F8.1)' ) TSYS_VALS(J13)

                      END IF
                      IF ( FRQ_CROSS(1,J13) .NE. 0    .AND. &
     &                     TSYS_VALS(J13) > TSYS__MIN .AND. &
     &                     TSYS_VALS(J13) < TSYS__MAX       ) THEN
!
                           TSYS_ARR(FRQ_CROSS(1,J13),IND_SCA,FRG_CROSS(1,J13),POL_CROSS(1,J13)) = TSYS_VALS(J13)
                           TIM_TSYS_ARR(FRQ_CROSS(1,J13),IND_SCA,FRG_CROSS(1,J13)) = TIM_TSYS
                           NUM_TSYS(FRQ_CROSS(1,J13),IND_SCA,FRG_CROSS(1,J13)) = NUM_TSYS(J13,IND_SCA,FRG_CROSS(1,J13)) + 1
                           FL_TSYS = .TRUE.
                           IND_SOU_ARR(IND_SCA) = IND_SOU
                      END IF
                      IF ( FRQ_CROSS(2,J13) .NE. 0 .AND. &
     &                     TSYS_VALS(J13) > TSYS__MIN .AND. &
     &                     TSYS_VALS(J13) < TSYS__MAX       ) THEN
!
                           TSYS_ARR(FRQ_CROSS(2,J13),IND_SCA,FRG_CROSS(2,J13),POL_CROSS(2,J13)) = TSYS_VALS(J13)
                           TIM_TSYS_ARR(FRQ_CROSS(2,J13),IND_SCA,FRG_CROSS(2,J13)) = TIM_TSYS
                           NUM_TSYS(FRQ_CROSS(2,J13),IND_SCA,FRG_CROSS(2,J13)) = NUM_TSYS(J13,IND_SCA,FRG_CROSS(2,J13)) + 1
                           FL_TSYS = .TRUE.
                           IND_SOU_ARR(IND_SCA) = IND_SOU
                      END IF
 4130              CONTINUE
                 ELSE
                   IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                        WRITE ( 6, 210 ) PIM%C_STA(IND_STA), J4, &
     &                                   BUF(J4)(1:I_LEN(BUF(J4)))
 210                    FORMAT ( 'Station ',A, ' Did not find scan for line ', &
     &                            I5,' : ', A )
                   END IF
              END IF
            ELSE IF ( BUF(J4)(IND(1,1):IND(2,1)) == 'INDEX' ) THEN
              CONTINUE
            ELSE IF ( BUF(J4)(IND(1,1):IND(2,1)) == '/' ) THEN
              CONTINUE
            ELSE IF ( BUF(J4)(IND(1,1):IND(1,1)) .NE. '!'  .AND. &
     &                ANTAB_MODE == 'evn_antab_file'             ) THEN
              CALL CHIN ( BUF(J4)(IND(1,1):IND(2,1)), DOY )
              IF ( DOY < 1  .OR. DOY > 366 ) THEN
                   CALL ERR_LOG ( 8259, IUER, 'PIMA_GET_ANTAB', 'Error '// &
     &                 'in parsing DOY field in line '//STR(1:I_LEN(STR))// &
     &                 ' of the antab file '//ANTAB_FILE )
                   GOTO 810
              END IF
              READ ( UNIT=BUF(J4)(IND(1,2):IND(1,2)+1), FMT=*, IOSTAT=IER ) IHR
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8260, IUER, 'PIMA_GET_ANTAB', 'Error '// &
     &                 'in parsing hour field in line '//STR(1:I_LEN(STR))// &
     &                 ' of the antab file '//ANTAB_FILE )
                   GOTO 810
              END IF
              IF ( IHR < 1 .OR. IHR .GE. 24 ) THEN
                   CALL ERR_LOG ( 8261, IUER, 'PIMA_GET_ANTAB', 'Wrong value '// &
     &                 'in hour field in line '//STR(1:I_LEN(STR))// &
     &                 ' of the antab file '//ANTAB_FILE )
                   GOTO 810
              END IF
!
              READ ( UNIT=BUF(J4)(IND(1,2)+3:IND(2,2)), FMT=*, IOSTAT=IER ) MIN_R8
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8262, IUER, 'PIMA_GET_ANTAB', 'Error '// &
     &                 'in parsing minutes field in line '//STR(1:I_LEN(STR))// &
     &                 ' of the antab file '//ANTAB_FILE )
                   GOTO 810
              END IF
              IF ( MIN_R8 < 0.0D0 .OR. MIN_R8 .GE. 60.0D0 ) THEN
                   CALL ERR_LOG ( 8263, IUER, 'PIMA_GET_ANTAB', 'Wrong value '// &
     &                 'in minutes field in line '//STR(1:I_LEN(STR))// &
     &                 ' of the antab file '//ANTAB_FILE )
                   GOTO 810
              END IF
!
              UTC = IHR*3600.0D0 + MIN_R8*60.0D0
!
              MJD = MJD_NEW_YEAR + DOY - 1
              CALL ERR_PASS ( IUER, IER )
              CALL VTD_UTC_TO_TAI ( VTD, MJD, UTC, TAI, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8264, IUER, 'PIMA_GET_ANTAB', 'Error '// &
     &                 'in converting UTC to TAI' )
                   GOTO 810
              END IF
              TIM_TSYS = (MJD - PIM%MJD_0)*86400.0D0 + (TAI - PIM%TAI_0)
              IND_SCA = 0
              DO 4140 J14=1,PIM%NSCA
                 UV_IND_BEG = 0
                 UV_IND_END = 0
                 DO 4150 J15=1,PIM%SCA(J14)%NBAS
                     IF ( PIM%OBS(PIM%SCA(J14)%OBS_IND(J15))%STA_IND(1) == IND_STA .OR. &
     &                    PIM%OBS(PIM%SCA(J14)%OBS_IND(J15))%STA_IND(2) == IND_STA      ) THEN
                          UV_IND_BEG = PIM%OBS(PIM%SCA(J14)%OBS_IND(J15))%UV_IND(1,1)
                          UV_IND_END = PIM%OBS(PIM%SCA(J14)%OBS_IND(J15))%UV_IND(PIM%OBS(PIM%SCA(J14)%OBS_IND(J15))%NUM_EPC(PIM%CONF%FRQ_GRP),1)
                     END IF
 4150            CONTINUE
                 IF ( UV_IND_BEG > 0  .AND.  UV_IND_END > 0 ) THEN
                      TIM_BEG = PIM%TIM_R8(PIM%UV_IND(UV_IND_BEG)%TIM_IND)
                      TIM_END = PIM%TIM_R8(PIM%UV_IND(UV_IND_END)%TIM_IND)
                      IF ( TIM_TSYS .GE. TIM_BEG - TIM_EPS  .AND. &
     &                     TIM_TSYS .LE. TIM_END + TIM_EPS        ) THEN
                           IND_SCA = J14
                      END IF
                 END IF
 4140         CONTINUE
              IF ( IND_SCA > 0 ) THEN
                   DO 4160 J16=3,LIND
                      READ ( UNIT=BUF(J4)(IND(1,J16):IND(2,J16)), FMT=*, IOSTAT=IER ) TSYS_VAL
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR1 )
                           CALL INCH  ( J16, STR1 )
                           CALL ERR_LOG ( 8265, IUER, 'PIMA_GET_ANTAB', &
     &                         'Error in parsing field '//STR1(1:I_LEN(STR1))// &
     &                         ' of line '//STR(1:I_LEN(STR))//' of '// &
     &                         'antab file '//ANTAB_FILE )
                           GOTO 810
                      END IF
!
! ------------------- Ignore negative values of Tsys
!
                      IF ( TSYS_VAL .LE. TSYS__MIN ) GOTO 4160
                      FL_TSYS = .TRUE.
                      IND_FRG = PIM%CONF%FRQ_GRP
!@@           IND_POL = 1 ! $$$ TEMPORARY $$$ TEMPORARY $$$ TEMPORARY $$$
                      DO 4170 J17=1,PIM%NFRQ
                         IF ( J16-2 == CROSS_COL(J17) ) THEN
                              NUM_TSYS(J17,IND_SCA,IND_FRG) = NUM_TSYS(J17,IND_SCA,IND_FRG) + 1
                              TSYS_ARR(J17,IND_SCA,IND_FRG,IND_POL) = ( TSYS_ARR(J17,IND_SCA,IND_FRG,IND_POL)* &
     &                           (NUM_TSYS(J17,IND_SCA,IND_FRG)-1) + TSYS_VAL)/NUM_TSYS(J17,IND_SCA,IND_FRG)
                              TIM_TSYS_ARR(J17,IND_SCA,IND_FRG)  = ( TIM_TSYS_ARR(J17,IND_SCA,IND_FRG)* &
     &                           (NUM_TSYS(J17,IND_SCA,IND_FRG)-1) + TIM_TSYS)/NUM_TSYS(J17,IND_SCA,IND_FRG)
                         END IF
 4170                 CONTINUE
 4160              CONTINUE
                   IND_SOU_ARR(IND_SCA) = IND_SOU
                ELSE
                   CALL CLRCH ( STR  )
                   CALL INCH  ( J4, STR  )
                   CALL ERR_LOG ( 2399, IUER, 'PIMA_GET_ANTAB', 'Trap of internal '// &
     &                 'control: cannot match a source for line '//STR(1:I_LEN(STR))// &
     &                 ' -- '//BUF(J4) )
                   RETURN
              END IF
         END IF
 440  CONTINUE
!
      IF ( FL_TSYS ) THEN
           PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOI = 0
           DO 4180 J18=1,PIM%NSCA
              FL_USE = .FALSE.
!!              DO 4190 J19=1,PIM%NFRG
              DO 4190 J19=PIM_MIN_FRG,PIM_MAX_FRG
                 DO 4200 J20=1,PIM%NFRQ
                    IF ( NUM_TSYS(J20,J18,J19) > 0 ) FL_USE = .TRUE.
 4200            CONTINUE
                 IF ( FL_USE ) THEN
                      PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOI = PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOI + 1
                      CROSS_SCA_TSYS(J18,J19) = PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOI
                      NUM_TSYS_FRG(J19) = NUM_TSYS_FRG(J19) + 1
                    ELSE
                      CROSS_SCA_TSYS(J18,J19) = 0
                 END IF
 4190         CONTINUE
 4180      CONTINUE
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE  ( 6, '(A)' ) 'Station '//PIM%C_STA(IND_STA)// &
     &                         ' Antab_mode: '//ANTAB_MODE
           WRITE  ( 6, 220 ) PIM%C_STA(IND_STA), FL_TSYS, FL_GAIN
 220       FORMAT ( 'Station ', A, ' Found_Tsys: ', L1, &
     &              ' Found_gain: ', L1 )
           IF ( FL_GAIN ) THEN
                WRITE  ( 6, 230 ) PIM%C_STA(IND_STA), DPFU, FRQ_DPFU
 230            FORMAT ( 'Station ', A, ' DPFU: ', 2(F8.5,1X), &
     &                    1X, ' DPU_FRQ: ',2(1PD12.5,1X) )
                WRITE  ( 6, 240 ) PIM%C_STA(IND_STA), L_POLY, &
     &                            GAIN_POLY(0:L_POLY)
 240            FORMAT ( 'Station ', A, ' Gain polynomial degree: ',I2, &
     &                   ' Gain polynomial: ',8(1PD12.5,1X) )
           END IF
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                WRITE ( 6, 250 ) PIM%C_STA(IND_STA), CROSS_COL(1:PIM%NFRQ)
 250            FORMAT ( 'Station ',A, ' Cross_col: ',16(I2,1X) )
           END IF
!
           WRITE  ( 6, 260 ) PIM%C_STA(IND_STA), N_ONS
 260       FORMAT ( 'Station ',A, ' Number of on_source entries: ', I5 )
           WRITE  ( 6, 270 ) PIM%C_STA(IND_STA), N_MET
 270       FORMAT ( 'Station ',A, ' Number of weather entries: ', I5 )
           WRITE  ( 6, 280 ) PIM%C_STA(IND_STA), N_CAB, SIGN_CAB
 280       FORMAT ( 'Station ',A, ' Number of cable calibration entries: ', &
     &               I5, ' SIGN_CAB = ', I2 )
           WRITE  ( 6, 290 ) PIM%C_STA(IND_STA), N_TSYS, &
     &                       PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOI
 290       FORMAT ( 'Station ',A, ' Number of Tsys entries: ', I6, &
     &              ' Number of obs with Tsys: ', I5 )
      END IF
!
      IF ( N_ONS > 0 ) THEN
!
! -------- Process DATA_ON/DATA_OFF flagging
!
           DO 4210 J21=1,PIM%NOBS
              IND_OBS = J21
              IF ( PIM%OBS(J21)%STA_IND(1) == IND_STA .OR. &
     &             PIM%OBS(J21)%STA_IND(2) == IND_STA      ) THEN
!
                   TIM_BEG = PIM%OBS(J21)%TIM_BEG
                   TIM_END = PIM%OBS(J21)%TIM_END
                   DO 4220 J22=1,N_ONS
                      IF ( SOU_ONS(J22) == PIM%C_SOU(PIM%OBS(J21)%SOU_IND) .AND. &
     &                     ( ( TIM_BEG .GE. TIM_ONS_ARR(1,J22) .AND. &
     &                         TIM_BEG .LE. TIM_ONS_ARR(2,J22)       ) .OR. &
     &                       ( TIM_END .GE. TIM_ONS_ARR(1,J22) .AND. &
     &                         TIM_END .LE. TIM_ONS_ARR(2,J22)       ) ) ) THEN
!!                         DO 4230 J23=1,PIM%NFRG
                         DO 4230 J23=PIM_MIN_FRG,PIM_MAX_FRG
                            DO 4240 J24=1,PIM%OBS(J21)%NUM_EPC(J23)
                               UV_IND = PIM%OBS(IND_OBS)%UV_IND(J24,J23)
                               IF ( PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND) < &
     &                               TIM_ONS_ARR(1,J22) ) THEN
!
! ---------------------------------- The AP time is BEFORE data_on
!
                                     PIM%OBS(J21)%CORR_FLAG(J24,J23) = -1
                                END IF
                                IF ( PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND) > &
     &                               TIM_ONS_ARR(2,J22) ) THEN
!
! ---------------------------------- The AP time is AFTER data_off
!
                                     PIM%OBS(J21)%CORR_FLAG(J24,J23) = -1
                                END IF
 4240                       CONTINUE
 4230                    CONTINUE
                      END IF
 4220              CONTINUE
              END IF
 4210      CONTINUE
      END IF
!
      IF ( N_MET > 0 ) THEN
           IF ( PIM%STA(IND_STA)%WEATHER%AVAIL      .AND. &
     &          PIM%STA(IND_STA)%WEATHER%NPOI > 0         ) THEN
!
                DEALLOCATE ( PIM%STA(IND_STA)%WEATHER%TIME_BEG )
                DEALLOCATE ( PIM%STA(IND_STA)%WEATHER%TIME_END )
                DEALLOCATE ( PIM%STA(IND_STA)%WEATHER%PRES     )
                DEALLOCATE ( PIM%STA(IND_STA)%WEATHER%TEMP     )
                DEALLOCATE ( PIM%STA(IND_STA)%WEATHER%HUMID    )
           END IF
!
           PIM%STA(IND_STA)%WEATHER%AVAIL = .TRUE.
           PIM%STA(IND_STA)%WEATHER%NPOI  = N_MET
!
           ALLOCATE ( PIM%STA(IND_STA)%WEATHER%TIME_BEG(PIM%STA(IND_STA)%WEATHER%NPOI), &
     &                STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*PIM%STA(IND_STA)%WEATHER%NPOI, STR )
                CALL ERR_LOG ( 8266, IUER, 'PIMA_GET_ANTAB', 'Failure to '// &
     &               'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &               'dynamic memory for PIM%STA(IND_STA)%WEATHER%PRES '// &
     &               'object' )
                RETURN
           END IF
!
           ALLOCATE ( PIM%STA(IND_STA)%WEATHER%TIME_END(PIM%STA(IND_STA)%WEATHER%NPOI), &
     &                STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*PIM%STA(IND_STA)%WEATHER%NPOI, STR )
                CALL ERR_LOG ( 8267, IUER, 'PIMA_GET_ANTAB', 'Failure to '// &
     &               'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &               'dynamic memory for PIM%STA(IND_STA)%WEATHER%PRES '// &
     &               'object' )
                RETURN
           END IF
!
           ALLOCATE ( PIM%STA(IND_STA)%WEATHER%PRES(PIM%STA(IND_STA)%WEATHER%NPOI), &
     &                STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*PIM%STA(IND_STA)%WEATHER%NPOI, STR )
                CALL ERR_LOG ( 8268, IUER, 'PIMA_GET_ANTAB', 'Failure to '// &
     &               'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &               'dynamic memory for PIM%STA(IND_STA)%WEATHER%PRES '// &
     &               'object' )
                RETURN
           END IF
!
           ALLOCATE ( PIM%STA(IND_STA)%WEATHER%TEMP(PIM%STA(IND_STA)%WEATHER%NPOI), &
     &                STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*PIM%STA(IND_STA)%WEATHER%NPOI, STR )
                CALL ERR_LOG ( 8269, IUER, 'PIMA_GET_ANTAB', 'Failure to '// &
     &               'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &               'dynamic memory for PIM%STA(IND_STA)%WEATHER%TEMP '// &
     &               'object' )
                RETURN
           END IF
!
           ALLOCATE ( PIM%STA(IND_STA)%WEATHER%HUMID(PIM%STA(IND_STA)%WEATHER%NPOI), &
     &                STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*PIM%STA(IND_STA)%WEATHER%NPOI, STR )
                CALL ERR_LOG ( 8270, IUER, 'PIMA_GET_ANTAB', 'Failure to '// &
                     'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &               'dynamic memory for PIM%STA(IND_STA)%WEATHER%HUMID '// &
     &               'object' )
                RETURN
           END IF
!
           K_MET = 0
           DO 4250 J25=1,NBUF
              CALL EXWORD ( BUF(J25), MIND, LIND, IND, REG, -2 )
!
! ----------- Parse the date of meteorological information
!
              IF ( BUF(J25)(IND(1,1):IND(2,1)) == 'METEO:' ) THEN
                   K_MET = K_MET + 1
                   CALL ERR_PASS ( IUER, IER )
                   CALL DATE_TO_TIME ( BUF(J25)(IND(1,2):IND(2,2)), MJD_TAG, &
     &                                 UTC_TAG, IER  )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J25, STR )
                        CALL ERR_LOG ( 8271, IUER, 'PIMA_GET_ANTAB', &
     &                      'Error in reading line '//STR(1:I_LEN(STR))// &
     &                      ' of the antab file '// &
     &                      ANTAB_FILE(1:I_LEN(ANTAB_FILE))// &
     &                      ' -- cannot parse the date '// &
     &                      BUF(J25)(IND(1,2):IND(2,2)) )
                        RETURN
                   END IF
!
                   PIM%STA(IND_STA)%WEATHER%TIME_BEG(K_MET) = &
     &                     ( MJD_TAG - PIM%MJD_0 )*86400.0D0 + &
     &                     ( UTC_TAG - PIM%UTC_MTAI - PIM%TAI_0 ) &
     &                     - PIM%OBS(1)%AP_LEN
                   PIM%STA(IND_STA)%WEATHER%TIME_END(K_MET) = &
     &                     ( MJD_TAG - PIM%MJD_0 )*86400.0D0 + &
     &                     ( UTC_TAG - PIM%UTC_MTAI - PIM%TAI_0 ) &
     &                     + PIM%OBS(1)%AP_LEN
!
                   READ ( UNIT=BUF(J25)(IND(1,3):IND(2,3)), FMT='(F5.1)', &
     &                    IOSTAT=IER ) PIM%STA(IND_STA)%WEATHER%TEMP(K_MET)
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J25, STR )
                        CALL ERR_LOG ( 8272, IUER, 'PIMA_GET_ANTAB', &
     &                      'Error in reading line '//STR(1:I_LEN(STR))// &
     &                      ' of the antab file '// &
     &                      ANTAB_FILE(1:I_LEN(ANTAB_FILE))// &
     &                      ' -- cannot parse air temperature '// &
     &                      BUF(J25)(IND(1,3):IND(2,3)) )
                        RETURN
                   END IF
!
                   READ ( UNIT=BUF(J25)(IND(1,4):IND(2,4)), FMT='(F7.0)', &
     &                    IOSTAT=IER ) PIM%STA(IND_STA)%WEATHER%PRES(K_MET)
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J25, STR )
                        CALL ERR_LOG ( 8273, IUER, 'PIMA_GET_ANTAB', &
     &                      'Error in reading line '//STR(1:I_LEN(STR))// &
     &                      ' of the antab file '// &
     &                      ANTAB_FILE(1:I_LEN(ANTAB_FILE))// &
     &                      ' -- cannot parse atmospheric pressure '// &
     &                      BUF(J25)(IND(1,4):IND(2,4)) )
                        RETURN
                   END IF
!
                   READ ( UNIT=BUF(J25)(IND(1,5):IND(2,5)), FMT='(F5.1)', &
     &                    IOSTAT=IER ) PIM%STA(IND_STA)%WEATHER%HUMID(K_MET)
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J25, STR )
                        CALL ERR_LOG ( 8274, IUER, 'PIMA_GET_ANTAB', &
     &                      'Error in reading line '//STR(1:I_LEN(STR))// &
     &                      ' of the antab file '// &
     &                      ANTAB_FILE(1:I_LEN(ANTAB_FILE))// &
     &                      ' -- cannot parse humidity '// &
     &                      BUF(J25)(IND(1,5):IND(2,5)) )
                        RETURN
                   END IF
              END IF
 4250      CONTINUE
      END IF
      IF ( N_CAB > 0 ) THEN
           IF ( PIM%STA(IND_STA)%CABLE%CAB_AVAIL ) THEN
                IF ( ASSOCIATED ( PIM%STA(IND_STA)%CABLE%TIM_CAB ) ) THEN
                     DEALLOCATE ( PIM%STA(IND_STA)%CABLE%TIM_CAB )
                END IF
                IF ( ASSOCIATED ( PIM%STA(IND_STA)%CABLE%CAB_DEL ) ) THEN
                     DEALLOCATE ( PIM%STA(IND_STA)%CABLE%CAB_DEL )
                END IF
           END IF
	   PIM%STA(IND_STA)%CABLE%CAB_AVAIL = .TRUE.
!
           PIM%STA(IND_STA)%CABLE%NPOI = N_CAB
           ALLOCATE ( PIM%STA(IND_STA)%CABLE%TIM_CAB(N_CAB), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*N_CAB, STR )
                CALL ERR_LOG ( 8275, IUER, 'PIMA_GET_ANTAB', 'Failure to '// &
     &               'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &               'dynamic memory for PIM%STA(IND_STA)%CABLE%TIM_CAB '// &
     &               'object' )
                RETURN
           END IF
!
           ALLOCATE ( PIM%STA(IND_STA)%CABLE%CAB_DEL(N_CAB), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*N_CAB, STR )
                CALL ERR_LOG ( 8276, IUER, 'PIMA_GET_ANTAB', 'Failure to '// &
     &               'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &               'dynamic memory for PIM%STA(IND_STA)%CABLE%CAB_DEL '// &
     &               'object' )
                RETURN
           END IF
!
           K_CAB = 0
           DO 4260 J26=1,NBUF
              CALL EXWORD ( BUF(J26), MIND, LIND, IND, REG, -2 )
!
! ----------- Parse the date of the cable length measurement
!
              IF ( BUF(J26)(IND(1,1):IND(2,1)) == 'CABLE:' ) THEN
                   K_CAB = K_CAB + 1
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL DATE_TO_TIME ( BUF(J26)(IND(1,2):IND(2,2)), MJD_TAG, &
     &                                 UTC_TAG, IER  )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( J26, STR )
                        CALL ERR_LOG ( 8278, IUER, 'PIMA_GET_ANTAB', &
     &                      'Error in reading line '//STR(1:I_LEN(STR))// &
     &                      ' of the antab file '// &
     &                      ANTAB_FILE(1:I_LEN(ANTAB_FILE))// &
     &                      ' -- cannot parse the date '// &
     &                      BUF(J26)(IND(1,2):IND(2,2)) )
                        RETURN
                   END IF
!
                   PIM%STA(IND_STA)%CABLE%TIM_CAB(K_CAB) = &
     &                     ( MJD_TAG - PIM%MJD_0 )*86400.0D0 + &
     &                     ( UTC_TAG - PIM%UTC_MTAI - PIM%TAI_0 )
!
                   READ ( UNIT=BUF(J26)(IND(1,3):IND(2,3)), FMT='(D12.5)', &
     &                    IOSTAT=IER ) PIM%STA(IND_STA)%CABLE%CAB_DEL(K_CAB)
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( J26, STR )
                        CALL ERR_LOG ( 8279, IUER, 'PIMA_GET_ANTAB', &
     &                      'Error in reading line '//STR(1:I_LEN(STR))// &
     &                      ' of the antab file '// &
     &                      ANTAB_FILE(1:I_LEN(ANTAB_FILE))// &
     &                      ' -- cannot parse cable delay '// &
     &                      BUF(J26)(IND(1,4):IND(2,4)) )
                        RETURN
                   END IF
!
! ---------------- Apply cable sign
!
                   PIM%STA(IND_STA)%CABLE%CAB_DEL(K_CAB) = SIGN_CAB* &
     &                              PIM%STA(IND_STA)%CABLE%CAB_DEL(K_CAB)
              END IF
!
              IV(J26) = 1
 4260      CONTINUE
!
! -------- Now we need to compute robust mean cable calibration and
! -------- subtract it from cable length values.
! -------- Keep in mind, cable cal may have jmps due to the procesdure
! -------- of determiing its sign. We need to compute the mean value
! -------- that is immune to these outlisers.
! -------- We iteratively remove M__CAB_SHARE share of points from
! -------- mean calculation
!
           N_ITER = K_CAB*M__CAB_SHARE
           DO 4270 J27=1,N_ITER
              AVR_CAB = 0.0D0
              MAX_DEV = 0.0D0
              IND_MAX_DEV = 1
!
! ----------- Compute the mean value for those elements which IV(kk) = 1
!
              NP = 0
              DO 4280 J28=1,K_CAB
                 IF ( IV(J28) == 1 ) THEN
                      AVR_CAB = AVR_CAB + PIM%STA(IND_STA)%CABLE%CAB_DEL(J28)
                      NP = NP + 1
                 END IF
 4280         CONTINUE
              AVR_CAB = AVR_CAB/NP
!
! ----------- Find the point with the maximum by module deviation from the
! ----------- mean among remaining points
!
              DO 4290 J29=1,K_CAB
                 IF ( DABS ( PIM%STA(IND_STA)%CABLE%CAB_DEL(J29) - AVR_CAB ) > &
     &                MAX_DEV  .AND.  IV(J29) == 1 ) THEN
!
                      MAX_DEV = DABS( PIM%STA(IND_STA)%CABLE%CAB_DEL(J29) - AVR_CAB )
                      IND_MAX_DEV = J29
                 END IF
 4290         CONTINUE
!
! ----------- And mark it as an outlier
!
              IV(IND_MAX_DEV) = 0
 4270      CONTINUE
!
! -------- Save the mean value
!
           PIM%STA(IND_STA)%CABLE%MEAN_CABLE = AVR_CAB
           PIM%STA(IND_STA)%CABLE%CABLE_SIGN = SIGN_CAB
!
! -------- Remove the mean cable calibration from cable lenghts
!
           DO 4300 J30=1,K_CAB
              PIM%STA(IND_STA)%CABLE%CAB_DEL(J30) = PIM%STA(IND_STA)%CABLE%CAB_DEL(J30) &
     &                                              - AVR_CAB
 4300      CONTINUE
        ELSE 
           PIM%STA(IND_STA)%CABLE%CAB_AVAIL = .FALSE.
           PIM%STA(IND_STA)%CABLE%NPOI = 0
      END IF
!
      IF ( FL_GAIN ) THEN
!
! -------- Now insert gain
!
           IF ( ASSOCIATED ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%TYP   ) ) THEN
!
! ------------- Deallocate memory, if it has already been allocated
!
                DEALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%TYP   )
                DEALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NTERM )
                DEALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%X_TYP )
                DEALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%Y_TYP )
                DEALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%X_VAL )
                DEALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%Y_VAL )
                DEALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%GAIN  )
                DEALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%SENS  )
           END IF
!
           PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%AVAIL = .TRUE.
           PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NFRQ  = PIM%NFRQ
           PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NPOL  = PIM%NPOL
           PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NTAB  = L_POLY+1
           ALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%TYP  (PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NFRQ,PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NPOL) )
           ALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NTERM(PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NFRQ,PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NPOL) )
           ALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%X_TYP(PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NFRQ,PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NPOL) )
           ALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%Y_TYP(PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NFRQ,PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NPOL) )
           ALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%X_VAL(PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NFRQ,PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NPOL) )
           ALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%Y_VAL(0:PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NTAB, &
     &                    PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NFRQ,PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NPOL) )
           ALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%GAIN(0:PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NTAB, &
     &                    PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NFRQ,PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NPOL) )
           ALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%SENS(PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NFRQ,PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NPOL) )
!
! -------- Set DPFU and GAIN
!
           DO 4310 J31=1,PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NPOL
              DO 4320 J32=1,PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NFRQ
                 IF ( (FRQ_DPFU(2) - FRQ_DPFU(1)) > FRQ_DIF__MIN ) THEN
!
! ------------------- Take into account frequency dependence of DPFU
!
                      PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%SENS(J32,J31) = DPFU(1) + &
     &                   (PIM%FRQ(J32,PIM%CONF%FRQ_GRP)%FREQ - FRQ_DPFU(1))*(DPFU(2) - DPFU(1))/ &
                         (FRQ_DPFU(2) - FRQ_DPFU(1))
                    ELSE
                      PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%SENS(J32,J31) = DPFU(1)
                 END IF
                 PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%TYP(J32,J31) = 2
                 PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%X_TYP(J32,J31) = 1
                 PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%Y_TYP(J32,J31) = 1
                 PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%X_VAL(J32,J31) = 0.0D0
!
! -------------- The caveat: indexing of arrays GAIN%Y_VAL and GAIN%GAIN
! -------------- starts from 1
!
                 DO 4330 J33=0,L_POLY
                    PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%GAIN(J33,J32,J31)  = GAIN_POLY(J33)
                    PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%Y_VAL(J33,J32,J31) = GAIN_POLY(J33)
 4330            CONTINUE
                 PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NTERM(J32,J31) = L_POLY + 1
 4320         CONTINUE
 4310      CONTINUE
      END IF
!
      N_MIS = 0
      IF ( FL_TSYS ) THEN
!
! -------- Allocate dynamic memory for TSYS
!
           IF ( ASSOCIATED ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%TSYS ) ) THEN
!
! ------------- Deallocated memory for Tsys if it has been previoysly allocated
!
                DEALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%TSYS         )
                DEALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%TIME_MID_R8  )
                DEALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%TIME_SPAN_R4 )
                DEALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%SOU_IND )
                DEALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%AZ_R4   )
                DEALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%ELEV_R4 )
           END IF
!
           PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%AVAIL = .TRUE.
           PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOL  = PIM%NPOL
!
           ALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%TSYS(PIM%NFRQ,PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOI,PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOL) )
           ALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%TIME_MID_R8(PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOI) )
           ALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%TIME_SPAN_R4(PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOI) )
           ALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%SOU_IND(PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOI) )
           ALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%AZ_R4(PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOI) )
           ALLOCATE ( PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%ELEV_R4(PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOI) )
!
           PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%TSYS         = 0.0D0
           PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%TIME_MID_R8  = 0.0D0
           PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%TIME_SPAN_R4 = 0.0
           PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%SOU_IND      = 0
           PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%AZ_R4        = 0.0
           PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%ELEV_R4      = 0.0
!
           IND_FRG = PIM%CONF%FRQ_GRP
           DO 4340 J34=1,PIM%NOBS
              IND_OBS = J34
!
! ----------- Check whether IND_STA station observed this observation and
! ----------- if yes, what was its station index in the baseline
!
              IND_STA_BAS = 0
              IF ( PIM%OBS(J34)%STA_IND(1) == IND_STA ) IND_STA_BAS = 1
              IF ( PIM%OBS(J34)%STA_IND(2) == IND_STA ) IND_STA_BAS = 2
              IF ( IND_STA_BAS == 0 ) GOTO 4340
!
              IND_SCA = PIM%OBS(J34)%SCA_IND
              PIM%OBS(J34)%TSYS_IND(IND_STA_BAS,:) = 0
              FL_FOUND = .FALSE.
              IF ( IND_SOU_ARR(IND_SCA) > 0 ) THEN
!!                   DO 4350 J35=1,PIM%NFRG
                   DO 4350 J35=PIM_MIN_FRG,PIM_MAX_FRG
                      IND_TSYS = CROSS_SCA_TSYS(IND_SCA,J35)
                      IF ( IND_TSYS == 0 ) GOTO 4350
                      IF ( IND_TSYS > PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOI ) GOTO 4350
                      IF ( J35 > PIM%CONF%FRQ_GRP ) GOTO 4350 ! To avoid working with uninitialized PIM%STA(IND_STA)%TSYS(J35)%
!
! ------------------- There is Tsys for this scan. Very good!
!
                      PIM%STA(IND_STA)%TSYS(J35)%TIME_SPAN_R4(IND_TSYS) = PIM__SPAN_DEF
                      PIM%STA(IND_STA)%TSYS(J35)%SOU_IND(IND_TSYS) = IND_SOU_ARR(IND_SCA)
                      PIM%OBS(IND_OBS)%TSYS_IND(IND_STA_BAS,J35)   = IND_TSYS
                      PIM%STA(IND_STA)%TSYS(J35)%AZ_R4(IND_TSYS)   = PIM%OBS(J34)%AZ(IND_STA_BAS)
                      PIM%STA(IND_STA)%TSYS(J35)%ELEV_R4(IND_TSYS) = PIM%OBS(J34)%ELEV(IND_STA_BAS)
                      PIM%STA(IND_STA)%TSYS(J35)%TIME_MID_R8(IND_TSYS) = 0.0
!
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                           STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_TSYS_ARR(1,IND_SCA,J35), IER )
                           WRITE ( 6, 2102 ) J34, IND_SCA, PIM%C_SOU(PIM%SCA(IND_SCA)%SOU_IND), &
     &                                       TIM_TSYS_ARR(1,IND_SCA,J35), STR(1:19)
 2102                      FORMAT ( 'PIMA_GET_ANTAB: Ind_obs: ', I5, ' Ind_sca: ', I5, &
     &                              ' Sou: ', A, ' Tim= ', F8.2, ' Date: ', A )
                      END IF
                      DO 4360 J36=1,PIM%NFRQ
                         IF ( TIM_TSYS_ARR(J36,IND_SCA,J35) > 0.0D0 ) THEN
                              PIM%STA(IND_STA)%TSYS(J35)%TIME_MID_R8(IND_TSYS)  = TIM_TSYS_ARR(J36,IND_SCA,J35)
                         END IF
                         PIM%STA(IND_STA)%TSYS(J35)%TSYS(J36,IND_TSYS,1) = TSYS_ARR(J36,IND_SCA,J35,1)
                         IF ( PIM%NPOL > 1 ) THEN
                              PIM%STA(IND_STA)%TSYS(J35)%TSYS(J36,IND_TSYS,2) = TSYS_ARR(J36,IND_SCA,J35,2)
                         END IF
 4360                 CONTINUE
 4350              CONTINUE
                   FL_FOUND = .TRUE.
                ELSE
                   DO 4370 J37=1,PIM%NSCA
!!                      DO 4380 J38=1,PIM%NFRG
                      DO 4380 J38=PIM_MIN_FRG,PIM_MAX_FRG
                         IF ( PIM%SCA(J37)%SOU_IND == PIM%SCA(IND_SCA)%SOU_IND .AND. &
     &                        CROSS_SCA_TSYS(J37,J38) > 0 ) THEN
                              IF ( DABS( PIM%TIM_R8(PIM%SCA(J37)%TIM_IND) -   &
     &                                   PIM%TIM_R8(PIM%SCA(IND_SCA)%TIM_IND) ) < &
     &                             TSYS__TIM_TOL ) THEN
!
                                   IND_SOU_ARR(IND_SCA) = PIM%SCA(IND_SCA)%SOU_IND
                                   CROSS_SCA_TSYS(IND_SCA,J38) = CROSS_SCA_TSYS(J37,J38)
                                   PIM%OBS(IND_OBS)%TSYS_IND(IND_STA_BAS,J38) = IND_TSYS
                                   FL_FOUND = .TRUE.
                              END IF
                         END IF
 4380                 CONTINUE
 4370              CONTINUE
              END IF
!
              IF ( .NOT. FL_FOUND ) THEN
                   N_MIS = N_MIS + 1
                   IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                        STR1 = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%OBS(IND_OBS)%TIM_BEG, -2 )
                        STR2 = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%OBS(IND_OBS)%TIM_END, -2 )
                        WRITE ( 6, 2100 ) PIM%C_STA(IND_STA), IND_OBS, &
     &                                    PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND), &
     &                                    STR1(1:24), STR2(1:24)
 2100                   FORMAT ( 'PIMA_GET_ANTAB: Station ',A, ' Cannot find Tsys for Obs ', I6, &
     &                           ' of source ', A, ' on [ ',A , ' , ', A , ' ]' )
                   END IF
                   GOTO 4340
              END IF
 4340      CONTINUE
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                IF ( N_MIS == 0 ) THEN
                     WRITE ( 6, 2110 ) PIM%C_STA(IND_STA)
 2110                FORMAT ( 'Station ', A, ' Tsys was found for all observations' )
                   ELSE 
                     WRITE ( 6, 2120 ) PIM%C_STA(IND_STA), N_MIS
 2120                FORMAT ( 'Station ', A, ' Tsys is missing for ', &
     &                         I5, ' observations' )
                END IF
           END IF
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                WRITE ( 6, 2130 ) PIM%C_STA(IND_STA), &
     &                            PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%AVAIL, &
     &                            PIM%STA(IND_STA)%TSYS(PIM%CONF%FRQ_GRP)%NPOI
 2130           FORMAT ( 'PIMA_GET_ANTAB: Sta: ', A, ' Tsys_avail: ', L1, &
     &                   ' Num_tsys: ', I4  )
           END IF
      END IF
!
! --- Clean-up
!
      CALL ERR_LOG ( 0, IUER )
 810  CONTINUE
      DEALLOCATE ( IND_SOU_ARR  )
      DEALLOCATE ( NUM_TSYS     )
      DEALLOCATE ( TSYS_ARR     )
      DEALLOCATE ( TIM_TSYS_ARR )
      DEALLOCATE ( BUF          )
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           IF ( IND_STA < 0 ) THEN
                WRITE ( 6, 2140 ) 
 2140           FORMAT ( 'Calibration has failed. No station name has been found'/ )
              ELSE
                WRITE ( 6, 2150 ) STA_NAM,  GET_CDATE()
 2150           FORMAT ( 'Station ',A, ' Calibration is finished on ', A/ )
           END IF
      END IF
!
      RETURN
      END  SUBROUTINE PIMA_GET_ANTAB  !#!#
