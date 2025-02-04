      SUBROUTINE STP_SNR ( VEX, STP, ISTA_STP, IND_VEX_SCA, SOU_NAM, MJD, UTC, &
     &                     SCAN_DUR, N_IFS, IND_IFS, BAND_MODE, ELEV, SCORR, &
     &                     COR_AMP, NOISE, TSYS, GAIN, SEFD, SNR, IUER )
! ********************************************************************************
! *                                                                              *
! *   Routine STP_SNR computes the station noise given the loss factor           *
! *   due to a non-ractangular bandpass shape, and a Vex file to be used         *
! *   to compute the number of recorded samples over the band used.              *
! *                                                                              *
! *   N.B: - It is assumed that all sampling is at 2 bit quantization,           *
! *          therefore the quantization loss factor is kept constant.            *
! *                                                                              *
! *   INPUT:                                                                     *
! *    VEX       =  VEX Object                       { Derived Type }            *
! *                                                                              *
! *    STP       =  STP Object                       { Derived Type }            *
! *                                                                              *
! *    ISTA_STP  =  Station Indices in STP           { INT } (2x1)               *
! *                                                                              *
! *    IND_VEX_SCA -- scan of the index of this observation in the vex data      *
! *                   structure.                                                 *
! *                                                                              *
! *    SOU_NAM   =  Source Name corresponding to the given SCORR                 *
! *                                                                              *
! *    MJD       =  Observation MJD                  { INT }                     *
! *                                                                              *
! *    UTC       =  Observation UTC                  { REAL }                    *
! *                                                                              *
! *    SCAN_DIR  -- scan duration in sec ( REAL*8 )                              *
! *                                                                              *
! *    ELEV      =  Elevation                        { REAL } [rad]              *
! *                                                                              *
! *    SCORR     =  Correlated Flux Density          { REAL } [Jy]               *
! *                                                                              *
! *    IUER      =  Error Handler                    { INT, OPT }                *
! *                 If IUER=0 no error message will be printed,                  *
! *                 even in the event of an error. However, for                  *
! *                 other possible values, i.e. IUER=-1,-2, & -3,                *
! *                 the error message will print to screen. For                  *
! *                 the latter case, i.e. IUER=-3, after printing                *
! *                 the program will terminate.                                  *
! *                 Default, IUER = -1                                           *
! *                                                                              *
! *   OUTPUT:                                                                    *
! *                                                                              *
! *            COR_AMP   =  Computed correlated anplitude    { REAL }            *
! *                                                                              *
! *            NOISE     =  Noise                            { REAL }            *
! *                                                                              *
! *            SEFD      =  System Equivalent Flux Density   { REAL } [Jy] (2x1) *
! *                                                                              *
! *            SNR       =  Computed SNR                     { REAL }            *
! *                                                                              *
! *   ###  31-OCT-2020  STP_NOISE   v1.0 (c)  N. Habana  31-OCT-2020  ###        *
! *   ###  10-NOV-2020  STP_SNR     v2.0 (c)  N. Habana  10-NOV-2020  ###        *
! *     -  Inputs the parsed VEX and STP objects, instead of their               *
! *        files.                                                                *
! *     -  Name changed from STP_NOISE to STP_SNR, and now computing             *
! *        Signal, Noise, and SNR.                                               *
! *   ###  23-NOV-2020  STP_SNR     v3.0 (c)  N. Habana  23-NOV-2020  ###        *
! *     -  Added second station STP.                                             *
! *     -  SEFDs now computed in file.                                           *
! *     -  Noise includes scan duration as a respresentation of                  *
! *        integration time.                                                     *
! *                                                                              *
! *   ###  23-NOV-2020       STP_SNR     v3.2 (c)  L. Petrov  19-SEP-2021  ###   *
! *                                                                              *
! ********************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vex.i'
      INCLUDE   'stp.i'
      TYPE ( STP__TYPE ) :: STP
      TYPE ( VEX_TYPE  ) :: VEX
      INTEGER*4  ISTA_STP(2), IND_VEX_SCA, N_IFS(4), IND_IFS(VEX__MCHA,4), MJD, IUER
      REAL*8     UTC, SCAN_DUR, ELEV(2), SCORR(4), TSYS(2,4), GAIN(2,4), &
     &           SEFD(2,4), COR_AMP(4), NOISE(4), SNR(4)
      CHARACTER  SOU_NAM*(*), BAND_MODE*(*)
      REAL*8     ETA, BT, KAPPA(2), FRQ
      PARAMETER  ( ETA = 0.8825D0 )
      INTEGER*4  J0, J1, J2, J3, J4, J5, IER
      INTEGER*4  FL_STA1, FL_STA2, FRQ_IDX(2)
      INTEGER*4  IDX_STA1, IDX_STA2, IND_VEX_STA(2), IND_SCA_STA(2)
      CHARACTER  BAND_ID*1, STR*32, STR1*32
      REAL*8     EPS_TIM
      PARAMETER  ( EPS_TIM = 1.D-4 )
      LOGICAL*1  FL_SOU
      REAL*8, EXTERNAL :: MJDSEC_TO_TIM
      INTEGER*4, EXTERNAL :: LINDEX, IFIND_PL
      INTEGER*4, EXTERNAL :: VEX_STA_ID_IDX
!
! --- Find the Frequency each station is on.
!
      IF ( ISTA_STP(1) < 1 .OR. ISTA_STP(1) > STP%NSTA ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( ISTA_STP(1), STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( STP%NSTA, STR1 )
           CALL ERR_LOG ( 2411, IUER, 'STP_SNR', 'Argument ISTA_STP(1) '// &
     &          TRIM(STR)//' is out of ragne [1, '//TRIM(STR1) )
           RETURN
      END IF
      IF ( ISTA_STP(2) < 1 .OR. ISTA_STP(2) > STP%NSTA ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( ISTA_STP(1), STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( STP%NSTA, STR1 )
           CALL ERR_LOG ( 2412, IUER, 'STP_SNR', 'Argument '// &
     &         'ISTA_STP(2) '//TRIM(STR)//' is out of range '// &
     &         '[1, '//TRIM(STR1) )
           RETURN
      END IF
!
      FRQ_IDX = 0
      DO 120 J0 =1,VEX%N_FRQ
         DO 130 J1=1,VEX%FRQ(J0)%N_STA

            IF ( VEX%FRQ(J0)%SITE_ID(J1) == STP%STA(ISTA_STP(1))%SHORT_NAME ) THEN
                 FRQ_IDX(1) = J0
              ELSE IF ( VEX%FRQ(J0)%SITE_ID(J1) == STP%STA(ISTA_STP(2))%SHORT_NAME ) THEN
                 FRQ_IDX(2) = J0
            END IF
 130     CONTINUE
 120  CONTINUE
      IF ( FRQ_IDX(1) == 0 ) THEN
           CALL CLRCH ( STR) 
           CALL INCH  ( VEX%N_FRQ, STR )
           CALL ERR_LOG ( 2413, IUER, 'STP_SNR', TRIM(STR)//' IFs have been defined, '// &
     &         'but no $IF was not defined for station '//STP%C_STA(ISTA_STP(1)) )
           RETURN 
      END IF 
      IF ( FRQ_IDX(2) == 0 ) THEN
           CALL CLRCH ( STR) 
           CALL INCH  ( VEX%N_FRQ, STR )
           CALL ERR_LOG ( 2414, IUER, 'STP_SNR', TRIM(STR)//' IFs have been defined, '// &
     &         'but no $IF was not defined for station '//STP%C_STA(ISTA_STP(2)) )
           RETURN 
      END IF 
!
! --- Compute the integration time.
! --- Find a schedule where the source was observed by the given stations
!
      IND_VEX_STA = 0
      IND_SCA_STA = 0
!
      IF ( VEX%SCA(IND_VEX_SCA)%SOU_NAME .NE. SOU_NAM ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND_VEX_SCA, STR )
           CALL ERR_LOG ( 2415, IUER, 'STP_SNR', 'Trap of internal '// &
     &         'control: source name in for scan '//TRIM(STR)//            &
     &         ' with name '//TRIM(VEX%SCA(IND_VEX_SCA)%SCAN_NAME)//       &
     &         ' but the obsevation has source name '//SOU_NAM )
           RETURN 
      END IF
!
      IND_VEX_STA(1) = VEX_STA_ID_IDX ( VEX, STP%STA(ISTA_STP(1))%SHORT_NAME )
      IF ( IND_VEX_STA(1) < 1 ) THEN
           CALL ERR_LOG ( 2416, IUER, 'STP_SNR',                    &
     &         'Station '//STP%STA(ISTA_STP(1))%NAME//' did not '// &
     &         'observe in experiment '//TRIM(VEX%EXPER_NAME) )
           RETURN
      END IF
!
      IND_SCA_STA(1) = IFIND_PL ( VEX%SCA(IND_VEX_SCA)%N_STA,              &
     &                            VEX%SCA(IND_VEX_SCA)%IND_STA, IND_VEX_STA(1) )
      IF ( IND_SCA_STA(1) < 1 ) THEN
         CALL CLRCH ( STR )
         CALL INCH  ( IND_VEX_SCA, STR )
         CALL ERR_LOG ( 2417, IUER, 'STP_SNR',                    &
     &       'Station '//STP%STA(ISTA_STP(1))%NAME//' did not '//  &
     &       'observe in scan '//TRIM(STR)//' in experiment '//  &
     &       TRIM(VEX%EXPER_NAME) )
         RETURN
      END IF
!
      IND_VEX_STA(2) = VEX_STA_ID_IDX ( VEX, STP%STA(ISTA_STP(2))%SHORT_NAME )
      IF ( IND_VEX_STA(2) < 1 ) THEN
         CALL ERR_LOG ( 2418, IUER, 'STP_SNR',                    &
     &        'Station '//STP%STA(ISTA_STP(2))%NAME//' did not '// &
     &        'observe in experiment '// TRIM(VEX%EXPER_NAME) )
         RETURN
      END IF
!
      IND_SCA_STA(2) = IFIND_PL ( VEX%SCA(IND_VEX_SCA)%N_STA,              &
     &                            VEX%SCA(IND_VEX_SCA)%IND_STA, IND_VEX_STA(2) )
      IF ( IND_SCA_STA(2) < 1 ) THEN
         CALL CLRCH ( STR )
         CALL INCH  ( IND_VEX_SCA, STR )
         CALL ERR_LOG ( 2419, IUER, 'STP_SNR',                    &
     &        'Station '//STP%STA(ISTA_STP(2))%NAME//' is not '// &
     &        'observed in scan '//TRIM(STR)//' in experiment '// &
     &        TRIM(VEX%EXPER_NAME) )
         RETURN
      END IF
!
      IF ( IND_SCA_STA(1) == 0 .AND. IND_SCA_STA(2) == 0 ) THEN
           WRITE ( 6, * ) ' SCAN_DUR= ', SCAN_DUR
           CALL CLRCH ( STR )
           CALL INCH  ( MJD, STR )
           CALL CLRCH ( STR1 )
           WRITE ( UNIT=STR1(1:8), FMT='(F8.1)' ) UTC
           CALL ERR_LOG ( 2420, IUER, 'STP_SNR',                             &
     &         'Did not find a scan for stations '//                         &
     &          STP%STA(ISTA_STP(1))%SHORT_NAME//' '//                       &
     &          STP%STA(ISTA_STP(2))%SHORT_NAME//' source '//TRIM(SOU_NAM)// &
     &          ' at MJD= '//TRIM(STR)//' and UTC time tag '//TRIM(STR1)//    &
     &          ' of the schedule of experiment '//TRIM(VEX%EXPER_NAME) )
           RETURN
      END IF
!
      IF ( .NOT. ( UTC .GE. VEX%SCA(IND_VEX_SCA)%UTC + &
     &                      VEX%SCA(IND_VEX_SCA)%START_OFFSET(IND_VEX_STA(1)) - EPS_TIM ) .AND. &
     &           ( UTC .LE. VEX%SCA(IND_VEX_SCA)%UTC + &
     &                      VEX%SCA(IND_VEX_SCA)%START_OFFSET(IND_VEX_STA(1)) + &
     &                      VEX%SCA(IND_VEX_SCA)%SCAN_DUR(IND_VEX_STA(1))     + EPS_TIM     ) ) THEN
           WRITE ( 6, * ) ' SCAN_DUR= ', SCAN_DUR
           CALL CLRCH ( STR )
           CALL INCH  ( MJD, STR )
           CALL CLRCH ( STR1 )
           WRITE ( UNIT=STR1(1:8), FMT='(F8.1)' ) UTC
           CALL ERR_LOG ( 2421, IUER, 'STP_SNR', 'Trap of internal control: '// &
     &         'UTC is out of range for the 1st station '// &
     &          STP%STA(ISTA_STP(1))%SHORT_NAME//' of the scan '// &
     &          TRIM(VEX%SCA(IND_VEX_SCA)%SCAN_NAME)// &
     &          ' at MJD= '//TRIM(STR)//' and UTC time tag '//TRIM(STR1)//    &
     &          ' of the schedule of experiment '//TRIM(VEX%EXPER_NAME) )
           RETURN
      END IF
!
      IF ( .NOT. ( UTC .GE. VEX%SCA(IND_VEX_SCA)%UTC + &
     &                      VEX%SCA(IND_VEX_SCA)%START_OFFSET(IND_VEX_STA(2)) - EPS_TIM ) .AND. &
     &           ( UTC .LE. VEX%SCA(IND_VEX_SCA)%UTC + &
     &                      VEX%SCA(IND_VEX_SCA)%START_OFFSET(IND_VEX_STA(2)) + &
     &                      VEX%SCA(IND_VEX_SCA)%SCAN_DUR(IND_VEX_STA(2))     + EPS_TIM     ) ) THEN
           WRITE ( 6, * ) ' SCAN_DUR= ', SCAN_DUR
           CALL CLRCH ( STR )
           CALL INCH  ( MJD, STR )
           CALL CLRCH ( STR1 )
           WRITE ( UNIT=STR1(1:8), FMT='(F8.1)' ) UTC
           CALL ERR_LOG ( 2422, IUER, 'STP_SNR', 'Trap of internal control: '// &
     &         'UTC is out of range for the 1st station '// &
     &          STP%STA(ISTA_STP(1))%SHORT_NAME//' of the scan '// &
     &          TRIM(VEX%SCA(IND_VEX_SCA)%SCAN_NAME)// &
     &          ' at MJD= '//TRIM(STR)//' and UTC time tag '//TRIM(STR1)//    &
     &          ' of the schedule of experiment '//TRIM(VEX%EXPER_NAME) )
           RETURN
      END IF
!
! --- Compute the number of recorded samples over the band used, at each
!     station, over a recording period.
!     BT = (Sampling rate at one IF) x (no. of IFs in that Band) x ( integration time )
!
      IF ( VEX%FRQ(FRQ_IDX(1))%SAMPLE_RATE < VEX__MIN_SAMPLE_RATE ) THEN
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR(1:12), FMT='(1PD15.7)' ) VEX%FRQ(FRQ_IDX(1))%SAMPLE_RATE
           CALL ERR_LOG ( 2423, IUER, 'STP_SNR', 'Too small sampling '// &
     &         'rate: '//TRIM(STR)//' Hz' )
           RETURN 
      END IF
      IF ( N_IFS(1) < 1 ) THEN
           CALL ERR_LOG ( 2424, IUER, 'STP_SNR', 'No IFs is specified in the '// &
     &         'input vex file' )
           RETURN 
      END IF
!
      DO 430 J3=1,4
         IF ( N_IFS(J3) > 0 ) THEN
              BT = VEX%FRQ(FRQ_IDX(1))%SAMPLE_RATE*N_IFS(J3)*SCAN_DUR
!
! ----------- Compute the mean noise amplitude
! ----------- N.B: We use the first bandpass loss since we are currently using 
!             the same value of BPSL at all bandwidths. When this is edited
!             in the STP file.
!
              NOISE(J3) = DSQRT(P2I)/( ETA * DSQRT( BT* DSQRT( STP%STA(ISTA_STP(1))%BPSL(1)%BETA* &
     &                                                         STP%STA(ISTA_STP(2))%BPSL(1)%BETA  ) ) )
              FRQ = (VEX%FRQ(FRQ_IDX(1))%SKY_FRQ(IND_IFS(1,J3)) + &
     &               VEX%FRQ(FRQ_IDX(1))%SKY_FRQ(IND_IFS(N_IFS(J3),J3)))/2.0D0
!
! ----------- Compute the SEFD's of each station
!     
              CALL ERR_PASS ( IUER, IER ) 
              CALL STP_SEFD ( STP, ISTA_STP(1), MJD, UTC, FRQ, ELEV(1), TSYS(1,J3), &
     &                        GAIN(1,J3), SEFD(1,J3), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2425, IUER, 'STP_SNR', 'Error in STP_SEFD for '// &
     &                 'the first station of the baseline '// &
     &                  STP%STA(ISTA_STP(1))%SHORT_NAME )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER ) 
              CALL STP_SEFD ( STP, ISTA_STP(2), MJD, UTC, FRQ, ELEV(2), TSYS(2,J3), &
     &                        GAIN(2,J3), SEFD(2,J3), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2426, IUER, 'STP_SNR', 'Error in STP_SEFD '// &
     &                 'for the second station of the baseline '// &
     &                  STP%STA(ISTA_STP(2))%SHORT_NAME )
                   RETURN
              END IF
!
! ----------- Compute the correlated amplitude
!
              COR_AMP(J3) = SCORR(J3)/DSQRT(SEFD(1,J3)*SEFD(2,J3))
!
! ----------- Compute the SNR
!
              SNR(J3) = COR_AMP(J3)/NOISE(J3)
          END IF
 430  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE !#! STP_SNR
