      PROGRAM BNC_SCAV
!
! **************************************************************************************
! *                                                                                    *
! *   Program BNC_SCAV                                                                 *
! *                                                                                    *
! *   Usage: input_bnc_file output_ave_anc_file sde|vlbi del verbosity                 *
! *                                                                                    *
! *   1st Argument: path to binary file                                { char }        *
! *   2nd Argument: path to prefilled scan average file                { char }        *
! *   3rd Argument: Experiment type                                                    *
! *                 Accepted labels: sde  -- single dish experiment    { char }        *
! *                                  vlbi -- VLBI experiment           { char }        *
! *   4th Argument: Maximum difference between scans                   { real*8 } [s]  *
! *   5th Argument: Verbosity level                                    { int*4 }       *
! *                                                                                    *
! *  ### 11-APR-2021     BNC_SCAV            v1.0 (c)    N. Habana   11-APR-2023 ###   *
! *                                                                                    *
! **************************************************************************************
!
      IMPLICIT   NONE
      INCLUDE    'atp.i'
      TYPE ( ANC__TYP   ) :: ANC
      CHARACTER, ALLOCATABLE :: BUF_SCAV(:)*(ANC__MSTR)
      CHARACTER, ALLOCATABLE :: BUF_OUT(:)*(ANC__MSTR)
      CHARACTER  FIL_BNC*128, FIL_SCAV*128, STR*128, STR1(2)*32, STR2*16
      CHARACTER  FIL_PATH*128, PLOTVAR*6, PLOTORD*4
      CHARACTER  STR_DO_DATE*64, TIT*64, CH_EXP*4, CH_VRB
      CHARACTER  CH_IND*12, CH_POL*2, CH_PLOT*4, CH_DATE(2)*22
      CHARACTER  DELIM*3, DELIM1
      INTEGER*4  LBNC, LANC, NUM_TPS, NUM_PCS, LSTR
      INTEGER*8  SIZE_I8, MLEN_STR
      INTEGER*4  IUER, IER, IVRB, ICNT, IVAR_FLG
      INTEGER*4  MIND, UNIX_DATE, IS
      LOGICAL*1  LEX
      CHARACTER  CH_TIM_DIF*6
      PARAMETER  ( MIND = 32 )
      PARAMETER  ( DELIM  =  CHAR(0)//CHAR(32)//CHAR(9) )                ! Null, Space, Tab
      PARAMETER  ( DELIM1 =  CHAR(47)  )                                 ! Forward slash "/"
      INTEGER*4  NBUF, LIND, IND(2,MIND)
      REAL*8,    DIMENSION(:,:), ALLOCATABLE :: TIM_TSYS_AVR
      REAL*8,    DIMENSION(:,:), ALLOCATABLE :: TSYS_RMS, TSYS_AVR
      REAL*8,    DIMENSION(:,:), ALLOCATABLE :: AZ_SCA, EL_SCA
      REAL*8     TIM_DIF_MAX, TIM(ANC__MEPC)
      REAL*8     TSYS(ANC__MEPC), EL_ARR(ANC__MEPC)
      REAL*8     AMP_SCA(ANC__MEPC), AZ_ARR(ANC__MEPC)
      REAL*8     PHA_SCA(ANC__MEPC), TIM_AVR(ANC__MEPC)
      REAL*8,    DIMENSION(:,:), ALLOCATABLE :: TIM_PCAL_AVR
      COMPLEX*8, DIMENSION(:,:), ALLOCATABLE :: PCAL_AVR
      REAL*8,    DIMENSION(:,:), ALLOCATABLE :: PCAL_AMP_AVR
      REAL*8,    DIMENSION(:,:), ALLOCATABLE :: PCAL_PHA_AVR
      REAL*8,    DIMENSION(:,:), ALLOCATABLE :: PCAL_AMP_RMS
      REAL*8,    DIMENSION(:,:), ALLOCATABLE :: PCAL_PHA_RMS
      COMPLEX*8  PCAL(ANC__MEPC), Z_AVR(ANC__MEPC)
      REAL*8     T1_AVR(ANC__MEPC)
      REAL*8     Y1_AVR(ANC__MEPC), Y1_RMS(ANC__MEPC), Y2_RMS(ANC__MEPC)
      INTEGER*4  ISCA(ANC__MEPC), IARR(ANC__MEPC)
      INTEGER*4  JP, J0, J1, J2, J3, J4
      INTEGER*4  K0, K1, K2, K3, K4, K5, K2CNT
      INTEGER*4  IND_CNT, NP, NOUT, IDX1, IDX2
      INTEGER*4  ICNT_MAX, IPCAL, IPCAL_MAX
      INTEGER*4  NS_TSYS(ANC__MTPS), NS_PCAL(ANC__MPCS)
      REAL*8     DELTS, DEL_MIN
      PARAMETER  ( DEL_MIN = 1.D-6 )
      INTEGER*4  ITPS(ANC__MEPC)
      INTEGER*4, DIMENSION(:,:), ALLOCATABLE :: ITPS_SCA, IPCS_SCA
      REAL*8,    EXTERNAL :: MJDSEC_TO_TIM
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, TIM_TO_DATE*23
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
      INTEGER*8, EXTERNAL :: LSEEK, READ
      INTEGER*4, EXTERNAL :: IXMN8, IXMN4, LINDEX, ILEN, I_LEN
      INTEGER*4, EXTERNAL :: LTM_DIF, FILE_INFO, IFIND_PL, FINDEX
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4  NS_TOT, IND_ARR(ANC__MEPC), IND_ARR_PCAL(ANC__MEPC)
      INTEGER*4  IN_SCAN(ANC__MEPC), IFND, NTSYS_OBS, NPCAL_OBS
      INTEGER*4  MP, MAXL_STRING, IT_WR, IP_WR                ! Max no. lines,  Max String length
      PARAMETER  ( MAXL_STRING = 256 )
      CHARACTER  BUF_TSYS(ANC__MBUF)*(MAXL_STRING)            ! Read Tsys Block 
      CHARACTER  BUF_PCAL(ANC__MBUF)*(MAXL_STRING)            ! Read PCal Block
      CHARACTER  BUF_PROV(5)*(MAXL_STRING)
      CHARACTER  SCA_NAM*12, SOU_NAM*8, DATA_SCOPE*14, SPEC_DATE*23
      CHARACTER  CUR_DATE*20
      REAL*8     TSINCE
      CHARACTER  CBNC*8, CBTS*8, CANC*8, CSCAV*8
      LOGICAL*1  FL_GLO, FL_PRO, FL_TSYS, FL_PCAL ! Block Flags
      CHARACTER  SCAV_STA*8, SCAV_EXP*16
      LOGICAL*1  FL_CHK
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~ 'Usage: bnc_scav bnc_file scav_file sde|vlbi del verbosity'
!~~ examples
! bnc_scav gsst04_orig.bnc gsst04_scav.anc sde 8.0 1
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      CBNC  = ".bnc"
      CBTS  = ".bts"
      CANC  = ".anc"
      CSCAV = "_scav"
!     
! --- Read user input
!
      IF ( IARGC() < 1 .OR. IARGC() > 5 ) THEN
         WRITE ( 6, '(A)' ) 'USAGE: '//                                 &
     &         'bnc_scav bnc_file scav_file sde|vlbi del verbosity '
         CALL EXIT ( 0 )
      ELSE
! ------
         write (6,'(A)') 'running BNC_SCAV'
!
! ------ Arg. 1
! ------ Get the bnc file and check if it exists?
!
         CALL GETARG ( 1, FIL_BNC )
         INQUIRE     ( FILE=FIL_BNC, EXIST=LEX )
         IF ( .NOT. LEX ) THEN ! 
            IUER = -1
            CALL ERR_LOG ( 6001, IUER, 'BNC_SCAV',                      &
     &              'Cannot find bnc file '//TRIM(FIL_BNC) )
            CALL EXIT ( 1 )
!
! ------- Is this a bnc file though?
!
         ELSE
            IDX1 = FINDEX( FIL_BNC, TRIM(CBNC) )
            IDX2 = FINDEX( FIL_BNC, TRIM(CBTS) )
            IF ( IDX1 == 0 .AND. IDX2 == 0 ) THEN
               IUER = -1
               CALL ERR_LOG ( 6012, IUER, 'BNC_SCAV',                   &
     &              TRIM(FIL_BNC)//" is not accepted .bnc naming")
               CALL EXIT ( 1 )
            END IF
         END IF ! lex
!
! ------ What is the length of the binary file path
!
         LBNC = I_LEN(FIL_BNC)
!     
! ------ Arg. 2
! ------ Get the anc scav file and check if it exists?
!
         CALL GETARG ( 2, FIL_SCAV )
         INQUIRE     ( FILE=FIL_SCAV, EXIST=LEX )
         IF ( .NOT. LEX ) THEN ! 
            IUER = -1
            CALL ERR_LOG ( 6002, IUER, 'BNC_SCAV',                      &
     &              'Cannot find anc file '//TRIM(FIL_SCAV) )
            CALL EXIT ( 1 )
         ELSE
            IDX1 = FINDEX( FIL_SCAV, TRIM(CSCAV) )
            IDX2 = FINDEX( FIL_SCAV, TRIM(CANC)  )
            IF ( IDX1 == 0 .AND. IDX2 == 0 ) THEN
               IUER = -1
               CALL ERR_LOG ( 6013, IUER, 'BNC_SCAV',                   &
     &              TRIM(FIL_SCAV)//" is expected to have "//           &
     &              TRIM(CSCAV)//" and be type "//TRIM(CANC) )
               CALL EXIT ( 1 )
            END IF
         END IF ! lex
!
! ------ What is the length of the scav file path
!
         LANC = I_LEN(FIL_SCAV)
!
! ------ Arg. 3
! ------ Get the experiment type
!
         CALL CLRCH  ( CH_EXP )
         CALL GETARG ( 3, CH_EXP )
         IF ( CH_EXP == 'sde' .OR. CH_EXP == 'vlbi' ) THEN
            CONTINUE
         ELSE
            IUER = -1
            CALL ERR_LOG ( 6003, IUER, 'BNC_SCAV',                      &
     &              'Unsupported experiment type '//CH_EXP//            &
     &              'expected: sde or vlbi' )
            CALL EXIT ( 1 )
         END IF ! sde|vlbi
!
! ------ Arg. 4
! ------ Get the maximum scan difference
!
         CALL CLRCH  ( CH_TIM_DIF )
         CALL GETARG ( 4, CH_TIM_DIF )
!
! --------- Check if the number has a decimal point?
!
         READ (UNIT=CH_TIM_DIF, FMT='(F6.2)', IOSTAT=IER) TIM_DIF_MAX
         IF ( IER .NE. 0 ) THEN !
            IUER = -1
            CALL ERR_LOG ( 6004, IUER, 'BNC_SCAV',                      &
     &              'Expected real number for scan diff. '//            &
     &              CH_TIM_DIF )
         END IF ! ier
!
! ------ Arg. 5
! ------ Get the verbosity
!
         CALL CLRCH  ( CH_VRB )
         CALL GETARG ( 5, CH_VRB )
         CALL CHIN   ( CH_VRB, IVRB )
      END IF
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
! --- Parse data from binary file to ANC type
!
      IUER = -1
      CALL BNC_PARSE ( FIL_BNC, ANC, IUER )
      IF ( IUER .NE. 0 ) THEN
         IUER = -2
         CALL ERR_LOG ( 6007, IUER, 'BNC_SCAV',                         &
     &           'Failure in parsing binary file data from '//FIL_BNC )
         CALL EXIT ( 1 )
      END IF
! ----------------------------------------------------------------------
!
! --- Go through scav file and allocate the buffer variable
!
      IS = FILE_INFO ( TRIM(FIL_SCAV)//CHAR(0), UNIX_DATE, SIZE_I8 )
      MLEN_STR = SIZE_I8/ANC__MAVLS ! expected number of lines
      ALLOCATE ( BUF_SCAV(MLEN_STR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
         CALL CLRCH ( STR )
         CALL IINCH ( INT8(ANC__MBUF)*INT8(MLEN_STR), STR )
         IUER = -1
         CALL ERR_LOG ( 6008, IUER, 'BNC_SCAV',                         &
     &           'Error in an attempt to allocate '//TRIM(STR)//        &
     &           ' bytes of dynamic memory for input file' )
         CALL EXIT (1)
      END IF
!     
! --- Read the pre-filled scan average file
!
      CALL ERR_PASS ( IUER, IER )
      CALL ANC_READ ( FIL_SCAV, MLEN_STR, NBUF, BUF_SCAV, IER )
      IF ( IER .NE. 0 ) THEN
         IUER = -1
         CALL ERR_LOG ( 6010, IUER, 'BNC_SCAV',                         &
     &           'Error in an attempt to read file with antenna '//     &
     &           'calibration format '//TRIM(FIL_SCAV) )
         DEALLOCATE ( BUF_SCAV )
         RETURN
      END IF
!
! --- Flag to check if you are done reading the station name and 
!     experiment code
!     
      FL_CHK   = .FALSE.
! ---
      DO 610 J1 = 1, NBUF/2

         CALL EXWORD ( BUF_SCAV(J1), MIND, LIND, IND, DELIM, IER )
!
! ------ Get the station name from the sample binary file
!
         IF ( BUF_SCAV(J1)(IND(1,1):IND(2,1)) == "STATION:" ) THEN
            SCAV_STA = BUF_SCAV(J1)(IND(1,2):IND(2,2))
!
! ------ Get the expirement code from the sample binary file
!
         ELSEIF ( BUF_SCAV(J1)(IND(1,1):IND(2,1)) == "EXP_CODE:" ) THEN
            SCAV_EXP = BUF_SCAV(J1)(IND(1,2):IND(2,2))
            FL_CHK   = .TRUE.
         END IF
!
! ------ We assume that the experiment code will always be read
!        after the station name, ergo, once code is in hand, we are
!        done here for now. 
!
         IF ( FL_CHK ) GO TO 611
! ------
 610  CONTINUE
! ---
 611  CONTINUE
!
! --- Do the station name and experiment code in the sample file, match
!     those in the parsed binary file?
!
      IF ( ANC%STA_NAM == SCAV_STA .AND. ANC%EXP_CODE == SCAV_EXP ) THEN
         CONTINUE
      ELSE
         IUER = -1
         CALL ERR_LOG ( 6014, IUER, 'BNC_SCAV',                         &
     &        'Sta: '//ANC%STA_NAM//' & Exp: '//ANC%EXP_CODE//' in '//  &
     &        TRIM(FIL_BNC)//' do not match '//SCAV_STA//' & '//        &
     &        SCAV_EXP//' in '//TRIM(FIL_SCAV) )
         CALL EXIT ( 1 )
      END IF
!
      FIL_PATH = FIL_SCAV
!     
! ----------------------------------------------------------------------     
! --- How many scans are there?
!
      IUER = -1
      IVAR_FLG = 1 !  Tsys
      CALL ATP_SCANS (ANC, TIM_DIF_MAX, IVAR_FLG, NS_TOT, IND_ARR, IUER)
!
! --- Allocate the scan averaging variables
!
      ALLOCATE ( ITPS_SCA     ( ANC%NUM_TPS, NS_TOT), STAT = IER )
      ALLOCATE ( TIM_TSYS_AVR ( ANC%NUM_TPS, NS_TOT), STAT = IER )
      ALLOCATE ( TSYS_AVR     ( ANC%NUM_TPS, NS_TOT), STAT = IER )
      ALLOCATE ( TSYS_RMS     ( ANC%NUM_TPS, NS_TOT), STAT = IER )
      ALLOCATE ( AZ_SCA       ( ANC%NUM_TPS, NS_TOT), STAT = IER )
      ALLOCATE ( EL_SCA       ( ANC%NUM_TPS, NS_TOT), STAT = IER )
!     
! --- Go through ANC and compute the tsys averages for each scan at a
!     given frequency
!
      ICNT_MAX = 0
      ICNT     = 0
      ITPS_SCA = 0
      IT_WR    = 0
! ---
      DO 310 J1 = 1, ANC%NUM_TPS
!
! ------ Accept only Tsys elements with defined frequencies
!
         DELTS = ABS( ANC%TPS(J1)%SKY_FRQ - ANC__FILLER_R8 ) 
         IF ( DELTS .LT. DEL_MIN  ) GOTO 310
! ------
         ICNT = ICNT + 1
! ------
         IF ( ICNT_MAX < ICNT) ICNT_MAX = ICNT
!
! ------ Prefill the arrays
!
         TIM    = 0.D0
         TSYS   = 0.D0
         EL_ARR = 0.D0
         AZ_ARR = 0.D0
         ISCA   = 0
         IARR   = 0
         NP     = 0
!
! ------ Get the raw Tsys data (filtered for outliers) at the index == J1
!
         IUER = -1
         CALL TSYS_TIME_FILTER_RAW ( ANC, J1, NP, TIM, TSYS, EL_ARR,    &
     &                               AZ_ARR, IUER )
!
! ------ Get the scan averages and RMS for Tsys (at the given scan
!        differences)
!        The averages are also filtered out for outliers.
!
         IUER = -1
         CALL TSYS_TIME_FILTER_SCAN ( ANC, J1, TIM_DIF_MAX, NP, TIM,    &
     &                                TSYS, NS_TSYS(ICNT), T1_AVR,     &
     &                                Y1_AVR, Y1_RMS, ISCA, IARR, IUER )
!
! ------ Proceed only if there are actual points to use
!
         IF ( NP > 0 ) THEN 
!
! --------- For this frequency, assign scan results
!
            DO 320 J2 = 1, NS_TOT
!     
! ------------ In the scan we are looking at (IND_ARR(J2)), does this
!              frequency (ANC%TPS(J1)%SKY_FRQ) participate.
!
               IFND = -1
               IFND = IFIND_PL ( NS_TSYS(ICNT), IARR, IND_ARR(J2) )
!     
! ------------ Did this frequency participate in that scan
!
               IF ( IFND == -1 ) THEN
                  TSYS_AVR(ICNT,J2)   = ANC__FILLER_R8
                  TSYS_RMS(ICNT,J2)   = ANC__FILLER_R8
               ELSE
!     
! --------------- Is the average Tsys of this scan above minimum Tsys
!
                  IF ( Y1_AVR(IFND) > ANC__TSYS_MIN .AND.                  &
     &                 Y1_AVR(IFND) < ANC__TSYS_MAX       ) THEN
! ------------------
                     AZ_SCA(ICNT,J2)   = AZ_ARR(ISCA(IFND))/DEG__TO__RAD        ! Scan Azimuths     
                     EL_SCA(ICNT,J2)   = EL_ARR(ISCA(IFND))/DEG__TO__RAD        ! Scan Elevations (> ANC__EL_MIN)
! ------------------
                     TIM_TSYS_AVR(ICNT,J2) = T1_AVR(IFND)                       ! average time for this scan at the frequ
                     TSYS_AVR(ICNT,J2)     = Y1_AVR(IFND)
                     TSYS_RMS(ICNT,J2)     = Y1_RMS(IFND)
!
! ------------------ What is the index within ANC%TPS for the frequency 
!                    we are looking at?
!     
                     ITPS_SCA(ICNT,J2) = J1
! ------------------ How many points have we observed thus far
                     IT_WR = IT_WR + 1

!@@                  PRINT *, ICNT, J2, AZ_SCA(ICNT,J2),  EL_SCA(ICNT,J2),           &
   !@@  &                     TIM_TSYS_AVR(ICNT,J2), TSYS_AVR(ICNT,J2),    &
     !@@&                     TSYS_RMS(ICNT,J2), ITPS_SCA(ICNT,J2)
!     
! --------------- Anything out of range we put as empty
!
                  ELSE
                     TSYS_AVR(ICNT,J2)     = ANC__FILLER_R8
                     TSYS_RMS(ICNT,J2)     = ANC__FILLER_R8
                  END IF
               END IF
 320        CONTINUE
         END IF
 310  CONTINUE
!
! --- Write these results to the TSYS BUFFER
!
      NTSYS_OBS = 0
      DO 330 K1 = 1, NS_TOT
         K2CNT = 0
         DO 340 K2 = 1, ICNT_MAX
!     
! --------- Write only if the element exists
! --------- For now we assume that the elements for source and data scope
!           are empty
!     
            IF ( ITPS_SCA(K2,K1) .NE. 0 ) THEN
! ------------
               K2CNT = K2CNT + K2               
! ------------
               CALL CLRCH ( SCA_NAM )
               CALL CLRCH ( SOU_NAM )
               CALL CLRCH ( DATA_SCOPE )
! ------------ 
               CALL INCH ( K1, STR2 )
               SCA_NAM = "tsno0"//TRIM(STR2)
! -----------               
               SOU_NAM = "DUMMY"
               DATA_SCOPE = "DUMMY"
!
! ------------ Convert the average time to calendar date
! ------------ Get seconds since J2000 and (MJD_TSYS, TAI_TSYS)
!
! ------------ The IF statement is to ensure all the observations in
!              a scan have the same time stamp.
!
               IF ( K2CNT == K2 ) THEN
                  TSINCE = MJDSEC_TO_TIM (ANC%MJD_TSYS, ANC%TAI_TSYS)
!
! --------------- Get seconds since J2000 and time of computation
!
                  TSINCE = TSINCE + TIM_TSYS_AVR(K2,K1)
!
! --------------- Convert the time elapsed to a date format
!
                  IUER = -1
                  SPEC_DATE = TIM_TO_DATE ( TSINCE, IUER )
               END IF                
! ------------
               NTSYS_OBS = NTSYS_OBS + 1
               WRITE ( BUF_TSYS(NTSYS_OBS), 101 )                       &
                       K1,                                              & ! Scan index
     &                 SPEC_DATE,                                       & ! UTC_Time_Tag
     &                 ANC%TPS(ITPS_SCA(K2,K1))%TAG,                    & ! Sensor Tag
     &                 TSYS_AVR(K2,K1),                                 & ! Tsys_Ave [K]
     &                 AZ_SCA(K2,K1),                                   & ! Az [deg]
     &                 EL_SCA(K2,K1),                                   & ! El [deg]
     &                 SOU_NAM,                                         & ! Source Name
     &                 SCA_NAM,                                         & ! Scan Name
     &                 DATA_SCOPE,                                      & ! Data_Scope Flag
     &                 TSYS_RMS(K2,K1)
            END IF
 340     CONTINUE
 330  CONTINUE

      DEALLOCATE ( ITPS_SCA     )
      DEALLOCATE ( TIM_TSYS_AVR )
      DEALLOCATE ( TSYS_AVR     )
      DEALLOCATE ( TSYS_RMS     )
      DEALLOCATE ( AZ_SCA       )
      DEALLOCATE ( EL_SCA       )
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$PCAL $$$$$$$$$$$$$$$$
!     
! --- Deal with PCal
! --- How many scans are there?
!
      IUER = -1
      IVAR_FLG = 2 !  PCal
      CALL ATP_SCANS ( ANC, TIM_DIF_MAX, IVAR_FLG, NS_TOT,              &
     &                 IND_ARR_PCAL, IUER )
!
! --- Allocate the scan averaging variables
!
      ALLOCATE ( IPCS_SCA     ( ANC%NUM_PCS, NS_TOT), STAT = IER )
      ALLOCATE ( TIM_PCAL_AVR ( ANC%NUM_PCS, NS_TOT), STAT = IER )
      ALLOCATE ( PCAL_AMP_AVR ( ANC%NUM_PCS, NS_TOT), STAT = IER )
      ALLOCATE ( PCAL_PHA_AVR ( ANC%NUM_PCS, NS_TOT), STAT = IER )
      ALLOCATE ( PCAL_AMP_RMS ( ANC%NUM_PCS, NS_TOT), STAT = IER )
      ALLOCATE ( PCAL_PHA_RMS ( ANC%NUM_PCS, NS_TOT), STAT = IER )
!     
! --- Go through ANC and compute the PCal averages for each scan at a
!     given frequency
!
      IPCAL_MAX = 0
      IPCAL     = 0
      IPCS_SCA  = 0
      IP_WR     = 0
! ---
      DO 410 J1 = 1, ANC%NUM_PCS
!
! ------ Accept only PCal elements with defined frequencies
!
         DELTS = ABS( ANC%PCS(J1)%SKY_FRQ - ANC__FILLER_R8 ) 
         IF ( DELTS .LT. DEL_MIN  ) GOTO 410
! ------
         IPCAL = IPCAL + 1
! ------
         IF ( IPCAL_MAX < IPCAL) IPCAL_MAX = IPCAL
!
! ------ Prefill the arrays
!
         TIM    = 0.D0
         PCAL   = CMPLX(0.D0)
         ISCA   = 0
         IARR   = 0
         NP     = 0
!
! ------ Get the raw Phase cal data (Filtered for NaN values) at the index == J1
!
         IUER = -1
         CALL PCAL_TIME_FILTER_RAW ( ANC, J1, NP, TIM, PCAL, IUER )
!
! ------ Get the PCAL scan averages and RMS for Amplitude and
!        Phase (at the given scan differences).
! ------ The averages are also filtered out for outliers.
! ------ T1_AVR = TIM_AVR, Z_AVR = PCAL_AVR {CMPLX}, Y1_RMS = AMP_RMS, Y2_RMS = PHA_RMS
!
         IUER = -1
         CALL PCAL_TIME_FILTER_SCAN ( ANC, J1, TIM_DIF_MAX, NP, TIM,    &
     &                                PCAL, NS_PCAL(IPCAL), T1_AVR,     &
     &                                Z_AVR, Y1_RMS, Y2_RMS, ISCA,      &
     &                                IARR, IUER )
!
! ------ Proceed only if there are actual points to use
!
         IF ( NP > 0 ) THEN 
!
! --------- For this frequency, assign scan results
!
            DO 420 J2 = 1, NS_TOT
!     
! ------------ In the scan we are looking at (IND_ARR(J2)), does this
!              frequency (ANC%PCS(J1)%SKY_FRQ) participate.
!
               IFND = -1
               IFND = IFIND_PL (NS_PCAL(IPCAL), IARR, IND_ARR_PCAL(J2))
!     
! ------------ Did this frequency participate in that scan
!
               IF ( IFND == -1 ) THEN
                  PCAL_PHA_AVR(IPCAL,J2)  =  ANC__FILLER_R8
                  PCAL_PHA_RMS(IPCAL,J2)  =  ANC__FILLER_R8
                  PCAL_AMP_AVR(IPCAL,J2)  =  ANC__FILLER_R8
                  PCAL_AMP_RMS(IPCAL,J2)  =  ANC__FILLER_R8
               ELSE
!
! --------------- Is the average Amplitude of this scan above minimum Tsys
!
! --------------- Is the average Pcal of this scan above minimum Pcal
!
                  IF ( ( ABS(Z_AVR(IFND)) > ANC__AMP_MIN ) .AND.          &
     &                 ( ABS(Z_AVR(IFND)) < ANC__AMP_MAX ) ) THEN
! ------------------
                     TIM_PCAL_AVR(IPCAL,J2) = T1_AVR(IFND)                      ! average time for this scan at the frequ
                     PCAL_AMP_AVR(IPCAL,J2) = ABS(Z_AVR(IFND))
                     PCAL_PHA_AVR(IPCAL,J2) = PHAS_CMPL_R4(Z_AVR(IFND))         ! ave phas (resolved to [-pi,pi])
                     PCAL_AMP_RMS(IPCAL,J2) = Y1_RMS(IFND)
                     PCAL_PHA_RMS(IPCAL,J2) = Y2_RMS(IFND)
!
! ------------------ What is the index within ANC%PCS for the frequency 
!                    we are looking at?
!
                     IPCS_SCA(IPCAL,J2) = J1
!
! ------------------ How many points have we observed thus far
!
                     IP_WR = IP_WR + 1

!@@                  PRINT *, IPCAL, J2, TIM_PCAL_AVR(IPCAL,J2),     &
   !@@  &                     PCAL_PHA_AVR(IPCAL,J2),  &
!@@     &                     PCAL_PHA_RMS(IPCAL,J2),  &
   !@@  &                     PCAL_AMP_AVR(IPCAL,J2),  &
!@@     &                     PCAL_AMP_RMS(IPCAL,J2),  &
   !@@  &                     IPCS_SCA(IPCAL,J2)
               
!     
! --------------- Anything out of range we put as empty
!
                  ELSE
                     PCAL_PHA_AVR(IPCAL,J2)  =  ANC__FILLER_R8
                     PCAL_PHA_RMS(IPCAL,J2)  =  ANC__FILLER_R8
                     PCAL_AMP_AVR(IPCAL,J2)  =  ANC__FILLER_R8
                     PCAL_AMP_RMS(IPCAL,J2)  =  ANC__FILLER_R8
                  END IF
               END IF
 420        CONTINUE
         END IF
 410  CONTINUE
!
! --- Write these results to the PCAL BUFFER
!
      NPCAL_OBS = 0
      DO 430 K1 = 1, NS_TOT
         K2CNT = 0
         DO 440 K2 = 1, IPCAL_MAX
!     
! --------- Write only if the element exists
! --------- For now we assume that the elements for source and data scope
!           are empty
!     
            IF ( IPCS_SCA(K2,K1) .NE. 0 ) THEN
! ------------
               K2CNT = K2CNT + K2               
! ------------
               CALL CLRCH ( SCA_NAM )
               CALL CLRCH ( SOU_NAM )
               CALL CLRCH ( DATA_SCOPE )
! ------------ 
               CALL INCH ( K1, STR2 )
               SCA_NAM = "pcno0"//TRIM(STR2)
! -----------               
               SOU_NAM = "DUMMY"
               DATA_SCOPE = "DUMMY"
!
! ------------ Convert the average time to calendar date
! ------------ Get seconds since J2000 and (MJD_PCAL, TAI_PCAL)
!
! ------------ The IF statement is to ensure all the observations in
!              a scan have the same time stamp.
!
               IF ( K2CNT == K2 ) THEN
                  TSINCE = MJDSEC_TO_TIM (ANC%MJD_PCAL, ANC%TAI_PCAL)
!
! --------------- Get seconds since J2000 and time of computation
!
                  TSINCE = TSINCE + TIM_PCAL_AVR(K2,K1)
!
! --------------- Convert the time elapsed to a date format
!
                  IUER = -1
                  SPEC_DATE = TIM_TO_DATE ( TSINCE, IUER )
               END IF                
! ------------

      !                  scan#    utc      pcs     ampl      phas
 !102  FORMAT("PCAL:", 2X, I6, 1X, A22, 2X, A9, 2X, F8.3, 2X, F9.5, 2X,  &
!                        source  scan_nam dataFlag ampl_rms  phas_rms
!     &                   A8, 2X, A10, 2X, A13, 2X, F8.4, 2X, F9.5  )

               
               NPCAL_OBS = NPCAL_OBS + 1
               WRITE ( BUF_PCAL(NPCAL_OBS), 102 )                       &
                       K1,                                              & ! Scan index
     &                 SPEC_DATE,                                       & ! UTC_Time_Tag
     &                 ANC%PCS(IPCS_SCA(K2,K1))%TAG,                    & ! Sensor Tag
     &                 PCAL_AMP_AVR(K2,K1)/ANC__AMP_SCA,                             & ! Ampl_Ave []
     &                 PCAL_PHA_AVR(K2,K1),                             & ! Phas_Ave [-pi,pi]
     &                 SOU_NAM,                                         & ! Source Name
     &                 SCA_NAM,                                         & ! Scan Name
     &                 DATA_SCOPE,                                      & ! Data_Scope Flag
     &                 PCAL_AMP_RMS(K2,K1)/ANC__AMP_SCA,                & ! Ampl_rms
     &                 PCAL_PHA_RMS(K2,K1)                                ! Phas_rms
            END IF
 440     CONTINUE
 430  CONTINUE
! ---
      DEALLOCATE ( IPCS_SCA     )
      DEALLOCATE ( TIM_PCAL_AVR )
      DEALLOCATE ( PCAL_AMP_AVR )
      DEALLOCATE ( PCAL_PHA_AVR )
      DEALLOCATE ( PCAL_AMP_RMS )
      DEALLOCATE ( PCAL_PHA_RMS )
!-----------------------------------------------------------------------
!
! --- Allocate the Buffer for output
!     N.B: Make it twice the size of BUF_SCAV      
!
      ALLOCATE ( BUF_OUT(2*MLEN_STR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
         CALL CLRCH ( STR )
         CALL IINCH ( INT8(ANC__MBUF)*INT8(2*MLEN_STR), STR )
         CALL ERR_LOG ( 6009, IUER, 'BNC_SCAV',                         &
     &           'Error in an attempt to allocate '//TRIM(STR)//        &
     &           ' bytes of dynamic memory for output file' )
         CALL EXIT (1)
      END IF
!
! --- What is the current time and date?
!
      CUR_DATE = GET_CDATE ( )
!
! --- Go through pre-filled buffere and parse information to the
!     output buffer.
!
      FL_GLO   = .FALSE.                  ! all the blocks where no information changes
      FL_PRO   = .FALSE.                  ! Provenance block
      FL_TSYS  = .FALSE.                  ! Tsys block
      FL_PCAL  = .FALSE.                  ! PhaseCal block
! ---
      NOUT = 0
      DO 510 J1 = 1, NBUF

         CALL EXWORD ( BUF_SCAV(J1), MIND, LIND, IND, DELIM, IER )
! ------
         NOUT  = NOUT + 1
!
! ------ Define the blocks
! ------ Edit the TSYS BLOCK
!     
         IF ( BUF_SCAV(J1)(IND(1,1):IND(2,1)) == "NUM_TSYS:" ) THEN
! ---------
            WRITE ( BUF_SCAV(J1), 111 )
!
! --------- Replace the number of TSYS's
!
            WRITE ( BUF_OUT(NOUT),   111 )
            WRITE ( BUF_OUT(NOUT+1), 112 ) NTSYS_OBS
            WRITE ( BUF_OUT(NOUT+2), 113 )
            WRITE ( BUF_OUT(NOUT+3), 114 )
            WRITE ( BUF_OUT(NOUT+4), 111 )
! ---------
            NOUT = NOUT + 5
! ---------  
            DO 511 K1 = 1, NTSYS_OBS
               BUF_OUT(NOUT) = BUF_TSYS(K1)
               NOUT = NOUT + 1
 511        CONTINUE
            NOUT = NOUT - 1
!
! ------ Edit the PCAL Block
!
         ELSEIF (BUF_SCAV(J1)(IND(1,1):IND(2,1))=="NUM_PCAL:") THEN
! ---------
            WRITE ( BUF_SCAV(J1), 111 )
!
! --------- Replace the number of PCAL's
!
            WRITE ( BUF_OUT(NOUT),   111 )
            WRITE ( BUF_OUT(NOUT+1), 115 ) NPCAL_OBS
            WRITE ( BUF_OUT(NOUT+2), 116 )
            WRITE ( BUF_OUT(NOUT+3), 117 )
            WRITE ( BUF_OUT(NOUT+4), 111 )
! ---------
            NOUT = NOUT + 5
! ---------  
            DO 513 K1 = 1, NPCAL_OBS
               BUF_OUT(NOUT) = BUF_PCAL(K1)
               NOUT = NOUT + 1
 513        CONTINUE
            NOUT = NOUT - 1
         ELSE
!
! --------- Make changes to the provenance block
!
            IF ( BUF_SCAV(J1)(IND(1,1):IND(2,1))=="PROVENANCE:" ) THEN
!
! ------------ length of this line
!
               LSTR = I_LEN(BUF_SCAV(J1))
! ------------
               IF ( BUF_SCAV(J1)(IND(1,3):IND(2,3)) ==                  &
     &              "NUM_FILES:" ) THEN
                  BUF_SCAV(J1)(IND(1,4):IND(2,4)) = "2"
! ------------
               ELSEIF ( BUF_SCAV(J1)(IND(1,3):IND(2,3)) ==             &
     &                  "DATA_TYPE:" ) THEN
                  BUF_PROV(1) = BUF_SCAV(J1)
                  BUF_PROV(1)(IND(1,4):LSTR) = "2 Scan Average File"
! ------------
               ELSEIF ( BUF_SCAV(J1)(IND(1,3):IND(2,3)) ==              &
     &                  "DATA_FILE:" ) THEN
                  BUF_PROV(2) = BUF_SCAV(J1)
                  BUF_PROV(2)(IND(1,4):IND(2,4)) = TRIM(FIL_PATH)
! ------------
               ELSEIF ( BUF_SCAV(J1)(IND(1,3):IND(2,3)) ==              &
     &                  "DATA_CREATED:" ) THEN
                  BUF_PROV(3) = BUF_SCAV(J1)
                  BUF_PROV(3)(IND(1,4):IND(2,4)) = "2"
                  BUF_PROV(3)(IND(1,5):IND(2,5)) = CUR_DATE
! ------------
               ELSEIF ( BUF_SCAV(J1)(IND(1,3):IND(2,3)) ==              &
     &                  "NUM_COMMENTS:" ) THEN
                  BUF_PROV(4) = BUF_SCAV(J1)
                  BUF_PROV(4)(IND(1,4):IND(2,4)) = "2"
! ------------
               ELSEIF ( BUF_SCAV(J1)(IND(1,3):IND(2,3)) ==              &
     &                  "COMMENT:" ) THEN
                  BUF_PROV(5) = BUF_SCAV(J1)
                  BUF_PROV(5)(IND(1,4):LSTR) =                          &
     &                          "Version 2 of the anc file"
               END IF
            END IF
!
! --------- erase the comment section previously assigned to orig file
!
            IF ( BUF_SCAV(J1)(IND(1,2):IND(2,2)) == "ScanIdx" ) THEN
               WRITE ( BUF_SCAV(J1), 111 )
            ELSEIF ( BUF_SCAV(J1)(IND(1,2):IND(2,2)) ==                 &
     &               "YYYY.DD.MM-hh:mm:ss.ff" ) THEN
               WRITE ( BUF_SCAV(J1), 111 )
            END IF
!     
! --------- copy as is
!     
            BUF_OUT(NOUT) = TRIM(BUF_SCAV(J1))
! ---------
            IF ( BUF_SCAV(J1)(IND(1,3):IND(2,3)) == "COMMENT:" ) THEN
               DO 512 K1 = 1, 5
                  BUF_OUT(NOUT) = BUF_PROV(K1)
                  NOUT = NOUT + 1
 512           CONTINUE
               NOUT = NOUT - 1
            END IF            
         END IF
 510  CONTINUE
!
! --- re-write to file
!
      IUER = -1
      CALL WR_TEXT ( NOUT, BUF_OUT, FIL_PATH, IUER )
! -----------------------------------------------------------------------
      !                 scan#    utc      tps     tsys      az          
 101  FORMAT ("TSYS:", 2X, I6, 2X, A22, 2X, A7, 2X, F7.1, 2X, F9.4, 2X,  &
!                      el      source  scan_nam  dataFlag rms
     &                 F9.4, 2X, A8, 2X, A10, 2X, A14, 2X, F7.2 )
      !                  scan#    utc      pcs     ampl      phas
 102  FORMAT ("PCAL:", 2X, I6, 1X, A22, 2X, A9, 2X, F7.3, 2X, F9.5, 2X,  &
!                     source  scan_nam dataFlag ampl_rms     phas_rms
     &                 A8, 2X, A10, 2X, A13, 2X, 1PD12.5, 2X, 1PD12.5  )
!
 103  FORMAT (                                                          &
     &   '#',                                                        /  &
     &   'NUM_TSYS: ', I6                                            /  &
     &   '#',                                                        /  &
     &   '#', 6X, 'ScanIdx', 2X, 'UTC_Time_tag', 12X, 'Sensor', 5X,     &
     &        'Tsys', 5X, 'Azimuth', 5X, 'Elevat', 2X, 'Source', 4X,    &
     &        'Scan', 8X, 'DataScopeFlag', 3X, 'Tsys_RMS',           /  &
     &   '#', 15X, 'YYYY.DD.MM-hh:mm:ss.ff', 2X, ' tag', 8X, 'K', 11X,  &
     &        'deg', 8X, 'deg', 43X, 'K',                            /  &
     &   '# ' )
!
 104  FORMAT (                                                          &
     &   '#',                                                        /  &
     &   'NUM_PCAL: ', I6                                            /  &
     &   '#',                                                        /  &
     &   '#', 6X, 'ScanIdx', 1X, 'UTC_Time_tag', 13X, 'Sensor', 6X,     &
     &        'Ampl', 4X, 'Phase', 5X, 'Source', 4X, 'Scan', 8X,        &
     &        'DataScopeFlag', 3X, 'Ampl_RMS', 6X, 'Phase_RMS'       /  &
     &   '#', 14X, 'YYYY.DD.MM-hh:mm:ss.ff', 2X, ' tag', 16X,           &
     &        'rad[-pi,pi)', 53X, 'rad',                            /  &
     &   '# ' )
!
 111  FORMAT ( '#' )
 112  FORMAT ( 'NUM_TSYS: ', I6 )
 113  FORMAT ( '#', 5X, 'ScanIdx', 2X, 'UTC_Time_tag', 12X, 'Sensor',   &
     &         5X, 'Tsys', 5X, 'Azimuth', 5X, 'Elevat', 2X, 'Source',   &
     &         4X, 'Scan', 8X, 'DataScopeFlag', 3X, 'Tsys_RMS' )
 114  FORMAT ( '#', 14X, 'YYYY.DD.MM-hh:mm:ss.ff', 3X, ' tag', 8X,      &
     &         11X, 'deg', 8X, 'deg', 43X, 'K' )
!
 115  FORMAT ( 'NUM_PCAL: ', I6 )
 116  FORMAT ( '#', 6X, 'ScanIdx', 1X,'UTC_Time_tag',13X,'Sensor', 6X,  &
     &        'Ampl', 4X, 'Phase', 5X, 'Source', 4X, 'Scan', 8X,        &
     &        'DataScopeFlag', 3X, 'Ampl_RMS', 6X, 'Phase_RMS' )
 117  FORMAT ( '#', 14X, 'YYYY.DD.MM-hh:mm:ss.ff', 2X, ' tag', 16X,     &
     &        'rad[-pi,pi)', 53X, 'rad' )
! ---
      END PROGRAM
