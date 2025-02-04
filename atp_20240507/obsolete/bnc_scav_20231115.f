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
      CHARACTER  PLOTVAR*6, PLOTORD*4
      CHARACTER  STR_DO_DATE*64, TIT*64, CH_EXP*4, CH_VRB
      CHARACTER  CH_IND*12, CH_POL*2, CH_PLOT*4, CH_DATE(2)*22
      CHARACTER  DELIM*3
      INTEGER*4  LBNC, LANC, NUM_TPS, NUM_PCS
      INTEGER*8  SIZE_I8, MLEN_STR
      INTEGER*4  IUER, IER, IVRB, ICNT, IVAR_FLG
      INTEGER*4  MIND, UNIX_DATE, IS
      LOGICAL*1  LEX
      CHARACTER  CH_TIM_DIF*6
      PARAMETER  ( MIND = 32 )
      PARAMETER  ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9) )                ! Null, Space, Tab
      INTEGER*4  NBUF, LIND, IND(2,MIND)
      REAL*8     TIM_TSYS_AVR(ANC__MTPS, ANC__MEPC), TIM_DIF_MAX
      REAL*8     TIM_PCAL_AVR(ANC__MPCS, ANC__MEPC), TIM(ANC__MEPC)
      REAL*8     TSYS_RMS(ANC__MTPS, ANC__MEPC), TSYS(ANC__MEPC)
      REAL*8     TSYS_AVR(ANC__MTPS, ANC__MEPC), EL_ARR(ANC__MEPC)
      REAL*8     AMP_SCA(ANC__MEPC), AZ_ARR(ANC__MEPC)
      REAL*8     PHA_SCA(ANC__MEPC), TIM_AVR(ANC__MEPC)
      REAL*8     AZ_SCA(ANC__MTPS, ANC__MEPC)
      REAL*8     EL_SCA(ANC__MTPS, ANC__MEPC)
      COMPLEX*8  PCAL_AVR(ANC__MPCS,ANC__MEPC), Z_AVR(ANC__MEPC)
      COMPLEX*8  PCAL(ANC__MEPC)
      REAL*8     PCAL_AMP_RMS(ANC__MPCS,ANC__MEPC)
      REAL*8     PCAL_PHA_RMS(ANC__MPCS,ANC__MEPC)
      REAL*8     T1_AVR(ANC__MEPC)
      REAL*8     Y1_AVR(ANC__MEPC), Y1_RMS(ANC__MEPC), Y2_RMS(ANC__MEPC)
      INTEGER*4  ISCA(ANC__MEPC), IARR(ANC__MEPC)
      INTEGER*4  JP, J0, J1, J2, J3, J4
      INTEGER*4  K0, K1, K2, K3, K4, K5
      INTEGER*4  IND_CNT, NP
      INTEGER*4  ICNT_MAX, J2_MAX
      INTEGER*4  NS_TSYS(ANC__MTPS), NS_PCAL(ANC__MPCS)
      REAL*8     DELTS, DEL_MIN
      PARAMETER  ( DEL_MIN = 1.D-6 )
      INTEGER*4  ITPS(ANC__MEPC), ITPS_SCA(ANC__MTPS, ANC__MEPC)
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
      INTEGER*8, EXTERNAL :: LSEEK, READ
      INTEGER*4, EXTERNAL :: IXMN8, IXMN4, LINDEX, ILEN, I_LEN
      INTEGER*4, EXTERNAL :: LTM_DIF, FILE_INFO, IFIND_PL
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
!$$$$$
      INTEGER*4  NS_TOT, IND_ARR(ANC__MEPC)
      INTEGER*4  IN_SCAN(ANC__MEPC), IFND
      REAL*8,    DIMENSION(:,:), ALLOCATABLE :: T2W_AVR, Y2W_AVR
      REAL*8,    DIMENSION(:,:), ALLOCATABLE :: Y2W_RMS, AZW, ELW
      INTEGER*4, DIMENSION(:,:), ALLOCATABLE :: ITPSW
      INTEGER*4  MP, MAXL_STRING, IT_WR, IWR ! Max no. of lines,  Max. String length
      PARAMETER  ( MAXL_STRING = 512 )
      PARAMETER  ( MP = 128*1024 )
      CHARACTER  BUF_TSYS(MP)*(MAXL_STRING)            ! Read Tsys Block 
      CHARACTER  SCA_NAM*12, SOU_NAM*8, DATA_SCOPE*14
      REAL*8     TSINCE
!     
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~ 'Usage: bnc_scav bnc_file scav_file sde|vlbi del verbosity'
!~~ examples
! bnc_scav gsst04_orig.bnc gsst04_scav.anc sde 8.0 1
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
            CALL ERR_LOG ( 6001, IUER, 'BNC_SCAV',                 &
     &              'Cannot find bnc file '//TRIM(FIL_BNC) )
            CALL EXIT ( 1 )
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
            CALL ERR_LOG ( 6004, IUER, 'BNC_PLOT',                      &
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
! --- Check if the scan average file corresponds to the given 
! --- N.B: This search assumes the current naming conversion of
!          ssssss_orig.bnc and ssssss_scav.anc for sde binary and scan
!          average files, versus vvvvvvVV_orig.bnc and vvvvvvVV_scav.anc
!          for the VLBI counter parts, where vvvvvv is the experiment
!          name and VV is the station id.
!
      CALL CLRCH(STR1(1))
      CALL CLRCH(STR1(2))
      IF ( CH_EXP == 'sde' ) THEN
         STR1(1) = FIL_BNC(LBNC-14:LBNC-9)        ! Exp name from sde bnc file path
         STR1(2) = FIL_SCAV(LANC-14:LANC-9)       ! Exp name from sde scav anc file path
         IF ( STR1(1) .NE. STR1(2) ) THEN
            IUER = -1
            CALL ERR_LOG ( 6005, IUER, 'BNC_SCAV',                      &
     &              'Experiment name mismatch between binary file'//    &
     &              TRIM(FIL_BNC)//' and scan average file '//FIL_SCAV )
         END IF
!     
! --- Currently only other option is vlbi
!
      ELSE
         STR1(1) = FIL_BNC(LBNC-16:LBNC-9)        ! Exp name from vlbi bnc file path (including station)
         STR1(2) = FIL_SCAV(LANC-16:LANC-9)       ! Exp name from vlbi scav anc file path
         IF ( STR1(1) .NE. STR1(2) ) THEN
            IUER = -1
            CALL ERR_LOG ( 6006, IUER, 'BNC_SCAV',                      &
     &              'Experiment name (with stn) mismatch between '//    &
     &              'binary file '//TRIM(FIL_BNC)//' and scan '//       &
     &              'average file '//TRIM(FIL_SCAV) )
         END IF
!
! ------ Get the experiment name w/o the station
!
         STR2 = STR1(1)(1:6)
      END IF
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
!
! --- Go through file and allocate the buffer variable
!
      IS = FILE_INFO ( TRIM(FIL_SCAV)//CHAR(0), UNIX_DATE, SIZE_I8 )
      MLEN_STR = SIZE_I8/ANC__MAVLS ! expected number of lines
      ALLOCATE ( BUF_SCAV(MLEN_STR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
         CALL CLRCH ( STR )
         CALL IINCH ( INT8(ANC__MBUF)*INT8(MLEN_STR), STR )
         CALL ERR_LOG ( 6008, IUER, 'BNC_SCAV',                         &
     &           'Error in an attempt to allocate '//TRIM(STR)//        &
     &           ' bytes of dynamic memory for input file' )
         CALL EXIT (1)
      END IF
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
! --- Read the pre-filled scan average file
!
      CALL ERR_PASS ( IUER, IER )
      CALL ANC_READ ( FIL_SCAV, MLEN_STR, NBUF, BUF_SCAV, IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 6010, IUER, 'BNC_SCAV',                         &
     &           'Error in an attempt to read file with antenna '//     &
     &           'calibration format '//TRIM(FIL_SCAV) )
         DEALLOCATE ( BUF_SCAV )
         RETURN
      END IF
!
! --- How many scans are there?
!
      IUER = -1
      IVAR_FLG = 1 !  Tsys
      CALL ATP_SCANS (ANC, TIM_DIF_MAX, IVAR_FLG, NS_TOT, IND_ARR, IUER)
!     
! --- Go through ANC and compute the tsys averages for each scan at a
!     given frequency
!
      ICNT = 0
!@@!      ITPS = 0
      IT_WR = 0
! ---
      DO 310 J1 = 1, ANC%NUM_TPS
!
! ------ Accept only Tsys elements with defined frequencies
!
         DELTS = ABS( ANC%TPS(J1)%SKY_FRQ - ANC__FILLER_R8 ) 
         IF ( DELTS .LT. DEL_MIN  ) GOTO 310
! ------
         ICNT = ICNT + 1
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
!@@@!!
!@@@!! --------- Which scans did this particular frequency participate in?
!@@@!!
!@@@!            IUER = -1
!@@@!            CALL ARRAY_COMPARE ( NS_TOT, NS_TSYS(ICNT), IND_ARR, IARR,  &
!@@@!     &                           IN_SCAN, IUER )
!
! --------- For this frequency, assign scan results
!
            DO 320 J2 = 1, NS_TOT
!
! ------------ What is the index within ANC%TPS for the frequency we are looking at
!
!ERROR!               ITPS_SCA(ICNT,J2) = J1
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
!ERROR!                  TSYS_AVR(ICNT,J2)   = ANC__FILLER_R8
!ERROR!                  TSYS_RMS(ICNT,J2)   = ANC__FILLER_R8
               ELSE
!     
! --------------- Is the average Tsys of this scan above minimum Tsys
!
                  IF ( Y1_AVR(IFND) > ANC__TSYS_MIN .AND.                  &
     &                 Y1_AVR(IFND) < ANC__TSYS_MAX       ) THEN
! ------------------
                     AZ_SCA(ICNT,J2)   = AZ_ARR(ISCA(IFND))/DEG__TO__RAD        ! Scan Azimuths     
!ERROR!                     EL_SCA(ICNT,J2)   = EL_ARR(ISCA(IFND))/DEG__TO__RAD        ! Scan Elevations (> ANC__EL_MIN)
! ------------------
!ERROR!                     TIM_TSYS_AVR(ICNT,J2) = T1_AVR(IFND)                       ! average time for this scan at the frequ
!ERROR!                     TSYS_AVR(ICNT,J2)     = Y1_AVR(IFND)
!ERROR!                     TSYS_RMS(ICNT,J2)     = Y1_RMS(IFND)
! ------------------

        PRINT *, "%%%%", AZ_SCA(ICNT, J2)
                     
                     IT_WR = IT_WR + 1
! ------------------
                     WRITE ( BUF_TSYS(IT_WR), 106 ) J1, ICNT, J2, IFND, &
     &                          T1_AVR(IFND), Y1_AVR(IFND),             &
     &                          Y1_RMS(IFND),                           &
     &                          AZ_ARR(ISCA(IFND))/DEG__TO__RAD,        &
     &                          EL_ARR(ISCA(IFND))/DEG__TO__RAD
!     
! --------------- Anything out of range we put as empty
!
                  ELSE
!ERROR!                     TSYS_AVR(ICNT,J2)     = ANC__FILLER_R8
!ERROR!                     TSYS_RMS(ICNT,J2)     = ANC__FILLER_R8
                  END IF
               END IF
 320        CONTINUE

         END IF
!
! Plot
!
!@@         IUER = -1
!@@         CALL DIAGI_1E(JP, TIM_TSYS_AVR, TSYS_AVR, TSYS_RMS, IUER )
        
 310  CONTINUE
!     
! --- Allocate the storage variables
!
      ALLOCATE ( ITPSW( IT_WR, IT_WR), STAT = IER )      
      ALLOCATE ( T2W_AVR( IT_WR, IT_WR), STAT = IER )      
      ALLOCATE ( Y2W_AVR( IT_WR, IT_WR), STAT = IER )      
      ALLOCATE ( Y2W_RMS( IT_WR, IT_WR), STAT = IER )      
      ALLOCATE ( AZW( IT_WR, IT_WR), STAT = IER )      
      ALLOCATE ( ELW( IT_WR, IT_WR), STAT = IER )      
!     
! --- Copy elements of BUF_TSYS to variables
!
      ITPSW    = 0
      ICNT_MAX = 0
      J2_MAX   = 0
! ---      
      DO 330 K1 = 1, IT_WR
         IF ( ILEN(BUF_TSYS(K1)) == 0 ) GO TO 330

         CALL EXWORD ( BUF_TSYS(K1), MIND, LIND, IND, DELIM, IER )
!
! ------- 
!
         READ ( UNIT = BUF_TSYS(K1)(IND(1,2):IND(2,2)),                 &
     &          FMT = '(I6)', IOSTAT = IER              ) J1               ! Freq. Ind.
! ------
         READ ( UNIT = BUF_TSYS(K1)(IND(1,4):IND(2,4)),                 &
     &          FMT = '(I6)', IOSTAT = IER              ) ICNT             ! Freq. Index
! ------
         READ ( UNIT = BUF_TSYS(K1)(IND(1,6):IND(2,6)),                 &
     &          FMT = '(I6)', IOSTAT = IER              ) J2               ! Scan Number
! ------
         READ ( UNIT = BUF_TSYS(K1)(IND(1,8):IND(2,8)),                 &
     &          FMT = '(I6)', IOSTAT = IER              ) IFND             ! IFND
! ------
         READ ( UNIT=BUF_TSYS(K1)(IND(1,10):IND(2,10)),                   &
     &          FMT='(F25.12)', IOSTAT=IER          ) T2W_AVR(ICNT,J2)       ! TIM_AVR
! ------
         READ ( UNIT=BUF_TSYS(K1)(IND(1,12):IND(2,12)),                   &
     &          FMT='(F15.8)', IOSTAT=IER           ) Y2W_AVR(ICNT,J2)       ! TSYS_AVR
! ------
         READ ( UNIT=BUF_TSYS(K1)(IND(1,14):IND(2,14)),                   &
     &          FMT='(F15.8)', IOSTAT=IER           ) Y2W_RMS(ICNT,J2)       ! TSYS_RMS
! ------
         READ ( UNIT=BUF_TSYS(K1)(IND(1,16):IND(2,16)),                   &
     &          FMT='(F15.8)', IOSTAT=IER           ) AZW(ICNT,J2)           ! AZ [deg]
! ------
         READ ( UNIT=BUF_TSYS(K1)(IND(1,16):IND(2,16)),                   &
     &          FMT='(F15.8)', IOSTAT=IER           ) ELW(ICNT,J2)           ! EL [deg]
!
! ------ What is the index within ANC%TPS for the point we are looking at
!
         ITPSW(ICNT,J2) = J1
!
! ------ What is the maximum scan number?
!
         IF ( J2_MAX < J2 ) J2_MAX = J2
!
! ------ What is the maximum freq. index we encountered?
!
         IF ( ICNT_MAX < ICNT) ICNT_MAX = ICNT
!     
 330  CONTINUE
!
! --- Clear the buffer and re-write it in the manner it will go to the file
!
      CALL CLRCH ( BUF_TSYS )
! ---
      IWR = 0
      DO 340 K3 = 1, J2_MAX
!!##
!!!         ! --------------- Get the time for this index
!!!! --------------- Get seconds since J2000 and (MJD_TSYS, TAI_TSYS)
!!!!
 !!!     TSINCE(2) = MJDSEC_TO_TIM (ANC%MJD_TSYS, ANC%TAI_TSYS)
!
! --------------- Get seconds since J2000 and time of computation
!
    !!!              TSINCE(2) = TSINCE(2) + TIM_AVR(IND_TIM)
!
! --------------- Convert the time elapsed to a date format
!
       !!!           IUER = -1
          !!!        SPEC_DATE(2) = TIM_TO_DATE ( TSINCE(2), IUER )
!!##




                  
                  
         DO 350 K2 = 1, ICNT_MAX
!     
! --------- Write only if the element exists
! --------- For now we assume that the elements for source and data scope
!           are empty
!     
            IF ( ITPSW(K2,K3) .NE. 0 ) THEN
!
! ------------- 
!
               CALL CLRCH ( SCA_NAM )
               CALL CLRCH ( SOU_NAM )
               CALL CLRCH ( DATA_SCOPE )
! ------------ 
               CALL INCH ( K3, STR2 )
               SCA_NAM = "no0"//TRIM(STR2)
! -----------               
               SOU_NAM = "DUMMY"
               DATA_SCOPE = "DUMMY"
!               
               IWR = IWR + 1
!!               WRITE ( BUF_TSYS(IWR), 107 ) K3,                         & ! Scan index
  !!   &                                      



 
               

            END IF
            



 350     CONTINUE
 340  CONTINUE
      





      
! 106  FORMAT( "J1: ", I6, "  ICNT: ", I6, "  J2: ", I6, "  IFND: ", I6, &
 !    &        "  TIM_AVR: " F25.12, "  TSYS_AVR: ", F15.8,              &
  !   &        "  TSYS_RMS: ", F15.8, "  AZ: ", F15.8, "  EL: ", F15.8 )


      
!#############################################################WE ARE HERE###############
!     
! --- Deal with PCal
!
!++      ICNT = 0
! ---
!++      DO 410 J1 = 1, ANC%NUM_PCS
!++!
! -!++----- Accept only Tsys elements with defined frequencies
!!++
    !++     DELTS = ABS( ANC%PCS(J1)%SKY_FRQ - ANC__FILLER_R8 ) 
       !++  IF ( DELTS .LT. DEL_MIN  ) GOTO 410
!++! ------
   !++      ICNT = ICNT + 1
!++!     
!++! ------ Prefill the arrays
!++!
   !++      TIM    = 0.D0
      !++   TSYS   = 0.D0
         !++ISCA   = 0
!++         IARR   = 0
   !++      NP     = 0

!++!
!++! --------- Get the raw Phase cal data (Filtered for NaN values)
!++!
   !++         IUER = -1
      !++      CALL PCAL_TIME_FILTER_RAW (ANC, J1, NP, TIM, PCAL, IUER)
!++!
!++! --------- Get the PCAL scan averages and RMS for Amplitude and
!++!           Phase (at the given scan differences).
!++! --------- The averages are also filtered out for outliers.
!++!     
   !++         IUER = -1
      !++      CALL PCAL_TIME_FILTER_SCAN ( ANC, IND_FRQ, TIM_DIF_MAX, NP, &
!++     &                                   T1, CX1, NS, TIM_AVR,          &
   !++  &                                   PCAL_AVR, PCAL_AMP_RMS,        &
!++     &                                   PCAL_PHA_RMS, IND_SCA,         &
   !++  &                                   IND_ARR, IUER )
!++!
!++! --------- Proceed only if there are actual points to use
!++!
   !++         IF ( NP > 0 ) THEN 
!++!
!++! ------------ Convert time to hours from midnight UTC
!++!     
   !++         END IF  


!++ 410  CONTINUE
      
         

      
 110  FORMAT (A22, 'PLOT= ',A5 ,' FRQ= ',F8.1,' POL= ', A1,' ID= ', A4 )
!
 104  FORMAT("TP_SENSOR:  ", I4, 3X, F8.2, 3X, A1, 3X, A4 )
 105  FORMAT("TSYS: ", I4, 1X, I4, 3X, A22, 3X, F7.1, 3X, F10.4, 2X, F8.4 )
!
 106  FORMAT( "J1: ", I6, "  ICNT: ", I6, "  J2: ", I6, "  IFND: ", I6, &
     &        "  TIM_AVR: " F25.12, "  TSYS_AVR: ", F15.8,              &
     &        "  TSYS_RMS: ", F15.8, "  AZ: ", F15.8, "  EL: ", F15.8 )
!
      !                  scan#    utc      tps     tsys      az          
 107  FORMAT("TSYS:", 2X, I6, 2X, A22, 2X, A7, 2X, F5.1, 2X, F9.4, 2X,  &
!                      el      source  scan_nam  dataFlag rms
     &                F7.4, 2X, A8, 2X, A12, 2X, A14, 2X, F5.2 )
      
      END PROGRAM
