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
      INTEGER*4  IUER, IER, IVRB, ICNT
      INTEGER*4  MIND, UNIX_DATE, IS
      LOGICAL*1  LEX
      CHARACTER  CH_TIM_DIF*6
      PARAMETER  ( MIND = 32 )
      PARAMETER  ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9) )                ! Null, Space, Tab
      INTEGER*4  NBUF, LIND, IND(2,MIND)
!##@@!!      REAL*8     TIM_TSYS_AVR(ANC__MTPS,ANC__MEPC), TIM_DIF_MAX
!##@@!!      REAL*8     TIM_PCAL_AVR(ANC__MPCS,ANC__MEPC), TIM(ANC__MEPC)
!##@@!!      REAL*8     TSYS_RMS(ANC__MTPS,ANC__MEPC), TSYS(ANC__MEPC)
!##@@!!      REAL*8     TSYS_AVR(ANC__MTPS,ANC__MEPC), EL_ARR(ANC__MEPC)
      REAL*8     TIM_TSYS_AVR(ANC__MEPC), TIM_DIF_MAX
      REAL*8     TIM_PCAL_AVR(ANC__MEPC), TIM(ANC__MEPC)
      REAL*8     TSYS_RMS(ANC__MEPC), TSYS(ANC__MEPC)
      REAL*8     TSYS_AVR(ANC__MEPC), EL_ARR(ANC__MEPC)
      REAL*8     AMP_SCA(ANC__MEPC), AZ_ARR(ANC__MEPC)
      REAL*8     PHA_SCA(ANC__MEPC), TIM_AVR(ANC__MEPC)
      REAL*8     AZ_SCA(ANC__MEPC)
      REAL*8     EL_SCA(ANC__MEPC)     ! EL_SCA(ANC__MPCS, ANC__MEPC)
      COMPLEX*8  PCAL_AVR(ANC__MPCS,ANC__MEPC), Z_AVR(ANC__MEPC)
      COMPLEX*8  PCAL(ANC__MEPC)
      REAL*8     PCAL_AMP_RMS(ANC__MPCS,ANC__MEPC)
      REAL*8     PCAL_PHA_RMS(ANC__MPCS,ANC__MEPC)
      REAL*8     Y1_AVR(ANC__MEPC), Y1_RMS(ANC__MEPC), Y2_RMS(ANC__MEPC)
      INTEGER*4  ISCA(ANC__MEPC), IARR(ANC__MEPC)
      INTEGER*4  JP, J0, J1, J2, J3, J4
      INTEGER*4  IND_CNT, NP
      INTEGER*4  NS_TSYS(ANC__MTPS), NS_PCAL(ANC__MPCS)
      REAL*8     DELTS, DEL_MIN
      PARAMETER  ( DEL_MIN = 1.D-6 )
      INTEGER*4  ITPS(ANC__MEPC)
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
      INTEGER*8, EXTERNAL :: LSEEK, READ
      INTEGER*4, EXTERNAL :: IXMN8, IXMN4, LINDEX, ILEN, I_LEN
      INTEGER*4, EXTERNAL :: LTM_DIF, FILE_INFO
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
!$$$$$
      REAL*8     XX(ANC__MEPC), YY(ANC__MEPC), ZZ(ANC__MEPC)
      INTEGER*4  K0
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
!
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
!
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
! --- Go through ANC and compute the tsys averages for each scan at a
!     given frequency
!
      ICNT = 0
      ITPS = 0
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
     &                                TSYS, NS_TSYS(ICNT), TIM_AVR,     &
     &                                Y1_AVR, Y1_RMS, ISCA, IARR, IUER )
!
! ------ Proceed only if there are actual points to use
!
         IF ( NP > 0 ) THEN 
!
! --------- Filter out any zero values from the average values
!
            JP = 0
            DO 320 J2 = 1, NS_TSYS(ICNT)
!
! ------------ Is the average Tsys of this scan above minimum Tsys
!
               IF ( Y1_AVR(J2) > ANC__TSYS_MIN .AND.                  &
     &              Y1_AVR(J2) < ANC__TSYS_MAX       ) THEN
! ---------------
                  JP = JP + 1                                           ! Update counter
! ---------------
                  AZ_SCA(JP)   = AZ_ARR(ISCA(J2))/DEG__TO__RAD ! Scan Azimuths     
                  EL_SCA(JP)   = EL_ARR(ISCA(J2))/DEG__TO__RAD ! Scan Elevations (> ANC__EL_MIN)
                  TSYS_AVR(JP) = Y1_AVR(J2) ! Filtered Tsys ave.
!
! ---------------
!
                  TIM_TSYS_AVR(JP)  =  TIM_AVR(J2) + ANC%TAI_TSYS +     &
     &                                 ANC%UTC_MTAI )/3600.D0 - 24.D0 
                  TSYS_RMS(JP)      =  Y1_RMS(J2)                                ! Tsys error
               END IF
 320        CONTINUE
         END IF

!
! Plot
!
!@@         IUER = -1
!@@         CALL DIAGI_1E(JP, TIM_TSYS_AVR, TSYS_AVR, TSYS_RMS, IUER )

         
 310  CONTINUE
!NH!!
!NH!! --- Go through ANC and compute the pcal averages for each scan at a
!NH!!     given frequency
!NH!!
!NH!      ICNT = 0
!NH!      DO 410 J1 = 1, ANC%NUM_PCS
!NH!!
!NH!! ------ Accept only Tsys elements with defined frequencies
!NH!!
!NH!         IF ( ANC%PCS(J1)%SKY_FRQ == ANC__FILLER_R8 ) GOTO 410
!NH!! ------
!NH!         ICNT = ICNT + 1
!NH!!     
!NH!! ------ Prefill the arrays
!NH!!
!NH!         ISCA   = 0
!NH!         IARR   = 0
!NH!! ------
!NH!         IUER = -1
!NH!         CALL PCAL_TIME_FILTER_RAW ( ANC, J1, NP, TIM, PCAL, IUER )
!NH!! ------
!NH!         IUER = -1
!NH!         CALL PCAL_TIME_FILTER_SCAN ( ANC, J1, TIM_DIF_MAX, NP, TIM,    &
!NH!     &                                PCAL, NS_PCAL(ICNT), TIM_AVR,     &
!NH!     &                                Z_AVR, Y1_RMS, Y2_RMS, ISCA,      &
!NH!     &                                IARR, IUER )
!NH!!     
!NH!! ------ Proceed only if there are actual points to use
!NH!!
!NH!         IF ( NP > 0 ) THEN 
!NH!!@@!!
!NH!!@@!! --------- Filter out any zero values from the average values
!NH!!@@!!
!NH!            JP = 0
!NH!            DO 420 J2 = 1, NS_PCAL(ICNT)
!NH!!@@!!
!NH!!@@!! ------------ In the event of one scan, let's override the check
!NH!!@@!!
!NH!!@@!               IF ( NS_PCAL(ICNT) == 1 ) GOTO 421
!NH!!@@!!     
!NH!!@@!! ------------ Is the average Pcal of this scan above minimum Pcal
!NH!!@@!!
!NH!!@@!               IF ( ( ABS(Z_AVR(J2)) > ANC__AMP_MIN ) .AND.             &
!NH!!@@!     &              ( ABS(Z_AVR(J2)) < ANC__AMP_MAX )       ) THEN
!NH!!@@!! ---------------
!NH!!@@! 421              CONTINUE
!NH!!@@!! ---------------
!NH!                  JP = JP + 1                                           ! Update counter
!NH!
!NH!        PRINT *, "%%%%%%%%%%%%% BNC_SCAV - 342 PCAL %%%%%%%%%%%%%%%%%%%"
!NH!        PRINT *, "PC#=", J1, "ICNT=", ICNT, "JP=", JP, "FRQ=", ANC%PCS(J1)%SKY_FRQ        
!NH!
!NH!                  
!NH!! ---------------
!NH!                  PCAL_AVR(ICNT,JP) = Z_AVR(J2)                         ! Filtered Pcal ave.
!NH!!
!NH!! ---------------
!NH!!
!NH!                  TIM_PCAL_AVR(ICNT,JP)  =  TIM_AVR(J2) + ANC%TAI_PCAL
!NH!                  PCAL_AVR(ICNT,JP)      =  Z_AVR(J2)
!NH!                  PCAL_AMP_RMS(ICNT,JP)  =  Y1_RMS(J2)                  ! Amplitude error
!NH!                  PCAL_PHA_RMS(ICNT,JP)  =  Y2_RMS(J2)                  ! Phase error
!NH!! ---------------
!NH!!@@!                  IF ( NS_PCAL(ICNT) == 1 ) GOTO 422
!NH!! ---------------
!NH!!@@!               END IF
!NH! 420        CONTINUE
!NH!         END IF
!NH! 410  CONTINUE
! ---
!@@! 422  CONTINUE
! ---
 110  FORMAT (A22, 'PLOT= ',A5 ,' FRQ= ',F8.1,' POL= ', A1,' ID= ', A4 )
!
 104  FORMAT("TP_SENSOR:  ", I4, 3X, F8.2, 3X, A1, 3X, A4 )
 105  FORMAT("TSYS: ", I4, 1X, I4, 3X, A22, 3X, F7.1, 3X, F10.4, 2X, F8.4 )

      END PROGRAM
