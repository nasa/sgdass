      SUBROUTINE ANC_PARSE ( FILIN, ANC, NERS, IUER )
!
! ************************************************************************
! *                                                                      *
! *   Routine ANC_PARSE                                                  *
! *                                                                      *
! *   INPUT:                                                             *
! *       FILIN   =  ASCII (original) antcal File      { CHAR }          *
! *                                                                      *
! *       NERS    =  Initialised NERS Package          { DERIVED TYPE }  *
! *                  Network Earth Rotation Service                      *
! *                                                                      *
! *       IUER    =  Error Handler                     { INT, OPT }      *
! *                        If IUER=0 no error message will be printed,   *
! *                        even in the event of an error. However, for   *
! *                        other possible values, i.e. IUER=-1,-2, & -3, *
! *                        the error message will print to screen. For   *
! *                        the latter case, i.e., IUER = -3, after       *
! *                        after printing the program will terminate.    *
! *                        Default, IUER = -1                            *
! *                                                                      *
! *   OUTPUT:                                                            *
! *       ANC    =  Parsed Antenna Calibration file    { DERIVED TYPE }  *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 07-MAY-2021  ANC_PARSE   v3.1 (d)  N. Habana  11-AUG-2025  ###  *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE   'atp.i'
      INCLUDE   'ners.i'
      CHARACTER  FILIN*(*)
      TYPE     ( ANC__TYP   ) :: ANC      
      TYPE     ( NERS__TYPE ) :: NERS
      CHARACTER, ALLOCATABLE :: BUF(:)*(ANC__MSTR)
      CHARACTER  CUR_LINE*(ANC__MSTR), PREV_LINE*(ANC__MSTR)
      INTEGER*4  IUER
      CHARACTER  DELIM*3, STR*128, STR1*16, STR2*16
      INTEGER*4  MIND, MJD_MIN, ICNT
      PARAMETER  ( MIND = 32 )
      PARAMETER  ( MJD_MIN = 44329 )
      PARAMETER  ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9) ) ! Null, Space, Tab
      INTEGER*4  NBUF, LIND, IND(2,MIND)
      INTEGER*4  IND_DOO, IND_MET, IND_CBL
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, JP1
      INTEGER*4  IND_TGPS, IND_PCS, IND_TPS, UNIX_DATE, IS, IER
      INTEGER*4  K_TPS, K_TTO, K_PCS, K_PCAL, K_SEFD, K_TGPS, K_GPS
      INTEGER*4  K_TPI, K_OPA, K_TATM
      INTEGER*4  ITPION, ITPIOFF, IPRV
      INTEGER*8  SIZE_I8, MLEN_STR, IL
      INTEGER*4  MJD, IANC_FMT_VERS
      INTEGER*4  IND_PRV_CUR, IND_PRV_PAS, IPRV_CNT
      REAL*8     TAI, UTC, UTC_MTAI, EPS
      PARAMETER  ( EPS = 1.D-4 )
      LOGICAL*1  FL_UPDATE
      REAL*8     RAMPL, RPHAS, PCAL_REAL, PCAL_IMG
      INTEGER*4, EXTERNAL :: LTM_DIF, FILE_INFO, I_LEN
!
      IS = FILE_INFO ( TRIM(FILIN)//CHAR(0), UNIX_DATE, SIZE_I8 )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6821, IUER, 'ANC_PARSE',                        &
     &         'Error in an attempt to get information about '// &
     &         'input file '//FILIN )
          RETURN 
      END IF
!
!@@@ WE MULTIPLY MLEN_STR BY 20, so that the binary version can be read @@@! !MH 20240415_1037
!
      MLEN_STR = (SIZE_I8/ANC__MAVLS)*20 ! expected number of lines
      ALLOCATE ( BUF(MLEN_STR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
         CALL CLRCH ( STR )
         CALL IINCH ( INT8(ANC__MBUF)*INT8(MLEN_STR), STR )
         CALL ERR_LOG ( 6822, IUER, 'ANC_PARSE',                        &
     &           'Error in an attempt to allocate '//TRIM(STR)//        &
     &           ' bytes of dynamic memory' )
         RETURN 
      END IF
!
! --- Read file
!
      CALL ERR_PASS ( IUER, IER )
      CALL ANC_READ ( FILIN, MLEN_STR, NBUF, BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6823, IUER, 'ANC_PARSE',                        &
     &         'Error in an attempt to read file with antenna '//     &
     &         'calibration format '//TRIM(FILIN) )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
      IF ( BUF(1)(1:24) .NE. '# ANTCAL Format  Version' ) THEN
           CALL ERR_LOG ( 6824, IUER, 'ANC_PARSE',                        &
     &         'Error in an attempt to read file with antenna '//     &
     &         'calibration format '//TRIM(FILIN)//                   &
     &         ' -- the first line does not contain magic' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
! --- Clean the ANC structure to avoid any lingering values
!
      IF ( ANC%STATUS .NE. ATP__INIT ) CALL ANC_CLEAN ( ANC ) 
!     
! --- Initialize dates and counters
!
      ANC%MJD_DOO     = -1
      ANC%MJD_MET     = -1
      ANC%MJD_TTO     = -1
      ANC%MJD_PCAL    = -1
      ANC%MJD_SEFD    = -1
      ANC%MJD_GPS     = -1
      ANC%MJD_TPI     = -1
      ANC%MJD_FILLER  = -1
      ANC%TAI_DOO     =  0.0D0
      ANC%TAI_MET     =  0.0D0
      ANC%TAI_TTO     =  0.0D0
      ANC%TAI_PCAL    =  0.0D0
      ANC%TAI_SEFD    =  0.0D0
      ANC%TAI_GPS     =  0.0D0
      ANC%TAI_TPI     =  0.0D0
      ANC%TAI_FILLER  =  0.0D0
! ---
      ANC%NUM_PRV  =  0
      ANC%NUM_DOO  =  0
      ANC%NUM_MET  =  0
      ANC%NUM_TPS  =  0
      ANC%NUM_TSYS =  0
      ANC%NUM_TPI  =  0
      ANC%NUM_PCS  =  0
      ANC%NUM_PCAL =  0
      ANC%NUM_TGPS =  0
      ANC%NUM_GPS  =  0
      ANC%NUM_SEFD =  0
      ANC%NUM_CBL  =  0
      ANC%NUM_OPA  =  0
      ANC%NUM_TATM =  0
! ---
      K_TPS    =  0
      K_TTO    =  0
      K_PCS    =  0
      K_PCAL   =  0
      K_SEFD   =  0
      K_TGPS   =  0
      K_GPS    =  0
      K_TPI    =  0
      K_OPA    =  0
      K_TATM   =  0
!     
      IND_TPS = 0
      IND_PCS = 0
      IND_DOO = 0
      IND_MET = 0
      IND_CBL = 0
! ---
      IND_PRV_CUR  =  0
      IND_PRV_PAS  =  0
      IPRV_CNT     =  0
      IPRV         = -1
!
! --- Parse Format version
!
      ANC%ANTCAL_FMT = BUF(1)
      CUR_LINE = BUF(1)
!
! --- Is this a recognised Format
!
      IF ( TRIM(ANC%ANTCAL_FMT) == TRIM( ANTCAL__FMT_1) ) THEN
         IANC_FMT_VERS = 1
      ELSEIF ( TRIM(ANC%ANTCAL_FMT) == TRIM( ANTCAL__FMT_2) ) THEN
         IANC_FMT_VERS = 2
      ELSE
         CALL ERR_LOG ( 6825, IUER, 'ANC_PARSE',                        &
     &           'Unsupported version of ANTCAL FMT'//TRIM(BUF(1)) )
         DEALLOCATE ( BUF )
         RETURN
      END IF
!
! --- Counter of lines in scan average file
! --- N.B: We've already filled the first line
!     
      ICNT = 1 
!
! --- Go through file
!     
      DO 410 J1=2,NBUF
         PREV_LINE = CUR_LINE
         CUR_LINE = BUF(J1)
         IF ( CUR_LINE(1:1) == '#' ) GOTO 410 ! Skip comments
!
! ------ Which Version are we dealing with
! ------ Version 1 = 2021.05.17
!
         IF ( IANC_FMT_VERS == 1 ) THEN
!
! --------- First process the performance critical lines: TSYS, PCAL, 
!           and TPI using the most efficent way
!
!==============================================================================
            IF ( CUR_LINE(1:5) == 'TSYS:' ) THEN
!
! ------------ This is the Tsys record
! ------------ Check whether the date filed in this record is the same as
! ------------ in the previous record
!
               IF ( CUR_LINE(15:36) .NE. PREV_LINE(15:36) ) THEN
!
! --------------- Not the same? Then we need to parse time
!
                  CALL ERR_PASS ( IUER, IER )
                  CALL DATE_TO_TIME ( CUR_LINE(15:36), MJD, UTC, IER)
                  IF ( IER .NE. 0 ) THEN
                     CALL CLRCH (     STR )
                     CALL INCH  ( J1, STR )
                     CALL ERR_LOG ( 6826, IUER, 'ANC_PARSE',            &
     &                       'Error in parsing TSYS date '//            &
     &                       CUR_LINE(15:36)//' on line '//             &
     &                       TRIM(STR) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF

                  IF ( ANC%MJD_TTO < 0 ) THEN
                     CALL ERR_PASS ( IUER, IER )
                     CALL NERS_GET_UTCMTAI ( NERS,                         &
     &                     (MJD - J2000__MJD)*86400.D0 + UTC,           &
     &                     UTC_MTAI, IER )
                     IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6827, IUER, 'ANC_PARSE',            &
     &                       'Error in getting UTC_MTAI on '//          &
     &                       'TSYS Block' )
                        DEALLOCATE ( BUF )
                        RETURN
                     END IF
!
! ------------------ MJD_TTO and TAI_TTO
!
                     ANC%MJD_TTO = MJD
                     ANC%TAI_TTO = UTC - UTC_MTAI
                  END IF
                  TAI = UTC - UTC_MTAI
               END IF
!
! ------------ Update the Tsys counter, given the time diff. error is met
!
               IF ( K_TTO == 0 ) THEN
                  FL_UPDATE = .TRUE.
               ELSE
                  IF ( DABS((MJD - ANC%MJD_TTO)*86400.D0 +              &
     &                      (TAI - ANC%TAI_TTO) - ANC%TTO(K_TTO)%TIM)   &
     &                  > 3*ANC__EPS_TIM ) THEN
                     FL_UPDATE = .TRUE.
                  ELSE
                     FL_UPDATE = .FALSE.
                  END IF
               END IF
               IF ( FL_UPDATE ) THEN
                  K_TTO = K_TTO + 1
!
! --------------- Check for overflow
!
                  IF ( K_TTO > ANC%NUM_TTO ) THEN
                     CALL CLRCH (     STR )
                     CALL INCH  ( J1, STR )
                     CALL IINCH ( K_TTO, STR1 )
                     CALL IINCH ( ANC%NUM_TTO, STR2 )
                     CALL ERR_LOG ( 6828, IUER, 'ANC_PARSE',            &
     &                      'Malformed antenna calibration file '//     &
     &                       TRIM(FILIN)//' -- too many TTO lines. '//  &
     &                       'Got '//TRIM(STR1)//' ,expected '//        &
     &                       TRIM(STR2) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- Compute time
!
                  ANC%TTO(K_TTO)%TIM = (MJD - ANC%MJD_TTO)*86400.D0 +   &
     &                                 (TAI - ANC%TAI_TTO)
                  READ ( UNIT=CUR_LINE(7:13),  FMT='(I7)'   )           &
     &                   ANC%TTO(K_TTO)%IND_SCA
                  READ ( UNIT=CUR_LINE(54:63), FMT='(F9.4)' )           &
     &                   ANC%TTO(K_TTO)%AZ                           
                  READ ( UNIT=CUR_LINE(66:72), FMT='(F8.4)' )           &
     &                   ANC%TTO(K_TTO)%EL                            
                  ANC%TTO(K_TTO)%AZ = ANC%TTO(K_TTO)%AZ*DEG__TO__RAD    ! Azimuth   [rad]
                  ANC%TTO(K_TTO)%EL = ANC%TTO(K_TTO)%EL*DEG__TO__RAD    ! Elevation [rad]
                  ANC%TTO(K_TTO)%SOU_NAM = CUR_LINE(74:82)              ! Source Name
                  ANC%TTO(K_TTO)%SCA_NAM = CUR_LINE(85:95)              ! Scan name
               END IF
!
! ------------ Is the sensor tag in TTO block available in list of tags
!              from TPS_SENSOR block
!
               IF ( IND_TPS < ANC%NUM_TPS ) THEN
                  IF ( CUR_LINE(39:44) == ANC%TPS_TAG(IND_TPS+1) ) THEN
                     IND_TPS = IND_TPS + 1
                  ELSE
                     IND_TPS = LTM_DIF ( 0, ANC%NUM_TPS, ANC%TPS_TAG,   &
     &                                   CUR_LINE(39:44) )
                  END IF
               ELSE
                  IND_TPS = LTM_DIF ( 0, ANC%NUM_TPS, ANC%TPS_TAG,      &
     &                                CUR_LINE(39:44) )
               END IF
               IF ( IND_TPS < 1 ) THEN
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6829, IUER, 'ANC_PARSE',               &
     &                    'Tag '//CUR_LINE(39:46)//'not part of '//     &
     &                    'TP_SENSOR tags in '//TRIM(FILIN)//' .See '   &
     &                    //'line '//TRIM(STR)//' on the TTO Block' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Parse the Tsys value to the index of TTO in ANC%TTO that 
!              coincides with the index of the sensor tag in TP_SENSOR 
!              Block. Units = K
!
               READ ( UNIT=CUR_LINE(47:52), FMT='(F6.1)' )                &
     &                ANC%TTO(K_TTO)%TSYS(IND_TPS)
               GOTO 410
!==============================================================================
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
            ELSE IF ( CUR_LINE(1:5) == 'PCAL:' ) THEN
!
! ------------ This is the Pcal record
! ------------ Check whether the date filed in this record is the same as
! ------------ in the previous record
!
               IF ( TRIM(CUR_LINE(15:37)) .NE.                          &
     &              TRIM(PREV_LINE(15:37))        ) THEN
!
! --------------- Not the same? Then we need to parse time
!
                  CALL ERR_PASS ( IUER, IER )
                  CALL DATE_TO_TIME ( TRIM(CUR_LINE(15:37)), MJD, UTC,  &
     &                                IER )
!
! ---------------
!
                  IF ( IER .NE. 0 ) THEN
                     CALL CLRCH (     STR )
                     CALL INCH  ( J1, STR )
                     CALL ERR_LOG ( 6830, IUER, 'ANC_PARSE',            &
     &                       'Error in parsing PCAL date '//            &
     &                       CUR_LINE(15:37)//' in line '//STR )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! ---------------
!
                  IF ( ANC%MJD_PCAL < 0 ) THEN
                     CALL ERR_PASS ( IUER, IER )
                     CALL NERS_GET_UTCMTAI ( NERS,                      &
     &                        (MJD - J2000__MJD)*86400.D0 + UTC,        &
     &                        UTC_MTAI, IER )
                     IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6831, IUER, 'ANC_PARSE',         &
     &                          'Error in getting UTC_MTAI on '//       &
     &                          'PCAL Block' )
                        DEALLOCATE ( BUF )
                        RETURN
                     END IF
!
! ------------------ MJD_PCAL and TAI_PCAL
!
                     ANC%MJD_PCAL = MJD
                     ANC%TAI_PCAL = UTC - UTC_MTAI
                  END IF
                  TAI = UTC - UTC_MTAI
               END IF
               IF ( K_PCAL == 0 ) THEN
                  FL_UPDATE = .TRUE.
               ELSE
                  IF ( DABS((MJD-ANC%MJD_PCAL)*86400.D0 +               &
     &                      (TAI-ANC%TAI_PCAL) - ANC%PCAL(K_PCAL)%TIM)  &
     &                  > 3*ANC__EPS_TIM ) THEN
                     FL_UPDATE = .TRUE.
                  ELSE
                     FL_UPDATE = .FALSE.
                  END IF
               END IF
!
               IF ( FL_UPDATE ) THEN
                  K_PCAL = K_PCAL + 1
!
! --------------- Check for overflow
!
                  IF ( K_PCAL > ANC%NUM_PCAL ) THEN
                     CALL CLRCH (     STR )
                     CALL INCH  ( J1, STR )
                     CALL IINCH ( K_PCAL, STR1 )
                     CALL IINCH ( ANC%NUM_PCAL, STR2 )
                     CALL ERR_LOG ( 6832, IUER, 'ANC_PARSE',            &
     &                      'Malformed antenna calibration file '//     &
     &                      TRIM(FILIN)//' -- too many PCAL lines. '//  &
     &                      'Got '//TRIM(STR1)//' ,expected '//         &
     &                      TRIM(STR2)//' last processed line: '//STR )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- Compute time tag of the pcal record
!
                  ANC%PCAL(K_PCAL)%TIM = (MJD - ANC%MJD_PCAL)*86400.D0  &
     &                                   + (TAI - ANC%TAI_PCAL)
                  READ ( UNIT=CUR_LINE(7:13), FMT='(I7)' )              &
     &                   ANC%PCAL(K_PCAL)%IND_SCA
                  ANC%PCAL(K_PCAL)%SOU_NAM = CUR_LINE(70:79)            ! Source Name
                  ANC%PCAL(K_PCAL)%SCA_NAM = CUR_LINE(81:91)            ! Scan name
               END IF
!
! ------------ Is the sensor tag in PCAL block available in list of tags
! ------------ from PCS_SENSOR block
!
               IF ( IND_PCS < ANC%NUM_PCS ) THEN
                  IF ( TRIM(CUR_LINE(39:45)) ==                         &
     &                 TRIM(ANC%PCS_TAG(IND_PCS+1)) ) THEN
                     IND_PCS = IND_PCS + 1
                  ELSE
                     IND_PCS = LTM_DIF ( 0, ANC%NUM_PCS, ANC%PCS_TAG,   &
     &                                   CUR_LINE(39:45) )
                  END IF
               ELSE
                  IND_PCS = LTM_DIF ( 0, ANC%NUM_PCS, ANC%PCS_TAG,      &
     &                                CUR_LINE(39:45) )
               END IF
               IF ( IND_PCS < 1 ) THEN
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6833, IUER, 'ANC_PARSE',               &
     &                    'Tag '//CUR_LINE(39:45)//' not a part of '//  &
     &                    'PC_SENSOR tags in '//TRIM(FILIN)//           &
     &                    ' .See line '//TRIM(STR)//' on PCAL Block' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Parse the PCal value to the index of PCAL_CMPL in ANC%PCAL 
! ------------ that coincides with the index of the sensor tag in 
! ------------ PC_SENSOR Block. Units = 
!
!------------------------------- 2025.08.29 correction ----------------------
!NH2025.08.29NH!               READ ( UNIT=CUR_LINE(48:57), FMT='(F9.3)' ) RAMPL ! Amplitude
!NH2025.08.29NH!               IF ( ABS(RAMPL - ANC%FILLER_R8) > EPS ) THEN
!NH2025.08.29NH!                  RAMPL = RAMPL*ANC__AMP_SCA
!NH2025.08.29NH!               ELSE
!NH2025.08.29NH!                  RAMPL = ANC__AMP_MIN
!NH2025.08.29NH!               END IF
!NH2025.08.29NH!!
!NH2025.08.29NH!               READ ( UNIT=CUR_LINE(60:67), FMT='(F8.5)' ) RPHAS ! Phase angle
!NH2025.08.29NH!!
!NH2025.08.29NH!! ------------ Convert the amplitude and phase to complex number
!NH2025.08.29NH!!
!NH2025.08.29NH!               ANC%PCAL(K_PCAL)%PCAL_CMPL(IND_PCS) =                    &
!NH2025.08.29NH!     &              CMPLX ( RAMPL*DCOS(RPHAS), RAMPL*DSIN(RPHAS) )
!
! ------------ 
!
               READ ( UNIT=CUR_LINE(48:57), FMT='(F9.3)' ) RAMPL ! Amplitude
               READ ( UNIT=CUR_LINE(60:67), FMT='(F8.5)' ) RPHAS ! Phase angle
!
               IF ( ABS(RAMPL - ANC%FILLER_R8) > EPS ) THEN
                  RAMPL = RAMPL*ANC__AMP_SCA
               ELSE
                  RAMPL = ANC%FILLER_R8
                  RPHAS = ANC%FILLER_R8
               END IF
!
! ------------ Convert the amplitude and phase to complex number
!
               ANC%PCAL(K_PCAL)%PCAL_CMPL(IND_PCS) =                    &
     &              CMPLX ( RAMPL*DCOS(RPHAS), RAMPL*DSIN(RPHAS) )
!------------------------------- 2025.08.29 correction ----------------------
               GOTO 410
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!
! --------- Parse information from the TPI Block
!
            ELSEIF ( CUR_LINE(1:4) == 'TPI:' ) THEN
!
! ------------ This is the TPI record
! ------------ Check whether the date filed in this record is the same as
! ------------ in the previous record
!
               IF ( TRIM(CUR_LINE(14:36)) .NE.                          &
     &              TRIM(PREV_LINE(14:36))        ) THEN
!
! --------------- Not the same? Then we need to parse time
!
                  CALL ERR_PASS ( IUER, IER )
                  CALL DATE_TO_TIME ( CUR_LINE(14:36), MJD, UTC, IER)
                  IF ( IER .NE. 0 ) THEN
                     CALL CLRCH (     STR )
                     CALL INCH  ( J1, STR )
                     CALL ERR_LOG ( 6892, IUER, 'ANC_PARSE',            &
     &                       'Error in parsing TPI date '//             &
     &                       CUR_LINE(15:36)//' on line '//TRIM(STR) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF

                  IF ( ANC%MJD_TPI < 0 ) THEN
                     CALL ERR_PASS ( IUER, IER )
                     CALL NERS_GET_UTCMTAI ( NERS,                      &
     &                     (MJD - J2000__MJD)*86400.D0 + UTC,           &
     &                     UTC_MTAI, IER )
                     IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6893, IUER, 'ANC_PARSE',         &
     &                       'Error in getting UTC_MTAI on '//          &
     &                       'TPI Block' )
                        DEALLOCATE ( BUF )
                        RETURN
                     END IF
!
! ------------------ MJD_TPI and TAI_TPI
!
                     ANC%MJD_TPI = MJD
                     ANC%TAI_TPI = UTC - UTC_MTAI
                  END IF
                  TAI = UTC - UTC_MTAI
               END IF
!
! ------------ Update the TPI counter, given the time diff. error is met
!
!
               IF ( K_TPI == 0 ) THEN
                  FL_UPDATE = .TRUE.
               ELSE
                  IF ( DABS((MJD - ANC%MJD_TPI)*86400.D0 +              &
     &                      (TAI - ANC%TAI_TPI) - ANC%TPI(K_TPI)%TIM)   &
     &                  > 3*ANC__EPS_TIM ) THEN
                     FL_UPDATE = .TRUE.
                  ELSE
                     FL_UPDATE = .FALSE.
                  END IF
               END IF
!
! ------------ If the counter was updated, then work on appending the 
!              TPI structure
!     
               IF ( FL_UPDATE ) THEN
                  K_TPI = K_TPI + 1
!
! --------------- Check for overflow
!
                  IF ( K_TPI > ANC%NUM_TPI ) THEN
                     CALL CLRCH (     STR )
                     CALL INCH  ( J1, STR )
                     CALL IINCH ( K_TPI, STR1 )
                     CALL IINCH ( ANC%NUM_TPI, STR2 )
                     CALL ERR_LOG ( 6894, IUER, 'ANC_PARSE',            &
     &                      'Malformed antenna calibration file '//     &
     &                       TRIM(FILIN)//' -- too many TPI lines. '//  &
     &                       'Got '//TRIM(STR1)//' ,expected '//        &
     &                       TRIM(STR2) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- Compute time
!
                  ANC%TPI(K_TPI)%TIM = (MJD - ANC%MJD_TPI)*86400.D0 +   &
     &                                 (TAI - ANC%TAI_TPI)
                  READ ( UNIT=CUR_LINE(6:12),  FMT='(I7)'   )           &
     &                   ANC%TPI(K_TPI)%IND_SCA
                  READ ( UNIT=CUR_LINE(65:73), FMT='(F9.4)' )           &
     &                   ANC%TPI(K_TPI)%AZ                           
                  READ ( UNIT=CUR_LINE(75:82), FMT='(F8.4)' )           &
     &                   ANC%TPI(K_TPI)%EL                            
                  ANC%TPI(K_TPI)%AZ = ANC%TPI(K_TPI)%AZ*DEG__TO__RAD    ! Azimuth   [rad]
                  ANC%TPI(K_TPI)%EL = ANC%TPI(K_TPI)%EL*DEG__TO__RAD    ! Elevation [rad]
                  ANC%TPI(K_TPI)%SOU_NAM = CUR_LINE(85:92)              ! Source Name
                  ANC%TPI(K_TPI)%SCA_NAM = CUR_LINE(95:105)             ! Scan name
               END IF
!
! ------------ Is the sensor tag in TPI block available in list of tags
!              from TPS_SENSOR block
!
               IF ( IND_TPS < ANC%NUM_TPS ) THEN
                  IF ( TRIM(CUR_LINE(38:44)) ==                         &
     &                 TRIM(ANC%TPS_TAG(IND_TPS+1)) ) THEN
                     IND_TPS = IND_TPS + 1
                  ELSE
                     IND_TPS = LTM_DIF ( 0, ANC%NUM_TPS, ANC%TPS_TAG,   &
     &                                   TRIM(CUR_LINE(38:44)) )
                  END IF
               ELSE
                  IND_TPS = LTM_DIF ( 0, ANC%NUM_TPS, ANC%TPS_TAG,      &
     &                                TRIM(CUR_LINE(38:44)) )
               END IF
               IF ( IND_TPS < 1 ) THEN
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6895, IUER, 'ANC_PARSE',               &
     &                    'Tag '//CUR_LINE(38:44)//'not part of '//    &
     &                    'TP_SENSOR tags in '//TRIM(FILIN)//' .See '   &
     &                    //'line '//TRIM(STR)//' on the TPI Block' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Parse the TPION and TPIOFF values to their corresponding
!              indices on TPION and TPIOFF in ANC%TPI also corresponding
!              to  the correct sensor tag in TP_SENSOR Block. Units = K
!
               READ ( UNIT=CUR_LINE(47:53), FMT='(I8)' )                &
     &                ANC%TPI(K_TPI)%TPION(IND_TPS) 
               READ ( UNIT=CUR_LINE(55:62), FMT='(I8)' )                &
     &                ANC%TPI(K_TPI)%TPIOFF(IND_TPS)
! ------------
               GOTO 410
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
            END IF
!
! ========= Now we process lines that are not performance critical
!
            CALL CLRCH  (     STR )
            CALL INCH   ( J1, STR )
            CALL EXWORD ( CUR_LINE, MIND, LIND, IND, DELIM, IER )       ! Extract words
! ---------
            IF ( CUR_LINE(IND(1,1):IND(2,1)) == 'STATION:' ) THEN
               ANC%STA_NAM  = CUR_LINE ( IND(1,2):IND(2,2) )            ! Get station name
! ---------
            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1)) == 'EXP_CODE:' ) THEN
               ANC%EXP_CODE = CUR_LINE(IND(1,2):IND(2,2))               ! Get the experiment code
! ---------
            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1)) == 'UTC_MTAI:' ) THEN
               READ ( UNIT=CUR_LINE(IND(1,2):IND(2,2)), FMT='(F10.5)' ) &
     &                ANC%UTC_MTAI                                      ! UTC_MTAI
!
! --------- Parse the filler information
!           -99.9 for real, -999 for integers, and n/a for characters
!
            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1)) == 'FILLERS:' ) THEN
!
! ------------ If not enough fillers are described
!
               IF ( LIND < 4 ) THEN
                  CALL ERR_LOG ( 6834, IUER, 'ANC_PARSE',               &
     &                    'Too few words on FILLERS block line '//      &
     &                    TRIM(STR)//' of antenna calibration file '    &
     &                    //FILIN )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
! ------------
               READ ( UNIT=CUR_LINE(IND(1,2):IND(2,2)), FMT='(F10.5)' ) &
     &                ANC%FILLER_R8                                      ! -99.9
               READ ( UNIT=CUR_LINE(IND(1,3):IND(2,3)), FMT='(I8)'    )     &
     &                ANC%FILLER_I4                                      ! -999
               ANC%FILLER_CH = CUR_LINE(IND(1,4):IND(2,4)) ! n/a
!
! --------- How many PROVENACEs are there?
!
            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1)) ==                     &
     &               'NUM_PROVENANCE:'                  ) THEN
               READ ( UNIT=CUR_LINE(IND(1,2):IND(2,2)), FMT='(I8)' )    &
     &                ANC%NUM_PRV
!
! ------------ Clear all the contents of ANC%PRV
!
               DO 470 JP1 = 1, ANC__MVPV
                  CALL CLRCH ( ANC%PRV(JP1)%BUF )
 470           CONTINUE
!     
! -------- How many DATA_ONs are there?
!          N.B: This is akeen to the number of scans in a VLBI experiment
!
            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1))=='NUM_DATA_ON:' ) THEN
               READ ( UNIT=CUR_LINE(IND(1,2):IND(2,2)), FMT='(I8)' )    &
     &                ANC%NUM_DOO
!
! ------------ Allocate the number to ANC%DOO
!
               ALLOCATE ( ANC%DOO(ANC%NUM_DOO), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6835, IUER, 'ANC_PARSE',               &
     &                 'Error in allocating memory for ANC%DOO while'// &
     &                 ' processing antenna calibration file'//FILIN )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! --------- How many METEOs are there? 
!           N.B: It seems like typically NUM_METEO = NUM_DATA_ON
!
            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1)) == 'NUM_METEO:' ) THEN
               READ ( UNIT=CUR_LINE(IND(1,2):IND(2,2)), FMT='(I8)' )    &
     &                ANC%NUM_MET
!
! ------------ Allocate the number to ANC%MET
!
               ALLOCATE ( ANC%MET(ANC%NUM_MET), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6836, IUER, 'ANC_PARSE',               &
     &                 'Error in allocating memory for ANC%METEO while' &
     &               //' processing antenna calibration file '//FILIN )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! --------- How many CABLEs are there? 
!           N.B: It seems like typically NUM_METEO = NUM_DATA_ON = NUM_CABLE
!
            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1)) == 'NUM_CABLE:' ) THEN
               READ ( UNIT=CUR_LINE(IND(1,2):IND(2,2)), FMT='(I8)' )    &
     &                ANC%NUM_CBL
!
! ------------ Allocate the number to ANC%CBL
!
               ALLOCATE ( ANC%CBL(ANC%NUM_CBL), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6837, IUER, 'ANC_PARSE',                  &
     &                 'Error in allocating memory for ANC%CBL while'   &
     &               //' processing antenna calibration file '//FILIN )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! --------- How many Tsys Ports are there?
!
            ELSEIF (CUR_LINE(IND(1,1):IND(2,1))=='NUM_TP_SENSOR:') THEN
               READ ( UNIT=CUR_LINE(IND(1,2):IND(2,2)), FMT='(I8)' )    &
     &                ANC%NUM_TPS
!
! ------------ Allocate the number to TPS
!
               ALLOCATE ( ANC%TPS(ANC%NUM_TPS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6838, IUER, 'ANC_PARSE',               &
     &                 'Error in allocating memory for ANC%TPS while'   &
     &               //' processing antenna calibration file '//FILIN )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! --------- How many Tsys observations are there?
!
            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1)) == 'NUM_TSYS:' ) THEN
               READ ( UNIT=CUR_LINE(IND(1,2):IND(2,2)), FMT='(I8)' )    &
     &                ANC%NUM_TSYS
               ANC%NUM_TTO = ANC%NUM_TSYS
!
! ------------ Allocate number to ANC%TTO
!
               ALLOCATE ( ANC%TTO(ANC%NUM_TSYS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6839, IUER, 'ANC_PARSE',                  &
     &                 'Error in allocating memory for ANC%TTO while '  &
     &               //'processing antenna calibration file '//FILIN )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Allocate NUM_TPS in each time stamp's TSYS
!
               DO 420 J2=1,ANC%NUM_TSYS
                  ALLOCATE ( ANC%TTO(J2)%TSYS(ANC%NUM_TPS), STAT=IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6840, IUER, 'ANC_PARSE',            &
     &                    'Error in allocating memory for the '//       &
     &                    'ANC%TTO(j2)%TSYS while processing ' //       &
     &                    'antenna calibration file '//TRIM(FILIN) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
                  ANC%TTO(J2)%TSYS = ANC%FILLER_R8
 420           CONTINUE
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!
! --------- How many TPI are there?
!
            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1)) == 'NUM_TPI:' ) THEN
               READ ( UNIT=CUR_LINE(IND(1,2):IND(2,2)), FMT='(I8)' )    &
     &                ANC%NUM_TPI
!
! ------------ Allocate number to ANC%TPI
!
               ALLOCATE ( ANC%TPI(ANC%NUM_TPI), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6841, IUER, 'ANC_PARSE',               &
     &                 'Error in allocating memory for ANC%TPI while  ' &
     &               //'processing antenna calibration file '//FILIN )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Allocate NUM_TPS in each time stamp's TPI
!
               DO 460 J2=1,ANC%NUM_TPI
                  ALLOCATE ( ANC%TPI(J2)%TPION(ANC%NUM_TPS),  STAT=IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6842, IUER, 'ANC_PARSE',            &
     &                    'Error in allocating memory for the '//       &
     &                    'ANC%TPI(j2)%TPION while processing ' //      &
     &                    'antenna calibration file '//TRIM(FILIN) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
                  ANC%TPI(J2)%TPION  = ANC%FILLER_R8
!
! ---------------
!
                  ALLOCATE ( ANC%TPI(J2)%TPIOFF(ANC%NUM_TPS), STAT=IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6843, IUER, 'ANC_PARSE',            &
     &                    'Error in allocating memory for the '//       &
     &                    'ANC%TPI(j2)%TPIOFF while processing ' //     &
     &                    'antenna calibration file '//TRIM(FILIN) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
                  ANC%TPI(J2)%TPIOFF = ANC%FILLER_R8
 460           CONTINUE
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
! --------- How many PC Sensors are there?
!
            ELSEIF (CUR_LINE(IND(1,1):IND(2,1))=='NUM_PC_SENSOR:') THEN
               READ ( UNIT=CUR_LINE(IND(1,2):IND(2,2)), FMT='(I8)' )    &
     &                ANC%NUM_PCS
!
! --------- Allocate the number to PCS
!
               ALLOCATE ( ANC%PCS(ANC%NUM_PCS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6844, IUER, 'ANC_PARSE',                  &
     &                 'Error in allocating memory for ANC%PCS while '  &
     &               //'processing antenna calibration file '//FILIN )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! --------- How many Phase calibrations are there?
!
            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1)) == 'NUM_PCAL:' ) THEN
               READ ( UNIT=CUR_LINE(IND(1,2):IND(2,2)), FMT='(I8)' )        &
     &             ANC%NUM_PCAL
!
! ------------ Allocate number to ANC%PCAL
!
               ALLOCATE ( ANC%PCAL(ANC%NUM_PCAL), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6845, IUER, 'ANC_PARSE',                  &
     &                 'Error in allocating memory for ANC%PCAL while ' &
     &               //'processing antenna calibration file '//FILIN )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! --------- Allocate NUM_PCS in each time stamp's PCAL
!
               DO 430 J2=1,ANC%NUM_PCAL
                  ALLOCATE ( ANC%PCAL(J2)%PCAL_CMPL(ANC%NUM_PCS),          &
     &                    STAT=IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6846, IUER, 'ANC_PARSE',               &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%PCAL(j2)%PCAL_CMPL while processing '//  &
     &                    'antenna calibration file '//FILIN)
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
                  ANC%PCAL(J2)%PCAL_CMPL = CMPLX(ANC__FILLER_R4, 0.0)
 430           CONTINUE
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!##############################################################################################
!
! --------- How many GPS timers are there?
!
            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1)) ==                     &
     &               'NUM_FMT2GPS_TIMER:' ) THEN
               READ ( UNIT=CUR_LINE(IND(1,2):IND(2,2)), FMT='(I8)' )    &
     &                ANC%NUM_TGPS
!
! ------------ Allocate the number to TGPS
!
               ALLOCATE ( ANC%TGPS(ANC%NUM_TGPS), STAT=IER )
! ------------
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6847, IUER, 'ANC_PARSE',               &
     &                 'Error allocating memory for ANC%TGPS while '//  &
     &                 'processing antenna calibration file '//FILIN )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ How many GPS observations are there, per sensor?
!              i.e. TOTAL_OBS = NUM_FMT2GPS_TIMER * NUM_FMTGPS
!

            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1)) == 'NUM_FMTGPS:' ) THEN
               READ ( UNIT=CUR_LINE(IND(1,2):IND(2,2)), FMT='(I8)' )    &
     &                ANC%NUM_GPS
!
! ------------ Allocate number to ANC%GPS
!
               ALLOCATE ( ANC%GPS(ANC%NUM_GPS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6848, IUER, 'ANC_PARSE',                  &
     &                 'Error in allocating memory for ANC%GPS while'// &
     &                 ' processing antenna calibration file '//FILIN )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! --------- Allocate NUM_TGPS in each time stamp's GPS
!
               DO 450 J2=1,ANC%NUM_GPS
! ------------
                  ALLOCATE ( ANC%GPS(J2)%FMG(ANC%NUM_TGPS), STAT=IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6849, IUER, 'ANC_PARSE',            &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%GPS(j2)%FMG while processing antenna '// &
     &                    'calibration file '//FILIN)
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
                  ANC%GPS(J2)%FMG = ANC%FILLER_R8
! ------------
                  ALLOCATE ( ANC%GPS(J2)%FMP(ANC%NUM_TGPS), STAT=IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6850, IUER, 'ANC_PARSE',            &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%GPS(J2)%FMP while processing antenna '// &
     &                    'calibration file '//FILIN)
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
                  ANC%GPS(J2)%FMP = ANC%FILLER_R8
 450           CONTINUE
!##############################################################################################
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!     
! --------- How many SEFD's are there
!
            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1)) == 'NUM_SEFD:' ) THEN
               READ ( UNIT=CUR_LINE(IND(1,2):IND(2,2)), FMT='(I8)' )    &
     &             ANC%NUM_SEFD
!
! --------- Allocate number to ANC%SEFD
!
               ALLOCATE ( ANC%SEFD(ANC%NUM_SEFD), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6851, IUER, 'ANC_PARSE',               &
     &                 'Error in allocating memory for ANC%SEFD while ' &
     &               //'processing antcal file '//TRIM(FILIN) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Allocate NUM_TPS pointers in ANC%SEFD
!              in each time stamp's SEFD, TSYS, TCAL, 
!              TRAT, and GAIN
!
               DO 440 J2=1,ANC%NUM_SEFD
!
! --------------- SEFD Time stamps
!
                  ALLOCATE ( ANC%SEFD(J2)%SEFD(ANC%NUM_TPS), STAT=IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6852, IUER, 'ANC_PARSE',            &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%SEFD(j2)%SEFD while processing '//       &
     &                    'antcal file '//TRIM(FILIN) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
                  ANC%SEFD(J2)%SEFD = ANC%FILLER_R8
!
! --------------- TSYS Time stamps
!
                  ALLOCATE ( ANC%SEFD(J2)%TSYS(ANC%NUM_TPS), STAT=IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6853, IUER, 'ANC_PARSE',            &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%SEFD(j2)%TSYS while processing '//       &
     &                    'antcal file '// TRIM(FILIN) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
                  ANC%SEFD(J2)%TSYS = ANC%FILLER_R8
!
! --------------- TCAL Time stamps
!
                  ALLOCATE ( ANC%SEFD(J2)%TCAL(ANC%NUM_TPS), STAT=IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6854, IUER, 'ANC_PARSE',            &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%SEFD(j2)%TCAL while processing '//       &
     &                    'antcal file '// TRIM(FILIN) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
                  ANC%SEFD(J2)%TCAL = ANC%FILLER_R8
!
! --------------- TRAT Time stamps
!
                  ALLOCATE ( ANC%SEFD(J2)%TRAT(ANC%NUM_TPS), STAT=IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6855, IUER, 'ANC_PARSE',            &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%SEFD(j2)%TRAT while processing '//       &
     &                    'antcal file '//TRIM(FILIN) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
                  ANC%SEFD(J2)%TRAT = ANC%FILLER_R8
!
! --------------- GAIN Time stamps
!
                  ALLOCATE ( ANC%SEFD(J2)%GAIN(ANC%NUM_TPS), STAT=IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6856, IUER, 'ANC_PARSE',            &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%SEFD(j2)%GAIN while processing '//       &
     &                    'antcal file '//TRIM(FILIN) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
                  ANC%SEFD(J2)%GAIN = ANC%FILLER_R8
 440           CONTINUE
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! --------- Parse information from the "PROVENANCE" block
!
            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1)) == 'PROVENANCE:' ) THEN
!
! ------------ Did we reach the "NUM_PROVENANCE" before getting here?
!
               IF ( ANC%NUM_PRV < 1 ) THEN
                  IUER = -1
                  CALL ERR_LOG ( 6896, IUER, 'ANC_PARSE',               &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- PROVENANCE: preceeds '//       &
     &                 'NUM_PROVENANCE: ' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Get the last value of index of Provenance
!              if this is the first time we are hitting this line, it
!              will be -1
!     
               IND_PRV_PAS = IPRV
!     
! ------------ Get the index of the provenance number
!
               READ ( UNIT=CUR_LINE(IND(1,2):IND(2,2)),FMT='(I8)' ) IPRV
!
! ------------ Update index of provenance to the current value
!
               IND_PRV_CUR = IPRV
!
! ------------ If the previous and current value differ, it means we have 
!              entered a new provenance part, and we can reset the counter
!     
               IF ( IND_PRV_CUR .NE. IND_PRV_PAS ) IPRV_CNT = 0
               IPRV_CNT    = IPRV_CNT + 1
!
! ------------ Parse the information to ANC%PRV
!              N.B: - We parse everything including the text that says
!                     "PROVENANCE: 1"
!
               ANC%PRV(IPRV)%BUF(IPRV_CNT) =                            &
     &                       CUR_LINE( IND(1,1):IND(2,LIND) )
!
! --------- Parse information from the "DATA_ON" block
!
            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1)) == 'DATA_ON:' ) THEN
! ---------
               IF ( LIND < 6 ) THEN ! We expect at least 6 words in line
                  CALL ERR_LOG ( 6857, IUER, 'ANC_PARSE',               &
     &                 'Too few words on DATA_ON block line '//TRIM(STR)&
     &                 //' of the antenna calibration file '//FILIN )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ We reached "DATA_ON" before we actually got a count on 
!              "NUM_DATA_ON"
!
               IF ( .NOT. ASSOCIATED ( ANC%DOO ) ) THEN
                  CALL ERR_LOG ( 6858, IUER, 'ANC_PARSE',               &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- DATA_ON: preceeds NUM_DATA_ON')
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
! ------------
               CALL CHIN ( CUR_LINE(IND(1,2):IND(2,2)), IND_DOO )
! ------------
               CALL ERR_PASS ( IUER, IER )
               CALL DATE_TO_TIME(CUR_LINE(IND(1,3):IND(2,3)),MJD,UTC,IER)
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6859, IUER, 'ANC_PARSE',               &
     &                 'Error in parsing DATA_ON date '//               &
     &                 CUR_LINE(IND(1,3):IND(2,3))//' on line '//       &
     &                 TRIM(STR) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
! ------------
               IF ( MJD < MJD_MIN ) GOTO 410 ! Skip if MJD DATA_ON is not defined
!
! ------------ In case we haven't defined MJD_DOO yet
!
               IF ( ANC%MJD_DOO < 0 ) THEN
                  CALL ERR_PASS ( IUER, IER )
                  CALL NERS_GET_UTCMTAI (NERS,                          &
     &                     (MJD - J2000__MJD)*86400.D0 + UTC,           &
     &                     UTC_MTAI, IER )
                  IF ( IER .NE. 0 ) THEN
                     WRITE ( 6, * ) 'J1= ', J1
                     WRITE ( 6, * ) 'DATE= ',CUR_LINE(IND(1,3):IND(2,3))
                     WRITE ( 6, * ) 'MJD= ', MJD, 'UTC= ', UTC
                     CALL ERR_LOG ( 6860, IUER, 'ANC_PARSE',            &
     &                    'Error in getting UTC_MTAI on DATA_ON Block' )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- Define MJD_DOO, and TAI_DOO
!
                  ANC%MJD_DOO = MJD
                  ANC%TAI_DOO = UTC - UTC_MTAI
                  IF ( ANC%UTC_MTAI .LT. 0.D0 ) THEN
                     ANC%UTC_MTAI = UTC_MTAI
                  END IF
               END IF
! ------------
               TAI = UTC - UTC_MTAI
               ANC%DOO(IND_DOO)%TIM(1) = (MJD - ANC%MJD_DOO)*86400.D0 + &
     &                                   (TAI - ANC%TAI_DOO)             ! Initial TIme
! ---------
               CALL ERR_PASS ( IUER, IER )
               CALL DATE_TO_TIME(CUR_LINE(IND(1,4):IND(2,4)),MJD,UTC,IER)
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6861, IUER, 'ANC_PARSE',               &
     &                    'Error in parsing DATA_ON date '//            &
     &                    CUR_LINE(IND(1,3):IND(2,3))//' on line'//     &
     &                    TRIM(STR) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
! ------------
               ANC%DOO(IND_DOO)%TIM(2)  = (MJD - ANC%MJD_DOO)*86400.D0 &
     &                                     + (TAI - ANC%TAI_DOO)              ! Final Time
               ANC%DOO(IND_DOO)%SOU_NAM = CUR_LINE(IND(1,5):IND(2,5))
               ANC%DOO(IND_DOO)%SCA_NAM = CUR_LINE(IND(1,6):IND(2,6))
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! --------- Parse information from the "METEO" block
!
            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1)) == 'METEO:' ) THEN
!
! ------------ We expect at least 9 words per line in this block
!
               IF ( LIND < 9 ) THEN                                   
                  CALL ERR_LOG ( 6862, IUER, 'ANC_PARSE',                  &
     &                 'Too few words on METEO block line '//TRIM(STR)//&
     &                 ' of the antenna calibration file '//FILIN )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ We reached "METEO" before "NUM_METEO"
!
               IF ( .NOT. ASSOCIATED ( ANC%MET ) ) THEN
                  CALL ERR_LOG ( 6863, IUER, 'ANC_PARSE',                  &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- METEO: preceeds NUM_METEO' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
! ------------
               CALL CHIN ( CUR_LINE(IND(1,2):IND(2,2)), IND_MET )
!
! ------------ NUM_METEO was not counted correctly
!
               IF ( IND_MET > ANC%NUM_MET ) THEN
                  CALL ERR_LOG ( 6864, IUER, 'ANC_PARSE',                  &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- too many METEO lines.' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
! ------------
               CALL ERR_PASS ( IUER, IER )
               CALL DATE_TO_TIME(CUR_LINE(IND(1,3):IND(2,3)), MJD, UTC, IER)
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6865, IUER, 'ANC_PARSE',                  &
     &                 'Error in parsing METEO date '//                 &
     &                 CUR_LINE(IND(1,3):IND(2,3))//' on line '//        &
     &                 TRIM(STR) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
! ------------
               IF ( ANC%MJD_MET < 0 ) THEN
                  CALL ERR_PASS ( IUER, IER )
                  CALL NERS_GET_UTCMTAI (NERS,                              &
     &                                (MJD - J2000__MJD)*86400.D0 + UTC, &
     &                                UTC_MTAI, IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6866, IUER, 'ANC_PARSE',               &
     &                      'Error in getting UTC_MTAI on METEO Block' )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- Define MJD_MET and TAI_MET
!
                  ANC%MJD_MET = MJD
                  ANC%TAI_MET = UTC - UTC_MTAI
               END IF
               TAI = UTC - UTC_MTAI
               ANC%MET(IND_MET)%TIM = (MJD - ANC%MJD_MET)*86400.D0 +    &
     &                             (TAI - ANC%TAI_MET)
! ------------
               READ ( UNIT=CUR_LINE(IND(1,4):IND(2,4)), FMT='(F10.5)' ) &
     &                ANC%MET(IND_MET)%TEMP                              ! Temperature [K]
! ------------
               READ ( UNIT=CUR_LINE(IND(1,5):IND(2,5)), FMT='(F10.5)' ) &
     &                ANC%MET(IND_MET)%PRES                              ! Pressure [Pa]
! ------------
               READ ( UNIT=CUR_LINE(IND(1,6):IND(2,6)), FMT='(F10.5)' ) &
     &                ANC%MET(IND_MET)%HUMID                             ! Humidity [%]
! ------------
               READ ( UNIT=CUR_LINE(IND(1,7):IND(2,7)), FMT='(I8)' )    &
     &                ANC%MET(IND_MET)%IND_SCA                           ! Scan Index {link to }
! ------------
               ANC%MET(IND_MET)%SOU_NAM = CUR_LINE(IND(1,8):IND(2,8))
! ------------
               ANC%MET(IND_MET)%SCA_NAM = CUR_LINE(IND(1,9):IND(2,9))
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!     
! --------- Parse information from the "CABLE" block
!
            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1)) == 'CABLE:' ) THEN
!
! ------------ We expect at least 6 words per line in this block
!
               IF ( LIND < 6 ) THEN
                  CALL ERR_LOG ( 6867, IUER, 'ANC_PARSE',                  &
     &                 'Too few words on CABLE block line '//TRIM(STR)//&
     &                 ' of the antenna calibration file '//FILIN )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ We reached "CABLE" before "NUM_CABLE"
!
               IF ( .NOT. ASSOCIATED ( ANC%CBL ) ) THEN
                  CALL ERR_LOG ( 6868, IUER, 'ANC_PARSE',                  &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- CABLE: preceeds NUM_CABLE' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
! ------------
               CALL CHIN ( CUR_LINE(IND(1,2):IND(2,2)), IND_CBL )
!
! ------------ NUM_CABLE was not counted correctly
!
               IF ( IND_CBL > ANC%NUM_CBL ) THEN
                  CALL ERR_LOG ( 6869, IUER, 'ANC_PARSE',                  &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- too many CABLE lines.' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
! ------------
               CALL ERR_PASS ( IUER, IER )
               CALL DATE_TO_TIME(CUR_LINE(IND(1,3):IND(2,3)), MJD, UTC,IER)
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6870, IUER, 'ANC_PARSE',                  &
     &                 'Error in parsing CABLE date '//                 &
     &                 CUR_LINE(IND(1,3):IND(2,3))//' on line '//       &
     &                 TRIM(STR) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
! ------------
               IF ( ANC%MJD_CBL < 0 ) THEN
                  CALL ERR_PASS ( IUER, IER )
                  CALL NERS_GET_UTCMTAI (NERS,                              &
     &                                (MJD - J2000__MJD)*86400.D0 + UTC, &
     &                                UTC_MTAI, IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6871, IUER, 'ANC_PARSE',                &
     &                      'Error in getting UTC_MTAI on CABLE Block' )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- Define MJD_CBL and TAI_CBL
!
                  ANC%MJD_CBL = MJD
                  ANC%TAI_CBL = UTC - UTC_MTAI
               END IF
               TAI = UTC - UTC_MTAI
               ANC%CBL(IND_CBL)%TIM = (MJD - ANC%MJD_CBL)*86400.D0 +    &
     &                             (TAI - ANC%TAI_CBL)
! --------- 
!               READ ( UNIT=CUR_LINE(IND(1,4):IND(2,4)), FMT='(F20.12)' )     &
!     &             ANC%CBL(IND_CBL)%DELAY                                ! Cable Delay [s]
               READ ( UNIT=CUR_LINE(IND(1,4):IND(2,4)),FMT='(1PE12.5)') &
     &                ANC%CBL(IND_CBL)%DELAY                                ! Cable Delay [s]
! --------- 
               ANC%CBL(IND_CBL)%SOU_NAM = CUR_LINE(IND(1,5):IND(2,5))
! --------- 
               ANC%CBL(IND_CBL)%SCA_NAM = CUR_LINE(IND(1,6):IND(2,6))
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! --------- Parse information from the TP_SENSOR block
!
            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1)) == 'TP_SENSOR:' ) THEN
! ---------
               IF ( LIND < 10 ) THEN ! No less than 10 words in this block
                  CALL ERR_LOG ( 6872, IUER, 'ANC_PARSE',                  &
     &                 'Too few words on TP_SENSOR block line '//       &
     &                 TRIM(STR)//' of the antenna calibration file '// &
     &                 TRIM(FILIN) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Update TPS counter
!
               K_TPS = K_TPS + 1
!
! ------------ We exceeded the defined NUM_TPS
!
               IF ( K_TPS > ANC%NUM_TPS ) THEN
                  CALL ERR_LOG ( 6873, IUER, 'ANC_PARSE',                  &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- too many TP_SENSOR lines' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Sensor Tag
!
               ANC%TPS(K_TPS)%TAG = CUR_LINE(IND(1,2):IND(2,2))
               ANC%TPS_TAG(K_TPS) = CUR_LINE(IND(1,2):IND(2,2))
! ------------
               READ ( UNIT=CUR_LINE(IND(1,3):IND(2,3)), FMT='(F20.5)' ) &
     &                ANC%TPS(K_TPS)%IF_FRQ                             ! Intermediate Freq. [MHz]
               READ ( UNIT=CUR_LINE(IND(1,4):IND(2,4)), FMT='(F20.5)' ) &
     &                ANC%TPS(K_TPS)%LO_FRQ                             ! Local Oscilator Freq. [MHz]
               READ ( UNIT=CUR_LINE(IND(1,5):IND(2,5)), FMT='(F20.5)' ) &
     &                ANC%TPS(K_TPS)%SKY_FRQ                            ! Sky Freq. [MHz]
               READ ( UNIT=CUR_LINE(IND(1,6):IND(2,6)), FMT='(F20.5)' ) &
     &                ANC%TPS(K_TPS)%BDW                                ! Bandwidth [MHz]
!
! ------------ Convert MHz to Hz
!
               ANC%TPS(K_TPS)%IF_FRQ  = ANC%TPS(K_TPS)%IF_FRQ*1.D6      ! Intermediate Freq. [Hz]
               ANC%TPS(K_TPS)%LO_FRQ  = ANC%TPS(K_TPS)%LO_FRQ*1.D6      ! Local Oscilator Freq. [Hz]
               ANC%TPS(K_TPS)%SKY_FRQ = ANC%TPS(K_TPS)%SKY_FRQ*1.D6     ! Sky Freq. [Hz]
               ANC%TPS(K_TPS)%BDW     = ANC%TPS(K_TPS)%BDW*1.D6         ! Bandwidth [Hz]
!
! ------------ Polarization
!
               IF ( CUR_LINE(IND(1,7):IND(2,7)) == 'R' ) THEN
                  ANC%TPS(K_TPS)%POL = ANC__R_POL
               ELSEIF ( CUR_LINE(IND(1,7):IND(2,7)) == 'L' ) THEN
                  ANC%TPS(K_TPS)%POL = ANC__L_POL
               ELSEIF ( CUR_LINE(IND(1,7):IND(2,7)) == 'H' ) THEN
                  ANC%TPS(K_TPS)%POL = ANC__H_POL
               ELSEIF ( CUR_LINE(IND(1,7):IND(2,7)) == 'V' ) THEN
                  ANC%TPS(K_TPS)%POL = ANC__V_POL
               ELSEIF ( CUR_LINE(IND(1,7):IND(2,7)) == 'X' ) THEN
                  ANC%TPS(K_TPS)%POL = ANC__X_POL
               ELSEIF ( CUR_LINE(IND(1,7):IND(2,7)) == 'Y' ) THEN
                  ANC%TPS(K_TPS)%POL = ANC__Y_POL
               ELSE 
                  CALL ERR_LOG ( 6874, IUER, 'ANC_PARSE',               &
     &                 CUR_LINE(IND(1,7):IND(2,7))//' in line '//       &
     &                 TRIM(STR)//' is not a supported polarization '// &
     &                 'code. We expected R, L, H, V, X, or Y. See '//  &
     &                 'the antenna calibration file '//TRIM(FILIN) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ TP_SENSOR ID
!
               ANC%TPS(K_TPS)%ID = CUR_LINE(IND(1,8):IND(2,8))
!
! ------------ Intermediate Frequency Index
!
               READ ( UNIT=CUR_LINE(IND(1,9):IND(2,9)), FMT='(I8)' )    &
     &             ANC%TPS(K_TPS)%IF_IND                                
!
! ------------ Sensor Sideband Code
!
               IF ( CUR_LINE(IND(1,10):IND(2,10)) == 'USB' ) THEN
                  ANC%TPS(K_TPS)%SSB = ANC__USB
! ------------
               ELSEIF ( CUR_LINE(IND(1,10):IND(2,10)) == 'LSB' ) THEN
                  ANC%TPS(K_TPS)%SSB = ANC__LSB
! ------------ 
               ELSEIF ( CUR_LINE(IND(1,10):IND(2,10)) == 'n/a' ) THEN
                  ANC%TPS(K_TPS)%SSB = ANC__NA
! ------------
               ELSEIF ( CUR_LINE(IND(1,10):IND(2,10)) == 'DUMMY' ) THEN
                  ANC%TPS(K_TPS)%SSB = ANC__NA
! ------------
               ELSE
                  CALL ERR_LOG ( 6875, IUER, 'ANC_PARSE',               &
     &                 CUR_LINE(IND(1,10):IND(2,10))//' in line '//     &
     &                 TRIM(STR)//' is not supported Sensor sideband'// &
     &                 ' code. We expected USB, LSB, n/a, or DUMMY. '// &
     &                 'See the antenna calibration file '//TRIM(FILIN))
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Net Sideband Code
!
               IF ( CUR_LINE(IND(1,11):IND(2,11)) == 'USB' ) THEN
                  ANC%TPS(K_TPS)%NSB = ANC__USB
! ------------
               ELSEIF ( CUR_LINE(IND(1,11):IND(2,11)) == 'LSB' ) THEN
                  ANC%TPS(K_TPS)%NSB = ANC__LSB
! ------------ 
               ELSEIF ( CUR_LINE(IND(1,11):IND(2,11)) == 'n/a' ) THEN
                  ANC%TPS(K_TPS)%NSB = ANC__NA
! ------------
               ELSEIF ( CUR_LINE(IND(1,11):IND(2,11)) == 'DUMMY' ) THEN
                  ANC%TPS(K_TPS)%NSB = ANC__NA
! ------------
               ELSE
                  CALL ERR_LOG ( 6875, IUER, 'ANC_PARSE',               &
     &                 CUR_LINE(IND(1,11):IND(2,11))//' in line '//     &
     &                 TRIM(STR)//' is not supported Net sideband'// &
     &                 ' code. We expected USB, LSB, n/a, or DUMMY. '// &
     &                 'See the antenna calibration file '//TRIM(FILIN))
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! --------  Parse info from the SEFD block
!
            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1)) == 'SEFD:' ) THEN
! ------------
               IF ( LIND < 11 ) THEN
                  CALL ERR_LOG ( 6876, IUER, 'ANC_PARSE',               &
     &                 'Too few words on SEFD block line '//TRIM(STR)// &
     &                 ' of the antcal file '//TRIM(FILIN) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
! ------------
               CALL ERR_PASS ( IUER, IER )
               CALL DATE_TO_TIME( CUR_LINE(IND(1,3):IND(2,3)), MJD,     &
     &                            UTC, IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6877, IUER, 'ANC_PARSE',               &
     &                 'Error in parsing SEFD date '//                  &
     &                 CUR_LINE(IND(1,3):IND(2,3))//' in line '//       &
     &                 TRIM(STR) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
! ------------
               IF ( ANC%MJD_SEFD < 0 ) THEN
                  CALL ERR_PASS ( IUER, IER )
                  CALL NERS_GET_UTCMTAI (NERS,                          &
     &                      (MJD - J2000__MJD)*86400.D0 + UTC,          &
     &                      UTC_MTAI, IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6878, IUER, 'ANC_PARSE',            &
     &                    'Error in getting UTC_MTAI on SEFD Block' )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- MJD_SEFD and TAI_SEFD
!
                  ANC%MJD_SEFD = MJD
                  ANC%TAI_SEFD = UTC - UTC_MTAI
               END IF
               TAI = UTC - UTC_MTAI
!
! ------------ Update the SEFD counter, given the time diff. error is met
!
               IF ( K_SEFD == 0 ) THEN
                  K_SEFD = 1
               ELSE
                  IF ( DABS((MJD - ANC%MJD_SEFD)*86400.D0 +              &
     &                      (TAI - ANC%TAI_SEFD) - ANC%SEFD(K_SEFD)%TIM) &
     &                 > 3*ANC__EPS_TIM ) THEN
                     K_SEFD = K_SEFD + 1
                  END IF
               END IF
!
! ------------ Error in logging NUM_SEFD
!
               IF ( K_SEFD > ANC%NUM_SEFD ) THEN
                  CALL IINCH ( K_SEFD, STR1 )
                  CALL IINCH ( ANC%NUM_SEFD, STR2 )
                  CALL ERR_LOG ( 6879, IUER, 'ANC_PARSE',               &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- too many SEFD lines. '//       &
     &                 'Got '//TRIM(STR1)//' ,expected '//TRIM(STR2) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Compute time
!
               ANC%SEFD(K_SEFD)%TIM = (MJD - ANC%MJD_SEFD)*86400.D0 +   &
     &                                (TAI - ANC%TAI_SEFD)
!
! ------------ Is the sensor tag in TSYS block available in list of tags
!              from TPS_SENSOR block
!
               IND_TPS = LTM_DIF ( 0, ANC%NUM_TPS, ANC%TPS_TAG,         &
     &                             CUR_LINE(IND(1,2):IND(2,2))   )
               IF ( IND_TPS < 1 ) THEN
                  CALL ERR_LOG ( 6880, IUER, 'ANC_PARSE',               &
     &                 'TP_SENSOR tag '//CUR_LINE(IND(1,2):IND(2,2))//  &
     &                 ' in SEFD block line '//TRIM(STR)//' not found' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Parse the SEFD value to the index of SEFD in ANC%SEFD that 
!              coincides with the index of the sensor tag in TP_SENSOR 
!              Block. Units = Jy
!
               READ ( UNIT=CUR_LINE(IND(1,4):IND(2,4)), FMT='(F20.12)' ) &
     &                ANC%SEFD(K_SEFD)%SEFD(IND_TPS)
!
! ------------ Parse the Tsys value to the index of TSYS in ANC%SEFD that 
!              coincides with the index of the sensor tag in TP_SENSOR 
!              Block. Units = K
!
               READ ( UNIT=CUR_LINE(IND(1,5):IND(2,5)), FMT='(F20.12)' ) &
     &                ANC%SEFD(K_SEFD)%TSYS(IND_TPS)
!
! ------------ Parse the Tcal value to the index of TCAL in ANC%SEFD that 
!              coincides with the index of the sensor tag in TP_SENSOR 
!              Block. Units = Jy
!
               READ ( UNIT=CUR_LINE(IND(1,6):IND(2,6)), FMT='(F20.12)' ) &
     &                ANC%SEFD(K_SEFD)%TCAL(IND_TPS)
!
! ------------ Parse the Trat value to the index of TRAT in ANC%SEFD that 
!              coincides with the index of the sensor tag in TP_SENSOR 
!              Block. Units = []
!
               READ ( UNIT=CUR_LINE(IND(1,7):IND(2,7)), FMT='(F20.12)' ) &
     &                ANC%SEFD(K_SEFD)%TRAT(IND_TPS)
!
! ------------ Parse the Gain value to the index of GAIN in ANC%SEFD that 
!              coincides with the index of the sensor tag in TP_SENSOR 
!              Block. Units = 
!
               READ ( UNIT=CUR_LINE(IND(1,8):IND(2,8)), FMT='(F20.12)' ) &
     &                ANC%SEFD(K_SEFD)%GAIN(IND_TPS)
! ------------
               READ ( UNIT=CUR_LINE(IND(1,9):IND(2,9)), FMT='(F20.12)' ) &
     &                ANC%SEFD(K_SEFD)%AZ
               ANC%SEFD(K_SEFD)%AZ = ANC%SEFD(K_SEFD)%AZ*DEG__TO__RAD    ! Azimuth [rad]
! ------------
               READ ( UNIT=CUR_LINE(IND(1,10):IND(2,10)),FMT='(F20.12)') &
     &                ANC%SEFD(K_SEFD)%EL                            
               ANC%SEFD(K_SEFD)%EL = ANC%SEFD(K_SEFD)%EL*DEG__TO__RAD    ! Elevation [rad]
! ------------
               ANC%SEFD(K_SEFD)%SOU_NAM = CUR_LINE(IND(1,11):IND(2,11))  ! Source Name
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!###################################################
!
! --------- Parse information from the PC_SENSOR
!
            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1)) == 'PC_SENSOR:' ) THEN
!
! ------------- Parse information from the PC_SENSOR block
!               No less than 7 words in this block
!
               IF ( LIND < 7 ) THEN                          
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6881, IUER, 'ANC_PARSE',               &
     &                 'Too few words on PC_SENSOR line block '//       &
     &                 TRIM(STR)//' of the antenna calibration file '// &
     &                 TRIM(FILIN) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Update PCS counter
!
               K_PCS = K_PCS + 1
!
! ------------ We exceeded the defined NUM_PCS
!
               IF ( K_PCS > ANC%NUM_PCS ) THEN
                  CALL IINCH ( K_PCS, STR1 )
                  CALL IINCH ( ANC%NUM_PCS, STR2 )
                  CALL ERR_LOG ( 6882, IUER, 'ANC_PARSE',               &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- too many PC_SENSORS lines. '// &
     &                 'Got '//TRIM(STR1)//' ,expected '//TRIM(STR2) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Sensor Tag
!
               ANC%PCS(K_PCS)%TAG = CUR_LINE(IND(1,2):IND(2,2))
               ANC%PCS_TAG(K_PCS) = CUR_LINE(IND(1,2):IND(2,2))
! ------------
               READ ( UNIT=CUR_LINE(IND(1,3):IND(2,3)), FMT='(F20.5)' )  &
     &                ANC%PCS(K_PCS)%SKY_FRQ                             ! Sky Frequency [MHz]
               ANC%PCS(K_PCS)%SKY_FRQ = ANC%PCS(K_PCS)%SKY_FRQ*1.D6      ! Sky Frequency [Hz]
!
! --------- Polarization
!
               IF ( CUR_LINE(IND(1,4):IND(2,4)) == 'R' ) THEN
                  ANC%PCS(K_PCS)%POL = ANC__R_POL
               ELSEIF ( CUR_LINE(IND(1,4):IND(2,4)) == 'L' ) THEN
                  ANC%PCS(K_PCS)%POL = ANC__L_POL
               ELSEIF ( CUR_LINE(IND(1,4):IND(2,4)) == 'H' ) THEN
                  ANC%PCS(K_PCS)%POL = ANC__H_POL
               ELSEIF ( CUR_LINE(IND(1,4):IND(2,4)) == 'V' ) THEN
                  ANC%PCS(K_PCS)%POL = ANC__V_POL
               ELSEIF ( CUR_LINE(IND(1,4):IND(2,4)) == 'X' ) THEN
                  ANC%PCS(K_PCS)%POL = ANC__X_POL
               ELSEIF ( CUR_LINE(IND(1,4):IND(2,4)) == 'Y' ) THEN
                  ANC%PCS(K_PCS)%POL = ANC__Y_POL
               ELSE 
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6883, IUER, 'ANC_PARSE',                  &
     &                 CUR_LINE(IND(1,4):IND(2,4))//' in line '//        &
     &                 TRIM(STR)//' is not a supported polarization '// &
     &                 'code. We expected R, L, H, V, X, or Y. See '//  &
     &                 'the antenna calibration file '//TRIM(FILIN) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ PC_SENSOR ID
!
               ANC%PCS(K_PCS)%ID = CUR_LINE(IND(1,5):IND(2,5)) 
! ---------
               READ ( UNIT=CUR_LINE(IND(1,6):IND(2,6)), FMT='(I8)' )        &
     &             ANC%PCS(K_PCS)%IF_IND                                ! Intermediate Frequency Index
!
! --------- Subband Code
!
               IF ( CUR_LINE(IND(1,7):IND(2,7)) == 'USB' ) THEN
                  ANC%PCS(K_PCS)%NSB = ANC__USB
               ELSEIF ( CUR_LINE(IND(1,7):IND(2,7)) == 'LSB' ) THEN
                  ANC%PCS(K_PCS)%NSB = ANC__LSB
               ELSEIF ( CUR_LINE(IND(1,7):IND(2,7)) == 'n/a' ) THEN
                  ANC%PCS(K_PCS)%NSB = ANC__NA
               ELSEIF ( CUR_LINE(IND(1,7):IND(2,7)) == 'DUMMY' ) THEN
                  ANC%PCS(K_PCS)%NSB = ANC__NA
               ELSE
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6884, IUER, 'ANC_PARSE',                  &
     &                 CUR_LINE(IND(1,7):IND(2,7))//' in line '//        &
     &                 TRIM(STR)//' is not a supported subband code.'// &
     &                 ' We expected USB, LSB, n/a, or DUMMY. See '//   &
     &                 'the antenna calibration file '//TRIM(FILIN) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! --------- Parse information from the FMT2GPS Block
!
            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1)) == 'FMT2GPS_TIMER:' ) THEN
!
! -----------  No less than 3 words in this block
!
               IF ( LIND < 3 ) THEN 
                  CALL ERR_LOG ( 6885, IUER, 'ANC_PARSE',               &
     &                 'Too few words in FMT2GPS_TIMER line '//TRIM(STR)&
     &                 //' of the antcal file '//TRIM(FILIN) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Update GPS counter
!
               K_TGPS = K_TGPS + 1
!
! ------------ We exceeded the defined NUM_TGPS
!
               IF ( K_TGPS > ANC%NUM_TGPS ) THEN
                  CALL IINCH ( K_TGPS, STR1 )
                  CALL IINCH ( ANC%NUM_TGPS, STR2 )
                  CALL ERR_LOG ( 6886, IUER, 'ANC_PARSE',                  &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- too many FMT2GPS_TIMER lines.' &
     &                //' Got '//TRIM(STR1)//' ,expected '//TRIM(STR2) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Timer Tag
!
               ANC%TGPS(K_TGPS)%TAG = CUR_LINE(IND(1,2):IND(2,2))
               ANC%TGPS_TAG(K_TGPS) = CUR_LINE(IND(1,2):IND(2,2))
!
! ------------ Board name
!
               ANC%TGPS(K_TGPS)%BOARD = CUR_LINE(IND(1,3):IND(2,3))
!
! --------- Parse information from the FMTGPS Block
!
            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1)) == 'FMTGPS:' ) THEN
! ------------
               IF ( LIND < 7 ) THEN
                  CALL ERR_LOG ( 6887, IUER, 'ANC_PARSE',               &
     &                 'Too few words in FMTGPS block line '//TRIM(STR) &
     &                 //' of the antcal file '//TRIM(FILIN) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
! ------------
               CALL ERR_PASS ( IUER, IER )
               CALL DATE_TO_TIME( CUR_LINE(IND(1,3):IND(2,3)), MJD,     &
     &                            UTC, IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6888, IUER, 'ANC_PARSE',               &
     &                 'Error in parsing FMTGPS date '//                &
     &                 CUR_LINE(IND(1,3):IND(2,3))//' in line '//       &
     &                 TRIM(STR) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
! ------------
               IF ( ANC%MJD_GPS < 0 ) THEN
                  CALL ERR_PASS ( IUER, IER )
                  CALL NERS_GET_UTCMTAI (NERS,                          &
     &                     (MJD - J2000__MJD)*86400.D0 + UTC,           &
     &                     UTC_MTAI, IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6889, IUER, 'ANC_PARSE',            &
     &                    'Error in getting UTC_MTAI on FMTGPS Block' )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- MJD_GPS and TAI_GPS
!
                  ANC%MJD_GPS = MJD
                  ANC%TAI_GPS = UTC - UTC_MTAI
               END IF
               TAI = UTC - UTC_MTAI
!
! ------------ Update the GPS counter, given the time diff. error is met
!
               IF ( K_GPS == 0 ) THEN
                  K_GPS = 1
               ELSE
                  IF ( DABS((MJD - ANC%MJD_GPS)*86400.D0 +              &
     &                      (TAI - ANC%TAI_GPS) - ANC%GPS(K_GPS)%TIM)   &
     &                  > 3*ANC__EPS_TIM ) THEN
                     K_GPS = K_GPS + 1
                  END IF
               END IF
!               
! ------------ Error in logging NUM_GPS
!
               IF ( K_GPS > ANC%NUM_GPS ) THEN
                  CALL IINCH ( K_GPS, STR1 )
                  CALL IINCH ( ANC%NUM_PCS, STR2 )
                  CALL ERR_LOG ( 6890, IUER, 'ANC_PARSE',               &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- too many FMTGPS lines. '//     &
     &                 'Got '//TRIM(STR1)//' ,expected '//TRIM(STR2) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Compute time
!
               ANC%GPS(K_GPS)%TIM = (MJD - ANC%MJD_GPS)*86400.D0 +      &
     &                           (TAI - ANC%TAI_GPS)
! ------------
               READ ( UNIT=CUR_LINE(IND(1,2):IND(2,2)), FMT='(I8)' )    &
     &             ANC%GPS(K_GPS)%IND_SCA
!
! --------- Is the Timer tag in FMTGPS block available in list of tags
!           from FMT2GPS_TIMER block
!
               IND_TGPS = LTM_DIF ( 0, ANC%NUM_GPS, ANC%TGPS_TAG,       &
     &                              CUR_LINE(IND(1,4):IND(2,4))   )
               IF ( IND_TGPS < 1 ) THEN
                  CALL ERR_LOG ( 6891, IUER, 'ANC_PARSE',               &
     &                 'Error in parsing FMT2GPS_TIMER tag '//          &
     &                 CUR_LINE(IND(1,4):IND(2,4))//' in FMTGPS line '//&
     &                 TRIM(STR) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Parse the Formatter minus GPS Time value to the index of
!              FMG in ANC%GPS that coincides with the index of the timer
!              tag in the FMT2GPS Block. Units = s
!
               READ ( UNIT=CUR_LINE(IND(1,5):IND(2,5)), FMT='(F20.12)' ) &
     &                ANC%GPS(K_GPS)%FMG(IND_TGPS)
!     
! ------------ In an updated version of Log2ant, we include the PPS 
! ------------ Therefore depending on the version of log2ant one uses, we 
!              could have:
!              1        2          3           4               5                       6                7      8
!              FMTGPS: Scan_Idx UTC_Time_tag Timer_tag Formatter_minus_GPS_time Formatter_minus_PPS_time Source Scan
!              FMTGPS: Scan_Idx UTC_Time_tag Timer_tag Formatter_minus_GPS_time Source                   Scan
!
               IF ( LIND == 7 ) THEN
!
! ------------ Source Name
!
                  ANC%GPS(K_GPS)%SOU_NAM = CUR_LINE(IND(1,6):IND(2,6))
!
! ------------ Scan name
!
                  ANC%GPS(K_GPS)%SCA_NAM = CUR_LINE(IND(1,7):IND(2,7))
               ELSEIF ( LIND == 8 ) THEN
!
! ------------ Parse the Formatter minus GPS Time value to the index 
!              of FMG in ANC%GPS that coincides with the index of 
!              the timer tag in the FMT2GPS Block. Units = s
!
                  READ (UNIT=CUR_LINE(IND(1,6):IND(2,6)),FMT='(F20.12)') &
     &                  ANC%GPS(K_GPS)%FMP(IND_TGPS)
!     
! ------------ Source Name
!
                  ANC%GPS(K_GPS)%SOU_NAM = CUR_LINE(IND(1,7):IND(2,7))
!
! ------------ Scan name
!
                  ANC%GPS(K_GPS)%SCA_NAM = CUR_LINE(IND(1,8):IND(2,8))
               END IF
            END IF
!**************************************************************************
! *************************************************************************
! * ------      PARSE THE VERSION 2 OF ANC FILES (2025.08.22)      ------ *
! *************************************************************************
!**************************************************************************
         ELSEIF ( IANC_FMT_VERS == 2 ) THEN
!     
! --------- Get the station name
!
            IF ( CUR_LINE(1:8) == 'STATION:' ) THEN
!
! ------------ Get the length of the line, and station name
!
               IL = I_LEN(CUR_LINE)            
               ANC%STA_NAM = TRIM( CUR_LINE(10:IL) )
!
! --------- Get the Experiment code
!
            ELSEIF ( CUR_LINE(1:9)) == 'EXP_CODE:' ) THEN
!
! ------------ Get the length of the line, and Experiment code
!
               IL = I_LEN(CUR_LINE)
               ANC%EXP_CODE = TRIM( CUR_LINE(11:IL) )
!
! --------- Get the UTC_MTAI given in the file
!
            ELSEIF ( CUR_LINE(1:9)) == 'UTC_MTAI:' ) THEN
!
! ------------ Get the length of the line, and station name
!
               IL = I_LEN(CUR_LINE)
               READ ( UNIT=TRIM(CUR_LINE(11:IL)), FMT = '(F10.5)' )      &
     &                ANC%UTC_MTAI
!
! --------- Parse the filler information (includes date)
!           -99.9 for real, -999 for integers, n/a or DUMMY for string,
!           and 1957.10.04-00:00:00 for date
!
            ELSEIF ( CUR_LINE(1:7)) == 'FILLERS:' ) THEN
!
! ------------ This section is short enough that we can effectively 
!              break it into individual words without losing 
!              processing speed (in any noticeable manner).
!
               CALL EXWORD ( CUR_LINE, MIND, LIND, IND, DELIM, IER )
!
               IF ( LIND < 5 ) THEN
!
! --------------- Store line number to STR
!
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6834, IUER, 'ANC_PARSE',               &
     &                    'Too few words on FILLERS block line '//      &
     &                    TRIM(STR)//' of antenna calibration file '    &
     &                    //FILIN )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
! ------------ 
               READ ( UNIT=CUR_LINE(IND(1,2):IND(2,2)), FMT='(F10.5)' ) &
     &                ANC%FILLER_R8                                      ! -99.9
               READ ( UNIT=CUR_LINE(IND(1,3):IND(2,3)), FMT='(I8)'    )     &
     &                ANC%FILLER_I4                                      ! -999
               ANC%FILLER_CH   = CUR_LINE(IND(1,4):IND(2,4))             ! DUMMY or n/a
!
! ------------ Filler Date =  1957.10.04-00:00:00
!
               ANC%FILLER_DATE = CUR_LINE(IND(1,5):IND(2,5))
! ------------
               CALL ERR_PASS ( IUER, IER )
               CALL DATE_TO_TIME ( ANC%FILLER_DATE, MJD, UTC, IER )
               IF ( IER .NE. 0 ) THEN
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6897, IUER, 'ANC_PARSE',               &
     &                    'Error in parsing DATA_ON date '//            &
     &                    CUR_LINE(IND(1,5):IND(2,5))//' on line '//    &
     &                    TRIM(STR) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
               IF ( ANC%MJD_FILLER < 0 ) THEN
                  CALL ERR_PASS ( IUER, IER )
                  CALL NERS_GET_UTCMTAI (NERS,                          &
     &                     (MJD - J2000__MJD)*86400.D0 + UTC,           &
     &                     UTC_MTAI, IER )
                  IF ( IER .NE. 0 ) THEN
                     WRITE ( 6, * ) 'J1= ', J1
                     WRITE ( 6, * ) 'DATE= ',CUR_LINE(IND(1,5):IND(2,5))
                     WRITE ( 6, * ) 'MJD= ', MJD, 'UTC= ', UTC
                     CALL ERR_LOG ( 6898, IUER, 'ANC_PARSE',            &
     &                    'Error in getting UTC_MTAI on DATA_ON Block' )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
               END IF
!
! ------------ Define ANC%MJD_FILLER, ANC%TAI_FILLER, and ANC%UTC_MTAI
!              This date might be too far out, and not a reliable source
!              for UTC_MTAI for when the experiment ran.
!
               ANC%MJD_FILLER = MJD
               ANC%TAI_FILLER = UTC - UTC_MTAI
!
! --------- Count the number of Provenances
!
            ELSEIF ( CUR_LINE(1:12) == 'NUM_PROVENANCE:' ) THEN
!
! ------------ Get the length of the line, and number of provenance
!
               IL = I_LEN(CUR_LINE)
               READ (UNIT=TRIM(CUR_LINE(17:IL)),FMT='(I8)') ANC%NUM_PRV
!
! --------- When we enter the part of the file that has 'DIMENSION'
!           it is time to check where the dimension belongs to
!     

            ELSEIF ( CUR_LINE(1:12) == 'DIMENSION:' ) THEN
!
! ------------ Extract the words and check the name of the block
!
               CALL EXWORD ( CUR_LINE, MIND, LIND, IND, DELIM, IER )
!     
! ------------ How many DATA_ONs are there?
!              N.B: This is akeen to the number of scans in a VLBI
!                   experiment
!     
               IF ( CUR_LINE(IND(1,2):IND(2,2)) == 'DATA_ON' ) THEN
! --------------
                  READ ( UNIT=CUR_LINE(IND(1,3):IND(2,3)), FMT='(I8)')  &
     &                   ANC%NUM_DOO
!
! --------------- Allocate the number to ANC%DOO
!
                  ALLOCATE ( ANC%DOO(ANC%NUM_DOO), STAT=IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6899, IUER, 'ANC_PARSE',            &
     &                       'Error allocating memory for ANC%DOO '//   &
     &                       'while processing antenna calibration '//  &
     &                       'file'//FILIN )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
                  GOTO 410
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     
! ------------ How many METEOs are there? 
!              N.B: It seems like typically NUM_METEO = NUM_DATA_ON
!
               ELSEIF ( CUR_LINE(IND(1,2):IND(2,2)) == 'METEO' ) THEN
!
                  READ ( UNIT=CUR_LINE(IND(1,3):IND(2,3)),FMT='(I8)')   &
     &                   ANC%NUM_MET
!
! --------------- Allocate the number to ANC%MET
!
                  ALLOCATE ( ANC%MET(ANC%NUM_MET), STAT=IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6900, IUER, 'ANC_PARSE',            &
     &                       'Error allocating memory for ANC%METEO '// &
     &                       'while processing antenna calibration '//  &
     &                       'file '//FILIN )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
                  GOTO 410
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------------ How many CABLEs are there? 
!              N.B: It seems like typically NUM_METEO = NUM_DATA_ON = NUM_CABLE
!
               ELSEIF ( CUR_LINE(IND(1,2):IND(2,2)) == 'CABLE' ) THEN
!
                  READ ( UNIT=CUR_LINE(IND(1,3):IND(2,3)), FMT='(I8)' ) &
     &                   ANC%NUM_CBL
!
! --------------- Allocate the number to ANC%CBL
!
                  ALLOCATE ( ANC%CBL(ANC%NUM_CBL), STAT=IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6901, IUER, 'ANC_PARSE',            &
     &                       'Error allocating memory for ANC%CBL '//   &
     &                       'while processing antenna calibration '//  &
     &                       'file '//FILIN )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
                  GOTO 410
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!     
! ------------ How many Tsys Ports are there? 
!
               ELSEIF ( CUR_LINE(IND(1,2):IND(2,2))=='TP_SENSOR' ) THEN
!
                  READ ( UNIT=CUR_LINE(IND(1,3):IND(2,3)), FMT='(I8)')  &
     &                   ANC%NUM_TPS
!
! --------------- Allocate the number to TPS
!
                  ALLOCATE ( ANC%TPS(ANC%NUM_TPS), STAT=IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6902, IUER, 'ANC_PARSE',            &
     &                       'Error allocating memory for ANC%TPS '//   &
     &                       'while  processing antenna calibration '// &
     &                       'file '//FILIN )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
                  GOTO 410
!==========================================================================================
!
! ------------ What are dimension of TTO and how many records are there
!
               ELSEIF ( CUR_LINE(IND(1,2):IND(2,2)) == 'TSYS' ) THEN
!
! --------------- This time we have 4 values
!                 The third value corresponds to num of epochs in Tsys,
!                 We assume that the same value will be found at TPI,
!                 PCAL, and FMTGPS blocks.
!                 N.B: It is assumed Tsys will always be reached before
!                      those other blocks, but some backends don't have
!                      TSYS recordings, hence we parse this value on all
!                      all the aforementioned blocks. Even if it's the
!                      same value
!                 The 4th value corresponds to the num of sensors, no
!                 need to  parse since we already got it at TP_SENSOR
!     
                  READ ( UNIT=CUR_LINE(IND(1,3):IND(2,3)), FMT='(I8)' ) &
     &                   ANC%NUM_EPO
!
! --------------- The next line will have the number of records of TTO
!                 "NUM_RECORDS: TSYS XXXXX"
!                 N.B: NUM_RECORDS .LE. NUM_TPS*NUM_EPO
!
                  IL = I_LEN(BUF(J1+1))
                  READ( UNIT=BUF(J1+1)(19:IL), FMT='(I8)' ) ANC%NUM_TSYS
                  ANC%NUM_TSYS = ANC%NUM_TTO
!
! --------------- Allocate number of epochs to ANC%TTO
!
                  ALLOCATE ( ANC%TTO(ANC%NUM_EPO), STAT=IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6903, IUER, 'ANC_PARSE',            &
     &                 'Error in allocating memory for ANC%TTO while '  &
     &               //'processing antenna calibration file '//FILIN )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- Allocate NUM_TPS in each time stamp's TSYS
!
                  DO 520 J2=1,ANC%NUM_EPO
                     ALLOCATE (ANC%TTO(J2)%TSYS(ANC%NUM_TPS), STAT=IER)
                     IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6904, IUER, 'ANC_PARSE',         &
     &                    'Error in allocating memory for the '//       &
     &                    'ANC%TTO(j2)%TSYS while processing ' //       &
     &                    'antenna calibration file '//TRIM(FILIN) )
                        DEALLOCATE ( BUF )
                        RETURN
                     END IF
                     ANC%TTO(J2)%TSYS = ANC%FILLER_R8
 520              CONTINUE
                  GOTO 410
!==========================================================================================
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!
! ------------ What are dimension of TPI and how many records are there
!              Same rules and notes as for TSYS dimensions above.
!     
               ELSEIF ( CUR_LINE(IND(1,2):IND(2,2)) == 'TPI' ) THEN
! ---------------
                  READ ( UNIT=CUR_LINE(IND(1,3):IND(2,3)), FMT='(I8)' ) &
     &                   ANC%NUM_EPO
!
! --------------- The next line will have the number of records of TPI
!                 "NUM_RECORDS: TPI XXXXX"
!                 N.B: NUM_RECORDS .LE. NUM_TPS*NUM_EPO
!
                  IL = I_LEN(BUF(J1+1))
                  READ( UNIT=BUF(J1+1)(18:IL), FMT='(I8)' ) ANC%NUM_TPI
!
! --------------- Allocate number to ANC%TPI
!
                  ALLOCATE ( ANC%TPI(ANC%NUM_EPO), STAT=IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6905, IUER, 'ANC_PARSE',            &
     &                       'Error allocating memory for ANC%TPI '//   &
     &                       'while processing antenna calibration '//  &
     &                       'file '//FILIN )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- Allocate NUM_TPS in each time stamp's TPI
!
                  DO 560 J2=1,ANC%NUM_EPO
                     ALLOCATE ( ANC%TPI(J2)%TPION(ANC%NUM_TPS),         &
     &                          STAT=IER )
                     IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6906, IUER, 'ANC_PARSE',         &
     &                          'Error in allocating memory for the '// &
     &                          'ANC%TPI(j2)%TPION while processing '// &
     &                          'antenna calibration file '//FILIN )
                        DEALLOCATE ( BUF )
                        RETURN
                     END IF
                     ANC%TPI(J2)%TPION  = ANC%FILLER_R8
! ------------------
                     ALLOCATE ( ANC%TPI(J2)%TPIOFF(ANC%NUM_TPS),        &
     &                          STAT=IER )
                     IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6907, IUER, 'ANC_PARSE',         &
     &                          'Error in allocating memory for the '// &
     &                          'ANC%TPI(j2)%TPIOFF while processing'// &
     &                          ' antenna calibration file '//FILIN )
                        DEALLOCATE ( BUF )
                        RETURN
                     END IF
                     ANC%TPI(J2)%TPIOFF = ANC%FILLER_R8
! ------------------
                     ALLOCATE ( ANC%TPI(J2)%TPIZERO(ANC%NUM_TPS),       &
     &                          STAT=IER )
                     IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6908, IUER, 'ANC_PARSE',         &
     &                          'Error in allocating memory for the '// &
     &                          'ANC%TPI(j2)%TPIZERO while processing'//&
     &                          ' antenna calibration file '//FILIN )
                        DEALLOCATE ( BUF )
                        RETURN
                     END IF
                     ANC%TPI(J2)%TPIZERO = ANC%FILLER_R8
 560              CONTINUE
                  GOTO 410
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<     
!     
! ------------ How many PC Sensors are there? 
!
               ELSEIF ( CUR_LINE(IND(1,2):IND(2,2))=='PC_SENSOR' ) THEN
!
                  READ ( UNIT=CUR_LINE(IND(1,3):IND(2,3)), FMT='(I8)')  &
     &                   ANC%NUM_PCS
!
! --------------- Allocate the number to PCS
!
                  ALLOCATE ( ANC%PCS(ANC%NUM_PCS), STAT=IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6909, IUER, 'ANC_PARSE',            &
     &                       'Error allocating memory for ANC%PCS '//   &
     &                       'while  processing antenna calibration '// &
     &                       'file '//FILIN )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
                  GOTO 410
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
! ------------ What are dimensions of PCAL and how many records are there
!              Same rules and notes as for TSYS and TPI dimensions above.
!
               ELSEIF ( CUR_LINE(IND(1,2):IND(2,2))=='PCAL' ) THEN
! ---------------
                  READ ( UNIT=CUR_LINE(IND(1,3):IND(2,3)), FMT='(I8)' ) &
     &                   ANC%NUM_EPO
!
! --------------- The next line will have the number of records of PCAL
!                 "NUM_RECORDS: PCAL XXXXXX"
!                 N.B: NUM_RECORDS .LE. NUM_PCS*NUM_EPO
!
                  IL = I_LEN(BUF(J1+1))
                  READ( UNIT=BUF(J1+1)(19:IL), FMT='(I8)' ) ANC%NUM_PCAL
!
! --------------- Allocate number to ANC%PCAL
!
                  ALLOCATE ( ANC%PCAL(ANC%NUM_EPO), STAT=IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6910, IUER, 'ANC_PARSE',            &
     &                       'Error in allocating memory for ANC%PCAL ' &
     &                     //'while processing antenna calibration '//  &
     &                       'file '//FILIN )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! -------------- Allocate NUM_PCS in each time stamp's PCAL
!
                  DO 530 J2=1,ANC%NUM_EPO
                     ALLOCATE ( ANC%PCAL(J2)%PCAL_CMPL(ANC%NUM_PCS),    &
     &                          STAT=IER )
                     IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6910, IUER, 'ANC_PARSE',         &
     &                          'Error in allocating memory for '//     &
     &                          'ANC%PCAL(j2)%PCAL_CMPL while '//       &
     &                          'processing antenna calibration '//     &
     &                          TRIM(FILIN) )
                        DEALLOCATE ( BUF )
                        RETURN
                     END IF
                     ANC%PCAL(J2)%PCAL_CMPL = CMPLX(ANC__FILLER_R4, 0.0)
 530              CONTINUE
                  GOTO 410
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!     
! ------------ How many GPS Timers are there
!
               ELSEIF (CUR_LINE(IND(1,2):IND(2,2))=='FMT2GPS_TIMER')THEN
!
                  READ ( UNIT=CUR_LINE(IND(1,3):IND(2,3)), FMT='(I8)' ) &
     &                   ANC%NUM_TGPS
!
! --------------- Allocate the number to TGPS
!
                  ALLOCATE ( ANC%TGPS(ANC%NUM_TGPS), STAT=IER )
! ---------------
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6911, IUER, 'ANC_PARSE',            &
     &                       'Error allocating memory for ANC%TGPS '//  &
     &                       'while processing antenna calibration '//  &
     &                       'file '//FILIN )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
                  GOTO 410
!##############################################################################################
!
! ------------ What are dimensions of PCAL and how many records are there
!              Same rules and notes as for TSYS, TPI and PCAL dimensions
!              above.
!
               ELSEIF ( CUR_LINE(IND(1,2):IND(2,2)) == 'FMTGPS' ) THEN
! ---------------
                  READ ( UNIT=CUR_LINE(IND(1,3):IND(2,3)), FMT='(I8)' ) &
     &                   ANC%NUM_EPO
!
! --------------- The next line will have the number of records of FMTGPS
!                 "NUM_RECORDS: FMTGPS XXXXX"
!                 N.B: NUM_RECORDS .LE. NUM_FMTGPS*NUM_EPO
!
                  IL = I_LEN(BUF(J1+1))
                  READ( UNIT=BUF(J1+1)(19:IL), FMT='(I8)' ) ANC%NUM_GPS
o!     
! --------------- Allocate number of epochs to ANC%GPS
!
                  ALLOCATE ( ANC%GPS(ANC%NUM_EPO), STAT=IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6912, IUER, 'ANC_PARSE',            &
     &                 'Error in allocating memory for ANC%GPS while'// &
     &                 ' processing antenna calibration file '//FILIN )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- Allocate NUM_TGPS in each time stamp's GPS
!
                  DO 550 J2=1, ANC%NUM_EPO
! ------------------
                     ALLOCATE (ANC%GPS(J2)%FMG(ANC%NUM_TGPS), STAT=IER)
                     IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6913, IUER, 'ANC_PARSE',         &
     &                          'Error in allocating memory for '//     &
     &                          'ANC%GPS(j2)%FMG while processing '//   &
     &                          'antenna calibration file '//FILIN )
                        DEALLOCATE ( BUF )
                        RETURN
                     END IF
                     ANC%GPS(J2)%FMG = ANC%FILLER_R8
! ------------
                     ALLOCATE (ANC%GPS(J2)%FMP(ANC%NUM_TGPS), STAT=IER)
                     IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6914, IUER, 'ANC_PARSE',         &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%GPS(J2)%FMP while processing antenna '// &
     &                    'calibration file '//FILIN )
                        DEALLOCATE ( BUF )
                        RETURN
                     END IF
                     ANC%GPS(J2)%FMP = ANC%FILLER_R8
 550              CONTINUE
                  GOTO 410
!##############################################################################################
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!     
! ------------ What are dimensions of SEFD and how many records are there
!              Same rules and notes as for TSYS, TPI, PCAL and SEFD
!              dimensions above.
!
               ELSEIF ( CUR_LINE(IND(1,2):IND(2,2)) == 'SEFD' ) THEN
! ---------------
                  READ ( UNIT=CUR_LINE(IND(1,3):IND(2,3)), FMT='(I5)' ) &
     &                   ANC%NUM_EPOSEFD
!
! --------------- The next line will have the number of records of SEFD
!                 "NUM_RECORDS: SEFD XXXX"
!                 N.B: NUM_RECORDS .LE. NUM_SEFD*NUM_EPOSEFD
!                      We used a different epoch variable because
!                      typically the number of epochs of SEFD are way
!                      less than those of the
                  IL = I_LEN(BUF(J1+1))
                  READ( UNIT=BUF(J1+1)(19:IL), FMT='(I8)' ) ANC%NUM_GPS
!     
! --------------- Allocate number to ANC%SEFD
!
                  ALLOCATE ( ANC%SEFD(ANC%NUM_SEFD), STAT=IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6915, IUER, 'ANC_PARSE',            &
     &                       'Error in allocating memory for ANC%SEFD'  &
     &                     //' while processing antcal file '//FILIN )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- Allocate NUM_TPS pointers in ANC%SEFD
!                 in each time stamp's SEFD, TSYS, TCAL, 
!                 TRAT, and GAIN
!
                  DO 540 J2=1,ANC%NUM_EPOSEFD
!
! ------------------ SEFD Time stamps
!
                     ALLOCATE (ANC%SEFD(J2)%SEFD(ANC%NUM_TPS), STAT=IER)
                     IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6916, IUER, 'ANC_PARSE',         &
     &                          'Error in allocating memory for '//     &
     &                          'ANC%SEFD(j2)%SEFD while processing '// &
     &                          'antcal file '//TRIM(FILIN) )
                        DEALLOCATE ( BUF )
                        RETURN
                     END IF
                     ANC%SEFD(J2)%SEFD = ANC%FILLER_R8
!
! ------------------ TSYS Time stamps
!
                     ALLOCATE (ANC%SEFD(J2)%TSYS(ANC%NUM_TPS), STAT=IER)
                     IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6917, IUER, 'ANC_PARSE',         &
     &                          'Error in allocating memory for '//     &
     &                          'ANC%SEFD(j2)%TSYS while processing '// &
     &                          'antcal file '// TRIM(FILIN) )
                        DEALLOCATE ( BUF )
                        RETURN
                     END IF
                     ANC%SEFD(J2)%TSYS = ANC%FILLER_R8
!
! ------------------ TCAL Time stamps
!
                     ALLOCATE (ANC%SEFD(J2)%TCAL(ANC%NUM_TPS), STAT=IER)
                     IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6918, IUER, 'ANC_PARSE',         &
     &                          'Error in allocating memory for '//     &
     &                          'ANC%SEFD(j2)%TCAL while processing '// &
     &                          'antcal file '// TRIM(FILIN) )
                        DEALLOCATE ( BUF )
                        RETURN
                     END IF
                     ANC%SEFD(J2)%TCAL = ANC%FILLER_R8
!
! ------------------ TRAT Time stamps
!
                     ALLOCATE (ANC%SEFD(J2)%TRAT(ANC%NUM_TPS), STAT=IER)
                     IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6919, IUER, 'ANC_PARSE',         &
     &                          'Error in allocating memory for '//     &
     &                          'ANC%SEFD(j2)%TRAT while processing '// &
     &                          'antcal file '//TRIM(FILIN) )
                        DEALLOCATE ( BUF )
                        RETURN
                     END IF
                     ANC%SEFD(J2)%TRAT = ANC%FILLER_R8
!
! ------------------ GAIN Time stamps
!
                     ALLOCATE (ANC%SEFD(J2)%GAIN(ANC%NUM_TPS), STAT=IER)
                     IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6920, IUER, 'ANC_PARSE',         &
     &                          'Error in allocating memory for '//     &
     &                          'ANC%SEFD(j2)%GAIN while processing '// &
     &                          'antcal file '//TRIM(FILIN) )
                        DEALLOCATE ( BUF )
                        RETURN
                     END IF
                     ANC%SEFD(J2)%GAIN = ANC%FILLER_R8
 540              CONTINUE
                  GOTO 410
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
 !  !                  TCAL setup doesn't really make sense to me. Deal with later
  !!                   (we will add TMOD when it is done)
!     
! ------------ What are dimensions of OPACITY and how many records are there
!              Opacity is a computed parameter on at least version 3 of the
!              anc file.
!              It should have at least the same dimensions as ANC%TTO%TSYS
!     
               ELSEIF ( CUR_LINE(IND(1,2):IND(2,2)) == 'OPACITY' ) THEN

!     
! --------------- The next line will have the number of records of OPACITY
!                 "NUM_RECORDS: OPACITY XXXX"
!
                  IL = I_LEN(BUF(J1+1))
                  READ( UNIT=BUF(J1+1)(22:IL), FMT='(I8)' ) ANC%NUM_OPA
!
!
! --------------- Allocate NUM_TPS in each time stamp's OPA
!
                  DO 570 J2=1,ANC%NUM_EPO
                     ALLOCATE (ANC%TTO(J2)%OPA(ANC%NUM_TPS), STAT=IER)
                     IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6921, IUER, 'ANC_PARSE',         &
     &                    'Error in allocating memory for the '//       &
     &                    'ANC%TTO(j2)%OPA while processing ' //        &
     &                    'antenna calibration file '//TRIM(FILIN) )
                        DEALLOCATE ( BUF )
                        RETURN
                     END IF
                     ANC%TTO(J2)%OPA = ANC%FILLER_R8
 570              CONTINUE
                  GOTO 410
!     
! ------------ What are dimensions of TATM and how many records are there
!              Total atmospheric brightness temperature is a computed
!              parameter on at least version 3 of the anc file.
!              It should have at least the same dimensions as ANC%TTO%TSYS
!     
               ELSEIF ( CUR_LINE(IND(1,2):IND(2,2)) == 'TATM' ) THEN
!     
! --------------- The next line will have the number of records of OPACITY
!                 "NUM_RECORDS: TATM XXXX"
!
                  IL = I_LEN(BUF(J1+1))
                  READ( UNIT=BUF(J1+1)(19:IL), FMT='(I8)' ) ANC%NUM_TATM
!
! --------------- Allocate NUM_TPS in each time stamp's TATM
!
                  DO 580 J2=1,ANC%NUM_EPO
                     ALLOCATE (ANC%TTO(J2)%TATM(ANC%NUM_TPS), STAT=IER)
                     IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6922, IUER, 'ANC_PARSE',         &
     &                    'Error in allocating memory for the '//       &
     &                    'ANC%TTO(j2)%OPA while processing ' //        &
     &                    'antenna calibration file '//TRIM(FILIN) )
                        DEALLOCATE ( BUF )
                        RETURN
                     END IF
                     ANC%TTO(J2)%TATM = ANC%FILLER_R8
 580              CONTINUE
                  GOTO 410
               END IF           ! The second word if, that tells us the block
! ALLOCATIONS DONE ABOVE THIS LINE. BELOW WE ARE PARSING THE NONE CRITICAL BLCKS             
!-------------------------------------------------------------------------------------
! --------- Parse information from the "PROVENANCE" block
!
            ELSEIF ( CUR_LINE(1:11) == 'PROVENANCE:' ) THEN
!
! ------------ Did we reach the "NUM_PROVENANCE" before getting here?
!
               IF ( ANC%NUM_PRV < 1 ) THEN
                  IUER = -1
                  CALL ERR_LOG ( 6923, IUER, 'ANC_PARSE',               &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- PROVENANCE: preceeds '//       &
     &                 'NUM_PROVENANCE: ' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ We want the index of the provenace number, so we will
!              just extract the indices from the first line.
!
               IF ( IPRV == -1 ) THEN
                  CALL EXWORD ( CUR_LINE, MIND, LIND, IND, DELIM, IER )
               END IF
!     
! ------------ Get the index of the provenance number
!
               READ ( UNIT=CUR_LINE(IND(1,2):IND(2,2)),FMT='(I8)' ) IPRV
!
! ------------ Update index of provenance to the current value
!
               IND_PRV_CUR = IPRV
!
! ------------ If the previous and current value differ, it means we have 
!              entered a new provenance part, and we can reset the counter
!     
               IF ( IND_PRV_CUR .NE. IND_PRV_PAS ) IPRV_CNT = 0
               IPRV_CNT    = IPRV_CNT + 1
!
! ------------ Parse the information to ANC%PRV
!              N.B: - We parse everything including the text that says
!                     "PROVENANCE: 1"
!
               IL = I_LEN(CUR_LINE)
               ANC%PRV(IPRV)%BUF(IPRV_CNT) = CUR_LINE(1:IL)
               GOTO 410
!
! --------- Parse information from the "DATA_ON" block
!
            ELSEIF ( CUR_LINE(1:8)) == 'DATA_ON:' ) THEN
! 
! ------------ Extract the indices of each word.
!              N.B: - To compensate for possible longer scan lengths we
!                     add 2 to the end index.
!
               IF ( IND_DOO == 0 ) THEN
                  CALL EXWORD ( CUR_LINE, MIND, LIND, IND, DELIM, IER )
!
! --------------- We expect at least 6 words in line
!
                  IF ( LIND < 6 ) THEN
                     CALL ERR_LOG ( 6924, IUER, 'ANC_PARSE',            &
     &                    'Too few words on DATA_ON block of the '//    &
     &                    'of the antenna calibration file '//FILIN )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- We reached "DATA_ON" before we actually got a count
!                 on its dimensions
!
                  IF ( .NOT. ASSOCIATED ( ANC%DOO ) ) THEN
                     CALL ERR_LOG ( 6925, IUER, 'ANC_PARSE',            &
     &                       'Malformed antenna calibration file '//    &
     &                       TRIM(FILIN)//' -- DATA_ON: preceeds '//    &
     &                       'DIMENSION: DATA_ON' )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
               END IF
!     
! ------------ Update index counter
!
               CALL CHIN ( CUR_LINE(10:IND(2,2)), IND_DOO )
!
! ------------ It is safe to assume that each date will be unique, ergo
!              no need to check if there are repeat dates.
!              However, in the odd event that there are repeat dates,
!              this block is not that long anyway.
!              Start epoch of scan
!     
               CALL ERR_PASS ( IUER, IER )
               CALL DATE_TO_TIME( CUR_LINE(IND(1,3):IND(2,3)), MJD,     &
     &                            UTC, IER )
               IF ( IER .NE. 0 ) THEN
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6926, IUER, 'ANC_PARSE',               &
     &                    'Error in parsing DATA_ON date '//            &
     &                    CUR_LINE(IND(1,3):IND(2,3))//' on line'//     &
     &                    TRIM(STR) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Skip if MJD for DATA_ON is below the minimum definition
!              Currently we have 44329 == 1980.03.31
!     
               IF ( MJD < MJD_MIN ) GOTO 410
!
! ------------ In case we haven't defined MJD_DOO yet
!
               IF ( ANC%MJD_DOO < 0 ) THEN
                  CALL ERR_PASS ( IUER, IER )
                  CALL NERS_GET_UTCMTAI ( NERS,                         &
     &                     (MJD - J2000__MJD)*86400.D0 + UTC,           &
     &                     UTC_MTAI, IER )
                  IF ( IER .NE. 0 ) THEN
                     WRITE ( 6, * ) 'J1= ', J1
                     WRITE ( 6, * ) 'DATE= ',CUR_LINE(IND(1,3):IND(2,3))
                     WRITE ( 6, * ) 'MJD= ', MJD, 'UTC= ', UTC
                     CALL ERR_LOG ( 6927, IUER, 'ANC_PARSE',            &
     &                       'Error getting UTC_MTAI on DATA_ON Block' )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- Define MJD_DOO, and TAI_DOO (initial epoch)
!
                  ANC%MJD_DOO = MJD
                  ANC%TAI_DOO = UTC - UTC_MTAI
                  IF ( ANC%UTC_MTAI .LT. 0.D0 ) THEN
                     ANC%UTC_MTAI = UTC_MTAI
                  END IF
               END IF
!
! ------------ Convert time to seconds from initial epoch
!
               TAI = UTC - UTC_MTAI
               ANC%DOO(IND_DOO)%TIM(1) = (MJD - ANC%MJD_DOO)*86400.D0 + &
     &                                   (TAI - ANC%TAI_DOO)             
! ---------
               CALL ERR_PASS ( IUER, IER )
               CALL DATE_TO_TIME( CUR_LINE(IND(1,4):IND(2,4)), MJD,     &
     &                            UTC, IER )
               IF ( IER .NE. 0 ) THEN
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6928, IUER, 'ANC_PARSE',               &
     &                    'Error in parsing DATA_ON date '//            &
     &                    CUR_LINE(IND(1,3):IND(2,3))//' on line'//     &
     &                    TRIM(STR) )
                  DEALLOCATE ( BUF )
                  RETURN
                  END
!
! ------------ End epoch of scan
!
               ANC%DOO(IND_DOO)%TIM(2)  = (MJD - ANC%MJD_DOO)*86400.D0  &
     &                                     + (TAI - ANC%TAI_DOO)
!
! ------------ N.B: There will always be at least 2 spaces between
!                   source and scan, so instead of using the end index
!                   of source, we will use the start index of scan - 2
!                   to define the location of source. 

               ANC%DOO(IND_DOO)%SOU_NAM = CUR_LINE(IND(1,5):IND(1,6)-2)
! 
! ------------ N.B: - Sometimes scan may vary in length, we are going
!                     to hope for the best and just tack 3 to the end of
!                     scan.
!               
               ANC%DOO(IND_DOO)%SCA_NAM = CUR_LINE(IND(1,6):IND(2,6)+2)
               GOTO 410
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! --------- Parse information from the "METEO" block
!
            ELSEIF ( CUR_LINE(1:6) == 'METEO:' ) THEN
! 
! ------------ Extract the indices of each word.
!              N.B: - To compensate for possible longer scan lengths we
!                     add 2 to the end index.
!
               IF ( IND_MET == 0 ) THEN
                  CALL EXWORD ( CUR_LINE, MIND, LIND, IND, DELIM, IER )
!
! --------------- We expect at least 9 words per line in this block
!
                  IF ( LIND < 9 ) THEN
                     CALL ERR_LOG ( 6929, IUER, 'ANC_PARSE',            &
     &                       'Too few words on METEO block of the '//   &
     &                       'antenna calibration file '//FILIN )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! ------------ We reached "METEO" before  getting dimensions
!
                  IF ( .NOT. ASSOCIATED ( ANC%MET ) ) THEN
                     CALL ERR_LOG ( 6930, IUER, 'ANC_PARSE',            &
     &                       'Malformed antenna calibration file '//    &
     &                       TRIM(FILIN)//' -- METEO: preceeds '//      &
     &                       'DIMENSION: METEO ' )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
               END IF
! ------------
               CALL CHIN ( CUR_LINE(8:IND(2,2)), IND_MET )
!
! ------------ NUM_METEO was not counted correctly
!
               IF ( IND_MET > ANC%NUM_MET ) THEN
                  CALL ERR_LOG ( 6931, IUER, 'ANC_PARSE',               &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- too many METEO lines.' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
! ------------
               CALL ERR_PASS ( IUER, IER )
               CALL DATE_TO_TIME( CUR_LINE(IND(1,3):IND(2,3)), MJD,     &
     &                            UTC, IER )
               IF ( IER .NE. 0 ) THEN
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6932, IUER, 'ANC_PARSE',               &
     &                    'Error in parsing METEO date '//              &
     &                    CUR_LINE(IND(1,3):IND(2,3))//' on line '//    &
     &                    TRIM(STR) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Skip if MJD for METEO is below the minimum definition
!              Currently we have 44329 == 1980.03.31
!     
               IF ( MJD < MJD_MIN ) GOTO 410
! ------------
               IF ( ANC%MJD_MET < 0 ) THEN
                  CALL ERR_PASS ( IUER, IER )
                  CALL NERS_GET_UTCMTAI ( NERS,                         &
     &                     (MJD - J2000__MJD)*86400.D0 + UTC,           &
     &                      UTC_MTAI, IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6933, IUER, 'ANC_PARSE',            &
     &                       'Error getting UTC_MTAI on METEO Block' )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- Define the initial MJD_MET and TAI_MET
!
                  ANC%MJD_MET = MJD
                  ANC%TAI_MET = UTC - UTC_MTAI
               END IF
               TAI = UTC - UTC_MTAI
               ANC%MET(IND_MET)%TIM = (MJD - ANC%MJD_MET)*86400.D0 +    &
     &                                (TAI - ANC%TAI_MET)
!
! ------------ Temperature [K]
! ------------ We will use the fact that the ending numbers are right 
!              justified, and characters are left justified.
!              We are avoiding using just the indices of the 4th word,
!              because it may be shorter than other values, e.g., if 
!              METEO: 1 has temp 0.0 K, then all other temperatures
!              would be unit values, if we just used that index.
!               
               READ ( UNIT=CUR_LINE(IND(2,3)+1:IND(2,4)),FMT='(F10.5)') &
     &                ANC%MET(IND_MET)%TEMP
!
! ------------ Pressure [Pa]
! ------------ Same deal as with temperature
!
               READ ( UNIT=CUR_LINE(IND(2,4)+1:IND(2,5)),FMT='(F10.5)') &
     &                ANC%MET(IND_MET)%PRES
!
! ------------ Humidity [%]
! ------------ Same deal as with temperature
!
               READ ( UNIT=CUR_LINE(IND(2,5)+1:IND(2,6)),FMT='(F10.5)') &
     &                ANC%MET(IND_MET)%HUMID
!
! ------------ Scan Index
! ------------ Same deal as with temperature
!
               READ ( UNIT=CUR_LINE(IND(2,6)+1:IND(2,7)), FMT='(I8)' )    &
     &                ANC%MET(IND_MET)%IND_SCA
!
! ------------ Source Name
! ------------ This is a string, so we use the fact that it is left
!              justified.
!
               ANC%MET(IND_MET)%SOU_NAM = CUR_LINE(IND(1,8):IND(1,9)-2)
!
! ------------ Scan Name
! ------------ This is a string, so we use the fact that it is left
!              justified.
!
               ANC%MET(IND_MET)%SCA_NAM = CUR_LINE(IND(1,9):IND(2,9)+2)
               GOTO 410
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!     
! --------- Parse information from the "CABLE" block
!
            ELSEIF ( CUR_LINE(1:6) == 'CABLE:' ) THEN
! 
! ------------ Extract the indices of each word.
!              N.B: - To compensate for possible longer scan lengths we
!                     add 2 to the end index.
!
               IF ( IND_CBL == 0 ) THEN
                  CALL EXWORD ( CUR_LINE, MIND, LIND, IND, DELIM, IER )
!     
! --------------- We expect at least 6 words per line in this block
!
                  IF ( LIND < 6 ) THEN
                     CALL ERR_LOG ( 6934, IUER, 'ANC_PARSE',            &
     &                       'Too few words on CABLE block of the '//   &
     &                       'antenna calibration file '//TRIM(FILIN) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! ------------ We reached "CABLE" before getting dimensions
!
                  IF ( .NOT. ASSOCIATED ( ANC%CBL ) ) THEN
                     CALL ERR_LOG ( 6935, IUER, 'ANC_PARSE',            &
     &                       'Malformed antenna calibration file '//    &
     &                       TRIM(FILIN)//' -- CABLE: preceeds ' //     &
     &                       ' DIMENSION: CABLE ' )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
               END IF
! ------------
               CALL CHIN ( CUR_LINE(8:IND(2,2)), IND_CBL )
!
! ------------ NUM_CABLE was not counted correctly
!
               IF ( IND_CBL > ANC%NUM_CBL ) THEN
                  CALL ERR_LOG ( 6936, IUER, 'ANC_PARSE',               &
     &                    'Malformed antenna calibration file '//       &
     &                    TRIM(FILIN)//' -- too many CABLE lines.' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
! ------------
               CALL ERR_PASS ( IUER, IER )
               CALL DATE_TO_TIME( CUR_LINE(IND(1,3):IND(2,3)), MJD,     &
     &                            UTC, IER )
               IF ( IER .NE. 0 ) THEN
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6937, IUER, 'ANC_PARSE',               &
     &                    'Error in parsing CABLE date '//              &
     &                    CUR_LINE(IND(1,3):IND(2,3))//' on line '//    &
     &                    TRIM(STR) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
! ------------
               IF ( ANC%MJD_CBL < 0 ) THEN
                  CALL ERR_PASS ( IUER, IER )
                  CALL NERS_GET_UTCMTAI (NERS,                              &
     &                                (MJD - J2000__MJD)*86400.D0 + UTC, &
     &                                UTC_MTAI, IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6938, IUER, 'ANC_PARSE',                &
     &                      'Error in getting UTC_MTAI on CABLE Block' )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- Define Initial MJD_CBL and TAI_CBL
!
                  ANC%MJD_CBL = MJD
                  ANC%TAI_CBL = UTC - UTC_MTAI
               END IF
               TAI = UTC - UTC_MTAI
               ANC%CBL(IND_CBL)%TIM = (MJD - ANC%MJD_CBL)*86400.D0 +    &
     &                             (TAI - ANC%TAI_CBL)
!
! ------------ Cable Delay [s]
!
               READ (UNIT=CUR_LINE(IND(2,3)+1:IND(2,4)),FMT='(1PE12.5)') &
     &               ANC%CBL(IND_CBL)%DELAY
!
! ------------ Source Name
! ------------ This is a string, so we use the fact that it is left
!              justified.
!
               ANC%CBL(IND_CBL)%SOU_NAM = CUR_LINE(IND(1,5):IND(2,6)-2)
!
! ------------ Scan Name
! ------------ This is a string, so we use the fact that it is left
!              justified.
!
               ANC%CBL(IND_CBL)%SCA_NAM = CUR_LINE(IND(1,6):IND(2,6)+2)
               GOTO 410
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! --------- Parse information from the TP_SENSOR block
!
            ELSEIF ( CUR_LINE(1:10) == 'TP_SENSOR:' ) THEN
! 
! ------------ Extract the indices of each word.
!              N.B: - To compensate for possible longer scan lengths we
!                     add 2 to the end index.
!
               IF ( K_TPS == 0 ) THEN
                  CALL EXWORD ( CUR_LINE, MIND, LIND, IND, DELIM, IER )
!              
! --------------- No less than 11 words in this block
!
                  IF ( LIND < 11 ) THEN
                     CALL ERR_LOG ( 6939, IUER, 'ANC_PARSE',            &
     &                    'Too few words on TP_SENSOR block of the '//  &
     &                    'antenna calibration file '//TRIM(FILIN) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- We reached "TP_SENSOR" before getting dimensions
!
                  IF ( .NOT. ASSOCIATED ( ANC%TPS ) ) THEN
                     CALL ERR_LOG ( 6940, IUER, 'ANC_PARSE',            &
     &                       'Malformed antenna calibration file '//    &
     &                       TRIM(FILIN)//' -- TP_SENSOR: preceeds '//  &
     &                       ' DIMENSION: TP_SENSOR ' )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
               END IF
!     
! ------------ Update TPS counter
!
               K_TPS = K_TPS + 1
!
! ------------ We exceeded the defined NUM_TPS
!
               IF ( K_TPS > ANC%NUM_TPS ) THEN
                  CALL ERR_LOG ( 6941, IUER, 'ANC_PARSE',               &
     &                    'Malformed antenna calibration file '//       &
     &                    TRIM(FILIN)//' -- too many TP_SENSOR lines' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Sensor Tag
!
               ANC%TPS(K_TPS)%TAG = CUR_LINE(IND(1,2):IND(2,2))
               ANC%TPS_TAG(K_TPS) = CUR_LINE(IND(1,2):IND(2,2))
!
! ------------ Intermediate Freq. [Hz]
!              Use the fact of positions of the values around it
!
               READ ( UNIT=TRIM(CUR_LINE(IND(2,2)+2:IND(2,3))),         &
     &                FMT='(F16.2)') ANC%TPS(K_TPS)%IF_FRQ
               ANC%TPS(K_TPS)%IF_FRQ  = ANC%TPS(K_TPS)%IF_FRQ*1.D6
!
! ------------ Local Oscilator Freq. [Hz]
!              Use the fact of positions of the values around it
!
               READ ( UNIT=TRIM(CUR_LINE(IND(2,3)+2:IND(2,4))), &
     &                FMT='(F16.2)') ANC%TPS(K_TPS)%LO_FRQ
               ANC%TPS(K_TPS)%LO_FRQ  = ANC%TPS(K_TPS)%LO_FRQ*1.D6
!
! ------------ Sky Freq. [Hz]
!              Use the fact of positions of the values around it
!
               READ ( UNIT=TRIM(CUR_LINE(IND(2,4)+2:IND(2,5))),         &
     &                FMT='(F16.2)') ANC%TPS(K_TPS)%SKY_FRQ
               ANC%TPS(K_TPS)%SKY_FRQ = ANC%TPS(K_TPS)%SKY_FRQ*1.D6
!
! ------------ Bandwidth [Hz]
!              Use the fact of positions of the values around it
!
               READ ( UNIT=TRIM(CUR_LINE(IND(2,5)+1:IND(2,6))),         &
     &                FMT='(F16.2)') ANC%TPS(K_TPS)%BDW
               ANC%TPS(K_TPS)%BDW     = ANC%TPS(K_TPS)%BDW*1.D6
!
! ------------ Polarization
! ------------ This is meant to be 1 character, ergo shouldn't change 
!              positions
!     
               IF ( CUR_LINE(IND(1,7):IND(2,7)) == 'R' ) THEN
                  ANC%TPS(K_TPS)%POL = ANC__R_POL
               ELSEIF ( CUR_LINE(IND(1,7):IND(2,7)) == 'L' ) THEN
                  ANC%TPS(K_TPS)%POL = ANC__L_POL
               ELSEIF ( CUR_LINE(IND(1,7):IND(2,7)) == 'H' ) THEN
                  ANC%TPS(K_TPS)%POL = ANC__H_POL
               ELSEIF ( CUR_LINE(IND(1,7):IND(2,7)) == 'V' ) THEN
                  ANC%TPS(K_TPS)%POL = ANC__V_POL
               ELSEIF ( CUR_LINE(IND(1,7):IND(2,7)) == 'X' ) THEN
                  ANC%TPS(K_TPS)%POL = ANC__X_POL
               ELSEIF ( CUR_LINE(IND(1,7):IND(2,7)) == 'Y' ) THEN
                  ANC%TPS(K_TPS)%POL = ANC__Y_POL
               ELSE 
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6942, IUER, 'ANC_PARSE',               &
     &                    CUR_LINE(IND(1,7):IND(2,7))//' in line '//    &
     &                    TRIM(STR)//' is not a supported polarization' &
     &                  //' code. We expected R, L, H, V, X, or Y. See' &
     &
                  //' the antenna calibration file '//TRIM(FILIN))
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ TP_SENSOR ID
!
               ANC%TPS(K_TPS)%ID = TRIM(CUR_LINE(IND(2,7)+2:IND(2,8)))
!
! ------------ Intermediate Frequency Index
! ------------ Will be either 1 or 2 digits long
!     
               READ ( UNIT=CUR_LINE(IND(1,9)-1:IND(2,9)), FMT='(I8)' )  &
     &                ANC%TPS(K_TPS)%IF_IND
!
! ------------ Sensor Sideband Code
!
               IF ( CUR_LINE(IND(2,9)+2:IND(2,10)) == 'USB' ) THEN
                  ANC%TPS(K_TPS)%SSB = ANC__USB
! ------------
               ELSEIF ( CUR_LINE(IND(2,9)+2:IND(2,10)) == 'LSB' ) THEN
                  ANC%TPS(K_TPS)%SSB = ANC__LSB
! ------------ 
               ELSEIF ( CUR_LINE(IND(2,9)+2:IND(2,10)) == 'n/a' ) THEN
                  ANC%TPS(K_TPS)%SSB = ANC__NA
! ------------
               ELSEIF ( CUR_LINE(IND(2,9)+2:IND(2,10)) == 'DUMMY' ) THEN
                  ANC%TPS(K_TPS)%SSB = ANC__NA
! ------------
               ELSE
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6943, IUER, 'ANC_PARSE',               &
     &                    CUR_LINE(IND(2,9)+2:IND(2,10))//' in line '// &
     &                    TRIM(STR)//' isnt supported Sensor sideband'  &
     &                  //' code. We expected USB, LSB, n/a, or DUMMY.' &
     &                  //' See the antenna calibration file '//        &
     &                    TRIM(FILIN) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Net Sideband Code
!
               IF ( TRIM(CUR_LINE(IND(2,10)+2:IND(2,11))) == 'USB') THEN
                  ANC%TPS(K_TPS)%NSB = ANC__USB
! ------------
               ELSEIF ( TRIM(CUR_LINE(IND(2,10)+2:IND(2,11))) ==        &
     &                  'LSB' ) THEN
                  ANC%TPS(K_TPS)%NSB = ANC__LSB
! ------------ 
               ELSEIF ( TRIM(CUR_LINE(IND(2,10)+2:IND(2,11))) ==        &
     &                  'n/a' ) THEN
                  ANC%TPS(K_TPS)%NSB = ANC__NA
! ------------
               ELSEIF ( TRIM(CUR_LINE(IND(2,10)+2:IND(2,11))) ==        &
     &                  'DUMMY') THEN
                  ANC%TPS(K_TPS)%NSB = ANC__NA
! ------------
               ELSE
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6944, IUER, 'ANC_PARSE',               &
     &                    CUR_LINE(IND(2,10)+2:IND(2,11))//' (line '//  &
     &                    TRIM(STR)//') isnt supported Net sideband'//  &
     &                    ' code. We expected USB, LSB, n/a, or DUMMY.' &
     &                  //' See the antenna calibration file '//        &
     &                    TRIM(FILIN) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!==============================================================================
!
! --------- Parse information from the Tsys block
!
            ELSEIF ( CUR_LINE(1:5) == 'TSYS:' ) THEN
!     
! ------------ This is the Tsys record
! ------------ Check whether the epoch index filled in this record is
!              the same as the previous record
!
               IF ( CUR_LINE(7:13) .NE. PREV_LINE(7:13) ) THEN
!
! --------------- Not the same? Then we need to parse time
!
                  CALL ERR_PASS ( IUER, IER )
                  CALL DATE_TO_TIME ( CUR_LINE(15:36), MJD, UTC, IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL CLRCH (     STR )
                     CALL INCH  ( J1, STR )
                     CALL ERR_LOG ( 6947, IUER, 'ANC_PARSE',            &
     &                       'Error in parsing TSYS date '//            &
     &                       CUR_LINE(15:36)//' on line '//             &
     &                       TRIM(STR) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
! ---------------
                  IF ( ANC%MJD_TTO < 0 ) THEN
                     CALL ERR_PASS ( IUER, IER )
                     CALL NERS_GET_UTCMTAI ( NERS,                         &
     &                     (MJD - J2000__MJD)*86400.D0 + UTC,           &
     &                     UTC_MTAI, IER )
                     IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6948, IUER, 'ANC_PARSE',            &
     &                       'Error in getting UTC_MTAI on '//          &
     &                       'TSYS Block' )
                        DEALLOCATE ( BUF )
                        RETURN
                     END IF
!
! ------------------ Initial MJD_TTO and TAI_TTO
!
                     ANC%MJD_TTO = MJD
                     ANC%TAI_TTO = UTC - UTC_MTAI
                  END IF
                  TAI = UTC - UTC_MTAI
               END IF
!
! ------------ Update the Tsys counter, given the time diff. error is met
!
               IF ( K_TTO == 0 ) THEN
                  FL_UPDATE = .TRUE.
               ELSE
                  IF ( DABS((MJD - ANC%MJD_TTO)*86400.D0 +              &
     &                      (TAI - ANC%TAI_TTO) - ANC%TTO(K_TTO)%TIM)   &
     &                  > 3*ANC__EPS_TIM ) THEN
                     FL_UPDATE = .TRUE.
                  ELSE
                     FL_UPDATE = .FALSE.
                  END IF
               END IF
               IF ( FL_UPDATE ) THEN
                  K_TTO = K_TTO + 1
!
! --------------- Check for overflow
!
                  IF ( K_TTO > ANC%NUM_EPO ) THEN
                     CALL CLRCH (     STR )
                     CALL INCH  ( J1, STR )
                     CALL IINCH ( K_TTO, STR1 )
                     CALL IINCH ( ANC%NUM_TTO, STR2 )
                     CALL ERR_LOG ( 6949, IUER, 'ANC_PARSE',            &
     &                      'Malformed antenna calibration file '//     &
     &                       TRIM(FILIN)//' -- too many TTO lines. '//  &
     &                       'Got '//TRIM(STR1)//' ,expected '//        &
     &                       TRIM(STR2) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- Compute time
!
                  ANC%TTO(K_TTO)%TIM = (MJD - ANC%MJD_TTO)*86400.D0 +   &
     &                                 (TAI - ANC%TAI_TTO)
!
! --------------- Scan or Epoch index
!
                  READ ( UNIT=CUR_LINE(7:13),  FMT='(I7)' )             &
     &                   ANC%TTO(K_TTO)%IND_SCA
!
! --------------- Azimuth [rad]     
!
                  READ ( UNIT=CUR_LINE(54:63)),  FMT='(F9.4)' )         &
     &                   ANC%TTO(K_TTO)%AZ
                  ANC%TTO(K_TTO)%AZ = ANC%TTO(K_TTO)%AZ*DEG__TO__RAD
!
! --------------- Elevation [rad]
!
                  READ ( UNIT=CUR_LINE(66:72), FMT='(F8.4)' )           &
     &                   ANC%TTO(K_TTO)%EL
                  ANC%TTO(K_TTO)%EL = ANC%TTO(K_TTO)%EL*DEG__TO__RAD
!
! --------------- Source Name
!
                  ANC%TTO(K_TTO)%SOU_NAM = CUR_LINE(74:82)
!
! --------------- Scan Name
!
                  ANC%TTO(K_TTO)%SCA_NAM = CUR_LINE(85:95)
               END IF
!
! ------------ Is the sensor tag in TTO block available in list of tags
!              from TPS_SENSOR block
!
               IF ( IND_TPS < ANC%NUM_TPS ) THEN
                  IF ( CUR_LINE(39:44) ==  ANC%TPS_TAG(IND_TPS+1) ) THEN
                     IND_TPS = IND_TPS + 1
                  ELSE
                     IND_TPS = LTM_DIF ( 0, ANC%NUM_TPS, ANC%TPS_TAG,   &
     &                                   CUR_LINE(39:44) )
                  END IF
               ELSE
                  IND_TPS = LTM_DIF ( 0, ANC%NUM_TPS, ANC%TPS_TAG,      &
     &                                CUR_LINE(39:44) )
               END IF
               IF ( IND_TPS < 1 ) THEN
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6950, IUER, 'ANC_PARSE',               &
     &                    'Tag '//CUR_LINE(39:46)//'not part of '//     &
     &                    'TP_SENSOR tags in '//TRIM(FILIN)//' .See '   &
     &                    //'line '//TRIM(STR)//' on the TSYS Block' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Parse the Tsys value to the index of TTO in ANC%TTO that 
!              coincides with the index of the sensor tag in TP_SENSOR 
!              Block. Units = K
!
               READ ( UNIT=CUR_LINE(47:52),FMT='(F6.1)' )               &
     &                ANC%TTO(K_TTO)%TSYS(IND_TPS)
               GOTO 410
!==============================================================================
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!
! --------- Parse information from the TPI Block
!
            ELSEIF ( CUR_LINE(1:4) == 'TPI:' ) THEN
!     
! ------------ This is the TPI record
! ------------ Check whether the epoch index filled in this record is
!              the same as in the previous record
!
               IF ( CUR_LINE(6:12) .NE. PREV_LINE(6:12) ) THEN
!
! --------------- Not the same? Then we need to parse time
!
                  CALL ERR_PASS ( IUER, IER )
                  CALL DATE_TO_TIME ( CUR_LINE(14:35), MJD, UTC, IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL CLRCH (     STR )
                     CALL INCH  ( J1, STR )
                     CALL ERR_LOG ( 6953, IUER, 'ANC_PARSE',            &
     &                       'Error in parsing TPI date '//             &
     &                       CUR_LINE(14:35)//' on line '// &
     &                       TRIM(STR) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
! ---------------
                  IF ( ANC%MJD_TPI < 0 ) THEN
                     CALL ERR_PASS ( IUER, IER )
                     CALL NERS_GET_UTCMTAI ( NERS,                      &
     &                        (MJD - J2000__MJD)*86400.D0 + UTC,        &
     &                        UTC_MTAI, IER )
                     IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6954, IUER, 'ANC_PARSE',         &
     &                          'Error getting UTC_MTAI on TPI Block' )
                        DEALLOCATE ( BUF )
                        RETURN
                     END IF
!
! ------------------ MJD_TPI and TAI_TPI
!
                     ANC%MJD_TPI = MJD
                     ANC%TAI_TPI = UTC - UTC_MTAI
                  END IF
                  TAI = UTC - UTC_MTAI
               END IF
!
! ------------ Update the TPI counter, given the time diff. error is met
!
               IF ( K_TPI == 0 ) THEN
                  FL_UPDATE = .TRUE.
               ELSE
                  IF ( DABS((MJD - ANC%MJD_TPI)*86400.D0 +              &
     &                      (TAI - ANC%TAI_TPI) - ANC%TPI(K_TPI)%TIM)   &
     &                  > 3*ANC__EPS_TIM ) THEN
                     FL_UPDATE = .TRUE.
                  ELSE
                     FL_UPDATE = .FALSE.
                  END IF
               END IF
!
! ------------ If the counter was updated, then work on appending the 
!              TPI structure
!     
               IF ( FL_UPDATE ) THEN
                  K_TPI = K_TPI + 1
!
! --------------- Check for overflow
!
                  IF ( K_TPI > ANC%NUM_EPO ) THEN
                     CALL IINCH ( K_TPI, STR1 )
                     CALL IINCH ( ANC%NUM_TPI, STR2 )
                     CALL ERR_LOG ( 6955, IUER, 'ANC_PARSE',            &
     &                      'Malformed antenna calibration file '//     &
     &                       TRIM(FILIN)//' -- too many TPI lines. '//  &
     &                       'Got '//TRIM(STR1)//' ,expected '//        &
     &                       TRIM(STR2) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- Compute time
!
                  ANC%TPI(K_TPI)%TIM = (MJD - ANC%MJD_TPI)*86400.D0 +   &
     &                                 (TAI - ANC%TAI_TPI)
!
! --------------- Scan or Epoch index
!
                  READ ( UNIT=CUR_LINE(6:12), FMT='(I7)' )              &
     &                   ANC%TPI(K_TPI)%IND_SCA
!
! --------------- Azimuth [rad]     
!
                  READ ( UNIT=CUR_LINE(74:82), FMT='(F9.4)' )           &
     &                   ANC%TPI(K_TPI)%AZ
                  ANC%TPI(K_TPI)%AZ = ANC%TPI(K_TPI)%AZ*DEG__TO__RAD
!
! --------------- Elevation [rad]
!
                  READ ( UNIT=CUR_LINE(85:91), FMT='(F7.4)' )           &
     &                   ANC%TPI(K_TPI)%EL
                  ANC%TPI(K_TPI)%EL = ANC%TPI(K_TPI)%EL*DEG__TO__RAD
!
! --------------- Source Name
!
                  ANC%TPI(K_TPI)%SOU_NAM = CUR_LINE(94:101) 
!
! --------------- Scan name
!
                  ANC%TPI(K_TPI)%SCA_NAM = TRIM(CUR_LINE(104:114))
               END IF
!
! ------------ Is the sensor tag in TPI block available in list of tags
!              from TPS_SENSOR block
!
               IF ( IND_TPS < ANC%NUM_TPS ) THEN
                  IF ( CUR_LINE(38:43) == ANC%TPS_TAG(IND_TPS+1) ) THEN
                     IND_TPS = IND_TPS + 1
                  ELSE
                     IND_TPS = LTM_DIF ( 0, ANC%NUM_TPS, ANC%TPS_TAG,   &
     &                                   TRIM(CUR_LINE(38:43)) )
                  END IF
               ELSE
                  IND_TPS = LTM_DIF ( 0, ANC%NUM_TPS, ANC%TPS_TAG,      &
     &                                CUR_LINE(38:43) )
               END IF
               IF ( IND_TPS < 1 ) THEN
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6956, IUER, 'ANC_PARSE',               &
     &                    'Tag '//CUR_LINE(38:44)//'not part of '//     &
     &                    'TP_SENSOR tags in '//TRIM(FILIN)//' .See '   &
     &                    //'line '//TRIM(STR)//' on the TPI Block' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Parse the TPION, TPIOFF, and TPIZERO values to their
!              respective indices on TPION and TPIOFF in ANC%TPI also
!              corresponding to the correct sensor tag in TP_SENSOR Block.
!              Units = K
!
               READ ( UNIT=CUR_LINE(46:53), FMT='(I8)' )                &
     &                ANC%TPI(K_TPI)%TPION(IND_TPS) 
               READ ( UNIT=CUR_LINE(55:62), FMT='(I8)' )                &
     &                ANC%TPI(K_TPI)%TPIOFF(IND_TPS)
               READ ( UNIT=CUR_LINE(64:71), FMT='(I8)' )                &
     &                ANC%TPI(K_TPI)%TPIOFF(IND_TPS)
! ------------
               GOTO 410
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!
! --------- Parse information from the PC_SENSOR
!
            ELSEIF ( CUR_LINE(IND(1,1):IND(2,1)) == 'PC_SENSOR:' ) THEN
! 
! ------------ Extract the indices of each word.
!              N.B: - To compensate for possible longer scan lengths we
!                     add 2 to the end index.
!
               IF ( K_PCS == 0 ) THEN
                  CALL EXWORD ( CUR_LINE, MIND, LIND, IND, DELIM, IER )
!
! ------------- Parse information from the PC_SENSOR block
!               No less than 8 words in this block
!
                  IF ( LIND < 8 ) THEN                          
                     CALL CLRCH (     STR )
                     CALL INCH  ( J1, STR )
                     CALL ERR_LOG ( 6957, IUER, 'ANC_PARSE',            &
     &                       'Too few words on PC_SENSOR line block '// &
     &                       TRIM(STR)//' of the antenna calibration'// &
     &                       ' file '//TRIM(FILIN) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- We reached "PC_SENSOR" before getting dimensions
!
                  IF ( .NOT. ASSOCIATED ( ANC%TPS ) ) THEN
                     CALL ERR_LOG ( 6958, IUER, 'ANC_PARSE',            &
     &                       'Malformed antenna calibration file '//    &
     &                       TRIM(FILIN)//' -- PC_SENSOR: preceeds '//  &
     &                       ' DIMENSION: PC_SENSOR ' )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
               END IF
!
! ------------ Update PCS counter
!
               K_PCS = K_PCS + 1
!
! ------------ We exceeded the defined NUM_PCS
!
               IF ( K_PCS > ANC%NUM_PCS ) THEN
                  CALL IINCH ( K_PCS, STR1 )
                  CALL IINCH ( ANC%NUM_PCS, STR2 )
                  CALL ERR_LOG ( 6959, IUER, 'ANC_PARSE',               &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- too many PC_SENSORS lines. '// &
     &                 'Got '//TRIM(STR1)//' ,expected '//TRIM(STR2) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Sensor Tag
!
               ANC%PCS(K_PCS)%TAG = CUR_LINE(IND(1,2):IND(2,2))
               ANC%PCS_TAG(K_PCS) = CUR_LINE(IND(1,2):IND(2,2))
!
! ------------ Sky Frequency [Hz]
!
               READ ( UNIT=TRIM(CUR_LINE(IND(2,2)+1:IND(2,3)),          &
     &                FMT='(F20.5)' ) ANC%PCS(K_PCS)%SKY_FRQ
               ANC%PCS(K_PCS)%SKY_FRQ = ANC%PCS(K_PCS)%SKY_FRQ*1.D6
!
! --------- Polarization
!
               IF ( CUR_LINE(IND(1,4):IND(2,4)) == 'R' ) THEN
                  ANC%PCS(K_PCS)%POL = ANC__R_POL
               ELSEIF ( CUR_LINE(IND(1,4):IND(2,4)) == 'L' ) THEN
                  ANC%PCS(K_PCS)%POL = ANC__L_POL
               ELSEIF ( CUR_LINE(IND(1,4):IND(2,4)) == 'H' ) THEN
                  ANC%PCS(K_PCS)%POL = ANC__H_POL
               ELSEIF ( CUR_LINE(IND(1,4):IND(2,4)) == 'V' ) THEN
                  ANC%PCS(K_PCS)%POL = ANC__V_POL
               ELSEIF ( CUR_LINE(IND(1,4):IND(2,4)) == 'X' ) THEN
                  ANC%PCS(K_PCS)%POL = ANC__X_POL
               ELSEIF ( CUR_LINE(IND(1,4):IND(2,4)) == 'Y' ) THEN
                  ANC%PCS(K_PCS)%POL = ANC__Y_POL
               ELSE 
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6960, IUER, 'ANC_PARSE',               &
     &                 CUR_LINE(IND(1,4):IND(2,4))//' in line '//       &
     &                 TRIM(STR)//' is not a supported polarization '// &
     &                 'code. We expected R, L, H, V, X, or Y. See '//  &
     &                 'the antenna calibration file '//TRIM(FILIN) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ PC_SENSOR ID
!
               ANC%PCS(K_PCS)%ID = TRIM(CUR_LINE(IND(2,4)+2:IND(2,5)))
!
! ------------ Intermediate Frequency Index
!
               READ ( UNIT=TRIM(CUR_LINE(IND(2,5)+2:IND(2,6)),          &
     &                FMT='(I8)' )  ANC%PCS(K_PCS)%IF_IND
!
! --------- Sensor Subband Code
!
               IF ( TRIM(CUR_LINE(IND(2,6)+2:IND(2,7))) == 'USB' ) THEN
                  ANC%PCS(K_PCS)%NSB = ANC__USB
               ELSEIF ( TRIM(CUR_LINE(IND(2,6)+2:IND(2,7))) ==          &
     &                  'LSB'                                    ) THEN
                  ANC%PCS(K_PCS)%NSB = ANC__LSB
               ELSEIF ( TRIM(CUR_LINE(IND(2,6)+2:IND(2,7))) ==          &
     &                  'n/a'                                    ) THEN
                  ANC%PCS(K_PCS)%NSB = ANC__NA
               ELSEIF ( TRIM(CUR_LINE(IND(2,6)+2:IND(2,7))) ==          &
     &                  'DUMMY'                                  ) THEN
                  ANC%PCS(K_PCS)%NSB = ANC__NA
               ELSE
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6961, IUER, 'ANC_PARSE',               &
     &                 CUR_LINE(IND(1,7):IND(2,7))//' in line '//       &
     &                 TRIM(STR)//' is not a supported subband code.'// &
     &                 ' We expected USB, LSB, n/a, or DUMMY. See '//   &
     &                 'the antenna calibration file '//TRIM(FILIN) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
            ELSE IF ( CUR_LINE(1:5) == 'PCAL:' ) THEN
!
! ------------ This is the Pcal record
! ------------ Check whether the epoch index filled on this record is 
!              the same as in the previous record
!
               IF ( CUR_LINE(7:13) .NE. PREV_LINE(3:13) ) THEN
!
! --------------- Not the same? Then we need to parse time
!
                  CALL ERR_PASS ( IUER, IER )
                  CALL DATE_TO_TIME ( TRIM(CUR_LINE(15:36)), MJD, UTC,  &
     &                                IER )
!
! ---------------
!
                  IF ( IER .NE. 0 ) THEN
                     CALL CLRCH (     STR )
                     CALL INCH  ( J1, STR )
                     CALL ERR_LOG ( 6962, IUER, 'ANC_PARSE',            &
     &                       'Error in parsing PCAL date '//            &
     &                       CUR_LINE(15:36)//' in line '//STR )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! ---------------
!
                  IF ( ANC%MJD_PCAL < 0 ) THEN
                     CALL ERR_PASS ( IUER, IER )
                     CALL NERS_GET_UTCMTAI ( NERS,                      &
     &                        (MJD - J2000__MJD)*86400.D0 + UTC,        &
     &                        UTC_MTAI, IER )
                     IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6963, IUER, 'ANC_PARSE',         &
     &                          'Error in getting UTC_MTAI on '//       &
     &                          'PCAL Block' )
                        DEALLOCATE ( BUF )
                        RETURN
                     END IF
!
! ------------------ MJD_PCAL and TAI_PCAL
!
                     ANC%MJD_PCAL = MJD
                     ANC%TAI_PCAL = UTC - UTC_MTAI
                  END IF
                  TAI = UTC - UTC_MTAI
               END IF
!
! ------------ Update the PCAL counter, given the time diff. error is met
!
               IF ( K_PCAL == 0 ) THEN
                  FL_UPDATE = .TRUE.
               ELSE
                  IF ( DABS((MJD-ANC%MJD_PCAL)*86400.D0 +               &
     &                      (TAI-ANC%TAI_PCAL) - ANC%PCAL(K_PCAL)%TIM)  &
     &                  > 3*ANC__EPS_TIM ) THEN
                     FL_UPDATE = .TRUE.
                  ELSE
                     FL_UPDATE = .FALSE.
                  END IF
               END IF
!
! ------------ If the counter was updated, then work on appending the 
!              PCAL structure
!     
               IF ( FL_UPDATE ) THEN
                  K_PCAL = K_PCAL + 1
!
! --------------- Check for overflow
!
                  IF ( K_PCAL > ANC%NUM_EPO ) THEN
                     CALL CLRCH (     STR )
                     CALL INCH  ( J1, STR )
                     CALL IINCH ( K_PCAL, STR1 )
                     CALL IINCH ( ANC%NUM_PCAL, STR2 )
                     CALL ERR_LOG ( 6964, IUER, 'ANC_PARSE',            &
     &                      'Malformed antenna calibration file '//     &
     &                      TRIM(FILIN)//' -- too many PCAL lines. '//  &
     &                      'Got '//TRIM(STR1)//' ,expected '//         &
     &                      TRIM(STR2)//' last processed line: '//STR )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- Compute time tag of the pcal record
!
                  ANC%PCAL(K_PCAL)%TIM = (MJD - ANC%MJD_PCAL)*86400.D0  &
     &                                   + (TAI - ANC%TAI_PCAL)
!
! --------------- Scan or Epoch index
!
                  READ ( UNIT=CUR_LINE(7:13), FMT='(I7)' )              &
     &                   ANC%PCAL(K_PCAL)%IND_SCA
!
! --------------- Source name
!
                  ANC%PCAL(K_PCAL)%SOU_NAM = CUR_LINE(71:77)
!
! --------------- Scan name
!
                  ANC%PCAL(K_PCAL)%SCA_NAM = CUR_LINE(81:91)
               END IF
!
! ------------ Is the sensor tag in PCAL block available in list of tags
! ------------ from PCS_SENSOR block
!
               IF ( IND_PCS < ANC%NUM_PCS ) THEN
                  IF ( TRIM(CUR_LINE(39:44)) ==                         &
     &                 TRIM(ANC%PCS_TAG(IND_PCS+1)) ) THEN
                     IND_PCS = IND_PCS + 1
                  ELSE
                     IND_PCS = LTM_DIF ( 0, ANC%NUM_PCS, ANC%PCS_TAG,   &
     &                                   CUR_LINE(39:44) )
                  END IF
               ELSE
                  IND_PCS = LTM_DIF ( 0, ANC%NUM_PCS, ANC%PCS_TAG,      &
     &                                CUR_LINE(39:44) )
               END IF
               IF ( IND_PCS < 1 ) THEN
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6964, IUER, 'ANC_PARSE',               &
     &                    'Tag '//CUR_LINE(39:44)//' not a part of '//  &
     &                    'PC_SENSOR tags in '//TRIM(FILIN)//           &
     &                    ' .See line '//TRIM(STR)//' on PCAL Block' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Parse the PCal value to the index of PCAL_CMPL in ANC%PCAL 
! ------------ that coincides with the index of the sensor tag in 
! ------------ PC_SENSOR Block. Units = 
!
               READ ( UNIT = CUR_LINE(46:54), FMT='(F9.3)' ) RAMPL ! Amplitude
               READ ( UNIT = CUR_LINE(58:66), FMT='(F9.5)' ) RPHAS ! Phase angle
!
               IF ( ABS(RAMPL - ANC%FILLER_R8) > EPS ) THEN
                  RAMPL = RAMPL*ANC__AMP_SCA
               ELSE
                  RAMPL = ANC%FILLER_R8
                  RPHAS = ANC%FILLER_R8
               END IF
!
! ------------ Convert the amplitude and phase to complex number
!
               ANC%PCAL(K_PCAL)%PCAL_CMPL(IND_PCS) =                    &
     &              CMPLX ( RAMPL*DCOS(RPHAS), RAMPL*DSIN(RPHAS) )
               GOTO 410
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
! --------- Parse information from the FMT2GPS Block
!
            ELSEIF ( CUR_LINE(1:14) == 'FMT2GPS_TIMER:' ) THEN
!
! ------------ Extract the indices of each word.
!
               IF ( K_TGPS == 0 ) THEN
                  CALL EXWORD ( CUR_LINE, MIND, LIND, IND, DELIM, IER )
!
! --------------  No less than 3 words in this block
!
                  IF ( LIND < 3 ) THEN
                     CALL ERR_LOG ( 6965, IUER, 'ANC_PARSE',            &
     &                       'Too few words in FMT2GPS_TIMER line '//   &
     &                       TRIM(STR)//' of the antcal file '//        &
     &                       TRIM(FILIN) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- We reached "FMT2GPS_TIMER" before getting dimensions
!
                  IF ( .NOT. ASSOCIATED ( ANC%TGPS ) ) THEN
                     CALL ERR_LOG ( 6966, IUER, 'ANC_PARSE',            &
     &                       'Malformed antenna calibration file '//    &
     &                       TRIM(FILIN)//' -- FMT2GPS_TIMER: preceeds' &
     &                     //' DIMENSION: FMT2GPS_TIMER ' )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
               END IF
!
! ------------ Update GPS counter
!
               K_TGPS = K_TGPS + 1
!
! ------------ We exceeded the defined NUM_TGPS
!
               IF ( K_TGPS > ANC%NUM_TGPS ) THEN
                  CALL IINCH ( K_TGPS, STR1 )
                  CALL IINCH ( ANC%NUM_TGPS, STR2 )
                  CALL ERR_LOG ( 6967, IUER, 'ANC_PARSE',               &
     &                    'Malformed antenna calibration file '//       &
     &                    TRIM(FILIN)//' -- too many FMT2GPS_TIMER '//  &
     &                    'lines. Got '//TRIM(STR1)//' ,expected '//    &
     &                    TRIM(STR2) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Timer Tag
!
               ANC%TGPS(K_TGPS)%TAG = CUR_LINE(IND(1,2):IND(2,2))
               ANC%TGPS_TAG(K_TGPS) = CUR_LINE(IND(1,2):IND(2,2))
!
! ------------ Board name
!
               ANC%TGPS(K_TGPS)%BOARD = CUR_LINE(IND(1,3):IND(2,3))
!##############################################################################
!
! --------- Parse information from the FMTGPS Block
!
            ELSEIF ( CUR_LINE(1:7) == 'FMTGPS:' ) THEN
!     
! ------------ This is the FMTGPS record
! ------------ Check whether the epoch index filled in this record is
!              the same as in the previous record
!              If they are not the same, then we parse time
!     
               IF ( CUR_LINE(9:15) .NE. PREV_LINE(9:15) ) THEN
! ---------------
                  CALL ERR_PASS ( IUER, IER )
                  CALL DATE_TO_TIME( CUR_LINE(17:38), MJD, UTC, IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6968, IUER, 'ANC_PARSE',            &
     &                       'Error in parsing FMTGPS date '//          &
     &                       CUR_LINE(IND(1,3):IND(2,3))//' in line '// &
     &                       TRIM(STR) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
! ---------------
                  IF ( ANC%MJD_GPS < 0 ) THEN
                     CALL ERR_PASS ( IUER, IER )
                     CALL NERS_GET_UTCMTAI ( NERS,                      &
     &                        (MJD - J2000__MJD)*86400.D0 + UTC,        &
     &                        UTC_MTAI, IER )
                     IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6969, IUER, 'ANC_PARSE',         &
     &                         'Error getting UTC_MTAI on FMTGPS Block')
                        DEALLOCATE ( BUF )
                        RETURN
                     END IF
!
! ------------------ MJD_GPS and TAI_GPS
!
                     ANC%MJD_GPS = MJD
                     ANC%TAI_GPS = UTC - UTC_MTAI
                  END IF
                  TAI = UTC - UTC_MTAI
               END IF
!
! ------------ Update the GPS counter, given the time diff. error is met
!
               IF ( K_GPS == 0 ) THEN
                  FL_UPDATE = .TRUE.
               ELSE
                  IF ( DABS((MJD - ANC%MJD_GPS)*86400.D0 +              &
     &                      (TAI - ANC%TAI_GPS) - ANC%GPS(K_GPS)%TIM)   &
     &                  > 3*ANC__EPS_TIM ) THEN
                     FL_UPDATE = .TRUE.
                  ELSE
                     FL_UPDATE = .FALSE.
                  END IF
               END IF
!
! ------------ If the counter was updated, then work on appending the 
!              GPS structure
!     
               IF ( FL_UPDATE ) THEN
                  K_GPS = K_GPS + 1
!
! --------------- Check for overflow
!
                  IF ( K_GPS > ANC%NUM_EPO ) THEN
                     CALL IINCH ( K_GPS, STR1 )
                     CALL IINCH ( ANC%NUM_PCS, STR2 )
                     CALL ERR_LOG ( 6970, IUER, 'ANC_PARSE',            &
     &                       'Malformed antenna calibration file '//    &
     &                       TRIM(FILIN)//' -- too many FMTGPS lines.'  &
     &                     //' Got '//TRIM(STR1)//' ,expected '//       &
     &                       TRIM(STR2) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
!
! --------------- Compute time
!
                  ANC%GPS(K_GPS)%TIM = (MJD - ANC%MJD_GPS)*86400.D0 +   &
     &                                 (TAI - ANC%TAI_GPS)
!
! --------------- Scan or Epoch Index
!
                  READ ( UNIT=CUR_LINE(9:15), FMT='(I8)' )              &
     &                   ANC%GPS(K_GPS)%IND_SCA
!
! --------------- Source Name
!
                  ANC%GPS(K_GPS)%SOU_NAM = CUR_LINE(90:97)
!
! --------------- Scan name
!
                  ANC%GPS(K_GPS)%SCA_NAM = CUR_LINE(100:110)
               END IF
!     
! ------------ Is the Timer tag in FMTGPS block available in list
!              of tags from FMT2GPS_TIMER block
!
               IF ( IND_TGPS < ANC%NUM_TGPS ) THEN
                  IF ( CUR_LINE(41:47)==ANC%TGPS_TAG(IND_TGPS+1) ) THEN
                     IND_TGPS = IND_TGPS + 1
                  ELSE
                     IND_TGPS = LTM_DIF (0, ANC%NUM_TGPS, ANC%TGPS_TAG, &
     &                                   TRIM(CUR_LINE(41:47)) )
                  END IF
               ELSE
                  IND_TGPS = LTM_DIF ( 0, ANC%NUM_TGPS, ANC%TGPS_TAG,   &
     &                                CUR_LINE(41:47) )
               END IF
               IF ( IND_TGPS < 1 ) THEN
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6971, IUER, 'ANC_PARSE',               &
     &                    'Tag '//CUR_LINE(41:47)//'not part of '//     &
     &                    'FMT2GPS_TIMER tags in '//TRIM(FILIN)//' . '  &
     &                    //'See line '//TRIM(STR)//' on the TPI Block')
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Parse the Formatter minus GPS Time value to the index of
!              FMG in ANC%GPS that coincides with the index of the timer
!              tag in the FMT2GPS Block. Units = s
!
               READ ( UNIT = CUR_LINE(50:65), FMT = '(1PE17.9)' )       &
     &                ANC%GPS(K_GPS)%FMG(IND_TGPS)
!     
! ------------ In an updated version of Log2ant, we include the PPS 
!
! ------------ Parse the Formatter minus GPS Time value to the index 
!              of FMG in ANC%GPS that coincides with the index of 
!              the timer tag in the FMT2GPS Block. Units = s
!
               READ ( UNIT = CUR_LINE(70:85), FMT = '(1PE17.9)')        &
     &                  ANC%GPS(K_GPS)%FMP(IND_TGPS)
!##############################################################################
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! --------  Parse info from the SEFD block
!
            ELSEIF ( CUR_LINE(1:5) == 'SEFD:' ) THEN
!     
! ------------ This is the SEFD record
! ------------ Check whether the epoch index filled in this record is
!              the same as the previous record
!              If not the same, then parse the date
!     
               IF ( CUR_LINE(7:13) .NE. PREV_LINE(7:13) ) THEN
! ---------------
                  CALL ERR_PASS ( IUER, IER )
                  CALL DATE_TO_TIME( CUR_LINE(16:37), MJD, UTC, IER )
                  IF ( IER .NE. 0 ) THEN
                     CALL CLRCH (     STR )
                     CALL INCH  ( J1, STR )
                     CALL ERR_LOG ( 6972, IUER, 'ANC_PARSE',            &
     &                       'Error in parsing SEFD date '//            &
     &                       CUR_LINE(16:37)//' in line '//TRIM(STR) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
! ---------------
                  IF ( ANC%MJD_SEFD < 0 ) THEN
                     CALL ERR_PASS ( IUER, IER )
                     CALL NERS_GET_UTCMTAI ( NERS,                      &
     &                        (MJD - J2000__MJD)*86400.D0 + UTC,        &
     &                        UTC_MTAI, IER )
                     IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6973, IUER, 'ANC_PARSE',         &
     &                          'Error getting UTC_MTAI on SEFD Block' )
                        DEALLOCATE ( BUF )
                        RETURN
                     END IF
!
! ------------------ MJD_SEFD and TAI_SEFD
!
                     ANC%MJD_SEFD = MJD
                     ANC%TAI_SEFD = UTC - UTC_MTAI
                  END IF
                  TAI = UTC - UTC_MTAI
               END IF
!
! ------------ Update the SEFD counter, given the time diff. error is met
!
               IF ( K_SEFD == 0 ) THEN
                  FL_UPDATE = .TRUE.
               ELSE
                  IF ( DABS((MJD - ANC%MJD_SEFD)*86400.D0 +              &
     &                      (TAI - ANC%TAI_SEFD) - ANC%SEFD(K_SEFD)%TIM)   &
     &                  > 3*ANC__EPS_TIM ) THEN
                     FL_UPDATE = .TRUE.
                  ELSE
                     FL_UPDATE = .FALSE.
                  END IF
               END IF
               IF ( FL_UPDATE ) THEN
                  K_SEFD = K_SEFD + 1
!
! --------------- Check for overflow
!
                  IF ( K_SEFD > ANC%NUM_EPOSEFD ) THEN
                     CALL CLRCH (     STR )
                     CALL INCH  ( J1, STR )
                     CALL IINCH ( K_SEFD, STR1 )
                     CALL IINCH ( ANC%NUM_SEFD, STR2 )
                     CALL ERR_LOG ( 6974, IUER, 'ANC_PARSE',            &
     &                      'Malformed antenna calibration file '//     &
     &                       TRIM(FILIN)//' -- too many SEFD lines. '//  &
     &                       'Got '//TRIM(STR1)//' ,expected '//        &
     &                       TRIM(STR2) )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
! ------------ Compute time
!
                  ANC%SEFD(K_SEFD)%TIM = (MJD-ANC%MJD_SEFD)*86400.D0 +  &
     &                                   (TAI-ANC%TAI_SEFD)
!
! --------------- Scan or Epoch index
!
                  READ ( UNIT = CUR_LINE(7:13),  FMT = '(I7)' )         &
     &                   ANC%SEFD(K_SEFD)%IND_SCA
!
! --------------- Azimuth [rad]     
!
                  READ ( UNIT = CUR_LINE(104:112)),  FMT = '(F9.4)' )   &
     &                   ANC%SEFD(K_SEFD)%AZ
                  ANC%SEFD(K_SEFD)%AZ = ANC%SEFD(K_SEFD)%AZ*DEG__TO__RAD
!
! --------------- Elevation [rad]
!
                  READ ( UNIT = CUR_LINE(115:122), FMT = '(F8.4)' )     &
     &                   ANC%SEFD(K_SEFD)%EL
                  ANC%SEFD(K_SEFD)%EL = ANC%SEFD(K_SEFD)%EL*DEG__TO__RAD
!
! --------------- Source Name
!
                  ANC%SEFD(K_SEFD)%SOU_NAM = TRIM(CUR_LINE(125:132))
               END IF
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@3554654565767
!
! ------------ Is the sensor tag in SEFD block available in list of tags
!              from TPS_SENSOR block
!
               IF ( IND_TPS < ANC%NUM_TPS ) THEN
                  IF ( CUR_LINE(40:45) ==  ANC%TPS_TAG(IND_TPS+1) ) THEN
                     IND_TPS = IND_TPS + 1
                  ELSE
                     IND_TPS = LTM_DIF ( 0, ANC%NUM_TPS, ANC%TPS_TAG,   &
     &                                   CUR_LINE(40:45) )
                  END IF
               ELSE
                  IND_TPS = LTM_DIF ( 0, ANC%NUM_TPS, ANC%TPS_TAG,      &
     &                                CUR_LINE(40:45) )
               END IF
               IF ( IND_TPS < 1 ) THEN
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6975, IUER, 'ANC_PARSE',               &
     &                    'Tag '//CUR_LINE(39:46)//'not part of '//     &
     &                    'TP_SENSOR tags in '//TRIM(FILIN)//' .See '   &
     &                    //'line '//TRIM(STR)//' on the TSYS Block' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Parse the SEFD value to the index of SEFD in ANC%SEFD that 
!              coincides with the index of the sensor tag in TP_SENSOR 
!              Block. Units = Jy
!
               READ ( UNIT=CUR_LINE(48:57), FMT='(F10.3)' )             &
     &                ANC%SEFD(K_SEFD)%SEFD(IND_TPS)
!
! ------------ Parse the Tsys value to the index of TSYS in ANC%SEFD that 
!              coincides with the index of the sensor tag in TP_SENSOR 
!              Block. Units = K
!
               READ ( UNIT=CUR_LINE(60:68), FMT='(F9.3)' )              &
     &                ANC%SEFD(K_SEFD)%TSYS(IND_TPS)
!
! ------------ Parse the Tcal value to the index of TCAL in ANC%SEFD that 
!              coincides with the index of the sensor tag in TP_SENSOR 
!              Block. Units = Jy
!
               READ ( UNIT=CUR_LINE(71:79), FMT='(F9.3)' )              &
     &                ANC%SEFD(K_SEFD)%TCAL(IND_TPS)
!
! ------------ Parse the Trat value to the index of TRAT in ANC%SEFD that 
!              coincides with the index of the sensor tag in TP_SENSOR 
!              Block. Units = []
!
               READ ( UNIT=CUR_LINE(82:90), FMT='(F9.3)' )              &
     &                ANC%SEFD(K_SEFD)%TRAT(IND_TPS)
!
! ------------ Parse the Gain value to the index of GAIN in ANC%SEFD that 
!              coincides with the index of the sensor tag in TP_SENSOR 
!              Block. Units = 
!
               READ ( UNIT=CUR_LINE(93:101), FMT='(F9.3)' ) &
     &                ANC%SEFD(K_SEFD)%GAIN(IND_TPS)
!
! ------------ Azimuth [rad]
!
               READ ( UNIT=CUR_LINE(104:112), FMT='(F9.4)' ) &
     &                ANC%SEFD(K_SEFD)%AZ
               ANC%SEFD(K_SEFD)%AZ = ANC%SEFD(K_SEFD)%AZ*DEG__TO__RAD
!
! ------------ Elevation [rad]
!
               READ ( UNIT=CUR_LINE(115:122),FMT='(F8.4)') &
     &                ANC%SEFD(K_SEFD)%EL                            
               ANC%SEFD(K_SEFD)%EL = ANC%SEFD(K_SEFD)%EL*DEG__TO__RAD 
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! --------- Parse information from the Opacity block
! --------- Note that Opacity is computed from the Tsys data, ergo
!           there is no need to parse the date. Just take the same
!           epoch from ANC%TTO
!     
            ELSEIF ( CUR_LINE(1:8) == 'OPACITY:' ) THEN
!     
! ------------ This is the Opacity record
! ------------ Check whether the epoch index filled in this record is
!              the same as the previous record
!     
               IF ( CUR_LINE(10:15) .NE. PREV_LINE(10:15) ) THEN
!
! --------------- If not, then use that index as the K_OPA
!
                  READ ( UNIT = CUR_LINE(10:15),  FMT='(I6)' )  K_OPA
!
! --------------- Skip any undefined indices
!
                  IF ( K_OPA < 0 ) GOTO 410 
!
! --------------- Check if this given index corresponds to the scan
!                 or epoch index that was reported when parsing TSYS
!
                  IF ( K_OPA .NE. ANC%TTO(K_OPA)%IND_SCA ) THEN
                     CALL CLRCH (     STR )
                     CALL INCH  ( J1, STR )
                     CALL IINCH ( K_OPA, STR1 )
                     CALL IINCH ( ANC%TTO(K_OPA)%IND_SCA, STR2 )
                     CALL ERR_LOG ( 6976, IUER, 'ANC_PARSE',            &
     &                       TRIM(FILIN)//' has malformed OPACITY '//   &
     &                       'indices. We expected line '//TRIM(STR)//  &
     &                       'to correspond with index '//TRIM(STR2)//  &
     &                       ' of TSYS block, not index'//TRIM(STR1)   )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
               END IF
!
! ------------ Is the sensor tag in OPACITY block available in list of tags
!              from TPS_SENSOR block
!
               IF ( IND_TPS < ANC%NUM_TPS ) THEN
                  IF ( CUR_LINE(41:46) ==  ANC%TPS_TAG(IND_TPS+1) ) THEN
                     IND_TPS = IND_TPS + 1
                  ELSE
                     IND_TPS = LTM_DIF ( 0, ANC%NUM_TPS, ANC%TPS_TAG,   &
     &                                   CUR_LINE(41:46) )
                  END IF
               ELSE
                  IND_TPS = LTM_DIF ( 0, ANC%NUM_TPS, ANC%TPS_TAG,      &
     &                                CUR_LINE(41:46) )
               END IF
               IF ( IND_TPS < 1 ) THEN
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6977, IUER, 'ANC_PARSE',               &
     &                    'Tag '//CUR_LINE(41:46)//'not part of '//     &
     &                    'TP_SENSOR tags in '//TRIM(FILIN)//' .See '// &
     &                    'line '//TRIM(STR)//' on the OPACITY Block' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Parse the OPACITY value to the index of TTO in ANC%TTO that 
!              coincides with the index of the sensor tag in TP_SENSOR 
!              Block. Units = K
!
               READ ( UNIT=CUR_LINE(48:54),FMT='(F7.4)' )               &
     &                ANC%TTO(K_OPA)%OPA(IND_TPS)
!
! --------- Parse information from the Opacity block
! --------- Note that Opacity is computed from the Tsys data, ergo
!           there is no need to parse the date. Just take the same
!           epoch from ANC%TTO
!     
            ELSEIF ( CUR_LINE(1:5) == 'TATM:' ) THEN
!     
! ------------ This is the Opacity record
! ------------ Check whether the epoch index filled in this record is
!              the same as the previous record
!     
               IF ( CUR_LINE(7:13) .NE. PREV_LINE(7:13) ) THEN
!
! --------------- If not, then use that index as the K_TATM
!
                  READ ( UNIT = CUR_LINE(7:13),  FMT='(I7)' )  K_TATM
!
! --------------- Skip any undefined indices
!
                  IF ( K_TATM < 0 ) GOTO 410
!
! --------------- Check if this given index corresponds to the scan
!                 or epoch index that was reported when parsing TSYS
!
                  IF ( K_TATM .NE. ANC%TTO(K_TATM)%IND_SCA ) THEN
                     CALL CLRCH (     STR )
                     CALL INCH  ( J1, STR )
                     CALL IINCH ( K_TATM, STR1 )
                     CALL IINCH ( ANC%TTO(K_TATM)%IND_SCA, STR2 )
                     CALL ERR_LOG ( 6976, IUER, 'ANC_PARSE',            &
     &                       TRIM(FILIN)//' has malformed TATM '//   &
     &                       'indices. We expected line '//TRIM(STR)//  &
     &                       'to correspond with index '//TRIM(STR2)//  &
     &                       ' of TSYS block, not index'//TRIM(STR1)   )
                     DEALLOCATE ( BUF )
                     RETURN
                  END IF
               END IF
!
! ------------ Is the sensor tag in TATM block available in list of tags
!              from TPS_SENSOR block
!
               IF ( IND_TPS < ANC%NUM_TPS ) THEN
                  IF ( CUR_LINE(40:45) ==  ANC%TPS_TAG(IND_TPS+1) ) THEN
                     IND_TPS = IND_TPS + 1
                  ELSE
                     IND_TPS = LTM_DIF ( 0, ANC%NUM_TPS, ANC%TPS_TAG,   &
     &                                   CUR_LINE(40:45) )
                  END IF
               ELSE
                  IND_TPS = LTM_DIF ( 0, ANC%NUM_TPS, ANC%TPS_TAG,      &
     &                                CUR_LINE(40:45) )
               END IF
               IF ( IND_TPS < 1 ) THEN
                  CALL CLRCH (     STR )
                  CALL INCH  ( J1, STR )
                  CALL ERR_LOG ( 6977, IUER, 'ANC_PARSE',               &
     &                    'Tag '//CUR_LINE(40:45)//'not part of '//     &
     &                    'TP_SENSOR tags in '//TRIM(FILIN)//' .See '// &
     &                    'line '//TRIM(STR)//' on the TATM Block' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Parse the TATM value to the index of TTO in ANC%TTO that 
!              coincides with the index of the sensor tag in TP_SENSOR 
!              Block. Units = K
!
               READ ( UNIT=CUR_LINE(47:51),FMT='(F5.1)' )               &
     &                ANC%TTO(K_TATM)%TATM(IND_TPS)
              
            END IF
         END IF
 410  CONTINUE 
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE ANC_PARSE   !#!
