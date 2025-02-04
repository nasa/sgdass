      SUBROUTINE BNC_PARSE ( FILIN, ANC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine BNC_PARSE                                                  *
! *                                                                      *
! *   read the contents of the binary file to ANC derived type           *
! *   N.B: - It is assumed every antcal file has a TSYS section, ergo    *
! *          a TPS block as well.                                        *
! *                                                                      *
! *   INPUT:                                                             *
! *       FILIN   =  Binary File                        { CHAR }         *
! *                  Currently holding - Tsys (Always)                   *
! *                                      Pcal (Sometimes                 *
! *                                                                      *
! *        IUER   =  Error Handler                      { INT, OPT }     *
! *                        If IUER=0 no error message will be printed,   *
! *                        even in the event of an error. However, for   *
! *                        other possible values, i.e. IUER=-1,-2, & -3, *
! *                        the error message will print to screen. For   *
! *                        the latter case, i.e., IUER = -3, after       *
! *                        after printing the program will terminate.    *
! *                        Default, IUER = -1                            *
! *                                                                      *
! *   OUTPUT:                                                            *
! *        ANC    =  Parsed Antenna Calibration file   { DERIVED TYPE }  *
! *                                                                      *
! *  ### 16-AUG-2022    BNC_PARSE  v1.0 (c)  N. Habana  16-AUG-2022 ###  *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE   'atp.i'
      TYPE     ( ANC__TYP   ) :: ANC      
      CHARACTER  FILIN*(*), STR1*9, STR2*9, STR3*9
      CHARACTER  STR_DOO_DATE*24, STR_TSYS_DATE*24, STR_MET_DATE*24
      CHARACTER  STR_SEFD_DATE*24, STR_GPS_DATE*24, STR_PCAL_DATE*24
      CHARACTER  STR*128, TIT*64, STR_UTCMTAI*5
      INTEGER*4  IUER, IER
      REAL*8     TIM_1ST
      REAL*8     FRQ_TSYS_ARR(ANC__MTPS), FRQ_PCAL_ARR(ANC__MPCS)
      INTEGER*1  POL_TSYS_ARR(ANC__MTPS), POL_PCAL_ARR(ANC__MPCS)
      CHARACTER  ID_TSYS_ARR(ANC__MTPS)*4, ID_PCAL_ARR(ANC__MPCS)*6
      CHARACTER  TAG_TSYS_ARR(ANC__MTPS)*8, TAG_PCAL_ARR(ANC__MPCS)*8
      INTEGER*4  NEP_ARR(ANC__MTPS), IND_ARR(ANC__MEPC)
      INTEGER*8  OFF_ARR(ANC__MTPS)
      INTEGER*4  LUN, NBT
      INTEGER*4  J1, J2, J3, J4
      REAL*4     TIM_TSYS_ARR(ANC__MEPC), TIM_PCAL_ARR(ANC__MEPC)
      REAL*4     TIM_GPS_ARR(ANC__MEPC), TIM_SEFD_ARR(ANC__MEPC)
      REAL*4     TSYS_ARR(ANC__MEPC)
      COMPLEX*8  PCAL_ARR(ANC__MEPC)
      REAL*4     AZ_ARR(ANC__MEPC), EL_ARR(ANC__MEPC)
      CHARACTER  TGPS_BOARD_ARR(ANC__MTGPS)
      REAL*4     FMGPS_ARR(ANC__MGPS), FMPPS_ARR(ANC__MGPS)
      REAL*4     SEFD_ARR(ANC__MEPC), TSYS_SEFD_ARR(ANC__MEPC)
      REAL*4     TCAL_SEFD_ARR(ANC__MEPC), GAIN_SEFD_ARR(ANC__MEPC)
      REAL*4     AZ_SEFD_ARR(ANC__MEPC), EL_SEFD_ARR(ANC__MEPC)
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, TIM_TO_DATE*23
! ---
      LUN = 18
!
! --- Open Binary File to read
!
      IUER = -1
      CALL BINF_OPEN ( FILIN, 'OLD', LUN, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
! --- Read the contents of the binary file in the order they were 
!     written in the "BNC_WRITE" routine
!
      IUER = -1
!@@@!      CALL RDBIN_ARRAY ( LUN, 'B1', LEN(BNC__LABEL), ANC%EXP_CODE,      &
!@@@!     &                   NBT, IUER )                                     ! label BNC file
      CALL RDBIN_ARRAY (LUN, 'B1',LEN(BNC__LABEL), BNC__LABEL, NBT, IUER) ! label BNC file
      IF ( NBT .NE. LEN(BNC__LABEL) ) THEN
         IUER = -1
         CALL ERR_LOG ( 5001, IUER, 'BNC_PARSE',                        &
     &           'Didnt find a BNC magic at the beginning of '//FILIN )  
         CALL EXIT ( 1 ) 
      END IF
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'B1', 16, ANC%EXP_CODE, NBT, IUER )        ! Experiment Code
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'B1', 8, ANC%STA_NAM,  NBT, IUER )         ! Station name
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'B1', 5, STR_UTCMTAI,  NBT, IUER )         ! UTC_MTAI
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'B1', 24, STR_DOO_DATE(1:24), NBT, IUER )  ! Initial DATA_ON block date
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'B1', 24, STR_TSYS_DATE(1:24), NBT, IUER ) ! Initial TSYS block date
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'B1', 24, STR_MET_DATE(1:24), NBT, IUER )  ! Initial METEO block date
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'B1', 24, STR_PCAL_DATE(1:24), NBT, IUER ) ! Initial PCAL block date
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'B1', 24, STR_GPS_DATE(1:24), NBT, IUER ) ! Initial GPS block date
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'B1', 24, STR_SEFD_DATE(1:24), NBT, IUER ) ! Initial SEFD block date
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_DOO, NBT, IUER )         ! Number of DATA_ON (scans)
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TPS, NBT, IUER )         ! Number of TP_Sensors
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TSYS, NBT, IUER )        ! Number of TSYS
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_PCS, NBT, IUER )         ! Number of PC_SENSORS
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_PCAL, NBT, IUER )        ! Number of PCAL
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TGPS, NBT, IUER )        ! Number of GPS Timers
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_GPS, NBT, IUER )         ! Number of GPS
! ---
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_SEFD, NBT, IUER )        ! Number of SEFD
!
! --- Write TSYS INFO, we assume this is always available
! --- First NUM_TPS of the Tags of the Tsys arrays
!
      IUER = -1
      CALL RDBIN_ARRAY( LUN, 'I8',ANC%NUM_TPS, TAG_TSYS_ARR, NBT, IUER)     ! Originally I4
!
! --- First NUM_TPS elements of the TSYS freq array
!
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'R8',ANC%NUM_TPS, FRQ_TSYS_ARR, NBT, IUER) 
!
! --- First NUM_TPS of the Tsys polarization array
!
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'I4',ANC%NUM_TPS, POL_TSYS_ARR, NBT, IUER)        !%%%! NOT SURE I AM NOT USING B1
!
! --- First NUM_TPS of the indices of the Tsys arrays
!
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'I4',ANC%NUM_TPS, ID_TSYS_ARR, NBT, IUER )
!
! --- First NUM_TPS of NEP_ARR
!
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_TPS, NEP_ARR, NBT, IUER )
!
! --- First NUM_TPS of OFF_ARR
!
      IUER = -1
      CALL RDBIN_ARRAY ( LUN, 'I8', ANC%NUM_TPS, OFF_ARR, NBT, IUER )
! ---
      IER = IUER
!
! --- Convert STR_UTCMTAI to real and parse to ANC type
!
      READ ( STR_UTCMTAI, '(F5.1)' ) ANC%UTC_MTAI
!
! --- Convert the dates to MJD and TAI, and parse to respcetive 
!     ANC variables
!
      IUER = 0
      CALL DATE_TO_TIME ( STR_DOO_DATE, ANC%MJD_DOO, ANC%TAI_DOO, IUER )
! ---
      IUER = 0
      CALL DATE_TO_TIME (STR_TSYS_DATE, ANC%MJD_TSYS, ANC%TAI_TSYS,IUER)
! ---
      IUER = 0
      CALL DATE_TO_TIME ( STR_MET_DATE, ANC%MJD_MET, ANC%TAI_MET, IUER )
! ---
      IUER = 0
      CALL DATE_TO_TIME (STR_PCAL_DATE, ANC%MJD_PCAL, ANC%TAI_PCAL,IUER)
! ---
      IUER = 0
      CALL DATE_TO_TIME ( STR_GPS_DATE, ANC%MJD_GPS, ANC%TAI_GPS,IUER )
! ---
      IUER = 0
      CALL DATE_TO_TIME (STR_SEFD_DATE, ANC%MJD_SEFD, ANC%TAI_SEFD,IUER)
!
! --- Allocate  ANC%TPS
!
      ALLOCATE ( ANC%TPS( ANC%NUM_TPS ), STAT = IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 5002, IUER, 'BNC_PARSE',                        &
     &           'Error in allocation memory for TP_SENSOR while '//    &
     &           'processing binary antenna calibration file '//FILIN )
         CALL EXIT ( 1 )
      END IF
!
! --- Allocate ANC%TSYS
!
      ALLOCATE ( ANC%TSYS(ANC%NUM_TSYS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 5003, IUER, 'BNC_PARSE',                        &
     &           'Error in allocation memory for ANC%TSYS while '//     &
     &           'processing binary antenna calibration file '//FILIN )
         CALL EXIT ( 1 )
      END IF
!
! --- Allocate each ANC%TSYS(i)%TSYS
!
      DO 430 J3 = 1, ANC%NUM_TSYS
         ALLOCATE ( ANC%TSYS(J3)%TSYS(ANC%NUM_TPS), STAT=IER )
         IF ( IER .NE. 0 ) THEN
            CALL ERR_LOG ( 5004, IUER, 'BNC_PARSE',                     &
     &              'Error in allocation memory for TSYS while'//       &
     &              'processing antenna calibration file '//FILIN)
            CALL EXIT ( 1 )
         END IF
430  CONTINUE
!
! --- Allocate ANC%NEP_ARR
!
      ALLOCATE ( ANC%NEP_ARR( ANC%NUM_TPS ), STAT = IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 5005, IUER, 'BNC_PARSE',                        &
     &           'Error in allocation memory for ANC%NEP_ARR while '//  &
     &           'processing binary antenna calibration file '//FILIN )
         CALL EXIT ( 1 )
      END IF
!
! --- Parse the TSYS data to ANC
!
      DO 410 J1 = 1, ANC%NUM_TPS
! ------
         ANC%TPS(J1)%TAG      =  TAG_TSYS_ARR(J1)
         ANC%TPS(J1)%SKY_FRQ  =  FRQ_TSYS_ARR(J1)
         ANC%TPS(J1)%POL      =  POL_TSYS_ARR(J1)
         ANC%TPS(J1)%ID       =  ID_TSYS_ARR(J1)
         ANC%NEP_ARR(J1)      =  NEP_ARR(J1)
!
! ------ Get time array for this TP_SENSOR
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_TSYS, TIM_TSYS_ARR,      &
     &                      NBT, IUER )
         IF ( IUER .NE. 0 ) THEN
            CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
            CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
            CALL CLRCH( STR3 ); CALL IINCH ( NEP_ARR(J1), STR3 )
            IUER = -1
            CALL ERR_LOG ( 5009, IUER, 'BNC_PARSE',                     &
     &              'Error in reading TIM_TSYS_ARR. NBT = '//           &
     &              TRIM(STR1)//' IND_FRQ = '//                         &
     &              TRIM(STR2)//' MEL = '//TRIM(STR3) )
            CALL EXIT ( 1 )
         END IF
!
! ------ Get Tsys array for this TP_SENSOR
!
         IUER = -1
         CALL RDBIN_ARRAY (LUN, 'R4', ANC%NUM_TSYS, TSYS_ARR, NBT, IUER)
         IF ( IUER .NE. 0 ) THEN
            CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
            CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
            CALL CLRCH( STR3 ); CALL IINCH ( NEP_ARR(J1), STR3 )
            IUER = -1
            CALL ERR_LOG ( 5005, IUER, 'BNC_PARSE',                     &
     &              'Error in reading TSYS_ARR. NBT = '//TRIM(STR1)//   &
     &              ' IND_FRQ = '//TRIM(STR2)//' MEL = '//TRIM(STR3) )
            CALL EXIT ( 1 )
         END IF
!
! ------ Get azimuth array for this TP_SENSOR
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_TSYS, AZ_ARR, NBT, IUER )
         IF ( IUER .NE. 0 ) THEN
            CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
            CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
            CALL CLRCH( STR3 ); CALL IINCH ( NEP_ARR(J1), STR3 )
            IUER = -1
            CALL ERR_LOG ( 5010, IUER, 'BNC_PARSE',                     &
     &              'Error in reading AZ_ARR. NBT = '//TRIM(STR1)//     &
     &              ' IND_FRQ = '//TRIM(STR2)//' MEL = '//TRIM(STR3) )
            CALL EXIT ( 1 )
         END IF
!
! ------ Get elevation array for this TP_SENSOR
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_TSYS, EL_ARR, NBT, IUER )
         IF ( IUER .NE. 0 ) THEN
            CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
            CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
            CALL CLRCH( STR3 ); CALL IINCH ( NEP_ARR(J1), STR3 )
            IUER = -1
            CALL ERR_LOG ( 5011, IUER, 'BNC_PARSE',                     &
     &              'Error in reading EL_ARR. NBT = '//TRIM(STR1)//     &
     &              ' IND_FRQ = '//TRIM(STR2)//' MEL = '//TRIM(STR3) )
            CALL EXIT ( 1 )
         END IF
!
! ------
!
         DO 420 J2 = 1, ANC%NUM_TSYS
! ---------
            ANC%TSYS(J2)%TIM       = REAL( TIM_TSYS_ARR(J2),  8 )
            ANC%TSYS(J2)%TSYS(J1)  = REAL( TSYS_ARR(J2), 8 )
            ANC%TSYS(J2)%AZ        = REAL( AZ_ARR(J2), 8 )
            ANC%TSYS(J2)%EL        = REAL( EL_ARR(J2), 8 )
 420     CONTINUE
 410  CONTINUE
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
! --- If the file has PCAL data, parse it
!
      IF ( ANC%NUM_PCS > 0 .AND. ANC%NUM_PCAL > 0 ) THEN
! ------
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I8',ANC%NUM_PCS, TAG_PCAL_ARR, NBT,   &
     &                      IUER )                                              ! Originally I4
! ------
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I8',ANC%NUM_PCS, FRQ_PCAL_ARR, NBT,   &
     &                      IUER )
! ------
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4',ANC%NUM_PCS, POL_PCAL_ARR, NBT,   &
     &                      IUER )                                         !%%%! NOT SURE I AM NOT USING B1
! ------
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I8',ANC%NUM_PCS, ID_PCAL_ARR, NBT,    &
     &                      IUER )                                                ! Originally I4
!
! ------ Allocate  ANC%PCS
!
         ALLOCATE ( ANC%PCS( ANC%NUM_PCS ), STAT = IER )
         IF ( IER .NE. 0 ) THEN
            CALL ERR_LOG ( 5006, IUER, 'BNC_PARSE',                     &
     &              'Error in allocation memory for PC_SENSOR '//       &
     &              'while processing binary antcal file '//FILIN )
            CALL EXIT ( 1 )
         END IF
!
! ------ Allocate ANC%PCAL
!
         ALLOCATE ( ANC%PCAL(ANC%NUM_PCAL), STAT=IER )
         IF ( IER .NE. 0 ) THEN
            CALL ERR_LOG ( 5007, IUER, 'BNC_PARSE',                     &
     &              'Error in allocation memory for ANC%PCAL '//        &
     &              'while processing binary antcal file '//FILIN )
            CALL EXIT ( 1 )
         END IF
!
! ------ Allocate each PCAL
!
         DO 432 J3 = 1, ANC%NUM_PCAL
            ALLOCATE ( ANC%PCAL(J3)%PCAL_CMPL(ANC%NUM_PCS), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5008, IUER, 'BNC_PARSE',                  &
     &                 'Error in allocation memory for PCAL_CMPL '//    &
     &                 'while processing antcal file '//FILIN )
               CALL EXIT ( 1 )
            END IF
 432     CONTINUE
!
! ------ Parse the PCAL data to ANC
!
         DO 412 J1 = 1, ANC%NUM_PCS
! ---------
            ANC%PCS(J1)%TAG      =  TAG_PCAL_ARR(J1)            
            ANC%PCS(J1)%SKY_FRQ  =  FRQ_PCAL_ARR(J1)
            ANC%PCS(J1)%POL      =  POL_PCAL_ARR(J1)
            ANC%PCS(J1)%ID       =  ID_PCAL_ARR(J1)
!
! --------- Get time array for this PC_SENSOR
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_PCAL, TIM_PCAL_ARR,   &
     &                         NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5012, IUER, 'BNC_PARSE',                  &
     &                 'Error in reading TIM_PCAL_ARR. '//              &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! --------- Get PCAL array for this PC_SENSOR
!
            IUER = -1
            CALL RDBIN_ARRAY (LUN, 'R8', ANC%NUM_PCAL, PCAL_ARR, NBT, IUER)
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5013, IUER, 'BNC_PARSE',                  &
     &                 'Error in reading PCAL_ARR. '//                  &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! ---------
!
            DO 422 J2 = 1, ANC%NUM_PCAL
! ------------
               ANC%PCAL(J2)%TIM  =  REAL( TIM_PCAL_ARR(J2),  8 )
               ANC%PCAL(J2)%PCAL_CMPL(J1)  = PCAL_ARR(J2)
 422        CONTINUE
 412     CONTINUE
      END IF
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!#####################################################
!
! --- If the antcal file has Formatter data
!
      IF ( ANC%NUM_TGPS > 0 .AND. ANC%NUM_GPS > 0 ) THEN
!
! ------ Copy the timer boards to array
!       
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_TGPS, TGPS_BOARD_ARR,    &
     &                      NBT, IUER )
!
! ------ Allocate ANC%TGPS
! 
         ALLOCATE ( ANC%TGPS( ANC%NUM_TGPS ), STAT = IER )
         IF ( IER .NE. 0 ) THEN
            CALL ERR_LOG ( 5014, IUER, 'BNC_PARSE',                     &
     &              'Error in allocation memory for FMT2GPS while '//   &
     &              'processing binary antcal file '//TRIM(FILIN) )
            CALL EXIT ( 1 )
         END IF
!
! ------ Allocate ANC%GPS
! 
         ALLOCATE ( ANC%GPS( ANC%NUM_GPS ), STAT = IER )
         IF ( IER .NE. 0 ) THEN
            CALL ERR_LOG ( 5015, IUER, 'BNC_PARSE',                     &
     &              'Error in allocation memory for FMTGPS while '//    &
     &              'processing binary antcal file '//TRIM(FILIN) )
            CALL EXIT ( 1 )
         END IF
!
! ------ Allocate each FMTGPS, and FMTPPS
!
         DO 413 J1 = 1, ANC%NUM_GPS
! ---------
            ALLOCATE ( ANC%GPS(J1)%FMG(ANC%NUM_TGPS), STAT = IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5016, IUER, 'BNC_PARSE',                  &
     &                 'Error in allocation memory for ANC%GPS%FMG '//  &
     &                 'processing binary antcal file '//TRIM(FILIN) )
               CALL EXIT ( 1 )
            END IF     
! ---------
            ALLOCATE ( ANC%GPS(J1)%FMP(ANC%NUM_TGPS), STAT = IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5017, IUER, 'BNC_PARSE',                  &
     &                 'Error in allocation memory for ANC%GPS%FMP '//  &
     &                 'processing binary antcal file '//TRIM(FILIN) )
               CALL EXIT ( 1 )
            END IF
 413     CONTINUE
!
! ------ 
!
         DO 423 J2 = 1, ANC%NUM_TGPS
!
! --------- Write the board information to array
!
            ANC%TGPS(J2)%BOARD = TGPS_BOARD_ARR(J2)
!
! --------- Get the time array from binary file
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_GPS, TIM_GPS_ARR,     &
     &                         NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5018, IUER, 'BNC_PARSE',                  &
     &                 'Error in reading TIM_GPS_ARR. '//               &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! --------- Get the formatter minus GPS time
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_GPS, FMGPS_ARR,       &         ! We recently changed from R8
     &                         NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5019, IUER, 'BNC_PARSE',                  &
     &                 'Error in reading FMGPS_ARR. '//                 &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! --------- Get the formatter minus PPS time
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_GPS, FMPPS_ARR,       &         ! We recently changed from R8
     &                         NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5020, IUER, 'BNC_PARSE',                  &
     &                 'Error in reading FMGPS_ARR. '//                 &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! --------- Parse to derived type
!
            DO 433 J3 = 1, ANC%NUM_GPS
! ------------
               ANC%GPS(J3)%TIM     =  REAL ( TIM_GPS_ARR(J3), 8 )
               ANC%GPS(J3)%FMG(J2) =  REAL ( FMGPS_ARR(J3),   8 )
               ANC%GPS(J3)%FMP(J2) =  REAL ( FMPPS_ARR(J3),   8 )
 433        CONTINUE 
423      CONTINUE 
      END IF
!#####################################################
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! --- If the file has SEFD data
!
      IF ( ANC%NUM_SEFD > 0 ) THEN
!
! ------ Allocate ANC%SEFD
! 
         ALLOCATE ( ANC%SEFD( ANC%NUM_SEFD ), STAT = IER )
         IF ( IER .NE. 0 ) THEN
            CALL ERR_LOG ( 5021, IUER, 'BNC_PARSE',                     &
     &              'Error in allocation memory for ANC%SEFD while '//  &
     &              'processing binary antcal file '//TRIM(FILIN) )
            CALL EXIT ( 1 )
         END IF
!
! ------ Allocate each pointer
!
         DO 414 J1 = 1, ANC%NUM_SEFD
! ---------
            ALLOCATE ( ANC%SEFD(J1)%SEFD(ANC%NUM_TPS), STAT = IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5022, IUER, 'BNC_PARSE',                  &
     &                 'Error in allocation memory for ANC%SEFD%SEFD'// &
     &                 ' processing binary antcal file '//TRIM(FILIN) )
               CALL EXIT ( 1 )
            END IF     
! ---------
            ALLOCATE ( ANC%SEFD(J1)%TSYS(ANC%NUM_TPS), STAT = IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5023, IUER, 'BNC_PARSE',                  &
     &                 'Error in allocation memory for ANC%SEFD%TSYS'// &
     &                 ' processing binary antcal file '//TRIM(FILIN) )
               CALL EXIT ( 1 )
            END IF
! ---------
            ALLOCATE ( ANC%SEFD(J1)%TCAL(ANC%NUM_TPS), STAT = IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5025, IUER, 'BNC_PARSE',                  &
     &                 'Error in allocation memory for ANC%SEFD%TCAL'// &
     &                 ' processing binary antcal file '//TRIM(FILIN) )
               CALL EXIT ( 1 )
            END IF
! ---------
            ALLOCATE ( ANC%SEFD(J1)%GAIN(ANC%NUM_TPS), STAT = IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5025, IUER, 'BNC_PARSE',                  &
     &                 'Error in allocation memory for ANC%SEFD%GAIN'// &
     &                 ' processing binary antcal file '//TRIM(FILIN) )
               CALL EXIT ( 1 )
            END IF
 414     CONTINUE
!
! ------
!
         DO 424 J2 = 1, ANC%NUM_TPS
!
! --------- Get the time array from binary file
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_SEFD, TIM_SEFD_ARR,     &
     &                         NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5026, IUER, 'BNC_PARSE',                  &
     &                 'Error in reading TIM_SEFD_ARR. '//               &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! ---------
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_SEFD, SEFD_ARR,       &
     &                         NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5026, IUER, 'BNC_PARSE',                  &
     &                 'Error in reading SEFD_ARR. '//                  &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! ---------
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_SEFD, TSYS_SEFD_ARR,  &
     &                         NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5026, IUER, 'BNC_PARSE',                  &
     &                 'Error in reading TSYS_SEFD_ARR. '//             &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! ---------
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_SEFD, TCAL_SEFD_ARR,  &
     &                         NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5026, IUER, 'BNC_PARSE',                  &
     &                 'Error in reading TCAL_SEFD_ARR. '//             &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! ---------
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_SEFD, GAIN_SEFD_ARR,  &
     &                         NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5026, IUER, 'BNC_PARSE',                  &
     &                 'Error in reading GAIN_SEFD_ARR. '//             &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! ---------
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_SEFD, AZ_SEFD_ARR,    &
     &                         NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5026, IUER, 'BNC_PARSE',                  &
     &                 'Error in reading AZ_SEFD_ARR. '//               &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! ---------
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_SEFD, EL_SEFD_ARR,    &
     &                         NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5026, IUER, 'BNC_PARSE',                  &
     &                 'Error in reading EL_SEFD_ARR. '//               &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! ---------
!
            DO 434 J3 = 1, ANC%NUM_SEFD
               ANC%SEFD(J3)%TIM      =  REAL ( TIM_SEFD_ARR(J3), 8 )
               ANC%SEFD(J3)%SEFD(J2) =  REAL ( SEFD_ARR(J3), 8 )
               ANC%SEFD(J3)%TSYS(J2) =  REAL ( TSYS_SEFD_ARR(J3), 8 )
               ANC%SEFD(J3)%TCAL(J2) =  REAL ( TCAL_SEFD_ARR(J3), 8 )
               ANC%SEFD(J3)%GAIN(J2) =  REAL ( GAIN_SEFD_ARR(J3), 8 )
               ANC%SEFD(J3)%AZ       =  REAL ( AZ_SEFD_ARR(J3), 8 )
               ANC%SEFD(J3)%EL       =  REAL ( EL_SEFD_ARR(J3), 8 )
 434        CONTINUE
 424     CONTINUE
      END IF
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

      CALL ERR_PASS (IER, IUER )      
!
! --- Close binary file
!
      CALL BINF_CLOSE ( LUN, IUER )
! ---
      RETURN
      END  SUBROUTINE BNC_PARSE  !#!
