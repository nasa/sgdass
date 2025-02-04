      SUBROUTINE BNC_WRITE ( ANC, FILOUT, IUER )
!
! ************************************************************************
! *                                                                      *
! *   Routine  BNC_WRITE                                                 *
! *                                                                      *
! *   Convert the contents of the antenna calibration file in ANC to a   *
! *   binary file.                                                       *
! *   N.B: - It is assumed every antcal file has a TSYS section, ergo    *
! *          a TPS block as well.                                        *
! *                                                                      *
! *   INPUT:                                                             *
! *        ANC    =  Parsed Antenna Calibration file   { DERIVED TYPE }  *
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
! *       FILOUT  = Binary File                         { CHAR }         *
! *                 Currently holding - Tsys (Always)                    *
! *                                     Pcal (Sometimes)                 *
! *                                                                      *
! *  ### 13-JUL-2021   BNC_WRITE    v2.2 (c) N. Habana  15-Nov-2023 ###  *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE   'atp.i'
      TYPE     ( ANC__TYP   ) :: ANC      
      CHARACTER  FILOUT*(*)
      INTEGER*4  IUER
      CHARACTER  STR*128, STR_UTCMTAI*5
      INTEGER*4  NEP_ARR(ANC__MTPS)
      INTEGER*8  OFF_ARR(ANC__MTPS)
      REAL*8     TIM_1ST
      REAL*8     FRQ_TSYS_ARR(ANC__MTPS), FRQ_PCAL_ARR(ANC__MPCS)
      REAL*4     TIM_TSYS_ARR(ANC__MEPC), TIM_PCAL_ARR(ANC__MEPC)
      REAL*4     TIM_GPS_ARR(ANC__MEPC), TIM_SEFD_ARR(ANC__MEPC)
      REAL*4     TSYS_ARR(ANC__MEPC)
      COMPLEX*8  PCAL_ARR(ANC__MEPC)
      REAL*4     AZ_ARR(ANC__MEPC),  EL_ARR(ANC__MEPC)
      INTEGER*1  POL_TSYS_ARR(ANC__MTPS), POL_PCAL_ARR(ANC__MPCS)
      CHARACTER  ID_TSYS_ARR(ANC__MTPS)*4, ID_PCAL_ARR(ANC__MPCS)*6
      CHARACTER  TAG_TSYS_ARR(ANC__MTPS)*8, TAG_PCAL_ARR(ANC__MPCS)*8
      CHARACTER  STR_PCAL_DATE*32, STR_GPS_DATE*32, STR_SEFD_DATE*32
      CHARACTER  STR_DOO_DATE*32, STR_MET_DATE*32, STR_TSYS_DATE*32
      CHARACTER  TGPS_BOARD_ARR(ANC__MTGPS)
      REAL*4     FMGPS_ARR(ANC__MGPS), FMPPS_ARR(ANC__MGPS)
      REAL*4     SEFD_ARR(ANC__MEPC), TSYS_SEFD_ARR(ANC__MEPC)
      REAL*4     TCAL_SEFD_ARR(ANC__MEPC), GAIN_SEFD_ARR(ANC__MEPC)
      REAL*4     AZ_SEFD_ARR(ANC__MEPC), EL_SEFD_ARR(ANC__MEPC)
      INTEGER*4  J1, J2, J3, J4, OFF_BEG, KP, LUN, IER
      CHARACTER, EXTERNAL :: TIM_TO_DATE*30, MJDSEC_TO_DATE*30
!
! --- Offset beginning
!
      OFF_BEG = LEN(BNC__LABEL) + 16 + 8 + 3*24 + 4*1 +                 &
     &          8*ANC%NUM_TPS + 1*ANC%NUM_TPS +                         &
     &          4*ANC%NUM_TPS + 4*ANC%NUM_TPS + 8*ANC%NUM_TPS + 12*4
!
! --- Convert UTC_MTAI to charater
!
      WRITE ( STR_UTCMTAI, '(F5.1)' ) ANC%UTC_MTAI
!
! --- Initial time [place holder]
!
      TIM_1ST = 1.D30
!
! --- Go through the ANC derived type by reading each TPS sensor.
!
      DO 410 J1=1,ANC%NUM_TPS
! ------ 
         KP = 0         ! Counter
!
! ------ Find the initial time for this, where the value of Tsys
!        is greater than the min. Which effectively means where 
!        Tsys value is not a filler (-99.9) 
!
         DO 420 J2=1,ANC%NUM_TSYS
            IF ( ANC%TSYS(J2)%TSYS(J1) > ANC__TSYS_MIN ) THEN
                 KP = KP + 1
                 TIM_1ST = MIN ( TIM_1ST, ANC%TSYS(J2)%TIM )
            END IF
 420     CONTINUE 
!
! ------ How many Tsys values using this TP_SENSOR are above 
!        ANC__TSYS_MIN
!
         NEP_ARR(J1) = KP
! ------
         IF ( NEP_ARR(J1) == 0 ) NEP_ARR(J1) = 1
!
! ------ ??Where do the offsets begin??
!
         IF ( J1 == 1 ) THEN
              OFF_ARR(J1) = OFF_BEG
         ELSE
              OFF_ARR(J1) = OFF_ARR(J1-1) + 4*(4*NEP_ARR(J1-1) + 4)
         END IF
!
! ------ Sky frequency at the current TP_Sensor
!
         FRQ_TSYS_ARR(J1) = ANC%TPS(J1)%SKY_FRQ
!
! ------ Polarization at the current TP_Sensor
!
         POL_TSYS_ARR(J1) = ANC%TPS(J1)%POL
!
! ------ Sensor ID at the current TP_Sensor
!
         ID_TSYS_ARR(J1)  = ANC%TPS(J1)%ID
!
! ------ Sensor Tag at the current TP_Sensor
!
         TAG_TSYS_ARR(J1)  = ANC%TPS(J1)%TAG
 410  CONTINUE
!
! --- If a PCAL section exists, then Go through the ANC derived type by 
!     reading each PC sensor.
!
      IF ( ANC%NUM_PCS > 0  .AND. ANC%NUM_PCAL > 0 ) THEN
         DO 412 J1=1,ANC%NUM_PCS
!
! --------- Sky frequency at the current PC_Sensor
!
            FRQ_PCAL_ARR(J1) = ANC%PCS(J1)%SKY_FRQ
!
! --------- Polarization at the current PC_Sensor
!
            POL_PCAL_ARR(J1) = ANC%PCS(J1)%POL
!
! --------- Sensor ID at the current PC_Sensor
!
            ID_PCAL_ARR(J1)  = ANC%PCS(J1)%ID
!
! --------- Sensor TAG at the current PC_Sensor
!
            TAG_PCAL_ARR(J1)  = ANC%PCS(J1)%TAG
 412     CONTINUE 
      END IF
!
! --- Convert initial DATA_ON date to string
!
      IER = 0
      STR_DOO_DATE  = MJDSEC_TO_DATE ( ANC%MJD_DOO, ANC%TAI_DOO, IER )
!
! --- Convert initial TSYS date to string
!
      IER = 0
      STR_TSYS_DATE = MJDSEC_TO_DATE ( ANC%MJD_TSYS, ANC%TAI_TSYS, IER )
!
! --- Convert initial METEO date to string
!
      IER = 0
      STR_MET_DATE  = MJDSEC_TO_DATE ( ANC%MJD_MET, ANC%TAI_MET, IER )
!
! --- Convert initial PCAL date to string
!
      IER = 0
      STR_PCAL_DATE = MJDSEC_TO_DATE ( ANC%MJD_PCAL, ANC%TAI_PCAL, IER )
!
! --- Convert initial GPS date to string
!
      IER = 0
      STR_GPS_DATE = MJDSEC_TO_DATE ( ANC%MJD_GPS, ANC%TAI_GPS, IER )
!
! --- Convert initial SEFD date to string
!
      IER = 0
      STR_SEFD_DATE = MJDSEC_TO_DATE ( ANC%MJD_SEFD, ANC%TAI_SEFD, IER )
! --- 
      LUN = 18                 ! File unit number  
!
! --- Open binary file to write to on unit LUN
!
      IER = -1
      CALL BINF_OPEN ( FILOUT, 'UNKNOWN', LUN, IER )
!
! --- Write to file
!     When later reading these, be sure to read them in this same order
!
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'B1', LEN(BNC__LABEL), BNC__LABEL, IER )  ! label to BNC file
! ---
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'B1', 16, ANC%EXP_CODE, IER )             ! Experiment code
! ---
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'B1',  8, ANC%STA_NAM,  IER )             ! Station Name
! ---
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'B1',  5, STR_UTCMTAI,  IER )             ! UTC_MTAI
! ---
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'B1', 24, STR_DOO_DATE(1:24), IER )       ! Initial DATA_ON block date
! ---
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'B1', 24, STR_TSYS_DATE(1:24), IER )      ! Initial TSYS block date
! ---
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'B1', 24, STR_MET_DATE(1:24), IER )       ! Initial METEO block date
! ---
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'B1', 24, STR_PCAL_DATE(1:24), IER )      ! Initial PCAL block date
! ---
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'B1', 24, STR_GPS_DATE(1:24), IER )       ! Initial GPS block date
! ---
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'B1', 24, STR_SEFD_DATE(1:24), IER )      ! Initial SEFD block date
! ---
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_DOO, IER )              ! Number of DATA_ON (Scans)
! ---
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TPS, IER )              ! Number of TP_SENSORS
! ---
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TSYS, IER )             ! Number of TSYS
! ---
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_PCS, IER )              ! Number of PC_SENSORS
! ---
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_PCAL, IER )             ! Number of PCAL
! ---
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TGPS, IER )             ! Number of GPS Timers
! ---
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_GPS, IER )              ! Number of GPS
! ---
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_SEFD, IER )             ! Number of SEFD
!
! --- GATHER TSYS INFO
! --- First NUM_TPS elements of TAG_TSYS_ARR
!
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'I8', ANC%NUM_TPS, TAG_TSYS_ARR,  IER )   ! originally I4
!
! --- First NUM_TPS elements of FRQ_TSYS_ARR
!
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'R8', ANC%NUM_TPS, FRQ_TSYS_ARR, IER )    
!
! --- First NUM_TPS elements of POL_TSYS_ARR
!
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'I4', ANC%NUM_TPS, POL_TSYS_ARR, IER )        !%%%! NOT SURE I AM NOT USING B1
!
! --- First NUM_TPS elements of ID_TSYS_ARR
!
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'I4', ANC%NUM_TPS, ID_TSYS_ARR,  IER )
!
! --- First NUM_TPS elements of NEP_ARR
!
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'I4', ANC%NUM_TPS, NEP_ARR, IER )
!
! --- First NUM_TPS elements of OFF_ARR
!
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'I8', ANC%NUM_TPS, OFF_ARR, IER )
!
! --- Write the content of timestamp against the number of TP_sensors
!     N.B: Each point is wrt to the current TP_SENSOR.
!          So at the end of the day, you will have NUM_TPS 
!          of each of the arrays.
!
      DO 430 J3 = 1, ANC%NUM_TPS
!
! ------ Grab the Time, Tsys, azimuth, and elevation to their
!        respective arrays. 
!
         DO 440 J4 = 1, ANC%NUM_TSYS
            TIM_TSYS_ARR(J4)  = ANC%TSYS(J4)%TIM - TIM_1ST
            TSYS_ARR(J4) = ANC%TSYS(J4)%TSYS(J3)
            AZ_ARR(J4)   = ANC%TSYS(J4)%AZ
            EL_ARR(J4)   = ANC%TSYS(J4)%EL
 440     CONTINUE 
!
! ------ Write the arrays to file
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_TSYS, TIM_TSYS_ARR, IER )
! ------
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_TSYS, TSYS_ARR, IER )
! ------
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_TSYS, AZ_ARR, IER )
! ------
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_TSYS, EL_ARR, IER )
430   CONTINUE 
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
! --- If the antcal file has PCAL data, then write it to the binary file
!
      IF ( ANC%NUM_PCS > 0 .AND. ANC%NUM_PCAL > 0 ) THEN
!
! ------ First NUM_PCS elements of TAG_PCAL_ARR
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I8', ANC%NUM_PCS, TAG_PCAL_ARR,  IER )        ! Originally I4
!
! ------ First NUM_PCS elements of FRQ_PCAL_ARR
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I8', ANC%NUM_PCS, FRQ_PCAL_ARR, IER )        
!
! ------ First NUM_PCS elements of POL_PCAL_ARR
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', ANC%NUM_PCS, POL_PCAL_ARR, IER )        !%%%! NOT SURE I AM NOT USING B1
!
! ------ First NUM_PCS elements of ID_PCAL_ARR
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I8', ANC%NUM_PCS, ID_PCAL_ARR,  IER )        ! Originally I4
!
! ------ Deal with the Phase-Cal time and values
! 
         DO 432 J3 = 1, ANC%NUM_PCS
!
! --------- Grab the Time, and Pcal to their respective arrays. 
!
            DO 442 J4 = 1, ANC%NUM_PCAL
               TIM_PCAL_ARR(J4)  = ANC%PCAL(J4)%TIM
               PCAL_ARR(J4) = ANC%PCAL(J4)%PCAL_CMPL(J3)
 442        CONTINUE 
!
! --------- Write the arrays to file
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'R4',ANC%NUM_PCAL, TIM_PCAL_ARR, IER)
! ---------
            IER = -1
            CALL WRBIN_ARRAY ( LUN, 'R8', ANC%NUM_PCAL, PCAL_ARR, IER )
 432     CONTINUE
      END IF
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!#####################################################
!
! --- If the antcal file has GPS data
!
      IF ( ANC%NUM_TGPS > 0 .AND. ANC%NUM_GPS > 0 ) THEN
! -------
         DO 413 J1 = 1, ANC%NUM_TGPS
!
! --------- GPS Board at the current TGPS_Timer
!
            TGPS_BOARD_ARR(J1) = ANC%TGPS(J1)%BOARD
 413     CONTINUE
!
! ------ Write the timer boards to file
!
         IER = -1
         CALL WRBIN_ARRAY (LUN, 'I4', ANC%NUM_TGPS, TGPS_BOARD_ARR, IER )
!
! ------ Deal with the GPS time, formatter differences
!
         DO 423 J2 = 1, ANC%NUM_TGPS
!
! --------- Grab the time, fmt - gps, and fmt - pps to their 
!           respective arrays
!
            DO 433 J3 = 1, ANC%NUM_GPS
               TIM_GPS_ARR(J3) = ANC%GPS(J3)%TIM
               FMGPS_ARR(J3)   = ANC%GPS(J3)%FMG(J2)
               FMPPS_ARR(J3)   = ANC%GPS(J3)%FMP(J2)
433        CONTINUE
!
! --------- Write the arrays to file
!
            IER = -1
            CALL WRBIN_ARRAY ( LUN, 'R4',ANC%NUM_GPS, TIM_GPS_ARR, IER )
! ---------
            IER = -1
            CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_GPS, FMGPS_ARR, IER )         ! We recently changed from R8
! ---------
            IER = -1
            CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_GPS, FMPPS_ARR, IER )         ! We recently changed from R8
 423     CONTINUE
      END IF
!#####################################################
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! --- If the file has SEFD data
!
      IF ( ANC%NUM_SEFD > 0 ) THEN
!
! ------ 
!
         DO 414 J1 = 1, ANC%NUM_TPS
!
! --------- Grab the Time, SEFD, Tsys, Tcal, Trat, azimuth, and 
!           elevation to their respective arrays.
!
            DO 424 J2 = 1, ANC%NUM_SEFD
               TIM_SEFD_ARR(J2)   =  ANC%SEFD(J2)%TIM
               SEFD_ARR(J2)       =  ANC%SEFD(J2)%SEFD(J1)
               TSYS_SEFD_ARR(J2)  =  ANC%SEFD(J2)%TSYS(J1)
               TCAL_SEFD_ARR(J2)  =  ANC%SEFD(J2)%TCAL(J1)
               GAIN_SEFD_ARR(J2)  =  ANC%SEFD(J2)%GAIN(J1)
               AZ_SEFD_ARR(J2)    =  ANC%SEFD(J2)%AZ
               EL_SEFD_ARR(J2)    =  ANC%SEFD(J2)%EL
 424        CONTINUE 
!
! --------- Write the arrays to file
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'R4',ANC%NUM_SEFD, TIM_SEFD_ARR, IER)
! ---------
            IER = -1
            CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_SEFD, SEFD_ARR, IER )
! ---------
            IER = -1
            CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_SEFD, TSYS_SEFD_ARR,  &
     &                         IER )
! ---------
            IER = -1
            CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_SEFD, TCAL_SEFD_ARR,  &
     &                         IER )
! ---------
            IER = -1
            CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_SEFD, GAIN_SEFD_ARR,  &
     &                         IER )
! ---------
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'R4', ANC%NUM_SEFD, AZ_SEFD_ARR, IER)
! ---------
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'R4', ANC%NUM_SEFD, EL_SEFD_ARR, IER)

 414  CONTINUE 
      END IF
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! --- Close the file
!
      IER = -1
      CALL BINF_CLOSE ( LUN, IER )
! ---
      CALL ERR_LOG ( 0, IUER )
! ---
      RETURN
      END  SUBROUTINE  BNC_WRITE  !#!#
