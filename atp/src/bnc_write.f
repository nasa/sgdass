      SUBROUTINE BNC_WRITE ( ANC, FILOUT, IUER )
!
! ************************************************************************
! *                                                                      *
! *   Routine  BNC_WRITE                                                 *
! *                                                                      *
! *   Convert the contents of the antenna calibration file in ANC to a   *
! *   binary file.                                                       *
! *   N.B: No blocks are assumed to be inherent to the anc file          *
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
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 13-JUL-2021   BNC_WRITE    v4.1 (d) N. Habana  21-SEP-2025 ###  *
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
      REAL*8     FRQ_TPS_ARR(ANC__MTPS), FRQ_PCS_ARR(ANC__MPCS)
      REAL*8     FRQ_ARR_IFTP(ANC__MTPS), FRQ_ARR_LOTP(ANC__MTPS)
      REAL*8     FRQ_ARR_BWTP(ANC__MTPS)
      REAL*4     TIM_TTO_ARR(ANC__MEPC), TIM_PCAL_ARR(ANC__MEPC)
      REAL*4     TIM_GPS_ARR(ANC__MEPC), TIM_SEFD_ARR(ANC__MEPC)
      REAL*4     TIM_CBL_ARR(ANC__MEPC), TIM_TPI_ARR(ANC__MEPC)
      REAL*4     TIM_MET_ARR(ANC__MEPC)
      REAL*4     TIM_DOO1_ARR(ANC__MEPC), TIM_DOO2_ARR(ANC__MEPC)
      REAL*4     DUR_TTO_ARR(ANC__MEPC), DUR_PCAL_ARR(ANC__MEPC)
      REAL*4     DUR_TPI_ARR(ANC__MEPC)
      REAL*4     TSYS_ARR(ANC__MEPC), OPA_ARR(ANC__MEPC)
      REAL*4     TATM_ARR(ANC__MEPC)
      INTEGER*4  TPION_ARR(ANC__MEPC), TPIOFF_ARR(ANC__MEPC)
      INTEGER*4  TPIZERO_ARR(ANC__MEPC)
      REAL*4     PRES_MET_ARR(ANC__MEPC), HUMID_MET_ARR(ANC__MEPC)
      REAL*4     TEMP_MET_ARR(ANC__MEPC)
      COMPLEX*8  PCAL_ARR(ANC__MEPC)
      REAL*4     AZ_TTO_ARR(ANC__MEPC), EL_TTO_ARR(ANC__MEPC)
      INTEGER*1  POL_TPS_ARR(ANC__MTPS), POL_PCS_ARR(ANC__MPCS)
      INTEGER*1  SSB_TPS_ARR(ANC__MTPS), SSB_PCS_ARR(ANC__MPCS)
      INTEGER*1  NSB_TPS_ARR(ANC__MTPS), NSB_PCS_ARR(ANC__MPCS)
      INTEGER*4  IFIDX_TPS_ARR(ANC__MTPS), IFIDX_PCS_ARR(ANC__MTPS)
      INTEGER*4  INDSC_MET_ARR(ANC__MEPC), INDSC_TTO_ARR(ANC__MEPC)
      INTEGER*4  INDSC_PCAL_ARR(ANC__MEPC), INDSC_TPI_ARR(ANC__MEPC)
      INTEGER*4  INDSC_GPS_ARR(ANC__MEPC), INDSC_SEFD_ARR(ANC__MEPC)
      CHARACTER  ID_TPS_ARR(ANC__MTPS)*4, ID_PCS_ARR(ANC__MPCS)*6
      CHARACTER  TAG_TPS_ARR(ANC__MTPS)*8, TAG_PCS_ARR(ANC__MPCS)*8
      CHARACTER  TAG_GPS_ARR(ANC__MTPS)*8
      CHARACTER  STR_PCAL_DATE*32, STR_GPS_DATE*32, STR_SEFD_DATE*32
      CHARACTER  STR_DOO_DATE*32, STR_MET_DATE*32, STR_TTO_DATE*32
      CHARACTER  STR_CBL_DATE*32, STR_TPI_DATE*32
      CHARACTER  PRV_ARR(ANC__MPRV)*256
      CHARACTER  TGPS_BOARD_ARR(ANC__MTGPS)
      CHARACTER  SOU_TTO_ARR(ANC__MEPC)*8, SOU_PCAL_ARR(ANC__MEPC)*8
      CHARACTER  SOU_TPI_ARR(ANC__MEPC)*8, SOU_SEFD_ARR(ANC__MEPC)*8
      CHARACTER  SOU_CBL_ARR(ANC__MEPC)*8, SOU_MET_ARR(ANC__MEPC)*8
      CHARACTER  SOU_DOO_ARR(ANC__MEPC)*8, SOU_GPS_ARR(ANC__MEPC)*8
      CHARACTER  SCA_TTO_ARR(ANC__MEPC)*16, SCA_PCAL_ARR(ANC__MEPC)*16
      CHARACTER  SCA_TPI_ARR(ANC__MEPC)*16
      CHARACTER  SCA_CBL_ARR(ANC__MEPC)*16, SCA_MET_ARR(ANC__MEPC)*16
      CHARACTER  SCA_DOO_ARR(ANC__MEPC)*16, SCA_GPS_ARR(ANC__MEPC)*16
      REAL*4     FMGPS_ARR(ANC__MGPS), FMPPS_ARR(ANC__MGPS)
      REAL*4     SEFD_ARR(ANC__MEPC), TSYS_SEFD_ARR(ANC__MEPC)
      REAL*4     TCAL_SEFD_ARR(ANC__MEPC), GAIN_SEFD_ARR(ANC__MEPC)
      REAL*4     TRAT_SEFD_ARR(ANC__MEPC)
      REAL*4     AZ_SEFD_ARR(ANC__MEPC), EL_SEFD_ARR(ANC__MEPC)
      REAL*4     AZ_TPI_ARR(ANC__MEPC), EL_TPI_ARR(ANC__MEPC)
      REAL*4     CBL_DEL_ARR(ANC__MEPC)
      INTEGER*4  J1, J2, J3, J4, J5, OFF_BEG, KP, LUN, NBT, IER
      INTEGER*4  IP1
      CHARACTER, EXTERNAL :: TIM_TO_DATE*30, MJDSEC_TO_DATE*30
      INTEGER*4  IANC_FMT_VERS
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Is this a recognised Format
!
      IF ( TRIM(ANC%ANTCAL_FMT) == TRIM( ANTCAL__FMT_1) ) THEN
         IANC_FMT_VERS = 1
!     
! ------ Offset beginning
!
         OFF_BEG = LEN(BNC__LABEL_1) + 16 + 8 + 3*24 + 4*1 +            &
     &             8*ANC%NUM_TPS + 1*ANC%NUM_TPS +                      &
     &             4*ANC%NUM_TPS + 4*ANC%NUM_TPS + 8*ANC%NUM_TPS + 12*4
      ELSEIF ( TRIM(ANC%ANTCAL_FMT) == TRIM(ANTCAL__FMT_2) ) THEN
         IANC_FMT_VERS = 2
!     
! ------ Offset beginning
!
         OFF_BEG = LEN(BNC__LABEL_2) + 16 + 8 + 3*24 + 4*1 +            &
     &             8*ANC%NUM_TPS + 1*ANC%NUM_TPS +                      &
     &             4*ANC%NUM_TPS + 4*ANC%NUM_TPS + 8*ANC%NUM_TPS + 12*4
      ELSE
         CALL ERR_LOG ( 8001, IUER, 'ANC_PARSE',                        &
     &           'Unsupported version of ANTCAL FMT'//                  &
     &           TRIM(ANC%ANTCAL_FMT) )
         RETURN
      END IF

      IF ( IANC_FMT_VERS == 1 ) THEN 
!
! ------ Convert UTC_MTAI to charater
!
         WRITE ( STR_UTCMTAI, '(F5.1)' ) ANC%UTC_MTAI
!
! ------ Initial time [place holder]
!
         TIM_1ST = 1.D30
! ------
         IF ( (ANC%NUM_TPS > 0) .AND. (ANC%NUM_TSYS > 0) ) THEN
!     
! --------- Go through the ANC derived type by reading each TPS sensor.
!
            DO 410 J1=1,ANC%NUM_TPS
! ------------
               KP = 0           ! Counter
!
! ------------ Find the initial time for this, where the value of
!              Tsys is greater than the min. Which effectively means
!              means where Tsys value is not a filler (-99.9)
!
               DO 420 J2=1,ANC%NUM_TSYS
                  IF ( ANC%TTO(J2)%TSYS(J1) > ANC__TSYS_MIN ) THEN
                     KP = KP + 1
                     TIM_1ST = MIN ( TIM_1ST, ANC%TTO(J2)%TIM )
                  END IF
 420           CONTINUE               
!
! ------------ How many Tsys values using this TP_SENSOR are above 
!              ANC__TSYS_MIN
!
               NEP_ARR(J1) = KP
! ------------
               IF ( NEP_ARR(J1) == 0 ) NEP_ARR(J1) = 1
!
! ------------ ??Where do the offsets begin??
!
               IF ( J1 == 1 ) THEN
                  OFF_ARR(J1) = OFF_BEG
               ELSE
                  OFF_ARR(J1) = OFF_ARR(J1-1) + 4*(4*NEP_ARR(J1-1) + 4)
               END IF
!
! ------------ Sky frequency at the current TP_Sensor
!
               FRQ_TPS_ARR(J1) = ANC%TPS(J1)%SKY_FRQ
!
! ------------ Polarization at the current TP_Sensor
!
               POL_TPS_ARR(J1) = ANC%TPS(J1)%POL
!
! ------------ Sensor ID at the current TP_Sensor
!
               ID_TPS_ARR(J1)  = ANC%TPS(J1)%ID
!
! ------------ Sensor Tag at the current TP_Sensor
!
               TAG_TPS_ARR(J1)  = ANC%TPS(J1)%TAG
 410        CONTINUE
         END IF
!     
! ------ If a PCAL section exists, then Go through the ANC derived 
!        type by reading each PC sensor.
!
         IF ( ANC%NUM_PCS > 0  .AND. ANC%NUM_PCAL > 0 ) THEN
            DO 412 J1=1,ANC%NUM_PCS
!
! ------------ Sky frequency at the current PC_Sensor
!
               FRQ_PCS_ARR(J1) = ANC%PCS(J1)%SKY_FRQ
!
! ------------ Polarization at the current PC_Sensor
!
               POL_PCS_ARR(J1) = ANC%PCS(J1)%POL
!
! ------------ Sensor ID at the current PC_Sensor
!
               ID_PCS_ARR(J1)  = ANC%PCS(J1)%ID
!
! ------------ Sensor TAG at the current PC_Sensor
!
               TAG_PCS_ARR(J1)  = ANC%PCS(J1)%TAG
 412        CONTINUE
         END IF
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! --- If the file has CABLE data
!
         IF ( ANC%NUM_CBL > 0 ) THEN
!
! ------ Grab the Time, and cable delays to their respective arrays
!
            DO 415 J1 = 1, ANC%NUM_CBL
               TIM_CBL_ARR(J1)  =  ANC%CBL(J1)%TIM
               CBL_DEL_ARR(J1)  =  ANC%CBL(J1)%DELAY
 415        CONTINUE
         END IF
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! --- If the file has MET data
!
         IF ( ANC%NUM_MET > 0 ) THEN
!
! ------ Grab the time, pressure, temperature, and relative humidity
!
            DO 416 J1 = 1, ANC%NUM_MET
               TIM_MET_ARR(J1)  = ANC%MET(J1)%TIM
               PRES_MET_ARR(J1) = ANC%MET(J1)%PRES
               TEMP_MET_ARR(J1) = ANC%MET(J1)%TEMP
               HUMID_MET_ARR(J1) = ANC%MET(J1)%HUMID
 416        CONTINUE
         END IF
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     
! ------ Convert initial DATA_ON date to string
!
         IER = 0
         STR_DOO_DATE = MJDSEC_TO_DATE ( ANC%MJD_DOO,                   &
     &                         ANC%TAI_DOO + ANC%UTC_MTAI, IER )
!
! ------ Convert initial TSYS date to string
!
         IER = 0
         STR_TTO_DATE = MJDSEC_TO_DATE (ANC%MJD_TTO,                   &
     &                         ANC%TAI_TTO + ANC%UTC_MTAI, IER)
!
! ------ Convert initial METEO date to string
!
         IER = 0
         STR_MET_DATE = MJDSEC_TO_DATE ( ANC%MJD_MET,                   &
     &                        ANC%TAI_MET + ANC%UTC_MTAI, IER )
!
! ------ Convert initial PCAL date to string
!
         IER = 0
         STR_PCAL_DATE = MJDSEC_TO_DATE (ANC%MJD_PCAL,                  &
     &                         ANC%TAI_PCAL + ANC%UTC_MTAI, IER )
!
! ------ Convert initial GPS date to string
!
         IER = 0
         STR_GPS_DATE = MJDSEC_TO_DATE ( ANC%MJD_GPS,                   &
     &                        ANC%TAI_GPS + ANC%UTC_MTAI, IER )
!
! ------ Convert initial SEFD date to string
!
         IER = 0
         STR_SEFD_DATE = MJDSEC_TO_DATE (ANC%MJD_SEFD,                  &
     &                         ANC%TAI_SEFD + ANC%UTC_MTAI, IER )
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------ Convert initial CABLE date to string
!
         IER = 0
         STR_CBL_DATE = MJDSEC_TO_DATE ( ANC%MJD_CBL,                   &
     &                        ANC%TAI_CBL + ANC%UTC_MTAI, IER )
!
! ------ Convert initial TPI date to string
!
         IER = 0
         STR_TPI_DATE = MJDSEC_TO_DATE ( ANC%MJD_TPI,                   &
     &                        ANC%TAI_TPI + ANC%UTC_MTAI, IER )
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------ File unit number
!
         LUN = 18
!
! ------ Open binary file to write to on unit LUN
!
         IER = -1
         CALL BINF_OPEN ( FILOUT, 'UNKNOWN', LUN, IER )
!
! ------ Write to file
!        When later reading these, be sure to read them in this same order
!------- Label to BNC file
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1', 32, BNC__LABEL_1, IER )
!
! ------ Experiment Code
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1', 16, ANC%EXP_CODE, IER )
!
! ------ Station name
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1',  8, ANC%STA_NAM,  IER )
!
! ------ UTC_MTAI
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1',  5, STR_UTCMTAI,  IER )
!
! ------ Initial DATA_ON block date 
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1', 24, STR_DOO_DATE(1:24), IER )
!
! ------ Initial TTO block date
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1', 24, STR_TTO_DATE(1:24), IER )
!
! ------  Initial METEO block date
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1', 24, STR_MET_DATE(1:24), IER )
!
! ------ Initial PCAL block date
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1', 24, STR_PCAL_DATE(1:24), IER )
!
! ------ Initial GPS block date
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1', 24, STR_GPS_DATE(1:24), IER )
!
! ------ Initial SEFD block date
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1', 24, STR_SEFD_DATE(1:24), IER )
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------ Initial CABLE block date
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1', 24, STR_CBL_DATE(1:24), IER )
!
! ------ Initial TPI block date
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1', 24, STR_TPI_DATE(1:24), IER )
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------ Number of DATA_ON (Scans)
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_DOO, IER )
!
! ------ Number of MET
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_MET, IER )
!
! ------  Number of TP_SENSORS
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TPS, IER )
!
! ------ Number of TSYS
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TSYS, IER )
!
! ------! Number of PC_SENSORS
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_PCS, IER )
!
! ------ Number of PCAL
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_PCAL, IER )
!
! ------ Number of GPS Timers
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TGPS, IER )
!
! ------ Number of GPS
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_GPS, IER )
!
! ------ Number of SEFD
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_SEFD, IER )
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------ Number of CBL
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_CBL, IER )
!
! ------ Number of TPI
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TPI, IER )
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! ------ If the antcal file has MET data, then write it to the binary file
!
         IF ( ANC%NUM_MET > 0 ) THEN
!     
! --------- METEOROLOGICAL Information      
! --------- Write the MET time array
!
            CALL WRBIN_ARRAY (LUN, 'R4', ANC%NUM_MET, TIM_MET_ARR, IER)
!
! --------- Write the MET pressure arra array
!
            CALL WRBIN_ARRAY (LUN, 'R4', ANC%NUM_MET, PRES_MET_ARR, IER)
!
! --------- Write the MET temperature array
!
            CALL WRBIN_ARRAY (LUN, 'R4', ANC%NUM_MET, TEMP_MET_ARR, IER)
!
! --------- Write the MET relative humidity array
!
            CALL WRBIN_ARRAY (LUN,'R4', ANC%NUM_MET, HUMID_MET_ARR, IER)
         END IF
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! ------ If the antcal file has TSYS data, then write it to the binary file
!
         IF ( (ANC%NUM_TPS > 0) .AND. (ANC%NUM_TSYS > 0) ) THEN
!     
! --------- GATHER TSYS INFO
! --------- First NUM_TPS elements of TAG_TPS_ARR
! ! originally I4
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'I8', ANC%NUM_TPS, TAG_TPS_ARR, IER)
!
! --------- First NUM_TPS elements of FRQ_TPS_ARR
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'R8', ANC%NUM_TPS, FRQ_TPS_ARR, IER)    
!
! --------- First NUM_TPS elements of POL_TPS_ARR
! !%%%! NOT SURE I AM NOT USING B1
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'I4', ANC%NUM_TPS, POL_TPS_ARR, IER)
!
! --------- First NUM_TPS elements of ID_TPS_ARR
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'I4', ANC%NUM_TPS, ID_TPS_ARR,  IER)
!
! --------- First NUM_TPS elements of NEP_ARR
!
            IER = -1
            CALL WRBIN_ARRAY ( LUN, 'I4', ANC%NUM_TPS, NEP_ARR, IER )
!
! --------- First NUM_TPS elements of OFF_ARR
!
            IER = -1
            CALL WRBIN_ARRAY ( LUN, 'I8', ANC%NUM_TPS, OFF_ARR, IER )
!
! --------- Write the content of timestamp against the number of
!           TP_sensors
!           N.B: Each point is wrt to the current TP_SENSOR.
!                So at the end of the day, you will have NUM_TPS 
!                of each of the arrays.
!
            DO 430 J3 = 1, ANC%NUM_TPS
!
! ------------ Grab the Time, Tsys, azimuth, and elevation to their
!              respective arrays. 
!
               DO 440 J4 = 1, ANC%NUM_TSYS
                  TIM_TTO_ARR(J4)  =  ANC%TTO(J4)%TIM ! - TIM_1ST
                  TSYS_ARR(J4)      =  ANC%TTO(J4)%TSYS(J3)
                  AZ_TTO_ARR(J4)        =  ANC%TTO(J4)%AZ
                  EL_TTO_ARR(J4)        =  ANC%TTO(J4)%EL
                  SOU_TTO_ARR(J4)  =  ANC%TTO(J4)%SOU_NAM
 440           CONTINUE
!
! ------------ Write the arrays to file
!
               IER = -1
               CALL WRBIN_ARRAY (LUN,'R4',ANC%NUM_TSYS, TIM_TTO_ARR, IER)
! ------------
               IER = -1
               CALL WRBIN_ARRAY (LUN,'R4',ANC%NUM_TSYS, TSYS_ARR, IER)
! ------------
               IER = -1
               CALL WRBIN_ARRAY (LUN,'R4',ANC%NUM_TSYS, AZ_TTO_ARR, IER)
! ------------
               IER = -1
               CALL WRBIN_ARRAY (LUN,'R4',ANC%NUM_TSYS, EL_TTO_ARR, IER)
! ------------
               IER = -1
               CALL WRBIN_ARRAY (LUN,'B1',ANC%NUM_TSYS, SOU_TTO_ARR, IER)
 430        CONTINUE
         END IF
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!@@NH@@!      PRINT *, "%%%%%%%%% BNC_WRITE - 434 %%%%%%%%%%%"
!
! ------ If the antcal file has PCAL data, then write it to the binary file
!
         IF ( ANC%NUM_PCS > 0 .AND. ANC%NUM_PCAL > 0 ) THEN
!
! --------- First NUM_PCS elements of TAG_PCS_ARR
! ! Originally I4
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'I8', ANC%NUM_PCS, TAG_PCS_ARR, IER)
!
! --------- First NUM_PCS elements of FRQ_PCS_ARR
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'I8', ANC%NUM_PCS, FRQ_PCS_ARR, IER)        
!
! --------- First NUM_PCS elements of POL_PCS_ARR
!        !%%%! NOT SURE I AM NOT USING B1
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'I4', ANC%NUM_PCS, POL_PCS_ARR, IER)
!
! --------- First NUM_PCS elements of ID_PCS_ARR
! ! Originally I4
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'I8', ANC%NUM_PCS, ID_PCS_ARR, IER)
!
! --------- Deal with the Phase-Cal time and values
! 
            DO 432 J3 = 1, ANC%NUM_PCS
!
! ------------ Grab the Time, and Pcal to their respective arrays. 
!
               DO 442 J4 = 1, ANC%NUM_PCAL
                  TIM_PCAL_ARR(J4)  = ANC%PCAL(J4)%TIM
                  PCAL_ARR(J4) = ANC%PCAL(J4)%PCAL_CMPL(J3)
                  SOU_PCAL_ARR(J4) = ANC%PCAL(J4)%SOU_NAM
 442           CONTINUE
!
! ------------ Write the arrays to file
!
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_PCAL,              &
     &                            TIM_PCAL_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'R8', ANC%NUM_PCAL,              &
     &                            PCAL_ARR, IER )
! ---------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'B1', ANC%NUM_PCAL,              &
     &                            SOU_PCAL_ARR, IER )
 432        CONTINUE
         END IF
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!####################################################################################
!
! ------ If the antcal file has GPS data
!
         IF ( ANC%NUM_TGPS > 0 .AND. ANC%NUM_GPS > 0 ) THEN
! -------
            DO 413 J1 = 1, ANC%NUM_TGPS
!
! ------------ GPS Board at the current TGPS_Timer
!
               TGPS_BOARD_ARR(J1) = ANC%TGPS(J1)%BOARD
 413        CONTINUE
!
! --------- Write the timer boards to file
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'I4', ANC%NUM_TGPS, TGPS_BOARD_ARR, IER )
!
! --------- Deal with the GPS time, formatter differences
!
            DO 423 J2 = 1, ANC%NUM_TGPS
!
! ------------ Grab the time, fmt - gps, and fmt - pps to their 
!              respective arrays
!
               DO 433 J3 = 1, ANC%NUM_GPS
                  TIM_GPS_ARR(J3) = ANC%GPS(J3)%TIM
                  FMGPS_ARR(J3)   = ANC%GPS(J3)%FMG(J2)
                  FMPPS_ARR(J3)   = ANC%GPS(J3)%FMP(J2)
 433           CONTINUE
!
! ------------ Write the arrays to file
!
               IER = -1
               CALL WRBIN_ARRAY (LUN,'R4',ANC%NUM_GPS, TIM_GPS_ARR, IER)
!
! ---------
! ! We recently changed from R8
               IER = -1
               CALL WRBIN_ARRAY (LUN,'R4', ANC%NUM_GPS, FMGPS_ARR, IER)
!
! ---------
!         ! We recently changed from R8
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_GPS, FMPPS_ARR, IER )
 423        CONTINUE
         END IF
!####################################################################################
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! ------ If the file has SEFD data, then write it to binary
!
         IF ( ANC%NUM_SEFD > 0 ) THEN
!
! ---------
!
            DO 414 J1 = 1, ANC%NUM_TPS
!
! ------------ Grab the Time, SEFD, Tsys, Tcal, Trat, azimuth, and 
!              elevation to their respective arrays.
!
               DO 424 J2 = 1, ANC%NUM_SEFD
                  TIM_SEFD_ARR(J2)   =  ANC%SEFD(J2)%TIM
                  SEFD_ARR(J2)       =  ANC%SEFD(J2)%SEFD(J1)
                  TSYS_SEFD_ARR(J2)  =  ANC%SEFD(J2)%TSYS(J1)
                  TCAL_SEFD_ARR(J2)  =  ANC%SEFD(J2)%TCAL(J1)
                  GAIN_SEFD_ARR(J2)  =  ANC%SEFD(J2)%GAIN(J1)
                  AZ_SEFD_ARR(J2)    =  ANC%SEFD(J2)%AZ
                  EL_SEFD_ARR(J2)    =  ANC%SEFD(J2)%EL
                  SOU_SEFD_ARR(J2)   =  ANC%SEFD(J2)%SOU_NAM
 424           CONTINUE 
!
! ------------ Write the arrays to file
!
               IER = -1
               CALL WRBIN_ARRAY (LUN,'R4',ANC%NUM_SEFD,TIM_SEFD_ARR,IER)
! ------------
               IER = -1
               CALL WRBIN_ARRAY (LUN,'R4', ANC%NUM_SEFD, SEFD_ARR, IER)
! ------------
               IER = -1
               CALL WRBIN_ARRAY(LUN,'R4',ANC%NUM_SEFD,TSYS_SEFD_ARR,IER)
! ------------
               IER = -1
               CALL WRBIN_ARRAY(LUN,'R4',ANC%NUM_SEFD,TCAL_SEFD_ARR,IER)
! ------------
               IER = -1
               CALL WRBIN_ARRAY(LUN,'R4',ANC%NUM_SEFD,GAIN_SEFD_ARR,IER)
! ------------
               IER = -1
               CALL WRBIN_ARRAY (LUN,'R4',ANC%NUM_SEFD,AZ_SEFD_ARR,IER)
! ------------
               IER = -1
               CALL WRBIN_ARRAY (LUN,'R4',ANC%NUM_SEFD,EL_SEFD_ARR,IER)
! ------------
               IER = -1
               CALL WRBIN_ARRAY (LUN,'I8',ANC%NUM_SEFD,SOU_SEFD_ARR,IER)
 414        CONTINUE
         END IF
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------ If the file has CBL data, then write it to binary
!
         IF ( ANC%NUM_CBL > 0 ) THEN
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'R4',ANC%NUM_CBL, TIM_CBL_ARR, IER)
! ---
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'R4',ANC%NUM_CBL, CBL_DEL_ARR, IER)
         END IF
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!
! ------ If the antcal file has TPI data, then write it to the binary file
!
         IF ( ANC%NUM_TPS > 0 .AND. ANC%NUM_TPI > 0 ) THEN
!
! --------- Deal with the TPI time and other arrays
! 
            DO 431 J3 = 1, ANC%NUM_TPS
!
! ------------ Grab the Time, and TPI's to their respective arrays. 
!
               DO 441 J4 = 1, ANC%NUM_TPI
                  TIM_TPI_ARR(J4)  =  ANC%TPI(J4)%TIM
                  TPION_ARR(J4)    =  ANC%TPI(J4)%TPION(J3)
                  TPIOFF_ARR(J4)   =  ANC%TPI(J4)%TPIOFF(J3)
                  AZ_TPI_ARR(J4)   =  ANC%TPI(J4)%AZ
                  EL_TPI_ARR(J4)   =  ANC%TPI(J4)%EL
                  SOU_TPI_ARR(J4)  =  ANC%TPI(J4)%SOU_NAM
 441           CONTINUE
!
! --------- Write the arrays to file
!
               IER = -1
               CALL WRBIN_ARRAY (LUN,'R4',ANC%NUM_TPI, TIM_TPI_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY (LUN,'I4',ANC%NUM_TPI, TPION_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY (LUN,'I4',ANC%NUM_TPI, TPIOFF_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY (LUN,'R4',ANC%NUM_TPI, AZ_TPI_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY (LUN,'R4',ANC%NUM_TPI, EL_TPI_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY (LUN,'B1',ANC%NUM_TPI, SOU_TPI_ARR, IER)
 431        CONTINUE
         END IF
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
! *************************************************************************
! *************************************************************************
! *                                                                       *
! * ------      PARSE THE VERSION 2 OF ANC FILES (2025.08.22)      ------ *
! *                                                                       *
! *************************************************************************
! *************************************************************************
      ELSEIF ( IANC_FMT_VERS == 2 ) THEN 
!
! ------ Convert UTC_MTAI to charater
!
         WRITE ( STR_UTCMTAI, '(F5.1)' ) ANC%UTC_MTAI
!
! ------ Initial time [place holder]
!
         TIM_1ST = 1.D30
!
! ------ Write TP_Sensor variables to array
!
         IF ( (ANC%NUM_TPS > 0) .AND. (ANC%NUM_TSYS > 0) ) THEN
!     
! --------- Go through the ANC derived type by reading each TPS sensor.
!
            DO 510 J1=1,ANC%NUM_TPS
! ------------
               KP = 0           ! Counter
!
! ------------ Find the initial time for this, where the value of
!              Tsys is greater than the min. Which effectively means
!              means where Tsys value is not a filler (-99.9)
!
               DO 520 J2=1,ANC%NUM_EPO_TTO
                  IF ( ANC%TTO(J2)%TSYS(J1) > ANC__TSYS_MIN ) THEN
                     KP = KP + 1
                     TIM_1ST = MIN ( TIM_1ST, ANC%TTO(J2)%TIM )
                  END IF
 520           CONTINUE               
!
! ------------ How many Tsys values using this TP_SENSOR are above 
!              ANC__TSYS_MIN
!
               NEP_ARR(J1) = KP
! ------------
               IF ( NEP_ARR(J1) == 0 ) NEP_ARR(J1) = 1
!
! ------------ ??Where do the offsets begin??
!
               IF ( J1 == 1 ) THEN
                  OFF_ARR(J1) = OFF_BEG
               ELSE
                  OFF_ARR(J1) = OFF_ARR(J1-1) + 4*(4*NEP_ARR(J1-1) + 4)
               END IF
!
! ------------ Sensor Tag at the current TP_Sensor
!
               TAG_TPS_ARR(J1)  = ANC%TPS(J1)%TAG
!
! ------------ IF Frequency
!
               FRQ_ARR_IFTP(J1) = ANC%TPS(J1)%IF_FRQ
!
! ------------ LO Frequency
!
               FRQ_ARR_LOTP(J1) = ANC%TPS(J1)%LO_FRQ
!
! ------------ Sky frequency at the current TP_Sensor
!
               FRQ_TPS_ARR(J1) = ANC%TPS(J1)%SKY_FRQ
!
! ------------ Bandwidth
!
               FRQ_ARR_BWTP(J1) = ANC%TPS(J1)%BDW
!
! ------------ Polarization at the current TP_Sensor
!
               POL_TPS_ARR(J1) = ANC%TPS(J1)%POL
!
! ------------ Sensor Sideband at the current TP_Sensor
!
               SSB_TPS_ARR(J1) = ANC%TPS(J1)%SSB
!
! ------------ Net Sideband at the current TP_Sensor
!
               NSB_TPS_ARR(J1) = ANC%TPS(J1)%NSB
!
! ------------ Sensor ID at the current TP_Sensor
!
               ID_TPS_ARR(J1)  = ANC%TPS(J1)%ID
!
! ------------ IF_IND at the current TP_Sensor
!
               IFIDX_TPS_ARR(J1) = ANC%TPS(J1)%IF_IND
 510        CONTINUE
         END IF
!     
! ------ If a PCAL section exists, then Go through the ANC derived 
!        type by reading each PC sensor.
!
         IF ( ANC%NUM_PCS > 0  .AND. ANC%NUM_PCAL > 0 ) THEN
            DO 512 J1=1,ANC%NUM_PCS
!
! ------------ Sensor TAG at the current PC_Sensor
!
               TAG_PCS_ARR(J1)  = ANC%PCS(J1)%TAG
!
! ------------ Sky frequency at the current PC_Sensor
!
               FRQ_PCS_ARR(J1) = ANC%PCS(J1)%SKY_FRQ
!
! ------------ Polarization at the current PC_Sensor
!
               POL_PCS_ARR(J1) = ANC%PCS(J1)%POL
!
! ------------ Sensor ID at the current PC_Sensor
!
               ID_PCS_ARR(J1)  = ANC%PCS(J1)%ID
!
! ------------ Sensor ID at the current TP_Sensor
!
               IFIDX_PCS_ARR(J1) = ANC%PCS(J1)%IF_IND
!
! ------------ Sensor Sideband at the current TP_Sensor
!
               SSB_PCS_ARR(J1) = ANC%PCS(J1)%SSB
!
! ------------ Net Sideband at the current TP_Sensor
!
               NSB_PCS_ARR(J1) = ANC%PCS(J1)%NSB
 512        CONTINUE
         END IF
!
! ------ If The file has GPS information
!        Copy the board and tag information to arrays
!
         IF ( ANC%NUM_TGPS > 0 .AND. ANC%NUM_GPS > 0 ) THEN
            DO 513 J1 = 1, ANC%NUM_TGPS
!
! ------------ Sensor Tag at the current TGPS_Timer
!
               TAG_GPS_ARR(J1)   = ANC%TGPS(J1)%TAG
!
! ------------ GPS Board at the current TGPS_Timer
!
               TGPS_BOARD_ARR(J1) = ANC%TGPS(J1)%BOARD
 513        CONTINUE
         END IF
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------ If the file has CABLE data
!
         IF ( ANC%NUM_CBL > 0 ) THEN
!
! ------ Grab the Time, and cable delays to their respective arrays
!
            DO 515 J1 = 1, ANC%NUM_CBL
               TIM_CBL_ARR(J1)  =  ANC%CBL(J1)%TIM
               CBL_DEL_ARR(J1)  =  ANC%CBL(J1)%DELAY
               SOU_CBL_ARR(J1)  =  ANC%CBL(J1)%SOU_NAM
               SCA_CBL_ARR(J1)  =  ANC%CBL(J1)%SCA_NAM
 515        CONTINUE
         END IF
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! ------ If the file has MET data
!
         IF ( ANC%NUM_MET > 0 ) THEN
!
! ------ Grab the time, pressure, temperature, relative humidity
!        source name, scan name, and scan index
!     
            DO 516 J1 = 1, ANC%NUM_MET
               TIM_MET_ARR(J1)   =  ANC%MET(J1)%TIM
               PRES_MET_ARR(J1)  =  ANC%MET(J1)%PRES
               TEMP_MET_ARR(J1)  =  ANC%MET(J1)%TEMP
               HUMID_MET_ARR(J1) =  ANC%MET(J1)%HUMID
               SOU_MET_ARR(J1)   =  ANC%MET(J1)%SOU_NAM
               SCA_MET_ARR(J1)   =  ANC%MET(J1)%SCA_NAM
               INDSC_MET_ARR(J1) =  ANC%MET(J1)%IND_SCA
 516        CONTINUE
         END IF
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! ------ If the file has DOO data
!
         IF ( ANC%NUM_DOO > 0 ) THEN
!
! ------ Grab the time, pressure, temperature, relative humidity
!        source name, scan name, and scan index
!     
            DO 517 J1 = 1, ANC%NUM_DOO
               TIM_DOO1_ARR(J1)  =  ANC%DOO(J1)%TIM(1)
               TIM_DOO2_ARR(J1)  =  ANC%DOO(J1)%TIM(2)
               SOU_DOO_ARR(J1)   =  ANC%DOO(J1)%SOU_NAM
               SCA_DOO_ARR(J1)   =  ANC%DOO(J1)%SCA_NAM
 517        CONTINUE
         END IF
!
! ------ Convert initial Dates to string
!
         IER = 0
         STR_DOO_DATE = MJDSEC_TO_DATE ( ANC%MJD_DOO,                   &
     &                         ANC%TAI_DOO + ANC%UTC_MTAI, IER )
! ------
         IER = 0
         STR_MET_DATE = MJDSEC_TO_DATE ( ANC%MJD_MET,                   &
     &                        ANC%TAI_MET + ANC%UTC_MTAI, IER )
! ------
         IER = 0
         STR_CBL_DATE = MJDSEC_TO_DATE ( ANC%MJD_CBL,                   &
     &                        ANC%TAI_CBL + ANC%UTC_MTAI, IER )
! ------
         IER = 0
         STR_TTO_DATE = MJDSEC_TO_DATE (ANC%MJD_TTO,                    &
     &                         ANC%TAI_TTO + ANC%UTC_MTAI, IER)
! ------
         IER = 0
         STR_TPI_DATE = MJDSEC_TO_DATE ( ANC%MJD_TPI,                   &
     &                        ANC%TAI_TPI + ANC%UTC_MTAI, IER )
! ------
         IER = 0
         STR_PCAL_DATE = MJDSEC_TO_DATE (ANC%MJD_PCAL,                  &
     &                         ANC%TAI_PCAL + ANC%UTC_MTAI, IER )
! ------
         IER = 0
         STR_GPS_DATE = MJDSEC_TO_DATE ( ANC%MJD_GPS,                   &
     &                        ANC%TAI_GPS + ANC%UTC_MTAI, IER )
! ------
         IER = 0
         STR_SEFD_DATE = MJDSEC_TO_DATE (ANC%MJD_SEFD,                  &
     &                         ANC%TAI_SEFD + ANC%UTC_MTAI, IER )
!
! ------ File unit number
!
         LUN = 18
!
! ------ Open binary file to write to on unit LUN
!
         IER = -1
         CALL BINF_OPEN ( FILOUT, 'UNKNOWN', LUN, IER )
!
! ------ Write to file
!        When later reading these, be sure to read them in this same order
!------- Label to BNC file
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1', 32, BNC__LABEL_2, IER )
!
! ------ Experiment Code
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1', 16, ANC%EXP_CODE, IER )
!
! ------ Station name
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1',  8, ANC%STA_NAM,  IER )
!
! ------ UTC_MTAI
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1',  5, STR_UTCMTAI,  IER )
!
! ------ Fillers
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'R4', 1, ANC%FILLER_R8, IER )
! ------
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1, ANC%FILLER_I4, IER )
! ------
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1', 19, ANC%FILLER_DATE, IER )
!     
! ------ Initial DATA_ON block date 
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1', 24, STR_DOO_DATE(1:24), IER )
!
! ------ Initial METEO block date
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1', 24, STR_MET_DATE(1:24), IER )
!
! ------ Initial CABLE block date
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1', 24, STR_CBL_DATE(1:24), IER )
!
! ------ Initial TTO block date
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1', 24, STR_TTO_DATE(1:24), IER )
!
! ------ Initial TPI block date
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1', 24, STR_TPI_DATE(1:24), IER )
!
! ------ Initial PCAL block date
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1', 24, STR_PCAL_DATE(1:24), IER )
!
! ------ Initial GPS block date
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1', 24, STR_GPS_DATE(1:24), IER )
!
! ------ Initial SEFD block date
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'B1', 24, STR_SEFD_DATE(1:24), IER )
!
! ------ Number of PROVENANCES, and the number of lines
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_PRV, IER )
! ------
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_PRV_LINES, IER )
!     
! ------ Number of DATA_ON (Scans)
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_DOO, IER )
!
! ------ Number of MET
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_MET, IER )
!
! ------ Number of CBL
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_CBL, IER )
!
! ------ Number of TP_SENSORS
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TPS, IER )
!
! ------ Number of TSYS, NUM_EPO_TTO, NUM_TTO
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TSYS, IER )
! ------
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_EPO_TTO, IER )
! ------
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TTO, IER )
!     
! ------ Number of TPI, EPOTPI
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TPI, IER )
! ------
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_EPO_TPI, IER )
!     
! ------ Number of PC_SENSORS
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_PCS, IER )
!
! ------ Number of PCAL, NUM_EPOPCAL
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_PCAL, IER )
! ------
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_EPO_PCAL, IER )
!     
! ------ Number of GPS Timers
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TGPS, IER )
!
! ------ Number of GPS, NUM_EPOGPS
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_GPS, IER )
! ------
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_EPO_GPS, IER )
!
! ------ Number of SEFD
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_SEFD, IER )
! ------
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_EPO_SEFD, IER )
!
! ------ In case this is a version 3 file.
!
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_OPA, IER ) 
! ------
         IER = -1
         CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TATM, IER )
!
! ------ Write the provenance block to file
!
         DO 450 J5=1,ANC%NUM_PRV
             IER = -1
             CALL WRBIN_ARRAY ( LUN, 'I4', 1,  ANC%PRV(J5)%N_TEXT, IER )
             IER = -1
             CALL WRBIN_ARRAY ( LUN, 'B1', ANC%PRV(J5)%N_TEXT, ANC%PRV(J5)%TEXT, IER )
 450     CONTINUE 
!     
! ------ If the antcal file has DOO data, then write it to the binary
!        file.
!
         IF ( ANC%NUM_DOO > 0 ) THEN
!     
! --------- DATA_ON Information      
! --------- Write the DOO intial time array
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'R4', ANC%NUM_DOO, TIM_DOO1_ARR, IER)
!
! --------- Write the DOO final time array
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'R4', ANC%NUM_DOO, TIM_DOO2_ARR, IER)
!     
! --------- Write the source names
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'B1', ANC%NUM_DOO, SOU_DOO_ARR, IER)
!     
! --------- Write the scan names
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'B1', ANC%NUM_DOO, SCA_DOO_ARR, IER)
         END IF
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! ------ If the antcal file has MET data, then write it to the binary file
!
         IF ( ANC%NUM_MET > 0 ) THEN
!     
! --------- METEOROLOGICAL Information      
! --------- Write the MET time array
!
            CALL WRBIN_ARRAY (LUN, 'R4', ANC%NUM_MET, TIM_MET_ARR, IER)
!
! --------- Write the MET pressure arra array
!
            CALL WRBIN_ARRAY (LUN, 'R4', ANC%NUM_MET, PRES_MET_ARR, IER)
!
! --------- Write the MET temperature array
!
            CALL WRBIN_ARRAY (LUN, 'R4', ANC%NUM_MET, TEMP_MET_ARR, IER)
!
! --------- Write the MET relative humidity array
!
            CALL WRBIN_ARRAY (LUN,'R4', ANC%NUM_MET, HUMID_MET_ARR, IER)
!     
! --------- Write the MET source names
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'B1', ANC%NUM_MET, SOU_MET_ARR, IER)
!     
! --------- Write the MET scan names
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'B1', ANC%NUM_MET, SCA_MET_ARR, IER)
!
! --------- Write the MET scan indices
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'I4',ANC%NUM_MET, INDSC_MET_ARR, IER)
         END IF
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------ If the file has CBL data, then write it to binary
!
         IF ( ANC%NUM_CBL > 0 ) THEN
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'R4',ANC%NUM_CBL, TIM_CBL_ARR, IER)
! ---------
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'R4',ANC%NUM_CBL, CBL_DEL_ARR, IER)
! ---------
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'B1', ANC%NUM_CBL, SOU_CBL_ARR, IER)
! ---------
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'B1', ANC%NUM_CBL, SCA_CBL_ARR, IER)
         END IF
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!========================================================================================
!{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!     
! ------ If the antcal file has TSYS data, then write it to the binary file
!
         IF ( (ANC%NUM_TPS > 0) .AND. (ANC%NUM_TSYS > 0) ) THEN
!     
! --------- WRITE THE TEMPERATURE SENSOR INFO
! --------- First NUM_TPS elements of TAG_TPS_ARR
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'I8', ANC%NUM_TPS, TAG_TPS_ARR, IER)
!
! --------- First NUM_TPS elements of FRQ_ARR_IFTP
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'R8', ANC%NUM_TPS, FRQ_ARR_IFTP, IER)    
!
! --------- First NUM_TPS elements of FRQ_ARR_LOTP
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'R8', ANC%NUM_TPS, FRQ_ARR_LOTP, IER)    
!
! --------- First NUM_TPS elements of FRQ_TPS_ARR
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'R8', ANC%NUM_TPS, FRQ_TPS_ARR, IER)    
!
! --------- First NUM_TPS elements of FRQ_ARR_BWTP
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'R8', ANC%NUM_TPS, FRQ_ARR_BWTP, IER)    
!
! --------- First NUM_TPS elements of POL_TPS_ARR
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'I4', ANC%NUM_TPS, POL_TPS_ARR, IER)
!
! --------- First NUM_TPS elements of SSB_TPS_ARR
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'I4', ANC%NUM_TPS, SSB_TPS_ARR, IER)
!
! --------- First NUM_TPS elements of NSB_TPS_ARR
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'I4', ANC%NUM_TPS, NSB_TPS_ARR, IER)
!
! --------- First NUM_TPS elements of ID_TPS_ARR
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'I4', ANC%NUM_TPS, ID_TPS_ARR,  IER)
!
! --------- First NUM_TPS elements of IFIDX_TPS_ARR
!
            IER = -1
            CALL WRBIN_ARRAY (LUN,'I4', ANC%NUM_TPS, IFIDX_TPS_ARR, IER)
!     
! --------- First NUM_TPS elements of NEP_ARR
!
            IER = -1
            CALL WRBIN_ARRAY ( LUN, 'I4', ANC%NUM_TPS, NEP_ARR, IER )
!
! --------- First NUM_TPS elements of OFF_ARR
!
            IER = -1
            CALL WRBIN_ARRAY ( LUN, 'I8', ANC%NUM_TPS, OFF_ARR, IER )
!
! --------- For TSYS Write the content of timestamp against the
!           number of TP_sensors
!           N.B: Each point is wrt to the current TP_SENSOR.
!                So at the end of the day, you will have NUM_TPS 
!                of each of the arrays.
!
            DO 530 J3 = 1, ANC%NUM_TPS
!
! ------------ Grab the Time, Duration, Tsys, azimuth, elevation,
!              scan/epoch index, source and scan names to their
!              respective arrays.
! ------------ We also parse and write TATM and OPA if they are there
!     
               DO 540 J4 = 1, ANC%NUM_EPO_TTO
                  TIM_TTO_ARR(J4)    =  ANC%TTO(J4)%TIM
                  DUR_TTO_ARR(J4)    =  ANC%TTO(J4)%DUR
                  TSYS_ARR(J4)       =  ANC%TTO(J4)%TSYS(J3)
                  AZ_TTO_ARR(J4)     =  ANC%TTO(J4)%AZ
                  EL_TTO_ARR(J4)     =  ANC%TTO(J4)%EL
                  INDSC_TTO_ARR(J4)  =  ANC%TTO(J4)%IND_SCA
                  SOU_TTO_ARR(J4)    =  ANC%TTO(J4)%SOU_NAM
                  SCA_TTO_ARR(J4)    =  ANC%TTO(J4)%SCA_NAM
!
! --------------- Write the TATM and Opacity data if it's there
!
                  IF ( ANC%NUM_TATM > 0 ) THEN
                     TATM_ARR(J4)  =  ANC%TTO(J4)%TATM(J3)
                  ENDIF
                  IF ( ANC%NUM_OPA > 0 ) THEN
                     OPA_ARR(J4)   =  ANC%TTO(J4)%OPA(J3)
                  ENDIF
 540           CONTINUE
!
! ------------ Write the arrays to file
!
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_TTO,            &
     &                            TIM_TTO_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_TTO,            &
     &                            DUR_TTO_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_TTO,            &
     &                            TSYS_ARR, IER)
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_TTO,            &
     &                            AZ_TTO_ARR, IER)
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_TTO,            &
     &                            EL_TTO_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'B1', ANC%NUM_EPO_TTO,            &
     &                            SOU_TTO_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'B1', ANC%NUM_EPO_TTO,            &
     &                            SCA_TTO_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'I4', ANC%NUM_EPO_TTO,            &
     &                            INDSC_TTO_ARR, IER )
!
! ------------ Write Total ATMospheric Brightness Temperature to file, 
!              if it is there.
!
               IF ( ANC%NUM_TATM > 0 ) THEN
                  IER = -1
                  CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_TTO,            &
     &                               TATM_ARR, IER)
               END IF
!
! ------------ Write Opacity temperature to file, if it is there.
!
               IF ( ANC%NUM_OPA > 0 ) THEN
                  IER = -1
                  CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_TTO,            &
     &                               OPA_ARR, IER)
               END IF
 530        CONTINUE
         END IF
!========================================================================================
!}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
! ------ If the antcal file has PCAL data, then write it to the binary file
!
         IF ( ANC%NUM_PCS > 0 .AND. ANC%NUM_PCAL > 0 ) THEN
!
! --------- First NUM_PCS elements of TAG_PCS_ARR
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'I8', ANC%NUM_PCS, TAG_PCS_ARR, IER)
!
! --------- First NUM_PCS elements of FRQ_PCS_ARR
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'I8', ANC%NUM_PCS, FRQ_PCS_ARR, IER)        
!
! --------- First NUM_PCS elements of POL_PCS_ARR
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'I4', ANC%NUM_PCS, POL_PCS_ARR, IER)
!
! --------- First NUM_PCS elements of ID_PCS_ARR
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'I8', ANC%NUM_PCS, ID_PCS_ARR, IER)
!
! --------- First NUM_PCS elements of IFIDX_PCS_ARR
!
            IER = -1
            CALL WRBIN_ARRAY (LUN,'I4', ANC%NUM_PCS, IFIDX_PCS_ARR, IER)
!
! --------- First NUM_PCS elements of SSB_PCS_ARR
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'I4', ANC%NUM_PCS, SSB_PCS_ARR, IER)
!
! --------- First NUM_PCS elements of NSB_PCS_ARR
!
            IER = -1
            CALL WRBIN_ARRAY (LUN, 'I4', ANC%NUM_PCS, NSB_PCS_ARR, IER)
!     
! --------- Deal with the Phase-Cal time and values
!
            DO 532 J3 = 1, ANC%NUM_PCS
!
! ------------ Grab the Time, and Pcal to their respective arrays. 
!
               DO 542 J4 = 1, ANC%NUM_EPO_PCAL
                  TIM_PCAL_ARR(J4)   =  ANC%PCAL(J4)%TIM
                  DUR_PCAL_ARR(J4)   =  ANC%PCAL(J4)%DUR
                  PCAL_ARR(J4)       =  ANC%PCAL(J4)%PCAL_CMPL(J3)
                  INDSC_PCAL_ARR(J4) =  ANC%PCAL(J4)%IND_SCA
                  SOU_PCAL_ARR(J4)   =  ANC%PCAL(J4)%SOU_NAM
                  SCA_PCAL_ARR(J4)   =  ANC%PCAL(J4)%SCA_NAM
 542           CONTINUE
!
! ------------ Write the arrays to file
!
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_PCAL,           &
     &                            TIM_PCAL_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_PCAL,           &
     &                            DUR_PCAL_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'R8', ANC%NUM_EPO_PCAL,           &
     &                            PCAL_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'I4', ANC%NUM_EPO_PCAL,           &
     &                            INDSC_PCAL_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'B1', ANC%NUM_EPO_PCAL,           &
     &                            SOU_PCAL_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'B1', ANC%NUM_EPO_PCAL,           &
     &                            SCA_PCAL_ARR, IER )
 532        CONTINUE
         END IF
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!
! ------ If the antcal file has TPI data, then write it to the binary file
!
         IF ( ANC%NUM_TPS > 0 .AND. ANC%NUM_TPI > 0 ) THEN
!
! --------- Deal with the TPI time and other arrays
! 
            DO 531 J3 = 1, ANC%NUM_TPS
!
! ------------ Grab the Time, and TPI's to their respective arrays. 
!
               DO 541 J4 = 1, ANC%NUM_EPO_TPI
                  TIM_TPI_ARR(J4)   =  ANC%TPI(J4)%TIM
                  DUR_TPI_ARR(J4)   =  ANC%TPI(J4)%DUR
                  TPION_ARR(J4)     =  ANC%TPI(J4)%TPION(J3)
                  TPIOFF_ARR(J4)    =  ANC%TPI(J4)%TPIOFF(J3)
                  TPIZERO_ARR(J4)   =  ANC%TPI(J4)%TPIZERO(J3)
                  AZ_TPI_ARR(J4)    =  ANC%TPI(J4)%AZ
                  EL_TPI_ARR(J4)    =  ANC%TPI(J4)%EL
                  INDSC_TPI_ARR(J4) =  ANC%TPI(J4)%IND_SCA
                  SOU_TPI_ARR(J4)   =  ANC%TPI(J4)%SOU_NAM
                  SCA_TPI_ARR(J4)   =  ANC%TPI(J4)%SCA_NAM
 541           CONTINUE
!
! --------- Write the arrays to file
!
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_TPI,            &
     &                            TIM_TPI_ARR,IER)
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_TPI,            &
     &                            DUR_TPI_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'I4', ANC%NUM_EPO_TPI,            &
     &                            TPION_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'I4', ANC%NUM_EPO_TPI,            &
     &                            TPIOFF_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'I4', ANC%NUM_EPO_TPI,            &
     &                            TPIZERO_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_TPI,            &
     &                            AZ_TPI_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_TPI,            &
     &                            EL_TPI_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN , 'I4', ANC%NUM_EPO_TPI,           &
     &                            INDSC_TPI_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'B1', ANC%NUM_EPO_TPI,            &
     &                            SOU_TPI_ARR,  IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'B1', ANC%NUM_EPO_TPI,            &
     &                            SCA_TPI_ARR,  IER )
 531        CONTINUE
         END IF
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!####################################################################################
!
! ------ If the antcal file has GPS data
!
         IF ( ANC%NUM_TGPS > 0 .AND. ANC%NUM_GPS > 0 ) THEN
!
! --------- Write the timer tags to file
!     
            IER = -1
            CALL WRBIN_ARRAY (LUN,'I4',ANC%NUM_TGPS,TAG_GPS_ARR,IER)
!
! --------- Write the timer boards to file
!
            IER = -1
            CALL WRBIN_ARRAY (LUN,'I4',ANC%NUM_TGPS,TGPS_BOARD_ARR,IER)
!
! --------- Deal with the GPS time, formatter differences
!
            DO 523 J2 = 1, ANC%NUM_TGPS
!
! ------------ Grab the time, fmt - gps, fmt - pps, scan index,
!              source and scan names  to their respective arrays
!
               DO 533 J3 = 1, ANC%NUM_EPO_GPS
                  TIM_GPS_ARR(J3) = ANC%GPS(J3)%TIM
                  FMGPS_ARR(J3)   = ANC%GPS(J3)%FMG(J2)
                  FMPPS_ARR(J3)   = ANC%GPS(J3)%FMP(J2)
                  INDSC_GPS_ARR(J3) =  ANC%GPS(J3)%IND_SCA
                  SOU_GPS_ARR(J3)   =  ANC%GPS(J3)%SOU_NAM
                  SCA_GPS_ARR(J3)   =  ANC%GPS(J3)%SCA_NAM
 533           CONTINUE
!
! ------------ Write the arrays to file
!
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_GPS,            &
     &                            TIM_GPS_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_GPS,            &
     &                            FMGPS_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_GPS,            &
     &                            FMPPS_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN , 'I4', ANC%NUM_EPO_GPS,           &
     &                            INDSC_GPS_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'B1', ANC%NUM_EPO_GPS,            &
     &                            SOU_GPS_ARR,  IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'B1', ANC%NUM_EPO_GPS,            &
     &                            SCA_GPS_ARR,  IER )
 523        CONTINUE
         END IF
!####################################################################################
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! ------ If the file has SEFD data, then write it to binary
!
         IF ( ANC%NUM_SEFD > 0 ) THEN
! ---------
            DO 514 J1 = 1, ANC%NUM_TPS
!
! ------------ Grab the Time, SEFD, Tsys, Tcal, Trat, azimuth, and 
!              elevation to their respective arrays.
!
               DO 524 J2 = 1, ANC%NUM_EPO_SEFD
                  TIM_SEFD_ARR(J2)   =  ANC%SEFD(J2)%TIM
                  SEFD_ARR(J2)       =  ANC%SEFD(J2)%SEFD(J1)
                  TSYS_SEFD_ARR(J2)  =  ANC%SEFD(J2)%TSYS(J1)
                  TCAL_SEFD_ARR(J2)  =  ANC%SEFD(J2)%TCAL(J1)
                  TRAT_SEFD_ARR(J2)  =  ANC%SEFD(J2)%TRAT(J1)
                  GAIN_SEFD_ARR(J2)  =  ANC%SEFD(J2)%GAIN(J1)
                  INDSC_SEFD_ARR(J2) =  ANC%SEFD(J2)%IND_SCA
                  AZ_SEFD_ARR(J2)    =  ANC%SEFD(J2)%AZ
                  EL_SEFD_ARR(J2)    =  ANC%SEFD(J2)%EL
                  SOU_SEFD_ARR(J2)   =  ANC%SEFD(J2)%SOU_NAM
 524           CONTINUE 
!
! ------------ Write the arrays to file
!
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_SEFD,           &
     &                            TIM_SEFD_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'R4',  ANC%NUM_EPO_SEFD           &
     &           ,                SEFD_ARR,  IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY( LUN, 'R4', ANC%NUM_EPO_SEFD,            &
     &                           TSYS_SEFD_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY( LUN, 'R4', ANC%NUM_EPO_SEFD,            &
     &                           TCAL_SEFD_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY( LUN, 'R4', ANC%NUM_EPO_SEFD,            &
     &                           TRAT_SEFD_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY( LUN, 'R4', ANC%NUM_EPO_SEFD,            &
     &                           GAIN_SEFD_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN , 'I4', ANC%NUM_EPO_SEFD,           &
     &                            INDSC_SEFD_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_SEFD,           &
     &                            AZ_SEFD_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_SEFD,           &
     &                            EL_SEFD_ARR, IER )
! ------------
               IER = -1
               CALL WRBIN_ARRAY ( LUN, 'I8', ANC%NUM_EPO_SEFD,           &
     &                            SOU_SEFD_ARR, IER )
 514        CONTINUE
         END IF
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      END IF
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
