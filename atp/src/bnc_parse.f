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
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 16-AUG-2022    BNC_PARSE  v3.0 (d)  N. Habana  21-SEP-2025 ###  *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE   'atp.i'
      TYPE     ( ANC__TYP   ) :: ANC      
      CHARACTER  FILIN*(*), STR1*9, STR2*9, STR3*9, STR4*256
      CHARACTER  STR_DOO_DATE*24, STR_TTO_DATE*24, STR_MET_DATE*24
      CHARACTER  STR_SEFD_DATE*24, STR_GPS_DATE*24, STR_PCAL_DATE*24
      CHARACTER  STR_CBL_DATE*24, STR_TPI_DATE*24
      CHARACTER  STR*128, TIT*64, STR_UTCMTAI*5
      INTEGER*4  IUER, IER
      REAL*8     TIM_1ST
      REAL*8     FRQ_TPS_ARR(ANC__MTPS), FRQ_PCS_ARR(ANC__MPCS)
      REAL*8     FRQ_ARR_IFTP(ANC__MTPS), FRQ_ARR_LOTP(ANC__MTPS)
      REAL*8     FRQ_ARR_BWTP(ANC__MTPS)
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
      INTEGER*4  NEP_ARR(ANC__MTPS), IND_ARR(ANC__MEPC)
      INTEGER*8  OFF_ARR(ANC__MTPS)
      INTEGER*4  LUN, NBT
      INTEGER*4  J1, J2, J3, J4, J5, J6
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
      CHARACTER  TGPS_BOARD_ARR(ANC__MTGPS)
      REAL*4     FMGPS_ARR(ANC__MGPS), FMPPS_ARR(ANC__MGPS)
      REAL*4     SEFD_ARR(ANC__MEPC), TSYS_SEFD_ARR(ANC__MEPC)
      REAL*4     TCAL_SEFD_ARR(ANC__MEPC), GAIN_SEFD_ARR(ANC__MEPC)
      REAL*4     AZ_SEFD_ARR(ANC__MEPC), EL_SEFD_ARR(ANC__MEPC)
      REAL*4     AZ_TPI_ARR(ANC__MEPC), EL_TPI_ARR(ANC__MEPC)
      REAL*4     TRAT_SEFD_ARR(ANC__MEPC)
      REAL*4     CBL_DEL_ARR(ANC__MEPC)
      CHARACTER  SOU_TTO_ARR(ANC__MEPC)*16, SOU_PCAL_ARR(ANC__MEPC)*16
      CHARACTER  SOU_TPI_ARR(ANC__MEPC)*16, SOU_SEFD_ARR(ANC__MEPC)*16
      CHARACTER  SOU_CBL_ARR(ANC__MEPC)*8, SOU_MET_ARR(ANC__MEPC)*8
      CHARACTER  SOU_DOO_ARR(ANC__MEPC)*8, SOU_GPS_ARR(ANC__MEPC)*8
      CHARACTER  SCA_TTO_ARR(ANC__MEPC)*16, SCA_PCAL_ARR(ANC__MEPC)*16
      CHARACTER  SCA_TPI_ARR(ANC__MEPC)*16
      CHARACTER  SCA_CBL_ARR(ANC__MEPC)*16, SCA_MET_ARR(ANC__MEPC)*16
      CHARACTER  SCA_DOO_ARR(ANC__MEPC)*16, SCA_GPS_ARR(ANC__MEPC)*16
      REAL*8     UTC_DOO, UTC_TTO, UTC_MET, UTC_PCAL, UTC_GPS
      REAL*8     UTC_SEFD, UTC_CBL, UTC_TPI
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, TIM_TO_DATE*23
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      INTEGER*4  MIND
      PARAMETER  ( MIND = 32 )
      INTEGER*4  IANC_FMT_VERS, LIND, IND(2,MIND)
      CHARACTER  DELIM*3
      PARAMETER  ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9) ) ! Null, Space, Tab
      INTEGER*4  IPC, IPP, IP1, IP2, IP3
      CHARACTER  BNC_LABEL*32
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
! --- Get the label of the BNC file
!
      IUER = -1
      CALL RDBIN_ARRAY (LUN, 'B1', 32, BNC_LABEL, NBT, IUER) ! label BNC file
! ---
      IF ( TRIM(BNC_LABEL) == TRIM(BNC__LABEL_1) ) THEN
         IANC_FMT_VERS = 1
      ELSEIF (  TRIM(BNC_LABEL) == TRIM(BNC__LABEL_1) ) THEN
         IANC_FMT_VERS = 2
      ELSE
         IUER = -1
         CALL ERR_LOG ( 5001, IUER, 'BNC_PARSE',                        &
     &           'Did not find a BNC magic at the beginning of '//FILIN)  
         CALL EXIT ( 1 ) 
      END IF
! ---
      IF ( IANC_FMT_VERS == 1 ) THEN
!
! ------ Experiment Code
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'B1', 16, ANC%EXP_CODE, NBT, IUER )
!
! ------ Station name
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN , 'B1 ', 8, ANC%STA_NAM, NBT, IUER )
!
! ------ UTC_MTAI
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'B1', 5, STR_UTCMTAI,  NBT, IUER )
!
! ------ Initial DATA_ON block date
!
         IUER = -1
         CALL RDBIN_ARRAY (LUN, 'B1', 24, STR_DOO_DATE(1:24), NBT, IUER)
!
! ------ Initial TSYS block date
!
         IUER = -1
         CALL RDBIN_ARRAY (LUN, 'B1', 24, STR_TTO_DATE(1:24), NBT, IUER)
!
! ------ Initial METEO block date
!
         IUER = -1
         CALL RDBIN_ARRAY (LUN, 'B1', 24, STR_MET_DATE(1:24), NBT, IUER)
!
! ------ Initial PCAL block date
!
         IUER = -1
         CALL RDBIN_ARRAY (LUN, 'B1', 24,STR_PCAL_DATE(1:24), NBT, IUER)
!         
! ------ Initial GPS block date
!
         IUER = -1
         CALL RDBIN_ARRAY (LUN, 'B1', 24, STR_GPS_DATE(1:24), NBT, IUER)
!
! ------ Initial SEFD block date
!
         IUER = -1
         CALL RDBIN_ARRAY (LUN, 'B1', 24,STR_SEFD_DATE(1:24), NBT, IUER)
!
! ------ Initial CABLE block date
!
         IUER = -1
         CALL RDBIN_ARRAY (LUN, 'B1', 24, STR_CBL_DATE(1:24), NBT, IUER)
!
! ------ Initial TPI block date
!
         IUER = -1
         CALL RDBIN_ARRAY (LUN, 'B1', 24, STR_TPI_DATE(1:24), NBT, IUER)
!
! ------ Number of DATA_ON (scans)
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_DOO, NBT, IUER )
!
! ------ Number of MET
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_MET, NBT, IUER )
!
! ------ Number of TP_Sensors
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TPS, NBT, IUER )
!
! ------ Number of TSYS
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TSYS, NBT, IUER )
!
! ------ Number of PC_SENSORS
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_PCS, NBT, IUER )
!
! ------ Number of PCAL
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_PCAL, NBT, IUER )
!
! ------ Number of GPS Timers
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TGPS, NBT, IUER )
!
! ------ Number of GPS
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_GPS, NBT, IUER )
!
! ------ Number of SEFD
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_SEFD, NBT, IUER )
!
! ------ Number of CABLE
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_CBL, NBT, IUER )
!
! ------ Number of TPI
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TPI, NBT, IUER )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     
! ------ Convert STR_UTCMTAI to real and parse to ANC type
!
         IER = IUER
         READ ( STR_UTCMTAI, '(F5.1)' ) ANC%UTC_MTAI
!
! ------ Convert the dates to MJD and TAI, and parse to respcetive 
!        ANC variables
!
         IUER = 0
         CALL DATE_TO_TIME ( STR_DOO_DATE, ANC%MJD_DOO, UTC_DOO, IUER )
         ANC%TAI_DOO = UTC_DOO - ANC%UTC_MTAI
! ------
         IUER = 0
         CALL DATE_TO_TIME ( STR_TTO_DATE, ANC%MJD_TTO, UTC_TTO, IUER)
         ANC%TAI_TTO = UTC_TTO - ANC%UTC_MTAI
! ------
         IUER = 0
         CALL DATE_TO_TIME ( STR_MET_DATE, ANC%MJD_MET, UTC_MET, IUER )
         ANC%TAI_MET = UTC_MET - ANC%UTC_MTAI
! ------
         IUER = 0
         CALL DATE_TO_TIME ( STR_PCAL_DATE, ANC%MJD_PCAL, UTC_PCAL, IUER )
         ANC%TAI_PCAL = UTC_PCAL - ANC%UTC_MTAI
! ------
         IUER = 0
         CALL DATE_TO_TIME ( STR_GPS_DATE, ANC%MJD_GPS, UTC_GPS,IUER )
         ANC%TAI_GPS = UTC_GPS - ANC%UTC_MTAI
! ------
         IUER = 0
         CALL DATE_TO_TIME ( STR_SEFD_DATE, ANC%MJD_SEFD, UTC_SEFD, IUER )
         ANC%TAI_SEFD = UTC_SEFD - ANC%UTC_MTAI
! ------
         IUER = 0
         CALL DATE_TO_TIME ( STR_CBL_DATE, ANC%MJD_CBL, UTC_CBL, IUER )
         ANC%TAI_CBL = UTC_CBL - ANC%UTC_MTAI
! ------
         IUER = 0
         CALL DATE_TO_TIME ( STR_TPI_DATE, ANC%MJD_TPI, UTC_TPI, IUER )
         ANC%TAI_TPI = UTC_TPI - ANC%UTC_MTAI
!     
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     
! ----- If there was meteorological data written, then parse it.
!
         IF ( ANC%NUM_MET > 0 ) THEN
!
! --------- Read the MET time array
!
            IUER = -1
            CALL RDBIN_ARRAY (LUN,'R4',ANC%NUM_MET,TIM_MET_ARR,NBT,IUER)
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5002, IUER, 'BNC_PARSE',                  &
     &                     'Error in reading TIM_MET_ARR. NBT = '//     &
     &                     TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! --------- Read the MET pressure array
!
            IUER = -1
            CALL RDBIN_ARRAY(LUN,'R4',ANC%NUM_MET,PRES_MET_ARR,NBT,IUER)
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5003, IUER, 'BNC_PARSE',                  &
     &                     'Error in reading PRES_MET_ARR. NBT = '//    &
     &                     TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! --------- Read the MET temperature array
!
            IUER = -1
            CALL RDBIN_ARRAY(LUN,'R4',ANC%NUM_MET,TEMP_MET_ARR,NBT,IUER)
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5004, IUER, 'BNC_PARSE',                  &
     &                 'Error in reading TEMP_MET_ARR. NBT = '//        &
     &                 TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! --------- Read the MET relative humidity array
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_MET, HUMID_MET_ARR,   &
     &           NBT, IUER)
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5005, IUER, 'BNC_PARSE',                  &
     &                 'Error in reading HUMID_MET_ARR. NBT = '//       &
     &                 TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!     
! --------- Allocate the MET derived type, ANC%MET
!
            ALLOCATE ( ANC%MET( ANC%NUM_MET ), STAT = IER )
!
! --------- Parse the read MET data
!
            DO 416 J1 = 1, ANC%NUM_MET
               ANC%MET(J1)%TIM   =  TIM_MET_ARR(J1) 
               ANC%MET(J1)%PRES  =  PRES_MET_ARR(J1)
               ANC%MET(J1)%TEMP  =  TEMP_MET_ARR(J1) 
               ANC%MET(J1)%HUMID = HUMID_MET_ARR(J1)
 416        CONTINUE
         END IF
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! ------ If the antcal file has TSYS data, then write it to the binary file
!
         IF ( (ANC%NUM_TPS > 0) .AND. (ANC%NUM_TSYS > 0) ) THEN      
!
! --------- Write TSYS INFO
! --------- First NUM_TPS of the Tags of the Tsys arrays (TAG_TPS_ARR)
! ! Originally I4
            IUER = -1
            CALL RDBIN_ARRAY(LUN,'I8',ANC%NUM_TPS,TAG_TPS_ARR,NBT,IUER)
!
! --------- First NUM_TPS elements of the TSYS freq array (FRQ_TPS_ARR)
!
            IUER = -1
            CALL RDBIN_ARRAY(LUN,'R8',ANC%NUM_TPS,FRQ_TPS_ARR,NBT,IUER) 
!
! -------- First NUM_TPS of the Tsys polarization array (POL_TPS_ARR)
! !%%%! NOT SURE I AM NOT USING B1
            IUER = -1
            CALL RDBIN_ARRAY(LUN,'I4',ANC%NUM_TPS,POL_TPS_ARR,NBT,IUER)
!     
! --------- First NUM_TPS of the indices of the Tsys arrays (ID_TPS_ARR)
!
            IUER = -1
            CALL RDBIN_ARRAY (LUN,'I4',ANC%NUM_TPS,ID_TPS_ARR,NBT,IUER)
!
! --------- First NUM_TPS of NEP_ARR
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_TPS,NEP_ARR,NBT,IUER )
!
! --------- First NUM_TPS of OFF_ARR
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'I8',ANC%NUM_TPS,OFF_ARR,NBT,IUER)
!
! --------- Allocate  ANC%TPS
!
            ALLOCATE ( ANC%TPS( ANC%NUM_TPS ), STAT = IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5006, IUER, 'BNC_PARSE',                  &
     &                 'Error in allocation memory for TP_SENSOR'//     &
     &                 ' while processing binary antenna '//            &
     &                 'calibration file '//TRIM(FILIN) )
               CALL EXIT ( 1 )
            END IF
!
! --------- Allocate ANC%TTO
!
            ALLOCATE ( ANC%TTO(ANC%NUM_TSYS), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5007, IUER, 'BNC_PARSE',                  &
     &                 'Error in allocation memory for ANC%TTO '//     &
     &                 'while processing binary antenna calibration'//  &
     &                 ' file '//FILIN)
               CALL EXIT ( 1 )
            END IF
!
! --------- Allocate each ANC%TTO(i)%TSYS
!
            DO 430 J3 = 1, ANC%NUM_TSYS
               ALLOCATE ( ANC%TTO(J3)%TSYS(ANC%NUM_TPS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5008, IUER, 'BNC_PARSE',               &
     &                    'Error in allocation memory for TSYS '//      &
     &                    'while processing antenna calibration '//     &
     &                    'file '//TRIM(FILIN) )
                  CALL EXIT ( 1 )
               END IF
 430        CONTINUE         
!
! --------- Allocate ANC%NEP_ARR
!
            ALLOCATE ( ANC%NEP_ARR( ANC%NUM_TPS ), STAT = IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5009, IUER, 'BNC_PARSE',                     &
     &              'Error in allocation memory for ANC%NEP_ARR' &
     &              //' while processing binary antenna '//      &
     &              'calibration file '//TRIM(FILIN) )
               CALL EXIT ( 1 )
            END IF
!
! ------ Parse the TSYS data to ANC
!
            DO 410 J1 = 1, ANC%NUM_TPS
! ------------
               ANC%TPS(J1)%TAG      =  TAG_TPS_ARR(J1)
               ANC%TPS(J1)%SKY_FRQ  =  FRQ_TPS_ARR(J1)
               ANC%TPS(J1)%POL      =  POL_TPS_ARR(J1)
               ANC%TPS(J1)%ID       =  ID_TPS_ARR(J1)
               ANC%NEP_ARR(J1)      =  NEP_ARR(J1)
!     
! ------------ Get time array for this TP_SENSOR
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_TSYS, TIM_TTO_ARR, &
     &                            NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  CALL CLRCH( STR3 ); CALL IINCH ( NEP_ARR(J1), STR3 )
                  IUER = -1
                  CALL ERR_LOG ( 5010, IUER, 'BNC_PARSE',               &
     &                    'Error in reading TIM_TTO_ARR. NBT = '//      &
     &                    TRIM(STR1)//' IND_FRQ = '//                   &
     &                    TRIM(STR2)//' MEL = '//TRIM(STR3) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get Tsys array for this TP_SENSOR
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_TSYS, TSYS_ARR,    &
     &                            NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  CALL CLRCH( STR3 ); CALL IINCH ( NEP_ARR(J1), STR3 )
                  IUER = -1
                  CALL ERR_LOG ( 5011, IUER, 'BNC_PARSE',               &
     &                    'Error in reading TSYS_ARR. NBT = '//         &
     &                    TRIM(STR1)//' IND_FRQ = '//TRIM(STR2)//       &
     &                    ' MEL = '//TRIM(STR3) )
                  CALL EXIT ( 1 )
               END IF
!     
!     --------- Get azimuth array for this TP_SENSOR
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_TSYS,              &
     &                            AZ_TTO_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  CALL CLRCH( STR3 ); CALL IINCH ( NEP_ARR(J1), STR3 )
                  IUER = -1
                  CALL ERR_LOG ( 5012, IUER, 'BNC_PARSE',               &
     &                    'Error in reading AZ_TTO_ARR. NBT = '//       &
     &                    TRIM(STR1)//' IND_FRQ = '//TRIM(STR2)//       &
     &                    ' MEL = '//TRIM(STR3) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get elevation array for this TP_SENSOR
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_TSYS,              &
     &                            EL_TTO_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  CALL CLRCH( STR3 ); CALL IINCH ( NEP_ARR(J1), STR3 )
                  IUER = -1
                  CALL ERR_LOG ( 5013, IUER, 'BNC_PARSE',               &
     &                    'Error in reading EL_TTO_ARR. NBT = '//       &
     &                    TRIM(STR1)//' IND_FRQ = '//TRIM(STR2)//       &
     &                    ' MEL = '//TRIM(STR3) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get Source array for this TP_SENSOR
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'B1', ANC%NUM_TSYS,              &
     &                            SOU_TTO_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  CALL CLRCH( STR3 ); CALL IINCH ( NEP_ARR(J1), STR3 )
                  IUER = -1
                  CALL ERR_LOG ( 5014, IUER, 'BNC_PARSE',               &
     &                    'Error in reading SOU_TTO_ARR. NBT = '//     &
     &                    TRIM(STR1)//' IND_FRQ = '//TRIM(STR2)//       &
     &                    ' MEL = '//TRIM(STR3) )
                  CALL EXIT ( 1 )
               END IF
!
! ------------
!
               DO 420 J2 = 1, ANC%NUM_TSYS
! ---------------
                  ANC%TTO(J2)%TIM       = REAL( TIM_TTO_ARR(J2),  8 )
                  ANC%TTO(J2)%TSYS(J1)  = REAL( TSYS_ARR(J2), 8 )
                  ANC%TTO(J2)%AZ        = REAL( AZ_TTO_ARR(J2), 8 )
                  ANC%TTO(J2)%EL        = REAL( EL_TTO_ARR(J2), 8 )
                  ANC%TTO(J2)%SOU_NAM   = SOU_TTO_ARR(J2)
 420           CONTINUE      
 410        CONTINUE
         END IF
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!     
! ------ If the file has PCAL data, parse it
!
         IF ( ANC%NUM_PCS > 0 .AND. ANC%NUM_PCAL > 0 ) THEN
! --------- ! Originally I4
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'I8', ANC%NUM_PCS,                  &
     &                         TAG_PCS_ARR, NBT, IUER )                                             
! ---------
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'I8', ANC%NUM_PCS,                  &
     &                         FRQ_PCS_ARR, NBT, IUER )
! ------   !%%%! NOT SURE I AM NOT USING B1
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_PCS,                  &
     &                         POL_PCS_ARR, NBT, IUER )                                      
! ---------      ! Originally I4
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'I8', ANC%NUM_PCS,                  &
     &                         ID_PCS_ARR, NBT, IUER )                                          
!     
! --------- Allocate  ANC%PCS
!     
            ALLOCATE ( ANC%PCS( ANC%NUM_PCS ), STAT = IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5015, IUER, 'BNC_PARSE',                  &
     &              'Error in allocation memory for PC_SENSOR '//       &
     &              'while processing binary antcal file '//FILIN )
               CALL EXIT ( 1 )
            END IF
!     
! --------- Allocate ANC%PCAL
!     
            ALLOCATE ( ANC%PCAL(ANC%NUM_PCAL), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5016, IUER, 'BNC_PARSE',                  &
     &              'Error in allocation memory for ANC%PCAL '//        &
     &              'while processing binary antcal file '//FILIN )
               CALL EXIT ( 1 )
            END IF
!     
! --------- Allocate each PCAL
!     
            DO 432 J3 = 1, ANC%NUM_PCAL
               ALLOCATE ( ANC%PCAL(J3)%PCAL_CMPL(ANC%NUM_PCS), STAT=IER)
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5017, IUER, 'BNC_PARSE',               &
     &                    'Error in allocation memory for PCAL_CMPL '// &
     &                    'while processing antcal file '//FILIN )
                  CALL EXIT ( 1 )
               END IF
 432        CONTINUE
!     
! --------- Parse the PCAL data to ANC
!     
            DO 412 J1 = 1, ANC%NUM_PCS
! ------------
               ANC%PCS(J1)%TAG      =  TAG_PCS_ARR(J1)            
               ANC%PCS(J1)%SKY_FRQ  =  FRQ_PCS_ARR(J1)
               ANC%PCS(J1)%POL      =  POL_PCS_ARR(J1)
               ANC%PCS(J1)%ID       =  ID_PCS_ARR(J1)
!     
! ------------ Get time array for this PC_SENSOR
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_PCAL,              &
     &                            TIM_PCAL_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5018, IUER, 'BNC_PARSE',               &
     &                 'Error in reading TIM_PCAL_ARR. '//              &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get PCAL array for this PC_SENSOR
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R8', ANC%NUM_PCAL,              &
     &                            PCAL_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5019, IUER, 'BNC_PARSE',               &
     &                 'Error in reading PCAL_ARR. '//                  &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get Source array for this PC_SENSOR
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'B1', ANC%NUM_PCAL,              &
     &                            SOU_PCAL_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5020, IUER, 'BNC_PARSE',               &
     &                    'Error in reading SOU_PCAL_ARR. '//           &
     &                    'NBT = '//TRIM(STR1)//                        &
     &                    ' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------
!     
               DO 422 J2 = 1, ANC%NUM_PCAL
! ---------------
                  ANC%PCAL(J2)%TIM           =  REAL( TIM_PCAL_ARR(J2), 8 )
                  ANC%PCAL(J2)%PCAL_CMPL(J1) = PCAL_ARR(J2)
                  ANC%PCAL(J2)%SOU_NAM       = SOU_PCAL_ARR(J2)
 422           CONTINUE
 412        CONTINUE
         END IF
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!#############################################################################
!     
! ------ If the antcal file has Formatter data
!     
         IF ( ANC%NUM_TGPS > 0 .AND. ANC%NUM_GPS > 0 ) THEN
!     
! --------- Copy the timer boards to array
!     
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_TGPS, TGPS_BOARD_ARR, &
     &           NBT, IUER )
!     
! --------- Allocate ANC%TGPS
!     
            ALLOCATE ( ANC%TGPS( ANC%NUM_TGPS ), STAT = IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5021, IUER, 'BNC_PARSE',                  &
     &              'Error in allocation memory for FMT2GPS while '//   &
     &              'processing binary antcal file '//TRIM(FILIN) )
               CALL EXIT ( 1 )
            END IF
!     
! --------- Allocate ANC%GPS
!     
            ALLOCATE ( ANC%GPS( ANC%NUM_GPS ), STAT = IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5022, IUER, 'BNC_PARSE',                  &
     &              'Error in allocation memory for FMTGPS while '//    &
     &              'processing binary antcal file '//TRIM(FILIN) )
               CALL EXIT ( 1 )
            END IF
!     
! --------- Allocate each FMTGPS, and FMTPPS
!     
            DO 413 J1 = 1, ANC%NUM_GPS
! ------------
               ALLOCATE ( ANC%GPS(J1)%FMG(ANC%NUM_TGPS), STAT = IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5023, IUER, 'BNC_PARSE',               &
     &                 'Error in allocation memory for ANC%GPS%FMG '//  &
     &                 'processing binary antcal file '//TRIM(FILIN) )
                  CALL EXIT ( 1 )
               END IF     
! ------------
               ALLOCATE ( ANC%GPS(J1)%FMP(ANC%NUM_TGPS), STAT = IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5024, IUER, 'BNC_PARSE',               &
     &                 'Error in allocation memory for ANC%GPS%FMP '//  &
     &                 'processing binary antcal file '//TRIM(FILIN) )
                  CALL EXIT ( 1 )
               END IF
 413        CONTINUE
!     
! ---------
!     
            DO 423 J2 = 1, ANC%NUM_TGPS
!     
! ------------ Write the board information to array
!     
               ANC%TGPS(J2)%BOARD = TGPS_BOARD_ARR(J2)
!     
! ------------ Get the time array from binary file
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_GPS, TIM_GPS_ARR,  &
     &              NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5025, IUER, 'BNC_PARSE',               &
     &                 'Error in reading TIM_GPS_ARR. '//               &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get the formatter minus GPS time
!      ! We recently changed from R8
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_GPS, FMGPS_ARR,    &
     &              NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5026, IUER, 'BNC_PARSE',               &
     &                 'Error in reading FMGPS_ARR. '//                 &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get the formatter minus PPS time
!      ! We recently changed from R8
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_GPS, FMPPS_ARR,    &
     &                            NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5027, IUER, 'BNC_PARSE',               &
     &                 'Error in reading FMGPS_ARR. '//                 &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Parse to derived type
!     
               DO 433 J3 = 1, ANC%NUM_GPS
! ----------------
                  ANC%GPS(J3)%TIM     =  REAL ( TIM_GPS_ARR(J3), 8 )
                  ANC%GPS(J3)%FMG(J2) =  REAL ( FMGPS_ARR(J3),   8 )
                  ANC%GPS(J3)%FMP(J2) =  REAL ( FMPPS_ARR(J3),   8 )
 433           CONTINUE 
 423        CONTINUE 
         END IF
!####################################################################################
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! ------ If the file has SEFD data
!     
         IF ( ANC%NUM_SEFD > 0 ) THEN
!
! --------- Allocate ANC%SEFD
!
            ALLOCATE ( ANC%SEFD( ANC%NUM_SEFD ), STAT = IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5028, IUER, 'BNC_PARSE',                  &
     &              'Error in allocation memory for ANC%SEFD while '//  &
     &              'processing binary antcal file '//TRIM(FILIN) )
               CALL EXIT ( 1 )
            END IF
!
! --------- Allocate each pointer
!
            DO 414 J1 = 1, ANC%NUM_SEFD
! ------------
               ALLOCATE ( ANC%SEFD(J1)%SEFD(ANC%NUM_TPS), STAT = IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5029, IUER, 'BNC_PARSE',               &
     &                 'Error in allocation memory for ANC%SEFD%SEFD'// &
     &                 ' processing binary antcal file '//TRIM(FILIN) )
                  CALL EXIT ( 1 )
               END IF     
! ------------
               ALLOCATE ( ANC%SEFD(J1)%TSYS(ANC%NUM_TPS), STAT = IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5030, IUER, 'BNC_PARSE',               &
     &                 'Error in allocation memory for ANC%SEFD%TSYS'// &
     &                 ' processing binary antcal file '//TRIM(FILIN) )
                  CALL EXIT ( 1 )
               END IF
! ------------
               ALLOCATE ( ANC%SEFD(J1)%TCAL(ANC%NUM_TPS), STAT = IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5031, IUER, 'BNC_PARSE',               &
     &                 'Error in allocation memory for ANC%SEFD%TCAL'// &
     &                 ' processing binary antcal file '//TRIM(FILIN) )
                  CALL EXIT ( 1 )
               END IF
! ------------
               ALLOCATE ( ANC%SEFD(J1)%GAIN(ANC%NUM_TPS), STAT = IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5032, IUER, 'BNC_PARSE',               &
     &                 'Error in allocation memory for ANC%SEFD%GAIN'// &
     &                 ' processing binary antcal file '//TRIM(FILIN) )
                  CALL EXIT ( 1 )
               END IF
 414        CONTINUE
!     
! ---------
!     
            DO 424 J2 = 1, ANC%NUM_TPS
!     
! ------------ Get the time array from binary file
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_SEFD,              &
     &                             TIM_SEFD_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5033, IUER, 'BNC_PARSE',               &
     &                 'Error in reading TIM_SEFD_ARR. '//              &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_SEFD, SEFD_ARR,    &
     &                            NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5034, IUER, 'BNC_PARSE',               &
     &                 'Error in reading SEFD_ARR. '//                  &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_SEFD,              &
     &                            TSYS_SEFD_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5035, IUER, 'BNC_PARSE',               &
     &                 'Error in reading TSYS_SEFD_ARR. '//             &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_SEFD,              &
     &                            TCAL_SEFD_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5036, IUER, 'BNC_PARSE',                  &
     &                 'Error in reading TCAL_SEFD_ARR. '//             &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_SEFD,              &
     &                             GAIN_SEFD_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5037, IUER, 'BNC_PARSE',               &
     &                 'Error in reading GAIN_SEFD_ARR. '//             &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_SEFD, AZ_SEFD_ARR, &
     &              NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5038, IUER, 'BNC_PARSE',               &
     &                 'Error in reading AZ_SEFD_ARR. '//               &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_SEFD, EL_SEFD_ARR, &
     &              NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5039, IUER, 'BNC_PARSE',               &
     &                 'Error in reading EL_SEFD_ARR. '//               &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF            
!
! ---------
!
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'I8', ANC%NUM_SEFD,              &
     &                            SOU_SEFD_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5040, IUER, 'BNC_PARSE',               &
     &                 'Error in reading SOU_SEFD_ARR. '//              &
     &                 'NBT = '//TRIM(STR1)//                           &
     &                 ' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT (1)
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
                  ANC%SEFD(J3)%SOU_NAM  =  SOU_SEFD_ARR(J3)
 434           CONTINUE
 424        CONTINUE
         END IF
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------ Parse the cable data if it exists
!     
         IF ( ANC%NUM_CBL > 0 ) THEN
!     
! --------- Allocate the cable derived type ANC%CBL
!     
            ALLOCATE ( ANC%CBL( ANC%NUM_CBL ), STAT = IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5041, IUER, 'BNC_PARSE',                  &
     &              'Error in allocation memory for ANC%CBL while '//   &
     &              'processing binary antcal file '//TRIM(FILIN) )
               CALL EXIT ( 1 )
            END IF
!     
! --------- Get the date string array from binary file
!     
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_CBL, TIM_CBL_ARR,     &
     &           NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               IUER = -1
               CALL ERR_LOG ( 5042, IUER, 'BNC_PARSE',                  &
     &              'Error in reading TIM_GPS_ARR. '//                  &
     &              'NBT = '//TRIM(STR1) )
               CALL EXIT ( 1 )
            END IF
!     
! --------- Get the cable delay
!     
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_CBL, CBL_DEL_ARR,     &
     &           NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               IUER = -1
               CALL ERR_LOG ( 5043, IUER, 'BNC_PARSE',                  &
     &              'Error in reading FMGPS_ARR. '//                    &
     &              'NBT = '//TRIM(STR1) )
               CALL EXIT ( 1 )
            END IF
!     
! --------- Parse the arrays to their respective
!     
            DO 440 J2 = 1, ANC%NUM_CBL
               ANC%CBL(J2)%TIM    = REAL( TIM_CBL_ARR(J2), 8 )
               ANC%CBL(J2)%DELAY  = REAL( CBL_DEL_ARR(J2), 8 )
 440        CONTINUE
         END IF
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!
! ------ If the antcal file has TPI data, then write it to the binary file
!
         IF ( ANC%NUM_TPS > 0 .AND. ANC%NUM_TPI > 0 ) THEN
!
! --------- Allocate the TPI derived type
!
            ALLOCATE ( ANC%TPI ( ANC%NUM_TPI ), STAT = IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5044, IUER, 'BNC_PARSE',                  &
     &              'Error in allocation memory for ANC%TPI while '//   &
     &              'processing binary antcal file '//TRIM(FILIN) )
               CALL EXIT ( 1 )
            END IF
!     
! --------- Allocate each ANC%TPI(i)%TPION
!     
            DO 451 J1 = 1, ANC%NUM_TPI
               ALLOCATE ( ANC%TPI(J1)%TPION(ANC%NUM_TPS),  STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5045, IUER, 'BNC_PARSE',               &
     &                    'Error in allocation memory for '//           &
     &                    'ANC%TPION while processing antenna '//       &
     &                   'calibration file '//TRIM(FILIN) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ 
!     
               ALLOCATE ( ANC%TPI(J1)%TPIOFF(ANC%NUM_TPS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5046, IUER, 'BNC_PARSE',               &
     &                    'Error in allocation memory for '//           &
     &                    'ANC%TPIOFF while processing antenna '//      &
     &                    'calibration file '//TRIM(FILIN) )
                  CALL EXIT ( 1 )
               END IF
 451        CONTINUE
!     
! --------- 
!     
            DO 452 J2 = 1, ANC%NUM_TPS
!     
! ------------ Get the time array from binary file
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_TPI, TIM_TPI_ARR,  &
     &                            NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5047, IUER, 'BNC_PARSE',               &
     &                    'Error in reading TIM_TPI_ARR. '//            &
     &                    'NBT = '//TRIM(STR1)//' IND_FRQ = '//         &
     &                     TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get the TPION array
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_TPI, TPION_ARR,    &
     &                            NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5048, IUER, 'BNC_PARSE',               &
     &                    'Error in reading TPION_ARR. '//              &
     &                    'NBT = '//TRIM(STR1)//' IND_FRQ = '//         &
     &                    TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get the TPIOFF array
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_TPI, TPIOFF_ARR,   &
     &                            NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5049, IUER, 'BNC_PARSE',               &
     &                    'Error in reading TPIOFF_ARR. '//             &
     &                    'NBT = '//TRIM(STR1)//' IND_FRQ = '//         &
     &                    TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get the AZ_TPI array
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_TPI, AZ_TPI_ARR,   &
     &                            NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5050, IUER, 'BNC_PARSE',               &
     &                 'Error in reading AZ_TPI_ARR. '//                &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get the EL_TPI array
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_TPI, EL_TPI_ARR,   &
     &                            NBT, IUER ) 
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5051, IUER, 'BNC_PARSE',               &
     &                    'Error in reading EL_TPI_ARR. '//             &
     &                    'NBT = '//TRIM(STR1)//' IND_FRQ = '//         &
     &                    TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ 
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'B1', ANC%NUM_TPI, SOU_TPI_ARR,  &
     &                            NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5052, IUER, 'BNC_PARSE',               &
     &                    'Error in reading SOU_TPI_ARR. '//            &
     &                    'NBT = '//TRIM(STR1)//                        &
     &                    ' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT (1)
               END IF
!     
! ------------- 
!     
               DO 453 J3 = 1, ANC%NUM_TPI
                  ANC%TPI(J3)%TIM        =  REAL ( TIM_TPI_ARR(J3), 8 )
                  ANC%TPI(J3)%TPION(J2)  =  REAL ( TPION_ARR(J3),   8 )
                  ANC%TPI(J3)%TPIOFF(J2) =  REAL ( TPIOFF_ARR(J3),  8 )
                  ANC%TPI(J3)%AZ         =  REAL ( AZ_TPI_ARR(J3),  8 )
                  ANC%TPI(J3)%EL         =  REAL ( EL_TPI_ARR(J3),  8 )
                  ANC%TPI(J3)%SOU_NAM    =  SOU_TPI_ARR(J3)
 453           CONTINUE
 452        CONTINUE
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
! ------ Experiment Code
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'B1', 16, ANC%EXP_CODE, NBT, IUER )
!
! ------ Station name
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN , 'B1 ', 8, ANC%STA_NAM, NBT, IUER )
!
! ------ UTC_MTAI
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'B1', 5, STR_UTCMTAI,  NBT, IUER )
!
! ------ Fillers
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'R4', 1, ANC%FILLER_R8, NBT, IUER )
! ------
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1, ANC%FILLER_I4, NBT, IUER )
! ------
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'B1', 19, ANC%FILLER_DATE, NBT, IUER )
!
! ------ Initial DATA_ON block date
!
         IUER = -1
         CALL RDBIN_ARRAY (LUN, 'B1', 24, STR_DOO_DATE(1:24), NBT, IUER)
!
! ------ Initial METEO block date
!
         IUER = -1
         CALL RDBIN_ARRAY (LUN, 'B1', 24, STR_MET_DATE(1:24), NBT, IUER)
!
! ------ Initial CABLE block date
!
         IUER = -1
         CALL RDBIN_ARRAY (LUN, 'B1', 24, STR_CBL_DATE(1:24), NBT, IUER)
!
! ------ Initial TTO block date
!
         IUER = -1
         CALL RDBIN_ARRAY (LUN, 'B1', 24, STR_TTO_DATE(1:24), NBT, IUER)
!
! ------ Initial TPI block date
!
         IUER = -1
         CALL RDBIN_ARRAY (LUN, 'B1', 24, STR_TPI_DATE(1:24), NBT, IUER)
!
! ------ Initial PCAL block date
!
         IUER = -1
         CALL RDBIN_ARRAY (LUN, 'B1', 24,STR_PCAL_DATE(1:24), NBT, IUER)
!         
! ------ Initial GPS block date
!
         IUER = -1
         CALL RDBIN_ARRAY (LUN, 'B1', 24, STR_GPS_DATE(1:24), NBT, IUER)
!
! ------ Initial SEFD block date
!
         IUER = -1
         CALL RDBIN_ARRAY (LUN, 'B1', 24,STR_SEFD_DATE(1:24), NBT, IUER)
!
! ------ Number of DATA_ON (scans)
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1, ANC%NUM_DOO, NBT, IUER )
!
! ------ Number of MET
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1, ANC%NUM_MET, NBT, IUER )
!
! ------ Number of CABLE
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1, ANC%NUM_CBL, NBT, IUER )
!
! ------ Number of TP_Sensors
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1, ANC%NUM_TPS, NBT, IUER )
!
! ------ Number of TSYS, NUM_EPO_TTO, NUM_TTO
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1, ANC%NUM_TSYS, NBT, IUER )
! ------
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1, ANC%NUM_EPO_TTO, NBT, IUER )
! ------
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1, ANC%NUM_TTO, NBT, IUER )
!
! ------ Number of TPI, NUM_EPO_TPI
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1, ANC%NUM_TPI, NBT, IUER )
! ------
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1, ANC%NUM_EPO_TPI, NBT, IUER )
!
! ------ Number of PC_SENSORS
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1, ANC%NUM_PCS, NBT, IUER )
!
! ------ Number of PCAL, NUM_EPO_PCAL
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1, ANC%NUM_PCAL, NBT, IUER )
! ------
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1, ANC%NUM_EPO_PCAL, NBT, IUER )
!
! ------ Number of GPS Timers
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1, ANC%NUM_TGPS, NBT, IUER )
!
! ------ Number of GPS
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1, ANC%NUM_GPS, NBT, IUER )
! ------
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1, ANC%NUM_EPO_GPS, NBT, IUER )
!
! ------ Number of SEFD
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1, ANC%NUM_SEFD, NBT, IUER )
! ------
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1, ANC%NUM_EPO_SEFD, NBT, IUER )
!
! ------ In case this is a version 3 file.
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_OPA, NBT, IUER )
! ------
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'I4', 1,  ANC%NUM_TATM, NBT, IUER )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     
! ------ Convert STR_UTCMTAI to real and parse to ANC type
!
         IER = IUER
         READ ( STR_UTCMTAI, '(F5.1)' ) ANC%UTC_MTAI
!
! ------ Convert the dates to MJD and TAI, and parse to respcetive 
!        ANC variables
!
         IUER = 0
         CALL DATE_TO_TIME ( STR_DOO_DATE, ANC%MJD_DOO, UTC_DOO, IUER )
         ANC%TAI_DOO = UTC_DOO - ANC%UTC_MTAI
! ------
         IUER = 0
         CALL DATE_TO_TIME ( STR_MET_DATE, ANC%MJD_MET, UTC_MET, IUER )
         ANC%TAI_MET = UTC_MET - ANC%UTC_MTAI
! ------
         IUER = 0
         CALL DATE_TO_TIME ( STR_CBL_DATE, ANC%MJD_CBL, UTC_CBL, IUER )
         ANC%TAI_CBL = UTC_CBL - ANC%UTC_MTAI
! ------
         IUER = 0
         CALL DATE_TO_TIME ( STR_TTO_DATE, ANC%MJD_TTO, UTC_TTO, IUER )
         ANC%TAI_TTO = UTC_TTO - ANC%UTC_MTAI
! ------
         IUER = 0
         CALL DATE_TO_TIME ( STR_TPI_DATE, ANC%MJD_TPI, UTC_TPI, IUER )
         ANC%TAI_TPI = UTC_TPI - ANC%UTC_MTAI
! ------
         IUER = 0
         CALL DATE_TO_TIME (STR_PCAL_DATE, ANC%MJD_PCAL, UTC_PCAL, IUER)
         ANC%TAI_PCAL = UTC_PCAL - ANC%UTC_MTAI
! ------
         IUER = 0
         CALL DATE_TO_TIME ( STR_GPS_DATE, ANC%MJD_GPS, UTC_GPS,IUER )
         ANC%TAI_GPS = UTC_GPS - ANC%UTC_MTAI
! ------
         IUER = 0
         CALL DATE_TO_TIME (STR_SEFD_DATE, ANC%MJD_SEFD, UTC_SEFD, IUER)
         ANC%TAI_SEFD = UTC_SEFD - ANC%UTC_MTAI
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! ------ Read the Provenance array
!
         DO 460 J6=1,ANC%NUM_PRV
            CALL ERR_PASS ( IUER, IER )
            CALL RDBIN_ARRAY ( LUN, 'I4', 1, ANC%PRV(J6)%N_TEXT, NBT, IER )
!
            CALL ERR_PASS ( IUER, IER )
            CALL RDBIN_ARRAY ( LUN, 'B1', ANC%PRV(J6)%N_TEXT, &
     &                         ANC%PRV(J6)%TEXT, NBT, IER )
 460     CONTINUE 
!     
! ------ If the antcal file has DOO data, then write it to the binary file
!
         IF ( ANC%NUM_DOO > 0 ) THEN
!
! --------- Read the DOO initial time array
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_DOO,                  &
     &                         TIM_DOO1_ARR, NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5053, IUER, 'BNC_PARSE',                  &
     &                     'Error in reading TIM_DOO1_ARR. NBT = '//    &
     &                     TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! --------- Read the DOO end time array
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_DOO,                  &
     &                         TIM_DOO2_ARR, NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5054, IUER, 'BNC_PARSE',                  &
     &                     'Error in reading TIM_DOO2_ARR. NBT = '//    &
     &                     TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! --------- Read the DOO Source name array
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'B1', ANC%NUM_DOO,                  &
     &                         SOU_DOO_ARR, NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5055, IUER, 'BNC_PARSE',                  &
     &                     'Error in reading SOU_DOO_ARR. NBT = '//    &
     &                     TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! --------- Read the DOO Scan name array
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'B1', ANC%NUM_DOO,                  &
     &                         SCA_DOO_ARR, NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5056, IUER, 'BNC_PARSE',                  &
     &                     'Error in reading SCA_DOO_ARR. NBT = '//    &
     &                     TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!     
! --------- Allocate the DOO derived type, ANC%DOO
!
            ALLOCATE ( ANC%DOO( ANC%NUM_DOO ), STAT = IER )
!
! --------- Parse the read MET data
!
            DO 502 J1 = 1, ANC%NUM_DOO
               ANC%DOO(J1)%TIM(1)   =  TIM_DOO1_ARR(J1) 
               ANC%DOO(J1)%TIM(2)   =  TIM_DOO1_ARR(J1) 
               ANC%DOO(J1)%SOU_NAM  =  SOU_DOO_ARR(J1)
               ANC%DOO(J1)%SCA_NAM  =  SCA_DOO_ARR(J1)
 502           CONTINUE
         END IF
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     
! ----- If there was meteorological data written, then parse it.
!
         IF ( ANC%NUM_MET > 0 ) THEN
!
! --------- Read the MET time array
!
            IUER = -1
            CALL RDBIN_ARRAY (LUN,'R4',ANC%NUM_MET,TIM_MET_ARR,NBT,IUER)
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5057, IUER, 'BNC_PARSE',                  &
     &                     'Error in reading TIM_MET_ARR. NBT = '//     &
     &                     TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! --------- Read the MET pressure array
!
            IUER = -1
            CALL RDBIN_ARRAY(LUN,'R4',ANC%NUM_MET,PRES_MET_ARR,NBT,IUER)
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5058, IUER, 'BNC_PARSE',                  &
     &                     'Error in reading PRES_MET_ARR. NBT = '//    &
     &                     TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! --------- Read the MET temperature array
!
            IUER = -1
            CALL RDBIN_ARRAY(LUN,'R4',ANC%NUM_MET,TEMP_MET_ARR,NBT,IUER)
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5059, IUER, 'BNC_PARSE',                  &
     &                 'Error in reading TEMP_MET_ARR. NBT = '//        &
     &                 TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! --------- Read the MET relative humidity array
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_MET, HUMID_MET_ARR,   &
     &           NBT, IUER)
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5060, IUER, 'BNC_PARSE',                  &
     &                 'Error in reading HUMID_MET_ARR. NBT = '//       &
     &                 TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! --------- Read the MET Source name array
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'B1', ANC%NUM_MET,                  &
     &                         SOU_MET_ARR, NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5061, IUER, 'BNC_PARSE',                  &
     &                     'Error in reading SOU_MET_ARR. NBT = '//    &
     &                     TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! --------- Read the MET Scan name array
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'B1', ANC%NUM_MET,                  &
     &                         SCA_MET_ARR, NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5062, IUER, 'BNC_PARSE',                  &
     &                     'Error in reading SCA_MET_ARR. NBT = '//    &
     &                     TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!     
! --------- Allocate the MET derived type, ANC%MET
!
            ALLOCATE ( ANC%MET( ANC%NUM_MET ), STAT = IER )
!
! --------- Parse the read MET data
!
            DO 516 J1 = 1, ANC%NUM_MET
               ANC%MET(J1)%TIM      =  TIM_MET_ARR(J1) 
               ANC%MET(J1)%PRES     =  PRES_MET_ARR(J1)
               ANC%MET(J1)%TEMP     =  TEMP_MET_ARR(J1) 
               ANC%MET(J1)%HUMID    =  HUMID_MET_ARR(J1)
               ANC%MET(J1)%SOU_NAM  =  SOU_MET_ARR(J1)
               ANC%MET(J1)%SCA_NAM  =  SCA_MET_ARR(J1)
 516        CONTINUE
         END IF
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------ Parse the cable data if it exists
!     
         IF ( ANC%NUM_CBL > 0 ) THEN
!     
! --------- Get the date string array from binary file
!     
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_CBL, TIM_CBL_ARR,     &
     &           NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               IUER = -1
               CALL ERR_LOG ( 5063, IUER, 'BNC_PARSE',                  &
     &              'Error in reading TIM_GPS_ARR. '//                  &
     &              'NBT = '//TRIM(STR1) )
               CALL EXIT ( 1 )
            END IF
!     
! --------- Get the cable delay
!     
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_CBL, CBL_DEL_ARR,     &
     &           NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               IUER = -1
               CALL ERR_LOG ( 5064, IUER, 'BNC_PARSE',                  &
     &              'Error in reading FMGPS_ARR. '//                    &
     &              'NBT = '//TRIM(STR1) )
               CALL EXIT ( 1 )
            END IF
!
! --------- Read the CBL Source name array
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'B1', ANC%NUM_CBL,                  &
     &                         SOU_CBL_ARR, NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5064, IUER, 'BNC_PARSE',                  &
     &                     'Error in reading SOU_CBL_ARR. NBT = '//    &
     &                     TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!
! --------- Read the CBL Scan name array
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'B1', ANC%NUM_CBL,                  &
     &                         SCA_CBL_ARR, NBT, IUER )
            IF ( IUER .NE. 0 ) THEN
               CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
               CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
               IUER = -1
               CALL ERR_LOG ( 5065, IUER, 'BNC_PARSE',                  &
     &                     'Error in reading SCA_CBL_ARR. NBT = '//    &
     &                     TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
               CALL EXIT ( 1 )
            END IF
!     
! --------- Allocate the cable derived type ANC%CBL
!     
            ALLOCATE ( ANC%CBL( ANC%NUM_CBL ), STAT = IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5066, IUER, 'BNC_PARSE',                  &
     &              'Error in allocation memory for ANC%CBL while '//   &
     &              'processing binary antcal file '//TRIM(FILIN) )
               CALL EXIT ( 1 )
            END IF
!     
! --------- Parse the arrays to their respective
!     
            DO 540 J2 = 1, ANC%NUM_CBL
               ANC%CBL(J2)%TIM      =  REAL( TIM_CBL_ARR(J2), 8 )
               ANC%CBL(J2)%DELAY    =  REAL( CBL_DEL_ARR(J2), 8 )
               ANC%CBL(J2)%SOU_NAM  =  SOU_CBL_ARR(J2)
               ANC%CBL(J2)%SCA_NAM  =  SCA_CBL_ARR(J2)
 540        CONTINUE
         END IF
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!========================================================================================
!{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
! ------ If the antcal file has TSYS data, then write it to the binary file
!
         IF ( (ANC%NUM_TPS > 0) .AND. (ANC%NUM_TSYS > 0) ) THEN      
!
! --------- Write TSYS INFO
! --------- First NUM_TPS of the Tags of the Tsys arrays (TAG_TPS_ARR)
!
            IUER = -1
            CALL RDBIN_ARRAY(LUN,'I8',ANC%NUM_TPS,TAG_TPS_ARR,NBT,IUER)
!     
! --------- First NUM_TPS elements of the TTO frequencies, IF, LO, and SKY
!
            IUER = -1
            CALL RDBIN_ARRAY(LUN,'R8',ANC%NUM_TPS,FRQ_ARR_IFTP,NBT,IUER) 
! ---------
            IUER = -1
            CALL RDBIN_ARRAY(LUN,'R8',ANC%NUM_TPS,FRQ_ARR_LOTP,NBT,IUER) 
! ---------
            IUER = -1
            CALL RDBIN_ARRAY(LUN,'R8',ANC%NUM_TPS,FRQ_TPS_ARR,NBT,IUER) 
!
! --------- First NUM_TPS elements of FRQ_ARR_BWTP
!
            IUER = -1
            CALL RDBIN_ARRAY(LUN,'R8',ANC%NUM_TPS,FRQ_ARR_BWTP,NBT,IUER) 
!
! -------- First NUM_TPS of the polarization array (POL_TPS_ARR)
!
            IUER = -1
            CALL RDBIN_ARRAY(LUN,'I4',ANC%NUM_TPS,POL_TPS_ARR,NBT,IUER)
!
! -------- First NUM_TPS of the sensor and net sideband array
!
            IUER = -1
            CALL RDBIN_ARRAY(LUN,'I4',ANC%NUM_TPS,SSB_TPS_ARR,NBT,IUER)
! ---------
            IUER = -1
            CALL RDBIN_ARRAY(LUN,'I4',ANC%NUM_TPS,NSB_TPS_ARR,NBT,IUER)
!     
! --------- First NUM_TPS of the Identities of the TPS arrays (ID_TPS_ARR)
!
            IUER = -1
            CALL RDBIN_ARRAY (LUN,'I4',ANC%NUM_TPS,ID_TPS_ARR,NBT,IUER)
!
! --------- First NUM_TPS elements of IFIDX_TPS_ARR
!
            IUER = -1
            CALL RDBIN_ARRAY(LUN,'I4',ANC%NUM_TPS,IFIDX_TPS_ARR,NBT,IUER)
!
! --------- First NUM_TPS of NEP_ARR
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_TPS,NEP_ARR,NBT,IUER )
!
! --------- First NUM_TPS of OFF_ARR
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'I8',ANC%NUM_TPS,OFF_ARR,NBT,IUER)
!
! --------- Allocate  ANC%TPS
!
            ALLOCATE ( ANC%TPS( ANC%NUM_TPS ), STAT = IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5067, IUER, 'BNC_PARSE',                  &
     &                 'Error in allocation memory for TP_SENSOR'//     &
     &                 ' while processing binary antenna '//            &
     &                 'calibration file '//TRIM(FILIN) )
               CALL EXIT ( 1 )
            END IF
!
! --------- Allocate ANC%TTO
!
            ALLOCATE ( ANC%TTO(ANC%NUM_EPO_TTO), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5068, IUER, 'BNC_PARSE',                  &
     &                 'Error in allocation memory for ANC%TTO '//      &
     &                 'while processing binary antenna calibration'//  &
     &                 ' file '//FILIN)
               CALL EXIT ( 1 )
            END IF
!
! --------- Allocate each ANC%TTO(i)%TSYS
!
            DO 530 J3 = 1, ANC%NUM_EPO_TTO
               ALLOCATE ( ANC%TTO(J3)%TSYS(ANC%NUM_TPS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5069, IUER, 'BNC_PARSE',               &
     &                    'Error in allocation memory for TSYS '//      &
     &                    'while processing antenna calibration '//     &
     &                    'file '//TRIM(FILIN) )
                  CALL EXIT ( 1 )
               END IF
 530        CONTINUE
!
! --------- Allocate ANC%NEP_ARR
!
            ALLOCATE ( ANC%NEP_ARR( ANC%NUM_TPS ), STAT = IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5070, IUER, 'BNC_PARSE',                     &
     &              'Error in allocation memory for ANC%NEP_ARR' &
     &              //' while processing binary antenna '//      &
     &              'calibration file '//TRIM(FILIN) )
               CALL EXIT ( 1 )
            END IF
!
! ------ Parse the TSYS data to ANC
!
            DO 510 J1 = 1, ANC%NUM_TPS
! ------------
               ANC%TPS(J1)%TAG      =  TAG_TPS_ARR(J1)
               ANC%TPS(J1)%IF_FRQ   =  FRQ_ARR_IFTP(J1)
               ANC%TPS(J1)%LO_FRQ   =  FRQ_ARR_LOTP(J1)
               ANC%TPS(J1)%SKY_FRQ  =  FRQ_TPS_ARR(J1)
               ANC%TPS(J1)%BDW      =  FRQ_ARR_BWTP(J1)
               ANC%TPS(J1)%POL      =  POL_TPS_ARR(J1)
               ANC%TPS(J1)%SSB      =  SSB_TPS_ARR(J1)
               ANC%TPS(J1)%NSB      =  NSB_TPS_ARR(J1)
               ANC%TPS(J1)%ID       =  ID_TPS_ARR(J1)
               ANC%TPS(J1)%IF_IND   =  IFIDX_TPS_ARR(J1)
               ANC%NEP_ARR(J1)      =  NEP_ARR(J1)
!
! ------------ Get time array for this TP_SENSOR
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_TTO,            &
     &                             TIM_TTO_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  CALL CLRCH( STR3 ); CALL IINCH ( NEP_ARR(J1), STR3 )
                  IUER = -1
                  CALL ERR_LOG ( 5071, IUER, 'BNC_PARSE',               &
     &                    'Error in reading TIM_TTO_ARR. NBT = '//      &
     &                    TRIM(STR1)//' IND_FRQ = '//                   &
     &                    TRIM(STR2)//' MEL = '//TRIM(STR3) )
                  CALL EXIT ( 1 )
               END IF
! ------------
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_TTO,            &
     &                             DUR_TTO_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  CALL CLRCH( STR3 ); CALL IINCH ( NEP_ARR(J1), STR3 )
                  IUER = -1
                  CALL ERR_LOG ( 5072, IUER, 'BNC_PARSE',               &
     &                    'Error in reading TIM_TTO_ARR. NBT = '//      &
     &                    TRIM(STR1)//' IND_FRQ = '//                   &
     &                    TRIM(STR2)//' MEL = '//TRIM(STR3) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get Tsys array for this TP_SENSOR
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_TTO, TSYS_ARR,  &
     &                            NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  CALL CLRCH( STR3 ); CALL IINCH ( NEP_ARR(J1), STR3 )
                  IUER = -1
                  CALL ERR_LOG ( 5073, IUER, 'BNC_PARSE',               &
     &                    'Error in reading TSYS_ARR. NBT = '//         &
     &                    TRIM(STR1)//' IND_FRQ = '//TRIM(STR2)//       &
     &                    ' MEL = '//TRIM(STR3) )
                  CALL EXIT ( 1 )
               END IF
!     
!     --------- Get azimuth array for this TP_SENSOR
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_TTO,            &
     &                            AZ_TTO_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  CALL CLRCH( STR3 ); CALL IINCH ( NEP_ARR(J1), STR3 )
                  IUER = -1
                  CALL ERR_LOG ( 5074, IUER, 'BNC_PARSE',               &
     &                    'Error in reading AZ_TTO_ARR. NBT = '//       &
     &                    TRIM(STR1)//' IND_FRQ = '//TRIM(STR2)//       &
     &                    ' MEL = '//TRIM(STR3) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get elevation array for this TP_SENSOR
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_TTO,            &
     &                            EL_TTO_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  CALL CLRCH( STR3 ); CALL IINCH ( NEP_ARR(J1), STR3 )
                  IUER = -1
                  CALL ERR_LOG ( 5075, IUER, 'BNC_PARSE',               &
     &                    'Error in reading EL_TTO_ARR. NBT = '//       &
     &                    TRIM(STR1)//' IND_FRQ = '//TRIM(STR2)//       &
     &                    ' MEL = '//TRIM(STR3) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get Source array for this TP_SENSOR
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'B1', ANC%NUM_EPO_TTO,            &
     &                            SOU_TTO_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  CALL CLRCH( STR3 ); CALL IINCH ( NEP_ARR(J1), STR3 )
                  IUER = -1
                  CALL ERR_LOG ( 5076, IUER, 'BNC_PARSE',               &
     &                    'Error in reading SOU_TTO_ARR. NBT = '//      &
     &                    TRIM(STR1)//' IND_FRQ = '//TRIM(STR2)//       &
     &                    ' MEL = '//TRIM(STR3) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get Scan array for this TP_SENSOR
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'B1', ANC%NUM_EPO_TTO,            &
     &                            SCA_TTO_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  CALL CLRCH( STR3 ); CALL IINCH ( NEP_ARR(J1), STR3 )
                  IUER = -1
                  CALL ERR_LOG ( 5077, IUER, 'BNC_PARSE',               &
     &                    'Error in reading SCA_TTO_ARR. NBT = '//      &
     &                    TRIM(STR1)//' IND_FRQ = '//TRIM(STR2)//       &
     &                    ' MEL = '//TRIM(STR3) )
                  CALL EXIT ( 1 )
               END IF
!
! ------------ Get Scan index
!
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_EPO_TTO,            &
     &                            INDSC_TTO_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  CALL CLRCH( STR3 ); CALL IINCH ( NEP_ARR(J1), STR3 )
                  IUER = -1
                  CALL ERR_LOG ( 5078, IUER, 'BNC_PARSE',               &
     &                    'Error in reading INDSC_TTO_ARR. NBT = '//    &
     &                    TRIM(STR1)//' IND_FRQ = '//TRIM(STR2)//       &
     &                    ' MEL = '//TRIM(STR3) )
                  CALL EXIT ( 1 )
               END IF
!
! ------------ Parse Total ATMospheric Brightness Temperature to file, 
!              if it is there.
!
               IF ( ANC%NUM_TATM > 0 ) THEN
                  IUER = -1
                  CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_TTO,         &
     &                               TATM_ARR, NBT, IUER)
                  IF ( IUER .NE. 0 ) THEN
                     CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                     CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                     CALL CLRCH( STR3 ); CALL IINCH ( NEP_ARR(J1), STR3)
                     IUER = -1
                     CALL ERR_LOG ( 5079, IUER, 'BNC_PARSE',            &
     &                       'Error in reading TATM_ARR. NBT = '//      &
     &                       TRIM(STR1)//' IND_FRQ = '//TRIM(STR2)//    &
     &                       ' MEL = '//TRIM(STR3) )
                     CALL EXIT ( 1 )
                  END IF
               END IF
!
! ------------ Parse Opacity Temperature to file, if it is there.
!
               IF ( ANC%NUM_OPA > 0 ) THEN
                  IUER = -1
                  CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_TTO,         &
     &                               OPA_ARR, NBT, IUER)
                  IF ( IUER .NE. 0 ) THEN
                     CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                     CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                     CALL CLRCH( STR3 ); CALL IINCH ( NEP_ARR(J1), STR3)
                     IUER = -1
                     CALL ERR_LOG ( 5080, IUER, 'BNC_PARSE',            &
     &                       'Error in reading OPA_ARR. NBT = '//      &
     &                       TRIM(STR1)//' IND_FRQ = '//TRIM(STR2)//    &
     &                       ' MEL = '//TRIM(STR3) )
                     CALL EXIT ( 1 )
                  END IF
               END IF
!
! ------------
!
               DO 520 J2 = 1, ANC%NUM_EPO_TTO
! ---------------
                  ANC%TTO(J2)%TIM       =  REAL( TIM_TTO_ARR(J2),  8 )
                  ANC%TTO(J2)%DUR       =  REAL( DUR_TTO_ARR(J2),  8 )
                  ANC%TTO(J2)%TSYS(J1)  =  REAL( TSYS_ARR(J2), 8 )
                  ANC%TTO(J2)%AZ        =  REAL( AZ_TTO_ARR(J2), 8 )
                  ANC%TTO(J2)%EL        =  REAL( EL_TTO_ARR(J2), 8 )
                  ANC%TTO(J2)%IND_SCA   =  INDSC_TTO_ARR(J2)
                  ANC%TTO(J2)%SOU_NAM   =  SOU_TTO_ARR(J2)
                  ANC%TTO(J2)%SCA_NAM   =  SCA_TTO_ARR(J2)
! ---------------
                  IF ( ANC%NUM_TATM > 0 ) THEN
                     ANC%TTO(J2)%TATM(J1)  =  REAL( TATM_ARR(J2), 8 )
                  END IF
                  IF ( ANC%NUM_OPA > 0 ) THEN
                     ANC%TTO(J2)%OPA(J1)  =  REAL( OPA_ARR(J2), 8 )
                  END IF
 520           CONTINUE
 510        CONTINUE
         END IF
!========================================================================================
!{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!     
! ------ If the file has PCAL data, parse it
!
         IF ( ANC%NUM_PCS > 0 .AND. ANC%NUM_PCAL > 0 ) THEN
! --------- 
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'I8', ANC%NUM_PCS,                  &
     &                         TAG_PCS_ARR, NBT, IUER )                                             
! ---------
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'I8', ANC%NUM_PCS,                  &
     &                         FRQ_PCS_ARR, NBT, IUER )
! ---------
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_PCS,                  &
     &                         POL_PCS_ARR, NBT, IUER )                                      
! --------- 
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'I8', ANC%NUM_PCS,                  &
     &                         ID_PCS_ARR, NBT, IUER )                                          
! ---------
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_PCS,                  &
     &                         IFIDX_PCS_ARR, NBT, IUER )            
! ---------
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_PCS,                  &
     &                         SSB_PCS_ARR, NBT, IUER )
! ---------
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_PCS,                  &
     &                         NSB_PCS_ARR, NBT, IUER )
!     
! --------- Allocate  ANC%PCS
!     
            ALLOCATE ( ANC%PCS( ANC%NUM_PCS ), STAT = IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5081, IUER, 'BNC_PARSE',                  &
     &              'Error in allocation memory for PC_SENSOR '//       &
     &              'while processing binary antcal file '//FILIN )
               CALL EXIT ( 1 )
            END IF
!     
! --------- Allocate ANC%PCAL
!     
            ALLOCATE ( ANC%PCAL(ANC%NUM_EPO_PCAL), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5082, IUER, 'BNC_PARSE',                  &
     &              'Error in allocation memory for ANC%PCAL '//        &
     &              'while processing binary antcal file '//FILIN )
               CALL EXIT ( 1 )
            END IF
!     
! --------- Allocate each PCAL
!     
            DO 532 J3 = 1, ANC%NUM_EPO_PCAL
               ALLOCATE ( ANC%PCAL(J3)%PCAL_CMPL(ANC%NUM_PCS), STAT=IER)
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5083, IUER, 'BNC_PARSE',               &
     &                    'Error in allocation memory for PCAL_CMPL '// &
     &                    'while processing antcal file '//FILIN )
                  CALL EXIT ( 1 )
               END IF
 532        CONTINUE
!     
! --------- Parse the PCAL data to ANC
!     
            DO 512 J1 = 1, ANC%NUM_PCS
! ------------
               ANC%PCS(J1)%TAG      =  TAG_PCS_ARR(J1)            
               ANC%PCS(J1)%SKY_FRQ  =  FRQ_PCS_ARR(J1)
               ANC%PCS(J1)%POL      =  POL_PCS_ARR(J1)
               ANC%PCS(J1)%ID       =  ID_PCS_ARR(J1)
               ANC%PCS(J1)%IF_IND   =  IFIDX_PCS_ARR(J1)
               ANC%PCS(J1)%SSB      =  SSB_PCS_ARR(J1)
               ANC%PCS(J1)%NSB      =  NSB_PCS_ARR(J1)
!
! ------------ Get PCAL time array for this PC_SENSOR
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_PCAL,              &
     &                            TIM_PCAL_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5084, IUER, 'BNC_PARSE',               &
     &                 'Error in reading TIM_PCAL_ARR. '//              &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!
! ------------ Get PCAL Duration array for this PC_SENSOR
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_PCAL,           &
     &                            DUR_PCAL_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5085, IUER, 'BNC_PARSE',               &
     &                 'Error in reading DUR_PCAL_ARR. '//              &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get PCAL array for this PC_SENSOR
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R8', ANC%NUM_EPO_PCAL,           &
     &                            PCAL_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5086, IUER, 'BNC_PARSE',               &
     &                 'Error in reading PCAL_ARR. '//                  &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!
! ------------ Get Scan index
!
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_EPO_PCAL,           &
     &                            INDSC_PCAL_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5087, IUER, 'BNC_PARSE',               &
     &                    'Error in reading INDSC_PCAL_ARR. NBT = '//    &
     &                    TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!
! ------------ Get Source array for this PC_SENSOR
!
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'B1', ANC%NUM_EPO_PCAL,              &
     &                            SOU_PCAL_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5088, IUER, 'BNC_PARSE',               &
     &                    'Error in reading SOU_PCAL_ARR. '//           &
     &                    'NBT = '//TRIM(STR1)//                        &
     &                    ' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get Scan array for this PC_SENSOR
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'B1', ANC%NUM_EPO_PCAL,              &
     &                            SCA_PCAL_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5089, IUER, 'BNC_PARSE',               &
     &                    'Error in reading SCA_PCAL_ARR. '//           &
     &                    'NBT = '//TRIM(STR1)//                        &
     &                    ' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------
!     
               DO 522 J2 = 1, ANC%NUM_EPO_PCAL
                  ANC%PCAL(J2)%TIM      =  REAL( TIM_PCAL_ARR(J2), 8 )
                  ANC%PCAL(J2)%DUR      =  REAL( DUR_PCAL_ARR(J2), 8 )
                  ANC%PCAL(J2)%PCAL_CMPL(J1) =  PCAL_ARR(J2)
                  ANC%PCAL(J2)%IND_SCA       =  INDSC_PCAL_ARR(J2)
                  ANC%PCAL(J2)%SOU_NAM       =  SOU_PCAL_ARR(J2)
                  ANC%PCAL(J2)%SCA_NAM       =  SCA_PCAL_ARR(J2)
 522           CONTINUE
 512        CONTINUE
         END IF
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!
! ------ If the antcal file has TPI data, then write it to the binary file
!
         IF ( ANC%NUM_TPS > 0 .AND. ANC%NUM_TPI > 0 ) THEN
!
! --------- Allocate the TPI derived type
!
            ALLOCATE ( ANC%TPI ( ANC%NUM_EPO_TPI ), STAT = IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5090, IUER, 'BNC_PARSE',                  &
     &              'Error in allocation memory for ANC%TPI while '//   &
     &              'processing binary antcal file '//TRIM(FILIN) )
               CALL EXIT ( 1 )
            END IF
!     
! --------- Allocate each TPION, TPIOFF, and TPIZERO
!     
            DO 551 J1 = 1, ANC%NUM_EPO_TPI
               ALLOCATE ( ANC%TPI(J1)%TPION(ANC%NUM_TPS),  STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5091, IUER, 'BNC_PARSE',               &
     &                    'Error in allocation memory for '//           &
     &                    'ANC%TPION while processing antenna '//       &
     &                   'calibration file '//TRIM(FILIN) )
                  CALL EXIT ( 1 )
               END IF
! ------------
               ALLOCATE ( ANC%TPI(J1)%TPIOFF(ANC%NUM_TPS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5092, IUER, 'BNC_PARSE',               &
     &                    'Error in allocation memory for '//           &
     &                    'ANC%TPIOFF while processing antenna '//      &
     &                    'calibration file '//TRIM(FILIN) )
                  CALL EXIT ( 1 )
               END IF
! ------------
               ALLOCATE ( ANC%TPI(J1)%TPIZERO(ANC%NUM_TPS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5093, IUER, 'BNC_PARSE',               &
     &                    'Error in allocation memory for '//           &
     &                    'ANC%TPIZERO while processing antenna '//     &
     &                    'calibration file '//TRIM(FILIN) )
                  CALL EXIT ( 1 )
               END IF
 551        CONTINUE
! --------- 
            DO 552 J2 = 1, ANC%NUM_TPS
!
! ------------ Get the time array from binary file
!
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_TPI,            &
     &                            TIM_TPI_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5094, IUER, 'BNC_PARSE',               &
     &                    'Error in reading TIM_TPI_ARR. '//            &
     &                    'NBT = '//TRIM(STR1)//' IND_FRQ = '//         &
     &                     TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!
! ------------ Get the Duration array from binary file
!
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_TPI,            &
     &                            DUR_TPI_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5095, IUER, 'BNC_PARSE',               &
     &                    'Error in reading DUR_TPI_ARR. '//            &
     &                    'NBT = '//TRIM(STR1)//' IND_FRQ = '//         &
     &                     TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get the TPION array
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_EPO_TPI,            &
     &                            TPION_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5096, IUER, 'BNC_PARSE',               &
     &                    'Error in reading TPION_ARR. '//              &
     &                    'NBT = '//TRIM(STR1)//' IND_FRQ = '//         &
     &                    TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get the TPIOFF array
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_EPO_TPI,            &
     &                            TPIOFF_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5097, IUER, 'BNC_PARSE',               &
     &                    'Error in reading TPIOFF_ARR. '//             &
     &                    'NBT = '//TRIM(STR1)//' IND_FRQ = '//         &
     &                    TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get the TPZEROF array
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_EPO_TPI,            &
     &                            TPIZERO_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5100, IUER, 'BNC_PARSE',               &
     &                    'Error in reading TPIZERO_ARR. '//            &
     &                    'NBT = '//TRIM(STR1)//' IND_FRQ = '//         &
     &                    TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!
! ------------ Get the AZ_TPI array
!
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_TPI,            &
     &                            AZ_TPI_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5101, IUER, 'BNC_PARSE',               &
     &                 'Error in reading AZ_TPI_ARR. '//                &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get the EL_TPI array
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_TPI,            &
     &                            EL_TPI_ARR, NBT, IUER ) 
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5102, IUER, 'BNC_PARSE',               &
     &                    'Error in reading EL_TPI_ARR. '//             &
     &                    'NBT = '//TRIM(STR1)//' IND_FRQ = '//         &
     &                    TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!
! ------------ Get Scan index
!
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_EPO_TPI,            &
     &                            INDSC_TPI_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5103, IUER, 'BNC_PARSE',               &
     &                    'Error in reading INDSC_TPI_ARR. NBT = '//    &
     &                    TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!
! ------------ 
!
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'B1', ANC%NUM_EPO_TPI,            &
     &                            SOU_TPI_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5104, IUER, 'BNC_PARSE',               &
     &                    'Error in reading SOU_TPI_ARR. '//            &
     &                    'NBT = '//TRIM(STR1)//                        &
     &                    ' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT (1)
               END IF
!     
! ------------ 
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'B1', ANC%NUM_EPO_TPI,            &
     &                            SCA_TPI_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5105, IUER, 'BNC_PARSE',               &
     &                    'Error in reading SOU_TPI_ARR. '//            &
     &                    'NBT = '//TRIM(STR1)//                        &
     &                    ' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT (1)
               END IF
!     
! ------------- 
!     
               DO 553 J3 = 1, ANC%NUM_EPO_TPI
                  ANC%TPI(J3)%TIM         =  REAL ( TIM_TPI_ARR(J3), 8 )
                  ANC%TPI(J3)%DUR         =  REAL ( DUR_TPI_ARR(J3), 8 )
                  ANC%TPI(J3)%TPION(J2)   =  TPION_ARR(J3)
                  ANC%TPI(J3)%TPIOFF(J2)  =  TPIOFF_ARR(J3)
                  ANC%TPI(J3)%TPIZERO(J2) =  TPIZERO_ARR(J3)
                  ANC%TPI(J3)%AZ          =  REAL ( AZ_TPI_ARR(J3), 8 )
                  ANC%TPI(J3)%EL          =  REAL ( EL_TPI_ARR(J3), 8 )
                  ANC%TPI(J3)%IND_SCA     =  INDSC_TPI_ARR(J3)
                  ANC%TPI(J3)%SOU_NAM     =  SOU_TPI_ARR(J3)
                  ANC%TPI(J3)%SCA_NAM     =  SCA_TPI_ARR(J3)
 553           CONTINUE
 552        CONTINUE
         END IF
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!####################################################################################
!     
! ------ If the antcal file has Formatter data
!     
         IF ( ANC%NUM_TGPS > 0 .AND. ANC%NUM_GPS > 0 ) THEN
!
! --------- Copy the timer tags to array
!
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_TGPS,                 &
     &                         TAG_GPS_ARR, NBT, IUER )
!     
! --------- Copy the timer boards to array
!     
            IUER = -1
            CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_TGPS,                 &
     &                         TGPS_BOARD_ARR, NBT, IUER )
!     
! --------- Allocate ANC%TGPS
!     
            ALLOCATE ( ANC%TGPS( ANC%NUM_TGPS ), STAT = IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5106, IUER, 'BNC_PARSE',                  &
     &                 'Error in allocation memory for FMT2GPS while '  &
     &               //'processing binary antcal file '//TRIM(FILIN) )
               CALL EXIT ( 1 )
            END IF
!     
! --------- Allocate ANC%GPS
!
            ALLOCATE ( ANC%GPS( ANC%NUM_EPO_GPS ), STAT = IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5107, IUER, 'BNC_PARSE',                  &
     &                 'Error in allocation memory for FMTGPS while '   &
     &               //'processing binary antcal file '//TRIM(FILIN) )
               CALL EXIT ( 1 )
            END IF
!     
! --------- Allocate each FMTGPS, and FMTPPS
!     
            DO 513 J1 = 1, ANC%NUM_EPO_GPS
! ------------
               ALLOCATE ( ANC%GPS(J1)%FMG(ANC%NUM_TGPS), STAT = IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5108, IUER, 'BNC_PARSE',               &
     &                    'Error allocating memory for ANC%GPS%FMG '//  &
     &                    'processing binary antcal file '//TRIM(FILIN))
                  CALL EXIT ( 1 )
               END IF     
! ------------
               ALLOCATE ( ANC%GPS(J1)%FMP(ANC%NUM_TGPS), STAT = IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5109, IUER, 'BNC_PARSE',               &
     &                    'Error allocating memory for ANC%GPS%FMP '//  &
     &                    'processing binary antcal file '//TRIM(FILIN))
                  CALL EXIT ( 1 )
               END IF
 513        CONTINUE
!     
! ---------
!     
            DO 523 J2 = 1, ANC%NUM_TGPS
!     
! ------------ Write the Tag and board information to array
!     
               ANC%TGPS(J2)%TAG    =  TAG_GPS_ARR(J2)
               ANC%TGPS(J2)%BOARD  =  TGPS_BOARD_ARR(J2)
!
! ------------ Get the time array from binary file
!
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_GPS,            &
     &                            TIM_GPS_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5110, IUER, 'BNC_PARSE',               &
     &                    'Error in reading TIM_GPS_ARR. NBT = '//      &
     &                    TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get the formatter minus GPS time
!
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_GPS,            &
     &                            FMGPS_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5111, IUER, 'BNC_PARSE',               &
     &                    'Error in reading FMGPS_ARR. NBT = '//        &
     &                    TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!     
! ------------ Get the formatter minus PPS time
!
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_GPS,            &
     &                            FMPPS_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5112, IUER, 'BNC_PARSE',               &
     &                    'Error in reading FMPPS_ARR. NBT = '//        &
     &                    TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!
! ------------ Get Scan index
!
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_EPO_GPS,            &
     &                            INDSC_GPS_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5113, IUER, 'BNC_PARSE',               &
     &                    'Error in reading INDSC_GPS_ARR. NBT = '//    &
     &                    TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
!
! ------------ 
!
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'B1', ANC%NUM_EPO_GPS,            &
     &                            SOU_GPS_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5114, IUER, 'BNC_PARSE',               &
     &                    'Error in reading SOU_GPS_ARR. '//            &
     &                    'NBT = '//TRIM(STR1)//                        &
     &                    ' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT (1)
               END IF
!     
! ------------ 
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'B1', ANC%NUM_EPO_GPS,            &
     &                            SCA_GPS_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5115, IUER, 'BNC_PARSE',               &
     &                    'Error in reading SOU_GPS_ARR. '//            &
     &                    'NBT = '//TRIM(STR1)//                        &
     &                    ' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT (1)
               END IF
!     
! ------------ Parse to derived type
!     
               DO 533 J3 = 1, ANC%NUM_EPO_GPS
                  ANC%GPS(J3)%TIM      =  REAL ( TIM_GPS_ARR(J3), 8 )
                  ANC%GPS(J3)%FMG(J2)  =  REAL ( FMGPS_ARR(J3),   8 )
                  ANC%GPS(J3)%FMP(J2)  =  REAL ( FMPPS_ARR(J3),   8 )
                  ANC%GPS(J3)%IND_SCA     =  INDSC_GPS_ARR(J3)
                  ANC%GPS(J3)%SOU_NAM     =  SOU_GPS_ARR(J3)
                  ANC%GPS(J3)%SCA_NAM     =  SCA_GPS_ARR(J3)
 533           CONTINUE 
 523        CONTINUE 
         END IF
!####################################################################################
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! ------ If the file has SEFD data
!     
         IF ( ANC%NUM_SEFD > 0 ) THEN
!
! --------- Allocate ANC%SEFD
!
            ALLOCATE ( ANC%SEFD( ANC%NUM_EPO_SEFD ), STAT = IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5028, IUER, 'BNC_PARSE',                  &
     &              'Error in allocation memory for ANC%SEFD while '//  &
     &              'processing binary antcal file '//TRIM(FILIN) )
               CALL EXIT ( 1 )
            END IF
!
! --------- Allocate each pointer
!
            DO 514 J1 = 1, ANC%NUM_EPO_SEFD
! ------------
               ALLOCATE ( ANC%SEFD(J1)%SEFD(ANC%NUM_TPS), STAT = IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5029, IUER, 'BNC_PARSE',               &
     &                 'Error in allocation memory for ANC%SEFD%SEFD'// &
     &                 ' processing binary antcal file '//TRIM(FILIN) )
                  CALL EXIT ( 1 )
               END IF     
! ------------
               ALLOCATE ( ANC%SEFD(J1)%TSYS(ANC%NUM_TPS), STAT = IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5030, IUER, 'BNC_PARSE',               &
     &                 'Error in allocation memory for ANC%SEFD%TSYS'// &
     &                 ' processing binary antcal file '//TRIM(FILIN) )
                  CALL EXIT ( 1 )
               END IF
! ------------
               ALLOCATE ( ANC%SEFD(J1)%TCAL(ANC%NUM_TPS), STAT = IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5031, IUER, 'BNC_PARSE',               &
     &                 'Error in allocation memory for ANC%SEFD%TCAL'// &
     &                 ' processing binary antcal file '//TRIM(FILIN) )
                  CALL EXIT ( 1 )
               END IF
! ------------
               ALLOCATE ( ANC%SEFD(J1)%TRAT(ANC%NUM_TPS), STAT = IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5031, IUER, 'BNC_PARSE',               &
     &                    'Error allocating memory for ANC%SEFD%TRAT '  &
     &                  //'processing binary antcal file '//TRIM(FILIN))
                  CALL EXIT ( 1 )
               END IF
! ------------
               ALLOCATE ( ANC%SEFD(J1)%GAIN(ANC%NUM_TPS), STAT = IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5032, IUER, 'BNC_PARSE',               &
     &                 'Error in allocation memory for ANC%SEFD%GAIN'// &
     &                 ' processing binary antcal file '//TRIM(FILIN))
                  CALL EXIT ( 1 )
               END IF
 514        CONTINUE
!
! ---------
!
            DO 524 J2 = 1, ANC%NUM_TPS
!     
! ------------ Get the time array from binary file
!     
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_SEFD,           &
     &                             TIM_SEFD_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5033, IUER, 'BNC_PARSE',               &
     &                 'Error in reading TIM_SEFD_ARR. '//              &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
! ------------
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_SEFD, SEFD_ARR, &
     &                            NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5034, IUER, 'BNC_PARSE',               &
     &                 'Error in reading SEFD_ARR. '//                  &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
! ------------
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_SEFD,           &
     &                            TSYS_SEFD_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5035, IUER, 'BNC_PARSE',               &
     &                 'Error in reading TSYS_SEFD_ARR. '//             &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
! ------------
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_SEFD,           &
     &                            TCAL_SEFD_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5036, IUER, 'BNC_PARSE',               &
     &                 'Error in reading TCAL_SEFD_ARR. '//             &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
! ------------
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_SEFD,           &
     &                            TRAT_SEFD_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5036, IUER, 'BNC_PARSE',               &
     &                 'Error in reading TCAL_SEFD_ARR. '//             &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
! ------------
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_SEFD,           &
     &                             GAIN_SEFD_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5037, IUER, 'BNC_PARSE',               &
     &                 'Error in reading GAIN_SEFD_ARR. '//             &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
! ------------
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'I4', ANC%NUM_EPO_SEFD,           &
     &                            INDSC_SEFD_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J1,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5113, IUER, 'BNC_PARSE',               &
     &                    'Error in reading INDSC_SEFD_ARR. NBT = '//   &
     &                    TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
! ------------
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_SEFD,           &
     &                            AZ_SEFD_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5038, IUER, 'BNC_PARSE',               &
     &                 'Error in reading AZ_SEFD_ARR. '//               &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF
! ------------
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'R4', ANC%NUM_EPO_SEFD,           &
     &                            EL_SEFD_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5039, IUER, 'BNC_PARSE',               &
     &                 'Error in reading EL_SEFD_ARR. '//               &
     &                 'NBT = '//TRIM(STR1)//' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT ( 1 )
               END IF            
! ---------
               IUER = -1
               CALL RDBIN_ARRAY ( LUN, 'I8', ANC%NUM_EPO_SEFD,           &
     &                            SOU_SEFD_ARR, NBT, IUER )
               IF ( IUER .NE. 0 ) THEN
                  CALL CLRCH( STR1 ); CALL IINCH ( NBT, STR1 )
                  CALL CLRCH( STR2 ); CALL IINCH ( J2,  STR2 )
                  IUER = -1
                  CALL ERR_LOG ( 5040, IUER, 'BNC_PARSE',               &
     &                 'Error in reading SOU_SEFD_ARR. '//              &
     &                 'NBT = '//TRIM(STR1)//                           &
     &                 ' IND_FRQ = '//TRIM(STR2) )
                  CALL EXIT (1)
               END IF
! ---------
               DO 534 J3 = 1, ANC%NUM_EPO_SEFD
                  ANC%SEFD(J3)%TIM       =  REAL ( TIM_SEFD_ARR(J3), 8 )
                  ANC%SEFD(J3)%SEFD(J2)  =  REAL ( SEFD_ARR(J3), 8 )
                  ANC%SEFD(J3)%TSYS(J2)  =  REAL ( TSYS_SEFD_ARR(J3), 8 )
                  ANC%SEFD(J3)%TCAL(J2)  =  REAL ( TCAL_SEFD_ARR(J3), 8 )
                  ANC%SEFD(J3)%TRAT(J2)  =  REAL ( TRAT_SEFD_ARR(J3), 8 )
                  ANC%SEFD(J3)%GAIN(J2)  =  REAL ( GAIN_SEFD_ARR(J3), 8 )
                  ANC%SEFD(J3)%IND_SCA   =  INDSC_SEFD_ARR(J3)
                  ANC%SEFD(J3)%AZ        =  REAL ( AZ_SEFD_ARR(J3), 8 )
                  ANC%SEFD(J3)%EL        =  REAL ( EL_SEFD_ARR(J3), 8 )
                  ANC%SEFD(J3)%SOU_NAM   =  SOU_SEFD_ARR(J3)
 534           CONTINUE
 524        CONTINUE
         END IF
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      END IF
! ---
      CALL ERR_PASS (IER, IUER )      
!
! --- Close binary file
!
      CALL BINF_CLOSE ( LUN, IUER )
! ---
      RETURN
      END  SUBROUTINE BNC_PARSE  !#!
