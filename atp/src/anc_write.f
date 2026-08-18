      SUBROUTINE ANC_WRITE ( ANC, IV_PRV, L_PRV, C_PRV, NERS,           &
     &                       FILOUT, IUER )
!
! *****************************************************************************
! *                                                                           *
! *   Routine ANC_WRITE                                                       *
! *                                                                           *
! *   INPUT:                                                                  *
! *       ANC    =  Parsed Antenna Calibration file        { DERIVED TYPE }   *
! *                                                                           *
! *       IV_PRV =  Version of Provenance to write                            *
! *                 == 1 : Version 1 of anc file. This output is similar      *
! *                        to output of log2ant, less comments.               *
! *                 == 2 : Version 2 of anc file. Scan averaged values        *
! *                 == 3 : Version 3 of anc file. Includes atmospheric        *
! *                         brightnes temperature and atmospheric opacity.    *      
! *                                                                           *
! *       L_PRV  =                                         { INT }            *
! *                 If zero then write the provenance as given in ANC%PRV     *
! *                 Else augment provenance with contents of C_PRV            *
! *                                                                           *
! *       C_PRV  =  Character holding the provenance block { CHAR }
! *                                                                           *
! *       NERS   =  Initialised NERS Package               { DERIVED TYPE }   *
! *                 Network Earth Rotation Service                            *
! *                                                                           *
! *       IUER   =  Error Handler                          { INT, OPT }       *
! *                 If IUER=0 no error message will be printed, even in the   *
! *                 event of an error. However, for other possible values,    *
! *                 i.e. IUER=-1,-2, & -3, the error message will print to    *
! *                 screen. For the latter case, i.e., IUER = -3, after       *
! *                 printing the program will terminate.                      *
! *                 Default, IUER = -1                                        *
! *                                                                           *
! *   OUTPUT:                                                                 *
! *       FILOUT   =  ASCII  antcal File                   { CHAR }           *
! *                   N.B: If declared file exists, it will be overwritten    *
! *                                                                           *
! *   Copyright (c) 1975-2025 United States Government as represented by      *
! *   the Administrator of the National Aeronautics and Space                 *
! *   Administration. All Rights Reserved.                                    *
! *   License: NASA Open Source Software Agreement (NOSA).                    *
! *                                                                           *
! *  ### 25-AUG-2025   ANC_WRITE      v1.0 (d)  N. Habana  21-SEP-2025  ###   *
! *                                                                           *
! *****************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'atp.i'
      INCLUDE   'ners.i'
      TYPE     ( ANC__TYP   ) :: ANC      
      TYPE     ( NERS__TYPE ) :: NERS
      CHARACTER  FILOUT*(*), C_PRV(ANC__MPRV)*(*)
      INTEGER*4  IUER, IV_PRV, L_PRV, IL
      INTEGER*4  IPP, IPC, IP1, IP2
      INTEGER*4  J1, J2, J3, J4, J5, J6, IER
      CHARACTER  CDATE1*30, CDATE2*30, STR*128, STR1*128
      REAL*8     PHAS, AMPL, EPS
      PARAMETER  ( EPS = 1.D-4 )
      INTEGER*4  MIND, LIND
      PARAMETER  ( MIND = 32 )
      CHARACTER  DELIM*3
      INTEGER*4  IND(2,MIND), LUN
      PARAMETER  ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9) )
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, GET_UNIT
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, TIM_TO_DATE*23
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
!
! --- Open the file to write to
!
      LUN=GET_UNIT()
      OPEN (UNIT=LUN, FILE = FILOUT, STATUS = 'UNKNOWN' )
!
! --- Write the contents of line to file for the 2021 version of log2ant
!
      IF ( ANC%ANTCAL_FMT ==  ANTCAL__FMT_1 ) THEN
         CALL ERR_LOG ( 6101, IUER, 'ANC_WRITE',                        &
     &           'Trap of internal control: attempt to write in '//     &
     &           ANTCAL__FMT_1//'. This case is not supported '//       &
     &           'any more' )
         RETURN 
      END IF
!
! ------ Write the format, station, Experiment code
!
!
! ------ Fillers
!
      WRITE( LUN, '(A)' ) ANC%ANTCAL_FMT
      WRITE( LUN, '(A)' ) '#'
      WRITE( LUN, '(A)' ) '# Fillers for uninitialized double, '//      &
     &                    'integer, string and date-time values'
      WRITE( LUN, '(A)' ) '# Filler values'
      WRITE( LUN, '(A)' ) '#           Real  Integer  Character Date'
      WRITE( LUN, '(A)' ) '#'
      WRITE( LUN, 202   ) ANC%FILLER_R8, ANC%FILLER_I4, ANC%FILLER_CH,  &
     &                    ANC%FILLER_DATE
 202  FORMAT ( "FILLERS:   ", F5.1, 2X, I5, 4X, A6, 4X, A19 )
      WRITE( LUN, '(A)' ) '#'
      WRITE( LUN, '(A)' ) 'STATION:  '//ANC%STA_NAM
      WRITE( LUN, '(A)' ) '#'
      WRITE( LUN, '(A)' ) 'EXP_CODE: '//ANC%EXP_CODE
      WRITE( LUN, 101   ) ANC%UTC_MTAI
 101  FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "#", 9X, "sec",                                             /  &
     &   "#",                                                        /  &
     &   "UTC_MTAI: ", F5.1,                                         /  &
     &   "#" )               
!     
! ------ Provenance
!
      IF ( IV_PRV .GT. ANC%NUM_PRV + 1 ) THEN
         CALL CLRCH ( STR )
         CALL INCH  ( IV_PRV, STR )
         CALL CLRCH ( STR1 )
         CALL INCH  ( ANC%NUM_PRV, STR1 )
         CALL ERR_LOG ( 6106, IUER, 'ANC_WRITE',                        &
     &           'Provenance verison is '//TRIM(STR)//                  &  
     &           ' which is much greater than the the number of '//     &
     &            'provenance blocks '//TRIM(STR1) )
         RETURN 
      END IF
         
! ------ Update the provenance if we have information on C_PRV
!
      IF ( IV_PRV .GE. 1 .AND. L_PRV .GE. 1 ) THEN
         IF ( IV_PRV == ANC%NUM_PRV + 1 ) THEN
            ANC%NUM_PRV = IV_PRV
         END IF
         ANC%PRV(IV_PRV)%N_TEXT = L_PRV
         DO 631 J1=1,L_PRV
            ANC%PRV(IV_PRV)%TEXT(J1) = C_PRV(J1)
 631     CONTINUE 
      END IF
      WRITE ( LUN, 103 ) ANC%NUM_PRV
 103  FORMAT ( "#" /                                                    &
     &        "NUM_PROVENANCE: ", I8 )
!
      DO 410 J1 = 1, ANC%NUM_PRV
         WRITE ( LUN, '(A)' ) '#'
         DO 420 J2 = 1,ANC%PRV(J1)%N_TEXT
            WRITE ( LUN, "('PROVENANCE: ', I1, 1X, A)" ) J1, TRIM(ANC%PRV(J1)%TEXT(J2))
 420     CONTINUE
 410  CONTINUE
!
! ------ Data On
!
      IF ( ANC%NUM_DOO > 0 ) THEN
!
         WRITE (LUN, 204) ANC%NUM_DOO
!
 204     FORMAT(                                                        &
     &   "#",                                                        /  &
     &   "DIMENSION: DATA_ON ",I8,                                   /  &
     &   "#",                                                        /  &
     &   "#", 7X, "ScanIdx Data_on UTC start", 7X, "Data_on UTC end",   &
     &        9X, "Source    ScanName",                              /  &
     &   "#", 15X, "YYYY.DD.MM-hh:mm:ss.ff  YYYY.DD.MM-hh:mm:ss.ff"  /  &
     &   "#" )
!            
         DO 431 J1 = 1, ANC%NUM_DOO
!
! --------- Convert the date from MJD, TAI to UTC YYYY.DD.MM-hh:mm:ss.ff
!
            CALL ERR_PASS ( IUER, IER )
            CDATE1 = MJDSEC_TO_DATE ( ANC%MJD_DOO, ANC%TAI_DOO +     &
     &           ANC%UTC_MTAI + ANC%DOO(J1)%TIM(1), IER  )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6110, IUER, 'ANC_WRITE',               &
     &              'Error in writing DOO date'  )
               RETURN
            END IF
            CALL ERR_PASS ( IUER, IER )
            CDATE2 = MJDSEC_TO_DATE ( ANC%MJD_DOO, ANC%TAI_DOO +     &
     &           ANC%UTC_MTAI + ANC%DOO(J1)%TIM(2), IER  )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6111, IUER, 'ANC_WRITE',               &
     &              'Error in writing DOO date'  )
               RETURN
            END IF
!              
            WRITE(LUN,1001) J1, CDATE1(1:22), CDATE2(1:22),             &
     &                      (ANC%DOO(J1)%SOU_NAM),                      &
     &                      (ANC%DOO(J1)%SCA_NAM)
!
!DATA_ON:      1 2025.07.22-17:30:00.01  2025.07.22-17:30:20.01  1803+784  203-1730a
 1001       FORMAT("DATA_ON: ", I6, X, A22, 2X, A22, 2X, A8, 2X, A16 )
!            
 431     CONTINUE
      END IF
      WRITE (LUN,115)
!
! ------ Cable block
!
      IF ( ANC%NUM_CBL > 0 ) THEN
!
         WRITE (LUN, 214) ANC%NUM_CBL
!
 214     FORMAT(                                                        &
     &   "#",                                                        /  &
     &   "DIMENSION: CABLE ", I8,                                    /  &
     &   "#",                                                        /  &
     &   "#     EpocInd  UTC Date", 17X, "Cable delay   Source",        &
     &        4X, "ScanName",                                        /  &
     &   "#", 14X, "YYYY.DD.MM-hh:mm:ss.ff   s",                     /  &
     &   "#" )
        
!     
         DO 441 J1 = 1, ANC%NUM_CBL
!
! ------------ Convert the date from MJD, TAI to UTC YYYY.DD.MM-hh:mm:ss.ff
!
            CALL ERR_PASS ( IUER, IER )
            CDATE1 = MJDSEC_TO_DATE ( ANC%MJD_CBL, ANC%TAI_CBL +     &
     &                      ANC%UTC_MTAI + ANC%CBL(J1)%TIM, IER  )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6112, IUER, 'ANC_WRITE',               &
     &                         'Error in writing MET date'  )
               RETURN
            END IF
!              
            WRITE(LUN,1002) J1, CDATE1(1:22), ANC%CBL(J1)%DELAY,        &
     &                      ANC%CBL(J1)%SOU_NAM,                        &
     &                      ANC%MET(J1)%SCA_NAM
!
!CABLE:      4  2025.07.17-17:39:08.45   4.33150E-10   0059+581  198-1739
 1002       FORMAT("CABLE:  ",I6, 2X, A22, 2X, 1PE12.5, 3X, A8, 2X, A16)
!           
 441     CONTINUE
      END IF
      WRITE (LUN,115)
!
! ------ Meteo Block
!
      IF ( ANC%NUM_MET > 0 ) THEN
!
         WRITE (LUN, 205) ANC%NUM_MET
!
 205     FORMAT(                                                           &
     &   "#",                                                        /  & 
     &   "DIMENSION: METEO ",I8,                                     /  &
     &   "#",                                                        /  &
     &   "#", 5X, "MeteoIdx  UTC Date", 17X, "Temp", 6X,                &
     &        "Pres   Humid", 2X, "ScanIdx  Source    Scan",         /  &
     &   "#", 15X, "YYYY.DD.MM-hh:mm:ss.ff     K", 8X, "Pa     %",   /  &
     &   "#" )
!
         DO 451 J1 = 1, ANC%NUM_MET
!
! --------- Convert the date from MJD, TAI to UTC YYYY.DD.MM-hh:mm:ss.ff
!
            CALL ERR_PASS ( IUER, IER )
            CDATE1 = MJDSEC_TO_DATE ( ANC%MJD_MET, ANC%TAI_MET +     &
     &                      ANC%UTC_MTAI + ANC%MET(J1)%TIM, IER  )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6113, IUER, 'ANC_WRITE',               &
     &                       'Error in writing MET date'  )
               RETURN
            END IF
!              
            WRITE(LUN,2003) J1, CDATE1(1:22), ANC%MET(J1)%TEMP,         &
     &                        ANC%MET(J1)%PRES, ANC%MET(J1)%HUMID,      &
     &                        ANC%MET(J1)%IND_SCA,                      &
     &                        (ANC%MET(J1)%SOU_NAM),                &
     &                        (ANC%MET(J1)%SCA_NAM)
!
!METEO:       6  2025.07.22-17:33:47.06  290.85  88900.0   94.2       6  0602+673  203-1733b
 2003 FORMAT("METEO: ", I7, 2X, A22, X, F7.2, X, F8.1, 2X, F5.1, X,     &
     &                  I7, 2X, A8, 2X, A16 )
!
 451     CONTINUE
      END IF
      WRITE (LUN,115)
!
! --- Tp_sensor Block
!
      IF ( ANC%NUM_TPS > 0 ) THEN
!
         WRITE (LUN, 206) ANC%NUM_TPS
!
 206     FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "DIMENSION: TP_SENSOR ",I8,                                 /  &
     &   "#",                                                        /  &
     &   "#", 11X, "Sensor  IF_Freq   LO_Freq",4X,                      &
     &        "Sky_frq_Cen BndWd", 2X, "Pol SensorId IF#",              &
     &        3X, "Sensor   Net   Comments",                       /  &
     &   "#", 13X, "tag     MHz", 7X, "MHz", 9X, "MHz", 8X, "MHz",      &
     &        19X, "SideBand SideBand"       ,                       /  &
     &   "#" )
!
         DO 461 J1 = 1, ANC%NUM_TPS
!
! --------- Write all Sensors, even the null ones
!
            WRITE(LUN,2004) TRIM(ANC%TPS(J1)%TAG),                      &
     &                      ANC%TPS(J1)%IF_FRQ*1.D-6,                   &
     &                      ANC%TPS(J1)%LO_FRQ*1.D-6,                   &
     &                      ANC%TPS(J1)%SKY_FRQ*1.D-6,                  &
     &                      ANC%TPS(J1)%BDW*1.D-6,                      &
     &                      ANC__POL( ANC%TPS(J1)%POL ),                &
     &                      ANC%TPS(J1)%ID,                             &
     &                      ANC%TPS(J1)%IF_IND,                         &
     &                      ANC__SB ( ANC%TPS(J1)%SSB ),                &
     &                      ANC__SB ( ANC%TPS(J1)%NSB )
!
!TP_SENSOR:  TP_001   528.00    2472.40     3016.40   32.00    H     15a0  15      USB     USB
 2004 FORMAT("TP_SENSOR:  ", A6, X, F8.2, X, F10.2, X, F11.2, 2X,       &
     &                     F6.2, 4X, A1, X, A8, 2X, I2, 2X, A7, X, A7 )
!
 461     CONTINUE
      END IF
      WRITE (LUN,115)
!
! --- TSYS Block
!
      IF ( ANC%NUM_TSYS > 0 .AND. ANC%NUM_TPS > 0 ) THEN
!
         WRITE ( LUN, 207 ) ANC%NUM_EPO_TTO, ANC%NUM_TPS, ANC%NUM_TSYS
!
 207     FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "DIMENSION:   TSYS ", I6, X, I4,                            /  &
     &   "NUM_RECORDS: TSYS ", I8,                                   /  &
     &   "#",                                                        /  &
     &   "#     EpocIdx UTC_Time_tag", 12X, "Sensor    Tsys", 4X,       &
     &        "Azimuth   Elevat  Source    Scan", 7X,                   &
     &        "DataScopeFlag",                                       /  &
     &   "#", 13X, "YYYY.DD.MM-hh:mm:ss.ff    tag", 6X, "K", 10X,       &
     &        "deg", 6X, "deg",                                      /  &
     &   "#" )
!     
! --------- Go through each Tsys then each sensor
!
         DO 471 J1=1,ANC%NUM_EPO_TTO
!
! ------------ Each ANC%TTO(J1)%TIM is supposed to be unique at this
!              point. Therefore we can convert each date from MJD, TAI
!              to UTC YYYY.DD.MM-hh:mm:ss.ff
!
            CALL ERR_PASS ( IUER, IER )
            CDATE1 = MJDSEC_TO_DATE ( ANC%MJD_TTO, ANC%TAI_TTO + &
     &               ANC%UTC_MTAI + ANC%TTO(J1)%TIM, IER  )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6114, IUER, 'ANC_WRITE', &
     &                         'Error in writing TTO date'  )
               RETURN
            END IF
!
! ------------ Go through the sensors.
!
            DO 481 J2=1,ANC%NUM_TPS
               WRITE ( LUN,2005) J1, CDATE1(1:22),                      &
     &                           TRIM(ANC%TPS(J2)%TAG),                 &
     &                           ANC%TTO(J1)%TSYS(J2),                  &
     &                           ANC%TTO(J1)%AZ/DEG__TO__RAD,           &
     &                           ANC%TTO(J1)%EL/DEG__TO__RAD,           &
     &                           ANC%TTO(J1)%SOU_NAM,                   &
     &                           ANC%TTO(J1)%SCA_NAM
 2005          FORMAT ( "TSYS: ", I7, 1X, A22, 2X, A6, 2X, F6.1, 1X,    &
     &                  F10.4, 1X, F8.4, 2X, A8, 2X, A16 )
 481        CONTINUE
 471     CONTINUE
      END IF
      WRITE ( LUN, 115 )
!
! ------ TPI Block
!
      IF ( ANC%NUM_TPI > 0 .AND. ANC%NUM_TPS > 0 ) THEN
!
         WRITE (LUN, 208) ANC%NUM_EPO_TPI, ANC%NUM_TPS, ANC%NUM_TPI
!
 208     FORMAT(                                                        &
     &   "#",                                                        /  &
     &   "DIMENSION:   TPI ", I6, X, I4,                             /  &
     &   "NUM_RECORDS: TPI ", I8,                                    /  &
     &   "#",                                                        /  &
     &   "#    Epoch   UTC_Time_tag", 12X, "Sensor", 5X,                &
     &        "TpiOn   TpiOff  TpiZero    Azimuth  Elevat",3X,          &
              "Source    Scan", 7X, "DataScopeFlag",                 /  &
     &   "#     Idx    YYYY.DD.MM-hh:mm:ss.ff    tag", 8X, "K", 8X,     &
     &        "K", 7X, "K", 7X, "deg", 6X, "deg",                    /  &
     &   "#" )
!     
! ------ Go through each TPI then each sensor
!
         DO 491 J1 = 1, ANC%NUM_EPO_TPI
!
! ------------ Each ANC%TPI(J1)%TIM is supposed to be unique at this
!              point. Therefore we can convert each date from MJD, TAI
!              to UTC YYYY.DD.MM-hh:mm:ss.ff
!
            CALL ERR_PASS ( IUER, IER )
            CDATE1 = MJDSEC_TO_DATE ( ANC%MJD_TPI, ANC%TAI_TPI +     &
     &                          ANC%UTC_MTAI + ANC%TPI(J1)%TIM, IER  )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6115, IUER, 'ANC_WRITE',               &
     &              'Error in writing TPI date'  )
               RETURN
            END IF
!
! ------------ Go through the sensors.
!
            DO 501 J2 = 1, ANC%NUM_TPS
!     
! --------------- Write to file, only epochs and sensors for which either
!                 TPION or TPIOFF were recorded, as well as sensors exist
!                 are recorded
!     
               WRITE ( LUN,2006) J1, CDATE1(1:22),                      &
     &                           TRIM(ANC%TPS(J2)%TAG),                 &
     &                           ANC%TPI(J1)%TPION(J2),                 &
     &                           ANC%TPI(J1)%TPIOFF(J2),                &
     &                           ANC%TPI(J1)%TPIZERO(J2),               &
     &                           ANC%TTO(J1)%AZ/DEG__TO__RAD,           &
     &                           ANC%TTO(J1)%EL/DEG__TO__RAD,           &
     &                           ANC%TTO(J1)%SOU_NAM,                   &
     &                           ANC%TTO(J1)%SCA_NAM
!
!TPI:       1 2025.07.22-17:30:00.15  TP_001     52920    51503      -99    -8.5618  14.0691  1803+784  203-1730a  DATA:VALID_ON
 2006          FORMAT("TPI: ", I7, X, A22, 2X, A6,2X, I8, X, I8, X, I8, &
     &                2X, F9.4, X, F8.4, 2X, A8, 2X, A16 )
!
 501        CONTINUE
 491     CONTINUE
      END IF
!
! ------ PC_sensor Block
!
      IF ( ANC%NUM_PCS > 0 ) THEN
!
         WRITE (LUN, 209) ANC%NUM_PCS
!
 209  FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "DIMENSION: PC_SENSOR ",I8,                                 /  &
     &   "#",                                                        /  &
     &   "#", 11X, "Sensor    Sky_frq_Cen   Pol  SensorId    IF# ",     &
     &        "SideBand   Comments",                                 /  &
     &   "#", 13X, "tag", 9X, "MHz",                                 /  &
     &   "#" )
!
         DO 511 J1 = 1, ANC%NUM_PCS
!
! ------------ Write all Sensors, even the null ones
!
            WRITE(LUN,2007) TRIM(ANC%PCS(J1)%TAG),                    &
     &                        ANC%PCS(J1)%SKY_FRQ*1.D-6,                &
     &                        ANC__POL( ANC%PCS(J1)%POL ),              &
     &                        ANC%PCS(J1)%ID,                           &
     &                        ANC%PCS(J1)%IF_IND,                       &
     &                        ANC__SB ( ANC%PCS(J1)%SSB ),              &
     &                        ANC__SB ( ANC%PCS(J1)%NSB )
!
!PC_SENSOR:  PS_001        3495.00    H    0a0000    -999   DUMMY      USB
 2007      FORMAT("PC_SENSOR:  ",A6,2X,F13.2,4X,A1,X,A9,2X,I6,X,A7,X,A7)
!
 511     CONTINUE
      END IF
      WRITE (LUN,115)
!
! ------ PCAL Block
!
      IF ( ANC%NUM_PCAL > 0 .AND. ANC%NUM_PCS > 0 ) THEN
!
         WRITE (LUN, 210) ANC%NUM_EPO_PCAL, ANC%NUM_PCS, ANC%NUM_PCAL
!
 210  FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "DIMENSION:   PCAL ", I6, X, I4,                            /  &
     &   "NUM_RECORDS: PCAL ", I8,                                   /  &
     &   "#",                                                        /  &
     &   "#     EpocIdx UTC_Time_tag", 12X, "Sensor     Ampl",          &
     &        6X, "Phase", 6X, "Source    Scan       DataScopeFlag", /  &
     &   "#", 13X, "YYYY.DD.MM-hh:mm:ss.ff    tag", 12X, "rad [0,2pi)",/&
     &   "#" )
!     
! ------ Go through each PhaseCal then each sensor
!
         DO 521 J1 = 1, ANC%NUM_EPO_PCAL
!
! ------------ Each ANC%PCAL(J1)%TIM is supposed to be unique at this
!              point. Therefore we can convert each date from MJD, TAI
!              to UTC YYYY.DD.MM-hh:mm:ss.ff
!
            CALL ERR_PASS ( IUER, IER )
            CDATE1 = MJDSEC_TO_DATE ( ANC%MJD_PCAL, ANC%TAI_PCAL +   &
     &           ANC%UTC_MTAI + ANC%PCAL(J1)%TIM, IER  )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6116, IUER, 'ANC_WRITE',  &
     &              'Error in writing PCAL date'  )
               RETURN
            END IF
!
! ------------ Go through the sensors.
!
            DO 531 J2 = 1, ANC%NUM_PCS
!
! --------------- Deconstruct the complex number
!
               PHAS = REAL(PHAS_CMPL_R4(ANC%PCAL(J1)%PCAL_CMPL(J2)),8)  &
     &                + 2*PI__NUM
               IF (PHAS > 2*PI__NUM) PHAS = PHAS - 2*PI__NUM 
               AMPL = ABS( ANC%PCAL(J1)%PCAL_CMPL(J2) ) !/ANC__AMP_SCA
!     
! --------------- The empty amplitude and phase where filled with
!                 ANC%PCAL(J2)%PCAL_CMPL = CMPLX(ANC__FILLER_R4, 0.0)
!                 Therefore, when writing we will isolate out any
!                 amplitudes that are (almost) equal to fillers
!
               WRITE ( LUN,2008) J1, CDATE1(1:22),  &
     &                           TRIM(ANC%PCS(J2)%TAG), &
     &                           AMPL, PHAS, &
     &                           (ANC%PCAL(J1)%SOU_NAM), &
     &                           (ANC%PCAL(J1)%SCA_NAM)                     
!
!PCAL:       1 2025.07.22-17:30:00.15  PS_001    37.907     0.49567    1803+784  203-1730a  DATA:VALID_ON
 2008          FORMAT("PCAL: ",I7,X, A22,2X, A6,X, F9.3,2X, F10.5,3X,   &
     &                 A10,X, A16)
!
 531        CONTINUE
 521     CONTINUE
      END IF
      WRITE (LUN, 115)
!
! ------ FMT2GPS_Timer Block
!
      IF ( ANC%NUM_TGPS > 0 ) THEN
! ---------
         WRITE (LUN, 211) ANC%NUM_TGPS
!
 211     FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "DIMENSION: FMT2GPS_TIMER ", I8,                            /  &
     &   "#",                                                        /  &
     &   "#", 17X, "Timer Board",                                    /  &
     &   "#", 19X, "tag",                                            /  &
     &   "#" )
! 
         DO 541 J1 = 1, ANC%NUM_TGPS
!
! ------------ Write all Sensors, even the null ones
!
            WRITE(LUN,1009) TRIM(ANC%TGPS(J1)%TAG),                   &
     &                      ANC%TGPS(J1)%BOARD
!
!FMT2GPS_TIMER:  TMR_001   a
 1009       FORMAT("FMT2GPS_TIMER:  ", A7, 3X, A )
!
 541     CONTINUE
      END IF
      WRITE (LUN,115)
!
! ------ FMTGPS
!
      IF ( ANC%NUM_TGPS > 0 .AND. ANC%NUM_GPS > 0 ) THEN
!
         WRITE (LUN, 212) ANC%NUM_EPO_GPS, ANC%NUM_TGPS, ANC%NUM_GPS
!
 212     FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "DIMENSION:   FMTGPS ", I6, X, I4                           /  &
     &   "NUM_RECORDS: FMTGPS ", I8,                                 /  &
     &   "#",                                                        /  &
     &   "#", 10X, "Epoc", 6X, "UTC_Time_tag", 9X, "Timer",             &
     &        3X, "Formatter minus", 5X, "Formatter minus",             &
     &        4X, "Source", 4X, "Scan",                              /  &
     &   "#", 10X, "Idx", 30X, "tag       GPS time", 12X, "PPS time",/  &
     &   "#", 15X, "YYYY.DD.MM-hh:mm:ss.ff", 17X, "s", 19X, "s",     /  &
     &   "#" )
!     
! ------ Go through each FMT-GPS then each sensor
!
         DO 551 J1 = 1, ANC%NUM_EPO_GPS
!
! --------- Each ANC%PCAL(J1)%TIM is supposed to be unique at this
!           point. Therefore we can convert each date from MJD, TAI
!           to UTC YYYY.DD.MM-hh:mm:ss.ff
!
            CALL ERR_PASS ( IUER, IER )
            CDATE1 = MJDSEC_TO_DATE ( ANC%MJD_GPS, ANC%TAI_GPS +     &
     &                      ANC%UTC_MTAI + ANC%GPS(J1)%TIM, IER  )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6117, IUER, 'ANC_WRITE',               &
     &                         'Error in writing GPS date'  )
               RETURN
            END IF
!
! ------------ Go through the sensors.
!
            DO 561 J2 = 1, ANC%NUM_TGPS
!     
! --------------- Write to file, only epochs for which (FMT-GPS) and
!                 (FMT-PPS) are fillers
!
               WRITE ( LUN,2010) J1, CDATE1(1:22),                      &
     &                           TRIM(ANC%TGPS(J2)%TAG),                &
     &                           ANC%GPS(J1)%FMG(J2),                   &
     &                           ANC%GPS(J1)%FMP(J2),                   &
     &                           ANC%GPS(J1)%SOU_NAM,                   &
     &                           ANC%GPS(J1)%SCA_NAM
!
!FMTGPS:       1 2025.08.15-09:00:01.15  TMR_001  -6.951953401e-05    -1.953124951e-08    AZEL      r1_f01_0001
 2010          FORMAT("FMTGPS: ", I7, X, A22, 2X, A7, X, 1PE17.9, 3X,   &
     &                   1PE17.9, 4X, A8, 2X, A16 )
!
 561        CONTINUE
 551     CONTINUE
      END IF
      WRITE (LUN, 115)
!
! ------ SEFD Block
!
      IF ( ANC%NUM_SEFD > 0 .AND. ANC%NUM_TPS > 0 ) THEN
!
         WRITE (LUN, 213) ANC%NUM_EPO_SEFD, ANC%NUM_TPS, ANC%NUM_SEFD
!
 213     FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "DIMENSION:   SEFD ", I6, X, I4,                            /  &
     &   "NUM_RECORDS: SEFD ", I8,                                   /  &
     &   "#",                                                        /  &
     &   "#     EpocIdx", 7X, "UTC_Time_tag", 7X, "Sensor      SEFD",   &
     &        8X, "Tsys", 7X, "Tcal", 7X, "Trat", 7X, "Gain      Az",   &
     &        8X,"El      Source",                                   /  &
     &   "#", 14X, "YYYY.DD.MM-hh:mm:ss.ff     tag", 7X, "Jy",          &
     &        10X, "K", 9X, "Jy", 17X,"Compress     deg", 7X, "deg", /  &
     &   "#" )
!     
! --------- Go through each SEFD then each sensor
!
         DO 571 J1 = 1, ANC%NUM_EPO_SEFD
!
! --------- Each ANC%SEFD(J1)%TIM is supposed to be unique at this
!           point. Therefore we can convert each date from MJD, TAI
!           to UTC YYYY.DD.MM-hh:mm:ss.ff
!
            CALL ERR_PASS ( IUER, IER )
            CDATE1 = MJDSEC_TO_DATE ( ANC%MJD_SEFD, ANC%TAI_SEFD +   &
     &                           ANC%UTC_MTAI + ANC%SEFD(J1)%TIM, IER  )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6118, IUER, 'ANC_WRITE',               &
     &                         'Error in writing SEFD date'  )
               RETURN
            END IF
!
! ------------ Go through the sensors.
!
            DO 581 J2 = 1, ANC%NUM_TPS
!     
! --------------- Write to file, only for sensors that have defined
!                 frequencies
!
               IF ( ANC%TPS(J2)%SKY_FRQ > 0 ) THEN
                  WRITE ( LUN, 2011 ) J1, CDATE1(1:22),                 &
     &                                TRIM(ANC%TPS(J2)%TAG),            &
     &                                ANC%SEFD(J1)%SEFD(J2),            &
     &                                ANC%SEFD(J1)%TSYS(J2),            &
     &                                ANC%SEFD(J1)%TCAL(J2),            &
     &                                ANC%SEFD(J1)%TRAT(J2),            &
     &                                ANC%SEFD(J1)%GAIN(J2),            &
     &                                ANC%SEFD(J1)%AZ/DEG__TO__RAD,     &
     &                                ANC%SEFD(J1)%EL/DEG__TO__RAD,     &
     &                                 (ANC%SEFD(J1)%SOU_NAM)
!
!SEFD:       1  2025.07.22-16:16:15.20  TP_001    3978.300     82.310    120.837      1.210      0.993    78.0000   35.5000  taurusa
 2011             FORMAT( "SEFD: ", I7, 2X, A22, 2X, A6, 2X, F10.3, 2X, &
     &                    F9.3, 2X, F9.3, 2X, F9.3, 2X, F9.3, 2X, F9.4, &
     &                    1X, F9.4, 2X, A8 )
               END IF 
 581        CONTINUE
 571     CONTINUE
      END IF
!     
! --- Opacity 
!
      IF ( L_PRV > 2 .AND. ANC%NUM_OPA > 0) THEN
!
! ------ We expect the number of opacity to be equal to the number of 
!
         WRITE (LUN, 216) ANC%NUM_EPO_TTO, ANC%NUM_TPS, ANC%NUM_OPA
!
 216     FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "DIMENSION:   OPACITY ", I6, X, I4,                         /  &
     &   "NUM_RECORDS: OPACITY ", I8,                                /  &
     &   "#",                                                        /  &
     &   "#      EpocIdx  UTC_Time_tag", 12X, "Sensor  Opacity", 4X,    &
     &        "Azimuth   Elevat  Sou_name  Scan_name",              /  &
     &   "#", 15X, "YYYY.DD.MM-hh:mm:ss.ff     tag", 13X,               &
     &        "deg", 6X, "deg",                                      /  &
     &   "#" )
!
! ------ Go through each Tsys then each sensor
!
         DO 591 J1 = 1, ANC%NUM_EPO_TTO
!
! --------- Each ANC%TTO(J1)%TIM is supposed to be unique at this
!           point. Therefore we can convert each date from MJD, TAI
!           to UTC YYYY.DD.MM-hh:mm:ss.ff
!
            CALL ERR_PASS ( IUER, IER )
            CDATE1 = MJDSEC_TO_DATE ( ANC%MJD_TTO, ANC%TAI_TTO +     &
     &                      ANC%UTC_MTAI + ANC%TTO(J1)%TIM, IER  )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6119, IUER, 'ANC_WRITE',               &
     &                         'Error in writing TTO date for opacity'  )
               RETURN
            END IF
!
! ------------ Go through the sensors.
!
            DO 601 J2 = 1, ANC%NUM_TPS
!
! --------------- Write to file, only epochs and sensors for which Tsys
!                 values were recorded, as well as sensors exist are
!                 recorded
!
               WRITE ( LUN, 1012 ) J1, CDATE1(1:22), &
     &                                TRIM(ANC%TPS(J2)%TAG), &
     &                                ANC%TTO(J1)%OPA(J2), &
     &                                ANC%TTO(J1)%AZ/DEG__TO__RAD, &
     &                                ANC%TTO(J1)%EL/DEG__TO__RAD, &
     &                                ANC%TTO(J1)%SOU_NAM, &
     &                                ANC%TTO(J1)%SCA_NAM
!
!OPACITY:     1  2020.07.13-11:30:00.17  TP_001  0.0009  -123.5678 82.4567  0300+470  no00001
 1012          FORMAT("OPACITY: ", I6, X, A22, 2X, A6, X, F7.4, 2X,     &
     &                 F9.4, X, F7.4, X, A9, 2X, A16 )
!
 601        CONTINUE
 591     CONTINUE
      END IF
      WRITE (LUN, 115)
!
! ------ Total Atmospheric Brightness Temperature
!
      IF ( L_PRV > 2 .AND. ANC%NUM_TATM > 0) THEN
!
! --------- We expect the number of opacity to be equal to the number of 
!
         WRITE (LUN, 217) ANC%NUM_EPO_TTO, ANC%NUM_TPS, ANC%NUM_OPA
!
 217     FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "DIMENSION:   TATM ", I6, X, I4,                            /  &
     &   "NUM_RECORDS: TATM ", I8,                                   /  &
     &   "#",                                                        /  &
     &   "#  EpochIdx    UTC_Time_tag", 12X, "Sensor    Tatm   ", 4X,   &
     &        "Azimuth   Elevat   Sou_name  Scan",                   /  &
     &   "#", 13X, "YYYY.DD.MM-hh:mm:ss.ff     tag", 5X, "K", 6X,       &
     &        "deg", 5X, "deg",                                      /  &
     &     "#" )
!     
! --------- Go through each Tsys then each sensor
!
         DO 611 J1 = 1, ANC%NUM_EPO_TTO
!
! ------------ Each ANC%TTO(J1)%TIM is supposed to be unique at this
!              point. Therefore we can convert each date from MJD, TAI
!              to UTC YYYY.DD.MM-hh:mm:ss.ff
!
            CALL ERR_PASS ( IUER, IER )
            CDATE1 = MJDSEC_TO_DATE ( ANC%MJD_TTO, ANC%TAI_TTO +     &
     &                      ANC%UTC_MTAI + ANC%TTO(J1)%TIM, IER  )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6120, IUER, 'ANC_WRITE',               &
     &                         'Error in writing TTO date for TATM'  )
               RETURN
            END IF
!
! ------------ Go through the sensors.
!
            DO 621 J2 = 1, ANC%NUM_TPS
!     
! --------------- Write to file, only epochs and sensors for which Tsys
!                 values were recorded, as well as sensors exist are
!                 recorded
!
               WRITE ( LUN,1013) J1, CDATE1(1:22),                      &
     &                           TRIM(ANC%TPS(J2)%TAG),                 &
     &                           ANC%TTO(J1)%TATM(J2),                  &
     &                           ANC%TTO(J1)%AZ/DEG__TO__RAD,           &
     &                           ANC%TTO(J1)%EL/DEG__TO__RAD,           &
     &                           ANC%TTO(J1)%SOU_NAM,                   &
     &                           ANC%TTO(J1)%SCA_NAM
!
!TATM:     1   2020.07.13-11:30:00.17   TP_001   1.6   -123.5678 82.4567  0300+470  no00001
 1013          FORMAT("TATM: ", I7, X, A22, 3X, A6, X, F6.1, 2X, F9.4,  &
     &                 X, F7.4, X, A9, 2X, A16 )
!               
 621        CONTINUE
 611     CONTINUE
      END IF
      WRITE (LUN, 115)
      CLOSE ( UNIT = LUN )
!
!-----------------------------------------------------------------------------
! -------  HEADERS
!-----------------------------------------------------------------------------
! --- N.B: - For '# ANTCAL Format  Version  0.96 of 2021.05.17' == v1
!          - For '# ANTCAL Format  Version  1.0 of 2025.08.14'  == v2
!
! --- UTC_MTAI header (v1 == v2)
!
!
! --- Filler values Header
!
 102  FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "# Filler values",                                          /  &
     &   "#", 11X, "Real  Integer  Character",                       /  &
     &   "#",                                                        /  &
     &   "FILLERS:   ", F5.1, 2X, I5, 4X, A6                         /  &
     &   "#"  )
!
! --- Provenance section
!
!
! --- Data Header
!
 104  FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "NUM_DATA_ON: ",I8,                                         /  &
     &   "#",                                                        /  &
     &   "#", 7X, "ScanIdx Data_on UTC start", 7X, "Data_on UTC end",   &
     &        9X, "Source    ScanName",                              /  &
     &   "#", 15X, "YYYY.DD.MM-hh:mm:ss.ff  YYYY.DD.MM-hh:mm:ss.ff", /  &
     &   "#" )
!
! --- Meteo Header
!
 105  FORMAT(                                                           &
     &   "#",                                                        /  & 
     &   "NUM_METEO: ",I8,                                           /  &
     &   "#",                                                        /  &
     &   "#", 5X, "MeteoIdx  UTC Date", 17X, "Temp", 6X,                &
     &        "Pres   Humid", 2X, "ScanIdx  Source    Scan",         /  &
     &   "#", 15X, "YYYY.DD.MM-hh:mm:ss.ff     K", 8X, "Pa     %",   /  &
     &   "#" )


!     
! --- TPSensor Tag Header (Sergei needs to correct v2 to match v1)
!
 106  FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "NUM_TP_SENSOR: ",I8,                                       /  &
     &   "#",                                                        /  &
     &   "#", 11X, "Sensor  IF_Freq   LO_Freq",4X,                      &
     &        "Sky_frq_Cen BndWd", 2X, "Pol SensorId IF#",              &
     &        3X, "Sensor     Net   Comments",                       /  &
     &   "#", 13X, "tag     MHz", 7X, "MHz", 9X, "MHz", 8X, "MHz",      &
     &        19X, "SideBand SideBand",                              /  &
     &   "#" )
!
! --- Tsys block
!
 107  FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "NUM_TSYS: ",I8,                                            /  &
     &   "#",                                                        /  &
     &   "#     EpocIdx UTC_Time_tag", 12X, "Sensor    Tsys", 4X,       &
     &        "Azimuth   Elevat  Source    Scan", 7X,                   &
     &        "DataScopeFlag",                                       /  &
     &   "#", 13X, "YYYY.DD.MM-hh:mm:ss.ff    tag", 6X, "K", 10X,       &
     &        "deg", 6X, "deg",                                      /  &
     &   "#" )
!
! --- TPI header (Sergei needs to include info for TPIZero on v2)
!
 108  FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "NUM_TPI: ",I8,                                             /  &
     &   "#",                                                        /  &
     &   "#    EpocIdx UTC_Time_tag", 12X, "Sensor", 5X,                &
     &        "TpiOn   TpiOff    Azimuth  Elevat   Source",             &
     &        4X, "Scan", 7X, "DataScopeFlag",                       /  &
     &   "#", 12X, "YYYY.DD.MM-hh:mm:ss.ff    tag", 8X, "K", 8X,        &
     &        "K", 6X, "deg", 6X, "deg",                             /  &
     &   "#" )
!
! --- PCSensor Tag Header
!
 109  FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "NUM_PC_SENSOR: ", I8,                                      /  &
     &   "#",                                                        /  &
     &   "#", 11X, "Sensor    Sky_frq_Cen   Pol  SensorId    IF# ",     &
     &        "SideBand   Comments",                                 /  &
     &   "#", 13X, "tag", 9X, "MHz",                                 /  &
     &   "#" )
!     
! --- PCAL Header
!
 110  FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "NUM_PCAL: ", I8,                                           /  &
     &   "#",                                                        /  &
     &   "#     EpocIdx UTC_Time_tag", 12X, "Sensor     Ampl",          &
     &        6X, "Phase      Source    Scan       DataScopeFlag",   /  &
     &   "#", 13X, "YYYY.DD.MM-hh:mm:ss.ff    tag", 12X, "rad [0,2pi)",/&
     &   "#" )
!
! --- Fmt2gps timer tags Header
!
 111  FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "NUM_FMT2GPS_TIMER: ", I8,                                  /  &
     &   "#",                                                        /  &
     &   "#", 17X, "Timer Board",                                    /  &
     &   "#", 19X, "tag",                                            /  &
     &   "#" )
!     
! --- Fmt2gps header
!
 112  FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "NUM_FMTGPS: ", I8,                                         /  &
     &   "#",                                                        /  &
     &   "#", 10X, "Epoc", 6X, "UTC_Time_tag", 9X, "Timer",             &
     &        3X, "Formatter minus", 5X, "Formatter minus",             &
     &        4X, "Source", 4X, "Scan",                              /  &
     &   "#", 10X, "Idx", 30X, "tag       GPS time", 12X, "PPS time",/  &
     &   "#", 15X, "YYYY.DD.MM-hh:mm:ss.ff", 17X, "s", 19X, "s",     /  &
     &   "#" )
!      
! --- SEFD Header
!     
 113  FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "NUM_SEFD: ", I8,                                           /  &
     &   "#",                                                        /  &
     &   "#     Sensor", 7X, "UTC_Time_tag", 10X, "SEFD",               &
     &        9X, "Tsys      Tcal", 8X, "Trat      Gain", 7X, "Az",     &
     &        8X, "El    Source",                                    /  &
     &   "#", 7X, "tag   YYYY.DD.MM-hh:mm:ss.ff      Jy", 11X, "K",     &
     &        9X, "Jy", 17X, "Compress     deg", 7X, "deg",          /  &
     &   "#" )
!
! --- Cable Header
!
 114  FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "NUM_CABLE: ", I8,                                          /  &
     &   "#",                                                        /  &
     &   "#     ScanInd  UTC Date",17X, "Cable delay   Source",         &
     &      4X,"ScanName",                                           /  &
     &   "#", 14X, "YYYY.DD.MM-hh:mm:ss.ff   s",                     /  &
     &   "#" )
!
! --- Empty line
!
 115  FORMAT("#")
!
! --- Opacity block
!
 116  FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "NUM_OPACITY: ",I8,                                         /  &
     &   "#",                                                        /  &
     &   "#     EpochIdx  UTC_Time_tag", 11X, "Sensor   Opacity ", 4X,  &
     &        "Azimuth   Elevat   Sou_name  Scan_name",              /  &
     &   "#", 15X, "YYYY.DD.MM-hh:mm:ss.ff     tag", 13X,               &
     &        "deg", 6X, "deg",                                      /  &
     &   "#" )
!
! ---Atmospheric Brightness Temperature Block
!
 117  FORMAT(                                                           &
     &   "#",                                                        /  &
     &   "NUM_TATM: ",I8,                                            /  &
     &   "#",                                                        /  &
     &   "#  EpochIdx    UTC_Time_tag", 12X, "Sensor    Tatm   ", 4X,   &
     &        "Azimuth   Elevat   Sou_name  Scan",                   /  &
     &   "#", 13X, "YYYY.DD.MM-hh:mm:ss.ff     tag", 5X, "K", 6X,       &
     &        "deg", 5X, "deg",                                      /  &
     &   "#" )
!
!-----------------------------------------------------------------------------
!--------------- BODY
!-----------------------------------------------------------------------------
!
!
!
!METEO:      11  2025.07.17-17:50:43.48  289.85  89020.0   90.0       9  0955+476  198-1750
 1003 FORMAT("METEO: ", I7, X, A23, X, F7.2, X, F8.1, X, F6.1, X,       &
     &                  I7, 2X, A8, 2X, A16 )
!
!TP_SENSOR:  TS_001   712.75    1500.00     2216.75    8.00    R       9u   9      USB     USB   002216.8-RCP-b-USB-USB-____9u-001500.0-000712.8:008.0
 1004 FORMAT("TP_SENSOR:  ", A6, 2X, F7.2, 2X, F9.2, 2X, F10.2, 2X,     &
     &                     F6.2, 2X, A3, 2X, A7, 2X, I2, 2X, A10, X, A7)
!
!      
!TSYS:       1 2025.07.17-17:29:56.17  TS_004    99.0   -99.9000 -99.9000  0642+449  198-1730   DATA:ON_SOURCE
 1005 FORMAT("TSYS: ", I7, X, A22, 2X, A6, 2X, F6.1, X, F10.4, 2X,      &
     &                 F7.4, X, A9, 2X, A16 )

!TSYS:       1 2025.08.15-09:00:01.15  TP_001    42.1   134.9966  54.9317  AZEL      r1_f01_0001  DATA:ON_SOURCE
!
!TPI:       1 2025.07.22-17:30:00.15  TS_001     52920    51503    -8.5618  14.0691  1803+784  203-1730a  DATA:VALID_ON
 1006 FORMAT("TPI: ", I7, X, A23, X, A7, X, I8, X, I8, 2X, F9.4, X,     &
     &                F8.4, 2X, A8, 2X, A16 )
!
!     
!PC_SENSOR:  PC_002        3490.00    H    0a0005    -999     USB   002472.4-0-a0-USB-__0a0005-01.4:05.0
 1007 FORMAT("PC_SENSOR:  ",A6, 2X, F13.2, 2X, A3, 2X, A9, 2X, I5, X,A8)
!
!      
!PCAL:       1 2025.07.22-17:30:00.15  PC_004    43.773     4.24290    1803+784  203-1730a  DATA:VALID_ON
 1008 FORMAT("PCAL: ",I7,X, A22,X, A7,2X, F8.3,2X, F10.5,3X, A10,X, A16)

!     
!
!FMTGPS:       1 2025.07.22-17:30:00.15  TMR_004   2.319921805e-05    -1.171874953e-08    1803+784  203-1730a
 1010 FORMAT("FMTGPS: ", I7, X, A22, 2X, A7, 2X, 1PE16.9, 3X, 1PE17.9,  &
     &                   3X, A9, 2X, A16 )
!
!
!SEFD: TS_003  2025.07.22-16:16:15.20    3821.400     81.600    117.075      1.170      1.002    78.0000   35.5000  taurusa
 1011 FORMAT("SEFD: ", A6, 2X, A22, 2X, F10.3, 2X, F9.3, 2X, F9.3, 2X,  &
     &     F9.3, 2X, F9.3, 2X, F9.4, 2X, F8.4, 2X, A9 )
!
!
!OPACITY:     1  2020.07.13-11:31:00.17  TP_002  0.0018  -123.4321 82.8901  0300+470  no00002     
!     
!
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END SUBROUTINE ANC_WRITE  !#!#
