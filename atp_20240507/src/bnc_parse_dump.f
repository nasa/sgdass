      PROGRAM BNC_PARSE_DUMP
!
! *********************************************************************************
! *                                                                               *
! *   Program BNC_PARSE_DUMP                                                      *
! *                                                                               *
! *  ### 16-AUG-2022     BNC_PLOT         v1.1 (c)    N. Habana  24-AUG-2022 ###  *
! *                                                                               *
! *********************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE   'atp.i'
      INCLUDE   'ners.i'
!@IN_ATP@!      INCLUDE   'astro_constants.i'
      TYPE     ( ANC__TYP   ) :: ANC      
      TYPE     ( NERS__TYPE ) :: NERS
      CHARACTER  FILIN*128, FILOUT*128, NERS_CONFIG*128, FIL_AVE*128
      CHARACTER  STR_TSYS_DATE*24,  STR_PCAL_DATE*24, CHA*4, POL_ID
      CHARACTER  STR_GPS_DATE*24,  STR_SEFD_DATE*24
      REAL*8     TATM, T1(ANC__MEPC), X1(ANC__MEPC)
      LOGICAL*1  LEX, FL_ANC
      INTEGER*4  IUER, LN
      INTEGER*4  J1, J2
      REAL*8     PHAS, AMPL
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, TIM_TO_DATE*23
      INTEGER*4, EXTERNAL :: I_LEN
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4

!     
      FILOUT = '/tmp/bnc_parse_dump.out'
      OPEN (UNIT=12, FILE = FILOUT, STATUS = 'UNKNOWN' )
!
! --- Read User Input
!
      IF ( IARGC() .NE. 1 ) THEN
         WRITE ( 6, '(A)' ) 'USAGE: bnc_parse_dump bnc|anc-file'
         CALL EXIT ( 0 )
      ELSE
         CALL GETARG (1, FILIN )
!
! ------ Does the file exist?
!
         INQUIRE ( FILE=FILIN, EXIST=LEX )
         IF ( .NOT. LEX ) THEN
            CALL ERR_LOG ( 5280, IUER, 'BNC_PARSE_DUMP',                &
     &              'Cannot find input antenna calibratin file '//      &
     &              TRIM(FILIN) )
            RETURN 
         END IF
!
! ------ Check if the file ends with anc or bts or bnc
!  
         LN = I_LEN ( FILIN )
         CHA = FILIN(LN-3:LN)
         IF ( (CHA == '.bnc') .OR. (CHA == '.bts') ) THEN
            IUER = -1
            CALL BNC_PARSE (FILIN, ANC, IUER)
!
            FL_ANC = .FALSE.
         ELSEIF ( CHA == '.anc' ) THEN
!
! --------- Initialization of NERS structures, reading andparsing NERS configuration file
!
            IUER = -1
            CALL NERS_INIT ( NERS_CONFIG, NERS, -1.0D0, -1.0D0, IUER )
            IF ( IUER .NE. 0 ) THEN
               IUER = -1
               CALL ERR_LOG ( 5281, IUER, 'BNC_PARSE_DUMP',             &
     &                 'Error in initializing NERS data structure' )
               CALL EXIT ( 1 )
            END IF
! ---------
            FIL_AVE = 'UNDF'
            IUER = -1
            CALL ANC_PARSE ( FILIN, FIL_AVE, ANC, NERS, IUER )
!
            FL_ANC = .TRUE.
         ELSE
            IUER = -1
            CALL ERR_LOG ( 5281, IUER, 'BNC_PARSE_DUMP',                &
     &              'Expected '//TRIM(FILIN)//' to end in '//           &
     &              '.anc or .bnc or .bts' )
            CALL EXIT(1)
         END IF
      END IF

      WRITE(12,*) "#"                                  ; CALL FLUSH (1)
      WRITE(12,*) "STATION:     ", ANC%STA_NAM         ; CALL FLUSH (1)
      WRITE(12,*) "#"                                  ; CALL FLUSH (1)
      WRITE(12,*) "EXP_CODE:    ", TRIM(ANC%EXP_CODE)  ; CALL FLUSH (1)
      WRITE(12,*) "#"                                  ; CALL FLUSH (1)
      WRITE(12,*) "UTC_MTAI:    ", ANC%UTC_MTAI        ; CALL FLUSH (1)
      WRITE(12,*) "#"                                  ; CALL FLUSH (1)
      WRITE(12,*) "NUM_DATA_ON: ", ANC%NUM_DOO         ; CALL FLUSH (1)
      WRITE(12,*) "Start_DOO:   ", MJDSEC_TO_DATE ( ANC%MJD_DOO,        &
     &                                             ANC%TAI_DOO +        &
     &                                             ANC%UTC_MTAI, IUER )
      CALL FLUSH (1)
      WRITE(12,*) "#"                                  ; CALL FLUSH (1)
      WRITE(12,*) "NUM_METEO:   ", ANC%NUM_MET         ; CALL FLUSH (1)
      WRITE(12,*) "Start_MET:   ", MJDSEC_TO_DATE ( ANC%MJD_MET,        &
     &                                              ANC%TAI_MET +       &
     &                                              ANC%UTC_MTAI, IUER )
      CALL FLUSH (1)
      WRITE(12,*) "#"                                  ; CALL FLUSH (1)
!
! --- TP_SENSOR SECTION
!
      WRITE(12,*) "NUM_TP_SENSOR: ", ANC%NUM_TPS       ; CALL FLUSH (1)
      WRITE(12,*) "#"                                  ; CALL FLUSH (1)
      WRITE(12,*) "#        Sensor   Sky_Frq_Cen  Pol SensorId "
      CALL FLUSH (1)
      WRITE(12,*) "#         idx         MHz "         ; CALL FLUSH (1)
      WRITE(12,*) "#"                                  ; CALL FLUSH (1)
!@#TSYS#@!      DO J1 = 1, ANC%NUM_TPS
!@#TSYS#@!        IF ( ANC%TPS(J1)%POL == ANC__R_POL ) THEN
!@#TSYS#@!            POL_ID = 'R'
!@#TSYS#@!         ELSEIF ( ANC%TPS(J1)%POL == ANC__L_POL ) THEN
!@#TSYS#@!            POL_ID = 'L'
!@#TSYS#@!         ELSEIF ( ANC%TPS(J1)%POL == ANC__H_POL ) THEN
!@#TSYS#@!            POL_ID = 'H'
!@#TSYS#@!         ELSEIF ( ANC%TPS(J1)%POL == ANC__V_POL ) THEN
!@#TSYS#@!            POL_ID = 'V'
!@#TSYS#@!         ELSEIF ( ANC%TPS(J1)%POL == ANC__X_POL ) THEN
!@#TSYS#@!            POL_ID = 'X'
!@#TSYS#@!         ELSEIF ( ANC%TPS(J1)%POL == ANC__Y_POL ) THEN
!@#TSYS#@!            POL_ID = 'Y'
!@#TSYS#@!         ELSE
!@#TSYS#@!            IUER = -1
!@#TSYS#@!            CALL ERR_LOG ( 5282, IUER, 'BNC_PARSE_DUMP',                &
!@#TSYS#@!     &              "!!!!POLARIZATION DECRIPTION ERROR!!!!!!!" )
!@#TSYS#@!         END IF
!@#TSYS#@!         POL_ID = ANC__POL ( ANC%TPS(J1)%POL )
!@#TSYS#@!         WRITE(12,104) J1, ANC%TPS(J1)%SKY_FRQ, POL_ID, ANC%TPS(J1)%ID
!@#TSYS#@!      END DO
      WRITE(12,*) "#"                                  ; CALL FLUSH (1)
!
! --- TSYS SECTION
!
      WRITE(12,*) "NUM_TSYS:    ", ANC%NUM_TSYS        ; CALL FLUSH (1)
      IUER = -1
      WRITE(12,*) "Start_TSYS:  ", MJDSEC_TO_DATE ( ANC%MJD_TSYS,       &
     &                                            ANC%TAI_TSYS +        &
     &                                            ANC%UTC_MTAI, IUER )
      CALL FLUSH (1)
      WRITE(12,*) "#"                                  ; CALL FLUSH (1)
      WRITE(12,*) "#   Sensor TSYS  UTC_Time_Tag               TSYS     Azim       Elev"
      CALL FLUSH (1)
      WRITE(12,*) "#     idx   idx  YYYY.MM.DD-hh:mm:ss.ff       K       Deg        Deg"
      CALL FLUSH (1)
      WRITE(12,*) "#"                                  ; CALL FLUSH (1)

!@#TSYS#@!      DO J1 = 1, ANC%NUM_TPS
!@#TSYS#@!         DO J2 = 1, ANC%NUM_TSYS
!@#TSYS#@!            TATM = ANC%MJD_TSYS*86400.D0 + ANC%TAI_TSYS +               &
!@#TSYS#@!     &             ANC%TSYS(J2)%TIM + ANC%UTC_MTAI -                    &
!@#TSYS#@!     &             J2000__MJD*86400.D0
!@#TSYS#@!            IUER = -1
!@#TSYS#@!            STR_TSYS_DATE = TIM_TO_DATE ( TATM, IUER )
!@#TSYS#@!            WRITE(12,105) J1, J2, STR_TSYS_DATE, ANC%TSYS(J2)%TSYS(J1),  &
!@#TSYS#@!     &                    ANC%TSYS(J2)%AZ/DEG__TO__RAD,                  &
!@#TSYS#@!     &                    ANC%TSYS(J2)%EL/DEG__TO__RAD    
!@#TSYS#@!            CALL FLUSH (1)
!@#TSYS#@!         END DO
!@#TSYS#@!      END DO
!
! --- PCS SECTION
!
      WRITE(12,*) "#"                                  ; CALL FLUSH (1)
      WRITE(12,*) "NUM_PC_SENSOR: ", ANC%NUM_PCS       ; CALL FLUSH (1)
      WRITE(12,*) "#"                                  ; CALL FLUSH (1)
      WRITE(12,*) "#        Sensor   Sky_Frq_Cen  Pol SensorId "
      CALL FLUSH (1)
      WRITE(12,*) "#         idx         MHz "         ; CALL FLUSH (1)
      WRITE(12,*) "#"                                  ; CALL FLUSH (1)
!@#PCAL#@!      DO J1 = 1, ANC%NUM_PCS
!@#PCAL#@!         POL_ID = ANC__POL ( ANC%PCS(J1)%POL )
!@#PCAL#@!         WRITE(12,106) J1, ANC%PCS(J1)%SKY_FRQ, POL_ID, ANC%PCS(J1)%ID
!@#PCAL#@!      END DO
      
!
! --- PCAL SECTION
!
      WRITE(12,*) "#"                                  ; CALL FLUSH (1)
      WRITE(12,*) "NUM_PCAL: ", ANC%NUM_PCAL           ; CALL FLUSH (1)

      IUER = -1
      WRITE(12,*) "Start_PCAL:  ", MJDSEC_TO_DATE ( ANC%MJD_PCAL,       &
     &                                              ANC%TAI_PCAL +        &
     &                                              ANC%UTC_MTAI, IUER )
      CALL FLUSH (1)
      WRITE(12,*) "#"                                  ; CALL FLUSH (1)
      WRITE(12,*) "#   Sensor PCAL  UTC_Time_Tag               Ampl     Phas      "
      CALL FLUSH (1)
      WRITE(12,*) "#     idx   idx  YYYY.MM.DD-hh:mm:ss.ff              Rad[-pi,pi]"
      CALL FLUSH (1)
      WRITE(12,*) "#"                                  ; CALL FLUSH (1)

!@#PCAL#@!      DO J1 = 1, ANC%NUM_PCS
!@#PCAL#@!         DO J2 = 1, ANC%NUM_PCAL
!@#PCAL#@!            TATM = ANC%MJD_PCAL*86400.D0 + ANC%TAI_PCAL +               &
!@#PCAL#@!     &             ANC%PCAL(J2)%TIM + ANC%UTC_MTAI -                    &
!@#PCAL#@!     &             J2000__MJD*86400.D0
!@#PCAL#@!            IUER = -1

!@#PCAL#@!            PHAS = REAL ( PHAS_CMPL_R4( ANC%PCAL(J2)%PCAL_CMPL(J1)), 8 )
!@#PCAL#@!            AMPL = ABS( ANC%PCAL(J2)%PCAL_CMPL(J1) ) 
      
!@#PCAL#@!            STR_PCAL_DATE = TIM_TO_DATE ( TATM, IUER )
!@#PCAL#@!            WRITE(12,107) J1, J2, STR_PCAL_DATE, AMPL, PHAS
!@#PCAL#@!            CALL FLUSH (1)
!@#PCAL#@!         END DO
!@#PCAL#@!      END DO
!
! --- TGPS Section
!
      WRITE(12,*) "#"                                          ; CALL FLUSH (1)
      WRITE(12,*) "NUM_FMT2GPS_TIMER: ", ANC%NUM_TGPS          ; CALL FLUSH (1)
      WRITE(12,*) "#"                                          ; CALL FLUSH (1)
!@#FMTG#@!      WRITE(12,*) "#                Sensor    Timer   Board " ; CALL FLUSH (1)
!@#FMTG#@!      WRITE(12,*) "#                 idx       Tag "           ; CALL FLUSH (1)
!@#FMTG#@!      WRITE(12,*) "#"                                          ; CALL FLUSH (1)

!@#FMTG#@!      DO J1 = 1, ANC%NUM_TGPS
!@#FMTG#@!         WRITE(12,108) J1, ANC%TGPS(J1)%TAG, ANC%TGPS(J1)%BOARD
!@#FMTG#@!      END DO
!
! --- GPS Section
!
      WRITE(12,*) "#"                                  ; CALL FLUSH (1)
      WRITE(12,*) "NUM_FMTGPS: ", ANC%NUM_GPS           ; CALL FLUSH (1)
      WRITE(12,*) "#"                                  ; CALL FLUSH (1)
!@#FMTG#@!      IUER = -1
!@#FMTG#@!      WRITE(12,*) "Start_FMT2GPS: ", MJDSEC_TO_DATE(ANC%MJD_GPS,        &
!@#FMTG#@!     &                                              ANC%TAI_GPS +       &
!@#FMTG#@!     &                                              ANC%UTC_MTAI, IUER )
!@#FMTG#@!      CALL FLUSH (1)
!@#FMTG#@!      WRITE(12,*) "#"                                  ; CALL FLUSH (1)
!@#FMTG#@!      WRITE(12,*) "#   Timer  GPS  UTC_Time_Tag             Board   FMTGPS     FMTPPS      "
!@#FMTG#@!      CALL FLUSH (1)
!@#FMTG#@!      WRITE(12,*) "#    idx   idx  YYYY.MM.DD-hh:mm:ss.ff            s          s "
!@#FMTG#@!      CALL FLUSH (1)
!@#FMTG#@!      WRITE(12,*) "#"                                  ; CALL FLUSH (1)
!@#FMTG#@!
!@#FMTG#@!      DO J1 = 1, ANC%NUM_TGPS
!@#FMTG#@!         DO J2 = 1, ANC%NUM_GPS
!@#FMTG#@!            TATM = ANC%MJD_GPS*86400.D0 + ANC%TAI_GPS +                 &
!@#FMTG#@!     &             ANC%GPS(J2)%TIM + ANC%UTC_MTAI -                     &
!@#FMTG#@!     &             J2000__MJD*86400.D0
!@#FMTG#@!            IUER = -1
!@#FMTG#@!
!@#FMTG#@!!            PRINT *, J2, ANC%GPS(J2)%TIM
!@#FMTG#@!            STR_GPS_DATE = TIM_TO_DATE ( TATM, IUER )
!@#FMTG#@!            WRITE(12,109) J1, J2, STR_GPS_DATE, ANC%TGPS(J1)%BOARD,     &
!@#FMTG#@!     &                    ANC%GPS(J2)%FMG(J1), ANC%GPS(J2)%FMP(J1)            
!@#FMTG#@!            CALL FLUSH (1)
!@#FMTG#@!         END DO
!@#FMTG#@!      END DO
!
! --- SEFD Section
!
      WRITE(12,*) "NUM_SEFD:    ", ANC%NUM_SEFD        ; CALL FLUSH (1)
      IUER = -1
      WRITE(12,*) "Start_SEFD:  ", MJDSEC_TO_DATE ( ANC%MJD_SEFD,       &
     &                                            ANC%TAI_SEFD +        &
     &                                            ANC%UTC_MTAI, IUER )
      CALL FLUSH (1)
      WRITE(12,*) "#"                                  ; CALL FLUSH (1)
      WRITE(12,*) "#   Sensor SEFD  UTC_Time_Tag               SEFD     Tsys  GAIN  Az     El"
      CALL FLUSH (1)
      WRITE(12,*) "#     idx   idx  YYYY.MM.DD-hh:mm:ss.ff      Jy       K          deg    deg "
      CALL FLUSH (1)
      WRITE(12,*) "#"                                  ; CALL FLUSH (1)

      DO J1 = 1, ANC%NUM_TPS
         DO J2 = 1, ANC%NUM_SEFD

            TATM = ANC%MJD_SEFD*86400.D0 + ANC%TAI_SEFD +               &
     &             ANC%SEFD(J2)%TIM + ANC%UTC_MTAI -                    &
     &             J2000__MJD*86400.D0
            IUER = -1
            STR_SEFD_DATE = TIM_TO_DATE ( TATM, IUER )

            IF ( ANC%SEFD(J2)%SEFD(J1) > 0 ) THEN
               WRITE(12,110) J1, J2, STR_SEFD_DATE,             &
     &                       ANC%SEFD(J2)%SEFD(J1),             &
     &                       ANC%SEFD(J2)%TSYS(J1),             &
     &                       ANC%SEFD(J2)%GAIN(J1),             &
     &                       ANC%SEFD(J2)%AZ/DEG__TO__RAD,      &
     &                       ANC%SEFD(J2)%EL/DEG__TO__RAD    
               CALL FLUSH (1)
            END IF
            
         END DO
      END DO
     
    
      WRITE (6,*) 'Output written to: '//TRIM(FILOUT)

!
!@@PLOT@@!      DO J1 = 25, 35
!@@PLOT@@!         DO J2 = 1, ANC%NUM_TPS
!@@PLOT@@!
!@@PLOT@@!            X1(J2) = ANC%TPS(J2)%SKY_FRQ
!@@PLOT@@!            T1(J2) = ANC%TSYS(J1)%TSYS(J2)
!@@PLOT@@!         END DO
!@@PLOT@@!!
!@@PLOT@@!         IUER = -1
!@@PLOT@@!         CALL DIAGI_1 ( ANC%NUM_TPS, X1, T1, IUER )
!@@PLOT@@!!
!@@PLOT@@!      END DO
!
 104  FORMAT("TP_SENSOR:  ", I4, 3X, F8.2, 3X, A1, 3X, A4 )
 105  FORMAT("TSYS: ", I4,1X, I4, 3X, A22, 3X, F7.1, 3X,F10.4, 2X,F8.4 )
 106  FORMAT("PC_SENSOR:  ", I4, 3X, F8.2, 3X, A1, 3X, A6 )
 107  FORMAT("PCAL: ", I4,1X, I4, 3X, A22, 3X, F7.3, 3X,F7.5 )
 108  FORMAT("FMT2GPS_TIMER:  ", I4, 2X, A8, 3X, A1 )
 109  FORMAT("FMTGPS: ", I4,1X, I4, 3X, A22, 2X, A1, 2X, 1P1E18.9, 1X, 1PE18.9)
 110  FORMAT("SEFD: ", I4, 1X, I4, 2X, A22, 2X, F8.3, 2X, F7.3, 2X, F6.3, 2X, F8.4, 2X,  F7.4 )

      END PROGRAM  !#!
