      SUBROUTINE ANC_PARSE_SIM ( FILIN, FILOUT, ANC, NERS, IUER )
!     
! ************************************************************************
! *                                                                      *
! *   Routine ANC_TO_BNC_SIM                                             *
! *                                                                      *
! *   INPUT:                                                             *
! *       FILIN   =  ASCII (original) antcal File      { CHAR }          *
! *                                                                      *
! *       FILOUT  =  ASCII (scan average) antcal File  { CHAR, OPT }     *
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
! *  ### 07-MAY-2021  ANC_PARSE   v3.0 (c)  N. Habana  06-Oct-2022  ###  *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT  NONE
      INCLUDE   'atp.i'
      INCLUDE   'ners.i'
      INCLUDE   'ners_local.i'
      TYPE     ( ANC__TYP   ) :: ANC      
      TYPE     ( NERS__TYPE ) :: NERS
      LOGICAL*1  LEX
      CHARACTER  NERS_CONFIG*128, BUFL*256, BUFL2*128
      CHARACTER  FILIN*128, FILOUT*128, FILBNC*128
      INTEGER*4  IUER, IER
      INTEGER*4  N_SECTS                       ! No. of Sections
      PARAMETER  ( N_SECTS = 10 )
      CHARACTER  SECTS(N_SECTS)*32, SECT_ID*32
      INTEGER*8  K1, K2, K3, K4, K5
      CHARACTER  DELIM*3, STR*128, STR1*16, STR2*16
      INTEGER*4  MJD, MIND, MJD_MIN, ICNT
      REAL*8     TAI, UTC, UTC_MTAI, EPS
      PARAMETER  ( EPS = 1.D-4 )
      PARAMETER  ( MIND = 32 )
      PARAMETER  ( MJD_MIN = 44329 )
      PARAMETER  ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9) ) ! Null, Space, Tab
      INTEGER*4  LIND, IND(2,MIND)
      INTEGER*4  IND_DOO, IND_MET
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7
      INTEGER*4  IND_TGPS, IND_PCS, IND_TPS, UNIX_DATE, IS
      INTEGER*4  K_TPS, K_TSYS, K_PCS, K_PCAL, K_SEFD, K_TGPS, K_GPS
      REAL*8     RAMPL, RPHAS, PCAL_REAL, PCAL_IMG
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF, FILE_INFO
      INTEGER*8, EXTERNAL :: IFIND_PL8, IFIND_SORT_PL8
      INTEGER*8  I8TPS, I8PCS, I8GPS, ANC__MTPS8, ANC__MPCS8
      PARAMETER  ( ANC__MTPS8 =  8*1024 )
      PARAMETER  ( ANC__MPCS8 = 16*1024 )
      INTEGER*8  I8TPS_ARR(ANC__MTPS8), K8_TPS_ARR(ANC__MTPS8), K8_TPS
      INTEGER*8  I8PCS_ARR(ANC__MPCS8), K8_PCS_ARR(ANC__MPCS8), K8_PCS
      INTEGER*8  IND8_TPS, IND8_TMP, I8NUM_TPS, IND8_PCS, I8NUM_PCS
      CHARACTER  TPS_TAG*8, PCS_TAG*8, TAG_CUR*8, TAG_PAS*8
      INTEGER*4  IND_CUR, IND_PAS
      INTEGER*4  IND4_CNT, IND4_CNT2      
      REAL*8     TS, TF, TS1, TF1, TS2, TF2, TS3, TF3, TS4, TF4
!     
! --- Check if the anc file exists
!
      INQUIRE ( FILE=FILIN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN !
         IUER = -1
         CALL ERR_LOG ( 5001, IUER, 'ANC_PARSE_SIM',                    &
     &              'Cannot find file '//TRIM(FILIN) )
         CALL EXIT ( 1 )
!
! --- If it exists, open it
!
      ELSE
         OPEN ( UNIT = 111, FILE = FILIN, STATUS = 'OLD' ) ! Original Anc file
      END IF ! lex
!     
! --- If given, check whether the average file exists
!
! ################################################################ 
! ########## DON'T FORGET TO CHECK IF THE CONTENT IS FROM ########
! ########## THE SAME EXPERIMENT                          ########
! ################################################################ 
      IF ( TRIM(FILOUT) .NE. 'UNDF' ) THEN
         INQUIRE ( FILE=FILOUT, EXIST=LEX )
!
! ------ If it doesn't exist, create a path to write to
!
!@@@!         IF ( .NOT. LEX ) THEN  !
            OPEN ( UNIT = 112, FILE = FILOUT, STATUS = 'UNKNOWN' ) ! the scan average file
!            
! ------ if it does exist, don't overwrite it.
!
!@@!         ELSE
    !@@!        IUER = -1
        !@@!    CALL ERR_LOG ( 5002, IUER, 'ANC_PARSE_SIM',                 &
!@@!     &           TRIM(FILOUT)//'already exists. Check contents' )
    !@@!        CALL EXIT ( 1 )
        !@@@! END IF                 ! lex
      END IF
!
! --- Get NERS_CONFIG file
! --- First, check environment variable NERS_CONFIG
!
      CALL GETENVAR ( 'NERS_CONFIG', NERS_CONFIG )
      IF ( ILEN(NERS_CONFIG) == 0 ) THEN
!
! ------ Second, check $HOME/.ners_config file
!
         CALL GETENVAR ( 'HOME', NERS_CONFIG )
         NERS_CONFIG = TRIM(NERS_CONFIG)//'/.ners_config'
         INQUIRE ( FILE=NERS_CONFIG, EXIST=LEX )
         IF ( .NOT. LEX ) THEN
!
! --------- Third, check for the system-wide ners configuration file 
!
            NERS_CONFIG = NERS__CONFIG
         END IF
      END IF      
!
! --- Innitialization of NERS structures, reading andparsing NERS configuration file
!
      IUER = -1
      CALL NERS_INIT ( NERS_CONFIG, NERS, -1.0D0, -1.0D0, IUER )
      IF ( IUER .NE. 0 ) THEN
         IUER = -1
         CALL ERR_LOG ( 5062, IUER, 'ANC_TO_BNC',                       &
     &           'Error in initializing NERS data structure' )
         CALL EXIT ( 1 )
      END IF
!
! --- Initialize dates and counters
!
      ANC%MJD_DOO  = -1
      ANC%MJD_MET  = -1
      ANC%MJD_TSYS = -1
      ANC%MJD_PCAL = -1
      ANC%MJD_SEFD = -1
      ANC%MJD_GPS  = -1
      ANC%TAI_DOO  =  0.0D0
      ANC%TAI_MET  =  0.0D0
      ANC%TAI_TSYS =  0.0D0
      ANC%TAI_PCAL =  0.0D0
      ANC%TAI_SEFD =  0.0D0
      ANC%TAI_GPS  =  0.0D0
! ---
      K_TPS    =  0
      K_TSYS   =  0
      K_PCS    =  0
      K_PCAL   =  0
      K_SEFD   =  0
      K_TGPS   =  0
      K_GPS    =  0
! ---
      K8_TPS   =  0
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
!     
! --- Section List in the preferred order
! DATA_ON METEO TP_SENSOR TSYS SEFD PC_SENSOR PCAL FMT2GPS_TIMER FMTGP
      SECTS(1)  = 'GLOBAL'
      SECTS(2)  = 'DATA_ON'
      SECTS(3)  = 'METEO'
      SECTS(4)  = 'TP_SENSOR'
      SECTS(5)  = 'TSYS'
      SECTS(6)  = 'PC_SENSOR'
      SECTS(7)  = 'PCAL'
      SECTS(8)  = 'FMT2GPS_TIMER'
      SECTS(9)  = 'FMTGPS'
      SECTS(10) = 'SEFD'
! ---  
      DO 410 K1 = 1, ANC__MFIL
!     
! ------ Read the file line by line
!
         READ ( 111, '(A)' ) BUFL
!
! ------ Store line number to STR
!
         CALL CLRCH (     STR )
         CALL INCH8 ( K1, STR )
!
! ------ Extract words from current line of anc file
!
         CALL EXWORD ( BUFL, MIND, LIND, IND, DELIM, IER ) ! Extract words         
!
! ------ Store the first word in line to BUFL2
!
         BUFL2 = BUFL(IND(1,1):IND(2,1))
!     
! ------ Divide the file into sections
! ------ N.B.: Also initialise number of parameters in each section, 
!              where it is required.
!
         IF ( TRIM(BUFL) == TRIM(ANTCAL__FMT) ) THEN
! ---------
            SECT_ID = SECTS(1)                                          ! Global section
! ------
         ELSEIF ( TRIM(BUFL2) == 'NUM_DATA_ON:' ) THEN         
! ---------
            SECT_ID = SECTS(2)                                          ! Data_On section
! ---------
            READ ( UNIT=BUFL(IND(1,2):IND(2,2)), FMT='(I8)' )           &
     &             ANC%NUM_DOO
!
! --------- Allocate the number to ANC%DOO
!
            ALLOCATE ( ANC%DOO(ANC%NUM_DOO), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5003, IUER, 'ANC_PARSE_SIM',              &
     &                 'Error in allocating memory for ANC%DOO while'// &
     &                 ' processing antenna calibration file'//FILIN )
               RETURN
            END IF
! ------ 
         ELSEIF ( TRIM(BUFL2) == 'NUM_METEO:' ) THEN
! ---------
            SECT_ID = SECTS(3)                                          ! Meteo section
! ---------
            READ ( UNIT=BUFL(IND(1,2):IND(2,2)), FMT='(I8)' )           &
     &             ANC%NUM_MET
!
! --------- Allocate the number to ANC%MET
!
            ALLOCATE ( ANC%MET(ANC%NUM_MET), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5004, IUER, 'ANC_PARSE_SIM',              &
     &                 'Error in allocating memory for ANC%METEO while' &
     &               //' processing antenna calibration file '//FILIN )
               RETURN
            END IF
! ------
         ELSEIF ( TRIM(BUFL2) == 'NUM_TP_SENSOR:' ) THEN
! ---------
            SECT_ID = SECTS(4)                                          ! TP_Sensor Section
! ---------
            READ ( UNIT=BUFL(IND(1,2):IND(2,2)), FMT='(I8)' )           &
     &             ANC%NUM_TPS
!
! --------- Convert NUM_TPS to integer*8
!
            I8NUM_TPS = INT ( ANC%NUM_TPS, 8 )
!
! --------- Allocate the number to TPS
!
             ALLOCATE ( ANC%TPS(ANC%NUM_TPS), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5005, IUER, 'ANC_PARSE_SIM',              &
     &                 'Error in allocating memory for ANC%TPS while'   &
     &               //' processing antenna calibration file '//FILIN )
               RETURN
            END IF
! ------
         ELSEIF ( TRIM(BUFL2) == 'NUM_TSYS:' ) THEN
!     
! --------- Replace the TSYS counter for the scav file
!
            IF ( FILOUT .NE. 'UNDF' ) WRITE (112,'(A)') 'NUM_TSYS: 0'
! ---------
            SECT_ID = SECTS(5)  ! Tsys Section
! ---------
            READ ( UNIT=BUFL(IND(1,2):IND(2,2)), FMT='(I8)' )           &
     &             ANC%NUM_TSYS
!
! --------- Allocate number to ANC%TSYS
!
            ALLOCATE ( ANC%TSYS(ANC%NUM_TSYS), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5006, IUER, 'ANC_PARSE_SIM',              &
     &                 'Error in allocating memory for ANC%TSYS while ' &
     &               //'processing antenna calibration file '//FILIN )
               RETURN
            END IF
!
! --------- Allocate NUM_TPS in each time stamp's TSYS
!
            DO 420 J2 = 1, ANC%NUM_TSYS
               ALLOCATE ( ANC%TSYS(J2)%TSYS(ANC%NUM_TPS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5007, IUER, 'ANC_PARSE_SIM',           &
     &                    'Error in allocating memory for the '//       &
     &                    'ANC%TSYS(j2)%TSYS while processing ' //      &
     &                    'antenna calibration file '//TRIM(FILIN) )
                  RETURN
               END IF
               ANC%TSYS(J2)%TSYS = ANC%FILLER_R8
 420        CONTINUE
!     
! --------- We assume that the TPS block has been read and filled, 
!           and so have the tags been converted to integer*8.
! --------- N.B: just because the tags (in string) are sorted, doesn't
!                mean that the int*8 will also be ordered.
!
!@@SORTI8_TPS@@!            CALL SORT2_I8 ( K8_TPS, I8TPS_ARR, K8_TPS_ARR )
            CALL SORT2_I8 ( I8NUM_TPS, I8TPS_ARR, K8_TPS_ARR )
! ------
         ELSEIF ( TRIM(BUFL2) == 'NUM_PC_SENSOR:' ) THEN
! ---------
            SECT_ID = SECTS(6)                                          ! PC_Sensor Section
! ---------
            READ ( UNIT=BUFL(IND(1,2):IND(2,2)), FMT='(I8)' )           &
     &             ANC%NUM_PCS
!
! --------- Convert NUM_TPS to integer*8
!
            I8NUM_PCS = INT ( ANC%NUM_PCS, 8 )
!
! --------- Allocate the number to PCS
!
            ALLOCATE ( ANC%PCS(ANC%NUM_PCS), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5008, IUER, 'ANC_PARSE_SIM',              &
     &                 'Error in allocating memory for ANC%PCS while '  &
     &               //'processing antenna calibration file '//FILIN )
               RETURN
            END IF
! ------
         ELSEIF ( TRIM(BUFL2) == 'NUM_PCAL:' ) THEN
!     
! --------- Replace the PCAL counter for the scav file
!
            IF ( FILOUT .NE. 'UNDF' ) WRITE (112,'(A)') 'NUM_PCAL: 0'
! ---------
            SECT_ID = SECTS(7)  ! PCAL Section
! ----------
            READ ( UNIT=BUFL(IND(1,2):IND(2,2)), FMT='(I8)' )           &
     &             ANC%NUM_PCAL
!
! --------- Allocate number to ANC%PCAL
!
            ALLOCATE ( ANC%PCAL(ANC%NUM_PCAL), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5009, IUER, 'ANC_PARSE_SIM',              &
     &                 'Error in allocating memory for ANC%PCAL while ' &
     &               //'processing antenna calibration file '//FILIN )
               RETURN
            END IF
!
! --------- Allocate NUM_PCS in each time stamp's PCAL
!
            DO 430 J2 = 1, ANC%NUM_PCAL
               ALLOCATE ( ANC%PCAL(J2)%PCAL_CMPL(ANC%NUM_PCS),          &
     &                    STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5010, IUER, 'ANC_PARSE_SIM',           &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%PCAL(j2)%PCAL_CMPL while processing '//  &
     &                    'antenna calibration file '//FILIN)
                  RETURN
               END IF
               ANC%PCAL(J2)%PCAL_CMPL = CMPLX(ANC__FILLER_R4, 0.0)
 430        CONTINUE
!
! --------- We assume that the PCS block has been read and filled, 
!           and so have the tags been converted to integer*8.
! --------- N.B: just because the tags (in string) are sorted, doesn't
!                mean that the int*8 will also be ordered.
!
       
!@@@SORTI8_PCS@@@!            CALL SORT2_I8 ( K8_PCS, I8PCS_ARR, K8_PCS_ARR )
            CALL SORT2_I8 ( I8NUM_PCS, I8PCS_ARR, K8_PCS_ARR )
            
! ------
         ELSEIF ( TRIM(BUFL2) == 'NUM_FMT2GPS_TIMER:' ) THEN
! ---------
            SECT_ID = SECTS(8)     ! FMT2GPS_Timer Section
! ---------
            READ ( UNIT=BUFL(IND(1,2):IND(2,2)), FMT='(I8)' )           &
     &             ANC%NUM_TGPS
!
! --------- Allocate the number to TGPS
!
            ALLOCATE ( ANC%TGPS(ANC%NUM_TGPS), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5011, IUER, 'ANC_PARSE_SIM',              &
     &                 'Error allocating memory for ANC%TGPS while '//  &
     &                 'processing antenna calibration file '//FILIN )
               RETURN
            END IF
! ------
         ELSEIF ( TRIM(BUFL2) == 'NUM_FMTGPS:' ) THEN
! ---------
            SECT_ID = SECTS(9)  ! FMTGPS Section
! ---------
            READ ( UNIT=BUFL(IND(1,2):IND(2,2)), FMT='(I8)' )           &
     &             ANC%NUM_GPS
!
! --------- Allocate number to ANC%GPS
!
            ALLOCATE ( ANC%GPS(ANC%NUM_GPS), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5012, IUER, 'ANC_PARSE_SIM',              &
     &                 'Error in allocating memory for ANC%GPS while'// &
     &                 ' processing antenna calibration file '//FILIN )
               RETURN
            END IF
!     
! --------- Allocate NUM_TGPS in each time stamp's GPS
!
            DO 450 J2 = 1, ANC%NUM_GPS
! ------------
               ALLOCATE ( ANC%GPS(J2)%FMG(ANC%NUM_TGPS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5013, IUER, 'ANC_PARSE_SIM',           &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%GPS(j2)%FMG while processing antenna '// &
     &                    'calibration file '//FILIN)
                  RETURN
               END IF
               ANC%GPS(J2)%FMG = ANC%FILLER_R8
! ------------
               ALLOCATE ( ANC%GPS(J2)%FMP(ANC%NUM_TGPS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5014, IUER, 'ANC_PARSE_SIM',           &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%GPS(J2)%FMP while processing antenna '// &
     &                    'calibration file '//FILIN)
                  RETURN
               END IF
               ANC%GPS(J2)%FMP = ANC%FILLER_R8
 450        CONTINUE 
! ------            
         ELSEIF ( TRIM(BUFL2) == 'NUM_SEFD:' ) THEN
! ---------
            SECT_ID = SECTS(10) ! SEFD Section
! ---------
            READ ( UNIT=BUFL(IND(1,2):IND(2,2)), FMT='(I8)' )           &
     &             ANC%NUM_SEFD
!
! --------- Allocate number to ANC%SEFD
!
            ALLOCATE ( ANC%SEFD(ANC%NUM_SEFD), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5015, IUER, 'ANC_PARSE_SIM',              &
     &                 'Error in allocating memory for ANC%SEFD while ' &
     &               //'processing antcal file '//TRIM(FILIN) )
               RETURN
            END IF
!
! --------- Allocate NUM_TPS pointers in ANC%SEFD
!           in each time stamp's SEFD, TSYS, TCAL, 
!           TRAT, and GAIN
!
            DO 440 J2=1,ANC%NUM_SEFD
!
! ------------ SEFD Time stamps
!
               ALLOCATE ( ANC%SEFD(J2)%SEFD(ANC%NUM_TPS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5016, IUER, 'ANC_PARSE_SIM',           &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%SEFD(j2)%SEFD while processing '//       &
     &                    'antcal file '//TRIM(FILIN) )
                  RETURN
               END IF
               ANC%SEFD(J2)%SEFD = ANC%FILLER_R8
!
! ------------ TSYS Time stamps
!
               ALLOCATE ( ANC%SEFD(J2)%TSYS(ANC%NUM_TPS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5017, IUER, 'ANC_PARSE_SIM',           &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%SEFD(j2)%TSYS while processing '//       &
     &                    'antcal file '// TRIM(FILIN) )
                  RETURN
               END IF
               ANC%SEFD(J2)%TSYS = ANC%FILLER_R8
!
! ------------ TCAL Time stamps
!
               ALLOCATE ( ANC%SEFD(J2)%TCAL(ANC%NUM_TPS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5018, IUER, 'ANC_PARSE_SIM',           &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%SEFD(j2)%TCAL while processing '//       &
     &                    'antcal file '// TRIM(FILIN) )
                  RETURN
               END IF
               ANC%SEFD(J2)%TCAL = ANC%FILLER_R8
!
! ------------ TRAT Time stamps
!
               ALLOCATE ( ANC%SEFD(J2)%TRAT(ANC%NUM_TPS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5019, IUER, 'ANC_PARSE_SIM',           &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%SEFD(j2)%TRAT while processing '//       &
     &                    'antcal file '//TRIM(FILIN) )
                  RETURN
               END IF
               ANC%SEFD(J2)%TRAT = ANC%FILLER_R8
!
! ------------ GAIN Time stamps
!
               ALLOCATE ( ANC%SEFD(J2)%GAIN(ANC%NUM_TPS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5020, IUER, 'ANC_PARSE_SIM',           &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%SEFD(j2)%GAIN while processing '//       &
     &                    'antcal file '//TRIM(FILIN) )
                  RETURN
               END IF
               ANC%SEFD(J2)%GAIN = ANC%FILLER_R8
 440        CONTINUE
!
! ------ We have reached the end of the file
!
         ELSEIF ( TRIM(BUFL) == '# End of file' ) THEN
! ---------
            IF ( FILOUT .NE. 'UNDF' ) THEN
               WRITE (112, '(A)' ) TRIM(BUFL)
            END IF
! ---------           
            GO TO 300          
         END IF                                 ! assigning sections 
!
! ------ Go through GLOBAL section
!         
         IF ( SECT_ID == SECTS(1) ) THEN
!
! --------- Parse the contents of the section to scan_ave
!
            IF ( FILOUT .NE. 'UNDF' ) THEN
               WRITE (112, '(A)' ) TRIM(BUFL)
            END IF
!
            IF ( BUFL(1:1) == '#' ) GOTO 410                             ! Skip comments
!     
! --------- Assign variables
!
            IF ( BUFL(IND(1,1):IND(2,1)) == 'STATION:' ) THEN
               ANC%STA_NAM  = BUFL(IND(1,2):IND(2,2))                    ! Get station name
! ------------
            ELSEIF ( BUFL(IND(1,1):IND(2,1)) == 'EXP_CODE:' ) THEN
               ANC%EXP_CODE = BUFL(IND(1,2):IND(2,2))                    ! Get the experiment code
! ------------
            ELSEIF ( BUFL(IND(1,1):IND(2,1)) == 'UTC_MTAI:' ) THEN
               READ ( UNIT=BUFL(IND(1,2):IND(2,2)), FMT='(F10.5)' )     &
     &                ANC%UTC_MTAI                                       ! UTC_MTAI
!
! --------- Parse the filler information
!        -99.9 for real, -999 for integers, and n/a for characters
!
            ELSEIF ( BUFL(IND(1,1):IND(2,1)) == 'FILLERS:' ) THEN
!
! ------------ If not enough fillers are described
!
               IF ( LIND < 4 ) THEN
                  CALL ERR_LOG ( 5020, IUER, 'ANC_PARSE_SIM',           &
     &                    'Too few words on FILLERS block line '//      &
     &                    TRIM(STR)//' of the antenna calibration '//   &
     &                    'file '//FILIN )
                  RETURN
               END IF
! ---------
               READ ( UNIT=BUFL(IND(1,2):IND(2,2)), FMT='(F10.5)' )     &
               
     &              ANC%FILLER_R8 ! -99.9
               
               READ ( UNIT=BUFL(IND(1,3):IND(2,3)), FMT='(I8)'    )     &
               
     &              ANC%FILLER_I4 ! -999
               
               ANC%FILLER_CH = BUFL(IND(1,4):IND(2,4)) ! n/a
               
!
! ------ How many PROVENACEs are there?
!
            ELSEIF ( BUFL(IND(1,1):IND(2,1)) == 'NUM_PROVENANCE:' ) THEN
               
               READ ( UNIT=BUFL(IND(1,2):IND(2,2)), FMT='(I8)' )        &
     &                ANC%NUM_PRV
            END IF             
!
! ------ Go through DATA_ON section
! 
         ELSEIF ( SECT_ID == SECTS(2) ) THEN
!
! --------- Parse the contents of the section to scan_ave
!
            IF ( FILOUT .NE. 'UNDF' ) THEN
               WRITE (112, '(A)' ) TRIM(BUFL)
            END IF
! ---------
            IF ( BUFL(1:1) == '#' ) GOTO 410                             ! Skip comments
!
! --------- Parse information from the "DATA_ON" block
!
            IF ( BUFL(IND(1,1):IND(2,1)) == 'DATA_ON:' ) THEN
! ---------
               IF ( LIND < 6 ) THEN ! We expect at least 6 words in line
                  CALL ERR_LOG ( 5021, IUER, 'ANC_PARSE_SIM',           &
     &                    'Too few words on DATA_ON block line '//      &
     &                    TRIM(STR)//' of the antenna calibration '     &
     &                    //' file '//TRIM(FILIN) )
                  RETURN
               END IF
!
! ------------ We reached "DATA_ON" before we actually got a count 
!              on "NUM_DATA_ON"
!
               IF ( .NOT. ASSOCIATED ( ANC%DOO ) ) THEN
                  CALL ERR_LOG ( 5022, IUER, 'ANC_PARSE_SIM',           &   
     &                    'Malformed antenna calibration file '         &
     &                    //TRIM(FILIN)//                               &
     &                    ' -- DATA_ON: preceeds NUM_DATA_ON' )
                  RETURN
               END IF
! ------------
               CALL CHIN ( BUFL(IND(1,2):IND(2,2)), IND_DOO )
! ------------
               CALL ERR_PASS ( IUER, IER )
               CALL DATE_TO_TIME ( BUFL(IND(1,3):IND(2,3)),             &
     &                             MJD, UTC, IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5023, IUER, 'ANC_PARSE_SIM',           &
     &                    'Error in parsing DATA_ON date '//            &
     &                    BUFL(IND(1,3):IND(2,3))//' on line '//        &
     &                    TRIM(STR) )
                  RETURN
               END IF
! ------------ 
               IF ( MJD < MJD_MIN ) GOTO 410 ! Skip if MJD DATA_ON is not defined
!
! ------------ In case we haven't defined MJD_DOO yet
!
               IF ( ANC%MJD_DOO < 0 ) THEN
                  CALL ERR_PASS ( IUER, IER )
                  CALL NERS_GET_UTCMTAI ( NERS,                         &
     &                         (MJD - J2000__MJD)*86400.D0 + UTC,       &
     &                         UTC_MTAI, IER )
                  IF ( IER .NE. 0 ) THEN
                     WRITE ( 6, * ) 'K1= ', K1
                     WRITE ( 6, * ) 'DATE= ', BUFL(IND(1,3):IND(2,3))
                     WRITE ( 6, * ) 'MJD= ', MJD, 'UTC= ', UTC
                     CALL ERR_LOG ( 5024, IUER, 'ANC_PARSE_SIM',        &
     &                       'Error in getting UTC_MTAI on '//          &
     &                       'DATA_ON Block' )
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
! ------------
               CALL ERR_PASS ( IUER, IER )
               CALL DATE_TO_TIME ( BUFL(IND(1,4):IND(2,4)),             &
     &                             MJD, UTC, IER )
               IF ( LIND < 6 ) THEN
                  CALL ERR_LOG ( 5025, IUER, 'ANC_PARSE_SIM',           &
     &                    'Error in parsing DATA_ON date '//            &
     &                    BUFL(IND(1,3):IND(2,3))//' on line '//        &
     &                    TRIM(STR) )
                  RETURN
               END IF
! ------------
               ANC%DOO(IND_DOO)%TIM(2) = (MJD - ANC%MJD_DOO)*86400.D0 + &
     &                                   (TAI - ANC%TAI_DOO)             ! Final Time
               ANC%DOO(IND_DOO)%SOU_NAM = BUFL(IND(1,5):IND(2,5))
               ANC%DOO(IND_DOO)%SCA_NAM = BUFL(IND(1,6):IND(2,6))
            END IF
!
! ------ Go through METEO section
! 
         ELSEIF ( SECT_ID == SECTS(3) ) THEN
!
! --------- Parse the contents of the section to scan_ave
!
            IF ( FILOUT .NE. 'UNDF' ) THEN
               WRITE (112, '(A)' ) TRIM(BUFL)
            END IF
! ---------
            IF ( BUFL(1:1) == '#' ) GOTO 410                             ! Skip comments
! ---------
            IF ( BUFL(IND(1,1):IND(2,1)) == 'METEO:' ) THEN
!
! ------------ We expect at least 9 words per line in this block
!
               IF ( LIND < 9 ) THEN                                     
                  CALL ERR_LOG ( 5026, IUER, 'ANC_PARSE_SIM',           &
     &                    'Too few words on METEO block line '//        &
     &                    TRIM(STR)//' of the antenna calibration file '&
     &                    //FILIN )
                  RETURN
               END IF
!
! ------------ We reached "METEO" before "NUM_METEO"
!
               IF ( .NOT. ASSOCIATED ( ANC%MET ) ) THEN
                  CALL ERR_LOG ( 5027, IUER, 'ANC_PARSE_SIM',           &
     &                    'Malformed antenna calibration file '//       &
     &                    TRIM(FILIN)//' -- METEO: preceeds NUM_METEO' )
                  RETURN
               END IF
! ------------
               CALL CHIN ( BUFL(IND(1,2):IND(2,2)), IND_MET )
!
! ------------ NUM_METEO was not counted correctly
!
               IF ( IND_MET > ANC%NUM_MET ) THEN
                  CALL ERR_LOG ( 5028, IUER, 'ANC_PARSE_SIM',           &
     &                    'Malformed antenna calibration file '//       &
     &                    TRIM(FILIN)//' -- too many METEO lines.' )
                  RETURN
               END IF
! ------------
               CALL ERR_PASS ( IUER, IER )
               CALL DATE_TO_TIME ( BUFL(IND(1,3):IND(2,3)),          &
     &                             MJD, UTC, IER)
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5029, IUER, 'ANC_PARSE_SIM',           &
     &                    'Error in parsing METEO date '//              &
     &                    BUFL(IND(1,3):IND(2,3))//' on line '//     &
     &                    TRIM(STR) )
                  RETURN
               END IF
! ------------
               IF ( ANC%MJD_MET < 0 ) THEN
!%UTC_MTAI%!                  CALL ERR_PASS ( IUER, IER )
!%UTC_MTAI%!                  CALL NERS_GET_UTCMTAI (NERS,                          &
!%UTC_MTAI%!     &                         (MJD - J2000__MJD)*86400.D0 + UTC,       &
!%UTC_MTAI%!     &                         UTC_MTAI, IER )
!%UTC_MTAI%!                  IF ( IER .NE. 0 ) THEN
!%UTC_MTAI%!                     CALL ERR_LOG ( 5030, IUER, 'ANC_PARSE_SIM',        &
!%UTC_MTAI%!     &                       'Error in getting UTC_MTAI on METEO Block')
!%UTC_MTAI%!                     RETURN
!%UTC_MTAI%!                  END IF
!
! --------------- Define MJD_MET and TAI_MET
!
                  ANC%MJD_MET = MJD
                  ANC%TAI_MET = UTC - UTC_MTAI
               END IF
               TAI = UTC - UTC_MTAI
               ANC%MET(IND_MET)%TIM = (MJD - ANC%MJD_MET)*86400.D0 +    &
     &              (TAI - ANC%TAI_MET)
! ------------ 
               READ ( UNIT=BUFL(IND(1,4):IND(2,4)), FMT='(F10.5)' )  &
     &                ANC%MET(IND_MET)%TEMP                              ! Temperature [K]
! ------------ 
               READ ( UNIT=BUFL(IND(1,5):IND(2,5)), FMT='(F10.5)' )  &
     &                ANC%MET(IND_MET)%PRES                              ! Pressure [Pa]
! ------------
               READ ( UNIT=BUFL(IND(1,6):IND(2,6)), FMT='(F10.5)' )  &
     &                 ANC%MET(IND_MET)%HUMID                            ! Humidity [%]
! ------------
               READ ( UNIT=BUFL(IND(1,7):IND(2,7)), FMT='(I8)' )     &
     &                ANC%MET(IND_MET)%IND_SCA                           ! Scan Index {link to }
! ------------
               ANC%MET(IND_MET)%SOU_NAM = BUFL(IND(1,8):IND(2,8))
! ------------
               ANC%MET(IND_MET)%SCA_NAM = BUFL(IND(1,9):IND(2,9))
            END IF
!     
! ------ Go through TP_SENSOR section
! 
         ELSEIF ( SECT_ID == SECTS(4) ) THEN
!     
! --------- Parse the contents of the section to scan_ave
!
            IF ( FILOUT .NE. 'UNDF' ) THEN
               WRITE (112, '(A)' ) TRIM(BUFL)
            END IF
! ---------
            IF ( BUFL(1:1) == '#' ) GOTO 410                             ! Skip comments
!
! --------- Parse information from the TP_SENSOR block
!
            IF ( BUFL(IND(1,1):IND(2,1)) == 'TP_SENSOR:' ) THEN
!
! ------------ No less than 10 words in this block
!
               IF ( LIND < 10 ) THEN
                  CALL ERR_LOG ( 5031, IUER, 'ANC_PARSE_SIM',           &
     &                    'Too few words on TP_SENSOR block line '//    &
     &                    TRIM(STR)//' of the antenna calibration file '&
     &                    //TRIM(FILIN) )
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
                  CALL ERR_LOG ( 5032, IUER, 'ANC_PARSE_SIM',           &
     &                    'Malformed antenna calibration file '//       &
     &                    TRIM(FILIN)//' -- too many TP_SENSOR lines' )
                  RETURN
               END IF
!
! ------------ Sensor Tag
!
               ANC%TPS(K_TPS)%TAG = BUFL(IND(1,2):IND(2,2))
               ANC%TPS_TAG(K_TPS) = BUFL(IND(1,2):IND(2,2))
!
! ------------ Copy the list to an integer8 type for optimised 
!              searching. Also copy the initial index, because we are
!              going to sort later and will need that index.
!
               K8_TPS             = INT ( K_TPS, 8)
               K8_TPS_ARR(K8_TPS) = K8_TPS
               I8TPS              = 0
               CALL MEMCPY ( I8TPS, %REF(ANC%TPS_TAG(K_TPS)), %VAL(8) )
!@@DON'TNEED@!               ANC%I8TPS(K_TPS)   = I8TPS
               I8TPS_ARR(K_TPS)   = I8TPS
! ------------
               READ ( UNIT=BUFL(IND(1,3):IND(2,3)), FMT='(F20.5)' )     &
     &                ANC%TPS(K_TPS)%IF_FRQ                              ! Intermediate Frequency [MHz]
! ------------ 
               READ ( UNIT=BUFL(IND(1,4):IND(2,4)), FMT='(F20.5)' )     &
     &                ANC%TPS(K_TPS)%LO_FRQ                              ! Local Oscilator Frequency [MHz]
! ------------
               READ ( UNIT=BUFL(IND(1,5):IND(2,5)), FMT='(F20.5)' )     &
     &                ANC%TPS(K_TPS)%SKY_FRQ                             ! Sky Frequency [MHz]
! ------------
               READ ( UNIT=BUFL(IND(1,6):IND(2,6)), FMT='(F20.5)' )     &
     &                ANC%TPS(K_TPS)%BDW                                 ! Bandwidth [MHz]
!
! ------------ Polarization
!
               IF ( BUFL(IND(1,7):IND(2,7)) == 'R' ) THEN
                  ANC%TPS(K_TPS)%POL = ANC__R_POL
               ELSEIF ( BUFL(IND(1,7):IND(2,7)) == 'L' ) THEN
                  ANC%TPS(K_TPS)%POL = ANC__L_POL
               ELSEIF ( BUFL(IND(1,7):IND(2,7)) == 'H' ) THEN
                  ANC%TPS(K_TPS)%POL = ANC__H_POL
               ELSEIF ( BUFL(IND(1,7):IND(2,7)) == 'V' ) THEN
                  ANC%TPS(K_TPS)%POL = ANC__V_POL
               ELSEIF ( BUFL(IND(1,7):IND(2,7)) == 'X' ) THEN
                  ANC%TPS(K_TPS)%POL = ANC__X_POL
               ELSEIF ( BUFL(IND(1,7):IND(2,7)) == 'Y' ) THEN
                  ANC%TPS(K_TPS)%POL = ANC__Y_POL
               ELSE
                  CALL ERR_LOG ( 5033, IUER, 'ANC_PARSE_SIM',           &
     &                    BUFL(IND(1,7):IND(2,7))//' in line '//        &
     &                    TRIM(STR)//' is not a supported '//           &
     &                    'polarization code. We expected: '//          &
     &                    ' R, L, H, V, X, or Y. Check the antenna '//  &
     &                    'calibration file '//TRIM(FILIN) )
                  RETURN
               END IF
!
! ------------ TP_SENSOR ID
!
               ANC%TPS(K_TPS)%ID = BUFL(IND(1,8):IND(2,8))
! ------------
               READ ( UNIT = BUFL(IND(1,9):IND(2,9)), FMT='(I8)' )      &
     &                ANC%TPS(K_TPS)%IF_IND                              ! Intermediate Frequency Index
!
! ------------ Subband Code
!
               IF ( BUFL(IND(1,10):IND(2,10)) == 'USB' ) THEN
                  ANC%TPS(K_TPS)%SUB = ANC__USB
! ------------ 
               ELSEIF ( BUFL(IND(1,10):IND(2,10)) == 'LSB' ) THEN
                  ANC%TPS(K_TPS)%SUB = ANC__LSB
! ------------ 
               ELSEIF ( BUFL(IND(1,10):IND(2,10)) == 'n/a' ) THEN
                  ANC%TPS(K_TPS)%SUB = ANC__NA
! ------------
               ELSEIF ( BUFL(IND(1,10):IND(2,10)) == 'DUMMY' ) THEN
                  ANC%TPS(K_TPS)%SUB = ANC__NA
! ------------ 
               ELSE
                  CALL ERR_LOG ( 5034, IUER, 'ANC_PARSE_SIM',           &
     &                    BUFL(IND(1,10):IND(2,10))//' in line '//      &
     &                    TRIM(STR)//' is not a supported subband '//   &
     &                    'code. We expected USB, LSB, n/a, or DUMMY.'//&
     &                    'Check the antenna calibration file '//       &
     &                    TRIM(FILIN) )
                  RETURN
               END IF
            END IF
!     
! ------ Go through TSYS section
! 
         ELSEIF ( SECT_ID == SECTS(5) ) THEN
! ---------
            IF ( FILOUT .NE. 'UNDF' ) THEN
               IF ( BUFL(1:1) == '#' ) WRITE (112, '(A)' ) TRIM(BUFL)
            END IF
!
! --------- Parse information from the TSYS Block
!
            IF ( BUFL(IND(1,1):IND(2,1)) == 'TSYS:' ) THEN
!
! ------------ Expecting no less than 10 words
!
               IF ( LIND < 10 ) THEN
                  CALL ERR_LOG ( 5035, IUER, 'ANC_PARSE_SIM',           &
     &                    'Too few words on TSYS sect line '//TRIM(STR) &
     &                  //' of the antenna calibration file '//FILIN )
                  RETURN
               END IF
!
! ------------ We reached "TSYS" before we actually got a count 
!              on "NUM_TSYS"
!
               IF ( .NOT. ASSOCIATED ( ANC%TSYS ) ) THEN
                  CALL ERR_LOG ( 5060, IUER, 'ANC_PARSE_SIM',           &   
     &                    'Malformed antenna calibration file '         &
     &                    //TRIM(FILIN)//'-- TSYS: preceeds NUM_TSYS' )
                  RETURN
               END IF
! ------------
               CALL ERR_PASS ( IUER, IER )
               CALL DATE_TO_TIME ( BUFL(IND(1,3):IND(2,3)),             &
     &                             MJD, UTC, IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5036, IUER, 'ANC_PARSE_SIM',           &
     &                    'Error in parsing TSYS date '//               &
     &                    BUFL(IND(1,3):IND(2,3))//' on line '//        &
     &                    TRIM(STR) )
                  RETURN
               END IF
! ------------
               IF ( ANC%MJD_TSYS < 0 ) THEN
!%UTC_MTAI%!                  CALL ERR_PASS ( IUER, IER )
!%UTC_MTAI%!                  CALL NERS_GET_UTCMTAI ( NERS,                         &
!%UTC_MTAI%!     &                         (MJD - J2000__MJD)*86400.D0 + UTC,       &
!%UTC_MTAI%!     &                         UTC_MTAI, IER )
!%UTC_MTAI%!! ---------------
!%UTC_MTAI%!                  IF ( IER .NE. 0 ) THEN
!%UTC_MTAI%!                     CALL ERR_LOG ( 5037, IUER, 'ANC_PARSE_SIM',        &
!%UTC_MTAI%!     &                    'Error in getting UTC_MTAI on TSYS Block' )
!%UTC_MTAI%!                     RETURN
!%UTC_MTAI%!                  END IF
!
! --------------- MJD_TSYS and TAI_TSYS
!
                  ANC%MJD_TSYS = MJD
                  ANC%TAI_TSYS = UTC - UTC_MTAI
               END IF
               TAI = UTC - UTC_MTAI
!
! ------------ Update the Tsys counter, given the time diff. error is met
!
               IF ( K_TSYS == 0 ) THEN
                  K_TSYS = 1
               ELSE
                  IF ( DABS( ( MJD - ANC%MJD_TSYS )*86400.D0 +          &
     &                       ( TAI - ANC%TAI_TSYS ) -                   &
     &                       ANC%TSYS(K_TSYS)%TIM              )        &
     &                 > 3*ANC__EPS_TIM ) THEN
                     K_TSYS = K_TSYS + 1
                  END IF
               END IF
!
! ------------ Error in logging NUM_TSYS
!
               IF ( K_TSYS > ANC%NUM_TSYS ) THEN
                  CALL IINCH ( K_TSYS, STR1 )
                  CALL IINCH ( ANC%NUM_TSYS, STR2 )
                  CALL ERR_LOG ( 5038, IUER, 'ANC_PARSE_SIM',           &
     &                    'Malformed antenna calibration file '//       &
     &                    TRIM(FILIN)//' -- too many TSYS lines. Got'// &
     &                    TRIM(STR1)//' ,expected '//TRIM(STR2) )
                  RETURN
               END IF
!
! ------------ Compute time
!
               ANC%TSYS(K_TSYS)%TIM = ( MJD - ANC%MJD_TSYS )*86400.D0 + &
     &                                ( TAI - ANC%TAI_TSYS )
! ------------
               READ ( UNIT=BUFL(IND(1,2):IND(2,2)), FMT='(I8)' )        &
     &                ANC%TSYS(K_TSYS)%IND_SCA
!
! ------------ Is the sensor tag in TSYS block available in list of
!              tags from TPS_SENSOR block
!
!@@TPSLIST@@!               IND_TPS = LTM_DIF ( 0, ANC%NUM_TPS, ANC%TPS_TAG,         &
!@@TPSLIST@@!     &                             BUFL(IND(1,4):IND(2,4))   )
!@@TPSLIST@@!               IF ( IND_TPS < 1 ) THEN
!@@TPSLIST@@!                   CALL ERR_LOG ( 5039, IUER, 'ANC_PARSE_SIM',           &
!@@TPSLIST@@!     &                    'Tag '//BUFL(IND(1,4):IND(2,4))//' not '//    &
!@@TPSLIST@@!     &                    'part of TP_SENSOR tags in '//TRIM(FILIN)//   &
!@@TPSLIST@@!     &                    '.See line '//TRIM(STR)//' on the TSYS Block')
!@@TPSLIST@@!                  RETURN
!@@TPSLIST@@!               END IF
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
! ------------ To minimise the burden of searching, we are going to 
!              check if the current TPS is just one up the previous one
!
               IF ( K_TSYS > 1 ) THEN
!
! --------------- grab the TP_SENSOR TAG on this line, and the one
!                 before.
!
                  TAG_PAS = TPS_TAG
                  TAG_CUR = BUFL(IND(1,4):IND(2,4))
!
! --------------- Is the current tag just one up from the previous?
!
                  IND_PAS = IND_TPS
                  IF ( ANC%TPS_TAG(IND_PAS+1) == TAG_CUR ) THEN
!
! ------------------ If the next tag, is just one up, then we 
!                    don't need to binary search, just update the tag from here.
!     
                     IND_TPS = IND_PAS + 1
! ------------------
                     GO TO 202
                  ELSE
                     GO TO 201
                  END IF
               END IF
! ------------
 201           CONTINUE
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!     
! ------------ Is the sensor tag in the TSYS block available in the tag
!              list. We are using the integer*8 conversion for 
!              efficient searching.
! ------------ First convert the tag in this line to INTEGER*8
!
               CALL CLRCH ( TPS_TAG )
               TPS_TAG = BUFL(IND(1,4):IND(2,4))
               I8TPS = 0
               CALL MEMCPY ( I8TPS, %REF(TPS_TAG), %VAL(8) )
!
! ------------ Second search for that INTEGER*8 on the sorted array list
!
               IND8_TMP = IFIND_SORT_PL8 ( I8NUM_TPS, I8TPS_ARR, I8TPS )
               IF ( IND8_TMP < 1 ) THEN
                  CALL ERR_LOG ( 5039, IUER, 'ANC_PARSE_SIM',           &
     &                    'Tag '//BUFL(IND(1,4):IND(2,4))//' not '//    &
     &                    'part of TP_SENSOR tags in '//TRIM(FILIN)//   &
     &                    '.See line '//TRIM(STR)//' on the TSYS Block')
                  RETURN
               ELSE
!
! --------------- Finally get the index on the tag name list
!
                  IND8_TPS = K8_TPS_ARR(IND8_TMP)
                  IND_TPS  = INT ( IND8_TPS, 4 )
               END IF
! ------------
 202           CONTINUE                 ! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!
! ------------ Parse the Tsys value to the index of TSYS in ANC%TSYS 
!              that coincides with the index of the sensor tag in 
!              TP_SENSOR Block. Units = K
!
               READ ( UNIT=BUFL(IND(1,5):IND(2,5)), FMT='(F20.12)' )    &
     &                ANC%TSYS(K_TSYS)%TSYS(IND_TPS)
! ------------
               READ ( UNIT=BUFL(IND(1,6):IND(2,6)), FMT='(F20.12)' )    &
     &                ANC%TSYS(K_TSYS)%AZ                           
               ANC%TSYS(K_TSYS)%AZ = ANC%TSYS(K_TSYS)%AZ*DEG__TO__RAD    ! Azimuth [rad]
! ------------
               READ ( UNIT=BUFL(IND(1,7):IND(2,7)), FMT='(F20.12)' )    &
     &                ANC%TSYS(K_TSYS)%EL                            
               ANC%TSYS(K_TSYS)%EL = ANC%TSYS(K_TSYS)%EL*DEG__TO__RAD    ! Elevation [rad]
! ------------
               ANC%TSYS(K_TSYS)%SOU_NAM = BUFL(IND(1,8):IND(2,8))        ! Source Name 
! ------------
               ANC%TSYS(K_TSYS)%SCA_NAM = BUFL(IND(1,9):IND(2,9))        ! Scan name
            END IF
!     
! ------ Go through PC_SENSOR section
! 
         ELSEIF ( SECT_ID == SECTS(6) ) THEN
!
! --------- Parse the contents of the section to scan_ave
!
            IF ( FILOUT .NE. 'UNDF' ) THEN
               WRITE (112, '(A)' ) TRIM(BUFL)
            END IF
! ---------
            IF ( BUFL(1:1) == '#' ) GOTO 410                             ! Skip comments
!
! --------- Parse information from the PC_SENSOR block
!
            IF ( BUFL(IND(1,1):IND(2,1)) == 'PC_SENSOR:' ) THEN
!
! ------------ No less than 7 words in this block
!
               IF ( LIND < 7 ) THEN
                  CALL ERR_LOG ( 5040, IUER, 'ANC_PARSE_SIM',           &
     &                    'Too few words on PC_SENSOR line block '//    &
     &                    TRIM(STR)//' of antenna calibration file '//  &
     &                    TRIM(FILIN) )
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
                  CALL ERR_LOG ( 5041, IUER, 'ANC_PARSE_SIM',           &
     &                    'Malformed antenna calibration file '//       &
     &                    TRIM(FILIN)//' -- too many PC_SENSORS lines.' &
     &                    //'Got '//TRIM(STR1)//' ,expected '//         &
     &                    TRIM(STR2) )
                  RETURN
               END IF
!
! ------------ Sensor Tag
!
               ANC%PCS(K_PCS)%TAG = BUFL(IND(1,2):IND(2,2))
               ANC%PCS_TAG(K_PCS) = BUFL(IND(1,2):IND(2,2))
!
! ------------ Copy the list to an integer8 type for optimised 
!              searching. Also copy the initial index, because we are
!              going to sort later and will need that index.
!
               K8_PCS             = INT ( K_PCS, 8)
               K8_PCS_ARR(K8_PCS) = K8_PCS
               I8PCS              = 0
               CALL MEMCPY ( I8PCS, %REF(ANC%PCS_TAG(K_PCS)), %VAL(8) )
!@@DON'TNEED@!               ANC%I8PCS(K_PCS)   = I8PCS
               I8PCS_ARR(K_PCS)   = I8PCS
! ------------
               READ ( UNIT=BUFL(IND(1,3):IND(2,3)), FMT='(F20.5)' )     &
     &                ANC%PCS(K_PCS)%SKY_FRQ                             ! Sky Frequency [MHz]
!
! ------------ Polarization
!
               IF ( BUFL(IND(1,4):IND(2,4)) == 'R' ) THEN
                  ANC%PCS(K_PCS)%POL = ANC__R_POL
               ELSEIF ( BUFL(IND(1,4):IND(2,4)) == 'L' ) THEN
                  ANC%PCS(K_PCS)%POL = ANC__L_POL
               ELSEIF ( BUFL(IND(1,4):IND(2,4)) == 'H' ) THEN
                  ANC%PCS(K_PCS)%POL = ANC__H_POL
               ELSEIF ( BUFL(IND(1,4):IND(2,4)) == 'V' ) THEN
                  ANC%PCS(K_PCS)%POL = ANC__V_POL
               ELSEIF ( BUFL(IND(1,4):IND(2,4)) == 'X' ) THEN
                  ANC%PCS(K_PCS)%POL = ANC__X_POL
               ELSEIF ( BUFL(IND(1,4):IND(2,4)) == 'Y' ) THEN
                  ANC%PCS(K_PCS)%POL = ANC__Y_POL
               ELSE
                  CALL ERR_LOG ( 5042, IUER, 'ANC_PARSE_SIM',           &
     &                    BUFL(IND(1,4):IND(2,4))//' in line '//        &
     &                    TRIM(STR)//' is not a supported '//           &
     &                    'polarization code. We expected '//           &
     &                    'R, L, H, V, X, or Y. Check the antenna '//   &
     &                    'calibration file '//TRIM(FILIN) )
                  RETURN
               END IF
!
! ------------ PC_SENSOR ID
!
               ANC%PCS(K_PCS)%ID = BUFL(IND(1,5):IND(2,5))
! ---------
               READ ( UNIT=BUFL(IND(1,6):IND(2,6)), FMT='(I8)' )        &
     &             ANC%PCS(K_PCS)%IF_IND                                ! Intermediate Frequency Index
!
! ------------ Subband Code
!
               IF ( BUFL(IND(1,7):IND(2,7)) == 'USB' ) THEN
                  ANC%PCS(K_PCS)%SUB = ANC__USB
               ELSEIF ( BUFL(IND(1,7):IND(2,7)) == 'LSB' ) THEN
                  ANC%PCS(K_PCS)%SUB = ANC__LSB
               ELSEIF ( BUFL(IND(1,7):IND(2,7)) == 'n/a' ) THEN
                  ANC%PCS(K_PCS)%SUB = ANC__NA
               ELSEIF ( BUFL(IND(1,7):IND(2,7)) == 'DUMMY' ) THEN
                  ANC%PCS(K_PCS)%SUB = ANC__NA
               ELSE
                  CALL ERR_LOG ( 5043, IUER, 'ANC_PARSE_SIM',           &
     &                    BUFL(IND(1,7):IND(2,7))//' in line '//        &
     &                    TRIM(STR)//' is not a supported subband '//   &
     &                    'code. We expected USB, LSB, n/a, or DUMMY.'  &
     &                    //'Check the antenna calibration file '//     &
     &                    TRIM(FILIN) )
                  RETURN
               END IF
            END IF
!     
! ------ Go through PCAL section
! 
         ELSEIF ( SECT_ID == SECTS(7) ) THEN
! ---------
            IF ( FILOUT .NE. 'UNDF' ) THEN
               IF ( BUFL(1:1) == '#' ) WRITE (112, '(A)' ) TRIM(BUFL)
            END IF
!
! --------- Parse information from the PCAL Block
!
            IF ( BUFL(IND(1,1):IND(2,1)) == 'PCAL:' ) THEN
! ------------
               IF ( LIND < 9 ) THEN
                  CALL ERR_LOG ( 5044, IUER, 'ANC_PARSE_SIM',           &
     &                    'Too few words in PCAL block line '//TRIM(STR)&
     &                    //' of antenna calibration file '//FILIN )
                  RETURN
               END IF
! ---------
               CALL ERR_PASS ( IUER, IER )
               CALL DATE_TO_TIME ( BUFL(IND(1,3):IND(2,3)),             &
     &                             MJD, UTC, IER )
! ------------
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5045, IUER, 'ANC_PARSE_SIM',           &
     &                    'Error in parsing PCAL date '//               &
     &                    BUFL(IND(1,3):IND(2,3))//' in line '//        &
     &                    TRIM(STR) )
                  RETURN
               END IF
! ------------
               IF ( ANC%MJD_PCAL < 0 ) THEN
!%UTC_MTAI%!                  CALL ERR_PASS ( IUER, IER )
!%UTC_MTAI%!                  CALL NERS_GET_UTCMTAI ( NERS,                         &
!%UTC_MTAI%!     &                         (MJD - J2000__MJD)*86400.D0 + UTC,       &
!%UTC_MTAI%!     &                         UTC_MTAI, IER )
!%UTC_MTAI%!                  IF ( IER .NE. 0 ) THEN
!%UTC_MTAI%!                     CALL ERR_LOG ( 5046, IUER, 'ANC_PARSE_SIM',        &
!%UTC_MTAI%!     &                       'Error in getting UTC_MTAI on PCAL Block' )
!%UTC_MTAI%!                     RETURN
!%UTC_MTAI%!                  END IF
!
! --------------- MJD_PCAL and TAI_PCAL
!
                  ANC%MJD_PCAL = MJD
                  ANC%TAI_PCAL = UTC - UTC_MTAI
               END IF
               TAI = UTC - UTC_MTAI
!
! ------------ Update the PCAL counter, given the time diff. error is met
!
               IF ( K_PCAL == 0 ) THEN
                  K_PCAL = 1
               ELSE
                  IF ( DABS( ( MJD - ANC%MJD_PCAL )*86400.D0 +          &
     &                       ( TAI - ANC%TAI_PCAL )          -          &
     &                       ANC%PCAL(K_PCAL)%TIM)                      &
     &                 > 3*ANC__EPS_TIM ) THEN
                     K_PCAL = K_PCAL + 1
                  END IF
               END IF
!
! ------------ Error in loading NUM_PCAL
!
               IF ( K_PCAL > ANC%NUM_PCAL ) THEN
                  CALL IINCH ( K_PCAL, STR1 )
                  CALL IINCH ( ANC%NUM_PCAL, STR2 )
                  CALL ERR_LOG ( 5047, IUER, 'ANC_PARSE_SIM',           &
     &                    'Malformed antenna calibration file '//       &
     &                    TRIM(FILIN)//' -- too many PCAL lines. '//    &
     &                    'Got '//TRIM(STR1)//',expected '//TRIM(STR2) )
                  RETURN
               END IF
!
! ------------ Compute time
!
               ANC%PCAL(K_PCAL)%TIM = (MJD - ANC%MJD_PCAL)*86400.D0 +   &
     &                                (TAI - ANC%TAI_PCAL)
!
! ------------
!
               READ ( UNIT=BUFL(IND(1,2):IND(2,2)), FMT='(I8)' )        &
     &                ANC%PCAL(K_PCAL)%IND_SCA
!
! ------------ Is the sensor tag in PCAL block available in list of tags
!              from PCS_SENSOR block
!
!@@PCSLIST@@!               IND_PCS = LTM_DIF ( 0, ANC%NUM_PCS, ANC%PCS_TAG,         &
!@@PCSLIST@@!     &                             BUFL(IND(1,4):IND(2,4))   )
!@@PCSLIST@@!               IF ( IND_PCS < 1 ) THEN
!@@PCSLIST@@!                  CALL ERR_LOG ( 5048, IUER, 'ANC_PARSE_SIM',           &
!@@PCSLIST@@!     &                    'Tag '//BUFL(IND(1,4):IND(2,4))//             &
!@@PCSLIST@@!     &                    'not part of PC_SENSOR tags in '//TRIM(FILIN) &
!@@PCSLIST@@!     &                    //'.See line '//TRIM(STR)//' on PCAL Block' )
!@@PCSLIST@@!                  RETURN
!@@PCSLIST@@!               END IF

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
! ------------ To minimise the burden of searching, we are going to 
!              check if the current TPS is just one up the previous one
!
               IF ( K_PCAL > 1 ) THEN
!
! --------------- grab the PC_SENSOR TAG on this line, and the one
!                 before.
!
                  IND_PAS = IND_PCS
                  TAG_CUR = BUFL(IND(1,4):IND(2,4))
!
! --------------- Is the current tag just one up from the previous?
!
                  IF ( ANC%PCS_TAG(IND_PAS+1) == TAG_CUR ) THEN
!
! ------------------ If the next tag, is just one up, then we 
!                    don't need to binary search, just update the tag from here.
!     
                     IND_PCS = IND_PAS + 1
! ------------------
                     GO TO 204
                  ELSE
                     GO TO 203
                  END IF
               END IF
! ------------
 203           CONTINUE
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
! ------------ Is the sensor tag in the PCAL block available in the tag
!              list. We are using the integer*8 conversion for 
!              efficient searching.
! ------------ First convert the tag in this line to INTEGER*8
!
               CALL CLRCH ( PCS_TAG )
               PCS_TAG = BUFL(IND(1,4):IND(2,4))
               I8PCS = 0
               CALL MEMCPY ( I8PCS, %REF(PCS_TAG), %VAL(8) )
!
! ------------ Second search for that INTEGER*8 on the sorted array list
!
               IND8_TMP = IFIND_SORT_PL8 ( I8NUM_PCS, I8PCS_ARR, I8PCS )
               IF ( IND8_TMP < 1 ) THEN
                  CALL ERR_LOG ( 5061, IUER, 'ANC_PARSE_SIM',           &
     &                    'Tag '//BUFL(IND(1,4):IND(2,4))//' not '//    &
     &                    'part of PC_SENSOR tags in '//TRIM(FILIN)//   &
     &                    '.See line '//TRIM(STR)//' on the PCAL Block')
                  RETURN
               ELSE
!
! --------------- Finally get the index on the tag name list
!
                  IND8_PCS = K8_PCS_ARR(IND8_TMP)
                  IND_PCS  = INT ( IND8_PCS, 4 )
               END IF
! ------------
 204           CONTINUE                 ! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!
! ------------ Parse the PCal value to the index of PCAL_CMPL in 
!              ANC%PCAL that coincides with the index of the sensor 
!              tag in PC_SENSOR Block. Units = 
!
               READ ( UNIT=BUFL(IND(1,5):IND(2,5)), FMT='(F20.12)' )    &
     &                RAMPL                                                 ! Amplitude
               IF ( ABS(RAMPL - ANC%FILLER_R8) > EPS ) THEN
                  RAMPL = RAMPL*ANC__AMP_SCA
               ELSE
                 RAMPL = ANC__AMP_MIN
               END IF
! ------------
               READ ( UNIT=BUFL(IND(1,6):IND(2,6)), FMT='(F20.12)' )    &
     &                RPHAS                                                 ! Phase angle
!
! ------------ Convert the amplitude and phase to complex number
!
               PCAL_REAL = RAMPL*DCOS(RPHAS)
               PCAL_IMG  = RAMPL*DSIN(RPHAS)
               ANC%PCAL(K_PCAL)%PCAL_CMPL(IND_PCS) =                    &
     &                          CMPLX(PCAL_REAL, PCAL_IMG)
! ------------
               ANC%PCAL(K_PCAL)%SOU_NAM = BUFL(IND(1,7):IND(2,7))        ! Source Name
! ------------ 
               ANC%PCAL(K_PCAL)%SCA_NAM = BUFL(IND(1,8):IND(2,8))        ! Scan name
            END IF
!     
! ------ Go through FMT2GPS_TIMER section
! 
         ELSEIF ( SECT_ID == SECTS(8) ) THEN
!
! --------- Parse the contents of the section to scan_ave
!
            IF ( FILOUT .NE. 'UNDF' ) THEN
               WRITE (112, '(A)' ) TRIM(BUFL)
            END IF
! ---------
            IF ( BUFL(1:1) == '#' ) GOTO 410                             ! Skip comments
!
! --------- Parse information from the FMT2GPS_TIMER
!
            IF ( BUFL(IND(1,1):IND(2,1)) == 'FMT2GPS_TIMER:' ) THEN
! ------------
               IF ( LIND < 3 ) THEN                                       ! No less than 3 words in this block
                  CALL ERR_LOG ( 5049, IUER, 'ANC_PARSE_SIM',           &
     &                    'Too few words in FMT2GPS_TIMER line '//      &
     &                    TRIM(STR)//' of the antcal file '//           &
     &             TRIM(FILIN) )
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
                  CALL ERR_LOG ( 5050, IUER, 'ANC_PARSE_SIM',           &
     &                    'Malformed antenna calibration file '//       &
     &                    TRIM(FILIN)//' -- too many FMT2GPS_TIMER '//  &
     &                    'lines. Got '//TRIM(STR1)//' ,expected '//    &
     &                    TRIM(STR2) )
                  RETURN
               END IF
!
! ------------ Timer Tag
!
               ANC%TGPS(K_TGPS)%TAG = BUFL(IND(1,2):IND(2,2))
               ANC%TGPS_TAG(K_TGPS) = BUFL(IND(1,2):IND(2,2))
!
! ------------ Board name
!
               ANC%TGPS(K_TGPS)%BOARD = BUFL(IND(1,3):IND(2,3))
            END IF
!     
! ------ Go through FMTGPS section
! 
         ELSEIF ( SECT_ID == SECTS(9) ) THEN
!
! --------- Parse the contents of the section to scan_ave
!
            IF ( FILOUT .NE. 'UNDF' ) THEN
               WRITE (112, '(A)' ) TRIM(BUFL)
            END IF
! ---------
            IF ( BUFL(1:1) == '#' ) GOTO 410                             ! Skip comments
!
! ------ Parse information from the FMTGPS Block
!
            IF ( BUFL(IND(1,1):IND(2,1)) == 'FMTGPS:' ) THEN
! ------------
               IF ( LIND < 7 ) THEN
                  CALL ERR_LOG ( 5051, IUER, 'ANC_PARSE_SIM',            &
     &                    'Too few words in FMTGPS block line '//       &
     &                    TRIM(STR)//' of antcal file '//TRIM(FILIN) )
                  RETURN
               END IF
! ------------
               CALL ERR_PASS ( IUER, IER )
               CALL DATE_TO_TIME ( BUFL(IND(1,3):IND(2,3)),          &
     &                     MJD, UTC, IER)
! ------------               
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5052, IUER, 'ANC_PARSE_SIM',               &
     &                    'Error in parsing FMTGPS date '//             &
     &                    BUFL(IND(1,3):IND(2,3))//' in line '//        &
     &                    TRIM(STR) )
                  RETURN
               END IF
! ------------
               IF ( ANC%MJD_GPS < 0 ) THEN
!%UTC_MTAI%!                  CALL ERR_PASS ( IUER, IER )
!%UTC_MTAI%!                  CALL NERS_GET_UTCMTAI ( NERS,                         &
!%UTC_MTAI%!     &                         (MJD - J2000__MJD)*86400.D0 + UTC,       &
!%UTC_MTAI%!     &                         UTC_MTAI, IER )
!%UTC_MTAI%!                  IF ( IER .NE. 0 ) THEN
!%UTC_MTAI%!                     CALL ERR_LOG ( 6880, IUER, 'ANC_PARSE_SIM',            &
!%UTC_MTAI%!     &                       'Error getting UTC_MTAI on FMTGPS Block' )
!%UTC_MTAI%!                     RETURN
!%UTC_MTAI%!                  END IF
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
                  IF ( DABS( (MJD - ANC%MJD_GPS)*86400.D0 +             &
     &                       (TAI - ANC%TAI_GPS) - ANC%GPS(K_GPS)%TIM)  &
     &                 > 3*ANC__EPS_TIM ) THEN
                     K_GPS = K_GPS + 1
                  END IF
               END IF
!
! ------------ Error in logging NUM_GPS
!
               IF ( K_GPS > ANC%NUM_GPS ) THEN
                  CALL IINCH ( K_GPS, STR1 )
                  CALL IINCH ( ANC%NUM_PCS, STR2 )
                  CALL ERR_LOG ( 5053, IUER, 'ANC_PARSE_SIM',           &
     &                    'Malformed antenna calibration file '//       &
     &                    TRIM(FILIN)//' -- too many FMTGPS lines. '//  &
     &                    'Got '//TRIM(STR1)//',expected '//TRIM(STR2) )
                  RETURN
               END IF
!
! ------------ Compute time
!
               ANC%GPS(K_GPS)%TIM = (MJD - ANC%MJD_GPS)*86400.D0 +      &
     &                              (TAI - ANC%TAI_GPS)
! ------------
               READ ( UNIT=BUFL(IND(1,2):IND(2,2)), FMT='(I8)' )        &
     &                ANC%GPS(K_GPS)%IND_SCA
!
! ------------ Is the Timer tag in FMTGPS block available in list 
!              of tags from FMT2GPS_TIMER block
!
               IND_TGPS = LTM_DIF ( 0, ANC%NUM_GPS, ANC%TGPS_TAG,       &
     &                              BUFL(IND(1,4):IND(2,4))   )
               IF ( IND_TGPS < 1 ) THEN
                  CALL ERR_LOG ( 5054, IUER, 'ANC_PARSE_SIM',           &
     &                    'Error in parsing FMT2GPS_TIMER tag '//       &
     &                    BUFL(IND(1,4):IND(2,4))//' in FMTGPS line '// &
     &                    TRIM(STR) )
                  RETURN
               END IF
!
! ------------ Parse the Formatter minus GPS Time value to the index of
!              FMG in ANC%GPS that coincides with the index of the timer
!              tag in the FMT2GPS Block. Units = s
!
               READ ( UNIT=BUFL(IND(1,5):IND(2,5)), FMT='(F20.12)' )    &
     &                ANC%GPS(K_GPS)%FMG(IND_TGPS)
!     
! ------------ In an updated version of Log2ant, we include the PPS 
! ------------ Therefore depending on the version of log2ant one uses, we 
!              could have:
!                 1        2          3           4               5                       6                7      8
!              FMTGPS: Scan_Idx UTC_Time_tag Timer_tag Formatter_minus_GPS_time Formatter_minus_PPS_time Source Scan
!              FMTGPS: Scan_Idx UTC_Time_tag Timer_tag Formatter_minus_GPS_time Source                   Scan
!
               IF ( LIND == 7 ) THEN
!
! --------------- Source Name
!
                  ANC%GPS(K_GPS)%SOU_NAM = BUFL(IND(1,6):IND(2,6))
!
! --------------- Scan name
!
                  ANC%GPS(K_GPS)%SCA_NAM = BUFL(IND(1,7):IND(2,7))
                  
               ELSEIF ( LIND == 8 ) THEN
                  
!
! --------------- Parse the Formatter minus GPS Time value to the index 
!                 of FMG in ANC%GPS that coincides with the index of 
!                 the timer tag in the FMT2GPS Block. Units = s
!
                  READ ( UNIT=BUFL(IND(1,6):IND(2,6)), FMT='(F20.12)' ) &
        &                ANC%GPS(K_GPS)%FMP(IND_TGPS)
!     
! --------------- Source Name
!
                  ANC%GPS(K_GPS)%SOU_NAM = BUFL(IND(1,7):IND(2,7))
!
! ------------ Scan name
!
                  ANC%GPS(K_GPS)%SCA_NAM = BUFL(IND(1,8):IND(2,8))
               END IF
            END IF
!     
! ------ Go through SEFD section
! 
         ELSEIF ( SECT_ID == SECTS(10) ) THEN
!
! --------- Parse the contents of the section to scan_ave
!
            IF ( FILOUT .NE. 'UNDF' ) THEN
               WRITE (112, '(A)' ) TRIM(BUFL)
            END IF
! ---------
            IF ( BUFL(1:1) == '#' ) GOTO 410                             ! Skip comments
!
! --------- Parse information from the "SEFD" Block
!
            IF ( BUFL(IND(1,1):IND(2,1)) == 'SEFD:' ) THEN
! ------------
               IF ( LIND < 11 ) THEN
                  CALL ERR_LOG ( 5055, IUER, 'ANC_PARSE_SIM',           &
     &                    'Too few words on SEFD block line '//         &
     &                    TRIM(STR)//' of antcal file '//TRIM(FILIN) )
                  RETURN
               END IF
! ------------
               CALL ERR_PASS ( IUER, IER )
               CALL DATE_TO_TIME ( BUFL(IND(1,3):IND(2,3)),             &
     &                             MJD, UTC, IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5056, IUER, 'ANC_PARSE_SIM',           &
     &                    'Error in parsing SEFD date '//               &
     &                    BUFL(IND(1,3):IND(2,3))//' in line '//        &
     &                    TRIM(STR) )
                  RETURN
               END IF
! ------------
               IF ( ANC%MJD_SEFD < 0 ) THEN
!%UTC_MTAI%!                  CALL ERR_PASS ( IUER, IER )
!%UTC_MTAI%!                  CALL NERS_GET_UTCMTAI ( NERS,                         &
!%UTC_MTAI%!     &                         (MJD - J2000__MJD)*86400.D0 + UTC,       &
!%UTC_MTAI%!     &                         UTC_MTAI, IER )
!%UTC_MTAI%!                  IF ( IER .NE. 0 ) THEN
!%UTC_MTAI%!                     CALL ERR_LOG ( 5057, IUER, 'ANC_PARSE_SIM',        &
!%UTC_MTAI%!     &                       'Error in getting UTC_MTAI on SEFD Block' )
!%UTC_MTAI%!                     RETURN
!%UTC_MTAI%!                  END IF
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
                  IF ( DABS( (MJD-ANC%MJD_SEFD)*86400.D0 +              &
     &                       (TAI-ANC%TAI_SEFD) - ANC%SEFD(K_SEFD)%TIM) &
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
                  CALL ERR_LOG ( 5058, IUER, 'ANC_PARSE_SIM',           &
     &                    'Malformed antenna calibration file '//       &
     &                    TRIM(FILIN)//' -- too many SEFD lines. Got '  &
     &                    //TRIM(STR1)//', expected '//TRIM(STR2) )
                  RETURN
               END IF
!
! ------------ Compute time
!
               ANC%SEFD(K_SEFD)%TIM = (MJD - ANC%MJD_SEFD)*86400.D0 +   &
     &                                (TAI - ANC%TAI_SEFD)
!@@SEFDTPSLIST@@!!
!@@SEFDTPSLIST@@!! ------------ Is the sensor tag in TSYS block available in list of tags
!@@SEFDTPSLIST@@!!              from TPS_SENSOR block
!@@SEFDTPSLIST@@!!
!@@SEFDTPSLIST@@!               IND_TPS = LTM_DIF ( 0, ANC%NUM_TPS, ANC%TPS_TAG,         &
!@@SEFDTPSLIST@@!     &                             BUFL(IND(1,2):IND(2,2))   )
!@@SEFDTPSLIST@@!               IF ( IND_TPS < 1 ) THEN
!@@SEFDTPSLIST@@!                  CALL ERR_LOG ( 5059, IUER, 'ANC_PARSE_SIM',           &
!@@SEFDTPSLIST@@!     &                    'TP_SENSOR tag '//BUFL(IND(1,2):IND(2,2))//   &
!@@SEFDTPSLIST@@!     &                    ' in SEFD block line '//TRIM(STR)//           &
!@@SEFDTPSLIST@@!     &                    ' not found.' )
!@@SEFDTPSLIST@@!                  RETURN
!@@SEFDTPSLIST@@!               END IF

!     
! ------------ Is the sensor tag in the TSYS block available in the tag
!              list. We are using the integer*8 conversion for 
!              efficient searching.
! ------------ First convert the tag in this line to INTEGER*8
!
               CALL CLRCH ( TPS_TAG )
               TPS_TAG = BUFL(IND(1,2):IND(2,2))
               I8TPS = 0
               CALL MEMCPY ( I8TPS, %REF(TPS_TAG), %VAL(8) )
!
! ------------ Second search for that INTEGER*8 on the sorted array list
!
               IND8_TMP = IFIND_SORT_PL8 ( I8NUM_TPS, I8TPS_ARR, I8TPS )
               IF ( IND8_TMP < 1 ) THEN
                  CALL ERR_LOG ( 5059, IUER, 'ANC_PARSE_SIM',           &
     &                    'TP_SENSOR tag '//BUFL(IND(1,2):IND(2,2))//   &
     &                    ' in SEFD block line '//TRIM(STR)//           &
     &                    ' not found.' )
                  RETURN
               ELSE
!
! --------------- Finally get the index on the tag name list
!
                  IND8_TPS = K8_TPS_ARR(IND8_TMP)
                  IND_TPS  = INT ( IND8_TPS, 4 )
               END IF
!     
! ------------ Parse the SEFD value to the index of SEFD in ANC%SEFD that 
!              coincides with the index of the sensor tag in TP_SENSOR 
!              Block. Units = Jy
!
               READ ( UNIT=BUFL(IND(1,4):IND(2,4)), FMT='(F20.12)' )    &
     &                ANC%SEFD(K_SEFD)%SEFD(IND_TPS)
!
! ------------ Parse the Tsys value to the index of TSYS in ANC%SEFD that 
!              coincides with the index of the sensor tag in TP_SENSOR 
!              Block. Units = K
!
               READ ( UNIT=BUFL(IND(1,5):IND(2,5)), FMT='(F20.12)' )    &
     &                ANC%SEFD(K_SEFD)%TSYS(IND_TPS)
!
! ------------ Parse the Tcal value to the index of TCAL in ANC%SEFD that 
!              coincides with the index of the sensor tag in TP_SENSOR 
!              Block. Units = Jy
!
               READ ( UNIT=BUFL(IND(1,6):IND(2,6)), FMT='(F20.12)' )    &
     &                ANC%SEFD(K_SEFD)%TCAL(IND_TPS)
!
! ------------ Parse the Trat value to the index of TRAT in ANC%SEFD that 
!              coincides with the index of the sensor tag in TP_SENSOR 
!              Block. Units = []
!
               READ ( UNIT=BUFL(IND(1,7):IND(2,7)), FMT='(F20.12)' )    &
     &                ANC%SEFD(K_SEFD)%TRAT(IND_TPS)
! 
! ------------ Parse the Gain value to the index of GAIN in ANC%SEFD that 
!              coincides with the index of the sensor tag in TP_SENSOR 
!              Block. Units = 
!
               READ ( UNIT=BUFL(IND(1,8):IND(2,8)), FMT='(F20.12)' )    &
     &                ANC%SEFD(K_SEFD)%GAIN(IND_TPS)
! ------------
               READ ( UNIT=BUFL(IND(1,9):IND(2,9)), FMT='(F20.12)' )    &
     &                ANC%SEFD(K_SEFD)%AZ
               ANC%SEFD(K_SEFD)%AZ = ANC%SEFD(K_SEFD)%AZ*DEG__TO__RAD    ! Azimuth [rad]
! ------------
               READ ( UNIT=BUFL(IND(1,10):IND(2,10)), FMT='(F20.12)' )  &
     &                ANC%SEFD(K_SEFD)%EL                            
               ANC%SEFD(K_SEFD)%EL = ANC%SEFD(K_SEFD)%EL*DEG__TO__RAD    ! Elevation [rad]
! ------------
               ANC%SEFD(K_SEFD)%SOU_NAM = BUFL(IND(1,11):IND(2,11))      ! Source Name
            END IF
         END IF   ! Going through sections
 410  CONTINUE
! ---      
 300  CONTINUE
! ---
      CLOSE ( UNIT = 111 )
      CLOSE ( UNIT = 112 )
! ---
      RETURN
      END SUBROUTINE ANC_PARSE_SIM !#!
