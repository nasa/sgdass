      SUBROUTINE ANC_PARSE ( FILIN, FILOUT, ANC, NERS, IUER )
!
! ************************************************************************
! *                                                                      *
! *   Routine ANC_PARSE                                                  *
! *                                                                      *
! *   INPUT:                                                             *
! *       FILIN   =  ASCII (original) antcal File      { CHAR }          *
! *                                                                      *
! *       FILOUT  =  ASCII (scan average) antcal File  { CHAR, OPT }          *
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
      IMPLICIT   NONE 
      INCLUDE   'atp.i'
      INCLUDE   'ners.i'
      CHARACTER  FILIN*128, FILOUT*128
      TYPE     ( ANC__TYP   ) :: ANC      
      TYPE     ( NERS__TYPE ) :: NERS
      CHARACTER, ALLOCATABLE :: BUF(:)*(ANC__MSTR)
      CHARACTER, ALLOCATABLE :: BUF_SCAV(:)*(ANC__MSTR)
      INTEGER*4  IUER
      CHARACTER  DELIM*3, STR*128, STR1*16, STR2*16
      INTEGER*4  MIND, MJD_MIN, ICNT
      PARAMETER  ( MIND = 32 )
      PARAMETER  ( MJD_MIN = 44329 )
      PARAMETER  ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9) ) ! Null, Space, Tab
      INTEGER*4  NBUF, LIND, IND(2,MIND), IND_DOO, IND_MET
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7
      INTEGER*4  IND_TGPS, IND_PCS, IND_TPS, UNIX_DATE, IS, IER
      INTEGER*4  K_TPS, K_TSYS, K_PCS, K_PCAL, K_SEFD, K_TGPS, K_GPS
      INTEGER*8  SIZE_I8, MLEN_STR
      INTEGER*4  MJD
      REAL*8     TAI, UTC, UTC_MTAI, EPS
      PARAMETER  ( EPS = 1.D-4 )
      REAL*8     RAMPL, RPHAS, PCAL_REAL, PCAL_IMG
      INTEGER*4, EXTERNAL :: LTM_DIF, FILE_INFO
!
      IS = FILE_INFO ( TRIM(FILIN)//CHAR(0), UNIX_DATE, SIZE_I8 )
!@@@ WE MULTIPLY MLEN_STR BY 20, so that the binary version can be read @@@! !MH 20240415_1037
      MLEN_STR = (SIZE_I8/ANC__MAVLS)*20 ! expected number of lines
      ALLOCATE ( BUF(MLEN_STR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
         CALL CLRCH ( STR )
         CALL IINCH ( INT8(ANC__MBUF)*INT8(MLEN_STR), STR )
         CALL ERR_LOG ( 6821, IUER, 'ANC_PARSE',                        &
     &           'Error in an attempt to allocate '//TRIM(STR)//        &
     &           ' bytes of dynamic memory' )
         RETURN 
      END IF
! ---
      CALL ERR_PASS ( IUER, IER )
      CALL ANC_READ ( FILIN, MLEN_STR, NBUF, BUF, IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 6822, IUER, 'ANC_PARSE',                        &
     &           'Error in an attempt to read file with antenna '//     &
     &           'calibration format '//TRIM(FILIN) )
         DEALLOCATE ( BUF )
         RETURN
      END IF
!
      IF ( BUF(1)(1:24) .NE. '# ANTCAL Format  Version' ) THEN
         CALL ERR_LOG ( 6823, IUER, 'ANC_PARSE',                        &
     &           'Error in an attempt to read file with antenna '//     &
     &           'calibration format '//TRIM(FILIN)//                   &
     &           ' -- the first line does not contain magic' )
         DEALLOCATE ( BUF )
         RETURN 
      END IF
!
! --- Allocate the same size of the original file to the scan average
!     output
!
      IF ( FILOUT == 'UNDF' ) GOTO 200 
!
      ALLOCATE ( BUF_SCAV(MLEN_STR/8), STAT=IER )
      IF ( IER .NE. 0 ) THEN
         CALL CLRCH ( STR )
         CALL IINCH ( INT8(ANC__MBUF)*INT8(MLEN_STR), STR )
         CALL ERR_LOG ( 6883, IUER, 'ANC_PARSE',                        &
     &           'Error in an attempt to allocate '//TRIM(STR)//        &
     &           ' bytes of dynamic memory for average file' )
         RETURN 
      END IF

!
! --- Write the first line to the the first buffer
!
      BUF_SCAV(1) = BUF(1)
! ---
 200  CONTINUE
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
!
! --- Parse Format version
!
      ANC%ANTCAL_FMT = BUF(1)
!
! --- Counter of lines in scan average file
! --- N.B: We've already filled the first line
!     
      ICNT = 1 
!
! --- Go through file
!     
      DO 410 J1=2,NBUF
!
! ------ Copy contents of original file to average file
! ------ N.B: We are not copying the TPI, TSYS, and PCAL sections 
!
         IF ( FILOUT == 'UNDF' ) GOTO 210 ! File is undefined, so skip this part
!
         IF ( BUF(J1)(1:9) == 'NUM_TSYS:' ) THEN
            BUF_SCAV(ICNT) = 'NUM_TSYS: 0'
         ELSEIF ( BUF(J1)(1:5) == 'TSYS:' ) THEN
            CONTINUE
         ELSEIF ( BUF(J1)(1:9) == 'NUM_TPI:' ) THEN
            BUF_SCAV(ICNT) = 'NUM_TPI: 0'
         ELSEIF ( BUF(J1)(1:5) == 'TPI:' ) THEN
            CONTINUE
         ELSEIF ( BUF(J1)(1:9) == 'NUM_PCAL:' ) THEN
            BUF_SCAV(ICNT) = 'NUM_PCAL: 0'
         ELSEIF ( BUF(J1)(1:5) == 'PCAL:' ) THEN
            CONTINUE
         ELSE
            ICNT = ICNT + 1
            BUF_SCAV(ICNT) = BUF(J1)
         END IF
! ------
 210     CONTINUE
!
! ------ Store line number to STR
!
         CALL CLRCH (     STR )
         CALL INCH  ( J1, STR )
! ------
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410 ! Skip comments
! ------
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, IER ) ! Extract words
! ------
         IF ( LIND < 2 ) GOTO 410 ! Skip 1 word lines
! ------ 
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'STATION:' ) THEN
            ANC%STA_NAM  = BUF(J1)(IND(1,2):IND(2,2))           ! Get station name
! ------
         ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) == 'EXP_CODE:' ) THEN
            ANC%EXP_CODE = BUF(J1)(IND(1,2):IND(2,2))           ! Get the experiment code
! ------
         ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) == 'UTC_MTAI:' ) THEN
            READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.5)' )     &
     &             ANC%UTC_MTAI                                 ! UTC_MTAI
!
! ------ Parse the filler information
!        -99.9 for real, -999 for integers, and n/a for characters
!
         ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) == 'FILLERS:' ) THEN
!
! --------- If not enough fillers are described
!
            IF ( LIND < 4 ) THEN
               CALL ERR_LOG ( 6824, IUER, 'ANC_PARSE',                  &
     &                 'Too few words on FILLERS block line '//TRIM(STR)&
     &                 //' of the antenna calibration file '//FILIN )
               DEALLOCATE ( BUF )
               RETURN
            END IF
! ---------
            READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.5)' )     &
     &             ANC%FILLER_R8                                ! -99.9
            READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(I8)'    )     &
     &             ANC%FILLER_I4                                ! -999
            ANC%FILLER_CH = BUF(J1)(IND(1,4):IND(2,4))          ! n/a
!
! ------ How many PROVENACEs are there?
!
         ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUM_PROVENANCE:' ) THEN
            READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I8)' )        &
     &             ANC%NUM_PRV
!
! ------ How many DATA_ONs are there?
!        N.B: This is akeen to the number of scans
!
         ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUM_DATA_ON:' ) THEN
            READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I8)' )        &
     &             ANC%NUM_DOO
!
! --------- Allocate the number to ANC%DOO
!
            ALLOCATE ( ANC%DOO(ANC%NUM_DOO), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6825, IUER, 'ANC_PARSE',                  &
     &                 'Error in allocating memory for ANC%DOO while'// &
     &                 ' processing antenna calibration file'//FILIN )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! ------ How many METEOs are there? 
!        N.B: It seems like typically NUM_METEO = NUM_DATA_ON
!
         ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUM_METEO:' ) THEN
            READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I8)' )        &
     &             ANC%NUM_MET
!
! --------- Allocate the number to ANC%MET
!
            ALLOCATE ( ANC%MET(ANC%NUM_MET), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6826, IUER, 'ANC_PARSE',                  &
     &                 'Error in allocating memory for ANC%METEO while' &
     &               //' processing antenna calibration file '//FILIN )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! ------ How many Tsys Ports are there?
!
         ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUM_TP_SENSOR:' ) THEN
            READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I8)' )        &
     &             ANC%NUM_TPS
!
! --------- Allocate the number to TPS
!
            ALLOCATE ( ANC%TPS(ANC%NUM_TPS), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6827, IUER, 'ANC_PARSE',                  &
     &                 'Error in allocating memory for ANC%TPS while'   &
     &               //' processing antenna calibration file '//FILIN )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! ------ How many Tsys observations are there?
!
         ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUM_TSYS:' ) THEN
            READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I8)' )        &
     &             ANC%NUM_TSYS
!
! --------- Allocate number to ANC%TSYS
!
            ALLOCATE ( ANC%TSYS(ANC%NUM_TSYS), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6828, IUER, 'ANC_PARSE',                  &
     &                 'Error in allocating memory for ANC%TSYS while ' &
     &               //'processing antenna calibration file '//FILIN )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! --------- Allocate NUM_TPS in each time stamp's TSYS
!
            DO 420 J2=1,ANC%NUM_TSYS
               ALLOCATE ( ANC%TSYS(J2)%TSYS(ANC%NUM_TPS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6829, IUER, 'ANC_PARSE',               &
     &                    'Error in allocating memory for the '//       &
     &                    'ANC%TSYS(j2)%TSYS while processing ' //      &
     &                    'antenna calibration file '//TRIM(FILIN) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
               ANC%TSYS(J2)%TSYS = ANC%FILLER_R8
 420        CONTINUE 
!
! ------ How many TPI are there?
!
         ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUM_TPI:' ) THEN
            READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I8)' )        &
     &             ANC%NUM_TPI
!
! ------ How many PC Sensors are there?
!
         ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUM_PC_SENSOR:' ) THEN
            READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I8)' )        &
     &             ANC%NUM_PCS
!
! --------- Allocate the number to PCS
!
            ALLOCATE ( ANC%PCS(ANC%NUM_PCS), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6830, IUER, 'ANC_PARSE',                  &
     &                 'Error in allocating memory for ANC%PCS while '  &
     &               //'processing antenna calibration file '//FILIN )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! ------ How many Phase calibrations are there?
!
         ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUM_PCAL:' ) THEN
            READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I8)' )        &
     &             ANC%NUM_PCAL
!
! --------- Allocate number to ANC%PCAL
!
            ALLOCATE ( ANC%PCAL(ANC%NUM_PCAL), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6831, IUER, 'ANC_PARSE',                  &
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
                  CALL ERR_LOG ( 6832, IUER, 'ANC_PARSE',               &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%PCAL(j2)%PCAL_CMPL while processing '//  &
     &                    'antenna calibration file '//FILIN)
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
               ANC%PCAL(J2)%PCAL_CMPL = CMPLX(ANC__FILLER_R4, 0.0)
 430        CONTINUE 
!
! ------ How many GPS timers are there?
!
         ELSEIF ( BUF(J1)(IND(1,1):IND(2,1))=='NUM_FMT2GPS_TIMER:') THEN
            READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I8)' )        &
     &             ANC%NUM_TGPS
!
! --------- Allocate the number to TGPS
!
            ALLOCATE ( ANC%TGPS(ANC%NUM_TGPS), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6833, IUER, 'ANC_PARSE',                  &
     &                 'Error allocating memory for ANC%TGPS while '//  &
     &                 'processing antenna calibration file '//FILIN )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! ------ How many GPS observations are there, per sensor?
!        i.e. TOTAL_OBS = NUM_FMT2GPS_TIMER * NUM_FMTGPS
!
         ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUM_FMTGPS:' ) THEN
            READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I8)' )        &
     &             ANC%NUM_GPS
!
! --------- Allocate number to ANC%GPS
!
            ALLOCATE ( ANC%GPS(ANC%NUM_GPS), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6834, IUER, 'ANC_PARSE',                  &
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
                  CALL ERR_LOG ( 6835, IUER, 'ANC_PARSE',               &
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
                  CALL ERR_LOG ( 6836, IUER, 'ANC_PARSE',               &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%GPS(J2)%FMP while processing antenna '// &
     &                    'calibration file '//FILIN)
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
               ANC%GPS(J2)%FMP = ANC%FILLER_R8
 450        CONTINUE 
!
! ------ How many SEFD's are there
!
         ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUM_SEFD:' ) THEN
            READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I8)' )        &
     &             ANC%NUM_SEFD
!
! --------- Allocate number to ANC%SEFD
!
            ALLOCATE ( ANC%SEFD(ANC%NUM_SEFD), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6837, IUER, 'ANC_PARSE',                  &
     &                 'Error in allocating memory for ANC%SEFD while ' &
     &               //'processing antcal file '//TRIM(FILIN) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! --------- Allocate NUM_TPS pointers in ANC%SEFD
!in each time stamp's SEFD, TSYS, TCAL, 
!           TRAT, and GAIN
!
            DO 440 J2=1,ANC%NUM_SEFD
!
! ------------ SEFD Time stamps
!
               ALLOCATE ( ANC%SEFD(J2)%SEFD(ANC%NUM_TPS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6838, IUER, 'ANC_PARSE',               &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%SEFD(j2)%SEFD while processing '//       &
     &                    'antcal file '//TRIM(FILIN) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
               ANC%SEFD(J2)%SEFD = ANC%FILLER_R8
!
! ------------ TSYS Time stamps
!
               ALLOCATE ( ANC%SEFD(J2)%TSYS(ANC%NUM_TPS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6839, IUER, 'ANC_PARSE',               &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%SEFD(j2)%TSYS while processing '//       &
     &                    'antcal file '// TRIM(FILIN) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
               ANC%SEFD(J2)%TSYS = ANC%FILLER_R8
!
! ------------ TCAL Time stamps
!
               ALLOCATE ( ANC%SEFD(J2)%TCAL(ANC%NUM_TPS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6840, IUER, 'ANC_PARSE',               &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%SEFD(j2)%TCAL while processing '//       &
     &                    'antcal file '// TRIM(FILIN) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
               ANC%SEFD(J2)%TCAL = ANC%FILLER_R8
!
! ------------ TRAT Time stamps
!
               ALLOCATE ( ANC%SEFD(J2)%TRAT(ANC%NUM_TPS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6841, IUER, 'ANC_PARSE',               &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%SEFD(j2)%TRAT while processing '//       &
     &                    'antcal file '//TRIM(FILIN) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
               ANC%SEFD(J2)%TRAT = ANC%FILLER_R8
!
! ------------ GAIN Time stamps
!
               ALLOCATE ( ANC%SEFD(J2)%GAIN(ANC%NUM_TPS), STAT=IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6842, IUER, 'ANC_PARSE',               &
     &                    'Error in allocating memory for '//           &
     &                    'ANC%SEFD(j2)%GAIN while processing '//       &
     &                    'antcal file '//TRIM(FILIN) )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
               ANC%SEFD(J2)%GAIN = ANC%FILLER_R8
 440        CONTINUE
!
! ------ Parse information from the "DATA_ON" block
!
         ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) == 'DATA_ON:' ) THEN
! ---------
            IF ( LIND < 6 ) THEN                ! We expect at least 6 words in line
               CALL CLRCH ( STR )
               CALL INCH  ( J1, STR )
               CALL ERR_LOG ( 6843, IUER, 'ANC_PARSE',                  &
     &                 'Too few words on DATA_ON block line '//TRIM(STR)&
     &                 //' of the antenna calibration file '//FILIN )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! --------- We reached "DATA_ON" before we actually got a count on 
!           "NUM_DATA_ON"
!
            IF ( .NOT. ASSOCIATED ( ANC%DOO ) ) THEN
               CALL ERR_LOG ( 6844, IUER, 'ANC_PARSE',                  &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- DATA_ON: preceeds NUM_DATA_ON')
               DEALLOCATE ( BUF )
               RETURN
            END IF
! ---------
            CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IND_DOO )
! ---------
            CALL ERR_PASS ( IUER, IER )
            CALL DATE_TO_TIME(BUF(J1)(IND(1,3):IND(2,3)), MJD, UTC, IER)
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6845, IUER, 'ANC_PARSE',                  &
     &                 'Error in parsing DATA_ON date '//               &
     &                 BUF(J1)(IND(1,3):IND(2,3))//' on line '//        &
     &                 TRIM(STR) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
! --------- 
            IF ( MJD < MJD_MIN ) GOTO 410 ! Skip if MJD DATA_ON is not defined
!
! --------- In case we haven't defined MJD_DOO yet
!
            IF ( ANC%MJD_DOO < 0 ) THEN
               CALL ERR_PASS ( IUER, IER )
               CALL NERS_GET_UTCMTAI (NERS,                              &
     &                                (MJD - J2000__MJD)*86400.D0 + UTC, &
     &                                UTC_MTAI, IER )
               IF ( IER .NE. 0 ) THEN
                  WRITE ( 6, * ) 'J1= ', J1
                  WRITE ( 6, * ) 'DATE= ', BUF(J1)(IND(1,3):IND(2,3))
                  WRITE ( 6, * ) 'MJD= ', MJD, 'UTC= ', UTC
                  CALL ERR_LOG ( 6846, IUER, 'ANC_PARSE',               &
     &                    'Error in getting UTC_MTAI on DATA_ON Block' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Define MJD_DOO, and TAI_DOO
!
               ANC%MJD_DOO = MJD
               ANC%TAI_DOO = UTC - UTC_MTAI
               IF ( ANC%UTC_MTAI .LT. 0.D0 ) THEN
                  ANC%UTC_MTAI = UTC_MTAI
               END IF
            END IF
! ---------
            TAI = UTC - UTC_MTAI
            ANC%DOO(IND_DOO)%TIM(1) = (MJD - ANC%MJD_DOO)*86400.D0 +    &
     &                                (TAI - ANC%TAI_DOO)               ! Initial TIme
! ---------
            CALL ERR_PASS ( IUER, IER )
            CALL DATE_TO_TIME(BUF(J1)(IND(1,4):IND(2,4)), MJD, UTC, IER)
            IF ( LIND < 6 ) THEN
               CALL ERR_LOG ( 6847, IUER, 'ANC_PARSE',                  &
     &                 'Error in parsing DATA_ON date '//               &
     &                  BUF(J1)(IND(1,3):IND(2,3))//' on line '//       &
     &                  TRIM(STR) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
! ---------
            ANC%DOO(IND_DOO)%TIM(2)  = (MJD - ANC%MJD_DOO)*86400.D0 +   &
     &                                 (TAI - ANC%TAI_DOO)              ! Final Time
            ANC%DOO(IND_DOO)%SOU_NAM = BUF(J1)(IND(1,5):IND(2,5)) 
            ANC%DOO(IND_DOO)%SCA_NAM = BUF(J1)(IND(1,6):IND(2,6)) 
!
! ------ Parse information from the "METEO" block
!
         ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) == 'METEO:' ) THEN
! ---------
            IF ( LIND < 9 ) THEN                                        ! We expect at least 9 words per line in this block
               CALL ERR_LOG ( 6848, IUER, 'ANC_PARSE',                  &
     &                 'Too few words on METEO block line '//TRIM(STR)//&
     &                 ' of the antenna calibration file '//FILIN )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! --------- We reached "METEO" before "NUM_METEO"
!
            IF ( .NOT. ASSOCIATED ( ANC%MET ) ) THEN
               CALL ERR_LOG ( 6849, IUER, 'ANC_PARSE',                  &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- METEO: preceeds NUM_METEO' )
               DEALLOCATE ( BUF )
               RETURN
            END IF
! ---------
            CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IND_MET )
!
! --------- NUM_METEO was not counted correctly
!
            IF ( IND_MET > ANC%NUM_MET ) THEN
               CALL ERR_LOG ( 6850, IUER, 'ANC_PARSE',                  &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- too many METEO lines.' )
               DEALLOCATE ( BUF )
               RETURN
            END IF
! ---------
            CALL ERR_PASS ( IUER, IER )
            CALL DATE_TO_TIME(BUF(J1)(IND(1,3):IND(2,3)), MJD, UTC, IER)
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6851, IUER, 'ANC_PARSE',                  &
     &                 'Error in parsing METEO date '//                 &
     &                 BUF(J1)(IND(1,3):IND(2,3))//' on line '//        &
     &                 TRIM(STR) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
! ---------
            IF ( ANC%MJD_MET < 0 ) THEN
               CALL ERR_PASS ( IUER, IER )
               CALL NERS_GET_UTCMTAI (NERS,                              &
     &                                (MJD - J2000__MJD)*86400.D0 + UTC, &
     &                                UTC_MTAI, IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6852, IUER, 'ANC_PARSE',               &
     &                      'Error in getting UTC_MTAI on METEO Block' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ Define MJD_MET and TAI_MET
!
               ANC%MJD_MET = MJD
               ANC%TAI_MET = UTC - UTC_MTAI
            END IF
            TAI = UTC - UTC_MTAI
            ANC%MET(IND_MET)%TIM = (MJD - ANC%MJD_MET)*86400.D0 +       &
     &                             (TAI - ANC%TAI_MET)
! --------- 
            READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F10.5)' )     &
     &             ANC%MET(IND_MET)%TEMP                                ! Temperature [K]
! --------- 
            READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F10.5)' )     &
     &             ANC%MET(IND_MET)%PRES                                ! Pressure [Pa]
! --------- 
            READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F10.5)' )     &
     &             ANC%MET(IND_MET)%HUMID                               ! Humidity [%]
! --------- 
            READ ( UNIT=BUF(J1)(IND(1,7):IND(2,7)), FMT='(I8)' )        &
     &             ANC%MET(IND_MET)%IND_SCA                             ! Scan Index {link to }
! --------- 
            ANC%MET(IND_MET)%SOU_NAM = BUF(J1)(IND(1,8):IND(2,8))
! --------- 
            ANC%MET(IND_MET)%SCA_NAM = BUF(J1)(IND(1,9):IND(2,9))
!
! ------ Parse information from the TP_SENSOR block
!
         ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TP_SENSOR:' ) THEN
! ---------
            IF ( LIND < 10 ) THEN                                       ! No less than 10 words in this block
               CALL ERR_LOG ( 6853, IUER, 'ANC_PARSE',                  &
     &                 'Too few words on TP_SENSOR block line '//       &
     &                 TRIM(STR)//' of the antenna calibration file '// &
     &                 TRIM(FILIN) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! --------- Update TPS counter
!
            K_TPS = K_TPS + 1
!
! --------- We exceeded the defined NUM_TPS
!
            IF ( K_TPS > ANC%NUM_TPS ) THEN
               CALL ERR_LOG ( 6854, IUER, 'ANC_PARSE',                  &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- too many TP_SENSOR lines' )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! --------- Sensor Tag
!
            ANC%TPS(K_TPS)%TAG = BUF(J1)(IND(1,2):IND(2,2))
            ANC%TPS_TAG(K_TPS) = BUF(J1)(IND(1,2):IND(2,2))
! --------- 
            READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F20.5)' )     &
     &             ANC%TPS(K_TPS)%IF_FRQ                                ! Intermediate Frequency [MHz]
            READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F20.5)' )     &
     &             ANC%TPS(K_TPS)%LO_FRQ                                ! Local Oscilator Frequency [MHz]
            READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F20.5)' )     &
     &             ANC%TPS(K_TPS)%SKY_FRQ                               ! Sky Frequency [MHz]
            READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F20.5)' )     &
     &             ANC%TPS(K_TPS)%BDW                                   ! Bandwidth [MHz]
!
! --------- Polarization
!
            IF ( BUF(J1)(IND(1,7):IND(2,7)) == 'R' ) THEN
               ANC%TPS(K_TPS)%POL = ANC__R_POL
            ELSEIF ( BUF(J1)(IND(1,7):IND(2,7)) == 'L' ) THEN
               ANC%TPS(K_TPS)%POL = ANC__L_POL
            ELSEIF ( BUF(J1)(IND(1,7):IND(2,7)) == 'H' ) THEN
               ANC%TPS(K_TPS)%POL = ANC__H_POL
            ELSEIF ( BUF(J1)(IND(1,7):IND(2,7)) == 'V' ) THEN
               ANC%TPS(K_TPS)%POL = ANC__V_POL
            ELSEIF ( BUF(J1)(IND(1,7):IND(2,7)) == 'X' ) THEN
               ANC%TPS(K_TPS)%POL = ANC__X_POL
            ELSEIF ( BUF(J1)(IND(1,7):IND(2,7)) == 'Y' ) THEN
               ANC%TPS(K_TPS)%POL = ANC__Y_POL
            ELSE 
               CALL ERR_LOG ( 6855, IUER, 'ANC_PARSE',                  &
     &                 BUF(J1)(IND(1,7):IND(2,7))//' in line '//        &
     &                 TRIM(STR)//' is not a supported polarization '// &
     &                 'code. We expected R, L, H, V, X, or Y. See '//  &
     &                 'the antenna calibration file '//TRIM(FILIN) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! --------- TP_SENSOR ID
!
            ANC%TPS(K_TPS)%ID = BUF(J1)(IND(1,8):IND(2,8)) 
! ---------
            READ ( UNIT=BUF(J1)(IND(1,9):IND(2,9)), FMT='(I8)' )        &
     &             ANC%TPS(K_TPS)%IF_IND                                ! Intermediate Frequency Index
!
! --------- Subband Code
!
            IF ( BUF(J1)(IND(1,10):IND(2,10)) == 'USB' ) THEN
               ANC%TPS(K_TPS)%SUB = ANC__USB
! --------- 
            ELSEIF ( BUF(J1)(IND(1,10):IND(2,10)) == 'LSB' ) THEN
               ANC%TPS(K_TPS)%SUB = ANC__LSB
! --------- 
            ELSEIF ( BUF(J1)(IND(1,10):IND(2,10)) == 'n/a' ) THEN
               ANC%TPS(K_TPS)%SUB = ANC__NA
! ---------
            ELSEIF ( BUF(J1)(IND(1,10):IND(2,10)) == 'DUMMY' ) THEN
               ANC%TPS(K_TPS)%SUB = ANC__NA
! --------- 
            ELSE
               CALL ERR_LOG ( 6856, IUER, 'ANC_PARSE',                  &
     &                 BUF(J1)(IND(1,10):IND(2,10))//' in line '//      &
     &                 TRIM(STR)//' is not a supported subband code.'// &
     &                 ' We expected USB, LSB, n/a, or DUMMY. See '//   &
     &                 'the antenna calibration file '//TRIM(FILIN) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! ------ Parse information from the TSYS Block
!
         ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TSYS:' ) THEN
! ---------
            IF ( LIND < 10 ) THEN
               CALL ERR_LOG ( 6857, IUER, 'ANC_PARSE',                  &
     &                 'Too few words on TSYS block line '//TRIM(STR)// &
     &                 ' of the antenna calibration file '//FILIN )
               DEALLOCATE ( BUF )
               RETURN
            END IF
! ---------
            CALL ERR_PASS ( IUER, IER )
            CALL DATE_TO_TIME(BUF(J1)(IND(1,3):IND(2,3)), MJD, UTC, IER)
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6858, IUER, 'ANC_PARSE',                  &
     &                 'Error in parsing TSYS date '//                  &
     &                 BUF(J1)(IND(1,3):IND(2,3))//' on line '//        &
     &                 TRIM(STR) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
! ---------
            IF ( ANC%MJD_TSYS < 0 ) THEN
               CALL ERR_PASS ( IUER, IER )
               CALL NERS_GET_UTCMTAI (NERS,                              &
     &                                (MJD - J2000__MJD)*86400.D0 + UTC, &
     &                                UTC_MTAI, IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6859, IUER, 'ANC_PARSE',               &
     &                    'Error in getting UTC_MTAI on TSYS Block' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ MJD_TSYS and TAI_TSYS
!
               ANC%MJD_TSYS = MJD
               ANC%TAI_TSYS = UTC - UTC_MTAI
            END IF
            TAI = UTC - UTC_MTAI
!
! --------- Update the Tsys counter, given the time diff. error is met
!
            IF ( K_TSYS == 0 ) THEN
               K_TSYS = 1
            ELSE
               IF ( DABS((MJD - ANC%MJD_TSYS)*86400.D0 +                &
     &                   (TAI - ANC%TAI_TSYS) - ANC%TSYS(K_TSYS)%TIM) > &
     &              3*ANC__EPS_TIM ) THEN
                  K_TSYS = K_TSYS + 1
               END IF
            END IF
!
! --------- Error in logging NUM_TSYS
!
            IF ( K_TSYS > ANC%NUM_TSYS ) THEN
               CALL IINCH ( K_TSYS, STR1 )
               CALL IINCH ( ANC%NUM_TSYS, STR2 )
               CALL ERR_LOG ( 6860, IUER, 'ANC_PARSE',                  &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- too many TSYS lines. '// &
     &                 'Got '//TRIM(STR1)//' ,expected '//TRIM(STR2) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! --------- Compute time
!
            ANC%TSYS(K_TSYS)%TIM = (MJD - ANC%MJD_TSYS)*86400.D0 +      &
     &                             (TAI - ANC%TAI_TSYS)
! ---------
            READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I8)' )        &
     &             ANC%TSYS(K_TSYS)%IND_SCA
!
! --------- Is the sensor tag in TSYS block available in list of tags
!           from TPS_SENSOR block
!
            IND_TPS = LTM_DIF ( 0, ANC%NUM_TPS, ANC%TPS_TAG,            &
     &                          BUF(J1)(IND(1,4):IND(2,4))   )
            IF ( IND_TPS < 1 ) THEN
               CALL ERR_LOG ( 6861, IUER, 'ANC_PARSE',                  &
     &                 'Tag '//BUF(J1)(IND(1,4):IND(2,4))//             &
     &                 'not part of TP_SENSOR tags in '//TRIM(FILIN)//  &
     &                 ' .See line '//TRIM(STR)//' on the TSYS Block' )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! --------- Parse the Tsys value to the index of TSYS in ANC%TSYS that 
!           coincides with the index of the sensor tag in TP_SENSOR 
!           Block. Units = K
!
            READ (UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F20.12)' )    &
     &            ANC%TSYS(K_TSYS)%TSYS(IND_TPS)
! ---------
            READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F20.12)' )    &
     &             ANC%TSYS(K_TSYS)%AZ                           
            ANC%TSYS(K_TSYS)%AZ = ANC%TSYS(K_TSYS)%AZ*DEG__TO__RAD      ! Azimuth [rad]
! ---------
            READ ( UNIT=BUF(J1)(IND(1,7):IND(2,7)), FMT='(F20.12)' )    &
     &             ANC%TSYS(K_TSYS)%EL                            
            ANC%TSYS(K_TSYS)%EL = ANC%TSYS(K_TSYS)%EL*DEG__TO__RAD      ! Elevation [rad]
! ---------
            ANC%TSYS(K_TSYS)%SOU_NAM = BUF(J1)(IND(1,8):IND(2,8))       ! Source Name
! ---------
            ANC%TSYS(K_TSYS)%SCA_NAM = BUF(J1)(IND(1,9):IND(2,9))       ! Scan name
!$$$$$$$$$$$$$$$$$$
!
! ------ Parse information from the PC_SENSOR block
!
         ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) == 'PC_SENSOR:' ) THEN
! ---------
            IF ( LIND < 7 ) THEN                                       ! No less than 7 words in this block
               CALL ERR_LOG ( 6862, IUER, 'ANC_PARSE',                  &
     &                 'Too few words on PC_SENSOR line block '//       &
     &                 TRIM(STR)//' of the antenna calibration file '// &
     &                 TRIM(FILIN) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! --------- Update PCS counter
!
            K_PCS = K_PCS + 1
!
! --------- We exceeded the defined NUM_PCS
!
            IF ( K_PCS > ANC%NUM_PCS ) THEN
               CALL IINCH ( K_PCS, STR1 )
               CALL IINCH ( ANC%NUM_PCS, STR2 )
               CALL ERR_LOG ( 6863, IUER, 'ANC_PARSE',                  &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- too many PC_SENSORS lines. '// &
     &                 'Got '//TRIM(STR1)//' ,expected '//TRIM(STR2) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! --------- Sensor Tag
!
            ANC%PCS(K_PCS)%TAG = BUF(J1)(IND(1,2):IND(2,2))
            ANC%PCS_TAG(K_PCS) = BUF(J1)(IND(1,2):IND(2,2))
! --------- 
            READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F20.5)' )     &
     &             ANC%PCS(K_PCS)%SKY_FRQ                               ! Sky Frequency [MHz]
!
! --------- Polarization
!
            IF ( BUF(J1)(IND(1,4):IND(2,4)) == 'R' ) THEN
               ANC%PCS(K_PCS)%POL = ANC__R_POL
            ELSEIF ( BUF(J1)(IND(1,4):IND(2,4)) == 'L' ) THEN
               ANC%PCS(K_PCS)%POL = ANC__L_POL
            ELSEIF ( BUF(J1)(IND(1,4):IND(2,4)) == 'H' ) THEN
               ANC%PCS(K_PCS)%POL = ANC__H_POL
            ELSEIF ( BUF(J1)(IND(1,4):IND(2,4)) == 'V' ) THEN
               ANC%PCS(K_PCS)%POL = ANC__V_POL
            ELSEIF ( BUF(J1)(IND(1,4):IND(2,4)) == 'X' ) THEN
               ANC%PCS(K_PCS)%POL = ANC__X_POL
            ELSEIF ( BUF(J1)(IND(1,4):IND(2,4)) == 'Y' ) THEN
               ANC%PCS(K_PCS)%POL = ANC__Y_POL
            ELSE 
               CALL ERR_LOG ( 6864, IUER, 'ANC_PARSE',                  &
     &                 BUF(J1)(IND(1,4):IND(2,4))//' in line '//        &
     &                 TRIM(STR)//' is not a supported polarization '// &
     &                 'code. We expected R, L, H, V, X, or Y. See '//  &
     &                 'the antenna calibration file '//TRIM(FILIN) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! --------- PC_SENSOR ID
!
            ANC%PCS(K_PCS)%ID = BUF(J1)(IND(1,5):IND(2,5)) 
! ---------
            READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(I8)' )        &
     &             ANC%PCS(K_PCS)%IF_IND                                ! Intermediate Frequency Index
!
! --------- Subband Code
!
            IF ( BUF(J1)(IND(1,7):IND(2,7)) == 'USB' ) THEN
               ANC%PCS(K_PCS)%SUB = ANC__USB
            ELSEIF ( BUF(J1)(IND(1,7):IND(2,7)) == 'LSB' ) THEN
               ANC%PCS(K_PCS)%SUB = ANC__LSB
            ELSEIF ( BUF(J1)(IND(1,7):IND(2,7)) == 'n/a' ) THEN
               ANC%PCS(K_PCS)%SUB = ANC__NA
            ELSEIF ( BUF(J1)(IND(1,7):IND(2,7)) == 'DUMMY' ) THEN
               ANC%PCS(K_PCS)%SUB = ANC__NA
            ELSE
               CALL ERR_LOG ( 6865, IUER, 'ANC_PARSE',                  &
     &                 BUF(J1)(IND(1,7):IND(2,7))//' in line '//        &
     &                 TRIM(STR)//' is not a supported subband code.'// &
     &                 ' We expected USB, LSB, n/a, or DUMMY. See '//   &
     &                 'the antenna calibration file '//TRIM(FILIN) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! ------ Parse information from the PCAL Block
!
         ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'PCAL:' ) THEN
! ---------
            IF ( LIND < 9 ) THEN
               CALL ERR_LOG ( 6866, IUER, 'ANC_PARSE',                  &
     &                 'Too few words in PCAL block line '//TRIM(STR)// &
     &                 ' of the antenna calibration file '//FILIN )
               DEALLOCATE ( BUF )
               RETURN
            END IF
! ---------
            CALL ERR_PASS ( IUER, IER )
            CALL DATE_TO_TIME(BUF(J1)(IND(1,3):IND(2,3)), MJD, UTC, IER)
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6867, IUER, 'ANC_PARSE',                  &
     &                 'Error in parsing PCAL date '//                       &
     &                 BUF(J1)(IND(1,3):IND(2,3))//' in line '//        &
     &                 TRIM(STR) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
! ---------
            IF ( ANC%MJD_PCAL < 0 ) THEN
               CALL ERR_PASS ( IUER, IER )
               CALL NERS_GET_UTCMTAI (NERS,                              &
     &                                (MJD - J2000__MJD)*86400.D0 + UTC, &
     &                                UTC_MTAI, IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6868, IUER, 'ANC_PARSE',               &
     &                    'Error in getting UTC_MTAI on PCAL Block' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ MJD_PCAL and TAI_PCAL
!
               ANC%MJD_PCAL = MJD
               ANC%TAI_PCAL = UTC - UTC_MTAI
            END IF
            TAI = UTC - UTC_MTAI
!
! --------- Update the PCAL counter, given the time diff. error is met
!
            IF ( K_PCAL == 0 ) THEN
               K_PCAL = 1
            ELSE
               IF ( DABS((MJD - ANC%MJD_PCAL)*86400.D0 +                &
     &                   (TAI - ANC%TAI_PCAL) - ANC%PCAL(K_PCAL)%TIM) > &
     &              3*ANC__EPS_TIM ) THEN
                  K_PCAL = K_PCAL + 1
               END IF
            END IF
!
! --------- Error in logging NUM_PCAL
!
            IF ( K_PCAL > ANC%NUM_PCAL ) THEN
               CALL IINCH ( K_PCAL, STR1 )
               CALL IINCH ( ANC%NUM_PCAL, STR2 )
               CALL ERR_LOG ( 6869, IUER, 'ANC_PARSE',                  &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- too many PCAL lines. '// &
     &                 'Got '//TRIM(STR1)//' ,expected '//TRIM(STR2) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! --------- Compute time
!
            ANC%PCAL(K_PCAL)%TIM = (MJD - ANC%MJD_PCAL)*86400.D0 +      &
     &                             (TAI - ANC%TAI_PCAL)
!
! ---------
!
            READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I8)' )        &
     &             ANC%PCAL(K_PCAL)%IND_SCA
!
! --------- Is the sensor tag in PCAL block available in list of tags
!           from PCS_SENSOR block
!
            IND_PCS = LTM_DIF ( 0, ANC%NUM_PCS, ANC%PCS_TAG,            &
     &                          BUF(J1)(IND(1,4):IND(2,4))   )
            IF ( IND_PCS < 1 ) THEN
               CALL ERR_LOG ( 6870, IUER, 'ANC_PARSE',                  &
     &                 'Tag '//BUF(J1)(IND(1,4):IND(2,4))//             &
     &                 'not part of PC_SENSOR tags in '//TRIM(FILIN)//  &
     &                 ' .See line '//TRIM(STR)//' on the PCAL Block' )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! --------- Parse the PCal value to the index of PCAL_CMPL in ANC%PCAL 
!           that coincides with the index of the sensor tag in 
!           PC_SENSOR Block. Units = 
!
            READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F20.12)' )    &
     &             RAMPL                                                 ! Amplitude
            IF ( ABS(RAMPL - ANC%FILLER_R8) > EPS ) THEN
                 RAMPL = RAMPL*ANC__AMP_SCA
               ELSE
                 RAMPL = ANC__AMP_MIN
            END IF
! ---------
            READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F20.12)' )    &
     &             RPHAS                                                 ! Phase angle
!
! --------- Convert the amplitude and phase to complex number
!
            PCAL_REAL = RAMPL*DCOS(RPHAS)
            PCAL_IMG  = RAMPL*DSIN(RPHAS)

            ANC%PCAL(K_PCAL)%PCAL_CMPL(IND_PCS) =                       &
     &                       CMPLX(PCAL_REAL, PCAL_IMG)
! ---------
            ANC%PCAL(K_PCAL)%SOU_NAM = BUF(J1)(IND(1,7):IND(2,7))       ! Source Name
! ---------
            ANC%PCAL(K_PCAL)%SCA_NAM = BUF(J1)(IND(1,8):IND(2,8))       ! Scan name
!$$$$$$$$$$$$$$$$$$
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! ------ Parse information from the "SEFD" Block
!

         ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SEFD:' ) THEN
! ---------
            IF ( LIND < 11 ) THEN
               CALL ERR_LOG ( 6871, IUER, 'ANC_PARSE',                  &
     &                 'Too few words on SEFD block line '//TRIM(STR)// &
     &                 ' of the antcal file '//TRIM(FILIN) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
! ---------
            CALL ERR_PASS ( IUER, IER )
            CALL DATE_TO_TIME(BUF(J1)(IND(1,3):IND(2,3)), MJD, UTC, IER)
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6872, IUER, 'ANC_PARSE',                  &
     &                 'Error in parsing SEFD date '//                  &
     &                 BUF(J1)(IND(1,3):IND(2,3))//' in line '//        &
     &                 TRIM(STR) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
! ---------
            IF ( ANC%MJD_SEFD < 0 ) THEN
               CALL ERR_PASS ( IUER, IER )
               CALL NERS_GET_UTCMTAI (NERS,                              &
     &                                (MJD - J2000__MJD)*86400.D0 + UTC, &
     &                                UTC_MTAI, IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6873, IUER, 'ANC_PARSE',               &
     &                    'Error in getting UTC_MTAI on SEFD Block' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ MJD_SEFD and TAI_SEFD
!
               ANC%MJD_SEFD = MJD
               ANC%TAI_SEFD = UTC - UTC_MTAI
            END IF
            TAI = UTC - UTC_MTAI
!
! --------- Update the SEFD counter, given the time diff. error is met
!
            IF ( K_SEFD == 0 ) THEN
               K_SEFD = 1
            ELSE
               IF ( DABS((MJD - ANC%MJD_SEFD)*86400.D0 +                &
     &                   (TAI - ANC%TAI_SEFD) - ANC%SEFD(K_SEFD)%TIM) > &
     &              3*ANC__EPS_TIM ) THEN
                  K_SEFD = K_SEFD + 1
               END IF
            END IF
!
! --------- Error in logging NUM_SEFD
!
            IF ( K_SEFD > ANC%NUM_SEFD ) THEN
               CALL IINCH ( K_SEFD, STR1 )
               CALL IINCH ( ANC%NUM_SEFD, STR2 )
               CALL ERR_LOG ( 6874, IUER, 'ANC_PARSE',                  &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- too many SEFD lines. '//       &
     &                 'Got '//TRIM(STR1)//' ,expected '//TRIM(STR2) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! --------- Compute time
!
            ANC%SEFD(K_SEFD)%TIM = (MJD - ANC%MJD_SEFD)*86400.D0 +      &
     &                             (TAI - ANC%TAI_SEFD)
!
! --------- Is the sensor tag in TSYS block available in list of tags
!           from TPS_SENSOR block
!
            IND_TPS = LTM_DIF ( 0, ANC%NUM_TPS, ANC%TPS_TAG,            &
     &                          BUF(J1)(IND(1,2):IND(2,2))   )
            IF ( IND_TPS < 1 ) THEN
               CALL ERR_LOG ( 6875, IUER, 'ANC_PARSE',                  &
     &                 'TP_SENSOR tag '//BUF(J1)(IND(1,2):IND(2,2))//   &
     &                 ' in SEFD block line '//TRIM(STR)//' not found' )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! --------- Parse the SEFD value to the index of SEFD in ANC%SEFD that 
!           coincides with the index of the sensor tag in TP_SENSOR 
!           Block. Units = Jy
!
            READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F20.12)' )    &
     &             ANC%SEFD(K_SEFD)%SEFD(IND_TPS)
!
! --------- Parse the Tsys value to the index of TSYS in ANC%SEFD that 
!           coincides with the index of the sensor tag in TP_SENSOR 
!           Block. Units = K
!
            READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F20.12)' )    &
     &             ANC%SEFD(K_SEFD)%TSYS(IND_TPS)
!
! --------- Parse the Tcal value to the index of TCAL in ANC%SEFD that 
!           coincides with the index of the sensor tag in TP_SENSOR 
!           Block. Units = Jy
!
            READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F20.12)' )    &
     &             ANC%SEFD(K_SEFD)%TCAL(IND_TPS)
!
! --------- Parse the Trat value to the index of TRAT in ANC%SEFD that 
!           coincides with the index of the sensor tag in TP_SENSOR 
!           Block. Units = []
!
            READ ( UNIT=BUF(J1)(IND(1,7):IND(2,7)), FMT='(F20.12)' )    &
     &             ANC%SEFD(K_SEFD)%TRAT(IND_TPS)
!
! --------- Parse the Gain value to the index of GAIN in ANC%SEFD that 
!           coincides with the index of the sensor tag in TP_SENSOR 
!           Block. Units = 
!
            READ ( UNIT=BUF(J1)(IND(1,8):IND(2,8)), FMT='(F20.12)' )    &
     &             ANC%SEFD(K_SEFD)%GAIN(IND_TPS)
! ---------
            READ ( UNIT=BUF(J1)(IND(1,9):IND(2,9)), FMT='(F20.12)' )    &
     &             ANC%SEFD(K_SEFD)%AZ
            ANC%SEFD(K_SEFD)%AZ = ANC%SEFD(K_SEFD)%AZ*DEG__TO__RAD      ! Azimuth [rad]
! ---------
            READ ( UNIT=BUF(J1)(IND(1,10):IND(2,10)), FMT='(F20.12)' )  &
     &             ANC%SEFD(K_SEFD)%EL                            
            ANC%SEFD(K_SEFD)%EL = ANC%SEFD(K_SEFD)%EL*DEG__TO__RAD      ! Elevation [rad]
! ---------
            ANC%SEFD(K_SEFD)%SOU_NAM = BUF(J1)(IND(1,11):IND(2,11))     ! Source Name
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!###################################################
!
! ------ Parse information from the FMT2GPS_TIMER
!
         ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) == 'FMT2GPS_TIMER:' ) THEN
! ---------
            IF ( LIND < 3 ) THEN                                       ! No less than 3 words in this block
               CALL ERR_LOG ( 6876, IUER, 'ANC_PARSE',                  &
     &                 'Too few words in FMT2GPS_TIMER line '//TRIM(STR)&
     &                 //' of the antcal file '//TRIM(FILIN) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! --------- Update GPS counter
!
            K_TGPS = K_TGPS + 1
!
! --------- We exceeded the defined NUM_TGPS
!
            IF ( K_TGPS > ANC%NUM_TGPS ) THEN
               CALL IINCH ( K_TGPS, STR1 )
               CALL IINCH ( ANC%NUM_TGPS, STR2 )
               CALL ERR_LOG ( 6877, IUER, 'ANC_PARSE',                  &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- too many FMT2GPS_TIMER lines.' &
     &                //' Got '//TRIM(STR1)//' ,expected '//TRIM(STR2) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! --------- Timer Tag
!
            ANC%TGPS(K_TGPS)%TAG = BUF(J1)(IND(1,2):IND(2,2))
            ANC%TGPS_TAG(K_TGPS) = BUF(J1)(IND(1,2):IND(2,2))
!
! --------- Board name
!
            ANC%TGPS(K_TGPS)%BOARD = BUF(J1)(IND(1,3):IND(2,3))
!
! ------ Parse information from the FMTGPS Block
!
         ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) == 'FMTGPS:' ) THEN
! ---------
            IF ( LIND < 7 ) THEN
               CALL ERR_LOG ( 6878, IUER, 'ANC_PARSE',                  &
     &                 'Too few words in FMTGPS block line '//TRIM(STR) &
     &                 //' of the antcal file '//TRIM(FILIN) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
! ---------
            CALL ERR_PASS ( IUER, IER )
            CALL DATE_TO_TIME(BUF(J1)(IND(1,3):IND(2,3)), MJD, UTC, IER)
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 6879, IUER, 'ANC_PARSE',                  &
     &                 'Error in parsing FMTGPS date '//                &
     &                 BUF(J1)(IND(1,3):IND(2,3))//' in line '//        &
     &                 TRIM(STR) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
! ---------
            IF ( ANC%MJD_GPS < 0 ) THEN
               CALL ERR_PASS ( IUER, IER )
               CALL NERS_GET_UTCMTAI (NERS,                              &
     &                                (MJD - J2000__MJD)*86400.D0 + UTC, &
     &                                UTC_MTAI, IER )
               IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6880, IUER, 'ANC_PARSE',               &
     &                    'Error in getting UTC_MTAI on FMTGPS Block' )
                  DEALLOCATE ( BUF )
                  RETURN
               END IF
!
! ------------ MJD_GPS and TAI_GPS
!
               ANC%MJD_GPS = MJD
               ANC%TAI_GPS = UTC - UTC_MTAI
            END IF
            TAI = UTC - UTC_MTAI
!
! --------- Update the GPS counter, given the time diff. error is met
!
            IF ( K_GPS == 0 ) THEN
               K_GPS = 1
            ELSE
               IF ( DABS((MJD - ANC%MJD_GPS)*86400.D0 +                 &
     &                   (TAI - ANC%TAI_GPS) - ANC%GPS(K_GPS)%TIM) >    &
     &              3*ANC__EPS_TIM ) THEN
                  K_GPS = K_GPS + 1
               END IF
            END IF
!
! --------- Error in logging NUM_GPS
!
            IF ( K_GPS > ANC%NUM_GPS ) THEN
               CALL IINCH ( K_GPS, STR1 )
               CALL IINCH ( ANC%NUM_PCS, STR2 )
               CALL ERR_LOG ( 6881, IUER, 'ANC_PARSE',                  &
     &                 'Malformed antenna calibration file '//          &
     &                 TRIM(FILIN)//' -- too many FMTGPS lines. '//     &
     &                 'Got '//TRIM(STR1)//' ,expected '//TRIM(STR2) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! --------- Compute time
!
            ANC%GPS(K_GPS)%TIM = (MJD - ANC%MJD_GPS)*86400.D0 +         &
     &                           (TAI - ANC%TAI_GPS)
! ---------
            READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I8)' )        &
     &             ANC%GPS(K_GPS)%IND_SCA
!
! --------- Is the Timer tag in FMTGPS block available in list of tags
!           from FMT2GPS_TIMER block
!
            IND_TGPS = LTM_DIF ( 0, ANC%NUM_GPS, ANC%TGPS_TAG,          &
     &                           BUF(J1)(IND(1,4):IND(2,4))   )
            IF ( IND_TGPS < 1 ) THEN
               CALL ERR_LOG ( 6882, IUER, 'ANC_PARSE',                  &
     &                 'Error in parsing FMT2GPS_TIMER tag '//          &
     &                 BUF(J1)(IND(1,4):IND(2,4))//' in FMTGPS line '// &
     &                 TRIM(STR) )
               DEALLOCATE ( BUF )
               RETURN
            END IF
!
! --------- Parse the Formatter minus GPS Time value to the index of
!           FMG in ANC%GPS that coincides with the index of the timer
!           tag in the FMT2GPS Block. Units = s
!
            READ (UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F20.12)' )     &
     &            ANC%GPS(K_GPS)%FMG(IND_TGPS)
!     
! --------- In an updated version of Log2ant, we include the PPS 
! --------- Therefore depending on the version of log2ant one uses, we 
!           could have:
!              1        2          3           4               5                       6                7      8
!           FMTGPS: Scan_Idx UTC_Time_tag Timer_tag Formatter_minus_GPS_time Formatter_minus_PPS_time Source Scan
!           FMTGPS: Scan_Idx UTC_Time_tag Timer_tag Formatter_minus_GPS_time Source                   Scan
!
            IF ( LIND == 7 ) THEN
!
! ------------ Source Name
!
               ANC%GPS(K_GPS)%SOU_NAM = BUF(J1)(IND(1,6):IND(2,6))
!
! ------------ Scan name
!
               ANC%GPS(K_GPS)%SCA_NAM = BUF(J1)(IND(1,7):IND(2,7))
            ELSEIF ( LIND == 8 ) THEN
!
! ------------ Parse the Formatter minus GPS Time value to the index 
!              of FMG in ANC%GPS that coincides with the index of 
!              the timer tag in the FMT2GPS Block. Units = s
!
               READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F20.12)' ) &
     &                ANC%GPS(K_GPS)%FMP(IND_TGPS)
!     
! ------------ Source Name
!
               ANC%GPS(K_GPS)%SOU_NAM = BUF(J1)(IND(1,7):IND(2,7))
!
! ------------ Scan name
!
               ANC%GPS(K_GPS)%SCA_NAM = BUF(J1)(IND(1,8):IND(2,8))
            END IF
!###################################################
         END IF
 410  CONTINUE 
!
! --- Copy the anc file information, less tsys and pcal data, to
!     another file. then clean it.
!
      IF ( ALLOCATED ( BUF_SCAV ) ) THEN
! ------
         IUER = -1
         CALL WR_TEXT ( ICNT, BUF_SCAV, FILOUT, IUER )
         IF ( IUER .NE. 0 ) THEN
            IUER = -1
            CALL ERR_LOG ( 6884, IUER, 'ANC_PARSE',                      &
     &             'Error in writing the output scan average file '//       &
     &              TRIM(FILOUT) )
            CALL EXIT ( 1 )
         END IF
! ------
         DEALLOCATE ( BUF_SCAV )
      END IF
! ---
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE ANC_PARSE   !#!
