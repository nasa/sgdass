      PROGRAM     TLE_EXAMPLE_TO_COO
!
! *******************************************************************************
! *                                                                             *
! *                                                                             *
! *                                                                             *
! *                                                                             *
! *******************************************************************************
!
      IMPLICIT    NONE 
      INCLUDE     'ners.i'
      INCLUDE     'ners_local.i'
      INCLUDE     'astro_constants.i'
      INCLUDE     'tle_sgp4.i'
      TYPE        ( NERS__TYPE ) :: NERS
      TYPE        ( EPH__TYPE ) :: EPH(3)
      CHARACTER   STR*128, DIR_TLE*64, FIL_TLE(3)*128, OUT_FIL*128
      INTEGER*8   STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER   ( GB = 1024*1024*1024 )
      PARAMETER   ( STACK_SIZE_IN_BYTES = INT8(4) * GB )
      INTEGER*8,  EXTERNAL :: SET_STACKSIZE 
      INTEGER*4   WHICHCONST
      CHARACTER   OPSMODE, TYPERUN, TYPEINPUT
      CHARACTER   NERS_CONFIG*128, HOME_DIR*128
      CHARACTER   TLEDATE*30, DATE_BEG*21
      REAL*8      UTC_BEG, TAI_BEG, UTC_MTAI
      INTEGER*4   MJD_BEG, IUER, IER
      INTEGER*4   J1
      REAL*8      X_TLE(3), XDOT_TLE(3), X_TRS(3), XDOT_TRS(3), AZ, EL
      REAL*8      XPY(3,3), XDOTPY(3,3), XDIF(3,3), XDOTDIF(3,3)
      LOGICAL*1   LEX
!
! --- Set stacksize. Alternative is to set stacksize in shell:
! --- commands limit stacksize 4000000 or limit -s 4000000
! --- and set evironment variable GOMP_STACKSIZE
! --- Program will crash in attempt to use default stacksize,
! --- because fortran uses stack for storing variables
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8  ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0),     &
     &               %VAL(1) )
!
! ---
!
      CALL GETENVAR ( 'NERS_CONFIG', NERS_CONFIG )
      IF ( NERS_CONFIG == ' ' ) THEN
!
! ------ Second, check $HOME/.ners_config file
!
         CALL GETENVAR ( 'HOME', HOME_DIR )
         NERS_CONFIG = TRIM(HOME_DIR)//'/.ners_config'
         INQUIRE ( FILE=NERS_CONFIG, EXIST=LEX )
         IF ( .NOT. LEX ) THEN
!
! --------- Third, check for the system-wide ners configuration file 
!
            NERS_CONFIG = NERS__CONFIG
         END IF
      END IF
! ---
      CALL ERR_PASS ( IUER, IER )
      CALL NERS_INIT ( NERS_CONFIG, NERS, -1.0D0, -1.0D0, IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 2102, IUER, 'TLE_EXAMPLE',                    &
     &                  'Error in initializing NERS data structure' )
         RETURN
      END IF
!@@NH20240404NH@@!!
!@@NH20240404NH@@!! --- Define TLE file
!@@NH20240404NH@@!!
!@@NH20240404NH@@!      DIR_TLE     = '/progs/tle_20230331/share/'
!@@NH20240404NH@@!      FIL_TLE(1)  = TRIM(DIR_TLE)//'ISS_ZARYA_22163.tle'
!@@NH20240404NH@@!      FIL_TLE(2)  = TRIM(DIR_TLE)//'NAVSTAR-81_22163.tle'
!@@NH20240404NH@@!      FIL_TLE(3)  = TRIM(DIR_TLE)//'TDRS-13_22163.tle'
!@@NH20240404NH@@!!@@@!      FIL_TLE = '/opt64/share/GPS43_22004.tle'
!@@NH20240404NH@@!!
!@@NH20240404NH@@!! --- Define time epoch to compute for the stat 
!@@NH20240404NH@@!!
!@@NH20240404NH@@!      IUER = -1
!@@NH20240404NH@@!      DATE_BEG = '2022.06.12_12:00:00'
!@@NH20240404NH@@!      CALL DATE_TO_TIME ( DATE_BEG, MJD_BEG, UTC_BEG, IUER ) 
!@@NH20240404NH@@!!
!@@NH20240404NH@@!! --- convert time to TAI
!@@NH20240404NH@@!!
!@@NH20240404NH@@!      CALL NERS_GET_UTCMTAI ( NERS,                                     &
!@@NH20240404NH@@!     &                        (MJD_BEG - J2000__MJD)*86400 + UTC_BEG,   &
!@@NH20240404NH@@!     &                        UTC_MTAI, IER )
!@@NH20240404NH@@!      IF ( IER .NE. 0 ) THEN
!@@NH20240404NH@@!         CALL ERR_LOG ( 2102, IUER, 'TLE_EXAMPLE',                      &
!@@NH20240404NH@@!     &                  'Error in extracting UTC minus TAI function' )
!@@NH20240404NH@@!         RETURN
!@@NH20240404NH@@!      END IF
!@@NH20240404NH@@!!
!@@NH20240404NH@@!! --- ISS_ZARYA at 2022.06.12_12:00:00 [ LEO ~ 450 km]
!@@NH20240404NH@@!!
!@@NH20240404NH@@!      XPY(1,1)   = -4412716.11074D0
!@@NH20240404NH@@!      XPY(1,2)   =  3213478.79910D0
!@@NH20240404NH@@!      XPY(1,3)   =  4038862.29711D0
!@@NH20240404NH@@!      XDOTPY(1,1)  = -5827.15947D0
!@@NH20240404NH@@!      XDOTPY(1,2)  = -3075.31148D0
!@@NH20240404NH@@!      XDOTPY(1,3)  = -3914.27614D0
!@@NH20240404NH@@!!
!@@NH20240404NH@@!! --- NAVSTAR-81 at 2022.06.12_12:00:00 [ MEO ~ 20 000 km]
!@@NH20240404NH@@!!
!@@NH20240404NH@@!      XPY(2,1)   =  -11155298.25403D0
!@@NH20240404NH@@!      XPY(2,2)   =  -18917593.73391D0
!@@NH20240404NH@@!      XPY(2,3)   =  -14926295.94707D0
!@@NH20240404NH@@!      XDOTPY(2,1)  =  3103.88198D0
!@@NH20240404NH@@!      XDOTPY(2,2)  =    -2.19813D0
!@@NH20240404NH@@!      XDOTPY(2,3)  = -2319.50393D0
!@@NH20240404NH@@!!
!@@NH20240404NH@@!! --- TDRS-13 at 2022.06.12_12:00:00 [ DSO ~ 35 000 km]
!@@NH20240404NH@@!!
!@@NH20240404NH@@!      XPY(3,1)   =  14894708.92668D0
!@@NH20240404NH@@!      XPY(3,2)   =  39220555.08252D0
!@@NH20240404NH@@!      XPY(3,3)   =   3289124.55725D0
!@@NH20240404NH@@!      XDOTPY(3,1)  =   -2879.86119D0
!@@NH20240404NH@@!      XDOTPY(3,2)  =    1093.83396D0
!@@NH20240404NH@@!      XDOTPY(3,3)  =     -24.50901D0
!@@NH20240404NH@@!!
!@@NH20240404NH@@!! --- Parse TLE file
!@@NH20240404NH@@!!
!@@NH20240404NH@@!      DO J1 = 1, 3
!@@NH20240404NH@@!         X_TLE = 0.D0
!@@NH20240404NH@@!         XDOT_TLE = 0.D0
!@@NH20240404NH@@!         IUER = -1
!@@NH20240404NH@@!         CALL TLE_PARSER ( FIL_TLE(J1), EPH(J1), IUER )     
!@@NH20240404NH@@!! ------
!@@NH20240404NH@@!         TAI_BEG = UTC_BEG - UTC_MTAI
!@@NH20240404NH@@!! ------
!@@NH20240404NH@@!         IUER = -1
!@@NH20240404NH@@!         CALL TLE_TO_CRS (EPH(J1), MJD_BEG, TAI_BEG, X_TLE, XDOT_TLE, IUER)
!@@NH20240404NH@@!!
!@@NH20240404NH@@!! ---
!@@NH20240404NH@@!!
!@@NH20240404NH@@!!
!@@NH20240404NH@@!! --- Ground Station
!@@NH20240404NH@@!!
!@@NH20240404NH@@!!##      STA_POS(1)  =  1130730.331D0   !GGAO12M
!@@NH20240404NH@@!!##      STA_POS(2)  = -4831246.540D0   !GGAO12M
!@@NH20240404NH@@!!##      STA_POS(3)  =  3994228.904D0   !GGAO12M
!@@NH20240404NH@@!!##         IUER = -1
!@@NH20240404NH@@!!##         CALL TLE_TO_TRS ( EPH, MJD_BEG, TAI_BEG, STA_POS, X_TRS, XDOT_TRS,     &
!@@NH20240404NH@@!!##     &                     AZ, EL, IUER )
!@@NH20240404NH@@!!##         PRINT *, "$$$$$$$$$$ TEST_SGP4_NH_2 - 97 %%%%%%%"
!@@NH20240404NH@@!!##         PRINT *, "X_TRS:    ", X_TRS
!@@NH20240404NH@@!!##         PRINT *, "XDOT_TRS: ", XDOT_TRS
!@@NH20240404NH@@!!
!@@NH20240404NH@@!! ------
!@@NH20240404NH@@!!
!@@NH20240404NH@@!         XDIF(J1, 1:3) = X_TLE - XPY(J1,1:3)
!@@NH20240404NH@@!         XDOTDIF(J1, 1:3) =  XDOT_TLE - XDOTPY(J1,1:3)
!@@NH20240404NH@@!
!@@NH20240404NH@@!         WRITE ( 6, * ) '-------------------------------------------'
!@@NH20240404NH@@!         WRITE ( 6, * ) J1, TRIM( FIL_TLE(J1) )
!@@NH20240404NH@@!         WRITE ( 6, * ) 'XDIF:    ', XDIF(J1, 1:3)
!@@NH20240404NH@@!         WRITE ( 6, * ) 'XDOTDIF: ', XDOTDIF(J1, 1:3)
!@@NH20240404NH@@!         WRITE ( 6, * ) 'DIST_DIFF:  ', DSQRT(XDIF(J1,1)**2 + XDIF(J1,2)**2 + XDIF(J1,3)**2)
!@@NH20240404NH@@!         WRITE ( 6, * ) 'SPEED_DIFF: ', DSQRT(XDOTDIF(J1,1)**2 + XDOTDIF(J1,2)**2 + XDOTDIF(J1,3)**2)
!@@NH20240404NH@@!         WRITE ( 6, * ) '-------------------------------------------'
!@@NH20240404NH@@!
!@@NH20240404NH@@!      END DO
!@@NH20240404NH@@!!@@NH20240404NH@@!!@@NH20240404NH@@!!@@NH20240404NH@@!!@@NH20240404NH@@!!@@NH20240404NH@@!!@@NH20240404NH@@!

!
! --- Define TLE file
!
      DIR_TLE     = '/home/nhabana/data/tle_azel/test_20240404'
      FIL_TLE(1)  = TRIM(DIR_TLE)//'/GNSS_G11_44506.tle'
!
! --- Define time epoch to compute for the start 
!
      IUER = -1
      DATE_BEG = '2023.01.25_00:00:18'
      CALL DATE_TO_TIME ( DATE_BEG, MJD_BEG, UTC_BEG, IUER ) 
!
! --- convert time to TAI
!
      CALL NERS_GET_UTCMTAI ( NERS,                                     &
     &                        (MJD_BEG - J2000__MJD)*86400 + UTC_BEG,   &
     &                        UTC_MTAI, IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 2102, IUER, 'TLE_EXAMPLE',                      &
     &                  'Error in extracting UTC minus TAI function' )
         RETURN
      END IF
!
! --- GPS SVN 75/RINEX G11 at 2023.01.25_00:00:00 {GPS TIME} [ MEO ~ 20 000 km]
!
      XPY(1,1)   = -12105.676714D0
      XPY(1,2)   = -21009.452793D0
      XPY(1,3)   = -10869.760199D0
      XDOTPY(1,1)  = 0.D0
      XDOTPY(1,2)  = 0.D0
      XDOTPY(1,3)  = 0.D0
!
! --- Parse TLE file
!
      DO J1 = 1, 1
         X_TLE = 0.D0
         XDOT_TLE = 0.D0
         IUER = -1
         CALL TLE_PARSER ( FIL_TLE(J1), EPH(J1), IUER )     
! ------
         TAI_BEG = UTC_BEG - UTC_MTAI
! ------
         IUER = -1
         CALL TLE_TO_CRS (EPH(J1), MJD_BEG, TAI_BEG, X_TLE, XDOT_TLE, IUER)
!
! ---
!
!
! --- Ground Station
!
!##      STA_POS(1)  =  1130730.331D0   !GGAO12M
!##      STA_POS(2)  = -4831246.540D0   !GGAO12M
!##      STA_POS(3)  =  3994228.904D0   !GGAO12M
!##         IUER = -1
!##         CALL TLE_TO_TRS ( EPH, MJD_BEG, TAI_BEG, STA_POS, X_TRS, XDOT_TRS,     &
!##     &                     AZ, EL, IUER )
!##         PRINT *, "$$$$$$$$$$ TEST_SGP4_NH_2 - 97 %%%%%%%"
!##         PRINT *, "X_TRS:    ", X_TRS
!##         PRINT *, "XDOT_TRS: ", XDOT_TRS
!
! ------
!
         XDIF(J1, 1:3) = X_TLE - XPY(J1,1:3)
         XDOTDIF(J1, 1:3) =  XDOT_TLE - XDOTPY(J1,1:3)

         WRITE ( 6, * ) '-------------------------------------------'
         WRITE ( 6, * ) J1, TRIM( FIL_TLE(J1) )
         WRITE ( 6, * ) 'XDIF:    ', XDIF(J1, 1:3)
         WRITE ( 6, * ) 'XDOTDIF: ', XDOTDIF(J1, 1:3)
         WRITE ( 6, * ) 'DIST_DIFF:  ', DSQRT(XDIF(J1,1)**2 + XDIF(J1,2)**2 + XDIF(J1,3)**2)
         WRITE ( 6, * ) 'SPEED_DIFF: ', DSQRT(XDOTDIF(J1,1)**2 + XDOTDIF(J1,2)**2 + XDOTDIF(J1,3)**2)
         WRITE ( 6, * ) '-------------------------------------------'

      END DO













      
      END PROGRAM TLE_EXAMPLE_TO_COO  !#!#
