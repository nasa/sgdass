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
      REAL*8      STA_POS(3)
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
      REAL*8      XTLE(3), VTLE(3), XTRS(3), VTRS(3), AZ, EL
      REAL*8      XCRS(3), VCRS(3)
      REAL*8      XTRS_JS(3), VTRS_JS(3), XCRS_JS(3), VCRS_JS(3)
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
!
! --- Define TLE file
!
      DIR_TLE     = '/home/nhabana/data/tle_azel/test_20250322'
      FIL_TLE(1)  = TRIM(DIR_TLE)//'/24876_GPS-BIIR-2PRN13_20250321.tle'
!
! --- Define time epoch to compute for the start 
!
      IUER = -1
      DATE_BEG = '2025.03.19_00:00:00' ! UTC
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
! --- GPS  BIIR-2 PRN13 at 2025.03.19_00:00:00 {GPS TIME} [ MEO ~ 20 000 km]
!
      XPY(1,1)   =  13544997.120D0
      XPY(1,2)   = -22663731.935D0
      XPY(1,3)   =    809788.284D0
      XDOTPY(1,1)  = 0.D0
      XDOTPY(1,2)  = 0.D0
      XDOTPY(1,3)  = 0.D0
!
! -- 
!
      XTRS_JS(1)     =  13547672.664D0
      XTRS_JS(2)     = -22658984.079D0
      XTRS_JS(3)     =    867882.142D0
      VTRS_JS(1)  =       161.487D0
      VTRS_JS(2)  =       251.837D0
      VTRS_JS(3)  =      3216.720D0
! ---
      XCRS_JS(1)  = -12125301.145D0
      XCRS_JS(2)  =  23449844.175D0
      XCRS_JS(3)  =    896712.791D0
      VCRS_JS(1)  =     -1878.776D0
      VCRS_JS(2)  =     -1125.647D0
      VCRS_JS(3)  =      3221.379D0


      PRINT *, "%%%%%%%%%%%%%%%% TLE_EXAMPLE_TO_COO - 121 %%%%%%%%%%"
!
! --- Parse TLE file
!
      DO J1 = 1, 1
         XTLE = 0.D0
         VTLE = 0.D0
         IUER = -1
         CALL TLE_PARSER ( FIL_TLE(J1), EPH(J1), IUER )     
! ------
         TAI_BEG = UTC_BEG - UTC_MTAI
! ------
         IUER = -1
         CALL TLE_TO_CRS (EPH(J1), MJD_BEG, TAI_BEG, XCRS, VCRS, IUER)
!
! --- Ground Station
!
      STA_POS(1)  =  1130730.331D0   !GGAO12M
      STA_POS(2)  = -4831246.540D0   !GGAO12M
      STA_POS(3)  =  3994228.904D0   !GGAO12M
         IUER = -1
 
        CALL TLE_TO_TRS ( EPH, MJD_BEG, TAI_BEG, STA_POS, XTRS, VTRS,     &
     &                     AZ, EL, IUER )

      PRINT *, "%%%%%%%%%%%%%% TLE_EXAMPLE_TO_COO - 146 %%%%%%%%"
         PRINT *, "XTRS_NH:    ", XTRS
         PRINT *, "XTRS_JS:    ", XTRS_JS
         PRINT *, "XTRS(NH-JS) ", XTRS - XTRS_JS
         PRINT *, "--------------------------------------"
         PRINT *, "VTRS_NH: ", VTRS
         PRINT *, "VTRS_JS: ", VTRS_JS
         PRINT *, "VTRS(NH-JS) ", VTRS - VTRS_JS
         PRINT *, "--------------------------------------"
         PRINT *, "XTRS(IG-JS): ", XPY(1,1:3) - XTRS_JS
         PRINT *, "VTRS(IG-JS): ", XDOTPY(1,1:3) - VTRS_JS
         PRINT *, "--------------------------------------"
      PRINT *, "%%%%%%%%%%%%%% TLE_EXAMPLE_TO_COO - 158 %%%%%%%%"
         PRINT *, "XCRS_NH:    ", XCRS
         PRINT *, "XCRS_JS:    ", XCRS_JS
         PRINT *, "XCRS(NH-JS) ", XCRS - XCRS_JS
         PRINT *, "--------------------------------------"
         PRINT *, "VCRS_NH: ", VCRS
         PRINT *, "VCRS_JS: ", VCRS_JS
         PRINT *, "VCRS(NH-JS) ", VCRS - VCRS_JS
      PRINT *, "%%%%%%%%%%%%%% TLE_EXAMPLE_TO_COO - 158 %%%%%%%%"

!
! ------
!
         XDIF(J1, 1:3) = XTRS - XPY(J1,1:3)
         XDOTDIF(J1, 1:3) =  VTRS - XDOTPY(J1,1:3)

         WRITE ( 6, * ) '-------------------------------------------'
         WRITE ( 6, * ) J1, TRIM( FIL_TLE(J1) )
         WRITE ( 6, * ) 'XDIF:    ', XDIF(J1, 1:3)
         WRITE ( 6, * ) 'XDOTDIF: ', XDOTDIF(J1, 1:3)
         WRITE ( 6, * ) 'DIST_DIFF:  ', DSQRT(XDIF(J1,1)**2 + XDIF(J1,2)**2 + XDIF(J1,3)**2)
         WRITE ( 6, * ) 'SPEED_DIFF: ', DSQRT(XDOTDIF(J1,1)**2 + XDOTDIF(J1,2)**2 + XDOTDIF(J1,3)**2)
         WRITE ( 6, * ) '-------------------------------------------'

      END DO













      
      END PROGRAM TLE_EXAMPLE_TO_COO  !#!#
