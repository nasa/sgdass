      PROGRAM TLE_AZEL_COMP
      IMPLICIT   NONE
      INCLUDE    'tle_eph.i'
      REAL*8     STA_X(3), ENU(3), SAT_POS(3), SAT_VEL(3)
      INTEGER*4  MJD, IUER, IPAR
      REAL*8     AE, FE, UTC, AZ, EL, DISP
      REAL*8     STA_GCN, STA_GDT, STA_LON, STA_H, STA_RD, STA_GRAV
      REAL*8     SAT_GCN, SAT_GDT, SAT_LON, SAT_H, SAT_RD, SAT_GRAV
      CHARACTER  FIL_TLE*128, DATE_BEG*21
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
! --- TLE File
!
      FIL_TLE = '/f1/home/nhabana/data/rfi/STARLINK-3795_2022_04_21.tle'
!      FIL_TLE = '/f1/home/nhabana/data/rfi/NAVSTAR81_2021_06_17.tle'
!      FIL_TLE = '/f1/home/nhabana/data/rfi/gps.tle'

!
! --- Computation time
!
      DATE_BEG = '2022.05.02_13:00:00.0'
      CALL DATE_TO_TIME ( DATE_BEG, MJD, UTC, IUER )
!
! --- Define the Flattening and semi-major axis (WGS72)
!
      AE  = 6378135.D0
      FE  = 1.D0/298.26D0
!
! --- Position of the GGAO12M antenna
!
      STA_X(1) =  1130730.245D0
      STA_X(2) = -4831245.953D0
      STA_X(3) =  3994228.228D0
!
! --- Position of satellite at time of interest
!
      CALL TLE_DRIVER ( FIL_TLE, MJD, UTC, SAT_POS, SAT_VEL, IUER )
!
! --- Convert the station coordinates to geodic lat/lon/h 
!
      IPAR = 0
      CALL REF_ELL_B ( IPAR, STA_X, AE, FE, STA_GCN, STA_GDT,           &
     &                 STA_LON, STA_H, STA_RD, STA_GRAV )
!
! --- Convert the satellite coordinates to geodic lat/lon/h 
!
      IPAR = 0
      CALL REF_ELL_B ( IPAR, SAT_POS, AE, FE, SAT_GCN, SAT_GDT,         &
     &                 SAT_LON, SAT_H, SAT_RD, SAT_GRAV )
!
! --- Compute the local East-North-Up coordinates of the station and 
!     satellite
!
      CALL GEOD_2_ENU( STA_LON, STA_GDT, STA_H, SAT_LON, SAT_GDT,       &
     &                 SAT_H, AE, FE, ENU )
!
! --- Compute the azimuth and elevation
!
      CALL ENU_2_AVD ( ENU, AZ, EL, DISP )

!
! --- Print output
!
      WRITE(6,*) 'DATE: ', DATE_BEG 
      WRITE(6,*) 'STA NAME:  GGAO '
      WRITE(6,*) 'STA COORD: ', STA_X
      WRITE(6,*) 'SAT NAME: '
      WRITE(6,*) 'SAT COORD: ', SAT_POS
      WRITE(6,*) 'SAT SPEED: ', SAT_VEL
      WRITE(6,*) "AZ-EL-Dist: ", AZ/DEG__TO__RAD, EL/DEG__TO__RAD, DISP


      END PROGRAM
