      FUNCTION   GPH_I2_TO_SI ( IVAL_I2 )
! ************************************************************************
! *                                                                      *
! *   Function GPH_I2_TO_SI converts geopotential height from internal   *
! *   netCDF units to meter.                                             *
! *                                                                      *
! *  ### 24-JUL-2002  GPH_I2_TO_SI  v1.0 (c)  L. Petrov  24-JUL-2002 ### *
! *                                                                      *
! ************************************************************************
      REAL*4     GPH_I2_TO_SI
      INTEGER*2  IVAL_I2
      GPH_I2_TO_SI = IVAL_I2 + 32066.0
      RETURN
      END  !#!  GPH_I2_TO_SI  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   GPH_SI_TO_I2 ( GPH_SI )
! ************************************************************************
! *                                                                      *
! *   Function GPH_SI_TO_I2  converts geopotential height from meters    *
! *   to internal netCDF units.                                          *
! *                                                                      *
! * ### 24-JUL-2002  GPH_SI_TO_I2  v1.0 (c)  L. Petrov  24-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
      INTEGER*2  GPH_SI_TO_I2
      REAL*4     GPH_SI
      GPH_SI_TO_I2 = ( GPH_SI - 32066.0 )
      RETURN
      END  !#!  GPH_I2_TO_SI  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   SPR_I2_TO_SI ( IVAL_I2 )
! ************************************************************************
! *                                                                      *
! *   Function SPR_I2_TO_SI converts surface pressure from internal      *
! *   netCDF units to Pascals.                                           *
! *                                                                      *
! *  ### 24-JUL-2002  SPR_I2_TO_SI  v1.0 (c)  L. Petrov  24-JUL-2002 ### *
! *                                                                      *
! ************************************************************************
      REAL*4     SPR_I2_TO_SI
      INTEGER*2  IVAL_I2
      SPR_I2_TO_SI = 367650.0 + IVAL_I2*10
      RETURN
      END  !#!  SPR_I2_TO_SI  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   SPR_SI_TO_I2 ( SPR_SI )
! ************************************************************************
! *                                                                      *
! *   Function SPR_SI_TO_I2  converts surface pressure from Pascals to   *
! *   internal netCDF units.                                             *
! *                                                                      *
! *  ### 24-JUL-2002  SPR_SI_TO_I2 v1.0 (c)  L. Petrov  24-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
      INTEGER*2  SPR_SI_TO_I2
      REAL*4     SPR_SI
      SPR_SI_TO_I2 = ( SPR_SI - 367650.0)/10
      RETURN
      END  !#!  SPR_SI_TO_I2  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HRS_TO_MJD ( HRS, MJD, SEC )
! ************************************************************************
! *                                                                      *
! *   Routine HRS_TO_MJD transforms date from the myhtological format    *
! *   used by meteorlogists to astronomical format: MJD and seconds.     *
! *                                                                      *
! *  ### 24-JUL-2002   HRS_TO_MJD  v1.1 (c)  L. Petrov  16-MAY-2004 ###  *
! *                                                                      *
! ************************************************************************
      REAL*8     HRS, SEC
      INTEGER*4  MJD
      REAL*8       HRS__2000
      PARAMETER  ( HRS__2000 = 17522904.0D0 )
      INTEGER*4    MJD__2000
      PARAMETER  ( MJD__2000 = 51544 )
      INTEGER*4  IDAY
!
      IDAY =   ( HRS - HRS__2000  )/24.0D0
      SEC  = ( ( HRS - HRS__2000  )  - 24*IDAY )*3600.0D0
      MJD = IDAY + MJD__2000
!
      RETURN
      END  !#!  HRS_TO_MJD  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   MJDSEC_TO_HRS ( MJD, SEC )
! ************************************************************************
! *                                                                      *
! *   Function MJDSEC_TO_HRS  converts the date in astronomical format:  *
! *   MJD, SEC, to mythological format used by meteorologists.           *
! *                                                                      *
! *  ### 26-JUL-2002  MJDSEC_TO_HRS  v1.1 (c) L. Petrov 16-MAY-2004 ###  *
! *                                                                      *
! ************************************************************************
      REAL*8       MJDSEC_TO_HRS, SEC
      INTEGER*4    MJD
      REAL*8       HRS__2000
      PARAMETER  ( HRS__2000 = 17522904.0D0 )
      INTEGER*4    MJD__2000
      PARAMETER  ( MJD__2000 = 51544 )
!
      MJDSEC_TO_HRS = ( MJD - MJD__2000 )*24.0D0 + HRS__2000 + SEC/3600.0D0
!
      RETURN
      END  !#!  MJDSEC_TO_HRS  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GRIB_TO_MJD ( GRIB_ARR, MJD, SEC )
! ************************************************************************
! *                                                                      *
! *   Routine GRIB_TO_MJD transforms the date from GRIB representation   *
! *   to astronomical format: MJD, SEC.                                  *
! *                                                                      *
! *  ### 31-JUL-2002  GRIB_TO_MJD  v1.0 (c)  L. Petrov  01-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  GRIB_ARR(4)
      INTEGER*4  MJD
      CHARACTER  DATE_STR*23
      REAL*8     SEC
!
      WRITE ( UNIT=DATE_STR, FMT='(I4,".",I2,".",I2,":",I2,":00:00.000")' ) &
     &        GRIB_ARR(1), GRIB_ARR(2), GRIB_ARR(3), GRIB_ARR(4)/100
      CALL BLANK_TO_ZERO ( DATE_STR )
      CALL DATE_TO_TIME  ( DATE_STR, MJD, SEC, -1 )
!
      RETURN
      END  !#!  GRIB_TO_MJD  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   SHUM_I2_TO_SI ( IVAL_I2 )
! ************************************************************************
! *                                                                      *
! *   Function SHUM_I2_TO_SI converts specific humidity from netCDF      *
! *   internal units to dimensionless quantity (kg/kg).                  *
! *                                                                      *
! *  ### 09-AUG-2002  SHUM_I2_TO_SI  v1.0 (c)  L. Petrov 09-AUG-2002 ### *
! *                                                                      *
! ************************************************************************
      REAL*4     SHUM_I2_TO_SI
      INTEGER*2  IVAL_I2
      SHUM_I2_TO_SI = 0.032666 + IVAL_I2*1.E-6
      RETURN
      END  !#!  SHUM_I2_TO_SI  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   TEMP_I2_TO_SI ( IVAL_I2 )
! ************************************************************************
! *                                                                      *
! *   Function TEMP_I2_TO_SI converts air temperature from netCDF        *
! *   internal units to degrees Kelvin.                                  *
! *                                                                      *
! *  ### 09-AUG-2002  TEMP_I2_TO_SI  v1.0 (c)  L. Petrov 09-AUG-2002 ### *
! *                                                                      *
! ************************************************************************
      REAL*4     TEMP_I2_TO_SI
      INTEGER*2  IVAL_I2
      TEMP_I2_TO_SI = 477.66 + IVAL_I2*0.01
      RETURN
      END  !#!  TEMP_I2_TO_SI  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION GPH_TO_HEIGHT ( PHI_GCN, GPH )
! ************************************************************************
! *                                                                      *
! *   Function GPH_TO_HEIGHT transforms geopotential height to           *
! *   the height above the reference ellipsoid.                          *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * PHI_GCN ( REAL*8    ) -- geocentric latitude, positive towards north *
! *                          in rad, in range [-pi/2,pi/2]               *
! *     GPH ( REAL*8    ) -- geopotential height in meters.              *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * <GPH_TO_HEIGHT> ( REAL*8    ) -- height above the reference          *
! *                                  ellipsoid, negative towards the     *
! *                                  geocenter, in meters.               *
! *                                                                      *
! * ### 12-AUG-2002  GPH_TO_HEIGHT  v1.1 (c) L. Petrov  16-MAY-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*8   GPH_TO_HEIGHT, GPH, PHI_GCN
      REAL*8   REA, FE, GM, GL, OM
      PARAMETER ( REA = 6378136.3D0     ) ! Equatorial Earth's radius
      PARAMETER (  FE = 1.D0/298.257D0  ) ! Earth's flattening
      PARAMETER (  GM = 3.986004418D14  ) ! Earth's gravity constant
      PARAMETER (  GL = 0.001931663D0   ) ! Somigliana constant
      PARAMETER (  OM = 7.2921151467D-5 ) ! Earth's nominal rotation rate
      REAL*8     EE, MR, RF
!
      EE  = DSQRT ( 2.D0*FE - FE**2 ) ! Earth's figure eccentricity
      MR  = OM**2*REA**3/GM*(1.D0-FE)
      RF  = REA/(1.D0 + FE + MR - 2.D0*FE*DSIN(PHI_GCN)**2 )
!
      GPH_TO_HEIGHT = (1.D0 + GL*DSIN(PHI_GCN)**2)/ &
     &                DSQRT(1.D0 - EE*DSIN(PHI_GCN)**2)/ &
     &                ( (1.D0 + GL/2.D0)/DSQRT(1.D0 - EE/2.D0) )* &
     &                (RF*GPH)/(RF-GPH)
      RETURN
      END  !#!  GPH_TO_HEIGHT  #!#
