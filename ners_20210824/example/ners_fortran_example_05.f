      PROGRAM    NERS_EXAMPLE_05
! ************************************************************************
! *                                                                      *
! *   Test program NERS_FORTRAN_EXAMPLE_05 demonstrates how to compute   *
! *   azimuth and elevation at a given station observing a given source  *
! *   at the specified moment of time using NERS.                        *
! *                                                                      *
! * ## 15-JUN-2018 NERS_FORTRAN_EXAMPLE_05 v2.0 (c) L. Petrov 28-JUN-2018 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'ners.i'        ! Include file with defintions of NERS data structure
      INCLUDE   'ners_local.i'  ! Include file with 
      INTEGER*4   J2000__MJD
      PARAMETER  (  J2000__MJD = 51544 ) ! 2000.01.01_00:00:00
      TYPE     ( NERS__TYPE ) :: NERS
      REAL*8     PI__NUM, DEG__TO__RAD
      PARAMETER  ( PI__NUM = 3.141592653589793D0 )
      PARAMETER  ( DEG__TO__RAD  = PI__NUM/180.0D0 )
      INTEGER*4  MJD_BEG, MJD_END, MJD_OBS
      REAL*8     TAI_BEG, TAI_END, TIME_TAI_BEG, TIME_TAI_END, &
     &           TIM_TAI_OBS, TIM_TAI, COO_TRS(3), RA, DEC
      REAL*8     AZ, EL, HA, AZ_RATE, EL_RATE, HA_RATE
      INTEGER*4  IUER
!
! --- Set start and stop dates
!
      MJD_BEG = 57600 ; TAI_BEG = 0.0D0
      MJD_END = 57601 ; TAI_END = 0.0D0
!
      TIME_TAI_BEG = (MJD_BEG - J2000__MJD)*86400.0D0 +  TAI_BEG
      TIME_TAI_END = (MJD_END - J2000__MJD)*86400.0D0 +  TAI_END
!
! --- Initialize NERS. We use default NERS__CONFIG file created during installation
! --- NERS__CONFIG is defined in ners.i
! --- Alternative: to use string 'NERS_CONFIG' if environoment variable NERS_CONFIG
! --- has been defined.
!
      IUER = -1
      CALL NERS_INIT    ( NERS__CONFIG, NERS, TIME_TAI_BEG, TIME_TAI_END, IUER )
      IF  ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      IUER = -1
      CALL NERS_LOAD ( NERS, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4912, IUER, 'NERS_FORTRAN_EXAMPLE_05', 'Error in '// &
     &         'an attempt to retrieve NERS forecast parameters '// &
     &         'form the remote server' )
           CALL EXIT ( 1 )
      END IF
!
      MJD_OBS     = 57600
      TIM_TAI_OBS = 20000.0D0 
      TIM_TAI     = (MJD_OBS - J2000__MJD)*86400.0D0 + TIM_TAI_OBS
      COO_TRS(1)  =  1446375.114D0  ! HN-VLBA
      COO_TRS(2)  = -4447939.660D0
      COO_TRS(3)  =  4322306.122D0 
      RA          =  0.0871803605 ! 3C84
      DEC         =  0.724515773  ! 
!
      IUER = -1
      CALL NERS_AZELHA_COMP ( NERS, TIM_TAI, COO_TRS, RA, DEC, 'radio', &
     &                        AZ, EL, HA, AZ_RATE, EL_RATE, HA_RATE, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4913, IUER, 'NERS_AZEL', 'Error in '// &
     &         'computing azimuth and elevation' )
           CALL EXIT ( 1 ) 
      END IF
!
      WRITE ( 6, 210 ) AZ/DEG__TO__RAD, EL/DEG__TO__RAD, HA/DEG__TO__RAD, &
     &                 AZ_RATE, EL_RATE, HA_RATE
 210  FORMAT ( ' Azimuth: ', F13.9,' deg ', &
     &         ' Elevation: ',F14.9, ' deg', &
     &         ' Hour_angle: ',F14.9, ' deg'/ &
     &         ' Azimuth_rate: ', 1PD14.7, ' rad/s ', &
     &         ' Elevation_rate: ', 1PD14.7, ' rad/s', &
     &         ' Hour_angle: ', 1PD14.7, ' rad/s' )
!
! --- Release memory allocated by NERS
! --- Constant NERS__ALL is defined in ners.i
!
      CALL NERS_QUIT    ( NERS__ALL, NERS )
      END  PROGRAM    NERS_EXAMPLE_05  !#!#
