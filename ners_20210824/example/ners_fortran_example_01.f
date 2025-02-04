      PROGRAM    NERS_FORTRAN_EXAMPLE_01
! ************************************************************************
! *                                                                      *
! *   Test program NERS_FORTRAN_EXAMPLE_01 demonstrates how to compute   *
! *   the Earth rotation matrix that transform a vector in the rotating  *
! *   terrestrial coordinate system fixed with respect the Earth's       *
! *   crust to the inertial non-rotating celestial coordinate system.    *
! *                                                                      *
! * ## 10-MAY-2017 NERS_FORTRAN_EXAMPLE_01 v1.0 (c) L. Petrov 12-MAY-2017 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'ners.i'        ! Include file with defintions of NERS data structure
      INCLUDE   'ners_local.i'  ! Include file with 
      INTEGER*4     J2000__MJD
      PARAMETER  (  J2000__MJD = 51544 ) ! Modified Julain date on 2000.01.01_00:00:00
      TYPE     ( NERS__TYPE ) :: NERS
      INTEGER*4  MJD_BEG, MJD_END, MJD_OBS
      REAL*8     TAI_BEG, TAI_END, TAI_OBS, TIME_TAI_BEG, TIME_TAI_END, TIME_TAI_OBS
      REAL*8     EARTH_ROT_MAT(3,3), TIME_TAI_START, TIME_TAI_STOP, TIM_TAI
      INTEGER*4  LPAR, IUER
!
! --- Set start and stop dates
!
      MJD_BEG = 56700 ; TAI_BEG = 12000.0D0
      MJD_END = 56702 ; TAI_END = 64200.0D0
      MJD_OBS = 56701 ; TAI_OBS = 28923.928426D0
!
      TIME_TAI_BEG = (MJD_BEG - J2000__MJD)*86400.0D0 +  TAI_BEG
      TIME_TAI_END = (MJD_END - J2000__MJD)*86400.0D0 +  TAI_END
      TIME_TAI_OBS = (MJD_OBS - J2000__MJD)*86400.0D0 +  TAI_OBS
!
! --- Initialize NERS. We use default NERS__CONFIG file created during installation
! --- NERS__CONFIG is defined in ners.i
!
      IUER = -1
      CALL NERS_INIT    ( NERS__CONFIG, NERS, TIME_TAI_BEG, TIME_TAI_END, IUER )
      IF  ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Get the 3x3 Earth Rotation Matrix EARTH_ROT_MAT
!
      IUER = -1
      CALL NERS_GET_EOP ( NERS, TIME_TAI_OBS, 'mat', 9,  LPAR, EARTH_ROT_MAT, IUER )
      IF  ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Print the matrix
!
      WRITE ( 6, 110 ) TIME_TAI_OBS, EARTH_ROT_MAT
 110  FORMAT ( 'Time_TAI_obs: ', 1PD19.12, ' Earth_ROT_MAT: '/ &
     &          3(0PF20.12,2X) )
!
! --- Release memory allocated by NERS
! --- NERS__ALL is defined in ners.i
!
      CALL NERS_QUIT    ( NERS__ALL, NERS )
      END  PROGRAM   NERS_FORTRAN_EXAMPLE_01  !#!#
