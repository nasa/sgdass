      PROGRAM    NERS_FORTRAN_EXAMPLE_02
! ************************************************************************
! *                                                                      *
! *   Test program NERS_FORTRAN_EXAMPLE_02 demonstrates how to compute   *
! *   TAI time that corresponds to UTC time tag. The UTC time tag        *
! *   is expressed as sum of days elapsed since 2000.01.01_00:00:00 UTC  *
! *   epoch multiplied by 86400.0 and the UTC time tag with respect to   *
! *   the midnight.                                                      *
! *                                                                      *
! * ## 10-MAY-2017 NERS_FORTRAN_EXAMPLE_02 v1.0 (c) L. Petrov 12-MAY-2017 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'ners.i'        ! Include file with defintions of NERS data structure
      INCLUDE   'ners_local.i'  ! Include file with 
      INTEGER*4   J2000__MJD
      PARAMETER  (  J2000__MJD = 51544 ) ! 2000.01.01_00:00:00
      TYPE     ( NERS__TYPE ) :: NERS
      INTEGER*4  MJD_BEG, MJD_END, MJD_OBS
      REAL*8     TAI_BEG, TAI_END, UTC_OBS, TIME_TAI_BEG, TIME_TAI_END, &
     &           TIME_UTC_OBS, TIME_TAI_OBS
      REAL*8     UTC_M_TAI
      INTEGER*4  LPAR, IUER
!
! --- Set start and stop dates
!
      MJD_BEG = 57700 ; TAI_BEG = 12000.0D0
      MJD_END = 57702 ; TAI_END = 64200.0D0
!
! --- Observation epoch MJD_OBS, UTC_OBS
!
      MJD_OBS = 57701 ; UTC_OBS = 28923.482534D0
!
! --- Tranform time and stop epochs as well as the obsevation to seconds
!
      TIME_TAI_BEG = (MJD_BEG - J2000__MJD)*86400.0D0 +  TAI_BEG
      TIME_TAI_END = (MJD_END - J2000__MJD)*86400.0D0 +  TAI_END
!
! --- NB: TIME_UTC_OBS is *not* interval of time elapsed since 2000.0 epoch!
!
      TIME_UTC_OBS = (MJD_OBS - J2000__MJD)*86400.0D0 +  UTC_OBS
!
! --- Initialize NERS. We use default NERS__CONFIG file created during installation
! --- NERS__CONFIG is defined in ners.i
!
      IUER = -1
      CALL NERS_INIT    ( NERS__CONFIG, NERS, TIME_TAI_BEG, TIME_TAI_END, IUER )
      IF  ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Get UTC_MTAI
!
      IUER = -1
      CALL NERS_GET_UTCMTAI ( NERS, TIME_UTC_OBS, UTC_M_TAI, IUER )
      IF  ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Compute time TAI that corresponds to UTC time tag TIME_UTC_OBS
!
      TIME_TAI_OBS = TIME_UTC_OBS - UTC_M_TAI
!
      WRITE ( 6, 110 ) TIME_UTC_OBS, UTC_M_TAI
 110  FORMAT ( 'Time_UTC_obs: ', F20.6, ' UTC_M_TAI: ', F5.1 )
!
! --- Release memory allocated by NERS
! --- NERS__ALL is defined in ners.i
!
      CALL NERS_QUIT    ( NERS__ALL, NERS )
      END  PROGRAM  NERS_FORTRAN_EXAMPLE_02  !#!#
