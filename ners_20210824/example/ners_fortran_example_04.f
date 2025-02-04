      PROGRAM    NERS_EXAMPLE_04
! ************************************************************************
! *                                                                      *
! *   Test program NERS_FORTRAN_EXAMPLE_04 demonstrates how to learn     *
! *   the range of dates for which the NERS provides the Earth           *
! *   orientation parameters. NERS routine NERS_INQ returns either two   *
! *   ranges:  1) the range of the EOP forecast which is based on        *
! *   measurements, also called the data assimilation range and          *
! *   2) the long-term prediction that is based in extrapolation, or     *
! *   the time epoch of the forecast generation. It is assumed that the  *
! *   EOP long-term prediction will be used only in a case of either     *
! *   NERS servers failure or a failure of the NERS client to establish  *
! *   Internet connection.                                               *
! *                                                                      *
! * ## 10-MAY-2017 NERS_FORTRAN_EXAMPLE_04 v1.0 (c) L. Petrov 12-MAY-2017 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'ners.i'        ! Include file with defintions of NERS data structure
      INCLUDE   'ners_local.i'  ! Include file with 
      INTEGER*4   J2000__MJD
      PARAMETER  (  J2000__MJD = 51544 ) ! 2000.01.01_00:00:00
      TYPE     ( NERS__TYPE ) :: NERS
      INTEGER*4  MJD_BEG, MJD_END, MJD_OBS
      REAL*8     TAI_BEG, TAI_END, TIME_TAI_BEG, TIME_TAI_END
      INTEGER*4  M_PAR
      PARAMETER  ( M_PAR =   3 ) 
      REAL*8     PARS(M_PAR)
      INTEGER*4  L_PAR, IUER
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
!
      IUER = -1
      CALL NERS_INIT    ( NERS__CONFIG, NERS, TIME_TAI_BEG, TIME_TAI_END, IUER )
      IF  ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Inquire the EOP ranges. When NERS_INQ is called with 'range' parameter,
! --- it returns three time epochs for two ranges
!
      IUER = -1
      CALL NERS_INQ ( NERS, 'range', M_PAR, L_PAR, PARS, IUER )
      IF  ( IUER .NE. 0 ) CALL EXIT ( 1 )
      WRITE ( 6, 110 ) PARS(1:2), PARS(2:3)
 110  FORMAT ( 'Data assimilation range:    ', F14.2, 2X, F14.2/ &
     &         'Long-term prediction range: ', F14.2, 2X, F14.2  )
!
! --- Inquire the time epoch of the forecast generation.
! --- When NERS_INQ is called with 'fcn_gen_time' parameter, it returns 
! --- the epoch of the forecast generation.
!
      IUER = -1
      CALL NERS_INQ ( NERS, 'fcs_gen_time', M_PAR, L_PAR, PARS, IUER )
      IF  ( IUER .NE. 0 ) CALL EXIT ( 1 )
      WRITE ( 6, 120 ) PARS(1)
 120  FORMAT ( 'Forecast generation time: ', F16.2 )
!
! --- Release memory allocated by NERS
! --- Constant NERS__ALL is defined in ners.i
!
      CALL NERS_QUIT    ( NERS__ALL, NERS )
      END  PROGRAM    NERS_EXAMPLE_04  !#!#
