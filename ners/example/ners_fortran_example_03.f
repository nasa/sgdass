      PROGRAM    NERS_EXAMPLE_03
! ************************************************************************
! *                                                                      *
! *   Test program NERS_FORTRAN_EXAMPLE_03 demonstrates how to compute   *
! *   the time series of 8 Earth orientation parameters with a specified *
! *   time step for the specified time range and print them as a table.  *
! *   NB: the units in the table are not SI units, but the non-standard  *
! *   units historically used in the past.                               *
! *                                                                      *
! * ## 10-MAY-2017 NERS_FORTRAN_EXAMPLE_03 v1.0 (c) L. Petrov 12-MAY-2017 ## *
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
      REAL*8     TIM_STEP
      INTEGER*4  M_PAR, M_SER
      PARAMETER  ( M_PAR =   8 ) 
      PARAMETER  ( M_SER = 256 ) 
      REAL*8     TIM(M_SER), EOPSER(M_SER,M_PAR)
      INTEGER*4  LPAR, NS, J1, IUER
!
! --- Set start and stop dates
!
      MJD_BEG = 57600 ; TAI_BEG = 0.0D0
      MJD_END = 57700 ; TAI_END = 0.0D0
!
      TIME_TAI_BEG = (MJD_BEG - J2000__MJD)*86400.0D0 +  TAI_BEG
      TIME_TAI_END = (MJD_END - J2000__MJD)*86400.0D0 +  TAI_END
!
! --- Time step of the series in seconds
!
      TIM_STEP     = 43200.0D0
!
! --- Initialize NERS. We use default NERS__CONFIG file created during installation
! --- NERS__CONFIG is defined in ners.i
!
      IUER = -1
      CALL NERS_INIT    ( NERS__CONFIG, NERS, TIME_TAI_BEG, TIME_TAI_BEG + TIM_STEP, IUER )
      IF  ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Get time series of Xpole, Ypole, UT1, X pole rate, Y pole rate, UT1 rate,
! --- nutation offset in longitude and nutation offset in obliquity with 
! --- in accordance to a model. NB: these parametdrs do not include empirical
! --- harmonic variations in the Earth rotation parameters with respect to the
! --- model
!
      IUER = -1
      CALL NERS_GET_SERIES ( NERS, TIME_TAI_BEG, TIME_TAI_END, TIM_STEP, &
     &                       'eops', M_PAR, M_SER, NS, TIM, EOPSER, IUER )
      IF  ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Printe the time series, NB: NERS_GET_SERIES returns EOP in non-standard units
!
      DO 410 J1=1,NS
         WRITE ( 6, 110 ) MJD_BEG + IDINT(TIM(J1)/86400.0), TIM(J1) - IDINT(TIM(J1)/86400.0)*86400.0D0, &
     &                    EOPSER(J1,1:8)
 110     FORMAT ( 'MJD: ', I5, ' TAI: ', F7.1, ' X_pole: ', F10.6, ' arcsec,'   &
     &                            ' Y_pole: ', F10.6, ' arcsec,'   &
     &                            ' UT1: ', F10.6, ' s,'           &
     &                            ' Xp_rate: ', F11.8,  ' arcsec/day,' &
     &                            ' Yp_rate: ', F11.8,  ' arcsec/day,' &
     &                            ' UT1_rate: ', F11.8, ' s/day,', & 
     &                            ' Dpsi: ', F10.6, ' arcsec,',    &
     &                            ' Deps: ', F10.6, ' arcsec'      )
 410  CONTINUE 
!
! --- Release memory allocated by NERS
! --- NERS__ALL is defined in ners.i
!
      CALL NERS_QUIT    ( NERS__ALL, NERS )
      END  PROGRAM    NERS_EXAMPLE_03  !#!#
