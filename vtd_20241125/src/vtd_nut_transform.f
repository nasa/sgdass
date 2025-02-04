      SUBROUTINE DPSI_DEPSILON_TO_XY ( PREC_MODE, MJD, TAI, DPSI, DEPS, &
     &                                 DX, DY, IUER  )
! ************************************************************************
! *                                                                      *
! *   Routine DPSI_DEPSILON_TO_XY transforms adjustments of nutation     *
! *   agles in the Newcomb-Andoyer form (\Delta\psi, \Delta\epsilon) to  *
! *   the Guinot-Capitaine (dX, dY) according to the precession code     *
! *   PREC_MODE.                                                         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  PREC_CODE ( INTEGER*4 ) -- Code of the precession expansion.        *
! *                             Supported codes:                         *
! *                             PREC__LIESKE1977                         *
! *                             PREC__IERS1996                           *
! *                             PREC__CAPITAINE2003                      *
! *                             PREC__CAPITAINE2005                      *
! *   MJD ( INTEGER*4 ) -- Modified Julian data on the midnight.         *
! *                        Units: days.                                  *
! *   TAI ( REAL*8    ) -- Moment of time. Units: sec.                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  DPSI ( REAL*8     ) -- Nutation in longitude. Unit: rad.            *
! *  DEPS ( REAL*8     ) -- Nutation in obliquity. Unit: rad.            *                                                                    *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  DX ( REAL*8     ) -- X Celestial Intermediate Pole coordinate.      *
! *                       Unit: rad.                                     *
! *  DY ( REAL*8     ) -- Y Celestial Intermediate Pole coordinate.      *
! *                       Unit: rad.                                     *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *                                                                      *
! * ## 21-FEB-2020 DPSI_DEPSILON_TO_XY v1.1 (c) L. Petrov 04-MAR-2020 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      INTEGER*4  PREC_MODE, MJD, IUER
      REAL*8     TAI, DPSI, DEPS, DX, DY 
      CHARACTER  STR*32, STR1*32
!
      REAL*8     TDB, TARG_TDB, EPS_0, EPS_A, PSI_A, CHI_A
      REAL*8     EPSA__LIESKE1977(0:3), EPSA__CAPITAINE2003(0:5), EPSA__CAPITAINE2005(0:5), EPSA__ARG(0:5)
      REAL*8     PSIA__LIESKE1977(0:3), PSIA__CAPITAINE2003(0:5), PSIA__CAPITAINE2005(0:5), PSIA__ARG(0:5)
      REAL*8     CHIA__LIESKE1977(0:3), CHIA__CAPITAINE2003(0:5), CHIA__CAPITAINE2005(0:5), CHIA__ARG(0:5)
!
      DATA       EPSA__LIESKE1977, PSIA__LIESKE1977, CHIA__LIESKE1977 &
     &     /                                                          &
     &          84381.448D0,  -46.81500D0, -0.00059D0,  0.001813D0,   & ! Eps_A
     &              0.0D0,   5038.47875D0, -1.07259D0, -0.001147D0,   & ! Psi_A
     &              0.0D0,     10.5526D0,  -2.38064D0, -0.001125D0    & ! Chi_A
     &     /
!
      DATA       EPSA__CAPITAINE2003, PSIA__CAPITAINE2003, CHIA__CAPITAINE2003 &
     &     /                                                                   &
     &        84381.448D0,  -46.84024D0,  -0.00059D0,    0.001813D0,    0.0D0,          0.0D0,          & ! Eps_A
     &              0.0D0, 5038.47875D0,  -1.07259D0,   -0.001147D0,    0.0D0,          0.0D0,          & ! Psi_A
     &              0.0D0,   10.5526D0,   -2.38064D0,   -0.001125D0,    0.0D0,          0.0D0           & ! Chi_A
     &     /
!
      DATA       EPSA__CAPITAINE2005, PSIA__CAPITAINE2005, CHIA__CAPITAINE2005 &
     &     /                                                                   &
     &        84381.406D0,  -46.836769D0, -0.0001831D0,  0.00200340D0, -0.000000576D0, -0.0000000434D0, & ! Eps_A
     &              0.0D0, 5038.481507D0, -1.0790069D0, -0.00114045D0,  0.000132851D0, -0.0000000951D0, & ! Psi_A
     &              0.0D0,   10.556403D0, -2.3814292D0, -0.00121193D0,  0.000170663D0, -0.0000000560D0  & ! Chi_A
     &     /
!
      IF ( PREC_MODE == PREC__LIESKE1977  .OR. PREC_MODE == PREC__IERS1996 ) THEN
           EPSA__ARG(0) = EPSA__LIESKE1977(0)*ARCSEC__TO__RAD
           EPSA__ARG(1) = EPSA__LIESKE1977(1)*ARCSEC__TO__RAD/CENT__TO__SEC
           EPSA__ARG(2) = EPSA__LIESKE1977(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
           EPSA__ARG(3) = EPSA__LIESKE1977(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
           EPSA__ARG(4) = 0.0D0
           EPSA__ARG(5) = 0.0D0
!
           PSIA__ARG(0) = PSIA__LIESKE1977(0)*ARCSEC__TO__RAD
           PSIA__ARG(1) = PSIA__LIESKE1977(1)*ARCSEC__TO__RAD/CENT__TO__SEC
           PSIA__ARG(2) = PSIA__LIESKE1977(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
           PSIA__ARG(3) = PSIA__LIESKE1977(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
           PSIA__ARG(4) = 0.0D0
           PSIA__ARG(5) = 0.0D0
!
           CHIA__ARG(0) = CHIA__LIESKE1977(0)*ARCSEC__TO__RAD
           CHIA__ARG(1) = CHIA__LIESKE1977(1)*ARCSEC__TO__RAD/CENT__TO__SEC
           CHIA__ARG(2) = CHIA__LIESKE1977(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
           CHIA__ARG(3) = CHIA__LIESKE1977(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
           CHIA__ARG(4) = 0.0D0
           CHIA__ARG(5) = 0.0D0
         ELSE IF ( PREC_MODE == PREC__CAPITAINE2003 ) THEN
           EPSA__ARG(0) = EPSA__CAPITAINE2003(0)*ARCSEC__TO__RAD
           EPSA__ARG(1) = EPSA__CAPITAINE2003(1)*ARCSEC__TO__RAD/CENT__TO__SEC
           EPSA__ARG(2) = EPSA__CAPITAINE2003(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
           EPSA__ARG(3) = EPSA__CAPITAINE2003(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
           EPSA__ARG(4) = EPSA__CAPITAINE2003(4)*ARCSEC__TO__RAD/CENT__TO__SEC**4
           EPSA__ARG(5) = EPSA__CAPITAINE2003(5)*ARCSEC__TO__RAD/CENT__TO__SEC**5
!
           PSIA__ARG(0) = PSIA__CAPITAINE2003(0)*ARCSEC__TO__RAD
           PSIA__ARG(1) = PSIA__CAPITAINE2003(1)*ARCSEC__TO__RAD/CENT__TO__SEC
           PSIA__ARG(2) = PSIA__CAPITAINE2003(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
           PSIA__ARG(3) = PSIA__CAPITAINE2003(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
           PSIA__ARG(4) = PSIA__CAPITAINE2003(4)*ARCSEC__TO__RAD/CENT__TO__SEC**4
           PSIA__ARG(5) = PSIA__CAPITAINE2003(5)*ARCSEC__TO__RAD/CENT__TO__SEC**5
!
           CHIA__ARG(0) = CHIA__CAPITAINE2003(0)*ARCSEC__TO__RAD
           CHIA__ARG(1) = CHIA__CAPITAINE2003(1)*ARCSEC__TO__RAD/CENT__TO__SEC
           CHIA__ARG(2) = CHIA__CAPITAINE2003(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
           CHIA__ARG(3) = CHIA__CAPITAINE2003(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
           CHIA__ARG(4) = CHIA__CAPITAINE2003(4)*ARCSEC__TO__RAD/CENT__TO__SEC**4
           CHIA__ARG(5) = CHIA__CAPITAINE2003(5)*ARCSEC__TO__RAD/CENT__TO__SEC**5
         ELSE IF ( PREC_MODE == PREC__CAPITAINE2005 ) THEN
           EPSA__ARG(0) = EPSA__CAPITAINE2005(0)*ARCSEC__TO__RAD
           EPSA__ARG(1) = EPSA__CAPITAINE2003(1)*ARCSEC__TO__RAD/CENT__TO__SEC
           EPSA__ARG(2) = EPSA__CAPITAINE2005(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
           EPSA__ARG(3) = EPSA__CAPITAINE2005(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
           EPSA__ARG(4) = EPSA__CAPITAINE2005(4)*ARCSEC__TO__RAD/CENT__TO__SEC**4
           EPSA__ARG(5) = EPSA__CAPITAINE2005(5)*ARCSEC__TO__RAD/CENT__TO__SEC**5
!
           PSIA__ARG(0) = PSIA__CAPITAINE2005(0)*ARCSEC__TO__RAD
           PSIA__ARG(1) = PSIA__CAPITAINE2005(1)*ARCSEC__TO__RAD/CENT__TO__SEC
           PSIA__ARG(2) = PSIA__CAPITAINE2005(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
           PSIA__ARG(3) = PSIA__CAPITAINE2005(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
           PSIA__ARG(4) = PSIA__CAPITAINE2005(4)*ARCSEC__TO__RAD/CENT__TO__SEC**4
           PSIA__ARG(5) = PSIA__CAPITAINE2005(5)*ARCSEC__TO__RAD/CENT__TO__SEC**5
!
           CHIA__ARG(0) = CHIA__CAPITAINE2005(0)*ARCSEC__TO__RAD
           CHIA__ARG(1) = CHIA__CAPITAINE2005(1)*ARCSEC__TO__RAD/CENT__TO__SEC
           CHIA__ARG(2) = CHIA__CAPITAINE2005(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
           CHIA__ARG(3) = CHIA__CAPITAINE2005(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
           CHIA__ARG(4) = CHIA__CAPITAINE2005(4)*ARCSEC__TO__RAD/CENT__TO__SEC**4
           CHIA__ARG(5) = CHIA__CAPITAINE2005(5)*ARCSEC__TO__RAD/CENT__TO__SEC**5
         ELSE 
           CALL CLRCH ( STR )
           CALL INCH  ( PREC_MODE, STR )
           CALL ERR_LOG ( 2481, IUER, 'DPSI_DEPSILON_TO_XY', 'Unpsupported '// &
     &         'precession code '//STR )
           RETURN 
      END IF
!
      IF ( MJD < VTD__MJD_MIN ) THEN
           CALL CLRCH ( STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( MJD, STR  )
           CALL INCH  ( VTD__MJD_MIN, STR1 )
           CALL ERR_LOG ( 2482, IUER, 'DPSI_DEPSILON_TO_XY', 'Wrong parameter MJD: '// &
     &          TRIM(STR)//' it should be no less than VTD__MJD_MIN: '//STR1 )
           RETURN 
      END IF
!
      IF ( MJD > VTD__MJD_MAX ) THEN
           CALL CLRCH ( STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( MJD, STR  )
           CALL INCH  ( VTD__MJD_MAX, STR1 )
           CALL ERR_LOG ( 2483, IUER, 'DPSI_DEPSILON_TO_XY', 'Wrong parameter MJD: '// &
     &          TRIM(STR)//' it should be no more than VTD__MJD_MAX: '//STR1 )
           RETURN 
      END IF
      IF ( TAI < -1.5D0*86400.0D0 .OR. TAI > 1.5D0*86400.0D0 ) THEN
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR, FMT='(1PD15.7)' ) TAI
           CALL ERR_LOG ( 2484, IUER, 'DPSI_DEPSILON_TO_XY', 'Wrong parameter TAI: '// &
     &          TRIM(STR)//' it should be no more than -+ 86400.0 seconds' )
           RETURN 
      END IF
      CALL TAI_TO_TDB  ( MJD, TAI, TDB )
      TARG_TDB = (MJD - J2000__MJD - 0.5D0)*86400.0D0 + TDB
!
      EPS_A  = EPSA__ARG(0)             + &
     &         EPSA__ARG(1)*TARG_TDB    + &
     &         EPSA__ARG(2)*TARG_TDB**2 + &
     &         EPSA__ARG(3)*TARG_TDB**3 + &
     &         EPSA__ARG(4)*TARG_TDB**4 + &
     &         EPSA__ARG(5)*TARG_TDB**5
!
      PSI_A  = PSIA__ARG(0)             + &
     &         PSIA__ARG(1)*TARG_TDB    + &
     &         PSIA__ARG(2)*TARG_TDB**2 + &
     &         PSIA__ARG(3)*TARG_TDB**3 + &
     &         PSIA__ARG(4)*TARG_TDB**4 + &
     &         PSIA__ARG(5)*TARG_TDB**5
!
      CHI_A  = CHIA__ARG(0)             + &
     &         CHIA__ARG(1)*TARG_TDB    + &
     &         CHIA__ARG(2)*TARG_TDB**2 + &
     &         CHIA__ARG(3)*TARG_TDB**3 + &
     &         CHIA__ARG(4)*TARG_TDB**4 + &
     &         CHIA__ARG(5)*TARG_TDB**5
      EPS_0 =  EPSA__ARG(0)
!
      DX = DPSI*DSIN(EPS_A) + (PSI_A*DCOS(EPS_0) - CHI_A)*DEPS
      DY = DEPS             - (PSI_A*DCOS(EPS_0) - CHI_A)*DPSI*DSIN(EPS_A) 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  DPSI_DEPSILON_TO_XY  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE XY_TO_DPSI_DEPSILON ( PREC_MODE, MJD, TAI, DX, DY, &
     &                                 DPSI, DEPS, IUER  )
! ************************************************************************
! *                                                                      *
! *   Routine XY_TO_DPSI_DEPSILON transforms adjustments of nutation     *
! *   agles in the Newcomb-Andojeyr form (\Delta\psi, \Delta\epsilon) to *
! *   the Guinot-Capitaine (dX, dY) according to the precession code     *
! *   PREC_MODE.                                                         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  PREC_CODE ( INTEGER*4 ) -- Code of the precession expansion.        *
! *                             Supported codes:                         *
! *                             PREC__LIESKE1977                         *
! *                             PREC__IERS1996                           *
! *                             PREC__CAPITAINE2003                      *
! *                             PREC__CAPITAINE2005                      *
! *   MJD ( INTEGER*4 ) -- Modified Julian data on the midnight.         *
! *                        Units: days.                                  *
! *   TAI ( REAL*8    ) -- Moment of time. Units: sec.                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  DX ( REAL*8     ) -- X Celestial Intermediate Pole coordinate.      *
! *                       Unit: rad.                                     *
! *  DY ( REAL*8     ) -- Y Celestial Intermediate Pole coordinate.      *
! *                       Unit: rad.                                     *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  DPSI ( REAL*8     ) -- Nutation in longitude. Unit: rad.            *
! *  DEPS ( REAL*8     ) -- Nutation in obliquity. Unit: rad.            *                                                                    *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ## 21-FEB-2020 XY_TO_DPSI_DEPSILON v1.1 (c) L. Petrov 04-MAR-2020 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      INTEGER*4  PREC_MODE, MJD, IUER
      REAL*8     TAI, DPSI, DEPS, DX, DY 
      CHARACTER  STR*32, STR1*32
!
      REAL*8     TDB, TARG_TDB, EPS_0, EPS_A, PSI_A, CHI_A
      REAL*8     EPSA__LIESKE1977(0:3), EPSA__CAPITAINE2003(0:5), EPSA__CAPITAINE2005(0:5), EPSA__ARG(0:5)
      REAL*8     PSIA__LIESKE1977(0:3), PSIA__CAPITAINE2003(0:5), PSIA__CAPITAINE2005(0:5), PSIA__ARG(0:5)
      REAL*8     CHIA__LIESKE1977(0:3), CHIA__CAPITAINE2003(0:5), CHIA__CAPITAINE2005(0:5), CHIA__ARG(0:5)
!
      DATA       EPSA__LIESKE1977, PSIA__LIESKE1977, CHIA__LIESKE1977 &
     &     /                                                          &
     &          84381.448D0,  -46.81500D0, -0.00059D0,  0.001813D0,   & ! Eps_A
     &              0.0D0,   5038.47875D0, -1.07259D0, -0.001147D0,   & ! Psi_A
     &              0.0D0,     10.5526D0,  -2.38064D0, -0.001125D0    & ! Chi_A
     &     /
!
      DATA       EPSA__CAPITAINE2003, PSIA__CAPITAINE2003, CHIA__CAPITAINE2003 &
     &     /                                                                   &
     &        84381.448D0,  -46.84024D0,  -0.00059D0,    0.001813D0,    0.0D0,          0.0D0,          & ! Eps_A
     &              0.0D0, 5038.47875D0,  -1.07259D0,   -0.001147D0,    0.0D0,          0.0D0,          & ! Psi_A
     &              0.0D0,   10.5526D0,   -2.38064D0,   -0.001125D0,    0.0D0,          0.0D0           & ! Chi_A
     &     /
!
      DATA       EPSA__CAPITAINE2005, PSIA__CAPITAINE2005, CHIA__CAPITAINE2005 &
     &     /                                                                   &
     &        84381.406D0,  -46.836769D0, -0.0001831D0,  0.00200340D0, -0.000000576D0, -0.0000000434D0, & ! Eps_A
     &              0.0D0, 5038.481507D0, -1.0790069D0, -0.00114045D0,  0.000132851D0, -0.0000000951D0, & ! Psi_A
     &              0.0D0,   10.556403D0, -2.3814292D0, -0.00121193D0,  0.000170663D0, -0.0000000560D0  & ! Chi_A
     &     /
!
      IF ( PREC_MODE == PREC__LIESKE1977  .OR. PREC_MODE == PREC__IERS1996 ) THEN
           EPSA__ARG(0) = EPSA__LIESKE1977(0)*ARCSEC__TO__RAD
           EPSA__ARG(1) = EPSA__LIESKE1977(1)*ARCSEC__TO__RAD/CENT__TO__SEC
           EPSA__ARG(2) = EPSA__LIESKE1977(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
           EPSA__ARG(3) = EPSA__LIESKE1977(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
           EPSA__ARG(4) = 0.0D0
           EPSA__ARG(5) = 0.0D0
!
           PSIA__ARG(0) = PSIA__LIESKE1977(0)*ARCSEC__TO__RAD
           PSIA__ARG(1) = PSIA__LIESKE1977(1)*ARCSEC__TO__RAD/CENT__TO__SEC
           PSIA__ARG(2) = PSIA__LIESKE1977(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
           PSIA__ARG(3) = PSIA__LIESKE1977(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
           PSIA__ARG(4) = 0.0D0
           PSIA__ARG(5) = 0.0D0
!
           CHIA__ARG(0) = CHIA__LIESKE1977(0)*ARCSEC__TO__RAD
           CHIA__ARG(1) = CHIA__LIESKE1977(1)*ARCSEC__TO__RAD/CENT__TO__SEC
           CHIA__ARG(2) = CHIA__LIESKE1977(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
           CHIA__ARG(3) = CHIA__LIESKE1977(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
           CHIA__ARG(4) = 0.0D0
           CHIA__ARG(5) = 0.0D0
         ELSE IF ( PREC_MODE == PREC__CAPITAINE2003 ) THEN
           EPSA__ARG(0) = EPSA__CAPITAINE2003(0)*ARCSEC__TO__RAD
           EPSA__ARG(1) = EPSA__CAPITAINE2003(1)*ARCSEC__TO__RAD/CENT__TO__SEC
           EPSA__ARG(2) = EPSA__CAPITAINE2003(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
           EPSA__ARG(3) = EPSA__CAPITAINE2003(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
           EPSA__ARG(4) = EPSA__CAPITAINE2003(4)*ARCSEC__TO__RAD/CENT__TO__SEC**4
           EPSA__ARG(5) = EPSA__CAPITAINE2003(5)*ARCSEC__TO__RAD/CENT__TO__SEC**5
!
           PSIA__ARG(0) = PSIA__CAPITAINE2003(0)*ARCSEC__TO__RAD
           PSIA__ARG(1) = PSIA__CAPITAINE2003(1)*ARCSEC__TO__RAD/CENT__TO__SEC
           PSIA__ARG(2) = PSIA__CAPITAINE2003(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
           PSIA__ARG(3) = PSIA__CAPITAINE2003(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
           PSIA__ARG(4) = PSIA__CAPITAINE2003(4)*ARCSEC__TO__RAD/CENT__TO__SEC**4
           PSIA__ARG(5) = PSIA__CAPITAINE2003(5)*ARCSEC__TO__RAD/CENT__TO__SEC**5
!
           CHIA__ARG(0) = CHIA__CAPITAINE2003(0)*ARCSEC__TO__RAD
           CHIA__ARG(1) = CHIA__CAPITAINE2003(1)*ARCSEC__TO__RAD/CENT__TO__SEC
           CHIA__ARG(2) = CHIA__CAPITAINE2003(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
           CHIA__ARG(3) = CHIA__CAPITAINE2003(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
           CHIA__ARG(4) = CHIA__CAPITAINE2003(4)*ARCSEC__TO__RAD/CENT__TO__SEC**4
           CHIA__ARG(5) = CHIA__CAPITAINE2003(5)*ARCSEC__TO__RAD/CENT__TO__SEC**5
         ELSE IF ( PREC_MODE == PREC__CAPITAINE2005 ) THEN
           EPSA__ARG(0) = EPSA__CAPITAINE2005(0)*ARCSEC__TO__RAD
           EPSA__ARG(1) = EPSA__CAPITAINE2003(1)*ARCSEC__TO__RAD/CENT__TO__SEC
           EPSA__ARG(2) = EPSA__CAPITAINE2005(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
           EPSA__ARG(3) = EPSA__CAPITAINE2005(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
           EPSA__ARG(4) = EPSA__CAPITAINE2005(4)*ARCSEC__TO__RAD/CENT__TO__SEC**4
           EPSA__ARG(5) = EPSA__CAPITAINE2005(5)*ARCSEC__TO__RAD/CENT__TO__SEC**5
!
           PSIA__ARG(0) = PSIA__CAPITAINE2005(0)*ARCSEC__TO__RAD
           PSIA__ARG(1) = PSIA__CAPITAINE2005(1)*ARCSEC__TO__RAD/CENT__TO__SEC
           PSIA__ARG(2) = PSIA__CAPITAINE2005(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
           PSIA__ARG(3) = PSIA__CAPITAINE2005(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
           PSIA__ARG(4) = PSIA__CAPITAINE2005(4)*ARCSEC__TO__RAD/CENT__TO__SEC**4
           PSIA__ARG(5) = PSIA__CAPITAINE2005(5)*ARCSEC__TO__RAD/CENT__TO__SEC**5
!
           CHIA__ARG(0) = CHIA__CAPITAINE2005(0)*ARCSEC__TO__RAD
           CHIA__ARG(1) = CHIA__CAPITAINE2005(1)*ARCSEC__TO__RAD/CENT__TO__SEC
           CHIA__ARG(2) = CHIA__CAPITAINE2005(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
           CHIA__ARG(3) = CHIA__CAPITAINE2005(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
           CHIA__ARG(4) = CHIA__CAPITAINE2005(4)*ARCSEC__TO__RAD/CENT__TO__SEC**4
           CHIA__ARG(5) = CHIA__CAPITAINE2005(5)*ARCSEC__TO__RAD/CENT__TO__SEC**5
         ELSE 
           CALL CLRCH ( STR )
           CALL INCH  ( PREC_MODE, STR )
           CALL ERR_LOG ( 2491, IUER, 'XY_TO_DPSI_DEPSILON', 'Unpsupported '// &
     &         'precession code '//STR )
           RETURN 
      END IF
!
      IF ( MJD < VTD__MJD_MIN ) THEN
           CALL CLRCH ( STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( MJD, STR  )
           CALL INCH  ( VTD__MJD_MIN, STR1 )
           CALL ERR_LOG ( 2492, IUER, 'XY_TO_DPSI_DEPSILON', 'Wrong parameter MJD: '// &
     &          TRIM(STR)//' it should be no less than VTD__MJD_MIN: '//STR1 )
           RETURN 
      END IF
!
      IF ( MJD > VTD__MJD_MAX ) THEN
           CALL CLRCH ( STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( MJD, STR  )
           CALL INCH  ( VTD__MJD_MAX, STR1 )
           CALL ERR_LOG ( 2493, IUER, 'XY_TO_DPSI_DEPSILON', 'Wrong parameter MJD: '// &
     &          TRIM(STR)//' it should be no more than VTD__MJD_MAX: '//STR1 )
           RETURN 
      END IF
      IF ( TAI < -1.5D0*86400.0D0 .OR. TAI > 1.5D0*86400.0D0 ) THEN
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR, FMT='(1PD15.7)' ) TAI
           CALL ERR_LOG ( 2494, IUER, 'XY_TO_DPSI_DEPSILON', 'Wrong parameter TAI: '// &
     &          TRIM(STR)//' it should be no more than -+ 86400.0 seconds' )
           RETURN 
      END IF
      CALL TAI_TO_TDB  ( MJD, TAI, TDB )
      TARG_TDB = (MJD - J2000__MJD - 0.5D0)*86400.0D0 + TDB
!
      EPS_A  = EPSA__ARG(0)             + &
     &         EPSA__ARG(1)*TARG_TDB    + &
     &         EPSA__ARG(2)*TARG_TDB**2 + &
     &         EPSA__ARG(3)*TARG_TDB**3 + &
     &         EPSA__ARG(4)*TARG_TDB**4 + &
     &         EPSA__ARG(5)*TARG_TDB**5
!
      PSI_A  = PSIA__ARG(0)             + &
     &         PSIA__ARG(1)*TARG_TDB    + &
     &         PSIA__ARG(2)*TARG_TDB**2 + &
     &         PSIA__ARG(3)*TARG_TDB**3 + &
     &         PSIA__ARG(4)*TARG_TDB**4 + &
     &         PSIA__ARG(5)*TARG_TDB**5
!
      CHI_A  = CHIA__ARG(0)             + &
     &         CHIA__ARG(1)*TARG_TDB    + &
     &         CHIA__ARG(2)*TARG_TDB**2 + &
     &         CHIA__ARG(3)*TARG_TDB**3 + &
     &         CHIA__ARG(4)*TARG_TDB**4 + &
     &         CHIA__ARG(5)*TARG_TDB**5
      EPS_0 = EPSA__ARG(0)
!
      DPSI = ( DX -    ( PSI_A*DCOS(EPS_0) - CHI_A ) * DY )/ &
     &       ( 1.0D0 + ( PSI_A*DCOS(EPS_0) - CHI_A )**2   )/DSIN(EPS_A)
!
      DEPS = ( DY +    ( PSI_A*DCOS(EPS_0) - CHI_A )*DX )/ &
     &       ( 1.0D0 + ( PSI_A*DCOS(EPS_0) - CHI_A )**2 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  XY_TO_DPSI_DEPSILON  !#!#
