         SUBROUTINE cekpartial(station,elevation,eldot)
!
!  CK  5/15/91
!
! this program overwrites the wet partial for delays in oborg with
!    the Chao wet partial  equation 19 p. 75. C.C. Chao "The
!    Tropospheric Calibration Model for Mariner Mars 1971", JPL
!    Tech Report 32-1587 (NASA Sci & Tech Info Facility N74-16983)
!
         IMPLICIT NONE
         INCLUDE 'solve.i'
         INCLUDE 'oborg.i'
!
!  AEE  911117  added rates.
        integer*2 station
        real*8 elevation
        real*8 eldot
        real*8 aChwet,bChwet
        parameter(aChwet=3.5e-4)
        parameter(bChwet=1.7e-2)
        REAL*8 SINEL      ! sine of elev angle
        REAL*8 DSINEL1,dsinel2     ! double precision version of SINEL
        REAL*8 tanEL      ! sine of elev angle
        REAL*8 DtanEL1,dtanel2     ! double precision version of SINEL
        REAL*8 COSEL  ! cosine of elev angle
        REAL*8 DCOSEL1 ! double precision version of COSEL
!
!  Over write partiales
!
        SINEL=SIN(ELEVATION)
        DSINEL1=DBLE(SINEL)
        tanEL=tan(ELEVATION)
        DtanEL1=DBLE(tanEL)
        COSEL=COS(ELEVATION)
        DCOSEL1=DBLE(COSEL)
!
! Calculate DELAY:
        ap(station,1)= 1.0d0/(dsinel1+(aChwet/ &   ! sign change now in cfacalc.f
     &              (dtanel1+bChwet)))
!
! Calculate RATE:
!
        ap(station,2)= -ap(station,1)**2*(DCOSEL1-aChwet/(DTANEL1+ &
     &  bChwet)**2/DCOSEL1**2)*eldot
!
        return
        END
