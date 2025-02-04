         SUBROUTINE do_dry_chao(station,elevation,eldot)
!
!
! this program overwrites the wet partial in oborg with
! the Chao dry partial. Calculations are from calc76 (program catmm.f).
! It is basically the same as cekpartial.f for calculating chao wet.
!
         IMPLICIT NONE
         INCLUDE 'solve.i'
         INCLUDE 'oborg.i'
!
!  AEE  920212 Original version
!
        integer*2 station
        real*8 elevation
        real*8 eldot
        real*8 aChwet,bChwet,rtrace1,rtrace2
        parameter(aChwet=3.5e-4)
        parameter(bChwet=1.7e-2)
        parameter(rtrace1=1.43D-3)
        parameter(rtrace2=4.45D-2)
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
        ap(station,1)= 1.0d0/(dsinel1+(rtrace1/ &   ! sign change now in cfacalc.f
     &              (dtanel1+rtrace2)))
!
! Calculate RATE:
!
        ap(station,2)= -ap(station,1)**2*(DCOSEL1-rtrace1/(DTANEL1+ &
     &  rtrace2)**2/DCOSEL1**2)*eldot
!
        return
        END
