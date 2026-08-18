      SUBROUTINE UT1ZT (T, DUT, DLOD, DOMEGA)
      IMPLICIT NONE
!
! 1.  UT1ZT PROGRAM SPECIFICATION
!
! 1.1 Evaluate the effects of zonal earth tides on the rotation
!     of the earth.
!
! 1.2 REFERENCES:
!                    1.  YODER, WILLIAMS, AND PARKE, 1981, "TIDAL
!                        VARIATIONS OF EARTH ROTATION", J. GEOPHYS.
!                        RES., VOL. 86, P. 881-891.
!                    2.  PROJECT MERIT STANDARDS, W. MELBOURNE, CHAIRMAN,
!                        U.S. NAVAL OBSERVATORY, WASHINGTON, D.C.,
!                        DEC. 27, 1983.
!
! 2.  UT1ZT INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      EXTERNAL UT1CMB
      REAL*8 T, seccon, el, elp, f, d, om, arg
      integer*2 j, i
!
! T - TDB time in Julian centuries since J2000 ( 01-JAN-2000 12:00 )
!
! 2.3 OUTPUT Variables:
!
      REAL*8 DUT,DLOD,DOMEGA
!
! DUT - Effect on UT, in seconds (subtract from observation, add to BIH UT1)
! DLOD - Effect on length of day, in seconds
! DOMEGA - Effect on rotational speed, radians/second.
!
! 2.4 COMMON BLOCKS USED
      REAL*8 X
      COMMON/UT1CM/ X(8,41)
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: ut1mu
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      DATA SECCON/206264.8062470964D0/
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!  GK,DM  ????   Created
!   DGG  840413  Documentation only
!   DGG  840605  Added debug coding
!   DGG  840807  Fixed coding
!   jwr  02-022  Added IMPLICIT NONE and typed seccon, el, and elp
!
! 5.  UT1ZT PROGRAM STRUCTURE
!
!   1.    COMPUTATION OF  FUNDAMENTAL ARGUMENTS
!
      EL = ((+0.064D0 * T + 31.310D0) * T + 715922.633D0) * T &
     &     + 485866.733D0 + DMOD(1325.0D0*T,1.0D0) * 1296000.0D0
      EL = DMOD(EL,1296000.0D0)
!
      ELP = ((-0.012D0 * T - 0.577D0) * T + 1292581.224D0) * T &
     &     + 1287099.804D00 + DMOD(99.0D0*T,1.0D0) * 1296000.0D0
      ELP = DMOD(ELP,1296000.0D0)
!
      F = ((+0.011D0 * T - 13.257D0) * T + 295263.137D0) * T &
     &     + 335778.877D0 + DMOD(1342.0D0*T,1.0D0) * 1296000.0D0
      F = DMOD(F,1296000.0D0)
!
      D = ((+0.019D0 * T - 6.891D0) * T + 1105601.328D0) * T &
     &     + 1072261.307D0 + DMOD(1236.0D0*T,1.0D0) * 1296000.0D0
      D = DMOD(D,1296000.0D0)
!
      OM = ((0.008D0 * T + 7.455D0) * T - 482890.539D0) * T &
     &     + 450160.280D0  - DMOD(5.0D0*T,1.0D0) * 1296000.0D0
      OM = DMOD(OM,1296000.0D0)
!
!   2.  INITIALIZE PROGRAM VARIABLES
!
      DUT = 0.0D+0
      DLOD = 0.0D+0
      DOMEGA = 0.0D+0
!
!   3.  SUM ZONAL TIDE TERMS
!
      DO 10 J=1,41
      I = 42 - J
!
!   4.  FORMATION OF MULTIPLES OF ARGUMENTS
!
      ARG = X(1,I) * EL &
     &    + X(2,I) * ELP &
     &    + X(3,I) * F &
     &    + X(4,I) * D &
     &    + X(5,I) * OM
      ARG = DMOD(ARG,1296000.0D0) / SECCON
!
!   5.  EVALUATE ZONAL TIDAL TERMS
!
      DUT    = X(6,I)*DSIN(ARG) + DUT
      DLOD   = X(7,I)*DCOS(ARG) + DLOD
      DOMEGA = X(8,I)*DCOS(ARG) + DOMEGA
   10 CONTINUE
      DUT = DUT * 1.0D-4
      DLOD = DLOD * 1.0D-5
      DOMEGA = DOMEGA * 1.0D-14
!
!    6.   CHECK TO SEE IF DEBUG OUTPUT IS NEEDED
!
!     IF (KUT1D .NE. 1) GO TO 600
!     GO TO 600
!
! 599 WRITE(ILUOUT,9000)
!9000 FORMAT(1X,"DEBUG OUTPUT FROM SUBROUTINE UT1ZT")
!     WRITE(ILUOUT,9200) EL,ELP,F,D,OM,ARG,DUT,DLOD,DOMEGA
!9200 FORMAT(1X,"EL = ",D30.16,/,1X,"ELP = ",D30.16,/,
!    1       1X,"F = ",D30.16,/,1X,"D = ",D30.16,/,
!    2       1X,"OM = ",D30.16,/,1X,"ARG = ",D30.16,/,
!    2       1X,"DUT = ",D30.16,/,1X,"DLOD = ",D30.16,/,
!    2       1X,"DOMEGA = ",D30.16)
  600 CONTINUE
!
!    7.   NORMAL TERMINATION
!
      RETURN
      END
