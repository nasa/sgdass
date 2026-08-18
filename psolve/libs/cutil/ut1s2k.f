      SUBROUTINE UT1S2K (FA2K, FAD2K, DUT, DLOD, DOMEGA)
      IMPLICIT None
!
!    From Calc 10.0, 2005.12.21, DG
!
!     Purpose: This subroutine evaluates the effects of zonal Earth tides on
!     the rotation of the Earth. The model used is from Defraigne and Smits,
!     1999, as recommended by the IERS Conventions (2003).
!
!     Input Variables:
!          fa(5)  = Fundamental arguments from subroutine NUTFA (arcseconds)
!          fad(5) = Time derivatives of fundamental arguments (arcsec/century)
!     Output Variables:
!          DUT    - 'UT1 minus UT1S'. Effect on UT (Subtract from observation,
!                   add to IERS UT1). (sec)
!          DLOD   = Effect on length of day. (seconds).
!          DOMEGA = Effect on rotational speed (radians/second).
!
!     Written by:
!       Brian J. Luzum     92.08.11
!     Modifications:
!       Brent A. Archinal  92.08.27  Name changed from zontids to
!                                    ut1szt, N dropped from argument
!                                    list, comments improved.
!       "     "  "         92.10.27  Special note added above.
!       "     "  "         92.12.17  All real variables set to double
!                                    precision, at Jim Ray's suggestion
!                                    (email of 92.11.20).
!       David Gordon       93.03.17  Array X changed to XS, debug printout
!                                    added for calc 8.0.
!       Norbert Zacharias  93.09.16  Take fundam. arg. from subr. NUTFA
!       David Gordon       94.04.06  Changed to 'Implicit None'.
!       David Gordon       98.01.21  Extensive mods from John Gipson to use
!                                    the series expansion of DUT to calculate
!                                    DUT, DLOD, and DOMEGA instead of seperate
!                                    series expansions. fad(5) added to
!                                    subroutine call. DBLE's removed. CMATH
!                                    common block added. Variable SECCON
!                                    removed and replaced with 1/CONVDS. Sign
!                                    of X(7,62) corrected.
!       David Gordon  2003.11.24     Name changed to UT1S2K, modified for
!                                    Defraigne and Smits (1999) model, as
!                                    recommended in IERS Conventions (2003).
!       David Gordon  2006.07.14     Bug fix. Define PI, TWOPI, HALFPI, CONVD, 
!                                    CONVDS, CONVHS, SECDAY (were in common
!                                    block in Calc). 
!
      REAL*8 DUT, DLOD, DOMEGA, ARG
      REAL*8 FA2K(14), FAD2K(14), ARG_DOT
      Integer*4 N, I
      REAL*8 XS(11,62), X1(220), X2(220), X3(220), X4(22)
      EQUIVALENCE(XS(1,  1),X1(1))
      EQUIVALENCE(XS(1, 21),X2(1))
      EQUIVALENCE(XS(1, 41),X3(1))
      EQUIVALENCE(XS(1, 61),X4(1))
!
      REAL*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      PARAMETER  ( PI = 3.1415926535897932D0 )
      PARAMETER  ( TWOPI  = 6.2831853071795865D0 )
      PARAMETER  ( HALFPI = 1.5707963267948966D0 )
      PARAMETER  ( CONVD  = 1.7453292519943296D-02 )
      PARAMETER  ( CONVDS = 4.8481368110953599D-06 )
      PARAMETER  ( CONVHS = 7.2722052166430399D-05 )
      PARAMETER  ( SECDAY = 86400.0D0 )
!
!            1. SECDAY -  The number of seconds in a day. (s/day)
!            2. TWOPI  -  PI times 2.0D0
!            3. CONVDS -  THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS
!                 (RAD/ARCSECOND)
!
!  N=Number of tidal terms to be used (62 for full set).
      DATA  N/62/
!
!***********************************************************************
!     Table of multiples of arguments and coefficients
!
!                  Multiple of            DUT        DLOD      DOMEGA
!             l   l'  F   D OMEGA     sin   cos    cos  sin   cos   sin
      DATA X1/1., 0., 2., 2., 2.,   -0.02, 0.00, 0.26, 0.00, -0.22, .00, &
     &        2., 0., 2., 0., 1.,   -0.04, 0.00, 0.38, 0.00, -0.32, .00, &
     &        2., 0., 2., 0., 2.,   -0.10, 0.00, 0.91, 0.00, -0.76, .00, &
     &        0., 0., 2., 2., 1.,   -0.05, 0.00, 0.45, 0.00, -0.38, .00, &
     &        0., 0., 2., 2., 2.,   -0.12, 0.00, 1.09, 0.01, -0.92,-.01, &
!
     &        1., 0., 2., 0., 0.,   -0.04, 0.00, 0.27, 0.00, -0.22, .0 , &
     &        1., 0., 2., 0., 1.,   -0.40, 0.00, 2.84, 0.02, -2.40,-.01, &
     &        1., 0., 2., 0., 2.,   -1.00, 0.01, 6.85, 0.04, -5.78,-.03, &
     &        3., 0., 0., 0., 0.,   -0.02, 0.00, 0.12, 0.00, -0.11, .00, &
     &       -1., 0., 2., 2., 1.,   -0.08, 0.00, 0.54, 0.00, -0.46, .00, &
!
     &       -1., 0., 2., 2., 2.,   -0.20, 0.00, 1.30, 0.01, -1.10,-.01, &
     &        1., 0., 0., 2., 0.,   -0.08, 0.00, 0.50, 0.00, -0.42, .00, &
     &        2., 0., 2.,-2., 2.,    0.02, 0.00,-0.11, 0.00,  0.09, .00, &
     &        0., 1., 2., 0., 2.,    0.03, 0.00,-0.12, 0.00,  0.10, .00, &
     &        0., 0., 2., 0., 0.,   -0.30, 0.00, 1.39, 0.01, -1.17,-.01, &
!
     &        0., 0., 2., 0., 1.,   -3.22, 0.02,14.86, 0.09,-12.54,-.08, &
     &        0., 0., 2., 0., 2.,   -7.79, 0.05,35.84, 0.22,-30.25,-.18, &
     &        2., 0., 0., 0.,-1.,    0.02, 0.00,-0.10, 0.00,  0.08, .00, &
     &        2., 0., 0., 0., 0.,   -0.34, 0.00, 1.55, 0.01, -1.31,-.01, &
     &        2., 0., 0., 0., 1.,    0.02, 0.00,-0.08, 0.00,  0.07, .00/
!
      DATA X2/0.,-1., 2., 0., 2.,   -0.02, 0.00, 0.11, 0.00, -0.09, .00, &
     &        0., 0., 0., 2.,-1.,    0.05, 0.00,-0.20, 0.00,  0.17, .00, &
     &        0., 0., 0., 2., 0.,   -0.74, 0.00, 3.14, 0.02, -2.65,-.02, &
     &        0., 0., 0., 2., 1.,   -0.05, 0.00, 0.22, 0.00, -0.19, .00, &
     &        0.,-1., 0., 2., 0.,   -0.05, 0.00, 0.21, 0.00, -0.17, .00, &
!
     &        1., 0., 2.,-2., 1.,    0.05, 0.00,-0.13, 0.00,  0.11, .00, &
     &        1., 0., 2.,-2., 2.,    0.10, 0.00,-0.26, 0.00,  0.22, .00, &
     &        1., 1., 0., 0., 0.,    0.04, 0.00,-0.10, 0.00,  0.08, .00, &
     &       -1., 0., 2., 0., 0.,    0.05, 0.00,-0.11, 0.00,  0.09, .00, &
     &       -1., 0., 2., 0., 1.,    0.18, 0.00,-0.41, 0.00,  0.35, .00, &
!
     &       -1., 0., 2., 0., 2.,    0.44, 0.00,-1.02,-0.01,  0.86, .01, &
     &        1., 0., 0., 0.,-1.,    0.54, 0.00,-1.23,-0.01,  1.04, .01, &
     &        1., 0., 0., 0., 0.,   -8.33, 0.06,18.99, 0.13,-16.03,-.11, &
     &        1., 0., 0., 0., 1.,    0.55, 0.00,-1.25,-0.01,  1.05, .01, &
     &        0., 0., 0., 1., 0.,    0.05, 0.00,-0.11, 0.00,  0.09, .00, &
!
     &        1.,-1., 0., 0., 0.,   -0.06, 0.00, 0.12, 0.00, -0.10, .00, &
     &       -1., 0., 0., 2.,-1.,    0.12, 0.00,-0.24, 0.00,  0.20, .00, &
     &       -1., 0., 0., 2., 0.,   -1.84, 0.01, 3.63, 0.02, -3.07,-.02, &
     &       -1., 0., 0., 2., 1.,    0.13, 0.00,-0.26, 0.00,  0.22, .00, &
     &        1., 0.,-2., 2.,-1.,    0.02, 0.00,-0.04, 0.00,  0.03, .03/
!
      DATA X3/-1.,-1.,0., 2., 0.,   -0.09, 0.00, 0.16, 0.00, -0.13, .00, &
     &        0., 2., 2.,-2., 2.,   -0.06, 0.00, 0.04, 0.00, -0.03, .00, &
     &        0., 1., 2.,-2., 1.,    0.03, 0.00,-0.02, 0.00,  0.01, .00, &
     &        0., 1., 2.,-2., 2.,   -1.91, 0.02, 0.98, 0.01, -0.83,-.01, &
     &        0., 0., 2.,-2., 0.,    0.26, 0.00,-0.09, 0.00,  0.08, .00, &
!
     &        0., 0., 2.,-2., 1.,    1.18,-0.01,-0.42, 0.00,  0.35, .00, &
     &        0., 0., 2.,-2., 2.,  -49.06, 0.43,16.88, 0.15,-14.25,-.13, &
     &        0., 2., 0., 0., 0.,   -0.20, 0.00, 0.07, 0.00, -0.06, .00, &
     &        2., 0., 0.,-2.,-1.,    0.05, 0.00,-0.02, 0.00,  0.01, .00, &
     &        2., 0., 0.,-2., 0.,   -0.56, 0.01, 0.17, 0.00, -0.14, .00, &
!
     &        2., 0., 0.,-2., 1.,    0.04, 0.00,-0.01, 0.00,  0.01, .00, &
     &        0.,-1., 2.,-2., 1.,   -0.05, 0.00, 0.01, 0.00, -0.01, .00, &
     &        0., 1., 0., 0.,-1.,    0.09, 0.00,-0.02, 0.00,  0.01, .00, &
     &        0.,-1., 2.,-2., 2.,    0.82,-0.01,-0.14, 0.00,  0.12, .00, &
     &        0., 1., 0., 0., 0.,  -15.65, 0.15, 2.69, 0.03, -2.27,-.02, &
!
     &        0., 1., 0., 0., 1.,   -0.14, 0.00, 0.02, 0.00, -0.02, .00, &
     &        1., 0., 0.,-1., 0.,    0.03, 0.00, 0.00, 0.00,  0.0 , .00, &
     &        2., 0.,-2., 0., 0.,   -0.14, 0.00,-0.01, 0.00,  0.01, .00, &
     &       -2., 0., 2., 0., 1.,    0.43,-0.01,-0.02, 0.00,  0.02, .00, &
     &       -1., 1., 0., 1., 0.,   -0.04, 0.00, 0.00, 0.00,  0.00, .00/
!
      DATA X4/0., 0., 0., 0., 2.,    8.20, 0.11, 0.15, 0.00, -0.13, .00, &
     &        0., 0., 0., 0., 1.,-1689.54,-25.04,-15.62,0.23,13.18,-.20/
!
!***********************************************************************
!
      DUT    = 0.0D+0
      DLOD   = 0.0D+0
      DOMEGA = 0.0D+0
!
!     Sum zonal tide terms
!
      DO 10 I=1,N
!   Formation of multiples of arguments
      ARG = XS(1,I)*FA2K(1) + XS(2,I)*FA2K(2) + XS(3,I)*FA2K(3) &
     &    + XS(4,I)*FA2K(4) + XS(5,I)*FA2K(5)
!!!   ARG = DMOD(ARG,1296000.0D0) * CONVDS
!  Already in radians!!!
      ARG = DMOD(ARG,1296000.0D0)
!   First derivative
      ARG_DOT = XS(1,I)*FAD2K(1)  + XS(2,I)*FAD2K(2) + XS(3,I)*FAD2K(3) &
     &    + XS(4,I)*FAD2K(4) + XS(5,I)*FAD2K(5)
! convert from radians/sec to arcsec/century
      ARG_DOT = ARG_DOT*(SECDAY*36525.D0)/CONVDS
!
!     Evaluate zonal tidal terms
      DUT    = XS(6,I)*DSIN(ARG) + XS(7,I)*DCOS(ARG) + DUT
      DLOD   = ( XS(6,I)*DCOS(ARG) - XS(7,I)*DSIN(ARG) )*ARG_DOT + DLOD
   10 CONTINUE
      DUT    = DUT    * 1.0D-4
      DLOD   = -DLOD * 1.0D-4 / (3.6525D+4 / CONVDS)
      DOMEGA = -DLOD * TWOPI / SECDAY**2
!
!     WRITE(6,9)
!   9 FORMAT(1X,"Debug output for subroutine UT1S2K")
!     WRITE(6,9200) L,LP,F,D,OM,ARG,DUT,DLOD,DOMEGA
!9200 FORMAT(1X,"L = ",D30.16,/,1X,"LP = ",D30.16,/, &
!    &       1X,"F = ",D30.16,/,1X,"D = ",D30.16,/, &
!    &       1X,"OM = ",D30.16,/,1X,"ARG = ",D30.16,/, &
!    &       1X,"DUT = ",D30.16,/,1X,"DLOD = ",D30.16,/, &
!    &       1X,"DOMEGA = ",D30.16)
!
      RETURN
      END
