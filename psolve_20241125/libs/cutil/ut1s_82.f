      SUBROUTINE UT1S_82 ( FA, DUT, DLOD, DOMEGA)
      IMPLICIT   NONE
!
!     Purpose: This subroutine evaluates the effects of zonal Earth tides on
!     the rotation of the Earth. The model used is from Yoder, Williams, and
!     Park (1981) and modified by the ocean effects as given in Dickman (1991)
!     as recommended by the IERS Standards, p. 117, 119-120 (1992).
!
!     Special Note: Under the UT1S definition, and as done by this routine,
!     zonal tides of _all_ periods are evaluated, including even those of
!     18.6 year period. Results will be substantially different (tenths of
!     seconds) from those evaluated with the "UT1R" algorithm, which only
!     includes the 41 terms for periods under 35 days. If you wish to determine
!     the effects from only those periods, please use the original Luzum
!     "zontids" routine, with N set to 41.  (B.A.)
!
!     Arguments:
!          fa     = fundamental arguments as of subroutine NUTFA
!          DUT    = Effect on UT (Subtract from observations.  Add to
!                   IERS UT1*) in seconds (out).
!          DLOD   = Effect on length of day in seconds (out).
!          DOMEGA = Effect on rotational speed in radians/second (out).
!
!     Written by:
!       Brian J. Luzum     92.08.11
!
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
!       PVT             JULY 2 1999  Removed definition of SECCON
!
!
      REAL*8 T, DUT, DLOD, DOMEGA, SECCON, L, LP, F, D, OM, &
     &     ARG, DMOD
      REAL*8 fa(5)
      REAL*8 XS(11,62), X1(220), X2(220), X3(220), X4(22)
      EQUIVALENCE(XS(1,  1),X1(1))
      EQUIVALENCE(XS(1, 21),X2(1))
      EQUIVALENCE(XS(1, 41),X3(1))
      EQUIVALENCE(XS(1, 61),X4(1))
      integer*2 n,i
      DATA SECCON/206264.8062470964D0/
!
!  N=Number of tidal terms to be used (62 for full set).
!  (This was an argument in the original zontids routine.)
!
      DATA  N/62/
!
!***********************************************************************
!
!     Table of multiples of arguments and coefficients
!
!                  Multiple of            DUT        DLOD      DOMEGA
!             l   l'  F   D OMEGA     sin   cos    cos  sin   cos   sin
      DATA X1/1., 0., 2., 2., 2.,    -0.02, 0.00,  0.3, 0.0, -0.2,  0.0, &
     &        2., 0., 2., 0., 1.,    -0.04, 0.00,  0.4, 0.0, -0.3,  0.0, &
     &        2., 0., 2., 0., 2.,    -0.10, 0.00,  0.9, 0.0, -0.8,  0.0, &
     &        0., 0., 2., 2., 1.,    -0.05, 0.00,  0.4, 0.0, -0.4,  0.0, &
     &        0., 0., 2., 2., 2.,    -0.12, 0.00,  1.1, 0.0, -0.9,  0.0, &
     &        1., 0., 2., 0., 0.,    -0.04, 0.00,  0.3, 0.0, -0.2,  0.0, &
     &        1., 0., 2., 0., 1.,    -0.40, 0.01,  2.7, 0.1, -2.3, -0.1, &
     &        1., 0., 2., 0., 2.,    -0.98, 0.03,  6.7, 0.2, -5.7, -0.2, &
     &        3., 0., 0., 0., 0.,    -0.02, 0.00,  0.1, 0.0, -0.1,  0.0, &
     &       -1., 0., 2., 2., 1.,    -0.08, 0.00,  0.5, 0.0, -0.5,  0.0, &
     &       -1., 0., 2., 2., 2.,    -0.20, 0.00,  1.3, 0.0, -1.1,  0.0, &
     &        1., 0., 0., 2., 0.,    -0.08, 0.00,  0.5, 0.0, -0.4,  0.0, &
     &        2., 0., 2.,-2., 2.,     0.02, 0.00, -0.1, 0.0,  0.1,  0.0, &
     &        0., 1., 2., 0., 2.,     0.03, 0.00, -0.1, 0.0,  0.1,  0.0, &
     &        0., 0., 2., 0., 0.,    -0.30, 0.00,  1.4, 0.0, -1.2,  0.0, &
     &        0., 0., 2., 0., 1.,    -3.20, 0.09, 14.7, 0.4,-12.4, -0.4, &
     &        0., 0., 2., 0., 2.,    -7.73, 0.21, 35.6, 1.0,-30.0, -0.8, &
     &        2., 0., 0., 0.,-1.,     0.02, 0.00, -0.1, 0.0,  0.1,  0.0, &
     &        2., 0., 0., 0., 0.,    -0.34, 0.00,  1.5, 0.0, -1.3,  0.0, &
     &        2., 0., 0., 0., 1.,     0.02, 0.00, -0.1, 0.0,  0.1,  0.0/
      DATA X2/0.,-1., 2., 0., 2.,    -0.02, 0.00,  0.1, 0.0, -0.1,  0.0, &
     &        0., 0., 0., 2.,-1.,     0.05, 0.00, -0.2, 0.0,  0.2,  0.0, &
     &        0., 0., 0., 2., 0.,    -0.72, 0.02,  3.1, 0.1, -2.6, -0.1, &
     &        0., 0., 0., 2., 1.,    -0.05, 0.00,  0.2, 0.0, -0.2,  0.0, &
     &        0.,-1., 0., 2., 0.,    -0.05, 0.00,  0.2, 0.0, -0.2,  0.0, &
     &        1., 0., 2.,-2., 1.,     0.05, 0.00, -0.1, 0.0,  0.1,  0.0, &
     &        1., 0., 2.,-2., 2.,     0.10, 0.00, -0.3, 0.0,  0.2,  0.0, &
     &        1., 1., 0., 0., 0.,     0.04, 0.00, -0.1, 0.0,  0.1,  0.0, &
     &       -1., 0., 2., 0., 0.,     0.05, 0.00, -0.1, 0.0,  0.1,  0.0, &
     &       -1., 0., 2., 0., 1.,     0.18, 0.00, -0.4, 0.0,  0.3,  0.0, &
     &       -1., 0., 2., 0., 2.,     0.44, 0.00, -1.0, 0.0,  0.9,  0.0, &
     &        1., 0., 0., 0.,-1.,     0.53, 0.00, -1.2, 0.0,  1.0,  0.0, &
     &        1., 0., 0., 0., 0.,    -8.33, 0.12, 19.0, 0.3,-16.0, -0.2, &
     &        1., 0., 0., 0., 1.,     0.54, 0.00, -1.2, 0.0,  1.0,  0.0, &
     &        0., 0., 0., 1., 0.,     0.05, 0.00, -0.1, 0.0,  0.1,  0.0, &
     &        1.,-1., 0., 0., 0.,    -0.06, 0.00,  0.1, 0.0, -0.1,  0.0, &
     &       -1., 0., 0., 2.,-1.,     0.12, 0.00, -0.2, 0.0,  0.2,  0.0, &
     &       -1., 0., 0., 2., 0.,    -1.84, 0.02,  3.6, 0.0, -3.0,  0.0, &
     &       -1., 0., 0., 2., 1.,     0.13, 0.00, -0.3, 0.0,  0.2,  0.0, &
     &        1., 0.,-2., 2.,-1.,     0.02, 0.00,  0.0, 0.0,  0.0,  0.0/
      DATA X3/-1.,-1.,0., 2., 0.,    -0.09, 0.00,  0.2, 0.0, -0.1,  0.0, &
     &        0., 2., 2.,-2., 2.,    -0.06, 0.00,  0.0, 0.0, -0.0,  0.0, &
     &        0., 1., 2.,-2., 1.,     0.03, 0.00, -0.0, 0.0,  0.0,  0.0, &
     &        0., 1., 2.,-2., 2.,    -1.88, 0.00,  1.0, 0.0, -0.8,  0.0, &
     &        0., 0., 2.,-2., 0.,     0.25, 0.00, -0.1, 0.0,  0.1,  0.0, &
     &        0., 0., 2.,-2., 1.,     1.17, 0.00, -0.4, 0.0,  0.3,  0.0, &
     &        0., 0., 2.,-2., 2.,   -48.84, 0.11, 16.8, 0.0,-14.2,  0.0, &
     &        0., 2., 0., 0., 0.,    -0.19, 0.00,  0.1, 0.0, -0.1,  0.0, &
     &        2., 0., 0.,-2.,-1.,     0.05, 0.00, -0.0, 0.0,  0.0,  0.0, &
     &        2., 0., 0.,-2., 0.,    -0.55, 0.00,  0.2, 0.0, -0.1,  0.0, &
     &        2., 0., 0.,-2., 1.,     0.04, 0.00, -0.0, 0.0,  0.0,  0.0, &
     &        0.,-1., 2.,-2., 1.,    -0.05, 0.00,  0.0, 0.0, -0.0,  0.0, &
     &        0., 1., 0., 0.,-1.,     0.09, 0.00, -0.0, 0.0,  0.0,  0.0, &
     &        0.,-1., 2.,-2., 2.,     0.83, 0.00, -0.1, 0.0,  0.1,  0.0, &
     &        0., 1., 0., 0., 0.,   -15.55, 0.02,  2.6, 0.0, -2.2,  0.0, &
     &        0., 1., 0., 0., 1.,    -0.14, 0.00,  0.0, 0.0, -0.0,  0.0, &
     &        1., 0., 0.,-1., 0.,     0.03, 0.00, -0.0, 0.0,  0.0,  0.0, &
     &        2., 0.,-2., 0., 0.,    -0.14, 0.00, -0.0, 0.0,  0.0,  0.0, &
     &       -2., 0., 2., 0., 1.,     0.42, 0.00, -0.0, 0.0,  0.0,  0.0, &
     &       -1., 1., 0., 1., 0.,     0.04, 0.00, -0.0, 0.0,  0.0,  0.0/
      DATA X4/0., 0., 0., 0., 2.,     7.90, 0.00,  0.1, 0.0, -0.1,  0.0, &
     &        0., 0., 0., 0., 1., -1637.68, 0.10,-10.4, 0.0,  8.8,  0.0/
!
!***********************************************************************
!
!     Fundamental arguments will be calculated only at one place:
!     in subroutine NUTFA in file cnutm.f
!
      L = fa (1)
      LP= fa (2)
      F = fa (3)
      D = fa (4)
      OM= fa (5)
!
      DUT    = 0.0D+0
      DLOD   = 0.0D+0
      DOMEGA = 0.0D+0
!
!     Sum zonal tide terms
!
      DO I=1,n
!       Formation of multiples of arguments
        ARG = XS(1,I) * L &
     &      + XS(2,I) * LP &
     &      + XS(3,I) * F &
     &      + XS(4,I) * D &
     &      + XS(5,I) * OM
        ARG = DMOD(ARG,1296000.0D0) / SECCON
!
!       Evaluate zonal tidal terms
        DUT    = XS( 6,I)*sin(ARG) + XS( 7,I)*cos(ARG) + DUT
        DLOD   = XS( 8,I)*cos(ARG) + XS( 9,I)*sin(ARG) + DLOD
        DOMEGA = XS(10,I)*cos(ARG) + XS(11,I)*sin(ARG) + DOMEGA
      enddo
!
      DUT    = DUT    * 1.0D-4
      DLOD   = DLOD   * 1.0D-5
      DOMEGA = DOMEGA * 1.0D-14
!
      RETURN
      END  !#!  UT1S_82  #!#
