      SUBROUTINE UT1S_83 ( FA, FA_DOT, DUT, DLOD, DOMEGA )
      IMPLICIT NONE
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
! Note added by JMGipson
!     This routine was substantially rewritten to calculate DUT_DOT,DLOD,
!     and DOMEGA using the series expansion of DUT.  This was done becuase
!     the old definition of DUT_DOT in terms of a separate series was
!     inconsistent.  I modified it so that it used the same table to calculate
!     the two values.
!     I also removed dlod and domega from the argument list, which solve/calc
!     does not use.  Instead I put in DUT_DOT, which is used.
!
!
!     Arguments:
!          fa     = fundamental arguments as of subroutine NUTFA (arc-seconds)
!          fa_dot = time derivative of fa given as arc-seconds/century
!          DUT    = Effect on UT (Subtract from observations.  Add to
!                   IERS UT1*) in seconds (out).
!          DUT_DOT = Change in UT1/Day.
!
! following two removed from argument list. (JMG)
!          DLOD   = Effect on length of day in seconds (out).
!          DOMEGA = Effect on rotational speed in radians/second (out).
!          Note:  DLOD=-DUT_DOT
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
!       J.M. Gipson        97.08.08  Substantially re-written
!       L.   Petrov        97.08.29  Change name and comments
!
      REAL*8 DUT, DLOD, DOMEGA, SECCON, ARG, DMOD,arg_dot
      DOUBLE PRECISION dut_dot
      DOUBLE PRECISION cos_arg,sin_arg
      REAL*8 fa(5), fa_dot(5)
      REAL*8 XS(7,62)
!
      integer*2 n,i
      DATA SECCON/206264.8062470964D0/
      DOUBLE PRECISION twopi/6.283185307179959d0/
!
!
!  N=Number of tidal terms to be used (62 for full set).
      data n/62/
!
!
!***********************************************************************
!
!     Table of multiples of arguments and coefficients
!
!                  Multiple of            DUT
!             l   l'  F   D OMEGA     sin   cos
      DATA XS/1., 0., 2., 2., 2.,    -0.02, 0.00, &
     &        2., 0., 2., 0., 1.,    -0.04, 0.00, &
     &        2., 0., 2., 0., 2.,    -0.10, 0.00, &
     &        0., 0., 2., 2., 1.,    -0.05, 0.00, &
     &        0., 0., 2., 2., 2.,    -0.12, 0.00, &
     &        1., 0., 2., 0., 0.,    -0.04, 0.00, &
     &        1., 0., 2., 0., 1.,    -0.40, 0.01, &
     &        1., 0., 2., 0., 2.,    -0.98, 0.03, &
     &        3., 0., 0., 0., 0.,    -0.02, 0.00, &
     &       -1., 0., 2., 2., 1.,    -0.08, 0.00, &
     &       -1., 0., 2., 2., 2.,    -0.20, 0.00, &
     &        1., 0., 0., 2., 0.,    -0.08, 0.00, &
     &        2., 0., 2.,-2., 2.,     0.02, 0.00, &
     &        0., 1., 2., 0., 2.,     0.03, 0.00, &
     &        0., 0., 2., 0., 0.,    -0.30, 0.00, &
     &        0., 0., 2., 0., 1.,    -3.20, 0.09, &
     &        0., 0., 2., 0., 2.,    -7.73, 0.21, &
     &        2., 0., 0., 0.,-1.,     0.02, 0.00, &
     &        2., 0., 0., 0., 0.,    -0.34, 0.00, &
     &        2., 0., 0., 0., 1.,     0.02, 0.00, &
     &        0.,-1., 2., 0., 2.,    -0.02, 0.00, &
     &        0., 0., 0., 2.,-1.,     0.05, 0.00, &
     &        0., 0., 0., 2., 0.,    -0.72, 0.02, &
     &        0., 0., 0., 2., 1.,    -0.05, 0.00, &
     &        0.,-1., 0., 2., 0.,    -0.05, 0.00, &
     &        1., 0., 2.,-2., 1.,     0.05, 0.00, &
     &        1., 0., 2.,-2., 2.,     0.10, 0.00, &
     &        1., 1., 0., 0., 0.,     0.04, 0.00, &
     &       -1., 0., 2., 0., 0.,     0.05, 0.00, &
     &       -1., 0., 2., 0., 1.,     0.18, 0.00, &
     &       -1., 0., 2., 0., 2.,     0.44, 0.00, &
     &        1., 0., 0., 0.,-1.,     0.53, 0.00, &
     &        1., 0., 0., 0., 0.,    -8.33, 0.12, &
     &        1., 0., 0., 0., 1.,     0.54, 0.00, &
     &        0., 0., 0., 1., 0.,     0.05, 0.00, &
     &        1.,-1., 0., 0., 0.,    -0.06, 0.00, &
     &       -1., 0., 0., 2.,-1.,     0.12, 0.00, &
     &       -1., 0., 0., 2., 0.,    -1.84, 0.02, &
     &       -1., 0., 0., 2., 1.,     0.13, 0.00, &
     &        1., 0.,-2., 2.,-1.,     0.02, 0.00, &
     &       -1.,-1., 0., 2., 0.,    -0.09, 0.00, &
     &        0., 2., 2.,-2., 2.,    -0.06, 0.00, &
     &        0., 1., 2.,-2., 1.,     0.03, 0.00, &
     &        0., 1., 2.,-2., 2.,    -1.88, 0.00, &
     &        0., 0., 2.,-2., 0.,     0.25, 0.00, &
     &        0., 0., 2.,-2., 1.,     1.17, 0.00, &
     &        0., 0., 2.,-2., 2.,   -48.84, 0.11, &
     &        0., 2., 0., 0., 0.,    -0.19, 0.00, &
     &        2., 0., 0.,-2.,-1.,     0.05, 0.00, &
     &        2., 0., 0.,-2., 0.,    -0.55, 0.00, &
     &        2., 0., 0.,-2., 1.,     0.04, 0.00, &
     &        0.,-1., 2.,-2., 1.,    -0.05, 0.00, &
     &        0., 1., 0., 0.,-1.,     0.09, 0.00, &
     &        0.,-1., 2.,-2., 2.,     0.83, 0.00, &
     &        0., 1., 0., 0., 0.,   -15.55, 0.02, &
     &        0., 1., 0., 0., 1.,    -0.14, 0.00, &
     &        1., 0., 0.,-1., 0.,     0.03, 0.00, &
     &        2., 0.,-2., 0., 0.,    -0.14, 0.00, &
     &       -2., 0., 2., 0., 1.,     0.42, 0.00, &
     &       -1., 1., 0., 1., 0.,     0.04, 0.00, &
     &        0., 0., 0., 0., 2.,     7.90, 0.00, &
     &        0., 0., 0., 0., 1., -1637.68,-0.10/
!
! Note: The last line contains corrected term for DUT1: "-0.10"
!       Previous versions contained misprint: "0.10"
!
!
!
!***********************************************************************
!
!     Fundamental arguments will be calculated only at one place:
!     in subroutine NUTFA in file cnutm.f
!
!      L = fa (1)
!      LP= fa (2)
!      F = fa (3)
!      D = fa (4)
!      OM= fa (5)
!
      DUT     = 0.0D+0
      DLOD    = 0.0D+0
      DOMEGA  = 0.0D+0
      DUT_DOT = 0.0d+0
!
! --- Sum zonal tide terms
!
      DO I=1,n
!       Formation of multiples of arguments
        arg=xs(1,i)*fa(1)+        xs(2,i)*fa(2)+    xs(3,i)*fa(3)+ &
     &      xs(4,i)*fa(4)+xs(5,i)*fa(5)
!
        arg_dot=xs(1,i)*fa_dot(1)+xs(2,i)*fa_dot(2)+xs(3,i)*fa_dot(3)+ &
     &      xs(4,i)*fa_dot(4)+xs(5,i)*fa_dot(5)
!
!------ Convert arg to radians:
!
        ARG = DMOD(ARG,1296000.0D0) / SECCON
!
! ----- Evaluate zonal tidal terms
!
        cos_arg=dCOS(Arg)
        sin_arg=dSIN(Arg)
        DUT    = xs(6,i) *sin_arg + xs(7,i)*cos_arg         +dut
        dut_dot=(xs(6,i) *cos_arg - xs(7,i)*sin_arg)*arg_dot+dut_dot
      enddo
!
! --- Rescale DUT, DUT_DOT
!
      DUT     = DUT    * 1.0D-4
!
! --- And convert dut_dot to DUT/day.  arg_dot is in units of AS/century
!
      dut_dot = dut_dot *1.0d-4 /seccon/(36525.d0)
!
! --- Added JM Gipson
! --- Original routine passed back these two. If you want to have these
! ---- then put them in argument list. However, solve doesn't use them.
!
      DLOD   =-DUT_DOT
      DOMEGA = DUT_DOT*TWOPI/(86400.D0)**2.
!
      RETURN
      END  !#!  UT1S_83  #!#
