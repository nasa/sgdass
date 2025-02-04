!
      BLOCK DATA NUTCMB
      IMPLICIT NONE
!
!
! 7.1.1 NUTCM IS THE NUTATION MODULE BLOCK DATA INITIALIZATION SECTION.
!       THE NUTATION SERIES IS ESTABLISHED HERE.  THIS VERSION CONTAINS
!       THE 1980 IAU THEORY OF NUTATION, FROM THE WORK OF J. M. WAHR,
!       SPECIFICALLY, THE WAHR NUTATION SERIES FOR AXIS B OF GILBERT &
!       DZIEWONSKI EARTH MODEL 1066A.
!
! 7.1.3 REFERENCES - 1) 'THE EXPLANATORY SUPPLEMENT TO THE AMERICAN
!                    EPHEMERIS AND NAUTICAL ALMANAC", P. 41-45, 98
!
!                    2) LIESKE, J.H., ET AL., EXPRESSIONS FOR THE
!                    PRECESSIONAL QUANTITIES BASED ON THE IAU (1976)
!                    SYSTEM OF ASTRONOMICAL CONSTANTS,
!                    ASTRON. ASTROPHYS. 58, 1-16, 1977.
!
!                    3) SEIDELMANN, P. K., 1980 IAU THEORY OF NUTATION:
!                    THE FINAL REPORT OF THE IAU WORKING GROUP ON
!                    NUTATION, CELEST. MECH. 27, PP. 79-106 (1982).
!
!                    4) WAHR, J. M., THE FORCED NUTATIONS OF ... EARTH,
!                    GEOPHYS. J. ROY. ASTR. SOC. 64, PP. 705-727 (1981).
!
      INTEGER*2 NOT
      REAL*8 X(9,20)
      COMMON / NUTCM_W / NOT,X
!
      REAL*8 X1
      DIMENSION X1(180)
      EQUIVALENCE (X(1,  1),X1(1))
!
      DATA  NOT/20/
!
!***********************************************************************
!
!               1980 IAU THEORY OF NUTATION (WAHR THEORY)
!           TABLE OF MULTIPLES OF ARGUMENTS AND COEFFICIENTS
!
!                   MULTIPLE OF            LONGITUDE        OBLIQUITY
!              L    L'   F    D  OMEGA   COEFF. OF SIN    COEFF. OF COS
      DATA X1/ 0.,  0.,  0.,  0.,  1., -171996., -174.2,  92025.,  8.9, &
     &         0.,  0.,  2., -2.,  2.,  -13187.,   -1.6,   5736., -3.1, &
     &         0.,  0.,  2.,  0.,  2.,   -2274.,   -0.2,    977., -0.5, &
     &         0.,  0.,  0.,  0.,  2.,    2062.,    0.2,   -895.,  0.5, &
     &         0.,  1.,  0.,  0.,  0.,    1426.,   -3.4,     54., -0.1, &
     &         1.,  0.,  0.,  0.,  0.,     712.,    0.1,     -7.,  0.0, &
     &         0.,  1.,  2., -2.,  2.,    -517.,    1.2,    224., -0.6, &
     &         0.,  0.,  2.,  0.,  1.,    -386.,   -0.4,    200.,  0.0, &
     &         1.,  0.,  2.,  0.,  2.,    -301.,    0.0,    129., -0.1, &
     &         0., -1.,  2., -2.,  2.,     217.,   -0.5,    -95.,  0.3, &
     &         1.,  0.,  0., -2.,  0.,    -158.,    0.0,     -1.,  0.0, &
     &         0.,  0.,  2., -2.,  1.,     129.,    0.1,    -70.,  0.0, &
     &        -1.,  0.,  2.,  0.,  2.,     123.,    0.0,    -53.,  0.0, &
     &         1.,  0.,  0.,  0.,  1.,      63.,    0.1,    -33.,  0.0, &
     &         0.,  0.,  0.,  2.,  0.,      63.,    0.0,     -2.,  0.0, &
     &        -1.,  0.,  2.,  2.,  2.,     -59.,    0.0,     26.,  0.0, &
     &        -1.,  0.,  0.,  0.,  1.,     -58.,   -0.1,     32.,  0.0, &
     &         1.,  0.,  2.,  0.,  1.,     -51.,    0.0,     27.,  0.0, &
     &         2.,  0.,  0., -2.,  0.,      48.,    0.0,      1.,  0.0, &
     &        -2.,  0.,  2.,  0.,  1.,      46.,    0.0,    -24.,  0.0/
!
!                    GEORGE KAPLAN  04/24/84
!
      END
