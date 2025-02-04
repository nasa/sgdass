!
! --- 24-JUL-98 L. Petrov (petrov@kuestner.geod.uni-bonn.de) 13-JAN-2002
! --- Include blocks for defining some numerical values of generlized
! --- Love numbers
!
      INTEGER*4  N$1, N$2
      REAL*8     LOVE_MDG97(9,6), LOVE_DDW99(9,6), LOVE_MAT00(9,6)
      DATA     ((LOVE_MDG97(N$1,N$2), N$2=1,6 ), N$1=1,9 ) &
!
! --- Numerical values of frequency-independent generalized Love numbers
! --- following MDG-97 model taken from the paper  P.M. Mathews, V. Dehant
! --- and J.M. Gipson "Tidal Station Displacements", Journal of Geophysical
! --- Research, 1997
!
! --- Value -9.9 means that that generlized Love number is frequency-dependent
! --- within the framework of the tidal model.
!
!    |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!    |           ELACITITY           |           ANELASTICITY         !
!    |   long    diurnal  semidirnal |   long    diurnal   semdiurnal !
!    |                               |                                !
     & / &
     &  .59980, -9.9    ,  0.60260,     -9.9    , -9.9    ,  0.60780, &  ! (1) h0
     &  .00000,  0.00000,  0.00000,     -9.9    , -9.9    , -0.00220, &  ! (2) hi
     & -.00060, -9.9    , -0.00060,     -0.00060, -9.9    , -0.00060, &  ! (3) h2
     &  .00010,  0.00000,  0.00000,      0.00010,  0.00000,  0.00000, &  ! (4) h'
     &  .08310, -9.9    ,  0.08310,     -9.9    , -9.9    ,  0.08470, &  ! (5) l0
     &  .00000,  0.00000,  0.00000,     -9.9    , -9.9    , -0.00070, &  ! (6) li
     &  .00000, -9.9    ,  0.00240,      0.00000, -9.9    ,  0.00240, &  ! (7) l1
     &  .00020,  0.00020,  0.00020,      0.00020,  0.00020,  0.00020, &  ! (8) l2
     &  .00000, -9.9    ,  0.00000,      0.00000, -9.9    ,  0.00000 &   ! (9) l'
     & /
!
      DATA     ((LOVE_DDW99(N$1,N$2), N$2=1,6 ), N$1=1,9 ) &
!
! --- Numerical values of frequency-independent generalized Love numbers
! --- following DDW-97 model taken from the paper V. Dehant, P. Defraigne
! --- and J.M. Wahr "Tides for a convective Earth", Journal of Geophysical
! --- Research, 1997
!
! --- Value -9.9 means that the generlized Love number is frequency-dependent
! --- within the framework of the tidal model.
!
!    |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!    |           ELACITITY           |          ANELASTICITY          !
!    |   long    diurnal  semidirnal |  long    diurnal   semdiurnal  !
!    |                               |                                !
     & / &
     &  0.59930, -9.9    ,  0.60137,    -9.9    , -9.9    ,  0.61004, &  ! (1) h0
     &  0.00000,  0.00000,  0.00000,     0.00000,  0.00000,  0.00000, &  ! (2) hi
     & -0.00050, -9.9    , -0.00047,    -0.00050, -9.9    , -0.00046, &  ! (3) h2
     &  0.00016,  0.00000,  0.00000,     0.00016,  0.00000,  0.00000, &  ! (4) h'
     &  0.08309, -9.9    ,  0.08354,    -9.9    , -9.9    ,  0.08572, &  ! (5) l0
     &  0.00000,  0.00000,  0.00000,     0.00000,  0.00000,  0.00000, &  ! (6) li
     &  0.00000, -9.9    ,  0.00100,     0.00000, -9.9    ,  0.00096, &  ! (7) l1
     &  0.00008, -9.9    ,  0.00008,     0.00008, -9.9    ,  0.00010, &  ! (8) l2
     &  0.00000, -9.9    ,  0.00000,     0.00000, -9.9    ,  0.00000 &   ! (9) l'
     & /
!
      DATA     ((LOVE_MAT00(N$1,N$2), N$2=1,3 ), N$1=1,9 ) &
!
! --- Numerical values of frequency-independent generalized Love numbers
! --- following IERS Conventions 2000
!
! --- Value -9.9 means that the generlized Love number is frequency-dependent
! --- within the framework of the tidal model.
!
!    |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|
!    |           ELACITITY           |
!    |   long    diurnal  semidirnal |
!    |                               |
     & / &
     & -9.9   ,  -9.9   ,   0.6078, &    ! (1) h0
     & -9.9   ,  -9.9   ,  -0.0022, &    ! (2) hi
     & -0.0006,  -9.9   ,  -0.0006, &    ! (3) h2
     &  0.0001,   0.0000,   0.0000, &    ! (4) h'
     & -9.9   ,  -9.9   ,   0.0847, &    ! (5) l0
     & -9.9   ,  -9.9   ,  -0.0007, &    ! (6) li
     &  0.0000,  -9.9   ,   0.0024, &    ! (7) l1
     &  0.0002,   0.0002,   0.0002, &    ! (8) l2
     &  0.0000,  -9.9   ,   0.0000 &     ! (9) l'
     & /
!
! --- Frequency independent Love numbers
!
      REAL*8     H2_LOVE, L2_LOVE, H3_LOVE, L3_LOVE, H2_FLUID, L2_FLUID
      PARAMETER  ( H2_LOVE  = 0.6090D0, L2_LOVE  = 0.0852D0 )
      PARAMETER  ( H3_LOVE  = 0.2920D0, L3_LOVE  = 0.0152D0 )
      PARAMETER  ( H2_FLUID = 0.94,     L2_FLUID = 0.0D0    )
