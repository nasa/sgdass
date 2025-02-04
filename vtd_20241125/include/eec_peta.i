!
! --- Expansion of cross-terms nutation-precession and nutation-nutation
! --- in the expression for the equation of equinox for NUT__PETA expansion
!
! --- Argument: time in seconds since J2000.0 (2000.01.01_12:00:00 TDB)
!
      INTEGER*4   N_EEC_PETA, I_EEC_PETA
      PARAMETER ( N_EEC_PETA =  5 )
      REAL*8  EEC_PETA_PHS(N_EEC_PETA), EEC_PETA_FRQ(N_EEC_PETA), &
     &        EEC_PETA_COS(N_EEC_PETA), EEC_PETA_SIN(N_EEC_PETA)
      DATA  ( EEC_PETA_PHS(I_EEC_PETA), EEC_PETA_FRQ(I_EEC_PETA), &
     &        EEC_PETA_COS(I_EEC_PETA), EEC_PETA_SIN(I_EEC_PETA), &
     &        I_EEC_PETA = 1, N_EEC_PETA ) &
!         Phase        Frequency             cos-term       sin-term     !  N
     &        / &
     &  2.18243920, -1.069696206302D-08,   0.000000D+00,  1.28462D-08, & !  1
     &  4.36487840, -2.139392412604D-08,   0.000000D+00,  3.72118D-10, & !  2
     &  5.68937989,  3.875158084385D-07,   0.000000D+00,  5.97517D-11, & !  3
     &  1.32450149,  4.089097325645D-07,   0.000000D+00,  5.66256D-11, & !  4
     &  3.50694069,  3.982127705015D-07,   0.000000D+00,  2.05607D-11  & !  5
     &        /
