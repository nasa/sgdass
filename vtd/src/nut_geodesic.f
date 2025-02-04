      SUBROUTINE NUT_GEODESIC ( MJD, TAI, E1_GDS, E2_GDS, DPSI_GDS, DEPS_GDS )
! ************************************************************************
! *                                                                      *
! *   Routine NUT_GEODESIC computes contribution to nutation angles      *
! *   on the specified moment of time due to so-called geodesic          *
! *   nutation.                                                          *
! *                                                                      *
! *   Reference:                                                         *
! *   T. Fukushima, "Geodesic Nutation", Astron. & Astrophys., vol. 244, *
! *      pp. L11-L12, 1991.                                              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      MJD ( INTEGER*4 ) -- Modified Julian date of the midnight of    *
! *                           the day of interest.                       *
! *      TAI ( REAL*8    ) -- Time in TAI elapsed from the midnight of   *
! *                           interest. Units: seconds.                  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   E1_GDS ( REAL*8    ) -- Rotation angle with respect to the axis 1  *
! *                           of the transformation from the celestial   *
! *                           frame to terrestrial frame. Units: rad.    *
! *   E2_GDS ( REAL*8    ) -- Rotation angle with respect to the axis 2  *
! *                           of the transformation from the celestial   *
! *                           frame to terrestrial frame. Units: rad.    *
! * DPSI_GDS ( REAL*8    ) -- Angle of nutation in longitude.            *
! *                           Units: rad.                                *
! * DEPS_GDS ( REAL*8    ) -- Angle of nutation in obliquity.            *
! *                           Units: rad.                                *
! *                                                                      *
! * ### 14-JUN-2004   NUT_GEODESIC   v1.0 (c) L. Petrov  14-JUN-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'nut_const.i'
      INTEGER*4  MJD
      REAL*8     TAI, E1_GDS, E2_GDS, DPSI_GDS, DEPS_GDS
      INTEGER*4  M_GDS
      PARAMETER  ( M_GDS = 6 ) 
      REAL*8     PHS_GDS(M_GDS), FRQ_GDS(M_GDS), AMP_GDS(M_GDS)
      INTEGER*4  N1, J1
      REAL*8     TDB, TARG_TDB, ARG
!
!      Phase                Frequency                amplitude (rad)
!
      DATA     ( PHS_GDS(N1), FRQ_GDS(N1), AMP_GDS(N1), N1=1,M_GDS ) &
     &         / &
     & 2.18243919663506D0, -7.293185551531016D-05,    6.3D-12, & ! -6798.28
     & 4.10074611054452D0, -7.291046159118412D-05,    0.5D-12, & !  6798.28
     & 0.04312518025661D0, -7.312025542848485D-05,  147.4D-12, & !  -365.26
     & 6.24006012692298D0, -7.272206167800943D-05, -147.4D-12, & !   365.26
     & 0.08625036051321D0, -7.331937132374864D-05,    1.9D-12, & !  -182.62
     & 6.19693494666637D0, -7.252294578274564D-05,   -1.9D-12  & !   182.62
     &         /
!
      CALL TAI_TO_TDB ( MJD, TAI, TDB )
      TARG_TDB = (MJD - 51544)*86400.0D0 - 43200.0D0 + TDB      
!
      E1_GDS = 0.0D0
      E2_GDS = 0.0D0
      DPSI_GDS = 0.D0
      DEPS_GDS = 0.D0
      DO 410 J1=1,M_GDS
!
! ------ First compute contribution to E1,E2 angles
!
         ARG = PHS_GDS(J1) + FRQ_GDS(J1)*TARG_TDB
         E1_GDS = E1_GDS + AMP_GDS(J1)*DCOS(ARG)
         E2_GDS = E2_GDS + AMP_GDS(J1)*DSIN(ARG)
!
! ------ Then compute contribution to DPSI/DEPS angles
!
         ARG = PHS_GDS(J1) + (FRQ_GDS(J1) + OM_NM + OM_PRC)*TARG_TDB
         DPSI_GDS = DPSI_GDS - AMP_GDS(J1)*DSIN(ARG)/DSIN(EPSILON_0)
         DEPS_GDS = DEPS_GDS + AMP_GDS(J1)*DCOS(ARG)
 410  CONTINUE 
!
      RETURN
      END  SUBROUTINE NUT_GEODESIC
