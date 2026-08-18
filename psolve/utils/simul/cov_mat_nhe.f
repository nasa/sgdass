      SUBROUTINE COV_MAT_NHE ( MJD, UTC, NOBS, AZ, EL, DHSEG, DH,       &
     &                         CN, H, VN, VE, COV, IUER )
! ***************************************************************************************
! *                                                                                     *
! *  Function SIM_SWD first generates a vector of turbulent equivalent zenith           *
! *  wet delays (ezwd) for a certain station. The ezwd is then mapped to the            *
! *  elevation of the observation using VMF1 or GMF. The mapping function is            *
! *  supposed to be free of error (i.e. the same mapping function and                   *
! *  coefficients are used for the generation of the slant delays as are for            *
! *  the lsm adjustment).                                                               *
! *                                                                                     *
! *  ASSUMPTION:                                                                        *
! *              - Analysis for 1 day, therefore MAX(UTC) <= 86400                      *
! *                                                                                     *
! *  REFERENCES:                                                                        *
! *              - Nilsson et al. 2007 "Simulations of atmospheric path delays  using   *
! *                turbulence models"                                                   *
! *              - B??hm et al. 2007 "Simulation of zenith wet delays an clocks"        *
! *              - Treuhaft and Lanyi 1987 "The effect of the dynamic wet troposphere   *
! *                on radio interferometric measurements"                               *
! *                                                                                     *
! *   INPUT:                                                                            *
! *          MJD     =  Modified Julian Days                  { INT }    (Nx1)          *
! *                                                                                     *
! *          UTC     =  UTC                                   { REAL }   (Nx1)          *
! *                                                                                     *
! *          NOBS    =  No. of observations                   { INT }                   *
! *                                                                                     *
! *          AZ      =  azimuth                               { REAL }   (Nx1)   [rad]  *
! *                                                                                     *
! *          EL      =  elevation                             { REAL }   (Nx1)   [rad]  *
! *                                                                                     *
! *          DHSEG   =  day sec segment                       { REAL }            [s]   *
! *                     time segments over which observations are to be correlated      *
! *                     in seconds.                                                     *
! *                     if 7200s (2hrs) is not short enough it might be better to       *
! *                     use 3600s (1hr). If possible use larger values.                 *
! *                                                                                     *
! *          DH      =  height increment                      { REAL }           [m]    *
! *                     for numeric integration (e.g. 200 m)                            *
! *                                                                                     *
! *          CN      =  refractive index structure constant   { REAL }  [1e-7*m^(-1/3)] *
! *                                                                                     *
! *          H       =  effective height of the troposphere   { REAL }           [m]    *
! *                                                                                     *
! *          VN      =  north component of the wind vector    { REAL }          [m/s]   *
! *                                                                                     *
! *          VE      =  east component of the wind vector     { REAL }          [m/s]   *
! *                                                                                     *
! *   OUTPUT:                                                                           *
! *          COV     = Covariance matrix                      { REAL }   (NxN)  [s^2]   *
! *                                                                                     *
! *  #### 31-JUL-2010    SIM_SWD         v1.3 (c)        A. Pany      31-JUL-2010 ###   *     
! *     - 23-JUN-2021    N. Habana      Translated the above version from Matlab         *
! *                                                                                     *
! ***************************************************************************************
!
! ########## N.B: Use the defined speed of light, not your current place holder "C__NUM"
      IMPLICIT   NONE
      INCLUDE    'astro_constants.i'
      INTEGER*4  IUER, IER
      INTEGER*4  MHEI__OCCAM
      PARAMETER  ( MHEI__OCCAM = 100 )
      REAL*8     C__NUM
      PARAMETER  ( C__NUM =  299792458.D0 )
      INTEGER*4  NDAYS, NOBS
      INTEGER*4  MJD(NOBS)
      REAL*8     UTC(NOBS), AZ(NOBS), EL(NOBS)
      REAL*8     COV(NOBS,NOBS)
      REAL*8     C(NOBS,NOBS), C11(NOBS,NOBS)
      REAL*8     C21(NOBS,NOBS), C22(NOBS,NOBS)
      REAL*8     DHSEG, DH, CN, H, VN, VE, WZD0
      REAL*8     CNALL, L0, L23, AZ1, EL1, R0(3)
      REAL*8     VV(3), TIM(NOBS), RI(NOBS,3), SEG_LEN
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, J17, J18, J19, J20, J21, J22, IND_SEG(NOBS)
      INTEGER*4  K
      REAL*8     ZS(MHEI__OCCAM**2), Z(MHEI__OCCAM**2), V(3,MHEI__OCCAM**2)
      REAL*8     R0Z(3,MHEI__OCCAM**2), R0ZS(3,MHEI__OCCAM**2)
      REAL*8     RHO4(MHEI__OCCAM**2), RHO4X(MHEI__OCCAM**2)
      REAL*8     TIM_SPLT(NOBS), RSUM
      INTEGER*4  NTIM_SEG(NOBS)
      REAL*8     RCOMP, RCOMP_V1(MHEI__OCCAM**2), RCOMP_V2(MHEI__OCCAM**2)
      REAL*8     RCOMP_M1(NOBS,NOBS), RCOMP_M2(NOBS,NOBS)
      REAL*8     SHR_SEG, TIM_EPS
      PARAMETER  ( SHR_SEG = 0.7D0 )
      PARAMETER  ( TIM_EPS = 1.0D0 )
      INTEGER*4  ICOMP, NH, NSEG, I1
      REAL*8     DTI0, DTJ0, DTIJ, DD1(3,MHEI__OCCAM**2)
      REAL*8     RHO1(MHEI__OCCAM**2), RHO1X(MHEI__OCCAM**2)
      REAL*8     RHO2(MHEI__OCCAM**2), RHO3(MHEI__OCCAM**2)
      REAL*8     RIZ(3,MHEI__OCCAM**2)
      REAL*8     RJZ(3,MHEI__OCCAM**2), RJZS(3,MHEI__OCCAM**2)
      REAL*8     DD2(3,MHEI__OCCAM**2), DD3(3,MHEI__OCCAM**2)
      REAL*8     X(NOBS), L(NOBS), L1(NOBS)
      INTEGER*4  ROW11A, COL11A, ROW21A, COL21A, ROW22A, COL22A
      INTEGER*4  ROW11B, COL11B, ROW21B, COL21B, ROW22B, COL22B
      INTEGER*4  NROW11, NCOL11, NROW21, NCOL21, NROW22, NCOL22
      INTEGER*4  NUM1, NUM2, NUM3, NUM_EPC_SEG(NOBS)
      INTEGER*4, EXTERNAL :: ICNT_IDX8
!
! --- Constants
!    
      L0     =  3.D6                    ! Saturation scale length ( Treuhaft & Lanyi 1987 )
      L23    =  L0**(2.D0/3.D0)
      CNALL  =  CN**2.D0/2.D0*DH**2.D0 
      NH     =  FLOOR(H/DH)             ! No. of height increments
!
! --- Wind vector [m/s]
!
      VV(1) = VN
      VV(2) = VE
      VV(3) = 0.D0       
!
! --- Time epochs counted from the 1st observation in seconds
!
      TIM(1) = 0.0D0
      DO 410 J1=2,NOBS
         TIM(J1) = (MJD(J1) - MJD(1))*86400.0D0 + (UTC(J1) - UTC(1))
         IF ( TIM(J1) < TIM(J1-1) ) THEN
              TIM(J1) = TIM(J1) + 1.D0
         END IF
 410  CONTINUE
!
! --- Observation vector
!
      RI = 0.D0
!
      DO 420 J2=1,NOBS
         RI(J2,1) = DCOS(AZ(J2))/DTAN(EL(J2))
         RI(J2,2) = DSIN(AZ(J2))/DTAN(EL(J2))
         RI(J2,3) = 1.D0
 420  CONTINUE
!
! --- Sacrificise the first observation
!
      AZ1   = 0.D0
      EL1   = PI__NUM/2.D0
      R0(1) = DCOS(AZ1)/DTAN(EL1)
      R0(2) = DSIN(AZ1)/DTAN(EL1)
      R0(3) = 1.D0
!
      K     = 0
!
! --- Preallocation
!
      ZS  =  0.D0
      Z   =  0.D0
      V   =  0.D0
!
      DO 430 J3=1,NH+1
         DO 440 J4=1,NH+1
            K      =  K + 1
            ZS(K)  =  DH*(J3-1)
            Z(K)   =  DH*(J4-1)
            V(1,K) =  VV(1)
            V(2,K) =  VV(2)
            V(3,K) =  VV(3)
!
            R0ZS(1,K)  = R0(1)*ZS(K)
            R0ZS(2,K)  = R0(2)*ZS(K)
            R0ZS(3,K)  = R0(3)*ZS(K)
            R0Z(1,K)   = R0(1)*Z(K)           
            R0Z(2,K)   = R0(2)*Z(K)
            R0Z(3,K)   = R0(3)*Z(K)
!
            RCOMP     =  (R0Z(1,K) - R0ZS(1,K))**2.D0  +                &
     &                   (R0Z(2,K) - R0ZS(2,K))**2.D0  +                &
     &                   (R0Z(3,K) - R0ZS(3,K))**2.D0                      ! [m^2]
            RHO4(K)   =  DSQRT(RCOMP)                                      ! [m]
            RHO4X(K)  =  RHO4(K)**(2.D0/3.D0)/                          & 
     &                   (1.D0+RHO4(K)**(2.D0/3.D0)/L23)                   ! [m^2/3]
 440     CONTINUE
 430  CONTINUE
!
! --- Compute NSEG -- the number of segments
!
      NSEG = IDINT ( (TIM(NOBS)+TIM_EPS)/DHSEG )
!
      IF ( NSEG*DHSEG - TIM(NOBS) > SHR_SEG*DHSEG ) THEN
!
! -------- If the last segment is longer than SHR*DHSEG,
! -------- we increase the number of segments and reduce the segment length
!
           NSEG = NSEG + 1
      END IF
      IF ( NSEG == 0 ) NSEG = 1 ! For instance if NOBS is 1
!
! --- Adjust segment length
!
      SEG_LEN = TIM(NOBS)/NSEG
      IF ( SEG_LEN < TIM_EPS ) THEN
           SEG_LEN = TIM_EPS
      END IF
      NUM_EPC_SEG = 0
      DO 450 J5=1,NOBS
!
! ------ IND_SEG -- the current segment index for the J5-th observation, counted from 1
!
         IND_SEG(J5) = IDINT ( TIM(J5)/SEG_LEN ) + 1
         IF ( IND_SEG(J5) > NSEG ) IND_SEG(J5) = NSEG
         NUM_EPC_SEG(IND_SEG(J5)) = NUM_EPC_SEG(IND_SEG(J5)) + 1
 450  CONTINUE
      IF ( NSEG == 1 ) THEN
!
! ======== Case: we have only one segment
!
           NUM3  = NUM_EPC_SEG(1)
           C = 0.D0
           DO 460 J6=1,NUM3
              DTI0  = TIM(J6)    ! [s]
              DO 470 J7 = 1, (NH+1)**2
                 RIZ(1,J7)  =  RI(J6,1)*Z(J7)
                 RIZ(2,J7)  =  RI(J6,2)*Z(J7)
                 RIZ(3,J7)  =  RI(J6,3)*Z(J7)
!
                 DD1(1,J7)  = RIZ(1,J7) - R0ZS(1,J7) + V(1,J7)*DTI0
                 DD1(2,J7)  = RIZ(2,J7) - R0ZS(2,J7) + V(2,J7)*DTI0
                 DD1(3,J7)  = RIZ(3,J7) - R0ZS(3,J7) + V(3,J7)*DTI0
!
                 RCOMP      = DD1(1,J7)**2.D0 + DD1(2,J7)**2.D0 +          &
     &                        DD1(3,J7)**2.D0
                 RHO1(J7)  = RCOMP**(1.D0/3.D0)
                 RHO1X(J7) = RHO1(J7)/(1.D0 + RHO1(J7)/L23)
 470          CONTINUE
!
              DO 480 J8=1,NUM3
!
! -------------- Compute the time differences in hours
!
                 DTJ0 = TIM(J8)
                 DTIJ = TIM(J8) - TIM(J6)
!
                 RSUM = 0.D0
                 DO 490 J9=1,(NH+1)**2
                    RJZ(1,J9)  =  RI(J8,1)*Z(J9)
                    RJZ(2,J9)  =  RI(J8,2)*Z(J9)
                    RJZ(3,J9)  =  RI(J8,3)*Z(J9)
!
                    RJZS(1,J9)  =  RI(J8,1)*ZS(J9)
                    RJZS(2,J9)  =  RI(J8,2)*ZS(J9)
                    RJZS(3,J9)  =  RI(J8,3)*ZS(J9)
!
! ----------------- Compute the separations
!
                    DD2(1,J9)  = RJZ(1,J9) - R0ZS(1,J9) + V(1,J9)*DTJ0
                    DD2(2,J9)  = RJZ(2,J9) - R0ZS(2,J9) + V(2,J9)*DTJ0
                    DD2(3,J9)  = RJZ(3,J9) - R0ZS(3,J9) + V(3,J9)*DTJ0
!
                    DD3(1,J9)  = RIZ(1,J9) - RJZS(1,J9) - V(1,J9)*DTIJ
                    DD3(2,J9)  = RIZ(2,J9) - RJZS(2,J9) - V(2,J9)*DTIJ
                    DD3(3,J9)  = RIZ(3,J9) - RJZS(3,J9) - V(3,J9)*DTIJ
!
                    RCOMP    = DD2(1,J9)**2.D0 + DD2(2,J9)**2.D0 +        &
     &                         DD2(3,J9)**2.D0
                    RHO2(J9) = RCOMP**(1.D0/3.D0)
!
                    RCOMP    = DD3(1,J9)**2.D0 + DD3(2,J9)**2.D0 +        &
     &                         DD3(3,J9)**2.D0
                    RHO3(J9) = RCOMP**(1.D0/3.D0)
!
                    RCOMP_V1(J9) = RHO1X(J9) + RHO2(J9)/(1.D0+RHO2(J9)/L23) &
     &                    - RHO3(J9)/(1.D0+RHO3(J9)/L23) - RHO4X(J9)
                    RSUM = RSUM + RCOMP_V1(J9)
 490             CONTINUE
!
                 C(J6,J8) = CNALL*RSUM/(C__NUM**2.D0)
 480          CONTINUE
 460       CONTINUE
      ELSE
!
! ------ First two segments
!
         J1    = 1
         NUM1  = NUM_EPC_SEG(J1)
         NUM2  = NUM_EPC_SEG(J1+1)
         NUM3  = NUM1 + NUM2
!
         C   = 0.D0
!
! ------ How many epochs before segment J1?
!
         K  =  SUM(NUM_EPC_SEG(1:J1-1))
!
         DO 4100 J10=1,NUM3
            DTI0 = TIM(K+J10)
            DO 4110 J11=1,(NH+1)**2              
               RIZ(1,J11)  =  RI(K+J10,1)*Z(J11)
               RIZ(2,J11)  =  RI(K+J10,2)*Z(J11)
               RIZ(3,J11)  =  RI(K+J10,3)*Z(J11)
!
               DD1(1,J11)  = RIZ(1,J11) - R0ZS(1,J11) + V(1,J11)*DTI0
               DD1(2,J11)  = RIZ(2,J11) - R0ZS(2,J11) + V(2,J11)*DTI0
               DD1(3,J11)  = RIZ(3,J11) - R0ZS(3,J11) + V(3,J11)*DTI0
!
               RCOMP     = DD1(1,J11)**2.D0 + DD1(2,J11)**2.D0 +          &
     &                     DD1(3,J11)**2.D0
               RHO1(J11)  = RCOMP**(1.D0/3.D0)
               RHO1X(J11) = RHO1(J11)/(1.D0 + RHO1(J11)/L23)
 4110       CONTINUE
!
            DO 4120 J12=1,NUM3
!
! ------------ Compute the time differences in hours
!
               DTJ0 = TIM(K+J12)
               DTIJ = DTJ0 - DTI0
!
! ------------ Compute the position Vectors
!
               RSUM = 0.D0
               DO 4130 J13 = 1, (NH+1)**2
                  RJZ(1,J13)  =  RI(K+J12,1)*Z(J13)
                  RJZ(2,J13)  =  RI(K+J12,2)*Z(J13)
                  RJZ(3,J13)  =  RI(K+J12,3)*Z(J13)
!
                  RJZS(1,J13)  =  RI(K+J12,1)*ZS(J13)
                  RJZS(2,J13)  =  RI(K+J12,2)*ZS(J13)
                  RJZS(3,J13)  =  RI(K+J12,3)*ZS(J13)
!
! --------------- Compute the separations
!
                  DD2(1,J13)  = RJZ(1,J13) - R0ZS(1,J13) + V(1,J13)*DTJ0
                  DD2(2,J13)  = RJZ(2,J13) - R0ZS(2,J13) + V(2,J13)*DTJ0
                  DD2(3,J13)  = RJZ(3,J13) - R0ZS(3,J13) + V(3,J13)*DTJ0
!
                  DD3(1,J13)  = RIZ(1,J13) - RJZS(1,J13) - V(1,J13)*DTIJ
                  DD3(2,J13)  = RIZ(2,J13) - RJZS(2,J13) - V(2,J13)*DTIJ
                  DD3(3,J13)  = RIZ(3,J13) - RJZS(3,J13) - V(3,J13)*DTIJ
!
                  RCOMP    = DD2(1,J13)**2.D0 + DD2(2,J13)**2.D0 +        &
     &                       DD2(3,J13)**2.D0
                  RHO2(J13) = RCOMP**(1.D0/3.D0)
!
                  RCOMP    = DD3(1,J13)**2.D0 + DD3(2,J13)**2.D0 +        &
     &                       DD3(3,J13)**2.D0
                  RHO3(J13) = RCOMP**(1.D0/3.D0)
!
                  RCOMP_V1(J13) = RHO1X(J13) + RHO2(J13)/(1.D0+RHO2(J13)/L23) &
     &                  - RHO3(J13)/(1.D0+RHO3(J13)/L23) - RHO4X(J13)
                   RSUM = RSUM + RCOMP_V1(J13)
 4130           CONTINUE
!
               C(J10,J12) = CNALL*RSUM/(C__NUM**2.D0)
 4120       CONTINUE           
 4100    CONTINUE
!
         C11 = C
!
! ------ Loop over 2 adjacent 2 Hr blocks
!
         DO 4140 J14=2,NSEG-1
            NUM1  = NUM_EPC_SEG(J14)
            NUM2  = NUM_EPC_SEG(J14+1)
            NUM3  = NUM1 + NUM2
!
            C(1:NUM3,1:NUM3) = 0.D0
!
! --------- How many epochs are before the segment J14?
!
            K  =  SUM(NUM_EPC_SEG(1:J14-1)) 
            C(1:NUM1,1:NUM1) = C11(1:NUM1,1:NUM1)
!
            DO 4150 J15 = 1, NUM1
               DTI0 = TIM(K+J15)
               DO 4160 J16=1,(NH+1)**2              
                  RIZ(1,J16)  =  RI(J15+K,1)*Z(J16)
                  RIZ(2,J16)  =  RI(J15+K,2)*Z(J16)
                  RIZ(3,J16)  =  RI(J15+K,3)*Z(J16)
!
                  DD1(1,J16)  = RIZ(1,J16) - R0ZS(1,J16) + V(1,J16)*DTI0
                  DD1(2,J16)  = RIZ(2,J16) - R0ZS(2,J16) + V(2,J16)*DTI0
                  DD1(3,J16)  = RIZ(3,J16) - R0ZS(3,J16) + V(3,J16)*DTI0
!
                  RCOMP     = DD1(1,J16)**2.D0 + DD1(2,J16)**2.D0 +          &
     &                        DD1(3,J16)**2.D0
                  RHO1(J16)  = RCOMP**(1.D0/3.D0)
                  RHO1X(J16) = RHO1(J16)/(1.D0 + RHO1(J16)/L23)
 4160          CONTINUE
!
! ------------ The differences
!
               DO 4170 J17=NUM1+1, NUM3
!
! --------------- Compute the time differences in hours
!
                  DTJ0 = TIM(K+J17)
                  DTIJ = DTJ0 - DTI0
!
! --------------- Compute the position Vectors
!
                  RSUM = 0.D0
                  DO 4180 J18=1,(NH+1)**2
                     RJZ(1,J18)  =  RI(J17+K,1)*Z(J18)
                     RJZ(2,J18)  =  RI(J17+K,2)*Z(J18)
                     RJZ(3,J18)  =  RI(J17+K,3)*Z(J18)
!
                     RJZS(1,J18)  =  RI(J17+K,1)*ZS(J18)
                     RJZS(2,J18)  =  RI(J17+K,2)*ZS(J18)
                     RJZS(3,J18)  =  RI(J17+K,3)*ZS(J18)
!
! ------------------ Compute the separations
!    
                     DD2(1,J18)  = RJZ(1,J18) - R0ZS(1,J18) + V(1,J18)*DTJ0
                     DD2(2,J18)  = RJZ(2,J18) - R0ZS(2,J18) + V(2,J18)*DTJ0
                     DD2(3,J18)  = RJZ(3,J18) - R0ZS(3,J18) + V(3,J18)*DTJ0
!    
                     DD3(1,J18)  = RJZ(1,J18) - RJZS(1,J18) - V(1,J18)*DTIJ
                     DD3(2,J18)  = RJZ(2,J18) - RJZS(2,J18) - V(2,J18)*DTIJ
                     DD3(3,J18)  = RJZ(3,J18) - RJZS(3,J18) - V(3,J18)*DTIJ
!    
                     RCOMP    = DD2(1,J18)**2.D0 + DD2(2,J18)**2.D0 +     &
     &                          DD2(3,J18)**2.D0
                     RHO2(J18) = RCOMP**(1.D0/3.D0)
!    
                     RCOMP    = DD3(1,J18)**2.D0 + DD3(2,J18)**2.D0 +     &
     &                          DD3(3,J18)**2.D0
                     RHO3(J18) = RCOMP**(1.D0/3.D0)
!
                     RCOMP_V1(J18) = RHO1X(J18) +                         &
     &                             RHO2(J18)/(1.D0+RHO2(J18)/L23) -       &
     &                             RHO3(J18)/(1.D0+RHO3(J18)/L23) -       &
     &                             RHO4X(J18)
                     RSUM = RSUM + RCOMP_V1(J18)
 4180             CONTINUE
                  C(J15,J17) = CNALL*RSUM/(C__NUM**2.D0)
 4170           CONTINUE
 4150        CONTINUE
!
! ---------
!
            DO 4190 J19 = NUM1+1, NUM3
               DTI0 = TIM(K+J19)
               DO 4200 J20=1,(NH+1)**2              
                  RIZ(1,J20)  =  RI(K+J19,1)*Z(J20)
                  RIZ(2,J20)  =  RI(K+J19,2)*Z(J20)
                  RIZ(3,J20)  =  RI(K+J19,3)*Z(J20)
!
                  DD1(1,J20)  = RIZ(1,J20) - R0ZS(1,J20) + V(1,J20)*DTI0
                  DD1(2,J20)  = RIZ(2,J20) - R0ZS(2,J20) + V(2,J20)*DTI0
                  DD1(3,J20)  = RIZ(3,J20) - R0ZS(3,J20) + V(3,J20)*DTI0
!
                  RCOMP     = DD1(1,J20)**2.D0 + DD1(2,J20)**2.D0 +       &
     &                        DD1(3,J20)**2.D0
                  RHO1(J20)  = RCOMP**(1.D0/3.D0)
                  RHO1X(J20) = RHO1(J20)/(1.D0 + RHO1(J20)/L23)
 4200          CONTINUE
!
! -------------
!
               DO 4210 J21=1,NUM3
!
! --------------- Compute the time differences in hours
!
                  DTJ0 = TIM(K+J21)
                  DTIJ = DTJ0 - DTI0
!
! --------------- Compute the position Vectors
!
                  RSUM = 0.D0
                  DO 4220 J22 = 1, (NH+1)**2
                     RJZ(1,J22)  =  RI(J21+K,1)*Z(J22)
                     RJZ(2,J22)  =  RI(J21+K,2)*Z(J22)
                     RJZ(3,J22)  =  RI(J21+K,3)*Z(J22)
!
                     RJZS(1,J22)  =  RI(J21+K,1)*ZS(J22)
                     RJZS(2,J22)  =  RI(J21+K,2)*ZS(J22)
                     RJZS(3,J22)  =  RI(J21+K,3)*ZS(J22)
!
! ------------------ Compute the separations
!
                     DD2(1,J22)  = RJZ(1,J22) - R0ZS(1,J22) + V(1,J22)*DTJ0
                     DD2(2,J22)  = RJZ(2,J22) - R0ZS(2,J22) + V(2,J22)*DTJ0
                     DD2(3,J22)  = RJZ(3,J22) - R0ZS(3,J22) + V(3,J22)*DTJ0
!
                     DD3(1,J22)  = RJZ(1,J22) - RJZS(1,J22) - V(1,J22)*DTIJ
                     DD3(2,J22)  = RJZ(2,J22) - RJZS(2,J22) - V(2,J22)*DTIJ
                     DD3(3,J22)  = RJZ(3,J22) - RJZS(3,J22) - V(3,J22)*DTIJ
!
                     RCOMP    = DD2(1,J22)**2.D0 + DD2(2,J22)**2.D0 +        &
     &                          DD2(3,J22)**2.D0
                     RHO2(J22) = RCOMP**(1.D0/3.D0)
!
                     RCOMP    = DD3(1,J22)**2.D0 + DD3(2,J22)**2.D0 +        &
     &                          DD3(3,J22)**2.D0
                     RHO3(J22) = RCOMP**(1.D0/3.D0)
!
                     RCOMP_V1(J22) = RHO1X(J22) +                          &
     &                              RHO2(J22)/(1.D0+RHO2(J22)/L23) -       &
     &                              RHO3(J22)/(1.D0+RHO3(J22)/L23) -       &
     &                              RHO4X(J22)
                     RSUM = RSUM + RCOMP_V1(J22)
 4220             CONTINUE
!
                  C(J19,J21) = CNALL*RSUM/(C__NUM**2.D0)              
 4210          CONTINUE
 4190       CONTINUE
!
            C11(NUM1+1:NUM3,NUM1+1:NUM3) = C(NUM1+1:NUM3,NUM1+1:NUM3)
 4140    CONTINUE
      END IF ! if NSEG==1
!
      COV = C
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  COV_MAT_NHE  !#!#
