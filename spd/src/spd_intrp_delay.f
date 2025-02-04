      SUBROUTINE SPD_INTRP_DELAY ( STA_NAM, L_STA, SPD, ELEV, AZIM, &
     &                             ELEV_RATE, AZIM_RATE, MJD_OBS, TAI_OBS, &
     &                             SPD_DELAY, SPD_RATE, SPD_DELAY_DER_ZEN, &
     &                             SPD_RATE_DER_ZEN, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SPD_INTRP_DELAY computes slant path delay, slant path      *
! *   delay rate, partial derivative of slant path delay on the delay    *
! *   in zenith direction, and partial derivative of slant path delay on *
! *   the delay in zenith direction. These computations are based on     *
! *   the coefficients of expansion of path delay over 3D B-spline basis.*
! *                                                                      *
! *   It is assumed routine SPD_LOAD_BSPD was been called before, it has *
! *   computed the coefficients of expansion slant path delay over       *
! *   B-spline basis and written them into array of SPD objects for      *
! *   a number of stations, including the station STA_NAM in this        *
! *   routine.                                                           *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * STA_NAM ( CHARACTER ) -- Station name. The station should be from    *
! *                          the list supplied to SPD_LOAD_SPD.          *
! *   L_STA ( INTEGER*4 ) -- The number of stations for which the        *
! *                          B-spline expansion coefficients has been    *
! *                          computed by routine SPD_LOAD_BSPD.          *
! *     SPD ( SPD_DEL__TYPE ) -- Array of objects with Slant Path        *
! *                          Delays. The internal fields of SPD array    *
! *                          contains the coefficient of B-spline        *
! *                          expansion of path delay. Dimension: L_STA.  *
! *    ELEV ( REAL*8    ) -- Elevation angle. Units: radian.             *
! *    AZIM ( REAL*8    ) -- Azimuth counted from North to East.         *
! *                          Units: radian.                              *
! * ELEV_RATE ( REAL*8  ) -- Rate of changes of elevation angle.         *
! *                          Units: rad/s.                               *
! * AZIM_RATE ( REAL*8  ) -- Rate of changes of azimuth angle.           *
! *                          Units: rad/s.                               *
! * MJD_OBS ( INTEGER*4 ) -- Integer Modified Julian Date of the         *
! *                          observation at midnight. Units: days.       *
! * TAI_BEG ( INTEGER*4 ) -- TAI time since the midnight of the          *
! *                          observation. Units: sec.                    *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * SPD_DELAY         ( REAL*8     ) -- Slant path delay. Units: s.      *
! * SPD_RATE          ( REAL*8     ) -- Time derivative of slant path    *
! *                                     delay. Units: dimensionless      *
! * SPD_DELAY_DER_ZEN ( REAL*8     ) -- Partial derivative of slant path *
! *                                     delay at a given AZ,EL over      *
! *                                     the path delay in zenith         *
! *                                     direction using only wet         *
! *                                     contribution to path delay       *
! *                                     (mapping function).              *
! *                                     Units: dimensionless.            *
! * SPD_RATE_DER_ZEN  ( REAL*8     ) -- Partial derivative of slant path *
! *                                     rate at a given AZ,EL over       *
! *                                     the path delay in zenith         *
! *                                     direction using only wet         *
! *                                     contribution to path delay.      *
! *                                     Units: 1/sec                     *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 10-JUL-2014  SPD_INTRP_DELAY  v2.1 (c) L. Petrov 26-AUG-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      INTEGER*4  L_STA, MJD_OBS, IUER
      TYPE     ( SPD_DEL__TYPE ) :: SPD(L_STA)
      CHARACTER  STA_NAM*(*)
      CHARACTER  STR*128, STR1*128, STR2*128
      REAL*8     ELEV, AZIM, ELEV_RATE, AZIM_RATE, TAI_OBS, SPD_DELAY, &
     &           SPD_RATE, SPD_DELAY_DER_ZEN, SPD_RATE_DER_ZEN
      REAL*8     SPD_HZD, SPD_WZD, SPD_ZEN_DER, SPD_ZEN_RATE, &
     &           SPD_DER(3,2), SPD_ZEN(2), MAP_ISA
      REAL*4     SPD_VAL(2), SPD_VAL_R4, spd_der_r4(3)
      REAL*4     EPS_MAR
      PARAMETER  ( EPS_MAR = 1.0E-5 )
      REAL*4     ARGS(3)
      INTEGER*4  DIMS(3), INDS(3), J1, J2, ISTA, IND, IER
      REAL*4,    EXTERNAL :: VAL_1D_BSPL4
      REAL*8,    EXTERNAL :: DEL_ISA, DEL_ISA_DER, INV_MAP_ISA, VAL_1D_BSPL 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN8 
!
! --- Search the staton name
!
      ISTA = 0
      DO 410 J1=1,L_STA
         IF ( SPD(J1)%STA%NAME == STA_NAM ) ISTA = J1
 410  CONTINUE 
      IF ( ISTA == 0 ) THEN
           CALL ERR_LOG ( 2441, IUER, 'SPD_INTRP_DELAY', 'Cannot find '// &
     &         'station name '//STA_NAM//' in internal fields of the '// &
     &         'array of SPD objects' )
           RETURN 
      END IF
!
! --- Check whether the elevation is within the range
!
      IF ( ELEV < INV_MAP_ISA ( DBLE(SPD(ISTA)%MAP_ARR(SPD(ISTA)%ELV%N_EL) ) ) ) THEN
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR(1:12), FMT='(1PD12.4)' ) ELEV/DEG__TO__RAD
           CALL CLRCH ( STR1 )
           WRITE ( UNIT=STR1(1:6), FMT='(F6.3)' ) &
     &             INV_MAP_ISA ( DBLE(SPD(ISTA)%MAP_ARR(SPD(ISTA)%ELV%N_EL)) )/DEG__TO__RAD
           CALL CLRCH ( STR2 )
           WRITE ( UNIT=STR2(1:6), FMT='(F6.3)' ) &
     &             INV_MAP_ISA ( DBLE(SPD(ISTA)%MAP_ARR(1)) )/DEG__TO__RAD
           CALL ERR_LOG ( 2438, IUER, 'SPD_INTRP_DELAY', 'Elevation angle '// &
     &         'for station '//STA_NAM//' -- '// &
     &          STR(1:I_LEN(STR))//' deg is out of range [ '// &
     &          STR1(1:I_LEN(STR1))//', '//STR2(1:I_LEN(STR2))//' ] deg' )
      END IF
!
! --- Compute arguments for B-spline interpolation
!
      MAP_ISA = DEL_ISA ( ELEV )/ DEL_ISA ( P2I )
      ARGS(1) = MAP_ISA
      ARGS(2) = AZIM
      ARGS(3) = ( MJD_OBS*86400.0D0 + TAI_OBS ) - SPD(ISTA)%TIM_BEG 
!
! --- ... and dimenstin of an array with B-spline coefficients
!
      DIMS(1) = SPD(ISTA)%ELV%N_EL
      DIMS(2) = SPD(ISTA)%AZM%N_AZ
      DIMS(3) = SPD(ISTA)%N_TIM
!
! --- Now we compute the pivotal indices. An extra caution is to be 
! --- taken for assigning the index of the pivotal element, if the
! --- argument is close to the boundary of the expansion interval
!
      IF ( ABS(ARGS(1) - SPD(ISTA)%MAP_ARR(1)) < &
     &     EPS_MAR*(SPD(ISTA)%MAP_ARR(2) - SPD(ISTA)%MAP_ARR(1)) ) THEN
           INDS(1) = 1
         ELSE IF ( ABS(ARGS(1) - SPD(ISTA)%MAP_ARR(DIMS(1))) < &
     &             EPS_MAR*(SPD(ISTA)%MAP_ARR(2) - SPD(ISTA)%MAP_ARR(1)) ) THEN
           INDS(1) = DIMS(1) - 1
         ELSE 
           INDS(1) = INT ( (ARGS(1) - SPD(ISTA)%MAP_ARR(1))/ &
     &                     (SPD(ISTA)%MAP_ARR(2) - SPD(ISTA)%MAP_ARR(1)) ) &
     &               + 1
      END IF
!
      IF ( ABS(ARGS(2) - SPD(ISTA)%AZM%AZIM(1) ) < &
     &     EPS_MAR*(SPD(ISTA)%AZM%AZIM(2) - SPD(ISTA)%AZM%AZIM(1)) ) THEN
           INDS(2) = 1
        ELSE IF ( ABS(ARGS(2) - SPD(ISTA)%AZM%AZIM(DIMS(2))) < &
     &            EPS_MAR*(SPD(ISTA)%AZM%AZIM(2) - SPD(ISTA)%AZM%AZIM(1)) ) THEN
           INDS(2) = DIMS(2) - 1
        ELSE 
           INDS(2) = INT ( (ARGS(2) - SPD(ISTA)%AZM%AZIM(1))/ &
     &                     (SPD(ISTA)%AZM%AZIM(2) - SPD(ISTA)%AZM%AZIM(1)) ) &
     &               + 1
      END IF
!
      IF ( ABS(ARGS(3) - SPD(ISTA)%TIM_ARR(1) ) < &
     &     EPS_MAR*(SPD(ISTA)%TIM_ARR(2) - SPD(ISTA)%TIM_ARR(1)) ) THEN
           INDS(3) = 1
         ELSE IF ( ABS(ARGS(3) - SPD(ISTA)%TIM_ARR(DIMS(3))) < &
     &             EPS_MAR*(SPD(ISTA)%TIM_ARR(2) - SPD(ISTA)%TIM_ARR(1)) ) THEN
           INDS(3) = DIMS(3) - 1
         ELSE 
           INDS(3) = INT ( (ARGS(3) - SPD(ISTA)%TIM_ARR(1))/ &
     &                     (SPD(ISTA)%TIM_ARR(2) - SPD(ISTA)%TIM_ARR(1)) ) &
     &               + 1
      END IF
!
! --- Check the range once again and issue error messages if out of range
!
      IF ( INDS(1) < 1  .OR.  INDS(1) .GE. DIMS(1) ) THEN
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR(1:12), FMT='(1PD12.4)' ) ELEV/DEG__TO__RAD
           CALL CLRCH ( STR1 )
           WRITE ( UNIT=STR1(1:6), FMT='(F6.3)' ) &
     &             INV_MAP_ISA ( DBLE(SPD(ISTA)%MAP_ARR(SPD(ISTA)%ELV%N_EL)) )/DEG__TO__RAD
           CALL CLRCH ( STR2 )
           WRITE ( UNIT=STR2(1:6), FMT='(F6.3)' ) &
     &             INV_MAP_ISA ( DBLE(SPD(ISTA)%MAP_ARR(1)) )/DEG__TO__RAD
           CALL ERR_LOG ( 2438, IUER, 'SPD_INTRP_DELAY', 'Elevation angle '// &
     &                   'for station '//STA_NAM//' -- '// &
     &                    STR(1:I_LEN(STR))//' deg is out of range [ '// &
     &                    STR1(1:I_LEN(STR1))//', '//STR2(1:I_LEN(STR2))//' ] deg' )
           RETURN 
      END IF
!
      IF ( INDS(2) < 1  .OR.  INDS(2) .GE. DIMS(2) ) THEN
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR(1:12), FMT='(1PD12.4)' ) AZIM/DEG__TO__RAD
           CALL CLRCH ( STR1 )
           WRITE ( UNIT=STR1(1:12), FMT='(1PD12.4)' ) SPD(ISTA)%AZM%AZIM(1)/DEG__TO__RAD
           CALL CLRCH ( STR2 )
           WRITE ( UNIT=STR2(1:12), FMT='(1PD12.4)' ) &
     &             SPD(ISTA)%AZM%AZIM(SPD(ISTA)%AZM%N_AZ)/DEG__TO__RAD
           CALL ERR_LOG ( 2439, IUER, 'SPD_INTRP_DELAY', 'Azimuth angle '// &
     &         'for station '//STA_NAM//' -- '// &
     &          STR(1:I_LEN(STR))//' deg is out of range [ '// &
     &          STR1(1:I_LEN(STR1))//', '//STR2(1:I_LEN(STR2))//' ] deg' )
           RETURN 
      END IF
!
      IF ( INDS(3) < 1  .OR.  INDS(3) .GE. DIMS(3) ) THEN
           CALL ERR_LOG ( 2440, IUER, 'SPD_INTRP_DELAY', 'Trap of '// &
     &         'internal control: observation epoch is out of '// &
     &         'range of the slant path delay B-spline expansion '// &
     &         'for station '//STA_NAM )
           RETURN 
      END IF
!
! --- Cycle over constituents of path delay
! --- 1) totat path delay

! --- 2) wet constituent of path delay
!
      DO 420 J2=1,SPD(ISTA)%MOD%N_RFR
         CALL VAL_DER_3D_BSPL4 ( ARGS, SPD__MDEG, DIMS, INDS, &
     &                      SPD(ISTA)%MAP_ARR, &
     &                      SPD(ISTA)%AZM%AZIM, &
     &                      SPD(ISTA)%TIM_ARR, &
     &                      SPD(ISTA)%DELS(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,J2), &
     &                      SPD_VAL(J2), SPD_DER_R4 )
         SPD_DER(1,J2) = SPD_DER_R4(1)*DEL_ISA_DER(ELEV)/DEL_ISA(P2I)
         SPD_DER(2,J2) = SPD_DER_R4(2)
         SPD_DER(3,J2) = SPD_DER_R4(3)
!
! ------ Also compute zenith path delay
!
         SPD_ZEN(J2) = VAL_1D_BSPL4 ( ARGS(3), SPD(ISTA)%N_TIM, &
     &                                SPD__MDEG, INDS(3), &
     &                                SPD(ISTA)%TIM_ARR, &
     &                                SPD(ISTA)%ZEN_DEL(1-SPD__MDEG,J2) )
 420  CONTINUE 
!
      SPD_DELAY = SPD_VAL(SPD__TOT) 
      SPD_RATE  = SPD_DER(1,SPD__TOT)*ELEV_RATE + &
     &            SPD_DER(2,SPD__TOT)*AZIM_RATE + &
     &            SPD_DER(3,SPD__TOT)
      IND = -1
      IF ( SPD(ISTA)%MF%MF_NAME == SPD__WATS_STR ) THEN
           SPD_DELAY_DER_ZEN = SPD_VAL(SPD__WAT)/SPD_ZEN(SPD__WAT)
           SPD_RATE_DER_ZEN  = ( SPD_DER(1,SPD__WAT)*ELEV_RATE + &
     &                           SPD_DER(2,SPD__WAT)*AZIM_RATE + &
     &                           SPD_DER(3,SPD__WAT) )/ &
     &                           SPD_RATE/SPD_ZEN(SPD__WAT)
        ELSE IF ( SPD(ISTA)%MF%MF_NAME == SPD__TOTS_STR ) THEN
           SPD_DELAY_DER_ZEN = SPD_VAL(SPD__TOT)/SPD_ZEN(SPD__TOT)
           SPD_RATE_DER_ZEN  = SPD_RATE/SPD_ZEN(SPD__WAT)
        ELSE IF ( SPD(ISTA)%MF%MF_NAME == SPD__NMFW_STR .OR. &
     &            SPD(ISTA)%MF%MF_NAME == SPD__NMFH_STR .OR. &
     &            SPD(ISTA)%MF%MF_NAME == SPD__GL_STR        ) THEN
           IF ( ELEV < P2I - EPS_MAR ) THEN
                IND = IXMN8 ( SPD(ISTA)%MF%L_NOD, SPD(ISTA)%MF%EL_ARG, ELEV )
                CALL VAL_DER_1D_BSPL ( ELEV, SPD(ISTA)%MF%L_NOD, &
     &                                            SPD__MDEG, IND, &
     &                                            SPD(ISTA)%MF%EL_ARG, &
     &                                            SPD(ISTA)%MF%MF_SPL, &
     &                                            SPD_DELAY_DER_ZEN, &
     &                                            SPD_RATE_DER_ZEN  )
              ELSE
                SPD_DELAY_DER_ZEN = 1.0D0
                SPD_RATE_DER_ZEN  = 0.0D0
           END IF
      END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   write ( 6, 220 ) SPD(ISTA)%STA%NAME, ELEV/DEG__TO__RAD, SPD_VAL(SPD__WAT), SPD_ZEN(SPD__WAT), &
!     &              SPD_DELAY_DER_ZEN, IND, SPD(ISTA)%MF%MF_NAME
! 220  FORMAT ( 'SPD_ITRP_DELAY sta: ', A, ' Elev= ', F6.2, &
!     &         ' Spd_slant: ', 1PD13.6, ' Spd_zen: ', 1PD13.6,  &
!     &         ' MF = ', 0PF10.5, ' IND = ', I5, ' TYP = ', A )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_INTRP_DELAY  !#!#
