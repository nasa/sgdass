      SUBROUTINE NERS_GET_HEO ( NERS, TIM, UT1_M_TAI, &
     &                          DPSI, DEPS, HEO_APS_VEC, &
     &                          DPSI_RATE, DEPS_RATE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  NERS_GET_HEO
! *                                                                      *
! *  ### 05-APR-2016  NERS_GET_HEO  v1.4 (c) L. Petrov  21-APR-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heo.i'
      INCLUDE   'ners.i'
      TYPE     ( HEO__STRUC ), POINTER :: HEO(:)
      TYPE     ( NERS__TYPE ) :: NERS
      INTEGER*4  MODE, IUER 
      REAL*8     TIM, UT1_M_TAI, DPSI, DEPS, HEO_APS_VEC(3,0:2), &
     &           DPSI_RATE, DEPS_RATE
      REAL*8     TARG_TAI, SARG_TAI_2PI, TARG_TDB, TDB, &
     &           E1, E2, E1_RATE, E2_RATE, CROSS_NUT_SCL
      INTEGER*4  J1, J2, L_HEO, MJD, IER 
      REAL*8     TAI, UTC_M_TAI, UT1_M_TDB
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, TIM_TO_DATE*23
!
      HEO_APS_VEC = 0.0D0
      L_HEO = NERS%FCS%L_HEO + NERS%FCS%L_HEOR
      ALLOCATE ( HEO(L_HEO), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5721, IUER, 'NERS_GET_HEO', 'Error in '// &
     &         'an attempt to allocate memory for array HEO' )
           RETURN
      END IF
      L_HEO = 0
      IF ( NERS%FCS%L_HEO > 0 ) THEN
           DO 410 J1=1,NERS%FCS%L_HEO
              L_HEO = L_HEO + 1
              HEO(L_HEO)%PHAS = NERS%FCS%HEO_ARG(J1,1)   
              HEO(L_HEO)%FREQ = NERS%FCS%HEO_ARG(J1,2)   
              HEO(L_HEO)%ACCL = NERS%FCS%HEO_ARG(J1,3)   
              HEO(L_HEO)%ROTANG(1,1) = NERS%FCS%HEO_AMP(J1,1,1) 
              HEO(L_HEO)%ROTANG(2,1) = NERS%FCS%HEO_AMP(J1,2,1) 
              HEO(L_HEO)%ROTANG(3,1) = NERS%FCS%HEO_AMP(J1,1,2) 
              HEO(L_HEO)%ROTANG(4,1) = NERS%FCS%HEO_AMP(J1,2,2) 
              HEO(L_HEO)%ROTANG(1,2) = 0.0D0
              HEO(L_HEO)%ROTANG(2,2) = 0.0D0
              HEO(L_HEO)%ROTANG(3,2) = 0.0D0
              HEO(L_HEO)%ROTANG(4,2) = 0.0D0
              HEO(L_HEO)%USE_VEL = .FALSE.
 410       CONTINUE 
      END IF
      IF ( NERS%FCS%L_HEOR > 0 ) THEN
           DO 420 J2=1,NERS%FCS%L_HEOR
              L_HEO = L_HEO + 1
              HEO(L_HEO)%PHAS = NERS%FCS%HEOR_ARG(J2,1)
              HEO(L_HEO)%FREQ = NERS%FCS%HEOR_ARG(J2,2)
              HEO(L_HEO)%ACCL = NERS%FCS%HEOR_ARG(J2,3)
              HEO(L_HEO)%ROTANG(1,1) = 0.0D0
              HEO(L_HEO)%ROTANG(2,1) = 0.0D0
              HEO(L_HEO)%ROTANG(3,1) = 0.0D0
              HEO(L_HEO)%ROTANG(4,1) = 0.0D0
              HEO(L_HEO)%ROTANG(1,2) = NERS%FCS%HEOR_AMP(J2,1,1)
              HEO(L_HEO)%ROTANG(2,2) = NERS%FCS%HEOR_AMP(J2,2,1) 
              HEO(L_HEO)%ROTANG(3,2) = NERS%FCS%HEOR_AMP(J2,1,2) 
              HEO(L_HEO)%ROTANG(4,2) = NERS%FCS%HEOR_AMP(J2,2,2) 
              HEO(L_HEO)%USE_VEL = .TRUE.
 420       CONTINUE 
      END IF
!
! --- Compute vector of perturbation Earth orientation and its the first
! --- and the second time derivative
!
      IER = IUER
      CALL NERS_APPLY_HEO ( TIM, NERS%FCS%TAI_HEO_EPOCH, &
     &                      L_HEO, HEO, HEO_APS_VEC(1,0), HEO_APS_VEC(1,1), &
     &                      HEO_APS_VEC(1,2), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5722, IUER, 'NERS_GET_HEO', 'Error in '// &
     &         'an attempt to apply harmonic Earth orientation '// &
     &         'variations' )
           RETURN
      END IF
      DEALLOCATE ( HEO )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  NERS_GET_HEO  !#!  
