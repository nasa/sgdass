      SUBROUTINE NERS_GET_MATROT ( NERS, TIM_TAI, MAT_ROT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine NERS_GET_MATROT
! *                                                                      *
! * ### 05-APR-2016  NERS_GET_MATROT v2.1 (c)  L. Petrov 15-NOV-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heo.i'
      INCLUDE   'ners.i'
      TYPE     ( NERS__TYPE ) :: NERS
      REAL*8     TIM_TAI, MAT_ROT(3,3,0:2)
      REAL*8     EVEC(3,0:2), HEO_APS_VEC(3,0:2), DPSI, DEPS, &
     &           DTRS_TO_CRS_DEOP(3,3,3), DPSI_RATE, DEPS_RATE
      INTEGER*4  IUER
      REAL*8     TAI, XPOL, YPOL, UT1_M_TAI, XPOL_RATE, YPOL_RATE, UT1_RATE,   &
     &           S_ANG, S_ANG_RATE
      TYPE     ( HEO__STRUC ), POINTER :: HEO(:)
      INTEGER*4  MJD, IVRB, J1, J2, L_HEO,IER
!
      IVRB = 0
      IER = IUER
      CALL NERS_GET_EVEC  ( NERS, TIM_TAI, EVEC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4611, IUER, 'NERS_GET_MATROT', 'Error in computing '// &
     &         'the vector of perturbation Earth orientation' ) 
           RETURN 
      END IF
!
      IER = IUER
      CALL NERS_GET_HEO ( NERS, TIM_TAI, EVEC(3,0)/UT1__TO__E3, DPSI, DEPS, &
     &                    HEO_APS_VEC, DPSI_RATE, DEPS_RATE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4612, IUER, 'NERS_GET_MATROT', 'Error in computing '// &
     &         'the vector of perturbation Earth orientation' ) 
           RETURN 
      END IF
!
      MJD = J2000__MJD  + INT(TIM_TAI/86400.0D0)
      TAI = TIM_TAI - 86400.0D0*INT(TIM_TAI/86400.0D0)
      XPOL      = EVEC(2,0)
      YPOL      = EVEC(1,0)
      UT1_M_TAI = EVEC(3,0)/UT1__TO__E3
      XPOL_RATE = EVEC(2,1)
      YPOL_RATE = EVEC(1,1)
      UT1_RATE  = EVEC(3,1)/UT1__TO__E3
!
      L_HEO = NERS%FCS%L_HEO + NERS%FCS%L_HEOR
      ALLOCATE ( HEO(L_HEO) )
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
              HEO(L_HEO)%USE_VEL     = .FALSE.
 410       CONTINUE 
      END IF
      IF ( NERS%FCS%L_HEOR > 0 ) THEN
           DO 420 J2=1,NERS%FCS%L_HEOR
              L_HEO = L_HEO + 1
              HEO(L_HEO)%PHAS = NERS%FCS%HEOR_ARG(J2,1)
              HEO(L_HEO)%FREQ = NERS%FCS%HEOR_ARG(J2,2)
              HEO(L_HEO)%ACCL = NERS%FCS%HEOR_ARG(J2,3)
              HEO(L_HEO)%ROTANG(1,2) = NERS%FCS%HEOR_AMP(J2,1,1)
              HEO(L_HEO)%ROTANG(2,2) = NERS%FCS%HEOR_AMP(J2,2,1) 
              HEO(L_HEO)%ROTANG(3,2) = NERS%FCS%HEOR_AMP(J2,1,2) 
              HEO(L_HEO)%ROTANG(4,2) = NERS%FCS%HEOR_AMP(J2,2,2) 
              HEO(L_HEO)%USE_VEL     = .TRUE.
 420       CONTINUE 
      END IF
!
      IER = IUER
      CALL NERS_ERM_NA ( 1, IVRB, PREC__CAPITAINE2003, NUT__MHB2000, &
     &                   MJD, TAI, XPOL, YPOL, UT1_M_TAI,  &
     &                   XPOL_RATE, YPOL_RATE, UT1_RATE,   &
     &                   S_ANG, S_ANG_RATE, &
     &                   L_HEO, NERS%FCS%TAI_HEO_EPOCH - 43200.0D0, HEO, &
     &                   MAT_ROT(1,1,0), MAT_ROT(1,1,1), MAT_ROT(1,1,2), &
     &                   DTRS_TO_CRS_DEOP, IER )
      DEALLOCATE ( HEO )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4613, IUER, 'NERS_GET_MATROT', 'Error in '// &
     &         'computing the vector of of the Earth rotation' ) 
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  NERS_GET_MATROT  !#!#
