      SUBROUTINE NERS_GET_EVEC ( NERS, TIM, EVEC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine NERS_GET_EVEC
! *                                                                      *
! *  ### 05-APR-2016  NERS_GET_EVEC  v2.0 (c)  L. Petrov 31-AUG-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'ners.i'
      TYPE     ( NERS__TYPE ) :: NERS
      REAL*8     TIM, EVEC(3,0:2)
      INTEGER*4  IUER 
      INTEGER*4  J1, J2, MJD, IER
      REAL*8     TAI, E3_HAR, E3_DOT_HAR, E3_DT2_HAR 
      CHARACTER  STR1*22, STR2*22, STR3*22
      REAL*8,    EXTERNAL :: EBSPL_VAL_R8, EBSPL_DER_R8, EBSPL_DR2_R8
!
      EVEC = 0.0D0
!
! --- Check the date
!
      IF ( TIM .GE. NERS%FCS%ARG_C(1) .AND. TIM .LE. NERS%FCS%ARG_C(NERS%FCS%NC-NERS__EDG_NODES) ) THEN
!
! -------- Use C04 series
!
           DO 410 J1=1,3
              EVEC(J1,0) = EBSPL_VAL_R8 ( NERS%FCS%NC, NERS__MDEG, TIM, NERS%FCS%ARG_C, &
     &                                    NERS%FCS%BSPL_C(1-NERS__MDEG,J1) )
              EVEC(J1,1) = EBSPL_DER_R8 ( NERS%FCS%NC, NERS__MDEG, TIM, NERS%FCS%ARG_C, &
     &                                    NERS%FCS%BSPL_C(1-NERS__MDEG,J1) )
              EVEC(J1,2) = EBSPL_DR2_R8 ( NERS%FCS%NC, NERS__MDEG, TIM, NERS%FCS%ARG_C, &
     &                                    NERS%FCS%BSPL_C(1-NERS__MDEG,J1) )
 410       CONTINUE 
        ELSE IF ( TIM .LE. NERS%FCS%ARG_3(NERS%FCS%NK_3) ) THEN
!
! -------- Use the forecast series
!
           DO 420 J2=1,2
              EVEC(J2,0) = EBSPL_VAL_R8 ( NERS%FCS%NK_12, NERS__MDEG, TIM, NERS%FCS%ARG_12, &
     &                                    NERS%FCS%BSPL_E12(1-NERS__MDEG,J2) )
              EVEC(J2,1) = EBSPL_DER_R8 ( NERS%FCS%NK_12, NERS__MDEG, TIM, NERS%FCS%ARG_12, &
     &                                    NERS%FCS%BSPL_E12(1-NERS__MDEG,J2) )
              EVEC(J2,2) = EBSPL_DR2_R8 ( NERS%FCS%NK_12, NERS__MDEG, TIM, NERS%FCS%ARG_12, &
     &                                    NERS%FCS%BSPL_E12(1-NERS__MDEG,J2) )
 420       CONTINUE 
!
           EVEC(3,0) = EBSPL_VAL_R8 ( NERS%FCS%NK_3, NERS__MDEG, TIM, NERS%FCS%ARG_3, &
     &                                NERS%FCS%BSPL_E3 )
           EVEC(3,1) = EBSPL_DER_R8 ( NERS%FCS%NK_3, NERS__MDEG, TIM, NERS%FCS%ARG_3, &
     &                                NERS%FCS%BSPL_E3 )
           EVEC(3,2) = EBSPL_DR2_R8 ( NERS%FCS%NK_3, NERS__MDEG, TIM, NERS%FCS%ARG_3, &
     &                                NERS%FCS%BSPL_E3 )
           MJD = J2000__MJD  + INT(TIM/86400.0D0)
           TAI = TIM - 86400.0D0*INT(TIM/86400.0D0)
           IF ( NERS%FCS%E3Z_APR_MOD == NERS__E3Z_D93 ) THEN
                CALL E3ZT_DICKMAN1993  ( 0, MJD, TAI, E3_HAR, E3_DOT_HAR, E3_DT2_HAR ) 
               ELSE IF ( NERS%FCS%E3Z_APR_MOD == NERS__E3Z_RE2014 ) THEN
                 CALL E3ZT_RE2014 ( 0, MJD, TAI, E3_HAR, E3_DOT_HAR, E3_DT2_HAR ) 
               ELSE IF ( NERS%FCS%E3Z_APR_MOD == NERS__E3Z_NONE   ) THEN
                 E3_HAR     = 0.0D0
                 E3_DOT_HAR = 0.0D0
                 E3_DT2_HAR = 0.0D0
           END IF
           EVEC(3,0) = EVEC(3,0) + E3_HAR
           EVEC(3,1) = EVEC(3,1) + E3_DOT_HAR
           EVEC(3,2) = EVEC(3,2) + E3_DT2_HAR
         ELSE
           WRITE ( UNIT=STR1, FMT='(1PD22.15)' ) TIM
           WRITE ( UNIT=STR2, FMT='(1PD22.15)' ) NERS%FCS%ARG_C(1) 
           WRITE ( UNIT=STR3, FMT='(1PD22.15)' ) NERS%FCS%ARG_3(NERS%FCS%NK_3) 
           CALL ERR_LOG ( 5711, IUER, 'NERS_GET_EVEC', 'Argument TIM is out of '// &
     &          'range: TIM= '//STR1//', valid range: [ '//STR2//', '//STR3 )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  NERS_GET_EVEC !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE EOP_NERS_UTCMTAI ( TIM, NERS, UTC_M_TAI, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine EOP_NERS_UTCMTAI
! *                                                                      *
! * ### 05-APR-2016  EOP_NERS_UTCMTAI v2.0 (c) L. Petrov 31-AUG-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'ners.i'
      TYPE     ( NERS__TYPE ) :: NERS
      REAL*8     TIM, UTC_M_TAI, TAI
      CHARACTER  STR_DATE*32
      INTEGER*4  IUER
      INTEGER*4  MJD, J1
      CHARACTER  MJDSEC_TO_DATE*30
!
      IF ( TIM .LE. NERS%FCS%ARG_UTC_M_TAI(1) ) THEN
           MJD = J2000__MJD  + INT(NERS%FCS%ARG_UTC_M_TAI(1)/86400.0D0)
           TAI = NERS%FCS%ARG_UTC_M_TAI(1) - &
     &           86400.0D0*INT(NERS%FCS%ARG_UTC_M_TAI(1)/86400.0D0)
           STR_DATE = MJDSEC_TO_DATE ( MJD, TAI, IUER ) 
           CALL ERR_LOG ( 5731, IUER, 'EOP_NERS_UTCMTAI', 'Time epoch is '// &
     &         'too early. The first supported epoch is '//STR_DATE(1:19) )
           RETURN 
      END IF
!
      IF ( TIM > NERS%FCS%ARG_UTC_M_TAI(NERS%FCS%NJ) ) THEN
           MJD = J2000__MJD  + INT(NERS%FCS%ARG_UTC_M_TAI(NERS%FCS%NJ)/86400.0D0)
           TAI = NERS%FCS%ARG_UTC_M_TAI(NERS%FCS%NJ) - &
     &           86400.0D0*INT(NERS%FCS%ARG_UTC_M_TAI(NERS%FCS%NJ)/86400.0D0)
           STR_DATE = MJDSEC_TO_DATE ( MJD, TAI, IUER ) 
           CALL ERR_LOG ( 5732, IUER, 'EOP_NERS_UTCMTAI', 'Time epoch is '// &
     &         'too far in the future. The last supported epoch is '//STR_DATE(1:19) )
           RETURN 
      END IF
!
      UTC_M_TAI = 0.0D0
      DO 410 J1=1,NERS%FCS%NJ
         IF ( TIM .GE. NERS%FCS%ARG_UTC_M_TAI(J1) ) THEN
              UTC_M_TAI = NERS%FCS%BSPL_UTC_M_TAI(J1) 
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  EOP_NERS_UTCMTAI  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_NERS_MATROT ( TIM, NERS, MAT_ROT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_NERS_MATROT
! *                                                                      *
! * ### 05-APR-2016  GET_NERS_MATROT v1.2 (c) L. Petrov  27-OCT-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'vtd.i'
      TYPE     ( NERS__TYPE ) :: NERS
      REAL*8     TIM, MAT_ROT(3,3,0:2)
      REAL*8     EVEC(3,0:2), HEO_APS_VEC(3,0:2), DPSI, DEPS, &
     &           DTRS_TO_CRS_DEOP(3,3,3), PTRS_TO_CRS_DEOP(3,3,3)
      INTEGER*4  IUER
      REAL*8     TAI, XPOL, YPOL, UT1_M_TAI, XPOL_RATE, YPOL_RATE, UT1_RATE,   &
     &           S_ANG, S_ANG_RATE, DPSI_RATE, DEPS_RATE
      TYPE     ( HEO__STRUC ), POINTER :: HEO(:)
      INTEGER*4  MJD, IVRB, J1, J2, L_HEO,IER
!
      IVRB = 0
      CALL ERR_PASS ( IUER, IER )
      CALL NERS_GET_EVEC ( NERS, TIM, EVEC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5741, IUER, 'GET_NERS_MATROT', 'Error in computing '// &
     &         'the vector of perturbation Earth orientation' ) 
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL NERS_GET_HEO ( NERS, TIM, EVEC(3,0)/UT1__TO__E3, DPSI, DEPS, &
     &                    HEO_APS_VEC, DPSI_RATE, DEPS_RATE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5742, IUER, 'GET_NERS_MATROT', 'Error in computing '// &
     &         'the vector of perturbation Earth orientation' ) 
           RETURN 
      END IF
!
      MJD = J2000__MJD  + INT(TIM/86400.0D0)
      TAI = TIM - 86400.0D0*INT(TIM/86400.0D0)
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
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_ERM_NA ( 1, IVRB, PREC__CAPITAINE2003, NUT__MHB2000, &
     &                  NUT__GDS_NO, VTD__NONE, NERS, MJD, TAI, &
     &                  XPOL,      YPOL,      UT1_M_TAI,  &
     &                  XPOL_RATE, YPOL_RATE, UT1_RATE,   &
     &                  S_ANG, S_ANG_RATE, &
     &                  L_HEO, NERS%FCS%TAI_HEO_EPOCH - 43200.0D0, HEO, &
     &                  MAT_ROT(1,1,0), MAT_ROT(1,1,1), MAT_ROT(1,1,2), &
     &                  PTRS_TO_CRS_DEOP, DTRS_TO_CRS_DEOP, IER )
      DEALLOCATE ( HEO )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5743, IUER, 'GET_NERS_MATROT', 'Error in computing '// &
     &         'the vector of perturbation Earth orientation' ) 
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_NERS_MATROT  !#!  
!
! ------------------------------------------------------------------------
!
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
      CALL EOP_NERS_UTCMTAI ( TIM, NERS, UTC_M_TAI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5723, IUER, 'NERS_GET_HEO', 'Error in '// &
     &         'computing function UTC_M_TAI' )
           RETURN
      END IF
      MJD = J2000__MJD  + INT(TIM/86400.0D0)
      TAI = TIM - 86400.0D0*INT(TIM/86400.0D0)
      CALL TAI_TO_TDB  ( MJD, TAI, TDB )
      TARG_TDB = TIM + (TDB - TAI)
      UT1_M_TDB = UT1_M_TAI - (TDB - TAI)
!
      CALL HEO_MHB2000  ( 4, TARG_TDB, UT1_M_TDB, E1, E2, &
     &                    DPSI, DEPS, E1_RATE, E2_RATE, &
     &                    DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  NERS_GET_HEO  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_APPLY_HEO ( TIM, HEO_EPOCH_SEC, L_HEO, HEO, &
     &                            HEO_VEC, HEO_VEC_DER1, HEO_VEC_DER2, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  NERS_APPLY_HEO  computes the vector of perturbing         *
! *   rotation due to Harmonic Earth Orientation variations using the    *
! *   model presented as an expansion over quasi-harmonic series.        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *          TIM ( REAL*8    ) -- TAI time in seconds elapsed since      *
! *                                 J2000.0 epoch ( 01 January 2000,     *
! *                                 12:00 ) for the moment for which     *
! *                                 the variations are computed.         *
! *  HEO_EPOCH_SEC ( REAL*8    ) -- Time in seconds elapsed since        *
! *                                 J2000.0 epoch ( 01 January 2000,     *
! *                                 12:00 ) of the amplitudes of the     *
! *                                 variations (which change with time). *
! *                                 since J2000.0 epoch.                 *
! *          L_HEO ( INTEGER*4 ) -- The number of harmonic constituents. *
! *            HEO ( RECORD    ) -- Array of HEO records with harmonic   *
! *                                 EOP constituents. Dimension: L_HEO.  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      HEO_VEC ( REAL*8     ) -- The vector of the perturbation Earth  *
! *                                rotation due to harmonic variations.  *
! *                                Dimension: 3. Units: radians.         *
! *                                Components: E1, E2, E3.               *
! * HEO_VEC_DER1 ( REAL*8     ) -- The first time derivative of the      *
! *                                vector of the perturbation Earth      *
! *                                rotation due to harmonic variations.  *
! *                                Dimension: 3. Units: rad/sec.         *
! *                                Components: E1, E2, E3.               *
! * HEO_VEC_DER2 ( REAL*8     ) -- The second time derivative of the     *
! *                                vector of the perturbation Earth      *
! *                                rotation due to harmonic variations.  *
! *                                Dimension: 3. Units: rad/sec^2.       *
! *                                Components: E1, E2, E3.               *
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
! * ### 01-OCT-2003  NERS_APPLY_HEO  v2.1 (c)  L. Petrov 11-MAR-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heo.i'
      REAL*8     TIM, HEO_EPOCH_SEC, HEO_VEC(3), &
     &           HEO_VEC_DER1(3), HEO_VEC_DER2(3)
      INTEGER*4  L_HEO, IUER
      TYPE     ( HEO__STRUC ) HEO(L_HEO)
      REAL*8     ARG, VEL, DCOS_ARG, DSIN_ARG, PMC_AMP, PMS_AMP, &
     &           E3C_AMP, E3S_AMP, TIM_ARG
      REAL*8     PI, PI2, P2I
      PARAMETER  ( PI=3.141592653589793D0, PI2=PI*2.D0, P2I = PI/2.0D0 )
      INTEGER*4  J1
!
! --- Initialization
!
      CALL NOUT_R8 ( 3, HEO_VEC      )
      CALL NOUT_R8 ( 3, HEO_VEC_DER1 )
      CALL NOUT_R8 ( 3, HEO_VEC_DER2 )
      TIM_ARG = TIM - 43200.0D0
!
! --- Summ all waves
!
      DO 410 J1=1,L_HEO
         ARG = (0.5D0*HEO(J1)%ACCL*TIM_ARG + HEO(J1)%FREQ)*TIM_ARG + &
     &         HEO(J1)%PHAS 
         VEL = HEO(J1)%ACCL*TIM_ARG + HEO(J1)%FREQ
!
         DCOS_ARG = DCOS(ARG)
         DSIN_ARG = DSIN(ARG)
!
         IF ( HEO(J1)%USE_VEL ) THEN
              PMC_AMP = HEO(J1)%ROTANG(HEO__PMC,HEO__ANG) + &
     &                  HEO(J1)%ROTANG(HEO__PMC,HEO__VEL)*(TIM_ARG - HEO_EPOCH_SEC)
              PMS_AMP = HEO(J1)%ROTANG(HEO__PMS,HEO__ANG) + &
     &                  HEO(J1)%ROTANG(HEO__PMS,HEO__VEL)*(TIM_ARG - HEO_EPOCH_SEC)
              E3C_AMP = HEO(J1)%ROTANG(HEO__E3C,HEO__ANG) + &
     &                  HEO(J1)%ROTANG(HEO__E3C,HEO__VEL)*(TIM_ARG - HEO_EPOCH_SEC)
              E3S_AMP = HEO(J1)%ROTANG(HEO__E3S,HEO__ANG) + &
     &                  HEO(J1)%ROTANG(HEO__E3S,HEO__VEL)*(TIM_ARG - HEO_EPOCH_SEC)
            ELSE
              PMC_AMP = HEO(J1)%ROTANG(HEO__PMC,HEO__ANG)
              PMS_AMP = HEO(J1)%ROTANG(HEO__PMS,HEO__ANG)
              E3C_AMP = HEO(J1)%ROTANG(HEO__E3C,HEO__ANG)
              E3S_AMP = HEO(J1)%ROTANG(HEO__E3S,HEO__ANG)
         END IF
!
         HEO_VEC(1) = HEO_VEC(1) + PMC_AMP*DCOS_ARG + PMS_AMP*DSIN_ARG
         HEO_VEC(2) = HEO_VEC(2) + PMC_AMP*DSIN_ARG - PMS_AMP*DCOS_ARG
         HEO_VEC(3) = HEO_VEC(3) + E3C_AMP*DCOS_ARG + E3S_AMP*DSIN_ARG
!
         HEO_VEC_DER1(1) = HEO_VEC_DER1(1) + &
     &                     VEL*(-PMC_AMP*DSIN_ARG + PMS_AMP*DCOS_ARG)
         HEO_VEC_DER1(2) = HEO_VEC_DER1(2) + &
     &                     VEL*( PMC_AMP*DCOS_ARG + PMS_AMP*DSIN_ARG)
         HEO_VEC_DER1(3) = HEO_VEC_DER1(3) + &
     &                     VEL*(-E3C_AMP*DSIN_ARG + E3S_AMP*DCOS_ARG)
!
         HEO_VEC_DER2(1) = HEO_VEC_DER2(1) - &
     &                     VEL**2*(PMC_AMP*DCOS_ARG + PMS_AMP*DSIN_ARG)
         HEO_VEC_DER2(2) = HEO_VEC_DER2(2) - &
     &                     VEL**2*(PMC_AMP*DSIN_ARG - PMS_AMP*DCOS_ARG)
         HEO_VEC_DER2(3) = HEO_VEC_DER2(3) - &
     &                     VEL**2*(E3C_AMP*DCOS_ARG + E3S_AMP*DSIN_ARG)
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  NERS_APPLY_HEO  !#!#
