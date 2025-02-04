      SUBROUTINE VTD_MOMENT ( SOU_NAM, MJD, TAI, VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_MOMENT  computes intermediate parameters for          *
! *   calculation of the VLBI theoretical delay and its partial          *
! *   derivatives which depends only on time, but do not depend on a     *
! *   station or a source.                                               *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  SOU_NAM ( CHARACTER ) -- The name of the observed source.           *
! *      MJD ( INTEGER*4 ) -- Modified Julian date of the midnight of    *
! *                           the observation.                           *
! *      TAI ( INTEGER*4 ) -- Time of the observations in seconds at     *
! *                           time scale TAI elapsed from the midnight.  *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *     VTD ( RECORD    ) -- Object which keeps configuration and data   *
! *                          related to VLBI Theoretical Delay (VTD)     *
! *                          package.                                    *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the vase of errors. *
! *                                                                      *
! *  ### 27-JAN-2004   VTD_MOMENT  v5.3 (c)  L. Petrov  24-AUG-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      CHARACTER  SOU_NAM*(*)
      INTEGER*4  MJD, IUER
      REAL*8     TAI
      TYPE     ( VTD__TYPE ) :: VTD
      CHARACTER  STR*128
      LOGICAL*1  FL_PLAN_ABR
      REAL*8     TIM_MOM, DT_SEC, TIM_SOU_BEG, TIM_SOU_END
      PARAMETER  ( DT_SEC = 0.02D0 )
      REAL*8     E3, E3_DOT, E3_DT2, MAT_TMP(3,3), TARG, TIM_ARG, TARG_TAI
      REAL*8     ERM_VAL(3), ERM_DER(3), ERM_DR2(3), &
     &           ERM_ERR(3), ERM_ER1(3), ERM_ER2(3), PARS(NERS__MPAR), &
     &           SAT_POS_TRS(3), SAT_VEL_TRS(3), POS_COMP_VEL(3), DST
      INTEGER*4  IPAR, KNOT, ISOU, IND_NZO, J1, J2, J3, J4, J5, J6, J7, &
     &           J8, J9, J10, J11, L_PAR, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      REAL*8,    EXTERNAL :: FSPL8, DSPL8, BSPL_VAL, BSPL_DER, BSPL_DR2, DP_VV_V
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN8, VTD_SOU_INDEX
!
      FL_PLAN_ABR = .FALSE.
      CALL GETENVAR ( 'VTD_PLAN_ABR', STR )
      IF ( STR(1:1) == 'Y' .OR. STR(1:1) == 'y' ) THEN
           FL_PLAN_ABR = .TRUE.
      END IF
!
      IF ( MJD     == VTD%MOM%MJD                   .AND. &
     &     SOU_NAM == VTD%MOM%SOU_NAM               .AND. &
     &     DABS(TAI - VTD%MOM%TAI ) < VTD__TIME_TOL       ) THEN
!
! -------- Nothing to do
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Initialize the date of station update
!
      DO 410 J1=1,VTD__M_STA
         VTD%STA(J1)%MJD = -1999999999
         VTD%STA(J1)%TAI = -1.D30
 410  CONTINUE
!
! --- Store the date of the observation
!
      VTD%MOM%MJD = MJD
      VTD%MOM%TAI = TAI
      VTD%MOM%SOU_NAM = SOU_NAM
!
! --- Check whether EOP file has been loaded
!
      IF ( ILEN(VTD%CONF%FINAM_AEM) > 0 ) THEN
!
! -------- Read a priori Earth rotation model
!
           CALL ERR_PASS ( IUER, IER ) 
           CALL READ_AEM ( VTD%CONF%FINAM_AEM, VTD%AEM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2221, IUER, 'VTD_MOMENT', 'Error in '// &
     &              'an attempt to read a priori Earth rotation model '// &
     &              'file '//VTD%CONF%FINAM_AEM )
                RETURN
           END IF
           VTD%STATUS_AEM = VTD__LOAD
         ELSE IF ( VTD%NERS%FCS_STATUS == NERS__LOAD .OR. &
     &             VTD%NERS%FCS_STATUS == NERS__INIT      ) THEN
           CALL ERR_PASS     ( IUER, IER ) 
           CALL NERS_GET_EOP ( VTD%NERS, (MJD - J2000__MJD)*86400.0D0 + TAI, &
     &                         'eop3r', NERS__MPAR, L_PAR, PARS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2222, IUER, 'VTD_MOMENT', 'Error in '// &
     &              'getting the Earth orientation parameters using NERS' )
                RETURN
           END IF
!
           VTD%MOM%XPL       = PARS(1)*ARCSEC__TO__RAD
           VTD%MOM%YPL       = PARS(2)*ARCSEC__TO__RAD
           VTD%MOM%UT1_M_TAI = PARS(3)
           VTD%MOM%XPL_RATE  = PARS(4)*ARCSEC__TO__RAD/86400.0D0
           VTD%MOM%YPL_RATE  = PARS(5)*ARCSEC__TO__RAD/86400.0D0
           VTD%MOM%UT1_RATE  = PARS(6)/86400.0D0
         ELSE 
           IF ( VTD%UEOP%STATUS .NE. UEOP__LOADED ) THEN
                CALL ERR_LOG ( 2223, IUER, 'VTD_MOMENT', 'EOP series '// &
     &              'has not loaded in the VTD data structure' )
                RETURN
           END IF
           TIM_MOM = ( VTD%MOM%MJD - J2000__MJD - 0.5D0)*86400.0D0 + VTD%MOM%TAI
!
! -------- Compute the node of EOP -- the index of the table with the Earth
! -------- orientation parameters which just precedes the date of the observation
!
           KNOT = IXMN8 ( VTD%UEOP%NP, VTD%UEOP%TIM, TIM_MOM )
           IF ( KNOT .EQ. -1 ) THEN
                CALL ERR_LOG ( 2224, IUER, 'VTD_MOMENT', 'Requested moment '// &
     &              'of time '//MJDSEC_TO_DATE( MJD, TAI, IER )// &
     &              ' is too early. The file with the EOP series '// &
     &              VTD%CONF%FINAM_EOP(1:I_LEN(VTD%CONF%FINAM_EOP))// &
     &              ' starts on '// &
     &              MJDSEC_TO_DATE( VTD%UEOP%MJD_BEG, VTD%UEOP%TAI_BEG, IER ) )
                RETURN
           END IF
!
           IF ( KNOT .EQ. -2 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( VTD%NERS%FCS_STATUS, STR )
                CALL ERR_LOG ( 2225, IUER, 'VTD_MOMENT', 'Requested '// &
     &              'moment of time '//MJDSEC_TO_DATE( MJD, TAI, IER )// &
     &              ' is too late. The file with the EOP series '// &
     &              VTD%CONF%FINAM_EOP(1:I_LEN(VTD%CONF%FINAM_EOP))// &
     &              ' ends on '// &
     &              MJDSEC_TO_DATE( VTD%UEOP%MJD_END, VTD%UEOP%TAI_END, IER )// &
     &              ' VTD%NERS%FCS_STATUS = '//TRIM(STR) )
                RETURN
           END IF
!
           IF ( VTD%UEOP%NP == 0 ) THEN
!
! ------------- No apriori EOP series
!
                VTD%MOM%XPL = 0.0D0
                VTD%MOM%YPL = 0.0D0
                VTD%MOM%UT1_M_TAI = 0.0D0
                VTD%MOM%XPL_RATE  = 0.0D0
                VTD%MOM%YPL_RATE  = 0.0D0
                VTD%MOM%UT1_RATE  = 0.0D0
              ELSE
!
! ------------- Interpolate apriori EOP in the form of time series using
! ------------- cubic spline interpolation. The coefficients of the 
! ------------- interpolating spline has already been computed
!
                VTD%MOM%XPL = FSPL8 ( TIM_MOM, VTD%UEOP%NP, VTD%UEOP%TIM, &
     &                                VTD%UEOP%VAL(1,UEOP__XPL), KNOT, &
     &                                VTD%UEOP%SPL(1,UEOP__XPL) )
                VTD%MOM%YPL = FSPL8 ( TIM_MOM, VTD%UEOP%NP, VTD%UEOP%TIM, &
     &                                VTD%UEOP%VAL(1,UEOP__YPL), KNOT, &
     &                                VTD%UEOP%SPL(1,UEOP__YPL) )
                VTD%MOM%UT1_M_TAI = FSPL8 ( TIM_MOM, VTD%UEOP%NP, &
     &                                      VTD%UEOP%TIM, &
     &                                      VTD%UEOP%VAL(1,UEOP__UT1_M_TAI), &
     &                                      KNOT, &
     &                                      VTD%UEOP%SPL(1,UEOP__UT1_M_TAI) )
!
                VTD%MOM%XPL_RATE = DSPL8 ( TIM_MOM, VTD%UEOP%NP, VTD%UEOP%TIM, &
     &                                     VTD%UEOP%VAL(1,UEOP__XPL), KNOT, &
     &                                     VTD%UEOP%SPL(1,UEOP__XPL) )
                VTD%MOM%YPL_RATE = DSPL8 ( TIM_MOM, VTD%UEOP%NP, VTD%UEOP%TIM, &
     &                                     VTD%UEOP%VAL(1,UEOP__YPL), KNOT, &
     &                                     VTD%UEOP%SPL(1,UEOP__YPL) )
                VTD%MOM%UT1_RATE = DSPL8 ( TIM_MOM, VTD%UEOP%NP, VTD%UEOP%TIM, &
     &                                     VTD%UEOP%VAL(1,UEOP__UT1_M_TAI), &
     &                                     KNOT, &
     &                                     VTD%UEOP%SPL(1,UEOP__UT1_M_TAI) )
           END IF
      END IF
!
      IF ( VTD%ERM%STATUS == VTD__LOAD ) THEN
!
! -------- Compute the small vector of the perturbing Earth's rotation
! -------- as well the first and the second derivatives and their formal
! -------- uncertainties at the specified moment of time using coefficients
! -------- of epxansion of this vector into B-spline basis.
!
           CALL ERR_PASS    ( IUER, IER )
           CALL VTD_GET_ERM ( VTD%MOM%MJD, VTD%MOM%TAI, VTD__M_ERD, &
     &                        VTD__M_ERM, VTD%ERM%DEGREE, VTD%ERM%NKNOTS, &
     &                        VTD%ERM%MJD_BEG, VTD%ERM%TAI_BEG, &
     &                        VTD%ERM%TIM, VTD%ERM%VAL, VTD%ERM%ERR, &
     &                        VTD%ERM%COV, ERM_VAL, ERM_DER, ERM_DR2, &
     &                        ERM_ERR, ERM_ER1, ERM_ER2, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2226, IUER, 'VTD_MOMENT', 'Error in an '// &
     &              'attempt to compute the vector of the small peturbing '// &
     &              'rotation of the planet Earth on moment of time '// &
     &               MJDSEC_TO_DATE( MJD, TAI, IER )//' using coefficients '// &
     &              'from ERM file '// &
     &               VTD%CONF%FINAM_ERM(1:I_LEN(VTD%CONF%FINAM_ERM)) )
                RETURN
           END IF
!
           IF ( VTD%UEOP%STATUS .NE. UEOP__LOADED ) THEN
                VTD%MOM%XPL       = 0.0D0
                VTD%MOM%YPL       = 0.0D0
                VTD%MOM%UT1_M_TAI = 0.0D0
!
                VTD%MOM%XPL_RATE  = 0.0D0
                VTD%MOM%YPL_RATE  = 0.0D0
                VTD%MOM%UT1_RATE  = 0.0D0
           END IF
!
           VTD%MOM%XPL       = VTD%MOM%XPL       + ERM_VAL(2)
           VTD%MOM%YPL       = VTD%MOM%YPL       + ERM_VAL(1)
           VTD%MOM%UT1_M_TAI = VTD%MOM%UT1_M_TAI + ERM_VAL(3)/UT1__TO__E3
!
           VTD%MOM%XPL_RATE  = VTD%MOM%XPL_RATE  + ERM_DER(2)
           VTD%MOM%YPL_RATE  = VTD%MOM%YPL_RATE  + ERM_DER(1)
           VTD%MOM%UT1_RATE  = VTD%MOM%UT1_RATE  + ERM_DER(3)/UT1__TO__E3
      END IF
!
      IF ( VTD%CONF%UZT_MODEL == UZT__DICKMAN1993 ) THEN
           CALL E3ZT_DICKMAN1993  (  0, VTD%MOM%MJD, VTD%MOM%TAI, &
     &                              E3, E3_DOT, E3_DT2 )
         ELSE IF ( VTD%CONF%UZT_MODEL == UZT__DICKMAN_PRINCIPLE ) THEN
           CALL E3ZT_DICKMAN1993  ( 13, VTD%MOM%MJD, VTD%MOM%TAI, &
     &                              E3, E3_DOT, E3_DT2 )
         ELSE IF ( VTD%CONF%UZT_MODEL == UZT__DICKMAN_SHORT ) THEN
           CALL E3ZT_DICKMAN1993  ( 12, VTD%MOM%MJD, VTD%MOM%TAI, &
     &                              E3, E3_DOT, E3_DT2 )
         ELSE IF ( VTD%CONF%UZT_MODEL == UZT__RE2014 ) THEN
           CALL E3ZT_RE2014 ( 0, VTD%MOM%MJD, VTD%MOM%TAI, &
     &                        E3, E3_DOT, E3_DT2 )
         ELSE IF ( VTD%CONF%UZT_MODEL == UZT__RE2014_SHORT ) THEN
           CALL E3ZT_RE2014 ( 1, VTD%MOM%MJD, VTD%MOM%TAI, &
     &                        E3, E3_DOT, E3_DT2 )
      END IF
!
      IF ( VTD%CONF%UZT_MODEL .NE. UZT__NONE ) THEN
           IF ( VTD%CONF%UZT_USE == UZT__ADD ) THEN
                VTD%MOM%UT1_M_TAI = VTD%MOM%UT1_M_TAI + E3/UT1__TO__E3
                VTD%MOM%UT1_RATE  = VTD%MOM%UT1_RATE  + E3_DOT/UT1__TO__E3
             ELSE IF ( VTD%CONF%UZT_USE == UZT__INTERPOLATE  ) THEN
                VTD%MOM%UT1_M_TAI = VTD%MOM%UT1_M_TAI + E3/UT1__TO__E3
                VTD%MOM%UT1_RATE  = VTD%MOM%UT1_RATE  + E3_DOT/UT1__TO__E3
             ELSE IF ( VTD%CONF%UZT_USE == UZT__SUBTRACT ) THEN
                VTD%MOM%UT1_M_TAI = VTD%MOM%UT1_M_TAI - E3/UT1__TO__E3
                VTD%MOM%UT1_RATE  = VTD%MOM%UT1_RATE  - E3_DOT/UT1__TO__E3
           END IF
      END IF
!
      IF ( VTD%STATUS_AEM == VTD__LOAD ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_AEM ( VTD%AEM, VTD%MOM%MJD, VTD%MOM%TAI, &
     &                    ERM_VAL, ERM_DER, ERM_DR2, VTD%L_HEO, &
     &                    VTD%HEO_EPOCH_SEC, VTD%HEO, VTD%MOM%TRS_TO_CRS, &
     &                    VTD%MOM%TRS_TO_CRS_DER1, VTD%MOM%TRS_TO_CRS_DER2, &
     &                    VTD%MOM%DTRS_TO_CRS_DEOP, VTD%MOM%UT1_M_TAI, &
     &                    VTD%MOM%S_ANG, VTD%CONF%IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2227, IUER, 'VTD_MOMENT', 'Error in '// &
     &              'computing rotation matrix from TRS to CRS using '// &
     &              'apriori ERM approach' )
                RETURN
           END IF
         ELSE 
           IPAR = 1
!
! -------- Compute the matrix of transformation form the terrestrical 
! -------- coordainte system to the celestial coordainte system using 
! -------- IERS approcah
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_ERM_NA ( IPAR, VTD%CONF%IVRB, VTD%CONF%PREC_EXP, &
     &                  VTD%CONF%NUT_EXP, VTD%CONF%NUT_GDS, &
     &                  VTD%CONF%EROT_COMPAT, VTD%NERS, VTD%MOM%MJD, VTD%MOM%TAI, &
     &                  VTD%MOM%XPL, VTD%MOM%YPL, VTD%MOM%UT1_M_TAI, &
     &                  VTD%MOM%XPL_RATE, VTD%MOM%YPL_RATE, VTD%MOM%UT1_RATE, &
     &                  VTD%MOM%S_ANG, VTD%MOM%S_ANG_RATE, VTD%L_HEO, &
     &                  VTD%HEO_EPOCH_SEC, VTD%HEO, VTD%MOM%TRS_TO_CRS, &
     &                  VTD%MOM%TRS_TO_CRS_DER1, VTD%MOM%TRS_TO_CRS_DER2, &
     &                  VTD%MOM%DTRS_TO_CRS_DEOP, VTD%MOM%DTRS_TO_CRS_DER1_DEOP, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2228, IUER, 'VTD_MOMENT', 'Error in '// &
     &              'computing rotation matrix from TRS to CRS using IERS '// &
     &              'approach' )
                RETURN
           END IF
      END IF
!
      IF ( VTD%CONF%IVRB .GE. 5 ) THEN
           WRITE ( 6, 210 ) 'ROT_MAT(1,1:3): ', VTD%MOM%TRS_TO_CRS(1,1), &
     &                                          VTD%MOM%TRS_TO_CRS(1,2), &
     &                                          VTD%MOM%TRS_TO_CRS(1,3)
!
           WRITE ( 6, 210 ) 'ROT_MAT(2,1:3): ', VTD%MOM%TRS_TO_CRS(2,1), &
     &                                          VTD%MOM%TRS_TO_CRS(2,2), &
     &                                          VTD%MOM%TRS_TO_CRS(2,3)
!
           WRITE ( 6, 210 ) 'ROT_MAT(3,1:3): ', VTD%MOM%TRS_TO_CRS(3,1), &
     &                                          VTD%MOM%TRS_TO_CRS(3,2), &
     &                                          VTD%MOM%TRS_TO_CRS(3,3)
!
           WRITE ( 6, '(A)' ) '-----------------------------------------'
!
           WRITE ( 6, 210 ) 'ROT_DER(1,1:3): ', VTD%MOM%TRS_TO_CRS_DER1(1,1), &
     &                                          VTD%MOM%TRS_TO_CRS_DER1(1,2), &
     &                                          VTD%MOM%TRS_TO_CRS_DER1(1,3)
!
           WRITE ( 6, 210 ) 'ROT_DER(2,1:3): ', VTD%MOM%TRS_TO_CRS_DER1(2,1), &
     &                                          VTD%MOM%TRS_TO_CRS_DER1(2,2), &
     &                                          VTD%MOM%TRS_TO_CRS_DER1(2,3)
!
           WRITE ( 6, 210 ) 'ROT_DER(3,1:3): ', VTD%MOM%TRS_TO_CRS_DER1(3,1), &
     &                                          VTD%MOM%TRS_TO_CRS_DER1(3,2), &
     &                                          VTD%MOM%TRS_TO_CRS_DER1(3,3)
!
           WRITE ( 6, '(A)' ) '-----------------------------------------'
!
           WRITE ( 6, 210 ) 'ROT_DR2(1,1:3): ', VTD%MOM%TRS_TO_CRS_DER2(1,1), &
     &                                          VTD%MOM%TRS_TO_CRS_DER2(1,2), &
     &                                          VTD%MOM%TRS_TO_CRS_DER2(1,3)
!
           WRITE ( 6, 210 ) 'ROT_DR2(2,1:3): ', VTD%MOM%TRS_TO_CRS_DER2(2,1), &
     &                                          VTD%MOM%TRS_TO_CRS_DER2(2,2), &
     &                                          VTD%MOM%TRS_TO_CRS_DER2(2,3)
!
           WRITE ( 6, 210 ) 'ROT_DR2(3,1:3): ', VTD%MOM%TRS_TO_CRS_DER2(3,1), &
     &                                          VTD%MOM%TRS_TO_CRS_DER2(3,2), &
     &                                          VTD%MOM%TRS_TO_CRS_DER2(3,3)
 210       FORMAT ( A, 3(2X,1F15.12 ) )
      END IF
!
! --- Compute time-dependent arguments for further computation of
! --- displacements caused by the Solid Earth tide
!
      CALL ERR_PASS  ( IUER, IER )
      CALL SOTID_TIM ( VTD%MOM%MJD, VTD%MOM%TAI, VTD%MOM%UT1_M_TAI, &
     &                 VTD%TIDCNF_STD, VTD%TIMTID, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2229, IUER, 'VTD_MOMENT', 'Error in an attempt '// &
     &         'to compute time vrying arguments for computation of '// &
     &         'displacements caused by solid Earth tides' )
           RETURN
      END IF
!
! --- Get position, velocities and acceleration of bodies in the Solar System
!
      DO 420 J2=1,VTD__M_PLA   ! Cycle over bodies
         CALL ERR_PASS  ( IUER, IER )
         CALL PLANETA_DE_EPH ( VTD%DE_EPH, VTD%MOM%MJD, VTD%MOM%TAI, &
     &                         VTD__PLANAM(J2),             &
     &                         VTD%MOM%PLAN(1,VTD__COO,J2), &
     &                         VTD%MOM%PLAN(1,VTD__VEL,J2), &
     &                         VTD%MOM%PLAN(1,VTD__ACC,J2), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2230, IUER, 'VTD_MOMENT', 'Error in an '// &
     &            'attempt to compute position, velocity and accelration '// &
     &            'of the '//VTD__PLANAM(J2)//' on '// &
     &             MJDSEC_TO_DATE( VTD%UEOP%MJD_BEG, VTD%UEOP%TAI_BEG, IER ) )
              RETURN
         END IF
 420  CONTINUE
!
      IF ( SOU_NAM == 'EOP_ONLY' ) THEN
!
! -------- A special case wheh we need only EOP, but do not want
! -------- to compute source-related parameters, for instance,
! -------- because of observations of a near zone object
!
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
      ISOU = VTD_SOU_INDEX ( VTD, SOU_NAM  )
      IF ( ISOU == 0 ) THEN
           CALL ERR_LOG ( 2231, IUER, 'VTD_MOMENT', 'Source with '//   &
     &         'name '//SOU_NAM//' was not found. '// &
     &         'Presumably, it was not loaded from input catalogues '// &
     &         VTD%CONF%FINAM_SOUCOO(1)(1:I_LEN(VTD%CONF%FINAM_SOUCOO(1)))// &
     &         ' '// &
     &         VTD%CONF%FINAM_SOUCOO(2)(1:I_LEN(VTD%CONF%FINAM_SOUCOO(2)))// &
     &         ' '// &
     &         VTD%CONF%FINAM_SOUCOO(3)(1:I_LEN(VTD%CONF%FINAM_SOUCOO(3)))// &
     &         ' '// &
     &         VTD%CONF%FINAM_SOUCOO(4)(1:I_LEN(VTD%CONF%FINAM_SOUCOO(4))) )
           RETURN
      END IF
!
      VTD%MOM%TDT = VTD%MOM%TAI + 32.184D0
      CALL TAI_TO_TDB ( VTD%MOM%MJD, VTD%MOM%TAI, VTD%MOM%TDB )
      IF ( VTD%SOU(ISOU)%OBJ_TYPE == VTD__SS .OR. &
     &     VTD%SOU(ISOU)%OBJ_TYPE == VTD__ES      ) THEN
!
! -------- A case of the near zone object. We have to get its coodinates by
! -------- computing it through the coefficients of expansion with B-spline
! -------- basis
!
           IF ( VTD%L_NZO == 0 ) THEN
                CALL ERR_LOG ( 2232, IUER, 'VTD_MOMENT', 'No position of '//  &
     &              'any near zone object was loaded. Cannot get position '// &
     &              'for source '//VTD%SOU(ISOU)%IVS_NAME )
                RETURN
           END IF
!
           IND_NZO = 0
           DO 430 J3=1,VTD%L_NZO
              IF ( VTD%NZO(J3)%NAME == VTD%SOU(ISOU)%NZO_NAME ) THEN
                   IND_NZO = J3
              END IF
 430       CONTINUE
           IF ( IND_NZO == 0 ) THEN
                CALL ERR_LOG ( 2233, IUER, 'VTD_MOMENT', 'Near zone object '// &
     &               VTD%SOU(ISOU)%NZO_NAME//' was not found. Cannot get '// &
     &              'position for source '//VTD%SOU(ISOU)%IVS_NAME )
                RETURN
           END IF
           VTD%SOU(ISOU)%IND_NZO = IND_NZO
!
           IF ( VTD%SOU(ISOU)%OBJ_TYPE == VTD__SS .OR. &
     &          VTD%SOU(ISOU)%OBJ_TYPE == VTD__ES      ) THEN
                TIM_ARG = VTD%MOM%TAI
           END IF
           TARG = (MJD     - VTD%NZO(IND_NZO)%MJD_BEG)*86400.0D0 + &
     &            (TIM_ARG - VTD%NZO(IND_NZO)%TIM_BEG)
           KNOT = IXMN8 ( VTD%NZO(IND_NZO)%NOD_SPL, &
     &                    VTD%NZO(IND_NZO)%TIM_ARR(1), TARG )
           IF ( KNOT .EQ. -1 ) THEN
                STR(1:28)  = MJDSEC_TO_DATE ( MJD, TAI, IER )
                STR(31:58) = MJDSEC_TO_DATE ( VTD%NZO(IND_NZO)%MJD_BEG, &
     &                                        VTD%NZO(IND_NZO)%TIM_BEG, IER )
                CALL ERR_LOG ( 2234, IUER, 'VTD_MOMENT', 'Moment of time '// &
     &               STR(1:28)//' at TAI precedes the first epoch for the '// &
     &              'near zone object '//VTD%SOU(ISOU)%NZO_NAME//' -- '// &
     &               STR(31:58) )
                RETURN
           END IF
!
           IF ( KNOT .EQ. -2 ) THEN
                STR(1:23)  = MJDSEC_TO_DATE ( MJD, TAI, IER )
                STR(31:53) = MJDSEC_TO_DATE ( VTD%NZO(IND_NZO)%MJD_BEG, &
     &                                        VTD%NZO(IND_NZO)%TIM_BEG + &
     &                       VTD%NZO(IND_NZO)%TIM_ARR(VTD%NZO(IND_NZO)%NOD_SPL), IER )
                CALL ERR_LOG ( 2235, IUER, 'VTD_MOMENT', 'Moment of time '// &
     &               STR(1:28)//' at TAI is after the last epoch for the '// &
     &              'near zone object '//VTD%SOU(ISOU)%NZO_NAME//' -- '// &
     &               STR(31:58) )
                RETURN
           END IF
           IF ( VTD%SOU(ISOU)%OBJ_TYPE == VTD__ES ) THEN
!
!---------------Satellite position is in TRS, need CRS
!
                DO 440 J4=1,3
                   SAT_POS_TRS(J4) = 0.0D0
                   SAT_VEL_TRS(J4) = 0.0D0
                   DO 450 J5=KNOT-VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%DEG_SPL,KNOT
                      SAT_POS_TRS(J4) = SAT_POS_TRS(J4) + &
               &          VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%SPL_ARR(J5,J4)* &
               &          BSPL_VAL ( VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%NOD_SPL, &
               &                     VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_ARR(1), &
               &                     VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%DEG_SPL, J5, TARG )
                      SAT_VEL_TRS(J4) = SAT_VEL_TRS(J4) + &
               &          VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%SPL_ARR(J5,J4)* &
               &          BSPL_DER ( VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%NOD_SPL, &
               &                     VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_ARR(1), &
               &                     VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%DEG_SPL, J5, TARG )
 450               CONTINUE
 440            CONTINUE

                CALL ERR_PASS ( IUER, IER )
!
!---------------Convert satellite position to CRF
!
          
                CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS(1,1), &
               &                      3, SAT_POS_TRS, &
               &                      3, VTD%SOU(ISOU)%SOU_CRS, IER )
          
                CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS(1,1), &
               &                      3, SAT_VEL_TRS, &
               &                      3, VTD%SOU(ISOU)%SOU_CRS_RATE, IER )
          
                CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS_DER1(1,1), &
               &                      3, SAT_POS_TRS, &
               &                      3, POS_COMP_VEL, IER )
                VTD%SOU(ISOU)%SOU_CRS_RATE = VTD%SOU(ISOU)%SOU_CRS_RATE + POS_COMP_VEL
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 2236, IER, 'VTD_LOAD_SOUCOO', 'Error in attempt '// &
          &              'to transform NZO TRS to CRS' )
                     RETURN
                END IF
              ELSE
                DO 460 J6=1,3
                   VTD%SOU(ISOU)%SOU_CRS(J6)      = 0.0D0
                   VTD%SOU(ISOU)%SOU_CRS_RATE(J6) = 0.0D0
                   DO 470 J7=KNOT-VTD%NZO(IND_NZO)%DEG_SPL,KNOT
                      VTD%SOU(ISOU)%SOU_CRS(J6) = VTD%SOU(ISOU)%SOU_CRS(J6) + &
     &                        VTD%NZO(IND_NZO)%SPL_ARR(J7,J6)* &
     &                        BSPL_VAL ( VTD%NZO(IND_NZO)%NOD_SPL, &
     &                                   VTD%NZO(IND_NZO)%TIM_ARR(1), &
     &                                   VTD%NZO(IND_NZO)%DEG_SPL, J7, TARG )
                      VTD%SOU(ISOU)%SOU_CRS_RATE(J6) = VTD%SOU(ISOU)%SOU_CRS_RATE(J6) + &
     &                        VTD%NZO(IND_NZO)%SPL_ARR(J7,J6)* &
     &                        BSPL_DER ( VTD%NZO(IND_NZO)%NOD_SPL, &
     &                                   VTD%NZO(IND_NZO)%TIM_ARR(1), &
     &                                   VTD%NZO(IND_NZO)%DEG_SPL, J7, TARG )
 470               CONTINUE
 460            CONTINUE
           END IF 
           IF ( FL_PLAN_ABR ) THEN
!
! ------------- Take into account planet aberration
!
                DST = DSQRT ( VTD%SOU(ISOU)%SOU_CRS(1)**2 + &
     &                        VTD%SOU(ISOU)%SOU_CRS(2)**2 + &
     &                        VTD%SOU(ISOU)%SOU_CRS(3)**2   )
                VTD%SOU(ISOU)%SOU_CRS = VTD%SOU(ISOU)%SOU_CRS - DST/VTD__C*VTD%SOU(ISOU)%SOU_CRS_RATE 
                IF ( VTD%CONF%FL_WARN ) THEN
                     WRITE ( 6, * ) 'VTD_MOMENT: WARNING!!!  VTD_PLAN_ABR environment variable is set up'
                END IF
           END IF
!
           CALL COPY_R8 ( 3, VTD%SOU(ISOU)%SOU_CRS, &
     &                       VTD%SOU(ISOU)%S_CRS    )
           CALL COPY_R8 ( 3, VTD%SOU(ISOU)%SOU_CRS_RATE, &
     &                       VTD%SOU(ISOU)%S_CRS_RATE    )
!
           CALL ERR_PASS ( IUER, IER )
           CALL DECPOL ( 3, VTD%SOU(ISOU)%S_CRS, &
     &                      VTD%SOU(ISOU)%DIST,  &
     &                      VTD%SOU(ISOU)%ALPHA, &
     &                      VTD%SOU(ISOU)%DELTA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2236, IER, 'VTD_LOAD_SOUCOO', 'Trap of '// &
     &              'internal control' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL DECPOL ( 3, VTD%SOU(ISOU)%S_CRS_RATE, &
     &                      VTD%SOU(ISOU)%DIST_RATE,  &
     &                      VTD%SOU(ISOU)%ALPHA_RATE, &
     &                      VTD%SOU(ISOU)%DELTA_RATE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2237, IER, 'VTD_LOAD_SOUCOO', 'Trap of '// &
     &              'internal control' )
                RETURN
           END IF
!
           CALL MUL_VC_V ( 3, VTD%SOU(ISOU)%S_CRS, &
     &                        1.0D0/VTD%SOU(ISOU)%DIST )
           CALL MUL_VC_V ( 3, VTD%SOU(ISOU)%S_CRS_RATE, &
     &                        1.0D0/VTD%SOU(ISOU)%DIST_RATE )
      END IF
!
      DO 480 J8=1,VTD%L_STA
         IF ( VTD%STA(J8)%STA_TYP .EQ. VTD__OR ) THEN
              IF ( VTD%L_NZO == 0 ) THEN
                   CALL ERR_LOG ( 2238, IUER, 'VTD_MOMENT', 'No position of '//  &
     &                 'any near zone object was loaded. Cannot get position '// &
     &                 'for antenna '//VTD%STA(J8)%IVS_NAME )
                   RETURN
              END IF
!
! ----------- A case of the near zone object. We have to get its coodinates by
! ----------- computing it through the coefficients of expansion over the 
! ----------- B-spline basis
!
              IF ( VTD%L_NZO == 0 ) THEN
                   CALL ERR_LOG ( 2239, IUER, 'VTD_MOMENT', 'No position of '//  &
     &                 'any orbiting object was loaded. Cannot get position '// &
     &                 'for antenna '//VTD%STA(J8)%IVS_NAME )
                   RETURN
              END IF
!
              IND_NZO = 0
              DO 490 J9=1,VTD%L_NZO
                 IF ( VTD%NZO(J9)%NAME == VTD%STA(J8)%IVS_NAME ) THEN
                      IND_NZO = J9
                 END IF
 490          CONTINUE
              IF ( IND_NZO == 0 ) THEN
                   CALL ERR_LOG ( 2240, IUER, 'VTD_MOMENT', 'No ephemerides '// &
     &                 'the the orbiting antenna '//VTD%STA(J8)%IVS_NAME// &
     &                 ' was found' )
                   RETURN
              END IF
!
              TARG = (MJD        - VTD%NZO(IND_NZO)%MJD_BEG)*86400.0D0 + &
     &               (VTD%MOM%TAI - VTD%NZO(IND_NZO)%TIM_BEG)
              KNOT = IXMN8 ( VTD%NZO(IND_NZO)%NOD_SPL, &
     &                       VTD%NZO(IND_NZO)%TIM_ARR(1), TARG )
              IF ( KNOT .EQ. -1 ) THEN
                  STR(1:28)  = MJDSEC_TO_DATE ( MJD, TAI, IER )
                  STR(31:58) = MJDSEC_TO_DATE ( VTD%NZO(IND_NZO)%MJD_BEG, &
     &                                          VTD%NZO(IND_NZO)%TIM_BEG, IER )
                  CALL ERR_LOG ( 2241, IUER, 'VTD_MOMENT', 'Moment of time '// &
     &                 STR(1:28)//' at TAI precedes the first epoch for the '// &
     &                'orbiting antenna '//VTD%STA(J8)%IVS_NAME//' -- '// &
     &                 STR(31:58) )
                  RETURN
              END IF
!
              IF ( KNOT .EQ. -2 ) THEN
                   STR(1:23)  = MJDSEC_TO_DATE ( MJD, TAI, IER )
                   STR(31:53) = MJDSEC_TO_DATE ( VTD%NZO(IND_NZO)%MJD_BEG, &
     &                                           VTD%NZO(IND_NZO)%TIM_BEG + &
     &                          VTD%NZO(IND_NZO)%TIM_ARR(VTD%NZO(IND_NZO)%NOD_SPL), IER )
                   CALL ERR_LOG ( 2242, IUER, 'VTD_MOMENT', 'Moment of time '// &
     &                  STR(1:28)//' at TAI is after the last epoch for the '// &
     &                  'orbiting antenna '//VTD%STA(J8)%IVS_NAME//' -- '// &
     &                  STR(31:58) )
                   RETURN
              END IF
!
              VTD%STA(J8)%COO_CRS = 0.0D0
              VTD%STA(J8)%VEL_CRS = 0.0D0
              VTD%STA(J8)%ACC_CRS = 0.0D0
              VTD%MOM%NZO_RLT     = 0.0D0
              DO 4100 J10=KNOT-VTD%NZO(IND_NZO)%DEG_SPL,KNOT
                 DO 4110 J11=1,3
                    VTD%STA(J8)%COO_CRS(J11) = VTD%STA(J8)%COO_CRS(J11) + &
     &                      VTD%NZO(IND_NZO)%SPL_ARR(J10,J11)* &
     &                      BSPL_VAL ( VTD%NZO(IND_NZO)%NOD_SPL, &
     &                                 VTD%NZO(IND_NZO)%TIM_ARR(1), &
     &                                 VTD%NZO(IND_NZO)%DEG_SPL, J10, TARG )
                    VTD%STA(J8)%VEL_CRS(J11) = VTD%STA(J8)%VEL_CRS(J11) + &
     &                      VTD%NZO(IND_NZO)%SPL_ARR(J10,J11)* &
     &                      BSPL_DER ( VTD%NZO(IND_NZO)%NOD_SPL, &
     &                                 VTD%NZO(IND_NZO)%TIM_ARR(1), &
     &                                 VTD%NZO(IND_NZO)%DEG_SPL, J10, TARG )
                    VTD%STA(J8)%ACC_CRS(J11) = VTD%STA(J8)%ACC_CRS(J11) + &
     &                      VTD%NZO(IND_NZO)%SPL_ARR(J10,J11)* &
     &                      BSPL_DR2 ( VTD%NZO(IND_NZO)%NOD_SPL, &
     &                                 VTD%NZO(IND_NZO)%TIM_ARR(1), &
     &                                 VTD%NZO(IND_NZO)%DEG_SPL, J10, TARG )
 4110            CONTINUE
!
                 VTD%MOM%NZO_RLT = VTD%MOM%NZO_RLT + &
     &               VTD%NZO(IND_NZO)%SPL_RLT_ARR(J10)* &
     &                       BSPL_VAL ( VTD%NZO(IND_NZO)%NOD_SPL, &
     &                                  VTD%NZO(IND_NZO)%TIM_ARR(1), &
     &                                  VTD%NZO(IND_NZO)%DEG_SPL, J10, TARG )
 4100         CONTINUE
         END IF
 480  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_MOMENT  !#!#
