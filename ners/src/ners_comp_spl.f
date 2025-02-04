      SUBROUTINE NERS_COMP_SPL ( NERS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  NERS_COMP_SPL computes the coefficients of the B-spline   *
! *   expansion of 15 auxilliary Earth parameters over the interval      *
! *   [NERS%TIM_START, NERS%TIM_STOP]. They are stored in the NERS       *
! *   object. NERS_COMP_SPL is the external routine and users are not    *
! *   supposed to call it directly. It is called indriectly from         *
! *   NERS_GET_EOP.                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    NERS ( NERS__TYPE ) -- The data structure that keeps internal     *
! *                           parameters related to the Network Earth    *
! *                           Rotation Service.                          *
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
! * ### 18-JUN-2016   NERS_COMP_SPL   v2.2 (c) L. Petrov 21-APR-2018 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'ners.i'
      INCLUDE   'heo.i'
      INTEGER*4  IUER
      TYPE     ( NERS__TYPE ) :: NERS
      REAL*8     TIM_EPS
      PARAMETER  ( TIM_EPS = 1.0D0 ) 
      REAL*8,    ALLOCATABLE :: TEMP_ARR(:)
      REAL*8     TIM_BEG, TIM_END, TIM_STEP, ARG_STEP, TAI, &
     &           EVEC(3,0:2), HEO_VEC(3,0:2), MAT_ROT(3,3,0:2), &
     &           DER_EOP(2,NERS__MEL), UTC_CUR, UTC_M_TAI, SAVE_TAI_GEN
      REAL*8     XPOL, YPOL, UT1_M_TAI, XPOL_RATE, YPOL_RATE, UT1_RATE,  &
     &           S_ANG, S_ANG_RATE, &
     &           DZETA, TETA, ZA, EPS_0, DZETA_RATE, &
     &           TETA_RATE, ZA_RATE, EPS_0_RATE, &
     &           E1_NUT, E2_NUT, DPSI, DEPS, &
     &           E1_NUT_RATE, E2_NUT_RATE, DPSI_RATE, DEPS_RATE, &
     &           E1_GDS, E2_GDS, DPSI_GDS, DEPS_GDS, &
     &           TRS_TO_CRS(3,3,0:2), DTRS_TO_CRS_DEOP(3,3,3), &
     &           DPSI_SEC, DEPS_SEC, DPSIR_SEC, DEPSR_SEC
      REAL*8     CNS_VAL_SIG, CNS_DER_SIG, CNS_DR2_SIG, ARG_EPS
      PARAMETER  ( CNS_VAL_SIG = 0.0D0 )
      PARAMETER  ( CNS_DER_SIG = 0.0D0 )
      PARAMETER  ( CNS_DR2_SIG = 0.0D0 )
      PARAMETER  ( ARG_EPS     = 1.D-6 )
      CHARACTER  STR*128, STR1*21, STR2*21
      INTEGER*8  SIZE_I8
      INTEGER*4  J1, J2, J3, ITURN, ITURN0, IND, IS, UNIX_DATE, MJD, IER
      INTEGER*4, EXTERNAL :: TIME, FILE_INFO, OMP_GET_THREAD_NUM
      CHARACTER, EXTERNAL :: TIM_TO_DATE*23
!
      IF ( NERS%FCS_STATUS .NE. NERS__LOAD ) THEN
           CALL ERR_LOG ( 4711, IUER, 'NERS_COMP_SPL', 'Trap of '// &
     &         'internal control: NERS was not loaded. Please execute '// &
     &         'routine ners_load first' )
           RETURN 
      END IF
!
! --- Get the last modifiction date of the local forecast file
!
      IS = FILE_INFO ( TRIM(NERS%CNF%FCS_FILE)//CHAR(0), UNIX_DATE, &
     &                 SIZE_I8 )
      IF ( NERS%CNF%AGE_SPL > 0.0D0                            .AND. &
     &     ( TIME ( %VAL(0) ) - UNIX_DATE ) > NERS%CNF%AGE_SPL       ) THEN
!
! -------- Spline is too old. We need to re-create it
!
           SAVE_TAI_GEN = NERS%FCS%TAI_GEN
           IF ( ( TIME ( %VAL(0) ) - UNIX_DATE ) > NERS%CNF%AGE_FCS ) THEN
!
! ------------- EOP forecast file is too old
!
                IER = IUER
                CALL NERS_FETCH ( NERS, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 4711, IUER, 'NERS_COMP_SPL', 'Error in '// &
     &                   'an attempt to retrieve NERS forecast parameters '// &
     &                   'form the remote server' )
                     RETURN 
               END IF
           END IF
!
! -------- Clean forecast related elements of the NERS internal data strcuture
!
           CALL NERS_QUIT ( NERS__FCS, NERS )
!
! -------- Load forecast file
!
           IER = IUER
           CALL NERS_LOAD ( NERS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4712, IUER, 'NERS_COMP_SPL', 'Error in '// &
     &              'an attempt to retrieve NERS forecast parameters '// &
     &              'form the remote server' )
                RETURN 
           END IF
      END IF
!
! --- Clean Bspline-related elements of the NERS internal data structure
!
      CALL NERS_QUIT ( NERS__EXP, NERS )
!
      TIM_BEG = -NERS__EDG_NODES*NERS__TIM_STEP
      TIM_END = NERS%TIM_STOP - NERS%TIM_START + NERS__EDG_NODES*NERS__TIM_STEP
      IF ( TIM_END < 0.0D0 ) THEN
           TIM_END = NERS%FCS%ARG_3(NERS%FCS%NK_3)
           TIM_BEG = NERS%FCS%ARG_3(NERS%FCS%NK_3) - NERS__INTR_MAX
      END IF
      IF ( TIM_BEG < TIM_END - NERS__INTR_MAX ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL CLRCH ( STR2 )
           WRITE ( UNIT=STR(1:10), FMT='(F10.4)' ) NERS__INTR_MAX/86400.0D0
           STR1 = TIM_TO_DATE ( TIM_BEG, IER )
           STR2 = TIM_TO_DATE ( TIM_END, IER )
           CALL ERR_LOG ( 4713, IUER, 'NERS_COMP_SPL', 'Trap of '// &
     &         'internal control: the interval for EOP spline '// &
     &         'expansion is too large: ['//STR1//', '//STR2//'] -- '// &
     &         'longer than '//TRIM(STR)//' days' )
           RETURN 
      END IF
!
! --- Compute the number of spline knots
!
      NERS%EXP%L_NOD = (TIM_END - TIM_BEG)/NERS__TIM_STEP
      IF ( TIM_END - TIM_BEG >  NERS%EXP%L_NOD*NERS__TIM_STEP + TIM_EPS ) THEN
           NERS%EXP%L_NOD = NERS%EXP%L_NOD + 1
      END IF
      ARG_STEP = (TIM_END - TIM_BEG)/(NERS%EXP%L_NOD - 1)
!
! --- Compute the number of time epochs for spline computation.
! --- NB: the number of epochs is grearter by NERS__EDG_NODES!!
!
      NERS%EXP%L_TIM = NERS%EXP%L_NOD + 2*NERS__EDG_NODES
!
      IF ( ASSOCIATED ( NERS%EXP%ARG ) ) DEALLOCATE ( NERS%EXP%ARG )
      ALLOCATE ( NERS%EXP%ARG(NERS%EXP%L_NOD), STAT=IER )
      IF ( IER .NE.0 ) THEN
           CALL ERR_LOG ( 4714, IUER, 'NERS_COMP_SPL', 'Error in attempt '// &
     &         'to allocate dynamic memory for array NERS%EXP%ARG' )
           RETURN 
      END IF
!
      IF ( ASSOCIATED ( NERS%EXP%BSPL ) ) DEALLOCATE ( NERS%EXP%BSPL )
      ALLOCATE ( NERS%EXP%BSPL(1-NERS__MDEG:NERS%EXP%L_NOD-1,NERS__MEL), STAT=IER )
      IF ( IER .NE.0 ) THEN
           CALL ERR_LOG ( 4715, IUER, 'NERS_COMP_SPL', 'Error in attempt '// &
     &         'to allocate dynamic memory for array NERS%EXP%BSPL' )
           RETURN 
      END IF
!
      IF ( ASSOCIATED ( NERS%EXP%TIM ) ) DEALLOCATE ( NERS%EXP%TIM )
      ALLOCATE ( NERS%EXP%TIM(NERS%EXP%L_TIM), STAT=IER )
      IF ( IER .NE.0 ) THEN
           CALL ERR_LOG ( 4716, IUER, 'NERS_COMP_SPL', 'Error in attempt '// &
     &         'to allocate dynamic memory for array NERS%EXP%TIM' )
           RETURN 
      END IF
!
      IF ( ASSOCIATED ( NERS%EXP%VAL ) ) DEALLOCATE ( NERS%EXP%VAL )
      ALLOCATE ( NERS%EXP%VAL(NERS%EXP%L_TIM,NERS__MEL), STAT=IER )
      IF ( IER .NE.0 ) THEN
           CALL ERR_LOG ( 4717, IUER, 'NERS_COMP_SPL', 'Error in attempt '// &
     &         'to allocate dynamic memory for array NERS%EXP%VAL' )
           RETURN 
      END IF
      NERS%EXP_STATUS = NERS__ALLC 
!
! --- Compute the array of spline knots
!
      DO 410 J1=1,NERS%EXP%L_NOD
         NERS%EXP%ARG(J1) = TIM_BEG + (J1-1)*ARG_STEP
 410  CONTINUE 
!
! --- Compute the array of time epochs used for B-spine expansion.
! --- Here is the trick: the time epochs conincides with the 
! --- array of knots *and*
! --- 1) there are NERS__EDG_NODES extra epochs between 
! ---    the first and second knot;
! --- 2) there are NERS__EDG_NODES extra epochs between 
! ---    the last and the last by one knot.
! --- These extra time epochs are placed at the equal distance between
! --- the knots:
!
!     o  o  o  o  o           o           o           o           o  o  o  o  o  time epochs 
! --- X-----------X-----------X-----------X-----------X-----------X-----------X  B-spline knots
!
      DO 420 J2=1,NERS%EXP%L_TIM
         IF ( J2 == 1 ) THEN
              NERS%EXP%TIM(J2) = TIM_BEG + ARG_EPS 
            ELSE IF ( J2 .LE. NERS__EDG_NODES + 1 ) THEN
              NERS%EXP%TIM(J2) = TIM_BEG + (J2-1)*ARG_STEP/(NERS__EDG_NODES+1)
            ELSE IF ( J2 .EQ. NERS%EXP%L_TIM ) THEN
              NERS%EXP%TIM(J2) = TIM_END - ARG_EPS
            ELSE IF ( J2 .GE. NERS%EXP%L_TIM - NERS__EDG_NODES ) THEN
              NERS%EXP%TIM(J2) = TIM_END + (J2-NERS%EXP%L_TIM)*ARG_STEP/(NERS__EDG_NODES+1)
            ELSE
              NERS%EXP%TIM(J2) = TIM_BEG + (J2-1-NERS__EDG_NODES)*ARG_STEP
         END IF
!
! ------ Compute the Euler angles ofthe Earth's orientation on time epochs
! ------ used fo B-spline evaluation
!
         IER = IUER
         CALL NERS_GET_EVEC ( NERS, NERS%TIM_START + NERS%EXP%TIM(J2), EVEC, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4718, IUER, 'NERS_COMP_SPL', 'Error in '// &
     &            'an attempt to compute the EOP vector on the '// &
     &            'requested moment of time' )
              RETURN 
         END IF
!
! ------ Compute the empirical harmonic vartions of the Euler angles 
! ------ of the Earth's orientation on time epochs
! ------ used fo B-spline evaluation
!
         IER = IUER
         CALL NERS_GET_HEO ( NERS, NERS%TIM_START + NERS%EXP%TIM(J2), &
     &                       EVEC(3,0)/UT1__TO__E3, DPSI, DEPS, HEO_VEC, &
     &                       DPSI_RATE, DEPS_RATE, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4719, IUER, 'NERS_COMP_SPL', 'Error in '// &
     &            'an attempt to compute the contribution of '// &
     &            'harmonic EOP variation of teh requested '// &
     &            'moment of time' )
              RETURN 
         END IF
!
! ------ Transform NERS%EXP%TIM(J2) to MJD/TAI
!
         MJD = J2000__MJD  + INT((NERS%TIM_START + NERS%EXP%TIM(J2))/86400.0D0)
         TAI = (NERS%TIM_START + NERS%EXP%TIM(J2)) - &
     &         86400.0D0*INT((NERS%TIM_START + NERS%EXP%TIM(J2))/86400.0D0)
!
! ------ Compute auxilliary Earth orientation parameters
!
         XPOL      = EVEC(2,0)
         YPOL      = EVEC(1,0)
         UT1_M_TAI = EVEC(3,0)/UT1__TO__E3
         XPOL_RATE = EVEC(2,1)
         YPOL_RATE = EVEC(1,1)
         UT1_RATE  = EVEC(3,1)/UT1__TO__E3
         IER = IUER
         CALL NERS_ERM_ANGS ( 1, 0, PREC__CAPITAINE2003, NUT__MHB2000, &
     &                        MJD, TAI, XPOL, YPOL, UT1_M_TAI, &
     &                        XPOL_RATE, YPOL_RATE, UT1_RATE,  &
     &                        S_ANG, S_ANG_RATE, &
     &                        DZETA, TETA, ZA, EPS_0, DZETA_RATE, &
     &                        TETA_RATE, ZA_RATE, EPS_0_RATE, &
     &                        E1_NUT, E2_NUT, DPSI, DEPS, &
     &                        DPSI_SEC, DEPS_SEC, DPSIR_SEC, DEPSR_SEC, &
     &                        E1_NUT_RATE, E2_NUT_RATE, DPSI_RATE, DEPS_RATE, &
     &                        E1_GDS, E2_GDS, DPSI_GDS, DEPS_GDS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4720, IUER, 'NERS_COMP_SPL', 'Error in '// &
     &            'an attempt to compute the auxiliary angles of the '// &
     &            'of the Earth rotation' )
              RETURN 
         END IF
!
! ------ Put the relevant parameters to the vector for B-spline expansion
!
         NERS%EXP%VAL(J2,NERS__E1) = EVEC(1,0)
         NERS%EXP%VAL(J2,NERS__E2) = EVEC(2,0)
         NERS%EXP%VAL(J2,NERS__E3) = EVEC(3,0)
         NERS%EXP%VAL(J2,NERS__H1) = HEO_VEC(1,0)
         NERS%EXP%VAL(J2,NERS__H2) = HEO_VEC(2,0)
         NERS%EXP%VAL(J2,NERS__H3) = HEO_VEC(3,0)
         NERS%EXP%VAL(J2,NERS__DP) = DPSI
         NERS%EXP%VAL(J2,NERS__DE) = DEPS
         NERS%EXP%VAL(J2,NERS__E1_NUT) = E1_NUT
         NERS%EXP%VAL(J2,NERS__E2_NUT) = E2_NUT
         NERS%EXP%VAL(J2,NERS__DZETA)  = DZETA
         NERS%EXP%VAL(J2,NERS__TETA)   = TETA
         NERS%EXP%VAL(J2,NERS__ZA)     = ZA
         NERS%EXP%VAL(J2,NERS__EPS0)   = EPS_0
         IF ( J2 == 1 ) THEN
              ITURN0 = IDNINT(S_ANG/PI2)
              NERS%EXP%VAL(J2,NERS__SANG) = S_ANG - PI2*ITURN0
            ELSE
              ITURN = IDNINT ( (S_ANG - PI2*ITURN0 - NERS%EXP%VAL(J2-1,NERS__SANG))/PI2 )
              NERS%EXP%VAL(J2,NERS__SANG) = S_ANG - PI2*(ITURN+ITURN0)
         END IF
 420  CONTINUE 
!
! --- Compute the B-spline expansion for the array of auxilliary Earth
! --- orientation parameters using least squares
!
      CALL ERR_PASS ( IUER, IER )
      CALL EBSPL_LSQ_CNS3_VEC ( NERS%EXP%L_TIM, NERS__MEL,  NERS%EXP%TIM, NERS%EXP%VAL,  &
     &                          NERS%EXP%L_NOD, NERS__MDEG, NERS%EXP%ARG, NERS%EXP%BSPL, &
     &                          CNS_VAL_SIG, CNS_DER_SIG, CNS_DR2_SIG, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4721, IUER, 'NERS_LOAD', 'Error in computing '// &
     &         'the coefficients of B-spline expansion' )
           RETURN 
      END IF
!
      UTC_CUR = TIME ( %VAL(0) ) - UNIX__J2000_UTC
!
      CALL ERR_PASS   ( IUER, IER )
      CALL NERS_GET_UTCMTAI ( NERS, UTC_CUR, UTC_M_TAI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4722, IUER, 'NERS_LOAD', 'Error in getting '// &
     &         'UTC minus TAI difference' )
           RETURN 
      END IF
      IF ( ASSOCIATED ( NERS%EXP%VAL ) ) DEALLOCATE ( NERS%EXP%VAL )
      IF ( ASSOCIATED ( NERS%EXP%TIM ) ) DEALLOCATE ( NERS%EXP%TIM )
      NERS%EXP_STATUS = NERS__COMP
      NERS%UTC_SPLN = UTC_CUR
      NERS%TIM_SPLN = UTC_CUR - UTC_M_TAI
!
      CALL ERR_LOG ( 0, IUER ) 
      RETURN
      END  SUBROUTINE  NERS_COMP_SPL  !#!#
