      SUBROUTINE NERS_GET_SERIES ( NERS, TIME_TAI_START, TIME_TAI_END, TIME_STEP, &
     &                             CPAR, M_PAR, M_SER, NS, TIM, SER, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  NERS_GET_SERIES
! *                                                                      *
! * ### 16-JUN-2016  NERS_GET_SERIES  v2.4 (c) L. Petrov 24-AUG-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      INCLUDE   'astro_constants.i'
      INCLUDE   'heo.i'
      TYPE     ( NERS__TYPE ) :: NERS
      INTEGER*4  M_PAR, M_SER, NS, IUER
      REAL*8     TIM(M_SER), SER(M_SER,M_PAR)
      REAL*8     TIME_TAI_START, TIME_TAI_END, TIME_STEP
      REAL*8     TAI_BEG, TAI_END, UTC_CUR
      CHARACTER  CPAR*(*)
      CHARACTER  STR*128, STR1*128
      REAL*8     TIM_EPS
      PARAMETER  ( TIM_EPS = 1.0D0 ) 
      REAL*8     EVEC(3,0:2), HEO_VEC(3,0:2), DPSI, DEPS, DPSI_RATE, DEPS_RATE
      REAL*8       LOD__TO__ER3
      PARAMETER  ( LOD__TO__ER3 =  1.00273781191135448E0*OM__EAR**2/PI2 )
      REAL*8     DEPS_GDS, DEPS_SEC, DEPSR_SEC, DPSI_GDS, DPSI_SEC, DPSIR_SEC, &
     &           DZETA, DZETA_RATE, E1_GDS, E1_NUT, E1_NUT_RATE, E2_GDS, E2_NUT, &
     &           E2_NUT_RATE, EPS_0, EPS_0_RATE, S_ANG, S_ANG_RATE, TETA, &
     &           TETA_RATE, UT1_M_TAI, UT1_RATE, XPOL, YPOL, XPOL_RATE, YPOL_RATE, &
     &           ZA, ZA_RATE, TAI, E3Z, E3Z_DOT, E3Z_DT2 
      INTEGER*4  J1, J2, J3, J4, K_PAR, IS, UNIX_DATE, MJD, IER
      INTEGER*8  SIZE_I8 
      CHARACTER, EXTERNAL :: TIM_TO_DATE*23
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, TIME, FILE_INFO
!
      UTC_CUR = TIME ( %VAL(0) ) - UNIX__J2000_UTC
      IF ( NERS%FCS_STATUS .NE. NERS__LOAD .OR. &
     &     (UTC_CUR - NERS%UTC_LOAD) > NERS__AGE_MIN ) THEN
!
           IF ( NERS%FCS_STATUS .NE. NERS__INIT ) THEN
                CALL ERR_LOG ( 4911, IUER, 'NERS_GET_EOP', 'NERS data '// &
     &              'structure has not been initialized. Please run '// &
     &              'NERS_INIT first' )
                RETURN 
           END IF
!
           IS = FILE_INFO ( TRIM(NERS%CNF%FCS_FILE)//CHAR(0), UNIX_DATE, &
     &                      SIZE_I8 )
           IF ( IS .NE. 0 .OR. (TIME(%VAL(0)) - UNIX_DATE) > NERS%CNF%AGE_FCS ) THEN
                IER = IUER
                CALL NERS_FETCH ( NERS, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 4912, IUER, 'NERS_GET_SERIES', 'Error in '// &
     &                   'an attempt to retrieve NERS forecast parameters '// &
     &                   'form the remote server' )
                     RETURN 
                END IF
           END IF
!
! -------- Load the forecast message
!
           IER = IUER
           CALL NERS_LOAD ( NERS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4913, IUER, 'NERS_GET_SERIES', 'Error in '// &
     &              'an attempt to retrieve NERS forecast parameters '// &
     &              'form the remote server' )
                RETURN 
           END IF
      END IF
!
      TAI_BEG = TIME_TAI_START
      TAI_END = TIME_TAI_END
      IF ( TAI_END < 0.0D0 ) TAI_END = NERS%FCS%ARG_3(NERS%FCS%NK_3)
!
      IF ( TIME_STEP < NERS__MIN_TIM_STEP ) THEN
           WRITE ( UNIT=STR(1:12), FMT='(1PD12.4)' ) TIME_STEP
           WRITE ( UNIT=STR1(1:5), FMT='(F5.0)'    ) NERS__MIN_TIM_STEP 
           CALL ERR_LOG ( 4914, IUER, 'NERS_GET_SERIES', 'Argument '// &
     &         'TIME_STEP '//TRIM(STR)//' is too small: less than '// &
     &          STR1 )
           RETURN 
      END IF
      IF ( TAI_END .LE. TAI_BEG ) THEN
           CALL ERR_LOG ( 4915, IUER, 'NERS_GET_SERIES', 'Argument '// &
     &         'TIME_TAI_START is less or equal than TIME_TAI_STOP' )
           RETURN 
      END IF
!
      IF ( TAI_BEG < NERS%FCS%ARG_C(1) - TIM_EPS ) THEN
           IER  = 0
           STR  = TIM_TO_DATE ( TAI_BEG,      IER )
           IER  = 0
           STR1 = TIM_TO_DATE ( NERS%FCS%ARG_C(1), IER )
           CALL ERR_LOG ( 4916, IUER, 'NERS_GET_SERIES', 'Argument '// &
     &         'TIME_TAI_START '//TRIM(STR)//' is too early: the first '// &
     &         'EOP epoch is '//STR1 )
           RETURN 
      END IF
!
      IF ( TAI_END > NERS%FCS%ARG_3(NERS%FCS%NK_3) + TIM_EPS .AND. &
     &      NERS%FCS%NL == 0 ) THEN
           IER  = 0
           STR  = TIM_TO_DATE ( TAI_END,      IER )
           IER  = 0
           STR1 = TIM_TO_DATE ( NERS%FCS%ARG_3(NERS%FCS%NK_3), IER )
           CALL ERR_LOG ( 4917, IUER, 'NERS_GET_SERIES', 'Argument '// &
     &         'TIME_TAI_STOP '//TRIM(STR)//' is too late: the last '// &
     &         'EOP epoch is '//STR1 )
           RETURN 
      END IF
      IF ( NERS%FCS%NL > 0 ) THEN
           IF ( TAI_END > NERS%FCS%ARG_L(NERS%FCS%NL) + TIM_EPS ) THEN
                IER  = 0
                STR  = TIM_TO_DATE ( TAI_END,      IER )
                IER  = 0
                STR1 = TIM_TO_DATE ( NERS%FCS%ARG_L(NERS%FCS%NL), IER )
                CALL ERR_LOG ( 4917, IUER, 'NERS_GET_SERIES', 'Argument '// &
     &              'TIME_TAI_STOP '//TRIM(STR)//' is too late: the last '// &
     &              'EOP long-tern prediction epoch is '//STR1 )
               RETURN 
           END IF
      END IF
!
      NS = (TAI_END - TAI_BEG)/TIME_STEP
!
      IF ( NS < 1 ) NS = 2
      IF ( NS > M_SER ) THEN
           CALL CLRCH ( STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( M_SER, STR )
           CALL INCH  ( NS,    STR1 )
           CALL ERR_LOG ( 4918, IUER, 'NERS_GET_SERIES', 'Argument '// &
     &         'M_SER '//TRIM(STR)//' is too small: it should be at least '// &
     &          TRIM(STR1) )
           RETURN 
      END IF
!
      K_PAR = 0
      IF ( CPAR == 'euler' ) THEN
           IF ( M_PAR < 3 ) K_PAR = 3 
         ELSE IF ( CPAR == 'euler_r' ) THEN
           IF ( M_PAR < 6 ) K_PAR = 6
         ELSE IF ( CPAR == 'heo' ) THEN
           IF ( M_PAR < 3 ) K_PAR = 3
         ELSE IF ( CPAR == 'heo_r' ) THEN
           IF ( M_PAR < 6 ) K_PAR = 6
         ELSE IF ( CPAR == 'polu' ) THEN
           IF ( M_PAR < 3 ) K_PAR = 3
         ELSE IF ( CPAR == 'poluz' ) THEN
           IF ( M_PAR < 3 ) K_PAR = 3
         ELSE IF ( CPAR == 'eops' ) THEN
           IF ( M_PAR < 8 ) K_PAR = 8
         ELSE IF ( CPAR == 'polu_r' ) THEN
           IF ( M_PAR < 6 ) K_PAR = 6
         ELSE IF ( CPAR == 'poluz_r' ) THEN
           IF ( M_PAR < 6 ) K_PAR = 6
         ELSE IF ( CPAR == 'nut' ) THEN
           IF ( M_PAR < 2 ) K_PAR = 2
         ELSE IF ( CPAR == 'utcmtai' ) THEN
           IF ( M_PAR < 1 ) K_PAR = 1
         ELSE
           CALL ERR_LOG ( 4919, IUER, 'NERS_GET_SERIES', 'Argument '// &
     &         'CPAR '//TRIM(CPAR)//' is not supported. The list of '// &
     &         'supported values: euler, euler_r, heo, heo_r, polu, '// &
     &         'poluz, polu_r, poluz_r, nut, utcmtai, eops' )
           RETURN 
      END IF
      IF ( K_PAR .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( M_PAR, STR  )
           CALL INCH  ( K_PAR, STR1 )
           CALL ERR_LOG ( 4920, IUER, 'NERS_GET_SERIES', 'Argument '// &
     &         'M_PAR is too small: '//TRIM(STR)//' while parameter '// &
     &          TRIM(CPAR)//' requires M_PAR '//TRIM(STR1)//' or greater' )
           RETURN 
      END IF
!
      DO 410 J1=1,NS
         TIM(J1) = (J1-1)*TIME_STEP
         IF ( TAI_BEG + TIM(J1) < NERS%FCS%ARG_C(1) ) THEN
              TIM(J1) = NERS%FCS%ARG_C(1) - TAI_BEG 
            ELSE IF ( NERS%FCS%NL > 0 ) THEN
              IF ( TAI_BEG + TIM(J1) > NERS%FCS%ARG_L(NERS%FCS%NL)   ) TIM(J1) = NERS%FCS%ARG_L(NERS%FCS%NL)   - TAI_BEG 
            ELSE
              IF ( TAI_BEG + TIM(J1) > NERS%FCS%ARG_3(NERS%FCS%NK_3) ) TIM(J1) = NERS%FCS%ARG_3(NERS%FCS%NK_3) - TAI_BEG 
         END IF
         IF ( CPAR == 'euler'  .OR.  CPAR == 'euler_r' .OR. &
     &        CPAR == 'heo'    .OR.  CPAR == 'heo_r'   .OR. &
     &        CPAR == 'polu'   .OR.  CPAR == 'polu_r'  .OR. &
     &        CPAR == 'poluz'  .OR.  CPAR == 'poluz_r' .OR. &
     &        CPAR == 'eops'                                ) THEN
!
              IER = IUER
              CALL NERS_GET_EVEC ( NERS, TAI_BEG + TIM(J1), EVEC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4921, IUER, 'NERS_GET_SERIES', 'Error in '// &
     &                 'interpolating the EOP' )
                   RETURN 
              END IF
              IF ( CPAR == 'euler' .OR. CPAR == 'euler_r' ) THEN
                   SER(J1,1) = EVEC(1,0)
                   SER(J1,2) = EVEC(2,0)
                   SER(J1,3) = EVEC(3,0)
              END IF
              IF ( CPAR == 'euler_r' ) THEN
                   SER(J1,4) = EVEC(1,1)
                   SER(J1,5) = EVEC(2,1)
                   SER(J1,6) = EVEC(3,1)
              END IF
              IF ( CPAR == 'polu'  .OR. CPAR == 'polu_r'  .OR. &
     &             CPAR == 'poluz' .OR. CPAR == 'poluz_r' .OR. &
     &             CPAR == 'eops' ) THEN
                   SER(J1,1) = EVEC(2,0)*RAD__TO__ARCSEC
                   SER(J1,2) = EVEC(1,0)*RAD__TO__ARCSEC
                   SER(J1,3) = EVEC(3,0)/UT1__TO__E3
              END IF
              IF ( CPAR == 'polu_r' .OR. CPAR == 'poluz_r' .OR. CPAR == 'eops' ) THEN
                   SER(J1,4) = EVEC(2,1)*RAD__TO__ARCSEC*86400.0D0
                   SER(J1,5) = EVEC(1,1)*RAD__TO__ARCSEC*86400.0D0
                   SER(J1,6) = EVEC(3,1)/LOD__TO__ER3
              END IF
              IF ( CPAR == 'poluz' .OR. CPAR == 'poluz_r' ) THEN
                   MJD = J2000__MJD  + INT((TAI_BEG + TIM(J1))/86400.0D0)
                   TAI = (TAI_BEG + TIM(J1)) - 86400.0D0*INT((TAI_BEG + TIM(J1))/86400.0D0)
                   IF ( NERS%FCS%E3Z_APR_MOD == NERS__E3Z_D93 ) THEN
                        CALL NERS_E3ZT_DICKMAN1993 ( 0, MJD, TAI, E3Z, E3Z_DOT, E3Z_DT2 ) 
                      ELSE IF ( NERS%FCS%E3Z_APR_MOD == NERS__E3Z_RE2014  ) THEN
                        CALL NERS_E3ZT_RE2014      ( 0, MJD, TAI, E3Z, E3Z_DOT, E3Z_DT2 ) 
                      ELSE
                        E3Z     = 0.0D0
                        E3Z_DOT = 0.0D0
                        E3Z_DT2 = 0.0D0
                   END IF
                   SER(J1,3) = SER(J1,3) - E3Z/UT1__TO__E3
                   IF ( CPAR == 'poluz_r' ) THEN
                        SER(J1,6) = SER(J1,6) - E3Z_DOT/LOD__TO__ER3
                   END IF
              END IF
         END IF
         IF ( CPAR == 'nut'   .OR. &
     &        CPAR == 'heo'   .OR. &
     &        CPAR == 'heo_r' .OR. &
     &        CPAR == 'eops'       ) THEN
!
              IER = IUER
              CALL NERS_GET_HEO ( NERS, TAI_BEG + TIM(J1), EVEC(3,0)/UT1__TO__E3, &
     &                            DPSI, DEPS, HEO_VEC, DPSI_RATE, DEPS_RATE, IER )
              IF ( CPAR == 'nut' .OR. CPAR == 'eops' ) THEN
!
! ---------------- Compute auxilliary angles of the Earth rotation
!
                   MJD = J2000__MJD  + INT((TAI_BEG + TIM(J1))/86400.0D0)
                   TAI = (TAI_BEG + TIM(J1)) - 86400.0D0*INT((TAI_BEG + TIM(J1))/86400.0D0)
                   CALL NERS_ERM_ANGS ( 1, 0, PREC__CAPITAINE2003, NUT__MHB2000, &
     &                                  MJD, TAI, XPOL, YPOL, UT1_M_TAI, &
     &                                  XPOL_RATE, YPOL_RATE, UT1_RATE,  &
     &                                  S_ANG, S_ANG_RATE, &
     &                                  DZETA, TETA, ZA, EPS_0, DZETA_RATE, &
     &                                  TETA_RATE, ZA_RATE, EPS_0_RATE, &
     &                                  E1_NUT, E2_NUT, DPSI, DEPS, &
     &                                  DPSI_SEC, DEPS_SEC, DPSIR_SEC, DEPSR_SEC, &
     &                                  E1_NUT_RATE, E2_NUT_RATE, DPSI_RATE, DEPS_RATE, &
     &                                  E1_GDS, E2_GDS, DPSI_GDS, DEPS_GDS, IER )
              END IF
!
              IF ( CPAR == 'heo' .OR. CPAR == 'heo_r' ) THEN
                   SER(J1,1) = HEO_VEC(1,0)
                   SER(J1,2) = HEO_VEC(2,0)
                   SER(J1,3) = HEO_VEC(3,0)
              END IF
              IF ( CPAR == 'heo_r' ) THEN
                   SER(J1,4) = HEO_VEC(1,1)
                   SER(J1,5) = HEO_VEC(2,1)
                   SER(J1,6) = HEO_VEC(3,1)
              END IF
              IF ( CPAR == 'nut' ) THEN
                   SER(J1,1) = DPSI
                   SER(J1,2) = DEPS
              END IF
              IF ( CPAR == 'eops' ) THEN
                   SER(J1,7) = DPSI*RAD__TO__ARCSEC
                   SER(J1,8) = DEPS*RAD__TO__ARCSEC
              END IF
         END IF
         IF ( CPAR == 'utcmtai' ) THEN
              CALL NERS_GET_UTCMTAI ( NERS, TAI_BEG + TIM(J1), SER(1,J1), IER )
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  NERS_GET_SERIES  !#!#
