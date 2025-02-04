      SUBROUTINE GETDB_METEO_CORRECT ( GVH, N_MET, IND_MET, TIM_MET, &
     &                                 ATM_PRES, AIR_TEMP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GETDB_METEO_CORRECT  analyzes records of atmospheric      *
! *   pressure and air temperature, detects missing records and if it    *
! *   finds missing records, it replaces them with a model.              *
! *                                                                      *
! * ## 04-MAR-2008 GETDB_METEO_CORRECT v1.2 (c) L. Petrov 11-SEP-2019 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'phys.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INCLUDE   'gvh.i'
      TYPE     ( GVH__STRU ) :: GVH
      INTEGER*4  N_MET(MAX_STA), IND_MET(2,MAX_OBS), IUER
      REAL*8     ATM_PRES(MAX_SCA,MAX_STA), AIR_TEMP(MAX_SCA,MAX_STA), &
     &           TIM_MET(MAX_SCA,MAX_STA)
      REAL*8     TIM_ARR(MAX_SCA), ATM_ARR(MAX_SCA), TEM_ARR(MAX_SCA), &
     &           SPL_ATM(MAX_SCA), SPL_TEM(MAX_SCA), TMP_ARR(MAX_SCA), &
     &           ARR_R8(1024)
      REAL*8     TIM_EPS
      PARAMETER  ( TIM_EPS = 1.0D0 ) 
      REAL*8     ATM_PRES_LOW, ATM_PRES_HIGH
      PARAMETER  ( ATM_PRES_LOW  =  50000.0D0 )
      PARAMETER  ( ATM_PRES_HIGH = 120000.0D0 )
      INTEGER*4, ALLOCATABLE :: OBS_TAB(:,:)
      INTEGER*4  MJD_UTC_OBS, MJD_BEG, MJD, ISITE(2)
      LOGICAL*4  FL_MET, FL_MIS, FL_SKIP(2)
      REAL*8     UTC_OBS, TAI_BEG, TAI, TIM_ARG
      REAL*8     LAT_GCN, LAT_GDT, LAMBDA, H_ELL, RD, G_ACC, HEI_GPT 
      CHARACTER  STR*32, DESCR*256
      INTEGER*4  J1, J2, J3, J4, J5, J6, DIMS(2), NP, IND_BEG, IND_END, IXP, &
     &           CLASS, TYP, NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, IER
      ADDRESS__TYPE :: ADR_DATA
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      REAL*8,    EXTERNAL :: FSPL8
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, IXMN8 
!
! --- Initialization
!
      CALL NOUT_I4 ( 2*MAX_OBS, IND_MET  )
      CALL NOUT_R8 ( MAX_SCA*INT4(MAX_STA), TIM_MET  )
      CALL NOUT_R8 ( MAX_SCA*INT4(MAX_STA), ATM_PRES )
      CALL NOUT_R8 ( MAX_SCA*INT4(MAX_STA), AIR_TEMP )
!
      CALL ERR_PASS      ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'ATM_PRES', DESCR, CLASS, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8611, IUER, 'GETDB_METEO_CORRECT', 'Error in '// &
     &         'inquiring lcode ATM_PRES' )
           RETURN
      END IF
      IF ( CLASS == 0 ) THEN
!
! -------- There is no meteorological data in the file. Nothing to correct.
!
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
      ALLOCATE ( OBS_TAB(3,NUMOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*3*NUMOBS, STR )      
           CALL ERR_LOG ( 8612, IUER, 'GETDB_METEO_CORRECT', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for observation table' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'OBS_TAB ', 1, 0, 4*3*NUMOBS, DIMS(1), DIMS(2), &
     &                  OBS_TAB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8613, IUER, 'GETDB_METEO_CORRECT', 'Error in '// &
     &         'getting lcode OBS_TAB' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'MJD_OBS ', 1, 0, 4, DIMS(1), DIMS(2), &
     &                  MJD_UTC_OBS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8614, IUER, 'GETDB_METEO_CORRECT', 'Error in '// &
     &         'getting lcode MJD_OBS' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'UTC_OBS ', 1, 0, 8, DIMS(1), DIMS(2), &
     &                  UTC_OBS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8615, IUER, 'GETDB_METEO_CORRECT', 'Error in '// &
     &         'getting lcode UTC_OBS' )
           RETURN 
      END IF
!
      MJD_BEG = MJD_UTC_OBS
      TAI_BEG = UTC_OBS - UTC_M_TAI
      IF ( TAI_BEG < 0.0D0 ) THEN
           MJD_BEG = MJD_BEG - 1
           TAI_BEG = TAI_BEG +86400.0D0
      END IF
!
      CALL NOUT_I4 ( INT4(MAX_STA), N_MET )
      DO 410 J1=1,NUMOBS
         ISITE(1) = OBS_TAB(2,J1)  
         ISITE(2) = OBS_TAB(3,J1)  
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'MJD_OBS ', J1, 0, 4, DIMS(1), DIMS(2), &
     &                     MJD_UTC_OBS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8616, IUER, 'GETDB_METEO_CORRECT', 'Error in '// &
     &                      'getting lcode MJD_OBS' )
              RETURN 
         END IF
   !
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'UTC_OBS ', J1, 0, 8, DIMS(1), DIMS(2), &
     &                     UTC_OBS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8617, IUER, 'GETDB_METEO_CORRECT', 'Error in '// &
     &            'getting lcode UTC_OBS' )
              RETURN 
         END IF
         MJD = MJD_UTC_OBS
         TAI = UTC_OBS - UTC_M_TAI
         IF ( TAI < 0.0D0 ) THEN
              MJD = MJD_BEG - 1
              TAI = TAI_BEG +86400.0D0
         END IF
!
         TIM_ARG = (MJD - MJD_BEG)*86400.0D0 + (TAI - TAI_BEG)
         FL_SKIP(1) = .FALSE.
         FL_SKIP(2) = .FALSE.
         IF ( N_MET(ISITE(1)) > 0 ) THEN
              IF ( TIM_ARG - TIM_MET(N_MET(ISITE(1)),ISITE(1)) < TIM_EPS ) THEN
                   FL_SKIP(1) = .TRUE.
              END IF
         END IF
         IF ( N_MET(ISITE(2)) > 0 ) THEN
              IF ( TIM_ARG - TIM_MET(N_MET(ISITE(2)),ISITE(2)) < TIM_EPS ) THEN
                   FL_SKIP(2) = .TRUE.
              END IF
         END IF
!
         IF ( .NOT. FL_SKIP(1) ) THEN
!
! ----------- Get air pressure
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'ATM_PRES', J1, 1, 8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8618, IUER, 'GETDB_METEO_CORRECT', 'Error '// &
     &                 'in getting lcode AIR_TEMP for the first station' )
                   RETURN
              END IF
              N_MET(ISITE(1)) = N_MET(ISITE(1)) + 1
              TIM_MET(N_MET(ISITE(1)),ISITE(1))  = TIM_ARG
              ATM_PRES(N_MET(ISITE(1)),ISITE(1)) = ARR_R8(1)
!
! ----------- Get air temperature
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'AIR_TEMP', J1, 1, 8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8619, IUER, 'GETDB_METEO_CORRECT', 'Error '// &
     &                 'in getting lcode AIR_TEMP for the first station' )
                   RETURN 
              END IF
              AIR_TEMP(N_MET(ISITE(1)),ISITE(1)) = ARR_R8(1)
         END IF
!
         IF ( .NOT. FL_SKIP(2) ) THEN
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'ATM_PRES', J1, 2, 8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8620, IUER, 'GETDB_METEO_CORRECT', 'Error '// &
     &                 'in getting lcode AIR_TEMP for the second station' )
                   RETURN 
              END IF
              N_MET(ISITE(2)) = N_MET(ISITE(2)) + 1
              TIM_MET(N_MET(ISITE(2)),ISITE(2))  = TIM_ARG
              ATM_PRES(N_MET(ISITE(2)),ISITE(2)) = ARR_R8(1)
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'AIR_TEMP', J1, 2, 8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8621, IUER, 'GETDB_METEO_CORRECT', 'Error '// &
     &                 'in getting lcode AIR_TEMP for the second station' )
                   RETURN 
              END IF
              AIR_TEMP(N_MET(ISITE(2)),ISITE(2)) = ARR_R8(1)
         END IF
!
         IND_MET(1,J1) = N_MET(ISITE(1)) 
         IND_MET(2,J1) = N_MET(ISITE(2)) 
 410  CONTINUE 
!
      DO 420 J2=1,NUMSTA
         IND_BEG = 0
         IND_END = 0
         FL_MET = .FALSE.
         FL_MIS = .FALSE.
         DO 430 J3=1,N_MET(J2)
            IF ( ATM_PRES(J3,J2) < ATM_PRES_LOW   .OR.  &
     &           ATM_PRES(J3,J2) > ATM_PRES_HIGH        ) THEN
                 FL_MIS = .TRUE.
               ELSE 
                 FL_MET = .TRUE.
            END IF
 430     CONTINUE 
         NP = 0
         IF ( .NOT. FL_MET ) THEN
!
! ----------- If no information about air pressure for this statin was at all
! ----------- collected, then compute the average atmosperic pressure 
! ----------- using regression
!
              CALL REF_ELL ( 0, VSITEC(1,J2), LAT_GCN, LAT_GDT, LAMBDA, H_ELL, &
     &                       RD, G_ACC )
              IF ( RD < REA$VRN/2.0D0 ) THEN
                   H_ELL = 0.0D0
                   G_ACC = 9.80665D0
              END IF
              DO 440 J4=1,N_MET(J2)
                 HEI_GPT = H_ELL*9.80665D0/G_ACC
                 ATM_PRES(J4,J2) = 101324.2D0 * DEXP ( -1.1859D-4*HEI_GPT &
     &                            -1.1343D-9*HEI_GPT**2 -2.5644D-14*HEI_GPT**3 )
                 AIR_TEMP(J4,J2) = 288.15D0 - 0.0065D0*H_ELL
 440          CONTINUE 
           ELSE IF ( FL_MIS .AND. FL_MET ) THEN
              NP = 0
              DO 450 J5=1,N_MET(J2)
                 IF ( ATM_PRES(J5,J2) > ATM_PRES_LOW  .AND. &
     &                ATM_PRES(J5,J2) < ATM_PRES_HIGH       ) THEN
                      IF ( IND_BEG == 0 ) IND_BEG = J5
                      IND_END = J5
                      NP = NP + 1
                      TIM_ARR(NP) = TIM_MET(J5,J2)
                      ATM_ARR(NP) = ATM_PRES(J5,J2) 
                      TEM_ARR(NP) = AIR_TEMP(J5,J2) 
                 END IF
 450          CONTINUE 
         END IF
         IF ( NP > 4 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL MAKE_SPLINE ( 3, NP, TIM_ARR, ATM_ARR, TMP_ARR(1), &
     &                           TMP_ARR(2), SPL_ATM, TMP_ARR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8622, IUER, 'GETDB_METEO_CORRECT', &
     &                 'Error in an attempt to compute coefficients of '// &
     &                 'interpolating spline for atmospheric pressure' )
                  RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL MAKE_SPLINE ( 3, NP, TIM_ARR, TEM_ARR, TMP_ARR(1), &
     &                           TMP_ARR(2), SPL_TEM, TMP_ARR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8623, IUER, 'GETDB_METEO_CORRECT', &
     &                 'Error in an attempt to compute coefficients of '// &
     &                 'interpolating spline for atmospheric pressure' )
                  RETURN 
              END IF
!
              DO 460 J6=1,N_MET(J2)
                 IF ( ATM_PRES(J6,J2) < ATM_PRES_LOW  .OR. &
     &                ATM_PRES(J6,J2) > ATM_PRES_HIGH       ) THEN
                      IF ( J6 < IND_BEG ) THEN
                           ATM_PRES(J6,J2) = ATM_PRES(IND_BEG,J2) 
                           AIR_TEMP(J6,J2) = AIR_TEMP(IND_BEG,J2)
                         ELSE IF ( J6 > IND_END ) THEN
                           ATM_PRES(J6,J2) = ATM_PRES(IND_END,J2) 
                           AIR_TEMP(J6,J2) = AIR_TEMP(IND_END,J2)
                         ELSE 
                           IXP = IXMN8 ( NP, TIM_ARR, TIM_MET(J6,J2) )
                           ATM_PRES(J6,J2) = FSPL8 ( TIM_MET(J6,J2), NP, &
     &                                       TIM_ARR, ATM_ARR, IXP, SPL_ATM )
                           AIR_TEMP(J6,J2) = FSPL8 ( TIM_MET(J6,J2), NP, &
     &                                       TIM_ARR, TEM_ARR, IXP, SPL_TEM )
                      END IF
                 END IF
 460          CONTINUE 
         END IF
 420  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE GETDB_METEO_CORRECT  !#!  
