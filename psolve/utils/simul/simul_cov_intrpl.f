      SUBROUTINE SIMUL_COV_INTRPL ( NI, ARG_IN, VAL_IN, &
     &                              NO, TIM_OBS, COR_OBS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SIMUL_COV_INTRPL
! *                                                                      *
! *  ### 17-NOV-2021 SIMUL_COV_INTRPL v1.0 (c) L. Petrov 17-NOV-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INTEGER*4  NI, NO, IUER
      INTEGER*4  DEG, MT_MAX, MT_MIN
      PARAMETER  ( DEG = 3 ) 
      PARAMETER  ( MT_MAX = 7 )
      PARAMETER  ( MT_MIN = 3 )
      REAL*8     ARG_IN(NI), VAL_IN(NI), TIM_OBS(NO), COR_OBS(NO)
      REAL*8     ARG_ZERO, RC, DT, WEI, SIG_ORI, SIG_DER, SIG_DR2, &
     &           RC_TAG, SIG_SQ_DIAG, SIG_INCR_MIN
      REAL*8     TIM_EPS
      PARAMETER  ( TIM_EPS = 1.0D0 )
      REAL*8,    ALLOCATABLE :: NOR_MAT(:), NOR_VEC(:), EST_VEC(:), &
     &                          EQU_OBS(:), ARG_SPL(:), &
     &                          COV(:,:), EV(:), COV_WORK(:)
      CHARACTER  STR_OMP_NUM_THREADS_SAVED*16, STR_OPENBLAS_NUM_THREADS_SAVED*16, &
     &           STR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           J13, J14, J15, J16, J17, J18, J19, &
     &           MT, KK, LPAR, LW, KI, KZ, NB, IER
      INTEGER*4  NTHR_SAVED, NUM_THR_OB_SAVED, SCHED_THR
      REAL*8,    EXTERNAL :: DP_VV_V, BSPL_VAL, BSPL_DER, BSPL_DR2
      INTEGER*4, EXTERNAL :: ILAENV
#ifdef GNU
      INTEGER*4, EXTERNAL     :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS, OPENBLAS_GET_NUM_THREADS
#else
      ADDRESS__TYPE, EXTERNAL :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS
#endif
!
      RC_TAG  = 1.D8
      SIG_ORI = 1.D-2
      SIG_DER = 1.D-3
      SIG_DR2 = 1.D-3
      ARG_ZERO = -1.001D30
      SIG_INCR_MIN = 0.01D0
      DT = 1.5*86400.0D0 
!
! --- Search for the first zero of the raw covariance
! --- KZ is the index of the input covariance that is positive and 
! ---    closest to the zero
! --- KI is the index of the input covariance that is closest to DT, but 
! ---    does not exist
!
      DO 410 J1=2,NI
         IF ( ARG_ZERO < -1.D30 .AND. VAL_IN(J1) < 0.0 .AND. VAL_IN(J1-1) > 0.0 ) THEN
              ARG_ZERO = ARG_IN(J1-1) - VAL_IN(J1-1)*(ARG_IN(J1) - ARG_IN(J1-1))/ &
     &                                               (VAL_IN(J1) - VAL_IN(J1-1))
              KZ = J1 
         END IF
         IF ( ARG_IN(J1) < DT ) THEN
              KI = J1
         END IF
 410  CONTINUE 
 810  CONTINUE 
      IF ( KI < NI ) THEN
           KI = KI + 1
      END IF
!
      CALL GETENV ( 'OMP_NUM_THREADS', STR_OMP_NUM_THREADS_SAVED )
      CALL SETENV ( 'OMP_NUM_THREADS'//CHAR(0), '1'//CHAR(0), %VAL(1) )
#if BLAS == openblas
      CALL GETENV ( 'OPENBLAS_NUM_THREADS', STR_OPENBLAS_NUM_THREADS_SAVED )
      CALL SETENV ( 'OPENBLAS_NUM_THREADS'//CHAR(0), '1'//CHAR(0), %VAL(1) )
      NUM_THR_OB_SAVED = OPENBLAS_GET_NUM_THREADS ()
      CALL OPENBLAS_SET_NUM_THREADS ( %VAL(1) )
#endif
!
   write ( 6, * ) ' ki = ', ki, ' kz= ', kz, ' arg_zero= ', arg_zero ! %%%
      DO 420 J2=MT_MAX,MT_MIN,-1
         MT = J2
         LPAR = 2*MT + DEG - 1
         ALLOCATE ( NOR_MAT(LPAR*(LPAR+1)/2), NOR_VEC(LPAR), EST_VEC(LPAR), &
     &              EQU_OBS(LPAR), ARG_SPL(2*MT), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8612, IUER, 'EXPAND_COR_BES', 'Error in '// &
     &            'an attempt to allocate memory' )
              CALL SETENV ( 'OMP_NUM_THREADS'//CHAR(0), TRIM(STR_OMP_NUM_THREADS_SAVED)//CHAR(0), %VAL(1) )
#if BLAS == openblas
              CALL SETENV ( 'OPENBLAS_NUM_THREADS'//CHAR(0), TRIM(STR_OPENBLAS_NUM_THREADS_SAVED)//CHAR(0), %VAL(1) )
              CALL OPENBLAS_SET_NUM_THREADS ( %VAL(NUM_THR_OB_SAVED) )
#endif
              RETURN 
         END IF
!
         DO 440 J4=1,MT
            ARG_SPL(J4) = ARG_IN(1) + ARG_ZERO* &
     &                    (1.0D0 - DCOS( (2*J4-1.0)/(3*MT)*PI__NUM ) )
 440     CONTINUE 
         ARG_SPL(1) = ARG_IN(1)
         DO 450 J5=1,MT
            ARG_SPL(MT+J5) = ARG_ZERO + J5*(ARG_IN(KI) - ARG_ZERO)/MT
 450     CONTINUE 
!
         NOR_MAT = 0.0D0
         NOR_VEC = 0.0D0
!      
         DO 460 J6=1,KI
            DO 470 J7=1-DEG,2*MT-1
               EQU_OBS(J7+DEG) = BSPL_VAL ( 2*MT, ARG_SPL, DEG, J7, ARG_IN(J6) ) 
 470        CONTINUE 
            CALL DIAD_CVT_S ( 1.0D0, LPAR, EQU_OBS, EQU_OBS, NOR_MAT )
            CALL NORVEC_UPD ( LPAR, 1.0D0, VAL_IN(J6), EQU_OBS, NOR_VEC )
 460     CONTINUE 
!
         DO 480 J8=1,MT
            EQU_OBS = 0.0D0
            DO 490 J9=1-DEG,MT
               EQU_OBS(J9+DEG) = BSPL_DR2 ( MT, ARG_SPL, DEG, J9, ARG_IN(J8) ) 
 490        CONTINUE 
            WEI = 1.D0/(SIG_DR2*ARG_ZERO/ARG_SPL(J8+1))
            CALL DIAD_CVT_S ( WEI**2, LPAR, EQU_OBS, EQU_OBS, NOR_MAT )
!
            IF ( J8 == 1 ) THEN
                 DO 4100 J10=1-DEG,MT
                    EQU_OBS(J10+DEG) = BSPL_VAL ( MT, ARG_SPL, DEG, J10, ARG_IN(J8) ) 
 4100            CONTINUE 
                 WEI = 1.D0/SIG_ORI
                 CALL DIAD_CVT_S ( WEI**2, LPAR, EQU_OBS, EQU_OBS, NOR_MAT )
                 CALL NORVEC_UPD ( LPAR, WEI, 1.0D0, EQU_OBS, NOR_VEC )
!
                 DO 4110 J11=1-DEG,MT
                    EQU_OBS(J11+DEG) = BSPL_DER ( MT, ARG_SPL, DEG, J11, ARG_IN(J8) ) 
 4110            CONTINUE 
                 WEI = 1.D0/SIG_DER
                 CALL DIAD_CVT_S ( WEI**2, LPAR, EQU_OBS, EQU_OBS, NOR_MAT )
            END IF
 480     CONTINUE 
!
         CALL ERR_PASS ( IUER, IER )
         CALL INVS ( LPAR, NOR_MAT, RC, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8613, IUER, 'EXPAND_COR_BES', 'Error in '// &
     &            'an attempt to invert matrix for normal equation of '// &
     &            'expansion of the empirical covariance kernel' )
              CALL SETENV ( 'OMP_NUM_THREADS'//CHAR(0), TRIM(STR_OMP_NUM_THREADS_SAVED)//CHAR(0), %VAL(1) )
#if BLAS == openblas
              CALL SETENV ( 'OPENBLAS_NUM_THREADS'//CHAR(0), TRIM(STR_OPENBLAS_NUM_THREADS_SAVED)//CHAR(0), %VAL(1) )
              CALL OPENBLAS_SET_NUM_THREADS ( %VAL(NUM_THR_OB_SAVED) )
#endif
              RETURN 
         END IF
!
         CALL MUL_MV_SV_V ( LPAR, NOR_MAT, LPAR, NOR_VEC, LPAR, EST_VEC, IER )
!
         DO 4120 J12=1,NO
            DO 4130 J13=1-DEG,2*MT-1
               IF ( J12 < NO ) THEN
                    EQU_OBS(J13+DEG) = BSPL_VAL ( 2*MT, ARG_SPL, DEG, J13, TIM_OBS(J12) ) 
                  ELSE 
                    EQU_OBS(J13+DEG) = BSPL_VAL ( 2*MT, ARG_SPL, DEG, J13, TIM_OBS(J12) - TIM_EPS ) 
               END IF
 4130       CONTINUE 
            COR_OBS(J12) = DP_VV_V ( LPAR, EQU_OBS, EST_VEC )
            IF ( TIM_OBS(J12) < TIM_EPS ) THEN
                 COR_OBS(J12) = 1.0D0
            END IF
 4120    CONTINUE 
!
         DEALLOCATE ( NOR_MAT, NOR_VEC, EST_VEC, EQU_OBS, ARG_SPL )
!
         NTHR_SAVED = OMP_GET_MAX_THREADS() ! Store the current number of threads
         CALL OMP_SET_NUM_THREADS ( %VAL(1) )
!
         NB = ILAENV ( 1, 'DSYTRD', 'U', NO, NO, -1, -1 )
         LW = (NB+2)*NO
         ALLOCATE   ( COV(NO,NO), EV(NO), COV_WORK(LW), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8613, IUER, 'EXPAND_COR_BES', 'Error in '// &
     &            'an attempt to allocate memory' )
              CALL SETENV ( 'OMP_NUM_THREADS'//CHAR(0), TRIM(STR_OMP_NUM_THREADS_SAVED)//CHAR(0), %VAL(1) )
#if BLAS == openblas
              CALL SETENV ( 'OPENBLAS_NUM_THREADS'//CHAR(0), TRIM(STR_OPENBLAS_NUM_THREADS_SAVED)//CHAR(0), %VAL(1) )
              CALL OPENBLAS_SET_NUM_THREADS ( %VAL(NUM_THR_OB_SAVED) )
#endif
              RETURN 
         END IF
!
         COV = 0.0D0
         DO 4140 J14=1,NO
            DO 4150 J15=1,J14
               COV(J15,J14) = COR_OBS(J14-J15+1)
               COV(J14,J15) = COV(J15,J14) 
 4150       CONTINUE 
 4140    CONTINUE 
!
! ------ Compute the eigenvalues
!
         CALL DSYEV ( 'N', 'U', NO, COV, NO, EV, COV_WORK, LW, IER )
         CALL OMP_SET_NUM_THREADS ( %VAL(NTHR_SAVED) )
!
         SIG_SQ_DIAG= EV(NO)/RC_TAG - EV(1)
         IF ( SIG_SQ_DIAG < 0.0 ) SIG_SQ_DIAG = 0.0D0
         DEALLOCATE ( COV, EV, COV_WORK )
   write ( 6, * ) ' j2= ', j2, ' sig= ', sngl(sig_sq_diag) ! %%%%%%%%%%%%%%%%
         IF ( SIG_SQ_DIAG < SIG_INCR_MIN ) GOTO 820
 420  CONTINUE 
 820  CONTINUE 
      CALL SETENV ( 'OMP_NUM_THREADS'//CHAR(0),      TRIM(STR_OMP_NUM_THREADS_SAVED)//CHAR(0),      %VAL(1) )
#if BLAS == openblas
      CALL SETENV ( 'OPENBLAS_NUM_THREADS'//CHAR(0), TRIM(STR_OPENBLAS_NUM_THREADS_SAVED)//CHAR(0), %VAL(1) )
      CALL OPENBLAS_SET_NUM_THREADS ( %VAL(NUM_THR_OB_SAVED) )
#endif
!
      COR_OBS(1) = 1.0D0
      DO 4160 J16=2,NO
         COR_OBS(J16) = COR_OBS(J16)/(1.0D0 + SIG_SQ_DIAG)
 4160 CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SIMUL_COV_INTRPL !#!#
