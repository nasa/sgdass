      SUBROUTINE PIMA_APPLY_ACCEL ( PIM, LCHN, LFRQ, LTIM, FRT_OFFS, &
     &                              TIM_ARR, WEI_PNT, PH_ACC, UV, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_APPLY_ACCEL 
! *                                                                      *
! * ### 04-OCT-2012 PIMA_APPLY_ACCEL v2.0 (c) L. Petrov  09-FEB-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  LCHN, LFRQ, LTIM, IUER
      COMPLEX*8  UV(LCHN,LFRQ,LTIM)
      REAL*4     WEI_PNT(LTIM)
      REAL*8     FRT_OFFS, TIM_ARR(LTIM), PH_ACC
      REAL*8     NOR_MAT(6), NOR_VEC(3), RCOND, FREQ_REF, &
     &           OBS_EQU(3), EST_VEC(3), RH, RC, PH_DEL_TERM, &
     &           PH_RAT_TERM, PH_ACC_TERM
      REAL*4     PHAS_ADD
#ifdef GNU
      INTEGER*4     NTHR, NTHR_SAVED
#else
      ADDRESS__TYPE NTHR, NTHR_SAVED
#endif
      INTEGER*4  J1, J2, J3, J4, J5, J6, IFRQ, IER
!
! --- Set the refrence frequency
!
      IF ( PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%SIDE_BAND == 1 ) THEN
           FREQ_REF = PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ
        ELSE 
           FREQ_REF = PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ + &
     &                PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%BAND_WIDTH - &
     &                PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%CHAN_WIDTH
      END IF
!
      NOR_MAT = 0.0D0
      NOR_VEC = 0.0D0
      DO 410 J1=1,LTIM
         DO 420 J2=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            IFRQ = IFRQ + 1
            DO 430 J3=1,LCHN
               OBS_EQU(1) = PI2
               OBS_EQU(2) = PI2*(PIM%FREQ_ARR(J3,J2,PIM%CONF%FRQ_GRP) - FREQ_REF)/FREQ_REF* &
     &                          (TIM_ARR(J1) - FRT_OFFS)
               OBS_EQU(3) = PI2*PIM%FREQ_ARR(J3,J2,PIM%CONF%FRQ_GRP)/FREQ_REF* &
     &                          (TIM_ARR(J1) - FRT_OFFS)**2/2.0D0
               RH = PH_ACC* PI2*PIM%FREQ_ARR(J3,J2,PIM%CONF%FRQ_GRP)/FREQ_REF* &
     &                     (TIM_ARR(J1) - FRT_OFFS)**2/2.D0
!
               NOR_MAT(1) = NOR_MAT(1) + OBS_EQU(1)*OBS_EQU(1)*WEI_PNT(J1)**2
               NOR_MAT(2) = NOR_MAT(2) + OBS_EQU(1)*OBS_EQU(2)*WEI_PNT(J1)**2
               NOR_MAT(3) = NOR_MAT(3) + OBS_EQU(2)*OBS_EQU(2)*WEI_PNT(J1)**2
               NOR_MAT(4) = NOR_MAT(4) + OBS_EQU(1)*OBS_EQU(3)*WEI_PNT(J1)**2
               NOR_MAT(5) = NOR_MAT(5) + OBS_EQU(2)*OBS_EQU(3)*WEI_PNT(J1)**2
               NOR_MAT(6) = NOR_MAT(6) + OBS_EQU(3)*OBS_EQU(3)*WEI_PNT(J1)**2
!
               NOR_VEC(1) = NOR_VEC(1) + OBS_EQU(1)*RH*WEI_PNT(J1)**2
               NOR_VEC(2) = NOR_VEC(2) + OBS_EQU(2)*RH*WEI_PNT(J1)**2
               NOR_VEC(3) = NOR_VEC(3) + OBS_EQU(3)*RH*WEI_PNT(J1)**2
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS ( 3, NOR_MAT, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7881, IUER, 'PIMA_APPLY_ACCEL', 'Failre to invert'// &
     &         ' normal matrix in computing quadratic regression' )
           RETURN 
      END IF
!
      IER = 0
      CALL MUL_MV_SV_V ( 3, NOR_MAT, 3, NOR_VEC, 3, EST_VEC, IER )
      PH_DEL_TERM = EST_VEC(1)
      PH_RAT_TERM = EST_VEC(2)
      PH_ACC_TERM = EST_VEC(3)
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 11 ) THEN
           WRITE ( 6, 110 ) PH_ACC, PH_DEL_TERM, &
     &                      PH_RAT_TERM, PH_ACC_TERM 
 110       FORMAT ( 'PIMA_APPLY_ACCEL Phs_acc: ', 1PD12.5, &
     &              ' Ph_del: ', 1PD12.5, ' Ph_rat: ', 1PD12.5, &
     &              ' Ph_acc: ', 1PD12.5 )
      END IF
!
      FREQ_REF = PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ
!
!
      NTHR = PIM%CONF%NUM_THREADS
      CALL OMP_SET_NUM_THREADS ( %VAL(NTHR) )
!
!$OMP PARALLEL DO IF ( NTHR > 1 ), &
!$OMP&         PRIVATE  ( J4, J5, J6, IFRQ, PHAS_ADD ) &
!$OMP&         SCHEDULE ( GUIDED )
      DO 440 J4=1,LTIM
         IFRQ = 0
         DO 450 J5=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            IFRQ = IFRQ + 1
            DO 460 J6=1,LCHN
               PHAS_ADD =  - PH_DEL_TERM* &
     &                          PI2*FREQ_REF &
     &                     - PH_RAT_TERM* &
     &                          PI2*PIM%FREQ_ARR(J6,J5,PIM%CONF%FRQ_GRP)* &
     &                          (TIM_ARR(J4) - FRT_OFFS) &
     &                     + PH_ACC* &
     &                          PI2*PIM%FREQ_ARR(J6,J5,PIM%CONF%FRQ_GRP)* &
     &                          (TIM_ARR(J4) - FRT_OFFS)**2/2.D0
!
               UV(J6,IFRQ,J4) = UV(J6,IFRQ,J4)*CMPLX( COS(PHAS_ADD), SIN(PHAS_ADD) )
 460        CONTINUE 
 450     CONTINUE 
 440  CONTINUE 
!$OMP END PARALLEL DO
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_APPLY_ACCEL  !#!#
