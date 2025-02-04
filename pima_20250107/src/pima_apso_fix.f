      SUBROUTINE PIMA_APSO_FIX ( PIM, VTD, IND_OBS, LFRQ, LTIM, LCHN,        &
     &                           DEL_APSO, RAT_APSO, ACC_APSO, DEL_QUAD_MAX, &
     &                           PHS_APSO_MAX, PHS_APSO_LIM, TIM_ARR,       &
     &                           FL_UPD_PHR, UV, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_FIX_APSO computes, delay, delay rate, and delay       *
! *   acceleration due to the differences between the a priori source    *
! *   positions specified in the PIMA control file and the posiotions    *
! *   used during correlation. The quadratic term is applied to the      *
! *   fringe phases.                                                     *
! *                                                                      *
! *  ### 13-AUG-2010  PIMA_APSO_FOX  v5.0 (c) L. Petrov  24-MAY-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      TYPE     ( VTD__TYPE   ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      INTEGER*4  IND_OBS, LFRQ, LTIM, LCHN, IUER
      REAL*8     DEL_APSO, RAT_APSO, ACC_APSO, DEL_QUAD_MAX, PHS_APSO_MAX, &
     &           PHS_APSO_LIM, TIM_ARR(LTIM)
      LOGICAL*1  FL_UPD_PHR
      COMPLEX*8  UV(LCHN,LFRQ,LTIM)
      INTEGER*4  IER
      INTEGER*4, EXTERNAL :: VTD_SOU_INDEX, ILEN, I_LEN
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_APSO_COMP ( PIM, VTD, IND_OBS, DEL_APSO, RAT_APSO, &
     &                      ACC_APSO, DEL_QUAD_MAX, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7861, IUER, 'PIMA_APSO_FIX', 'Error in an '// &
     &         'attempt to compute the contribution to delay, delay rate, '// &
     &         'and delay acceleration due to changes in a priori '// &
     &         'positions of '//PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND) )
           RETURN 
      END IF
      PHS_APSO_MAX = MAX ( PI2*PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)*DEL_QUAD_MAX, &
     &                     PI2*PIM%FREQ_ARR(PIM%NCHN,PIM%CONF%END_FRQ,PIM%CONF%FRQ_GRP)*DEL_QUAD_MAX  &
     &                  )                    
      IF ( PHS_APSO_MAX > PHS_APSO_LIM ) THEN
           LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
           LTIM = PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
           LCHN = PIM%NCHN
!
! -------- If the maximum contribution is greater than the limit,
! -------- apply quadratic term to the phase
! -------- we apply phase delay acceleration and if requested, apply group delay rate
! -------- Phase delay and phase delay rate kept unchachanged!
! -------- They are accounted when we compute total delays and phase delay from 
! -------- residuals quantities by PIMA_APR_DELAY
!
           CALL PIMA_APSO_APPLY ( PIM, IND_OBS, LCHN, LFRQ, LTIM, RAT_APSO, &
     &                            ACC_APSO, DEL_QUAD_MAX, TIM_ARR, FL_UPD_PHR, UV )
      END IF
      CALL ERR_LOG ( 0, IUER ) 
      RETURN
      END  SUBROUTINE  PIMA_APSO_FIX  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_APSO_COMP ( PIM, VTD, IND_OBS, DEL_APSO, RAT_APSO, &
     &                            ACC_APSO, DEL_QUAD_MAX, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_APSO_COMP 
! *                                                                      *
! * ### 24-MAY-2020  PIMA_APSO_COMP   v1.1 (c) L. Petrov 14-AUG-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      TYPE     ( VTD__TYPE   ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      INTEGER*4  IND_OBS, IUER
      REAL*8     FRT_OFFS, DEL_APSO, RAT_APSO, ACC_APSO, DEL_QUAD_MAX
      REAL*8     DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), TAU_PH, RATE_PH
      REAL*8     ALPHA_SAVE, DELTA_SAVE, S_VEC_SAVE(3), TIM_ARR(3), &
     &           TIM_BEG, TIM_END, DEL_ARR(3,2), RAT_ARR(3,2)
      REAL*8     TIM_EPS
      PARAMETER  ( TIM_EPS = 0.001D0 )
      INTEGER*2  IND_STA(2), IND_SOU
      INTEGER*4  LTIM, FRG_IND, J1, J2, VTD_SOU_IND, IER
      INTEGER*4, EXTERNAL :: VTD_SOU_INDEX, ILEN, I_LEN
!
      IND_STA = PIM%OBS(IND_OBS)%STA_IND
      IND_SOU = PIM%OBS(IND_OBS)%SOU_IND
!
      OBS_TYP%PLRZ    = 'RR'     
      OBS_TYP%FRQ_REF(1) = PIM%REF_FREQ
      OBS_TYP%FRQ_REF(2) = 0.0D0
      OBS_TYP%N_BND      = 1
      OBS_TYP%DELAY_TYPE = VTD__ML__DTP
      OBS_TYP%FRQ_ION_EFF(1) = 2.3D9
      OBS_TYP%FRQ_ION_EFF(2) = 0.0D0
      OBS_TYP%STATUS     = VTD__BND 
!
      LTIM    = PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
      FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
      TIM_BEG = PIM%TIM_R8( PIM%UV_IND( PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND) )%TIM_IND )
      TIM_END = PIM%TIM_R8( PIM%UV_IND( PIM%OBS(IND_OBS)%UV_IND(LTIM,FRG_IND) )%TIM_IND )
      TIM_ARR(1) = 0.D0
      TIM_ARR(2) = PIM%OBS(IND_OBS)%FRT_OFFSET(1)
      TIM_ARR(3) = TIM_END - TIM_BEG
!
! --- Remember the index of the observed source
!
      VTD_SOU_IND = VTD_SOU_INDEX ( VTD, PIM%C_SOU(IND_SOU) )
!
! --- Disable automatic NERS update during the run
!
      VTD%NERS%CNF%AGE_FCS = 1.D15
      VTD%NERS%CNF%AGE_SPL = 1.D15
!
!!   write ( 6, * ) 'TIM_BEG= ', TIM_BEG, ' TIM_ARR= ', tim_arr(1:3) ! %%%%%%%%%%%%%%%%%
      DO 410 J1=1,3
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_DELAY ( PIM%C_SOU(IND_SOU), PIM%C_STA(IND_STA(1)), &
     &                    PIM%C_STA(IND_STA(2)), PIM%MJD_0, &
     &                    PIM%TAI_0 + TIM_BEG + TIM_ARR(J1), &
     &                    OBS_TYP, VTD, DEL_ARR(J1,1), RAT_ARR(J1,1), &
     &                    DER_DEL, DER_RAT, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7861, IUER, 'PIMA_APSO_COMP', 'Error in an '// &
     &            'attempt to compute VLBI time delay for '// &
     &             PIM%C_SOU(IND_SOU) )
              RETURN 
         END IF
 410  CONTINUE 
!
! --- Store the nominal source position in the temporary place
!
      ALPHA_SAVE = VTD%SOU(VTD_SOU_IND)%ALPHA
      DELTA_SAVE = VTD%SOU(VTD_SOU_IND)%DELTA
      S_VEC_SAVE = VTD%SOU(VTD_SOU_IND)%S_CRS
!
! --- Put into the VTD data structure source positions used for correlation
!
      IF ( PIM%SOU(IND_SOU)%IND_SWAP == 0 ) THEN
           VTD%SOU(VTD_SOU_IND)%ALPHA = PIM%SOU(IND_SOU)%ALPHA_INP
           VTD%SOU(VTD_SOU_IND)%DELTA = PIM%SOU(IND_SOU)%DELTA_INP
        ELSE
           VTD%SOU(VTD_SOU_IND)%ALPHA = PIM%SOU(PIM%SOU(IND_SOU)%IND_SWAP)%ALPHA_INP
           VTD%SOU(VTD_SOU_IND)%DELTA = PIM%SOU(PIM%SOU(IND_SOU)%IND_SWAP)%DELTA_INP
      END IF
!
      VTD%SOU(VTD_SOU_IND)%S_CRS(1) = DCOS(VTD%SOU(VTD_SOU_IND)%DELTA)* &
     &                                DCOS(VTD%SOU(VTD_SOU_IND)%ALPHA)
      VTD%SOU(VTD_SOU_IND)%S_CRS(2) = DCOS(VTD%SOU(VTD_SOU_IND)%DELTA)* &
     &                                DSIN(VTD%SOU(VTD_SOU_IND)%ALPHA)
      VTD%SOU(VTD_SOU_IND)%S_CRS(3) = DSIN(VTD%SOU(VTD_SOU_IND)%DELTA)
!
      DO 420 J2=1,3
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_DELAY ( PIM%C_SOU(IND_SOU), PIM%C_STA(IND_STA(1)), &
     &                    PIM%C_STA(IND_STA(2)), PIM%MJD_0, &
     &                    PIM%TAI_0 + TIM_BEG + TIM_ARR(J2), &
     &                    OBS_TYP, VTD, DEL_ARR(J2,2), RAT_ARR(J2,2), &
     &                    DER_DEL, DER_RAT, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7862, IUER, 'PIMA_APSO_COMP', 'Error in an '// &
     &            'attempt to compute VLBI time delay for '// &
     &             PIM%C_SOU(IND_SOU) )
              RETURN 
         END IF
 420  CONTINUE 
!
! --- Restore nominal source positions
!
      VTD%SOU(VTD_SOU_IND)%ALPHA = ALPHA_SAVE 
      VTD%SOU(VTD_SOU_IND)%DELTA = DELTA_SAVE 
      VTD%SOU(VTD_SOU_IND)%S_CRS = S_VEC_SAVE 
!
      DEL_APSO  = DEL_ARR(2,1) - DEL_ARR(2,2) 
      RAT_APSO  = RAT_ARR(2,1) - RAT_ARR(2,2) 
      IF ( (TIM_ARR(3) - TIM_ARR(1)) > TIM_EPS ) THEN
           ACC_APSO  = ( (RAT_ARR(3,1) - RAT_ARR(3,2)) - (RAT_ARR(1,1) - RAT_ARR(1,2)) )/ &
     &                   (TIM_ARR(3) - TIM_ARR(1))
         ELSE
           ACC_APSO  = 0.0
      END IF
      DEL_QUAD_MAX = MAX ( ABS(ACC_APSO*(TIM_ARR(3) - TIM_ARR(2))**2/2.0D0), &
     &                     ABS(ACC_APSO*(TIM_ARR(1) - TIM_ARR(2))**2/2.0D0)  )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_APSO_COMP   !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_APSO_APPLY ( PIM, IND_OBS, LCHN, LFRQ, LTIM, &
     &                             RAT_APSO, ACC_APSO, DEL_QUAD_MAX, &
     &                             TIM_ARR, FL_UPD_PHR, UV )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_APSO_APPLY
! *                                                                      *
! * ### 24-MAY-2020  PIMA_APSO_APPLY  v1.0 (c) L. Petrov 24-MAY-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  LFRQ, LTIM, LCHN, IND_OBS, IUER
      COMPLEX*8  UV(LCHN,LFRQ,LTIM)
      LOGICAL*1  FL_UPD_PHR
      REAL*8     TIM_ARR(*), RAT_APSO, ACC_APSO, DEL_QUAD_MAX
      REAL*4     PHAS_ADD
      REAL*8     FREQ_REF
      INTEGER*4  IFRQ, J1, J2, J3
      INTEGER*4, EXTERNAL :: VTD_SOU_INDEX, ILEN, I_LEN
!
      FREQ_REF = PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ
      DO 410 J1=1,LTIM
         IFRQ = 0
         DO 420 J2=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            IFRQ = IFRQ + 1
            DO 430 J3=1,LCHN
               PHAS_ADD =  PI2*PIM%FREQ_ARR(J3,J2,PIM%CONF%FRQ_GRP)* &
     &                     (TIM_ARR(J1) - PIM%OBS(IND_OBS)%FRT_OFFSET(1))**2/2.D0*ACC_APSO 
               IF ( FL_UPD_PHR ) THEN
                    PHAS_ADD = PHAS_ADD + PI2*(PIM%FREQ_ARR(J3,J2,PIM%CONF%FRQ_GRP) - FREQ_REF)* &
     &                                        (TIM_ARR(J1) - PIM%OBS(IND_OBS)%FRT_OFFSET(1))*RAT_APSO
               END IF
               UV(J3,IFRQ,J1) = UV(J3,IFRQ,J1)*CMPLX( COS(PHAS_ADD), SIN(PHAS_ADD) )
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
!
      RETURN
      END  SUBROUTINE  PIMA_APSO_APPLY  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_UV_APR_APPLY ( PIM, LCHN, LFRQ, LTIM, TIM_FRT, TIM_ARR, &
     &                               APR_GR_DEL, APR_PH_RAT, APR_PH_ACC, &
     &                               UV )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_UV_APR_APPLY
! *                                                                      *
! * ### 31-JUL-2022 PIMA_UV_APR_APPLY v1.0 (c) L. Petrov 31-JUL-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  LFRQ, LTIM, LCHN, IUER
      INTEGER*4  IND_MD(LCHN,LFRQ), IND_RT(LTIM)
      COMPLEX*8  UV(LCHN,LFRQ,LTIM)
      LOGICAL*1  FL_UPD_PHR
      REAL*8     TIM_FRT, TIM_ARR(LTIM), APR_GR_DEL, APR_PH_RAT, APR_PH_ACC
      REAL*8     PHAS_ADD, FREQ_REF, FREQ_DIF, TIM_DIF
      INTEGER*4  IFRQ, J1, J2, J3
!
      FREQ_REF = PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ
      DO 410 J1=1,LTIM
         TIM_DIF = TIM_ARR(J1) - TIM_FRT
         IFRQ = 0
         DO 420 J2=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            IFRQ = IFRQ + 1
            DO 430 J3=1,LCHN
               FREQ_DIF = PIM%FREQ_ARR(J3,J2,PIM%CONF%FRQ_GRP) - FREQ_REF
               PHAS_ADD =   APR_PH_RAT*PI2*FREQ_REF*TIM_DIF &
     &                    + APR_GR_DEL*PI2*FREQ_DIF         &
     &                    + APR_PH_ACC*PI2*FREQ_REF*TIM_DIF**2/2.0D0
               UV(J3,IFRQ,J1) = UV(J3,IFRQ,J1)*CMPLX( COS(PHAS_ADD), SIN(PHAS_ADD) )
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
!
      RETURN
      END  SUBROUTINE  PIMA_UV_APR_APPLY  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_UV2_APR_APPLY ( PIM, LCHN, LFRQ, LTIM, TIM_FRT, TIM_ARR, &
     &                                L_MD, L_RT, IND_MD, IND_RT, &
     &                                APR_GR_DEL, APR_PH_RAT, APR_PH_ACC, &
     &                                UV_2D )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_UV2_APR_APPLY
! *                                                                      *
! * ### 31-JUL-2022 PIMA_UV2_APR_APPLY v1.0 (c) L. Petrov 31-JUL-2022 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  LFRQ, LTIM, LCHN, L_MD, L_RT, IUER
      INTEGER*4  IND_MD(LCHN,LFRQ), IND_RT(LTIM)
      COMPLEX*8  UV_2D(L_MD,L_RT)
      LOGICAL*1  FL_UPD_PHR
      REAL*8     TIM_FRT, TIM_ARR(LTIM), APR_GR_DEL, APR_PH_RAT, APR_PH_ACC
      REAL*8     PHAS_ADD, FREQ_REF, FREQ_DIF, TIM_DIF
      INTEGER*4  IFRQ, J1, J2, J3
!
      FREQ_REF = PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ
      DO 410 J1=1,LTIM
         TIM_DIF = TIM_ARR(J1) - TIM_FRT
         IFRQ = 0
         DO 420 J2=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            IFRQ = IFRQ + 1
            DO 430 J3=1,LCHN
               FREQ_DIF = PIM%FREQ_ARR(J3,J2,PIM%CONF%FRQ_GRP) - FREQ_REF
               PHAS_ADD =   APR_PH_RAT*PI2*FREQ_REF*TIM_DIF &
     &                    + APR_GR_DEL*PI2*FREQ_DIF         &
     &                    + APR_PH_ACC*PI2*FREQ_REF*TIM_DIF**2/2.0D0
               IF ( IND_MD(J3,IFRQ) > 0 .AND. IND_RT(J1) > 0 ) THEN
                    UV_2D(IND_MD(J3,IFRQ),IND_RT(J1)) = UV_2D(IND_MD(J3,IFRQ),IND_RT(J1))* &
     &                                                CMPLX( COS(PHAS_ADD), SIN(PHAS_ADD) )
               END IF
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
!
      RETURN
      END  SUBROUTINE  PIMA_UV2_APR_APPLY  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_FIX_APSO_OLD ( PIM, VTD, LCHN, LFRQ, LTIM, IND_STA, &
     &                           IND_SOU, TIME_BEG, FRT_OFFS, TIM_ARR, &
     &                           PHS_APSO_LIM, FL_UPD_PHR, UV, PH_DEL_APSO, &
     &                           PH_RAT_APSO, PH_ACC_APSO, PHS_APSO_MAX, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_FIX_APSO computes, delay, delay rate, and delay       *
! *   accelerating due to the differences between the a priori source    *
! *   positions specified in the PIMA control file and the posiotions    *
! *   used during correlation. The quadratic term is applied to the      *
! *   fringe phases.                                                     *
! *                                                                      *
! *  ### 13-AUG-2010  PIMA_FIX_APSO  v4.1 (c) L. Petrov  24-MAY-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      TYPE     ( VTD__TYPE   ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      INTEGER*4  LCHN, LFRQ, LTIM, IUER
      INTEGER*2  IND_STA(2), IND_SOU
      REAL*8     TIME_BEG, FRT_OFFS, TIM_ARR(LTIM), PHS_APSO_LIM, &
     &           PH_DEL_APSO, PH_RAT_APSO, PH_ACC_APSO, PHS_APSO_MAX
      COMPLEX*8  UV(LCHN,LFRQ,LTIM)
      LOGICAL*1  FL_UPD_PHR
      REAL*8     DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), TAU_PH, RATE_PH
      REAL*8     DEL_ARR(PIM__MUV,2), RAT_ARR(PIM__MUV,2), &
     &           ALPHA_SAVE, DELTA_SAVE, S_VEC_SAVE(3)
      REAL*8     NOR_MAT(6), NOR_VEC(3), RCOND, FREQ_REF, &
     &           OBS_EQU(3), EST_VEC(3), TIM_AVR, DEL_FRT(2), RAT_FRT(2)
#ifdef GNU
      INTEGER*4     NTHR, NTHR_SAVED
#else
      ADDRESS__TYPE NTHR, NTHR_SAVED
#endif
      REAL*4     PHAS_ADD
      LOGICAL*1  FL_ERROR
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, IFRQ, VTD_SOU_IND, IER 
      INTEGER*4, EXTERNAL :: VTD_SOU_INDEX, OMP_GET_THREAD_NUM, &
     &                       GET_PROC_INFO, ILEN, I_LEN
!
!@      WRITE ( 6, * ) 'BEG   of PIMA_FIX_APSO RssAnon: ', GET_PROC_INFO ( 'RssAnon' ) ! %%%%%%%
      NTHR = PIM%CONF%NUM_THREADS
      CALL OMP_SET_NUM_THREADS ( %VAL(NTHR) )
!
! --- Initialization
!
      PHS_APSO_MAX = 0.0D0
      PH_DEL_APSO  = 0.0D0
      PH_RAT_APSO  = 0.0D0
      PH_ACC_APSO  = 0.0D0
!
      OBS_TYP%PLRZ    = 'RR'     
      OBS_TYP%FRQ_REF(1) = PIM%REF_FREQ
      OBS_TYP%FRQ_REF(2) = 0.0D0
      OBS_TYP%N_BND      = 1
      OBS_TYP%DELAY_TYPE = VTD__ML__DTP
      OBS_TYP%FRQ_ION_EFF(1) = 2.3D9
      OBS_TYP%FRQ_ION_EFF(2) = 0.0D0
      OBS_TYP%STATUS     = VTD__BND 
!
! --- Remember the index of the observed source
!
      VTD_SOU_IND = VTD_SOU_INDEX ( VTD, PIM%C_SOU(IND_SOU) )
!
! --- Disable automatic NERS update during the run
!
      VTD%NERS%CNF%AGE_FCS = 1.D15
      VTD%NERS%CNF%AGE_SPL = 1.D15
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_DELAY ( PIM%C_SOU(IND_SOU), PIM%C_STA(IND_STA(1)), &
     &                 PIM%C_STA(IND_STA(2)), PIM%MJD_0, &
     &                 PIM%TAI_0 + TIME_BEG + FRT_OFFS, &
     &                 OBS_TYP, VTD, DEL_FRT(1), RAT_FRT(1), &
     &                 DER_DEL, DER_RAT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7851, IUER, 'PIMA_FIX_APSO', 'Error in an '// &
     &         'attempt to compute VLBI time delay for '// &
     &          PIM%C_SOU(IND_SOU) )
           RETURN 
      END IF
!
      FL_ERROR = .FALSE.
!
! --- Compute path delay for the nominal position for each time epoch
!
!$OMP PARALLEL DO IF    ( NTHR > 1 ), &
!$OMP&         PRIVATE  ( J1, TAU_PH, RATE_PH, DER_DEL, DER_RAT, IER ), &
!$OMP&         FIRSTPRIVATE  ( VTD ), &
!$OMP&         SCHEDULE ( STATIC )
      DO 410 J1=1,LTIM
         IF ( FL_ERROR ) GOTO 810
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_DELAY ( PIM%C_SOU(IND_SOU), PIM%C_STA(IND_STA(1)), &
     &                    PIM%C_STA(IND_STA(2)), PIM%MJD_0, &
     &                    PIM%TAI_0 + TIME_BEG + TIM_ARR(J1), &
     &                    OBS_TYP, VTD, TAU_PH, RATE_PH, &
     &                    DER_DEL, DER_RAT, IER )
         IF ( IER .NE. 0 ) THEN
!$OMP CRITICAL
              CALL ERR_LOG ( 7851, IUER, 'PIMA_FIX_APSO', 'Error in an '// &
     &            'attempt to compute VLBI time delay for '// &
     &             PIM%C_SOU(IND_SOU) )
              FL_ERROR = .TRUE.
!$OMP END CRITICAL
              GOTO 810
         END IF
!
! ------ Store path delay
!
         DEL_ARR(J1,1) = TAU_PH  - DEL_FRT(1)
         RAT_ARR(J1,1) = RATE_PH - RAT_FRT(1)
 810     CONTINUE 
 410  CONTINUE 
!$OMP END PARALLEL DO
      IF ( FL_ERROR ) RETURN 
!
! --- Store the nominal source position in the temporary place
!
      ALPHA_SAVE = VTD%SOU(VTD_SOU_IND)%ALPHA
      DELTA_SAVE = VTD%SOU(VTD_SOU_IND)%DELTA
      S_VEC_SAVE = VTD%SOU(VTD_SOU_IND)%S_CRS
!
! --- Put into the VTD data structure source positions used for correlation
!
      IF ( PIM%SOU(IND_SOU)%IND_SWAP == 0 ) THEN
           VTD%SOU(VTD_SOU_IND)%ALPHA = PIM%SOU(IND_SOU)%ALPHA_INP
           VTD%SOU(VTD_SOU_IND)%DELTA = PIM%SOU(IND_SOU)%DELTA_INP
        ELSE
           VTD%SOU(VTD_SOU_IND)%ALPHA = PIM%SOU(PIM%SOU(IND_SOU)%IND_SWAP)%ALPHA_INP
           VTD%SOU(VTD_SOU_IND)%DELTA = PIM%SOU(PIM%SOU(IND_SOU)%IND_SWAP)%DELTA_INP
      END IF
!
      VTD%SOU(VTD_SOU_IND)%S_CRS(1) = DCOS(VTD%SOU(VTD_SOU_IND)%DELTA)* &
     &                                DCOS(VTD%SOU(VTD_SOU_IND)%ALPHA)
      VTD%SOU(VTD_SOU_IND)%S_CRS(2) = DCOS(VTD%SOU(VTD_SOU_IND)%DELTA)* &
     &                                DSIN(VTD%SOU(VTD_SOU_IND)%ALPHA)
      VTD%SOU(VTD_SOU_IND)%S_CRS(3) = DSIN(VTD%SOU(VTD_SOU_IND)%DELTA)
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_DELAY ( PIM%C_SOU(IND_SOU), PIM%C_STA(IND_STA(1)), &
     &                 PIM%C_STA(IND_STA(2)), PIM%MJD_0, &
     &                 PIM%TAI_0 + TIME_BEG + FRT_OFFS, &
     &                 OBS_TYP, VTD, DEL_FRT(2), RAT_FRT(2), &
     &                 DER_DEL, DER_RAT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7851, IUER, 'PIMA_FIX_APSO', 'Error in an '// &
     &         'attempt to compute VLBI time delay for '// &
     &          PIM%C_SOU(IND_SOU) )
           RETURN 
      END IF
!
! --- Compute path delay for the source position used for correlation
! --- for each time epoch
!
      FL_ERROR = .FALSE.
!$OMP PARALLEL DO IF    ( NTHR > 1 ), &
!$OMP&         PRIVATE  ( J2, TAU_PH, RATE_PH, DER_DEL, DER_RAT, IER ), &
!$OMP&         FIRSTPRIVATE  ( VTD ), &
!$OMP&         SCHEDULE ( STATIC )
      DO 420 J2=1,LTIM
         IF ( FL_ERROR ) GOTO 820
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_DELAY ( PIM%C_SOU(IND_SOU), PIM%C_STA(IND_STA(1)), &
     &                    PIM%C_STA(IND_STA(2)), PIM%MJD_0, &
     &                    PIM%TAI_0 + TIME_BEG + TIM_ARR(J2), &
     &                    OBS_TYP, VTD, TAU_PH, RATE_PH, &
     &                    DER_DEL, DER_RAT, IER )
         IF ( IER .NE. 0 ) THEN
!$OMP CRITICAL
              CALL ERR_LOG ( 7852, IUER, 'PIMA_FIX_APSO', 'Error in an '// &
     &            'attempt to compute VLBI time delay for '// &
     &             PIM%C_SOU(IND_SOU) )
              FL_ERROR = .TRUE.
!$OMP END CRITICAL
              GOTO 820
         END IF
!
! ------ Store the phase delay
!
         DEL_ARR(J2,2) = TAU_PH  - DEL_FRT(2)
         RAT_ARR(J2,2) = RATE_PH - RAT_FRT(2)
 820     CONTINUE 
 420  CONTINUE 
!$OMP END PARALLEL DO
      IF ( FL_ERROR ) RETURN 
!
! --- Compute the average time
!
      TIM_AVR = 0.0D0
      DO 430 J3=1,LTIM
         TIM_AVR = TIM_AVR + (TIM_ARR(J3) - FRT_OFFS)
 430  CONTINUE 
      TIM_AVR = TIM_AVR/LTIM
!
! --- Now we fit the second order polynomial for the average time tag
!
      CALL NOUT_R8 ( 6, NOR_MAT )
      CALL NOUT_R8 ( 3, NOR_VEC )
      DO 440 J4=1,LTIM
         OBS_EQU(1) =  1.0D0
         OBS_EQU(2) =  TIM_ARR(J4) - FRT_OFFS
         OBS_EQU(3) = (TIM_ARR(J4) - FRT_OFFS)**2/2.0D0
!
         NOR_MAT(1) = NOR_MAT(1) + OBS_EQU(1)*OBS_EQU(1)
         NOR_MAT(2) = NOR_MAT(2) + OBS_EQU(1)*OBS_EQU(2)
         NOR_MAT(3) = NOR_MAT(3) + OBS_EQU(2)*OBS_EQU(2)
         NOR_MAT(4) = NOR_MAT(4) + OBS_EQU(1)*OBS_EQU(3)
         NOR_MAT(5) = NOR_MAT(5) + OBS_EQU(2)*OBS_EQU(3)
         NOR_MAT(6) = NOR_MAT(6) + OBS_EQU(3)*OBS_EQU(3)
!
         NOR_VEC(1) = NOR_VEC(1) + OBS_EQU(1)*(DEL_ARR(J4,1) - DEL_ARR(J4,2))
         NOR_VEC(2) = NOR_VEC(2) + OBS_EQU(2)*(DEL_ARR(J4,1) - DEL_ARR(J4,2))
         NOR_VEC(3) = NOR_VEC(3) + OBS_EQU(3)*(DEL_ARR(J4,1) - DEL_ARR(J4,2))
 440  CONTINUE 
!
      IF ( PIM%CONF%DEBUG_LEVEL == 6 ) THEN
           CALL CLRCH  ( STR )
           CALL RH_TAT ( VTD%SOU(VTD_SOU_IND)%ALPHA, 6, STR(1:20),  -2 )
           CALL RG_TAT ( VTD%SOU(VTD_SOU_IND)%DELTA, 5, STR(21:40), -2 )
           CALL RH_TAT ( ALPHA_SAVE, 6, STR(41:60), -2 )
           CALL RG_TAT ( DELTA_SAVE, 5, STR(61:80), -2 )
!     
           WRITE ( 6, * ) 'PIMA_FIX_APSO Sou: '//PIM%C_SOU(IND_SOU)// &
          &               ' Correl '//STR(1:16)//'    '//STR(21:35)
           WRITE ( 6, * ) '              Sou: '//PIM%C_SOU(IND_SOU)// &
          &               ' New    '//STR(41:56)//'    '//STR(61:75)
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS ( 3, NOR_MAT, RCOND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7853, IUER, 'PIMA_FIX_APSO', 'Failure to invert'// &
     &         ' normal matrix in computing quadratic regression over the'// &
     &         ' contribution of the 3rd derivative for '//PIM%C_SOU(IND_SOU) )
           RETURN 
      END IF
!
      IER = 0
      CALL MUL_MV_SV_V ( 3, NOR_MAT, 3, NOR_VEC, 3, EST_VEC, IER )
!
! --- Now we compute the path delay, path delay rate, and phase acceleration
! --- at the FRT epoch
!
      PH_DEL_APSO = (DEL_FRT(1) - DEL_FRT(2)) + EST_VEC(1)
      PH_RAT_APSO =  EST_VEC(2)
      PH_ACC_APSO =  EST_VEC(3)
!
! --- Compute maximum contribution to phase due to phase acceleration
!
      PHS_APSO_MAX = 0.0D0
      DO 450 J5=1,LTIM
         IF ( ABS(PH_ACC_APSO*(TIM_ARR(J5) - FRT_OFFS)**2/2.D0* &
     &             PI2*PIM%FREQ_ARR(LCHN,PIM%CONF%END_FRQ,PIM%CONF%FRQ_GRP)) > PHS_APSO_MAX ) THEN
              PHS_APSO_MAX = ABS(PH_ACC_APSO*(TIM_ARR(J5) - FRT_OFFS)**2/2.D0* &
     &             PI2*PIM%FREQ_ARR(LCHN,PIM%CONF%END_FRQ,PIM%CONF%FRQ_GRP))
        END IF
 450  CONTINUE 
!
      IF ( PHS_APSO_MAX > PHS_APSO_LIM ) THEN
!
! -------- If the maximum contribution is greater than the limit,
! -------- apply quadratic term to the phase
! -------- we apply phase delay acceleration and if requested, apply group delay rate
! -------- Phase delay and phase delay rate kept unchachanged!
! -------- They are accounted when we compute total delays and phase delay from 
! -------- residuals quantities by PIMA_APR_DELAY
!
           FREQ_REF = PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ
           DO 460 J6=1,LTIM
              IFRQ = 0
              DO 470 J7=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                 IFRQ = IFRQ + 1
                 DO 480 J8=1,LCHN
                    PHAS_ADD =  PI2*PIM%FREQ_ARR(J8,J7,PIM%CONF%FRQ_GRP)* &
     &                          (TIM_ARR(J6) - FRT_OFFS)**2/2.D0*PH_ACC_APSO 
                    IF ( FL_UPD_PHR ) THEN
                         PHAS_ADD = PHAS_ADD + PI2*(PIM%FREQ_ARR(J8,J7,PIM%CONF%FRQ_GRP) - FREQ_REF)* &
     &                                             (TIM_ARR(J6) - FRT_OFFS)*PH_RAT_APSO
                    END IF
                    UV(J8,IFRQ,J6) = UV(J8,IFRQ,J6)*CMPLX( COS(PHAS_ADD), SIN(PHAS_ADD) )
 480             CONTINUE 
 470          CONTINUE 
 460       CONTINUE 
         ELSE 
           PH_DEL_APSO = 0.0D0
           PH_RAT_APSO = 0.0D0
           PH_ACC_APSO = 0.0D0
      END IF
!
! --- Restore nominal source positions
!
      VTD%SOU(VTD_SOU_IND)%ALPHA = ALPHA_SAVE 
      VTD%SOU(VTD_SOU_IND)%DELTA = DELTA_SAVE 
      VTD%SOU(VTD_SOU_IND)%S_CRS = S_VEC_SAVE 
!
!@      WRITE ( 6, * ) 'END   of PIMA_FIX_APSO RssAnon: ', GET_PROC_INFO ( 'RssAnon' ) ! %%%%%%%
      CALL ERR_LOG ( 0, IUER ) 
      RETURN
      END  SUBROUTINE  PIMA_FIX_APSO_OLD  !#!#
