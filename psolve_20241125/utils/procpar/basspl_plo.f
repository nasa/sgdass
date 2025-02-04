      SUBROUTINE BASSPL_PLO ( L_SPE, SPE, NP, TIM, VAL, ERR, STA, &
     &                        TIM_REF, WEI_SCL, VEL_SCL, EST_VEC, NOR_MAT )
! ************************************************************************
! *                                                                      *
! *   Routine  BASSPL_PLO  
! *                                                                      *
! *  ### 03-MAR-2005   BASSPL_PLO   v1.0 (c) L. Petrov  03-MAR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'diagi.i'
      INTEGER*4  L_SPE, NP
      TYPE       ( SPE__TYPE  ) :: SPE(L_SPE)
      TYPE       ( DIAGI_STRU ) :: DIAGI_S
      REAL*8     TIM_REF, WEI_SCL, TIM(NP), VAL(NP), ERR(NP)
      REAL*8     VEL_SCL, EST_VEC(*), NOR_MAT(*)
      REAL*8     NOD_ARR(M__SPN), NOD_VAL(M__SPN), NOD_ARG(M__SPN), SIG 
      CHARACTER  STA(2)*(*)
      INTEGER*4  M_INT
      PARAMETER  ( M_INT = 64 )
      REAL*8,    ALLOCATABLE :: PLT_ARG(:), PLT_VAL(:), PLT_ERR(:), &
     &                          MOD_ARG(:), MOD_VAL(:), MOD_ERR(:), &
     &                          PLT_RES(:)
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, IER
      CHARACTER  ZAG*128, UNIT*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, KP, MV
      REAL*8     TIM_BEG, TIM_SPAN
!
      MV = M_INT*SPE(1)%L_NOD
      ALLOCATE ( PLT_ARG(NP) )
      ALLOCATE ( PLT_VAL(NP) )
      ALLOCATE ( PLT_RES(NP) )
      ALLOCATE ( PLT_ERR(NP) )
      ALLOCATE ( MOD_ARG(MV) )
      ALLOCATE ( MOD_VAL(MV) )
      ALLOCATE ( MOD_ERR(MV) )
!
      KP = 0
      DO 430 J3=1,SPE(1)%L_NOD-1
         TIM_BEG =  (SPE(1)%MJD(J3)-J2000__MJD)*86400.0D0 + &
     &              (SPE(1)%TAI(J3)-43200.0D0)
         TIM_SPAN = (SPE(1)%MJD(J3+1)-J2000__MJD)*86400.0D0 + &
     &              (SPE(1)%TAI(J3+1)-43200.0D0) - TIM_BEG
         DO 440 J4=1,M_INT
            KP = KP + 1
            MOD_ARG(KP) = TIM_BEG + (J4-1)*TIM_SPAN/(M_INT-1)
            IF ( KP ==  1 ) MOD_ARG(KP) = MOD_ARG(KP) + 1.D-1
            IF ( KP == MV ) MOD_ARG(KP) = MOD_ARG(KP) - 1.D-1
            CALL LINTREND_BSPL ( SPE(1)%K_NOD, SPE(1)%TIM(1), SPE(1)%DEGREE, &
     &                           TIM_REF, 1.D0/(TIM(NP) - TIM(1)), EST_VEC,  &
     &                           NOR_MAT, MOD_ARG(KP), MOD_VAL(KP), MOD_ERR(KP) )
!
            MOD_ARG(KP) = MOD_ARG(KP)/(86400.0D0*365.25D0) + 2000.0D0 
            MOD_VAL(KP) = MOD_VAL(KP)*1.D3
            MOD_ERR(KP) = MOD_ERR(KP)*WEI_SCL*1.D3
 440     CONTINUE 
 430  CONTINUE 
!
      DO 450 J5=1,NP
         PLT_ARG(J5) = TIM(J5)/(86400.0D0*365.25D0) + 2000.0D0
         PLT_VAL(J5) = VAL(J5)*1.D3
         CALL LINTREND_BSPL ( SPE(1)%K_NOD, SPE(1)%TIM(1), SPE(1)%DEGREE, &
     &                        TIM_REF, 1.D0/(TIM(NP) - TIM(1)), EST_VEC,  &
     &                        NOR_MAT, TIM(J5), PLT_RES(J5), SIG )
         PLT_RES(J5) = (VAL(J5) - PLT_RES(J5))*1.D3
         PLT_ERR(J5) = ERR(J5)*1.D3
 450  CONTINUE 
!
      NOD_ARG(1) = NOD_ARG(1) + 0.1D0
      NOD_ARG(SPE(1)%K_NOD) = NOD_ARG(SPE(1)%K_NOD) - 0.1D0
      DO 460 J6=1,SPE(1)%K_NOD
         NOD_ARG(J6) = SPE(1)%TIM(J6) + 1.D-5
         IF ( J6 == SPE(1)%K_NOD ) NOD_ARG(J6) = SPE(1)%TIM(J6) - 1.D-5
         CALL LINTREND_BSPL ( SPE(1)%K_NOD, SPE(1)%TIM(1), SPE(1)%DEGREE, &
     &                        TIM_REF, 1.D0/(TIM(NP) - TIM(1)), EST_VEC,  &
     &                        NOR_MAT,  NOD_ARG(J6), NOD_VAL(J6), SIG )
         NOD_ARG(J6) = SPE(1)%TIM(J6)/(86400.0D0*365.25D0) + 2000.0D0 
         NOD_VAL(J6) = NOD_VAL(J6)*1.D3
 460  CONTINUE 
!
! --- Clear DIAGI_S object
!
      CALL NOUT ( SIZEOF(DIAGI_S), DIAGI_S )
!
! --- Setting defaults values of the plotting parameters
!
      CALL DIAGI_DEF  ( IBST, ILST, IOST, IPST, IWST, IDEV, ZAG, UNIT, &
     &                  ICL1, ICL2, ICL3, IER )
!
! --- Setting up the values of the DIAGI internal data structure for the further
! --- plotting
!
      DIAGI_S%IDEV      = IDEV
      DIAGI_S%NCLR      = 3
      DIAGI_S%NPOI(1)   = KP
      DIAGI_S%ADR_X8(1) = LOC(MOD_ARG)
      DIAGI_S%ADR_Y8(1) = LOC(MOD_VAL)
      DIAGI_S%ADR_E8(1) = LOC(MOD_ERR)
      DIAGI_S%LER(1)    = .TRUE.
      DIAGI_S%ICOL(1)   = ICL2
      DIAGI_S%IBST(1)   = 4
      DIAGI_S%ILST(1)   = 2
      DIAGI_S%IOST(1)   = IOST
      DIAGI_S%IPST(1)   = 1
      DIAGI_S%IWST(1)   = 3
!
      DIAGI_S%NPOI(2)   = NP
      DIAGI_S%ADR_X8(2) = LOC(PLT_ARG)
      DIAGI_S%ADR_Y8(2) = LOC(PLT_VAL)
      DIAGI_S%ADR_E8(2) = LOC(PLT_ERR)
      DIAGI_S%LER(2)    = .TRUE.
      DIAGI_S%ICOL(2)   = ICL1
      DIAGI_S%IBST(2)   = 2
      DIAGI_S%ILST(2)   = ILST
      DIAGI_S%IOST(2)   = IOST
      DIAGI_S%IPST(2)   = 2
      DIAGI_S%IWST(2)   = IWST
!
      DIAGI_S%NPOI(3)   = SPE(1)%K_NOD
      DIAGI_S%ADR_X8(3) = LOC(NOD_ARG)
      DIAGI_S%ADR_Y8(3) = LOC(NOD_VAL)
      DIAGI_S%ADR_E8(3) = 0
      DIAGI_S%LER(3)    = .FALSE.
      DIAGI_S%ICOL(3)   = ICL2
      DIAGI_S%IBST(3)   = 0
      DIAGI_S%ILST(3)   = 1
      DIAGI_S%IOST(3)   = IOST
      DIAGI_S%IPST(3)   = 3
      DIAGI_S%IWST(3)   = 1
!
      DIAGI_S%ICLR      = 1
      DIAGI_S%XMIN      = 1.0
      DIAGI_S%XMAX      = 0.0
      DIAGI_S%YMIN      = 1.0
      DIAGI_S%YMAX      = 0.0
      DIAGI_S%ZAG       = 'Baseline '//STA(1)//'/'//STA(2)//' (mm)'
      DIAGI_S%NAME      = NAME__DEF
      DIAGI_S%ARG_UNITS = 'Time in years'
      DIAGI_S%ITRM      = 0
      DIAGI_S%IBATCH    = 0
      DIAGI_S%STATUS    = DIA__DEF
!
! --- Calling the main routine of DiaGI
!
      CALL DIAGI     ( DIAGI_S, IER )
!
      DIAGI_S%NCLR      = 1
      DIAGI_S%IDEV      = IDEV
      DIAGI_S%NPOI(1)   = NP
      DIAGI_S%ADR_X8(1) = LOC(PLT_ARG)
      DIAGI_S%ADR_Y8(1) = LOC(PLT_RES)
      DIAGI_S%ADR_E8(1) = LOC(PLT_ERR)
      DIAGI_S%LER(1)    = .TRUE.
      DIAGI_S%ICOL(1)   = ICL3
      DIAGI_S%IBST(1)   = 2
      DIAGI_S%ILST(1)   = ILST
      DIAGI_S%IOST(1)   = IOST
      DIAGI_S%IPST(1)   = 2
      DIAGI_S%IWST(1)   = IWST
!
      DIAGI_S%ICLR      = 1
      DIAGI_S%XMIN      = 1.0
      DIAGI_S%XMAX      = 0.0
      DIAGI_S%YMIN      = 1.0
      DIAGI_S%YMAX      = 0.0
      DIAGI_S%ZAG       = 'Baseline '//STA(1)//'/'//STA(2)//' (mm)'
      DIAGI_S%NAME      = NAME__DEF
      DIAGI_S%ARG_UNITS = 'Time in years'
      DIAGI_S%ITRM      = 0
      DIAGI_S%IBATCH    = 0
      DIAGI_S%STATUS    = DIA__DEF
      CALL DIAGI     ( DIAGI_S, IER )
!
      DEALLOCATE ( PLT_ARG )
      DEALLOCATE ( PLT_VAL )
      DEALLOCATE ( PLT_RES )
      DEALLOCATE ( PLT_ERR )
      DEALLOCATE ( MOD_ARG )
      DEALLOCATE ( MOD_VAL )
      DEALLOCATE ( MOD_ERR )
!
      RETURN
      END  SUBROUTINE  BASSPL_PLO
!
! ------------------------------------------------------------------------
!
      SUBROUTINE BASLIN_PLO ( NP, TIM, VAL, ERR, STA, TIM_REF, WEI_SCL, &
     &                        VEL_SCL, EST_VEC, NOR_MAT )
! ************************************************************************
! *                                                                      *
! *   Routine  BASLIN_PLO  
! *                                                                      *
! *  ### 03-MAR-2005   BASLIN_PLO   v1.0 (c) L. Petrov  03-MAR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'diagi.i'
      INTEGER*4  NP
      TYPE       ( DIAGI_STRU ) :: DIAGI_S
      REAL*8     TIM_REF, WEI_SCL, TIM(NP), VAL(NP), ERR(NP)
      REAL*8     VEL_SCL, EST_VEC(*), NOR_MAT(*)
      CHARACTER  STA(2)*(*)
      INTEGER*4  M_INT
      PARAMETER  ( M_INT = 64 )
      REAL*8,    ALLOCATABLE :: PLT_ARG(:), PLT_VAL(:), PLT_ERR(:), &
     &                          MOD_ARG(:), MOD_VAL(:), MOD_ERR(:)
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, IER
      CHARACTER  ZAG*128, UNIT*128
      INTEGER*4  J1
      REAL*8     TIM_BEG, TIM_SPAN, EQU_VEC(2)
!
      ALLOCATE ( PLT_ARG(NP) )
      ALLOCATE ( PLT_VAL(NP) )
      ALLOCATE ( PLT_ERR(NP) )
      ALLOCATE ( MOD_ARG(NP) )
      ALLOCATE ( MOD_VAL(NP) )
      ALLOCATE ( MOD_ERR(NP) )
!
      DO 410 J1=1,NP
         PLT_ARG(J1) = TIM(J1)/(86400.0D0*365.25D0) + 2000.0D0
         PLT_VAL(J1) = VAL(J1)*1.D3
         PLT_ERR(J1) = ERR(J1)*1.D3
!
         EQU_VEC(1)  = 1.0D0
         EQU_VEC(2)  = (TIM(J1)-TIM_REF)*VEL_SCL
         MOD_VAL(J1) = EST_VEC(1)*EQU_VEC(1) + EST_VEC(2)*EQU_VEC(2)
         MOD_ERR(J1) = DSQRT (       NOR_MAT(1)*EQU_VEC(1)*EQU_VEC(1) + &
     &                         2.0D0*NOR_MAT(2)*EQU_VEC(1)*EQU_VEC(2) + &
     &                               NOR_MAT(3)*EQU_VEC(2)*EQU_VEC(2)   )
!
         MOD_ARG(J1) = TIM(J1)/(86400.0D0*365.25D0) + 2000.0D0
         MOD_VAL(J1) = MOD_VAL(J1)*1.D3
         MOD_ERR(J1) = MOD_ERR(J1)*WEI_SCL*1.D3
         MOD_ERR(J1) = DSQRT (NOR_MAT(1) )*wei_scl*1.d3
 410  CONTINUE 
!
! --- Clear DIAGI_S object
!
      CALL NOUT ( SIZEOF(DIAGI_S), DIAGI_S )
!
! --- Setting defaults values of the plotting parameters
!
      CALL DIAGI_DEF  ( IBST, ILST, IOST, IPST, IWST, IDEV, ZAG, UNIT, &
     &                  ICL1, ICL2, ICL3, IER )
!
! --- Setting up the values of the DIAGI internal data structure for the further
! --- plotting
!
      DIAGI_S%IDEV      = IDEV
      DIAGI_S%NCLR      = 2
      DIAGI_S%NPOI(1)   = NP
      DIAGI_S%ADR_X8(1) = LOC(MOD_ARG)
      DIAGI_S%ADR_Y8(1) = LOC(MOD_VAL)
      DIAGI_S%ADR_E8(1) = LOC(MOD_ERR)
      DIAGI_S%LER(1)    = .TRUE.
      DIAGI_S%ICOL(1)   = ICL2
      DIAGI_S%IBST(1)   = 4
      DIAGI_S%ILST(1)   = 2
      DIAGI_S%IOST(1)   = IOST
      DIAGI_S%IPST(1)   = 1
      DIAGI_S%IWST(1)   = 2
!
      DIAGI_S%NPOI(2)   = NP
      DIAGI_S%ADR_X8(2) = LOC(PLT_ARG)
      DIAGI_S%ADR_Y8(2) = LOC(PLT_VAL)
      DIAGI_S%ADR_E8(2) = LOC(PLT_ERR)
      DIAGI_S%LER(2)    = .TRUE.
      DIAGI_S%ICOL(2)   = ICL1
      DIAGI_S%IBST(2)   = 2
      DIAGI_S%ILST(2)   = ILST
      DIAGI_S%IOST(2)   = IOST
      DIAGI_S%IPST(2)   = 2
      DIAGI_S%IWST(2)   = IWST
!
      DIAGI_S%ICLR      = 1
      DIAGI_S%XMIN      = 1.0
      DIAGI_S%XMAX      = 0.0
      DIAGI_S%YMIN      = 1.0
      DIAGI_S%YMAX      = 0.0
      DIAGI_S%ZAG       = 'Baseline '//STA(1)//'/'//STA(2)//' (mm)'
      DIAGI_S%NAME      = NAME__DEF
      DIAGI_S%ARG_UNITS = 'Time in years'
      DIAGI_S%ITRM      = 0
      DIAGI_S%IBATCH    = 0
      DIAGI_S%STATUS    = DIA__DEF
!
! --- Calling the main routine of DiaGI
!
      CALL DIAGI     ( DIAGI_S, IER )
!
      DEALLOCATE ( PLT_ARG )
      DEALLOCATE ( PLT_VAL )
      DEALLOCATE ( PLT_ERR )
      DEALLOCATE ( MOD_ARG )
      DEALLOCATE ( MOD_VAL )
      DEALLOCATE ( MOD_ERR )
!
      RETURN
      END  SUBROUTINE  BASLIN_PLO
