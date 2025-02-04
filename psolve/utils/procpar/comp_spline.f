      SUBROUTINE COMP_SPLINE ( L_SPE, SPE, TIM_REF, WEI_SCL, NP, TIM, VAL, &
     &                         ERR, STA, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine COMP_SPLINE
! *                                                                      *
! *  ### 03-MAR-2005  COMP_SPLINE  v1.0 (c)  L. Petrov  03-MAR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INTEGER*4  L_SPE, NP, IUER
      TYPE       ( SPE__TYPE ) :: SPE(L_SPE)
      REAL*8     TIM_REF, WEI_SCL, TIM(NP), VAL(NP), ERR(NP)
      REAL*8,    ALLOCATABLE :: EQU_CNS(:), EQU_VEC(:), NOR_MAT(:), &
     &                          NOR_VEC(:), EST_VEC(:)
      CHARACTER  STA(2)*(*)
      REAL*8     NORM, RC, SIG, TIM_BEG, TIM_SPAN, WEI_SQ
      INTEGER*4  M_INT
      PARAMETER  ( M_INT = 64  )
      PARAMETER  ( SIG = 1.D-7 )
      INTEGER*4  L_EQU, LL_EQU, J1, J2, J3, J4, J5, J6, J7, J8, J9, &
     &           KP, INOD, IEQU, KNOT, MV, IER
      REAL*8,    EXTERNAL :: BSPL_VAL, BSPL_MOM1_FULL, BSPL_INT1_FULL, DP_VV_V
      INTEGER*4, EXTERNAL :: IXMN8
      INTEGER*4  LOCS, I, J
      LOCS(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
!
      MV = M_INT*SPE(1)%K_NOD
      L_EQU = SPE(1)%K_NOD + SPE(1)%DEGREE - 1 + 2
      LL_EQU = (L_EQU*(L_EQU+1))/2
      ALLOCATE ( EQU_VEC(L_EQU) )
      ALLOCATE ( EQU_CNS(L_EQU) )
      ALLOCATE ( NOR_VEC(L_EQU) )
      ALLOCATE ( EST_VEC(L_EQU) )
      ALLOCATE ( NOR_MAT(LL_EQU) )
!
      CALL NOUT_R8 (  L_EQU, NOR_VEC ) 
      CALL NOUT_R8 ( LL_EQU, NOR_MAT ) 
!
      write ( 6, * ) 'k-nod= ', spe(1)%k_nod
      write ( 6, * ) 'tim  = ', spe(1)%tim(1:spe(1)%k_nod)/(86400.0D0*365.2422)
!       call matview_1 ( spe(1)%k_nod, 1, spe(1)%tim(1) ) ! %%%
!       call diagi_1e ( np, tim, val, err, -3 ) ! %%%
      DO 430 J3=1,NP
         CALL NOUT_R8 ( L_EQU, EQU_VEC )
         INOD = IXMN8 ( SPE(1)%K_NOD, SPE(1)%TIM(1), TIM(J3) )
         IF ( INOD .GE. 1  .AND.  INOD .LE. SPE(1)%K_NOD ) THEN
              DO 440 J4=-SPE(1)%DEGREE,0
                 KNOT = INOD + J4
                 EQU_VEC(KNOT+SPE(1)%DEGREE) = &
     &                   BSPL_VAL ( SPE(1)%K_NOD, &
     &                              SPE(1)%TIM(1), &
     &                              SPE(1)%DEGREE, KNOT, TIM(J3) )
 440          CONTINUE 
         END IF
!
         IEQU = L_EQU - 1
         EQU_VEC(IEQU) = 1.0D0
!
         IEQU = L_EQU 
         EQU_VEC(IEQU) = (TIM(J3) - TIM_REF)/(TIM(NP) - TIM(1))
!
! ------ Update normal matrix
!
         WEI_SQ = 1.D0/(ERR(J3)/WEI_SCL)**2
         CALL DIAD_CVT_S ( WEI_SQ, L_EQU, EQU_VEC, EQU_VEC, NOR_MAT )
!
! ------ ... and normal vector
!
         DO 450 J5=1,L_EQU
            NOR_VEC(J5) = NOR_VEC(J5) + EQU_VEC(J5)*VAL(J3)*WEI_SQ
 450     CONTINUE
 430  CONTINUE 
!
! --- Constraint: zero mean
!
      CALL NOUT_R8 ( L_EQU, EQU_CNS )
      DO 460 J6=1,SPE(1)%K_NOD
         INOD = J6 - SPE(1)%DEGREE 
         EQU_CNS(J6) = BSPL_INT1_FULL ( SPE(1)%K_NOD, SPE(1)%TIM(1), &
     &                                  SPE(1)%DEGREE, INOD )
 460  CONTINUE 
      CALL NORM_VEC   ( L_EQU, EQU_CNS, NORM )
      CALL DIAD_CVT_S ( 1.D0/SIG, L_EQU, EQU_CNS, EQU_CNS, NOR_MAT )
!
! --- Constraint: zero drift
!
      CALL NOUT_R8 ( L_EQU, EQU_CNS )
      DO 470 J7=1,SPE(1)%K_NOD
         INOD = J7 - SPE(1)%DEGREE
         EQU_CNS(J7) = BSPL_MOM1_FULL ( SPE(1)%K_NOD, SPE(1)%TIM(1), &
     &                                  SPE(1)%DEGREE, INOD )
 470  CONTINUE 
      CALL NORM_VEC   ( L_EQU, EQU_CNS, NORM )
      CALL DIAD_CVT_S ( 1.D0/SIG, L_EQU, EQU_CNS, EQU_CNS, NOR_MAT )
!
! --- Invert the matrix
!
!@       call matview_1 ( k_nod, 1, spe(1)%tim ) ! %%%
!@       call matview_2 ( l_equ, nor_mat ) ! %%%
      CALL ERR_PASS ( IUER, IER )
      CALL INVS ( L_EQU, NOR_MAT, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1021, -1, 'COMP_SPLINE', 'Error in an attempt '// &
     &             'to invert normal matrix' )
           RETURN
      END IF
!
! --- Get solution
!
      CALL MUL_MV_SV_V ( L_EQU, NOR_MAT, L_EQU, NOR_VEC, L_EQU, EST_VEC, -3 )
!
!@       call matview_1 ( l_equ, 1, est_vec ) ! %%%%%%%%%
      WRITE ( 6, * ) ' RC = ', RC
      WRITE ( 6, * ) ' SH_ERR = ', DSQRT ( NOR_MAT(LOCS(L_EQU-1,L_EQU-1)) )
!
      CALL BASSPL_PLO ( L_SPE, SPE(1), NP, TIM, VAL, ERR, STA, TIM_REF, &
     &                  WEI_SCL, 1.D0/(TIM(NP) - TIM(1)), EST_VEC, NOR_MAT )
!
      DEALLOCATE ( EQU_VEC )
      DEALLOCATE ( EQU_CNS )
      DEALLOCATE ( NOR_VEC )
      DEALLOCATE ( EST_VEC )
      DEALLOCATE ( NOR_MAT )
!
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  COMP_SPLINE
!
! ------------------------------------------------------------------------
!
      SUBROUTINE COMP_LINE ( TIM_REF, WEI_SCL, NP, TIM, VAL, ERR, STA, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine COMP_LINE
! *                                                                      *
! *  ### 03-MAR-2005  COMP_LINE  v1.0 (c)  L. Petrov  03-MAR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INTEGER*4  NP, IUER
      REAL*8     TIM_REF, WEI_SCL, TIM(NP), VAL(NP), ERR(NP)
      REAL*8,    ALLOCATABLE :: EQU_CNS(:), EQU_VEC(:), NOR_MAT(:), &
     &                          NOR_VEC(:), EST_VEC(:)
      CHARACTER  STA(2)*(*)
      REAL*8     NORM, RC, WEI_SQ, TIM_BEG, TIM_SPAN
      INTEGER*4  M_INT
      PARAMETER  ( M_INT = 64  )
      INTEGER*4  L_EQU, LL_EQU, J1, J2, IER
      REAL*8,    EXTERNAL :: BSPL_VAL, BSPL_MOM1_FULL, BSPL_INT1_FULL, DP_VV_V
      INTEGER*4, EXTERNAL :: IXMN8
      INTEGER*4  LOCS, I, J
      LOCS(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
!
      L_EQU = 2
      LL_EQU = (L_EQU*(L_EQU+1))/2
      ALLOCATE ( EQU_VEC(L_EQU) )
      ALLOCATE ( EQU_CNS(L_EQU) )
      ALLOCATE ( NOR_VEC(L_EQU) )
      ALLOCATE ( EST_VEC(L_EQU) )
      ALLOCATE ( NOR_MAT(LL_EQU) )
!
      CALL NOUT_R8 (  L_EQU, NOR_VEC ) 
      CALL NOUT_R8 ( LL_EQU, NOR_MAT ) 
!
      DO 410 J1=1,NP
         EQU_VEC(1) = 1.0D0
         EQU_VEC(2) = (TIM(J1) - TIM_REF)/(TIM(NP) - TIM(1))
!
! ------ Update normal matrix
!
         WEI_SQ = 1.D0/(ERR(J1)/WEI_SCL)**2
         CALL DIAD_CVT_S ( WEI_SQ, L_EQU, EQU_VEC, EQU_VEC, NOR_MAT )
!
! ------ ... and normal vector
!
         DO 420 J2=1,L_EQU
            NOR_VEC(J2) = NOR_VEC(J2) + EQU_VEC(J2)*VAL(J1)*WEI_SQ
 420     CONTINUE
!@         write ( 6, * ) ' j1=',j1,' val=',val(j1), ' err=',err(j1) ! %%%%%%%%
 410  CONTINUE 
!
! --- Invert the matrix
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS ( L_EQU, NOR_MAT, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1021, -1, 'COMP_SPLINE', 'Error in an attempt '// &
     &             'to invert normal matrix' )
           RETURN
      END IF
!
! --- Get solution
!
      CALL MUL_MV_SV_V ( L_EQU, NOR_MAT, L_EQU, NOR_VEC, L_EQU, EST_VEC, -3 )
!
      WRITE ( 6, * ) ' RC = ', RC
      WRITE ( 6, * ) ' COM__LINE: SH_ERR = ', DSQRT ( NOR_MAT(LOCS(L_EQU-1,L_EQU-1)) )
!
      CALL BASLIN_PLO ( NP, TIM, VAL, ERR, STA, TIM_REF, WEI_SCL, &
     &                  1.D0/(TIM(NP) - TIM(1)), EST_VEC, NOR_MAT )
!
      DEALLOCATE ( EQU_VEC )
      DEALLOCATE ( EQU_CNS )
      DEALLOCATE ( NOR_VEC )
      DEALLOCATE ( EST_VEC )
      DEALLOCATE ( NOR_MAT )
!
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  COMP_LINE
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LINTREND_BSPL ( K_NOD, NOD_ARR, L_DEG, ARG_REF, VEL_SCL, &
     &                           EST_VEC, NOR_MAT, ARG, VAL, SIG )
! ************************************************************************
! *                                                                      *
! *   Routines LINTREND_BSPL 
! *                                                                      *
! *  ### 03-MAR-2005  LINTREND_BSPL  v1.0 (c) L. Petrov 03-MAR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  K_NOD, L_DEG
      REAL*8     NOD_ARR(K_NOD), EST_VEC(*), NOR_MAT(*), ARG_REF, VEL_SCL, &
     &           ARG, VAL, SIG
      REAL*8,    ALLOCATABLE :: EQU_VEC(:), TMP_VEC(:)
      INTEGER*4  INOD, KNOT, L_EQU, J1
      INTEGER*4, EXTERNAL :: IXMN8
      REAL*8,    EXTERNAL :: BSPL_VAL, DP_VV_V
      INTEGER*4  LOCS, I, J
      LOCS(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
!
      L_EQU = K_NOD + L_DEG - 1 + 2
      ALLOCATE ( EQU_VEC(L_EQU) )
      ALLOCATE ( TMP_VEC(L_EQU) )
! %%%%%%%%%%%
!@      write ( 6, * ) ' l_nod=',l_nod,' l_deg = ', l_deg ! %%%%%%%%
!@      call pause ( 'lintrend_bspl' ) ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%
!@        call matview_1 ( l_equ, 1, est_vec )  ! %%%%%%%%%%%%%%%%%%%%%%
!
      CALL NOUT_R8 ( L_EQU, EQU_VEC )
      INOD = IXMN8 ( K_NOD, NOD_ARR(1), ARG )
      IF ( INOD .GT. 0   .AND.  INOD .LE. K_NOD ) THEN
           DO 410 J1=-L_DEG,0
              KNOT = INOD + J1
              EQU_VEC(KNOT+L_DEG) = BSPL_VAL ( K_NOD, NOD_ARR, L_DEG, KNOT, ARG )
 410       CONTINUE 
      END IF
      EQU_VEC(L_EQU-1) = 1.0D0
      EQU_VEC(L_EQU)   = (ARG - ARG_REF)*VEL_SCL
!
      VAL = DP_VV_V    ( L_EQU, EQU_VEC, EST_VEC )
      CALL MUL_MV_SV_V ( L_EQU, NOR_MAT, L_EQU, EQU_VEC, L_EQU, TMP_VEC, -3 )
      SIG = DSQRT ( DP_VV_V ( L_EQU, EQU_VEC, TMP_VEC )  )
!
!@      SIG = DSQRT ( &
!@     &        NOR_MAT(LOCS(L_EQU-1,L_EQU-1))*EQU_VEC(L_EQU-1)**2 + &
!@     &        NOR_MAT(LOCS(L_EQU-1,L_EQU))*EQU_VEC(L_EQU-1)*EQU_VEC(L_EQU) + &
!@     &        NOR_MAT(LOCS(L_EQU,L_EQU))*EQU_VEC(L_EQU)**2 )
!
      DEALLOCATE ( EQU_VEC )
      DEALLOCATE ( TMP_VEC )
!
      RETURN
      END  SUBROUTINE  LINTREND_BSPL 
