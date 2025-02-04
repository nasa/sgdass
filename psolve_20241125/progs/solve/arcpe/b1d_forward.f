      SUBROUTINE B1D_FORWARD ( G, L, A_GG, A_LG, A_LL, B_G, B_L, &
     &           W_GG, W_LG, W_LL, W_G, W_L, SCALE_L, RCOND, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  B1D_FORWARD eliminates influence of the local parameters  *
! *   from the global parameters.                                        *
! *                                                                      *
! *  ###  11-FEB-97   B1D_FORWARD  v1.0  (c)  L. Petrov  12-FEB-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*4  G, L, IUER
      REAL*8     A_GG(*), A_LG(L,G), A_LL(*), B_G(G), B_L(L), &
     &           W_GG(*), W_LG(L,G), W_LL(*), W_G(G), W_L(L), &
     &           SCALE_L(L)
      REAL*8     RCOND
!
! --- I. Inversion A{ll}
!     ~~~~~~~~~~~~~~~~~~
!
! --- Scaling local block. Scaling vectors will be calculated and put
! --- in SCALE_L. Normal system will be modified so that main diagonal will
! --- contain only 1
!
      CALL SCALER ( A_LL, B_L, SCALE_L, INT2(L) )
!
! --- Decomposition with calculation condition number
!
!!!!!         call tim_init ( )
!!      CALL INVMAT1_S   ( L, A_LL, RCOND, -1          )
!!      CALL MUL_MV_SV_V ( L, A_LL, L, B_L, L, W_L, -3 )
!!      CALL COPY_V      ( L, W_L,     B_L             )
      CALL DPPCO ( A_LL, INT2(L), RCOND, W_L )
!
! --- Solving the local system
!
      CALL DPPSL ( A_LL, B_L, INT2(L) )
!
! --- Calculation inverse matrix
!
      CALL DPPIN ( A_LL, INT2(L) )
!!!!!!         call tim_tp ( ,,, )
!!!!!!         type *,' B1D_FORWARD: It was matrix inversion: l=',l
!
! --- Unscaling covariance matrix and vector estimates
!
      CALL UNSCALER ( A_LL, B_L, SCALE_L, INT2(L) )
!
! --- II. Elimination local parameters from global block
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! --- WLG = A(-1){ll} * A{lg}
!
      CALL MUL_MM_SI_I ( L, A_LL, L, G, A_LG, L, G, W_LG, -3 )
!
! --- WGG = A(T){lg} * A(-1){ll} * A{lg}
!
      CALL MUL_MM_TI_S ( L, G, A_LG, L, G, W_LG, G, W_GG, -3 )
!
! --- A{gg} := A{gg} - A(T){lg} * A(-1){ll} * A{lg}
!
      CALL SUB_VV      ( (G*(G+1))/2, A_GG, W_GG )
!
! --- W_G = A{lg} * B{l}
!
      CALL MUL_MV_TV_V ( L, G, A_LG, L, B_L, G, W_G, -3 )
!
! --- B_G := B{g} - A{lg} * B{l}
!
      CALL SUB_VV      ( G, B_G, W_G )
!
      RETURN
      END  !#!  B1D_FORWARD  #!#
