      SUBROUTINE B1D_COV ( G, L, A_GG, A_LG, A_LL, B_G, B_L, &
     &                           W_LG, W_LL, W_L )
! ************************************************************************
! *                                                                      *
! *   Routine  B1D_COV  calculates adjustments of the local parameters,  *
! *   local-global and local-local covariance matrices on the back run   *
! *   of the B1D algorithm.                                              *
! *                                                                      *
! *  ###  12-FEB-1997  B1D_COV     v1.0  (c) L. Petrov 12-FEB-1997  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  G, L, A_GG(*), A_LG(L,G), A_LL(*), B_G(G), B_L(L), &
     &           W_LG(L,G), W_LL(*), W(L), W_L(L), IER
!
! --- W_LG = A_LG * A_GG (local-global covariance matrix)
!
      IER = -1
      CALL MUL_MM_IS_I ( L, G, A_LG, G, A_GG, L, G, W_LG, IER )
!!      CALL MUL_MM_ST_I ( G, A_GG, L, G, A_LG, G, L, W_LG, IER ) ! faster!
!
! --- W_ll = A_LG * W_LG(T)  (correction to local-local covariance matrix)
!
      IER = -1
      CALL MUL_MM_IT_S ( L, G, A_LG, L, G, W_LG, L, W_LL, IER )
!!      CALL MUL_MM_II_S ( L, G, A_LG, G, L, W_LG, L, W_LL, IER ) ! faster!
!
! --- A_LL := A_LL + W_LL (local-loacl covariance matrix)
!
      IER = -1
      CALL ADD_VV      ( (L*(L+1))/2, A_LL, W_LL, IER )
!
! --- W_L = A_LG * B_G (correction to local adjustments)
!
      IER = -1
      CALL MUL_MV_IV_V ( L, G, A_LG, G, B_G, L, W_L, IER )
!
! --- B_L := B_L + W_L (adjustments of local parameters)
!
      CALL ADD_VV      ( L, B_L, W_L )
!
      RETURN
      END  !#!  B1D_COV  #!#
