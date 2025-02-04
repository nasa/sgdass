      SUBROUTINE MUL_H1C ( MATL1, INP_MAT, &
     &                     MATR1, OUT_MAT )
! ************************************************************************
! *                                                                      *
! *   Routine  MUL_H1C
! *                                                                      *
! *  ### 20-NOV-2018     MUL_H5C   v1.0 (c)  L. Petrov  20-NOV-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      COMPLEX*8  MATL1(2,2), INP_MAT(2,2), &
     &           MATR1(2,2), OUT_MAT(2,2)
      COMPLEX*8  TRES(2,2), TOPR(2,2)
!
      CALL MUL_HC2 ( MATL1, INP_MAT, TRES    )
      CALL MUL_CC2 ( TRES,  MATR1,   OUT_MAT )
      RETURN
      END  SUBROUTINE  MUL_H1C  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_C1H ( MATL1, INP_MAT, &
     &                     MATR1, OUT_MAT )
! ************************************************************************
! *                                                                      *
! *   Routine  MUL_C1H
! *                                                                      *
! *  ### 20-NOV-2018     MUL_H5C   v1.0 (c)  L. Petrov  20-NOV-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      COMPLEX*8  MATL1(2,2), INP_MAT(2,2), &
     &           MATR1(2,2), OUT_MAT(2,2)
      COMPLEX*8  TRES(2,2), TOPR(2,2)
!
      CALL MUL_CC2 ( MATL1, INP_MAT, TRES    )
      CALL MUL_CH2 ( TRES,  MATR1,   OUT_MAT )
      RETURN
      END  SUBROUTINE  MUL_C1H  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_H2C ( MATL1, MATL2, INP_MAT, &
     &                     MATR1, MATR2, OUT_MAT )
! ************************************************************************
! *                                                                      *
! *   Routine  MUL_H2C
! *                                                                      *
! *  ### 20-NOV-2018     MUL_H2C   v1.0 (c)  L. Petrov  20-NOV-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      COMPLEX*8  MATL1(2,2), MATL2(2,2), INP_MAT(2,2), &
     &           MATR1(2,2), MATR2(2,2), OUT_MAT(2,2)
      COMPLEX*8  TRES(2,2), TOPR(2,2)
!
      CALL MUL_HC2 ( MATL2, INP_MAT, TRES ) ; TOPR = TRES
      CALL MUL_HC2 ( MATL1, TOPR,    TRES ) ; TOPR = TRES
      CALL MUL_CC2 ( TOPR,  MATR1,   TRES ) ; TOPR = TRES
      CALL MUL_CC2 ( TOPR,  MATR2,   OUT_MAT )
      RETURN
      END  SUBROUTINE  MUL_H2C  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_C2H ( MATL1, MATL2, INP_MAT, &
     &                     MATR1, MATR2, OUT_MAT )
! ************************************************************************
! *                                                                      *
! *   Routine  MUL_C2H
! *                                                                      *
! *  ### 20-NOV-2018     MUL_C2H   v1.0 (c)  L. Petrov  20-NOV-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      COMPLEX*8  MATL1(2,2), MATL2(2,2), INP_MAT(2,2), &
     &           MATR1(2,2), MATR2(2,2), OUT_MAT(2,2)
      COMPLEX*8  TRES(2,2), TOPR(2,2)
!
      CALL MUL_CC2 ( MATL2, INP_MAT, TRES ) ; TOPR = TRES
      CALL MUL_CC2 ( MATL1, TOPR,    TRES ) ; TOPR = TRES
      CALL MUL_CH2 ( TOPR,  MATR1,   TRES ) ; TOPR = TRES
      CALL MUL_CH2 ( TOPR,  MATR2,   OUT_MAT )
      RETURN
      END  SUBROUTINE  MUL_C2H  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_H3C ( MATL1, MATL2, MATL3, INP_MAT, &
     &                     MATR1, MATR2, MATR3, OUT_MAT )
! ************************************************************************
! *                                                                      *
! *   Routine  MUL_H3C
! *                                                                      *
! *  ### 20-NOV-2018     MUL_H3C   v1.0 (c)  L. Petrov  20-NOV-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      COMPLEX*8  MATL1(2,2), MATL2(2,2), MATL3(2,2), MATL5(2,2), INP_MAT(2,2), &
     &           MATR1(2,2), MATR2(2,2), MATR3(2,2), OUT_MAT(2,2)
      COMPLEX*8  TRES(2,2), TOPR(2,2)
!
      CALL MUL_CH2 ( MATL3, INP_MAT, TRES ) ; TOPR = TRES
      CALL MUL_CH2 ( MATL2, TOPR,    TRES ) ; TOPR = TRES
      CALL MUL_CH2 ( MATL1, TOPR,    TRES ) ; TOPR = TRES
      CALL MUL_CC2 ( TOPR,  MATR1,   TRES ) ; TOPR = TRES
      CALL MUL_CC2 ( TOPR,  MATR2,   TRES ) ; TOPR = TRES
      CALL MUL_CC2 ( TOPR,  MATR3,   OUT_MAT )
      RETURN
      END  SUBROUTINE  MUL_H3C  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_C3H ( MATL1, MATL2, MATL3, INP_MAT, &
     &                     MATR1, MATR2, MATR3, OUT_MAT )
! ************************************************************************
! *                                                                      *
! *   Routine  MUL_C3H
! *                                                                      *
! *  ### 20-NOV-2018               v1.0 (c)  L. Petrov  20-NOV-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      COMPLEX*8  MATL1(2,2), MATL2(2,2), MATL3(2,2), INP_MAT(2,2), &
     &           MATR1(2,2), MATR2(2,2), MATR3(2,2), OUT_MAT(2,2)
      COMPLEX*8  TRES(2,2), TOPR(2,2)
!
      CALL MUL_CC2 ( MATL3, INP_MAT,  TRES ) ; TOPR = TRES
      CALL MUL_CC2 ( MATL2, TOPR,     TRES ) ; TOPR = TRES
      CALL MUL_CC2 ( MATL1, TOPR,     TRES ) ; TOPR = TRES
      CALL MUL_CH2 ( TOPR,  MATR1,    TRES ) ; TOPR = TRES
      CALL MUL_CH2 ( TOPR,  MATR2,    TRES ) ; TOPR = TRES
      CALL MUL_CH2 ( TOPR,  MATR3, OUT_MAT )
      RETURN
      END  SUBROUTINE  MUL_C3H  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_H5C ( MATL1, MATL2, MATL3, MATL4, MATL5, INP_MAT, &
     &                     MATR1, MATR2, MATR3, MATR4, MATR5, OUT_MAT )
! ************************************************************************
! *                                                                      *
! *   Routine  MUL_H5C
! *                                                                      *
! *  ### 20-NOV-2018     MUL_H5C   v1.0 (c)  L. Petrov  20-NOV-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      COMPLEX*8  MATL1(2,2), MATL2(2,2), MATL3(2,2), MATL4(2,2), MATL5(2,2), INP_MAT(2,2), &
     &           MATR1(2,2), MATR2(2,2), MATR3(2,2), MATR4(2,2), MATR5(2,2), OUT_MAT(2,2)
      COMPLEX*8  TRES(2,2), TOPR(2,2)
!
      CALL MUL_CH2 ( MATL5, INP_MAT, TRES ) ; TOPR = TRES
      CALL MUL_CH2 ( MATL4, TOPR,    TRES ) ; TOPR = TRES
      CALL MUL_CH2 ( MATL3, TOPR,    TRES ) ; TOPR = TRES
      CALL MUL_CH2 ( MATL2, TOPR,    TRES ) ; TOPR = TRES
      CALL MUL_CH2 ( MATL1, TOPR,    TRES ) ; TOPR = TRES
      CALL MUL_CC2 ( TOPR,  MATR1,   TRES ) ; TOPR = TRES
      CALL MUL_CC2 ( TOPR,  MATR2,   TRES ) ; TOPR = TRES
      CALL MUL_CC2 ( TOPR,  MATR3,   TRES ) ; TOPR = TRES
      CALL MUL_CC2 ( TOPR,  MATR4,   TRES ) ; TOPR = TRES
      CALL MUL_CC2 ( TOPR,  MATR5,   OUT_MAT )
      RETURN
      END  SUBROUTINE  MUL_H5C  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_C5H ( MATL1, MATL2, MATL3, MATL4, MATL5, INP_MAT, &
     &                     MATR1, MATR2, MATR3, MATR4, MATR5, OUT_MAT )
! ************************************************************************
! *                                                                      *
! *   Routine  MUL_C5H
! *                                                                      *
! *  ### 20-NOV-2018               v1.0 (c)  L. Petrov  20-NOV-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      COMPLEX*8  MATL1(2,2), MATL2(2,2), MATL3(2,2), MATL4(2,2), MATL5(2,2), INP_MAT(2,2), &
     &           MATR1(2,2), MATR2(2,2), MATR3(2,2), MATR4(2,2), MATR5(2,2), OUT_MAT(2,2)
      COMPLEX*8  TRES(2,2), TOPR(2,2)
!
      CALL MUL_CC2 ( MATL5, INP_MAT, TRES ) ; TOPR = TRES
      CALL MUL_CC2 ( MATL4, TOPR,    TRES ) ; TOPR = TRES
      CALL MUL_CC2 ( MATL3, TOPR,    TRES ) ; TOPR = TRES
      CALL MUL_CC2 ( MATL2, TOPR,    TRES ) ; TOPR = TRES
      CALL MUL_CC2 ( MATL1, TOPR,    TRES ) ; TOPR = TRES
      CALL MUL_CH2 ( TOPR,  MATR1,   TRES ) ; TOPR = TRES
      CALL MUL_CH2 ( TOPR,  MATR2,   TRES ) ; TOPR = TRES
      CALL MUL_CH2 ( TOPR,  MATR3,   TRES ) ; TOPR = TRES
      CALL MUL_CH2 ( TOPR,  MATR4,   TRES ) ; TOPR = TRES
      CALL MUL_CH2 ( TOPR,  MATR5,   OUT_MAT )
      RETURN
      END  SUBROUTINE  MUL_C5H  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_CC2 ( MAT1, MAT2, MAT3 )
      IMPLICIT   NONE 
      COMPLEX*8  MAT1(2,2), MAT2(2,2), MAT3(2,2)
!
      MAT3(1,1) = MAT1(1,1)*MAT2(1,1) + MAT1(1,2)*MAT2(2,1)
      MAT3(2,1) = MAT1(2,1)*MAT2(1,1) + MAT1(2,2)*MAT2(2,1)
      MAT3(1,2) = MAT1(1,1)*MAT2(1,2) + MAT1(1,2)*MAT2(2,2)
      MAT3(2,2) = MAT1(2,1)*MAT2(1,2) + MAT1(2,2)*MAT2(2,2)
      RETURN
      END  SUBROUTINE  MUL_CC2  !#!# 
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_HC2 ( MAT1, MAT2, MAT3 )
      IMPLICIT   NONE 
      COMPLEX*8  MAT1(2,2), MAT2(2,2), MAT3(2,2)
!
      MAT3(1,1) = CONJG(MAT1(1,1))*MAT2(1,1) + CONJG(MAT1(2,1))*MAT2(2,1)
      MAT3(2,1) = CONJG(MAT1(1,2))*MAT2(1,1) + CONJG(MAT1(2,2))*MAT2(2,1)
      MAT3(1,2) = CONJG(MAT1(1,1))*MAT2(1,2) + CONJG(MAT1(2,1))*MAT2(2,2)
      MAT3(2,2) = CONJG(MAT1(1,2))*MAT2(1,2) + CONJG(MAT1(2,2))*MAT2(2,2)
!
      RETURN
      END  SUBROUTINE  MUL_HC2  !#!# 
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_CH2 ( MAT1, MAT2, MAT3 )
      IMPLICIT   NONE 
      COMPLEX*8  MAT1(2,2), MAT2(2,2), MAT3(2,2)
!
      MAT3(1,1) = MAT1(1,1)*CONJG(MAT2(1,1)) + MAT1(1,2)*CONJG(MAT2(1,2))
      MAT3(2,1) = MAT1(2,1)*CONJG(MAT2(1,1)) + MAT1(2,2)*CONJG(MAT2(1,2))
      MAT3(1,2) = MAT1(1,1)*CONJG(MAT2(2,1)) + MAT1(1,2)*CONJG(MAT2(2,2))
      MAT3(2,2) = MAT1(2,1)*CONJG(MAT2(2,1)) + MAT1(2,2)*CONJG(MAT2(2,2))
      RETURN
      END  SUBROUTINE  MUL_CH2  !#!#
