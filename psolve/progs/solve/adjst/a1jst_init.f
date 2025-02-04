      SUBROUTINE A1JST_INIT ( MAX_ARC_STA, &
     &           ATMSTA, ATMRMS, ATMCNT, ATMCNTX, ATMTRA, &
     &           CLKSTA, CLKRMS, CLKCNT, CLKCNTX, CLKTRA, &
     &           GRDSTA, GRDRMS, GRDCNT, GRDCNTX, GRDTRA, &
     &           OVRSTA, OVRRMS, OVRCNT, OVRCNTX, &
     &           OVRTRA_ATM, OVRTRA_CLK, OVRTRA_GRD, &
     &           OVRSTA_CLK, OVRRMS_CLK, OVRCNT_CLK, OVRCNTX_CLK, &
     &           OVRSTA_GRD, OVRRMS_GRD, OVRCNT_GRD, OVRCNTX_GRD, &
     &           PRINT_CONT, JCLOCKX, NPREV, KFIRST )
! ************************************************************************
! *                                                                      *
! *   Auxillary routine  A1JST_INIT  initializes variables used in       *
! *   A1JST routine.                                                     *
! *                                                                      *
! *  ###  17-MAR-99    A1JST_INIT  v1.0  (c)  L. Petrov  17-MAR-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT  NONE
      INTEGER*2 MAX_ARC_STA, JCLOCKX, NPREV
      REAL*8    ATMSTA(MAX_ARC_STA), ATMRMS(MAX_ARC_STA), &
     &          ATMTRA(MAX_ARC_STA)
      INTEGER*2 ATMCNT(MAX_ARC_STA), ATMCNTX(MAX_ARC_STA)
      REAL*8    CLKSTA(MAX_ARC_STA), CLKRMS(MAX_ARC_STA), &
     &          CLKTRA(MAX_ARC_STA)
      INTEGER*2 CLKCNT(MAX_ARC_STA), CLKCNTX(MAX_ARC_STA)
      REAL*8    GRDSTA(MAX_ARC_STA), GRDRMS(MAX_ARC_STA), &
     &          GRDTRA(MAX_ARC_STA)
      INTEGER*2 GRDCNT(MAX_ARC_STA), GRDCNTX(MAX_ARC_STA)
      REAL*8    OVRTRA_CLK, OVRTRA_ATM, OVRTRA_GRD
      REAL*8    OVRSTA,     OVRRMS, &
     &          OVRSTA_CLK, OVRRMS_CLK, &
     &          OVRSTA_GRD, OVRRMS_GRD
      INTEGER*2 OVRCNT,     OVRCNTX, &
     &          OVRCNT_CLK, OVRCNTX_CLK, &
     &          OVRCNT_GRD, OVRCNTX_GRD
      LOGICAL*2 PRINT_CONT, KFIRST
!
      INTEGER*2 J1
!
      DO 410 J1=1,MAX_ARC_STA
         ATMSTA (J1) = 0.0D0
         ATMRMS (J1) = 0.0D0
         ATMCNT (J1) = 0
         ATMCNTX(J1) = 0
         ATMTRA (J1) = 0.0D0
!
         CLKSTA (J1) = 0.0D0
         CLKRMS (J1) = 0.0D0
         CLKCNT (J1) = 0
         CLKCNTX(J1) = 0
         CLKTRA (J1) = 0.0D0
!
         GRDSTA (J1) = 0.0D0
         GRDRMS (J1) = 0.0D0
         GRDCNT (J1) = 0
         GRDCNTX(J1) = 0
         GRDTRA (J1) = 0.0D0
 410  CONTINUE
!
      OVRRMS      = 0.0D0
      OVRSTA      = 0.0D0
      OVRCNT      = 0
      OVRCNTX     = 0
!
      OVRTRA_ATM  = 0.0D0
      OVRTRA_CLK  = 0.0D0
      OVRTRA_GRD  = 0.0D0
!
      OVRRMS_CLK  = 0.0D0
      OVRSTA_CLK  = 0.0D0
      OVRCNT_CLK  = 0
      OVRCNTX_CLK = 0
!
      OVRRMS_GRD  = 0.0D0
      OVRSTA_GRD  = 0.0D0
      OVRCNT_GRD  = 0
      OVRCNTX_GRD = 0
!
      PRINT_CONT  = .FALSE.
      JCLOCKX     = 1
      NPREV       = 1
      KFIRST      = .TRUE.
!
      RETURN
      END  !#!  A1JST_INIT  #!#
