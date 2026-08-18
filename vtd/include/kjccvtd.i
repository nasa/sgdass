! >>>>> Include block for kjccvtd 
! >>>>> 2021.04.07  (c)  L. Petrov  v 1.00  2025.06.29_17:05:00
!
      INTEGER*4    KFI__ARR_MAX
      PARAMETER  ( KFI__ARR_MAX = 8 )
!
      TYPE     KJCC_HEADER__TYPE
!!!!!!!!!
      END TYPE KJCC_HEADER__TYPE
!
      TYPE  KJCC_IM__TYPE
            CHARACTER  OBSCODE*32   ! detailed description of variable OBSCODE 
            INTEGER*4  NPOLY        ! ... the same
            INTEGER*4  REVISION
            INTEGER*4  NSS
            REAL*8     TMJD
            REAL*8     DELT
            CHARACTER  OBJNAM*32
            INTEGER*4  ANTNUM
            REAL*8     IFR
            REAL*8     PDELAY(KFI__ARR_MAX)
            REAL*8     GDELAY(KFI__ARR_MAX)
            REAL*8     PRATE(KFI__ARR_MAX)
            REAL*8     GRATE(KFI__ARR_MAX)
            REAL*8     DDELAY(KFI__ARR_MAX)
            REAL*8     DRATE(KFI__ARR_MAX)
            REAL*8     P2ND(KFI__ARR_MAX)
            REAL*8     G2ND(KFI__ARR_MAX)
            REAL*8     P3RD(KFI__ARR_MAX)
            REAL*8     G3RD(KFI__ARR_MAX)
            REAL*8     P4TH(KFI__ARR_MAX)
       END TYPE  KJCC_IM__TYPE
