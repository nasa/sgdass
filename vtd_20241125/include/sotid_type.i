!
! >> include-block for estimation of the displacements due to
! >> the Earth's solid tides.
! >>
! >> L. Petrov  22-JAN-2001        21-JUN-2004 11:11:54
!
!
! --- Constants used by SOTID
!
      INTEGER*4  SOTID__MAX_STA, SOTID__NAM_LEN, SOTID__NLOVE
      PARAMETER  ( SOTID__MAX_STA = 16*1024 )
      PARAMETER  ( SOTID__NAM_LEN = 8 )
      PARAMETER  ( SOTID__NLOVE   = 9 )
!
      TYPE      STATID__STRU
         REAL*8     PHI_GCN
         REAL*8     LONGITUDE
         REAL*8     REN__CFS(3,3)
         REAL*8     XRC2(5,3,0:2)
         REAL*8     XRS2(5,3,0:2)
         REAL*8     XIC2(2,3,0:2)
         REAL*8     XIS2(2,3,0:2)
         REAL*8     XRC3(3,0:3)
         REAL*8     XRS3(3,0:3)
         CHARACTER  NAME*(SOTID__NAM_LEN)
      END TYPE  STATID__STRU !  STATID__STRU  !
!
      TYPE      TIMTID__STRU
         REAL*8     LARC2(5,3,0:2,0:1)
         REAL*8     LARS2(5,3,0:2,0:1)
         REAL*8     LAIC2(2,3,0:2,0:1)
         REAL*8     LAIS2(2,3,0:2,0:1)
         REAL*8     LARC3(3,0:3,0:1)
         REAL*8     LARS3(3,0:3,0:1)
      END TYPE  TIMTID__STRU !  TIMTID__STRU  !
!
      TYPE      TIDCNF__STRU
         INTEGER*4  MODEL_2D
         INTEGER*4  MODEL_3D
         INTEGER*4  GEN_LOVE
         INTEGER*4  ORDER_2D
         INTEGER*4  ZF_LOVE
         INTEGER*4  N_STA
      END TYPE  TIDCNF__STRU !  TIDCNF__STRU /
!
!
! << end of include block sotid_type.i
!
