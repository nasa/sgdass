!
! >>>>> INCLUDE-BLOCK with descriptions data structures used for calculation
!       of coefficients of linear spline
! 
!       lspline.i  29-OCT-1998  v1.1  (c)  L. Petrov  --  2017.12.08_20:48:45
!
      INTEGER*4  M_LSP, MPAR_LSP, MELM_LSP
      PARAMETER  ( M_LSP = 256          ) ! maximal number of segments
      PARAMETER  ( MPAR_LSP = M_LSP + 3 ) ! maximal number of parameters
      PARAMETER  ( MELM_LSP = (MPAR_LSP*(MPAR_LSP+1))/2 )
      INTEGER*4  LSP__UND, LSP__INI, LSP__CMP 
      PARAMETER  ( LSP__UND =   -1 )      ! Code: undefined
      PARAMETER  ( LSP__INI = 1701 )      ! Code: initalized
      PARAMETER  ( LSP__CMP = 1702 )      ! Code: computed
      REAL*8     LSP__OVD, SHARE__OVD, CNST__MIN
      PARAMETER  ( LSP__OVD = 0.2 )       ! With of overdraft region (relative)
      PARAMETER  ( SHARE__OVD = 0.01D0 )  ! Share of the overdrafted segment
!                                         ! which forces LSPLINE_GETSEG to 
!                                         ! add additional boundary
      PARAMETER  ( CNST__MIN = 1.D-15 )   ! Mininal accptable sigma of
!                                         ! the constraint on rate of changes. 
!
      TYPE  LSPLINE__STRU
          INTEGER*4     L_LSP          ! Number of segments
          REAL*8        EPOCH(M_LSP)   ! Arrays of segment boundary
	  REAL*8        SPAN           ! length of argument span for 1 segment
	  REAL*8        DURA           ! Duration od enitre data set
	  REAL*8        TIM_FIRST      ! First argument
	  REAL*8        TIM_MIDDLE     ! Argument at the middle of the set
	  REAL*8        TIM_LAST       ! last argument
	  REAL*8        GLO_RATE_CNS   ! Value of the global rate constraint 
	  REAL*8        SEG_RATE_CNS   ! Value of the segmented rate constraint 
!
          REAL*8        COV(MELM_LSP)  ! Covariancs matrix
          REAL*8        EST(MPAR_LSP)  ! Vector of the estiamtes
          REAL*8        DSP(MPAR_LSP)  ! Vector of formal uncertainties of the
!                                      !     estiamtes
          INTEGER*4     STATUS         ! Status
      END TYPE LSPLINE__STRU  !
!
! <<<<< end of INCLUDE-BLOCK  lspline.i
!
