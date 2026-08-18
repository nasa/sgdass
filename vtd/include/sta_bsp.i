!
!  >>>> Include block sta_bsp
!  >>>> This block keeps definition of data structure for description
!  >>>> the file with site displacements modeled with Expansion with B-spline
!  >>>> basis
!  >>>>
!  >>>>
!  >>>> 2006.08.28  L. Petrov   2006.08.28_11:33:36
!
! --- Data structure for spline site position parameterization
!
      INTEGER*4  M__DEG_BSP, M__EPC_BSP
      PARAMETER  ( M__DEG_BSP =    3 ) ! max degree of spline for pos. estimation
      PARAMETER  ( M__EPC_BSP =  128 ) ! max number of epochs for spline pos estim.
      TYPE BSPPOS__TYPE
          SEQUENCE
          CHARACTER  STATION*8
	  CHARACTER  FILE_NAME*128
	  REAL*8     APR_COO(3)    ! Apriori site position
	  REAL*8     APR_VEL(3)    ! Apriori site velocity
	  REAL*8     TIM_COO       ! Refereence epoch of coordinats in sec
          INTEGER*4  DEGREE        ! Degree of the sppline
	  INTEGER*4  L_NOD         ! The number of nodes.
	  LOGICAL*4  EST_COO
	  LOGICAL*4  EST_VEL
          REAL*8     TIM(1-M__DEG_BSP:M__EPC_BSP)   ! Time argument in seconds after J2000
	  REAL*8     POS(3,1-M__DEG_BSP:M__EPC_BSP) ! Spline coefficients
	  REAL*8     ADJ_COO(3)    ! Adjustment of site position
	  REAL*8     ADJ_VEL(3)    ! Adjustment site velocity
	  REAL*8,    POINTER :: COV(:)
      END TYPE BSPPOS__TYPE
!
      CHARACTER  BSP__LABEL*36
      PARAMETER  ( BSP__LABEL = 'BSPPOS  Format version of 2005.03.14' )
!
!  >>>> end of include block sta_bsp
!
