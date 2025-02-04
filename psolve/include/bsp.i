!
!  >>>> Include block BSP
!  >>>> This block keeps definition of data structure for description
!  >>>> the file with site displacements modeled with Expansion with B-spline
!  >>>> basis
!  >>>>
!  >>>> Dependes on solve.i
!  >>>>
!  >>>> 2005.03.15 L. Petrov   15-MAR-2005 13:55:18
!
! --- Data structure for spline site position parameterization
!
      TYPE BSPSTA__TYPE
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
          REAL*8     TIM(1-M__SPD:M__SPN)   ! Time argument in seconds after J2000
	  REAL*8     POS(3,1-M__SPD:M__SPN) ! Spline coefficients
	  REAL*8     ADJ_COO(3)    ! Adjustment of site position
	  REAL*8     ADJ_VEL(3)    ! Adjustment site velocity
	  REAL*8,    POINTER :: COV(:)
      END TYPE BSPSTA__TYPE
!
      CHARACTER  BSP__LABEL*36
      PARAMETER  ( BSP__LABEL = 'BSPSTA  Format version of 2005.03.14' )
!
!  >>>> end of include block BSP
!
