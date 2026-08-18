!
!  >>>> Include block BSP
!  >>>> This block keeps definition of data structure for description
!  >>>> the file with site displacements modeled with Expansion with B-spline
!  >>>> basis
!  >>>>
!  >>>> Dependes on vtd.i or solve.i
!  >>>>
!  >>>> 2005.03.15 L. Petrov   2007.10.31_22:46:49
!
! --- Data structure for spline site position parameterization
!
      TYPE BSPPOS__TYPE
          SEQUENCE
          CHARACTER  STATION*8
	  CHARACTER  SOLUTION_ID*64
	  CHARACTER  SOLUTION_DATE*32
	  CHARACTER  FILE_NAME*128
          INTEGER*4  L_DEG    ! Degree of the spline
	  INTEGER*4  L_NOD    ! The number of nodes.
	  LOGICAL*4  FL_EST_COO
	  LOGICAL*4  FL_EST_VEL
	  LOGICAL*4  FL_EST_SPL
	  LOGICAL*4  FL_COV_AVL
	  REAL*8     TIM_REF
          REAL*8     TIM(1-VTD__M_SPD:VTD__M_NOD)   ! Time argument in seconds after J2000
          REAL*8     SPL(1-VTD__M_SPD:VTD__M_NOD,3) ! Spline coefficients
	  REAL*8     COO(3)        ! Total site position vector
	  REAL*8     VEL(3)        ! Total site velocity vector
	  REAL*8,    POINTER :: COV(:,:,:,:)
      END TYPE BSPPOS__TYPE
!
      REAL*8       BSPPOS__RD_AREA
      PARAMETER  ( BSPPOS__RD_AREA = 3.0 ) ! Radius of the area around the 
!                                          ! station for which 
!                                          ! the displacemenet are valid
!
      CHARACTER  BSPPOS__LABEL*36
      PARAMETER  ( BSPPOS__LABEL = 'BSPPOS  Format version of 2007.10.30' )
!
      CHARACTER    BSPPOS_FORMAT_HELP*17
      PARAMETER  ( BSPPOS_FORMAT_HELP = 'bsppos_format.txt' )
!
!  >>>> end of include block BSPPOS
!
