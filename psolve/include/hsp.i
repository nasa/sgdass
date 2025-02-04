!
!  >>>> Include block HSP
!  >>>> This block keeps definition of data structure for description
!  >>>> the file with site displacements modeled with Expansion with B-spline
!  >>>> basis
!  >>>> 
!  >>>> Dependes on solve.i
!  >>>> 
!  >>>> 2005.03.23 L. Petrov   29-MAR-2005 09:23:48
!
! --- Data structure for spline site position parameterization
!
      TYPE      HSP__TYPE
	  CHARACTER  HAR_NAME*8
	  INTEGER*4  L_STA
	  INTEGER*4  L_COV
          TYPE       ( HPESOL__TYPE ), POINTER :: STA(:)
	  CHARACTER, POINTER :: C_STA(:)*8
          REAL*8,    POINTER :: COO(:,:)
          REAL*8,    POINTER :: EST(:,:,:)
          REAL*8,    POINTER :: SIG(:,:,:)
          REAL*8,    POINTER :: COV(:)
      END  TYPE HSP__TYPE
