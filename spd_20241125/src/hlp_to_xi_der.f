      SUBROUTINE HLP_TO_XI_DER ( DER_HLP, VEC_HLP, AZ, EL, DER_XI )
! ************************************************************************
! *                                                                      *
! *   Routine  HLP_TO_XI_DER 
! *                                                                      *
! * ### 06-MAR-2008   HLP_TO_XI_DER  v2.0 (c) L. Petrov  14-FEB-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
!
      REAL*8     VEC_HLP(3), DER_HLP(3), AZ, EL, DER_XI(3)
      REAL*8     MAT_XI_TO_X3(3,3), REA, FE, ECC_SQ
      PARAMETER  ( REA     = SPD__REA_WGS84  ) ! Earth's equatorial radius
      PARAMETER  ( FE      = SPD__FLAT_WGS84 ) ! Earth's flattening
      PARAMETER  ( ECC_SQ  = 2.D0*FE - FE**2 ) ! Earth's eccentricity
      REAL*8     DX_DH_MAT(3,3), DH_DX_MAT(3,3), DXI_DH_MAT(3,3)
      REAL*8     RC, MAT(3,3), EPS
      INTEGER*4  IER
!%%   write ( 6, * ) 'GGGG 21' ! %%%
!
! --- Compute elements of the derivatives of XYZ vector on HLP variables
!
      DX_DH_MAT(1,1) = DCOS(VEC_HLP(3))*DCOS(VEC_HLP(2))
      DX_DH_MAT(2,1) = DCOS(VEC_HLP(3))*DSIN(VEC_HLP(2))
      DX_DH_MAT(3,1) = DSIN(VEC_HLP(3))
!
      DX_DH_MAT(1,2) = -( (1.0D0/DSQRT( 1.0D0 - ECC_SQ*DSIN(VEC_HLP(3))**2) + VEC_HLP(1)/REA) &
     &                  )* DCOS(VEC_HLP(3))*DSIN(VEC_HLP(2))
      DX_DH_MAT(2,2) =  ( (1.0D0/DSQRT( 1.0D0 - ECC_SQ*DSIN(VEC_HLP(3))**2) + VEC_HLP(1)/REA) &
     &                  )* DCOS(VEC_HLP(3))*DCOS(VEC_HLP(2))
      DX_DH_MAT(3,2) = 0.0D0
!
      DX_DH_MAT(1,3) =     ECC_SQ*DSIN(VEC_HLP(3))*DCOS(VEC_HLP(3))/ &
     &                     DSQRT( 1.0D0 - ECC_SQ*DSIN(VEC_HLP(3))**2 )**3 * &
     &                     DCOS(VEC_HLP(3))*DCOS(VEC_HLP(2)) &
     &                 - ( 1.0D0/DSQRT( 1.0D0 - ECC_SQ*DSIN(VEC_HLP(3))**2 ) + VEC_HLP(1)/REA &
     &                   )* DSIN(VEC_HLP(3))*DCOS(VEC_HLP(2))
!
      DX_DH_MAT(2,3) =     ECC_SQ*DSIN(VEC_HLP(3))*DCOS(VEC_HLP(3))/ &
     &                     DSQRT( 1.0D0 - ECC_SQ*DSIN(VEC_HLP(3))**2 )**3 * &
     &                     DCOS(VEC_HLP(3))*DSIN(VEC_HLP(2)) &
     &                 - ( 1.0D0/DSQRT( 1.0D0 - ECC_SQ*DSIN(VEC_HLP(3))**2 ) + VEC_HLP(1)/REA &
     &                   )* DSIN(VEC_HLP(3))*DSIN(VEC_HLP(2))
!
      DX_DH_MAT(3,3) =     ECC_SQ*(1.D0 - ECC_SQ)*DSIN(VEC_HLP(3))*DCOS(VEC_HLP(3))/ &
     &                     DSQRT( 1.0D0 - ECC_SQ*DSIN(VEC_HLP(3))**2 )**3* &
     &                     DSIN(VEC_HLP(3)) &
     &                 +   &
     &                   ( (1.D0 - ECC_SQ)/DSQRT(1.0D0 - ECC_SQ*DSIN(VEC_HLP(3))**2 ) + VEC_HLP(1)/REA &
     &                   )* DCOS(VEC_HLP(3))
      DH_DX_MAT = DX_DH_MAT
      IER = -1
      EPS = 1.D-10
      CALL INVA ( 3, DH_DX_MAT, EPS, IER )
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) ' DX_DH_MAT= ', DX_DH_MAT
      END IF
      DH_DX_MAT(1:3,2:3) = DH_DX_MAT(1:3,2:3)/REA 
!     
! --- Compute MAT_XI_TO_X3 -- 3x3 matrix of transformation from
! --- xi-coordinates to XYZ:
! --- Rz(pi - az) * Rx(pi/2) * R2(pi/2 - lat_gdt -el) * R3(-lon)
!
      CALL GET_MAT_XI_TO_X3 ( VEC_HLP(3), VEC_HLP(2), AZ, EL, MAT_XI_TO_X3 )
      CALL MUL_MM_TI_I ( 3, 3, MAT_XI_TO_X3, 3, 3, DH_DX_MAT, &
     &                   3, 3, DXI_DH_MAT, -2 )
      CALL MUL_MV_IV_V ( 3, 3, DXI_DH_MAT, 3, DER_HLP, 3, DER_XI, -2 )
!
      RETURN
      END  SUBROUTINE  HLP_TO_XI_DER  !#!
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_MAT_XI_TO_X3 ( PHI_GDT, LON, AZ, EL, MAT_XI_TO_X3 )
      IMPLICIT   NONE 
!
! --- Rz(pi - az) * Rx(pi/2) * R2(pi/2 - lat_gdt -el) * R3(-lon)
!
      INCLUDE   'astro_constants.i'
      REAL*8     PHI_GDT, LON, AZ, EL, MAT_XI_TO_X3(3,3)
      REAL*8     MAT_R1(3,3), MAT_R2(3,3), MAT_R3(3,3), MAT_R4(3,3), MAT_R5(3,3), MAT_TMP(3,3)
!
      CALL VTD_ROTMAT ( 3, -LON,           MAT_R1 )
      CALL VTD_ROTMAT ( 2, -P2I + PHI_GDT, MAT_R2 )
      CALL VTD_ROTMAT ( 1, -P2I,           MAT_R3 )
      CALL VTD_ROTMAT ( 2,  AZ - PI__NUM,  MAT_R4 )
      CALL VTD_ROTMAT ( 3, -EL,            MAT_R5 )
!
      MAT_TMP = MAT_R5
      CALL MUL_MM_II_I ( 3, 3, MAT_R4, 3, 3, MAT_TMP, &
     &                   3, 3, MAT_XI_TO_X3,  -2 )
      MAT_TMP = MAT_XI_TO_X3
      CALL MUL_MM_II_I ( 3, 3, MAT_R3, 3, 3, MAT_TMP, &
     &                   3, 3, MAT_XI_TO_X3,  -2 )
      MAT_TMP = MAT_XI_TO_X3
      CALL MUL_MM_II_I ( 3, 3, MAT_R2, 3, 3, MAT_TMP, &
     &                   3, 3, MAT_XI_TO_X3,  -2 )
      MAT_TMP = MAT_XI_TO_X3
      CALL MUL_MM_II_I ( 3, 3, MAT_R1, 3, 3, MAT_TMP, &
     &                   3, 3, MAT_XI_TO_X3,  -2 )
!
      RETURN
      END  SUBROUTINE  GET_MAT_XI_TO_X3  !#!#
