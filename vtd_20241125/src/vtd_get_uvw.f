      SUBROUTINE VTD_GET_UVW ( SOU_NAM, STA1_NAM, STA2_NAM, MJD, TAI, &
     &                         FREQ, VTD, UVW, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine VTD_GET_UVW computes UVW projection of the      *
! *   baseline vector to the source direction. The projections are       *
! *   expressed in wavelenghts.                                          *
! *                                                                      *
! *  ### 30-JAN-2006   VTD_GET_UVW  v1.1 (c)  L. Petrov  08-MAY-2006 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      CHARACTER  SOU_NAM*(*), STA1_NAM*(*), STA2_NAM*(*)
      INTEGER*4  MJD, IUER
      REAL*8     FREQ, TAI, UVW(3)
      REAL*8     STEP_UV, Z_TRS(3), Z_VEC(3), X_VEC(3), Y_VEC(3), &
     &           TMP_VEC(3), B_CRS(3), RD, BX, BY, BZ
      INTEGER*4  ISTA(2), ISOU, IER
      INTEGER*4, EXTERNAL :: VTD_STA_INDEX, VTD_SOU_INDEX 
      REAL*8,    EXTERNAL :: DP_VV_V 
!
      ISTA(1) = VTD_STA_INDEX ( VTD, STA1_NAM )
      ISTA(2) = VTD_STA_INDEX ( VTD, STA2_NAM )
      ISOU    = VTD_SOU_INDEX ( VTD, SOU_NAM  )
!
      Z_TRS(1) = 0.0D0
      Z_TRS(2) = 0.0D0
      Z_TRS(3) = 1.0D0
!
      IER = -1
      CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, 3, Z_TRS, 3, Z_VEC, IER )
      CALL VM83 ( Z_VEC, VTD%SOU(ISOU)%S_CRS, X_VEC ) 
      CALL NORM_VEC ( 3, X_VEC, RD ) 
!
      CALL VM83 ( VTD%SOU(ISOU)%S_CRS, X_VEC, Y_VEC ) 
      CALL NORM_VEC ( 3, Y_VEC, RD ) 
!
      CALL VM83 ( X_VEC, Y_VEC, Z_VEC ) 
      CALL NORM_VEC ( 3, Z_VEC, RD ) 
!
! --- Compute the baseline vector
!
      CALL SUB_VV_V ( 3, VTD%STA(ISTA(2))%COO_CRS, VTD%STA(ISTA(1))%COO_CRS, B_CRS )
!
! --- Compute projection of the baseline vector to the image plane
!
      BX = DP_VV_V ( 3, X_VEC, B_CRS )
      BY = DP_VV_V ( 3, Y_VEC, B_CRS )
      BZ = DP_VV_V ( 3, Z_VEC, B_CRS )
!
      UVW(1) = BX/VTD__C*FREQ
      UVW(2) = BY/VTD__C*FREQ
      UVW(3) = BZ/VTD__C*FREQ
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_GET_UVW  !#!  
