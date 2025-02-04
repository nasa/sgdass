      FUNCTION SPD_GET_ATT ( SPD, I_FRQ, MAT_XI_TO_X3, VEC_GROUND_XYZ, &
     &                       VEC_XI, IUER )
! ************************************************************************
! *                                                                      *
! *   FUNCTION SPD_GET_ATT
! *                                                                      *
! * ### 11-SEP-2014    SPD_GET_ATT   v3.0 (c) L. Petrov  11-SEP-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      REAL*8     SPD_GET_ATT
      REAL*8     MAT_XI_TO_X3(3,3), VEC_GROUND_XYZ(3), VEC_XI(3)
      INTEGER*4  I_FRQ, IUER
      REAL*8     VEC_XYZ(3), VEC_HLP(3), ARGS(3)
      INTEGER*4  INDS(3), DIMS(3), IER
      REAL*8,    EXTERNAL :: VAL_3D_BSPLE3
!
! --- Transform vector coordinate from XI,Y,Z to crust-fixed 
! --- Cartesian vector X,Y,Z ( VEC_XYZ ) with respect to 
! --- the receiver
!
      CALL MUL_MV_IV_V ( 3, 3, MAT_XI_TO_X3, 3, VEC_XI, 3, VEC_XYZ, -2 )
!
! --- Transform XYZ to HLP coordinates that are used for
! --- interpolating the global refractivity field
!
      VEC_XYZ = VEC_GROUND_XYZ + VEC_XYZ 
!
      CALL XYZ_TO_HLP ( SPD, VEC_XYZ, VEC_HLP )
!
! --- Set pivotal indices
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPD_GET_INDS ( SPD, VEC_HLP, ARGS, INDS, DIMS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6191, IUER, 'GET_REFR_HLP', 'Trap of '// &
     &         'internal control: error in computing pivotal index' )
           RETURN 
      END IF
!
      SPD_GET_ATT = DEXP ( VAL_3D_BSPLE3 ( ARGS, DIMS, INDS, &
     &                                     SPD%LEV,  &
     &                                     SPD%LON,  &
     &                                     SPD%LAT,  &
     &               SPD%REF_3D(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,SPD__MTYP+I_FRQ) ) )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION  SPD_GET_ATT  !#!#
