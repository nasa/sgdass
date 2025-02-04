      SUBROUTINE GET_REFR_DER_XI ( SPD, TYP, MAT_XI_TO_X3, VEC_GROUND_XYZ, &
     &                             VEC_XI, AZ, EL, REFR_VAL, REFR_DER_XI, &
     &                             IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_REFR_DER_XI
! *                                                                      *
! * ### 25-FEB-2008  GET_REFR_DER_XI v3.0 (c) L. Petrov 18-FEB-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      REAL*8     MAT_XI_TO_X3(3,3), VEC_GROUND_XYZ(3), VEC_XI(3), REFR_VAL, &
     &           REFR_DER_XI(3), AZ, EL
      INTEGER*4  TYP, IUER
      REAL*8     VEC_XYZ(3), VEC_HLP(3)
      REAL*8     DELTA_ARG, DELTA_R, DELTA_VEC_X3(3), DELTA_VEC_HLP(3), &
     &           RDER_HLP(3)
      INTEGER*4  IER
      PARAMETER  ( DELTA_ARG = 1.0D0 ) 
      LOGICAL*1  FL_DER_ANALYTIC 
      PARAMETER  ( FL_DER_ANALYTIC = .FALSE. )
!!      parameter  ( fl_der_analytic = .true. )
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
      IF ( FL_DER_ANALYTIC ) THEN
!
! -------- Get the total refractivity and its partial derivatives 
! -------- in HLP coordinate system
!
           CALL GET_REFR_DER_HLP ( SPD, TYP, VEC_HLP, REFR_VAL, RDER_HLP )
!
! -------- ... and convert the partial derivatives from the HLP 
! -------- system to the XI-system
! 
           CALL HLP_TO_XI_DER  ( RDER_HLP, VEC_HLP, AZ, EL, REFR_DER_XI )
        ELSE
          CALL ERR_PASS ( IUER, IER )
          CALL GET_REFR_HLP ( SPD, TYP, VEC_HLP, REFR_VAL, IER )
          IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5561, IUER, 'GET_REFR_DER_XI', 'Error in '// &
     &             'computing refractivity.' )
               RETURN 
          END IF
!
          DELTA_VEC_X3(1) = VEC_XI(1) + DELTA_ARG
          DELTA_VEC_X3(2) = VEC_XI(2) 
          DELTA_VEC_X3(3) = VEC_XI(3)
          CALL MUL_MV_IV_V ( 3, 3, MAT_XI_TO_X3, 3, DELTA_VEC_X3, 3, VEC_XYZ, -2 )
          VEC_XYZ = VEC_GROUND_XYZ + VEC_XYZ 
          CALL XYZ_TO_HLP ( SPD, VEC_XYZ, VEC_HLP )
          CALL ERR_PASS ( IUER, IER )
          CALL GET_REFR_HLP ( SPD, TYP, VEC_HLP, DELTA_R, IER )
          IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5562, IUER, 'GET_REFR_DER_XI', 'Error in '// &
     &             'computing refractivity' )
               RETURN 
          END IF
          REFR_DER_XI(1) = (DELTA_R - REFR_VAL)/DELTA_ARG
!
          DELTA_VEC_X3(1) = VEC_XI(1) 
          DELTA_VEC_X3(2) = VEC_XI(2) + DELTA_ARG
          DELTA_VEC_X3(3) = VEC_XI(3)
          CALL MUL_MV_IV_V ( 3, 3, MAT_XI_TO_X3, 3, DELTA_VEC_X3, 3, VEC_XYZ, -2 )
          VEC_XYZ = VEC_GROUND_XYZ + VEC_XYZ 
          CALL XYZ_TO_HLP ( SPD, VEC_XYZ, VEC_HLP )
          CALL ERR_PASS ( IUER, IER )
          CALL GET_REFR_HLP ( SPD, TYP, VEC_HLP, DELTA_R, IER )
          IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5563, IUER, 'GET_REFR_DER_XI', 'Error in '// &
     &             'computing refractivity' )
               RETURN 
          END IF
          REFR_DER_XI(2) = (DELTA_R - REFR_VAL)/DELTA_ARG
!
          IF ( SPD%CONF%SPD_ALG .EQ. SPD__ALG_Y_NONLOC ) THEN
               REFR_DER_XI(3) = 0.0D0
             ELSE 
               DELTA_VEC_X3(1) = VEC_XI(1)
               DELTA_VEC_X3(2) = VEC_XI(2)
               DELTA_VEC_X3(3) = VEC_XI(3) + DELTA_ARG
               CALL MUL_MV_IV_V ( 3, 3, MAT_XI_TO_X3, 3, DELTA_VEC_X3, 3, VEC_XYZ, -2 )
               VEC_XYZ = VEC_GROUND_XYZ + VEC_XYZ 
               CALL XYZ_TO_HLP ( SPD, VEC_XYZ, VEC_HLP )
               CALL ERR_PASS ( IUER, IER )
               CALL GET_REFR_HLP ( SPD, TYP, VEC_HLP, DELTA_R, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 5564, IUER, 'GET_REFR_DER_XI', 'Error in '// &
     &                  'computing refractivity' ) 
                    RETURN 
               END IF
               REFR_DER_XI(3) = (DELTA_R - REFR_VAL)/DELTA_ARG
          END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_REFR_DER_XI  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_REFR_XI ( SPD, TYP, MAT_XI_TO_X3, VEC_GROUND_XYZ, &
     &                         VEC_XI, REFR_VAL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_REFR_DER_XI
! *                                                                      *
! * ### 25-FEB-2008  GET_REFR_DER_XI v3.0 (c) L. Petrov 18-FEB-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      REAL*8     MAT_XI_TO_X3(3,3), VEC_GROUND_XYZ(3), VEC_XI(3), REFR_VAL
      INTEGER*4  TYP, IUER
      REAL*8     VEC_XYZ(3), VEC_HLP(3)
      INTEGER*4  IER
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

      CALL ERR_PASS ( IUER, IER )
      CALL GET_REFR_HLP ( SPD, TYP,  VEC_HLP, REFR_VAL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5561, IUER, 'GET_REFR_DER_XI', 'Error in '// &
     &         'computing refractivity.' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_REFR_XI  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_REFR_HLP ( SPD, TYP, VEC_HLP, REFR_VAL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_REFR_HLP
! *                                                                      *
! * ### 11-MAY-2014    GET_REFR_HLP   v1.0 (c) L. Petrov 11-MAY-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      REAL*8     VEC_HLP(3), REFR_VAL
      REAL*8     ARGS(3)
      INTEGER*4  TYP, INDS(3), DIMS(3), IUER
      INTEGER*4  IER
      REAL*8,    EXTERNAL :: VAL_3D_BSPL, VAL_3D_BSPLE3
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
!      REFR_VAL = VAL_3D_BSPL ( ARGS, SPD__MDEG, DIMS, INDS, &
!     &                         SPD%LEV(1),  &
!     &                         SPD%LON(1),  &
!     &                         SPD%LAT(1),  &
!     &                         SPD%REF_3D(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,TYP) )
!      REFR_VAL = VAL_3D_BSPLE ( ARGS, SPD__MDEG, DIMS, INDS, &
!     &                           SPD%LEV,  &
!     &                           SPD%LON,  &
!     &                           SPD%LAT,  &
!     &                           SPD%REF_3D(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,TYP) )
!!!
      REFR_VAL = VAL_3D_BSPLE3 ( ARGS, DIMS, INDS, &
     &                           SPD%LEV,  &
     &                           SPD%LON,  &
     &                           SPD%LAT,  &
     &                           SPD%REF_3D(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,TYP) )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_REFR_HLP  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_REFR_HLPT ( SPD_4D, TYP, VEC_HLP, MJD, TAI, REFR_VAL, &
     &                           IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_REFR_HLP
! *                                                                      *
! * ### 10-NOV-2014   GET_REFR_HLP   v1.0 (c) L. Petrov 10-NOV-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE     ( SPD_4D__TYPE ) :: SPD_4D
      INTEGER*4  MJD
      REAL*8     VEC_HLP(3), REFR_VAL, TAI
      REAL*4     ARGS(4)
      INTEGER*4  TYP, INDS(4), IUER
      INTEGER*4  IER
      REAL*8,    EXTERNAL :: VAL_3D_BSPL, VAL_3D_BSPLE, VAL_3D_BSPLE3
!
! --- Set pivotal indices
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPD_4D_GET_INDS ( SPD_4D, MJD, TAI, VEC_HLP, ARGS, INDS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6191, IUER, 'GET_REFR_HLP', 'Trap of '// &
     &         'internal control: error in computing pivotal index' )
           RETURN 
      END IF
!
!      REFR_VAL = VAL_3D_BSPL ( ARGS, SPD__MDEG, DIMS, INDS, &
!     &                         SPD%LEV(1),  &
!     &                         SPD%LON(1),  &
!     &                         SPD%LAT(1),  &
!     &                         SPD%REF_3D(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,TYP) )
!      REFR_VAL = VAL_3D_BSPLE ( ARGS, SPD__MDEG, DIMS, INDS, &
!     &                           SPD%LEV,  &
!     &                           SPD%LON,  &
!     &                           SPD%LAT,  &
!     &                           SPD%REF_3D(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,TYP) )
!!!
      REFR_VAL = VAL_3D_BSPLE3 ( ARGS, SPD_4D%DIMS(1:4), INDS, &
     &                           SPD_4D%LEV,  &
     &                           SPD_4D%LON,  &
     &                           SPD_4D%LAT,  &
     &                           SPD_4D%TIM,  &
     &                           SPD_4D%REFR(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,TYP) )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_REFR_HLPT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_REFR_DER_HLP ( SPD, TYP, VEC_HLP, REF_R8, REF_DER_R8 )
! ************************************************************************
! *                                                                      *
! *   Routine GET_REFR_DER_HLP
! *                                                                      *
! * ### 25-FEB-2008  GET_REFR_DER_HLP v2.0 (c) L. Petrov 18-FEB-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      REAL*8     VEC_HLP(3), REF_R8, REF_DER_R8(3)
      REAL*8     REF_R4, REF_DER_R4(3), REF_TMP_DER_R4(3) !!
      REAL*8     ARGS(3) !!
      INTEGER*4  TYP, INDS(3), DIMS(3), IUER
!
! --- Set pivotal indices
!
      IUER = -1
      CALL SPD_GET_INDS ( SPD, VEC_HLP, ARGS, INDS, DIMS, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6181, IUER, 'GET_REFR_DER_HLP', 'Trap of '// &
     &         'internal control: error in computing pivotal index' )
           CALL PETUTIL_TRAP ()
      END IF
!
!      CALL VAL_DER_3D_BSPL ( ARGS, SPD__MDEG, DIMS, INDS, &
!     &                       SPD%LEV(1),  &
!     &                       SPD%LON(1),  &
!     &                       SPD%LAT(1),  &
!     &                       SPD%REF_3D(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,TYP), &
!     &                       REF_R4, REF_DER_R4 )
!      REF_R8     = REF_R4
!      REF_DER_R8 = REF_DER_R4
!!
!      CALL VAL_DER_3D_BSPLE ( ARGS, SPD__MDEG, DIMS, INDS, &
!     &                         SPD%LEV,  &
!     &                         SPD%LON,  &
!     &                         SPD%LAT,  &
!     &                         SPD%REF_3D(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,TYP), &
!     &                         REF_R8, REF_DER_R8 )
      CALL VAL_DER_3D_BSPLE3 ( ARGS, DIMS, INDS, &
     &                         SPD%LEV,  &
     &                         SPD%LON,  &
     &                         SPD%LAT,  &
     &                         SPD%REF_3D(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,TYP), &
     &                         REF_R8, REF_DER_R8 )
!
      RETURN
      END  SUBROUTINE GET_REFR_DER_HLP  !#!  
