      SUBROUTINE SPD_GET_INDS ( SPD, VEC_HLP, ARGS, INDS, DIMS, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine  SPD_GET_INDS computes pivotal indices and      *
! *   the argument vector for using them
! *                                                                      *
! * ### 16-FEB-2014  SPD_GET_INDS  v1.0 (c)  L. Petrov  16-FEB-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      REAL*8     VEC_HLP(3)
      REAL*8     ARGS(3)  !!
      REAL*4     EPS
      PARAMETER  ( EPS = 0.05 )
      INTEGER*4  INDS(3), DIMS(3), IUER
      INTEGER*4, EXTERNAL :: IXMN4, IXMN8
!
      ARGS(1) = VEC_HLP(1)
      ARGS(2) = VEC_HLP(2)
      ARGS(3) = VEC_HLP(3)
!
      DIMS(1) = SPD%NLEV
      DIMS(2) = SPD%NLON
      DIMS(3) = SPD%NLAT
!
      IF ( VEC_HLP(1) < SPD%LEV(1) ) THEN
!
! -------- Check for undershot
!
           IF ( VEC_HLP(1) .GE. SPD%LEV(1) - EPS*(SPD%LEV(2) - SPD%LEV(1)) ) THEN
!
! ------------- This is correctable undershot
!
                INDS(1) = 1
              ELSE 
                IUER = -1
                WRITE ( 6, * ) 'VEC_HLP(1) = ', VEC_HLP(1) 
                WRITE ( 6, * ) 'SPD%LEV(1,2) = ', SPD%LEV(1), SPD%LEV(2)
                CALL ERR_LOG ( 6161, IUER, 'SPD_GET_INDS', 'Trap of '// &
     &              'internal control: wrong height' )
                RETURN 
           END IF
         ELSE IF ( VEC_HLP(1) > SPD%LEV(SPD%NLEV) ) THEN
!
! -------- Check for overrshot
!
           IF ( VEC_HLP(1) .LE. SPD%LEV(SPD%NLEV) + EPS*(SPD%LEV(SPD%NLEV) - SPD%LEV(SPD%NLEV-1)) ) THEN
!
! ------------- This is correctable overshot
!
                INDS(1) = SPD%NLEV
              ELSE 
                IUER = -1
                WRITE ( 6, * ) 'VEC_HLP(1) = ', VEC_HLP(1) 
                WRITE ( 6, * ) 'SPD%LEV(NLEV-1,NLEV) = ', SPD%LEV(SPD%NLEV-1), SPD%LEV(SPD%NLEV)
                CALL ERR_LOG ( 6162, IUER, 'GET_GET_INDS', 'Trap of '// &
     &              'internal control: wrong height' )
                RETURN 
           END IF
         ELSE 
!!           INDS(1) = IXMN4 ( SPD%NLEV, SPD%LEV(1), SNGL(VEC_HLP(1)) )
           INDS(1) = IXMN8 ( SPD%NLEV, SPD%LEV(1), VEC_HLP(1) )
      END IF
!
      IF ( VEC_HLP(2) < SPD%LON(1) ) THEN
           IF ( VEC_HLP(2) .GE. SPD%LON(1) - EPS*(SPD%LON(2) - SPD%LON(1)) ) THEN
!
! ------------- Correctable undeshot
!
                ARGS(2) = VEC_HLP(2)+PI2
!!                INDS(2) = IXMN4 ( SPD%NLON, SPD%LON(1), SNGL(VEC_HLP(2)+PI2) )
                INDS(2) = IXMN8 ( SPD%NLON, SPD%LON(1), VEC_HLP(2)+PI2 )
              ELSE 
                IUER = -1
                WRITE ( 6, * ) 'VEC_HLP(2) = ', VEC_HLP(2) 
                WRITE ( 6, * ) 'SPD%LON(1:2) = ', SPD%LON(1), SPD%LON(2)
                CALL ERR_LOG ( 6163, IUER, 'GET_GET_INDS', 'Trap of '// &
     &              'internal control: wrong longitude' )
                RETURN 
           END IF
         ELSE IF ( VEC_HLP(2) < SPD%LON(2) ) THEN
           ARGS(2) = VEC_HLP(2)+PI2
!!           INDS(2) = IXMN4 ( SPD%NLON, SPD%LON(1), SNGL(VEC_HLP(2)+PI2) )
           INDS(2) = IXMN8 ( SPD%NLON, SPD%LON(1), VEC_HLP(2)+PI2 )
         ELSE IF ( VEC_HLP(2) > SPD%LON(SPD%NLON) ) THEN
           IUER = -1
           WRITE ( 6, * ) 'VEC_HLP(2) = ', VEC_HLP(2) 
           WRITE ( 6, * ) 'SPD%LON(NLON-1,NLON) = ', SPD%LON(SPD%NLON-1), SPD%LON(SPD%NLON)
           CALL ERR_LOG ( 6164, IUER, 'GET_GET_INDS', 'Trap of '// &
     &         'internal control: wrong longitude' )
           RETURN 
         ELSE 
!!           INDS(2) = IXMN4 ( SPD%NLON, SPD%LON(1), SNGL(VEC_HLP(2)) )
           INDS(2) = IXMN8 ( SPD%NLON, SPD%LON(1), VEC_HLP(2) )
      END IF
!
      IF ( VEC_HLP(3) < SPD%LAT(1) ) THEN
!
! -------- Check for undershot
!
           IF ( VEC_HLP(3) .GE. SPD%LAT(1) - EPS*(SPD%LAT(2) - SPD%LAT(1)) ) THEN
                INDS(3) = 1
              ELSE 
                IUER = -1
                WRITE ( 6, * ) 'VEC_HLP(3) = ', VEC_HLP(3) 
                WRITE ( 6, * ) 'SPD%LAT(1:2) = ', SPD%LAT(1), SPD%LAT(2)
                CALL ERR_LOG ( 6165, IUER, 'GET_GET_INDS', 'Trap of '// &
     &              'internal control: wrong latitude' )
                RETURN
           END IF
         ELSE IF ( VEC_HLP(3) > SPD%LAT(SPD%NLAT) ) THEN
!
! -------- Check for overshot
!
           IF ( VEC_HLP(3) .LE. SPD%LAT(SPD%NLAT) + EPS*(SPD%LAT(SPD%NLAT) - SPD%LAT(SPD%NLAT-1)) ) THEN
                INDS(3) = SPD%NLAT
              ELSE 
                IUER = -1
                WRITE ( 6, * ) 'VEC_HLP(3) = ', VEC_HLP(3) 
                WRITE ( 6, * ) 'SPD%LAT(NLAT-1:NLAT) = ', SPD%LAT(SPD%NLAT-1), SPD%LAT(SPD%NLAT)
                CALL ERR_LOG ( 6166, IUER, 'GET_GET_INDS', 'Trap of '// &
     &              'internal control: wrong latitude' )
                RETURN 
           END IF
         ELSE 
!!           INDS(3) = IXMN4 ( SPD%NLAT, SPD%LAT(1), SNGL(VEC_HLP(3)) )
           INDS(3) = IXMN8 ( SPD%NLAT, SPD%LAT(1), VEC_HLP(3) )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_GET_INDS  !#!#
