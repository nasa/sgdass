      SUBROUTINE SPD_4D_GET_INDS ( SPD_4D, MJD, TAI, VEC_HLP, ARGS, &
     &                             INDS, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine  SPD_4D_GET_INDS computes pivotal indices and   *
! *   the argument vector for using them
! *                                                                      *
! * ### 10-NOV-2014 SPD_4D_GET_INDS  v1.1 (c)  L. Petrov 22-APR-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE     ( SPD_4D__TYPE ) :: SPD_4D
      REAL*8     VEC_HLP(3)
      REAL*4     ARGS(4)
      INTEGER*4  MJD, INDS(4), IUER
      REAL*8     TAI
      REAL*4     EPS
      PARAMETER  ( EPS = 0.05 )
      INTEGER*4, EXTERNAL :: IXMN4, IXMN8
!
      ARGS(1) = VEC_HLP(1)
      ARGS(2) = VEC_HLP(2)
      ARGS(3) = VEC_HLP(3)
      ARGS(4) = (MJD - SPD_4D%MJD_0)*86400.0D0 + (TAI - SPD_4D%TAI_0)
!
      IF ( ARGS(1) < SPD_4D%LEV(1) ) THEN
!
! -------- Check for undershot
!
           IF ( ARGS(1) .GE. SPD_4D%LEV(1) - EPS*(SPD_4D%LEV(2) - SPD_4D%LEV(1)) ) THEN
!
! ------------- This is correctable undershot
!
                INDS(1) = 1
              ELSE 
!$OMP           CRITICAL
                IUER = -1
                WRITE ( 6, * ) 'ARGS(1) = ', ARGS(1) 
                WRITE ( 6, * ) 'SPD_4D%LEV(1,2) = ', SPD_4D%LEV(1), SPD_4D%LEV(2)
                CALL ERR_LOG ( 6191, IUER, 'SPD_4D_GET_INDS', 'Trap of '// &
     &              'internal control: wrong height' )
!$OMP           END CRITICAL
                RETURN 
           END IF
         ELSE IF ( ARGS(1) > SPD_4D%LEV(SPD_4D%DIMS(1)) ) THEN
!
! -------- Check for overrshot
!
           IF ( ARGS(1) .LE. SPD_4D%LEV(SPD_4D%DIMS(1)) + EPS*(SPD_4D%LEV(SPD_4D%DIMS(1)) - SPD_4D%LEV(SPD_4D%DIMS(1)-1)) ) THEN
!
! ------------- This is correctable overshot
!
                INDS(1) = SPD_4D%DIMS(1)
              ELSE 
                IUER = -1
                WRITE ( 6, * ) 'ARGS(1) = ', ARGS(1) 
                WRITE ( 6, * ) 'SPD_4D%LEV(NLEV-1,NLEV) = ', SPD_4D%LEV(SPD_4D%DIMS(1)-1), SPD_4D%LEV(SPD_4D%DIMS(1))
                CALL ERR_LOG ( 6192, IUER, 'SPD_4D_GET_INDS', 'Trap of '// &
     &              'internal control: wrong height' )
                RETURN 
           END IF
         ELSE 
           INDS(1) = IXMN4 ( SPD_4D%DIMS(1), SPD_4D%LEV(1), ARGS(1) )
!!           INDS(1) = IXMN8 ( SPD_4D%DIMS(1), SPD_4D%LEV(1), ARGS(1) )
      END IF
!
      IF ( ARGS(2) < SPD_4D%LON(1) ) THEN
           IF ( ARGS(2) .GE. SPD_4D%LON(1) - EPS*(SPD_4D%LON(2) - SPD_4D%LON(1)) ) THEN
!
! ------------- Correctable undeshot
!
                ARGS(2) = ARGS(2)+PI2
                INDS(2) = IXMN4 ( SPD_4D%DIMS(2), SPD_4D%LON(1), SNGL(ARGS(2)+PI2) )
!!                INDS(2) = IXMN8 ( SPD_4D%DIMS(2), SPD_4D%LON(1), ARGS(2)+PI2 )
              ELSE 
                IUER = -1
                WRITE ( 6, * ) 'ARGS(2) = ', ARGS(2) 
                WRITE ( 6, * ) 'SPD_4D%LON(1:2) = ', SPD_4D%LON(1), SPD_4D%LON(2)
                CALL ERR_LOG ( 6193, IUER, 'SPD_4D_GET_INDS', 'Trap of '// &
     &              'internal control: wrong longitude' )
                RETURN 
           END IF
         ELSE IF ( ARGS(2) < SPD_4D%LON(2) ) THEN
           ARGS(2) = VEC_HLP(2)+PI2
           INDS(2) = IXMN4 ( SPD_4D%DIMS(2), SPD_4D%LON(1), SNGL(VEC_HLP(2)+PI2) )
!!           INDS(2) = IXMN8 ( SPD_4D%DIMS(2), SPD_4D%LON(1), VEC_HLP(2)+PI2 )
         ELSE IF ( ARGS(2) > SPD_4D%LON(SPD_4D%DIMS(2)) ) THEN
           IUER = -1
           WRITE ( 6, * ) 'ARGS(2) = ', ARGS(2) 
           WRITE ( 6, * ) 'SPD_4D%LON(NLON-1,NLON) = ', SPD_4D%LON(SPD_4D%DIMS(2)-1), SPD_4D%LON(SPD_4D%DIMS(2))
           CALL ERR_LOG ( 6194, IUER, 'SPD_4D_GET_INDS', 'Trap of '// &
     &         'internal control: wrong longitude' )
           RETURN 
         ELSE 
           INDS(2) = IXMN4 ( SPD_4D%DIMS(2), SPD_4D%LON(1), ARGS(2) )
!!           INDS(2) = IXMN8 ( SPD_4D%DIMS(2), SPD_4D%LON(1), ARGS(2) )
      END IF
!
      IF ( ARGS(3) < SPD_4D%LAT(1) ) THEN
!
! -------- Check for undershot
!
           IF ( ARGS(3) .GE. SPD_4D%LAT(1) - EPS*(SPD_4D%LAT(2) - SPD_4D%LAT(1)) ) THEN
                INDS(3) = 1
              ELSE 
                IUER = -1
                WRITE ( 6, * ) 'ARGS(3) = ', ARGS(3) 
                WRITE ( 6, * ) 'SPD_4D%LAT(1:2) = ', SPD_4D%LAT(1), SPD_4D%LAT(2)
                CALL ERR_LOG ( 6195, IUER, 'SPD_4D_GET_INDS', 'Trap of '// &
     &              'internal control: wrong latitude' )
                RETURN
           END IF
         ELSE IF ( ARGS(3) > SPD_4D%LAT(SPD_4D%DIMS(3)) ) THEN
!
! -------- Check for overshot
!
           IF ( ARGS(3) .LE. SPD_4D%LAT(SPD_4D%DIMS(3)) + EPS*(SPD_4D%LAT(SPD_4D%DIMS(3)) - SPD_4D%LAT(SPD_4D%DIMS(3)-1)) ) THEN
                INDS(3) = SPD_4D%DIMS(3)
              ELSE 
                IUER = -1
                WRITE ( 6, * ) 'ARGS(3) = ', ARGS(3) 
                WRITE ( 6, * ) 'SPD_4D%LAT(NLAT-1:NLAT) = ', SPD_4D%LAT(SPD_4D%DIMS(3)-1), SPD_4D%LAT(SPD_4D%DIMS(3))
                CALL ERR_LOG ( 6196, IUER, 'SPD_4D_GET_INDS', 'Trap of '// &
     &              'internal control: wrong latitude' )
                RETURN 
           END IF
         ELSE 
           INDS(3) = IXMN4 ( SPD_4D%DIMS(3), SPD_4D%LAT(1), ARGS(3) )
!!           INDS(3) = IXMN8 ( SPD_4D%DIMS(3), SPD_4D%LAT(1), ARGS(3) )
      END IF
!
      IF ( ARGS(4) < 0.0 ) THEN
!
! -------- Check for undershot
!
           IF ( (MJD*86400.0D0 + TAI) < EPS ) THEN
                INDS(4) = 1
              ELSE 
                IUER = -1
                WRITE ( 6, * ) 'ARGS(4) = ', ARGS(4) 
                CALL ERR_LOG ( 6197, IUER, 'SPD_4D_GET_INDS', 'Trap of '// &
     &              'internal control: wrong time argument' )
                RETURN
           END IF
         ELSE IF ( ARGS(4) > SPD_4D%TIM(SPD_4D%DIMS(4)) ) THEN
!
! -------- Check for overshot
!
           IF ( ARGS(4) .LE. SPD_4D%TIM(SPD_4D%DIMS(4)) + EPS ) THEN
                INDS(4) = SPD_4D%DIMS(4)
              ELSE 
                IUER = -1
                WRITE ( 6, * ) 'ARGS(4) = ', ARGS(4) 
                WRITE ( 6, * ) 'MJD, TAI= ',  MJD, TAI
                WRITE ( 6, * ) 'SPD_4D%MJD_0, SPD_4D%TAI_0= ', SPD_4D%MJD_0, SPD_4D%TAI_0
                CALL ERR_LOG ( 6198, IUER, 'SPD_4D_GET_INDS', 'Trap of '// &
     &              'internal control: wrong time' )
                RETURN 
           END IF
         ELSE 
           INDS(4) = IXMN4 ( SPD_4D%DIMS(4), SPD_4D%TIM(1), ARGS(4) )
!!           INDS(4) = IXMN8 ( SPD_4D%DIMS(4), SPD_4D%TIM(1), ARGS(4) )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_4D_GET_INDS  !#!#
