        SUBROUTINE NERS_ROTMAT ( IAX, ANGLE, MAT )
! ************************************************************************
! *                                                                      *
! *     Auxiliary routine  NERS_ROTMAT  computes the rotation matrix     *
! *   3x3 with respet to the axis IAX at the angle ANGLE (in rad).       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    IAX ( INTEGER*4 ) -- index of the axis: one of 1 (X), 2 (Y),      *
! *                         or 3 (Z)                                     *
! *  ANGLE ( REAL*8    ) -- Angle. Units: rad.                           *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    MAT ( REAL*8    ) -- Rotation matrix 3,3                          *
! *                                                                      *
! * ###  23-JUN-1992  NERS_ROTMAT   v2.0 (c) L. Petrov 08-DEC-2003  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IAX
      REAL*8     ANGLE, MAT(3,3), DC, DS
!
      DC = DCOS(ANGLE)
      DS = DSIN(ANGLE)
!
      IF ( IAX .EQ. 1 ) THEN
           MAT(1,1) = 1.D0
           MAT(1,2) = 0.D0
           MAT(1,3) = 0.D0
!
           MAT(2,1) = 0.D0
           MAT(2,2) = DC
           MAT(2,3) = DS
!
           MAT(3,1) =  0.D0
           MAT(3,2) = -1.D0*DS
           MAT(3,3) = DC
        ELSE IF ( IAX .EQ. 2 ) THEN
           MAT(1,1) = DC
           MAT(1,2) = 0.D0
           MAT(1,3) = -1.D0*DS
!
           MAT(2,1) = 0.D0
           MAT(2,2) = 1.D0
           MAT(2,3) = 0.D0
!
           MAT(3,1) = DS
           MAT(3,2) = 0.D0
           MAT(3,3) = DC
        ELSE IF ( IAX .EQ. 3 ) THEN
           MAT(1,1) = DC
           MAT(1,2) = DS
           MAT(1,3) = 0.D0
!
           MAT(2,1) = -1.D0*DS
           MAT(2,2) = DC
           MAT(2,3) = 0.D0
!
           MAT(3,1) = 0.D0
           MAT(3,2) = 0.D0
           MAT(3,3) = 1.D0
        ELSE
           WRITE ( 6, * ) ' IAX = ', IAX
           CALL ERR_LOG ( 123, -1, 'NERS_ROTMAT', 'Error of internal '// &
     &         'control: wrong parameter IAX' )
           CALL EXIT ( 1 )
      END IF
!
      RETURN
      END  SUBROUTINE  NERS_ROTMAT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_ROTMAT_DER ( IAX, ANGLE, ANGLE_RATE, MAT )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine  NERS_ROTMAT_DER  computes the first derivative  *
! *   of rotation matrix 3x3 with respet to the axis IAX at the angle    *
! *   ANGLE (in rad).                                                    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *         IAX ( INTEGER*4 ) -- index of the axis: one of 1 (X), 2 (Y), *
! *                              or 3 (Z)                                *
! *       ANGLE ( REAL*8    ) -- Angle. Units: rad.                      *
! *  ANGLE_RATE ( REAL*8    ) -- Rate of change of the angle.            *
! *                              Units: rad/sec.                         *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    MAT ( REAL*8    ) -- Rotation matrix 3,3                          *
! *                                                                      *
! * ### 23-JUN-1992  NERS_ROTMAT_DER  v2.0 (c) L. Petrov 08-DEC-2003 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IAX
      REAL*8     ANGLE, ANGLE_RATE, MAT(3,3), DC, DS
!
      DC = DCOS(ANGLE)
      DS = DSIN(ANGLE)
!
      IF ( IAX .EQ. 1 ) THEN
           MAT(1,1) =  0.D0
           MAT(1,2) =  0.D0
           MAT(1,3) =  0.D0
!
           MAT(2,1) =  0.D0
           MAT(2,2) = -ANGLE_RATE*DS
           MAT(2,3) =  ANGLE_RATE*DC
!
           MAT(3,1) =  0.D0
           MAT(3,2) = -ANGLE_RATE*DC
           MAT(3,3) = -ANGLE_RATE*DS
        ELSE IF ( IAX .EQ. 2 ) THEN
           MAT(1,1) = -ANGLE_RATE*DS
           MAT(1,2) =  0.D0
           MAT(1,3) = -ANGLE_RATE*DC
!
           MAT(2,1) =  0.D0
           MAT(2,2) =  0.D0
           MAT(2,3) =  0.D0
!
           MAT(3,1) =  ANGLE_RATE*DC
           MAT(3,2) =  0.D0
           MAT(3,3) = -ANGLE_RATE*DS
        ELSE IF ( IAX .EQ. 3 ) THEN
           MAT(1,1) = -ANGLE_RATE*DS
           MAT(1,2) =  ANGLE_RATE*DC
           MAT(1,3) =  0.D0
!
           MAT(2,1) = -ANGLE_RATE*DC
           MAT(2,2) = -ANGLE_RATE*DS
           MAT(2,3) =  0.D0
!
           MAT(3,1) =  0.D0
           MAT(3,2) =  0.D0
           MAT(3,3) =  0.D0
        ELSE
           WRITE ( 6, * ) ' IAX = ', IAX
           CALL ERR_LOG ( 124, -1, 'NERS_ROTMAT_DER', 'Error of internal '// &
     &         'control: wrong parameter IAX' )
           CALL EXIT ( 1 )
      END IF
!
      RETURN
      END  SUBROUTINE  NERS_ROTMAT_DER  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_ROTMAT_DER2 ( IAX, ANGLE, ANGLE_RATE, ANGLE_ACCL, MAT )
! ************************************************************************
! *                                                                      *
! *     Auxiliary routine  NERS_ROTMAT_DER2  computes the second         *
! *   derivative of the rotation matrix 3x3 with respet to the axis      *
! *   IAX at the angle ANGLE.                                            *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *         IAX ( INTEGER*4 ) -- index of the axis: one of 1 (X), 2 (Y), *
! *                              or 3 (Z)                                *
! *       ANGLE ( REAL*8    ) -- Angle. Units: rad.                      *
! *  ANGLE_RATE ( REAL*8    ) -- Rate of change of the angle.            *
! *                              Units: rad/sec.                         *
! *  ANGLE_ACCL ( REAL*8    ) -- Acceleration of the change of the angle.*
! *                              Units: rad/sec^2.                       *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    MAT ( REAL*8    ) -- Rotation matrix 3,3                          *
! *                                                                      *
! * ### 23-JUN-1992  NERS_ROTMAT_DER2 v2.0 (c) L. Petrov 08-JUN-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IAX
      REAL*8     ANGLE, ANGLE_RATE, ANGLE_ACCL, MAT(3,3), DC, DS
!
      DC = DCOS(ANGLE)
      DS = DSIN(ANGLE)
!
      IF ( IAX .EQ. 1 ) THEN
           MAT(1,1)=0.D0
           MAT(1,2)=0.D0
           MAT(1,3)=0.D0
!
           MAT(2,1) = 0.D0
           MAT(2,2) = -ANGLE_ACCL*DS - (ANGLE_RATE*ANGLE_RATE)*DC
           MAT(2,3) =  ANGLE_ACCL*DC - (ANGLE_RATE*ANGLE_RATE)*DS
!
           MAT(3,1) = 0.D0
           MAT(3,2) = -ANGLE_ACCL*DC + (ANGLE_RATE*ANGLE_RATE)*DS
           MAT(3,3) = -ANGLE_ACCL*DS - (ANGLE_RATE*ANGLE_RATE)*DC
        ELSE IF ( IAX .EQ. 2 ) THEN
           MAT(1,1) = -ANGLE_ACCL*DS + (ANGLE_RATE*ANGLE_RATE)*DC
           MAT(1,2) =  0.D0
           MAT(1,3) = -ANGLE_ACCL*DC + (ANGLE_RATE*ANGLE_RATE)*DS
!
           MAT(2,1) =  0.D0
           MAT(2,2) =  0.D0
           MAT(2,3) =  0.D0
!
           MAT(3,1) =  ANGLE_ACCL*DC - (ANGLE_RATE*ANGLE_RATE)*DS
           MAT(3,2) =  0.D0
           MAT(3,3) = -ANGLE_ACCL*DS + (ANGLE_RATE*ANGLE_RATE)*DC
        ELSE IF ( IAX .EQ. 3 ) THEN
           MAT(1,1) = -ANGLE_ACCL*DS - (ANGLE_RATE*ANGLE_RATE)*DC
           MAT(1,2) =  ANGLE_ACCL*DC - (ANGLE_RATE*ANGLE_RATE)*DS
           MAT(1,3) =  0.D0
!
           MAT(2,1) = -ANGLE_ACCL*DC + (ANGLE_RATE*ANGLE_RATE)*DS
           MAT(2,2) = -ANGLE_ACCL*DS - (ANGLE_RATE*ANGLE_RATE)*DC
           MAT(2,3) = 0.D0
!
           MAT(3,1) = 0.D0
           MAT(3,2) = 0.D0
           MAT(3,3) = 0.D0
        ELSE
           WRITE ( 6, * ) ' IAX = ', IAX
           CALL ERR_LOG ( 125, -1, 'NERS_ROTMAT_DER2', 'Error of internal '// &
     &         'control: wrong parameter IAX' )
           CALL EXIT ( 1 )
      END IF
!
      RETURN
      END  SUBROUTINE  NERS_ROTMAT_DER2  !#!#
