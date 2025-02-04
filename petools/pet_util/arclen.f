      FUNCTION ARC_LEN_AD ( ALP1, DEL1, ALP2, DEL2 )
! ************************************************************************
! *                                                                      *
! *   Routine ARC_LEN_AD returns the arc lenth between two sources       *
! *   with specified right ascensions and declinations.                  *
! *                                                                      *
! *  ### 06-DEC-2010  ARC_LEN_AD   v1.0 (c)  L. Petrov  06-DEC-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT NONE 
      REAL*8   ALP1, DEL1, ALP2, DEL2
      REAL*8   ARC_LEN_AD
      REAL*8   VEC1(3), VEC2(3)
!
      VEC1(1) = DCOS(DEL1)*DCOS(ALP1)
      VEC1(2) = DCOS(DEL1)*DSIN(ALP1)
      VEC1(3) = DSIN(DEL1)
!
      VEC2(1) = DCOS(DEL2)*DCOS(ALP2)
      VEC2(2) = DCOS(DEL2)*DSIN(ALP2)
      VEC2(3) = DSIN(DEL2)
!
      ARC_LEN_AD = 2.D0*DATAN ( DSQRT( (VEC1(1) - VEC2(1))**2 + &
     &                                 (VEC1(2) - VEC2(2))**2 + &
     &                                 (VEC1(3) - VEC2(3))**2   &
     &                               )/2.0D0 )
!
      RETURN
      END  FUNCTION ARC_LEN_AD  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION ARC_LEN_VEC ( S_VEC1, S_VEC2 )
! ************************************************************************
! *                                                                      *
! *   Routine ARC_LEN_AD returns the arc lenth between two sources       *
! *   with specified unit vector of their positions.                     *
! *                                                                      *
! *  ### 06-DEC-2010  ARC_LEN_VEC  v1.0 (c)  L. Petrov  06-DEC-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT NONE 
      REAL*8   ARC_LEN_VEC
      REAL*8   S_VEC1(3), S_VEC2(3)
!
      ARC_LEN_VEC = 2.D0*DATAN ( DSQRT( (S_VEC1(1) - S_VEC2(1))**2 + &
     &                                  (S_VEC1(2) - S_VEC2(2))**2 + &
     &                                  (S_VEC1(3) - S_VEC2(3))**2   &
     &                                )/2.0D0 )
!
      RETURN
      END  FUNCTION ARC_LEN_VEC  !#!  
