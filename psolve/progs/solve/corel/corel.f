      PROGRAM COREL
      IMPLICIT NONE
!
      INCLUDE 'corpar.i'
      INCLUDE 'corema.i'
      INCLUDE 'corcom.i'
      INCLUDE 'precm.i'
      LOGICAL*2 KREF
!
      CALL PRE_PROG()
      INCLUDE 'corel_version.i' ! Set revision date of the current version
!
      kref=.TRUE.
!
      CALL INITI()
!
      CALL PREPAS(kref)
!
      CALL PROCES(kref)
!
      call end_prog()
      END
