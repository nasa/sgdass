      SUBROUTINE SPHE_QUIT ( FSH )
! ************************************************************************
! *                                                                      *
! *   Routine SPHE_QUIT releases dynamic memory allocated inside  the    *
! *   initialize internal data structure for consecutive computation     *
! *   of direct or inverse spherical transform and then releases memory  *
! *   allocated for this structure itself.                               *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  FSH ( SPHE_TYPE ) -- Internal data structure of fourpack packate    *
! *                       that keeps internal arrays with intermediate   *
! *                       results and their status for possible re-use.  *
! *                                                                      *
! * ###   08-AUG-2012   SPHE_QUIT  v1.1  (c)  L. Petrov 17-FEB-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'fourpack.i'
      TYPE     ( SPHE_TYPE ) :: FSH
!
      IF ( ASSOCIATED ( FSH%AJ   ) ) DEALLOCATE ( FSH%AJ )
      IF ( ASSOCIATED ( FSH%F1   ) ) DEALLOCATE ( FSH%F1 )
      IF ( ASSOCIATED ( FSH%F2   ) ) DEALLOCATE ( FSH%F2 )
      IF ( ASSOCIATED ( FSH%F3   ) ) DEALLOCATE ( FSH%F3 )
      IF ( ASSOCIATED ( FSH%PL   ) ) DEALLOCATE ( FSH%PL )
      IF ( ASSOCIATED ( FSH%PLT  ) ) DEALLOCATE ( FSH%PLT )
      IF ( ASSOCIATED ( FSH%DPLT ) ) DEALLOCATE ( FSH%DPLT )
      IF ( ASSOCIATED ( FSH%MSIN ) ) DEALLOCATE ( FSH%MSIN )
      IF ( ASSOCIATED ( FSH%MCOS ) ) DEALLOCATE ( FSH%MCOS )
!
      CALL FREE ( LOC(FSH) )
!
      RETURN
      END  SUBROUTINE  SPHE_QUIT  !#!#
