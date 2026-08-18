      SUBROUTINE SPD_CLIENT_VERSION_CHECK ()
! ************************************************************************
! *                                                                      *
! *   Routine SPD_CLIENT_VERSION_CHECK checks version mismatch between   *
! *   the version of the SPD client library defined in the spd.i and     *
! *   the spd clioent version compiled in.                               *
! *                                                                      *
! * ## 27-APR-2020 SPD_CLIENT_VERSION_CHECK v2.0 (c) L. Petrov 05-NOV-2020 ##  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      CHARACTER  SPD_COMPILED_VERS*8, SPD_DEFINED_VERS*8
!
      CALL SPD_CLIENT_VERSION ( 'SPD_CLIENT_VERSION', SPD_COMPILED_VERS )
      SPD_DEFINED_VERS  = SPD_CLIENT__LABEL(13:20)
      IF ( SPD_DEFINED_VERS .NE. SPD_COMPILED_VERS ) THEN
           CALL ERR_LOG ( 8001, -3, 'SPD_CLIENT_VERSION_CHECK', 'A mismatch '// &
     &         'between the SPD client version defined in spd.i '//SPD_DEFINED_VERS// &
     &         ' and the version defined during compilation '//SPD_COMPILED_VERS// &
     &         ' has been detected. Please reconfigure and re-compile spd library '// &
     &         'with commands reconfigure; make clean; make; make install' )
           CALL EXIT ( 1 )
      END IF
      RETURN
      END  SUBROUTINE  SPD_CLIENT_VERSION_CHECK  !#!#
