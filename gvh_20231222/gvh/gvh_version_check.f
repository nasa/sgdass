      SUBROUTINE GVH_VERSION_CHECK ()
! ************************************************************************
! *                                                                      *
! *   Routine GVH_VERSION_CHECK checks version mismatch between          *
! *   the version of the GVH library defined in the gvh.i and            *
! *   the gvh version compiled in.                                       *
! *                                                                      *
! * ## 05-NOV-2020  GVH_VERSION_CHECK v1.0 (c) L. Petrov 05-NOV-2020 ##  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'gvh.i'
      CHARACTER  GVH_COMPILED_VERS*8, GVH_DEFINED_VERS*8
!
      CALL GVH_VERSION ( 'GVH_VERSION', GVH_COMPILED_VERS )
      GVH_DEFINED_VERS  = GVH__LABEL(5:12)
      IF ( GVH_DEFINED_VERS .NE. GVH_COMPILED_VERS ) THEN
           CALL ERR_LOG ( 8001, -3, 'GVH_VERSION_CHECK', 'A mismatch '// &
     &         'between the GVH client version defined in spd.i '//GVH_DEFINED_VERS// &
     &         ' and the version defined during compilation '//GVH_COMPILED_VERS// &
     &         ' has been detected. Please reconfigure and re-compile spd library '// &
     &         'with commands reconfigure; make clean; make; make install' )
           CALL EXIT ( 1 )
      END IF
      RETURN
      END  SUBROUTINE  GVH_VERSION_CHECK  !#!#
