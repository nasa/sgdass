      SUBROUTINE SET_MERG ( CMERG )
! ************************************************************************
! *                                                                      *
! *   Auxillary subroutine for updatibng field MERGCGM in glbcm.i        *
! *   common block. This routine is used in order to resolve name        *
! *   conflicts: name MERGCGM is used differently in some bath routines. *
! *                                                                      *
! *  ### 23-OCT-2000   SET_MERG    v1.0 (c)  L. Petrov  23-OCT-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbcm.i'
      CHARACTER  CMERG(10)*(*)
!
      CALL USE_GLBFIL ( 'OR' )
      MERGCGM = CMERG(1)
      CALL USE_GLBFIL ( 'WC' )
!
      RETURN
      END  !#!  SET_MERG  #!#
