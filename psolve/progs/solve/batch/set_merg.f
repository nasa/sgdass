      SUBROUTINE SET_MERG ( CMERG )
! ************************************************************************
! *                                                                      *
! *   Auxillary subroutine for updatibng field MERGCGM in glbcm.i        *
! *   common block. This routine is used in order to resolve name        *
! *   conflicts: name MERGCGM is used differently in some bath routines. *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 23-OCT-2000   SET_MERG    v1.0 (d)  L. Petrov  23-OCT-2000 ###  *
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
