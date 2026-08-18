      SUBROUTINE PIMA_INIT ( PIM )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine initializes PIMA internal data structure.       *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 06-JAN-2006   PIMA_INIT   v1.0 (d)  L. Petrov  06-JAN-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
!
      CALL NOUT ( SIZEOF(PIM), PIM )
!
      RETURN
      END  SUBROUTINE  PIMA_INIT  !#!#
