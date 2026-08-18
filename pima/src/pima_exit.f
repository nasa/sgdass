      SUBROUTINE PIMA_EXIT ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_EXIT
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 09-JAN-2006    PIMA_EXIT   v1.0 (d)  L. Petrov 09-JAN-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE    ) :: PIM
      INTEGER*4  IUER
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END   SUBROUTINE   PIMA_EXIT  !#!#
