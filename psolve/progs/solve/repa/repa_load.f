      SUBROUTINE REPA_LOAD ( REP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine REPA_LOAD loads the data into internal data sructures of   *
! *   REPA.                                                              *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 01-DEC-2004   REPA_LOAD   v1.0 (d)  L. Petrov  01-DEC-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'repa.i' 
      TYPE     ( REP__TYPE ) :: REP
      INTEGER*4  IUER
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  REPA_LOAD
