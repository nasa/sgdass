      SUBROUTINE PIMA_SPLT_SORT ( PIM, LFRQ, LTIM, STA_IND_ARR, WEI_ARR, &
     &                            TIM_ARR, IND_OBS_ARR, UVO, SNR_ARR )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_SPLT_SORT 
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ### 19-MAY-2012  PIMA_SPLT_SORT  v1.0 (d) L. Petrov 19-MAY-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INTEGER*4  IUER
      TYPE     ( PIMA__TYPE     ) :: PIM
      INTEGER*4  
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_SPLT_SORT  !#!#
