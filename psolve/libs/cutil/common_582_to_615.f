      SUBROUTINE COMMON_582_to_615()
! ************************************************************************
! *                                                                      *
! *   Routine  COMMON_582_TO_615  transfroms common block from 592-block *
! *   format (pre DEC-2025) to 615-block format (post DEC-2025 ).        *
! *                                                                      *
! *   It is assumed that the image of 46-block of SOCOM resids there     *
! *   where the image of 58-block should be located.                     *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ### 13-DEC-2025 COMMON_582_TO_615 v1.0 (d) L. Petrov 13-DEC-2025 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INTEGER*4  NB, NB_582
      INTEGER*4  MP, JSOCOM_BLOCKS_582
      PARAMETER  ( MP = 1024*1024 )
      PARAMETER  ( JSOCOM_BLOCKS_582 = 582 )
      INTEGER*1  ISOCOM_582(MP)
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      NB_582 = JSOCOM_BLOCKS_582*INT4(BLOCK_BYTES)
      NB    = JSOCOM_BLOCKS*INT4(BLOCK_BYTES)
!
! --- Copying content of socom to its 582-block sister
!
      CALL LIB$MOVC3 ( NB_582, ISOCOM, ISOCOM_582 )
      CALL NOUT      ( NB,     ISOCOM )
      CALL LIB$MOVC3 ( NB_582, ISOCOM_582, ISOCOM )
      MEAN_BAS_EFF_FREQ = 0.0D0
!
      RETURN
      END  !#!  COMMON_582_TO_615  #!#
