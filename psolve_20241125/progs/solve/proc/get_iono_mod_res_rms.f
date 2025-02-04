      FUNCTION   GET_IONO_MOD_RES_RMS ( APR_IONO_RMS )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_IONO_MOD_RES_RMS
! *                                                                      *
! * ## 07-APR-2022 GET_IONO_MOD_RES_RMS v1.1 (c) L. Petrov 10-JAN-2023 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     APR_IONO_RMS
      REAL*8     GET_IONO_MOD_RES_RMS 
      REAL*8     EPS
      PARAMETER  ( EPS = 1.D-13 )
      INTEGER*4  DEG, NK
      PARAMETER  ( DEG = 3 )
      PARAMETER  ( NK  = 4 )
!
      REAL*8  ARG_NOD(NK), SPL(1-DEG:NK-1)
      DATA    ARG_NOD /               &
     &                   0.00000D-12, &
     &                  20.00000D-12, &
     &                 120.00000D-12, &
     &                1300.00000D-12  &
     &                      /
!
      DATA    SPL     /                    &
     &                        5.08507D-12, &
     &                       12.52318D-12, &
     &                       24.64614D-12, &
     &                      114.00000D-12, &
     &                      114.00000D-12, &
     &                      114.00000D-12  &
     &                /
      REAL*8, EXTERNAL :: EBSPL_VAL_R8 
      IF ( APR_IONO_RMS < ARG_NOD(NK) - EPS ) THEN
           GET_IONO_MOD_RES_RMS = EBSPL_VAL_R8 ( NK, DEG, APR_IONO_RMS, ARG_NOD, SPL )
         ELSE
           GET_IONO_MOD_RES_RMS = EBSPL_VAL_R8 ( NK, DEG, ARG_NOD(NK) - EPS, ARG_NOD, SPL )
      END IF
      RETURN
      END  FUNCTION  GET_IONO_MOD_RES_RMS  !#!#
