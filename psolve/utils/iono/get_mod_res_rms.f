      FUNCTION   GET_MOD_RES_RMS ( REG_MOD, APR_IONO_RMS )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_MOD_RES_RMS
! *                                                                      *
! * ### 07-APR-2022  GET_MOD_RES_RMS  v1.0 (c) L. Petrov 07-APR-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  REG_MOD*(*)
      REAL*8     APR_IONO_RMS
      REAL*8     GET_MOD_RES_RMS 
      INTEGER*4  DEG, NK
      PARAMETER  ( DEG = 3 )
      PARAMETER  ( NK  = 4 )
!
!  Using /vlbi/solutions/rfc_dr1/est_iono_mod_850.stat, 
!        /vlbi/solutions/rfc_dr1/vlba_24.ses
!       MODE = 'res_mpf'
!
!  Run on 2022.10.03_12:02:02
! 
      REAL*8  ARG_NOD(NK), SPL(1-DEG:NK-1), REG_SH, REG_DR
!@      DATA    ARG_NOD /               &
!@     &                   0.00000D-12, &
!@     &                  35.00000D-12, &
!@     &                 120.00000D-12, &
!@     &                1300.00000D-12  &
!@     &                      /
!@!
!@      DATA    SPL     /                    &
!@     &                        6.3D-12, &
!@     &                       14.8D-12, &
!@     &                       23.5D-12, &
!@     &                      114.0D-12, &
!@     &                      114.0D-12, &
!@     &                      114.0D-12  &
!@     &                /
!@      REAL*8  ARG_NOD(NK), SPL(1-DEG:NK-1)
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
!
      PARAMETER  ( REG_SH = 7.1D-12 )
      PARAMETER  ( REG_DR = 0.434D0 )
      REAL*8, EXTERNAL :: EBSPL_VAL_R8 
      IF ( REG_MOD == 'spl' ) THEN
           GET_MOD_RES_RMS = EBSPL_VAL_R8 ( NK, DEG, APR_IONO_RMS, ARG_NOD, SPL )
         ELSE IF ( REG_MOD == 'lin' ) THEN
           GET_MOD_RES_RMS = REG_SH + REG_DR*APR_IONO_RMS
      END IF
      RETURN
      END  FUNCTION  GET_MOD_RES_RMS  !#!#
