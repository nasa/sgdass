      SUBROUTINE FLYBY_MAP_INIT()
! ************************************************************************
! *                                                                      *
! *   Routine  FLYBY_MAP_INIT  sets status "not initialized" for flags   *
! *   used by FLYBY_MAP.  It should be ALWAYS called before the          *
! *   processing the first observation of the session!                   *
! *                                                                      *
! *  ###  08-JAN-99  FLYBY_MAP_INIT  v1.3  (c) L. Petrov  22-APR-99 ###  *
! *                                                                      *
! *  2002.12.26  pet  Added initialization of XYZ_PSV array.             *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'bindisp.i'
      INCLUDE   'flyby.i'
      INTEGER*2 J1
!
      UC_INITIALIZED  = .FALSE.
      UF_INITIALIZED  = .FALSE.
      XC_INITIALIZED  = .FALSE.
      XF_INITIALIZED  = .FALSE.
      YC_INITIALIZED  = .FALSE.
      YF_INITIALIZED  = .FALSE.
!
      UT1_INITIALIZED = .FALSE.
      XW_INITIALIZED  = .FALSE.
      YW_INITIALIZED  = .FALSE.
!
      UT1_ZINITIALIZED= .FALSE.
      WX_ZINITIALIZED = .FALSE.
      WY_ZINITIALIZED = .FALSE.
!
      DO 410 J1=1,MAX_ARC_STA
         METEO_WARN(J1) = .FALSE.
!
         CAL_INIT(J1)   = .FALSE.
         PRT_INIT(J1)   = .FALSE.
!
         TIME_CAL_OLD(J1)  = -1.D11
         TIME_PRT_OLD(J1)  = -1.D11
!
         ISTAR_CAL_OLD(J1) = -11111
         ISTAR_PRT_OLD(J1) = -11111
!
         XYZ_PSV(1,J1) = 0.0D0
         XYZ_PSV(2,J1) = 0.0D0
         XYZ_PSV(3,J1) = 0.0D0
!
         XYZ_BSP(1,J1) = 0.0D0
         XYZ_BSP(2,J1) = 0.0D0
         XYZ_BSP(3,J1) = 0.0D0
!
         SCA_LAST(J1) = -1
 410  CONTINUE
!
      IO = 0
      MAX_F_SAVED = MAX_F
!
      RETURN
      END  !#!  FLYBY_MAP_INIT  #!#
