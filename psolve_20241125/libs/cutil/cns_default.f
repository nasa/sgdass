      SUBROUTINE CNS_DEFAULT()
! ************************************************************************
! *                                                                      *
! *   Routine  CNS_DEFAULT  sets sigmas of constraints in glbc4.i in     *
! *   according with default values kept as named constants in solve.i   *
! *                                                                      *
! *   NB: It is assumed that glbc4.i has been read already. glbc4.i is   *
! *       not written in disk by cns_default.                            *
! *                                                                      *
! *  ###  22-JUL-98   CNS_DEFAULT  v1.2  (c) L. Petrov  02-SEP-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
!
      LIN_STA_SIGMA = LIN_STA__SIG__DEF ! Linear combination of stat. pos
      BAS_CLK_SIGMA = BAS_CLK__SIG__DEF ! Baseline-dependent clocks
      SRC_COO_SIGMA = SRC_COO__SIG__DEF ! Source coordinates
      NNT_POS_SIGMA = NNT_POS__SIG__DEF ! No-net translation for st. pos.
      NNR_POS_SIGMA = NNR_POS__SIG__DEF ! No-net rotation station posit.
      NNT_VEL_SIGMA = NNT_VEL__SIG__DEF ! No-net translation velocities
      NNR_VEL_SIGMA = NNR_VEL__SIG__DEF ! No-net rotation velocities
      STA_WEA_SIGMA = STA_WEA__SIG__DEF ! Station positions
      VEL_WEA_SIGMA = VEL_WEA__SIG__DEF ! Velocities
      VEL_DIR_SIGMA = VEL_DIR__SIG__DEF ! Velocity direction
      VEL_CMP_SIGMA = VEL_CMP__SIG__DEF ! Velocity components
      STA_ORG_SIGMA = STA_ORG__SIG__DEF ! Station positions origin
      VEL_ORG_SIGMA = VEL_ORG__SIG__DEF ! Velocity origin
      VEL_SET_SIGMA = VEL_SET__SIG__DEF ! Set of velicities
      STA_TIE_SIGMA = STA_TIE__SIG__DEF ! Station ties
      VEL_TIE_SIGMA = VEL_TIE__SIG__DEF ! Velocity ties
      RAS_ORG_SIGMA = RAS_ORG__SIG__DEF ! Right ascension origin
      DCL_ORG_SIGMA = DCL_ORG__SIG__DEF ! Declination origin
      NNR_SRC_SIGMA = NNR_SRC__SIG__DEF ! No-net rotation for sources
      NNR_PRP_SIGMA = NNR_PRP__SIG__DEF ! No-net rotation for proper motion
      NUT_CMP_SIGMA = NUT_CMP__SIG__DEF ! Nutation components
      VEL_VER_SIGMA = VEL_VER__SIG__DEF ! Vertical velocity
!
      RETURN
      END  !#!  CNS_DEFAULT  #!#
