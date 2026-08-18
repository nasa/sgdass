!
! This file is generated automatically by use_local.f from
!      template_file: /g1/progs/petools_20160831/include/matvec.templ
!      local customization file: /g1/progs/petools_20160831/support/petools_intel_core_i7.lcl
!
!      PLEASE DON'T EDIT THIS FILE!
!      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
! >>>>> INCLUDE-BLOCK with definitions for the optimal performance of
!       matvec library tuned for Celeron 800MHz processor
!       Parameters defined below control work of the optimized algorithms
!
!       matvec.i
!
!       WHO  WHEN         WHAT
!       pet  2001.04.11   creation
!       pet  2001.04.16   transformed to the template form
!       pet  2002.09.02   Added DB1__MUL_MM_SV_V
!       pet  2002.09.21   Added DB3__MUL_MM_SI_I, DB3__MUL_MM_IS_I
!       pet  2002.09.28   Added DB3__MUL_MM_ST_I, DB3__MUL_MM_TS_I
!       pet  2002.10.05   Added DB3__MUL_MM_SS_I
!       pet  2002.10.12   Added DB3__MUL_MM_SS_S, DB3__MUL_MM_II_S,
!                               DB3__MUL_MM_IT_S, DB3__MUL_MM_IT_S
!       pet  2002.10.19   Added MFLOP__PEAK
!       pet  2002.11.30   Added DB__INVMAT_MIN, DB__INVMAT_MAX,
!                               DB__INVMAT_DIR, DB__INVMAT_EPS
!       pet  2003.11.11   Added Increased DB__INVMAT_MAX from 24 to 32
!
!
      INTEGER*4   MAX__DIM
      INTEGER*4   DB__DP_VV_V,      DB__ADD_RCT,      &
     &            DB__MUL_MM_II_I,  DB__MUL_MM_IT_I,  &
     &            DB__MUL_MM_TI_I,  DB__MUL_MM_TT_I,  &
     &            DB1__MUL_MM_II_S, DB2__MUL_MM_II_S, DB3__MUL_MM_II_S, &
     &            DB1__MUL_MM_IT_S, DB2__MUL_MM_IT_S, DB3__MUL_MM_IT_S, &
     &            DB1__MUL_MM_TI_S, DB2__MUL_MM_TI_S, DB3__MUL_MM_TI_S, &
     &            DB1__MUL_MM_SI_I, DB2__MUL_MM_SI_I, DB3__MUL_MM_SI_I, &
     &            DB1__MUL_MM_ST_I, DB2__MUL_MM_ST_I, DB3__MUL_MM_ST_I, &
     &            DB1__MUL_MM_IS_I, DB2__MUL_MM_IS_I, DB3__MUL_MM_IS_I, &
     &            DB1__MUL_MM_TS_I, DB2__MUL_MM_TS_I, DB3__MUL_MM_TS_I, &
     &            DB1__MUL_MM_SS_I, DB2__MUL_MM_SS_I, DB3__MUL_MM_SS_I, &
     &            DB1__MUL_MM_SS_S, DB2__MUL_MM_SS_S, DB3__MUL_MM_SS_S, &
     &            DB1__MUL_MV_SV_V,                                     &
     &            DB__INVMAT_MIN,   DB__INVMAT_MAX,   DB__INVMAT_DIR,   &
     &            DB__DPPSL
      REAL*8    MFLOP__PEAK
      REAL*8    RC_REF, MIN_VAL
      PARAMETER  ( RC_REF  = 1.D16  )    ! Reference conditional number
      PARAMETER  ( MIN_VAL = 1.D-30 )    ! Minimum allowed value
!
      PARAMETER ( MAX__DIM = 128*1024 )  ! maximum matrix dimension
!
      REAL*8      DB__INVMAT_EPS
      PARAMETER ( DB__INVMAT_EPS = 1.0D-15 ) ! Local customization
!
      REAL*8      COND__MAX
      PARAMETER ( COND__MAX = 1.0D11 ) ! Local customization
!
      PARAMETER ( DB__DP_VV_V = 32 ) ! Local customization
      PARAMETER ( DB__ADD_RCT = 64 ) ! Local customization
      PARAMETER ( DB__MUL_MM_II_I = 5 ) ! Local customization
      PARAMETER ( DB__MUL_MM_IT_I = 5 ) ! Local customization
      PARAMETER ( DB__MUL_MM_TI_I = 5 ) ! Local customization
      PARAMETER ( DB__MUL_MM_TT_I = 5 ) ! Local customization
!
      PARAMETER ( DB1__MUL_MM_II_S = 36 ) ! Local customization
      PARAMETER ( DB2__MUL_MM_II_S = 768 ) ! Local customization
      PARAMETER ( DB3__MUL_MM_II_S = 512 ) ! Local customization
!
      PARAMETER ( DB1__MUL_MM_IT_S = 7 ) ! Local customization
      PARAMETER ( DB2__MUL_MM_IT_S = 768 ) ! Local customization
      PARAMETER ( DB3__MUL_MM_IT_S = 512 ) ! Local customization
!
      PARAMETER ( DB1__MUL_MM_TI_S = 35 ) ! Local customization
      PARAMETER ( DB2__MUL_MM_TI_S = 512 ) ! Local customization
      PARAMETER ( DB3__MUL_MM_TI_S = 512 ) ! Local customization
!
      PARAMETER ( DB1__MUL_MM_SI_I = 8 ) ! Local customization
      PARAMETER ( DB2__MUL_MM_SI_I = 2048 ) ! Local customization
      PARAMETER ( DB3__MUL_MM_SI_I = 2048 ) ! Local customization
!
      PARAMETER ( DB1__MUL_MM_ST_I = 14 ) ! Local customization
      PARAMETER ( DB2__MUL_MM_ST_I = 2048 ) ! Local customization
      PARAMETER ( DB3__MUL_MM_ST_I = 2048 ) ! Local customization
!
      PARAMETER ( DB1__MUL_MM_IS_I = 32 ) ! Local customization
      PARAMETER ( DB2__MUL_MM_IS_I = 2048 ) ! Local customization
      PARAMETER ( DB3__MUL_MM_IS_I = 2048 ) ! Local customization
!
      PARAMETER ( DB1__MUL_MM_TS_I = 32 ) ! Local customization
      PARAMETER ( DB2__MUL_MM_TS_I = 2048 ) ! Local customization
      PARAMETER ( DB3__MUL_MM_TS_I = 2048 ) ! Local customization
!
      PARAMETER ( DB1__MUL_MM_SS_I = 16 ) ! Local customization
      PARAMETER ( DB2__MUL_MM_SS_I = 2048 ) ! Local customization
      PARAMETER ( DB3__MUL_MM_SS_I = 2048 ) ! Local customization
!
      PARAMETER ( DB1__MUL_MM_SS_S = 60 ) ! Local customization
      PARAMETER ( DB2__MUL_MM_SS_S = 384 ) ! Local customization
      PARAMETER ( DB3__MUL_MM_SS_S = 384 ) ! Local customization
!
      PARAMETER ( DB1__MUL_MV_SV_V = 32 ) ! Local customization
!
      PARAMETER ( DB__INVMAT_MAX = 32 )
      PARAMETER ( DB__INVMAT_DIR = 32 ) ! Local customization
      PARAMETER ( DB__INVMAT_MIN = 4 ) ! Local customization
!
      PARAMETER ( DB__DPPSL = 64 ) ! Local customization
!
      PARAMETER ( MFLOP__PEAK = 8000.0 ) ! Local customization
