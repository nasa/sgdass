!
! >>>>> Include block for package IONO_VMOD v 1.4  2016.12.29_15:31:51
! >>>>> For computation VLBI tuned ionospheric path delay model
!
      CHARACTER  IVM__LABEL*38, IVM__FMT*56
      PARAMETER  ( IVM__LABEL = 'IONO_VMOD  Version  1.4  of 2016.12.29' )
      PARAMETER  ( IVM__FMT   = '# External iono path delay. Format version of 2016.12.29' )
!
        INTEGER*4    IVM__MOBS, IVM__MSOU, IVM__MSTA, IVM__MBAS, IVM__MPAR, IVM__U, IVM__L
        PARAMETER  ( IVM__MOBS = 64*1024 )
        PARAMETER  ( IVM__MPAR = 64*1024 )
        PARAMETER  ( IVM__MSOU = 1024    )
        PARAMETER  ( IVM__MSTA = 32      )
        PARAMETER  ( IVM__MBAS = (IVM__MSTA*(IVM__MSTA+1))/2 )
        PARAMETER  ( IVM__U    = 1       )
        PARAMETER  ( IVM__L    = 2       )
!
        REAL*8       UVM__TIM_MRG, REWEI__IVM, NSIG__IVM
        PARAMETER  ( UVM__TIM_MRG = 1000.0D0 )
        PARAMETER  ( REWEI__IVM = 20.0D-12 )
        PARAMETER  ( NSIG__IVM  = 3.5D0 )
!
      	TYPE      IONOOB__STRU
            CHARACTER  SOU_NAM*8
            CHARACTER  STA_NAM(2)*8
            INTEGER*4  MJD
            LOGICAL*1  FL_GX
            LOGICAL*1  FL_GS
            LOGICAL*1  FL_GXS
            LOGICAL*1  FILLER
            REAL*8     UTC
            REAL*8     TAI
            REAL*8     TAU(2)
            REAL*8     TAU_GC(2)
            REAL*8     TAU_ERR(2)
            REAL*8     TAU_REWEI
            REAL*8     FRQ(2)
            REAL*8     IONO_MAP(2)
            REAL*8     TAU_IONOG(2)
            REAL*8     TAU_IONOV(2)
            REAL*8     TAU_IONOV_ERR(2)
            REAL*8     TAU_IONOM(2)
            REAL*8     TAU_IONOM_EXC(2)
            REAL*8     TAU_IONOM_ERR(2)
        END TYPE  IONOOB__STRU
!
      	TYPE      IONOST__STRU
            INTEGER*4  NB     ! Number of good points, including outliers
            INTEGER*4  NO_VG  ! Number of outliers
            INTEGER*4  NO_VM
            INTEGER*4  NO_VMX

            REAL*8  BIAS_V
            REAL*8  BIAS_VG
            REAL*8  BIAS_VM
            REAL*8  BIAS_VMX
            REAL*8  RMS_V
            REAL*8  RMS_VG
            REAL*8  RMS_VM
            REAL*8  RMS_VMX
            REAL*8  WRMS_V
            REAL*8  WRMS_VG
            REAL*8  WRMS_VM
            REAL*8  WRMS_VMX
      	END TYPE  IONOST__STRU
!
      	TYPE      IONOV__STRU
            CHARACTER  DB_NAME*10
            CHARACTER  DB_VERS*4
            CHARACTER  FILLER*2
            CHARACTER  FIL_IONO*128
            INTEGER*4  NOBS
            TYPE       ( IONOOB__STRU ), POINTER :: OBS(:) => NULL()
            TYPE       ( IONOST__STRU )  :: STAT(IVM__MBAS)
            INTEGER*4  N_BAS
            CHARACTER  C_BAS(IVM__MBAS)*19
            REAL*8     DUR
            INTEGER*4  STATUS
      	END  TYPE IONOV__STRU
        INTEGER*4  IVM__UNDF
        INTEGER*4  IVM__LOAD
        PARAMETER  ( IVM__UNDF = 0     )
        PARAMETER  ( IVM__LOAD = 28302 )
