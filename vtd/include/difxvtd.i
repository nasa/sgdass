! >>>>> Include block for difxvtd 
! >>>>> 2021.04.07  (c)  L. Petrov  v 1.00  2025.06.29_17:05:00
!
      INTEGER*4  DFI__MFRQ, DFI__MSTA, DFI__MSOU, DFI__MEOP, DFI__MSCA, DFI__MPOL
      PARAMETER  ( DFI__MFRQ =   64 )
      PARAMETER  ( DFI__MSTA =  256 )
      PARAMETER  ( DFI__MSOU = 1024 )
      PARAMETER  ( DFI__MEOP =  128 )
      PARAMETER  ( DFI__MSCA =  512 )
      PARAMETER  ( DFI__MPOL =    6 )
      TYPE  DFI__TYPE
            CHARACTER  CALC_FIL*128
            CHARACTER  IM_FILE*128
            INTEGER*4  INPUT_MJD_START
            INTEGER*4  CALC_MJD_START
            REAL*8     INPUT_UTC_START
            REAL*8     CALC_UTC_START
            REAL*8     DUR
            INTEGER*4  IVRB
            INTEGER*4  N_FRQ
            INTEGER*4  N_STA
            INTEGER*4  N_SOU
            INTEGER*4  N_SCA
            REAL*8     FRQ(DFI__MFRQ)
            REAL*8     BW(DFI__MFRQ)
            INTEGER*4  SUB_BAND(DFI__MFRQ)
            CHARACTER  SHR_STA_NAM(DFI__MSTA)*2
            CHARACTER  IVS_STA_NAM(DFI__MSTA)*8
            CHARACTER  SOU_NAM(DFI__MSOU)*16
            CHARACTER  IVS_SOU_NAM(DFI__MSOU)*8
!
            CHARACTER  SCAN_ID(DFI__MSCA)*16
            INTEGER*4  SCAN_IND_SOU(DFI__MSCA)
            REAL*8     SCAN_START_OFFSET(DFI__MSCA)
            REAL*8     SCAN_DUR(DFI__MSCA)
!
            INTEGER*4  N_CLP(DFI__MSTA)
            INTEGER*4  MJD_CLO_REF(DFI__MSTA)
            REAL*8     UTC_CLO_REF(DFI__MSTA)
!          
            REAL*8     CLO_POL(0:DFI__MPOL,DFI__MSTA)
            CHARACTER  MOUNT(DFI__MSTA)*8
            REAL*8     AXIS_OFF(DFI__MSTA)
            REAL*8     STA_POS(3,DFI__MSTA)
            REAL*8     SOU_COO(2,DFI__MSOU)
            REAL*8     UTC_MTAI
            INTEGER*4  N_EOP
            INTEGER*4  MJD_EOP(DFI__MEOP)
            INTEGER*4  UTC_EOP(DFI__MEOP)
            REAL*8     EOP(3,DFI__MEOP)
!
            INTEGER*4  NSTA_TAB
            INTEGER*4  NSOU_TAB
            CHARACTER  FIL_STA_TAB*128
            CHARACTER  FIL_SOU_TAB*128
            CHARACTER  STA_TABLE(DFI__MSTA,2)*8
            CHARACTER  SOU_TABLE(DFI__MSOU,2)*10
      END   TYPE  DFI__TYPE
      INTEGER*4    DFI__LSB, DFI__USB
      PARAMETER  ( DFI__LSB = 1 )
      PARAMETER  ( DFI__USB = 2 )
      REAL*8       DFI__DUR_POL
      INTEGER*4    DFI__DEG_POL
      PARAMETER  ( DFI__DUR_POL = 120.0D0 )
      PARAMETER  ( DFI__DEG_POL =   5     )
      INTEGER*4  DFI__EMBED, DFI__EXTERNAL
      PARAMETER  ( DFI__EMBED    = 78032545 )
      PARAMETER  ( DFI__EXTERNAL = 20389195 )
!
      CHARACTER    DFI__STANAM__LABEL*46, DFI__SOUNAM__LABEL*32, DFI__SOUNAM__LABEL_01*32
      PARAMETER  ( DFI__STANAM__LABEL    = '# STATION-NAMES.  Format version of 2006.01.06' )
      PARAMETER  ( DFI__SOUNAM__LABEL    = '# SOURCE-NAMES  v 2.0 2005.09.06' )
      PARAMETER  ( DFI__SOUNAM__LABEL_01 = '# SOURCE-TABLE  v 1.0 2025.06.29' )
!
! >>>>> End of include block for package difx2vtd
!
