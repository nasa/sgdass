!@This is the start of file &WEIEM
      integer*4 ARRSIZE
      PARAMETER (ARRSIZE=MAX_OBS*20)
      INTEGER*2 IRSFL(ARRSIZE),IBL(8,max_arc_bsl,max_dbs)
      REAL*8 ET(4,max_arc_bsl,max_dbs)
      COMMON /WEIEM/ IRSFL,IBL,ET
