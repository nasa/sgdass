!
! >>>>> INCLUDE-BLOCK with descriptions of data structure used to faciliate
!       handling NCORT, SOCAL and ATMPART. It keeps a lot of parameters which
!       are used only for excnage NCORT, SOCAL and ATMPART
!
!       include block solve.i SHOULD BE declared before!
!
!       ncrec.i  12-SEP-97  v1.1  (c)  L. Petrov  --  17-NOV-99 14:08:52
!
      TYPE      NCREC__STRU
          INTEGER*2   JSITN(4,MAX_ARC_STA)
          INTEGER*4   JCAPPL(MAX_ARC_STA)
          INTEGER*2   JCAFFL(7,MAX_ARC_STA)
          INTEGER*2   JSITI(MAX_ARC_STA)
          INTEGER*4   ITT(MAX_ARC_STA)
          INTEGER*2   ITTB(MAX_ARC_BSL)
          REAL*8      ET(2,MAX_ARC_BSL)
	  REAL*8      SE(MAX_ARC_STA)
          REAL*8      SS(MAX_ARC_SRC)
          REAL*8      LATS(MAX_ARC_STA)
          REAL*8      HEIGHTS(MAX_ARC_STA)
          INTEGER*2   OBCAPL
          INTEGER*2   MCAPL
          INTEGER*4   JCAVAL(MAX_ARC_STA)
          INTEGER*4   AX_TYPES(MAX_ARC_STA)
          INTEGER*2   NFCAL
          INTEGER*4   NAMSTA
          CHARACTER   FCAL_NAMES(MAX_ARC_STA)*8
          REAL*8      AX_OFFS(MAX_ARC_STA)
          REAL*8      BARO_CALS(MAX_ARC_STA)
          REAL*8      BARO_HEIGHTS(MAX_ARC_STA)
          REAL*8      AVG_ATM(4,MAX_ARC_STA)
      END TYPE  NCREC__STRU   ! NCREC__STRU /
!
! <<<<< end of INCLUDE-BLOCK  ncrec.i
!
