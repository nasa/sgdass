!@This is the start of file &GLBC3
!
!   modifications:
!
!   kdb 950720 Change declaration of velohoriz from logical*2 to integer*2
!              to accomodate splitting of velocity_origin horizontal keyword
!              into horizontal and vertical choices.
!   kdb 961125 Accomodate new batch suppression options,
!              no net rotation sources and
!              no net rotation and translation positions and velocities
!   kdb 970312 Implement the ability to use weighted or uniform no_net
!              constraints via the batopt file, using JMG's algorithm.
!              Also implement in batopt the ability to use the horizontal
!              projection matrix vs. just the identity matrix for the
!              no net constraints, using JMG's algorithm.
!   pet 970929 Renamed IFILL to IFILL_GLBC3 to avoid intereference with oborg.i
!   pet 2000.08.01  Added variables BEGMARK_GLBC3_I2, ENDMARK_GLBC3_I2
!                   and constant LEN_GLBC3_FILL_I2
!   pet 2002.03.13  Added variables NNT_POS_GLO, NNT_POS_LOC, NNR_POS_GLO,
!                   NNR_POS_LOC, NNR_SRC_GLO, NNR_SRC_LOC -- they keep flags
!                   whether constraints should be imposed on local or global
!                   paramters or even both
!   pet 2002.09.23  Added variables DTOSTA_CHR
!   pet 2003.09.01  Changed dimension of from SOUSUP(SRC_BIT_WORDS,7) to
!                   SOUSUP(SRC_BIT_WORDS,8) 
!   pet 2007.07.26  Added variables with suffix META
!
!   Flags and their meanings:
!
      INTEGER*2 LEN_GLBC3_FILL_I2, BEGMARK_GLBC3_I2, ENDMARK_GLBC3_I2
      PARAMETER ( LEN_GLBC3_FILL_I2 = 239 )     ! Length of unused space in
      INTEGER*2 IFILL_GLBC3(LEN_GLBC3_FILL_I2) ! Integer*2 words
!
      LOGICAL*2 PRESUP, RELSUP, TIDSUP
      INTEGER*2 VELOHORIZ, NUTSUP(116), DEFVEL, DEFCMP, DEFSRC
      INTEGER*2 VELSUP(STA_BIT_WORDS,6), CMPSUP(STA_BIT_WORDS,9)
      INTEGER*2 ISTASP, IND_NNT_POS(2), IND_NNR_POS(2), IND_NNT_VEL(2), IND_NNR_VEL(2)
      INTEGER*2 DATSTA(4), DTOSTA(4)
      INTEGER*2 SOUSUP(SRC_BIT_WORDS,9)
      INTEGER*2 ISRCSP, IND_NNR_SRC(2), IND_NNR_PRP(2), IND_SOU_ADM(2)
      INTEGER*2 NUMVELGRP, VELTIES(MAX_STA)
      INTEGER*2 NUMSTAGRP, STATIES(MAX_STA)
      LOGICAL*2 NNT_POS_GLO, NNT_POS_LOC, NNR_POS_GLO, NNR_POS_LOC
      LOGICAL*2 NNR_SRC_GLO, NNR_SRC_LOC
      CHARACTER FIXED_PLATE*4
      INTEGER*4 N_RWT_STA, N_RWT_SRC
      REAL*8    NUVEL_WT
      CHARACTER*2 KMATRIX_NNTP, KMATRIX_NNTV, &
     &            KSIG_SCALE_NNTP, KSIG_SCALE_NNTV, &
     &            KSIG_SCALE_NNRP, KSIG_SCALE_NNRV, KSIG_SCALE_NNRS, &
     &            KSIG_SCALE_NNRQ
      REAL*8     RWT_EL_STA(MAX_STA), RWT_SRC(MAX_SRC)
!
      CHARACTER  STASUP(MAX_STA)*8
      CHARACTER  SRCSUP(MAX_SRC)*8
      INTEGER*2  STASUP_I2(4,MAX_STA), SRCSUP_I2(4,MAX_SRC)
      CHARACTER  DTOSTA_CHR*8, DATSTA_CHR*8
      EQUIVALENCE ( STASUP_I2, STASUP  )
      EQUIVALENCE ( SRCSUP_I2, SRCSUP  )
      EQUIVALENCE ( DTOSTA, DTOSTA_CHR )
      EQUIVALENCE ( DATSTA, DATSTA_CHR )
!
! --- In fact this staff should go to socom, but this would have requireed
! --- to regenerate of superfiles. This is ugly
!
      REAL*8   META_CLO_INTR(0:SLV__MAX_SOLTYP-1),             & 
     &         META_ATM_INTR(0:SLV__MAX_SOLTYP-1),             &
     &         META_TIL_INTR(0:SLV__MAX_SOLTYP-1),             &
     &         META_EOP_CNS(11,0:SLV__MAX_SOLTYP-1),           &
     &         META_CLO_CNS(0:SLV__MAX_SOLTYP-1,MAX_ARC_STA),  &
     &         META_ATM_CNS(0:SLV__MAX_SOLTYP-1,MAX_ARC_STA),  &
     &         META_TLOF_CNS(0:SLV__MAX_SOLTYP-1,MAX_ARC_STA), &
     &         META_TLRT_CNS(0:SLV__MAX_SOLTYP-1,MAX_ARC_STA), &
     &         META_STPS_CNS(0:SLV__MAX_SOLTYP-1,MAX_ARC_STA), &
     &         META_SOCO_CNS(0:SLV__MAX_SOLTYP-1,MAX_ARC_SRC), &
     &         META_BSCL_CNS(0:SLV__MAX_SOLTYP-1,MAX_ARC_BSL), &
     &         META_RW_DEL(0:SLV__MAX_SOLTYP-1,MAX_ARC_BSL),   &
     &         META_RW_RAT(0:SLV__MAX_SOLTYP-1,MAX_ARC_BSL)
      INTEGER*2   META_DGCL_EST(0:SLV__MAX_SOLTYP-1,MAX_ARC_STA)
      CHARACTER   META_RW_BAS(2,MAX_ARC_BSL)*8
      INTEGER*4   META_N_BAS, META_N_STA, META_N_SOU
!
      COMMON / GLBC3 / &
     &         BEGMARK_GLBC3_I2, &
     &         PRESUP, RELSUP, TIDSUP, NUTSUP, DEFVEL, VELSUP, CMPSUP, &
     &         ISTASP, STASUP, &
     &         DATSTA, DTOSTA, SOUSUP, ISRCSP, SRCSUP, &
     &         DEFCMP, DEFSRC, NUMVELGRP, VELTIES, VELOHORIZ, &
     &         NUMSTAGRP, STATIES, FIXED_PLATE, NUVEL_WT, &
     &         KMATRIX_NNTP, KMATRIX_NNTV, &
     &         KSIG_SCALE_NNTP, KSIG_SCALE_NNTV, &
     &         KSIG_SCALE_NNRP, KSIG_SCALE_NNRV, KSIG_SCALE_NNRS, &
     &         KSIG_SCALE_NNRQ, NNT_POS_GLO, NNT_POS_LOC, NNR_POS_GLO, &
     &         NNR_POS_LOC, NNR_SRC_GLO, NNR_SRC_LOC, &
     &         IND_NNT_POS, IND_NNR_POS, IND_NNT_VEL, IND_NNR_VEL, &
     &         IND_NNR_SRC, IND_NNR_PRP, &
     &         META_CLO_INTR, & 
     &         META_ATM_INTR, &
     &         META_TIL_INTR, &
     &         META_EOP_CNS,  &
     &         META_CLO_CNS,  &
     &         META_ATM_CNS,  &
     &         META_TLOF_CNS, &
     &         META_TLRT_CNS, &
     &         META_STPS_CNS, &
     &         META_SOCO_CNS, &
     &         META_BSCL_CNS, &
     &         META_RW_DEL,   &
     &         META_RW_RAT,   &
     &         META_RW_BAS,   &
     &         META_DGCL_EST, &
     &         META_N_BAS,    &
     &         META_N_STA,    &
     &         META_N_SOU,    &
     &         IND_SOU_ADM,   &
     &         N_RWT_STA,     &
     &         N_RWT_SRC,     &
     &         RWT_EL_STA,    &
     &         RWT_SRC,       &
     &         IFILL_GLBC3,   &
     &         ENDMARK_GLBC3_I2
