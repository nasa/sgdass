!@This is the start of file &PRFIL (256 blocks)
!
! Note: If any changes are made here, changes may be necessary in
!       ../sdbh/parblk.f.
!
! 256-block PARFIL
!
      INTEGER*2    MAX_SRC_1074
      INTEGER*4   JPARFIL_BLOCKS_1074, JPARFIL_WORDS_1074
      PARAMETER  ( MAX_SRC_1074  = 4096 )
      PARAMETER  ( JPARFIL_BLOCKS_1074 = 1074, &
     &             JPARFIL_WORDS_1074  = JPARFIL_BLOCKS_1074*BLOCK_WORDS )
      REAL*8 &
     &       VAXOF_1074(MAX_STA), &
     &       VSITEC_1074(3,MAX_STA), &
     &       VSTARC_1074(2,MAX_SRC_1074), &
     &       VATM_1074, &
     &       VREL_1074, &
     &       VTIDE_1074(3), &
     &       VPREC_1074, &
     &       VNUT_1074(2,6), &
     &       VSITEV_1074(3,MAX_STA), &
     &       VNUTOP_1074(2,6)
!
      REAL*8 &
     &       BARO_CAL_1074(MAX_ARC_STA), &
     &       BARO_HEIGHT_1074(MAX_ARC_STA)
!
      REAL*8       STA_FJD_BEG_1074(MAX_STA), &
     &             STA_FJD_END_1074(MAX_STA), &
     &             STA_FJD_MID_1074(MAX_STA), &
     &             SRC_FJD_BEG_1074(MAX_SRC_1074), &
     &             SRC_FJD_END_1074(MAX_SRC_1074), &
     &             SRC_FJD_MID_1074(MAX_SRC_1074)
      REAL*8       GLO_FJDOBS_MIN_1074, GLO_FJDOBS_MAX_1074
!
      INTEGER*4    NSES_STA_1074(MAX_STA), &
     &             NSES_SRC_1074(MAX_SRC_1074)
!
      INTEGER*2 &
     &       ISTRN_1074(4,MAX_SRC_1074), &
     &       ISITN_1074(4,MAX_STA), &
     &       MONUMENTS_1074(5,MAX_STA), &
     &       NUMSEL_1074, &
     &       IDBPSL_1074(6,MAX_DBS), &
     &       PWCNUMEP_1074
!
      CHARACTER  PWCFNAME_1074*16
      REAL*8 &
     &       VSITED_1074(MAX_STA), &
     &       PSITED_1074(MAX_STA)
      INTEGER*2 &
     &       PWCSIZEP_1074, &
     &       IZFREE_1074(35)
      CHARACTER    ISITN_CHR_1074(MAX_STA)*8, &
     &             ISTRN_CHR_1074(MAX_SRC_1074)*8, &
     &             LMONUMENTS_1074(MAX_STA)*10, &
     &             MONUMENTS_CHR_1074(MAX_STA)*10
!	
      INTEGER*2 &
     &       IPARFIL_1074(JPARFIL_WORDS_1074)
!
      COMMON / PARFL / &
!
! --- REAL*8
!
     &       VAXOF_1074, &
     &       VSITEC_1074, &
     &       VSTARC_1074, &
     &       VATM_1074, &
     &       VREL_1074, &
     &       VTIDE_1074, &
     &       VPREC_1074, &
     &       VNUT_1074, &
     &       VSITEV_1074, &
     &       VNUTOP_1074, &
     &       BARO_CAL_1074, &
     &       BARO_HEIGHT_1074, &
!
! --- INTEGER*2
!
     &       ISTRN_1074, &
     &       ISITN_1074, &
     &       MONUMENTS_1074, &
     &       NUMSEL_1074, &
     &       IDBPSL_1074, &
!
! ---- REAL*8
!
     &       VSITED_1074, &
     &       PSITED_1074, &
     &       PWCNUMEP_1074, &
     &       PWCFNAME_1074, &
!
!     again REAL*8
!
     &       STA_FJD_BEG_1074, &
     &       STA_FJD_END_1074, &
     &       STA_FJD_MID_1074, &
     &       SRC_FJD_BEG_1074, &
     &       SRC_FJD_END_1074, &
     &       SRC_FJD_MID_1074, &
     &       GLO_FJDOBS_MIN_1074, GLO_FJDOBS_MAX_1074, &
!
!     again INTEGER*4
!
     &       NSES_STA_1074, &
     &       NSES_SRC_1074, &
!
! ---- integer*2
!
     &       PWCSIZEP_1074, &
     &       IZFREE_1074
!
      EQUIVALENCE ( IPARFIL_1074, VAXOF_1074 )
