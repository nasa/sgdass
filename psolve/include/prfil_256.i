!@This is the start of file &PRFIL (256 blocks)
!
! Note: If any changes are made here, changes may be necessary in
!       ../sdbh/parblk.f.
!
! 256-block PARFIL
!
      INTEGER*2    MAX_SRC_256
      INTEGER*4   JPARFIL_BLOCKS_256, JPARFIL_WORDS_256
      PARAMETER  ( MAX_SRC_256  = 768 )
      PARAMETER  ( JPARFIL_BLOCKS_256 = 256, &
     &             JPARFIL_WORDS_256  = JPARFIL_BLOCKS_256*BLOCK_WORDS )
      REAL*8 &
     &       VAXOF_256(MAX_STA), &
     &       VSITEC_256(3,MAX_STA), &
     &       VSTARC_256(2,MAX_SRC_256), &
     &       VATM_256, &
     &       VREL_256, &
     &       VTIDE_256(3), &
     &       VPREC_256, &
     &       VNUT_256(2,6), &
     &       VSITEV_256(3,MAX_STA), &
     &       VNUTOP_256(2,6)
!
      REAL*8 &
     &       BARO_CAL_256(MAX_ARC_STA), &
     &       BARO_HEIGHT_256(MAX_ARC_STA)
!
      INTEGER*2 &
     &       ISTRN_256(4,MAX_SRC_256), &
     &       ISITN_256(4,MAX_STA), &
     &       MONUMENTS_256(5,MAX_STA), &
     &       NUMSEL_256, &
     &       IDBPSL_256(6,MAX_DBS), &
     &       PWCNUMEP_256
!
      CHARACTER  PWCFNAME_256*16
      REAL*8 &
     &       VSITED_256(MAX_STA), &
     &       PSITED_256(MAX_STA)
      INTEGER*2 &
     &       PWCSIZEP_256, &
     &       IZFREE_256(35)
!
      INTEGER*2 &
     &       IPARFIL_256(JPARFIL_WORDS_256)
!
      COMMON / PARFL / &
!
! --- REAL*8
!
     &       VAXOF_256, &
     &       VSITEC_256, &
     &       VSTARC_256, &
     &       VATM_256, &
     &       VREL_256, &
     &       VTIDE_256, &
     &       VPREC_256, &
     &       VNUT_256, &
     &       VSITEV_256, &
     &       VNUTOP_256, &
     &       BARO_CAL_256, &
     &       BARO_HEIGHT_256, &
!
! --- INTEGER*2
!
     &       ISTRN_256, &
     &       ISITN_256, &
     &       MONUMENTS_256, &
     &       NUMSEL_256, &
     &       IDBPSL_256, &
!
! ---- REAL*8
!
     &       VSITED_256, &
     &       PSITED_256, &
     &       PWCNUMEP_256, &
     &       PWCFNAME_256, &
!
! ---- integer*2
!
     &       PWCSIZEP_256, &
     &       IZFREE_256
!
      EQUIVALENCE ( IPARFIL_256, VAXOF_256 )
