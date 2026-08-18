!@This is the start of file Q_PRFIL
!
!  This file is clone of PRFIL with Q_ prepended to all the variable
!  names and the common block name.
!
      INTEGER*2    Q_LEN_PRFIL_FILLER_I2 ! Free, unused space in two-byte words
      PARAMETER  ( Q_LEN_PRFIL_FILLER_I2 = 67 )
!
      REAL*8 &
     &             Q_VAXOF(MAX_STA), &
     &             Q_VSITEC(3,MAX_STA), &
     &             Q_VSTARC(2,MAX_SRC), &
     &             Q_VATM, &
     &             Q_VREL, &
     &             Q_VTIDE(3), &
     &             Q_VPREC, &
     &             Q_VNUT(2,6), &
     &             Q_VSITEV(3,MAX_STA), &
     &             Q_VNUTOP(2,6), &
     &             Q_VSITEC_TIM_REF_MJD
!
      REAL*8 &
     &             Q_BARO_CAL(MAX_ARC_STA), &
     &             Q_BARO_HEIGHT(MAX_ARC_STA)
!
      REAL*8       Q_STA_FJD_BEG(MAX_STA), &
     &             Q_STA_FJD_END(MAX_STA), &
     &             Q_STA_FJD_MID(MAX_STA), &
     &             Q_SRC_FJD_BEG(MAX_SRC), &
     &             Q_SRC_FJD_END(MAX_SRC), &
     &             Q_SRC_FJD_MID(MAX_SRC)
      REAL*8       Q_GLO_FJDOBS_MIN, &
     &             Q_GLO_FJDOBS_MAX
!
      INTEGER*4    Q_NSES_STA(MAX_STA), &
     &             Q_NSES_SRC(MAX_SRC)
!
      INTEGER*2 &
     &             Q_ISTRN(4,MAX_SOU), &
     &             Q_ISITN(4,MAX_STA), &
     &             Q_MONUMENTS(5,MAX_STA), &
     &             Q_NUMSEL, &
     &             Q_IDBPSL(6,MAX_DBS), &
     &             Q_PWCNUMEP, &
     &             Q_IFIRST_PRFIL_I2, &
     &             Q_ILAST_PRFIL_I2
      CHARACTER*16 &
     &             Q_PWCFNAME
      REAL*8 &
     &             Q_VSITED(MAX_STA), &
     &             Q_PSITED(MAX_STA)
      INTEGER*2 &
     &             Q_PWCSIZEP, &
     &             Q_IZFREE(LEN_PRFIL_FILLER_I2)
!
      INTEGER*2    Q_IPARFIL(JPARFIL_WORDS), &
     &             Q_IPAR1(20600), &
     &             Q_IPAR2(11878)
      CHARACTER    Q_ISITN_CHR(MAX_STA)*8,   &
     &             Q_ISTRN_CHR(MAX_SOU)*8,   &
     &             Q_JNAME(MAX_SOU)*10,      &
     &             Q_LMONUMENTS(MAX_STA)*10, &
     &             Q_MONUMENTS_CHR(MAX_STA)*10
      EQUIVALENCE  ( Q_ISITN, Q_ISITN_CHR )
      EQUIVALENCE  ( Q_ISTRN, Q_ISTRN_CHR )
      EQUIVALENCE  ( Q_IPAR1, Q_VAXOF )
      EQUIVALENCE  ( Q_IPAR2, Q_ISTRN )
      EQUIVALENCE  ( Q_MONUMENTS, Q_LMONUMENTS )
      EQUIVALENCE  ( Q_MONUMENTS, Q_MONUMENTS_CHR )
!
      COMMON /PARFL/ &
!
!     REAL*8
!
     &           Q_VAXOF, &
     &           Q_VSITEC, &
     &           Q_VSTARC, &
     &           Q_VATM, &
     &           Q_VREL, &
     &           Q_VTIDE, &
     &           Q_VPREC, &
     &           Q_VNUT, &
     &           Q_VSITEV, &
     &           Q_VNUTOP, &
     &           Q_BARO_CAL, &
     &           Q_BARO_HEIGHT, &
!
!     INTEGER*2
!
     &           Q_ISTRN, &
     &           Q_JNAME, &
     &           Q_ISITN, &
     &           Q_MONUMENTS, &
     &           Q_NUMSEL, &
     &           Q_IDBPSL, &
!
!     real*8
!
     &           Q_VSITED, &
     &           Q_PSITED, &
     &           Q_PWCNUMEP, &
     &           Q_PWCFNAME, &
!
!     integer*2
!
     &           Q_PWCSIZEP, &
!
!     again REAL*8
!
     &           Q_STA_FJD_BEG, &
     &           Q_STA_FJD_END, &
     &           Q_STA_FJD_MID, &
     &           Q_SRC_FJD_BEG, &
     &           Q_SRC_FJD_END, &
     &           Q_SRC_FJD_MID, &
     &           Q_GLO_FJDOBS_MIN, &
     &           Q_GLO_FJDOBS_MAX, &
!
!     again INTEGER*4
!
     &           Q_NSES_STA, &
     &           Q_NSES_SRC, &
!
!     again REAL*8
!
     &           Q_VSITEC_TIM_REF_MJD, &
!
! -------------- last variables
!
     &           Q_IZFREE, &
     &           Q_ILAST_PRFIL_I2
!
      EQUIVALENCE (Q_IPARFIL, Q_VAXOF)
      EQUIVALENCE (Q_IFIRST_PRFIL_I2, Q_VAXOF)
