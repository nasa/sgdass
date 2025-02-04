!@This is the start of file &PRFIL (1074-blocks long)
!
! Note: If any changes are made here, changes may be necessary in
!       ../sdbh/parblk.f.
!
! kdb 11/22/95 Increase ipar2 array by one word to account for the word skipped
!              to place vsited on a real*8 boundary.
! JMG  1/09/96 Equivaleneced monuments to an ascii string
! BA     98.08.26  Note added above.
! pet  2000.06.13  Added parameter  LEN_PRFIL_FILLER_I2, IFIRST_PRFIL,
!                                   ILAST_PRFIL
! pet  2002.09.26  Added varaiables STA_FJD_BEG, STA_FJD_END, STA_FJD_MID,
!                                   SRC_FJD_BEG, SRC_FJD_END, SRC_FJD_MID,
!                                   NSES_STA, NSES_SRC
! pet  2002.10.04  Added two variables GLO_FJDOBS_MIN and GLO_FJDOBS_MAX
!                  which keep minimal and maximal nonimal Julian date
!                  of any session in the global solution
! pet  2020.12.30  Added VSITEC_TIM_REF_MJD that keeps the reference epoch 
!                  for site position apriori
!
! If you change this file you must also change Q_PRFIL
!
!
      INTEGER*2    LEN_PRFIL_FILLER_I2 ! Free, unused space in two-byte words
      PARAMETER  ( LEN_PRFIL_FILLER_I2 = 119 )
!
      REAL*8 &
     &             VAXOF(MAX_STA), &
     &             VSITEC(3,MAX_STA), &
     &             VSTARC(2,MAX_SRC), &
     &             VATM, &
     &             VREL, &
     &             VTIDE(3), &
     &             VPREC, &
     &             VNUT(2,6), &
     &             VSITEV(3,MAX_STA), &
     &             VNUTOP(2,6), &
     &             VSITEC_TIM_REF_MJD
!
      REAL*8 &
     &             BARO_CAL(MAX_ARC_STA), &
     &             BARO_HEIGHT(MAX_ARC_STA)
!
      REAL*8       STA_FJD_BEG(MAX_STA), &
     &             STA_FJD_END(MAX_STA), &
     &             STA_FJD_MID(MAX_STA), &
     &             SRC_FJD_BEG(MAX_SRC), &
     &             SRC_FJD_END(MAX_SRC), &
     &             SRC_FJD_MID(MAX_SRC)
      REAL*8       GLO_FJDOBS_MIN, GLO_FJDOBS_MAX
!
      INTEGER*4    NSES_STA(MAX_STA), &
     &             NSES_SRC(MAX_SRC)
!
      INTEGER*2 &
     &             ISTRN(4,MAX_SOU), &
     &             ISITN(4,MAX_STA), &
     &             MONUMENTS(5,MAX_STA), &
     &             NUMSEL, &
     &             IDBPSL(6,MAX_DBS), &
     &             PWCNUMEP, &
     &             IFIRST_PRFIL_I2, &
     &             ILAST_PRFIL_I2
      CHARACTER*16 &
     &             PWCFNAME
      REAL*8 &
     &             VSITED(MAX_STA), &
     &             PSITED(MAX_STA)
      INTEGER*2 &
     &             PWCSIZEP, &
     &             IZFREE(LEN_PRFIL_FILLER_I2)
!@      INTEGER*2    SRC_FLAGS_I2(MAX_SRC)
!
      INTEGER*2    IPARFIL(JPARFIL_WORDS), &
     &             IPAR1(20600), &
     &             IPAR2(35552)
      CHARACTER    ISITN_CHR(MAX_STA)*8,   &
     &             ISTRN_CHR(MAX_SOU)*8,   &
     &             JNAME(MAX_SOU)*10,      &
     &             LMONUMENTS(MAX_STA)*10, &
     &             MONUMENTS_CHR(MAX_STA)*10
      CHARACTER     BNAME(MAX_SOU)*8
      EQUIVALENCE ( ISITN, ISITN_CHR )
      EQUIVALENCE ( ISTRN, ISTRN_CHR )
      EQUIVALENCE ( BNAME, ISTRN_CHR )
      EQUIVALENCE ( IPAR1, VAXOF )
      EQUIVALENCE ( IPAR2, ISTRN )
      EQUIVALENCE ( MONUMENTS, LMONUMENTS )
      EQUIVALENCE ( MONUMENTS, MONUMENTS_CHR )
!
      COMMON /PARFL/ &
!
!     REAL*8
!
     &           VAXOF, &
     &           VSITEC, &
     &           VSTARC, &
     &           VATM, &
     &           VREL, &
     &           VTIDE, &
     &           VPREC, &
     &           VNUT, &
     &           VSITEV, &
     &           VNUTOP, &
     &           BARO_CAL, &
     &           BARO_HEIGHT, &
!
!     INTEGER*2
!
     &           ISTRN, &
     &           JNAME, &
     &           ISITN, &
     &           MONUMENTS, &
     &           NUMSEL, &
     &           IDBPSL, &
!
!     real*8
!
     &           VSITED, &
     &           PSITED, &
     &           PWCNUMEP, &
     &           PWCFNAME, &
!
!     integer*2
!
     &           PWCSIZEP, &
!
!     again REAL*8
!
     &           STA_FJD_BEG, &
     &           STA_FJD_END, &
     &           STA_FJD_MID, &
     &           SRC_FJD_BEG, &
     &           SRC_FJD_END, &
     &           SRC_FJD_MID, &
     &           GLO_FJDOBS_MIN, GLO_FJDOBS_MAX, &
!
!     again INTEGER*4
!
     &           NSES_STA, &
     &           NSES_SRC, &
!
!    again REAl*8
!
     &           VSITEC_TIM_REF_MJD, &
!
!     again INTEGER*2
!
!@                 SRC_FLAGS_I2, &
!
! -------------- last variables
!
     &           IZFREE, &
     &           ILAST_PRFIL_I2
!
      EQUIVALENCE ( IPARFIL,VAXOF)
      EQUIVALENCE ( IFIRST_PRFIL_I2, VAXOF)
!
!    PARFL SPECS:
!    Contains the master catalogs of sites and sources, and other globally
!    defined constants.  It is saved in file 'PARFIL'.
!
! VAXOF        Axis offsets for max_sta sites. meters
! VSITEC       X,Y,Z geocentric coords, max_sta sites. (m)
! VSITEC_TIM_REF_MJD reference data for the apriori site positinos. Units: MJD (days)
! VSTARC       Ra and Dec for max_src sources. (radians)
! VATM         Universal zenith path delay a priori. (nsec)
! VREL         Gamma a priori. (Unitless)
! VTIDE        Univeral earth tide aprioris.
! VPREC        Precession constant a priori.
! VNUT         Nutation constant a prioris.
! VSITEV       X,Y,Z velocities for max_sta sites. (m/yr)
! VNUTOP       Nutation a priori for out of pahse terms.
! BARO_CAL     Barometer calibration by station.  SUBTRACT!
!              from observed to get corrected. 32 sites
! BARO_HEIGHT  Barometer TO(!) intersection of axis height
!              offset in meters by station. 32 sites
! ISTRN        Source names array. 8 chars X max_src sources
! istrn_chr    Character*8 verion istrn. Equivalenced.
! ISITN        Site names array. 8 chars X max_sta sites.
! isitn_chr    Character*8 verion isitn. Equivalenced.
! MONUMENTS    0 character names for max_sta sites.
! NUMSEL       Number of data bases in SOLVE files.
! IDBPSL       The list of data bases in the SOLVE files.
! **skip**     start real*8 vsited on real*8 boundary
! VSITED
! PSITED
! PWCNUMEP
! PWCFNAME
! PWCSIZEP
! GLO_FJDOBS_MIN  --  minimal Julian date over all sessions in global run
! GLO_FJDOBS_MAX  --  maximal Julian date over all sessions in global run
! STA_FJD_BEG  Array of the Julian date of the first observation at this station
! STA_FJD_END  Array of the Julian date of the last observation  at this station
! STA_FJD_MID  Array of the Julian date of the middle epoch for this station
! STR_FJD_BEG  Array of the Julian date of the first observation of this source
! STR_FJD_END  Array of the Julian date of the last observation  of this source
! SRC_FJD_MID  Array of the Julian date of the middle epoch for this source
! NSES_STA     Array with the number of sessions this station participated
! NSES_SRC     Array with the number of sessions this source was observed
! SRC_FLAGS_I2 Array of source flags
! IZFREE       Unused space.
