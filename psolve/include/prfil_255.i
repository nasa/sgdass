!@This is the start of file &PRFIL
! 255 block version
! kdb 11/22/95 Increase ipar2 array by one word to account for the word skipped
!              to place vsited on a real*8 boundary.
!
!
      REAL*8 &
     & VAXOF_255(MAX_STA), VSITEC_255(3,MAX_STA), VSTARC_255(2,MAX_SRC), &
     & VATM_255, VREL_255, VTIDE_255(3), VPREC_255, VNUT_255(2,6), &
     & VSITEV_255(3,MAX_STA)  , VNUTOP_255(2,6)
!
      REAL*8 &
     & BARO_CAL_255(16)  ,  BARO_HEIGHT_255(16)
!
      INTEGER*2 &
     & ISTRN_255(4,MAX_SRC), ISITN_255(4,MAX_STA), &
     & MONUMENTS_255(5,MAX_STA), &
     & NUMSEL_255  , IDBPSL_255(6,MAX_DBS)  ,  pwcnumep_255
      character*16 &
     & pwcfname_255
      REAL*8 &
     & vsited_255(MAX_STA),psited_255(MAX_STA)
      INTEGER*2 &
     & pwcsizep_255,IZFREE_255(35)
!
      INTEGER*2 IPARFIL_255(32640),ipar1_255(20600), &
     &        ipar2_255(11878)
      character*8 isitn_chr_255(MAX_STA)
      equivalence (isitn_255,isitn_chr_255)
      character*8 istrn_chr_255(MAX_SRC)
      equivalence (istrn_255,istrn_chr_255)
      equivalence (ipar1_255,vaxof_255)
      equivalence (ipar2_255,istrn_255)
!
      COMMON /PARFL_255/ &
!     REAL*8
     & VAXOF_255, VSITEC_255, VSTARC_255, &
     & VATM_255, VREL_255, VTIDE_255, VPREC_255, VNUT_255, &
     & VSITEV_255, VNUTOP_255, &
     & BARO_CAL_255,  BARO_HEIGHT_255, &
!
!     INTEGER*2
     & ISTRN_255, ISITN_255, MONUMENTS_255, &
     & NUMSEL_255, IDBPSL_255, &
!     real*8
     & vsited_255,  psited_255,  pwcnumep_255,   pwcfname_255, &
!     integer*2
     & pwcsizep_255,IZFREE_255
!
      EQUIVALENCE (IPARFIL_255,VAXOF_255)
!
!    PARFL SPECS:
!    Contains the master catalogs of sites and sources, and other globally
!    defined constants.  It is saved in file 'PARFIL'.
!
!        (note: max_sta = 512, max_src = 768)
!  Name    Location in common
!           # words  words
! VAXOF        2048     1 -  2048  Axis offsets for max_sta sites. meters
! VSITEC       6144  2049 -  8192  X,Y,Z geocentric coords, max_sta sites. (m)
! VSTARC       6144  8193 - 14336  Ra and Dec for max_src sources. (radians)
! VATM            4 14337 - 14340  Universal zenith path delay a priori. (nsec)
! VREL            4 14341 - 14344  Gamma a priori. (Unitless)
! VTIDE          12 14345 - 14356  Univeral earth tide aprioris.
! VPREC           4 14357 - 14360  Precession constant a priori.
! VNUT           48 14361 - 14408  Nutation constant a prioris.
! VSITEV       6144 14409 - 20552  X,Y,Z velocities for max_sta sites. (m/yr)
! VNUTOP         48 20553 - 20600  Nutation a priori for out of pahse terms.
! BARO_CAL       64 20601 - 20664  Barometer calibration by station.  SUBTRACT!
!                                  from observed to get corrected.  16 sites
! BARO_HEIGHT    64 20665 - 20728  Barometer TO(!) intersection of axis height
!                                  offset in meters by station.  16 sites
! ISTRN        3072 20729 - 23800  Source names array. 8 chars X max_src sources
! istrn_chr                   Character*8 verion istrn. Equivalenced.
! ISITN        2048 23801 - 25848  Site names array. 8 chars X max_sta sites.
! isitn_chr                   Character*8 verion isitn. Equivalenced.
! MONUMENTS    2560 25849 - 28408  10 character names for max_sta sites.
! NUMSEL          1         28409  Number of data bases in SOLVE files.
! IDBPSL         90 28410 - 28499  The list of data bases in the SOLVE files.
! **skip**        1         28500  start real*8 vsited on real*8 boundary
! VSITED       2048 28501 - 30548
! PSITED       2048 30549 - 32596
! PWCNUMEP        1         32597
! PWCFNAME        8 32598 - 32605
! PWCSIZEP        1         32606
! IZFREE         35 32607 - 32641  Unused space.
!     (note that izfree should be declared 34 words long to end parfil on
!       an even number of blocks (32640 words = 255 blocks),
!       but this is a long-standing error which doesn't appear to cause any
!       harm, presumably because the gsfcb jparfil_words parameter controls
!       how many words are actually written to and read from disk.
!         KDB 11/21/95)
