!@This is the start of file OOPRFIL
!
! kdb 11/22/95 Increase ipar2 array by one word to account for the word skipped
!              to place vsited on a real*8 boundary.
!
!
      REAL*8 &
     & ooVAXOF(MAX_STA), ooVSITEC(3,MAX_STA), ooVSTARC(2,MAX_SRC), &
     & ooVATM  , ooVREL  , ooVTIDE(3)  , ooVPREC  , ooVNUT(2,6), &
     & ooVSITEV(3,MAX_STA)  , ooVNUTOP(2,6)
!
      REAL*4 &
     & ooBARO_CAL(16)  ,  ooBARO_HEIGHT(16)
!
      INTEGER*2 &
     & ooISTRN(4,MAX_SRC), ooISITN(4,MAX_STA), ooMONUMENTS(5,MAX_STA), &
     & ooNUMSEL  , ooIDBPSL(6,MAX_DBS)  ,  oopwcnumep
      character*16 &
     & oopwcfname
      REAL*8 &
     & oovsited(MAX_STA),oopsited(MAX_STA)
      INTEGER*2 &
     & oopwcsizep,ooIZFREE(99)
!
      INTEGER*2 ooIPARFIL(JPARFIL_WORDS),ooipar1(20600),ooipar2(11878)
      character*8 ooisitn_chr(max_sta)
      equivalence (ooisitn,ooisitn_chr)
      character*8 ooistrn_chr(max_src)
      equivalence (ooistrn,ooistrn_chr)
      equivalence (ooipar1,oovaxof)
      equivalence (ooipar2,ooistrn)
!
      COMMON /OOPARFL/ &
!     REAL*8
     & ooVAXOF           , ooVSITEC             , ooVSTARC    , &
     & ooVATM  , ooVREL  , ooVTIDE     , ooVPREC  , ooVNUT    , &
     & ooVSITEV             , ooVNUTOP       , &
     & ooBARO_CAL  ,  ooBARO_HEIGHT      , &
!
!     INTEGER*2
     & ooISTRN             , ooISITN             , ooMONUMENTS , &
     & ooNUMSEL  , ooIDBPSL        , &
!     real*8
     & oovsited   ,  oopsited    ,  oopwcnumep   ,   oopwcfname , &
!     integer*2
     & oopwcsizep,ooIZFREE
!
      EQUIVALENCE (ooIPARFIL,ooVAXOF)
!
!    PARFL SPECS:
!    Contains the master catalogs of sites and sources, and other globally
!    defined constants.  It it saved in file 'PARFIL'.
!
!     Name      Location in common
!     ISITN        1  -  512  Site names array. 8 characters by 128 sites.
!     isitn_chr          Character*8 verion isitn. Equivalenced.
!     MONUMENTS  513  - 1152  10 character names for 128 sites.
!     ISTRN     1153  - 2688  Source names array. 8 characters by 384 sources.
!     istrn_chr          Character*8 verion istrn. Equivalenced.
!     VAXOF     2689  - 3200  Axis offsets for 128 sites. meters
!     VSITEC    3201  - 4736  X,Y,Z geocentric coordinates for 128 sites. (m)
!     VSITEV    4737  - 6272  X,Y,Z velocities for 128 sites. (m/yr)
!     VSTARC    6273  - 9344  Ra and Dec for 384 sources. (radians)
!     VATM      9345  - 9348  Universal zenith path delay a priori. (nsec)
!     VREL      9349  - 9352  Gamma a priori. (Unitless)
!     VTIDE     9353  - 9364  Univeral earth tide aprioris.
!     VPREC     9365  - 9368  Precession constant a priori.
!     VNUT      9369  - 9416  Nutation constant a prioris.
!     IDBPSL    9417  - 9476  The list of data bases in the SOLVE files.
!     NUMSEL    9477  - 9477  Number of data bases in SOLVE files.
!     VNUTOP    9478  -       Nutation a priori for out of pahse terms.
!     BARO_CAL                Barometer calibration by station.  SUBTRACT!
!                             from observed to get corrected.
!     BARO_HEIGHT             Barometer TO(!) intersection of axis height
!                             offset in meters by station.
!     IZFREE    9478  - 9600  Unused space.
