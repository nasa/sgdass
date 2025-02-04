!@This is the start of file OPRFIL (a copy of prfil with 128
!    of 128)
!
!
      REAL*8 &
     & OVAXOF(128)  ,OVSITEC(3,128)  ,OVSTARC(2,MAX_SRC)  , &
     & OVATM  ,OVREL  ,OVTIDE(3)  ,OVPREC  ,OVNUT(2,6)  , &
     & OVSITEV(3,128)  ,OVNUTOP(2,6)
!
      REAL*4 &
     & OBARO_CAL(16)  , OBARO_HEIGHT(16)
!
      INTEGER*2 &
     & OISTRN(4,MAX_SRC)  ,OISITN(4,128)  ,OMONUMENTS(5,128)  , &
     & ONUMSEL  ,OIDBPSL(6,MAX_DBS)
      REAL*8 &
     & Ovsited(128)
      INTEGER*2 &
     & OIZFREE(109)
!
      INTEGER*2 OIPARFIL(14848)
!
      COMMON /OPARFL/ &
!     REAL*8
     & OVAXOF           ,OVSITEC             ,OVSTARC             , &
     & OVATM  ,OVREL  ,OVTIDE     ,OVPREC  ,OVNUT       , &
     & OVSITEV             ,OVNUTOP       , &
!     REAL*4
     &OBARO_CAL  , OBARO_HEIGHT      , &
!
!     INTEGER*2
     &OISTRN             ,OISITN             ,OMONUMENTS             , &
     &ONUMSEL  ,OIDBPSL        , &
!     real*8
     &ovsited   , &
!     integer*2
     &OIZFREE
!
      EQUIVALENCE (OIPARFIL,OVAXOF)
!
!    PARFL SPECS:
!    Contains the master catalogs of sites and sources, and other globally
!    defined constants.  It it saved in file 'PARFIL'.
!
!     Name      Location in common
!     ISITN        1  -  512  Site names array. 8 characters by 128 sites.
!     MONUMENTS  513  - 1152  10 character names for 128 sites.
!     ISTRN     1153  - 2688  Source names array. 8 characters by 384 sources.
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
