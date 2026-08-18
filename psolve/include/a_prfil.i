!@This is the start of file &PRFIL
!
!  this is a representation of the CURRENT A-900 parfil
!
      INTEGER*2 &
     & A_IZFREE( 11),A_NUMSEL,A_IDBPSL(6,10),A_ISTRN(4,A_MAX_SRC), &
     & A_ISITN(4,A_MAX_STA),A_MONUMENTS(5,A_MAX_STA), &
     &A_IPARFIL(A_JPARFIL_WORDS)
!
!     REAL*4
      CHARACTER*4 &
     &A_BARO_CAL(A_MAX_ARC_STA)      ,A_BARO_HEIGHT(A_MAX_ARC_STA)
!
!     REAL*8
      CHARACTER*8 &
     & A_VAXOF(A_MAX_STA), A_VSITEC(3,A_MAX_STA), A_VSTARC(2,A_MAX_SRC), &
     &  A_VATM     , &
     & A_VREL          , A_VTIDE(3)         , A_VPREC            , &
     & A_VNUT(2,6) , &
     & A_VSITEV(3,A_MAX_STA), A_VNUTOP(2,6)
!
      COMMON /A_PARFL/ &
     & A_ISITN, A_MONUMENTS,    A_ISTRN,    A_VAXOF,   A_VSITEC, &
     & A_VSITEV, A_VSTARC, &
     &  A_VATM,      A_VREL,    A_VTIDE,    A_VPREC,     A_VNUT, &
     & A_IDBPSL, A_NUMSEL, &
     & A_VNUTOP, A_BARO_CAL,    A_BARO_HEIGHT, &
     & A_IZFREE
!
      EQUIVALENCE (A_IPARFIL(1),A_ISITN(1,1))
!
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
!     VNUTOP                  ?
!     BARO_CAL                Barometer calibration by station.  SUBTRACT!
!                             from observed to get corrected.
!     BARO_HEIGHT             Barometer TO(!) intersection of axis height
!                             offset in meters by station.
!     IZFREE    9478  - 9600  Unused space.
!
