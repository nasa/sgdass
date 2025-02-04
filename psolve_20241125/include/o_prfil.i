!@This is the start of file &ORFIL
!
!  this is a representation of the OLD a-900 parfil
!
      COMMON /OARFL/ &
     &              OISITN(4,50)    ,OISTRN(4,200)    ,OVAXOF(50), &
     &OVSITEC(3,50) ,OVSTARC(2,200)  ,OVATM            ,OVREL     , &
     &OVTIDE(3)     ,OVPREC          ,OVNUT(2,6)       ,OIDBPSL(6,10), &
     &ONUMSEL       ,OMONUMENTS(5,50),OIZFREE(163)
!
      INTEGER*2 &
     &OIZFREE       ,ONUMSEL         ,OIDBPSL          ,OISTRN    , &
     &OISITN                         ,OMONUMENTS
!
!     REAL*6
      CHARACTER*6 &
     &OVAXOF        ,OVSITEC         ,OVSTARC          ,OVATM     , &
     &OVREL         ,OVTIDE          ,OVPREC           ,OVNUT
!
!
!    THIS IS THE OLD PARFIL WITH THE NAMES CHANGED TO PROTECT THE INNOCENT
!    PARFL SPECS:
!    Contains the master catalogs of sites and sources, and other globally
!    defined constants.  It it saved in file 'PARFIL'.
!
!     Name      Location in common
!     IPRDC        1  -   16  DCB buffer stub.
!     ISITN       17  -  216  Site names array. 8 characters by 50 sites.
!     ISTRN      216  - 1016  Source names array. 8 characters by 200 sources.
!     VAXOF     2016  - 1166  Axis offsets for 50 sites. meters
!     VSITEC    1166  - 1616  X,Y,Z geocentric coordinates for 50 sites. (m)
!     VSTARC    1616  - 2816  Ra and Dec for 200 sources. (radians)
!     VATM      2816  - 2819  Universal zenith path delay a priori. (nsec)
!     VREL      2819  - 2822  Gamma a priori. (Unitless)
!     VTIDE     2822  - 2831  Univeral earth tide aprioris.
!     VPREC     2831  - 2834  Precession constant a priori.
!     VNUT      2834  - 2870  Nutation constant a prioris.
!     IDBPSL    2870  - 2930  The list of data bases in the SOLVE files.
!     NUMSEL    2930  - 2931  Number of data bases in SOLVE files.
!     MONUMENTS 2930  - 3181  10 character names for 50 sites.
!     IZFREE    3181  - 3344  Unused space.
!
