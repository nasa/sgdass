!!Skull
!
!     Skeleton Common Space & Parameters.
!
!     Programmers:
!       Gregg Cooke           90.02.12  Creation.
!
!     Modifications:
!       Melvin White    94.01.31  All statements referring to slew rates
!                                 and limit stops dropped.
!                                 Added "cplat" and "iplat".
!       Leonid Petrov   2000.12.15  Inreased MAXSTR from 1024 to 16384
!     Parameters:
!
!     MAXSTN determines the total number of stations allowed in the
!     station catalog.
!     MAXSTR determines the total number of sources allowed in the
!     source catalog.
!     Lower these values to decrease the size of the stack if you are
!     having troubles loading skeleton, but remember that they must
!     be at least big enough to accomodate blokq.dat.
!     As of 890505      the blokq file had ~100 stations and ~400  sources.
!     As of 2000.12.15  the blokq file had ~160 stations and ~2700 sources.
!
!     CONDG is the conversion factor for degrees to radians.
!
      INTEGER*2 MAXSTN, MAXSTR
      REAL*8 CONDG
      PARAMETER ( MAXSTR = 16384, &   ! maximal number of sources
     &            MAXSTN = 256, &     ! maximal number of stations
     &            CONDG  = .17453292519D-01 )
!
!     Specifications:
!
!     HIST     --  History entry for the new version of the skeleton.
!
!     ISRN     --  Source names.
!     NSTAR    --  Number of sources.
!     STRC     --  Source coordinates.
!     ISRPOS   --  Source references.
!
!     ISTN     --  Station names.
!     NSITE    --  Number of stations.
!     SITR     --  Station coordinates.
!     IAXIS    --  Antenna axis types.
!     STZEN    --  Antenna zenith path delays.
!     AXOFF    --  Antenna axis offsets.
!     IPLAT    --  Tectonic plate names.
!
!     OCAMP    --  Vertical ocean loading amplitudes.
!     OCPHS    --  Vertical ocean loading phases.
!     HOCAMP   --  Horizontal ocean loading amplitudes.
!     HOCPHS   --  Horizontal ocean loading phases.
!     OCESTAT  --  Either 'YES ' or 'NO  ', indicating the presence of
!                  horizontal ocean loading values.
!
      character hist*80, lfto*80, cstn(MAXSTN)*8, csrn(MAXSTR)*8, &
     &          cplat(MAXSTN)*4
      integer*2 nsite, nstar, nocn, istn(4,MAXSTN), iaxis(MAXSTN), &
     &          isrn(4,MAXSTR), isrpos(10,MAXSTR), ocestat(2,MAXSTN), &
     &          stni(MAXSTN), srni(MAXSTR), iplat(2,MAXSTN)
      real*8 sitr(3,MAXSTN), stzen(MAXSTN), axoff(MAXSTN), &
     &       strc(2,MAXSTR), ocamp(11,MAXSTN), ocphs(11,MAXSTN), &
     &       hocamp(11,2,MAXSTN), hocphs(11,2,MAXSTN)
      equivalence (cstn,istn), (csrn,isrn),(cplat,iplat)
!
!     Common Spaces:
!
!     The common block for the skeleton subroutines holds the entire
!     skeleton database.
!
      common /skull/ hist, lfto, nsite, nstar, nocn, istn, iaxis, &
     &               isrn, isrpos, ocestat, stni, srni, iplat, sitr, &
     &               stzen, axoff, strc, ocamp, ocphs, hocamp, hocphs
!
