!!Apricm
!
!     Site and Source A Priori Common Space & Parameters.
!
!     Programmers:
!       Gregg Cooke    90.02.12  Creation.
!
!     Modifications:
!       Melvin White   94.01.31  Added "cplat" and "iplat".
!                                Dropped "slrat" and "xlims".
!                                Included ../includes/param.i entry.
!                                Removed declaration and parameter
!                                statements for MXDBSIT and MXDBSTR.
!       Brent Archinal 98.04.28  param.i include moved to user routines
!                                to avoid HP-UX 10 compiler include bug.
!
!     Parameters:
!
!     MXDBSIT determines the number of stations that can be retrieved
!     from the station catalog for a given database.
!     MXDBSTR determines the number of sources that can be retrieved
!     from the source catalog for a given database.
!
!     Usage:
!
!     Routines using this include should first include the
!     "../includes/param.i" file.
!
!     include '../includes/param.i'
!
!     Specifications:
!
!     STRC   --  Source coordinates.
!     SOREF  --  Source references.
!
!     XSITR  --  Site coordinates.
!     AXIST  --  Antenna axis types.
!     XZEN   --  Antenna zenith path delays.
!     AXSOF  --  Antenna axis offsets.
!     IPLAT  --  Tectonic plate names.
!
!     XAMP   --  Vertical ocean loading amplitudes.
!     XPHS   --  Vertical ocean loading phases.
!     XHAMP  --  Horizontal ocean loading amplitudes.
!     XHPHS  --  Horizontal ocean loading phases.
!     OSTAT  --  Either 'YES ' or 'NO  ', indicating the presence of
!                horizontal ocean loading values.
!
      character cplat(MXDBSIT)*4
      integer*2 axist(MXDBSIT), soref(10,MXDBSTR), ostat(2,MXDBSIT), &
     &          iplat(2,MXDBSIT)
      real*8 xsitr(3,MXDBSIT), xzen(MXDBSIT), axsof(MXDBSIT), &
     &       strc(2,MXDBSTR), &
     &       xamp(11,MXDBSIT), xphs(11,MXDBSIT), &
     &       xhamp(11,2,MXDBSIT), xhphs(11,2,MXDBSIT)
      equivalence (cplat,iplat)
!
!     Common Spaces:
!
!     The common block for the skeleton subroutines holds only enough
!     a priori information for one database.
!
      common /apricm/ axist, soref, ostat, iplat, xsitr, xzen, axsof, &
     &                strc, xamp, xphs, xhamp, xhphs
