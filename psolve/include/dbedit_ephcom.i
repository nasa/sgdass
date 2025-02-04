!!Ephcom
!
!     This include contains the declaration for the EPHEM library
!     common space holding most of the ephemeris info.
!
!     Programmers:
!       Gregg Cooke           90.02.12  Creation.
!
!     Parameters:
!
!     The total number of ephemeris values that can be used
!     by the routine is below.
!
      integer*2  MAXEPH
      parameter (MAXEPH = 60)
!
!     Specifications:
!
!     MOOND   --  Moon ephemeris data.
!     SUND    --  Sun ephemeris data.
!     EARTHD  --  Earth ephemeris data.
!     DEPSD   --  Delta epsilon.
!     DPSID   --  Delta psi.
!     TIME1   --  First time of ephemeris data.
!     TIMEDF  --  Interval of ephemeris data.
!     TITLE   --  PEP tape title.
!     NEPH    --  Ephemeris epoch.
!
      real*8 moond(3,2,MAXEPH), sund(3,2,MAXEPH), earthd(3,3,MAXEPH), &
     &       depsd(2,MAXEPH), dpsid(2,MAXEPH), time1, timedf
      integer*2 title(64), neph
!
!     Common Spaces:
!
      common /ephut/ moond, sund,   earthd, depsd, dpsid, &
     &               time1, timedf, title,  neph
