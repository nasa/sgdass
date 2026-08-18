!!Ut1com
!
!     This include contains the declaration for the APRIORI library
!     common space holding all of the UT1PM info.  It is used by
!     both the maintenance and access functions of the library.
!
!     Programmers:
!       Gregg Cooke           90.03.30  Creation.
!     Modifications:
!       Brent Archinal        90.12.13  Default location of UT1PM.DAT
!                                       added and MXUPDT changed from
!                                       2048 to 6000.
!       Kaybee Wilcox         92.04.23  DFUTNM parameter setup commented
!                                       out as now obtained by routines
!                                       from param.i.
!       Brent Archinal        94.06.23  MXUPDT changed from 6000 to
!                                       15000 (~41 years of 1 day data).
!
!     Parameters:
!
!     DFUTNM is the default UT1PM filename.
!
!      character DFUTNM*19
!      parameter (DFUTNM = '/data/erp/ut1pm.dat')
!
!     The total number of ut1pm values for the ut1pm update array and the
!     UT1, polar motion, and rotation epoch arrays.
!
      integer*2  MXUTPM, MXUPDT
      parameter (MXUTPM = 20, &
     &           MXUPDT = 15000)
!
!     Specifications:
!
!     UT1NM   --  The file name of the current ut1pm file.
!     UTIMS   --  Array containing timely information about the current
!                 ut1pm file (all times are in modified Julian format):
!                   Position    Description
!                   -----------------------------
!                      1        Beginning of ut1pm file.
!                      2        End of ut1pm file.
!                      3        Time of last update.
!                      4        Beginning of last update interval.
!                      5        End of last update interval.
!     UTVER   --  Version number of current ut1pm file.
!     UINTV   --  Interval of ut1pm.dat file.
!     NRECS   --  Total number of records in ut1pm file.
!     UTDCB   --  Unit number of ut1pm file.
!     SERES   --  Four character series designation.
!     UTHST   --  Description of current ut1pm file.
!     UPHST   --  History entry of last update.
!     UT1UP   --  Array for ut1pm file update information.
!     UINF    --  UT1PM information array.
!     U1A     --  UT1 data.
!     WOBBL   --  Wobble data.
!     ROTEP   --  Rotation epoch data.
!     TAIUTC  --  TAI - UTC array.
!     A1UTC   --  A1 - UTC array.
!     A1TAI   --  A1 - TAI array.
!     TIDE    --  Flag indicating presence of long term tides.
!
      character seres*4, uthst*40, uphst*16, ut1nm*255
      integer*2 utdcb, utver, tide
      integer*4 nrecs
      real*8 u1a(MXUTPM), wobbl(2,MXUTPM), rotep(2,MXUTPM), &
     &       taiutc(3), a1utc(3), a1tai(3), uinf(4), utims(5), &
     &       uintv, ut1up(8,MXUPDT)
!
      common /ut1com/ ut1nm, utims, utver, uintv, nrecs, utdcb, tide, &
     &                seres, uthst, uphst, ut1up, u1a,   wobbl, rotep, &
     &                uinf,  a1utc, a1tai, taiutc
