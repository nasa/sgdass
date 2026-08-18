!!Toctab
!
!     Common space for the master table of contents.  Other values hold
!     statistics needed to access the toc array and the database handler
!     efficiently.
!
!     Written:
!       Gregg Cooke     90.03.26
!
!     Modifications:
!       Kaybee Wilcox   92.04.23  "ltc" array added, comments updated.
!       Brent Archinal
!
!     Parameters:
!
!     MAXLCODE is the total number of lcodes the program can handle.
!     MAXTOCS is the total number of tocs the program can handle.
!
      integer*2 MAXLCODE, MAXTOCS
      parameter (MAXLCODE = 300, &
     &           MAXTOCS  = 3)
!
!     Specifications:
!
!     LTOC -- The format of the ltoc array is as follows:
!                Position  Type  Description
!                --------  ----  -----------
!                   1-8     C*8   Lcode
!                   5-7     I*2   Rapid search parameters
!                    8      I*2   Data type
!                   9-11    I*2   Dimensions
!                  23-54    C*32  Descriptor
!             The character portion of this area is initialized in
!             initl.f.  The integer portion is copied from LTC in
!             dbedit.f.
!
!     LTC  -- Same as LTOC, but holds integer portion only (e.g.
!             positions 5-11 above).  These values intialized in
!             initl.f, and copied to LTOC in main program (dbedit.f).
!
!     NDAT -- The ndat array holds information about database data types.
!             The data types are represented in this order:
!                 Position  Data Type
!                 --------  ---------
!                    1      real*8 (real*6 on the A900)
!                    2      integer*2
!                    3      integer*2
!                    4      real*8
!                    5      integer*4
!
!     NTYP -- The ntyp array holds table of contents totals.  The header
!             toc is not represented, so the first position is the total
!             number of tocs *including* the header toc.
!
      character ctoc(MAXLCODE)*54
      integer*2 ltoc(27,MAXLCODE), ndat(5), ntyp(MAXTOCS)
      integer*2 ltc(27,MAXLCODE)
      equivalence (ctoc,ltoc)
!
!     Common Space:
!
      common /toctab/ ltoc,ltc, ndat, ntyp
!
