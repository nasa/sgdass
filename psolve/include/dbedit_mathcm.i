!!Mathcm
!
!     This is the common block holding all the math constants and their
!     lcodes and dimensions.
!
!     Programmers:
!       Gregg Cooke           90.02.12  Creation.
!     Modifications:
!       Darin Miller          91.12.26  Added nnsite and nnstar.
!       Brent Archinal        94.02.05  Lowered maxmath, maxdata to
!                                       match new mathbd.f values.
!
!     Parameters:
!
!     MAXMATH is the number of math lcodes.
!     MAXDATA is the size of the array to store them.
!
      integer*2   MAXMATH, MAXDATA
      parameter ( MAXMATH = 17, &
     &            MAXDATA = 19 )
!
!     Specifications:
!
!     MLCOD  --  Math lcodes, plus space for rapid search values.
!     MHIST  --  Math lcode descriptions.
!     MDIM   --  Math lcode dimensions.
!     MATH   --  The actual math constants themselves, set in the
!                accompanying block data.
!     NNSITE --  Number of sites in skeleton database.
!     NNSTAR --  Number of sources in skeleton database.
!
      CHARACTER MLCOD(MAXMATH)*14, MHIST(MAXMATH)*32
      INTEGER*2 MDIM(MAXMATH), NNSITE, NNSTAR
      REAL*8    MATH(MAXDATA)
!
!     Common Spaces:
!
      COMMON / MATHCM / MLCOD, MHIST, MDIM, MATH, NNSITE, NNSTAR
!
