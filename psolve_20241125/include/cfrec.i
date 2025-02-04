!!cfrec.i
!
!     Common containing the error message flag
!     for wrong flags in control file.
!
!     Programmers:
!       Kaybee Wilcox   92.11.10  Creation.
!
!     Modifications:
!       Brent Archinal  92.12.28  Rewritten.  Name
!                                 changed from geterr.i to cfrec.i
!
!     Specifications:
!     error  -- If true, unrecognized control flags are reported by
!               cfrec.
!
      logical*4 cferr
      common /cfer/ cferr
!
