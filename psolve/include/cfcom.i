!!Cfflgs
!
!     Common containing the control file flags and related values.
!
!     Programmers:
!       Gregg Cooke        90.02.08  Creation.
!     Modifications:
!       Kaybee Wilcox      92.11.10  added a variable (errflg) to
!                                    store error messages.
!
!     Parameters:
!
!     These are the number of header flags and number of option flags.
!     Do not set them below 10 and 64, respectively.
!
      integer*2 MAXHDR, MAXCFL
      parameter (MAXHDR = 10, &
     &           MAXCFL = 64)
!
!     Specifications:
!
!     BHEDR  -- The list of header flags.
!     CFLAGS -- The list of option flags.
!     KFLAGS -- An array of indices referencing which option flags belong
!               to which header flags.
!     NHDR, MXHEDR, MXCFLG -- Work space used by cfrec and cfopn.
!
      character bhedr(MAXHDR)*16, cflags(MAXCFL)*16,errflg*16
      integer*2 kflgs(MAXHDR), nhdr, mxhedr, mxcflg
      common /cfflgs/ bhedr,cflags,kflgs,nhdr,mxhedr,mxcflg,errflg
!
