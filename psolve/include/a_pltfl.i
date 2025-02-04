!@This is the start of file &PLTFL
!
!  this is a representation of the A-900 pltfil
!
      INTEGER*2 A_IPLTDC(16),A_NUMBLK,A_NPLT,A_NPLST(100), &
     &          A_NBLPL(4,2,96)
      INTEGER*2 A_NFRE2(32),A_NPLBLK(50),A_NPLEX(50),A_NPTPL(96), &
     &          A_NFREE(32)
!     REAL*6 A_TPLBLK(50)
      CHARACTER*6 A_TPLBLK(50)
      COMMON/A_PLTFL/A_IPLTDC,A_NUMBLK,A_NPLT,A_NPLST,A_NBLPL,A_NFRE2, &
     &          A_NPLBLK,A_NPLEX, &
     &           A_TPLBLK,A_NPTPL,A_NFREE
