!
! "namblk.f"
!
! Purpose           Routine to zero and blank out parameter file (namfl)
!                   common block.
!
! Written           Brent A. Archinal
!                   Earth Orientation Department
!                   U. S. Naval Observatory
!                   3450 Massachusetts Avenue, N.W.
!                   Washington, D.C. 20392
!                   USA
!                   baa@casa.usno.navy.mil, 202-762-1564
!
! Date              August 27, 1998 (finished August 31,1998)
!
! Notes             1. Based on ../include/namfl.i.  Therefore SHOULD BE
!                   UPDATED WHENEVER namfl.i IS CHANGED!
!                   2. Call once per program, before use of "NAMFIL".
!                   Should be called by batch, cres, gtsup, niell, proc,
!                   reway, setfil, trans, all of which read NAMFIL.
!                   3. There was a ../cutil file by this name (e.g.
!                   namblk.f).  This was a block data routine "namblkb"
!                   that only set istatus and kstatus to 0 - however
!                   it was no longer being loaded into any program!
!
! Variables
!  istatus   I*2    Flag to indicate whether NAMFIL is open.
!                   1 - Open.
!                   2 - Closed.
!                   Set in opennamefil.f and closenamefil.f.
!
!  kstatus   I*2    Flag to indicate whether NAMFIL has been written to.
!                   0 - Has not been written to.
!                   1 - Has been rewound or written to.
!                   Used in closenamfil.f and putcard.f.
!
! Subprograms used
!  Fortran          len
!
! Common            namchr, namhol
!
! System            f77 on HP-UX 10.2
!
! Modifications
!
!CCCCC
      SUBROUTINE NAMBLK()
!
      IMPLICIT NONE
      INTEGER*4 I, J, K
!
      INCLUDE 'solve.i'
      INCLUDE 'namfl.i'
!
!     write(6,*) "Now entering cutil/namblk."
!
!     real*8
!
!
!     integer*2
!
      ISTATUS = 0
      IREC    = 0
      IACTSEC = 0
      KSTATUS = 0
!
! --- Zero out iqq, and equivalenced "idbs", "ilast", and "ifirst".
!
      DO I = 1, 16
         IQQ(I) = 0
      ENDDO
!
!     character
!
! --- Blank out kbuf, and equivalenced "lbuf".
!
      DO I = 1, LEN(KBUF)
         KBUF(I:I) = ' '
      ENDDO
      DO I = 1, LEN(NNAME)
         NNAME(I:I) = ' '
      ENDDO
      DO I = 1, LEN(LHOLD)
         LHOLD(I:I) = ' '
      ENDDO
!
!X    write(6,*) "Now leaving cutil/namblk."
!
      RETURN
      END    !#!  NAMBLK  #!#
