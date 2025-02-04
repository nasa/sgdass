!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
      integer*2 function getunit_not (n, inots)
!
!     This function was written to allow getunit to be conveniently
!     patched into programs with some units already hard wired e.g.
!     those that call the solarc routines.
!
!     This function calls getunit, then checks to see if the
!     unit selected by getunit is one in the list inots.  If so
!     it calls getunit again and continues this process until
!     it gets a unit NOT in the list.
!
!     DS Caprette HSTX 94/08/17  Wrote from scratch.
!
      implicit none
!
!     input variables
!
      integer*2  n        !number of already alocated lus (length of inots)
      integer*2  inots(*) !list of already alocated lus
!
!     internal variables
!
      integer*2  i, iunit
!
!     output variable
!
      integer*2  getunit
!
!
 100  iunit = getunit()
      do i = 1, n
        if ( iunit .eq. inots(n) ) goto 100
      end do
      getunit_not = iunit
!
      return
      end
