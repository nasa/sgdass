      SUBROUTINE get_weights(constant,nbline)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GET_WEIGHTS PROGRAM SPECIFICATION
!
! 1.1 Get re-weight constants from NAMFIL.
!
! 1.2 REFERENCES:
!
! 2.  GET_WEIGHTS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables:
!
      real*8 constant(4,MAX_ARC_BSL)
      integer*2 nbline
!
! CONSTANT - The reweight values from the namfil, four per baseline.
!            (All baselines' values are equal if by arc weighting is used.
! NBLINE - number of baselines.
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: prces
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      character*72 jbuf
      character*8 jbasl(2)
      integer*2 idb,ierr
      INTEGER*4 IOS
      real*8 ee(4)
      data idb/1/
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
!   kdb  970204  New site weighting feature.
!
! 5.  GET_WEIGHTS PROGRAM STRUCTURE
!
      ierr = 0
      nbline = 0
!
! Get first REWT card from NAMFIL
!
      call getcard( idb, 'REWT', INT2(1), jbuf, ierr )
      do while (ierr .eq. 0)
!
! Read REWT cards until finished or error
!
        IF ( INDEX ( JBUF(23:32), '*' ) > 0 ) JBUF(23:32) = '      0.00'
        IF ( INDEX ( JBUF(33:42), '*' ) > 0 ) JBUF(33:42) = '      0.00'
        IF ( INDEX ( JBUF(43:52), '*' ) > 0 ) JBUF(43:52) = '      0.00'
        IF ( INDEX ( JBUF(53:62), '*' ) > 0 ) JBUF(53:62) = '      0.00'
        READ ( JBUF, '(5X,A8,1X,A8,4F10.2,8X)', IOSTAT=IOS ) JBASL, EE
        CALL FERR ( INT2(IOS), "Decoding REWT card", INT2(0), INT2(0) )
!
        nbline = nbline + 1
        constant(1,nbline) = ee(1)
        constant(2,nbline) = ee(2)
        constant(3,nbline) = ee(3)
        constant(4,nbline) = ee(4)
        call getcard( idb, 'REWT', INT2(0), jbuf, ierr )
      enddo
!
! If error, pause and report it
!
      if(ierr.ne.1) then
!       pause 'error reading NAMFIL in get_weights'
        call ferr( INT2(122), 'error reading NAMFIL in get_weights', INT2(0), &
     &       INT2(0) )
      endif
!
      return
      end
