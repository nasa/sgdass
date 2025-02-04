      LOGICAL*2 function get_source_weight(istrn,weight_file,constants)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 1.  GET_SOURCE_WEIGHT PROGRAM SPECIFICATION
!
! 1.1 Look in file for new re-weight constants. Weight file
!      format is:
!         col 1-8:    source name
!         remaining columns contain four free-format numbers:
!           1. group delay reweight (picoseconds)
!           2. group delay solution delay rate reweight (femtosec/sec)
!           3. phase delay reweight (picoseconds)
!           4. phase delay solution delay rate reweight (femtosec/sec)
!
! 1.2 REFERENCES:
!
! 2.  GET_SOURCE_WEIGHT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      character*(*) istrn,weight_file
!
! SOURCE_WEIGHT_FILE - Name of file containing source_dependent weights
!
! 2.3 OUTPUT Variables:
!
      real*8 constants(4)
!
! CONSTANTS - New weights read from file
! GET_SOURCE_WEIGHT - True if found, false otherwise
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arcset
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      character*80 cdum
      integer*2 iver,decimaltoint
      INTEGER*4  ierr
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  GET_SOURCE_WEIGHT PROGRAM STRUCTURE
!
      get_source_weight=.false.
      open(65,file=weight_file,iostat=ierr)
      call ferr( INT2(ierr), 'opening'//weight_file, INT2(0), INT2(0) )
!
 121  continue
      read(65,'(A)',iostat=ierr,end=300) cdum
      call ferr( INT2(ierr), 'reading weight file', INT2(0), INT2(0) )
      if(cdum(1:8).eq.istrn) go to 250
      goto 121
!
!  found it, now decode
!
250   continue
      read(cdum(9:),*,iostat=ierr) constants
      call ferr( INT2(ierr), 'decoding weights', INT2(0), INT2(0) )
      get_source_weight=.true.
!
300   continue
      close(65,iostat=ierr)
      call ferr( INT2(ierr), 'closing'//weight_file, INT2(0), INT2(0) )
!
      return
      end
