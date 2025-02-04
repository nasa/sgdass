      SUBROUTINE parminfo(buffer,covs_wanted,partype,want_status)
!
      implicit none
!
!     written by kdb 7/8/96
!
!     purpose: Given an input buffer, identify the type of the input parameter,
!              and whether or not the user wants its covariance.
!
!     restrictions: parminfo is a little loose in its assumption of what's a
!                   site position parameter, but it currently trusts
!                   solest to later check the input buffer and verify that
!                   the buffer's site name is really a site name
!                   (via antenna.dat).  Hopefully any errors will be caught
!                   there.
!
!
!     input arguments:
!
!     buffer - line from CVRFxx containing the parameter in question
!     covs_wanted - bit array containing the type of parameters for which
!                   covariance information is desired
!
      character*(*) buffer
      integer*2 covs_wanted
!
!     output arguments:
!
!     partype - type of parameter (a number, e.g., 1 for site position.
!                                  0 for miscellaneous types currently not
!                                    relevant)
!     want_status - 1 if the parameter's covariance information is desired
!                   0 if not
!
      integer*2 partype,want_status
!     local variables
      logical*2 kbit
      integer*2 inumber, ierr, ifields
      character*6 numcheck
      logical*2 kvalid
!
      if (buffer(16:17).eq.' X'.or.buffer(16:17).eq.' Y'.or. &
     &     buffer(16:17).eq.' Z'.or.buffer(16:17).eq.' U'.or. &
     &     buffer(16:17).eq.' E'.or.buffer(16:17).eq.' N') then
        if (buffer(19:26).eq.'VELOCITY') then !site velocity
          partype = 2
        else  !site position (regular or episodic)
          partype = 1
        endif
      else if (buffer(10:15).eq.'WOBBLE') then
        partype = 3
      else if (buffer(8:14).eq.'UT1-TAI') then
        partype = 4
      else !other
        partype = 0
      endif
      want_status = 0
      if (partype.ne.0) then
        if (kbit(covs_wanted,partype)) want_status = 1
      endif
!
      return
      end
