      SUBROUTINE search_spool_list (nstart, nlast, npat, qpat, qlist, &
     &                                qtype, ierr)
!
!     Lets user search through list of spool files for a specific pattern
!     in the solution id substring.
!
!     93/03/01, D.S. Caprette Hughes STX
!     93/04/01, D.S. Caprette Hughes STX expanded search to include
!               (optionally) the comment.  Changed search algorith
!               to simpler one that might work.
!     00/10/26, K. Baver range checking is being enabled,
!                        so convert arguments from (1) to (*).
!
!
!     Called subroutines:   none
!
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     Updated to specificaly type integers which
!-------------------------------------------------
!     Input variable:
!
      integer*2      nstart      !starting point for search
      integer*2      nlast       !end of array of spool info
      character*60   qpat        !pattern for search
!
!     Output variable:
!
      integer*2     ierr
      integer*2     npat        !number of spool info record matching pattern
!
!
!
      integer*2    i
      integer*2    ios
      integer*2    plen        !Length of search pattern
      integer*2    tlen        !Length of search pattern
      integer*2    it1, it2    !position in qstring
      integer*2    trimlen
!
      LOGICAL*2 kdone
!
      character*8    qtype          !'solution', or 'comment '
      character*60   qstring        !string from qlist in which to search
!                                   !(solution tag, or comment)
      character*230  qlist(*)       !array of spool info from splist_name
!
!
!     The format of the list file is:
!
!     characters 1-2:  user tag
!               3-10:  solution tag (centered format: that is,
!                                       "   2a   "
!                                       "  15    "
!                                       " 701both"
!                                       "1633rep "  )
!              11-12:  version number  (e.g., " 1")
!             13-167:  full path to spool file (if path is smaller than field,
!                      path is left-justified and blanks fill the remaining
!                      space)
!            168-170:  length of full path (e.g., " 41")
!            171-230:  comment - description of the solution version (e.g.,
!                       that run of the solution)
!                         (left-justified in field, with blanks as filler)
!
!
      ios = 0
      kdone = .false.
      write (qstring, "(60x)")
!
!
!
      plen = trimlen(qpat)
!
      i = nstart
      do while ((.not. kdone).and.(i .lt. nlast))
!
        if (qtype .eq. 'solution') then
          read (qlist(i)(3:10), '(a)') qstring
        else      !  qtype =  'comment '
          read (qlist(i)(171:), '(a)') qstring
        end if
        tlen = trimlen(qstring)
!
        if (tlen .ge. plen) then
          it2 = tlen - plen + 1
          do it1 = 1,it2
            if (qstring(it1:(it1+plen-1)) .eq. qpat) kdone = .true.
          end do
        end if
        i = i + 1
      end do
!
!
      npat = i - 1
      if (kdone .eq. .true.) then
        ierr= 0
      else if (kdone .eq. .false.) then
        ierr= -1  !pattern not found
      end if
!
      return
      end
