      SUBROUTINE get_avg_atm(cdbnam,avg_atm)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GET_AVG_ATM PROGRAM SPECIFICATION
!
! 1.1 Look in file for average atmosphere values by station
!
! 1.2 REFERENCES:
!
! 2.  GET_AVG_ATM INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      character*(*) cdbnam
!
! 2.3 OUTPUT Variables:
!
      real*8 avg_atm(4,*)
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arcset
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      character*80 cdum,token,val_string
      integer*2 iver,decimaltoint,i,ict,trimlen
      INTEGER*4  IERR
      real*8 val
      character*8 cursta
      character*10 curnam
      character*54 errbuf
      logical*2 nbl_found
!
! 4.  HISTORY
!   modified
!
!   WHO   WHEN   WHAT
!   kdb  950831  Add atmospheric turbulence rate constant and mapping function
!                parameter error delay and rate constants.
!                Better error handling (trap typos, catch the wrong number
!                of values (too few or too many)).
!   kdb  950905  Add the ability to specify a single set of values for all
!                sites in a database.
!                Allow lowercase site names in file.
!   kdb  980123  New feature: $ALLDATABASES can be used to specify a given set
!                of values for all databases rather than requiring users to
!                specify each database separately.
!                (Note that exceptions to this can be specified by
!                   placing those databases' information before the
!                   $ALLDATABASES information).
!
! 5.  GET_AVG_ATM PROGRAM STRUCTURE
!
      do i=1,MAX_ARC_STA
        do ict = 1,4
          avg_atm(ict,i) = 0.d0
        enddo
      enddo
      if (eldep_file.eq.' ') return
      open(65,file=eldep_file,iostat=ierr)
      CALL FERR( INT2(IERR), 'opening'//eldep_file, INT2(0), INT2(0) )
!
!     find the right database in the elevation dependent file
!     (or the $ALLDATABASES notation, which matches any database)
!
      curnam = ' '
      do while (curnam.ne.cdbnam)
        read(65,'(A)',END=121) cdum
        if (cdum.eq.'$ALLDATABASES') then
!         The next set of information applies to all databases.  Use it.
          curnam = cdbnam
        else
!         Found information for a specific database.  Prepare to test it
!         against the requested database.
          curnam = cdum(1:10)
        endif
      enddo
 121  continue
      if (curnam.ne.cdbnam) return
!
!     read the site information for the located database
!
      read(65,'(A)',iostat=ierr,end=250) cdum
      call ferr( INT2(ierr), 'reading eldep file', INT2(0), INT2(0) )
      do while (cdum(1:1).ne.'$')
        cursta = cdum(2:9)
        call casefold(cursta )
        val_string = cdum(10:)
        nbl_found = .false.
        ict = 1
        do while (ict.le.4)
          call splitstring(val_string,token,val_string )
          if (trimlen(token).ne.0) then
            nbl_found = .true.
            read(token,*,iostat=ierr) val
            if ( INT2(ierr) .ne. 0 ) then
              errbuf = 'non-numeric val for '//cdbnam//': '// &
     &              cursta//' in eldep file'
              call ferr( INT2(ierr), errbuf, INT2(0), INT2(0) )
            endif
            do i=1,numsta
              if (isitn_chr(i).eq.cursta.or.cursta.eq.'ALLSITES') then
                avg_atm(ict,i) = val
              endif
            enddo
            ict = ict + 1
            if (ict.eq.5) then
              call splitstring(val_string,token,val_string )
              if (trimlen(token).ne.0) then
                errbuf = 'too many values for '//cdbnam//': '// &
     &            cursta//' in eldep file'
                call ferr( INT2(225), errbuf, INT2(0), INT2(0) )
              endif
            endif
          else
            ict = 5
          endif
        enddo
        if (.not.nbl_found) then
          errbuf = 'no values given for '//cdbnam//': '// &
     &          cursta//' in eldep file'
          call ferr( INT2(234), errbuf, INT2(0), INT2(0) )
        endif
        read(65,'(A)',END=250) cdum
      enddo
250   continue
      close(65,iostat=ierr)
      call ferr( INT2(ierr), 'closing'//eldep_file, INT2(0), INT2(0) )
!
      return
      end  !#!  GET_AVG_ATM  #!#
