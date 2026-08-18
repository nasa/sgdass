      SUBROUTINE get_file_gen(item_type,use_file,file_found)
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
      character*1   item_type
      INTEGER*2      num_spools
      INTEGER*4     ios
      CHARACTER*157 hold_name
      CHARACTER*79  qstr
      LOGICAL*2 file_found
      CHARACTER*157 use_file
      character*10 qcat
      integer*2 qlen
!
!     modifications:
!
!     95/07/12 kdb Add ability to use input covariance and control files
!                  that are not catalogued in a solution archiving system.
!     98/08/03 kdb Fix: check file existence by opening to lu 107, not lu 40.
!                  The problem was that 40 is the lu for spoolfile access,
!                  so this subroutine left the spool file open to lu 40.
!                  However, the subroutine would close lu 40 to the covariance
!                  and control files, so that if the spoolfile were opened
!                  first, it didn't stay open, causing later errors.
!
      file_found = .false.
      num_spools = 0
  100 write (hold_name,"(60x)")
      if (item_type.eq.'S') then
        qcat = 'spool'
        qlen = 5
      else if (item_type.eq.'V') then
        qcat = 'covariance'
        qlen = 10
      else if (item_type.eq.'C') then
        qcat = 'control'
        qlen = 7
      endif
      qstr = " Name (with path) of "// &
     &        qcat(1:qlen)//" file (:: if done)?"
      call asnl(qstr )
      call getstr_f(hold_name )
      call nl_mn()
      IF(num_spools .EQ. 0  .and. hold_name(1:2) .EQ. '::') then
        STOP
      else if (hold_name.NE.'::') THEN
        use_file = hold_name
        num_spools = num_spools + 1
!
!       Open the file, checking for its existence.
!
        open(107,IOSTAT=IOS,file=use_file,STATUS='OLD')
        IF (IOS.NE.0) THEN
          write(qstr ,'("IOS from open is ",I5)') IOS
          call as2nl(qstr )
          GO TO 100
        ELSE
          qstr = use_file(1:79)
          call as2nl(qstr )
          qstr = 'appears valid'
          call asnl(qstr )
          file_found = .true.
          close(107)
        ENDIF
      ENDIF
!
      Return
      END
