       SUBROUTINE backf(iunit,ierr)
!
!      Backspace a sequential unformatted file written by writf,
!        which writes a record indicating the real record's length,
!        then writes the real record.
!      A returned error of -50 indicates problems--the file is
!        now positioned within a pair of length and data records,
!        instead of at the beginning
!
       IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
       integer*2 iunit
       INTEGER*4  ierr
!
       backspace(iunit,err=100,iostat=ierr)
 100   if (ierr.eq.0) then
         backspace(iunit,err=110,iostat=ierr)
 110     if (ierr.ne.0) ierr=-50
       end if
       return
       end
