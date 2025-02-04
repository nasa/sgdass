       SUBROUTINE readf_asc (iunit,kerr,ibuf,ibl,il)
       IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!  ASCII only version of READF
!  Input:
       integer*2 iunit
!        -iunit : logical unit for reading
       integer*2 kerr
!        -kerr  : variable to return error on input (nonzero if error)
       integer*2 ibl
!        -ibl : buffer length
!
!  Output:
       integer*2 il
!        -il    : number of characters read in
       integer*2 ibuf(*)
!        -ibuf  : integer buffer for reading
!
!  Local:
       character*256 ch
!        -ch    : character buffer for initial input
       integer*2     trimlen
!        -trimlen : find number of character read in
       integer*4 k
!
!        -k : variable for iostat error-checking
!
!  880523  -written by P. Ryan
!  2004.04.13  Replaced definition ibuf(1) with ibuf(*)
!
!
       read(iunit,10,iostat=k) ch
10     format (A256)
       kerr = k
       il   = trimlen(ch)
       if (kerr .ne. 0) then
         il = -1
       endif
       call char2hol ( ch, ibuf, INT2(1), il )
       if (il .ne. -1) then
         il = (il+1)/2  ! changes to number of memory words
       end if
       return
       end
