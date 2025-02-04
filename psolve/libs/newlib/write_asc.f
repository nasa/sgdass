       SUBROUTINE writf_asc (iunit,kerr,ibuf,ilc)
       IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 il
!-----END of imp added lines.
!
!
!  ASCII only version of WRITF
!  Input:
       integer*2 iunit
!        iunit : logical unit for writing
       integer*2 kerr
!        kerr  : variable to return error on output (nonzero if error)
!
!  Output:
       integer*2 ilc
!        ilc   : number of memory words to write
       integer*2 ibuf(*)
!        ibuf  : integer buffer for reading
!
!  Local:
       character*256 ch
!        ch    : character buffer for initial input
       integer*4 k
!        k     : variable for iostat errorchecking
       integer*2 j
!        j     : number of *characters*
!
!  880523  written by P. Ryan
!  2004.04.13  Replaced definition ibuf(1) with ibuf(*)
!
!
       j = 2*ilc
       call hol2char ( ibuf, INT2(1), j, ch )
       write (iunit,10,iostat=k) ch(1:j)
10     format (A)
       kerr = k
       if (kerr .ne. 0) then
         il = -1
       endif
       return
       end
