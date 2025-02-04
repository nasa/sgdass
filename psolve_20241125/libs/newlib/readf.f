      SUBROUTINE READF ( IUNIT, KERR, IBUF, IL, ILN )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!  Input:
!        iunit   : logical unit for reading
!        kerr    : variable to return error on input (nonzero if error)
!        il      : buffer length in 16-bit words.
!
!  Output:
!        ibuf    : integer buffer for reading
!        ILN     : number of I*2 words to be read.
!
!  Local:
!        k       : error return
!
!  880523  written by P. Ryan
!  880614  revised to handle binary unformatted records by L. Floyd
!  2004.04.13  Replaced definition ibuf(1) with ibuf(*)
!  040323       jwr:  Bug in handling error return for Linux read fixed.
!  2004.10.19   pet   Replaced name read_linux_unformatted with 
!                     read_endian_unformatted
!     050729 kdb  The code is still having problems processing the case where
!                 the caller asks to read less data than is in a record.
!                 In this case, the program will execute a second, unwanted
!                 read to finish reading the data.  This in turn seems to
!                 mess up reads of subsequent data, either because of
!                 a problem in tracking records or because of problems in
!                 the way the caller tracks the reading of records.
!                 To get around this, this program will now read all actual
!                 data in the record into a buffer, then dump the requested
!                 amount into the output buffer.
!
       integer*2 iunit, kerr, ibuf(*), il, ILN, ilread
       integer*2 il_disk_read, il_return
       integer*4 k, i, ieof
!
! ---- Read in record descriptor. It contains the record length
!
#ifdef LINUX
       il_disk_read = 1 
       il_return = 1 
       call read_endian_unformatted(iunit,il_disk_read,il_return,ilread,ieof)
       if(ieof.eq.-1) then
         kerr = 0
         ILN = -1
         return
       endif
       call endian_swap_i2(ilread)
#else
       read(iunit,iostat=k,err=30,end=20) ilread
#endif
       if(ilread.lt.il) then
         ILN = ilread
       else
         ILN = il
       endif
!
! ---- Read the 2nd record to get the body of the record
!
#ifdef LINUX
!      Note:
!         il_disk_read - number of words to read from disk
!         il_return - number of words to return to this sub (and the caller)
!
       il_disk_read = ilread
       il_return = iln
       call read_endian_unformatted(iunit,il_disk_read,il_return,ibuf,ieof)
       if(ieof.ne.0) then
         stop "read_linux_unformatted - bad eof"
       endif
       kerr = 0
       return
#else
       read(iunit,iostat=k,err=30,end=20) (ibuf(i),i=1,ILN)
       kerr = k
#endif
       return
 20    CONTINUE 
       ILN = -1                 ! comes here for end of file
       kerr = 0
       return
 30    CONTINUE 
       kerr = k                 ! comes here for errors on HP read
       return
       end
