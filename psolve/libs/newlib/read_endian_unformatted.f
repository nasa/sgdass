      subroutine read_endian_unformatted(id,il_disk_read,il_return,ibuf,ieof)
      implicit none
!
!     Lowest level routine for reading unformatted HP-UX database files
!     into Linux. Have to handle the Endian problem.
!
!     Input:
      INTEGER*2  id      !The unit number to read.
!     INTEGER*2  ilen    !The number of I*2 words in the record to be read.
      INTEGER*2  il_disk_read !The number of I*2 words to be read from disk.
      INTEGER*2  il_return !The number of I*2 words to be returned to the caller

!     Output:
      INTEGER*2 IBUF(*)  !The output buffer.
      integer*4 ieof     ! -1 for eof, otherwise 0
     
!     Created by Jim Ryan June 2003
!
!     modifications:
!
!     031210 kdb Initialize ieof to 0.  Otherwise, the LINUX INTEL compiler
!                will set ieof to a random number, causing the calling sub 
!                (readf) to think it's found a bad eof and abort.
!     050623 kdb  There is an error in the check to verify that the data
!                 records are the expected length.  A false error can sometimes
!                 be found when checking the second record of each
!                 data length/data value pair:
!
!                        record 1:  1  data_length 1
!                        record 2: data_length   actual_data  data_length
!                 because read_unformatted_endian and its caller readf
!                 track the data_length value from record 1 in a variable
!                 that also sets the amount of the data requested from 
!                 record 2, and a change in the request creates an error in
!                 the length checking.
!                 An effort was made to track the requested length and the
!                 verification length separately, but then it was discovered
!                 that some ordinary, otherwise usable databases have 
!                 data errors (for example, record 1 values of the form
!                    0 data_length 1).  So length checking will be disabled 
!                 for now to eliminate false error flagging and allow useful
!                 databases to be processed.
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
!     050818 kdb  Linux reads each pair of database records as
!                            1   n    1
!                            n   ~~~~~ (n words)   n
!                 At C. Ma's request, checking will be put back in to flag
!                 errors within pairs of database records.  Specifically,
!                 the code will check the integrity of the "bookend" length
!                 values (1 in the first record and n in the second record).
!                 Because these errors are believed to be numerous and benign, 
!                 the program will flag them but will not terminate.  
!                 Instead, the user will need to have the errors investigated 
!                 after the processing finishes.

      integer*4 ilen4,jlen4,ios,i
      integer*2 ilen1
      integer*2, allocatable :: internal_ibuf(:)
      character*1 flag_bookend_errors
      character*1 bookend_response
      data flag_bookend_errors /'y'/
      save flag_bookend_errors
!
      ieof = 0
!
!     Read the record into the internal array.
!
      allocate(internal_ibuf(il_disk_read))
      read(id,iostat=ios) ilen4,(internal_ibuf(i),i=1,il_disk_read),jlen4
      if(ios.eq.-1) then
        ieof = -1
        return
      endif
      if(ios.ne.0) then 
        write(*,*) "read_endian_unformatted: read error :ios = ",ios
        stop "read_endian_unformatted: error 1"
      endif 
!
!     Transfer from the internal array to the output buffer.
!
      do i = 1,il_return
         ibuf(i) = internal_ibuf(i)
      end do
!
!     Do the byte swapping for the Little Endian to Big.
      call endian_swap_i4(ilen4)
      call endian_swap_i4(jlen4)

!     Convert ilen4 from bytes to words
      ilen4 = ilen4/2
      jlen4 = jlen4/2
      if(ilen4.ne.il_disk_read .or. jlen4.ne.il_disk_read) then
        if (flag_bookend_errors.eq.'y') then
          write(*,*) "read_endian_unformatted: bookend length value(s) wrong "
          write(*,*) "Was expecting ",il_disk_read, " (buffer) ", il_disk_read
          write(*,*) "  but got ",ilen4," (buffer) ",jlen4
          write(*,*) "This problem is probably benign, so I will not stop," 
          write(*,*) "but you should check your operation when you are done."
          bookend_response = ' '
          do while (bookend_response.ne.'C'.and. &
     &              bookend_response.ne.'S')
            write(6,'("(C)ontinue or (S)top flagging bookend problems? ")')
            write(6,'(" Note: (S)top will turn off flagging for the  ", &
     &                " rest of this program run.")') 
            read(5,"(a1)") bookend_response
            call casefold (bookend_response)
            if (bookend_response.eq.'S') flag_bookend_errors = 'n'
          end do
        endif      
      endif

!
      deallocate(internal_ibuf)
      return
      end
 
