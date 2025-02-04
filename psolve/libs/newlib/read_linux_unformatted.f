      subroutine read_linux_unformatted(id,ilen,ibuf,ieof)
      implicit none
!
!     Lowest level routine for reading unformatted HP-UX database files
!     into Linux. Have to handle the Endian problem.
!
!     Input:
      INTEGER*2  id      !The unit number to read.
      INTEGER*2  ilen    !The number of I*2 words in the record to be read.

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

      integer*4 ilen4,jlen4,ios,i
      integer*2 ilen1
!
!     Read in the record.
!
      ieof = 0
      read(id,iostat=ios) ilen4,(ibuf(i),i=1,ilen),jlen4
      if(ios.eq.-1) then
        ieof = -1
        return
      endif
      if(ios.ne.0) then 
        write(*,*) "read_linux_unformatted: read error :ios = ",ios
        stop "read_linux_unformatted: error 1"
      endif 
!
!     Do the byte swapping for the Little Endian to Big.
      call endian_swap_i4(ilen4)

!     Convert ilen4 from bytes to words
      ilen4 = ilen4/2
!
      if(ilen4.ne.ilen) then
        write(*,*) "read_linux_unformatted: Wrong number of words read."
        write(*,*) "Was expecting ",ilen, "words, but got ",ilen4," words"
        stop "read_linux_unformatted:2"
      endif      
!
      return
      end
 
