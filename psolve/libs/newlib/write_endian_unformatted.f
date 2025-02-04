      subroutine write_endian_unformatted(id,ilen,ibuf)
      implicit none
!
!     Lowest level routine for writing unformatted database files with 
!     Endian/Lahey fortran on a PC. Doing the Endian swapping so the files
!     will look like HP files.
!
!     Input:
      INTEGER*2  id      !The unit number to read.
      INTEGER*2  ilen    !The number of 16-bit words to be written.
      INTEGER*2  ibuf(*) !The buffer to be written out. 
!     Created by Jim Ryan September 2003

      integer*4 ilen4,ios,i
      integer*1 ibyte(8)
!
!     The record must contain the number of BYTES writen as an I*4 number
!     at the beginning and the end of the record.
!
      ilen4 = ilen*2
!     Do the byte swapping for theBig Endian to Little.
      call endian_swap_i4(ilen4)
!
!     Write the record.
      write(id,iostat=ios) ilen4,(ibuf(i),i=1,ilen),ilen4
      if(ios.ne.0) then 
        write(*,*) "write_endian_unformatted: write error :ios = ",ios
        stop "write_endian_unformatted: error 1"
      endif 
!
      return
      end
