      REAL*4 FUNCTION REIO (mode,idevice,ibuf,length)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 mode, idevice, length
!-----END of imp added lines.
!
!
!
! Replacement for REIO exec call on A900.  Reads in a character string and
! passes it back as a Hollerith string.  The function value returned is
! a real*4 with an integer representing the number of characters read in
! saved in the lower 2 bytes.  Devices: stdin = 5, stdout = 6.
!
!      -P. Ryan    12.5.88
!  2004.04.13  Replaced definition ibuf(1) with ibuf(*)
!
!
       character*256 cr
!        - cr   : character string
       integer*2     i,ibuf(*),trimlen,icrl,ir(2), ifc
       INTEGER*4  ios
       real*4        rex
!
       equivalence(ir(1),rex)
!
!  check whether the length is in words or characters
!  + for words, - for characters
!
       if (length .lt. 0) then ! characters
         icrl = -length
       else
         icrl = length*2 ! words
       end if
!
!
!  read or write?    1=read, 2=write
!
       if (mode .eq. 1) then
         ios = -1
         do while (ios .ne. 0)
           read(idevice,'(A)',iostat=ios) cr
           if (ios .ne. 0) then
             print*, 'error: re-enter data',ios
           end if
         end do
         i = trimlen(cr)
         call char2hol( cr, ibuf, INT2(1), i )
       else ! mode == 2
         ifc = 1
         call hol2char (ibuf,ifc,icrl,cr )
         write(idevice,20) cr(1:icrl)
20     format (A)
       end if
       ir(2) = i
       reio  = rex
       end
