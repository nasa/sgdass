
      subroutine endian_swap_i2(ihold)
!
!     Does the Big Endian to Little Endian swap (or vice versa) for one
!     i*2 variable.
!     :03:06:23: Jim Ryan     :
!
      integer*2 ihold, jhold
      integer*1 i1,j1hold(2)
      equivalence (j1hold,jhold)

#ifdef LITTLE_ENDIAN 
      jhold = ihold
      i1 = j1hold(1)
      j1hold(1) = j1hold(2)
      j1hold(2) = i1
      ihold = jhold
#else
!     No operaton
      continue
#endif
      return
      end

      subroutine endian_swap_r4(rhold)
!
!     Does the Big Endian to Little Endian swap (or vice versa) for one
!     reak*4 variable.
!     :03:06:23: Jim Ryan     :
!
      real*4 rhold, xhold
      integer*1 x1,x1hold(4)
      equivalence (x1hold,xhold)

#ifdef LITTLE_ENDIAN
      xhold     = rhold
      x1        = x1hold(1)
      x1hold(1) = x1hold(4)
      x1hold(4) = x1

      x1        = x1hold(2)
      x1hold(2) = x1hold(3)
      x1hold(3) = x1    
      rhold = xhold
#else
!     No operation
      continue
#endif  
      return
      end

      subroutine endian_swap_i4(jhold)

!     Does the Big Endian to Little Endian swap (or vice versa) for one
!     integer*4 variable.
!     :03:06:23: Jim Ryan     :
!
      integer*4 jhold, xhold
      integer*1 x1,x1hold(4)
      equivalence (x1hold,xhold)

#ifdef LITTLE_ENDIAN      
      xhold     = jhold
      x1        = x1hold(1)
      x1hold(1) = x1hold(4)
      x1hold(4) = x1

      x1        = x1hold(2)
      x1hold(2) = x1hold(3)
      x1hold(3) = x1
      jhold = xhold
#else
!     No operation
#endif

      return
      end

      subroutine endian_swap_r8(rhold)      
!     Does the Big Endian to Little Endian swap (or vice versa) for one
!     real*8 variable.
!     :03:06:23: Jim Ryan     :
!
      real*8 rhold, xhold
      integer*1 x1,x1hold(8)
      equivalence (x1hold,xhold)

#ifdef LITTLE_ENDIAN
      xhold     = rhold
      x1        = x1hold(1)
      x1hold(1) = x1hold(8)
      x1hold(8) = x1

      x1        = x1hold(7)
      x1hold(7) = x1hold(2)
      x1hold(2) = x1

      x1        = x1hold(6)
      x1hold(6) = x1hold(3)
      x1hold(3) = x1

      x1        = x1hold(5)
      x1hold(5) = x1hold(4)
      x1hold(4) = x1

      rhold = xhold
#else
!     No operation
      continue
#endif 

      return
      end


      subroutine endian_swap_i2r8(ihold)
      implicit none

!     Does the Big Endian to Little Endian swap (or vice versa) for one
!     real*8 variable stored in a buffer of 4 I*2 variables. This routine
!     is needed for dbase96.
!     :03:06:23: Jim Ryan     :
!
      integer*2 ihold(4), xhold(4)
      integer*1 x1, x1hold(8)
      equivalence (x1hold,xhold)
#ifdef LITTLE_ENDIAN
      xhold(1)     = ihold(1)
      xhold(2)     = ihold(2)
      xhold(3)     = ihold(3)
      xhold(4)     = ihold(4)

      x1        = x1hold(1)
      x1hold(1) = x1hold(8)
      x1hold(8) = x1

      x1        = x1hold(7)
      x1hold(7) = x1hold(2)
      x1hold(2) = x1

      x1        = x1hold(6)
      x1hold(6) = x1hold(3)
      x1hold(3) = x1

      x1        = x1hold(5)
      x1hold(5) = x1hold(4)
      x1hold(4) = x1

      ihold(1) = xhold(1)
      ihold(2) = xhold(2)
      ihold(3) = xhold(3)
      ihold(4) = xhold(4)
#else
!     No operation
      continue
#endif
      return
      end


      subroutine endian_swap_i2i4(ihold)
      implicit none
!
!     Does the Big Endian to Little Endian swap (or vice versa) for one
!     integer*4 variable stored in a buffer of 2 I*2 variables. (This
!     routine is need by dbase96 which stores all types of variables in
!     i*2 arrays. (It's still 1975.)
!     :03:06:23: Jim Ryan     :
!
      integer*2 ihold(2), xhold(2)
      integer*1 x1, x1hold(4)
      equivalence (x1hold,xhold)

#ifdef LITTLE_ENDIAN
      xhold(1)     = ihold(1)
      xhold(2)     = ihold(2)

      x1        = x1hold(1)
      x1hold(1) = x1hold(4)
      x1hold(4) = x1

      x1        = x1hold(3)
      x1hold(3) = x1hold(2)
      x1hold(2) = x1

      ihold(1) = xhold(1)
      ihold(2) = xhold(2)
#else
!     No operation
      continue
#endif
      return
      end



      subroutine endian_swap_buffer(ibuffer,nword4,data_type)
      implicit none
!
!     Does the Big Endian to Little Endian swap (or vice versa) for a 
!     buffer of nword4 integer*2 words:
!
!          i*2 i*2  .... i*2
!           1   2        nword4 
!
!     to be interpreted as a particular data type given by data_type:
!
!        I2 = integer*2
!        I4 = integer*4
!        R4 = real*4
!        R8 = real*8
!
!     :03:12:17: Karen Baver  :

      character*2 data_type
      integer*4 nword4
      integer*2 ibuffer(*)

!     local variables
 
      integer*4 i
      integer*4 i4buf
      real*4 r4buf
      real*8 r8buf
      integer*2 i4hold(2),r4hold(2),r8hold(4)
      equivalence (i4buf,i4hold(1))
      equivalence (r4buf,r4hold(1))
      equivalence (r8buf,r8hold(1))
 
#ifdef LITTLE_ENDIAN
      if (data_type.eq.'I2') then
!       integer*2
        do i = 1,nword4
          call endian_swap_i2(ibuffer(i))
        enddo
      else if (data_type.eq.'I4') then
!       integer*4
        do i = 2,nword4,2
          i4hold(1) = ibuffer(i-1)
          i4hold(2) = ibuffer(i)
          call endian_swap_i4(i4buf)
          ibuffer(i-1) = i4hold(1) 
          ibuffer(i) = i4hold(2) 
        end do
      else if (data_type.eq.'R4') then
!       real*4
        do i = 2,nword4,2
          r4hold(1) = ibuffer(i-1)
          r4hold(2) = ibuffer(i)
          call endian_swap_r4(r4buf)
          ibuffer(i-1) = r4hold(1) 
          ibuffer(i) = r4hold(2) 
        end do
      else if (data_type.eq.'R8') then
!       real*8
        do i = 4,nword4,4
          r8hold(1) = ibuffer(i-3)
          r8hold(2) = ibuffer(i-2)
          r8hold(3) = ibuffer(i-1)
          r8hold(4) = ibuffer(i)
          call endian_swap_r8(r8buf)
          ibuffer(i-3) = r8hold(1) 
          ibuffer(i-2) = r8hold(2) 
          ibuffer(i-1) = r8hold(3) 
          ibuffer(i) = r8hold(4) 
        end do
      endif
#else
!     No operation
      continue
#endif
      return
      end
