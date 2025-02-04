
      function endian_flip_i2(ihold)
!
!     Does the Big Endian to Little Endian swap (or vice versa) for one
!     i*2 variable.
!     function form of Jim Ryan's endian_swap_i2 subroutine
!       (also has HPUX path to leave variable untouched)
!     :03.08.07 kdb wrote
!
      integer*2 endian_flip_i2
      integer*2 ihold, jhold
      integer*1 i1,j1hold(2)
      equivalence (j1hold,jhold)
 
#ifdef LITTLE_ENDIAN
      jhold = ihold
      i1 = j1hold(1)
      j1hold(1) = j1hold(2)
      j1hold(2) = i1
      endian_flip_i2 = jhold
#else
!     pass back untouched
      endian_flip_i2 = ihold
#endif
 
      return
      end

      function endian_flip_r4(rhold)
!
!     Does the Big Endian to Little Endian swap (or vice versa) for one
!     reak*4 variable.
!     function form of Jim Ryan's endian_swap_r4 subroutine
!       (also has HPUX path to leave variable untouched)
!     :03.08.07 kdb wrote
!
      real*4 endian_flip_r4
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
      endian_flip_r4 = xhold
#else
!     pass back untouched
      endian_flip_r4 = rhold
#endif

      return
      end

      function endian_flip_i4(jhold)

!     Does the Big Endian to Little Endian swap (or vice versa) for one
!     integer*4 variable.
!     function form of Jim Ryan's endian_swap_i4 subroutine
!       (also has HPUX path to leave variable untouched)
!     :03.08.07 kdb wrote
!
      integer*4 endian_flip_i4
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
      endian_flip_i4 = xhold
#else
!     pass back untouched
      endian_flip_i4 = jhold
#endif

      return
      end

      function endian_flip_r8(rhold)      
!     Does the Big Endian to Little Endian swap (or vice versa) for one
!     real*8 variable.
!     function form of Jim Ryan's endian_swap_r8 subroutine
!       (also has HPUX path to leave variable untouched)
!     :03.08.07 kdb wrote
!
      real*8 endian_flip_r8
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

      endian_flip_r8 = xhold
#else
!  pass back untouched
      endian_flip_r8 = rhold
#endif


      return
      end

