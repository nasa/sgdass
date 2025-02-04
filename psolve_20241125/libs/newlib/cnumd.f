      Subroutine Cnumd(num,buf)
      Implicit none
      Integer*2 num, buf(*)
!
      Integer*2 tnumary(3), tnum, b, d, i
      Character*6 ctnum, ctnumary
      Equivalence (tnumary(1), ctnumary)
!
      ctnumary = ' '
      b = 0
      d = 0
      tnum = num
      write(ctnum,'(i6)') tnum
      do I = 6,1,-1
        if (ctnum(I:I) .eq. ' ') then
          b = b + 1
          ctnumary(b:b) = ' '
        else
          ctnumary(6-d:6-d) = ctnum(i:i)
          d = d + 1
        endif
      enddo
      do i = 1, 3
        buf(i) = tnumary(i)
      enddo
!
      return
      end
