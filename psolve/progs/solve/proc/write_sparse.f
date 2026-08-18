      SUBROUTINE write_sparse(Cmat,nparm)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
! write out a sparse matrix.
!     most of C's elements are 0.  Write out only non-zero ones.
      double precision cmat(*)
      INTEGER*2 nparm,iparm,jparm
      integer*4 indx4,iptr
      character*63 fname
      INCLUDE 'precm.i'
!
      fname   = PRE_SCR_DIR(:PRE_SD_LEN)//'CSPR'//PRE_LETRS
!
!
      open(66,file=fname)
!
!
      do iparm=1,nparm
        do jparm=iparm,nparm
           iptr=indx4(iparm,jparm)
           if(cmat(iptr) .ne. 0) then
              write(66,*) iparm,jparm, cmat(iptr)
           endif
        end do
      end do
! this is a flag indicating we are done.
      write(66,*) 0,0, 0.
      close(66)
      return
      end
