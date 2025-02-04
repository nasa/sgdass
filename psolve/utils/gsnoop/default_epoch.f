      Subroutine default_epoch
!
!     Creates tables of annual positions
!
      IMPLICIT NONE
      INCLUDE 'gsnoop_com.i'
      integer*2 i,j, k
      integer*2 len, trimlen
      character*80   qstr
!
!     When annual positions were not found in the spool file, use the
!     first site reference epoch as the default.
!
      write (qstr , "('Using ',3(i2,x),'as the default epoch.')") &
     &       (irefdate(k,1),k=1,3)
      call as2nl(qstr)
      do k=1,nsite
        read (site_names(k)(19:23),"(i5)") i
        do j = 1,3
          xyz_epoch(j,1,i)     = xyz(j,i)
          xyz_epoch_sig(j,1,i) = xyz_sig(j,i)
        end do
      end do
      epoch_count = 1
      epochs(1) = irefdate(1,1)
!
      end
