      SUBROUTINE reorder_bl_wts(bl_wts,ibsln,numsta,imode,bl_wts_out)
! on input rewts are baseline wts
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
      Double Precision bl_wts(2,*),bl_wts_out(2,*)
      INTEGER*2 ibsln(2,*)
      INTEGER*2 numsta
!
      INTEGER*2 num_baselines,ibase,idr
      integeR*2 ibase_index
      integer*2 imode
!
! Restore baseline order that given by ibsln,i.e., namfile order.
! or vice versa.
!     imode=1  take from namfil mode to following order:
!                     (2,1)   (3,1),(3,2), (4,1),(4,2),(4,3)
! etc
!
      num_baselines= (numsta*(numsta-1))/2
! extract weights for delay and rate.
!
! jwr 2002.12.17 Two incorrect calls to ibase_index fixed by adding
!                numsta in the argument list.  Hope it is right.
!                Sleeping bug, but why should it have slept.
!
      do idr=1,2
      do ibase=1,num_baselines
      if(imode .eq. 1) then
        bl_wts_out(idr,ibase_index(ibsln(1,ibase),numsta))=bl_wts(idr,ibase)
      else
        bl_wts_out(idr,ibase)= bl_wts(idr,ibase_index(ibsln(1,ibase),numsta))
      endif
      end do
      end do
      return
      end
