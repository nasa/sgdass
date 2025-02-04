      SUBROUTINE wts_bl_oth(bl_wts,iwt_mode,numsta,wts,resid)
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
! on input bl_wts are baseline wts
      Double Precision bl_wts(2,*),wts(2,*)
      INTEGER*2 iwt_mode,numsta
      Double Precision resid(2)
      Double Precision wts_sum
!
!
      INTEGER*2 num_baselines,ibase,idr,istat1,istat2
      integer*2 ibsln(2)
      equivalence(ibsln(1),istat1),(ibsln(2),istat2)
      integeR*2 ibase_index
      double precision temp
      integer*2 istat
!
! from a set of baseli*ne weights extract the following:
! if iwt_mode =0 global weights.
!    iwt_mode =1 station weights
!    iwt_mode =2 baseline weghts
! Thhis is done in a least squares sense.
! if iwt_mode is 0, just do average.
!    iwt_mode is 2, just do copy.
!    iwt_mode is 1, the least squares process is a little more involved.
! but still amenable to solution in closed form.  In this case
! the least squares equations reduce to solution of:
!     N wts= V
! where V(j) = sum(wts) where sum is over all wts involving station j.
!     N = (numsta-2)*I+K
!  where K is the matrix consisting of all 1's.
! e.g. numsta=3:      1 0 0        1 1 1
!                 N=  0 1 0     +  1 1 1
!                     0 0 1        1 1 1
! using the fact that K^2=numsta*K, we have:
!  N^-1=1/(numsta-2)*(I - 1/(2*(numsta-1) K)
!
!
      num_baselines= (numsta*(numsta-1))/2
! extract weights for delay and rate.
      do idr=1,2
        resid(idr)=0.
        IF(iwt_mode .eq.0) then
          wts_sum=0.
          do ibase=1,num_baselines
            wts_sum=wts_sum+bl_wts(idr,ibase)**2.
          end do
          wts(idr,1)=sqrt(wts_sum/num_baselines)
          do ibase=1,num_baselines
              resid(idr)=resid(idr)+ &
     &           (wts(idr,1)**2.-bl_wts(idr,ibase)**2.)**2.
          end do
          resid(idr)=SQRT(resid(idr)/num_baselines)
        ELSEIF(iwt_mode .EQ. 2) then
          do ibase=1,num_baselines
           wts(idr,ibase)=bl_wts(idr,ibase)
          end do
        ELSEIF(iwt_mode .EQ. 1) then
! special case: numsta=2
          IF(numsta .EQ. 2) then
             wts(idr,1)=bl_wts(idr,1)/sqrt(2.)
             wts(idr,2)=bl_wts(idr,1)/sqrt(2.)
             resid(idr)=0.
          ELSE
! make V vector defined above. Store it in space for V
            do istat=1,numsta
              wts(idr,istat)=0.
            end do
            wts_sum=0.
            do istat1=1,numsta
            do istat2=istat1+1,numsta
               temp=bl_wts(idr,ibase_index(ibsln,numsta))**2.
               wts(idr,istat1)=wts(idr,istat1)+temp
               wts(idr,istat2)=wts(idr,istat2)+temp
               wts_sum=wts_sum+temp
            end do
            end do
            do istat=1,numsta
              wts(idr,istat)=1./(numsta- &
     &               2)*(wts(idr,istat)-wts_sum/(numsta-1))
              if(wts(idr,istat) .lt. 0) wts(idr,istat)=0.
              wts(idr,istat)=sqrt(wts(idr,istat))
            end do
! have station weights. Compute residuals.
            do istat1=1,numsta
            do istat2=istat1+1,numsta
               temp=bl_wts(idr,ibase_index(ibsln,numsta))**2.
               resid(idr)=resid(idr) &
     &          +(temp-wts(idr,istat1)**2-wts(idr,istat2)**2)**2.
            end do
            end do
            resid(idr)=SQRT(resid(idr)/num_baselines)
          ENDIF
        ENDIF
      ENDDO
      return
      end
