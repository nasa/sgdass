      Double Precision function func(wts)
! this calculates the function we whant to minimize:
!    sum(over wts)  (chi_sq_wt-1./chi_sq_wt)^2+other terms.
! where
!     chi_Sq_wt = (Sum(resid_sq/(sig_sq+bl_wts_sq))/num_terms
!
!     The sum is over all observations of a given weights.
!     This term tries to make chi_sq per weight equal to 1.
!
!     The other terms are there to make the weights positive (exp-wt),
!                                     and to keep them  small (wts^2).
!
!     The first term should get close to 0.
!
! The baseline weights are derived from the input weights.
! The input weights can be either
!     global weights
!     station weights
!     baseline weights.
!
! The second term tries to spread the error around the system.
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
! this is passed.  These are current adjustments
!
      Double Precision wts(2,*)
      INTEGER*4 iobs,iwt
!
      INCLUDE "solve.i"
      INCLUDE "reway.i"
      INTEGER*2 ibase_index
!
      double precision tmp
      INTEGER*2 ibl_ptr
      integer*2 idr,iend
      INTEGER*2 iwt_ptr
      LOGICAL*4 IS_R8_NAN
      double precision chi_sq_tot
      double precision sig_sq_ratio
      double precision temp_f,temp_c,temp
!
!
      call wts_oth_bl(wts,nstat,iwt_mode,bl_wts_sq )
!
!
      alpha=2.
!
!
! start to compute Chi_square per weight.
      do idr=1,2
! initialize sum to 0.
        if(kuse(idr)) then
          do iwt=1,num_wts
            chi_sq(idr,iwt)=0.
            num_obs_chi(idr,iwt)=0
            chi_dof(idr,iwt)=0.
          end do
        do iobs=1,nobs
          ibl_ptr=ibase_index(ibsln(1,iobs),nstat)
          if ( sig_raw_sq(idr,iobs) + bl_wts_sq(idr,ibl_ptr) > 1.d-20 ) then
               tmp=res(idr,IOBS)**2/(sig_raw_sq(idr,iobs) + bl_wts_sq(idr,ibl_ptr))
            else 
               tmp = 1.d-9
          end if
          sig_sq_ratio=sig_fact_sq(idr,iobs)
          if(iwt_mode .eq. 0) then
            iwt_ptr=1
            chi_sq(idr,iwt_ptr)=chi_sq(idr,iwt_ptr)+tmp
            num_obs_chi(idr,iwt_ptr)=num_obs_chi(idr,iwt_ptr)+1
            chi_dof(idr,iwt_ptr)=chi_dof(idr,iwt_ptr)+sig_sq_ratio
          else if(iwt_mode .eq. 2) then
            iwt_ptr=ibl_ptr
            chi_sq(idr,iwt_ptr)=chi_sq(idr,iwt_ptr)+tmp
            num_obs_chi(idr,iwt_ptr)=num_obs_chi(idr,iwt_ptr)+1
            chi_dof(idr,iwt_ptr)=chi_dof(idr,iwt_ptr)+sig_sq_ratio
          else if(iwt_mode .eq. 1) then
            do iend=1,2
              iwt_ptr=ibsln(iend,iobs)
              chi_sq(idr,iwt_ptr)=chi_sq(idr,iwt_ptr)+tmp
              num_obs_chi(idr,iwt_ptr)=num_obs_chi(idr,iwt_ptr)+1
              chi_dof(idr,iwt_ptr)=chi_dof(idr,iwt_ptr)+sig_sq_ratio
            end do
          endif
        enddo
        endif
      enddo
!
!
      func=0.
      chi_sq_tot=0.
      do idr=1,2
        avg_wt(idr)=0.
        avg_wt_sq(idr)=0.
        if(kuse(idr)) then
          do iwt=1,num_wts
            if(num_obs_chi(idr,iwt) .ne. 0) then
              chi_sq(idr,iwt)=chi_sq(idr,iwt)/chi_dof(idr,iwt)
              chi_sq_tot=chi_sq_tot+chi_sq(idr,iwt)
              avg_wt_sq(idr)=avg_wt_sq(idr)+wts(idr,iwt)**2.
              avg_wt(idr)   =avg_wt(idr) +  wts(idr,iwt)
!
!
              temp=wts(idr,iwt)-wt_floor(idr)
              if(temp .gt. 0) then
                temp_f=0.
              else
                temp_f=alpha*temp*temp
              endif
!
!
              temp=wt_ceiling(idr)-wts(idr,iwt)
              if(temp .gt. 0) then
                temp_c=0.
              else
                temp_c=alpha*temp*temp
              endif
!
              if ( IS_R8_NAN(chi_sq(idr,iwt)) ) chi_sq(idr,iwt) = 1.d-15
              if ( IS_R8_NAN(chi_dof(idr,iwt)) ) chi_dof(idr,iwt) = 1.d-15
!
              if ( chi_sq(idr,iwt) .lt. 1.d-15 ) chi_sq(idr,iwt) = 1.d-15
              func=func+chi_dof(idr, &
     &                iwt)*(chi_sq(idr,iwt)-1./chi_sq(idr,iwt))**2. &
     &            +temp_f+temp_c
           endif
          end do
          avg_wt_sq(idr)=avg_wt_sq(idr)/num_wts
          avg_wt(idr)=avg_wt(idr)/num_wts
          func=func+beta*(avg_wt_sq(idr)-avg_wt(idr)** &
     &            2.)/way_scale(idr)**2.
        endif
      end do
!
!
      func=func/2.
      chi_sq_tot=chi_sq_tot/num_wts
      return
      end
      SUBROUTINE  dfunc(wts,grad)
! this calculates the gradient of the function we want to minimize. See it's defintion above.
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
! this is passed.  These are current adjustments
      Double Precision wts(2,*),grad(2,*)
      INTEGER*2 iobs
      DOUBLE PRECISION tmp
!
!
      INCLUDE "solve.i"
      INCLUDE "reway.i"
      INTEGER*2 ibase_index
! INITIALIZE
      INTEGER*2 iwt,idr,iwt_ptr
      INTEGER*2 iptr1,iptr2,ibl_ptr
      double precision temp_f,temp_c,temp
!
!
! start to compute gradients
      do idr=1,2
! initialize sum to 0.
        do iwt=1,num_wts
         if(kuse(idr)) then
!
!
!     Updated to specificaly type integers which
!-------------------------------------------------
           temp=wts(idr,iwt)-wt_floor(idr)
           if(temp .gt. 0) then
             temp_f=0.
            else
             temp_f=alpha*temp
            endif
!
!
            temp=wt_ceiling(idr)-wts(idr,iwt)
            if(temp .gt. 0) then
              temp_c=0.
            else
              temp_c=-alpha*temp
            endif
!
!
          grad(idr,iwt)=temp_f+ &
     &    temp_c+beta*(wts(idr,iwt)-avg_wt(idr))/(num_wts*way_scale(idr)**2.)
         else
          grad(idr,iwt)=0.
         endif
        end do
!
!
        if(kuse(idr)) then
        do iobs=1,nobs
          ibl_ptr=ibase_index(ibsln(1,iobs),nstat)
          tmp=-res(idr,IOBS)** &
     &        2/(sig_raw_sq(idr,iobs)+bl_wts_sq(idr,ibl_ptr))**2.
          if(iwt_mode .eq. 0) then
            iwt_ptr=1
            grad(idr,iwt_ptr)=grad(idr, &
     &          iwt_ptr)+ tmp*wts(idr, &
     &      iwt_ptr)*(chi_sq(idr,iwt_ptr)-1./chi_sq(idr,iwt_ptr)**3.)
         else if(iwt_mode .eq. 2) then
            iwt_ptr=ibl_ptr
            grad(idr,iwt_ptr)=grad(idr, &
     &          iwt_ptr)+ tmp*wts(idr, &
     &        iwt_ptr)*(chi_sq(idr,iwt_ptr)-1./chi_sq(idr,iwt_ptr)**3.)
         else if(iwt_mode .eq. 1) then
            iptr1=ibsln(1,iobs)
            iptr2=ibsln(2,iobs)
            tmp=tmp*( &
     &         (chi_sq(idr,iptr1)-1./chi_sq(idr,iptr1)**3.)+ &
     &         (chi_sq(idr,iptr2)-1./chi_sq(idr,iptr2)**3.))
            grad(idr,iptr1)=grad(idr,iptr1)+tmp*wts(idr,iptr1)
            grad(idr,iptr2)=grad(idr,iptr2)+tmp*wts(idr,iptr2)
         endif
        enddo
        endif
      enddo
!
!
      return
      end
