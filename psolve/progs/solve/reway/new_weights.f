      SUBROUTINE new_weights(bl_wts_nf)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     Updated to specificaly type integers which
!-------------------------------------------------
!
      INCLUDE 'solve.i'
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'reway.i'
! Solve for new weights.
! The idea is that these new weights are used in a new solve solution immediately.
!
      double precision bl_wts_nf(2,*)
      real*8 wts(2,max_bl)
      double precision chi_SqX(2,max_bl)       !Expectation of Chi_sq
      double precision chi_Sq0(2,max_bl)       !Actual Chi_Sq
      double precision del_chi_sq(2,max_bl)    !Difference between actual and expected residual square.
      double precision afac(2,max_bl)
      double precision del_wt_sq(2,max_bl)     !Change in weights.
!
!
      integer*4 iobs
      double precision sig_sq
      INTEGER*2 num_bl
      integer*2 ibl_vec(2)
      integer*2 istat,jstat
      equivalence (ibl_vec(1),istat)
      equivalence (ibl_vec(2),jstat)
!
      double precision bvec(nstat),amat(nstat*(nstat+1)/2)
      double precision resid(2)
      integer*2 ibase_index
      integer*4 indx4
      double precision chi_sq0_tot,chi_sqX_tot
      double precision fudge/1.0/
!
!
      integer*2 idr,ibl,id_ptr,iwt
! convert baseline weigths to other kinds of weights:
      call wts_bl_oth(bl_wts_nf,iwt_mode,numsta,wts,resid )
      call wts_oth_bl(wts,nstat,iwt_mode,bl_wts_sq )
!
!
! Compute difference between
      chi_sq0=0.
      chi_sqX=0.
      afac=0.
      chi_Sq0_tot=0.
      chi_Sqx_tot=0.
      do idr=1,2                       !Start of IDR loop.
! loop over all observations.
      do iobs=1,nobs
        ibl=ibase_index(ibsln(1,iobs),nstat)
        sig_sq= (sig_raw_sq(idr,iobs)+bl_wts_sq(idr,ibl))      !weight of this observation
        chi_sq0(idr,ibl)=chi_sq0(idr,ibl)+res(idr,iobs)**2/sig_sq
        chi_sqX(idr,ibl)=chi_sqX(idr,ibl)+sig_fact_sq(idr,iobs)
         afac(idr,ibl)=afac(idr,ibl)+sig_fact_sq(idr,iobs)/sig_sq
      end do
! compute changes to wts.  Two simple cases first.
      del_chi_sq=chi_sq0-chi_sqX
      num_bl=nstat*(nstat-1)/2
      IF(iwt_mode .eq. 0) then
        do ibl=2,num_bl
          del_chi_sq(idr,1)=del_chi_sq(idr,1)+del_chi_sq(idr,ibl)
          afac(idr,1)=afac(idr,1)+afac(idr,ibl)
        end do
        del_wt_sq(idr,1)=del_chi_sq(idr,1)/afac(1,idr)
      else if(iwt_mode .eq. 2) then
        chi_sq0_tot=0.
        chi_sqx_tot=0.
        do ibl=1,num_bl
          del_wt_sq(idr,ibl)=del_chi_sq(idr,ibl)/afac(idr,ibl)
          chi_sq0_tot=chi_sq0_tot+chi_sq0(idr,ibl)
          chi_sqX_tot=chi_sqX_tot+chi_sqX(idr,ibl)
        end do
      else if(iwt_mode .eq. 1) then
        amat=0.
        bvec=0.
        do istat=1,nstat
          id_ptr=indx4(istat,istat)
! in this loop we make bvec and diagonal part of amat.
          do jstat=1,nstat
            if(istat .ne. jstat) then
              ibl=ibase_index(ibl_vec,nstat)
              bvec(istat)=bvec(istat)+del_chi_sq(idr,ibl)
              amat(id_ptr)=amat(id_ptr)+afac(idr,ibl)
            endif
          end do
! now do the off diagonal terms.
          do jstat=1,istat-1
!             amat(indx4(istat,jstat))=
!     >               afac(idr,ibase_index(ibl_vec,nstat))
          end do
        end do
!
!
! special case: 2x2 matrix is singular.
! Following fixes this by alloting half the error to each.
        if(nstat .eq. 2) then
           del_wt_sq(idr,1)=0.5*bvec(1)/amat(1)
           del_wt_sq(idr,2)=del_wt_sq(idr,1)
        else
          call invert_norm_tri(amat,bvec,nstat )
          do istat=1,nstat
           del_wt_sq(idr,istat)=bvec(istat)
          end do
        endif
      ENDIF
! update the weights
      do iwt=1,num_wts
        wts(idr,iwt)=max(wts(idr,iwt)**2+fudge*del_wt_sq(idr,iwt),0.0d0)
        wts(idr,iwt)=sqrt(wts(idr,iwt))
      end do
      END DO                    !End IDR  loop
!      if(iwt_mode .eq. 2) then
!       bl_wts_nf(1:2,1:num_wts)=wts(1:2,1:num_wts)
!       return
!      endif
!
      call wts_oth_bl(wts,nstat,iwt_mode,bl_wts_sq )
      do iwt=1,num_bl
      do idr=1,2
       bl_wts_nf(idr,iwt)=sqrt(bl_wts_sq(idr,iwt))
      end do
      end do
!
!
      return
      end
