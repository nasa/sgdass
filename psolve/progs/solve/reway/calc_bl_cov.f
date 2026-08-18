      SUBROUTINE calc_bl_cov(ibsln, istar,res,sig,sig_fact_sq, &
     &    nobs,nstat)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      integer*2 num_scans
!     Updated to specificaly type integers which
!-------------------------------------------------
! calculate the observed baseline Delay co-variance.
      integer*4 nobs                                      !number of observations
      integer*2 ibsln(2,nobs)                             !baseline
      integer*2 istar(*)                                  !star
      double precision res(2,nobs)                        !residual
      double precision sig(2,nobs)                        !sigma of residual
      double precision sig_fact_sq(2,nobs)
      INTEGER*2 nstat                                       !number of stations.
!
      integer*2 ibase_index                               !function
! local variables
      double precision res_now(nstat*(nstat+1)/2)         !normalized residual for this obs
      double precision resp_now(nstat*(nstat+1)/2)        !predicted normalized (post_fit) residual
      double precision cov_res(nstat*(nstat+1)*(nstat*(nstat+1)+2)/8)
      double precision cov_resp(nstat*(nstat+1)*(nstat*(nstat+1)+2)/8)
      integer*2        num_cov(nstat*(nstat+1)*(nstat*(nstat+1)+2)/8)
!
      integer*2 num_bl
      integer*2 ibl_ptr
      LOGICAL*2 kobs(nstat*(nstat+1)/2)
      integer*2 ibl1(2),ibl2(2)                  !baseline vectors
      integer*2 is1,js1,is2,js2                  !stations
      equivalence (is1,ibl1(1))
      equivalence (js1,ibl1(2))
      equivalence (is2,ibl2(1))
      equivalence (js2,ibl2(2))
      integer*2 ibl_ptr1,ibl_ptr2
      integer*4 indx4
      integer*2 ij_ptr
      integer*2 iobs
!
      num_bl=nstat*(nstat-1)/2
!
!
      cov_resp=0.
      cov_res=0.
      num_cov=0
      res_now=0.
      resp_now=0.
      kobs=.false.
!
!
      num_scans=0
      do iobs=1,nobs
! get all residuals in a single scan.
        ibl_ptr=ibase_index(ibsln(1,iobs),nstat)
        res_now(ibl_ptr) =res(1,iobs)/sig(1,iobs)
        resp_now(ibl_ptr)=res(1, &
     &                 iobs)/(sig(1,iobs)*sqrt(sig_fact_sq(1,iobs)))
        kobs(ibl_ptr) = .true.                     !this baseline observed on this scan?
! is next observation a new source? Yes, then close out this scan.
        if(istar(iobs) .ne. istar(iobs+1)) then
           num_scans=num_scans+1
           do ibl_ptr1=1,num_bl
            if(kobs(ibl_ptr1)) then
              do ibl_ptr2=1,ibl_ptr1
               if(kobs(ibl_ptr2)) then
                 ij_ptr=indx4(ibl_ptr1,ibl_ptr2)
                 cov_resp(ij_ptr)=cov_resp(ij_ptr)+ &
     &                       resp_now(ibl_ptr1)*resp_now(ibl_ptr2)
                 cov_res(ij_ptr)=cov_resp(ij_ptr)+ &
     &                       res_now(ibl_ptr1)*res_now(ibl_ptr2)
                  num_cov(ij_ptr)=num_cov(ij_ptr)+1
                endif
              enddo
            endif
           enddo
          kobs=.false.
        endif
      end do
!
!
! now dump out baseline covariance.
      open(63,file="bl_cov.tmp")
      write(63, *) "Baseline co-variance info"
      write(63,*) "Number of scans", num_scans
      write(63, *) "i1 j1 i2 j2  num  resp res"
!
      do is1=1,nstat
      do js1=1,is1-1
        ibl_ptr1=ibase_index(ibl1,nstat)      !generate first baseline
        do is2=1,is1                       !and second.
        do js2=1,is2-1
          ibl_ptr2=ibase_index(ibl2,nstat)
          ij_ptr=indx4(ibl_ptr1,ibl_ptr2)
          if(num_cov(ij_ptr) .ne. 0) then
            write(63, &
     &        '(2i2,2x,2i2,2x,i4,2g16.5)')is1,js1, is2,js2, num_cov(ij_ptr), &
     &        cov_resp(ij_ptr)/num_cov(ij_ptr), &
     &        cov_res(ij_ptr)/num_cov(ij_ptr)
          else
            write(63, &
     &        '(2i2,2x,2i2,2x,i4)')is1,js1, is2,js2, num_cov(ij_ptr)
          endif
        end do
        end do
      end do
      end do
      close(63)
      return
      end
