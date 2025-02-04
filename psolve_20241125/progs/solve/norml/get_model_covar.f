      SUBROUTINE get_model_covar(lplate_stat,r,v_ap,nstat,lfixed,a,b)
! subroutine to return nuvel normal equations for use in solve.
! written August 30, 1993
!  JMGipson
!  On input
!    lplate_stat -- array containing plate names -- 1 for each station.
!    r           -- array containing station positions (meters.)
!    v_ap        -- apriori velocities of stations (meters/year.)
!    nstat -- number of parameters.
!    lfixed -- fixed plate.  Possibilities include
!                            NOAM,PACF,COCO... ... or:NROT
!                            last is "no-net rotation"
! on Exit
!   a, b normal equation form of co-variance for stations.
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
      INCLUDE 'precm.i'
!
      integer*2 nstat,nparm,istat,jstat,M_GPAm
      character*4 lplate_stat(nstat)
      real*8 r(3,nstat),v_ap(3,nstat)
      character*4  lfixed
      real*8 a(*),b(3,nstat)
      integer*4 ntri
      integer*2 max_stat
      parameter (max_stat=200,M_GPAm=max_stat*3)
      integer*2 iplate_stat(max_stat)
!
      integer*2 max_plates,max_tri,max_rt
      parameter (max_plates=20,max_rt=max_plates*3, &
     &  max_tri=(max_rt*(max_rt+1))/2)
      real*8 ap(max_tri),bp(3,max_plates)
      real*8 btmp(3,max_plates)
      real*8 wt_vec(3,max_rt)
      real*8 cov_tmp(3,3)
      real*8 big/1.d10/
      real*8 small/1.d-20/
      real*8 v_tmp(3)
      integer*2 num_plates,num_rots,num_tri
      integer*2 iplate,jplate
      INTEGER*4 IOS
! conversion factor from micro degrees per year rotation
! to meters per year velocity.
!
      real*8 udeg2Myr
      real*8 udeg2Myr_sq
      parameter (udeg2Myr=(3.14159/180.)*1e-6, &
     &  udeg2Myr_sq=udeg2Myr**2)
!
      integer*4 indx4,iptr,jptr
      integer*2 ix,iy,istat_off,jstat_off
      integer*2 iplate_off,jplate_off
      real*8 atmp(3,3)
      character*4 lplate(max_plates),ltmp
      integer*2 getunit,lu_in
      integer*2 i,j,k,irow,icol,irow_tot,icol_tot,jrow_tot
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
!
      if(nstat .gt. max_stat) then
        write(*,*) "Not enough space in nuvel_cov!"
        stop
      endif
!
!
! first thing to do is read in nuvel model, and co-variances.
! this is from Minster model.
      lu_in=getunit()
      open(lu_in,file=PRE_SAV_DIR(:PRE_SV_LEN)//NUVEL_MOD,IOSTAT=ios)
      call ferr( INT2(ios), "Opening NUVEL model file", INT2(0), INT2(0) )
      num_plates=1
10    continue
      read(lu_in,*,end=20,err=10) lplate(num_plates),ltmp, &
     &  btmp(1,num_plates),btmp(2,num_plates),btmp(3,num_plates)
      num_plates=num_plates+1
      goto 10
20    continue
      num_plates=num_plates-1
      close(lu_in)
!
!
! now read in the covariane matrix.
      open(lu_in,file=PRE_SAV_DIR(:PRE_SV_LEN)//NUVEL_COV,IOSTAT=ios)
      call ferr( INT2(ios), "Opening NUVEL covariance file", INT2(0), INT2(0) )
      num_rots=3*num_plates
      num_tri=(num_rots*(num_rots+1))/2
      do i=1,num_rots
        do j=1,num_rots
        iptr=indx4(i,j)
        read(lu_in,*,err=21,end=21) ap(iptr)
      end do
      end do
21    continue
      close(lu_in)
! have the rotation rates and co-variances.  This assumes that
! the pacific plate is fixed.  Need to do some putzing around to
! get other plates fixed.
!
!
! first invert equations.
      CALL DPPFA ( AP, NUM_ROTS )
      CALL DPPIN ( AP, NUM_ROTS )
! append additional rows corresponding to suppressed plate.
      do irow=1,3
        do icol=1,3
          atmp(irow,icol)=0.
        end  do
      end do
!
!
      do iplate=1,num_plates
         do irow=1,3
            do icol=1,3
              irow_tot=3*num_plates+irow
              icol_tot=3*(iplate-1)+icol
              iptr=indx4(irow_tot,icol_tot)
              ap(iptr)=0.
              do jplate=1,num_plates
                jrow_tot=(jplate-1)*3+irow
                jptr=indx4(jrow_tot,icol_tot)
                ap(iptr)=ap(iptr)-ap(jptr)
                atmp(irow,icol)=atmp(irow,icol)+ap(jptr)
              end do
            end do
         end do
      end do
      do irow=1,3
        do icol=1,irow
          iptr=indx4( INT2(num_rots+irow), INT2(num_rots+icol) )
          ap(iptr)=atmp(irow,icol)
        end do
      end do
      num_plates=num_plates+1
      num_rots=num_rots+3
      lplate(num_plates)="PCFC"
      btmp(1,num_plates)=0.
      btmp(2,num_plates)=0.
      btmp(3,num_plates)=0.
! make associated b vector
      call trimat_times_vec(ap,btmp,bp,num_rots )
!
!
! Now find and constrain the fixed plate
      do i=1,num_plates
        if(lfixed .eq. lplate(i)) then
           do j=1,3
             iptr=indx4( INT2(3*(i-1)+j), INT2(3*(i-1)+j) )
             ap(iptr)=ap(iptr)+big
           end do
           goto 50
         endif
      end do
! fixed plate not found.  Check to see if no net rotation.
      if(lfixed .ne. "NROT") then
        writE(*,*) "Fixed plate not found!"
        stop
      endif
! read in weighting factors
      open(lu_in,file=PRE_SAV_DIR(:PRE_SV_LEN)//NUVEL_WGT,IOSTAT=ios)
      call ferr( INT2(ios), "Opening NUVEL weight file", INT2(0), INT2(0) )
      do i=1,num_rots
        read(lu_in,*,IOSTAT=ios) wt_vec(1,i),wt_vec(2,i),wt_vec(3,i)
        call ferr( INT2(ios), "Reading weighting factors", INT2(0), INT2(0) )
      end do
      close(lu_in)
!
!
! Make "average" rotation to be zero.
      do k=1,3
        do i=1,num_rots
          do j=1,i
           ap(indx4(i,j))=ap(indx4(i,j))+big*wt_vec(k,i)*wt_vec(k,j)
           end do
        end do
      end do
50    continue
!
!
! Now un-invert equations.
! At this point a contains co-variance info for nuvel model with
! some plate fixed, or with no net rotation fixed.
      CALL DPPFA ( AP, NUM_ROTS )
      CALL DPPSL ( AP, BP, NUM_ROTS )
      CALL DPPIN ( AP, NUM_ROTS )
!
!
! OK. Now we start to compute the co-variances, etc associated with
! the stations we want.  First, zero out a and b matrices.
      nparm=3*nstat
      ntri=(nparm*(nparm+1))/2
      do i=1,ntri
       a(i)=0.
      end do
!
!
! find which plate each station lies on.
      do istat=1,nstat
! first find which station this station is on.
        iplate_stat(istat)=0
        do jplate=1,num_plates
          if(lplate_stat(istat) .eq. lplate(jplate)) then
             iplate_stat(istat)=jplate
            goto 60
          endif
        end do
        write(*,*) "Can't find: ",lplate_stat(istat)
        write(*,*) "Using pacific plate."
        iplate_stat(istat)=num_plates
60      continue
        call cross_product8(bp(1,iplate_stat(istat)),r(1,istat),v_tmp )
        do ix=1,3
          b(ix,istat)=v_tmp(ix)*udeg2Myr-v_ap(ix,istat)
        end do
!        write(*,'(a10,4i10)') lplate_stat(istat),jplate,
!     >     r(1,istat),r(2,istat),r(3,istat)
!
      end do
! now compute new covariance.
!
      do istat=1,nstat
         istat_off=3*(istat-1)
         iplate_off=3*(iplate_stat(istat)-1)
         do jstat=1,istat
           jstat_off=3*(jstat-1)
           jplate_off=3*(iplate_stat(jstat)-1)
           do ix=1,3
             do iy=1,3
               cov_tmp(ix,iy)=ap(indx4( INT2(ix+iplate_off), INT2(iy+ &
     &         jplate_off) ))*udeg2Myr_sq
             end do
           end do
           call tran_covar(cov_tmp,r(1,istat),r(1,jstat) )
! At this point have covariance info for horizontal components.
! now we add in covariance info for local vertical.
           if(istat .eq. jstat) then
             do ix=1,3
               do iy=1,3
                 cov_tmp(ix,iy)=cov_tmp(ix,iy)+ &
     &             r(ix,istat)*r(iy,istat)*small
               end do
               cov_tmp(ix,ix)=cov_tmp(ix,ix)+1.e-9
             end do
           endif
           do ix=1,3
             do iy=1,3
               a(indx4( INT2(ix+istat_off), INT2(iy+jstat_off) ))=cov_tmp(ix, &
     &         iy)
             end do
           end do
         end do
      end do
!
!
! uninvert normal equations.
!
      CALL DPPFA ( A, NPARM )
      CALL DPPSL ( A, B, INT2(INT4(NPARM)) )
      CALL DPPIN ( A, NPARM )
!
!
      return
      end
      SUBROUTINE tran_covar(cov_tmp,r1,r2)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!
      integer*2 i,j,k
      real*8 cov_tmp(3,3),r1(3),r2(3)
      real*8 tmat(3,3)
      real*8 tran(3,3)
!
      call make_tran(tran,r2 )
!
!
      do i=1,3
        do j=1,3
          tmat(i,j)=0.
            do k=1,3
              tmat(i,j)=tmat(i,j)+cov_tmp(i,k)*tran(k,j)
            end do
         end do
      end do
!
!
      call make_tran(tran,r1 )
      do i=1,3
        do j=1,3
          cov_tmp(i,j)=0.
            do k=1,3
            cov_tmp(i,j)=cov_tmp(i,j)+tran(k,i)*tmat(k,j)
            end do
         end do
      end do
!
!
      return
      end
      SUBROUTINE make_tran(tran,r)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 i, j
!-----END of imp added lines.
!
      real*8 r(3),tran(3,3)
!
      do i=1,3
        do j=1,3
          tran(i,j)=0.
        end do
      end do
!
!
      tran(1,2)=r(3)
      tran(2,3)=r(1)
      tran(3,1)=r(2)
      tran(3,2)=-r(1)
      tran(1,3)=-r(2)
      tran(2,1)=-r(3)
!
!
      tran(1,1)=0.
      tran(2,2)=0.
      tran(3,3)=0.
      return
      end
      SUBROUTINE trimat_times_vec(ap,btmp,b,num_rots)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!
      integer*2 num_rots
      integer*2 i,j
      integer*4 indx4
      real*8 ap(*),btmp(*),b(*)
!
      do i=1,num_rots
       b(i)=0.
       do j=1,num_rots
         b(i)=b(i)+ap(indx4(i,j))*btmp(j)
         end do
       end do
       return
       end
       SUBROUTINE cross_product8(r1,r2,rout)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
       real*8 r1(3),r2(3),rout(3)
!
       rout(1)=r1(2)*r2(3)-r1(3)*r2(2)
       rout(2)=r1(3)*r2(1)-r1(1)*r2(3)
       rout(3)=r1(1)*r2(2)-r1(2)*r2(1)
       return
       end
