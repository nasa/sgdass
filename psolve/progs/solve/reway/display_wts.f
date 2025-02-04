       SUBROUTINE display_wts(idatyp,wts0,wts,chi_sq,chi_dof, &
     &   nobs,iwt_mode, num_wts,    nstat,lstat,pause_display)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
!     Updated to specificaly type integers which
!-------------------------------------------------
      integer*2 idatyp
      integer*2 nstat
      integer*4 inverse_base
      double precision wts0(2,*),wts(2,*) ,chi_sq(2,*)
      double precision chi_dof(2,*)
      integer*4 nobs(2,*)
      integer*2 iwt_mode,num_wts
      character*8 lstat(*)
      integer*2 ibsln(2)
      integer*4 ibsln4
!      character*6 lunit(2)/" ps "," fs/s "/
      equivalence (ibsln,ibsln4)
      logical*2 pause_display
! everything below is used to write out wts.
      integer*2 iwt
      character*18 lwt_name
      character*78 bufstr
      character*4 cchar
      integer*4 ichar4
      equivalence (ichar4,cchar)
      integer*4 ix,iy
      integer*4 i4p0, i4p4, i4p6, i4p50
      character*5 ltype
      character*8 lkind(0:2)/"Global","Station","Baseline"/
      data i4p0, i4p4, i4p6, i4p50 /0,4,6,50/
      LOGICAL*2 krate,kdelay
      character*25 lprefix
      data lprefix/"Weight name          #  " /
      character*27 ldelay,lrate
      data ldelay/"Delay0   Delay    Chi_sq"/
      data  lrate/" Rate0    Rate    Chi_sq"/
      INTEGER*2 i0,ie,i
      INTEGER*2 idr
      double precision chi_sq_tot(2),wts_rms_tot(2),wts_rms_tot0(2)
      integer*4 nobs_tot(2)
      double precision dof_tot(2)
      integer*2 isecs,idum
      LOGICAL*4 DATYP_INQ
!
!     modifications
!     kdb 970610 New option, P, to turn off pauses in iterative listing
!     pet 980203 Substituted hard-coded test of solution type by DATYP_INQ
!
      ltype='Group'
!!      if(mod(idatyp,3).eq.1) ltype='Phase'
!!      kdelay=idatyp.ne.6
!!      krate=idatyp/3.ne.1
      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) LTYPE = 'Phase'
      KDELAY = DATYP_INQ ( IDATYP, DELAY__DTP )
      KRATE  = DATYP_INQ ( IDATYP,  RATE__DTP )
! JMG 97FEB03
! Modified to always display both kinds of weights.
! JMG 97FEB13. Modified to compute total chi_square, etc
      kdelay=.true.
      krate=.true.
!
!
      call setcr_mn(i4p0,i4p0 )
      call clear_mn()
      write(bufstr,1) ltype,lkind(iwt_mode),num_wts
1     &
     & FORMAT(" ",a5," Weights in use, kind of weights,number: ",a8,i6)
      call addstr_f(bufstr )
      call nl_mn()
2     continue
!
!
      call nl_mn()
      if(krate .and. kdelay) then
        i0=1
        ie=2
        bufstr= lprefix//ldelay//lrate
      else if(kdelay) then
        i0=1
        ie=1
        bufstr= lprefix//ldelay
      else if(krate) then
        i0=2
        ie=2
        bufstr= lprefix//lrate
      else
        return
      endif
!
!
      do idr=1,2
       nobs_tot(idr)=0
       chi_sq_tot(idr)=0
       wts_rms_tot(idr)=0
       wts_rms_tot0(idr)=0
      enddo
      dof_tot=0.
!
!
      call addstr_f(bufstr )
      call nl_mn()
      do iwt=1,num_wts
         if(iwt_mode .eq. 0) then
            lwt_name="Global"
         else if(iwt_mode .eq. 1) then
            lwt_name=lstat(iwt)
         else if(iwt_mode .eq. 2) then
           ibsln4=inverse_base(iwt,nstat)
           write(lwt_name,'(a8,"-",a8)')lstat(ibsln(1)),lstat(ibsln(2))
         endif
         do idr=1,2
           chi_sq_tot(idr)=chi_sq_tot(idr)+ &
     &                      chi_sq(idr,iwt)*chi_dof(idr,iwt)
           wts_rms_tot0(idr)=wts_rms_tot0(idr) &
     &                        +wts0(idr,iwt)**2.*nobs(idr,iwt)
           wts_rms_tot(idr) &
     &                        =wts_rms_tot(idr)+wts(idr,iwt)**2.*nobs(idr,iwt)
           nobs_tot(idr)=nobs_tot(idr)+nobs(idr,iwt)
           dof_tot(idr)=dof_tot(idr)+chi_dof(idr,iwt)
         enddo
         write(bufstr,100)lwt_name,nobs(i0,iwt), &
     & (wts0(i,iwt),wts(i,iwt), chi_sq(i,iwt),i=i0,ie)
!
         call addstr_f(bufstr )
         call nl_mn()
      end do
!
!
! write out "average" chisquare. Skip if global mode because redundant
      if(iwt_mode .ne. 0) then
        do idr=1,2
          chi_sq_tot(idr)=chi_sq_tot(idr)/dof_tot(idr)
          wts_rms_tot(idr)=sqrt(wts_rms_tot(idr)/nobs_tot(idr))
          wts_rms_tot0(idr)=sqrt(wts_rms_tot0(idr)/nobs_tot(idr))
        end do
        lwt_name="Total"
        write(bufstr,100)lwt_name,nobs_tot(1), &
     &    (wts_rms_tot0(i),wts_rms_tot(i), &
     &     chi_sq_tot(i),i=i0,ie)
        call addstr_f(bufstr )
        call nl_mn()
      endif
!
!
100   format(a18,i5,2(f8.2,1x,f8.2,1x,f8.3,1x),f8.3)
      call nl_mn()
      if (pause_display) then
        call addstr_f("Hit any key to return" )
        call senkr_mn(ix,iy,ichar4 )
      else
        call refresh_mn()
#ifdef GNU
         CALL SLEEP ( 1 )
#else
         CALL FUNC_SLEEP ( 1 )
#endif
      endif
      return
      end
