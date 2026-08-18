      SUBROUTINE enter_wts(idatyp,wts,iwt_mode, &
     &   num_wts,nstat,lstat)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
!     Updated to specificaly type integers which
!-------------------------------------------------
!
!     created by kdb 970228, based on display_wts.f
!     modified   JMG 971028  to enter in rate weights.
!                pet 980203  Substituted hard-coded test of solution type
!                            by DATYP_INQ
      integer*2 idatyp
      integer*2 nstat
      integer*4 inverse_base
      double precision wts(2,*)
      integer*2 iwt_mode,num_wts
      character*8 lstat(*)
      integer*2 ibsln(2)
      integer*4 ibsln4
      character*6 lunit(2)/" ps "," fs/s "/
      equivalence (ibsln,ibsln4)
! everything below is used to write out wts.
      integer*2 iwt
      character*18 lwt_name
      character*110 bufstr
      character*4 cchar
      integer*4 ichar4
      equivalence (ichar4,cchar)
      integer*4 ix,iy
      integer*4 i4p0, i4p4, i4p6, i4p50
      character*5 ltype
      character*8 lkind(0:2)/"Global","Station","Baseline"/
      integer*2 ipass
      INTEGER*4  IERR
      data i4p0, i4p4, i4p6, i4p50 /0,4,6,50/
      LOGICAL*2 krate,kdelay
      character*26 lprefix
      logical*2 accepted
      double precision new_wts(2)
      data lprefix/"Weight name  "/
      INTEGER*2 i0,ie,i
      integer*2 trimlen,iblen
      LOGICAL*4 DATYP_INQ
!
      ltype='Group'
!      if(mod(idatyp,3).eq.1) ltype='Phase'
!      kdelay=idatyp.ne.6
!      krate=idatyp/3.ne.1
      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) LTYPE = 'Phase'
      KDELAY = DATYP_INQ ( IDATYP, DELAY__DTP )
      KRATE  = DATYP_INQ ( IDATYP,  RATE__DTP )
!     kdelay=.true.
!     krate=.true.
!
      call setcr_mn(i4p0,i4p0 )
      call clear_mn()
!
      write(bufstr,1) ltype,lkind(iwt_mode),num_wts
1     &
     & FORMAT(" ",a5," Weights in use, kind of weights,number: ",a8,i6)
      call addstr_f(bufstr )
      call nl_mn()
      call nl_mn()
      krate=.true.
      kdelay=.true.
      if(krate .and. kdelay) then
        i0=1
        ie=2
        bufstr= "  Weight name         Delay      Rate"
      else if(kdelay) then
        i0=1
        ie=1
        bufstr= "  Weight name         Delay"
      else if(krate) then
        i0=2
        ie=2
        bufstr= "  Weight name         Rate"
      else
        return       !Some error condition?
      endif
      call addstr_f(bufstr )
      call nl_mn()
!
!     Loop through the weights 3 times to:
!       display the current list
!       display and change each weight
!       display the updated list
!
      do ipass = 1,3
!
        if (ipass.eq.2) then
          write(bufstr, &
     &      "('Enter new weight(s) (return for no change)')")
          call addstr_f(bufstr )
          call nl_mn()
        else if (ipass.eq.3) then
          call nl_mn()
        endif
        do iwt=1,num_wts
          if(iwt_mode .eq. 0) then
            lwt_name="Global"
          else if(iwt_mode .eq. 1) then
            lwt_name=lstat(iwt)
          else if(iwt_mode .eq. 2) then
            ibsln4=inverse_base(iwt,nstat)
            write(lwt_name,'(a8,"-",a8)')lstat(ibsln(1)),lstat(ibsln(2))
          endif
          if (ipass/2 * 2 .ne. ipass) then
!           pass 1 or 3 - display without changing
            if(krate .and. kdelay) then
              write(bufstr,100)lwt_name, &
     &           (wts(i,iwt),lunit(i),i=i0,ie)
100           format(a18,2(f8.3,a6))
            else
              write(bufstr,101)lwt_name, &
     &           (wts(i,iwt),lunit(i),i=i0,ie)
101           format(a18,f8.3,a6)
            endif
            iblen = trimlen(bufstr)
            call addstr_f(bufstr(1:iblen) )
            call nl_mn()
          else
            accepted = .false.
            do while (.not.accepted)
!             pass 2 - display and change
              if(krate .and. kdelay) then
                write(bufstr,102)lwt_name, &
     &             (wts(i,iwt),lunit(i),i=i0,ie)
102             format(a18,2(f8.3,a6),1X)
              else
                write(bufstr,103)lwt_name, &
     &             (wts(i,iwt),lunit(i),i=i0,ie)
103             format(a18,f8.3,a6,2X)
              endif
              iblen = trimlen(bufstr) + 2
              call addstr_f(bufstr(1:iblen) )
!             Get new weights
              bufstr = ' '
              call getstr_f(bufstr )
              if (trimlen(bufstr).eq.0) then
!               User wants to leave things alone for this one
                accepted = .true.
              else
                read(bufstr,*,iostat=ierr, &
     &            err=110)(new_wts(i),i=i0,ie)
 110            if (ierr.eq.0) then
                  accepted = .true.
                  do i = i0,ie
                     wts(i,iwt) = new_wts(i)
                  enddo
                endif
              endif
            enddo !changing - loop until acceptance
          endif !this pass is display vs. change
        end do !loop over weights
        if (ipass/2 * 2 .ne. ipass) then
          call nl_mn()
          call addstr_f("Hit any key to return" )
          call nl_mn()
          call senkr_mn(ix,iy,ichar4 )
        endif
      end do !loop over the three passes
!
      return
      end
