      SUBROUTINE get_resid(idbeg,idend, &
     &  nstat,bl_wts,res,sig,el,sig_raw_Sq, &
     &  sig_fact_sq, ibsln,istar, icount)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
      INCLUDE 'socom.i'
      INCLUDE 'resfl.i'
!
      integeR*2 nstat
      integer*4 icount
      integer*2 ibsln(2,*)             !baseline
      integer*2 istar(*)               !star we are observing
      REAL*8 bl_wts(2,*)
      real*8 res(2,*)                  !residual
      real*8 sig(2,*)                  !sigma
      real*8 el(2,*)                   !elevatoin
      real*8 sig_raw_sq(2,*)
      real*8 sig_fact_sq(2,*)
      character*80 ldum
!
!
      LOGICAL*1 FL_NEG 
      logical*2 kbit
      integer*4 idbeg,idend        !first, last observation to read in.
! Modified 97OCT28 to handle case when we are using a db other than 1st.
!
! Modified 97NOV04 to get residual, rather than square.  This is useful for some calculations.
!
!
! load the residuals from disk
!
! input:
!     nobs: number of observations from resfil
!     delays: true if delays were used in solution
!     rates:  true if rates were used in solution
!     constant( ): error constants used in last solution
!             (1): in picoseconds
!             (2): in femtoseconds/second
!
! output:
!     res: residuals (delay in picoseconsd, rate in fmtsec/sec)
!     sig_raw_sq: raw sigma of observation
!     sig_fact_sq:  sig_post_fit_sq/sig_raw_sq  (always <1)
!     icount: number of included observation, necessarily the same
!             for delay and rates
!
!
!
      integer*4 iobs
      integer*2 ibase_index,ibl_ptr
!
      call acs_resfil('O' )
      icount=0
!
!
10    continue
      fl_neg = .false.
      do iobs=idbeg,idend
        call use_resfil(iobs,'R' )
        IF ( RDERR < 1.D-12 ) IRUNW = 1
        if(irunw.eq.0) then
          icount=icount+1
          ibsln(1,icount)=irsite(1)
          ibsln(2,icount)=irsite(2)
          istar(icount)=irstar
          el(1,icount)=relev(1)
          el(2,icount)=relev(2)
          ibl_ptr=ibase_index(irsite,nstat)
! delay
          res(1,icount)=rdoc*1d3
          sig(1,icount)=rderr*1d3
          sig_fact_sq(1,icount)=(pderr*1d3)**2
          sig_fact_sq(1,icount)=(pderr/rderr)**2
          sig_raw_sq(1,icount)=(rderr*1d3)**2-bl_wts(1,ibl_ptr)
          if(sig_raw_sq(1,icount) .lt. 0.0d0 ) then
            write(ldum, &
     &     '("MYWAY: Delay sigmas  squared negative!",1pe15.7)') &
     &      sig_raw_sq(1,icount)
!@            call ferr2( INT2(-1), ldum, INT2(0), INT2(0), INT2(2) )
!@            write(*,*) "IOBS, count:",iobs,icount, bl_wts(1,ibl_ptr)
!@            call end_prog()
             sig_raw_sq(1,icount) = 1.d-24
          endif
! rate
          res(2,icount)=rroc*1d3
          sig(2,icount)=rrerr*1d3
          sig_fact_sq(2,icount)=(prerr*1d3)**2
          sig_fact_sq(2,icount)=(prerr/rrerr)**2
          sig_raw_sq(2,icount)=(rrerr*1d3)**2-bl_wts(2,ibl_ptr)
          if ( sig_raw_sq(2,icount) .lt. 0.0D0 ) then
               sig_raw_sq(2,icount) = 1.d-24
                    write ( *, '("MYWAY: Rate sigmas  squared negative!",1pe15.7)' ) &
     &                           sig_raw_sq(2,icount)
               fl_neg = .true.
          endif
        endif
      enddo
      if ( fl_neg ) then
           call pause ( 'REWAY(get_resid) Hit any key to continue' )
      end if
!
!
      call acs_resfil('C' )
!
      return
      end
