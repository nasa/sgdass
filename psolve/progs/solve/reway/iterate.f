      SUBROUTINE iterate(pause_reway,kfall_back,knew_update, &
     &   max_iter,chi_tol)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
!     Updated to specificaly type integers which
!-------------------------------------------------
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
!
!  iterate solution, adjusting weights until rchisq is 1.0
!
!     kdb 970204 Fix error:  don't display
!                pass by pass weights on the screen in a batch solution.
!                Fix second error:  turn off spooling, full output
!                and glbbsl during solution iteration. (Code to do so had been
!                skipped for debugging purposes.)
!                Third fix: display of first screen on each pass is too slow
!                for users in site weight mode, so write reassuring message
!                that input to continue has been registered.
!     jmg 970213 Several changes.
!                1. Can exit if chi-square stops.
!                2. If stops converging in one wt mode, go to another.
!         970307 1. Itetartion counter 2 above wasn't working. Fixed it.
!                2. Stop  if sufficiently close to 1.000
!                3. Changed format of writing out old,new chi_square.
!                4. Write out iteration counter when we finish an iteration.
!     jmg 970318 Fix bug: don't use numobs for number of residuals.  (Wrong
!                if doing two databases.)
!     kdb 970318 Change pause to ferr2 call.
!     kdb 970610 New option, P, to turn off pauses in iterative listing.
!     jmg 971030 Pass through 1.) maximum number of iterations.
!                             2.) chi-square tolerance.
!                             3.) fall back mode.
!     pet 980121 Changed a bit logic of when intermediary information is
!                printed. Intermediary informatiuon is printed only when
!                pause_reway is .TRUE.
!     pet 980203  Substituted hard-coded test of solution type by DATYP_INQ
!     jmg 990125  Fixed a bug in fall back mode.
!                 Wasn't changing the number of weights.
!     pet 990428  Added start_mn after GLOBL
!     jwr 021217  TRUE__L2 and FALSE__L2 introduced for -i2 conversion.
!     pet 2019.03.23  Added initialization of wts
!
      INCLUDE 'socom.i'
      INCLUDE 'precm.i'
      INCLUDE 'prfil.i'
      INCLUDE 'reway.i'
!
! --- Variables passed to here.
!
      logical*2 pause_reway           !pause on iterations.
      logical*2 kfall_back            !If iteration fails, fall back.
      logical*2 knew_update           !Use alternative update mode.
      integer*2 max_iter              !Maximum number of iterations.
      double precision chi_tol        !tolerance for convergence.
!
!
!
      integer*2 CLK_BIT_WORDS
      parameter (CLK_BIT_WORDS=(MAX_CLK+WORD_BITS-1)/WORD_BITS)
      characteR lbuf*79
      LOGICAL*2 delays,rates
      logical*2 save_spool,save_minout,save_batch,save_lclbsl
      logical*2 save_glbbsl,save_bit12,kbit
      integer*2 iwt
      real*8 max_diff,temp
      real*8 func
      character*8 ltype(2)/"Delay","Rate"/
      integer*4 ix,iy,ichar4
!
      integer*2 num_bl
      parameter (num_bl=(max_arc_sta*(max_arc_sta-1))/2)
      real*8 bl_wts_nf(2,num_bl),wts(2,num_bl)
      real*8 wts0(2,num_bl)
      real*8 resid(2)
      integer*2 ibl_mode
      integer*2 idr
      double precision chi_sq_tot(2),chi_sq_tot0(2)
      double precision rnorm_tot(2)
! this is a function which returns number of residuals
!
! various stuff associated with resfile
      integer*4 idbeg,idend                      !start,end of data in resfile.
      logical*2 ksflag                           !sband flag
      integer*2 idb                              !which database
!
      logical*2 pause_display
      CHARACTER  STR*10
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4, EXTERNAL ::  DATYP_INQ
      INTEGER*4, EXTERNAL :: I_LEN
!
! check what kind of solution
!
!      delays=idatyp.ne.6
!      rates=idatyp/3.ne.1
      DELAYS = DATYP_INQ ( IDATYP, DELAY__DTP )
      RATES  = DATYP_INQ ( IDATYP,  RATE__DTP )
!
! save various control flags
!
      save_spool=kspool
      save_minout=kminout
      save_batch=kbatch
      save_lclbsl=klclbsl
      save_glbbsl=kglbbsl
      save_bit12=kbit( pre_ip(3), INT2(12))
!
! set the flags the way we want them
!
      call set_spool (FALSE__L2 )
      call set_minout(TRUE__L2 )
      call set_glbbsl(FALSE__L2 )
      call set_batch (TRUE__L2 )
      call set_lclbsl(FALSE__L2 )
      call sbit( pre_ip(3), INT2(12), INT2(1) )
!
      if(iwt_mode .eq. 0) then
         num_wts=1
      elseif (iwt_mode .eq. 1) then
        num_wts=nstat
      else if(iwt_mode .eq. 2) then
         num_wts=nstat*(nstat-1)/2
      endif
!
!
      call use_common('O' )
      call use_common('WC' )
      max_diff=2.
      WTS = 0.0d0
!
!
! Modified to keep track of number of iterations.
! Should converge within max_iterations.
!   If not, and kfall_back is on, then change weight mode to decrease freedom.
!   Finally stops on global weights.
      REWAY_ITCOU=0
      chi_sq_tot0=100.
!
!
      do while(.true.)
       if(REWAY_ITCOU .eq. max_iter) then
            if(.not. kfall_back) goto 100
          iwt_mode = iwt_mode-1
          if(iwt_mode .lt. 0) goto 100
          REWAY_ITCOU=0
!
! --------- jmg 99JAN25  Change num_wts if we change the weight mode
!
            IF ( IWT_MODE .EQ. 0 ) THEN
                 NUM_WTS = 1
              ELSE IF ( IWT_MODE .EQ. 1 ) THEN
                 NUM_WTS = NSTAT
            ENDIF
         ENDIF
       REWAY_ITCOU=REWAY_ITCOU+1
         CALL USE_GLBFIL_4 ( 'OWC' )
         if (.not.save_batch) call end_mn
         call run_prog( 'GLOBL', 'WAIT', INT2(0) )
         if ( .not.save_batch ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( INT4(REWAY_ITCOU), STR )
              CALL START_MN()
              CALL SETCR_MN ( 0, 0 )
              CALL ADDSTR_F ( '  REWAY --> REWAY     Iteration '// &
     &                         STR(1:I_LEN(STR)) )
              CALL SETCR_MN ( 50,  0 )
              CALL SETCR_MN ( 0,   1 )
         END IF
         call use_common('ORC' )
         call use_glbfil('ORC' )
         CALL SOCOM_EXT()
!
! read in residuals, subtracting appropriate stuff
!
        ibl_mode=2
        call get_bl_wts(bl_wts_nf,idb,idbeg,idend,ksflag )
!
!
! get wts before the adjustment
        call wts_bl_oth(bl_wts_nf,iwt_mode,numsta,wts0,resid )
        call wts_oth_bl(bl_wts_nf,nstat,ibl_mode,bl_wts_sq )
        call get_resid(idbeg,idend,nstat,bl_wts_sq,res,sig,el, &
     &       sig_raw_sq,sig_fact_sq,ibsln,istar, nobs )
! compute chi_sq per wt for this solution.
        temp=func(wts)
        krate=.not.krate
        kdelay=.not.kdelay
        if(kdelay .or. krate) then
          temp=func(wts)
        endif
        krate=.not.krate
        kdelay=.not.kdelay
!
!
! compute chi_sqare of this solution
        do idr=1,2
          chi_sq_tot(idr)=0.
          rnorm_tot(idr)=0.
          do iwt=1,num_wts
            rnorm_tot(idr)=rnorm_tot(idr)+chi_dof(idr,iwt)
            chi_sq_tot(idr)=chi_sq_tot(idr) &
     &        +chi_sq(idr,iwt)*chi_dof(idr,iwt)
          end do
        end do
        do idr=1,2
          chi_sq_tot(idr)=chi_sq_tot(idr)/rnorm_tot(idr)
        end do
!
!
        if ( .not. save_batch  .and.  pause_reway ) then
             write(lbuf, &
     &       '(a,2x,i4,"/",i4)')" Type      Old          New       Dif   Iteration", &
     &       REWAY_ITCOU,max_iter
             call addstr_f(lbuf )
             call nl_mn()
             do idr=1,2
                write(lbuf,'(a6,3f11.4)') ltype(idr), &
     &                chi_sq_tot0(idr),chi_sq_tot(idr), &
     &                chi_sq_tot0(idr)-chi_sq_tot(idr)
                call addstr_f(lbuf )
                call nl_mn()
             end do
!
             call nl_mn()
             call addstr_f("Hit any key to return" )
             call senkr_mn(ix,iy,ichar4 )
             call nl_mn()
             call addstr_f("Acknowledged" )
             call nl_mn()
             call refresh_mn()
        endif
!
! ----- If chi_sq has stopped changing, exit loop.
!
        if(abs(chi_sq_tot(1)-chi_sq_tot0(1)) .le. chi_tol .and. &
     &     abs(chi_sq_tot(2)-chi_sq_tot0(2)) .le. chi_tol) goto 100
        chi_sq_tot0(1)=chi_sq_tot(1)
        chi_sq_tot0(2)=chi_sq_tot(2)
! exit if sufficiently close to 1.
        if(abs(chi_sq_tot(1)-1.) .le. chi_tol .and. &
     &     abs(chi_sq_tot(2)-1.) .le. chi_tol) goto 100
! exit if done a lot of iterations, and still pretty close
        if(REWAY_ITCOU .ge. max_iter-1 .and. &
     &     abs(chi_sq_tot(1)-1.) .le. chi_tol .and. &
     &     abs(chi_sq_tot(2)-1.) .le. chi_tol) goto 100
!
!
! Ok. Chi_square still changing. Adjust weights
        call adjust_weights(bl_wts_nf,kbatch,knew_update,chi_tol )
! get wts after the adjustment.
        call wts_bl_oth(bl_wts_nf,iwt_mode,numsta,wts,resid )
!
!
        if ( .not.save_batch .and. pause_reway ) then
          pause_display = pause_reway
          call display_wts(idatyp,wts0,wts,chi_sq,chi_dof,num_obs_chi, &
     &         iwt_mode,num_wts,nstat,  isitn_chr,pause_display )
        endif
! write out the baseline wts.
        call put_bl_wts(bl_wts_nf,idb )
      enddo
! break exit
100   continue
      call use_common('OR' )
      call socom_ext()
      call use_common('WC' )
!
! restore setup
!
      call set_spool(save_spool )
      call set_minout(save_minout )
      call set_batch(save_batch )
      call set_lclbsl(save_lclbsl )
      call set_glbbsl(save_glbbsl )
      call ksbit( pre_ip(3), INT2(12), save_bit12 )
!
      RETURN
      END  !#!  ITERATE  #!#
