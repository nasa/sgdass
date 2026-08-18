      program reway
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
      INCLUDE 'solve.i'
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'precm.i'
      INCLUDE 'glbcm.i'
!
! reway: re-weigh the data to make the reduced Chi-square unity
!
!  modifications
!  March 1996.
!  Extensively modified by JMGipson to incorporate new weighting algorithm.
!
!  kdb 970204 Site weighting feature.
!             Also related new feature to determine the current weighting type
!             and update the glbfil with this information.
!             (Includes fix to March 1996 new weighting algorithm:
!               was calling get_resid upon initialization
!               (before running globl) and was therefore accessing the resfil
!                before it was being written.  This produced weird values
!                that led to a segmentation violation.
!             Also bring up Enter specific values option under new reway.
!             Also rename option Z to R (reinitialize errors to zero) and
!                                R to N (reread Namfil errors)
!  kdb 970306 Fix error in initialization of weights in batch mode.
!             (Was setting bl_wts_nf to the desired values, but then saving
!              the uninitialized array bl_wts_zr in the namfil.  This
!              saved *'s in the namfil for some arcs, causing an error in
!              proc reading the namfil rewt card.)
!             Plus change : array index to do loop index for compatibility
!              at other installations.
!  jmg 970318 Fix bug: don't use numobs for number of residuals.  (Wrong
!                if doing two databases.)
!             Blank screen in update mode so user knows activity is occurring.
!  kdb 970318 change pause to ferr2 call.
!  kdb 970610 New option, P, to turn off pauses in iterative listing.
!
!  JMG 971105 Modified so that if we changed from baseline to station weights,
!             would store new weights on return to optin
!  KDB 971125 Set default delay and rate weight floors to 0.
!  KDB 971125 Option to set the weight floors via a standard file.
!             (In the absence of the file, set the floors to 0.)
!  KDB 971201 Fix error: only set the weight floors via a file in batch mode.
!             In interactive mode, set the floors to 0.
!  KDB 971201 Fix error: only set the weight floors via a file in batch mode.
!             In interactive mode, set the floors to 0.
!  pet 980121 Added capacity to set new default modes of the REWAY in
!             environment variables. Environment variables related to REWAY
!             are read when new database or superfile is read. That values
!             are stored in glbc4. REWAY reads these parameters and if they
!             are updated then it writes them back in glbc4.
!  pet 980203 Substituted hard-coded test of solution type by DATYP_INQ
!  pet 990216 Added call of the program reway_help which prints on the screen
!             on-line help
!  jwr 021217 TRUE__L2 and FALSE__L2 introduced to facilate -i2 conversion.
!
!  Our job is find the change in constant that will make rchisq unity.
!  This is a non-linear problem and may require several iterations
!  of solutions and rewighting to stablize.
!
!  Upto two error constants are found for each database:
!    one for delays and one for weights
!
      CHARACTER   VER_REWAY*21, GET_VERSION*54
      INCLUDE 'reway.i'
!
      character*1 action
      LOGICAL*2 okay
      integer*2 num_bl,ibase
      parameter (num_bl=(max_arc_sta*(max_arc_sta-1))/2)
      double precision resid(2)
      real*8 bl_wts_nf(2,num_bl)
      real*8 wts(2,num_bl),wts0(2,num_bl)
      real*8 tol
      integer*2 ibl_mode
      INTEGER*4  IOS
      real*8 temp
      real*8 func
      logical*2 kfirst_batch,save_spool,save_minout,save_bit12,kbit
      logical*2 save_lclbsl,save_glbbsl
      integer*2 ict3
      logical*2 pause_reway,pause_display
      LOGICAL*4 DATYP_INQ
!
      integer*4 num_res          !Number of residuals
      integer*4 idbeg,idend      !starting and stoping observations
      logical*2 ksflag           !sband flag
      integer*2 idb
      integer*4 i4p0/0/
!
      integer*2 max_iter         !maximum number of iterations
      double precision chi_tol   !chi_square tolerance.
      logical*2 kfall_back       !fall back mode on?
      logical*2 knew_update      !new weight update mode on?
      integer*2 ierr
      double precision wt_read(2)
!
      INTEGER*2     NUMDB, LDBNAM(5,15), IDBV(15), IDBE(15)
      CHARACTER     CDBNAM(15)*10, DBNAME*16
      EQUIVALENCE ( CDBNAM, LDBNAM(1,1) )
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4 I_LEN
!
! --- Get start-up info
!
      CALL PRE_PROG()
      INCLUDE 'reway_version.i' ! Set revision date of the current version
      VER_REWAY = GET_VERSION ()
!
      call use_glbfil   ( 'OR'  )
      call use_glbfil_4 (  'RC' )
      if (kbit( intracom_reway, INT2(1) )) then
        pause_reway = .true.
      else
        pause_reway = .false.
      endif
      call use_common ( 'ORC' )
      CALL SOCOM_EXT()
!
!
! clear the screen if not in batch mode
      if(.not. kbatch) then
        call start_mn()
        call refresh_mn()
      endif
!
! --- Batch mode defaults
!
      max_iter=10     ! maximum number of iterations.
      chi_tol=0.005   ! Tolerate 0.5 percent change in chi_square
! I.e., stop when chi_square changes by less than this
!                     ! amount, or is this close to 1.
      knew_update=.false.
      kfall_back=.true.   ! Fall back mode is on
!
!     Part of the reway initialization now involves getting residuals from an
!     initial solution, rather than waiting for the iteration mode to run
!     a solution and then getting the initial residuals.  The user is
!     expected to run the solution himself in interactive mode.  Run the
!     batch mode solution now.
!
      if (kbatch) then
! put in reasonable starting guesses
        do ict3 = 1,num_bl
           bl_wts_nf(1,ict3)=10.
           bl_wts_nf(2,ict3)=100.
        enddo
      idb=1
        call put_bl_wts(bl_wts_nf,idb )
!
!       Save old flags and set up new values for solution run.
!
        save_spool=kspool
        save_minout=kminout
        save_bit12=kbit( pre_ip(3), INT2(12))
        save_lclbsl=klclbsl
        save_glbbsl=kglbbsl
!
        call set_spool (FALSE__L2 )
        call set_minout(TRUE__L2 )
        call set_glbbsl(FALSE__L2 )
        call set_lclbsl(FALSE__L2 )
!       make sure residuals are stored in the resfil, the whole point of this
!       globl call
        call sbit( pre_ip(3), INT2(12), INT2(1) )
!
        call run_prog( 'GLOBL', 'WAIT', INT2(0) )
!
!       Put back original flags
!
        call set_spool(save_spool )
        call set_minout(save_minout )
        call ksbit( pre_ip(3), INT2(12), save_bit12 )
        call set_lclbsl(save_lclbsl )
        call set_glbbsl(save_glbbsl )
        call use_glbfil('ORC' )
        call use_common('ORC' )
        CALL SOCOM_EXT()
      endif
!
! *** End start up info *****
!
      kfirst_batch = .true.
!      kdelay=idatyp.ne.6
!      krate=idatyp/3.ne.1
      KDELAY = DATYP_INQ ( IDATYP, DELAY__DTP )
      KRATE  = DATYP_INQ ( IDATYP,  RATE__DTP )
! Initialize part of "reway.i"
! Read in baseline weights.
      nstat=numsta
      call get_bl_wts(bl_wts_nf,idb,idbeg,idend,ksflag )
!
! --- Extraction database name from NAMFIL
!
      CALL DBPOX ( NUMDB, LDBNAM, IDBV, IDBE )
      CALL CLRCH ( DBNAME )
      DBNAME = CDBNAM(IDB)//' <'
      CALL INCH ( INT4(IDBV(IDB)), DBNAME(13:) )
      DBNAME(I_LEN(DBNAME)+1:) = '>'
!
      wt_floor(1)=0
      wt_floor(2)=0
      if (kbatch) then
        open (85,file=PRE_SAV_DIR(:PRE_SV_LEN)//"reway_settings", &
     &         iostat=ios, &
     &         status='old',access='sequential',form='formatted')
        ierr = ios
        if ( ierr .eq. 0 ) then
             read(85,*,iostat=ios) wt_read(1),wt_read(2)
             ierr = ios
             if ( ierr.eq.0 ) then
                  wt_floor(1) = wt_read(1)
                  wt_floor(2) = wt_read(2)
               else
                 call ferr( ierr, "Reading reway_settings file", INT2(0), &
     &                INT2(0) )
             endif
             close (85)
!           else if (ierr.ne.908) then
!             call ferr ( ierr, "Opening reway_settings file", INT2(0), INT2(0) )
             wt_floor(1) = 1.D-12
             wt_floor(2) = 1.D-12
        endif
      endif
!
      if(ksflag) then
        wt_ceiling(1)=3000
        wt_ceiling(2)=10000
      else
        wt_ceiling(1)=300
        wt_ceiling(2)=1000
      endif
!
!
!
! read in residuals, subtracting appropriate stuff
!
      num_res=idend-idbeg+1
      if(num_res.gt.MAX_OBS)call ferr2( num_res, 'too many residuals', &
     &   INT2(0), INT2(0), INT2(2) )
!
      ibl_mode=2
!     Get squares of baseline weights.
!     (wts_oth_bl actually outputs the squares of the input weights)
      call wts_oth_bl(bl_wts_nf,nstat,ibl_mode,bl_wts_sq )
!
!
      call get_resid(idbeg,idend,nstat,bl_wts_sq,res,sig,el, &
     &     sig_raw_Sq, sig_fact_sq, ibsln,istar, nobs )
!
! Determine what kind of weights we have now. This is
! done by seeing residuals
!
      if(ksflag) then
        tol=2.
      else
        tol=0.3
      endif
      do iwt_mode=0,2
        call wts_bl_oth(bl_wts_nf,iwt_mode,numsta,wts,resid )
!***Start of debug****
        if(iwt_mode .eq. 0) then
           num_wts=1
         elseif (iwt_mode .eq. 1) then
             num_wts=nstat
         else if(iwt_mode .eq. 2) then
             num_wts=nstat*(nstat-1)/2
         endif
! tol has to be so big because of roundoff error.
! when bl_wts are stored, they are stored with
! only a couple of significant digits.
         tol=0.2*num_wts
!****End of Debug****
        if(krate .and.kdelay) then
          if(resid(1) .le. tol .and. resid(2) .le. tol) goto 1
        else
          if(krate .and. resid(2) .le. tol .or. &
     &       kdelay .and. resid(1) .le. tol) goto 1
        endif
      end do
1     continue
!
!  Reway may have been called in a special mode to set a glbfil variable
!  to show the current weighting scheme.  If so, do that now and exit.
!
      if (weighting_type.eq.'!!') then
        call use_glbfil('OR' )
        if (iwt_mode.eq.0) then
          weighting_type = 'DB'
        else if (iwt_mode .eq. 1) then
          weighting_type = 'ST'
        else if (iwt_mode .eq. 2) then
          weighting_type = 'BL'
        endif
        call use_glbfil('WC' )
        call end_prog()
      endif
      IF ( .NOT. KBATCH ) THEN
           CALL REWAYPAR_SET ( IWT_MODE, PAUSE_REWAY, KFALL_BACK, KNEW_UPDATE, &
     &                         MAX_ITER, CHI_TOL, WT_FLOOR, WT_CEILING )
           IF ( IWT_MODE .EQ. 0 ) THEN
                NUM_WTS=1
             ELSEIF (IWT_MODE .EQ. 1) THEN
                NUM_WTS=NSTAT
             ELSE IF(IWT_MODE .EQ. 2) THEN
                NUM_WTS=NSTAT*(NSTAT-1)/2
           ENDIF
      END IF
!
!  if batch, mush on and iterate, otherwise ask the user for directions
!
4     continue
!     (point of return if the weighting mode (global, site, baseline) changes)
!     (note: wts0 is weight value when entering this weighting mode
!            (global etc.) expressed in terms of this mode)
      call wts_bl_oth(bl_wts_nf,iwt_mode,numsta,wts0,resid )
5     continue
!     (note: wts is current weight value expressed in terms of this mode)
      call wts_bl_oth(bl_wts_nf,iwt_mode,numsta,wts,resid )
! way_scale(1),2 are "large residuals"
      way_scale(1)=50.
      way_scale(2)=300.
      if(kbatch) then
        if (kfirst_batch) then
          kfirst_batch = .false.
!
!         The batch program will now be able to request the different
!         weighting modes (site, by arc, baseline).  Set the requested mode
!         up now.
!
          if (weighting_type.eq.'D!') then !weight by arc
            action='G'
          else if (weighting_type.eq.'S!') then !weight by site
            action='S'
          else if (weighting_type.eq.'B!') then !weight by baseline
            action='B'
          else
            action='I'
            okay=.false.
          endif
        else
          action='I'
          okay=.false.
        endif
      else
!
        call start_mn()
        call setcr_mn(i4p0,i4p0 )
        call menu ( action, idatyp, iwt_mode, num_wts, pause_reway, &
     &              kfall_back, knew_update, max_iter, chi_tol, &
     &              wt_floor, wt_ceiling, DBNAME )
        call setcr_mn(i4p0,i4p0 )
        call clear_mn()
      endif
! this computes chi_sq initially.
      temp=func(wts)
      krate=.not.krate
      kdelay=.not.kdelay
      if(kdelay .or. krate) then
        temp=func(wts)
      endif
      krate=.not.krate
      kdelay=.not.kdelay
!
!  possible actions:
!
      if(action .eq. 'G') then
! Choose global weights
         iwt_mode=0
         num_wts=1
         CALL REWAYPAR_SAVE ( IWT_MODE, PAUSE_REWAY, KFALL_BACK, KNEW_UPDATE, &
     &                        MAX_ITER, CHI_TOL, WT_FLOOR, WT_CEILING )
         goto 4
      else if(action .eq. 'S') then
! Choose station weights
         iwt_mode=1
         num_wts=nstat
         CALL REWAYPAR_SAVE ( IWT_MODE, PAUSE_REWAY, KFALL_BACK, KNEW_UPDATE, &
     &                        MAX_ITER, CHI_TOL, WT_FLOOR, WT_CEILING )
         goto 4
      else if(action .eq. 'B') then
! choose baseline weights
         iwt_mode = 2
         num_wts=(nstat*(nstat-1)/2)
         CALL REWAYPAR_SAVE ( IWT_MODE, PAUSE_REWAY, KFALL_BACK, KNEW_UPDATE, &
     &                        MAX_ITER, CHI_TOL, WT_FLOOR, WT_CEILING )
         goto 4
      else if(action.eq.'P') then
          call use_glbfil ( 'ORC' )
          if (kbit( intracom_reway, INT2(1) )) then
            call sbit( intracom_reway, INT2(1), INT2(0) )
            pause_reway = .false.
          else
            call sbit( intracom_reway, INT2(1), INT2(1) )
            pause_reway = .true.
          endif
          call use_glbfil   ( 'OWC' )
          CALL REWAYPAR_SAVE ( IWT_MODE, PAUSE_REWAY, KFALL_BACK, KNEW_UPDATE, &
     &                         MAX_ITER, CHI_TOL, WT_FLOOR, WT_CEILING )
          goto 4
      else if(action .eq. 'X') then
         knew_update=.not.knew_update
         CALL REWAYPAR_SAVE ( IWT_MODE, PAUSE_REWAY, KFALL_BACK, KNEW_UPDATE, &
     &                        MAX_ITER, CHI_TOL, WT_FLOOR, WT_CEILING )
         goto 4
      else if(action .eq. 'F') then
         kfall_back=.not.kfall_back
         CALL REWAYPAR_SAVE ( IWT_MODE, PAUSE_REWAY, KFALL_BACK, KNEW_UPDATE, &
     &                        MAX_ITER, CHI_TOL, WT_FLOOR, WT_CEILING )
         goto 4
      else if(action .eq. "L") then
         call enter_noise_floor(wt_floor )
         CALL REWAYPAR_SAVE ( IWT_MODE, PAUSE_REWAY, KFALL_BACK, KNEW_UPDATE, &
     &                        MAX_ITER, CHI_TOL, WT_FLOOR, WT_CEILING )
         goto 4
      else if(action .eq. "%") then
         call enter_noise_ceiling(wt_ceiling )
         CALL REWAYPAR_SAVE ( IWT_MODE, PAUSE_REWAY, KFALL_BACK, KNEW_UPDATE, &
     &                        MAX_ITER, CHI_TOL, WT_FLOOR, WT_CEILING )
         goto 4
      else if(action .eq. 'D') then
! display the weights. First calculate them.
        pause_display = .true.
      call display_wts(idatyp,wts0,wts,chi_sq,chi_dof,num_obs_chi, &
     &     iwt_mode,num_wts,nstat,  isitn_chr,pause_display )
       goto 5
      elseif(action.eq.'R') then
! Zero the baseline weights
        do ibase=1,num_bl
           bl_wts_nf(1,ibase)=0.
           bl_wts_nf(2,ibase)=0.
        end do
      elseif(action.eq.'C') then
! JMG Feb1496  Put in reasonable guess
        do ibase=1,num_bl
           bl_wts_nf(1,ibase)=10.
           bl_wts_nf(2,ibase)=100.
        end do
      else if(action .eq. 'N') then
! Re-read the constants.
        call get_bl_wts(bl_wts_nf,idb,idbeg,idend,ksflag )
      else if(action.eq.'E') then
! Enter arbitrary weights
       call enter_wts(idatyp,wts,iwt_mode,num_wts,nstat,isitn_chr )
       call wts_oth_bl(wts,nstat,iwt_mode,bl_wts_nf )
       do ibase = 1,num_bl
         bl_wts_nf(1,ibase) = dsqrt(bl_wts_nf(1,ibase))
         bl_wts_nf(2,ibase) = dsqrt(bl_wts_nf(2,ibase))
       enddo
      else if(action.eq.'U') then
!  update the weights, so that chi-square is unity
       call start_mn()
       call setcr_mn(i4p0,i4p0 )
       call addstr_f("Finding new weights" )
       call nl_mn()
       call adjust_weights(bl_wts_nf,kbatch,knew_update,chi_tol )
!
!
       call wts_bl_oth(bl_wts_nf,iwt_mode,numsta,wts,resid )
       pause_display = .true.
       call display_wts(idatyp,wts0,wts,chi_sq,chi_dof,num_obs_chi, &
     &      iwt_mode,num_wts,nstat,  isitn_chr,pause_display )
      else if(action.eq.'I') then
! write out the baseline wts. (iterate re-reads them from the namfil.)
       call put_bl_wts(bl_wts_nf,idb )
       call iterate(pause_reway,kfall_back,knew_update, &
     &      max_iter,chi_tol )
        call get_bl_wts(bl_wts_nf,idb,idbeg,idend,ksflag )
        call wts_bl_oth(bl_wts_nf,iwt_mode,numsta,wts,resid )
      else if(action .eq. 'M') then
! Change the maximum number of iterations.
         call enter_max_iter(max_iter )
         CALL REWAYPAR_SAVE ( IWT_MODE, PAUSE_REWAY, KFALL_BACK, KNEW_UPDATE, &
     &                        MAX_ITER, CHI_TOL, WT_FLOOR, WT_CEILING )
      else if(action .eq. 'T') then
! change the chi-square tolerance.
         call enter_chi_tol(chi_tol )
         CALL REWAYPAR_SAVE ( IWT_MODE, PAUSE_REWAY, KFALL_BACK, KNEW_UPDATE, &
     &                        MAX_ITER, CHI_TOL, WT_FLOOR, WT_CEILING )
! calculate and write out the baseline covariance info.
      else if(action .eq. 'V') then
        call calc_bl_cov(ibsln, istar,res,sig,sig_fact_sq, &
     &       nobs,nstat )
      else if(action .eq. 'A') then
!  abort! Return to optin without doing anything.
        if(.not.kbatch) call end_mn
        call end_prog()
      ELSE IF ( ACTION .EQ. 'H' ) THEN
!
! ----- Printing on the screen on-line help
!
        CALL REWAY_HELP ( VER_REWAY )
      endif
!
      if(action .eq. 'O'.or. kbatch) then
        if (.not.kbatch) then
          call use_glbfil('OR' )
          if (iwt_mode.eq.0) then
            weighting_type = 'DB'
          else if (iwt_mode .eq. 1) then
            weighting_type = 'ST'
          else if (iwt_mode .eq. 2) then
            weighting_type = 'BL'
          endif
          call use_glbfil   ( 'OWC' )
          CALL REWAYPAR_SAVE ( IWT_MODE, PAUSE_REWAY, KFALL_BACK, KNEW_UPDATE, &
     &                         MAX_ITER, CHI_TOL, WT_FLOOR, WT_CEILING )
        endif
        call wts_oth_bl(wts,nstat,iwt_mode,bl_wts_sq )
        do ibase = 1,num_bl
          bl_wts_nf(1,ibase) = dsqrt(bl_wts_sq(1,ibase))
          bl_wts_nf(2,ibase) = dsqrt(bl_wts_sq(2,ibase))
        enddo
        call put_bl_wts(bl_wts_nf,idb )
!
        call update_resid_sigma(idbeg,idend,nstat,bl_wts_sq, &
     &       sig_raw_sq,sig_fact_sq )
!
        if(.not.kbatch) call end_mn
        call end_prog()
      endif
      goto 5
!
      END  !#!  REWAY  #!#
