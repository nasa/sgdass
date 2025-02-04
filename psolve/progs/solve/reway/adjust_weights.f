      SUBROUTINE adjust_weights(bl_wts_nf,kbatch,knew_mode,chi_tol)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
      INCLUDE 'solve.i'
!
!
!  adjust weights based on last solution
!
!  socom and glbcm assumed to already be loaded
!
!  modifications:
!
!  jmg 970203
!  kdb 970205 fix 970203 change  (instead of toggling kdelay, code was setting
!             it to .not. krate)
!
!
      double precision bl_wts_nf(2,*)  !current baseline weights
      logical*2 kbatch                 !batch mode?
      logical*2 knew_mode              !new way of adjusting weights.
      double precision chi_tol         !convergence tolerance
!
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'reway.i'
      real*8     wts(2,max_bl)
      real*8     resid(2)
      real*8     fret,fret_old
      INTEGER*2 iter/10/
      INTEGER*2 iwt,idr
      integeR*2  ibl_mode/2/
      INTEGER*2 num_baselines
      integer*2  num_wts_tot
      LOGICAL*4  DATYP_INQ
!
      if(knew_mode) then
         call new_weights(bl_wts_nf )
         return
      endif
!
!  determine solution type
!
!      kdelay=idatyp.ne.6
!      krate=idatyp/3.ne.1
      KDELAY = DATYP_INQ ( IDATYP, DELAY__DTP )
      KRATE  = DATYP_INQ ( IDATYP,  RATE__DTP )
      num_baselines=(numsta*(numsta-1))/2
!
!
      call wts_bl_oth(bl_wts_nf,iwt_mode,numsta,wts,resid )
      call wts_oth_bl(wts,nstat,iwt_mode,bl_wts_sq )
!
! Starting with this initial value, minimize objective function.
! using Numerical Recipes Routine.
!
! modified to be same as chi_tol
!      chi_tol =0.001
      chi_tol=chi_tol
      num_wts_tot=num_wts*2
! we go through this loop twice.
! 1st time we use what ever observables were in the solution to adjust the wts
!     Delay, rate or both.
! 2nd time we use the other observables, if any.
      do idr=1,2
        if(idr .eq. 2) then
! toggle delay,rate
          krate=.not.krate
          kdelay=.not.kdelay
        endif
        if(krate .or. kdelay) then
            WRITE ( 6, * ) 'adjust_wegihgt: This code called routines from Numerical Recipes'
            WRITE ( 6, * ) 'STOP: This code was removed because of copy right concern'
            CALL EXIT ( 1 )
!!          call frprmn(wts,num_wts_tot,chi_tol,iter,fret,kbatch )
          fret_old=fret+2*chi_tol
        endif
        if(idr .eq. 2) then
! toggle delay,rate (restore them)
          krate=.not.krate
          kdelay=.not.kdelay
        endif
      end do
!
! now make bl_wts_nf
!
      call wts_oth_bl(wts,nstat,iwt_mode,bl_wts_sq )
      do idr=1,2
        do iwt=1,num_baselines
          bl_wts_nf(idr,iwt)=sqrt(bl_wts_sq(idr,iwt))
        end do
      end do
!
!
!
!
99999 continue
      return
      end
