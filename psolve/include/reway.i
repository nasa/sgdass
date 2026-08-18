!     Last change:  JG   10 Nov 97    2:37 pm
! common block definitions.
      double precision alpha,beta
      double precision avg_wt(2),avg_wt_sq(2)
      INTEGER*4 nobs
      INTEGER*2 nstat
      INTEGER max_bl
      PARAMETER(max_bl=(max_arc_sta*(max_arc_sta-1)/2))
! everything whose first INDEX is DIM 2 goes over delay/rate
      DOUBLE PRECISION bl_wts_sq(2,max_bl)
! we have one chi_sq for each KIND of weight.
      DOUBLE PRECISION chi_sq(2,max_bl),chi_dof(2,max_bl)
      double precision sig_rms(2,max_bl)
      INTEGER*4 num_obs_chi(2,max_bl)
! observation dependent stuff
      INTEGER*2 ibsln(2,max_obs),istar(max_obs)
      Double Precision res(2,max_obs),sig_raw_sq(2,max_obs)
      double precision sig(2,max_obs),el(2,max_obs)
      double precision sig_fact_sq(2,max_obs)
! imode=0 uniform weights.
!      =1 station weights.
!      =2 baseline weights.
      INTEGER*2 iwt_mode,num_wts
      LOGICAL kdelay,krate
      LOGICAL kuse(2)
      equivalence (kuse(1),kdelay)
      equivalence (kuse(2),krate)
      real*8 way_scale(2)
      double precision wt_floor(2),wt_ceiling(2)
      COMMON /func_com/way_scale,alpha,beta,iwt_mode, &
     & num_wts, &
     & avg_wt,avg_wt_sq, &
     & nstat,nobs, &
     & sig_rms,chi_sq,num_obs_chi,chi_dof, &
     & kuse, bl_wts_sq,ibsln,istar,res,sig,el,sig_raw_sq,sig_fact_sq, &
     & wt_floor,wt_ceiling
