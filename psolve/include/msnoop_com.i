!     msnoop common        (handles up to nst stations)
!
      integer*2 nst
      parameter (nst=300)
!
      integer*2 numsta(2)   ! 1 = number station in spoolfile 1
!                           ! 2 = number station in spoolfile 1
!                           + any in spoolfile 2 that were not
!                             in spoolfile 1
!
      real*8 x(2,nst),     y(2,nst),     z(2,nst), &
     &       haj(2,nst),   aaj(2,nst), &
     &       has(2,nst),   aas(2,nst), &
     &       hv(2,nst),    az(2,nst), &
     &       hvs(2,nst),   azs(2,nst), &
     &       xs(2,nst),    ys(2,nst),    zs(2,nst), &
     &       xv(2,nst),    yv(2,nst),    zv(2,nst), &
     &       ev(2,nst),    nv(2,nst),    uv(2,nst), &
     &       ea(2,nst),    na(2,nst),    ua(2,nst), &
     &       eva(2,nst),   nva(2,nst),   uva(2,nst), &
     &       eas(2,nst),   nas(2,nst),   uas(2,nst), &
     &       evs(2,nst),   nvs(2,nst),   uvs(2,nst), &
     &       xvs(2,nst),   yvs(2,nst),   zvs(2,nst), &
     &       elm(2,3,nst), ela(2,3,nst), ele(2,3,nst), &
     &       cor(2,15,nst)
!
      character*23 sitename(2,nst)
      character*5 run_mode
!
      logical kmdebug
!
      common /msnoop_com/ &
     &       numsta, &
     &       x,   y,   z, &
     &       haj, aaj, &
     &       has, aas, &
     &       hv,  az, &
     &       hvs, azs, &
     &       xs,  ys,  zs, &
     &       xv,  yv,  zv, &
     &       ev,  nv,  uv, &
     &       ea,  na,  ua, &
     &       eva,  nva,  uva, &
     &       evs,  nvs,  uvs, &
     &       eas,  nas,  uas, &
     &       xvs, yvs, zvs, &
     &       elm, ela, ele, &
     &       cor, sitename, &
     &       kmdebug, run_mode
!
