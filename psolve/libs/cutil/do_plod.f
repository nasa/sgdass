      SUBROUTINE do_plod()
      implicit none
!
!     This routine calculates user calibrations and adds them
!     to the observation theoretical delays/rates
!     This version adds the atmosphere pressure correction at
!     each station. If pressure is outside the range [600,1100]mb
!     it is computed from the temperature and site height by
!     extrapolating up from sea level pressure.
!     Used T. Herring's seasonal temperature model for this temp.
!
!     HISTORY:
!
!     * written by D.MacMillan 11/23/92
!     modified 6/1/93 by MWH for use within SOLVE
!
!
      INCLUDE 'solve.i'
      INCLUDE 'oborg.i'
      INCLUDE 'prfil.i'
      INCLUDE 'socom.i'
      INCLUDE 'precm.i'
      INCLUDE 'glbc4.i'
!
      real*8 calib(2)               ! Delay and rate calibrations
      real*8 pravg(200),prave(20)     ! Average pressure at each site
      character*80 CBUFF
      integer*2 IBUFF(40),np,ista
      real*8 mm_sec,mm_usec,degrad
      real*8 tc,pres,relh,tb,lat1,ht,stap(3),phi,elon,h
      integer*4 isign(2)
      integer*2 ksite(4),jsite(4,200),i,j,k,n,nd
      character*12 dum2
      real*8 dum1,dum3,temp
      logical*2 equal
!
      data isign/-1,1/                ! delay -> station 2-station 1
!
      mm_sec = 1.d-3/vlight      ! sec/mm ; partial units [sec/__]
      mm_usec = 1.d03/vlight     ! usec/mm ; delay units
!
!     Read observation file to obtain information to compute
!     user partial for each observation
!
!
        do np = 1,2    ! loop over stations 1 and 2
          ISTA = ISITE(np)   ! station number
!
!    Check that pressure is OK
               temp=tempc(np)
               if(atmpr(np) .lt. 600 .or. atmpr(np).gt. 1100) then
                 do j=1,3
                 stap(j)=VSITEC(j,ISTA)     ! from prfil.i
                 end do
                call plh(stap,phi,elon,h)   ! convert xyz to latlonhgt
                ht = h/1000.               ! m to km
                call met_seasonal(tc,pres,relh,tb,phi,ht)
               else
                pres = atmpr(np)
               end if
!
!              add pressure term of  mm/mb:
               calib(1) = -isign(np)*mm_usec*sin(elev(np)) &
     &                          *sitpld(ISTA)*(pres-refpres(ista))
               dt = dt + calib(1)
!
        end do        ! end loop over stations in pair
!
      RETURN
      END
