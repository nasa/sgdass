!!SitStr
!
!     Common space to keep the site and source lists.
!
!     Programmers:
!       Gregg Cooke        90.02.08  Creation.
!
!     Modifications:
!       Brent Archinal     95.03.09  Maximum number of sources
!                                    increased from 200 to 500
!                                    and maximum number of baselines
!                                    from 50 to 190 (20 stations).
!                                    Also noted that MAXSIT is the
!                                    maximum number of baselines and not
!                                    the maximum number of stations.
!       Brent Archinal   1999.08.04  Maximum number of sources per
!                                    experiment increased from 500
!                                    to 1000, and maximum number of
!                                    baselines from 190 to 496 (32
!                                    stations).
!
!     Parameters:
!
!     MAXSIT and MAXSTR are the maximum number of baseline and source
!     names allowed in an input or output database.
!
      integer*2  MAXSIT, MAXSTR
      parameter (MAXSIT  = 496, &
     &           MAXSTR  = 1000)
!
!     Specifications:
!
!     Each 'm'-suffixed list is a master, each 'i'-suffixed list is the
!     current includes list, and each 'd'-suffixed list is the current
!     deletes list. Each 'n'-prefixed value is the corresponding list
!     counter. SITE and STAR are the lists of station and source names
!     that actually make it into the current database.
!
      character basem(MAXSIT)*16, sitem(MAXSIT)*8,  starm(MAXSTR)*8, &
     &          basei(MAXSIT)*16, based(MAXSIT)*16, sitei(MAXSIT)*8, &
     &          sited(MAXSIT)*8,  stari(MAXSTR)*8,  stard(MAXSTR)*8, &
     &          site(MAXSIT)*8,   star(MAXSTR)*8
      integer*2 nbasm, nsitm, nstrm, nbasi, nbasd, nsiti, &
     &          nsitd, nstri, nstrd, nsite, nstar
      integer*2 mbas(8,MAXSIT),  mstr(4,MAXSTR), &
     &          lsite(4,MAXSIT), lstar(4,MAXSTR)
      equivalence (basem,mbas), (starm,mstr), &
     &            (site,lsite), (star,lstar)
!
!     Common Space:
!
      common /sitstr/ basem, sitem, starm, basei, based, sitei, &
     &                sited, stari, stard, site,  star,  nbasm, &
     &                nsitm, nstrm, nbasi, nbasd, nsiti, nsitd, &
     &                nstri, nstrd, nsite, nstar
!
