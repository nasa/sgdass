!
!     Last change:  JG    7 Mar 96    4:25 pm
      SUBROUTINE wts_oth_bl(wts,nstat,iwt_mode,bl_wts_sq)
! given a set of weights corresponding to ibsln, then make bl_wts.
! input weights are either global/station/baseline weights
!
! on entry
!    wts = wts
!    nstat   -- number of stations.
!    iwt_mode = 0   global
!            1   station
!            2   baseline
! on exit
!     bl_wts=baseline wts.
! order is (2,1),  (3,1),(3,2),  (4,1),(4,2),(4,3) etc.
!
        implicit none
! the first index on wts is delay/rate
      DOUBLE PRECISION wts(2,*),bl_wts_sq(2,*)
      INTEGER*2 nstat,iwt_mode
! the first index here labels the station.
!
      INTEGER*2 num_baselines,ibase
      INTEGER*2 idr
      INTEGER*2 ibase_index
      integer*2 i1,i2,ibsln(2)
      equivalence (ibsln(1),i1)
      equivalence (ibsln(2),i2)
!
      num_baselines=(nstat*(nstat-1))/2
      do idr=1,2
       IF(iwt_mode .EQ. 0) then
         do ibase=1,num_baselines
             bl_wts_Sq(idr,ibase)=wts(idr,1)**2.
         end do
       ELSEIF(iwt_mode .EQ. 1) then
         do i1=1,nstat
         do i2=i1+1,nstat
           bl_wts_sq(idr,ibase_index(ibsln,nstat))= &
     &        wts(idr,i1)**2.+wts(idr,i2)**2.
         end do
         end do
       ELSEIF(iwt_mode .EQ. 2) then
         do ibase=1,num_baselines
            bl_wts_sq(idr,ibase)=wts(idr,ibase)**2.
         end do
       ENDIF
      END do
      return
      end
!********************************************************************************
! return a unique integer for a given baseline.
      INTEGER*2 function ibase_index(ibsln,nstat)
! this is a pointer into baseline array in namfil order:
!  (1,2), (1,3),     (1,n)
      IMPLICIT NONE                         !Added by IMP/jwr
!         (2,3),..   (2,n)
!               ...  (n-1,n)
      INTEGER*2 ibsln(2)
      integer*2 j,k,nstat
      if(ibsln(1) .lt. ibsln(2) ) then
          j=ibsln(1)
          k=ibsln(2)
      else
          j=ibsln(2)
          k=ibsln(1)
      endif
      ibase_index=k-(j*(j+1))/2+nstat*(j-1)
      return
      end
!***************************************************
      integer*4 function inverse_base(iwt,nstat)
! return baseline corresponding to iwt.
      IMPLICIT NONE                         !Added by IMP/jwr
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 k
!-----END of imp added lines.
!
      integer*2 ibsln(2)
      integer*4 itemp4
      equivalence (ibsln(1),itemp4)
      equivalence (ibsln(1),j)
      equivalence (ibsln(2),k)
      integeR*2 iwt
      integer*2 nstat,i,j
      integer*2 ibase_index
      integer*2 num_baselines
      num_baselines=iwt*(iwt-1)/2
!
      do j=1,nstat
         do k=j+1,nstat
           if(ibase_index(ibsln,nstat) .eq. iwt) then
             inverse_base=itemp4
             return
           endif
         end do
      end do
      return
      end
