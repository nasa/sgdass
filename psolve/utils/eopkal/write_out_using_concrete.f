        SUBROUTINE WRITE_OUT_USING_CONCRETE ( date, &
     &  fjd_con, z_con, cov_con, num_con, &
     &  fjd_meas,z_meas,cov_meas,num_eop,num_meas,kut1s)
! on entry
!   date -- julian date to write out values
!   fjd_con,z_ocn,cov_con -same for concrete
!   num_con             -- number of concrete points
!   fjd_meas,z_meas,cov_meas-- times, values and covariances measured using VLBI
!   num_meas            -- number of points
!
! This routine writes out values using concrete values and sigmas,
! assuming an offset between concrete values and VLBI values.
! The main reason it exists is to write out EOP values for first few experiemnts
! where we don't have enough data to use VLBI.
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      integer*4 num_con,iptr,iptr0
      INTEGER*2 num_eop                                   !number of EOP measured
!     Updated to specificaly type integers which
!-------------------------------------------------
      INTEGER*4 num_meas
      REAL*8      fjd_meas(*),z_meas(num_eop,*)     !Epoch, EOP values measured
      REAL*8      cov_meas(num_eop*(num_eop+1)/2,*) !Covariance .
      LOGICAL*2 kut1s
!
      REAL*8      fjd_con(*), z_con(3,*),cov_con(6,*)
      REAL*8      date
      REAL*8      eop_vec(3),eop_vec1(4),eop_vec2(4),eop_off(3)
      REAL*8      eop_cov(10),eop_cov1(6),eop_cov2(6)
      REAL*8      date1,date2
!
      integeR*2 itype
      INTEGER*2 ixref(3)/1,2,3/
      LOGICAL*2 klod/.false./                             !don't write out LOD
      LOGICAL*2 kplot/.false./                            !or Plot file
!
! find interpolated value using concrete series.
      call interpolate_concrete(date, &
     &                fjd_con,z_con,cov_con,num_con,eop_vec,eop_cov )
!
!
! three possibilities:
!  1.)  date occurs prior to first measured eop value.
!  2.)  date occurs after first eop value.
!  3.)  date occurs after first, and before last.
!
! now find where in mjd series date is.
      iptr = 1
      do while((fjd_meas(iptr).le.date).and.(iptr.lt.num_meas))
        iptr=iptr+1
      end do
!
!
! find offset between concrete and VLBI series for 1st and last case.
      if((iptr .eq. 1) .or. &
     &   ((iptr .eq. num_meas) .and. (fjd_meas(iptr) .lt. date))) then
        date1 = fjd_meas(iptr)
        DATE2 = FJD_MEAS(IPTR)
        call interpolate_concrete(date1, &
     &                fjd_con,z_con,cov_con,num_con,eop_vec1,eop_cov1 )
!
!
        eop_off(1:3)=z_meas(1:3,iptr)-eop_vec(1:3)
      else if(fjd_meas(iptr) .gt. fjd_con(num_con)) then
   write ( 6,  * ) 'WRITE_OUT_USING_CONCRETE  A2' ! %%%
         do while(fjd_meas(iptr) .gt. fjd_con(num_con))
          iptr=iptr-1
         end do
        date1 = fjd_meas(iptr)
        DATE2 = FJD_MEAS(IPTR)
        call interpolate_concrete(date1, &
     &                fjd_con,z_con,cov_con,num_con,eop_vec1,eop_cov1 )
        eop_off(1:3)=z_meas(1:3,iptr)-eop_vec(1:3)
      else
        date1=fjd_meas(iptr-1)
        date2=fjd_meas(iptr)
        call interpolate_concrete(date1, &
     &                fjd_con,z_con,cov_con,num_con,eop_vec1,eop_cov1 )
        call interpolate_concrete(date2, &
     &                fjd_con,z_con,cov_con,num_con,eop_vec2,eop_cov2 )
! compute offset.  Varies linearly between two measured values.
        eop_off(1:3)= &
     &     (z_meas(1:3,iptr-1)-eop_vec1(1:3))*(date2-date)/(date2- &
     &   date1)+ (z_meas(1:3,iptr) - eop_vec2(1:3))*(date-date1)/(date2-date1)
       endif
! find total eop value.
      eop_vec=eop_vec+eop_off
!
!
      if(abs(date1-date) .lt. .5) then
         itype = 1
         iptr0=max(1,iptr-1)
         call write_out_mod_line(date,eop_vec,cov_meas(1,iptr0),itype, &
     &        ixref,kut1s,klod,kplot )
      else if(abs(date2-date) .lt. .5) then
         itype = 1
         call write_out_mod_line(date,eop_vec,cov_meas(1,iptr),itype, &
     &        ixref,kut1s,klod,kplot )
      else
        itype =-1
        call write_out_mod_line(date,eop_vec,eop_cov,itype, &
     &       ixref,kut1s,klod,kplot )
      endif
!
      RETURN
      END  !#!  WRITE_OUT_USING_CONCRETE   #!#
