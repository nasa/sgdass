      SUBROUTINE tidepart(part_ut1xy)
      IMPLICIT NONE
!
! this routine calculates the partials for the tidal components.
!
!     HISTORY:
!     * Written Dec 11, 1993, by JMGipson.
!     * modified 5/18/93 by MWHayes to be called by partl
!     * modified 8/12/94 by JMgipson to correctly do rate partials.
!
!
      INCLUDE 'solve.i'
      INCLUDE 'oborg.i'
      INCLUDE 'prfil.i'
      INCLUDE 'socom.i'
      INCLUDE 'precm.i'
      INCLUDE 'glbc4.i'
!
!
! place where we store the partial derivatives.
! order is delay/rate, cosine/sine,  (ut1 tides and xy tides)
        real*8 part_ut1xy(2,2,MAX_SDC)
        real*8  fjd_tot
        integer*2 itide,i,i6,ipart
!
! fundamental arguments, derivitive of fundamental argument
! units radians, radians/sec
        real*8 fund_arg(6,2), arg,argd
! sine and cosine
        real*8 ctmp,stmp,scale
        real*8 dotarg
!
!
!
! compute fundamental arguments
!
        fjd_tot = fjd+fract
        call tide_angles(fjd_tot,fund_arg)
!
!
        i6=6
! Scale converts from microseconds to seconds. Minus is because of
! Calc conventions
        scale=-1.d-6
        ipart=1
        do itide=1,num_sde_ut1
! calculate argument, and derivative
          arg =dotarg(sde_arg(1,ipart),fund_arg(1,1),i6)
          argd=dotarg(sde_arg(1,ipart),fund_arg(1,2),i6)
          ctmp=dcos(arg)*scale
          stmp=dsin(arg)*scale
          part_ut1xy(1,1,ipart)=rotp(3,1)*ctmp
          part_ut1xy(2,1,ipart)=rotp(3,2)*ctmp-rotp(3,1)*stmp*argd
          part_ut1xy(1,2,ipart)=rotp(3,1)*stmp
          part_ut1xy(2,2,ipart)=rotp(3,2)*stmp+rotp(3,1)*ctmp*argd
          ipart=ipart+1
        end do
!
! now do XY stuff. scale converts to microradians
        scale = 1./206264806.2
!
        do itide=num_sde_ut1+1,num_sde_ut1+num_sde_xy
          arg =dotarg(sde_arg(1,ipart),fund_arg(1,1),i6)
          argd=dotarg(sde_arg(1,ipart),fund_arg(1,2),i6)
          ctmp=dcos(arg)*scale
          stmp=dsin(arg)*scale
          part_ut1xy(1,1,ipart)=-rotp(1,1)*ctmp+rotp(2,1)*stmp
          part_ut1xy(2,1,ipart)=-rotp(1,2)*ctmp+rotp(2,2)*stmp &
     &                       +(rotp(1,1)*stmp+rotp(2,1)*ctmp)*argd
!
          part_ut1xy(1,2,ipart)= rotp(1,1)*stmp+rotp(2,1)*ctmp
          part_ut1xy(2,2,ipart)=+rotp(1,2)*stmp+rotp(2,2)*ctmp &
     &                       +(rotp(1,1)*ctmp-rotp(2,1)*stmp)*argd
          ipart=ipart+1
        end do
      RETURN
      END
