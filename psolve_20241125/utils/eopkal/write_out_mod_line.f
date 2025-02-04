      SUBROUTINE WRITE_OUT_MOD_LINE ( fjd,x_out,cov_out,itype,ixref, &
     &  kut1s,klod,kplot)
! write out line for mod file
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!
!     Updated to specificaly type integers which
!-------------------------------------------------
! input line
      REAL*8     fjd            !date
      REAL*8     x_out(*)       !parameters to output
      REAL*8     cov_out(*)       !associated covariance
      REAL*8     DOMEGA
      INTEGER*2 itype                   !type of data
!itype=1 data within 0.5 Day
!itype=2 mod file
!itype=-1 none of the above
      INTEGER*2 ixref(3)                !index of X,Y,UT1 into x_out
      LOGICAL*2 kut1s                     !Add ut1s back in?
      LOGICAL*2 klod                      !WRite out LOD?
      LOGICAL*2 kplot                     !write out plot file
!
      integer*2 iyear,imonth,iday,itime
!
! Used to remove UT1S
      REAL*8      cent_julian,dut,dut_dot
      REAL*8      fa(5),fad(5)
!
! EOP values, correlations and sigmas.
      REAL*8      u,x,y,xu_cor,yu_cor,xy_cor
      REAL*8      x_sig,y_sig,u_sig
! this is for outputling LOD
      REAL*8      RLOD,RLOD_sig
!
      INTEGER*4 iu                     !This holds iu converted to integer*4
!
      integer*2 itemp
      INTEGER*4 indx4
!
! itype=1  data on this fjd.
! itype=-1 using filter to extrapolate.
! itype=2  using concrete series.
!
      x = x_out(ixref(1))
      y = x_out(ixref(2))
      u = x_out(ixref(3))
!
!
      x_sig = dsqrt(cov_out(indx4(ixref(1),ixref(1))))
      y_sig = dsqrt(cov_out(indx4(ixref(2),ixref(2))))
      u_sig = dsqrt(cov_out(indx4(ixref(3),ixref(3))))
!
!
      xy_cor=cov_out(indx4(ixref(1),ixref(2)))/(x_sig*y_sig)
      xu_cor=cov_out(indx4(ixref(1),ixref(3)))/(x_sig*u_sig)
      yu_cor=cov_out(indx4(ixref(2),ixref(3)))/(y_sig*u_sig)
!
!
!
!
      if(kut1s) then
        call nutfa(fjd,0.d0,cent_julian,fa,fad )
        call ut1s_83 ( fa, fad, dut, dut_dot, domega )
        u=u+dut*1.d6
      endif
!
!
      if(x_sig .gt. 99) x_sig = 99.99
      if(y_sig .gt. 99) y_sig = 99.99
      iu=u
!
!
      if(klod) then
        itemp=ixref(3)+1
! note: units of rlod are microseconds/day.
! Also, rlod is -dut/dt
        RLOD=-X_out(itemp)
        rlod_sig=dsqrt(cov_out(indx4(itemp,itemp)))
        write(55,'(f9.1,3(f8.2,1x))') fjd, &
     &  (rlod-dut_dot*1.d6), rlod,   rlod_sig
      endif
!
!
      if(kplot) then
        call mdyjl(imonth,iday,iyear,itime,fjd )
        if(klod) then
          write(54,900) iyear,imonth,iday, &
     &    x*1.d3,x_sig*1.d3,y*1.d3,y_sig*1.d3,u,u_sig, &
     &     rlod,rlod_sig
        else
          write(54,900) iyear,imonth,iday, &
     &    x*1.d3,x_sig*1.d3,y*1.d3,y_sig*1.d3,u,u_sig
        endif
      endif
900   format(i6,1x,2i3,1x,2(f9.1,1x,f7.1,1x),1x,f12.1,1x,f6.1,2f8.1)
!
      IF ( (ITYPE .EQ. -1) .OR. (ITYPE .EQ. 2) ) THEN
           U_SIG = -U_SIG
           X_SIG = -X_SIG
           Y_SIG = -Y_SIG
      ENDIF
!
      IF ( XY_COR .GT.  0.999 ) XY_COR =  0.999
      IF ( XY_COR .LT. -0.999 ) XY_COR = -0.999
      IF ( XU_COR .GT.  0.999 ) XU_COR =  0.999
      IF ( XU_COR .LT. -0.999 ) XU_COR = -0.999
      IF ( YU_COR .GT.  0.999 ) YU_COR =  0.999
      IF ( YU_COR .LT. -0.999 ) YU_COR = -0.999
      WRITE ( 2, '(F9.1,F8.4,1X,F7.4,I10,1X,F6.4,1X,F6.4,1X,F7.0, &
     &             1X,F5.3,1X,F5.3,1X,F5.3)') &
     &        FJD, X/100.0D0, Y/100.0D0, IU, X_SIG/100.0D0, Y_SIG/100.0D0, &
     &        U_SIG, XY_COR, XU_COR, YU_COR
!
      RETURN
      END  !#!  WRITE_OUT_MOD_LINE  #!#
