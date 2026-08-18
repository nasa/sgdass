      SUBROUTINE UT1PM_INT(T,tbinfo,ytable,yout,rate,mode1,mode2, &
     &yp1,ypn,y2,xtable,initialized)
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     Performs a linear, cubic,or cubic spline interpolation for UT1PM mapping
!     and optional rate computation. Originally copied from program CALC
!     and subroutine intrp_eomod.
!
!     :93.12.15:jwr: Modified to use cubic spline from Numerical Recipes.
!     01-JUL-99      modified by P. Tomasi y2(2) into y2(*) and int --> int1
!                    in order to avoid conflict with intrinsic function
!
      REAL*8 T,tt,tbinfo(3),ytable(*),yout,y1(2),y2(*),xint(4), &
     &       s,f2,rate
      INTEGER*4 ilast, int1, n, nn, nr, iuer
      INTEGER*2 mode1,mode2
      character*150 errstr
      REAL*8,  ALLOCATABLE :: WORK_ARR(:)
!
!----------------------------------------------------------------------------
!     Input:
!     T         - time of observation in J.D.
!     tbinfo(3) - info on tabular data points: 1) J.D. of first point,
!                 2) interval in days, 3) number of points.
!     ytable(7) - tabular data (X-wobble, Y-wobble, or TAI-UT1)
!     mode1     - interpolation mode,
!                 1 --> linear interpolation,
!                 3 --> cubic interpolation, and
!                 4 --> cubic spline interpolation.
!     mode2     - integer to control compution of rate,
!                 0 --> don't compute rate (set rate = 0.0),
!                 1 --> compute rate
!
!     Output:
!     yout      - value of the desired quantity at time T interpolated
!                 from the data in ytable.
!     rate      - derivative of the data in ytable at time T, i.e. slope
!                 or rate-of-change of the line at time T
!
!     Input and Output: The follow are computed here during initialization and
!                 passed up the the calling routine and must be passed back for
!                 for subsequent interpolations.
!     initiallized: Must be set to false by calling program to start cubic spline.
!                 Reset to true here.
!     yp1 & ypn   Estimates of the first derivatives of the function at the epochs
!                 of first and last points in the table.
!     y2        - 2nd derivatives of the function. Computed in spline initialization
!                 and then use in the evaluations.
!     xtable    - the spline needs a table of independent values, so it is constructed
!                 during initialization and passed in and out.
!
      integer*4   npoint, ierr4
!     real*8      yp1,ypn,y2(*),xtable(*), jd_first,interval
!
! Modified by P. Tomasi July 1 1999 y2(*) already defined
!
      real*8      yp1,ypn,xtable(*), jd_first,interval
      logical*2   initialized
      real*8      ydot,ydot2,ydot3
      integer*2   i
!----------------------------------------------------------------------------
!     HISTORY
!   who   when     what
!   DG    910701   Created for new UT1PM mapping scheme
!   DG    910816   Name changed to ut1pm_int.f. Linear interpolation
!                  mode added.
!   DG    910827   Added code for rate (time derivative) computation
!   DG    910925   Changed to print error message and terminate if
!                  interpolation mode not set to 1 or 3.
!   :93.12.15:jwr: Substantially modified to support cubic spline interpolation.
!   2022.02.25 pet Updated to remove non-open source code
!
!-------------------------------------------------------------------
!     Here is the new code for cubic spline interpolation.
      If(mode1.eq.4) then ! cubic spline interpolation
        jd_first = tbinfo(1)
        interval = tbinfo(2)
        npoint   = tbinfo(3)+.001
        if(.not.initialized) then ! must initialize
!         Make simple estimates of the 1st derivatives at the first and
!         last points.
!
          ALLOCATE ( WORK_ARR(NPOINT) )
          yp1 = (ytable(     2)-ytable(       1))/interval
          ypn = (ytable(npoint)-ytable(npoint-1))/interval
          do i = 1,npoint
            xtable(i) = jd_first + interval*(i-1)
          enddo
!!old          call spline(xtable,ytable,npoint,yp1,ypn,y2,ierr4 )
          IERR4 = -1
          CALL MAKE_SPLINE ( 2, NPOINT, XTABLE, YTABLE, YP1, YPN, Y2, WORK_ARR, IERR4 )
          if(ierr4.ne.0) then
            write(errstr,'("ut1pm_int: Unable to ititialize spline.")')
            call ferr( INT2(220), errstr, INT2(0), INT2(0) )
          endif
          DEALLOCATE ( WORK_ARR )
          initialized = .true.
        endif
!
!       Do the spline interpolafion.
!!old        call splint4(xtable,ytable,y2,npoint,t,yout,ydot,ydot2,ydot3,ierr4 )
        IERR4 = -1
        CALL SPLINT4 ( XTABLE, YTABLE, Y2, NPOINT, T, YOUT, YDOT, YDOT2, YDOT3, IERR4 )
        if(ierr4.ne.0) then
          write(errstr,'( &
     &    "ut1pm_int: Unable to spline interpolate for jd = ",f20.7)') t
          call ferr( INT2(220), errstr, INT2(0), INT2(0) )
        endif
!
        if(mode2.ne.0) then
!         Convert the time units from per table interval to per day.
          rate = ydot/interval
        else
          rate = 0.d0
        endif
!
        return
      endif
!
!--------------------------------------------------------------------
!     Below is the old code for linear and simple cubic interpolation.
!
!     Convert time to the units of the interval of the table relative
!     to the first point of the table
      tt = (T - tbinfo(1))/tbinfo(2)
      int1= IDINT(tt)
      tt = tt - dfloat(int1)
      int1= int1- 1
      if(abs(tt) .lt. 1.d-9) then
        tt = tt + 1.d0
        int1= int1- 1
      endif
!
      IF (mode1.eq.3) then         ! cubic interpolation
!       Select the 4 tabular points. Verify that the interpolation is not
!       outside the range of the table.
        ilast = IDINT(tbinfo(3))
        do n=1,4
          nn = int1+ n
!         Make sure we're within the range of the tables
          if((nn.lt.1) .or. (nn.gt.ilast)) then
            write(errstr,150)
            call ferr( INT2(201), errstr, INT2(0), INT2(0) )
          endif
          xint(n) = ytable(nn)
        end do
!
!       Interpolate in the table
        do n=1,2
          nr = n+1
          f2 = (xint(nr+1) + xint(nr-1))/6.D0
          y1(n) =  (4.D0/3.D0)*xint(nr) - f2
          y2(n) = -(1.D0/3.D0)*xint(nr) + f2
        end do
!
        s = 1.D0 - tt
        yout = ((tt * (y1(2) + tt**2 * y2(2)) + s * (y1(1) + &
     &           s**2 * y2(1))))
!
!       Rate computation
        if(mode2.eq.1) then
         rate = (y1(2) + 3.D0*y2(2)*tt**2 - y1(1) - 3.D0*y2(1)*s**2)/ &
     &          tbinfo(2)
        else
          rate = 0.0D0
        endif
        return
      endif
!
      IF(mode1.eq.1) then     ! linear interpolation
!       Make sure we're within the range of the tables
        if(int1+2 .lt. 1 .or. int1+3 .gt. dint(tbinfo(3)+.0001)) then
          write(errstr,150)
  150     format(' Interpolating outside UT1PM mod file. Terminating', &
     &           ' in subroutine ut1pm_int!')
          call addstr_f(errstr )
          call nl_mn()
          write(errstr,'(" tbinfo ",i10,2i5," int",i10)') tbinfo,int1
          call addstr_f(errstr )
          call nl_mn()
          call refresh_mn()
          call ferr( INT2(202), "ut1pm_int", INT2(0), INT2(0) )
          stop
        endif
!
        yout = ((ytable(int1+3) - ytable(int1+2)) * tt) + ytable(int1+2)
!
!       Rate computation
        if(mode2.eq.1) then
          rate = (ytable(int1+3) - ytable(int1+2)) / tbinfo(2)
        else
          rate = 0.0D0
        endif
!
!
       return
      ENDIF
!
!     Oops! we have problems. If we are here then the interpolation flag
!     was not set properly (must be 1 or 3). We refuse to continue if
!     this happens. Tell the user we are quitting.
!
      write(errstr,165) mode1
  165 format(' Interpolation mode = ',I3,':  Terminating in ', &
     &        'subroutine ut1pm_int!!!')
      call ferr( INT2(220), errstr, INT2(0), INT2(0) )
      return
      end
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPLINT4 ( TIM, VAL, SPL, MP, XP, YP, YDOT, YDOT2, YDOT3, IUER )
      IMPLICIT   NONE 
      INTEGER*4  MP, IUER 
      REAL*8     TIM(MP), VAL(MP), SPL(MP), XP, YP, YDOT, YDOT2, YDOT3
      INTEGER*4  IXC
      INTEGER*4, EXTERNAL :: IXMN8 
      REAL*8,    EXTERNAL :: FSPL8, DSPL8, D2SPL8, D3SPL8
!
      IXC = IXMN8 ( MP, TIM, XP )
      IF ( IXC < 1 ) THEN
           IUER = 1
         ELSE 
           IUER = 0
      END IF
      YP = FSPL8 ( XP, MP, TIM, VAL, IXC, SPL )
      YDOT  = DSPL8  ( XP, MP, TIM, VAL, IXC, SPL )
      YDOT2 = D2SPL8 ( XP, MP, TIM, VAL, IXC, SPL )
      YDOT3 = D3SPL8 ( XP, MP, TIM, VAL, IXC, SPL )
      RETURN
      END SUBROUTINE SPLINT4 
