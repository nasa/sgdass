      SUBROUTINE READ_EOPJMG ( fjd_meas,z_meas,cov_meas,num_eop,kuse_rate, &
     &     max_meas,num_meas,kmon,kneos_iris,sig1_max, &
     &     idate_start,idate_end,idate_first,idate_last, &
     &     num_rejects, num_doubles,num_tot)
!
! modifications
! input year is in 4 digits
!
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
! variables defined in argument list.
! A.) Associated with data.
      INTEGER*2 num_eop                !# eop used.
!     Updated to specificaly type integers which
!-------------------------------------------------
!Always use X,Y,U.
!       X_dot,y_dot,ut_dot optional
      INTEGER*4 max_meas               !Maximum number of measurements
      INTEGER*4 num_meas               !Actual number we do.
      REAL*8     fjd_meas(max_meas)        !Epoch
      REAL*8     z_meas(num_eop,max_meas)  !EOP measured
      REAL*8      cov_meas(num_eop*(num_eop+1)/2,max_meas)     !Covariance
! B.) Associated with various flags.
      LOGICAL*2 kuse_rate(3)             !Rate flags
!
      LOGICAL*2 kmon                     !show number being readin, etc.
      LOGICAL*2 kneos_iris               !Use only neos/iris/navnet/polaris
      REAL*8      sig1_max       !maximum value for this sigma
!
      INTEGER*4 idate_start,idate_end  !External limits of data
      integer*4 idate_first,idate_last !limits of data.
!
      integer*2 num_eop_tri            !size of covariance matrix
!
! We read in the file into ldum.
      character*220 ldum,ldum_in,ldum_old,ldum_test
      INTEGER*2 ilen_ldum/220/
!
! This are read in from file.
      REAL*8     date
      integer*2 iyear,imonth,iday,ihour,imin     !Time tags.
      INTEGER*4 idate                  !Date read in
      integer*4 itype                  !Type of measurement
      REAL*8      x,y,u,ur                 !X,Y,U and U_rate values
      REAL*8      x_sig,y_sig,u_sig,ur_sig !          Sigmas
      REAL*8      mjday
!
      REAL*8      xy_cor,xu_cor,yu_cor     !correlation matrix
      REAL*8      ur_u_cor,ur_x_cor,ur_y_cor
!
      INTEGER*4 num_obs
      REAL*8     sig1,sig2,sig3            !Error ellipse stuff
      REAL*8     lat1,lon1,lat2,lon2,lat3,lon3  !Error ellipse stuff
!
      LOGICAL*2 kfirst_double/.true./
      integer*2 len_cdate_test
      character*20 cdate_test
!
!
! Functions.
      REAL*8     fjldy           !Function which returns Julian day.
      integer*2 trimlen
!
! This which are calculated as we go along.
      integer*4 num_rejects            !Number of rejects
      INTEGER*4 num_doubles            !Number of doubles
      INTEGER*4 num_tot                !Total number of experiments.
!
      REAL*8     FRAC_JD         !Fraction of Julian day
!
! temporary arrays.
      REAL*8      z_tmp(num_eop),cov_tmp(num_eop*(num_eop+1)/2)
!
! the following are special kinds of experiments.
      INTEGER*4 iris        /1/        !Kinds of experiments
      INTEGER*4 ipolaris   /10/
      INTEGER*4 ineos      /14/
      INTEGER*4 inavnet    /6/
! Types of experiments from Jim Ryan
!      9,  !GNUT
!      1,  !IRIS
!      1,  !IRIS-A
!      2,  !IRIS-P
!      3,  !IRIS-S
!      5,  !MOBL
!      6,  !NAVNET
!      7,  !NGS
!     10,  !POLA
!      8,  !USNO
!     12,  !JAPAN
!     13,  !NAVEX
!     11,  !NRL
!     15,  !GERMAN
!     17,  !VLBA
!     18,  !NEOS-B
!     14,  !NEOS-A
!     19,  !GSI
!     20,  !CRL
!     21,  !EUR
!     23,  !EURMOB
!     24,  !EUR    USSR
!     22,  !SGP
!     25,  !DSN
!     26,  !IRIS-I
!     27,  !NAVINT
!     28,  !CRF
!     29,  !APT
!     30/  !unknown
!
      num_eop_tri=num_eop*(num_eop+1)/2
!
!
      num_rejects=0
      num_tot=0
      num_doubles=0
!
!
! in this loop  we read in the data.
      num_meas = 0
!
!
! space to start time.
      idate_first = idate_start-1
      do while(idate_first .lt. idate_start)
5       continue
        READ(1,'(a212)', END=400,ERR=5) ldum_in
        READ(ldum_in,'(i4,2i2)',ERR=5) iyear,imonth,iday
! get rid HHMM part of date.
        IF(iyear .GT. 0) idate_first=iyear*10000+imonth*100+iday
      end do
      ldum_test = ldum_in
      call splitstring(ldum_test,cdate_test,ldum_test )
      len_cdate_test =  trimlen(cdate_test)
      if (len_cdate_test.lt.11) then
        write(*,*) "four digit input years are now required"
        write(*,*) "aborting!!"
        stop
      endif
      GOTO 112
!
!
!
!
99    CONTINUE
      WRITE(3,'(a)') ldum_in
      num_rejects=num_rejects+1
!
!
100   continue
      if(kmon.and.mod(num_meas,100) .eq. 0) THEN
        write(*,*) num_meas
!        return
      endif
!
!
      read(1,'(a212)',end=400,err=99) ldum_in
      if(index(ldum_in,"reference") .ne. 0) goto 100
!
!
112   continue
      ldum=ldum_in
      num_tot=num_tot+1
! get rid of |'s and put spaces before -'s
      call clean_ldum(ldum,ilen_ldum )
!
!
      read(ldum,'(i4,i2,i2,i2,i2)') iyear,imonth,iday,ihour,imin
!
!
      read(ldum,*,end=400,err=99) date,mjday,x,y,u,ur, &
     &    x_sig,y_sig,u_sig,ur_sig, xy_cor,xu_cor,yu_cor, &
     &    ur_u_cor,ur_x_cor,ur_y_cor, &
     &    sig1,sig2,sig3,lat1,lon1,lat2,lon2,lat3,lon3, &
     &    num_obs,itype
!
!
      idate=date/10000
      if(idate .gt. idate_end) then
         idate_last=idate
         return
      endif
!
!
! use Chopo's criteria
      if(num_meas .eq. 0) goto 150
      IF(itype .EQ. ipolaris) GOTO 150
!
!
! Always use polaris.
!
! if supposed to use only neos/iris/navnet AND are not one of these, abort.
      IF((idate .ge. 19801104) .and. &
     &    kneos_iris .AND. &
     &   .NOT. (itype .EQ. iris    .OR. &
     &          itype .EQ. inavnet .OR. &
     &          itype .EQ. ineos))                    GOTO 99
!
!
140   continue
      if(sig1  .gt. sig1_max) GOTO 99       !Various tests on error ellipse.
      if(sig3  .gt. .5)       goto 99
      if(sig3+sig2 .gt. 2.)   goto 99
!
!
150   continue
      num_meas=num_meas+1
      if(num_meas .gt. max_meas) then
        num_meas=num_meas-1
        WRITE(*,*) "READ_EOPJMG: Exhausted storage space!"
        WRITE(*,*) "Aborting"
        stop
      endif
!
!
! make covariance matrix
      cov_meas(1,num_meas)=x_sig*x_sig
      cov_meas(2,num_meas)=x_sig*y_sig*xy_cor
      cov_meas(3,num_meas)=y_sig*y_sig
      cov_meas(4,num_meas)=x_sig*u_sig*xu_cor
      cov_meas(5,num_meas)=y_sig*u_sig*yu_cor
      cov_meas(6,num_meas)=u_sig*u_sig
!
!
      frac_jd = float(ihour)/24.+float(imin)/(24.d0*60.d0)
      fjd_meas(num_meas)=fjldy(imonth,iday,iyear)+frac_jd
!
!
      z_meas(1,num_meas) = x
      z_meas(2,num_meas) = y
      z_meas(3,num_meas) = u*1.d6
!
!
! use UT1 rates?
      IF(kuse_rate(3)) then
        z_meas(4,num_meas)= ur
        cov_meas(7, num_meas)= ur_sig* x_sig*ur_x_cor
        cov_meas(8, num_meas)= ur_sig* y_sig*ur_y_cor
        cov_meas(9, num_meas)= ur_sig* u_sig*ur_u_cor
        cov_meas(10,num_meas)= ur_sig* ur_sig
      endif
      call make_cov_positive(cov_meas,num_eop_tri )
! if we have two experiments with the same time, we average them,
! using their correlation matrices.
      IF((num_meas .gt. 1) .and. &
     &    (fjd_meas(num_meas) .eq. fjd_meas(num_meas-1))) then
        call &
     &       add_using_covar(z_meas(1,num_meas-1),cov_meas(1,num_meas-1), &
     &       z_meas(1,num_meas),  cov_meas(1,num_meas), &
     &       z_tmp,cov_tmp, num_eop )
        num_doubles=num_doubles+1
        if(kfirst_double) write(14,'(a)') ldum_old
        kfirst_double=.false.
        write(14,'(a)') ldum_in
        z_meas(1:num_eop, num_meas-1)=z_tmp(1:num_eop)
        cov_meas(1:num_eop,num_meas-1)=cov_tmp(1:num_eop)
        num_meas=num_meas-1
        goto 100
      ENDIF
!
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!      type *,'    num_meas=',num_meas,' fjd = ', fjd_meas(num_meas)    ! %%
!      type *,' x =',x,' y=',y,' u=',u,' ur=',ur                        ! %%
!      type *,' x_sig =',x_sig,' y_sig=',y_sig,' u_sig=',u_sig,         ! %%
!     #                        ' ur_sig=',ur_sig                        ! %%
!        type *,'num_meas=',num_meas,                                  ! %%%
!     #         ' cov_meas = ',( cov_meas(i6,num_meas), i6=1,10 )      ! %%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      ldum_old=ldum_in
      kfirst_double=.true.
      GOTO 100
!
!
400   continue
      RETURN
      END  !#!  READ_EOPJMG  #!#
