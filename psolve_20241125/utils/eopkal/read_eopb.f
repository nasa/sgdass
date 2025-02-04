      SUBROUTINE READ_EOPB ( FJD_MEAS, Z_MEAS, COV_MEAS, NUM_EOP, KUSE_RATE, &
     &                       MAX_MEAS, NUM_MEAS, KMON, KNEOS_IRIS, SIG1_MAX, &
     &                       IDATE_START, IDATE_END, IDATE_FIRST, IDATE_LAST, &
     &                       NUM_REJECTS, NUM_DOUBLES, NUM_TOT )
! ************************************************************************
! *                                                                      *
! *   Reads EOP series in EOPB format
! *                                                                      *
! *  ### 02-OCT-2000    READ_EOPB  v2.0 (c)  L. Petrov  04-JUN-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! changes: assume 4 digit year in input format
!
!     Updated to specificaly type integers which
!-------------------------------------------------
! variables defined in argument list.
! A.) Associated with data.
      INTEGER*2 num_eop                !# eop used.
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
! We read in the file into ldum.
      character*256 ldum,ldum_in,ldum_old
      INTEGER*2     ilen_ldum / 256 /
!
! This are read in from file.
!
      INTEGER*2 IYEAR, IMONTH, IDAY    ! Time tags.
      INTEGER*4 idate                  ! Date read in
      CHARACTER   DB_NAME*10, SCODE*6
      REAL*8      X, Y, U, UR                    ! X, Y, U and U_rate values
      REAL*8      X_SIG, Y_SIG, U_SIG, UR_SIG    ! Sigmas
!
      REAL*8      xy_cor,xu_cor,yu_cor     !correlation matrix
!
      REAL*8      psi_sig,eps_sig,psi,eps  !read,not_used
      REAL*8      psi_eps_cor              !read,not used.
      INTEGER*4 num_obs
!
      LOGICAL*2 kfirst_double/.true./
!
! Functions.
      integer*2 get_leapsec_jmg            !Function which returns leapsec
      integer*2 num_eop_tri            !size of covariance matrix
!
!
      INTEGER*4 ios            ! used in leap second.
      REAL*8    xleap_Vec(5)   ! Used for removing leapsecond
      REAL*8    DMJD
!
! This which are calculated as we go along.
      integer*4 num_rejects            !Number of rejects
      INTEGER*4 num_doubles            !Number of doubles
      INTEGER*4 num_tot                !Total number of experiments.
!
      REAL*8     UR_U_COR, UR_X_COR, UR_Y_COR, DURA, WRMS
      REAL*8     XPR, YPR, XR_SIG, YR_SIG
      INTEGER*4  IUER
      CHARACTER  DATE_STR*23, JD_TO_DATE*23
      INTEGER*4, EXTERNAL :: ILEN
!
! temporary arrays.
      REAL*8      z_tmp(num_eop), cov_tmp(num_eop*(num_eop+1)/2)
!
!   WHO   WHEN        WHAT
!
!   pet   2000.10.03  Created by taken as a sample read_iers and read_eopjmg
!
!CCCCC
      NUM_REJECTS = 0
      NUM_TOT     = 0
      NUM_DOUBLES = 0
      NUM_EOP_TRI = NUM_EOP*(NUM_EOP+1)/2
      IDATE_LAST  = 0
!
! --- In this loop  we read in the data.
!
      NUM_MEAS = 1
      IDATE_FIRST = IDATE_START-1
      DO WHILE ( IDATE_FIRST .LT. IDATE_START )
5        CONTINUE
         READ ( 1, '(A)', END=400, ERR=5  ) LDUM_IN
         READ ( LDUM_IN, '(2X,F12.6)', ERR=5  ) DMJD
         IF ( LDUM_IN(1:1)  .EQ. '#' ) GOTO 5
         IF ( ILEN(LDUM_IN) .EQ.  0  ) GOTO 5
!
         READ ( LDUM_IN, FMT='(1X, F12.6 )', END=400, ERR=9 ) DMJD
         IUER = -1
         DATE_STR = JD_TO_DATE ( 2400000.5000001D0 + DMJD, IUER )
         READ ( UNIT=DATE_STR, FMT='(I4,1X,I2,1X,I2)' ) IYEAR, IMONTH, IDAY
         IF ( IYEAR .GT. 0 ) IDATE_FIRST = IYEAR*10000 + IMONTH*100 + IDAY
      END DO
!
      GOTO 12
!
! --- start of processing EOPB style
! ... Write out rejects
!
 9    CONTINUE
      WRITE ( 3, '(a)' ) LDUM_IN
      NUM_REJECTS=NUM_REJECTS+1
10    CONTINUE
      IF ( KMON .AND. MOD  (NUM_MEAS, 100 ) .EQ. 0 ) THEN
           WRITE ( *, 210 ) NUM_MEAS, CHAR(13)
 210       FORMAT ( 'Read line ',I6,'  ',A$ )
      ENDIF
!
      READ ( 1, '(A)', END=400, ERR=9 ) LDUM_IN
      IF ( INDEX ( LDUM_IN, "reference" ) .NE. 0 ) GOTO 10
 12   CONTINUE
      LDUM=LDUM_IN
      NUM_TOT=NUM_TOT+1
!
      READ ( LDUM, FMT=110, END=400, ERR=9 ) DMJD, DB_NAME, SCODE, &
     &       X, Y, U, PSI, EPS, XPR, YPR, UR, &
     &       X_SIG, Y_SIG, U_SIG, PSI_SIG, EPS_SIG, XR_SIG, YR_SIG, UR_SIG, &
     &       XY_COR, XU_COR, YU_COR, PSI_EPS_COR, UR_U_COR, UR_X_COR, UR_Y_COR, &
     &       DURA, WRMS, NUM_OBS
 110  FORMAT ( 2X, F12.6,1X, A10,1X, A6,1X, &
     &  F8.6,1X, F8.6,1X, F11.7,1X, F8.3,1X, F8.3,1X, F9.6,1X, F9.6,1X, F7.4,1X, &
     &  F8.6,1X, F8.6,1X, F9.7,1X,  F7.3,1X, F7.3,1X, F9.6,1X, F9.6,1X, F7.4,1X, &
     &  F6.4,1X, F6.4,1X, F6.4,1X, F6.4,1X, F6.4,1X, F6.4,1X, F6.4,1X, &
     &  F5.2,1X, F7.2,1X, I6 )
!!        type *,' xy_cor=',xy_cor, ' xu_cor=',xu_cor,' yu_cor=',yu_cor ! %%%
!!        type *,' db_name=',db_name,' x_sig=',x_sig, ' y_sig=',y_sig,' u_sig=',u_sig ! %%%
!!        type *,' db_name=',db_name,' x=',x,' y=',y,' u=',u ! %%%
!!        type *,' scode=',scode,' x=',x,' y=',y,' u=',u ! %%%
      X = X *1.D3
      Y = Y *1.D3
      X_SIG = X_SIG *1.D3
      Y_SIG = Y_SIG *1.D3
      U_SIG = U_SIG *1.D6
!
!      UR = 0.0
!      UR_SIG = 0.0
!      UR_U_COR = 0.0
!      UR_X_COR = 0.0
!      UR_Y_COR = 0.0
!
      IUER = -1
!
! --- Get Julian day
!
      FJD_MEAS(NUM_MEAS) = 2400000.5000001D0 + DMJD
      DATE_STR = JD_TO_DATE ( FJD_MEAS(NUM_MEAS), IUER )
      READ ( UNIT=DATE_STR, FMT='(I4,1X,I2,1X,I2)' ) IYEAR, IMONTH, IDAY
      IF ( IYEAR .GT. 0 ) IDATE = IYEAR*10000 + IMONTH*100 + IDAY
!
      IF ( NUM_MEAS .EQ. 1 ) IDATE_FIRST = IDATE
      IF ( IDATE .GT. IDATE_END ) THEN
           IDATE_LAST=IDATE
           RETURN
      ENDIF
!
! --- Make co_variance matrix
!
      COV_MEAS ( 1, NUM_MEAS ) = X_SIG*X_SIG
      COV_MEAS ( 2, NUM_MEAS ) = X_SIG*Y_SIG*XY_COR
      COV_MEAS ( 3, NUM_MEAS ) = Y_SIG*Y_SIG
      COV_MEAS ( 4, NUM_MEAS ) = X_SIG*U_SIG*XU_COR
      COV_MEAS ( 5, NUM_MEAS ) = Y_SIG*U_SIG*YU_COR
      COV_MEAS ( 6, NUM_MEAS ) = U_SIG*U_SIG
!
!
      Z_MEAS(1,NUM_MEAS) = X
      Z_MEAS(2,NUM_MEAS) = Y
      Z_MEAS(3,NUM_MEAS) = U*1.D6
!!      IOS = GET_LEAPSEC_JMG ( FJD_MEAS(NUM_MEAS), XLEAP_VEC )
!!      Z_MEAS(3,NUM_MEAS) = ( U - XLEAP_VEC(2) )*1.D6
!
! --- Use UT1 rates?
!
      IF ( KUSE_RATE(3) ) THEN
           Z_MEAS(4,NUM_MEAS)    = UR
           COV_MEAS(7, NUM_MEAS) = UR_SIG* X_SIG*UR_X_COR
           COV_MEAS(8, NUM_MEAS) = UR_SIG* Y_SIG*UR_Y_COR
           COV_MEAS(9, NUM_MEAS) = UR_SIG* U_SIG*UR_U_COR
           COV_MEAS(10,NUM_MEAS) = UR_SIG* UR_SIG
      ENDIF
!
      CALL MAKE_COV_POSITIVE ( COV_MEAS, NUM_EOP_TRI )
!
! --- if we have two experiments with the same time, we average them,
! --- C using their correlation matrices.
!
      IF ( NUM_MEAS .GT. 1 ) THEN
           IF ( FJD_MEAS(NUM_MEAS) .EQ. FJD_MEAS(NUM_MEAS-1) ) THEN
                CALL ADD_USING_COVAR ( Z_MEAS(1,NUM_MEAS-1), &
     &                                 COV_MEAS(1,NUM_MEAS-1), &
     &                                 Z_MEAS(1,NUM_MEAS),  &
     &                                 COV_MEAS(1,NUM_MEAS), &
     &                                 Z_TMP,COV_TMP, NUM_EOP )
                IF ( KFIRST_DOUBLE ) WRITE ( 14, '(A)' ) LDUM_OLD
                KFIRST_DOUBLE = .FALSE.
                WRITE ( 14, '(A)' ) LDUM_IN
                NUM_DOUBLES = NUM_DOUBLES+1
                Z_MEAS(1:NUM_EOP,NUM_MEAS-1)   = Z_TMP(1:NUM_EOP)
                COV_MEAS(1:NUM_EOP,NUM_MEAS-1) = COV_TMP(1:NUM_EOP)
                GOTO 10
           ENDIF
      ENDIF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!      type *,'    num_meas=',num_meas,' fjd = ', fjd_meas(num_meas)    ! %%
!      type *,' x =',x,' y=',y,' u=',u,' ur=',ur                        ! %%
!      type *,' x_sig =',x_sig,' y_sig=',y_sig,' u_sig=',u_sig,         ! %%
!     #                        ' ur_sig=',ur_sig                        ! %%
!        type *,'num_meas=',num_meas,                                   ! %%
!     #         ' cov_meas = ',( cov_meas(iday,num_meas), iday=1,10 )   ! %%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      LDUM_OLD=LDUM_IN
      KFIRST_DOUBLE=.TRUE.
!
      NUM_MEAS = NUM_MEAS+1
      IF ( NUM_MEAS .LE. MAX_MEAS ) THEN
           GOTO 10
        ELSE
           WRITE ( *, * )  "Exhausted storage space!"
           CALL EXIT ( 3 )
      ENDIF
 400  CONTINUE
      NUM_MEAS = NUM_MEAS - 1
      WRITE ( *, 220 ) NUM_MEAS
 220  FORMAT ( 1X/I6,' lines were read from the input EOP file' )
      IDATE_LAST = IDATE
!
      RETURN
      END  !#!  READ_EOPB  #!#
