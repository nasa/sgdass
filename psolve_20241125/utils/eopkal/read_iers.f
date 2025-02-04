      SUBROUTINE READ_IERS ( FJD_MEAS, Z_MEAS, COV_MEAS, NUM_EOP, KUSE_RATE, &
     &                       MAX_MEAS, NUM_MEAS, KMON, KNEOS_IRIS, SIG1_MAX, &
     &                       IDATE_START, IDATE_END, IDATE_FIRST, IDATE_LAST, &
     &                       NUM_REJECTS, NUM_DOUBLES, NUM_TOT )
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
      character*220 ldum,ldum_in,ldum_old,ldum_test
      INTEGER*2 ilen_ldum/220/
!
! This are read in from file.
!
      integer*2 iyear,imonth,iday,ihour,imin     !Time tags.
      INTEGER*4 idate                  !Date read in
      integer*4 itype                  !Type of measurement
      REAL*8      x,y,u                    !X,Y,U and U_rate values
      REAL*8      x_sig,y_sig,u_sig !          Sigmas
      REAL*8      w_d_cor
      REAL*8      mjday
!
      REAL*8      xy_cor,xu_cor,yu_cor     !correlation matrix
!
      REAL*8      psi_sig,eps_sig,psi,eps  !read,not_used
      REAL*8      psi_eps_cor              !read,not used.
      INTEGER*4 num_obs
      INTEGER*2 i6
!
      LOGICAL*2 kfirst_double/.true./
!
! Functions.
      integer*2 get_leapsec_jmg            !Function which returns leapsec
      REAL*8     fjldy           !Function which returns Julian day.
!
      INTEGER*4 ios                    !used in leap second.
      REAL*8      xleap_Vec(5)   !Used for removing leapsecond
!
! This which are calculated as we go along.
      integer*4 num_rejects            !Number of rejects
      INTEGER*4 num_doubles            !Number of doubles
      INTEGER*4 num_tot                !Total number of experiments.
!
      REAL*8     FRAC_JD         !Fraction of Julian day
!
      character*20 cdate_test
      integer*2 trimlen,len_cdate_test
!
! temporary arrays.
      REAL*8      z_tmp(num_eop),cov_tmp(num_eop*(num_eop+1)/2)
      INTEGER*4 ipolaris   /10/
!
! the following are special kinds of experiments.
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
      NUM_REJECTS=0
      NUM_TOT=0
      NUM_DOUBLES=0
!
! --- In this loop  we read in the data.
!
      NUM_MEAS = 1
      IDATE_FIRST = IDATE_START-1
      DO WHILE ( IDATE_FIRST .LT. IDATE_START )
5        CONTINUE
         READ ( 1, '(A212)', END=400, ERR=5 ) LDUM_IN
         READ ( LDUM_IN, '(I4,2I2)', ERR=5  ) IYEAR, IMONTH, IDAY
!
! ------ get rid HHMM part of date.
!
         IF ( IYEAR .GT. 0 ) IDATE_FIRST = IYEAR*10000 + IMONTH*100 + IDAY
      END DO
!
      LDUM_TEST = LDUM_IN
      CALL SPLITSTRING ( LDUM_TEST, CDATE_TEST, LDUM_TEST )
      LEN_CDATE_TEST =  TRIMLEN ( CDATE_TEST )
      IF ( LEN_CDATE_TEST .LT. 11 ) THEN
           WRITE ( *, * ) "four digit input years are now required"
           WRITE ( *, * ) "aborting!!"
           CALL EXIT ( 2 )
      ENDIF
      GOTO 12
!
! --- start of processing IERS style
!  ... WRite out rejects
!
 9    CONTINUE
      WRITE ( 3, '(a)' ) LDUM_IN
      NUM_REJECTS=NUM_REJECTS+1
10    CONTINUE
      IF ( KMON .AND. MOD  (NUM_MEAS, 100 ) .EQ. 0 ) THEN
           WRITE ( *, * ) NUM_MEAS
      ENDIF
!
      READ ( 1, '(A202)', END=400, ERR=9 ) LDUM_IN
      IF ( INDEX ( LDUM_IN, "reference" ) .NE. 0 ) GOTO 10
 12   CONTINUE
      LDUM=LDUM_IN
      NUM_TOT=NUM_TOT+1
!
! --- get rid of |'s and put spaces before -'s
!
      CALL CLEAN_LDUM ( LDUM, ILEN_LDUM )
      READ ( LDUM, '(I4, I2, I2, I2, I2)' ) IYEAR, IMONTH, IDAY, IHOUR, IMIN
      READ ( LDUM, *, END=400, ERR=9 ) IDATE, MJDAY, X, Y, U, PSI, EPS, &
     &       X_SIG, Y_SIG, U_SIG, PSI_SIG, EPS_SIG, &
     &       W_D_COR, XY_COR, XU_COR, YU_COR, PSI_EPS_COR, &
     &       NUM_OBS, ITYPE
!
      IDATE = IDATE/10000
      IF ( IDATE .GT. IDATE_END ) THEN
           IDATE_LAST=IDATE
           RETURN
      ENDIF
!
! --- Convert from units of .1 msec to microseconds
!
      U_SIG = U_SIG*100.0D0
!
! --- use Chopo's criteria
!
      IF ( NUM_MEAS .EQ. 1     ) GOTO 15
      IF ( ITYPE .EQ. IPOLARIS ) GOTO 15   ! Always use Polaris data.
      IF ( U_SIG .LT. 100      ) GOTO 15           ! UT1 threshold
!
! --- will skip remaining points
! --- UNLESS A.) Single baseline experiment (Correlations high) AND
!         B.) u_sig <1000.
!
      IF ( (ABS(XY_COR) .GT. 0.9 ) .AND. ( ABS(XU_COR) .GT. 0.9 ) .AND. &
     &     (ABS(YU_COR) .GT. 0.9 ) .AND. ( U_SIG .LT. 1000.0 )         ) GOTO 15
      GOTO 9
15    CONTINUE
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
      I6=6
      CALL MAKE_COV_POSITIVE ( COV_MEAS, I6 )
!
! --- Get Julian day
!
      FRAC_JD = FLOAT(IHOUR)/24. + FLOAT(IMIN) / ( 24.D0 * 60.D0 )
      FJD_MEAS ( NUM_MEAS ) = FJLDY ( IMONTH, IDAY, IYEAR ) + FRAC_JD
      IOS = GET_LEAPSEC_JMG ( FJD_MEAS(NUM_MEAS), XLEAP_VEC )
!
      Z_MEAS(1,NUM_MEAS) = X*1.D3
      Z_MEAS(2,NUM_MEAS) = Y*1.D3
      Z_MEAS(3,NUM_MEAS) = ( U - XLEAP_VEC(2) )*1.D6
!
! --- if we have two experiments with the same time, we average them,
! --- C using their correlation matrices.
!
      IF ( ( NUM_MEAS .GT. 1 ) .AND. &
     &     ( FJD_MEAS(NUM_MEAS) .EQ. FJD_MEAS(NUM_MEAS-1) ) ) THEN
           CALL ADD_USING_COVAR ( Z_MEAS(1,NUM_MEAS-1), COV_MEAS(1,NUM_MEAS-1), &
     &                            Z_MEAS(1,NUM_MEAS),  COV_MEAS(1,NUM_MEAS), &
     &                            Z_TMP,COV_TMP, NUM_EOP )
           IF ( KFIRST_DOUBLE ) WRITE ( 14, '(A)' ) LDUM_OLD
           KFIRST_DOUBLE = .FALSE.
           WRITE ( 14, '(A)' ) LDUM_IN
           NUM_DOUBLES = NUM_DOUBLES+1
           Z_MEAS(1:NUM_EOP,NUM_MEAS-1)   = Z_TMP(1:NUM_EOP)
           COV_MEAS(1:NUM_EOP,NUM_MEAS-1) = COV_TMP
           GOTO 10
      ENDIF
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
400   CONTINUE
      RETURN
      END  !#!  READ_IERS  #!#
