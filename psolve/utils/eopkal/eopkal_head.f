      PROGRAM    EOPKAL_LAUNCH
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      CHARACTER    STR*32
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PSOLVE__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL EOPKAL()
      END  PROGRAM  EOPKAL_LAUNCH
!
! ------------------------------------------------------------------------
!
      SUBROUTINE EOPKAL()
! ************************************************************************
! *                                                                      *
! *   Take EOP data file produced by SNOOP,and make a mod file to be     *
! *   used by SOLVE.                                                     *
! *                                                                      *
! *  ###  05-SEP-1997  EOPKAL      v3.1 (c)  J. Gipson 19-JAN-2001  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
      INCLUDE 'solve.i'
      character*64  linfile,loutfile,lerrfile,ldubfile, LSTSFILE*128
      character*64  lplotfile,llodfile
      CHARACTER*10  linfile_type
      integer*4 num_lines
!
! This is stuff from Concrete series.
!
      INTEGER*4 MAX_CON,NUM_CON        ! max number from contcrete
      parameter (max_con=300)
!
! note: concrete only has X,Y, UT1.
! The covariance is set to diagonal components.
!
      REAL*8    fjd_con(max_con)          !time tags of concrete series
      REAL*8     z_con(3,max_con)          !values of concrete
      REAL*8     cov_con(6,max_con)        !and covariance matrix
      REAL*8     time_con_start            !Beginning time of series
      INTEGER*2 num_eop_con/3/
      LOGICAL*2 kuse_rate_con(3)/.false.,.false.,.false./
      REAL*8     time_thres_con/2444340.5/ !Threshold between concrete and VLBI
!
! --- Structures to store input data.
! --- These store EOP estimates from Solve. Can be any of the following
! --- in this order:
!     z_meas = XP,YP,UT1,XP_dot,YP_dot,UT1_Dot
! --- At the moment don't use XP_dot, YP_dot, but leave space for them
!
      INTEGER*2 MAX_EOP, NUM_EOP, MAX_EOP_TRI
      PARAMETER ( MAX_EOP=6, MAX_EOP_TRI=MAX_EOP*(MAX_EOP+1)/2 )
      integer*4 max_meas,num_meas           !Max measurements, actual number
      parameter (max_meas=20000)
      real*8 fjd_meas(max_meas)             ! time of data
      REAL*8 z_meas(max_eop,max_meas)       ! estimated values
      real*8 cov_meas(max_eop_tri,max_meas) ! covariance matrix
!
      LOGICAL*2 kuse_rate(3)                  ! Do we use rate info?
      LOGICAL*2 kmon                          ! monitor progress
      LOGICAL*2 kneos_iris                    ! use only EOP stuff
      LOGICAL*2 kut1s                         ! ut1s removed and added
      LOGICAL*2 klod                      ! output LOD?
      LOGICAL*2 kplot                     ! make file suitable for plotting
      REAL*8        sig1_max          ! maximum value for this
      REAL*8        sig_scale         ! Scale input formal errors by this
      INTEGER*4 idate_start,idate_end       ! limits of data we use
      INTEGER*4 idate_first,idate_last      ! actual limits of data.
      INTEGER*4 num_rejects                 ! number of points rejected
      INTEGER*4 num_doubles                 ! number of simultaneous measurements
      INTEGER*4 num_tot                     ! total number of measurements
      CHARACTER  STR1*75, STR2*75, STR3*75, STR4*75, USER_NAME*128, &
     &           USER_REALNAME*128, USER_E_ADDRESS*128
      CHARACTER  GET_CDATE*19, JD_TO_DATE*23
!
      REAL*8     time_step
!
! Start of data for filter.
! These deal with our model for EOP. This includes stochastic parameters.
!
      INCLUDE  'eopkal_ut.i'                        !Common blocks which contain info
      INCLUDE  'eopkal_pm.i'
      INTEGER*2 max_parm,max_parm_tri       !total number of parms
      PARAMETER ( max_parm=max_pm+max_ut)
      PARAMETER ( max_parm_tri=max_parm*(max_parm+1)/2 )
!
      INTEGER*2 num_parm                 ! Actual number of parms.
      REAL*8     proj(max_parm,max_eop)  ! Mapping from measurement to filter space
      INTEGER*2 ixref(max_eop)
!
! result of forward filter.
!
      REAL*8     x_filt_for(max_parm,max_meas)
      REAL*8     cov_filt_for(max_parm_tri,max_meas)
!
! result of backward filter
!
      REAL*8     x_filt_bck(max_parm,max_meas)
      REAL*8     cov_filt_bck(max_parm_tri,max_meas)
!
! internal variables.
!
      REAL*8     time_start      !First time in mod file
      REAL*8     time_end        !Last time in mod file
      REAL*8     time_cur        !current time
      REAL*8     time_next       !next time to do.
      INTEGER*4  IMEAS
      INTEGER*4  IYEAR_B, IMON_B, IDAY_B, IYEAR_E, IMON_E, IDAY_E, IMON_X
      CHARACTER  DATE_B*10, DATE_BB*26, DATE_E*10, DATE_EE*26, DATE_X*26, &
     &           DB_E*10, MON(12)*3
      CHARACTER  EOP_FORMAT*15, RESERVED_HEADER*15
      DATA       &
     &           MON/ &
     &             'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', &
     &             'JUL', 'AUG', 'SEP', 'OCT', 'NOV', &
     &           'DEC'/
      DATA       EOP_FORMAT      / 'EOP-MOD Ver 2.0' /
      DATA       RESERVED_HEADER / '               ' /
      CHARACTER  LVERSION*54, GET_VERSION*54
      INTEGER*4  NT, J1
      INTEGER*2  IP_ARR(5), INT2_ARG
      INTEGER*4  I_LEN
!
! History:
! Based on old EOP_KAL.
!
!   WHO  WHEN        WHAT
!
!   jmg  98OCT26     Modified to output plot data.
!
!   jmg  98NOV16     Added option to make plot file
!
!   jmg  99JAN11     Writes out error message if concrete is missing
!
!   pet  2000.10.04  Put under CALC/SOLVE installator. Changed version
!                    name scheme. Added a block of extended header comments.
!                    Added sign # before each line of tail comments.
!                    Added support of new format: EOPB. Split the former
!                    eop_kal.f file onto a set of files. Added status file:
!                    status file contains a line "EOPKAL: finished at xxxx"
!                    in the case of success and contains a line
!                    "EOPKAL started at xxxx" in the case of failure.
!                    Added support of alternative command line syntax:
!                    eopkal -i <input_file> -o <output_file> -s <status_file>
!
!   pet  2000.10.06  Added a trap of internal control for checking validity of
!                    time range.
!
!   pet  2001.01.19  Made the first line of the output file compliant with
!                    EOP-MOD Ver. 2 format
!
!   pet  2018.08.06  Fixed a bug: real number ofwas used as a loop counter
!
!CCCCC
      INCLUDE 'eopkal_version.i' ! Set revision date of the current version
!
      LVERSION = GET_VERSION()
      WRITE ( *, * ) LVERSION(1:I_LEN(LVERSION))
      WRITE ( *, * ) "Make eop mod file using Kalman filtering."
      WRITE ( *, * ) " "
      CALL UNPACK_RMPAR ( IP_ARR )
!
! --- Setup this run.
!
      CALL GET_RUN_PARM ( LINFILE, LOUTFILE, LERRFILE, LDUBFILE, LLODFILE, &
     &                    LPLOTFILE, LINFILE_TYPE, LSTSFILE, NUM_EOP, &
     &                    KUSE_RATE, KMON, KNEOS_IRIS, KUT1S, KLOD, KPLOT, &
     &                    KPM_ANNUAL, KPM_LINEAR, KUT_ANNUAL, KUT_SEMI, &
     &                    SIG1_MAX, SIG_SCALE, TIME_STEP, IDATE_START, &
     &                    IDATE_END )
!
! --- Open up various auxiliary files.
!
      OPEN  ( 3, FILE=LERRFILE )
      OPEN ( 14, FILE=LDUBFILE )
!
! --- Output LOD if requested.
!
      IF ( KLOD ) THEN
           WRITE ( *, * ) "LOD written out to file lod.dat"
           CALL MAKE_LOD_CNT()
           OPEN ( 55, FILE=LLODFILE )
           WRITE ( 55, * )  "FJD       LOD    LOD_S    LOD_SIG"
      ENDIF
!
! --- Output plot if requested.
!
      IF ( KPLOT ) THEN
           WRITE ( *, * ) "plot written out to file plot.dat"
           OPEN ( 54, FILE=LPLOTFILE )
           IF ( KLOD ) THEN
                WRITE ( 54, *) &
     &             "Polar motion in microarcseconds, UT1,LOD in microseconds"
                WRITE ( 54, '(A)') &
     &             "Year Mon Day    XPole   X_SIG     YPole    Y_Sig    "// &
     &             "   UT1      UT1_sig   LOD   LOD_Sig"
             ELSE
                WRITE ( 54, *) &
     &             "Polar motion in microarcseconds, UT1 in microseconds"
                WRITE ( 54, '(A)') &
     &             "Year Mon Day   XPole   X_SIG    YPole    Y_Sig    "// &
     &             "   UT1      UT1_sig "
           ENDIF
      ENDIF
!
      NUM_PM = 4
      IF ( KPM_ANNUAL ) NUM_PM = NUM_PM+2
      IF ( KPM_LINEAR ) NUM_PM = NUM_PM+2
      NUM_UT=2
      IF ( KUT_ANNUAL ) NUM_UT=NUM_UT+2
      IF ( KUT_SEMI   ) NUM_UT=NUM_UT+2
      NUM_PARM=NUM_UT+NUM_PM
!
      CALL MAKE_PROJ ( NUM_EOP, KUSE_RATE, PROJ, IXREF )
  10  CONTINUE
!
! --- read in the data.
!
      IF ( LINFILE_TYPE .EQ. "EOPJMG" ) THEN
           CALL READ_EOPJMG ( FJD_MEAS, Z_MEAS, COV_MEAS, NUM_EOP, KUSE_RATE, &
     &                        MAX_MEAS, NUM_MEAS, KMON, KNEOS_IRIS, SIG1_MAX, &
     &                        IDATE_START, IDATE_END, IDATE_FIRST, IDATE_LAST, &
     &                        NUM_REJECTS, NUM_DOUBLES, NUM_TOT )
         ELSE IF ( LINFILE_TYPE .EQ. "IERS" ) THEN
           CALL READ_IERS ( FJD_MEAS, Z_MEAS, COV_MEAS, NUM_EOP, KUSE_RATE, &
     &                      MAX_MEAS, NUM_MEAS, KMON, KNEOS_IRIS, SIG1_MAX, &
     &                      IDATE_START, IDATE_END, IDATE_FIRST, IDATE_LAST, &
     &                      NUM_REJECTS, NUM_DOUBLES, NUM_TOT )
         ELSE IF ( LINFILE_TYPE .EQ. "EOPB" ) THEN
           CALL READ_EOPB ( FJD_MEAS, Z_MEAS, COV_MEAS, NUM_EOP, KUSE_RATE, &
     &                      MAX_MEAS, NUM_MEAS, KMON, KNEOS_IRIS, SIG1_MAX, &
     &                      IDATE_START, IDATE_END, IDATE_FIRST, IDATE_LAST, &
     &                      NUM_REJECTS, NUM_DOUBLES, NUM_TOT )
         ELSE
           WRITE ( *, * ) "Unknown input file type"
           CALL EXIT ( 1 )
      ENDIF
!
! --- Close input file and comment file.
!
      CLOSE(1)
!
! --- Rescale the covariances appropriately.
!
      COV_MEAS=COV_MEAS*SIG_SCALE
!
! --- Remove Yoder terms if necessary.
!
      IF ( KUT1S ) CALL DE_UT1S ( FJD_MEAS, Z_MEAS, NUM_EOP, KUSE_RATE, &
     &                            NUM_MEAS )
      TIME_START = AINT ( FJD_MEAS(1) ) - 15.5D0
      TIME_CON_START = TIME_START -1.0D0
      TIME_END  = AINT ( FJD_MEAS(NUM_MEAS) ) + 15.0D0
!
      IF ( TIME_START .LE. 2440000.5D0  .OR.  TIME_START .GE. 2490000.5D0 ) THEN
           WRITE ( *, * ) 'Wrong value of TIME_START: ',TIME_START
           WRITE ( *, * ) 'Please check whether input file complies with '// &
     &                     LINFILE_TYPE(1:I_LEN(LINFILE_TYPE))//' format'
           CALL EXIT ( 1 )
      END IF
!
      IF ( TIME_END .LE. 2440000.5D0  .OR.  TIME_END .GE. 2490000.5D0 ) THEN
           WRITE ( *, * ) 'Wrong value of TIME_END: ',TIME_END
           WRITE ( *, * ) 'Please check whether input file complies with '// &
     &                     LINFILE_TYPE(1:I_LEN(LINFILE_TYPE))//' format'
           CALL EXIT ( 1 )
      END IF
!
! --- Write header of mod file.
!
      NUM_LINES = (TIME_END-TIME_START+1)/TIME_STEP
      WRITE ( 2, &
     &        '(A15, 2X, F9.1, 2X, F4.1, 2X, I5, 2X, A8, 2X, A8, 1X, A15 )' )EOP_FORMAT, TIME_START, TIME_STEP, NUM_LINES, 'UT1-TAI ', &
     &       'UNDEF   ', RESERVED_HEADER
!@      WRITE ( 2, '(F9.1, F4.2, I4, 1X, "UT1 ", 7X, A, 5X,
!@     #        "Do tail for details")' ) TIME_START, TIME_STEP, NUM_LINES,
!@     #         LVERSION(1:22)
      CALL CLRCH ( STR1 )
      STR1 = '# Generated by '//LVERSION
      WRITE ( 2, '(A)' ) STR1
!
! --- Format the date of the first session
!
      IYEAR_B =  IDATE_FIRST/10000
      IMON_B  = (IDATE_FIRST - IYEAR_B*10000)/100
      IDAY_B  = (IDATE_FIRST - IYEAR_B*10000 - IMON_B*100 )
      WRITE ( UNIT=DATE_B, FMT='(I4,".",I2,".",I2)' ) IYEAR_B, IMON_B, IDAY_B
      CALL BLANK_TO_ZERO ( DATE_B )
      DATE_BB = JD_TO_DATE ( TIME_START, -3 )
!
! --- Format the date of the last session
!
      IYEAR_E =  IDATE_LAST/10000
      IMON_E  = (IDATE_LAST - IYEAR_E*10000)/100
      IDAY_E  = (IDATE_LAST - IYEAR_E*10000 - IMON_E*100 )
      WRITE ( UNIT=DATE_E, FMT='(I4,".",I2,".",I2)' ) IYEAR_E, IMON_E, IDAY_E
      CALL BLANK_TO_ZERO ( DATE_E )
      DATE_EE = JD_TO_DATE ( TIME_END, -3 )
!
! --- Format date of the last session which is good for processing
!
      DATE_X = JD_TO_DATE ( TIME_END-8.D0, -3 )
      CALL BLANK_TO_ZERO ( DATE_X )
      CALL CHIN ( DATE_X(6:7),  IMON_X  )
      IF ( IMON_X .GE. 1  .AND.  IMON_X .LE. 12 ) THEN
           DB_E = '$'//DATE_X(3:4)//MON(IMON_X)//DATE_X(9:10)//'zz'
         ELSE
           DB_E = '$?????????'
      END IF
!
      CALL CLRCH ( STR1 )
      CALL CLRCH ( STR2 )
      CALL CLRCH ( STR3 )
      CALL CLRCH ( STR4 )
      STR1 = '# Detailed description: '// &
     &       'ftp://gemini.gsfc.nasa.gov/pub/misc/jmg/eop_kal.pdf'
      STR2 = '# Analysis center: '//CENTER_ABR//' ( '//CENTER_FULL_NAME//' )'
      STR3 = '# Local time: '//GET_CDATE()
      CALL GETINFO_USER   ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
      STR4 = '# Analyst: '//USER_REALNAME(1:I_LEN(USER_REALNAME))// &
     &                     ' ( '//USER_E_ADDRESS(1:I_LEN(USER_E_ADDRESS))//' )'
!
!!      write ( 6 , * ) fjd_meas(1:num_meas) ! %%%%%%%%%%%%%%%
      WRITE  ( 2, 110 ) STR1, STR2, STR3, STR4, LINFILE(1:60), &
     &                  NUM_MEAS, DATE_B, FJD_MEAS(1), &
     &                            DATE_E, FJD_MEAS(NUM_MEAS), &
     &                            DATE_BB(1:10), DATE_EE(1:10), DB_E
 110  FORMAT ( '# Earth orientation parameters series from VLBI smoothed ', &
     &         'by Kalman filter  '/ &
     &         '# Software: CALC/SOLVE + eopkal ', 42X, ' ' / &
     &         A/ &
     &         A/ &
     &         A/ &
     &         A/ &
     &         '#',73X, ' ' / &
     &         '# Input file: ',A, ' '/ &
     &         '# Number of used points in input file: ',I6, 29X, ' ' / &
     &         '# First session: ',A,'   MJD: ',F11.3, 28X,' ' / &
     &         '# Last session:  ',A,'   MJD: ',F11.3, 28X,' ' / &
     &         '# Output values are in range: [ ',A,', ',A,' ] ', 17X,' ' / &
     &         '#',73X,' '/ &
     &         '# Good for processing session not after ',A, 24X,' '/ &
     &         '#',73X,' ' )
!
! --- 0. Use Concrete series to give shape of early data before 80APR12.
!
      IF ( TIME_START .LT. TIME_THRES_CON ) THEN
           NUM_CON = TIME_THRES_CON - TIME_START + 7
           IF ( NUM_CON .GT. MAX_CON ) THEN
!
! ------------- Should never reach here.
!
                WRITE ( *, * ) 'TIME_THRES_CON = ',TIME_THRES_CON
                WRITE ( *, * ) 'TIME_START     = ',TIME_START
                WRITE ( *, * ) 'NUM_CON = ',NUM_CON,' MAX_CON=',MAX_CON
                WRITE ( *, * ) "Main: Help! not enough space for concrete."
                WRITE ( *, * ) "Please recompile."
                CALL EXIT ( 2 )
           ENDIF
!
! -------- Now read in concrete file
!
           WRITE ( *, * ) "Reading in concrete file. "
           CALL READ_CON ( FJD_CON, Z_CON, COV_CON, TIME_CON_START, NUM_CON )
!
! -------- De-Yoder concrrete if required.
!
           IF ( KUT1S ) CALL DE_UT1S ( FJD_CON, Z_CON, NUM_EOP_CON, &
     &                                 KUSE_RATE_CON, NUM_CON )
!
! --------- ...and use it to write out first few measurements...
!
            NT = NINT(TIME_THRES_CON - TIME_START)
            DO 410 J1=1,NT
               TIME_CUR = TIME_START + (J1-1)*TIME_STEP
               CALL WRITE_OUT_USING_CONCRETE ( TIME_CUR, FJD_CON, Z_CON, COV_CON, &
     &              NUM_CON, FJD_MEAS, Z_MEAS, COV_MEAS, NUM_EOP, NUM_MEAS, &
     &              KUT1S )
 410        CONTINUE 
            TIME_START=TIME_CUR
      ENDIF
!
! --- Now do forward and reverse filters
!
13    CONTINUE
      CALL FORWARD_FILTER ( FJD_MEAS, Z_MEAS, COV_MEAS, NUM_EOP, NUM_MEAS, &
     &                      X_FILT_FOR, COV_FILT_FOR, NUM_PARM, PROJ, KMON )
!
! --- and backward filter
!
      CALL BACKWARD_FILTER ( FJD_MEAS, Z_MEAS, COV_MEAS, NUM_EOP, NUM_MEAS, &
     &                       X_FILT_BCK, COV_FILT_BCK, NUM_PARM, PROJ, KMON )
!
! --- Read to start making mod file
! --- 1) Extrapolate backward from current position.
!
      IMEAS=1
      CALL EXTRAPOLATE ( FJD_MEAS, X_FILT_BCK, COV_FILT_BCK, NUM_PARM, IMEAS, &
     &                   TIME_START, FJD_MEAS(1), TIME_STEP, IXREF, KUT1S, &
     &                   KLOD, KPLOT, TIME_NEXT )
!
! --- 2) Now start the smoothing.
!
      TIME_START=TIME_NEXT
      CALL KALMAN_SMOOTHER ( FJD_MEAS, X_FILT_BCK, COV_FILT_BCK, &
     &                       X_FILT_FOR, COV_FILT_FOR, NUM_PARM, NUM_MEAS, &
     &                       TIME_START, FJD_MEAS(NUM_MEAS), TIME_STEP, IXREF, &
     &                       KUT1S, KMON, KLOD, KPLOT, TIME_NEXT )
!
! --- 3.) Extrapolate beyond end of data.
!
      TIME_START = TIME_NEXT
      TIME_END = FJD_MEAS(NUM_MEAS) + 15.D0
      IMEAS = NUM_MEAS
      CALL EXTRAPOLATE ( FJD_MEAS, X_FILT_FOR, COV_FILT_FOR, NUM_PARM, IMEAS, &
     &                   TIME_START, TIME_END, TIME_STEP, IXREF, KUT1S, KLOD, &
     &                   KPLOT, TIME_NEXT )
!
! --- We are done with calculations.  A little cleanup.
!
      CALL SUMMARIZE_RUN_PARM_INFO ( LINFILE, LOUTFILE, LERRFILE, LDUBFILE, &
     &                               LINFILE_TYPE, KUSE_RATE, KMON, KNEOS_IRIS, &
     &                               KUT1S, KPM_ANNUAL, KPM_LINEAR, KUT_ANNUAL, &
     &                               KUT_SEMI, SIG1_MAX, SIG_SCALE, TIME_STEP, &
     &                               IDATE_START, IDATE_END, IDATE_FIRST, &
     &                               IDATE_LAST, NUM_REJECTS, NUM_DOUBLES, &
     &                               NUM_TOT )
!
 200  CONTINUE
      CLOSE(2)
      CLOSE(4)
      CLOSE(5)
      IF ( KPLOT )  THEN
           CLOSE(54)
      END IF
!
      IF ( KLOD ) THEN
           CLOSE(55)
      END IF
!
      OPEN ( UNIT=3, FILE=LSTSFILE, STATUS='UNKNOWN' )
      WRITE ( 3, '(A)' ) "EOPKAL: finished at "//GET_CDATE()
      CLOSE ( UNIT = 3 )
      END  SUBROUTINE  EOPKAL  !w#!#
