      PROGRAM    SLEW_ADJUST
! ************************************************************************
! *                                                                      *
! *   Program SLEW_ADJUST adjusts the a priori slewing model using       *
! *   a VLBI Field System log file of a single dish experiment scheduled *
! *   in sds format and converted to snap/proc using sds_to_snap.py      *
! *   tool. The adjusted parameters are settle time, slewing rate,       *
! *   slewing acceleration for both axes. An experiment should be        *
! *   properly designed in order to separate settle time and slewing     *
! *   acceleration. Considering that some experiments may not enough     *
! *   coverage over azimuth an elevation, SLEW_ADJUST implement an       *
! *   iterative adjustment procedure with guards on minimum and maximum  *
! *   values.                                                            *
! *                                                                      *
! *   Usage: slew_adjust slew-file el|elup|eldn|az                       *
! *                      comp|plot                                       *
! *                      [-tset_min val]                                 *
! *                      [-tset_max val]                                 *
! *                      [-rate_min val]                                 *
! *                      [-rate_max val]                                 *
! *                      [-accl_min val]                                 *
! *                      [-accl_max val]                                 *
! *                      [-az_max val]                                   *
! *                      [-dif_max val]                                  *
! *                      [-n_iter val]                                   *
! *                                                                      *
! *     slew-file -- results of processing a VLBI field system log file  *
! *                  with program sds_slew_process.py . It contains      *
! *                  a priori slewing parameters, computed and actual    *
! *                  slewing time for each observation.                  *
! *     el|az     -- axis: az (for azimuth) and el (for elevation).      *
! *     comp|plot -- compute -- to compute slewing parameters without    *
! *                             making a plot.                           *
! *                  plot    -- to compute slewing parameters and make   *
! *                             a plot.                                  *
! *     tset_min  -- minimum settle time (sec).                          *
! *     tset_max  -- maximum settle time (sec).                          *
! *     rate_min  -- minimum slewing rate (deg/sec).                     *
! *     rate_max  -- maximum slewing rate (deg/sec).                     *
! *     accl_min  -- minimum slewing acceleration (deg/sec^2).           *
! *     accl_max  -- maximum slewing acceleration (deg/sec^2).           *
! *     az_max    -- maximum azimuth arc. Some antennas will not make    *
! *                  arc longer than 180 deg despite being commanded.    *
! *     dif_max   -- maximum allowed difference between estimated        *
! *                  and actual slewing time.                            *
! *     n_iter    -- the number of iterations.                           *
! *                                                                      *
! *  ### 02-JUN-2021   SLEW_ADJUST  v3.0 (c)  L. Petrov  2025.08.06  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4    MO, MIND
      PARAMETER  ( MO   = 32*1024 )
      PARAMETER  ( MIND =   32 )
      CHARACTER  FIL*128, BUF(MO)*512, STR*32, STR1*32, STR2*32, COMMAND_LINE*1024
      REAL*8     AZ, AZ_PREV, EL, EL_PREV, D_AZ, D_EL, SLEW_EST, &
     &           SLEW_AZ, SLEW_EL, ACCL_AZ, ACCL_EL, TSETTLE_AZ, TSETTLE_EL, &
     &           APR_VAL(3,2), APR_COR(3,2), SLEW_AZ_CAL, SLEW_EL_CAL
      REAL*8     SLEW_CAL(MO), SLEW_OBS(MO), DIST(MO,2), DIST_SHT(MO), SLEW_SHT(MO), &
     &           DIST_LNG(MO), SLEW_LNG(MO), T8(MO), X8(MO), Y8(MO), Z8(MO)
      REAL*8     DIF_MAX
      CHARACTER  SLEW_FMT__LABEL*84, MODE*32, STA_NAM*8, DIR*4, AXIS*4
      CHARACTER  OUT(MO)*256, PROG__LABEL*32
      PARAMETER  ( SLEW_FMT__LABEL = '# Slew measurement from a single dish experiment. Format version  1.00 of 2021.06.29' )
      PARAMETER  ( PROG__LABEL = 'SLEW_ADJUST v 3.0  of 2025.08.06' )
      LOGICAL*1  FL_BAD
      INTEGER*4  T__AZ, T__EL
      PARAMETER  ( T__AZ = 1 )
      PARAMETER  ( T__EL = 2 )
      REAL*8     NOR_MAT(6), NOR_VEC(6), EQU_OBS(3), EST_VEC(3), RH, RC, SIG, &
     &           DIST_AZ_ACEL, DIST_EL_ACEL, SLEW_AZ_ACEL, SLEW_EL_ACEL, WEI, &
     &           TSET_MIN, TSET_MAX, RATE_MIN, RATE_MAX, ACCL_MIN, ACCL_MAX, AZ_MAX
      REAL*8     APR(0:2,2), APS(0:2,2), APR_NEW(0:2,2), APR_ORIG(0:2,2), DIST_ACC(2), SLEW_APR(2), SLEW_APS(2)
      REAL*8     RES_VAL, RES_MIN, RES_MAX, RES_AVR, RES_RMS
      REAL*8     SW, SD, ST, SDT, STT, DET, DX, DY
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, NO, IP, &
     &           JP, LP, KP, MP, NP, QP, LIND, IND(2,MIND), &
     &           N_ITER, IVRB, ITYP, NARG, IUER
!!!!!!
!
!  Dcrit = v**2/a
!  t1    = 2 \sqrt ( d/a )
!  t2    = d/v + v/a
!
!!!!!!
!
! --- Defaults
!
      DIF_MAX  =  5.0D0
      TSET_MIN =  0.2D0
      TSET_MAX = 40.0D0
      RATE_MIN =  0.2D0
      RATE_MAX = 20.0D0
      ACCL_MIN =  0.1D0
      ACCL_MAX =  5.0D0
      N_ITER   =  8
!
      AZ_MAX   =  179.D0
      MODE     = 'compute'
!
      IVRB = 2
      CALL GETARG ( 0, COMMAND_LINE )
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, * ) 'Usage slew_adjust slew-file el|elup|eldn|az comp|plot '// &
                          '[-tset_min val] [-tset_max val] '// &
     &                    '[-rate_min val] [-rate_max val] '// &
     &                    '[-accl_min val] [-accl_max val] '// &
     &                    '[-az_max val] [-dif_max val] [-n_iter val]'
           CALL EXIT ( 0 )
         ELSE
!
! -------- Get the slew file name
!
           CALL GETARG (  1, FIL )
           COMMAND_LINE = TRIM(COMMAND_LINE)//' '//FIL
!
! -------- Get the axis
!
           CALL GETARG (  2, AXIS ) 
           CALL TRAN   ( 12, AXIS, AXIS )
           COMMAND_LINE = TRIM(COMMAND_LINE)//' '//AXIS
           IF ( AXIS == 'az' ) THEN
                ITYP = 1
              ELSE IF ( AXIS == 'el'   .OR. &
     &                  AXIS == 'elup' .OR. &
     &                  AXIS == 'eldn'      ) THEN
                ITYP = 2
              ELSE
                IUER = -1
                CALL ERR_LOG ( 7901, IUER, 'SLEW_ADJST', 'Wrong 2nd argument axis '//AXIS// &
     &              'only el, elup, eldn, and az are supported' )
                CALL EXIT ( 1 )
           END IF
           CALL GETARG ( 3, MODE ) 
           CALL TRAN ( 12, MODE, MODE )
           COMMAND_LINE = TRIM(COMMAND_LINE)//' '//MODE
!
! -------- Parse remaining arguments
!
           NARG = 3
           DO 410 J1=1,MIND
!
! ----------- Get the keyword
!
              NARG = NARG + 1
              IF ( NARG > IARGC() ) GOTO 810
              CALL GETARG ( NARG, STR ) 
              COMMAND_LINE = TRIM(COMMAND_LINE)//' '//STR
!
! ----------- And get its value
!
              NARG = NARG + 1
              IF ( NARG > IARGC() ) THEN
                   WRITE ( 6, '(A)' ) 'Error in parsing command line: argument '//TRIM(STR)// &
     &                                ' requires a value'
                   CALL EXIT ( 1 )
              END IF
              CALL GETARG ( NARG, STR1 ) 
!
              COMMAND_LINE = TRIM(COMMAND_LINE)//' '//STR1
              IF ( STR == '-tset_min' ) THEN
                   IF ( INDEX ( STR1, '.' ) < 1 ) STR1 = TRIM(STR1)//'.0'
                   READ ( UNIT=STR1, FMT='(F10.3)', IOSTAT=IUER, IOMSG=STR2 ) TSET_MIN
                ELSE IF ( STR == '-tset_max' ) THEN
                   IF ( INDEX ( STR1, '.' ) < 1 ) STR1 = TRIM(STR1)//'.0'
                   READ ( UNIT=STR1, FMT='(F10.3)', IOSTAT=IUER, IOMSG=STR2 ) TSET_MAX
                ELSE IF ( STR == '-rate_min' ) THEN
                   IF ( INDEX ( STR1, '.' ) < 1 ) STR1 = TRIM(STR1)//'.0'
                   READ ( UNIT=STR1, FMT='(F10.3)', IOSTAT=IUER, IOMSG=STR2 ) RATE_MIN
                ELSE IF ( STR == '-rate_max' ) THEN
                   IF ( INDEX ( STR1, '.' ) < 1 ) STR1 = TRIM(STR1)//'.0'
                   READ ( UNIT=STR1, FMT='(F10.3)', IOSTAT=IUER, IOMSG=STR2 ) RATE_MAX
                ELSE IF ( STR == '-accl_min' ) THEN
                   IF ( INDEX ( STR1, '.' ) < 1 ) STR1 = TRIM(STR1)//'.0'
                   READ ( UNIT=STR1, FMT='(F10.3)', IOSTAT=IUER, IOMSG=STR2 ) ACCL_MIN
                ELSE IF ( STR == '-accl_max' ) THEN
                   IF ( INDEX ( STR1, '.' ) < 1 ) STR1 = TRIM(STR1)//'.0'
                   READ ( UNIT=STR1, FMT='(F10.3)', IOSTAT=IUER, IOMSG=STR2 ) ACCL_MAX
                ELSE IF ( STR == '-dif_max' ) THEN
                   IF ( INDEX ( STR1, '.' ) < 1 ) STR1 = TRIM(STR1)//'.0'
                   READ ( UNIT=STR1, FMT='(F10.3)', IOSTAT=IUER, IOMSG=STR2 ) DIF_MAX
                ELSE IF ( STR == '-az_max' ) THEN
                   IF ( INDEX ( STR1, '.' ) < 1 ) STR1 = TRIM(STR1)//'.0'
                   READ ( UNIT=STR1, FMT='(F10.3)', IOSTAT=IUER, IOMSG=STR2 ) AZ_MAX
                ELSE IF ( STR == '-n_iter' ) THEN
                   READ ( UNIT=STR1, FMT='(I4)', IOSTAT=IUER, IOMSG=STR2 ) N_ITER
                ELSE 
                   WRITE ( 6, '(A)' ) 'Error in parsing command line: '// &
     &                                'unrecognized argument '//TRIM(STR) 
                   CALL EXIT ( 1 )
              END IF
              IF ( IUER .NE. 0 ) THEN
                   WRITE ( 6, '(A)' ) 'Error in parsing command line argument '//TRIM(STR1)// &
     &                                ' : wrong value '//TRIM(STR1)//' -- '//TRIM(STR2)
                   CALL EXIT ( 1 )
              END IF
 410       CONTINUE 
 810       CONTINUE 
      END IF 
!
! --- Read the slew file
!
      IUER = -1 
      CALL RD_TEXT ( FIL, MO, BUF, NP, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Check the label of the slew file
!
      IF ( BUF(1) == SLEW_FMT__LABEL ) THEN
           CONTINUE 
         ELSE
           IUER = -1
           CALL ERR_LOG ( 7902, IUER, 'SLEW_ADJST', 'Wrong magic in the 1st line '// &
     &        'of the input file '//FIL )
           CALL EXIT ( 1 )
      END IF
!
! --- Process that slew file
!
      LP = 0    ! The total number of slews
      NOR_MAT = 0.0D0
      NOR_VEC = 0.0D0
      WRITE ( 6, '(A)' ) '======================================================='
      WRITE ( 6, '(A)' ) PROG__LABEL
      WRITE ( 6, '(A)' ) 'Processed by command '//TRIM(COMMAND_LINE)
!
      DO 420 J2=1,NP
         CALL EXWORD ( BUF(J2), MIND, LIND, IND, CHAR(32)//','//CHAR(9), IUER )
         IF ( BUF(J2)(1:13) == '# Experiment:' ) THEN
              WRITE ( 6, '(A)' ) TRIM(BUF(J2)(3:))
            ELSE IF ( BUF(J2)(1:13) == '# Start date:' ) THEN
              WRITE ( 6, '(A)' ) TRIM(BUF(J2)(3:))//' UTC'
            ELSE IF ( BUF(J2)(1:13) == '# SHORT_NAME:' ) THEN
              STA_NAM = BUF(J2)(IND(1,3):IND(2,3))
            ELSE IF ( BUF(J2)(1:10) == '# SLEW_AZ:'        ) THEN
              READ ( UNIT=BUF(J2)(IND(1,5):IND(2,5)), FMT='(F6.2)' ) SLEW_AZ
              APR_VAL(1,1) = SLEW_AZ
            ELSE IF ( BUF(J2)(1:10) == '# ACCL_AZ:'        ) THEN
              READ ( UNIT=BUF(J2)(IND(1,5):IND(2,5)), FMT='(F6.2)' ) ACCL_AZ
              APR_VAL(2,1) = ACCL_AZ
            ELSE IF ( BUF(J2)(1:10) == '# SLEW_EL:'        ) THEN
              READ ( UNIT=BUF(J2)(IND(1,5):IND(2,5)), FMT='(F6.2)' ) SLEW_EL
              APR_VAL(1,2) = SLEW_EL
            ELSE IF ( BUF(J2)(1:10) == '# ACCL_EL:'        ) THEN
              READ ( UNIT=BUF(J2)(IND(1,5):IND(2,5)), FMT='(F6.2)' ) ACCL_EL
              APR_VAL(2,2) = ACCL_EL
            ELSE IF ( BUF(J2)(1:13) == '# TSETTLE_AZ:'     ) THEN
              READ ( UNIT=BUF(J2)(IND(1,5):IND(2,5)), FMT='(F6.2)' ) TSETTLE_AZ
              APR_VAL(3,1) = TSETTLE_AZ
            ELSE IF ( BUF(J2)(1:13) == '# TSETTLE_EL:'     ) THEN
              READ ( UNIT=BUF(J2)(IND(1,5):IND(2,5)), FMT='(F6.2)' ) TSETTLE_EL
              APR_VAL(3,2) = TSETTLE_EL
            ELSE IF ( BUF(J2)(1:24) == '# 1st_axis_slewing_rate:'        ) THEN
              READ ( UNIT=BUF(J2)(IND(1,4):IND(2,4)), FMT='(F6.2)' ) SLEW_AZ
              APR_VAL(1,1) = SLEW_AZ
            ELSE IF ( BUF(J2)(1:24) == '# 2nd_axis_slewing_rate:'        ) THEN
              READ ( UNIT=BUF(J2)(IND(1,4):IND(2,4)), FMT='(F6.2)' ) SLEW_EL
              APR_VAL(1,2) = SLEW_EL
            ELSE IF ( BUF(J2)(1:24) == '# 1st_axis_slewing_accl:'        ) THEN
              READ ( UNIT=BUF(J2)(IND(1,4):IND(2,4)), FMT='(F6.2)' ) ACCL_AZ
              APR_VAL(2,1) = ACCL_AZ
            ELSE IF ( BUF(J2)(1:24) == '# 2nd_axis_slewing_accl:'        ) THEN
              READ ( UNIT=BUF(J2)(IND(1,4):IND(2,4)), FMT='(F6.2)' ) ACCL_EL
              APR_VAL(2,2) = ACCL_EL
            ELSE IF ( BUF(J2)(1:23) == '# 1st_axis_settle_time:'        ) THEN
              READ ( UNIT=BUF(J2)(IND(1,4):IND(2,4)), FMT='(F6.2)' ) TSETTLE_AZ
              APR_VAL(3,1) = TSETTLE_AZ
            ELSE IF ( BUF(J2)(1:23) == '# 2nd_axis_settle_time:'        ) THEN
              READ ( UNIT=BUF(J2)(IND(1,4):IND(2,4)), FMT='(F6.2)' ) TSETTLE_EL
              APR_VAL(3,2) = TSETTLE_EL
            ELSE IF ( INDEX ( BUF(J2), 'Slewed from az/el' ) > 0 ) THEN
              LP = LP + 1
              READ ( UNIT=BUF(J2)(IND(1,5):IND(2,5)),   FMT='(F8.2)' ) AZ_PREV
              READ ( UNIT=BUF(J2)(IND(1,6):IND(2,6)),   FMT='(F8.2)' ) EL_PREV
              READ ( UNIT=BUF(J2)(IND(1,8):IND(2,8)),   FMT='(F8.2)' ) AZ
              READ ( UNIT=BUF(J2)(IND(1,9):IND(2,9)),   FMT='(F8.2)' ) EL
              READ ( UNIT=BUF(J2)(IND(1,12):IND(2,12)), FMT='(F8.2)' ) SLEW_CAL(LP)
              READ ( UNIT=BUF(J2)(IND(1,15):IND(2,15)), FMT='(F8.2)' ) SLEW_OBS(LP)
!
! ----------- Get slewing distnaces over azimuth and elevation
!
              DIST(LP,T__AZ) = ABS ( AZ - AZ_PREV ) 
              IF ( ABS(DIST(LP,T__AZ) - 360.0) < 0.1 ) DIST(LP,T__AZ) = 0.0
              DIST(LP,T__EL) = ABS ( EL - EL_PREV ) 
         END IF
 420  CONTINUE 
!
! --- Copy the apriori slewing parameters
!
      APR(0,T__AZ) = TSETTLE_AZ
      APR(1,T__AZ) = SLEW_AZ
      APR(2,T__AZ) = ACCL_AZ
      APR(0,T__EL) = TSETTLE_EL
      APR(1,T__EL) = SLEW_EL
      APR(2,T__EL) = ACCL_EL
      APR_ORIG = APR
!
      IF ( MODE(1:3) == 'com' ) THEN
           WRITE ( 6, '(A)' ) ' '
      END IF
!
! --- Cycle over iterations
!
      DO 430 J3=1,N_ITER
!
! ------ Initialization
!
         IP = 0 ! The counter of points used for parameter estimation of settle time and rate
         JP = 0 ! The counte of points used for plotting
         KP = 0 ! The counter of points used for processing
         MP = 0 ! The counter of points with short arcs used for extimation of acceleration
         QP = 0 ! The counter of bypassed points
         SW  = 0.0D0
         SD  = 0.0D0
         ST  = 0.0D0
         SDT = 0.0D0
         STT = 0.0D0
         DO 440 J4=1,LP
!
! --------- DIST_ACC(T_AZ) -- a critical distance over azimuth beyond which antenna uses a constant rate
! --------- Determine slewing time over azimuth
!
            DIST_ACC(T__AZ) = APR(1,T__AZ)**2/APR(2,T__AZ)
            IF ( DIST(J4,T__AZ) > DIST_ACC(T__AZ)  ) THEN
                 SLEW_APR(T__AZ) = APR(0,T__AZ) + DIST(J4,T__AZ)/APR(1,T__AZ) + APR(1,T__AZ)/APR(2,T__AZ) 
               ELSE
                 SLEW_APR(T__AZ) = APR(0,T__AZ) + 2.0D0*DSQRT ( DIST(J4,T__AZ)/APR(2,T__AZ) )
            END IF
!
! --------- DIST_ACC(T_EL) -- a critical distance over azimuth beyond which antenna uses a constant rate
! --------- Determine slewing time over elevation
!
            DIST_ACC(T__EL) = APR(1,T__EL)**2/APR(2,T__EL)
            IF ( DIST(J4,T__EL) > DIST_ACC(T__EL)  ) THEN
                 SLEW_APR(T__EL) = APR(0,T__EL) + DIST(J4,T__EL)/APR(1,T__EL) + APR(1,T__EL)/APR(2,T__EL) 
               ELSE
                 SLEW_APR(T__EL) = APR(0,T__EL) + 2.0D0*DSQRT ( DIST(J4,T__EL)/APR(2,T__EL) )
            END IF
!
! --------- Bypass very long arcs
!
!!            IF ( DIST(J4,T__AZ) > AZ_MAX ) write ( 6, * ) 'MM J4= ', J4, '  DIST= ', DIST(J4,T__AZ) ! %%%%%%%%%%
            IF ( DIST(J4,T__AZ) > AZ_MAX ) GOTO 440
!
            IF ( AXIS == 'az' .AND. SLEW_APR(T__AZ) - APR(0,T__AZ) > SLEW_APR(T__EL) - APR(0,T__EL) ) THEN
                 IF ( DABS( SLEW_OBS(J4) - SLEW_APR(T__AZ) ) > DIF_MAX ) THEN
                      QP = QP + 1 ! Counter of bypassed points
                      GOTO 440
                 END IF
                 KP = KP + 1 ! The counter of points used for processing
                 IF (   MODE == 'plot' .OR. &
     &                ( MODE == '-plot' .AND. DIST(J4,ITYP) < DIST_ACC(ITYP) ) .OR. &
     &                ( MODE == '+plot' .AND. DIST(J4,ITYP) > DIST_ACC(ITYP) )      ) THEN
!
                      JP = JP + 1 ! The counte of poitns used for plotting
                      T8(JP) = DIST(J4,ITYP)
                      X8(JP) = SLEW_OBS(J4)
                      Y8(JP) = SLEW_APR(ITYP)
                 END IF
               ELSE IF ( AXIS == 'el' .AND. SLEW_APR(T__EL) - APR(0,T__EL) > SLEW_APR(T__AZ) - APR(0,T__AZ) ) THEN
                 IF ( DABS( SLEW_OBS(J4) - SLEW_APR(T__EL) ) > DIF_MAX ) THEN
                      QP = QP + 1
                      GOTO 440
                 END IF
                 KP = KP + 1
                 IF (   MODE == 'plot' .OR. &
     &                ( MODE == '-plot' .AND. DIST(J4,ITYP) < DIST_ACC(ITYP) ) .OR. &
     &                ( MODE == '+plot' .AND. DIST(J4,ITYP) > DIST_ACC(ITYP) )      ) THEN
!
                      JP = JP + 1  ! The counter of points to plot
                      T8(JP) = DIST(J4,ITYP)
                      X8(JP) = SLEW_OBS(J4)
                      Y8(JP) = SLEW_APR(ITYP)
                 END IF
               ELSE
!!    write ( 6, * ) 'DD j4= ', j4, ' mp= ', mp, ' ip = ', ip, SLEW_APR(T__AZ) - APR(0,T__AZ), SLEW_APR(T__EL) - APR(0,T__EL)  ! %%%%%%%%55
                 GOTO 440
            END IF
            IF ( DIST(J4,ITYP) > DIST_ACC(ITYP) ) THEN
!
! -------------- Long distance that includes constant rate
!
                 IP  = IP  + 1  ! The counter of points used for parameter estimation of settle time and rate
                 SW  = SW  + 1.0D0
                 SD  = SD  + (SLEW_OBS(J4) - SLEW_APR(ITYP))
                 ST  = ST  + DIST(J4,ITYP)
                 SDT = SDT + (SLEW_OBS(J4) - SLEW_APR(ITYP))*DIST(J4,ITYP)
                 STT = STT + DIST(J4,ITYP)**2
                 DIST_LNG(IP) = DIST(J4,ITYP)
                 SLEW_LNG(IP) = SLEW_OBS(J4)
               ELSE
!
! -------------- Short distance that includes only acceleration and deceleration
!
                 MP = MP + 1 ! The counter of points used for parameter estimation of acceleration
                 DIST_SHT(MP) = DIST(J4,ITYP)
                 SLEW_SHT(MP) = SLEW_OBS(J4)
            END IF
 440     CONTINUE 
         IF ( IP > 1 ) THEN
!
! ----------- Long arc: solve for settle time and slew rate keeping 
! ----------- acceleration time fixed
!
              DET = SW*STT - ST*ST
              DX = SW*SDT - SD*ST
              DY = SD*STT - ST*SDT
              APS(0,ITYP) = DY/DET
              APS(1,ITYP) = DX/DET
            ELSE 
              APS(0,ITYP) = 0.0
              APS(1,ITYP) = 0.0
         END IF
         IF ( MP > 0 ) THEN
!
! ----------- Short arc: Solve for acceleration only keeping settle time and slewing rate fixed
!
              APS(2,ITYP) = 0.0D0
              DO 450 J5=1,MP
                 APS(2,ITYP) = APS(2,ITYP) + 4.0D0*DIST_SHT(J5)/(SLEW_SHT(J5) - (APR(0,ITYP) + APS(0,ITYP)))**2
 450          CONTINUE 
              APS(2,ITYP) = APS(2,ITYP)/MP
            ELSE
              APS(2,ITYP) = 0.0D0
         END IF
!
! ------ Update apriori
!
         APR_NEW(0:1,ITYP) = APR(0:1,ITYP) + APS(0:1,ITYP) 
         APR_NEW(2,ITYP)   = APS(2,ITYP)   
         APR_NEW(0,ITYP)   = MAX ( APR_NEW(0,ITYP), TSET_MIN )
         APR_NEW(1,ITYP)   = MAX ( APR_NEW(1,ITYP), RATE_MIN )
         APR_NEW(2,ITYP)   = MAX ( ACCL_MIN, MIN ( APR_NEW(2,ITYP), ACCL_MAX ) )
!
! ------ Compute residuals
!
         RES_MIN =  1.0D10
         RES_MAX = -1.0D10
         RES_AVR =  0.0D0
         RES_RMS = 0.0D0
         IF ( IP > 0 ) THEN
!
! ----------- First, counting long arcs
!
              DO 460 J6=1,IP
                 SLEW_APS(ITYP) = APR_NEW(0,ITYP) + DIST_LNG(J6)/APR_NEW(1,ITYP) + APR_NEW(1,ITYP)/APR_NEW(2,ITYP) 
                 RES_VAL = SLEW_LNG(J6) - SLEW_APS(ITYP)
                 RES_MIN = MIN ( RES_MIN, RES_VAL )
                 RES_MAX = MAX ( RES_MAX, RES_VAL )
                 RES_AVR = RES_AVR + RES_VAL
                 RES_RMS = RES_RMS + RES_VAL**2
 460          CONTINUE 
         END IF
         IF ( MP > 0 ) THEN
!
! ----------- Second, counting short arcs
!
              DO 470 J7=1,MP
                 SLEW_APS(ITYP) = APR_NEW(0,ITYP) + 2.0D0*DSQRT ( DIST_SHT(J7)/APR_NEW(2,ITYP) )
                 RES_VAL = SLEW_SHT(J7) - SLEW_APS(ITYP)
                 RES_MIN = MIN ( RES_MIN, RES_VAL )
                 RES_MAX = MAX ( RES_MAX, RES_VAL )
                 RES_AVR = RES_AVR + RES_VAL
                 RES_RMS = RES_RMS + RES_VAL**2
 470          CONTINUE 
         END IF
         IF ( IP+MP > 0 ) THEN
              RES_AVR = RES_AVR/(IP+MP)
              RES_RMS = DSQRT ( RES_RMS/(IP+MP) )
            ELSE 
              RES_AVR = 0.0D0
              RES_RMS = 0.0D0
         END IF
!
         IF ( J3 == N_ITER .AND. &
     &        ( MODE == '-plot' .OR. MODE == '+plot' .OR. MODE == 'plot' ) ) THEN
!
! ----------- Prepare plot
!
              CALL SORT8  ( JP, T8, X8 )
              DO 480 J8=1,JP
                 IF ( AXIS == 'az' ) THEN
!
! ------------------- Compute Y8 -- original slewing time
!
                      DIST_ACC(T__AZ) = APR_ORIG(1,T__AZ)**2/APR_ORIG(2,T__AZ)
                      IF ( T8(J8) > DIST_ACC(T__AZ)  ) THEN
                           Y8(J8) = APR_ORIG(0,T__AZ) + T8(J8)/APR_ORIG(1,T__AZ) + APR_ORIG(1,T__AZ)/APR_ORIG(2,T__AZ) 
                        ELSE
                           Y8(J8) = APR_ORIG(0,T__AZ) + 2.0D0*DSQRT ( T8(J8)/APR_ORIG(2,T__AZ) )
                      END IF
!
! ------------------- Compute Z8 -- best fit slewing time
!
                      DIST_ACC(T__AZ) = APR_NEW(1,T__AZ)**2/APR_NEW(2,T__AZ)
                      IF ( T8(J8) > DIST_ACC(T__AZ)  ) THEN
                           Z8(J8) = APR_NEW(0,T__AZ) + T8(J8)/APR_NEW(1,T__AZ) + APR_NEW(1,T__AZ)/APR_NEW(2,T__AZ) 
                        ELSE
                           Z8(J8) = APR_NEW(0,T__AZ) + 2.0D0*DSQRT ( T8(J8)/APR_NEW(2,T__AZ) )
                      END IF
                   ELSE IF ( AXIS == 'el' ) THEN
!
! ------------------- Compute Y8 -- original slewing time
!
                      DIST_ACC(T__EL) = APR_ORIG(1,T__EL)**2/APR_ORIG(2,T__EL)
                      IF ( T8(J8) > DIST_ACC(T__EL)  ) THEN
                           Y8(J8) = APR_ORIG(0,T__EL) + T8(J8)/APR_ORIG(1,T__EL) + APR_ORIG(1,T__EL)/APR_ORIG(2,T__EL) 
                        ELSE
                           Y8(J8) = APR_ORIG(0,T__EL) + 2.0D0*DSQRT ( T8(J8)/APR_ORIG(2,T__EL) )
                      END IF
!
! ------------------- Compute Z8 -- best fit slewing time
!
                      DIST_ACC(T__EL) = APR_NEW(1,T__EL)**2/APR_NEW(2,T__EL)
                      IF ( T8(J8) > DIST_ACC(T__EL)  ) THEN
                           Z8(J8) = APR_NEW(0,T__EL) + T8(J8)/APR_NEW(1,T__EL) + APR_NEW(1,T__EL)/APR_NEW(2,T__EL) 
                        ELSE
                           Z8(J8) = APR_NEW(0,T__EL) + 2.0D0*DSQRT ( T8(J8)/APR_NEW(2,T__EL) )
                      END IF
                 END IF
 480          CONTINUE 
!
              CALL DIAGI_SETDEF ( IUER, 'DIAGI_CTIT', STA_NAM//' slewing time over '//AXIS//' in sec' )
              CALL DIAGI_SETDEF ( IUER, 'DIAGI_UNIT',  AXIS//' in deg' )
              CALL DIAGI_SETDEF ( IUER, 'DIAGI_IPST', 5 )
              CALL DIAGI_3 ( JP, T8, X8, JP, T8, Y8, JP, T8, Z8, IUER )
         END IF
         IF ( MODE(1:9) == 'compute_v' ) THEN
              WRITE ( 6, 110 ) J3, IP, MP, APR_NEW(0:2,ITYP), APS(0:2,ITYP), RES_AVR, RES_RMS
 110          FORMAT ( 'Iter: ', I2, ' # pts ', I3, 1X, I3, &
     &                 ' APR: ', 3(F8.3, 1X), ' APS: ', 3(F8.3,1X), &
     &                 ' RES: ', F6.1, 1X, F6.1 )
         END IF
         APR(0:2,ITYP) = APR_NEW(0:2,ITYP)
 430  CONTINUE 
      IF ( MODE(1:9) == 'compute_v' ) THEN
           WRITE ( 6, '(A)' ) ' '
      END IF
!
      WRITE ( 6, 130 ) STA_NAM, AXIS, APR(0:2,ITYP)
      WRITE ( 6, 130 ) STA_NAM, AXIS, APR(0:2,ITYP)
 130  FORMAT ( A, 2X, A4, ' Settle_time: ', F6.1, ' sec  Slew_rate: ', F6.2, &
     &         ' deg/s,  Slew_accel: ', F6.2, ' deg/sec^2' )
      WRITE ( 6, 140 ) STA_NAM, AXIS, DIF_MAX, &
     &                 STA_NAM, AXIS, KP+QP,   &
     &                 STA_NAM, AXIS, IP+MP,   &
     &                 STA_NAM, AXIS, RES_MIN, &
     &                 STA_NAM, AXIS, RES_MAX, &
     &                 STA_NAM, AXIS, RES_AVR, &
     &                 STA_NAM, AXIS, RES_RMS  
 140  FORMAT ( A, 2X, A4, ' Cutoff for bad observations: ', F7.2, ' sec'/ &
     &         A, 2X, A4, ' # collected observations:    ', I4/ &
     &         A, 2X, A4, ' # used obseration:           ', I4/ &
     &         A, 2X, A4, ' min residual:                ', F6.2, ' sec'/ &
     &         A, 2X, A4, ' max residual:                ', F6.2, ' sec'/ &
     &         A, 2X, A4, ' avr residual:                ', F6.2, ' sec'/ &
     &         A, 2X, A4, ' rms residual:                ', F6.2, ' sec')
      WRITE ( 6, '(A)' ) '======================================================='
!
      CALL TRAN  ( 11, AXIS, AXIS )
      WRITE ( 6, 150 ) AXIS, STA_NAM, APR(1,ITYP)
      WRITE ( 6, 160 ) AXIS, STA_NAM, APR(2,ITYP)
      WRITE ( 6, 170 ) AXIS, STA_NAM, APR(0,ITYP)
 150  FORMAT ( 'SLEW_', A2, ':       ', A8, '  deg/sec      ', F4.1 )
 160  FORMAT ( 'ACCL_', A2, ':       ', A8, '  deg/sec^2    ', F4.1 )
 170  FORMAT ( 'TSETTLE_', A2, ':    ', A8, '  sec          ', F4.1 )
!
      END  PROGRAM  SLEW_ADJUST  !#!#
