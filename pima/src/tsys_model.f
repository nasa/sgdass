      SUBROUTINE TSYS_MODEL ( N,  TIME, ELEV, TSYS, &
     &                        NM, TIME_MOD,   TSYS_MOD, &
     &                        TSYS_ZEN, TSYS_T0, ELEV_T0, &
     &                        NT, TIME_T,     TSYS_T, &
     &                        NE, ELEV_E,     TSYS_E, &
     &                        NO, TIME_OUT, ELEV_OUT, TSYS_OUT, &
     &                        IREF_INI, FLAG_INI, TSYS_ZEN_MEAN, TSYS_RMS, &
     &                        TSYS_MIN, TSYS_MAX, TSYS_0, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  TSYS_MODEL  computes parameters of the model of system    *
! *   temperature and calculates a number of output arrays related to    *
! *   the system temperature.                                            *
! *                                                                      *
! *   Measurements of the system temperature recorded at a station are   *
! *   presented in the form.                                             *
! *                                                                      *
! *      T_sys = T_o * a(t) * b(e)                                       *
! *                                                                      *
! *   where  a(t) is a function of time represented by a linear spline;  *
! *          b(e) -- function of elevation represented by linear spline. *
! *          T_o  -- minimal system temperature.                         *
! *          Functions a(t) and b(e) are normalized to have minimal      *
! *          value 1.0                                                   *
! *                                                                      *
! *          Parameter T_o and coefficients of the spline a(t) and b(e)  *
! *   are found by iterative non-linear LSQ.                             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *        N ( INTEGER*4 ) -- Input number of points: number of values   *
! *                           of time tag, elevation angle and system    *
! *                           temperature at the station during an       *
! *                           experiment.                                *
! *     TIME ( REAL*8    ) -- Array of time tags of the observations in  *
! *                           seconds elapsed from the first observation.*
! *                           Dimension: N.                              *
! *     ELEV ( REAL*8    ) -- Array of the elevation angles of the       *
! *                           sources observed at the station (in rad).  *
! *                           Dimension: N.                              *
! *     TSYS ( REAL*8    ) -- Array of system temperatures in K.         *
! *                           Dimension: N.                              *
! *     IVRB ( INTEGER*4 ) -- Verbosity mode. IVRB = 0 -- silent mode,   *
! *                           IVRB=1 -- normal mode, IVRB>1 -- debugging *
! *                           mode.                                      *
! *       NO ( INTEGER*4 ) -- Number of points for the output Tsys.      *
! * TIME_OUT ( REAL*8    ) -- Array of time tags of output array in      *
! *                           seconds elapsed from the first observation.*
! *                           Dimension: NO. Array TIME_OUT may have     *
! *                           more points than array TIME.               *
! * ELEV_OUT ( REAL*8    ) -- Array of elevation angles of the output    *
! *                           arrat that match TIME_OUT. Dimension: rad. *
! *                           Dimension: NO.                             *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *       NM ( INTEGER*4 ) -- Number of points for a function of system  *
! *                           temperature versus time in the output      *
! *                           array.  NM =< N                            *
! * TIME_MOD ( REAL*8    ) -- Array of time tags of the observations in  *
! *                           seconds elapsed from the first observation.*
! *                           Dimension: NM.                             *
! * TSYS_MOD ( REAL*8    ) -- Array of the modeled system temperatures   *
! *                           in K. Dimension: NM.                       *
! * TSYS_ZEN ( REAL*8    ) -- Array of modeled system temperatures in    *
! *                           zenith direction. Dimension: NM.           *
! * TSYS_T0  ( REAL*8    ) -- Array of modeled system temperatures       *
! *                           normalized for changes with time.          *
! *                           Dimension: NM.                             *
! * ELEV_T0  ( REAL*8    ) -- Array of elevations that corresponds to    *
! *                           TSYS_T0. Dimension: NM.                    *
! *       NT ( INTEGER*4 ) -- Number of points for a function of system  *
! *                           temperature versus time in the output      *
! *                           array.  NT =< N                            *
! *   TIME_T ( REAL*8    ) -- Array of time tags of the observations in  *
! *                           seconds elapsed from the first observation.*
! *                           Dimension: NT.                             *
! *   TSYS_T ( REAL*8    ) -- Normalized system temperature in K which   *
! *                           corresponds to the elevation when T_sys    *
! *                           is minimal. Dimension: NT.                 *
! *       NE ( INTEGER*4 ) -- Number of points for a function of system  *
! *                           temperature versus elevation in the output *
! *                           array.  NE =< N                            *
! *   ELEV_E ( REAL*8    ) -- Array of elevation angles of the sources   *
! *                           in rad. Dimension: NE.                     *
! *   TSYS_E ( REAL*8    ) -- Array of system temperatures in K which    *
! *                           corresponds to the moment of time when the *
! *                           system temperature was minimal.            *
! *                           Dimension: NE.                             *
! * TSYS_OUT ( REAL*8    ) -- Array of the modeled output Tsys that      *
! *                           matches the input arrays.                  *
! *                           Units: K. Dimension: NA.                   *
! * IREF_INI ( INTEGER*4 ) -- Array of cross reference between the       *
! *                           output array TIME_MOD and the input array  *
! *                           TIME: TIME_MODE(IREF_INI(k)) = TIME(k).    *
! *                           Dimension: NE.                             *
! * FLAG_INI ( LOGICAL*1 ) -- Array of flags whether the modeled value   *
! *                           of Tsys is computed for a given measured   *
! *                           Tsys.                                      *
! * TSYS_ZEN_MEAN ( REAL*8 ) -- Averaged zenith system temperature.      *
! * TSYS_RMS ( REAL*8     ) --  Root mean square of variations of zenith *
! *                             system temperature with time (in K).     *
! * TSYS_MIN ( REAL*8     ) --  Minimal value of modeled system          *
! *                             temperature (in K).                      *
! * TSYS_MAX ( REAL*8     ) --  Maximal value of modeled system          *
! *                             temperature (in K).                      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  18-JAN-1999  TSYS_MODEL  v2.6  (c)  L. Petrov 14-APR-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MAX_SCA
      PARAMETER  ( MAX_SCA = 8192 )
      INCLUDE    'lspline.i'
      INTEGER*4  N, NM, NT, NE, NO, IREF_INI(N), IVRB, IUER
      REAL*8     TIME(N), ELEV(N), TSYS(N), TIME_MOD(N), TSYS_MOD(N), &
     &           TSYS_ZEN(N), TSYS_T0(N), ELEV_T0(N), TIME_T(N), TSYS_T(N), &
     &           ELEV_E(N), TSYS_E(N), TSYS_ZEN_MEAN, TSYS_RMS, TSYS_MIN, &
     &           TSYS_MAX, TSYS_0
      LOGICAL*1  FLAG_INI(N)
      REAL*8     AT(MAX_SCA), BE(MAX_SCA), SIGA(MAX_SCA), SIGB(MAX_SCA), &
     &           ELEV_BS(MAX_SCA), TSYS_BS(MAX_SCA), TSYS_AT(MAX_SCA), &
     &           TSYS_EL(MAX_SCA), TIME_EL(MAX_SCA), TSYS__LIM_MIN, &
     &           TSYS__LIM_MAX, TIME_OUT(MAX_SCA), ELEV_OUT(MAX_SCA), &
     &           TSYS_OUT(MAX_SCA), VAL_TIM, VAL_ELEV
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           IV_T_SAVE, MITER, ITER, NM_MIN, N1, N2, IER
      INTEGER*4  ME, MT, MIN_OBS, MINABS_OBS, IND_OUT, IND_INC
      INTEGER*4  IV_E(MAX_SCA), IV_T(MAX_SCA), &
     &           IXREF_EL_TIM(MAX_SCA), IXREF_TIM_EL(MAX_SCA)
      REAL*8     PIMA__SPAN_T
      REAL*8     GR_CNS__T, SR_CNS__T, GR_CNS__E, SR_CNS__E, SPAN_T, &
     &           SPAN_E, EL_MIN, EL_MAX, SH_OUT, TSYS__MAX, EPS__TDIF
      REAL*8     VAL, SIG, VAL_OUT, VAL_INC, DIF_MAX, DIF_MIN, BE_MIN, AT_MIN, &
     &           DS, TSYS_AV, TSYS_DSP_MIN, LAST_TIME
      REAL*8     T1(32768), T2(32768), T3(32768), X1(32768), X2(32768), X3(32768)
!
      REAL*8      PI, PI2, P2I
      PARAMETER ( PI=3.141592653589793D0, PI2=PI*2.D0, P2I = PI/2.D0 )
      PARAMETER  ( TSYS_DSP_MIN = 1.D-3 ) ! Minimal dispersion
!
      TYPE ( LSPLINE__STRU ) ::  LSPLINE_E, LSPLINE_T
      PARAMETER  ( GR_CNS__T  = 9.0            )
      PARAMETER  ( SR_CNS__T  = 1.0D-4         )
      PARAMETER  ( GR_CNS__E  = 9.0            )
      PARAMETER  ( SR_CNS__E  = 5.0            )
      PARAMETER  ( PIMA__SPAN_T = 1200.0D0     )
      PARAMETER  ( MITER      =  8             )
      PARAMETER  ( MT         = 72             )
      PARAMETER  ( ME         = 16             )
      PARAMETER  ( EL_MIN     =  3.0*PI/180.0  )
      PARAMETER  ( EL_MAX     = 90.0*PI/180.0  )
      PARAMETER  ( SH_OUT     = 0.25           )
      PARAMETER  ( TSYS__MAX  = 1.D30          )
      PARAMETER  ( EPS__TDIF  = 5.0/3600.0     )
      PARAMETER  ( MINABS_OBS = 8              )
      PARAMETER  ( TSYS__LIM_MIN =       8.0D0 )
      PARAMETER  ( TSYS__LIM_MAX =    3000.0D0 )
      PARAMETER  ( ITER = 5                    )
      REAL*8     MSIG(MITER)
      DATA       MSIG / 8.0, 6.0, 5.0, 4.0, 3.5, 3.0, 3.0, 3.0 /
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Initialization
!
      NM = 0
      NT = 0
      NE = 0
      IF ( N .LE. 0 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      CALL NOUT_R8 ( N, TSYS_MOD )
      CALL NOUT_R8 ( N, TSYS_ZEN )
      CALL NOUT_R8 ( N, TSYS_T0  )
      CALL NOUT_R8 ( N, ELEV_T0  )
      CALL NOUT_R8 ( N, TIME_T   )
      CALL NOUT_R8 ( N, TSYS_T   )
      CALL NOUT_R8 ( N, ELEV_E   )
      CALL NOUT_R8 ( N, TSYS_E   )
      CALL NOUT_I4 ( N, IREF_INI )
      FLAG_INI = .FALSE.
!
! --- Get non-standard time span for Tsys
!
      CALL GETENVAR ( 'PIMAVAR_TSYS_SPAN_T', STR )
      IF ( ILEN(STR) > 0 ) THEN
           IF ( INDEX ( STR, '.' ) < 1 ) THEN
                STR = TRIM(STR)//'.0'
           END IF
           READ ( UNIT=STR, FMT='(F10.5)', IOSTAT=IER ) SPAN_T
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5131, IUER, 'TSYS_MODEL', 'Error in attempt '// &
     &              'to parse environment variable PIMA_TSYS_SPAN_T '// &
     &              TRIM(STR)//' -- a real number was expected' )
                RETURN
           END IF
         ELSE
           SPAN_T = PIMA__SPAN_T
      END IF
!
! --- Compute MIN_OBS -- minimal number of allowed observations to build
! --- a spline
!
      MIN_OBS = MAX ( INT((TIME(N)-TIME(1))/SPAN_T + 1), MINABS_OBS )
      IF ( N .LE. MIN_OBS ) THEN
!
! -------- Nothing to do -- to few observations
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
      SPAN_E = (EL_MAX-EL_MIN)/(ME-1)
!
! --- Create arrays of ELEV_BS, TIME_EL, TSYS_EL sorted by increasing elevation
! --- angle
!
      CALL COPY_R8 ( N, ELEV, ELEV_BS )
      CALL COPY_R8 ( N, TIME, TIME_EL )
      CALL COPY_R8 ( N, TSYS, TSYS_EL )
      CALL SORT83  ( N, ELEV_BS, TIME_EL, TSYS_EL )
!
! --- Some initialization for further compuation process.
! --- A cross correspondence table table between values of system temperature
! --- as a function of time <---> values of T_sys as a function of elevation
! --- is computed also.
! --- Function A(t) and B(e) is initilialised as identical to unity at all
! --- nodes.
!
      NM = 0
      TSYS_AV = 0.0
      DO 410 J1=1,N
         NM = NM + 1
         IV_E(J1) = 1
         IV_T(J1) = 1
         AT(J1)   = 1.0D0
         BE(J1)   = 1.0D0
         IF ( ELEV(J1) > EL_MIN ) THEN
              SIGA(J1) = DSIN(ELEV(J1))
            ELSE 
              SIGA(J1) = DSIN(EL_MIN)
         END IF
         SIGB(J1) = 1.0D0
         IF ( TSYS(J1) < TSYS__LIM_MIN .OR. TSYS(J1) > TSYS__LIM_MAX ) THEN
              IV_T(J1) = 0
              NM = NM - 1
            ELSE 
              TSYS_AV  = TSYS_AV + TSYS(J1)
              TSYS_T0(NM) = TSYS(J1)
         END IF
!
         DO 420 J2=1,N
            IF ( DABS ( TIME(J1) - TIME_EL(J2) ) .LT. EPS__TDIF ) THEN
                 IXREF_EL_TIM(J2) = J1
                 IXREF_TIM_EL(J1) = J2
                 IF ( IV_T(J1) == 0 ) THEN
                      IV_E(J2) = 0
                 END IF
            END IF
 420     CONTINUE
 410  CONTINUE
!
! --- Compute NM_MIN -- minimal number of observations which are allowed
! --- to remain after rejecting outliers
!
      NM_MIN = MAX ( MIN_OBS, INT(NM*(1.0-SH_OUT)) )
!
! --- TSYS_AV -- median system temperature over entire data set
!
      IF ( NM > 0 ) THEN
!!           TSYS_AV = TSYS_AV/NM
           CALL SORT_R8 ( NM, TSYS_T0 ) 
           TSYS_AV = TSYS_T0(NM/2)
      END IF
      CALL NOUT_R8 ( N, TSYS_T0  )
!
      DO 430 J3=1,ITER
!
! ------ Compute TSYS_BS -- current estimates of B(e) on the basis of
! ------ A(t) and TSYS_AV
!
         DO 440 J4=1,N
            TSYS_BS(J4) = TSYS_EL(J4)/(AT(IXREF_EL_TIM(J4))*TSYS_AV)
 440     CONTINUE
!
! ------ Compute linear spline approximating B(e)
!
         CALL ERR_PASS ( IUER, IER )
         CALL LSPLINE_INIT ( N, ELEV_BS, IV_E, 1, SPAN_E, GR_CNS__E, &
     &                       SR_CNS__E, LSPLINE_E, IER )
         IF ( IER .NE. 0 ) THEN
              WRITE ( 6, * ) ' j3=',j3, ' n=',n, ' span_e = ', span_e
              CALL ERR_LOG ( 5131, IUER, 'TSYS_MODEL', 'Error in attempt '// &
     &            'to initilaize linear spline' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL LSPLINE_CMP ( N, ELEV_BS, TSYS_BS, SIGB, IV_E, 1, LSPLINE_E, IER )
         IF ( IER .NE. 0 ) THEN
              WRITE ( 6, * ) ' j1=',j1
              CALL ERR_LOG ( 5132, IUER, 'TSYS_MODEL', 'Error during '// &
     &            'attempt to compute coefficients of linear spline' )
              RETURN
         END IF
!
! ------ Rejection of outliers and restoration of good points
!
         DO 450 J5=1,N
            DS = 0.0
            DIF_MAX = 0.0
            DIF_MIN = 1.D20
            BE_MIN  = TSYS__MAX
            IND_OUT = 0
            IND_INC = 0
            VAL_OUT = 0.0D0
            DO 460 J6=1,N
!
! ------------ Get the value of linear spline that approximates B(e)
!
               CALL ERR_PASS ( IUER, IER )
               CALL LSPLINE_GET ( ELEV_BS(J6), LSPLINE_E, VAL, SIG, IER )
               IF ( IER .NE. 0 ) THEN
                    WRITE ( 6, * ) ' j3=',j3
                    CALL ERR_LOG ( 5133, IUER, 'TSYS_MODEL', 'Error '// &
     &                  'during attempt to compute value of linear '// &
     &                  'spline' )
                    RETURN
               END IF
!
! ------------ ... and produce a difference between the value of B(e) and
! ------------ a value of linear spline approximating B(e)
!
               IF ( IV_E(J6) .EQ. 1 ) THEN
                    DS = DS + (VAL - TSYS_BS(J6))**2
                    IF ( DABS(VAL - TSYS_BS(J6)) .GT. DIF_MAX ) THEN
                         DIF_MAX = DABS(VAL - TSYS_BS(J6))
                         VAL_OUT = VAL
                         IND_OUT = J6
                    END IF
                    IF ( VAL .LT. BE_MIN ) BE_MIN = VAL
                 ELSE
                    IF ( DABS(VAL - TSYS_BS(J6)) .LT. DIF_MIN ) THEN
                         DIF_MIN = DABS(VAL - TSYS_BS(J6))
                         VAL_INC = VAL
                         IND_INC = J6
                    END IF
               END IF
               BE(J6) = VAL
 460        CONTINUE
!
! --------- DS -- normalized variance of a difference B(e) and approximating
! --------- function.
!
            DS = DSQRT ( DS/(NM-1) )
            IF ( IND_OUT > 0                .AND. &
     &           DIF_MAX .GT. DS*MSIG(J3)   .AND. &
     &           DIF_MAX .GT. TSYS_DSP_MIN  .AND. &
     &           NM .GT. NM_MIN                   ) THEN
!
! -------------- Since this difference exceeds the limit, this point is
! -------------- marked as an outlier, removed from the solution,
! -------------- and solution is updated
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL LSPLINE_UPD ( -1, ELEV_BS(IND_OUT), VAL_OUT, SIGB(IND_OUT), &
     &                              LSPLINE_E, IER )
                 IF ( IER .NE. 0 ) THEN
                      WRITE ( 6, * ) ' j3=',j3
                      CALL ERR_LOG ( 5134, IUER, 'TSYS_MODEL', 'Error '// &
     &                    'during attempt to compute value of linear '// &
     &                    'spline' )
                      RETURN
                 END IF
                 NM = NM - 1
                 IV_E(IND_OUT) = 0
                 IV_T(IXREF_EL_TIM(IND_OUT)) = 0
               ELSE IF ( IND_INC > 0 .AND. DIF_MIN .LT. DS*MSIG(J3) ) THEN
!
! -------------- If this difference is less than the limit then this point is
! -------------- marked as an a point to be restored. This point is returned
! -------------- back to the solution and solution is updated
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL LSPLINE_UPD ( 1, ELEV_BS(IND_INC), VAL_INC, SIGB(IND_INC), &
     &                              LSPLINE_E, IER )
                 IF ( IER .NE. 0 ) THEN
                      WRITE ( 6, * ) ' j3=',j3
                      CALL ERR_LOG ( 5135, IUER, 'TSYS_MODEL', 'Error '// &
     &                    'during attempt to compute value of linear '// &
     &                    'spline' )
                      RETURN
                 END IF
                 NM = NM + 1
                 IV_E(IND_INC) = 1
                 IV_T(IXREF_EL_TIM(IND_INC)) = 0
               ELSE
                 GOTO 850
            END IF
 450     CONTINUE
 850     CONTINUE
!!   call diagi_2 ( n, elev_bs, be, n, elev_bs, tsys_bs, ier ) ! %%%%
!
! ------ Compute function A(t) on the basis of B(e) and TSYS_AV
!
         DO 470 J7=1,N
            TSYS_AT(J7) = TSYS(J7)/(BE(IXREF_TIM_EL(J7))*TSYS_AV)
 470     CONTINUE
!
! ------ Now compute linear spline for time dependence
!
         CALL ERR_PASS ( IUER, IER )
         CALL LSPLINE_INIT ( N, TIME, IV_T, 1, SPAN_T, GR_CNS__T, SR_CNS__T, &
     &                       LSPLINE_T, IER )
         IF ( IER .NE. 0 ) THEN
              WRITE ( 6, * ) ' j1=',j1
              CALL ERR_LOG ( 5136, IUER, 'TSYS_MODEL', 'Error in attempt '// &
     &            'to initilaize linear spline' )
              RETURN
         END IF
!
! ------ A trick: set the last segment of time spline to cover the last
! ------ time epoch of the output array, regardess whether this epoch
! ------ is good or bad.
!
         LAST_TIME = TIME(N)
         IV_T_SAVE = IV_T(N)
         TIME(N) = MAX ( LAST_TIME, TIME_OUT(NO) )
         IV_T(N) = 1
!
         CALL ERR_PASS ( IUER, IER )
         CALL LSPLINE_CMP ( N, TIME, TSYS_AT, SIGA, IV_T, 1, LSPLINE_T, IER )
         IF ( IER .NE. 0 ) THEN
              WRITE ( 6, * ) ' j1=',j1
              CALL ERR_LOG ( 5137, IUER, 'TSYS_MODEL', 'Error during '// &
     &            'an attempt to compute coefficients of linear spline' )
              RETURN
         END IF
         TIME(N) = LAST_TIME
         IV_T(N) = IV_T_SAVE
!
! ------ Outliers rejection
!
         DO 480 J8=1,N
            DS = 0.0
            DIF_MAX = 0.0
            DIF_MIN = 1.D20
            AT_MIN  = TSYS__MAX
            IND_OUT = 0
            IND_INC = 0
            DO 490 J9=1,N
!
! ------------ Get a value of A(t) by applying linear spline approximation
!
               CALL ERR_PASS ( IUER, IER )
               CALL LSPLINE_GET ( TIME(J9), LSPLINE_T, VAL, SIG, IER )
               IF ( IER .NE. 0 ) THEN
                    WRITE ( 6, * ) ' j3=',j3
                    CALL ERR_LOG ( 5138, IUER, 'TSYS_MODEL', 'Error '// &
     &                  'during attempt to compute value of linear '// &
     &                  'spline' )
                    RETURN
               END IF
!
! ------------ ... produced a difference between the value of Tsys(t) and
! ------------ a value of linear spline approximating Tsys(t)
!
               IF ( IV_T(J9) .EQ. 1 ) THEN
                    DS = DS + (VAL - TSYS_AT(J9))**2
                    IF ( DABS(VAL - TSYS_AT(J9)) .GT. DIF_MAX ) THEN
                         DIF_MAX = DABS(VAL - TSYS_AT(J9))
                         VAL_OUT = VAL
                         IND_OUT = J9
                    END IF
                    IF ( VAL .LT. AT_MIN ) AT_MIN = VAL
                    AT(J9) = VAL
                  ELSE
                    IF ( DABS(VAL - TSYS_AT(J9)) .LT. DIF_MIN ) THEN
                         DIF_MIN = DABS(VAL - TSYS_AT(J9))
                         VAL_INC = VAL
                         IND_INC = J9
                    END IF
               END IF
 490        CONTINUE
!
            DS = DSQRT ( DS/(NM-1) )
            IF ( IND_OUT > 0                .AND. &
     &           DIF_MAX .GT. DS*MSIG(J3)   .AND. &
     &           DIF_MAX .GT. TSYS_DSP_MIN  .AND. &
     &           NM .GT. NM_MIN                   ) THEN
!
! -------------- If this difference exceeds the limit, then this point is
! -------------- marked as an outlier, removed from the solution,
! -------------- and the solution is updated
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL LSPLINE_UPD( -1, TIME(IND_OUT), VAL_OUT, SIGA(IND_OUT), &
     &                              LSPLINE_T, IER )
                 IF ( IER .NE. 0 ) THEN
                      WRITE ( 6, * ) ' J3=', J3, ' NM = ', NM, ' N= ', N
                      CALL ERR_LOG ( 5139, IUER, 'TSYS_MODEL', 'Error '// &
     &                    'during attempt to update the coefficients of '// &
     &                    'B-spline by subtracting the point ' )
                      RETURN
                 END IF
                 NM = NM - 1
                 IV_T(IND_OUT) = 0
                 IV_E(IXREF_TIM_EL(IND_OUT)) = 0
               ELSE IF ( IND_INC > 0 .AND. DIF_MIN .LT. DS*MSIG(J3) ) THEN
!
! -------------- We restore this point
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL LSPLINE_UPD ( 1, TIME(IND_INC), VAL_INC, SIGA(IND_INC), &
     &                              LSPLINE_T, IER )
                 IF ( IER .NE. 0 ) THEN
                      WRITE ( 6, * ) ' j3=',j3
                      CALL ERR_LOG ( 5140, IUER, 'TSYS_MODEL', 'Error '// &
     &                    'during attempt to update the coefficients of '// &
     &                    'B-spline by adding the point ' )
                      RETURN
                 END IF
                 NM = NM + 1
                 IV_T(IND_INC) = 1
                 IV_E(IXREF_TIM_EL(IND_INC)) = 1
               ELSE
                 GOTO 880
            END IF
            IF ( IVRB == 22 ) THEN
                 N1 = 0
                 N2 = 0
                 DO 510 J1=1,N
                    IF ( IV_T(J1) == 1 ) THEN
                         N1 = N1 + 1
                         T1(N1) = TIME(J1)
                         X1(N1) = TSYS_AT(J1)
                       ELSE
                         N2 = N2 + 1
                         T2(N2) = TIME(J1)
                         X2(N2) = TSYS_AT(J1)
                    END IF
 510             CONTINUE 
                 write ( 6, * ) ' j8= ', int2(j8), ' nm = ', nm , ' ind_out= ', ind_out, ' ind_inc= ', ind_inc ! %%%%%%%
                 call diagi_3 ( n, time, at, n1, t1, x1, n2, t2, x2, ier ) ! %%%%
            END IF
 480     CONTINUE
 880     CONTINUE
 430  CONTINUE
!
! --- Compute values of linear spline of a function A(t)
!
      CALL ERR_PASS ( IUER, IER )
      CALL LSPLINE_GETSEG ( N, LSPLINE_T, NT, TIME_T, TSYS_T, SIGA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5141, IUER, 'TSYS_MODEL', 'Error during '// &
     &         'attempt to compute value of linear spline' )
           RETURN
      END IF
!
! --- Normalization of a function Tsys(t) to make it corresponding
! --- the elevation where T_sys is minimal
!
      DO 4100 J10=1,NT
         TSYS_T(J10) = TSYS_T(J10)*TSYS_AV*BE_MIN
 4100 CONTINUE
!
! --- Compute values of linear spline of a function B(e)
!
      CALL ERR_PASS ( IUER, IER )
      CALL LSPLINE_GETSEG ( N, LSPLINE_E, NE, ELEV_E, TSYS_E, SIGB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5142, IUER, 'TSYS_MODEL', 'Error during '// &
     &         'attempt to compute value of linear spline' )
           RETURN
      END IF
!
! --- Normalization of a function Tsts(e) to make it corresponding
! --- the time epoch where T_sys is minimal
!
      DO 4110 J11=1,NE
         ELEV_E(J11) = ELEV_E(J11) ! *180.0/PI
         TSYS_E(J11) = TSYS_E(J11)*TSYS_AV*AT_MIN
 4110 CONTINUE
!
! --- Initialization...
!
      TSYS_MAX      = 0.0
      TSYS_MIN      = TSYS__MAX
      TSYS_RMS      = 0.0
      TSYS_ZEN_MEAN = 0.0
      CALL NOUT_I4 ( N, IREF_INI )
      NM = 0
!
! --- Finally compute arrays of modeled system temperature and zenith system
! --- temperature for not rejected points. Compute also IREF_INI -- a table
! --- of cross reference between initial and model points.
!
      DO 4120 J12=1,N
         IF ( IV_T(J12) .EQ. 1 ) THEN
              NM = NM + 1
              IREF_INI(NM) = J12
              TIME_MOD(NM) = TIME(J12)
              TSYS_MOD(NM) = TSYS_AV*AT(J12)*BE(IXREF_TIM_EL(J12))
              TSYS_ZEN(NM) = TSYS(J12)/(BE(IXREF_TIM_EL(J12))/BE_MIN)
              TSYS_T0(NM)  = TSYS(J12)/(AT(J12)/AT_MIN)
              ELEV_T0(NM)  = ELEV(J12)
!
              TSYS_RMS      = TSYS_RMS      + (TSYS(J12) - TSYS_MOD(NM))**2
              TSYS_ZEN_MEAN = TSYS_ZEN_MEAN + TSYS_ZEN(NM)
              IF ( TSYS_MOD(NM) .LT. TSYS_MIN ) TSYS_MIN = TSYS_MOD(NM)
              IF ( TSYS_MOD(NM) .GT. TSYS_MAX ) TSYS_MAX = TSYS_MOD(NM)
              IF ( IV_E(IXREF_TIM_EL(J12)) .EQ. 1 ) THEN
                   FLAG_INI(J12) = .TRUE.
              END IF
         END IF
 4120 CONTINUE
!
      DO 4130 J13=1,NO
!
! ------ Get a value of A(t) by applying linear spline approximation
!
         CALL ERR_PASS ( IUER, IER )
         CALL LSPLINE_GET ( TIME_OUT(J13), LSPLINE_T, VAL_TIM, SIG, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5143, IUER, 'TSYS_MODEL', 'Error '// &
     &             'during attempt to compute value of linear '// &
     &             'spline over time' )
              RETURN
         END IF
!
! ------ Get a value of B(e) by applying linear spline approximation
!
         IF ( ELEV_OUT(J13) > EL_MIN ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL LSPLINE_GET ( ELEV_OUT(J13), LSPLINE_E, VAL_ELEV, SIG, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5138, IUER, 'TSYS_MODEL', 'Error '// &
     &                 'during attempt to compute value of linear '// &
     &                 'spline over elevation' )
                   RETURN
              END IF
            ELSE
              VAL_ELEV = 0.0D0
         END IF
         TSYS_OUT(J13) = TSYS_AV*VAL_TIM*VAL_ELEV
 4130 CONTINUE 
      TSYS_RMS = DSQRT ( TSYS_RMS/NM )
      TSYS_ZEN_MEAN = TSYS_ZEN_MEAN/NM
      TSYS_0 = TSYS_AV
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  TSYS_MODEL  #!#
