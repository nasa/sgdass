      SUBROUTINE SOTID_TIM ( MJD, TAI, UT1_M_TAI, TIDCNF, TIMTID, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SOTID_TIM  calculates intermediary time-dependent sums    *
! *   over harmonics of the tidal potential of the second and the third  *
! *   degree. These quantities are used in calculation displacements     *
! *   due to Earth solid tides, pole tides and ocean loading as well as  *
! *   their first time derivatives. They are put in the object TIDCNF.   *
! *   Routine SOTID_TIM should be called each time for the new moment    *
! *   of time before SOTID_DSP which computes tidal displacement for     *
! *   the specific moment of time specified in the call to SOTID_TIM     *
! *   for the specific station. Truncated harmonic expansion             *
! *   HW95 is used for computation.                                      * 
! *                                                                      *
! * ________________________ Input parameters:  ________________________ *
! *                                                                      *
! *       MJD ( INTEGER*4 ) -- Integer fraction of the Modified Julian   *
! *                            Day -- MJD at the midnight of the         *
! *                            observations. It has the meaning of the   *
! *                            INTEGER number of days elapsed from       *
! *                            0 hours of 01 January 2000).              *
! *       TAI ( REAL*8    ) -- Time at TAI scale of the moment under     *
! *                            consideration (in sec).                   *
! * UT1_M_TAI ( REAL*8    ) -- Value of the function UT1-TAI at the      *
! *                            moment of observation. This quantity is   *
! *                            obtained from the analysis of the         *
! *                            astronomical observations.                *
! *    TIDCNF ( RECORD    ) -- Object which holds configuration          *
! *                            parameters of SOTID.                      *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *    TIMTID ( RECORD    ) -- Object with time-dependent intermediary   *
! *                            quantities used for computation of        *
! *                            displacements caused by the Earth's solid *
! *                            tides.                                    *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *    References:                                                       *
! *                                                                      *
! *    1) T. Hartmann and H-G. Wenzel "The HW95 tidal potential          *
! *       catalogue", Geophysical Research Letters", Vol. 22(24),        *
! *       p. 3553-3556, 1995.                                            *
! *    2) L. Petrov "Memo about algorithm for calculation of site        *
! *       displacements due to the Earths tides of the second order",    *
! *       1998.                                                          *
! *    3) L. Petrov "Study of Harmonic site position variations          *
! *                  determined by VLBI", J. Geophys. Res., vol. 108,    *
! *                  No. B4, 2190, 10.1029/2002JB001801, 2003.           *
! *                                                                      *
! *  ###  22-AUG-1998   SOTID_TIM   v4.0 (c) L. Petrov 21-JUN-2004  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT    NONE
!
! --- This include block contains variables of INTEGER*4  NW_HW -- the number
! --- of waves of the expansion, N1 (work variables); 4 REAL*8  arrays:
! --- PHAS (rad) -- phase of the tidal wave on 0h TDB J2000.0, FREQ (rad/sec)
! --- frequency of the tidal wave on 0h TDB J2000.0, ACCL (rad/sec^2) --
! --- doubled acceleration of the tidal wave, AMPL (m^2/sec^2) -- amplitude
! --- of the potential for the tidal wave. It also contains INTEGER*4  array
! --- M which contains the degree of the tidal wave
!
      INCLUDE    'sotid_type.i'
      INCLUDE    'sotid_data.i'
!
      INTEGER*4   MJD, IUER
      REAL*8      TAI, UT1_M_TAI
      TYPE ( TIDCNF__STRU ) ::  TIDCNF
      TYPE ( TIMTID__STRU ) ::  TIMTID
      REAL*8     TDB, ARG, VEL, AC, AS
      INTEGER*4  J1, J2, J3, J4, J5, IER
      CHARACTER  STR*32
      REAL*8     DELTA_TDB, LOVE_VEC(SOTID__NLOVE), UT1_M_TDB
      REAL*8     LVAS(SOTID__NLOVE), LVAC(SOTID__NLOVE)
      INTEGER*4  I_LEN
!
! --- Calculation of TDB argument
!
      CALL SOTID_TAI_TDB ( MJD, TAI, TDB )
      UT1_M_TDB = UT1_M_TAI - (TDB-TAI)
!
! --- DELTA_TDB -- elapsed time (on the scale TDB) from fundamental epoch
! --- J2000.0
!
      DELTA_TDB = (MJD - SOTID__MJD_J2000)*86400.D0 - 43200.D0 + TDB
!
! --- Initializatoin
!
      CALL NOUT_R8 ( 3*5*3*2, TIMTID%LARS2 )
      CALL NOUT_R8 ( 3*5*3*2, TIMTID%LARC2 )
      CALL NOUT_R8 ( 3*2*3*2, TIMTID%LAIS2 )
      CALL NOUT_R8 ( 3*2*3*2, TIMTID%LAIC2 )
      CALL NOUT_R8 ( 3*4*2,   TIMTID%LARS3 )
      CALL NOUT_R8 ( 3*4*2,   TIMTID%LARC3 )
!
! --- Summation over all NW waves of the second order
!
      DO 410 J1=1,NW
!
! ------ Get Love number for the frequency of the J1-th tidal wave
!
         IER = IUER
         CALL SOTID_GET_LOVE ( TIDCNF, 2, M2_TID(J1), FREQ2_TID(J1), &
     &                         LOVE_VEC, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              WRITE ( UNIT=STR(1:12), FMT='(I6)' ) J1
              CALL CHASHL ( STR )
              CALL ERR_LOG ( 5401, IUER, 'SOTID_TIM', 'Error in '// &
     &            'computing Love numbers for the '//STR(1:I_LEN(STR))// &
     &            '-th wave' )
              RETURN
         END IF
!
! ------ Tidal argument without longitudial term
!
         ARG = (0.5D0*ACCL2_TID(J1)*DELTA_TDB + FREQ2_TID(J1))*DELTA_TDB + &
     &          PHAS2_TID(J1) + M2_TID(J1)*UT1_M_TDB*SOTID__PI2/86400.0D0
!
! ------ Cycle over derivatives, starting from 0-th
!
         DO 420 J2=SOTID__DER0,SOTID__DER1
            IF ( J2 == SOTID__DER0 ) THEN
                 AC = AMPL2_TID(J1)*DCOS(ARG)
                 AS = AMPL2_TID(J1)*DSIN(ARG)
               ELSE IF ( J2 == SOTID__DER1 ) THEN
                 VEL = ACCL2_TID(J1)*DELTA_TDB + FREQ2_TID(J1)
                 AC = -VEL*AMPL2_TID(J1)*DSIN(ARG)
                 AS =  VEL*AMPL2_TID(J1)*DCOS(ARG)
            END IF
!
! --------- Create arrays Love_vec(i)*sin(arg) and Love_vec(i)*cos(arg)
!
            DO 430 J3=1,SOTID__NLOVE
               LVAS(J3) = LOVE_VEC(J3)*AS
               LVAC(J3) = LOVE_VEC(J3)*AC
 430        CONTINUE
!
            IF ( TIDCNF%GEN_LOVE .EQ. SOTID__PL_ONLY ) THEN
!
! ------------ Zero out Love numbers for zero-th and 1-st degree in this mode
!
               LVAS(4) = 0.0D0
               LVAC(4) = 0.0D0
               LVAS(7) = 0.0D0
               LVAC(7) = 0.0D0
               LVAS(9) = 0.0D0
               LVAC(9) = 0.0D0
            END IF
!
            IF ( TIDCNF%GEN_LOVE .EQ. SOTID__PRN_ONLY ) THEN
!
! ----------- Only principal Love numbers are considered
!
! ----------- Radial
!
              TIMTID%LARS2(1,1,M2_TID(J1),J2) = TIMTID%LARS2(1,1,M2_TID(J1),J2) + LVAS(1) ! h0(r) sin
              TIMTID%LARC2(1,1,M2_TID(J1),J2) = TIMTID%LARC2(1,1,M2_TID(J1),J2) + LVAC(1) ! h0(r) cos
              TIMTID%LAIS2(1,1,M2_TID(J1),J2) = TIMTID%LAIS2(1,1,M2_TID(J1),J2) + LVAS(2) ! h(i)  sin
              TIMTID%LAIC2(1,1,M2_TID(J1),J2) = TIMTID%LAIC2(1,1,M2_TID(J1),J2) + LVAC(2) ! h(i)  cos
!
! ----------- East
!
              TIMTID%LARS2(1,2,M2_TID(J1),J2) = TIMTID%LARS2(1,2,M2_TID(J1),J2) + LVAS(5) ! l0(r) sin
              TIMTID%LARC2(1,2,M2_TID(J1),J2) = TIMTID%LARC2(1,2,M2_TID(J1),J2) + LVAC(5) ! l0(r) cos
              TIMTID%LAIS2(1,2,M2_TID(J1),J2) = TIMTID%LAIS2(1,2,M2_TID(J1),J2) + LVAS(6) ! l(i)  sin
              TIMTID%LAIC2(1,2,M2_TID(J1),J2) = TIMTID%LAIC2(1,2,M2_TID(J1),J2) + LVAC(6) ! l(i)  cos
!
! ----------- North
!
              TIMTID%LARS2(1,3,M2_TID(J1),J2) = TIMTID%LARS2(1,2,M2_TID(J1),J2) ! l0(r) sin
              TIMTID%LARC2(1,3,M2_TID(J1),J2) = TIMTID%LARC2(1,2,M2_TID(J1),J2) ! l0(r) cos
              TIMTID%LAIS2(1,3,M2_TID(J1),J2) = TIMTID%LAIS2(1,2,M2_TID(J1),J2) ! l(i)  sin
              TIMTID%LAIC2(1,3,M2_TID(J1),J2) = TIMTID%LAIC2(1,2,M2_TID(J1),J2) ! l(i)  cos
             ELSE IF ( TIDCNF%GEN_LOVE .EQ. SOTID__PL_ONLY  .OR. &
     &                 TIDCNF%GEN_LOVE .EQ. SOTID__GEN_ALL       )THEN
!
! ----------- All generalized Love numbers are considered
!
! ----------- Radial
!
              TIMTID%LARS2(1,1,M2_TID(J1),J2) = TIMTID%LARS2(1,1,M2_TID(J1),J2) + LVAS(1) ! h0(r) sin
              TIMTID%LARS2(2,1,M2_TID(J1),J2) = TIMTID%LARS2(2,1,M2_TID(J1),J2) + LVAS(3) ! h2    sin
              TIMTID%LARS2(4,1,M2_TID(J1),J2) = TIMTID%LARS2(4,1,M2_TID(J1),J2) + LVAS(4) ! h'    sin
!
              TIMTID%LARC2(1,1,M2_TID(J1),J2) = TIMTID%LARC2(1,1,M2_TID(J1),J2) + LVAC(1) ! h0(r) cos
              TIMTID%LARC2(2,1,M2_TID(J1),J2) = TIMTID%LARC2(2,1,M2_TID(J1),J2) + LVAC(3) ! h2    cos
              TIMTID%LARC2(4,1,M2_TID(J1),J2) = TIMTID%LARC2(4,1,M2_TID(J1),J2) + LVAC(4) ! h'    cos
!
              TIMTID%LAIS2(1,1,M2_TID(J1),J2) = TIMTID%LAIS2(1,1,M2_TID(J1),J2) + LVAS(2) ! h(i)  sin
              TIMTID%LAIC2(1,1,M2_TID(J1),J2) = TIMTID%LAIC2(1,1,M2_TID(J1),J2) + LVAC(2) ! h(i)  cos
!
! ----------- East
!
              TIMTID%LARS2(1,2,M2_TID(J1),J2) = TIMTID%LARS2(1,2,M2_TID(J1),J2) + LVAS(5) ! l0(r) sin
              TIMTID%LARS2(2,2,M2_TID(J1),J2) = TIMTID%LARS2(2,2,M2_TID(J1),J2) + LVAS(8) ! l2    sin
              TIMTID%LARS2(3,2,M2_TID(J1),J2) = TIMTID%LARS2(3,2,M2_TID(J1),J2) + LVAS(7) ! l1    sin
              TIMTID%LARS2(4,2,M2_TID(J1),J2) = TIMTID%LARS2(4,2,M2_TID(J1),J2) + LVAS(9) ! l'    sin
!
              TIMTID%LARC2(1,2,M2_TID(J1),J2) = TIMTID%LARC2(1,2,M2_TID(J1),J2) + LVAC(5) ! l0(r) cos
              TIMTID%LARC2(2,2,M2_TID(J1),J2) = TIMTID%LARC2(2,2,M2_TID(J1),J2) + LVAC(8) ! l2    cos
              TIMTID%LARC2(3,2,M2_TID(J1),J2) = TIMTID%LARC2(3,2,M2_TID(J1),J2) + LVAC(7) ! l1    cos
              TIMTID%LARC2(4,2,M2_TID(J1),J2) = TIMTID%LARC2(4,2,M2_TID(J1),J2) + LVAC(9) ! l'    cos
!
              TIMTID%LAIS2(1,2,M2_TID(J1),J2) = TIMTID%LAIS2(1,2,M2_TID(J1),J2) + LVAS(6) ! l(i)  sin
              TIMTID%LAIC2(1,2,M2_TID(J1),J2) = TIMTID%LAIC2(1,2,M2_TID(J1),J2) + LVAC(6) ! l(i)  cos
!
! ----------- North
!
              TIMTID%LARS2(1,3,M2_TID(J1),J2) = TIMTID%LARS2(1,2,M2_TID(J1),J2) ! l0(r) sin
              TIMTID%LARS2(2,3,M2_TID(J1),J2) = TIMTID%LARS2(2,2,M2_TID(J1),J2) ! l2    sin
              TIMTID%LARS2(3,3,M2_TID(J1),J2) = TIMTID%LARS2(3,2,M2_TID(J1),J2) ! l1    sin
              TIMTID%LARS2(4,3,M2_TID(J1),J2) = TIMTID%LARS2(4,2,M2_TID(J1),J2) ! l'    sin
!
              TIMTID%LARC2(1,3,M2_TID(J1),J2) = TIMTID%LARC2(1,2,M2_TID(J1),J2) ! l0(r) cos
              TIMTID%LARC2(2,3,M2_TID(J1),J2) = TIMTID%LARC2(2,2,M2_TID(J1),J2) ! l2    cos
              TIMTID%LARC2(3,3,M2_TID(J1),J2) = TIMTID%LARC2(3,2,M2_TID(J1),J2) ! l1    cos
              TIMTID%LARC2(4,3,M2_TID(J1),J2) = TIMTID%LARC2(4,2,M2_TID(J1),J2) ! l'    cos
!
              TIMTID%LAIS2(1,3,M2_TID(J1),J2) = TIMTID%LAIS2(1,2,M2_TID(J1),J2) ! l(i)  sin
              TIMTID%LAIC2(1,3,M2_TID(J1),J2) = TIMTID%LAIC2(1,2,M2_TID(J1),J2) ! l(i)  cos
             END IF
 420     CONTINUE 
  410 CONTINUE
!
      IF ( TIDCNF%MODEL_3D .EQ. SOTID__3D_MDG97 ) THEN
!
! -------- Summation over all NW3 waves of the third order
!
           DO 440 J4=1,NW3
!
! ----------- Tidal argument without longitudial term
!
              ARG = (0.5D0*ACCL3_TID(J4)*DELTA_TDB + FREQ3_TID(J4))*DELTA_TDB + &
     &               PHAS3_TID(J4) + M3_TID(J4)*UT1_M_TDB*SOTID__PI2/86400.0D0
              DO 450 J5=SOTID__DER0,SOTID__DER1
                 IF ( J5 == SOTID__DER0 ) THEN
                      AC = AMPL3_TID(J4)*DCOS(ARG)
                      AS = AMPL3_TID(J4)*DSIN(ARG)
                    ELSE IF ( J5 == SOTID__DER1 ) THEN
                     VEL = ACCL3_TID(J4)*DELTA_TDB + FREQ3_TID(J4)
                     AC = -VEL*AMPL3_TID(J4)*DSIN(ARG)
                     AS =  VEL*AMPL3_TID(J4)*DCOS(ARG)
                 END IF
!
                 TIMTID%LARC3(1,M3_TID(J4),J5) = TIMTID%LARC3(1,M3_TID(J4),J5) + H3_LOVE*AC  ! h3
                 TIMTID%LARC3(2,M3_TID(J4),J5) = TIMTID%LARC3(2,M3_TID(J4),J5) + L3_LOVE*AC  ! l3
                 TIMTID%LARC3(3,M3_TID(J4),J5) = TIMTID%LARC3(2,M3_TID(J4),J5)               ! l3
!
                 TIMTID%LARS3(1,M3_TID(J4),J5) = TIMTID%LARS3(1,M3_TID(J4),J5) + H3_LOVE*AS  ! h3
                 TIMTID%LARS3(2,M3_TID(J4),J5) = TIMTID%LARS3(2,M3_TID(J4),J5) + L3_LOVE*AS  ! l3
                 TIMTID%LARS3(3,M3_TID(J4),J5) = TIMTID%LARS3(2,M3_TID(J4),J5)               ! l3
 450          CONTINUE
 440       CONTINUE
      END IF
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END SUBROUTINE SOTID_TIM
