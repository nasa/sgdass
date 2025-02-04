      SUBROUTINE SOTID_DSP ( TIDCNF, TIMTID, STATID, D_REN, DT_REN )
! ************************************************************************
! *                                                                      *
! *     Routine  SOTID_DSP  calculates site displacements due to the     *
! *   Earth's tides of the second degree and third degree using          *
! *   harmonic expansion of tidal potential according to configuration   *
! *   parameters of SOTID. It is assumed that session-dependent and      *
! *   time-dependent intermediary quantities were calculated before:     *
! *                                                                      *
! *   CALL SOTID_SET ( ... ) -- set SOTID configuration. Called once     *
! *                             in a program.                            *
! *   CALL SOTID_PRE ( ... ) -- compute station specific,                *
! *                             time-independent intermediary variables. *
! *                             Called once in a program.                *
! *   CALL SOTID_TIM ( ... ) -- compute station-independent,             *
! *                             time-specific intermediary variables on  *
! *                             the moment of time under consideration.  *
! *                                                                      *
! *   Thus, SOTID_DSP computes site displacement for the station         *
! *   specified in the call SOTID_PRE for the moment of time specified   *
! *   in the call SOTID_TIM according with configuration specified in    *
! *   the call SOTID_SET.                                                *
! *                                                                      *
! * ________________________ Input parameters:  ________________________ *
! *                                                                      *
! *   TIDCNF ( RECORD    ) -- Object which holds configuration           *
! *                           parameters of SOTID.                       *
! *   TIMTID ( RECORD    ) -- Object with time-dependent intermediary    *
! *                           quantities used fro computation of         *
! *                           displacements caused by the Earth's solid  *
! *                           tides.                                     *
! *   STATID ( RECORD    ) -- Object with information about station.     *
! *                           SOTID_PRE fills some fields of this object.*
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *    D_REN ( REAL*8    ) -- Vector of tidal displacement in REN        *
! *                           topocentric system: radial(up), east,      *
! *                           north. Units: meters.                      *
! *   DT_REN ( REAL*8    ) -- Vector of tidal velocity in REN            *
! *                           topocentric system: radial(up), east,      *
! *                           north. Units: meters/sec.                  *
! *                                                                      *
! *    References:                                                       *
! *    1) L. Petrov "Study of Harmonic site position variations          *
! *                  determined by VLBI", J. Geophys. Res., vol. 108,    *
! *                  No. B4, 2190, 10.1029/2002JB001801, 2003.           *
! *    2) T. Hartmann and H-G. Wenzel "The HW95 tidal potential          *
! *       catalogue", Geophysical Research Letters", Vol. 22(24),        *
! *       p. 3553-3556, 1995.                                            *
! *    3) P. Melchior, "The Tides of the Planet Earth", Oxford, 1983.    *
! *    4) Dehant V., Defraigne P., and Wahr J.M., 1999, "Tides for an    *
! *       Earth in a non-hydrostatic equilibrium.", J. Geophys. Res.,    *
! *       vol. 104, B1, pp. 1035-1058.                                   *
! *    5) P.M. Mathews, B.A. Buffet, I.I. Shapiro "Love number for       *
! *       a rotating spheroidal Earth: New definitions and numerical     *
! *       values", GRL, Vol. 22(5), pp.5579-582, 1995.                   *
! *    6) Wahr, J.M., "Body tides on an elliptical, elastic, rotating    *
! *       and oceanless Earth", Geophysical Journal of Royal             *
! *       Astronomical Society, Vol. 64(3), pp. 677-703, 1981.           *
! *    7) L. Petrov "Secondary analysis of geodetic VLBI observations",  *
! *       Messages of Institute of Applied Astronomy, N 74, 1995,        *
! *       Sanct-Petersburg, p. 44 (in Russian).                          *
! *                                                                      *
! *  ###  22-AUG-1998    SOTID_DSP   v3.1 (c) L. Petrov 12-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'sotid_type.i'
      INCLUDE    'sotid_data.i'
      REAL*8     D_REN(3), DT_REN(3)
      TYPE ( TIDCNF__STRU ) ::  TIDCNF
      TYPE ( TIMTID__STRU ) ::  TIMTID
      TYPE ( STATID__STRU ) ::  STATID
      REAL*8     VEC(3,0:1)
      INTEGER*4  J1, J2, J3, J4, J5
!
! --- Initialization
!
      CALL NOUT_R8 ( 3, D_REN )
      CALL NOUT_R8 ( 3, DT_REN )
      CALL NOUT_R8 ( 3*2, VEC )
!
! --- Computation of displacements due to the tides of the second order
!
      DO 410 J1=SOTID__DER0,SOTID__DER1
         DO 420 J2=0,2 ! Over degree
            DO 430 J3=1,3 ! Over radial, east and north component
               IF ( TIDCNF%GEN_LOVE .EQ. SOTID__PRN_ONLY ) THEN
!
! ----------------- Only principal Love numbers are taken into account
!
                    IF ( J3 .EQ. 1  .OR.  J3 .EQ. 3 ) THEN
                         VEC(J3,J1) = VEC(J3,J1) &
     &                          + STATID%XRC2(1,J3,J2)*TIMTID%LARC2(1,J3,J2,J1) &
     &                          - STATID%XRS2(1,J3,J2)*TIMTID%LARS2(1,J3,J2,J1) &
     &                          - STATID%XIC2(1,J3,J2)*TIMTID%LAIS2(1,J3,J2,J1) &
     &                          - STATID%XIS2(1,J3,J2)*TIMTID%LAIC2(1,J3,J2,J1)
                      ELSE IF ( J3 .EQ. 2 ) THEN
                         VEC(J3,J1) = VEC(J3,J1) &
     &                          + STATID%XRC2(1,J3,J2)*TIMTID%LARS2(1,J3,J2,J1) &
     &                          + STATID%XRS2(1,J3,J2)*TIMTID%LARC2(1,J3,J2,J1) &
     &                          + STATID%XIC2(1,J3,J2)*TIMTID%LAIC2(1,J3,J2,J1) &
     &                          - STATID%XIS2(1,J3,J2)*TIMTID%LAIS2(1,J3,J2,J1)
                    END IF
                  ELSE IF ( TIDCNF%GEN_LOVE .EQ. SOTID__PL_ONLY ) THEN
!
! ----------------- Only principal and latitudinal generalized Love numbers are
! ----------------- taken into account
!
                    IF ( J3 .EQ. 1  .OR.  J3 .EQ. 3 ) THEN
                         VEC(J3,J1) = VEC(J3,J1) &
     &                   + (   STATID%XRC2(1,J3,J2)*TIMTID%LARC2(1,J3,J2,J1) &
     &                       + STATID%XRC2(2,J3,J2)*TIMTID%LARC2(2,J3,J2,J1) &
     &                     ) &
     &                   - (   STATID%XRS2(1,J3,J2)*TIMTID%LARS2(1,J3,J2,J1) &
     &                       + STATID%XRS2(2,J3,J2)*TIMTID%LARS2(2,J3,J2,J1) &
     &                     ) &
     &                   -     STATID%XIC2(1,J3,J2)*TIMTID%LAIS2(1,J3,J2,J1) &
     &                   -     STATID%XIS2(1,J3,J2)*TIMTID%LAIC2(1,J3,J2,J1)
                      ELSE IF ( J3 .EQ. 2 ) THEN
                         VEC(J3,J1) = VEC(J3,J1) &
     &                   + (   STATID%XRC2(1,J3,J2)*TIMTID%LARS2(1,J3,J2,J1) &
     &                       + STATID%XRC2(2,J3,J2)*TIMTID%LARS2(2,J3,J2,J1) &
     &                     ) &
     &                   + (   STATID%XRS2(1,J3,J2)*TIMTID%LARC2(1,J3,J2,J1) &
     &                       + STATID%XRS2(2,J3,J2)*TIMTID%LARC2(2,J3,J2,J1) &
     &                     ) &
     &                   +     STATID%XIC2(1,J3,J2)*TIMTID%LAIC2(1,J3,J2,J1) &
     &                   -     STATID%XIS2(1,J3,J2)*TIMTID%LAIS2(1,J3,J2,J1)
                    END IF
                  ELSE IF ( TIDCNF%GEN_LOVE .EQ. SOTID__GEN_ALL ) THEN
!
! ----------------- All generalized Love numbers are taken into account
!
                    IF ( J3 .EQ. 1  .OR.  J3 .EQ. 3 ) THEN
!
! ---------------------- Radial and north direction
!
                         VEC(J3,J1) = VEC(J3,J1) &
     &                   + (   STATID%XRC2(1,J3,J2)*TIMTID%LARC2(1,J3,J2,J1) &
     &                       + STATID%XRC2(2,J3,J2)*TIMTID%LARC2(2,J3,J2,J1) &
     &                       + STATID%XRC2(3,J3,J2)*TIMTID%LARC2(3,J3,J2,J1) &
     &                       + STATID%XRC2(4,J3,J2)*TIMTID%LARC2(4,J3,J2,J1) &
     &                     ) &
     &                   - (   STATID%XRS2(1,J3,J2)*TIMTID%LARS2(1,J3,J2,J1) &
     &                       + STATID%XRS2(2,J3,J2)*TIMTID%LARS2(2,J3,J2,J1) &
     &                       + STATID%XRS2(3,J3,J2)*TIMTID%LARS2(3,J3,J2,J1) &
     &                       + STATID%XRS2(4,J3,J2)*TIMTID%LARS2(4,J3,J2,J1) &
     &                     ) &
     &                   -     STATID%XIC2(1,J3,J2)*TIMTID%LAIS2(1,J3,J2,J1) &
     &                   -     STATID%XIS2(1,J3,J2)*TIMTID%LAIC2(1,J3,J2,J1)
                       ELSE IF ( J3 .EQ. 2 ) THEN
!
! ---------------------- East direction
!
                         VEC(J3,J1) = VEC(J3,J1) &
     &                   + (   STATID%XRC2(1,J3,J2)*TIMTID%LARS2(1,J3,J2,J1) &
     &                       + STATID%XRC2(2,J3,J2)*TIMTID%LARS2(2,J3,J2,J1) &
     &                       + STATID%XRC2(3,J3,J2)*TIMTID%LARS2(3,J3,J2,J1) &
     &                       + STATID%XRC2(4,J3,J2)*TIMTID%LARS2(4,J3,J2,J1) &
     &                     ) &
     &                   + (   STATID%XRS2(1,J3,J2)*TIMTID%LARC2(1,J3,J2,J1) &
     &                       + STATID%XRS2(2,J3,J2)*TIMTID%LARC2(2,J3,J2,J1) &
     &                       + STATID%XRS2(3,J3,J2)*TIMTID%LARC2(3,J3,J2,J1) &
     &                       + STATID%XRS2(4,J3,J2)*TIMTID%LARC2(4,J3,J2,J1) &
     &                     ) &
     &                   +     STATID%XIC2(1,J3,J2)*TIMTID%LAIC2(1,J3,J2,J1) &
     &                   -     STATID%XIS2(1,J3,J2)*TIMTID%LAIS2(1,J3,J2,J1)
                    END IF
               END IF
  430       CONTINUE
  420    CONTINUE
!
         IF ( TIDCNF%MODEL_3D .EQ. SOTID__3D_MDG97 ) THEN
!
! ----------- Computation of station displacements due to the tides
! ----------- of the third order
!   
              DO 440 J4=0,3 ! Over degree
                 DO 450 J5=1,3 ! Over radial, east and north component
                    IF ( J5 .EQ. 1  .OR.  J5 .EQ. 3 ) THEN
!
! ---------------------- Radial and north direction
!
                         VEC(J5,J1) = VEC(J5,J1) &
     &                            + STATID%XRC3(J5,J4)*TIMTID%LARC3(J5,J4,J1) &
     &                            - STATID%XRS3(J5,J4)*TIMTID%LARS3(J5,J4,J1)
                       ELSE IF ( J5 .EQ. 2 ) THEN
!
! ---------------------- East direction
!
                         VEC(J5,J1) = VEC(J5,J1) &
     &                            + STATID%XRC3(J5,J4)*TIMTID%LARS3(J5,J4,J1) &
     &                            + STATID%XRS3(J5,J4)*TIMTID%LARC3(J5,J4,J1)
                    END IF
 450          CONTINUE
 440       CONTINUE
         END IF
 410  CONTINUE 
!
      CALL COPY_R8 ( 3, VEC(1,SOTID__DER0), D_REN  )
      CALL COPY_R8 ( 3, VEC(1,SOTID__DER1), DT_REN )
!
      RETURN
      END  SUBROUTINE SOTID_DSP
