      SUBROUTINE VTD_DELAY ( SOU_NAM, STA1_NAM, STA2_NAM, MJD, TAI, &
     &                       OBS_TYP, VTD, DELAY, RATE, DER_DEL, DER_RAT, &
     &                       IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_DELAY  is the main routine of the package for         *
! *   computation of the VLBI Time Delay (VTD). It computes group delay  *
! *   and phase delay as well as partial derivatives of group delay and  *
! *   delay rate with respect to parameters of the model at the          *
! *   specified moment of time, for the specified pair of stations and   *
! *   for the specified source using the model specified from the        *
! *   control file previously loaded into the data structure VTD.        *
! *   It is assumed that routines VTD_INIT, VTD_CONF and VTD_LOAD were   *
! *   called before the call of VTD_DELAY.                               *
! *                                                                      *
! *   VLBI Time delay is defined as the difference of two intervals of   *
! *   proper time: 1) the interval of proper time of station #2 between  *
! *   events: coming the wave front to the reference point on the moving *
! *   axis and clock synchronization; 2) the interval of proper time of  *
! *   station #1 between events: coming the wave front to the reference  *
! *   point on the moving axis and clock synchronization. The time delay *
! *   is referred to the moment of coming the wave front to the          *
! *   reference point on the moving axis of the first antenna at time    *
! *   measured by the time scale TAI. The reference point of the station *
! *   for which modeling is done is defined as the point on the moving   *
! *   axes which has the minimal distance to the fixed axis. In the      *
! *   case if axes intersect, this is the point of intersection.         *
! *                                                                      *
! *   Precision of computation of VLBI time delay is deemed to be no     *
! *   worse than 1.D-12 seconds, precision of computation of delay rate  *
! *   is no worse than 1.D-15 . Accuracy of time delay computation is    *
! *   limited by the accuracy of a priori information. It can be as good *
! *   as 1.0D-10 .                                                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  SOU_NAM ( CHARACTER ) -- Name of the source. The source should      *
! *                           be in the input catalogue defined in the   *
! *                           control file.                              *
! * STA1_NAM ( CHARACTER ) -- Name of the first station of the baseline. *
! *                           The station should be in the input         *
! *                           catalogue defined in the control file.     *
! * STA2_NAM ( CHARACTER ) -- Name of the second station of the baseline.*
! *                           The station should be in the input         *
! *                           catalogue defined in the control file.     *
! *      MJD ( INTEGER*4 ) -- Modified Julian date of the midnight of    *
! *                           the observation.                           *
! *      TAI ( INTEGER*4 ) -- Time of the observations in seconds at     *
! *                           time scale TAI elapsed from the midnight.  *
! *  OBS_TYP ( VTD__OBS_TYPE ) -- The object with information about      *
! *                                  observation type, polarization and  *
! *                                  frequency setup of the experiment.  *
! *                                  It is used for                      *
! *                               1) computing source structure          *
! *                                  contribution;                       *
! *                               2) computing ionospheric contribution; *
! *                               3) computing contribution to phase     *
! *                                  caused by delay parallactic angle   *
! *                                  rotation.                           *
! *    Fields of VTD__OBS_TYPE:                                          *
! *                                                                      *
! *    INTEGER*4   %L_FRQ  ! The number of frequency channels within the *
! *                          band                                        *
! *    CHARACTER*2 %PLRZ   ! Polarization at each band: RR, LL, RL or LR *
! *                          'RR' -- both antennas observed right        *
! *                                  circular polarization;              *
! *                          'LL' -- both antennas observed right        *
! *                                  circular polarization;              *
! *                          'LR' -- Reference antenna observed left     *
! *                                  circular polarization and another   *
! *                                  antenna observed right circular     *
! *                                  polarization.                       *
! *                          'RL' -- Reference antenna observed left     *
! *                                  circular polarization and another   *
! *                                  antenna observed right circular     *
! *                                  polarization.                       *
! *                          'NO' -- Contribution due to parallactic     *
! *                                  angle correction is not computed.   *
! *    INTEGER*4   %STATUS  ! Status: VTD__UNDF, VTD__BND, VTD__CHN      *
! *                         VTD__UNDF -- ! Frequency setup is undefined. *
! *                                        This indicates that no        *
! *                                        information about frequencies *
! *                                        and polarization is available.*
! *                         VTD__BND  -- ! Frequency channel setup is    *
! *                                        undefined, but the reference  *
! *                                        frequency, effective          *
! *                                        ionospheric frequencies and   *
! *                                        polarization are known.       *
! *                         VTD__CHN  -- ! Frequency channel setup is    *
! *                                        known. Effective ionospheric  *
! *                                        frequencies are computed on   *
! *                                        the fly.                      *
! *    INTEGER*4   %N_BND ! Number of either bands or channel depending  *
! *                         on the status.                               *
! *    INTEGER*4   %DELAY_TYPE ! Delay type. Delay type depends on       *
! *                              band: low or high and on observable for *
! *                              each band: phase delay, multi-band      *
! *                              delay, narrow-band delay. The list of   *
! *                              supported combination is defined in     *
! *                              vtd.i                                   *
! *    CHARACTER   %EXP_NAME    ! Experiment name. Experiment name may   *
! *                             ! be used for gaining access to external *
! *                             ! calibration files.                     *
! *    CHARACTER   %SCAN_NAME   ! Scan name. Scan name may be used for   *
! *                             ! gaining access to external calibration *
! *                             ! files.                                 *
! *    REAL*8      %FRQ ! Array of cyclic frequencies. Dimension %L_FRQ. *
! *    REAL*8      %WEI ! Array of channel weights. Dimension %L_FRQ.    *
! *    REAL*8      %FRQ_REF  ! Reference frequency per band.             *
! *    REAL*8      %FRQ_ION_EFF ! Effective cyclic ionosphere            *
! *                             ! frequency for this type of delay       *
! *                             ! contribution. Units: Herz.             *
! *                                                                      *
! *    If no information about frequencies and polarization is           *
! *    available, set OBS_TYP%STATUS to VTD__UNDF. Then contents         *
! *    of  OBS_TYP will be ignored and contribution to delay caused      *
! *    by the ionosphere, source structure and parallactic anlge         *
! *    rotation will be set to zero.                                     *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    DELAY ( REAL*8    ) -- VLBI time delay. Units: seconds.           *
! *     RATE ( REAL*8    ) -- VLBI delay rate. Units: dimensionless.     *
! *  DER_DEL ( REAL*8    ) -- Vector of partial derivatives of group     *
! *                           delay with respect to parameters of the    *
! *                           model or some important intermediate       *
! *                           quantities. Dimension: DER__NDER.          *
! *            Symbolic names of the indexes of the partial derivatives  *
! *            in the array DER_DEL are defined in vtd.i .               *
! *            Their meaning:                                            *
! *                                                                      *
! *            VTD__DER_E1    -- Earth rotation Euler angle 1            *
! *            VTD__DER_E2    -- Earth rotation Euler angle 2            *
! *            VTD__DER_E3    -- Earth rotation Euler angle 3            *
! *            VTD__DER_ST1X  -- Station 1, X coordinate                 *
! *            VTD__DER_ST1Y  -- Station 1, Y coordinate                 *
! *            VTD__DER_ST1Z  -- Station 1, Z coordinate                 *
! *            VTD__DER_ST2X  -- Station 2, X coordinate                 *
! *            VTD__DER_ST2Y  -- Station 2, Y coordinate                 *
! *            VTD__DER_ST2Z  -- Station 2, Z coordinate                 *
! *            VTD__DER_RA    -- Right ascension of the far-zone source  *
! *            VTD__DER_DL    -- Declination of the far-zone source      *
! *            VTD__DER_AT1   -- Station 1, atmospheric path delay       *
! *            VTD__DER_AT2   -- Station 2, atmospheric path delay       *
! *            VTD__DER_ATN1  -- Station 1, atmospheric north tilt       *
! *            VTD__DER_ATE1  -- Station 1, atmospheric east  tilt       *
! *            VTD__DER_ATN2  -- Station 2, atmospheric north tilt       *
! *            VTD__DER_ATE2  -- Station 2, atmospheric east  tilt       *
! *            VTD__DER_POS1  -- Position of near zone object, X coord.  *
! *            VTD__DER_POS2  -- Position of near zone object, Y coord.  *
! *            VTD__DER_POS3  -- Position of near zone object, Z coord.  *
! *            VTD__DER_VEL1  -- Velocity of near zone object, X coord.  *
! *            VTD__DER_VEL2  -- Velocity of near zone object, Y coord.  *
! *            VTD__DER_VEL3  -- Velocity of near zone object, Z coord.  *
! *            VTD__DER_AXF1  -- Antenna axis offset length 1st antenna. *
! *            VTD__DER_AXF2  -- Antenna axis offset length 2nd antenna. *
! *            VTD__DER_GAMMA -- Relativity parameter gammaa.            *
! *                                                                      *
! *            VTD__ELEV1     -- Elevation angle of the 1st antenna      *
! *            VTD__ELEV2     -- Elevation angle of the 2nd antenna      *
! *            VTD__AZIM1     -- Aximuth angle of the 1st antenna        *
! *            VTD__AZIM2     -- Aximuth angle of the 2nd antenna        *
! *            VTD__TROP1     -- Slant   troposphere path at the         *
! *                              1st antenna.                            *
! *            VTD__TROP2     -- Slant   troposphere path at the         *
! *                              2nd antenna.                            *
! *            VTD__IONO1     -- Slant   ionosphere  path at the         *
! *                              1st antenna.                            *
! *            VTD__IONO2     -- Slant   ionosphere  path at the         *
! *                              2nd antenna.                            *
! *            VTD__TRP_HZD1  -- Hydrostatic troposphere path delay in   *
! *                              zentith at the 1st antenna.             *
! *            VTD__TRP_HZD2  -- Hydrostatic troposphere path delay in   *
! *                              zentith at the 2nd antenna.             *
! *            VTD__TRP_WZD1  -- Non-hydrostatic troposphere path delay  *
! *                              in zentith at the 1st antenna.          *
! *            VTD__TRP_WZD2  -- Non-hydrostatic troposphere path delay  *
! *                              in zentith at the 1nd antenna.          *
! *            VTD__PARAL1    -- Parallactic agle at the 1st antenna     *
! *            VTD__PARAL2    -- Parallactic agle at the 2nd antenna     *
! *            VTD__STRUC     -- Contribution of the source structure    *
! *                              to time delay.                          *
! *            VTD__ACCLR     -- Phase delay acceleration                *
! *            VTD__FEED1     -- Feed horn orientation agle at           *
! *                              the 1st antenna.                        *
! *            VTD__FEED2     -- Feed horn orientation agle at           *
! *                              the 2nd antenna.                        *
! *            VTD__SUR_PRS1  -- Surface atmospheric pressure at         *
! *                              the 1st station from the model.         *
! *            VTD__SUR_PRS2  -- Surface atmospheric pressure at         *
! *                              the 2nd station from the model.         *
! *            VTD__SUR_PWP1  -- Surface partial pressure of water vapor *
! *                              at the 1st station from the model.      *
! *            VTD__SUR_PWP2  -- Surface partial pressure of water vapor *
! *                              at the 2nd station from the model.      *
! *            VTD__SUR_TEM1  -- Surface temerature at the 1st station   *
! *                              from the model.                         *
! *            VTD__SUR_TEM2  -- Surface temerature at the 2nd station   *
! *                              from the model.                         *
! *            VTD__DEL_GCN1  -- Geometric path delay between the        *
! *                              geocetner and the 1st station at the    *
! *                              moment of the wavefront passing         *
! *                              the geocenter.                          *
! *            VTD__DEL_GCN2  -- Geometric path delay between the        *
! *                              geocetner and the 2nd station at the    *
! *                              moment of the wavefront passing         *
! *                              the geocenter.                          *
! *            VTD__UVX       -- Projection of the baseline vector on    *
! *                              the plane tangetial the source along    *
! *                              X axis. Units: meters.                  *
! *            VTD__UVY       -- Projection of the baseline vector on    *
! *                              the plane tangetial the source along    *
! *                              Y axis. Units: meters.                  *
! *                                                                      *
! *  DER_RAT ( REAL*8    ) -- Vector of partial derivatives of phase     *
! *                           delay rate with respect to parameters of   *
! *                           the model. Dimension: DER__NDER.           *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *     VTD ( RECORD    ) -- Object which keeps configuration and data   *
! *                          related to VLBI Theoretical Delay (VTD)     *
! *                          package.                                    *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the vase of errors. *
! *                                                                      *
! *  ### 27-JAN-2004   VTD_DELAY v3.19 (c)  L. Petrov  19-OCT-2023  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      CHARACTER  SOU_NAM*(*), STA1_NAM*(*), STA2_NAM*(*)
      INTEGER*4  MJD, IUER
      REAL*8     TAI, DELAY, RATE, &
     &           DER_DEL(VTD__NDER), DER_RAT(VTD__NDER)
      TYPE     ( VTD__TYPE      ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      REAL*8     AXOF_CRS(3,2), AXOF_TRS(3,2), STD_REN(3,2), STV_REN(3,2), &
     &           STD_TRS(3,2),  STV_TRS(3,2),  PTD_REN(3,2), PTD_TRS(3,2), &
     &           PSD_TRS(3),    PSV_TRS(3),    PSD_REN(3),   PSV_REN(3),   &
     &           RT_CRS(3,2),   STD_CRS(3,2),  STV_CRS(3,2), PSD_TRS_SUM(3,2), &
     &           PSV_TRS_SUM(3,2), TIME_REF, RD, UP_CRS_RATE(3),   &
     &           OMEGA,  AXOF_UP(2), AXOF_UP_RATE(2), PSD_CRS(3), &
     &           AXOF_CRS_RATE(3,2), AXOF_TRS_RATE(3,2), &
     &           UNIT_AXOF_CRS(3,2), UNIT_AXOF_CRS_RATE(3,2), CORR_FD(2)
      REAL*8     STD_CRS_DER1(3,2)
      INTEGER*4  ISTA(2), ISOU, NN, J1, J2, J3, J4, J5, ERR_MODE, IER
      REAL*8     TAU_GEOM, RATE_GEOM, ACLR_GEOM, TAU_STRUC, RATE_STRUC,  &
     &           TROP_GEOM_TAU, TROP_GEOM_RATE, TAU_GALABR, RATE_GALABR, &
     &           TMP(12)
      REAL*8,    SAVE :: IONO_LAT_GDT_PP(VTD__M_STA), IONO_LONG_PP(VTD__M_STA)
      LOGICAL*4  FL_STA(2)
      CHARACTER  STA_NAM*8, STR*128, SOLVE_DEBUG*8
      REAL*8     UP_CRS(3),    UP_UEN(3),    UP_TRS(3),    &
     &           NORTH_CRS(3), NORTH_UEN(3), NORTH_TRS(3), &
     &           EAST_CRS(3),  EAST_UEN(3),  EAST_TRS(3), &
     &           VEC_PROJ_EN(3), VAL, DT, N_PROJ, E_PROJ, &
     &           VEL_BAR_CRS(3), ACC_BAR_CRS(3), &
     &           S_APP_MAG, VEC1(3), VEC2(3), VEL_REL_SAT(3), ACC_REL_SAT(3), &
     &           ACC_SAT(3), AVEC_CRS(3,2), AVEC_CRS_RATE(3,2), VEC_PE_RATE(3), &
     &           NORTH_CRS_RATE(3), EAST_CRS_RATE(3), N_PROJ_RATE, E_PROJ_RATE, &
     &           DELAY_BIAS, RATE_BIAS, S_MAG, SOU_MAG
      REAL*8     SV, SV2, SB1, SB2, SA, COS_SO_EA, FEED_COS, C1, C2, EPS
      PARAMETER  ( EPS = 1.D-6 )
      REAL*8     X_VEC(3), Y_VEC(3), Z_VEC(3), B_CRS(3)
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      LOGICAL*4, EXTERNAL :: PROBE_WRITE_ADDRESS
      REAL*8,    EXTERNAL :: ATAN_CS, DP_VV_V, NMF_W, NMF_H
      INTEGER*4, EXTERNAL :: GET_ERR_MODE, ILEN, I_LEN, VTD_STA_INDEX, VTD_SOU_INDEX
!
      ERR_MODE = GET_ERR_MODE ()
      IF ( ERR_MODE == 0 ) THEN
           IF ( .NOT. PROBE_WRITE_ADDRESS ( VTD%STATUS ) ) THEN
                CALL ERR_LOG ( 2211, IUER, 'VTD_DELAY', 'Object VTD is not '// &
    &               'accessible for writing. It is an indication of a very '// &
    &               'serious error. Please check the argument list' )
                RETURN
           END IF
      END IF
!
      IF ( VTD%STATUS .NE. VTD__LOAD ) THEN
           CALL ERR_LOG ( 2212, IUER, 'VTD_DELAY', 'Routine VTD_LOAD was '// &
     &         'not executed before the call of VTD_DELAY' )
           RETURN
      END IF
      CALL GETENVAR ( 'SOLVE_DEBUG', SOLVE_DEBUG )
!
! --- Compute parameters which depends only on time, but do not depend on
! --- station
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_MOMENT ( SOU_NAM, MJD, TAI, VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2213, IUER, 'VTD_DELAY', 'Error in an attempt '// &
     &         'to compute station-independent time varying intermediate '// &
     &         'quantities' )
           RETURN
      END IF
!
      IF ( VTD%CONF%IVRB .GE. 1 ) THEN
           WRITE ( 6, 210 ) 'TRS_TO_CRS  = ', &
     &                      VTD%MOM%TRS_TO_CRS(1,1), &
     &                      VTD%MOM%TRS_TO_CRS(2,1), &
     &                      VTD%MOM%TRS_TO_CRS(3,1), &
     &                      'TRS_TO_CRS  = ',        &
     &                      VTD%MOM%TRS_TO_CRS(1,2), &
     &                      VTD%MOM%TRS_TO_CRS(2,2), &
     &                      VTD%MOM%TRS_TO_CRS(3,2), &
     &                      'TRS_TO_CRS  = ',        &
     &                      VTD%MOM%TRS_TO_CRS(1,3), &
     &                      VTD%MOM%TRS_TO_CRS(2,3), &
     &                      VTD%MOM%TRS_TO_CRS(3,3)
           WRITE ( 6, 210 ) 'TRS_TO_CRS1 = ',             &
     &                      VTD%MOM%TRS_TO_CRS_DER1(1,1), &
     &                      VTD%MOM%TRS_TO_CRS_DER1(2,1), &
     &                      VTD%MOM%TRS_TO_CRS_DER1(3,1), &
     &                      'TRS_TO_CRS1 = ',             &
     &                      VTD%MOM%TRS_TO_CRS_DER1(1,2), &
     &                      VTD%MOM%TRS_TO_CRS_DER1(2,2), &
     &                      VTD%MOM%TRS_TO_CRS_DER1(3,2), &
     &                      'TRS_TO_CRS1 = ',             &
     &                      VTD%MOM%TRS_TO_CRS_DER1(1,3), &
     &                      VTD%MOM%TRS_TO_CRS_DER1(2,3), &
     &                      VTD%MOM%TRS_TO_CRS_DER1(3,3)
   210     FORMAT ( 3( A, 3(F18.15,2X)/) )
      END IF
!
      ISTA(1) = VTD_STA_INDEX ( VTD, STA1_NAM )
      ISTA(2) = VTD_STA_INDEX ( VTD, STA2_NAM )
      ISOU    = VTD_SOU_INDEX ( VTD, SOU_NAM  )
!
      IF ( ISTA(1) .EQ. 0 ) THEN
           CALL ERR_LOG ( 2214, IUER, 'VTD_DELAY', 'Station '//STA1_NAM// &
     &         ' was not found in the list of stations' )
           RETURN
      END IF
!
      IF ( ISTA(2) .EQ. 0 ) THEN
           CALL ERR_LOG ( 2215, IUER, 'VTD_DELAY', 'Station '//STA2_NAM// &
     &         ' was not found in the list of stations' )
           RETURN
      END IF
!
      IF ( ISOU .EQ. 0 ) THEN
           CALL ERR_LOG ( 2216, IUER, 'VTD_DELAY', 'Source '//SOU_NAM// &
     &         ' was not found in the list of sources' )
           RETURN
      END IF
      IF ( VTD%CONF%IVRB .GE. 4 ) THEN
           WRITE ( 6, 260 ) VTD%SOU(ISOU)%IVS_NAME, &
     &                      VTD%SOU(ISOU)%ALPHA, &
     &                      VTD%SOU(ISOU)%DELTA, &
     &                      VTD%SOU(ISOU)%S_CRS, &
     &                      VTD%SOU(ISOU)%OBJ_TYPE, &
     &                      VTD%SOU(ISOU)%NZO_NAME
 260       FORMAT ( 'Source: ',A8,' Alpha: ', F20.16,' Delta: ', F20.16/ &
     &              'S_vec: ', 3(F18.15,1X), ' Type: ' , A, ' Nzo_name: ', A )
           IF ( VTD%SOU(ISOU)%OBJ_TYPE == VTD__ES .OR. &
     &          VTD%SOU(ISOU)%OBJ_TYPE == VTD__SS      ) THEN
                WRITE ( 6, 266 ) VTD%SOU(ISOU)%IVS_NAME, VTD%SOU(ISOU)%DIST, &
     &                           VTD%SOU(ISOU)%DIST_RATE, VTD%SOU(ISOU)%S_CRS_RATE
 266            FORMAT ( 'Sou:    ', A8, ' Dist: ', 1PD15.8, ' Dist_dot= ', 1PD15.8, &
     &                   ' S_vec_dot: ', 3(1PD15.8,1X) )
           END IF
      END IF
!
      DO 430 J3=1,2 ! Cycle over stations: the first and the second
         FL_STA(J3) = .FALSE.
         IF ( J3 .EQ. 1 ) STA_NAM = STA1_NAM
         IF ( J3 .EQ. 2 ) STA_NAM = STA2_NAM
!
! ------ Check whether station-dependent parameters have been already
! ------ been computed
!
         IF ( MJD     == VTD%STA(ISTA(J3))%MJD                   .AND. &
     &        STA_NAM == VTD%STA(ISTA(J3))%IVS_NAME              .AND. &
     &        VTD%STA(ISTA(J3))%STA_TYP == VTD__GR               .AND. &
     &        DABS(TAI - VTD%STA(ISTA(J3))%TAI ) < VTD__TIME_TOL       ) THEN
!
              IF ( VTD%STA(ISTA(J3))%IONO_STATUS == VTD__IONO_BADF ) THEN
!
! ---------------- Redo computation of inosphere delay if the present status
! ---------------- of ionosphere delay is "BAD FREQUENCY". It may happen
! ---------------- that previous observation was non-detection, and ionsphere
! ---------------- effective frequency may be not defined. In that case we cannot
! ---------------- re-use ionosphere path delay from the previous observation
! ---------------- at this station.
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL VTD_IONO_DELAY ( VTD, OBS_TYP, ISTA(J3), ISOU, &
     &                                   VTD%STA(ISTA(J3))%TEC, &
     &                                   VTD%STA(ISTA(J3))%IONO_MAP, &
     &                                   VTD%STA(ISTA(J3))%IONO_DEL, &
     &                                   VTD%STA(ISTA(J3))%IONO_RATE,  &
     &                                   IONO_LAT_GDT_PP(ISTA(J3)), &
     &                                   IONO_LONG_PP(ISTA(J3)), &
     &                                   VTD%STA(ISTA(J3))%IONO_STATUS,  IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 2217, IUER, 'VTD_DELAY', 'Error in '// &
     &                      'an attempt to compute the contribution of the '// &
     &                      'ionosphere on VLBI time delay' )
                        RETURN
                   END IF
              END IF
              GOTO 430
         END IF
!
         VTD%STA(ISTA(J3))%MJD = MJD
         VTD%STA(ISTA(J3))%TAI = TAI
!
         CALL NOUT_R8 ( 3, AXOF_CRS(1,J3) )
         CALL NOUT_R8 ( 3, AXOF_TRS(1,J3) )
         CALL NOUT_R8 ( 3, AXOF_CRS_RATE(1,J3) )
         CALL NOUT_R8 ( 3, AXOF_TRS_RATE(1,J3) )
         AXOF_UP(J3) = 0.0D0
         AXOF_UP_RATE(J3) = 0.0D0
         IF ( VTD%STA(ISTA(J3))%STA_TYP .NE. VTD__GC .AND. &
     &        ( VTD%CONF%AXOF_MODEL .EQ. VTD__YES  .OR. &
     &          VTD%CONF%AXOF_MODEL .EQ. VTD__CALC      ) ) THEN
!
! ----------- Compute the vector of antenna's axis offset
!
              CALL ERR_PASS ( IUER, IER )
              CALL VTD_AXOF ( VTD, ISTA(J3), ISOU, AXOF_CRS(1,J3), &
     &                        AXOF_CRS_RATE(1,J3), AXOF_UP(J3), &
     &                        AXOF_UP_RATE(J3), AVEC_CRS(1,J3), &
     &                        AVEC_CRS_RATE(1,J3), UNIT_AXOF_CRS(1,J3), &
     &                        UNIT_AXOF_CRS_RATE(1,J3), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2218, IUER, 'VTD_DELAY', 'Error in an '// &
     &                 'attempt to compute antenna axis offset for station '// &
     &                  VTD%STA(ISTA(J3))%IVS_NAME )
                   RETURN
              END IF
!
! ----------- Transform the vector of antenna axis offset from CRS to TRS
!
              IER = -1
              CALL MUL_MV_TV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                              3, AXOF_CRS(1,J3),     &
     &                              3, AXOF_TRS(1,J3), IER )
              IF ( VTD%CONF%FL_RATE ) THEN
                   IER = -1
                   CALL MUL_MV_TV_V ( 3, 3, VTD%MOM%TRS_TO_CRS,      &
     &                                   3, AXOF_CRS_RATE(1,J3),     &
     &                                   3, AXOF_TRS_RATE(1,J3), IER )
                   IER = -1
                   CALL MUL_MV_TV_V ( 3, 3, VTD%MOM%TRS_TO_CRS_DER1, &
     &                                   3, AXOF_CRS(1,J3),          &
     &                                   3, VEC1, IER )
                   CALL ADD_VV ( 3, AXOF_TRS_RATE(1,J3), VEC1 )
              END IF
!
              IF ( VTD%CONF%IVRB .GE. 1 ) THEN
                   WRITE ( 6, 110 ) VTD%STA(ISTA(J3))%IVS_NAME, &
     &                              (AXOF_CRS(NN,J3), NN=1,3)
 110               FORMAT ( 1X,'Axof vector      ',A, ' (CRS): ', &
     &                      3(F12.9,', '),' m ' )
              END IF
!
! ----------- Compute derivatives on anteanna axis offset length
!
              VTD%STA(ISTA(J3))%AXIS_DELAY_DER = &
     &                     DP_VV_V ( 3, UNIT_AXOF_CRS(1,J3), &
     &                               VTD%SOU(ISOU)%S_CRS )/ VTD__C / &
     &            (1.0D0 + DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART),  &
     &                                  VTD%SOU(ISOU)%S_CRS ) / VTD__C   )
              VTD%STA(ISTA(J3))%AXIS_RATE_DER  = &
     &                     DP_VV_V ( 3, UNIT_AXOF_CRS_RATE(1,J3), &
     &                               VTD%SOU(ISOU)%S_CRS )/ VTD__C / &
     &            (1.0D0 + DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART),  &
     &                                  VTD%SOU(ISOU)%S_CRS ) / VTD__C   )
            ELSE
              VTD%STA(ISTA(J3))%AXIS_DELAY_DER = 0.0D0
              VTD%STA(ISTA(J3))%AXIS_RATE_DER  = 0.0D0
         END IF
!
         IF ( VTD%STA(ISTA(J3))%STA_TYP .NE. VTD__GC .AND. &
     &        VTD%CONF%PARALLACTIC_ANGLE == VTD__YES       ) THEN
!
! ----------- Compute parallactic angle for both stations and its time
! ----------- derivative
!
              CALL VTD_PARANG ( AVEC_CRS(1,J3), AVEC_CRS_RATE(1,J3), &
     &                          VTD%SOU(ISOU)%S_CRS, VTD%MOM%TRS_TO_CRS, &
     &                          VTD%STA(ISTA(J3))%PARAL_ANG, &
     &                          VTD%STA(ISTA(J3))%PARAL_ANG_RATE )
            ELSE
              VTD%STA(ISTA(J3))%PARAL_ANG      = 0.0D0
              VTD%STA(ISTA(J3))%PARAL_ANG_RATE = 0.0D0
         END IF
!
         IF ( ILEN(VTD%CONF%FINAM_ANTI) > 0        .AND. &
     &        VTD%STA(ISTA(J3))%STA_TYP == VTD__GR       ) THEN
!
              CALL ERR_PASS ( IUER, IER )
              CALL VTD_ANT_THERMAL ( VTD, ISOU, ISTA(J3), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2219, IUER, 'VTD_DELAY', 'Error in an '// &
     &                 'attempt to compute thermal deformation '// &
     &                 'contribution to path delay for the antenna at '// &
     &                 'the station of '//VTD%STA(ISTA(J3))%IVS_NAME )
                   RETURN
              END IF
            ELSE
              VTD%STA(ISTA(J3))%ANT_THERMAL_DEL = 0.0D0
         END IF
!
! ------ Compute displacement caused by the solid Earth tides
!
         IF ( VTD%CONF%STD_2ND_MODEL == VTD__NONE     .OR. &
     &        VTD%STA(ISTA(J3))%STA_TYP .NE. VTD__GR       ) THEN
              CALL NOUT_R8 ( 3, STD_REN(1,J3) )
              CALL NOUT_R8 ( 3, STD_TRS(1,J3) )
              CALL NOUT_R8 ( 3, STV_REN(1,J3) )
              CALL NOUT_R8 ( 3, STV_TRS(1,J3) )
            ELSE
              CALL SOTID_DSP ( VTD%TIDCNF_STD, VTD%TIMTID, &
     &                         VTD%STATID(ISTA(J3)), STD_REN(1,J3), &
     &                         STV_REN(1,J3) )
!
! ----------- Transform the displacement vector and its first time derivative
! ----------- from REN to TRS
!
              CALL MUL_MV_IV_V ( 3, 3, VTD%STA(ISTA(J3))%REN_TO_TRS, &
     &                           3, STD_REN(1,J3), 3, STD_TRS(1,J3), IER )
              CALL MUL_MV_IV_V ( 3, 3, VTD%STA(ISTA(J3))%REN_TO_TRS, &
     &                           3, STV_REN(1,J3), 3, STV_TRS(1,J3), IER )
              IF ( VTD%CONF%IVRB .GE. 1 ) THEN
                   CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, 3, &
     &                                STD_TRS(1,J3), 3, STD_CRS(1,J3), IER )
                   CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, 3, &
     &                                STV_TRS(1,J3), 3, STV_CRS(1,J3), IER )
!
                   WRITE ( 6, 120 ) VTD%STA(ISTA(J3))%IVS_NAME, 'REN', &
     &                              (STD_REN(NN,J3), NN=1,3)
                   WRITE ( 6, 120 ) VTD%STA(ISTA(J3))%IVS_NAME, 'TRS', &
     &                              (STD_TRS(NN,J3), NN=1,3)
                   WRITE ( 6, 120 ) VTD%STA(ISTA(J3))%IVS_NAME, 'CRS', &
     &                              (STD_CRS(NN,J3), NN=1,3)
 120               FORMAT ( 1X,'Solid Earth tide ',A, ' (',A,'): ', &
     &                      3(F12.9,', '),' m ' )
!
                   CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS_DER1, 3, &
     &                                STD_TRS(1,J3), 3, STD_CRS_DER1(1,J3), IER )
!
                   WRITE ( 6, 130 ) VTD%STA(ISTA(J3))%IVS_NAME, 'REN', &
     &                              (STV_REN(NN,J3), NN=1,3)
                   WRITE ( 6, 130 ) VTD%STA(ISTA(J3))%IVS_NAME, 'TRS', &
     &                              (STV_TRS(NN,J3), NN=1,3)
                   WRITE ( 6, 130 ) VTD%STA(ISTA(J3))%IVS_NAME, 'CRS', &
     &                              (STV_CRS(NN,J3)+STD_CRS_DER1(NN,J3), NN=1,3)
!
 130               FORMAT ( 1X,'Rate of std      ',A, ' (',A,'): ', &
     &                      3(1PD12.5,', '),' m/' )
              END IF
         END IF
!
! ------ Compute displacements caused by pole tide
!
         IF ( VTD%CONF%PTD_MODEL == VTD__NONE        .OR. &
     &        VTD%STA(ISTA(J3))%STA_TYP .NE. VTD__GR      ) THEN
!
              CALL NOUT_R8 ( 3, PTD_REN(1,J3) )
              CALL NOUT_R8 ( 3, PTD_TRS(1,J3) )
            ELSE
              CALL ERR_PASS ( IUER, IER )
              CALL VTD_POTID_DSP ( VTD, ISTA(J3), PTD_REN(1,J3), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2220, IUER, 'VTD_DELAY', 'Error in an '// &
     &                'attempt to compute displacements caused by pole '// &
     &                'tide at station '//VTD%STA(ISTA(J3))%IVS_NAME )
                   RETURN
              END IF
!
              IF ( VTD%CONF%IVRB .GE. 1 ) THEN
                   WRITE ( 6, 140 ) VTD%STA(ISTA(J3))%IVS_NAME, &
     &                              (PTD_REN(NN,J3), NN=1,3)
 140               FORMAT ( 1X,'Pole tide for    ',A, ' (REN): ', &
     &                      3(F12.9,', '),' m ' )
              END IF
!
! ----------- Transform the displacement vector from REN to TRS
!
              CALL MUL_MV_IV_V ( 3, 3, VTD%STA(ISTA(J3))%REN_TO_TRS, &
     &                           3, PTD_REN(1,J3), 3, PTD_TRS(1,J3), IER )
         END IF
!
! ------ Compute position variations for various other numerical models
!
         CALL NOUT_R8 ( 3, PSD_TRS_SUM(1,J3) )
         CALL NOUT_R8 ( 3, PSV_TRS_SUM(1,J3) )
         DO 440 J4=1,VTD__M_PSF
            IF ( ILEN(VTD%CONF%POSVAR_FIL(J4)) .GT. 0   .AND. &
     &           VTD%STA(ISTA(J3))%STA_TYP == VTD__GR        ) THEN
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL VTD_GET_POSVAR ( VTD, ISTA(J3), J4, PSD_TRS, PSV_TRS, &
     &                                 IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 2221, IUER, 'VTD_DELAY', 'Error in an '// &
     &                    'attempt to compute displacements caused by '// &
     &                    'position variation at station '// &
     &                    VTD%STA(ISTA(J3))%IVS_NAME )
                      RETURN
                 END IF
!
                 IF ( VTD%CONF%IVRB .GE. 1 ) THEN
                      CALL MUL_MV_TV_V ( 3, 3, VTD%STA(ISTA(J3))%REN_TO_TRS, &
     &                                   3, PSD_TRS, 3, PSD_REN, IER )
                      CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, 3, &
     &                                   PSD_TRS, 3, PSD_CRS, IER )
                      WRITE ( 6, 150 ) J4, VTD%STA(ISTA(J3))%IVS_NAME, 'TRS', &
     &                                 (PSD_TRS(NN), NN=1,3)
                      WRITE ( 6, 150 ) J4, VTD%STA(ISTA(J3))%IVS_NAME, 'REN', &
     &                                 (PSD_REN(NN), NN=1,3)
                      WRITE ( 6, 150 ) J4, VTD%STA(ISTA(J3))%IVS_NAME, 'CRS', &
     &                                 (PSD_CRS(NN), NN=1,3)
 150                  FORMAT ( 1X,'Pos.varitaions ',I1, 1X, A, ' (',A,'): ', &
     &                             3(F12.9,', '),' m ' )
!
                      CALL MUL_MV_TV_V ( 3, 3, VTD%STA(ISTA(J3))%REN_TO_TRS, &
     &                                   3, PSV_TRS, 3, PSV_REN, IER )
                      WRITE ( 6, 160 ) J4, VTD%STA(ISTA(J3))%IVS_NAME, 'TRS', &
     &                                 (PSV_TRS(NN), NN=1,3)
                      WRITE ( 6, 160 ) J4, VTD%STA(ISTA(J3))%IVS_NAME, 'REN', &
     &                                 (PSV_REN(NN), NN=1,3)
 160                  FORMAT ( 1X,'Rate pos.var.  ',I1, 1X, A, ' (',A,'): ', &
     &                          3(1PD12.5,', '),' m/' )
                 END IF
!
! -------------- Add displacements due to this model to the total displacements
!
                 CALL ADD_VV ( 3, PSD_TRS_SUM(1,J3), PSD_TRS )
                 CALL ADD_VV ( 3, PSV_TRS_SUM(1,J3), PSV_TRS )
             END IF
 440     CONTINUE
!
! ------ Compute durataion of time from the site position reference epoch
! ------ till now
!
         TIME_REF =   (VTD%MOM%MJD - VTD%STA(ISTA(J3))%MJD_REF)*86400.0D0 &
     &              + (VTD%MOM%TAI - VTD%STA(ISTA(J3))%TAI_REF)
         DO 450 J5=1,3
            VTD%STA(ISTA(J3))%MOM_COO_TRS(J5) = VTD%STA(ISTA(J3))%BEG_TRS(J5) &
     &                      + VTD%STA(ISTA(J3))%VEL_TRS(J5)*TIME_REF  &
     &                      + AXOF_TRS(J5,J3)                         &
     &                      + STD_TRS(J5,J3)                          &
     &                      + PTD_TRS(J5,J3)                          &
     &                      + PSD_TRS_SUM(J5,J3)
            VTD%STA(ISTA(J3))%MOM_VEL_TRS(J5) =   &
     &                         VTD%STA(ISTA(J3))%VEL_TRS(J5) &
     &                       + AXOF_TRS_RATE(J5,J3)          &
     &                       + STV_TRS(J5,J3)                &
     &                       + PSV_TRS_SUM(J5,J3)
            IF ( VTD%CONF%IVRB .GE. 1 ) THEN
                 WRITE ( 6, 170 ) VTD%MOM%MJD, VTD%MOM%TAI, J5, &
     &                            VTD%STA(ISTA(J3))%IVS_NAME, 'TRS', &
     &                            VTD%STA(ISTA(J3))%MOM_COO_TRS(J5), &
     &                            VTD%STA(ISTA(J3))%MOM_VEL_TRS(J5)
 170             FORMAT ( 1X,'MJD= ',I5,' TAI= ', F8.2, ' pos. ',I1, 1X, &
     &                    A, ' (',A,'): ', F14.5,' ', F12.7 )
            ENDIF
 450     CONTINUE
!
! ------ Transform the total vector of instantaneous site coordinates
! ------ from TRS to CRS
!
         IF ( VTD%STA(ISTA(J3))%STA_TYP == VTD__GR  ) THEN
              CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                              3, VTD%STA(ISTA(J3))%MOM_COO_TRS, &
     &                              3, VTD%STA(ISTA(J3))%COO_CRS, IER )
              CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                              3, VTD%STA(ISTA(J3))%MOM_VEL_TRS, &
     &                              3, RT_CRS(1,J3), IER )
            ELSE 
              CALL NOUT_R8 ( 3, RT_CRS(1,J3) )
         END IF
!
! ------ Compute the vector of local zenith in CRS and then
! ------ rate of its change
!
         UP_UEN(1) = 1.0D0
         UP_UEN(2) = 0.0D0
         UP_UEN(3) = 0.0D0
         CALL MUL_MV_IV_V ( 3, 3, VTD%STA(ISTA(J3))%UEN_TO_TRS,  &
     &                         3, UP_UEN, 3, UP_TRS, IER )
         CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                         3, UP_TRS, 3, UP_CRS, IER )
         IF ( VTD%CONF%FL_RATE ) THEN
              CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS_DER1, &
     &                           3, UP_TRS, 3, UP_CRS_RATE, IER )
         END IF
         NORTH_UEN(1) = 0.0D0
         NORTH_UEN(2) = 0.0D0
         NORTH_UEN(3) = 1.0D0
         CALL MUL_MV_IV_V ( 3, 3, VTD%STA(ISTA(J3))%UEN_TO_TRS,  &
     &                         3, NORTH_UEN, 3, NORTH_TRS, IER )
         CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                         3, NORTH_TRS, 3, NORTH_CRS, IER )
         IF ( VTD%CONF%FL_RATE ) THEN
              CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS_DER1, &
     &                           3, NORTH_TRS, 3, NORTH_CRS_RATE, IER )
         END IF
!
! ------ Compute velocity of the station
! ------ First: TRS_TO_CRS' * R_VEC
!
         IF ( VTD%STA(ISTA(J3))%STA_TYP == VTD__GR  ) THEN
              CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS_DER1, &
     &                           3, VTD%STA(ISTA(J3))%MOM_COO_TRS, &
     &                           3, VTD%STA(ISTA(J3))%VEL_CRS, IER )
           ELSE IF ( VTD%STA(ISTA(J3))%STA_TYP == VTD__GC  ) THEN
              CALL NOUT_R8 ( 3, VTD%STA(ISTA(J3))%VEL_CRS )
           ELSE IF ( VTD%STA(ISTA(J3))%STA_TYP == VTD__OR  ) THEN
              CONTINUE 
         END IF
!
! ------ Second: TRS_TO_CRS * R_VEC'
!
         CALL ADD_VV ( 3, VTD%STA(ISTA(J3))%VEL_CRS, RT_CRS(1,J3) )
!
! ------ Compute acceleration of the station as TRS_TO_CRS'' * R_VEC
!
         IF ( VTD%STA(ISTA(J3))%STA_TYP == VTD__GR  ) THEN
              CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS_DER2, &
     &                              3, VTD%STA(ISTA(J3))%MOM_COO_TRS, &
     &                              3, VTD%STA(ISTA(J3))%ACC_CRS, IER )
           ELSE IF ( VTD%STA(ISTA(J3))%STA_TYP == VTD__GC  ) THEN
              CALL NOUT_R8 ( 3, VTD%STA(ISTA(J3))%ACC_CRS )
           ELSE IF ( VTD%STA(ISTA(J3))%STA_TYP == VTD__OR  ) THEN
              CONTINUE 
         END IF
!
! ------ Get baricentric velocity of the station
!
         CALL ADDC_VV ( 3, 1.0D0, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                     1.0D0, VTD%STA(ISTA(J3))%VEL_CRS, VEL_BAR_CRS )
!
! ------ Compute geometric elevation angle (no refraction, but aberration)
!
         IF ( VTD%SOU(ISOU)%OBJ_TYPE == VTD__ES .OR. &
     &        VTD%SOU(ISOU)%OBJ_TYPE == VTD__SS      ) THEN
!
! ----------- If Earth satellite or a Solar System objeect, need to compute the topocentric source vector
!
              VTD%STA(ISTA(J3))%TOPO_S_CRS      =  VTD%SOU(ISOU)%SOU_CRS - VTD%STA(ISTA(J3))%COO_CRS
              VTD%STA(ISTA(J3))%TOPO_S_CRS_RATE =  VTD%SOU(ISOU)%SOU_CRS - VTD%STA(ISTA(J3))%VEL_CRS
              CALL NORM_VEC ( 3, VTD%STA(ISTA(J3))%TOPO_S_CRS, VTD%STA(ISTA(J3))%TOPO_S_DIST )
              VEL_REL_SAT = VTD%SOU(ISOU)%SOU_CRS_RATE - VTD%STA(ISTA(J3))%VEL_CRS
              SV = DP_VV_V  ( 3, VTD%STA(ISTA(J3))%TOPO_S_CRS, VEL_REL_SAT )
              CALL ADDC_VV  ( 3, 1.0D0/VTD__C,        VEL_REL_SAT, &
     &                           (1.0D0 - SV/VTD__C), VTD%STA(ISTA(J3))%TOPO_S_CRS, &
     &                           VTD%SOU(ISOU)%SAPP_CRS )
           ELSE
              VTD%STA(ISTA(J3))%TOPO_S_CRS      =  VTD%SOU(ISOU)%SOU_CRS
              VTD%STA(ISTA(J3))%TOPO_S_CRS_RATE =  VTD%SOU(ISOU)%SOU_CRS_RATE
              SV = DP_VV_V  ( 3, VTD%SOU(ISOU)%S_CRS, VEL_BAR_CRS )
              CALL ADDC_VV  ( 3, 1.0D0/VTD__C,        VEL_BAR_CRS, &
     &                           (1.0D0 - SV/VTD__C), VTD%SOU(ISOU)%S_CRS, &
     &                           VTD%SOU(ISOU)%SAPP_CRS )
         END IF
!         
         CALL NORM_VEC ( 3, VTD%SOU(ISOU)%SAPP_CRS, S_APP_MAG )
         VTD%STA(ISTA(J3))%ELEV = DASIN ( DP_VV_V ( 3, UP_CRS, VTD%SOU(ISOU)%SAPP_CRS ) )

         IF ( VTD%CONF%FL_RATE ) THEN
!
! ----------- ... and its rate
!
              IF ( VTD%SOU(ISOU)%OBJ_TYPE == VTD__ES .OR. &
     &             VTD%SOU(ISOU)%OBJ_TYPE == VTD__SS      ) THEN
!
! ---------------- Use 2 body satellite acceleration to get relative acc.
!
                   SOU_MAG = SQRT ( DP_VV_V ( 3, VTD%SOU(ISOU)%SOU_CRS(1:3), &
     &                              VTD%SOU(ISOU)%SOU_CRS(1:3) ) )
                   ACC_SAT = -SOTID__GEO_CG * VTD%SOU(ISOU)%SOU_CRS(1:3) / SOU_MAG**3
                   ACC_REL_SAT = ACC_SAT - VTD%STA(ISTA(J3))%ACC_CRS
                   SA = DP_VV_V  ( 3, VTD%STA(J3)%TOPO_S_CRS, ACC_SAT )
                   CALL ADDC_VV  ( 3, 1.0D0/VTD__C/S_APP_MAG, ACC_SAT, &
     &                                -SA/VTD__C/S_APP_MAG,   VTD%STA(J3)%TOPO_S_CRS, &
     &                                VTD%SOU(ISOU)%SAPP_CRS_RATE )
                ELSE
                   CALL ADDC_VV ( 3, 1.0D0, VTD%MOM%PLAN(1,VTD__ACC,VTD__EART), &
     &                               1.0D0, VTD%STA(ISTA(J3))%ACC_CRS, ACC_BAR_CRS )
                   SA = DP_VV_V  ( 3, VTD%SOU(ISOU)%S_CRS, ACC_BAR_CRS )
                   CALL ADDC_VV  ( 3, 1.0D0/VTD__C/S_APP_MAG, ACC_BAR_CRS, &
     &                                -SA/VTD__C/S_APP_MAG,   VTD%SOU(ISOU)%S_CRS, &
     &                                VTD%SOU(ISOU)%SAPP_CRS_RATE )
              END IF
!
! ----------- ... and its rate of change
!
              VTD%STA(ISTA(J3))%ELEV_DER = ( DP_VV_V( 3, UP_CRS_RATE, VTD%SOU(ISOU)%SAPP_CRS) + &
     &                                       DP_VV_V( 3, UP_CRS, VTD%SOU(ISOU)%SAPP_CRS_RATE)   )/ &
     &               DSQRT ( 1.0D0 - DP_VV_V ( 3, UP_CRS, VTD%SOU(ISOU)%SAPP_CRS )**2 )
         END IF
!
! ------ Now let us compute azimuth. In order to do it, first compute
! ------ the projection of the source vector to the horizontal plane
!
         CALL ADDC_VV  ( 3, 1.0D0, VTD%SOU(ISOU)%SAPP_CRS, -DP_VV_V( 3, UP_CRS, VTD%SOU(ISOU)%SAPP_CRS ), &
     &                   UP_CRS, VEC_PROJ_EN )
         CALL NORM_VEC ( 3, VEC_PROJ_EN, VAL )
         IF ( VTD%CONF%FL_RATE ) THEN
              VEC_PE_RATE =   VTD%SOU(ISOU)%SAPP_CRS_RATE &
     &                 - DP_VV_V ( 3, VTD%SOU(ISOU)%SAPP_CRS,      UP_CRS      )*UP_CRS_RATE &
     &                 - DP_VV_V ( 3, VTD%SOU(ISOU)%SAPP_CRS_RATE, UP_CRS      )*UP_CRS &
     &                 - DP_VV_V ( 3, VTD%SOU(ISOU)%SAPP_CRS,      UP_CRS_RATE )*UP_CRS
              VEC_PE_RATE = ( VEC_PE_RATE &
     &                       - DP_VV_V ( 3, VEC_PROJ_EN, VEC_PE_RATE )* &
     &                         VEC_PROJ_EN )/VAL
         END IF
!
! ------ Then compute the north projection of that projection ...
!
         N_PROJ = DP_VV_V ( 3, VEC_PROJ_EN, NORTH_CRS )
!
! ------ ... and east projection of that projection
!
         CALL VM83 ( NORTH_CRS, UP_CRS, EAST_CRS )
         E_PROJ = DP_VV_V ( 3, VEC_PROJ_EN, EAST_CRS  )
         IF ( VTD%CONF%FL_RATE ) THEN
!
! ----------- ... and its time derivative.
!
              N_PROJ_RATE = DP_VV_V ( 3, VEC_PE_RATE, NORTH_CRS ) + &
     &                      DP_VV_V ( 3, VEC_PROJ_EN, NORTH_CRS_RATE )
              CALL VM83 ( NORTH_CRS_RATE, UP_CRS, VEC1 )
              CALL VM83 ( NORTH_CRS, UP_CRS_RATE, VEC2 )
              EAST_CRS_RATE = VEC1 + VEC2
              E_PROJ_RATE = DP_VV_V ( 3, VEC_PE_RATE, EAST_CRS ) + &
     &                      DP_VV_V ( 3, VEC_PROJ_EN, EAST_CRS_RATE )
         END IF
!
! ------ From these two projections we get the azimuth. Ugh!
!
         IF ( VTD%STA(ISTA(J3))%STA_TYP == VTD__GR ) THEN
              VTD%STA(ISTA(J3))%AZ = ATAN_CS ( N_PROJ, E_PROJ )
            ELSE 
              VTD%STA(ISTA(J3))%AZ = 0.0D0
         END IF
         IF ( VTD%CONF%FL_RATE ) THEN
              IF ( VTD%STA(ISTA(J3))%STA_TYP == VTD__GR ) THEN
                   VTD%STA(ISTA(J3))%AZ_DER  = ( E_PROJ_RATE*N_PROJ - &
     &                                           E_PROJ*N_PROJ_RATE   )/ &
     &                                         ( E_PROJ**2 + N_PROJ**2 )
                 ELSE
!
! ---------------- This may happen of the station is "GEOCENTR"
!
                   VTD%STA(ISTA(J3))%AZ_DER  = 0.0D0
             END IF
         END IF
!
! ------ Compute troposperic path delay
!
         IF ( VTD%STA(ISTA(J3))%STA_TYP == VTD__GR ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL VTD_TROPDEL ( VTD, ISTA(J3),                     &
     &                           VTD%STA(ISTA(J3))%TROP_HZD,        &
     &                           VTD%STA(ISTA(J3))%TROP_WZD,        &
     &                           VTD%STA(ISTA(J3))%TROP_DEL,        &
     &                           VTD%STA(ISTA(J3))%TROP_DEL_RATE,   &
     &                           VTD%STA(ISTA(J3))%TROP_ZEN_DER,    &
     &                           VTD%STA(ISTA(J3))%TROP_ZEN_RATE,   &
     &                           VTD%STA(ISTA(J3))%TROP_TILT_N_DER, &
     &                           VTD%STA(ISTA(J3))%TROP_TILT_E_DER, &
     &                           VTD%STA(ISTA(J3))%SUR_PRS, &
     &                           VTD%STA(ISTA(J3))%SUR_PWP, & 
     &                           VTD%STA(ISTA(J3))%SUR_TEM, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2222, IUER, 'VTD_DELAY', 'Error in an '// &
     &             'attempt to compute tropospheric delay at station '// &
     &              VTD%STA(ISTA(J3))%IVS_NAME )
                   RETURN
              END IF
            ELSE
              VTD%STA(ISTA(J3))%TROP_HZD        = 0.0D0
              VTD%STA(ISTA(J3))%TROP_WZD        = 0.0D0
              VTD%STA(ISTA(J3))%TROP_DEL        = 0.0D0
              VTD%STA(ISTA(J3))%TROP_DEL_RATE   = 0.0D0
              VTD%STA(ISTA(J3))%TROP_ZEN_DER    = 0.0D0
              VTD%STA(ISTA(J3))%TROP_ZEN_RATE   = 0.0D0
              VTD%STA(ISTA(J3))%TROP_TILT_N_DER = 0.0D0
              VTD%STA(ISTA(J3))%TROP_TILT_E_DER = 0.0D0
              VTD%STA(ISTA(J3))%SUR_TEM         = 0.0D0
              VTD%STA(ISTA(J3))%SUR_PWP         = 0.0D0
              VTD%STA(ISTA(J3))%SUR_PRS         = 0.0D0
         END IF
         IF ( ( VTD%CONF%IONO_MODEL == VIO__GNSS_TEC     .OR. &
     &          VTD%CONF%IONO_MODEL == VIO__GNSS_TEC_MOD .OR. &
     &          VTD%CONF%IONO_MODEL == VIO__GNSS_TEC_MHI      ) .AND. &
     &          VTD%STA(ISTA(J3))%STA_TYP == VTD__GR                  ) THEN
!
              CALL ERR_PASS ( IUER, IER )
              CALL VTD_IONO_DELAY ( VTD, OBS_TYP, ISTA(J3), ISOU, &
     &                              VTD%STA(ISTA(J3))%TEC, &
     &                              VTD%STA(ISTA(J3))%IONO_MAP, &
     &                              VTD%STA(ISTA(J3))%IONO_DEL, &
     &                              VTD%STA(ISTA(J3))%IONO_RATE,  &
     &                              IONO_LAT_GDT_PP(ISTA(J3)), &
     &                              IONO_LONG_PP(ISTA(J3)), &
     &                              VTD%STA(ISTA(J3))%IONO_STATUS,  IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2223, IUER, 'VTD_DELAY', 'Error in '// &
     &                 'an attempt to compute the contribution of the '// &
     &                 'ionosphere on VLBI time delay' )
                   RETURN
              END IF
            ELSE
              VTD%STA(ISTA(J3))%TEC       = 0.0D0
              VTD%STA(ISTA(J3))%IONO_MAP  = 0.0D0
              VTD%STA(ISTA(J3))%IONO_DEL  = 0.0D0
              VTD%STA(ISTA(J3))%IONO_RATE = 0.0D0
         END IF
!
         IF ( ILEN(VTD%CONF%FINAM_AGD) > 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL VTD_ANT_GRAVITY ( VTD, ISTA(J3), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2224, IUER, 'VTD_DELAY', 'Error in an '// &
     &                 'attempt to compute antenna gravity deformation '// &
     &                 'contribution to path delay for the antenna at '// &
     &                 'the station of '//VTD%STA(ISTA(J3))%IVS_NAME )
                   RETURN
              END IF
            ELSE
              VTD%STA(ISTA(J3))%ANT_GRAVITY_DEL = 0.0D0
         END IF
!
         IF ( VTD%CONF%IVRB .GE. 1 ) THEN
              WRITE ( 6, 180 ) VTD%STA(ISTA(J3))%IVS_NAME, &
     &                     VTD%STA(ISTA(J3))%ELEV/DEG__TO__RAD, &
     &                     VTD%STA(ISTA(J3))%TROP_HZD*1.D9, &
     &                     VTD%STA(ISTA(J3))%TROP_DEL*1.D9, &
     &                     VTD%STA(ISTA(J3))%ATM_PRES, &
     &                     VTD%STA(ISTA(J3))%AIR_TEMP, &
     &                     NMF_H ( VTD%MOM%MJD, VTD%MOM%TAI, &
     &                     VTD%STA(ISTA(J3))%LAT_GDT, &
     &                     VTD%STA(ISTA(J3))%HEI_ELL, &
     &                     VTD%STA(ISTA(J3))%ELEV ), &
     &                     VTD%STA(ISTA(J3))%TROP_ZEN_DER, &
     &                     VTD%STA(ISTA(J3))%AZ/DEG__TO__RAD
  180         FORMAT ( 1X, A, ' Elevation: ', F13.9,' deg ', &
     &                        ' TROP_HZD = ', F9.4,' TROP_DEL = ',F9.4/ &
     &                        '          Pres=', F8.1,' Temp=',F7.3, &
     &                        ' NMF_H=', F10.7, ' NMF_W=',F10.7/ &
     &                     9X,' Azimuth: ',F14.9, ' deg' )
         END IF
         IF ( VTD%STA(ISTA(J3))%MOUNT_TYPE .EQ. 'AZEL' .OR.  VTD%STA(ISTA(J3))%MOUNT_TYPE .EQ. 'X-YE' ) THEN
              VTD%STA(ISTA(J3))%FEED_ANG      = VTD%STA(ISTA(J3))%PARAL_ANG
              VTD%STA(ISTA(J3))%FEED_ANG_RATE = VTD%STA(ISTA(J3))%PARAL_ANG_RATE
            ELSE IF ( VTD%STA(ISTA(J3))%MOUNT_TYPE .EQ. 'NASM' .OR. &
     &                VTD%STA(ISTA(J3))%MOUNT_TYPE .EQ. 'NASL' ) THEN
              VTD%STA(ISTA(J3))%FEED_ANG      = VTD%STA(ISTA(J3))%PARAL_ANG - &
     &                                          VTD%STA(ISTA(J3))%ELEV
              VTD%STA(ISTA(J3))%FEED_ANG_RATE = VTD%STA(ISTA(J3))%PARAL_ANG_RATE - &
     &                                          VTD%STA(ISTA(J3))%ELEV_DER
            ELSE IF ( VTD%STA(ISTA(J3))%MOUNT_TYPE .EQ. 'NASP' .OR. &
     &                VTD%STA(ISTA(J3))%MOUNT_TYPE .EQ. 'NASR'      ) THEN
              VTD%STA(ISTA(J3))%FEED_ANG      = VTD%STA(ISTA(J3))%PARAL_ANG + &
     &                                          VTD%STA(ISTA(J3))%ELEV
              VTD%STA(ISTA(J3))%FEED_ANG_RATE = VTD%STA(ISTA(J3))%PARAL_ANG_RATE + &
     &                                          VTD%STA(ISTA(J3))%ELEV_DER
            ELSE IF ( VTD%STA(ISTA(J3))%MOUNT_TYPE .EQ. 'EQUA' ) THEN
              VTD%STA(ISTA(J3))%FEED_ANG      = 0.0D0
              VTD%STA(ISTA(J3))%FEED_ANG_RATE = 0.0D0
            ELSE IF ( VTD%STA(ISTA(J3))%MOUNT_TYPE .EQ. 'X-YN' ) THEN
              VTD%STA(ISTA(J3))%FEED_ANG      = 0.0D0
              VTD%STA(ISTA(J3))%FEED_ANG_RATE = 0.0D0
            ELSE IF ( .FALSE. ) THEN !VTD%STA(ISTA(J3))%MOUNT_TYPE .EQ. 'X-YE' ) THEN
!
! ----------- East angle in local topocentric coordinate system
!
              EAST_UEN(1) = 0.0D0
              EAST_UEN(2) = 1.0D0
              EAST_UEN(3) = 0.0D0
              CALL MUL_MV_IV_V ( 3, 3, VTD%STA(ISTA(J3))%UEN_TO_TRS, 3, EAST_UEN, &
     &                           3, EAST_TRS, IER )
              CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, 3, EAST_TRS, &
     &                           3, EAST_CRS, IER )
!
! ----------- COS_SO_EA -- cosine of the angle between the apparent source position
! -----------              and the east direction. 
!
              COS_SO_EA = DP_VV_V ( 3, EAST_CRS, VTD%SOU(ISOU)%SAPP_CRS )
!
! ----------- cos(Feed_ang) = -Tan(delta)/Tan(a), where a is arccos of COS_SO_EA
!
              FEED_COS = -VTD%SOU(ISOU)%SAPP_CRS(3)/ &
     &                    DSQRT( VTD%SOU(ISOU)%SAPP_CRS(1)**2 + VTD%SOU(ISOU)%SAPP_CRS(2)**2 + EPS**2 )* &
     &                    COS_SO_EA/DSQRT(1.0D0 - COS_SO_EA**2) 
              IF ( FEED_COS >  1.0D0 ) FEED_COS =  1.0D0
              IF ( FEED_COS < -1.0D0 ) FEED_COS = -1.0D0
!
! ----------- (Tested on S-band 20110414_a)
!
              VTD%STA(ISTA(J3))%FEED_ANG =  DACOS ( FEED_COS )
              VTD%STA(ISTA(J3))%FEED_ANG_RATE = 0.0D0
            ELSE IF ( VTD%STA(ISTA(J3))%MOUNT_TYPE .EQ. 'BWG ' ) THEN
              VTD%STA(ISTA(J3))%FEED_ANG      = VTD%STA(ISTA(J3))%PARAL_ANG - VTD%STA(ISTA(J3))%AZ
              VTD%STA(ISTA(J3))%FEED_ANG_RATE = VTD%STA(ISTA(J3))%PARAL_ANG_RATE - VTD%STA(ISTA(J3))%AZ_DER
            ELSE 
              VTD%STA(ISTA(J3))%FEED_ANG      = 0.0D0
              VTD%STA(ISTA(J3))%FEED_ANG_RATE = 0.0D0
         END IF
!
! ------ Compute coupling between troposphere path delay and antenna's
! ------ axis offset
!
         IF ( VTD%CONF%TROP_AXOF_COUPL .EQ. VTD__YES ) THEN
              CALL VTD_TROP_AXOF_COUPL ( AXOF_UP(J3), AXOF_UP_RATE(J3),     &
     &                                   ( VTD%STA(ISTA(J3))%TROP_HZD +     &
     &                                     VTD%STA(ISTA(J3))%TROP_WZD ),    &
     &                                     VTD%STA(ISTA(J3))%TROP_AXOF_TAU, &
     &                                     VTD%STA(ISTA(J3))%TROP_AXOF_RAT  )
         END IF
         FL_STA(J3) = .TRUE.
 430  CONTINUE ! over stations: the first and the second
!
! --- Add a pseudo station "geocenter"
!
      VTD%STA(VTD%L_STA+1)%COO_CRS = 0.0D0
      VTD%STA(VTD%L_STA+1)%VEL_CRS = 0.0D0
      VTD%STA(VTD%L_STA+1)%ACC_CRS = 0.0D0
      VTD%STA(VTD%L_STA+1)%MOM_COO_TRS = 0.0D0
      VTD%STA(VTD%L_STA+1)%STA_TYP = VTD__GC
!
! --- Get partial derivaties
!
      DER_DEL(VTD__DER_AT1)  = -VTD%STA(ISTA(1))%TROP_ZEN_DER
      DER_DEL(VTD__DER_AT2)  =  VTD%STA(ISTA(2))%TROP_ZEN_DER
      DER_DEL(VTD__DER_ATN1) = -VTD%STA(ISTA(1))%TROP_TILT_N_DER
      DER_DEL(VTD__DER_ATE1) = -VTD%STA(ISTA(1))%TROP_TILT_E_DER
      DER_DEL(VTD__DER_ATN2) =  VTD%STA(ISTA(2))%TROP_TILT_N_DER
      DER_DEL(VTD__DER_ATE2) =  VTD%STA(ISTA(2))%TROP_TILT_E_DER
!
      DER_DEL(VTD__DER_AXF1) =  VTD%STA(ISTA(1))%AXIS_DELAY_DER
      DER_DEL(VTD__DER_AXF2) = -VTD%STA(ISTA(2))%AXIS_DELAY_DER
!
      DER_RAT(VTD__DER_AT1)  = -VTD%STA(ISTA(1))%TROP_ZEN_RATE
      DER_RAT(VTD__DER_AT2)  =  VTD%STA(ISTA(2))%TROP_ZEN_RATE
!
! --- Compute geometrical path delay from a point-like source
!
      ACLR_GEOM = 0.0D0
      CALL ERR_PASS ( IUER, IER )
      IF ( ( VTD%SOU(ISOU)%OBJ_TYPE == VTD__MG  .OR. &
     &       VTD%SOU(ISOU)%OBJ_TYPE == VTD__GAL      ) .AND. &
     &     VTD%CONF%GEOM_EXPR_FAR_ZONE .EQ. VTD__KS1999            ) THEN
           CALL VTD_KS1999 ( VTD, ISTA(1), ISTA(2), ISOU, &
     &                       VTD%MOM%TRS_TO_CRS, VTD%MOM%DTRS_TO_CRS_DEOP, &
     &                       TAU_GEOM, RATE_GEOM, ACLR_GEOM, &
     &                       DER_DEL(VTD__DER_EOP(1)),  &
     &                       DER_DEL(VTD__DER_STA1(1)), &
     &                       DER_DEL(VTD__DER_STA2(1)), &
     &                       DER_DEL(VTD__DER_RA),      &
     &                       DER_DEL(VTD__DER_DL), IER )
         ELSE IF ( ( VTD%SOU(ISOU)%OBJ_TYPE == VTD__MG  .OR. &
     &               VTD%SOU(ISOU)%OBJ_TYPE == VTD__GAL      ) .AND. &
     &             VTD%CONF%GEOM_EXPR_FAR_ZONE .EQ. VTD__PK2001          ) THEN
           CALL VTD_PK2001 ( VTD, ISTA(1), ISTA(2), ISOU, &
     &                       VTD%MOM%TRS_TO_CRS, &
     &                       VTD%MOM%TRS_TO_CRS_DER1, &
     &                       VTD%MOM%DTRS_TO_CRS_DEOP, &
     &                       VTD%MOM%DTRS_TO_CRS_DER1_DEOP, &
     &                       VTD%MOM%NZO_RLT, &
     &                       TAU_GEOM, RATE_GEOM, ACLR_GEOM, &
     &                       DER_DEL(VTD__DER_EOP(1)),  &
     &                       DER_DEL(VTD__DER_STA1(1)), &
     &                       DER_DEL(VTD__DER_STA2(1)), &
     &                       DER_DEL(VTD__DER_RA),      &
     &                       DER_DEL(VTD__DER_DL),      &
     &                       DER_DEL(VTD__DER_GAMMA),   &
     &                       DER_RAT(VTD__DER_EOP(1)),  &
     &                       DER_RAT(VTD__DER_STA1(1)), &
     &                       DER_RAT(VTD__DER_STA2(1)), &
     &                       DER_RAT(VTD__DER_RA),      &
     &                       DER_RAT(VTD__DER_DL), IER )
!
! -------- Compute path delay between the geocenter and the 1st station at the moment of time 
! -------- at the geocenter
!
           CALL VTD_PK2001 ( VTD, VTD%L_STA+1, ISTA(1), ISOU, &
     &                       VTD%MOM%TRS_TO_CRS, &
     &                       VTD%MOM%TRS_TO_CRS_DER1, &
     &                       VTD%MOM%DTRS_TO_CRS_DEOP, &
     &                       VTD%MOM%DTRS_TO_CRS_DER1_DEOP, &
     &                       VTD%MOM%NZO_RLT, &
     &                       DER_DEL(VTD__DEL_GCN1), DER_RAT(VTD__DEL_GCN1),    &
     &                       TMP(1), TMP(2), TMP(3), TMP(4),  TMP(5),  TMP(6),  &
     &                       TMP(7), TMP(8), TMP(9), TMP(10), TMP(11), TMP(12), &
     &                       IER )
!
! -------- Compute path delay between the geocenter and the 1st station at the moment of time 
! -------- at the geocenter
!
           CALL VTD_PK2001 ( VTD, VTD%L_STA+1, ISTA(2), ISOU, &
     &                       VTD%MOM%TRS_TO_CRS, &
     &                       VTD%MOM%TRS_TO_CRS_DER1, &
     &                       VTD%MOM%DTRS_TO_CRS_DEOP, &
     &                       VTD%MOM%DTRS_TO_CRS_DER1_DEOP, &
     &                       VTD%MOM%NZO_RLT, &
     &                       DER_DEL(VTD__DEL_GCN2), DER_RAT(VTD__DEL_GCN2),    &
     &                       TMP(1), TMP(2), TMP(3), TMP(4),  TMP(5),  TMP(6),  &
     &                       TMP(7), TMP(8), TMP(9), TMP(10), TMP(11), TMP(12), &
     &                       IER )
         ELSE IF ( ( VTD%SOU(ISOU)%OBJ_TYPE == VTD__SS .OR. &
     &               VTD%SOU(ISOU)%OBJ_TYPE == VTD__ES      ) .AND. &
     &               VTD%CONF%GEOM_EXPR_NEAR_ZONE .EQ. VTD__LT ) THEN
           CALL VTD_NZO_LT ( VTD, ISTA(1), ISTA(2), ISOU, &
     &                       TAU_GEOM, RATE_GEOM,         &
     &                       DER_DEL(VTD__DER_EOP(1)),    &
     &                       DER_DEL(VTD__DER_STA1(1)),   &
     &                       DER_DEL(VTD__DER_STA2(1)),   &
     &                       DER_DEL(VTD__DER_POS),       &
     &                       DER_DEL(VTD__DER_VEL),       &
     &                       DER_RAT(VTD__DER_EOP(1)),    &
     &                       DER_RAT(VTD__DER_STA1(1)),   &
     &                       DER_RAT(VTD__DER_STA2(1)),   &
     &                       DER_RAT(VTD__DER_POS),       &
     &                       DER_RAT(VTD__DER_VEL),       &
     &                       IER )
         ELSE IF ( ( VTD%SOU(ISOU)%OBJ_TYPE == VTD__SS .OR. &
     &               VTD%SOU(ISOU)%OBJ_TYPE == VTD__ES      ) .AND. &
     &               VTD%CONF%GEOM_EXPR_NEAR_ZONE .EQ. VTD__JN2019 ) THEN
           CALL VTD_JN2019 ( VTD, ISTA(1), ISTA(2), ISOU, &
     &                       OBS_TYP, TAU_GEOM, RATE_GEOM,&
     &                       DER_DEL(VTD__DER_EOP(1)),    &
     &                       DER_DEL(VTD__DER_STA1(1)),   &
     &                       DER_DEL(VTD__DER_STA2(1)),   &
     &                       DER_DEL(VTD__DER_POS),       &
     &                       DER_DEL(VTD__DER_VEL),       &
     &                       DER_RAT(VTD__DER_EOP(1)),    &
     &                       DER_RAT(VTD__DER_STA1(1)),   &
     &                       DER_RAT(VTD__DER_STA2(1)),   &
     &                       DER_RAT(VTD__DER_POS),       &
     &                       DER_RAT(VTD__DER_VEL),       &
     &                       IER )
           DER_DEL(VTD__DER_RA) = 0.0D0
           DER_DEL(VTD__DER_DL) = 0.0D0
           DER_RAT(VTD__DER_RA) = 0.0D0
           DER_RAT(VTD__DER_DL) = 0.0D0
         ELSE
           CALL ERR_LOG ( 2225, IUER, 'VTD_DELAY', 'Error in configuration: '// &
     &         'no algorithm was selected. Please check fields '// &
     &         'GEOM_EXPR_FAR_ZONE and GEOM_EXPR_NEAR_ZONE' )
           RETURN
      END IF
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2226, IUER, 'VTD_DELAY', 'Error in an attempt '// &
     &         'to compute geometrical time delay and their derivatives' )
           RETURN
      END IF
      DER_DEL(VTD__ACCLR) = ACLR_GEOM
!
! --- Compute coupling between the tropospheric path delay and
! --- the geometric delay
!
      IF ( VTD%CONF%TROP_GEOM_COUPL .EQ. VTD__YES .AND. &
           VTD%STA(ISTA(2))%STA_TYP == VTD__GR          ) THEN
!
           CALL VTD_TROP_GEOM_COUPL ( VTD, ISOU, &
     &          VTD%STA(ISTA(2))%TROP_DEL     -VTD%STA(ISTA(1))%TROP_DEL,      &
     &          VTD%STA(ISTA(2))%TROP_DEL_RATE-VTD%STA(ISTA(1))%TROP_DEL_RATE, &
     &          TROP_GEOM_TAU, TROP_GEOM_RATE  )
         ELSE
           TROP_GEOM_TAU  = 0.0D0
           TROP_GEOM_RATE = 0.0D0
      END IF
!
! --- Computed contribution caused by source structure
!
      IF ( ILEN(VTD%CONF%FINAM_STRUC) > 0 ) THEN
           CALL ERR_PASS  ( IUER, IER )
           CALL VTD_STRUC ( ISTA(1), ISTA(2), ISOU, VTD, OBS_TYP, &
     &                      TAU_STRUC, RATE_STRUC, CORR_FD, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2227, IUER, 'VTD_DELAY', 'Error in an '// &
     &              'attempt to compute contribution to delay and delay '// &
     &              'rate caused by source structure' )
                RETURN
           END IF
         ELSE
!
! -------- We need to compute UV_COOR -- projection of the baseline vector
! -------- on the plane normal to the source. We need to do it even if
! -------- we are not going to use source maps in VTD. This information may
! -------- be need to VTD users who may manipulate with maps outside VTD
!
! -------- Vector to the northen pole of the celestiral coordinate system
!
           Z_VEC(1) =  VTD%MOM%YPL
           Z_VEC(2) = -VTD%MOM%XPL
           Z_VEC(3) =  1.0D0
!
           CALL VM83 ( VTD%SOU(ISOU)%SAPP_CRS, Z_VEC, X_VEC )
           CALL NORM_VEC ( 3, X_VEC, RD )
!
           CALL VM83 ( VTD%SOU(ISOU)%SAPP_CRS, X_VEC, Y_VEC )
           CALL NORM_VEC ( 3, Y_VEC, RD )
!
! -------- Compute the baseline vector
!
           CALL SUB_VV_V ( 3, VTD%STA(ISTA(2))%COO_CRS, &
     &                        VTD%STA(ISTA(1))%COO_CRS, &
     &                        B_CRS )
!
! -------- Compute projection of the baseline vector to the image plane
!
           VTD%UV_COOR(1) =  DP_VV_V ( 3, X_VEC,                  B_CRS )
           VTD%UV_COOR(2) =  DP_VV_V ( 3, Y_VEC,                  B_CRS )
           VTD%UV_COOR(3) = -DP_VV_V ( 3, VTD%SOU(ISOU)%SAPP_CRS, B_CRS )
!
           TAU_STRUC  = 0.0D0
           RATE_STRUC = 0.0D0
           CORR_FD    = 0.0D0
      END IF
      DER_DEL(VTD__STRUC) = TAU_STRUC
      DER_DEL(VTD__UVX)   = VTD%UV_COOR(1)
      DER_DEL(VTD__UVY)   = VTD%UV_COOR(2)
      DER_DEL(VTD__UVW)   = VTD%UV_COOR(3)
      IF ( VTD%CONF%TEST(1) == 1 ) THEN
!
! -------- Special case for tests
!
           TAU_STRUC = 0.D0
      END IF
      IF ( VTD%CONF%GAL_ABR == VTD__YES ) THEN
           CALL ERR_PASS (  IUER, IER )
           CALL VTD_GAL_ABR ( VTD, ISTA(1), ISTA(2), ISOU, &
     &                        TAU_GALABR, RATE_GALABR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2228, IUER, 'VTD_DELAY', 'Error in an '// &
     &              'attempt to compute contribution to delay and delay '// &
     &              'rate caused by galactic aberration' )
                RETURN
           END IF
         ELSE
!
! -------- Special case for tests
!
           TAU_GALABR  = 0.D0
           RATE_GALABR = 0.D0
      END IF
!
      CALL ERR_PASS (  IUER, IER )
      CALL VTD_SOU_DEBIAS ( VTD, OBS_TYP, ISOU, DER_DEL, DER_RAT, &
     &                      DELAY_BIAS, RATE_BIAS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2229, IUER, 'VTD_DELAY', 'Error in computation '// &
     &         'of the source position bias' )
           RETURN
      END IF
!
! --- Summ the terms
!
      DELAY = TAU_GEOM    &
     &          - VTD%STA(ISTA(1))%TROP_DEL &
     &          + VTD%STA(ISTA(2))%TROP_DEL &
     &          - VTD%STA(ISTA(1))%IONO_DEL &
     &          + VTD%STA(ISTA(2))%IONO_DEL &
     &          - VTD%STA(ISTA(1))%TROP_AXOF_TAU &
     &          + VTD%STA(ISTA(2))%TROP_AXOF_TAU &
     &          - VTD%STA(ISTA(1))%ANT_GRAVITY_DEL &
     &          + VTD%STA(ISTA(2))%ANT_GRAVITY_DEL &
     &          - VTD%STA(ISTA(1))%ANT_THERMAL_DEL &
     &          + VTD%STA(ISTA(2))%ANT_THERMAL_DEL &
     &          + TROP_GEOM_TAU &
     &          + TAU_STRUC  &
     &          + TAU_GALABR &
     &          + DELAY_BIAS
!
      RATE  = RATE_GEOM &
     &          + TROP_GEOM_RATE &
     &          - VTD%STA(ISTA(1))%TROP_DEL_RATE &
     &          + VTD%STA(ISTA(2))%TROP_DEL_RATE &
     &          - VTD%STA(ISTA(1))%TROP_AXOF_RAT &
     &          + VTD%STA(ISTA(2))%TROP_AXOF_RAT &
     &          - VTD%STA(ISTA(1))%IONO_RATE &
     &          + VTD%STA(ISTA(2))%IONO_RATE &
     &          + RATE_STRUC  &
     &          + RATE_GALABR &
     &          + RATE_BIAS
!
      IF ( VTD%CONF%AXOF_MODEL == VTD__CALC ) THEN
           DELAY = DELAY &
     &              - VTD%STA(ISTA(1))%DELAY_AXOF_VTD  &
     &              + VTD%STA(ISTA(2))%DELAY_AXOF_VTD  &
     &              + VTD%STA(ISTA(1))%DELAY_AXOF_CALC &
     &              - VTD%STA(ISTA(2))%DELAY_AXOF_CALC
      END IF
!
      DER_DEL(VTD__ELEV1) = VTD%STA(ISTA(1))%ELEV
      DER_DEL(VTD__ELEV2) = VTD%STA(ISTA(2))%ELEV
      DER_DEL(VTD__AZIM1) = VTD%STA(ISTA(1))%AZ
      DER_DEL(VTD__AZIM2) = VTD%STA(ISTA(2))%AZ
      DER_DEL(VTD__TROP1) = VTD%STA(ISTA(1))%TROP_DEL
      DER_DEL(VTD__TROP2) = VTD%STA(ISTA(2))%TROP_DEL
      DER_DEL(VTD__IONO1) = VTD%STA(ISTA(1))%IONO_DEL
      DER_DEL(VTD__IONO2) = VTD%STA(ISTA(2))%IONO_DEL
      DER_DEL(VTD__DER_IONO1) = VTD%STA(ISTA(1))%IONO_MAP
      DER_DEL(VTD__DER_IONO2) = VTD%STA(ISTA(2))%IONO_MAP
      DER_DEL(VTD__TRP_HZD1) = VTD%STA(ISTA(1))%TROP_HZD - VTD%STA(ISTA(1))%TROP_WZD
      DER_DEL(VTD__TRP_HZD2) = VTD%STA(ISTA(2))%TROP_HZD - VTD%STA(ISTA(2))%TROP_WZD
      DER_DEL(VTD__TRP_WZD1) = VTD%STA(ISTA(1))%TROP_WZD
      DER_DEL(VTD__TRP_WZD2) = VTD%STA(ISTA(2))%TROP_WZD
      DER_DEL(VTD__PARAL1)   = VTD%STA(ISTA(1))%PARAL_ANG
      DER_DEL(VTD__PARAL2)   = VTD%STA(ISTA(2))%PARAL_ANG
      DER_DEL(VTD__STRUC)    = TAU_STRUC
      DER_RAT(VTD__STRUC)    = RATE_STRUC
      DER_DEL(VTD__FEED1)    = VTD%STA(ISTA(1))%FEED_ANG
      DER_DEL(VTD__FEED2)    = VTD%STA(ISTA(2))%FEED_ANG
      DER_DEL(VTD__SUR_PRS1) = VTD%STA(ISTA(1))%SUR_PRS
      DER_DEL(VTD__SUR_PRS2) = VTD%STA(ISTA(2))%SUR_PRS
      DER_DEL(VTD__SUR_PWP1) = VTD%STA(ISTA(1))%SUR_PWP
      DER_DEL(VTD__SUR_PWP2) = VTD%STA(ISTA(2))%SUR_PWP
      DER_DEL(VTD__SUR_TEM1) = VTD%STA(ISTA(1))%SUR_TEM
      DER_DEL(VTD__SUR_TEM2) = VTD%STA(ISTA(2))%SUR_TEM
      DER_DEL(VTD__COR_FD1)  = CORR_FD(1)
      DER_DEL(VTD__COR_FD2)  = CORR_FD(2)
!
      IF ( VTD%CONF%PARALLACTIC_ANGLE == VTD__YES ) THEN
!
! -------- Apply parallactic angle reduction. NB: care about signs!!
! -------- They are different for LCP and RCP.
! -------- We do it only of reference freqwuency is sane
!
           IF ( OBS_TYP%DELAY_TYPE == VTD__PL__DTP   .OR. &
     &          OBS_TYP%DELAY_TYPE == VTD__PH__DTP   .OR. &
     &          OBS_TYP%DELAY_TYPE == VTD__PLML__DTP .OR. &
     &          OBS_TYP%DELAY_TYPE == VTD__PHML__DTP      ) THEN
!
                IF ( OBS_TYP%FRQ_REF(1) > VTD__FREQ_MIN .AND. &
     &               OBS_TYP%FRQ_REF(1) < VTD__FREQ_MAX      ) THEN
                     IF ( OBS_TYP%PLRZ(1:1) == 'L' ) THEN
                          DELAY = DELAY + VTD%STA(ISTA(1))%FEED_ANG/(PI2*OBS_TYP%FRQ_REF(1))
                          RATE  =  RATE + VTD%STA(ISTA(1))%FEED_ANG_RATE/(PI2*OBS_TYP%FRQ_REF(1))
                        ELSE IF ( OBS_TYP%PLRZ(1:1) == 'R' ) THEN
                          DELAY = DELAY - VTD%STA(ISTA(1))%FEED_ANG/(PI2*OBS_TYP%FRQ_REF(1))
                          RATE  = RATE  - VTD%STA(ISTA(1))%FEED_ANG_RATE/(PI2*OBS_TYP%FRQ_REF(1))
                     END IF
                     IF ( OBS_TYP%PLRZ(2:2) == 'L' ) THEN
                          DELAY = DELAY - VTD%STA(ISTA(2))%FEED_ANG/(PI2*OBS_TYP%FRQ_REF(1))
                          RATE  = RATE  - VTD%STA(ISTA(2))%FEED_ANG_RATE/(PI2*OBS_TYP%FRQ_REF(1))
                        ELSE IF ( OBS_TYP%PLRZ(2:2) == 'R' ) THEN
                          DELAY = DELAY + VTD%STA(ISTA(2))%FEED_ANG/(PI2*OBS_TYP%FRQ_REF(1))
                          RATE  = RATE  + VTD%STA(ISTA(2))%FEED_ANG_RATE/(PI2*OBS_TYP%FRQ_REF(1))
                     END IF
                END IF
              ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PHML__DTP .OR. &
     &                  OBS_TYP%DELAY_TYPE == VTD__PHMH__DTP      ) THEN
!
                IF ( OBS_TYP%FRQ_REF(2) > VTD__FREQ_MIN .AND. &
     &               OBS_TYP%FRQ_REF(2) < VTD__FREQ_MAX      ) THEN
                     IF ( OBS_TYP%PLRZ(1:1) == 'L' ) THEN
                          DELAY = DELAY + VTD%STA(ISTA(1))%FEED_ANG/(PI2*OBS_TYP%FRQ_REF(2))
                          RATE  =  RATE + VTD%STA(ISTA(1))%FEED_ANG_RATE/(PI2*OBS_TYP%FRQ_REF(2))
                        ELSE IF ( OBS_TYP%PLRZ(1:1) == 'R' ) THEN
                          DELAY = DELAY - VTD%STA(ISTA(1))%FEED_ANG/(PI2*OBS_TYP%FRQ_REF(2))
                          RATE  = RATE  - VTD%STA(ISTA(1))%FEED_ANG_RATE/(PI2*OBS_TYP%FRQ_REF(2))
                     END IF
                     IF ( OBS_TYP%PLRZ(2:2) == 'L' ) THEN
                          DELAY = DELAY - VTD%STA(ISTA(2))%FEED_ANG/(PI2*OBS_TYP%FRQ_REF(2))
                          RATE  = RATE  - VTD%STA(ISTA(2))%FEED_ANG_RATE/(PI2*OBS_TYP%FRQ_REF(2))
                        ELSE IF ( OBS_TYP%PLRZ(2:2) == 'R' ) THEN
                          DELAY = DELAY + VTD%STA(ISTA(2))%FEED_ANG/(PI2*OBS_TYP%FRQ_REF(2))
                          RATE  = RATE  + VTD%STA(ISTA(2))%FEED_ANG_RATE/(PI2*OBS_TYP%FRQ_REF(2))
                     END IF
                END IF
           END IF
      END IF
!
      IF ( VTD%CONF%IVRB .GE. 2 ) THEN
           SV  = DP_VV_V ( 3, VTD%SOU(ISOU)%S_CRS, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART))
           SV2 = DP_VV_V ( 3, VTD%SOU(ISOU)%S_CRS, VTD%STA(ISTA(2))%VEL_CRS )
           SB1 = DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                        VTD%STA(ISTA(1))%MOM_COO_TRS )
           SB2 = DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                        VTD%STA(ISTA(2))%MOM_COO_TRS )
!
           IF ( FL_STA(1)  .AND.  FL_STA(2) ) THEN
                VEC1(1) = PTD_TRS(1,1) - PTD_TRS(1,2)
                VEC1(2) = PTD_TRS(2,1) - PTD_TRS(2,2)
                VEC1(3) = PTD_TRS(3,1) - PTD_TRS(3,2)
                CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, 3, VEC1, &
     &                             3, VEC2, IER )
                DT  = DP_VV_V ( 3, VTD%SOU(ISOU)%S_CRS, VEC2 )/VTD__C/ &
     &               (1.0D0 + (SV+SV2)/VTD__C)
                WRITE ( 6, * ) 'Contribution due to pole tide:    DT = ',DT
           END IF
!
           IF ( VTD%CONF%AXOF_MODEL .EQ. VTD__YES  ) THEN
                IF ( FL_STA(1)  .AND.  FL_STA(2) ) THEN
                     VEC1(1) = AXOF_CRS(1,1) - AXOF_CRS(1,2)
                     VEC1(2) = AXOF_CRS(2,1) - AXOF_CRS(2,2)
                     VEC1(3) = AXOF_CRS(3,1) - AXOF_CRS(3,2)
                     DT  = DP_VV_V ( 3, VTD%SOU(ISOU)%S_CRS, VEC1 )/VTD__C/ &
     &                             (1.0D0 + (SV+SV2)/VTD__C)
                     WRITE ( 6, 220 ) 'Contribution Axof vtd  ', DT
                END IF
              ELSE
                WRITE ( 6, 220 ) 'Contribution Axof Calc ', &
     &                         VTD%STA(ISTA(1))%DELAY_AXOF_CALC - &
     &                         VTD%STA(ISTA(2))%DELAY_AXOF_CALC
 220            FORMAT ( 1X,A,1X, 1PD23.15 )
           END IF
!
           WRITE ( 6, * ) 'Contribution of trop_geom_coupl: =', TROP_GEOM_TAU
           WRITE ( 6, * ) 'Contribution of trop_axof_coupl: =', &
     &                     VTD%STA(ISTA(2))%TROP_AXOF_TAU - &
     &                     VTD%STA(ISTA(1))%TROP_AXOF_TAU
!
           WRITE ( 6, * ) 'TAU_GEOM = ',TAU_GEOM
           WRITE ( 6, * ) 'TAU_TROP_DEL = ', VTD%STA(ISTA(1))%TROP_DEL, &
     &                                       VTD%STA(ISTA(2))%TROP_DEL
           WRITE ( 6, * ) 'IONO_DEL = ', DER_DEL(VTD__IONO1), DER_DEL(VTD__IONO2)
           WRITE ( 6, * ) 'DELAY    = ', DELAY
      END IF
      IF ( SOLVE_DEBUG == 'GET_IONO' ) THEN
!
! -------- Support of a kludge environment variable. We output
! -------- geodetic latitdude and lonogitude of the ionosphere 
! -------- piersing point and occupy slots reerved for surface pressure
!
           DER_DEL(VTD__SUR_PRS1) = IONO_LAT_GDT_PP(ISTA(1))
           DER_DEL(VTD__SUR_PRS2) = IONO_LAT_GDT_PP(ISTA(2))
           DER_DEL(VTD__SUR_PWP1) = IONO_LONG_PP(ISTA(1))
           DER_DEL(VTD__SUR_PWP2) = IONO_LONG_PP(ISTA(2))
      END IF      
      IF ( VTD%CONF%IVRB .GE. 5 ) THEN
           CALL PAUSE ( 'VTD_DELAY' )
      END IF
      CALL GETENVAR ( 'VTD_GRADI_TIEDS', STR )
      IF ( STR == 'YES' ) THEN
           DER_DEL(51) = STD_REN(1,1)
           DER_DEL(52) = STD_REN(2,1)
           DER_DEL(53) = STD_REN(3,1)
           DER_DEL(54) = VTD%STA(ISTA(1))%HEI_ELL
           DER_DEL(55) = VTD%STA(ISTA(1))%GAC_ELL
           DER_DEL(56) = VTD%STA(ISTA(1))%RAD
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_DELAY  !#!#
