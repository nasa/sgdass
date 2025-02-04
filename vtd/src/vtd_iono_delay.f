      SUBROUTINE VTD_IONO_DELAY ( VTD, OBS_TYP, ISTA, ISOU, TEC, IONO_MAP, &
     &                            IONO_DEL, IONO_RATE, LAT_GDT_PP, LONG_PP, &
     &                            IONO_STATUS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_IONO_DELAY  computes the contribution of the          *
! *   ionosphere to path delay for the station ISTA that observes        *
! *   the source ISOU using the total global elctron contents maps.      *
! *   It is assumed that the ionosphere TEC maps were read from input    *
! *   files, parsed, and the coefficients ofthe 3D spline that           *
! *   interpolates them has been already computed.                       *
! *                                                                      *
! *   The ionsosphere path delay depends on the type of observables:     *
! *   group delays or phase delays. It also depends on the effective     *
! *   ionosphere frequency. The information about the type of            *
! *   observables and the frequnecies is in OBS_TYP object.              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
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
! *    If no information about frequencies and polarization is           *
! *    available, set OBS_TYP%STATUS to VTD__UNDF. Then contents         *
! *    of  OBS_TYP will be ignored and contribution to delay caused      *
! *    by the ionosphere, source structure and parallactic anlge         *
! *    rotation will be set to zero.                                     *
! *                                                                      *
! *  ISTA ( INTEGER*4 ) -- Index of the observing station.               *
! *  ISOU ( INTEGER*4 ) -- INdex of the observed source.                 *
! *                                                                      *
! * _________________________ Ouptut parameters: _______________________ *
! *                                                                      *
! *         TEC ( REAL*8    ) -- Value of the Total Electron Contents    *
! *                              at the point of piercing the            *
! *                              ionosphere. Units: TECU.                *
! *   IONO_MAP  ( REAL*8    ) -- The ionosphere mapping function: the    *
! *                              ration of the slanted path delay in     *
! *                              the ionosphere to the vertical path     *
! *                              delay at the piercing point.            *
! *   IONO_DEL  ( REAL*8    ) -- Ionosphere path delay. Units: sec.      *
! *  IONO_RATE  ( REAL*8    ) -- Ionosphere path delay rate. Units:      *
! *                              dimensionless.                          *
! *  LAT_GDT_PP ( REAL*8    ) -- Geodetic latitude of the ionosphere     *
! *                              piercing point. Units: rad.             *
! *  LONG_PP    ( REAL*8    ) -- Longitude of the ionosphere piercing    *
! *                              point. Units: rad.                      *
! * IONO_STATUS ( INTEGER*4 ) -- Ionosphere delay status. One of         *
! *               VTD__IONO_GOOD -- Good.                                *
! *               VTD__IONO_BADF -- Returned zero, because effective     *
! *                                 frequency is out of range.           *
! *               VTD__IONO_BADT -- Returned zero, because TEC is out    *
! *                                 of range.                            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     VTD ( VTD__TYPE ) -- Object which keeps configuration and data   *
! *                          related to VLBI Theoretical Delay (VTD)     *
! *                          package.                                    *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 14-MAY-2010  VTD_IONO_DELAY  v2.4 (c) L. Petrov  27-SEP-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE      ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      INTEGER*4  ISTA, ISOU, IONO_STATUS, IUER
      REAL*8     TEC, IONO_MAP, IONO_DEL, IONO_RATE
      REAL*8     GAMMA, IONO_DIST, PP_CRS(3), PP_TRS(3), &
     &           LAT_GCN_PP, LAT_GDT_PP, LONG_PP, PP, VAL, &
     &           IONO_MAP_RATE, EFF_FRQ_SQ, TIM, FRQ__MIN
      REAL*4     AXL(3), AXR(3), TEC_3LIN
      PARAMETER  ( FRQ__MIN = 1.D7 )
      CHARACTER  STR*128, STR1*128, SOLVE_DEBUG*8
      INTEGER*4  DIMS(3), INDS(3), IER
      REAL*4     ARGS(3)
      REAL*8     IONO_SCALE, UTC_TRIAL, TAI_TRIAL, UTCMTAI
      LOGICAL*1  FL_TEST
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      REAL*4,    EXTERNAL :: VAL_3D_BSPL4
      REAL*8,    EXTERNAL :: ATAN_CS, VTD_IONO_MF, VTD_IONO_SLM_MF, VTD_IONO_600_MF
!
      FL_TEST = .FALSE.
      IF ( VTD%IONO%STATUS_SPL .NE. VIO__COMP ) THEN
           CALL ERR_LOG ( 2141, IUER, 'VTD_IONO_DELAY', 'Trap of '// &
     &         'internal control: coefficients of TEC maps interpolating '// &
     &         'spline have not been computed' )
           RETURN
      END IF
      IF ( VTD%STA(ISTA)%BEG_TRS(1) == 0.0D0 .AND. &
     &     VTD%STA(ISTA)%BEG_TRS(2) == 0.0D0 .AND. &
     &     VTD%STA(ISTA)%BEG_TRS(3) == 0.0D0       ) THEN
           IONO_DEL = 0.0D0
           IONO_RATE = 0.0D0
           IONO_STATUS = VTD__IONO_GOOD
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      CALL GETENVAR ( 'SOLVE_DEBUG', SOLVE_DEBUG )
      CALL GETENVAR ( 'VTD_IONO_SCALE', STR      )
      IF ( ILEN(STR) .NE. 0 ) THEN
           READ ( UNIT=STR, FMT=* ) IONO_SCALE
           WRITE ( 6, * ) 'VTD_IONO_DELAY: use IONO_SCALE ', IONO_SCALE
         ELSE
           IONO_SCALE = VTD%CONF%IONOSPHERE_SCALE
      END IF
!
! --- Solve the triangle and find the distance from the station to the
! --- ionosphere pierce point. Gamma is the angle at the ionosphere
! --- piercing point between the station and the geocenter
!
!      gamma = dasin ( dcos(vtd%sta(ista)%elev)/ &
!     &                (1.0d0 + vtd%iono%header%height/vtd__rea) )
!!
      GAMMA = DASIN ( VTD%STA(ISTA)%RAD/(VTD__RMN + VTD%IONO%HEADER%HEIGHT)* &
     &                ( DCOS(VTD%STA(ISTA)%ELEV) - (VTD%STA(ISTA)%LAT_GDT - VTD%STA(ISTA)%LAT_GCN)*DCOS(VTD%STA(ISTA)%ELEV)* &
     &                  DCOS(VTD%STA(ISTA)%AZ)    ) &
     &              )
      IONO_DIST = VTD__REA* &
     &            DSQRT (  &
     &                      1.0D0 &
     &                   + (1.0D0 + VTD%IONO%HEADER%HEIGHT/VTD__REA)**2 &
     &                   - 2.D0*(1.0D0 + VTD%IONO%HEADER%HEIGHT/VTD__REA)* &
     &                                   DSIN(VTD%STA(ISTA)%ELEV + GAMMA) &
     &                  )
!
! --- Compute the geocentric vector of the ionosphere pierce point in CRS
!
      CALL ADDC_VV  ( 3, 1.0D0, VTD%STA(ISTA)%COO_CRS, IONO_DIST, &
     &                VTD%SOU(ISOU)%S_CRS, PP_CRS )
!
! --- Transform the geocentric vector of the ionosphere point from CRS to TRS
!
      CALL MUL_MV_TV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                   3, PP_CRS, 3, PP_TRS, IER )
      CALL REF_ELL ( 0, PP_TRS, LAT_GCN_PP, LAT_GDT_PP, LONG_PP, VAL, VAL, VAL )
      IF ( LONG_PP .GT. PI__NUM ) THEN
           LONG_PP = LONG_PP - PI2
         ELSE IF ( LONG_PP .LT. -PI__NUM ) THEN
           LONG_PP = LONG_PP + PI2
      END IF
!
      DIMS(1) = VTD%IONO%HEADER%NLON
      DIMS(2) = VTD%IONO%HEADER%NLAT
      DIMS(3) = VTD%IONO%HEADER%NEPC
!
      ARGS(1) = LONG_PP
      INDS(1) = INT ( (LONG_PP - VTD%IONO%HEADER%LON_MIN)/VTD%IONO%HEADER%LON_STEP ) + 1
!
! --- Correct possible overshot/undershot due to rounding
!
      IF ( INDS(1) .LE. 0 ) INDS(1) = 1
      IF ( INDS(1) .GE. VTD%IONO%HEADER%NLON-1 ) INDS(1) = VTD%IONO%HEADER%NLON - 1
!
      ARGS(2) = LAT_GCN_PP
      INDS(2) = INT ( (LAT_GCN_PP - VTD%IONO%HEADER%LAT_MIN)/VTD%IONO%HEADER%LAT_STEP ) + 1
!
! --- Correct possible overshot/undershot due to rounding
!
      IF ( INDS(2) .LE. 0 ) INDS(2) = 1
      IF ( INDS(2) .GE. VTD%IONO%HEADER%NLAT-1 ) INDS(2) = VTD%IONO%HEADER%NLAT - 1
!
! --- Here we compute UTCMTAI -- UTC mnus TAI
!
!
      UTC_TRIAL = VTD%MOM%TAI
      CALL VTD_UTC_TO_TAI ( VTD, VTD%MOM%MJD, UTC_TRIAL, TAI_TRIAL, IER )
      UTCMTAI = UTC_TRIAL - TAI_TRIAL 
!
! --- NB: argument for ARGS(3) is UTC, not time!
!
      ARGS(3) = ( (VTD%MOM%MJD - VTD%IONO%HEADER%MJD_BEG)*86400.0D0 + &
     &            (VTD%MOM%TAI + UTCMTAI - VTD%IONO%HEADER%UTC_BEG ) )/ &
     &          VTD%IONO%HEADER%TIM_STEP
      INDS(3) = INT(ARGS(3)) + 1
!
! --- Correct possible overshot/undershot due to rounding
!
      IF ( INDS(3) .LE. 0 ) INDS(3) = 1
      IF ( INDS(3) .GE. VTD%IONO%HEADER%NEPC-1 ) INDS(3) = VTD%IONO%HEADER%NEPC - 1
!
! --- Compute the value of the total electron contents in the ionosphere
! --- piercing point
!
      TEC = VAL_3D_BSPL4 ( ARGS, VIO__M_DEG, DIMS, INDS, &
     &                     VTD%IONO%LON_VAL, VTD%IONO%LAT_VAL, &
     &                     VTD%IONO%TIM_VAL, VTD%IONO%TEC_SPL )
      IF ( TEC < -VTD__TEC_MAX  .OR.  TEC > VTD__TEC_MAX ) THEN
           IONO_STATUS = VTD__IONO_BADT
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
      IF ( TEC < VTD__TEC_MIN ) TEC = VTD__TEC_MIN
!
! --- Compute the ionosphere mapping function
!
      IF ( VTD%CONF%IONO_MODEL == VIO__GNSS_TEC ) THEN
           IONO_MAP = VTD_IONO_MF ( VTD%STA(ISTA)%ELEV, VTD%IONO%HEADER%HEIGHT )
        ELSE IF ( VTD%CONF%IONO_MODEL == VIO__GNSS_TEC_MOD ) THEN
           IONO_MAP = VTD_IONO_SLM_MF ( VTD%STA(ISTA)%ELEV )
        ELSE IF ( VTD%CONF%IONO_MODEL == VIO__GNSS_TEC_MHI ) THEN
           IONO_MAP = VTD_IONO_600_MF ( VTD%STA(ISTA)%ELEV )
      END IF
!
! --- ... and its time derivative
!
      IONO_MAP_RATE = -DSIN(VTD%STA(ISTA)%ELEV)*DCOS(VTD%STA(ISTA)%ELEV)* &
     &                 VTD%STA(ISTA)%ELEV_DER/ &
     &                (1.0D0 + VTD%IONO%HEADER%HEIGHT/VTD__REA)*IONO_MAP**3
!
! --- Get the square of the effective frequnecy
!
      IF (        OBS_TYP%DELAY_TYPE == VTD__PL__DTP   ) THEN
           IF ( OBS_TYP%FRQ_ION_EFF(1) < VTD__FREQ_MIN .OR. &
     &          OBS_TYP%FRQ_ION_EFF(1) > VTD__FREQ_MAX      ) THEN
!
                IONO_STATUS = VTD__IONO_BADF
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
           EFF_FRQ_SQ = -OBS_TYP%FRQ_ION_EFF(1)**2
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PH__DTP   ) THEN
           IF ( OBS_TYP%FRQ_ION_EFF(2) < VTD__FREQ_MIN .OR. &
     &          OBS_TYP%FRQ_ION_EFF(2) > VTD__FREQ_MAX      ) THEN
!
                IONO_STATUS = VTD__IONO_BADF
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
           EFF_FRQ_SQ = -OBS_TYP%FRQ_ION_EFF(2)**2
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__SL__DTP   ) THEN
           IF ( OBS_TYP%FRQ_ION_EFF(1) < VTD__FREQ_MIN .OR. &
     &          OBS_TYP%FRQ_ION_EFF(1) > VTD__FREQ_MAX      ) THEN
!
                IONO_STATUS = VTD__IONO_BADF
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
           EFF_FRQ_SQ =  OBS_TYP%FRQ_ION_EFF(1)**2
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__SH__DTP   ) THEN
           IF ( OBS_TYP%FRQ_ION_EFF(2) < VTD__FREQ_MIN .OR. &
     &          OBS_TYP%FRQ_ION_EFF(2) > VTD__FREQ_MAX      ) THEN
!
                IONO_STATUS = VTD__IONO_BADF
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
           EFF_FRQ_SQ =  OBS_TYP%FRQ_ION_EFF(2)**2
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__ML__DTP   ) THEN
           IF ( OBS_TYP%FRQ_ION_EFF(1) < VTD__FREQ_MIN .OR. &
     &          OBS_TYP%FRQ_ION_EFF(1) > VTD__FREQ_MAX      ) THEN
!
                IONO_STATUS = VTD__IONO_BADF
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
           EFF_FRQ_SQ =  OBS_TYP%FRQ_ION_EFF(1)**2
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__MH__DTP   ) THEN
           IF ( OBS_TYP%FRQ_ION_EFF(2) < VTD__FREQ_MIN .OR. &
     &          OBS_TYP%FRQ_ION_EFF(2) > VTD__FREQ_MAX      ) THEN
                IONO_STATUS = VTD__IONO_BADF
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
           EFF_FRQ_SQ =  OBS_TYP%FRQ_ION_EFF(2)**2
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PLPH__DTP ) THEN
           EFF_FRQ_SQ = OBS_TYP%FRQ_ION_EFF(1)**2
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__SLSH__DTP ) THEN
           EFF_FRQ_SQ =  0.0D0
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__MLMH__DTP ) THEN
           IF ( SOLVE_DEBUG == 'GET_IONO' ) THEN
                IF ( OBS_TYP%FRQ_ION_EFF(2) < VTD__FREQ_MIN .OR. &
     &               OBS_TYP%FRQ_ION_EFF(2) > VTD__FREQ_MAX      ) THEN
                     IONO_STATUS = VTD__IONO_BADF
                     CALL ERR_LOG ( 0, IUER )
                     RETURN
                END IF
                EFF_FRQ_SQ = OBS_TYP%FRQ_ION_EFF(2)**2
              ELSE
                EFF_FRQ_SQ =  0.0D0
           END IF
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PLMH__DTP ) THEN
           EFF_FRQ_SQ =  0.0D0
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PHML__DTP ) THEN
           EFF_FRQ_SQ =  0.0D0
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PHMH__DTP ) THEN
           EFF_FRQ_SQ =  0.0D0
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PLML__DTP ) THEN
           EFF_FRQ_SQ =  0.0D0
        ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PLMH__DTP ) THEN
           EFF_FRQ_SQ =  0.0D0
      END IF
!
      IF ( DABS(EFF_FRQ_SQ) > FRQ__MIN**2 ) THEN
!
! -------- Compute path delay
!
           IONO_DEL  = IONO_SCALE * ALPHA__VIO/(PI2**2)*TEC*IONO_MAP/EFF_FRQ_SQ
           IONO_RATE = IONO_SCALE * ALPHA__VIO/(PI2**2)*TEC*IONO_MAP_RATE/EFF_FRQ_SQ
         ELSE
           IONO_DEL  = 0.0D0
           IONO_RATE = 0.0D0
      END IF
      IF ( FL_TEST ) THEN
!
! -------- Tri-linear interpolation. This is done for tests
!
           AXL(1) = ((ARGS(1)+PI__NUM)/VTD%IONO%HEADER%LON_STEP - INDS(1) + 1)
           AXL(2) = ((ARGS(2)+P2I)/VTD%IONO%HEADER%LAT_STEP - INDS(2) + 1)
           AXL(3) = (ARGS(3) - INDS(3) + 1)
           AXR(1) = 1.D0 - AXL(1)
           AXR(2) = 1.D0 - AXL(2)
           AXR(3) = 1.D0 - AXL(3)
           TEC_3LIN = &
     &         VTD%IONO%TEC_VAL(INDS(1),  INDS(2),  INDS(3))  *AXR(1)*AXR(2)*AXR(3) &
     &       + VTD%IONO%TEC_VAL(INDS(1),  INDS(2),  INDS(3)+1)*AXR(1)*AXR(2)*AXL(3) &
     &       + VTD%IONO%TEC_VAL(INDS(1),  INDS(2)+1,INDS(3))  *AXR(1)*AXL(2)*AXR(3) &
     &       + VTD%IONO%TEC_VAL(INDS(1),  INDS(2)+1,INDS(3)+1)*AXR(1)*AXL(2)*AXL(3) &
     &       + VTD%IONO%TEC_VAL(INDS(1)+1,INDS(2),  INDS(3))  *AXL(1)*AXR(2)*AXR(3) &
     &       + VTD%IONO%TEC_VAL(INDS(1)+1,INDS(2),  INDS(3)+1)*AXL(1)*AXR(2)*AXL(3) &
     &       + VTD%IONO%TEC_VAL(INDS(1)+1,INDS(2)+1,INDS(3))  *AXL(1)*AXL(2)*AXR(3) &
     &       + VTD%IONO%TEC_VAL(INDS(1)+1,INDS(2)+1,INDS(3)+1)*AXL(1)*AXL(2)*AXL(3)
           TEC_3LIN = TEC_3LIN*VTD%IONO%HEADER%SCALE
      END IF
!
      IF ( VTD%CONF%IVRB == 7  ) THEN
           WRITE ( 6, 110 ) VTD%STA(ISTA)%IVS_NAME, VTD%STA(ISTA)%LAT_GCN/DEG__TO__RAD, &
     &                      LAT_GCN_PP/DEG__TO__RAD, LAT_GDT_PP/DEG__TO__RAD, LONG_PP/DEG__TO__RAD, &
     &                      VTD%STA(ISTA)%ELEV/DEG__TO__RAD, VTD%STA(ISTA)%AZ/DEG__TO__RAD
 110       FORMAT ( 'VTD_IONO_DELAY sta: ', A, ' sta_lat_gcn= ', F8.5, &
     &              ' pp_lat_gcn= ', F9.5, ' pp_lat_gdt= ', F9.5, ' pp_lon: ', F10.5, &
     &              ' deg || elev= ', F6.3, ' az= ', F8.3 )
!!           WRITE  ( 6, 120 ) VTD%STA(ISTA)%IVS_NAME, TEC, INDS(2), INDS(1), INDS(3)
           WRITE  ( 6, 120 ) VTD%STA(ISTA)%IVS_NAME, TEC, INDS(2), INDS(1), INDS(3), &
     &                       VTD%IONO%LAT_VAL(INDS(2))/DEG__TO__RAD, &
     &                       VTD%IONO%LON_VAL(INDS(1))/DEG__TO__RAD
 120       FORMAT ( 'VTD_IONO_DELAY sta: ', A, ' tec= ', F10.3, ' ind_lat: ', I3, &
     &              ' Ind_lon: ', I3, ' Ind_tim: ', I4, &
     &              ' Lat_grid: ', F6.2, ' lon_grid: ', F8.2 )
           STR = MJDSEC_TO_DATE ( VTD%IONO%HEADER%MJD_BEG, VTD%IONO%HEADER%UTC_BEG, IER )
           WRITE ( 6, 130 ) STR(1:19), VTD%IONO%HEADER%TIM_STEP
 130       FORMAT ( 'VTD_IONO_DELAY UTC_beg: ', A, ' Tim_step: ', F7.1 ) 
           WRITE ( 6, * ) 'VTD_IONO_DELAY-423: TEC= ', SNGL(TEC), &
     &                    ' IONO_MAP = ', SNGL(IONO_MAP) , &
     &                    ' MM =  ', VTD_IONO_MF ( VTD%STA(ISTA)%ELEV, 450.0D3 ),  &
     &                    ' SLM= ', VTD_IONO_SLM_MF ( VTD%STA(ISTA)%ELEV ), &
     &                    ' ELEV= ', SNGL(VTD%STA(ISTA)%ELEV), &
     &                    ' HEI = ', SNGL(VTD%IONO%HEADER%HEIGHT)
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_IONO_DELAY  !#!#
