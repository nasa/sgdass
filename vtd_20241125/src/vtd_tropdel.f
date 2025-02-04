      SUBROUTINE VTD_TROPDEL ( VTD, ISTA, TROP_HZD, TROP_WZD, TROP_DEL, &
     &                         TROP_DEL_RATE, TROP_ZEN_DER, TROP_ZEN_RATE, &
     &                         TROP_TILT_N, TROP_TILT_E, SUR_PRS, SUR_PWP, &
     &                         SUR_TEM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_TROPDEL computes hydrostatic constituent of the       *
! *   atmosphere zenith path delay, (wet) non-hydrostatic constituent    *
! *   troposphere path delay, multiplies hydrostatic zenith path delay   *
! *   by hydrostatic mapping function, multiplies wet path delay by      *
! *   wet mapping function, adds two components of path delay together   *
! *   and writes then in TROP_DEL. If SPD biases were defiend, then      *
! *   the routine will apply them. VTD_TROPDEL computes the rate         *
! *   of change of the troposphere path delay as well.                   *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      VTD ( RECORD    ) -- Object which keeps configuration and data  *
! *                           related to VLBI Theoretical Delay (VTD)    *
! *                           package.                                   *
! *     ISTA ( INTEGER*4 ) -- Index of the station in the VTD station    *
! *                           list.                                      *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * TROP_HZD ( REAL*8    ) -- Hydrostatic component of the zenith path   *
! *                           delay. Units: seconds.                     *
! * TROP_WZD ( REAL*8    ) -- Non-hydrostatic, wet component of the      *
! *                           zenith path delay. Units: seconds.         *
! * TROP_DEL ( REAL*8    ) -- Total path delay in the direction to the   *
! *                           source. Units: seconds.                    *
! * TROP_DEL_RATE ( REAL*8    ) -- Rate of change of the total path      *
! *                                delay in the direction to the source. *
! *                                Units: dimensionless.                 *
! * TROP_ZEN_DER  ( REAL*8    ) -- Partial derivative of the troposphere *
! *                                delay wrt to the zenith path delay.   *
! *                                Units: dimensionless.                 *
! * TROP_ZEN_RATE ( REAL*8    ) -- Partial derivative of the troposphere *
! *                                delay wrt to the zenith path delay.   *
! *                                Units: 1/s.                           *
! * TROP_TILT_N   ( REAL*8    ) -- Partial derivative of the tilt of the *
! *                                atmosphere symmetry axis in east      *
! *                                direction multiplied by the apriori   *
! *                                zenith delay. Units: seconds.         *
! * TROP_TILT_E   ( REAL*8    ) -- Partial derivative of the tilt of the *
! *                                atmosphere symmetry axis in north     *
! *                                direction multiplied by the apriori   *
! *                                zenith delay. Units: seconds.         *
! *     SUR_PRS   ( REAL*8    ) -- Surface temperature from the model.   *
! *                                Units: Pa.                            *
! *     SUR_PRS   ( REAL*8    ) -- Surface parital pressure of water     *
! *                                vapor from the model. Units: Pa.      *
! *     SUR_TEM   ( REAL*8    ) -- Surface temperature from the model.   *
! *                                Units: K.                             *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
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
! *                                                                      *
! *  ### 29-JAN-2004   VTD_TROPDEL  v6.1 (c)  L. Petrov 07-JAN-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      REAL*8     TROP_HZD, TROP_WZD, TROP_DEL, TROP_DEL_RATE, &
     &           TROP_ZEN_DER, TROP_ZEN_RATE, TROP_TILT_N, TROP_TILT_E, &
     &           SUR_PRS, SUR_PWP, SUR_TEM
      INTEGER*4  ISTA, IUER
      REAL*8     HEI_GPT, MMF, MMF_RATE, MAP_ISA, TROP_RATE_ZEN_DER
      CHARACTER  STR*32, STR1*32, STR2*32
      REAL*8     ATM_PRES_LOW, ATM_PRES_HIGH, C__CHEN_HERRING
      PARAMETER  ( ATM_PRES_LOW  =  50000.0D0 )
      PARAMETER  ( ATM_PRES_HIGH = 120000.0D0 )
      PARAMETER  ( C__CHEN_HERRING = 0.0032D0 )
      REAL*4     SPD_VAL_R4, SPD_DER_R4(3)
      REAL*8     SPD_VAL(3), SPD_ZEN(3), SPD_DER(3,3), ELEV_USE
      REAL*4     EPS_MAR
      REAL*8     EPS_ELEV
      PARAMETER  ( EPS_MAR  = 1.0E-5 )
      PARAMETER  ( EPS_ELEV = 1.0D-8 )
      REAL*4     ARGS(3)
      INTEGER*4  DIMS(3), INDS(3), J1, IND, IER
      LOGICAL*1  FL_DEBUG, FL_WARN_CONT
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*32
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN8
      REAL*4,    EXTERNAL :: VAL_1D_BSPL4, VAL_3D_BSPL4, VAL_1D_BSPL 
      REAL*8,    EXTERNAL :: DEL_ISA, DEL_ISA_DER, INV_MAP_ISA, NMF_H, &
     &                       NMF_H_RATE, NMF_W, VTD_MMF_ZEN, ZENDEL_SAA, &
     &                       VTD_IONO_MF, NMF_W_RATE
!
      FL_DEBUG = .FALSE.
      TROP_DEL      = 0.0D0
      TROP_DEL_RATE = 0.0D0
!
      IF ( VTD%STA(ISTA)%ATM_PRES .LT. ATM_PRES_LOW   .OR.  &
     &     VTD%STA(ISTA)%ATM_PRES .GT. ATM_PRES_HIGH        ) THEN
!
! -------- Surface pressure is insane
!
           IF ( VTD%CONF%METEO_DEF .EQ. VTD__IMA ) THEN
!
! ------------- Use IMA standard atmosphere instead of that.
!
                HEI_GPT = VTD%STA(ISTA)%HEI_ELL*9.80665D0/VTD%STA(ISTA)%GAC_ELL
                VTD%STA(ISTA)%ATM_PRES = 101324.2D0 * DEXP ( -1.1859D-4*HEI_GPT &
     &                           -1.1343D-9*HEI_GPT**2 -2.5644D-14*HEI_GPT**3 )
                VTD%STA(ISTA)%AIR_TEMP = 288.15D0 - 0.0065D0*VTD%STA(ISTA)%HEI_ELL
              ELSE
!
! ------------- Use Calc default instead of
!
                VTD%STA(ISTA)%ATM_PRES = 101325.0D0* &
     &                (1.0D0 - (6.5D-3)*VTD%STA(ISTA)%HEI_ELL/293.15D0)**5.26D0
           END IF
      END IF
!
! --- Compute hydrostatic zenith path delay
!
      IF ( VTD%CONF%HZD_MODEL == VTD__SAA ) THEN
           TROP_HZD = ZENDEL_SAA ( VTD%STA(ISTA)%ATM_PRES, &
     &                             VTD%STA(ISTA)%LAT_GCN, &
     &                             VTD%STA(ISTA)%HEI_ELL )
         ELSE IF ( VTD%CONF%HZD_MODEL == VTD__MMF ) THEN
           CALL ERR_PASS ( IUER, IER )
           TROP_HZD = VTD_MMF_ZEN ( VTD, HYD__IND, ISTA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2431, IUER, 'VTD_TROPDEL', 'Error in '// &
     &              'an attempt to compute MMF HYD zenith path delay' )
                RETURN
           END IF
         ELSE IF ( VTD%CONF%HZD_MODEL == VTD__NONE ) THEN
           TROP_HZD = 0.0D0
         ELSE
           CALL CLRCH ( STR )
           CALL INCH  ( VTD%CONF%HZD_MODEL, STR )
           CALL ERR_LOG ( 2432, IUER, 'VTD_TROPDEL', 'Unsupported code '// &
     &         'for the model for hydrostatic troposphere path delay '// &
     &         'in zenith direction: '//STR )
           RETURN
      END IF
!
! --- Compute wet (non-hydrostatic) zenith path delay
!
      IF ( VTD%CONF%WZD_MODEL == VTD__NONE ) THEN
           TROP_WZD = 0.0D0
         ELSE IF ( VTD%CONF%WZD_MODEL == VTD__MMF ) THEN
           CALL ERR_PASS ( IUER, IER )
           TROP_WZD = VTD_MMF_ZEN ( VTD, NHY__IND, ISTA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2433, IUER, 'VTD_TROPDEL', 'Error in '// &
     &              'an attempt to compute MMF NHY zenith path delay' )
                RETURN
           END IF
         ELSE
           CALL ERR_LOG ( 2434, IUER, 'VTD_TROPDEL', 'Unsupported code '// &
     &         'for the model for wet troposphere path delay in zenith '// &
     &         'direction' )
           RETURN
      END IF
!
! --- Compute mapping function for hydrostatic path delay and the rate of
! --- change of mapping function.
! --- Multiply it by zenith path delay and get the total delay
!
      IF ( VTD%CONF%HMF_MODEL .EQ. VTD__NMFH ) THEN
           TROP_DEL = TROP_HZD * NMF_H ( VTD%MOM%MJD, VTD%MOM%TAI, &
     &                VTD%STA(ISTA)%LAT_GDT, VTD%STA(ISTA)%HEI_ELL, &
     &                VTD%STA(ISTA)%ELEV )
           TROP_DEL_RATE = TROP_HZD * NMF_H_RATE ( VTD%MOM%MJD, VTD%MOM%TAI, &
     &                VTD%STA(ISTA)%LAT_GDT, VTD%STA(ISTA)%HEI_ELL, &
     &                VTD%STA(ISTA)%ELEV, VTD%STA(ISTA)%ELEV_DER )
         ELSE IF ( VTD%CONF%HMF_MODEL .EQ. VTD__MMF ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_MMF  ( VTD, HYD__IND, ISTA, MMF, MMF_RATE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2435, IUER, 'VTD_TROPDEL', 'Error in '// &
     &              'an attempt to compute HYD MMF' )
                RETURN
           END IF
           TROP_DEL      = TROP_HZD * MMF
           TROP_DEL_RATE = TROP_HZD * MMF_RATE
         ELSE IF ( VTD%CONF%HMF_MODEL .EQ. VTD__NONE ) THEN
           TROP_DEL = 0.0D0
           TROP_DEL_RATE = 0.0D0
         ELSE
           CALL ERR_LOG ( 2436, IUER, 'VTD_TROPDEL', 'Unknown hydrostatic '// &
     &         'mapping function code' )
           RETURN
      END IF
!
! --- Compute a derivative of the path delay versus zenith path delay 
! --- (mapping function)
!
      IF ( VTD%CONF%ATM_PARTIAL_TYPE == VTD__NONE ) THEN
           TROP_ZEN_DER = 0.0D0
         ELSE IF ( VTD%CONF%ATM_PARTIAL_TYPE == VTD__NMFW ) THEN
           TROP_ZEN_DER  = NMF_W ( VTD%STA(ISTA)%LAT_GDT, VTD%STA(ISTA)%ELEV  )
           TROP_ZEN_RATE = NMF_W_RATE ( VTD%STA(ISTA)%LAT_GDT, VTD%STA(ISTA)%ELEV, &
     &                                  VTD%STA(ISTA)%ELEV_DER )
         ELSE IF ( VTD%CONF%ATM_PARTIAL_TYPE == VTD__NMFH ) THEN
           TROP_ZEN_DER  = NMF_H ( VTD%MOM%MJD, VTD%MOM%TAI, &
     &                            VTD%STA(ISTA)%LAT_GDT, VTD%STA(ISTA)%HEI_ELL, &
     &                            VTD%STA(ISTA)%ELEV )
           TROP_ZEN_RATE = NMF_H_RATE ( VTD%MOM%MJD, VTD%MOM%TAI, &
     &                            VTD%STA(ISTA)%LAT_GDT, VTD%STA(ISTA)%HEI_ELL, &
     &                            VTD%STA(ISTA)%ELEV, VTD%STA(ISTA)%ELEV_DER  )
         ELSE IF ( VTD%CONF%ATM_PARTIAL_TYPE .EQ. VTD__MMF ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_MMF  ( VTD, NHY__IND, ISTA, MMF, MMF_RATE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2437, IUER, 'VTD_TROPDEL', 'Error in '// &
     &              'an attempt to compute HYD MMF' )
                RETURN
           END IF
           TROP_ZEN_DER   = MMF
           TROP_ZEN_RATE  = MMF_RATE
           TROP_DEL       = TROP_DEL + TROP_WZD * MMF
           TROP_DEL_RATE  = TROP_DEL_RATE + TROP_WZD * MMF_RATE
         ELSE IF ( VTD%CONF%ATM_PARTIAL_TYPE .EQ. VTD__IONO_350 ) THEN
!
! -------- Ionosphere mapping function with the ionosphere height 350 km
!
           TROP_ZEN_DER = VTD_IONO_MF ( VTD%STA(ISTA)%ELEV, 350.0D0*1.D3 )
         ELSE IF ( VTD%CONF%ATM_PARTIAL_TYPE .EQ. VTD__IONO_400 ) THEN
           TROP_ZEN_DER = VTD_IONO_MF ( VTD%STA(ISTA)%ELEV, 400.0D0*1.D3 )
         ELSE IF ( VTD%CONF%ATM_PARTIAL_TYPE .EQ. VTD__IONO_450 ) THEN
           TROP_ZEN_DER = VTD_IONO_MF ( VTD%STA(ISTA)%ELEV, 450.0D0*1.D3 )
         ELSE IF ( VTD%CONF%ATM_PARTIAL_TYPE .EQ. VTD__IONO_500 ) THEN
           TROP_ZEN_DER = VTD_IONO_MF ( VTD%STA(ISTA)%ELEV, 500.0D0*1.D3 )
         ELSE IF ( VTD%CONF%ATM_PARTIAL_TYPE .EQ. VTD__GL ) THEN
           IF ( VTD%MF%STATUS .NE. VTD__LOAD ) THEN
                CALL ERR_LOG ( 2438, IUER, 'VTD_TROPDEL', 'Trap of '// &
     &              'internal control: mapping function for the '// &
     &              'Gaussian layer model has not been loaded' )
                RETURN
           END IF
!
           IF ( VTD%STA(ISTA)%ELEV < VTD%MF%EL_ARG(1) ) THEN
                ELEV_USE = VTD%MF%EL_ARG(1) + EPS_ELEV
                IF ( VTD%CONF%FL_WARN ) THEN
                     FL_WARN_CONT = .TRUE.
                     IF ( VTD%CONF%SPD_MODEL   == TRP__SPD_3D ) THEN
                          IF ( VTD%SPD_3D(ISTA)%ELV%N_EL > 0 ) THEN
                               IF ( ELEV_USE <  INV_MAP_ISA( DBLE(VTD%SPD_3D(ISTA)%MAP_ARR(VTD%SPD_3D(ISTA)%ELV%N_EL) ) ) ) THEN
                                    FL_WARN_CONT = .FALSE.
                               END IF
                          END IF
                     END IF
                     IF ( FL_WARN_CONT ) THEN
                          WRITE ( 6, '(A,F6.2,A,F6.2)' ) 'VTD_TROPDEL: Warning elevation '// &
     &                               ' angle is ', VTD%STA(ISTA)%ELEV/DEG__TO__RAD, &
     &                               ' deg at station '//VTD%STA(ISTA)%IVS_NAME// &
     &                               ', which is below the lowest limit ', &
     &                               VTD%MF%EL_ARG(1)/DEG__TO__RAD
                     END IF
                END IF
              ELSE IF ( VTD%STA(ISTA)%ELEV > VTD%MF%EL_ARG(VTD%MF%L_NOD) ) THEN
                ELEV_USE = VTD%MF%EL_ARG(VTD%MF%L_NOD) 
              ELSE 
                ELEV_USE = VTD%STA(ISTA)%ELEV 
           END IF
           IND = IXMN8 ( VTD%MF%L_NOD, VTD%MF%EL_ARG, ELEV_USE )
           IF ( IND .LE. 0 ) THEN
                CALL CLRCH ( STR )
                WRITE ( UNIT=STR(1:12), FMT='(1PD12.4)' ) &
     &                                  VTD%STA(ISTA)%ELEV/DEG__TO__RAD
                CALL CLRCH ( STR1 )
                WRITE ( UNIT=STR1(1:6), FMT='(F6.3)' ) &
     &                                  VTD%MF%EL_ARG(1)/DEG__TO__RAD
                CALL CLRCH ( STR2 )
                WRITE ( UNIT=STR2(1:6), FMT='(F6.3)' ) &
     &                                  VTD%MF%EL_ARG(VTD%MF%L_NOD)/DEG__TO__RAD
                WRITE ( 6, * ) ' IND = ', IND
                WRITE ( 6, * ) ' VTD%MF%EL_ARG= ', VTD%MF%EL_ARG
                WRITE ( 6, * ) ' VTD%STA(ISTA)%ELEV = ', VTD%STA(ISTA)%ELEV
                CALL FLUSH ( 6 )
                CALL ERR_LOG ( 2439, IUER, 'VTD_TROPDEL', 'Trap of '// &
     &              'internal control: elevation angle is out of range '// &
     &              'for computing partial derivative over atmosphere '// &
     &              'path in zenith direction: ELEV= '//STR(1:I_LEN(STR))// &
     &              ' deg, but the rnage is ['//STR(1:I_LEN(STR1))// &
     &              ' , '//STR2(1:I_LEN(STR2))//' ] deg' )
                RETURN
           END IF
           CALL VAL_DER_1D_BSPL ( ELEV_USE, VTD%MF%L_NOD, &
     &                            VTD__M_SPD, IND, VTD%MF%EL_ARG, &
     &                            VTD%MF%MF_SPL, TROP_ZEN_DER, &
     &                            TROP_ZEN_RATE )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   write ( 6, 217 ) vtd%sta(ista)%ivs_name, elev_use/deg__to__rad, trop_zen_der, & ! %%%%%%%%%%%%%
!     &              nmf_w ( vtd%sta(ista)%lat_gdt, vtd%sta(ista)%elev )    ! %%%%%%%%%%%%%%%%%%%%%
! 217  format ( 'VTD_TROPDEL. Sta: ',A, ' Elev= ', F8.4, ' deg  tzd: ', F8.4, ' tt: ', F8.4 ) ! %%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      END IF
!
      IF ( VTD%CONF%SPD_MODEL   == TRP__SPD_3D  .AND.  &
     &     VTD%STATUS_SPD(ISTA) == SPD__INTR           ) THEN
!
! -------- Interpolate path delay computed with using numerical weather models
!
           IF ( VTD%STA(ISTA)%ELEV > INV_MAP_ISA ( DBLE(VTD%SPD_3D(ISTA)%MAP_ARR(VTD%SPD_3D(ISTA)%ELV%N_EL) ) ) ) THEN
!
              IF ( VTD%STA(ISTA)%ELEV > 0.0D0 .AND. &
     &             VTD%STA(ISTA)%ELEV < INV_MAP_ISA ( DBLE(VTD%SPD_3D(ISTA)%MAP_ARR(VTD%SPD_3D(ISTA)%ELV%N_EL))) ) THEN
                   MAP_ISA = VTD%SPD_3D(ISTA)%MAP_ARR(VTD%SPD_3D(ISTA)%ELV%N_EL) 
                 ELSE 
                   MAP_ISA = DEL_ISA ( VTD%STA(ISTA)%ELEV )/ DEL_ISA ( P2I )
              END IF
              ARGS(1) = MAP_ISA
              ARGS(2) = VTD%STA(ISTA)%AZ
              ARGS(3) = ( VTD%MOM%MJD*86400.0D0 + VTD%MOM%TAI ) - &
     &                  VTD%SPD_3D(ISTA)%TIM_BEG 
!
              DIMS(1) = VTD%SPD_3D(ISTA)%ELV%N_EL
              DIMS(2) = VTD%SPD_3D(ISTA)%AZM%N_AZ
              DIMS(3) = VTD%SPD_3D(ISTA)%N_TIM
!
! ----------- Now we compute the pivotal index. An extra caution is to be 
! ----------- taken for assigning the index of the pivotal element, if the
! ----------- argument is close to the boundary of the expansion interval
!
              IF ( ABS(ARGS(1) - VTD%SPD_3D(ISTA)%MAP_ARR(1)) < &
     &             EPS_MAR*(VTD%SPD_3D(ISTA)%MAP_ARR(2) - VTD%SPD_3D(ISTA)%MAP_ARR(1)) ) THEN
                   INDS(1) = 1
                 ELSE IF ( ABS(ARGS(1) - VTD%SPD_3D(ISTA)%MAP_ARR(DIMS(1))) < &
     &                     EPS_MAR*(VTD%SPD_3D(ISTA)%MAP_ARR(2) - VTD%SPD_3D(ISTA)%MAP_ARR(1)) ) THEN
                   INDS(1) = DIMS(1) - 1
                 ELSE 
                   INDS(1) = INT ( (ARGS(1) - VTD%SPD_3D(ISTA)%MAP_ARR(1))/ &
     &                             (VTD%SPD_3D(ISTA)%MAP_ARR(2) - VTD%SPD_3D(ISTA)%MAP_ARR(1)) ) &
     &                       + 1
              END IF
!
              IF ( ABS(ARGS(2) - VTD%SPD_3D(ISTA)%AZM%AZIM(1) ) < &
     &             EPS_MAR*(VTD%SPD_3D(ISTA)%AZM%AZIM(2) - VTD%SPD_3D(ISTA)%AZM%AZIM(1)) ) THEN
                   INDS(2) = 1
                ELSE IF ( ABS(ARGS(2) - VTD%SPD_3D(ISTA)%AZM%AZIM(DIMS(2))) < &
     &                    EPS_MAR*(VTD%SPD_3D(ISTA)%AZM%AZIM(2) - VTD%SPD_3D(ISTA)%AZM%AZIM(1)) ) THEN
                   INDS(2) = DIMS(2) - 1
                ELSE 
                   INDS(2) = INT ( (ARGS(2) - VTD%SPD_3D(ISTA)%AZM%AZIM(1))/ &
     &                             (VTD%SPD_3D(ISTA)%AZM%AZIM(2) - VTD%SPD_3D(ISTA)%AZM%AZIM(1)) ) &
     &                       + 1
              END IF
!
              IF ( ABS(ARGS(3) - VTD%SPD_3D(ISTA)%TIM_ARR(1) ) < &
     &             EPS_MAR*(VTD%SPD_3D(ISTA)%TIM_ARR(2) - VTD%SPD_3D(ISTA)%TIM_ARR(1)) ) THEN
                   INDS(3) = 1
                ELSE IF ( ABS(ARGS(3) - VTD%SPD_3D(ISTA)%TIM_ARR(DIMS(3))) < &
     &                    EPS_MAR*(VTD%SPD_3D(ISTA)%TIM_ARR(2) - VTD%SPD_3D(ISTA)%TIM_ARR(1)) ) THEN
                   INDS(3) = DIMS(3) - 1
                ELSE 
                   INDS(3) = INT ( (ARGS(3) - VTD%SPD_3D(ISTA)%TIM_ARR(1))/ &
     &                             (VTD%SPD_3D(ISTA)%TIM_ARR(2) - VTD%SPD_3D(ISTA)%TIM_ARR(1)) ) &
     &                       + 1
              END IF
!
! ----------- Check the range
!
              IF ( INDS(1) < 1  .OR.  INDS(1) .GE. DIMS(1) ) THEN
                   CALL CLRCH ( STR )
                   WRITE ( UNIT=STR(1:12), FMT='(1PD12.4)' ) VTD%STA(ISTA)%ELEV/DEG__TO__RAD
                   CALL CLRCH ( STR1 )
                   WRITE ( UNIT=STR1(1:6), FMT='(F6.3)' ) &
     &                     INV_MAP_ISA ( DBLE(VTD%SPD_3D(ISTA)%MAP_ARR(VTD%SPD_3D(ISTA)%ELV%N_EL)) )/DEG__TO__RAD
                   CALL CLRCH ( STR2 )
                   WRITE ( UNIT=STR2(1:6), FMT='(F6.3)' ) &
     &                     INV_MAP_ISA ( DBLE(VTD%SPD_3D(ISTA)%MAP_ARR(1)) )/DEG__TO__RAD
!@                   CALL ERR_LOG ( 2440, IUER, 'VTD_TROPDEL', 'Elevation angle '// &
                   CALL ERR_LOG ( 2441, -2, 'VTD_TROPDEL', 'Elevation angle '// &
     &                 'for station '//VTD%STA(ISTA)%IVS_NAME//' -- '// &
     &                  STR(1:I_LEN(STR))//' deg is out of range [ '// &
     &                  STR1(1:I_LEN(STR1))//', '//STR2(1:I_LEN(STR2))//' ] deg' )
!
                   TROP_HZD = 0.0D0
                   TROP_WZD = 0.0D0
                   TROP_DEL = 0.0D0
                   TROP_DEL_RATE = 0.0D0
                   TROP_ZEN_DER  = 1.0D0
                   TROP_TILT_N = 1.0D0
                   TROP_TILT_E = 1.0D0
!
                   CALL ERR_LOG ( 0, IUER )
                   RETURN 
              END IF
!
              IF ( INDS(2) < 1  .OR.  INDS(2) .GE. DIMS(2) ) THEN
                   CALL CLRCH ( STR )
                   WRITE ( UNIT=STR(1:12), FMT='(1PD12.4)' ) VTD%STA(ISTA)%AZ/DEG__TO__RAD
                   CALL CLRCH ( STR1 )
                   WRITE ( UNIT=STR1(1:12), FMT='(1PD12.4)' ) &
     &                     VTD%SPD_3D(ISTA)%AZM%AZIM(1)/DEG__TO__RAD
                   CALL CLRCH ( STR2 )
                   WRITE ( UNIT=STR2(1:12), FMT='(1PD12.4)' ) &
     &                     VTD%SPD_3D(ISTA)%AZM%AZIM(VTD%SPD_3D(ISTA)%AZM%N_AZ)/DEG__TO__RAD
                   CALL ERR_LOG ( 2443, IUER, 'VTD_TROPDEL', 'Azimuth angle '// &
     &                 'for station '//VTD%STA(ISTA)%IVS_NAME//' -- '// &
     &                  STR(1:I_LEN(STR))//' deg is out of range [ '// &
     &                  STR1(1:I_LEN(STR1))//', '//STR2(1:I_LEN(STR2))//' ] deg' )
                   RETURN 
              END IF
!
              IF ( INDS(3) < 1  .OR.  INDS(3) .GE. DIMS(3) ) THEN
                   CALL ERR_LOG ( 2444, IUER, 'VTD_TROPDEL', 'Trap of '// &
     &                 'internal control: observation epoch is out of '// &
     &                 'range of the slant path delay B-spline expansion '// &
     &                 'for station '//VTD%STA(ISTA)%IVS_NAME )
                   RETURN 
              END IF
!
              DO 410 J1=1,VTD%SPD_3D(ISTA)%MOD%N_RFR
                 IF ( VTD%CONF%FL_RATE ) THEN
                      CALL VAL_DER_3D_BSPL4 ( ARGS, SPD__MDEG, DIMS, INDS, &
     &                                VTD%SPD_3D(ISTA)%MAP_ARR, &
     &                                VTD%SPD_3D(ISTA)%AZM%AZIM, &
     &                                VTD%SPD_3D(ISTA)%TIM_ARR, &
     &                                VTD%SPD_3D(ISTA)%DELS(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,J1), &
     &                                SPD_VAL_R4, SPD_DER_R4 )
                      SPD_VAL(J1)   = SPD_VAL_R4
                      SPD_DER(1,J1) = SPD_DER_R4(1)* &
     &                                DEL_ISA_DER(VTD%STA(ISTA)%ELEV)/ &
     &                                DEL_ISA(P2I)
                      SPD_DER(2,J1) = SPD_DER_R4(2)
                      SPD_DER(3,J1) = SPD_DER_R4(3)
                   ELSE
                      SPD_VAL(J1) = VAL_3D_BSPL4 ( ARGS, SPD__MDEG, DIMS, INDS, &
     &                                     VTD%SPD_3D(ISTA)%MAP_ARR, &
     &                                     VTD%SPD_3D(ISTA)%AZM%AZIM, &
     &                                     VTD%SPD_3D(ISTA)%TIM_ARR, &
     &                                     VTD%SPD_3D(ISTA)%DELS(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,J1) )
                 END IF
                 SPD_ZEN(J1) = VAL_1D_BSPL4 ( ARGS(3), VTD%SPD_3D(ISTA)%N_TIM, &
     &                                        SPD__MDEG, INDS(3), &
     &                                        VTD%SPD_3D(ISTA)%TIM_ARR, &
     &                                        VTD%SPD_3D(ISTA)%ZEN_DEL(1-SPD__MDEG,J1) )
 410          CONTINUE 
!
              TROP_DEL = SPD_VAL(1) 
              TROP_DEL_RATE = SPD_DER(1,1)*VTD%STA(ISTA)%ELEV_DER + &
     &                        SPD_DER(2,1)*VTD%STA(ISTA)%AZ_DER + &
     &                        SPD_DER(3,1)
              IF ( VTD%CONF%ATM_PARTIAL_TYPE .EQ. VTD__TOTS ) THEN
                    TROP_ZEN_DER  = SPD_VAL(1)/SPD_ZEN(1)
                    TROP_ZEN_RATE = SPD_DER(1,1)/SPD_ZEN(1)*VTD%STA(ISTA)%ELEV_DER + &
     &                              SPD_DER(2,1)/SPD_ZEN(1)*VTD%STA(ISTA)%AZ_DER
                 ELSE IF ( VTD%CONF%ATM_PARTIAL_TYPE .EQ. VTD__WATS ) THEN
                    TROP_ZEN_DER  = SPD_VAL(2)/SPD_ZEN(2)
                    TROP_ZEN_RATE = SPD_DER(1,2)/SPD_ZEN(2)*VTD%STA(ISTA)%ELEV_DER + &
     &                              SPD_DER(2,2)/SPD_ZEN(2)*VTD%STA(ISTA)%AZ_DER
              END IF
              TROP_HZD = SPD_ZEN(SPD__TOT) 
              TROP_WZD = SPD_ZEN(SPD__WAT) 
              IF ( FL_DEBUG ) THEN
                   STR = MJDSEC_TO_DATE ( VTD%MOM%MJD, VTD%MOM%TAI, IER )
                   WRITE ( 6, 210 ) STR(1:24), VTD%STA(ISTA)%IVS_NAME, &
     &                              VTD%STA(ISTA)%AZ/DEG__TO__RAD, &
     &                              VTD%STA(ISTA)%ELEV/DEG__TO__RAD, SPD_VAL(1)
 210               FORMAT ( 'VTD_TROPDEL Date: ',A, ' Sta: ', A, ' Az: ', F8.4, &
     &                      ' deg  El: ', F8.4, '  Tot_tropo_del: ', 1PD13.6 )
              END IF
              SUR_PRS = VAL_1D_BSPL4 ( ARGS(3), VTD%SPD_3D(ISTA)%N_TIM, &
     &                                 SPD__MDEG, INDS(3), &
     &                                 VTD%SPD_3D(ISTA)%TIM_ARR, &
     &                                 VTD%SPD_3D(ISTA)%SUR_PRS )
              SUR_PWP = VAL_1D_BSPL4 ( ARGS(3), VTD%SPD_3D(ISTA)%N_TIM, &
     &                                 SPD__MDEG, INDS(3), &
     &                                 VTD%SPD_3D(ISTA)%TIM_ARR, &
     &                                 VTD%SPD_3D(ISTA)%SUR_PWP )
              SUR_TEM = VAL_1D_BSPL4 ( ARGS(3), VTD%SPD_3D(ISTA)%N_TIM, &
     &                                 SPD__MDEG, INDS(3), &
     &                                 VTD%SPD_3D(ISTA)%TIM_ARR, &
     &                                 VTD%SPD_3D(ISTA)%SUR_TEM )
           ELSE IF ( VTD%CONF%SPD_MODEL   == TRP__SPD_3D ) THEN
              TROP_DEL     = 0.0D0
              IF ( VTD%CONF%ATM_PARTIAL_TYPE .EQ. VTD__TOTS .OR. &
     &             VTD%CONF%ATM_PARTIAL_TYPE .EQ. VTD__WATS      ) THEN
                   TROP_ZEN_DER = 0.0D0
              END IF
              TROP_HZD     = 0.0D0
              TROP_WZD     = 0.0D0
              IF ( VTD%CONF%SPD_MODEL   == TRP__SPD_3D  .AND.  &
     &             VTD%STATUS_SPD(ISTA) == SPD__INTR            ) THEN
                   IF ( VTD%CONF%IVRB .GE. 2 .AND. &
     &                  VTD%CONF%FL_WARN     .AND. &
     &                  VTD%STA(ISTA)%ELEV   <  INV_MAP_ISA( DBLE(VTD%SPD_3D(ISTA)%MAP_ARR(VTD%SPD_3D(ISTA)%ELV%N_EL) ) ) ) THEN
!
                        CALL CLRCH ( STR )
                        WRITE ( UNIT=STR(1:12), FMT='(1PD12.4)' ) VTD%STA(ISTA)%ELEV/DEG__TO__RAD
                        CALL CLRCH ( STR1 )
                        WRITE ( UNIT=STR1(1:6), FMT='(F6.3)' ) &
     &                          INV_MAP_ISA ( DBLE(VTD%SPD_3D(ISTA)%MAP_ARR(VTD%SPD_3D(ISTA)%ELV%N_EL)) )/DEG__TO__RAD
                        WRITE ( 6, '(A)' ) 'VTD_LOAD WARNING: Cannot compute '// &
     &                         'slant path delays for '//STR(1:I_LEN(STR))// &
     &                         ' stations: '//VTD%STA(ISTA)%IVS_NAME//' because its '// &
     &                         'elevation angle '//STR(1:I_LEN(STR))//' deg is '// &
     &                         'than '//STR1(1:I_LEN(STR1))//' deg'
                   END IF
              END IF
          END IF
      END IF
!
      TROP_TILT_N = 0.0D0
      TROP_TILT_E = 0.0D0
      IF ( VTD%CONF%ATD_PARTIALS == VTD__NE ) THEN
           TROP_TILT_N = -TROP_ZEN_DER*DCOS(VTD%STA(ISTA)%AZ)
           TROP_TILT_E = -TROP_ZEN_DER*DSIN(VTD%STA(ISTA)%AZ)
        ELSE IF ( VTD%CONF%ATD_PARTIALS == VTD__MM95 ) THEN
           TROP_TILT_N = NMF_H ( VTD%MOM%MJD, VTD%MOM%TAI,                     &
     &                           VTD%STA(ISTA)%LAT_GDT, VTD%STA(ISTA)%HEI_ELL, &
     &                           VTD%STA(ISTA)%ELEV )/         &
     &                           DTAN(VTD%STA(ISTA)%ELEV)* &
     &                           DCOS(VTD%STA(ISTA)%AZ)
           TROP_TILT_E = NMF_H ( VTD%MOM%MJD, VTD%MOM%TAI,                     &
     &                           VTD%STA(ISTA)%LAT_GDT, VTD%STA(ISTA)%HEI_ELL, &
     &                           VTD%STA(ISTA)%ELEV )/         &
     &                           DTAN(VTD%STA(ISTA)%ELEV)* &
     &                           DSIN(VTD%STA(ISTA)%AZ)
         ELSE IF ( VTD%CONF%ATD_PARTIALS == VTD__TNMFH ) THEN
           TROP_TILT_N = NMF_H ( VTD%MOM%MJD, VTD%MOM%TAI,                     &
     &                           VTD%STA(ISTA)%LAT_GDT, VTD%STA(ISTA)%HEI_ELL, &
     &                           VTD%STA(ISTA)%ELEV )*DCOS(VTD%STA(ISTA)%AZ)/  &
     &                           ( DTAN(VTD%STA(ISTA)%ELEV)* &
     &                             DSIN(VTD%STA(ISTA)%ELEV) + C__CHEN_HERRING )
           TROP_TILT_E = NMF_H ( VTD%MOM%MJD, VTD%MOM%TAI,                     &
     &                           VTD%STA(ISTA)%LAT_GDT, VTD%STA(ISTA)%HEI_ELL, &
     &                           VTD%STA(ISTA)%ELEV )*DSIN(VTD%STA(ISTA)%AZ)/  &
     &                           ( DTAN(VTD%STA(ISTA)%ELEV)* &
     &                             DSIN(VTD%STA(ISTA)%ELEV) + C__CHEN_HERRING )
         ELSE IF ( VTD%CONF%ATD_PARTIALS == VTD__TNMFW ) THEN
           TROP_TILT_N = NMF_W ( VTD%STA(ISTA)%LAT_GDT, VTD%STA(ISTA)%ELEV) &
     &                           *DCOS(VTD%STA(ISTA)%AZ)/  &
     &                           ( DTAN(VTD%STA(ISTA)%ELEV)* &
     &                             DSIN(VTD%STA(ISTA)%ELEV) + C__CHEN_HERRING )
           TROP_TILT_E = NMF_W ( VTD%STA(ISTA)%LAT_GDT, VTD%STA(ISTA)%ELEV) &
     &                           *DSIN(VTD%STA(ISTA)%AZ)/  &
     &                           ( DTAN(VTD%STA(ISTA)%ELEV)* &
     &                             DSIN(VTD%STA(ISTA)%ELEV) + C__CHEN_HERRING )
         ELSE IF ( VTD%CONF%ATD_PARTIALS == VTD__NONE  ) THEN
           TROP_TILT_N = 0.0D0
           TROP_TILT_E = 0.0D0
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_TROPDEL  !#!#
