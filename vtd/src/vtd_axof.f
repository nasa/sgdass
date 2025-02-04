      SUBROUTINE VTD_AXOF ( VTD, ISTA, ISOU, AXOF_CRS, AXOF_CRS_RATE, &
     &                      AXOF_UP, AXOF_UP_RATE, AVEC_CRS, AVEC_CRS_RATE, &
     &                      UNIT_AXOF_CRS, UNIT_AXOF_CRS_RATE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_AXOF  computes the vector of the antenna axis offset. *
! *   The vector of antenna's axis offset is determine as the vector     *
! *   originating from the point on the fixed antenna's axis to the      *
! *   point on the moving antennas axis along the line which is          *
! *   orthogonal to both axes. The point on the moving axis where the    *
! *   vector of the antenna's offset ends is the physical point for      *
! *   which geometrical pat delay is computed.                           *
! *                                                                      *
! *   This module supports two modes for computation of antenna axis     *
! *   offset: intrinsic and mode compatibility with bug in Calc versions *
! *   1994-2004. If VTD%CONF%AXOF_MODEL == VTD__YES, then intrinsic      *
! *   mode is used. If VTD%CONF%AXOF_MODEL == VTD__CALC, then in         *
! *   addition to computation of the vector of antenna axis offset       *
! *   AXOF_CTS and progection of this vector to local zenith AXOF_UP     *
! *   two quantities are computed:                                       *
! *   VTD%STA(ISTA)%DELAY_AXOF_VTD  -- contribution to delay due to      *
! *                                    antenna axis offset according to  *
! *                                    intrinsic formula;                *
! *   VTD%STA(ISTA)%DELAY_AXOF_CALC -- contribution to delay due to      *
! *                                    antenna axis offset in a mode of  *
! *                                    compatibility with bug in Calc.   *
! *                                                                      *
! *   Keep in mind: AXOF_CRS, AXOF_CRS_RATE and AXOF_UP are always       *
! *   computed according to instrinsic formula, which is considered      *
! *   as a correct way of computations.                                  *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      VTD ( RECORD    ) -- Object which keeps configuration and data  *
! *                           related to VLBI Theoretical Delay (VTD)    *
! *                           package.                                   *
! *     ISTA ( INTEGER*4 ) -- Index of the station in the VTD station    *
! *                           list.                                      *
! *     ISOU ( INTEGER*4 ) -- Index of the source in the VTD source list.*
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *      AXOF_CRS ( REAL*8    ) -- Antenna's axis offset vector in the   *
! *                                celestial coordinate system.          *
! *                                Dimension: 3. Units: meters.          *
! * AXOF_CRS_RATE ( REAL*8    ) -- Rate of change of antenna's axis      *
! *                                offset vector in the celestial        *
! *                                coordinate system.                    *
! *                                Dimension: 3. Units: meter/sec.       *
! *       AXOF_UP ( REAL*8    ) -- Displacement of the point on the      *
! *                                moving antenna's axis in vertical     *
! *                                direction. This point is used for     *
! *                                modeling geometric path delay.        *
! *                                Units: meters.                        *
! *  AXOF_UP_RATE ( REAL*8    ) -- Rate of change of AXOF_UP.            *
! *                                Units: m/s.                           *
! *      AVEC_CRS ( REAL*8    ) -- Unit vector of the immovable antenna  *
! *                                axis.                                 *
! * AVEC_CRS_RATE ( REAL*8    ) -- Time derivative of AVEC_CRS in 1/s    *
! * UNIT_AXOF_CRS ( REAL*8    ) -- Antenna's unit axis offset vector in  *
! *                                the celestial coordinate system.      *
! *                                Dimension: 3. Units: d/l.             *
! * UNIT_AXOF_CRS_RATE ( REAL*8 ) -- Rate of change of antenna's axis    *
! *                                offset unit vector in the celestial   *
! *                                coordinate system.                    *
! *                                Dimension: 3. Units: sec^-1.          *
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
! *  ### 28-JAN-2004     VTD_AXOF  v4.4 (c)  L. Petrov  02-MAY-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  ISTA, ISOU, IUER
      REAL*8     AXOF_CRS(3), AXOF_CRS_RATE(3), AXOF_UP, AXOF_UP_RATE, &
     &           UNIT_AXOF_CRS(3), UNIT_AXOF_CRS_RATE(3)
      REAL*8     AVEC_REN(3), AVEC_TRS(3), AVEC_CRS(3), AVEC_CRS_RATE(3), &
     &           LAT, LNG, RZ_TRS(3), RZ_CRS(3), RZ_CRS_RATE(3), AXOF_TRS(3)
      REAL*8     VEC_TMP(3), SV, SA, SAR, SRA, SP(3), SP_RATE(3), &
     &           CTG_E, CTG_E_RATE, RZS_VEC(3), RZS_RATE(3), &
     &           RZS_NORM, RN
      REAL*8       AXOF__MIN
      PARAMETER  ( AXOF__MIN = 1.D-6 )
      INTEGER*4  J1, J2, IER
      REAL*8     EL_ABR, BEND_ANG, S_ABBR(3), AXOF_CRS_CALC(3), AVEC_TMP(3), &
     &           N_AIR
      REAL*8     DP_VV_V, SBEND_CALC
!
! --- Determine the unit vector of the fixed antenna's axis in local UEN
! --- coordinate system. It depends on mounting type
!
      IF ( VTD%STA(ISTA)%MOUNT_TYPE .EQ. 'AZEL' .OR. &
     &     VTD%STA(ISTA)%MOUNT_TYPE .EQ. 'NASM' .OR. &
     &     VTD%STA(ISTA)%MOUNT_TYPE .EQ. 'NASP' .OR. &
     &     VTD%STA(ISTA)%MOUNT_TYPE .EQ. 'NASR' .OR. &
     &     VTD%STA(ISTA)%MOUNT_TYPE .EQ. 'NASL' .OR. &
     &     VTD%STA(ISTA)%MOUNT_TYPE .EQ. 'BWG ' .OR. &
     &     VTD%STA(ISTA)%MOUNT_TYPE .EQ. 'GNSS'      ) THEN
!
! -------- Azimuthal mounting
!
           AVEC_REN(1) = 1.0D0
           AVEC_REN(2) = 0.0D0
           AVEC_REN(3) = 0.0D0
           CALL MUL_MV_IV_V ( 3, 3, VTD%STA(ISTA)%UEN_TO_TRS, 3, AVEC_REN, &
     &                        3, AVEC_TRS, IER )
        ELSE IF ( VTD%STA(ISTA)%MOUNT_TYPE .EQ. 'EQUA' ) THEN
!
! -------- Equatorial mounting
!
           AVEC_TRS(1) = 0.0D0
           AVEC_TRS(2) = 0.0D0
           AVEC_TRS(3) = 1.0D0
        ELSE IF ( VTD%STA(ISTA)%MOUNT_TYPE .EQ. 'X-YN' ) THEN
!
! -------- X-Y mounting in North direction
!
           AVEC_REN(1) = 0.0D0
           AVEC_REN(2) = 0.0D0
           AVEC_REN(3) = 1.0D0
           CALL MUL_MV_IV_V ( 3, 3, VTD%STA(ISTA)%UEN_TO_TRS, 3, AVEC_REN, &
     &                        3, AVEC_TRS, IER )
        ELSE IF ( VTD%STA(ISTA)%MOUNT_TYPE .EQ. 'X-YE' ) THEN
!
! -------- X-Y mounting in East direction
!
           AVEC_REN(1) = 0.0D0
           AVEC_REN(2) = 1.0D0
           AVEC_REN(3) = 0.0D0
           CALL MUL_MV_IV_V ( 3, 3, VTD%STA(ISTA)%UEN_TO_TRS, 3, AVEC_REN, &
     &                        3, AVEC_TRS, IER )
        ELSE IF ( VTD%STA(ISTA)%MOUNT_TYPE .EQ. 'RICH' ) THEN
!
! -------- Special case of Richmond antenna
!
           CALL GR_TAT ( '+39_03_36.0', LAT, IER )
           CALL GR_TAT ( '-00_07_12.0', LNG, IER )
           AVEC_REN(1) = DSIN(LAT)
           AVEC_REN(2) = DCOS(LAT)*DSIN(LNG)
           AVEC_REN(3) = DCOS(LAT)*DCOS(LNG)
           CALL MUL_MV_IV_V ( 3, 3, VTD%STA(ISTA)%UEN_TO_TRS, 3, AVEC_REN, &
     &                        3, AVEC_TRS, IER )
      END IF
!
! --- Rotate the vector of antenna's fixed axis to the celestial coordinate
! --- system
!
      CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, 3, AVEC_TRS, &
     &                      3, AVEC_CRS, IER )
      IF ( VTD%CONF%FL_RATE ) THEN
           CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS_DER1, 3, AVEC_TRS, &
     &                           3, AVEC_CRS_RATE, IER )
      END IF
!
! --- Compute the vector of local zenith in local UEN system
!
      RZ_TRS(1) =  DCOS(VTD%STA(ISTA)%LAT_GDT)*DCOS(VTD%STA(ISTA)%LONG)
      RZ_TRS(2) =  DCOS(VTD%STA(ISTA)%LAT_GDT)*DSIN(VTD%STA(ISTA)%LONG)
      RZ_TRS(3) =  DSIN(VTD%STA(ISTA)%LAT_GDT)
!
! --- ... and then rotate it to the celestial reference system
!
      CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, 3, RZ_TRS, 3, RZ_CRS, IER )
      IF ( VTD%CONF%FL_RATE ) THEN
           CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS_DER1, 3, RZ_TRS, &
     &                           3, RZ_CRS_RATE, IER )
      END IF
!
! --- Compute intermediate vectors for refraction computation
!
      CALL VM83 ( VTD%SOU(ISOU)%S_CRS, RZ_CRS,  VEC_TMP )
      CALL VM83 ( VEC_TMP, VTD%SOU(ISOU)%S_CRS, SP      )
      CALL NORM_VEC ( 3, SP, RN )
!
! --- CTG_E -- cotangent of elevation
!
      CTG_E = DSQRT ( 1.0D0 - DP_VV_V ( 3, RZ_CRS, VTD%SOU(ISOU)%S_CRS )**2 )/ &
     &                        DP_VV_V ( 3, RZ_CRS, VTD%SOU(ISOU)%S_CRS )
      SV = DP_VV_V ( 3, VTD%SOU(ISOU)%S_CRS, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART) )
      IF ( VTD%CONF%FL_RATE ) THEN
           CALL ADDC_VV ( 3, 1.0D0, RZ_CRS, &
     &                   -DP_VV_V ( 3, RZ_CRS, VTD%SOU(ISOU)%S_CRS ), &
     &                    VTD%SOU(ISOU)%S_CRS, RZS_VEC )
           CALL NORM_VEC ( 3, RZS_VEC, RZS_NORM )
           CALL ADDC_VV  ( 3, 1.0D0, RZ_CRS_RATE, &
     &                    -DP_VV_V ( 3, RZ_CRS_RATE, VTD%SOU(ISOU)%S_CRS ), &
     &                     VTD%SOU(ISOU)%S_CRS, RZS_RATE )
           CALL ADDC_VV ( 3,  1.0D0/RZS_NORM,                  RZS_RATE, &
     &                   -DP_VV_V ( 3, RZS_VEC, RZS_RATE )/RZS_NORM, RZS_VEC, &
     &                    SP_RATE )
           CTG_E_RATE = -DP_VV_V ( 3, RZ_CRS_RATE, VTD%SOU(ISOU)%S_CRS )/ &
     &        ( DP_VV_V ( 3, RZ_CRS, VTD%SOU(ISOU)%S_CRS )**2* &
     &          DSQRT ( 1.0D0 - DP_VV_V ( 3, RZ_CRS, VTD%SOU(ISOU)%S_CRS )**2 ))
           SA = DP_VV_V ( 3, VTD%SOU(ISOU)%S_CRS, VTD%MOM%PLAN(1,VTD__ACC,VTD__EART) )
      END IF
!
! --- Compute SAPP_CRS -- apparent vector on the source corrected for annual
! --- aberration and refraction
!
      DO 410 J1=1,3
         VTD%SOU(ISOU)%SAPP_CRS(J1) =   VTD%MOM%PLAN(1,VTD__VEL,VTD__EART)/VTD__C       &
     &                                - SV*VTD%SOU(ISOU)%S_CRS(J1)/VTD__C               &
     &                                + DCOS ( VTD__RFR*CTG_E )*VTD%SOU(ISOU)%S_CRS(J1) &
     &                                + DSIN ( VTD__RFR*CTG_E )*SP(J1)
         IF ( VTD%CONF%FL_RATE ) THEN
              VTD%SOU(ISOU)%SAPP_CRS_RATE(J1) =  &
     &             VTD%MOM%PLAN(1,VTD__ACC,VTD__EART)/VTD__C            &
     &           - SA*VTD%SOU(ISOU)%S_CRS(J1)/VTD__C                    &
     &           - DSIN ( VTD__RFR*CTG_E )*CTG_E_RATE*VTD__RFR*         &
     &                                     VTD%SOU(ISOU)%S_CRS(J1)      &
     &           + DSIN ( VTD__RFR*CTG_E )*SP_RATE(J1)                  &
     &           + DCOS ( VTD__RFR*CTG_E )*CTG_E_RATE*VTD__RFR*SP(J1)
         END IF
 410  CONTINUE
      CALL NORM_VEC ( 3, VTD%SOU(ISOU)%SAPP_CRS, RN )
!
! --- Finally compute antenna's axis offset
!
      CALL VM83 ( VTD%SOU(ISOU)%SAPP_CRS, AVEC_CRS, VEC_TMP )
      CALL VM83 ( AVEC_CRS, VEC_TMP, AXOF_CRS )
      CALL NORM_VEC ( 3, AXOF_CRS, RN )
      IF ( VTD%CONF%FL_RATE  .AND.  DABS(RN) > AXOF__MIN ) THEN
           SA  = DP_VV_V ( 3, VTD%SOU(ISOU)%SAPP_CRS,      AVEC_CRS      )
           SRA = DP_VV_V ( 3, VTD%SOU(ISOU)%SAPP_CRS_RATE, AVEC_CRS      )
           SAR = DP_VV_V ( 3, VTD%SOU(ISOU)%SAPP_CRS,      AVEC_CRS_RATE )
           DO 420 J2=1,3
              VEC_TMP(J2) =                                      &
     &                        VTD%SOU(ISOU)%SAPP_CRS_RATE(J2)/RN &
     &                      - SA*  AVEC_CRS_RATE(J2)/RN          &
     &                      - SRA* AVEC_CRS(J2)/RN               &
     &                      - SAR* AVEC_CRS(J2)/RN
 420       CONTINUE 
!
           CALL ADDC_VV ( 3, 1.0D0,                            VEC_TMP,  &
     &                      -DP_VV_V ( 3, AXOF_CRS, VEC_TMP ), AXOF_CRS, &
     &                       AXOF_CRS_RATE )
           CALL COPY_R8  ( 3, AXOF_CRS_RATE, UNIT_AXOF_CRS_RATE )
           CALL MUL_VC_V ( 3, AXOF_CRS_RATE, VTD%STA(ISTA)%AXIS_OFFSET )
         ELSE 
           AXOF_CRS_RATE(1) = 0.0D0
           AXOF_CRS_RATE(2) = 0.0D0
           AXOF_CRS_RATE(3) = 0.0D0
      END IF
      CALL COPY_R8 ( 3, AXOF_CRS, UNIT_AXOF_CRS )
      CALL MUL_VC_V ( 3, AXOF_CRS, VTD%STA(ISTA)%AXIS_OFFSET )
!
      IF ( VTD%CONF%AXOF_MODEL == VTD__CALC ) THEN
!
! -------- Special trick for compatibility with a bug in Calc
!
! -------- Store contribution to delay due to antenna axis offset at this
! -------- station according to correct model
!
           VTD%STA(ISTA)%DELAY_AXOF_VTD = &
     &             DP_VV_V ( 3, AXOF_CRS, VTD%SOU(ISOU)%S_CRS )/VTD__C/ &
     &             (1.0D0 + SV/VTD__C)
!
! -------- Compute aberrate unit source vector (S_ABRR)
!
           CALL ADDC_VV ( 3, 1.D0/VTD__C, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                   -SV/VTD__C, VTD%SOU(ISOU)%S_CRS, S_ABBR )
           CALL ADD_VV   ( 3, S_ABBR, VTD%SOU(ISOU)%S_CRS )
           CALL NORM_VEC ( 3, S_ABBR, RN )
!
! -------- Compute the elecation of the abberated source
!
           EL_ABR = DASIN ( DP_VV_V ( 3, S_ABBR, RZ_CRS ) )
!
! -------- Comppute bending angle due to refraction
!
           BEND_ANG = SBEND_CALC ( &
     &           EL_ABR, &
     &           293.15D0 - 6.5D-3*VTD%STA(ISTA)%HEI_ELL, &
     &           0.5D0, &
     &           760.0D0*(1.D0 - 6.5D-3*VTD%STA(ISTA)%HEI_ELL/293.15D0)**5.26D0 )
!
! ------- Compute apparent source position vector which takes into account
! ------- both annual aberration and refraction (SAPP)
!
          CALL ADDC_VV ( 3, DCOS ( BEND_ANG ), S_ABBR, &
     &                      DSIN ( BEND_ANG ), SP,     VTD%SOU(ISOU)%SAPP_CRS )
!
! ------- Compute vector an antenna offsetin CRS (AXOF_CRS_CALC)
!
          CALL VM83 ( VTD%SOU(ISOU)%SAPP_CRS, AVEC_CRS, VEC_TMP )
          CALL VM83 ( AVEC_CRS, VEC_TMP, AXOF_CRS_CALC )
          CALL NORM_VEC ( 3, AXOF_CRS_CALC, RN )
          CALL MUL_VC_V ( 3, AXOF_CRS_CALC, VTD%STA(ISTA)%AXIS_OFFSET )
!
! ------- Compute air index of refraction
!
          N_AIR = 77.6D-6*1013.25D0* &
     &            (1.D0 - 6.5D-3*VTD%STA(ISTA)%HEI_ELL/293.15D0)**5.26D0/ &
     &            (293.15D0 - 6.5D-3*VTD%STA(ISTA)%HEI_ELL) + 1.D0
!
! ------- Finally compute contribution to delay due to antenna axis offset
!
          VTD%STA(ISTA)%DELAY_AXOF_CALC = DP_VV_V ( 3, AXOF_CRS_CALC, &
     &                                              VTD%SOU(ISOU)%SAPP_CRS )/ &
     &                                    VTD__C*N_AIR
      END IF
!
! --- Now compute projection of the anntenna's axis offset into local zenith
! --- This will be needed later for computing a coupling term for geometric
! --- delay
!
      IF ( VTD%STA(ISTA)%MOUNT_TYPE .EQ. 'AZEL' .OR. &
     &     VTD%STA(ISTA)%MOUNT_TYPE .EQ. 'GNSS'      ) THEN
           AXOF_UP = 0.0D0 ! of course, for azimuthal mounting
         ELSE
           CALL MUL_MV_TV_V  ( 3, 3, VTD%MOM%TRS_TO_CRS, 3, AXOF_CRS, &
     &                         3, AXOF_TRS, IER )
           AXOF_UP = DP_VV_V ( 3, AXOF_TRS, RZ_TRS )
           IF ( VTD%CONF%FL_RATE ) THEN
                CALL MUL_MV_TV_V  ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                                 3, AXOF_CRS_RATE, &
     &                                 3, AVEC_TMP, IER )
                AXOF_UP_RATE = DP_VV_V ( 3, AVEC_TMP, RZ_TRS )
!
                CALL MUL_MV_TV_V  ( 3, 3, VTD%MOM%TRS_TO_CRS_DER1, &
     &                                 3, AXOF_CRS, &
     &                                 3, AVEC_TMP, IER )
                AXOF_UP_RATE = AXOF_UP_RATE + DP_VV_V ( 3, AVEC_TMP, RZ_TRS )
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  VTD_AXOF  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION SBEND_CALC ( EL_RAD, TEMP_K, HUMID_F, PRESS_HG )
! ************************************************************************
! *                                                                      *
! *   Enigmatic routine. Nobody knows what is its origin. According to   *
! *   rumours it computes refercation angle. This rroutine was used by   *
! *   Calc package.                                                      *
! *                                                                      *
! *  ### 08-OCT-2004   SBEND_CALC  v1.0 (c)  L. Petrov  08-OCT-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT NONE
!
! input:
!   El_rad   -- elevation angle in radians
!   Press_Hg -- Pressure in mm of Mercury (Hg)
!   TEMP_K   -- Temperature in Kelvins
!   HUMID_F  -- relative humidity (percent)
!
! output   --
!   Sbend  -- bending angle in radians.
!
      REAL*8 SBEND_CALC
      Real*8 EL_RAD, TEMP_K, HUMID_F, PRESS_HG
!
      Real*8 e(12),wp1(4),d3
      Real*8 fp,ft,fw,u,x,ad1,ad2,bd1,bd2,zd2,r,delta
      Real*8 a1,a2,b1,b2,c1,c2,e1,e2,e3,e4,e5,e6,e7,e8,e9
      Real*8 e10,e11,e12,p1,p2,t1,t2,z1,z2,w0,w1,w2,w3
      Integer*4 I
!
      equivalence (e( 1), e1),(e( 2), e2),(e( 3), e3),(e( 4), e4), &
     &            (e( 5), e5),(e( 6), e6),(e( 7), e7),(e( 8), e8), &
     &            (e( 9), e9),(e(10),e10),(e(11),e11),(e(12),e12)
      equivalence (wp1(1),w0),(wp1(2),w1),(wp1(3),w2),(wp1(4),w3)
!
      data a1, a2 /     0.40816d0, 112.30d0  /
      data b1, b2 /     0.12820d0, 142.88d0  /
      data c1, c2 /     0.80000d0,  99.344d0 /
      data e   /    46.625d0  ,  45.375d0 ,     4.1572d0,  1.4468d0  , &
     &               0.25391d0,   2.2716d0,    -1.3465d0, -4.3877d0  , &
     &               3.1484d0 ,   4.5201d0,    -1.8982d0,  0.89000d0 /
      data p1 /   760.0d0 /
      data t1 /   273.0d0 /
      data wp1 / 22000.0d0    ,  17.149d0 ,  4684.1d0,    38.450d0   /
      data z1 /  91.870d0 /
!
!      Real*8         PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY
!      COMMON /CMATH/ PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY
!           VARIABLES 'FROM':
!              1. HALFPI - THE VALUE OF PI/2
!              2. CONVD  - THE CONVERSION FACTOR FROM DEGREES TO RADIANS
!                          (RAD/DEG)
!
! STATEMENT FUCNTION
!
      REAL*8     CONVD
      DELTA(AD1,AD2,BD1,BD2,ZD2)=(AD2-AD1)*DEXP(BD1*(ZD2-BD2))
!
! CONVERT UNITS
! --- Zenith angle in degrees
!
      Z2 = 90.0D0 - EL_RAD*57.295779512D0
!
! --- Temperature in Kelvins
!
      T2 = TEMP_K
!
! --- Fractional humidity (0.0 -> 1.0)
!
      R = HUMID_F
!
! --- Pressure in mm of Hg
!
      P2 = PRESS_HG
!
! --- Calculate corrections for pres, temp, and wetness
!
      D3 = 1.0D0 + DELTA(Z1,Z2,C1,C2,Z2)
      FP = (P2/P1) * ( 1.0D0 - DELTA(P1,P2,A1,A2,Z2)/D3 )
      FT = (T1/T2) * ( 1.0D0 - DELTA(T1,T2,B1,B2,Z2)/D3 )
      FW = 1.0D0 + W0*R*DEXP((W1*T2-W2)/(T2-W3)) / (T2*P2)
!
! --- Calculate optical refraction
!
      U=(Z2-E1)/E2
      X=E11
      DO 10 I=1,8
        X = E(11-I) + U*X
  10  CONTINUE
!
! --- Combine factors and finish optical factor
!
      SBEND_CALC = FT*FP*FW*( DEXP(X/D3) - E12 )
!
! --- Back to radians from arc seconds
!@      sbend=(sbend/3600.0d0)*CONVD
!
      SBEND_CALC = SBEND_CALC/206264.806D0
!
      RETURN
      END  FUNCTION  SBEND_CALC
