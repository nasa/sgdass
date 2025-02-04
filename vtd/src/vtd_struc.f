      SUBROUTINE VTD_STRUC ( ISTA1, ISTA2, ISOU, VTD, OBS_TYP, &
     &                       DELAY_STR, RATE_STR, CORR_FD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_STRUC  
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     ISTA1 ( INTEGER*4 ) -- Index in the list of station of the first *
! *                            station of the baseline.                  *
! *     ISTA2 ( INTEGER*4 ) -- Index in the list of station of the       *
! *                            second station of the baseline.           *
! *      ISOU ( INTEGER*4 ) -- Index in the list of sources of the       *
! *                            source under consideration.               *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
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
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * DELAY_STR  ( REAL*8    ) -- VLBI time delay caused by source         *
! *                             structure. Units: seconds.               *
! * DELAY_RATE ( REAL*8    ) -- VLBI delay rate caused by source         *
! *                             structure. Units: seconds.               *
! * CORR_FD    ( REAL*8    ) -- Correlated flux density. Unit: Jy.       *
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
! *  ### 27-FEB-2004    VTD_STRUC  v2.4 (c)  L. Petrov  26-JUL-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      INTEGER*4  ISTA1, ISTA2, ISOU, IUER
      REAL*8     DELAY_STR, RATE_STR, CORR_FD(2)
      TYPE     ( VTD__TYPE      ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      REAL*8     STEP_UV, Z_VEC(3), X_VEC(3), Y_VEC(3), &
     &           B_CRS(3), RD, BX, BY, FRQ__MIN
      PARAMETER  ( FRQ__MIN = 1.D7 ) 
      CHARACTER  STR*128
      REAL*8     BX_PIXEL, BY_PIXEL
      REAL*8     GR_DEL_HIGH, GR_DEL_LOW, FREQ_GR_HIGH, FREQ_GR_LOW, &
     &           PH_DEL_HIGH, PH_DEL_LOW, FREQ_PH_HIGH, FREQ_PH_LOW
      LOGICAL*1  FL_TEST, FL_PX, FL_GX
      INTEGER*4  IND_MAP_HIGH, IND_MAP_LOW, DELAY_TYPE_SAVED, IER
      REAL*8,    EXTERNAL :: DP_VV_V, VTD_GR_DEL_STRUC, VTD_PH_DEL_STRUC, &
     &                       VTD_CORR_AMP 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      FL_TEST   = .FALSE.
!
      DELAY_STR = 0.0D0
      RATE_STR  = 0.0D0
      CORR_FD   = 0.0D0
#ifdef VTD__NO_STRUC
      CALL ERR_LOG ( 2611, IUER, 'VTD_STRUC', 'You are attempting to '// &
     &              'compute contribution to delay caused by the source '// &
     &              'structure, but your VTD library was configured '// &
     &              'without support for this computation. Please either '//&
     &              'set SOURCE_STRUCTURE: NONE in your vtd configuration '// &
     &              'file '// &
     &              VTD%CONF%CONFIG_FINAM(1:I_LEN(VTD%CONF%CONFIG_FINAM))// &
     &              ' or re-install VTD library with option '// &
     &              '--with-cfitsio={directory} and re-link your application' )
      RETURN 
#else
!
      FL_PX = .FALSE.
      FL_GX = .FALSE.
      CALL GETENVAR ( 'VTD_DEBUG_STRUC', STR )
      IF ( ILEN(STR) > 0 ) THEN
           IF ( STR == 'AS_PX' ) THEN
                FL_PX   = .TRUE.
                FL_TEST = .TRUE.
           END IF
           IF ( STR == 'AS_GX' ) THEN
                FL_GX   = .TRUE.
                FL_TEST = .TRUE.
           END IF
      END IF
!
      IF ( FL_PX ) THEN
           DELAY_TYPE_SAVED   = OBS_TYP%DELAY_TYPE
           OBS_TYP%DELAY_TYPE = VTD__PH__DTP
      END IF
      IF ( FL_GX ) THEN
           DELAY_TYPE_SAVED   = OBS_TYP%DELAY_TYPE
           OBS_TYP%DELAY_TYPE = VTD__MH__DTP
      END IF
!
! --- Vector to the northern pole of the celestiral coordinate system
!
      Z_VEC(1) = 0.0D0
      Z_VEC(2) = 0.0D0
      Z_VEC(3) = 1.0D0
!
      CALL VM83 ( VTD%SOU(ISOU)%S_CRS, Z_VEC, X_VEC ) 
      CALL NORM_VEC ( 3, X_VEC, RD ) 
!
      CALL VM83 ( VTD%SOU(ISOU)%S_CRS, X_VEC, Y_VEC ) 
      CALL NORM_VEC ( 3, Y_VEC, RD ) 
!
! --- Compute the baseline vector
!
      CALL SUB_VV_V ( 3, VTD%STA(ISTA2)%COO_CRS, VTD%STA(ISTA1)%COO_CRS, B_CRS )
!
! --- Compute projection of the baseline vector to the image plane
!
      BX = DP_VV_V ( 3, X_VEC, B_CRS )
      BY = DP_VV_V ( 3, Y_VEC, B_CRS )
      VTD%UV_COOR(1) = BX
      VTD%UV_COOR(2) = BY
!
      IF ( VTD%SOU(ISOU)%MAP_USAGE_CODE(1) == STRUC__DEL ) THEN
         IF ( OBS_TYP%DELAY_TYPE == VTD__MLMH__DTP      .AND.  &
     &        OBS_TYP%FRQ_ION_EFF(1) > FRQ__MIN         .AND.  &
     &        OBS_TYP%FRQ_ION_EFF(2) > FRQ__MIN         .AND.  &
     &        VTD%SOU(ISOU)%FL_STRUC(1)                 .AND.  &
     &        VTD%SOU(ISOU)%FL_STRUC(2)                 .AND.  &
     &        VTD%SOU(ISOU)%MAP_USAGE_CODE(2) == STRUC__DEL    ) THEN
!
! ----------- Determine which effecitve ionosphere frequency corresponds to 
! ----------- the high frequency, and which corresponds to the low frequency
!
              IF ( OBS_TYP%FRQ_ION_EFF(1) > OBS_TYP%FRQ_ION_EFF(2) ) THEN
                   FREQ_GR_HIGH = OBS_TYP%FRQ_ION_EFF(1) 
                   FREQ_GR_LOW  = OBS_TYP%FRQ_ION_EFF(2) 
                 ELSE
                   FREQ_GR_HIGH = OBS_TYP%FRQ_ION_EFF(2) 
                   FREQ_GR_LOW  = OBS_TYP%FRQ_ION_EFF(1) 
              END IF
!
! ----------- Determine which map corresponds to the low frequency and which 
! ----------- corresponds to the high frequency
!
              IF ( VTD%SOU(ISOU)%MAP(1)%FREQ > VTD%SOU(ISOU)%MAP(2)%FREQ ) THEN
                   IND_MAP_HIGH = 1
                   IND_MAP_LOW  = 2
                 ELSE 
                   IND_MAP_HIGH = 2
                   IND_MAP_LOW  = 1
              END IF
!
! ----------- Compute contribution of source structure to the group delay of 
! ----------- the high frequency
!
              CALL ERR_PASS ( IUER, IER )
              GR_DEL_HIGH = VTD_GR_DEL_STRUC ( BX, BY, FREQ_GR_HIGH, &
     &                                 VTD%SOU(ISOU)%MAP(IND_MAP_HIGH), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2611, IUER, 'VTD_STRUC', 'Error in an '// &
     &                 'attempt to compute contribution to delay from the '// &
     &                 'map for the high frequency for source '// &
     &                  VTD%SOU(ISOU)%IVS_NAME )
                   RETURN 
              END IF
!
! ----------- Compute contribution of source structure to the group delay of 
! ----------- the low frequency
!
              CALL ERR_PASS ( IUER, IER )
              GR_DEL_LOW = VTD_GR_DEL_STRUC ( BX, BY, FREQ_GR_LOW, &
     &                                VTD%SOU(ISOU)%MAP(IND_MAP_LOW), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2612, IUER, 'VTD_STRUC', 'Error in an '// &
     &                 'attempt to compute contribution to delay from the '// &
     &                 'map for the low frequency for source '// &
     &                  VTD%SOU(ISOU)%IVS_NAME )
                   RETURN 
              END IF
!
! ----------- Compute ionosphere free combination of high and low 
! ----------- structure delays
!
              DELAY_STR = &
     &           ( GR_DEL_HIGH*FREQ_GR_HIGH**2 - GR_DEL_LOW*FREQ_GR_LOW**2 )/ &
     &           (             FREQ_GR_HIGH**2 -            FREQ_GR_LOW**2 ) 
              RATE_STR  = 0.0D0 ! Temporarily
              IF ( FL_TEST ) THEN
                   WRITE ( 6, 110 ) VTD%STA(ISTA1)%IVS_NAME, &
     &                              VTD%STA(ISTA2)%IVS_NAME, &
     &                              VTD%SOU(ISOU)%IVS_NAME, &
     &                              1.D12*GR_DEL_HIGH, 1.D12*GR_DEL_LOW, 1.D12*DELAY_STR, &
     &                              OBS_TYP%FRQ_ION_EFF(1)
 110               FORMAT ( 'VTD_STRUC: ', A, ' / ', A, 2X, A, 2X, &
                             ' Delay_str (high/low/dual): ', 3(F9.3,1X), ' ps Frq= ', F16.1 )
              END IF
!
! ----------- Special test modes:
!
              IF ( VTD%CONF%TEST(1) == 2  .AND. VTD%CONF%TEST(2) == 0 ) THEN
                   IER = -1
                   PH_DEL_HIGH = VTD_PH_DEL_STRUC ( BX, BY, FREQ_GR_HIGH, &
     &                                   VTD%SOU(ISOU)%MAP(IND_MAP_HIGH), IER )
                   IF ( IER .NE. 0 ) CALL EXIT ( 1 )
!
                   IER = -1
                   PH_DEL_LOW  = VTD_PH_DEL_STRUC ( BX, BY, FREQ_GR_LOW, &
     &                                   VTD%SOU(ISOU)%MAP(IND_MAP_LOW), IER )
                   IF ( IER .NE. 0 ) CALL EXIT ( 1 )
!
                   DELAY_STR = &
     &               ( PH_DEL_HIGH*FREQ_GR_HIGH**2 - PH_DEL_LOW*FREQ_GR_LOW**2 )/ &
     &               (             FREQ_GR_HIGH**2 -            FREQ_GR_LOW**2 ) 
                 ELSE IF ( VTD%CONF%TEST(1) == 2  .AND. VTD%CONF%TEST(2) == 1 ) THEN
                   IER = -1
                   DELAY_STR = VTD_PH_DEL_STRUC ( BX, BY, FREQ_GR_HIGH, &
     &                                   VTD%SOU(ISOU)%MAP(IND_MAP_HIGH), IER )
                   IF ( IER .NE. 0 ) CALL EXIT ( 1 )
                 ELSE IF ( VTD%CONF%TEST(1) == 2  .AND. VTD%CONF%TEST(2) == 1 ) THEN
                   IER = -1
                   DELAY_STR = VTD_PH_DEL_STRUC ( BX, BY, FREQ_GR_LOW, &
     &                                   VTD%SOU(ISOU)%MAP(IND_MAP_LOW), IER )
                   IF ( IER .NE. 0 ) CALL EXIT ( 1 )
              END IF
!
              IF ( VTD%CONF%TEST(1) == 0  .AND. VTD%CONF%TEST(2) == 1 ) THEN
                   DELAY_STR = GR_DEL_HIGH
              END IF
              IF ( VTD%CONF%TEST(1) == 0  .AND. VTD%CONF%TEST(2) == 2 ) THEN
                   DELAY_STR = GR_DEL_LOW
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CORR_FD(IND_MAP_HIGH) = VTD_CORR_AMP ( BX, BY, FREQ_GR_HIGH, &
     &                                               VTD%SOU(ISOU)%MAP(IND_MAP_HIGH), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2614, IUER, 'VTD_STRUC', 'Error in an '// &
     &                 'attempt to compute correlated flux density at '// &
     &                 'the high frequency for source '// &
     &                  VTD%SOU(ISOU)%IVS_NAME )
                   RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CORR_FD(IND_MAP_LOW)  = VTD_CORR_AMP ( BX, BY, FREQ_GR_LOW, &
     &                                               VTD%SOU(ISOU)%MAP(IND_MAP_LOW), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2615, IUER, 'VTD_STRUC', 'Error in an '// &
     &                 'attempt to compute correlated flux density at '// &
     &                 'the low frequency for source '// &
     &                  VTD%SOU(ISOU)%IVS_NAME )
                   RETURN 
              END IF
            ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__ML__DTP ) THEN
!
! ----------- Determine which map corresponds to the low frequency and which 
! ----------- corresponds to the high frequency
!
              IF ( ILEN(VTD%SOU(ISOU)%MAP(1)%FINAM) > 1 .AND. &
     &             ILEN(VTD%SOU(ISOU)%MAP(2)%FINAM) > 1       ) THEN
                   IF ( VTD%SOU(ISOU)%MAP(1)%FREQ > VTD%SOU(ISOU)%MAP(2)%FREQ ) THEN
                        IND_MAP_HIGH = 1
                        IND_MAP_LOW  = 2
                      ELSE 
                        IND_MAP_HIGH = 2
                        IND_MAP_LOW  = 1
                   END IF
                 ELSE  
                   IND_MAP_LOW = 1
              END IF
!
              IF ( OBS_TYP%FRQ_ION_EFF(2) > 0.0D0 ) THEN
                   IF ( OBS_TYP%FRQ_ION_EFF(1) > OBS_TYP%FRQ_ION_EFF(2) ) THEN
                        FREQ_GR_HIGH = OBS_TYP%FRQ_ION_EFF(1) 
                        FREQ_GR_LOW  = OBS_TYP%FRQ_ION_EFF(2) 
                      ELSE
                        FREQ_GR_HIGH = OBS_TYP%FRQ_ION_EFF(2) 
                        FREQ_GR_LOW  = OBS_TYP%FRQ_ION_EFF(1) 
                   END IF
                 ELSE 
                   FREQ_GR_LOW  = OBS_TYP%FRQ_ION_EFF(1) 
              END IF
!
              IF ( VTD%SOU(ISOU)%FL_STRUC(IND_MAP_LOW)                  .AND.  &
     &             VTD%SOU(ISOU)%MAP_USAGE_CODE(IND_MAP_LOW) == STRUC__DEL     ) THEN
!
! ---------------- Single band case
!
                   CALL ERR_PASS ( IUER, IER )
                   DELAY_STR = VTD_GR_DEL_STRUC ( BX, BY, OBS_TYP%FRQ_ION_EFF(IND_MAP_LOW) , &
     &                                            VTD%SOU(ISOU)%MAP(IND_MAP_LOW), IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 2613, IUER, 'VTD_STRUC', 'Error in an '// &
     &                      'attempt to compute contribution to delay from the '// &
     &                      'map for the high frequency for source '// &
     &                       VTD%SOU(ISOU)%IVS_NAME )
                        RETURN 
                   END IF
                   CALL ERR_PASS ( IUER, IER )
                   CORR_FD(1) = 0.0
                   CORR_FD(2) = VTD_CORR_AMP ( BX, BY, OBS_TYP%FRQ_ION_EFF(IND_MAP_LOW), &
     &                                          VTD%SOU(ISOU)%MAP(IND_MAP_LOW), IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 2615, IUER, 'VTD_STRUC', 'Error in an '// &
     &                      'attempt to compute correlated flux density at '// &
     &                      'the low frequency for source '// &
     &                       VTD%SOU(ISOU)%IVS_NAME )
                        RETURN 
                   END IF
                   IF ( FL_TEST ) THEN
                        WRITE ( 6, 120 ) VTD%STA(ISTA1)%IVS_NAME, &
     &                                   VTD%STA(ISTA2)%IVS_NAME, &
     &                                   VTD%SOU(ISOU)%IVS_NAME, &
     &                                   1.D12*DELAY_STR, &
     &                                   FREQ_GR_LOW
 120                    FORMAT ( 'VTD_STRUC: ', A, ' / ', A, 2X, A, 2X, &
                                  ' Delay_str: ', F8.2, ' ps Frq= ', F16.1 )
                   END IF
                 ELSE
                   DELAY_STR = 0.0D0
              END IF
            ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__MH__DTP .OR. &
     &                OBS_TYP%DELAY_TYPE == VTD__PH__DTP      ) THEN
!
! ----------- Determine which map corresponds to the low frequency and which 
! ----------- corresponds to the high frequency
!
              IF ( ILEN(VTD%SOU(ISOU)%MAP(1)%FINAM) > 1 .AND. &
     &             ILEN(VTD%SOU(ISOU)%MAP(2)%FINAM) > 1       ) THEN
                   IF ( VTD%SOU(ISOU)%MAP(1)%FREQ > VTD%SOU(ISOU)%MAP(2)%FREQ ) THEN
                        IND_MAP_HIGH = 1
                        IND_MAP_LOW  = 2
                      ELSE 
                        IND_MAP_HIGH = 2
                        IND_MAP_LOW  = 1
                   END IF
                 ELSE  
                   IND_MAP_HIGH = 1
              END IF
!
              IF ( OBS_TYP%FRQ_ION_EFF(2) > 0.0D0 ) THEN
                   IF ( OBS_TYP%FRQ_ION_EFF(1) > OBS_TYP%FRQ_ION_EFF(2) ) THEN
                        FREQ_GR_HIGH = OBS_TYP%FRQ_ION_EFF(1) 
                        FREQ_GR_LOW  = OBS_TYP%FRQ_ION_EFF(2) 
                      ELSE
                        FREQ_GR_HIGH = OBS_TYP%FRQ_ION_EFF(2) 
                        FREQ_GR_LOW  = OBS_TYP%FRQ_ION_EFF(1) 
                   END IF
                 ELSE 
                   FREQ_GR_HIGH  = OBS_TYP%FRQ_ION_EFF(1) 
              END IF
              IF ( VTD%SOU(ISOU)%FL_STRUC(IND_MAP_HIGH)                  .AND.  &
     &             VTD%SOU(ISOU)%MAP_USAGE_CODE(IND_MAP_HIGH) == STRUC__DEL     ) THEN
!
! ---------------- Single band case
!
                   CALL ERR_PASS ( IUER, IER )
                   IF ( OBS_TYP%DELAY_TYPE == VTD__MH__DTP ) THEN
                        DELAY_STR = VTD_GR_DEL_STRUC ( BX, BY, OBS_TYP%FRQ_ION_EFF(IND_MAP_HIGH) , &
     &                                                 VTD%SOU(ISOU)%MAP(IND_MAP_HIGH), IER )
                     ELSE IF ( OBS_TYP%DELAY_TYPE == VTD__PH__DTP ) THEN
                        DELAY_STR = VTD_PH_DEL_STRUC ( BX, BY, OBS_TYP%FRQ_ION_EFF(IND_MAP_HIGH) , &
     &                                                 VTD%SOU(ISOU)%MAP(IND_MAP_HIGH), IER )
                   END IF
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 2613, IUER, 'VTD_STRUC', 'Error in an '// &
     &                      'attempt to compute contribution to delay from the '// &
     &                      'map for the high frequency for source '// &
     &                       VTD%SOU(ISOU)%IVS_NAME )
                        RETURN 
                   END IF
!
                   CALL ERR_PASS ( IUER, IER )
                   CORR_FD(1) = VTD_CORR_AMP ( BX, BY, OBS_TYP%FRQ_ION_EFF(IND_MAP_HIGH), &
     &                                          VTD%SOU(ISOU)%MAP(IND_MAP_HIGH), IER )
                   CORR_FD(2) = 0.0
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 2615, IUER, 'VTD_STRUC', 'Error in an '// &
     &                      'attempt to compute correlated flux density at '// &
     &                      'the high frequency for source '// &
     &                       VTD%SOU(ISOU)%IVS_NAME )
                        RETURN 
                   END IF
!
                   IF ( FL_TEST ) THEN
                        WRITE ( 6, 120 ) VTD%STA(ISTA1)%IVS_NAME, &
     &                                   VTD%STA(ISTA2)%IVS_NAME, &
     &                                   VTD%SOU(ISOU)%IVS_NAME, &
     &                                   1.D12*DELAY_STR, &
     &                                   FREQ_GR_HIGH 
                   END IF
              END IF
         END IF
      END IF
      IF ( FL_PX .OR. FL_GX ) THEN
           OBS_TYP%DELAY_TYPE = DELAY_TYPE_SAVED   
      END IF
!
#endif
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE VTD_STRUC  !#!  
