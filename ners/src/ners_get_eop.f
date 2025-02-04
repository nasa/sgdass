      SUBROUTINE NERS_GET_EOP ( NERS, TIM_TAI, CPAR, M_PAR, L_PAR, PARS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  NERS_GET_EOP computes selected EOP using Network          *
! *   Earth Rotation Service. Routine NERS_INIT should be executed       *
! *   before the first call of NERS_GET_EOP. In a case if the range of   *
! *   time interval was supplied to NERS_INIT, NERS_GET_EOP will use     *
! *   the coefficients ofthe B-spline expansion of the Earth's           *
! *   orientation interpolation parameters over this interval. These     *
! *   coefficients are computed at the first call of NERS_GET_EOP.       *
! *   If NERS_INIT was called with parameters TIME_TAI_START and         *
! *   TIME_TAI_STOP equal to -1, then the Earth orientation parameters   *
! *   are computed directly.                                             *
! *                                                                      *
! *   The parameter(s) of the Earth orientation is specfied by keyword   *
! *   CPAR. NERS_GET_EOP return the vector PARS of parameters of length  *
! *   L_PAR. The output vector is sized to M_PAR elements. The maximum   *
! *   value of L_PAR is specified in constant NERS__MPAR in ners.i       *
! *   include block.                                                     *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * TIM_TAI ( REAL*8     ) -- Time for which the Earth orientation       *
! *                           parameter(s) is to be computed elapsed     *
! *                           since 2000.01.01_00:00:00.0 TAI. Unit: sec.*
! *                           If TIM_TAI .LE. -1.0D14, then the EOP on   *
! *                           the current moment of time will be         *
! *                           computed.                                  *
! *                                                                      *
! *                           NB: it is common to use tiem tag UTC.      *
! *                           The time tag UTC should be converted to    *
! *                           time in TAI scale prior call to            *
! *                           NERS_GET_EOP.                              *
! *    CPAR ( CHARACTER  ) -- The Earth orientationi parameter name.     *
! *                           Supported paraameters:                     *
! *                                                                      *
! *           mat     --  3x3 matrix of the transformation from the      *
! *                       terrestrial coordinate system to the celestial *
! *                       coordinate system. Unit: dimensionless.        *
! *                                                                      *
! *           matr    --  First time derivative of the 3x3 matrix of the *
! *                       transformation from the terrestrial coordinate *
! *                       system to the celestial coordinate system.     *
! *                       Unit: 1/s.                                     *
! *                                                                      *
! *           matrr   --  Second time derivative of the 3x3 matrix of    *
! *                       the transformation from the terrestrial        *
! *                       coordinate system to the celestial coordinate  *
! *                       system. Unit: 1/s^2.                           *
! *                                                                      *
! *           matall  --  3x3x3 array of the matrix of the               *
! *                       transformation from the terrestrial coordinate *
! *                       system to the celestial coordinate system,     *
! *                       its first and second time derivative.          *
! *                       [1:3,1:3,1] -- transformation matrix from the  *
! *                                      terrestrial coordinate system   *
! *                                      to the celestial coordinate     *
! *                                      system,                         *
! *                       [1:3,1:3,2] -- first time derivative of the    *
! *                                      transformation matrix above.    *
! *                       [1:3,1:3,3] -- second time derivative of the   *
! *                                      transformation matrix above.    *
! *                                                                      *
! *           utcmtai --  Value of function UTC minus TAI. Units: s.     *
! *                                                                      *
! *           e1      --  Euler  angle around axis one.   Units: rad.    *
! *                                                                      *
! *           e2      --  Euler  angle around axis two.   Units: rad.    *
! *                                                                      *
! *           e3      --  Euler  angle around axis three. Units: rad.    *
! *                                                                      *
! *           e       --  Euler angles.                                  *
! *                       1: Euler  angle around axis one.   Units: rad. *
! *                       2: Euler  angle around axis two.   Units: rad. *
! *                       3: Euler  angle around axis three. Units: rad. *
! *                                                                      *
! *           e1r     --  First  time derivative of Euler angle around   *
! *                       axis one.   Units: rad/s.                      *
! *                                                                      *
! *           e2r     --  First  time derivative of Euler angle around   *
! *                       axis two.   Units: rad/s.                      *
! *                                                                      *
! *           e3r     --  First  time derivative of Euler angle around   *
! *                       axis three. Units: rad/s.                      *
! *                                                                      *
! *           er      --  Euler angles and their first derivatives.      *
! *                       1: Euler  angle around axis one.   Units: rad. *
! *                       2: Euler  angle around axis two.   Units: rad. *
! *                       3: Euler  angle around axis three. Units: rad. *
! *                       4: First  time derivative of Euler angle       *
! *                          around axis one.   Units: rad/s.            *
! *                       5: First  time derivative of Euler angle       *
! *                          around axis two.   Units: rad/s.            *
! *                       6: First  time derivative of Euler angle       *
! *                          around axis three. Units: rad/s.            *
! *                                                                      *
! *           e1rr    --  Second time derivative of Euler angle around   *
! *                       axis one.   Units: rad/s**2.                   *
! *                                                                      *
! *           e2rr    --  Second time derivative of Euler angle around   *
! *                       axis two.   Units: rad/s**2.                   *
! *                                                                      *
! *           e3rr    --  Second time derivative of Euler angle around   *
! *                       axis three. Units: rad/s**2.                   *
! *                                                                      *
! *           err     --  Euler angles and their first and second        *
! *                       derivatives.                                   *
! *                       1: Euler  angle around axis one.   Units: rad. *
! *                       2: Euler  angle around axis two.   Units: rad. *
! *                       3: Euler  angle around axis three. Units: rad. *
! *                       4: First  time derivative of Euler angle       *
! *                          around axis one.   Units: rad/s.            *
! *                       5: First  time derivative of Euler angle       *
! *                          around axis two.   Units: rad/s.            *
! *                       6: First  time derivative of Euler angle       *
! *                          around axis three. Units: rad/s.            *
! *                       7: Second time derivative of Euler angle       *
! *                          around axis one.   Units: rad/s**2.         *
! *                       8: Second time derivative of Euler angle       *
! *                          around axis two.   Units: rad/s**2.         *
! *                       9: First  time derivative of Euler angle       *
! *                          Second axis three. Units: rad/s**2.         *
! *                                                                      *
! *           ezrr    --  Euler angles and their first and second        *
! *                       derivatives.                                   *
! *                       1: Euler  angle around axis one.   Units: rad. *
! *                       2: Euler  angle around axis two.   Units: rad. *
! *                       3: Euler  angle around axis three with the     *
! *                          contribution of zonal tides removed.        *
! *                          Units: rad.                                 *
! *                       4: First  time derivative of Euler angle       *
! *                          around axis one.   Units: rad/s.            *
! *                       5: First  time derivative of Euler angle       *
! *                          around axis two.   Units: rad/s.            *
! *                       6: First  time derivative of Euler angle       *
! *                          around axis three with the contribution of  *
! *                          zonal tides removed. Units: rad/s.          *
! *                       7: Second time derivative of Euler angle       *
! *                          around axis one.   Units: rad/s**2.         *
! *                       8: Second time derivative of Euler angle       *
! *                          around axis two.   Units: rad/s**2.         *
! *                       9: Second time derivative of Euler angle       *
! *                          axis three with the contribution of zonal   *
! *                          tides removed. Units: rad/s**2.             *
! *                                                                      *
! *           ut1mtai --  Angle  UT1 minus TAI. Units: s.                *
! *                                                                      *
! *           ut1rat  --  First  time derivative of angle UT1 minus TAI. *
! *                       Units s/s.                                     *
! *                                                                      *
! *           ut1rr   --  Second time derivative of angle UT1 minus TAI. *
! *                       Units s/s^2.                                   *
! *                                                                      *
! *           lod     --  Length of day. Units: s.                       *
! *                                                                      *
! *           lodr    --  Length of day rate. Units: s/day.              *
! *                                                                      *
! *           xpol    --  X pole coordinate. Unit: arcsec.               *
! *                                                                      *
! *           ypol    --  Y pole coordinate. Unit: arcsec.               *
! *                                                                      *
! *           xpolr   --  First  time derivative of X pole coordinate.   *
! *                       Unit: arcsec/day.                              *
! *                                                                      *
! *           ypolr   --  First  time derivative of Y pole coordinate.   *
! *                       Unit: arcsec/day.                              *
! *                                                                      *
! *           xpolrr  --  Second time derivative of X pole coordinate.   *
! *                       Unit: arcsec/day**2.                           *
! *                                                                      *
! *           ypolrr  --  Second time derivative of Y pole coordinate.   *
! *                       Unit: arcsec/day**2.                           *
! *                                                                      *
! *           eop3    --  Array of three EOP parameters:                 *
! *                       1: X pole coordinate. Unit: arcsec.            *
! *                       2: Y pole coordinate. Unit: arcsec.            *
! *                       3: UT1 minus TAI.     Unit: s.                 *
! *                                                                      *
! *           eop3r   --  Array of six EOP parameters:                   *
! *                       1: X pole coordinate. Unit: arcsec.            *
! *                       2: Y pole coordinate. Unit: arcsec.            *
! *                       3: UT1 minus TAI. Unit: s.                     *
! *                       4: First  time derivative of X pole            *
! *                          coordinate. Unit: arcsec/day.               *
! *                       5: Second time derivative of Y pole            *
! *                          coordinate. Unit: arcsec/day.               *
! *                       6: First  time derivative of UT1 minus TAI.    *
! *                          Unit: s/day.                                *
! *           eop3zr  --  Array of six EOP parameters:                   *
! *                       1: X pole coordinate. Unit: arcsec.            *
! *                       2: Y pole coordinate. Unit: arcsec.            *
! *                       3: UT1 minus TAI with the contribution of      *
! *                          zonal tides removed. Unit: s.               *
! *                       4: First  time derivative of X pole            *
! *                          coordinate. Unit: arcsec/day.               *
! *                       5: Second time derivative of Y pole            *
! *                          coordinate. Unit: arcsec/day.               *
! *                       6: First  time derivative of UT1 minus TAI     *
! *                          with the contribution of zonal tides        *
! *                          removed. Unit: s/day.                       *
! *                                                                      *
! *           dpsi    --  Nutation angle in longitude. Units: rad        *
! *                                                                      *
! *           deps    --  Nutation angle in obliquity. Units: rad        *
! *                                                                      *
! *           dpsir   --  First time derivative of nutation angle in     *
! *                       longitude. Units: rad/s.                       *
! *                                                                      *
! *           depsr   --  First time derivative of nutation angle in     *
! *                       obliquity. Units: rad/s.                       *
! *                                                                      *
! *           nut     --  Array of two EOP parameters:
! *                       1: Nutation angle in longitude. Units: rad     *
! *                       2: Nutation angle in obliquity. Units: rad     *
! *                                                                      *
! *           nutr    --  Array of four EOP parameters:                  *
! *                       1: Nutation angle in longitude. Units: rad     *
! *                       2: Nutation angle in obliquity. Units: rad     *
! *                       3: First time derivative of nutation angle in  *
! *                          longitude. Units: rad/s.                    *
! *                       4: First time derivative of nutation angle in  *
! *                          obliquity. Units: rad/s.                    *
! *                                                                      *
! *           eops    --  Array of eight EOP parameters:                 *
! *                       1: X pole coordinate. Unit: arcsec.            *
! *                       2: Y pole coordinate. Unit: arcsec.            *
! *                       3: UT1 minus TAI.     Unit: s.                 *
! *                       4: First  time derivative of X pole            *
! *                          coordinate. Unit: arcsec/day.               *
! *                       5: Second time derivative of Y pole            *
! *                          coordinate. Unit: arcsec/day.               *
! *                       6: First  time derivative of UT1 minus TAI     *
! *                          coordinate. Unit: s/day.                    *
! *                       7: Nutation angle in longitude. Units: arcsec  *
! *                       8: Nutation angle in obliquity. Units: arcsec  *
! *           eopzs   --  Array of eight EOP parameters:                 *
! *                       1: X pole coordinate. Unit: arcsec.            *
! *                       2: Y pole coordinate. Unit: arcsec.            *
! *                       3: UT1 minus TAI .     Unit: s.                 *
! *                       4: First  time derivative of X pole            *
! *                          coordinate. Unit: arcsec/day.               *
! *                       5: Second time derivative of Y pole            *
! *                          coordinate. Unit: arcsec/day.               *
! *                       6: First  time derivative of UT1 minus TAI     *
! *                          coordinate. Unit: s/day.                    *
! *                       7: Nutation angle in longitude. Units: arcsec  *
! *                       8: Nutation angle in obliquity. Units: arcsec  *
! *                                                                      *
! *           h1      --  Contribution of empirical harmonic variations  *
! *                       in the EOPs with respect to axis 1. Units: rad *
! *                                                                      *
! *           h2      --  Contribution of empirical harmonic variations  *
! *                       in the EOPs with respect to axis 2. Units: rad *
! *                                                                      *
! *           h3      --  Contribution of empirical harmonic variations  *
! *                       in the EOPs with respect to axis 3. Units: rad *
! *                                                                      *
! *           h1r     --  First  time derivative of the contribution of  *
! *                       empirical harmonic variations in the EOPs with *
! *                       respect to axis 1. Units: rad/s.               *
! *                                                                      *
! *           h2r     --  First  time derivative of the contribution of  *
! *                       empirical harmonic variations in the EOPs with *
! *                       respect to axis 2. Units: rad/s.               *
! *                                                                      *
! *           h3r     --  First  time derivative of the contribution of  *
! *                       empirical harmonic variations in the EOPs with *
! *                       respect to axis 3. Units: rad/s.               *
! *                                                                      *
! *           h1rr    --  Second time derivative of the contribution of  *
! *                       empirical harmonic variations in the EOPs with *
! *                       respect to axis 1. Units: rad/s^2.             *
! *                                                                      *
! *           h2rr    --  Second time derivative of the contribution of  *
! *                       empirical harmonic variations in the EOPs with *
! *                       respect to axis 2. Units: rad/s^2.             *
! *                                                                      *
! *           h3rr    --  Second time derivative of the contribution of  *
! *                       empirical harmonic variations in the EOPs with *
! *                       respect to axis 3. Units: rad/s^2.             *
! *                                                                      *
! *           heo     --  Array of three components of the contribution  *
! *                       of empirical harmonic variations in the EOPs:  *
! *                       1: Contribution of empirical harmonic          *
! *                          variations in the EOPs with respect to      *
! *                          axis 1. Units: rad.                         *
! *                       2: Contribution of empirical harmonic          *
! *                          variations in the EOPs with respect to      *
! *                          axis 2. Units: rad.                         *
! *                       3: Contribution of empirical harmonic          *
! *                          variations in the EOPs with respect to      *
! *                          axis 3. Units: rad.                         *
! *                                                                      *
! *           heor    --  Array of three components of the first time    *
! *                       derivative of the contribution of empirical    *
! *                       harmonic variations in the EOPs:               *
! *                       1: First  time derivative of the contribution  *
! *                          of empirical harmonic variations in the     *
! *                          EOPs with respect to axis 1. Units: rad/s.  *
! *                       2: First  time derivative of the contribution  *
! *                          of empirical harmonic variations in the     *
! *                          EOPs with respect to axis 2. Units: rad/s.  *
! *                       3: First  time derivative of the contribution  *
! *                          of empirical harmonic variations in the     *
! *                          EOPs with respect to axis 3. Units: rad/s.  *
! *                                                                      *
! *           heorr   --  Array of three components of the first time    *
! *                       derivative of the contribution of empirical    *
! *                       harmonic variations in the EOPs:               *
! *                       1: Second time derivative of the contribution  *
! *                          of empirical harmonic variations in the     *
! *                          EOPs with respect to axis 1. Units: rad/s^2 *
! *                       2: Second time derivative of the contribution  *
! *                          of empirical harmonic variations in the     *
! *                          EOPs with respect to axis 2. Units: rad/s^2 *
! *                       3: Second time derivative of the contribution  *
! *                          of empirical harmonic variations in the     *
! *                          EOPs with respect to axis 3. Units: rad/s^2 *
! *                                                                      *
! *   M_PAR ( INTEGER*4  ) -- The maximum number of expected parameters  *
! *                           in the output array. If unsure, you can    *
! *                           set it to NERS__MPAR defined in ners.i     *
! *                           include block.                             *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   L_PAR ( INTEGER*4  ) -- The number of elements in the output       *
! *                           array of the Earth orientation parameters. *
! *    PARS ( REAL*8     ) -- The array of the Earth orientation         *
! *                           parameters.                                *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    NERS ( NERS__TYPE ) -- The data structure that keeps internal     *
! *                           parameters related to the Network Earth    *
! *                           Rotation Service.                          *
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
! *  Speed: 25 mks for the Earth rotation matrix and 1-2 mks for other   *
! *         parameters.                                                  *
! *                                                                      *
! *  ### 16-JUN-2016  NERS_GET_EOP  v2.6 (c) L. Petrov  23-AUG-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'ners.i'
      INCLUDE   'heo.i'
      TYPE     ( NERS__TYPE ) :: NERS
      INTEGER*4  M_PAR, L_PAR, IUER 
      CHARACTER  CPAR*(*)
      REAL*8     PARS(M_PAR), TIM_TAI, EVEC(3,0:2), UTC_CUR, TAI_CUR, &
     &           TIM_TAI_USED, UTC_M_TAI, DPSI, DEPS, HEO_APS_VEC(3,0:2), &
     &           MAT_ROT(3,3,0:2), DPSI_RATE, DEPS_RATE, TAI, UT1_M_TAI
      REAL*8     XPOL, YPOL, XPOL_RATE, YPOL_RATE, UT1_RATE,  &
     &           S_ANG, S_ANG_RATE, DZETA, TETA, ZA, EPS_0, DZETA_RATE, &
     &           TETA_RATE, ZA_RATE, EPS_0_RATE, E1_NUT, E2_NUT, &
     &           E1_NUT_RATE, E2_NUT_RATE, HEO_VEC(3,0:2), &
     &           E1_GDS, E2_GDS, DPSI_GDS, DEPS_GDS, AGE_SPL_MAX, &
     &           DPSI_SEC, DEPS_SEC, DPSIR_SEC, DEPSR_SEC, &
     &           E3Z, E3Z_DOT, E3Z_DT2 
      CHARACTER  STR*32, STR1*32
      REAL*8       LOD__TO__ER3
      PARAMETER  ( LOD__TO__ER3 =  1.00273781191135448E0*OM__EAR**2/PI2 )
      LOGICAL*1  FL_ERROR 
      INTEGER*4  IS, UNIX_DATE, IVRB, MJD, IER
      INTEGER*8  SIZE_I8
      INTEGER*4, EXTERNAL :: OMP_GET_THREAD_NUM
      LOGICAL*4, EXTERNAL :: IS_R8_NAN, OMP_IN_PARALLEL
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, TIME, FILE_INFO
!
      IVRB = 0
      HEO_APS_VEC = 0.0D0
      DPSI = 0.0D0
      DEPS = 0.0D0
      DPSI_RATE = 0.0D0
      DEPS_RATE = 0.0D0
!
      IF ( IS_R8_NAN(TIM_TAI) ) THEN
           CALL ERR_LOG ( 4311, IUER, 'NERS_GET_EOP', 'Argument TIM_TAI '// &
     &         'is not a number' )
           RETURN 
      END IF
      IF ( TIM_TAI < NERS__MIN_TIM .AND. TIM_TAI > NERS__FIL_TIM ) THEN
           WRITE ( UNIT=STR,  FMT='(1PD22.15)' ) TIM_TAI
           WRITE ( UNIT=STR1, FMT='(1PD22.15)' ) NERS__MIN_TIM
           CALL ERR_LOG ( 4312, IUER, 'NERS_GET_EOP', 'Too small TIM_TAI parameter: '// &
     &          STR(1:22)//' -- less than the lower limit '//STR1 )
           RETURN 
      END IF
!
      IF ( TIM_TAI > NERS__MAX_TIM ) THEN
           WRITE ( UNIT=STR,  FMT='(1PD22.15)' ) TIM_TAI
           WRITE ( UNIT=STR1, FMT='(1PD22.15)' ) NERS__MAX_TIM
           CALL ERR_LOG ( 4313, IUER, 'NERS_GET_EOP', 'Too small TIM_TAI parameter: '// &
     &          STR(1:22)//' -- greater than the upper limit '//STR1 )
           RETURN 
      END IF
!
! --- Set maximum allowed spline age. If the time intervla between
! --- spline computation and the current time is greater than AGE_SPL_MIN,
! --- it will be re-computed
!
      IF ( NERS%CNF%AGE_SPL < NERS__AGE_MIN - 1.0D-8 ) THEN
           AGE_SPL_MAX = NERS__AGE_MIN
        ELSE IF ( NERS%CNF%AGE_SPL < NERS%CNF%AGE_FCS ) THEN
           AGE_SPL_MAX = NERS%CNF%AGE_SPL 
        ELSE 
           AGE_SPL_MAX = NERS%CNF%AGE_FCS 
      END IF 
!
      FL_ERROR = .FALSE.
!
! --- Get UTC_CUR -- the UTC time tag for the current moment of time
!
      UTC_CUR = TIME ( %VAL(0) ) - UNIX__J2000_UTC
!$OMP CRITICAL (NERS_CHECK_AND_LOAD)
!
! --- This region should be processed only by one thread at once
!
      IF ( NERS%FCS_STATUS .NE. NERS__LOAD .OR. &
     &     (UTC_CUR - NERS%UTC_LOAD) > NERS__AGE_MIN ) THEN
!
! -------- The file with NERS message either was not loaded or is too old
!
           IF ( NERS%FCS_STATUS .NE. NERS__INIT .AND. &
     &          NERS%FCS_STATUS .NE. NERS__LOAD       ) THEN
!
                CALL CLRCH ( STR )
                CALL INCH  ( NERS%FCS_STATUS, STR )
                CALL ERR_LOG ( 4314, IUER, 'NERS_GET_EOP', 'NERS data '// &
     &              'structure has not been initialized. NERS%FCS_STATUS= '// &
     &              TRIM(STR)//'. Please run NERS_INIT first' )
                FL_ERROR = .TRUE.
           END IF
           IF ( .NOT. FL_ERROR ) THEN
!
!-------------- Get the size of the NERS file and its modification date
!
                IS = FILE_INFO ( TRIM(NERS%CNF%FCS_FILE)//CHAR(0), UNIX_DATE, &
     &                          SIZE_I8 )
                IF ( IS .NE. 0 .OR. (TIME(%VAL(0)) - UNIX_DATE) > NERS%CNF%AGE_FCS ) THEN
!
! ------------------ Either the file does not exist or it is too old.
! ------------------ Then fetch the NERS file and download it to the designated
! ------------------ directory
!
                     IER = IUER
                     CALL NERS_FETCH ( NERS, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 4315, IUER, 'NERS_GET_EOP', 'Error in '// &
     &                        'an attempt to retrieve NERS forecast parameters '// &
     &                        'form the remote server' )
                          FL_ERROR = .TRUE.
                     END IF
                     CALL NERS_QUIT ( NERS__FCS, NERS )
                END IF
           END IF
!
           IF ( .NOT. FL_ERROR .AND. NERS%FCS_STATUS .NE. NERS__LOAD ) THEN
!
! ------------- Read the NERS message and ingest it to the internal NERS data records
!
                IER = IUER
                CALL NERS_LOAD ( NERS, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 4316, IUER, 'NERS_GET_EOP', 'Error in '// &
     &                    'an attempt to retrieve NERS forecast parameters '// &
     &                    'form the remote server' )
                     FL_ERROR = .TRUE.
                END IF
           END IF
      END IF
!$OMP END CRITICAL (NERS_CHECK_AND_LOAD)
      IF ( FL_ERROR ) THEN
           RETURN 
      END IF
!!
      IF ( TIM_TAI .LE. NERS__FIL_TIM + 1.D0/NERS__FIL_TIM ) THEN
!
! -------- Convert the UTC time tag for the current moment time to TAI
!
           CALL NERS_GET_UTCMTAI ( NERS, UTC_CUR, UTC_M_TAI, IER )
           TIM_TAI_USED = UTC_CUR - UTC_M_TAI
         ELSE 
           TIM_TAI_USED = TIM_TAI
      END IF
!
      IF ( CPAR == 'utcmtai' ) THEN
!
! -------- Specical case: request for the UTC minus TAI function
!
           IER = IUER
           CALL NERS_GET_UTCMTAI ( NERS, TIM_TAI_USED, UTC_M_TAI, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4317, IUER, 'NERS_GET_EOP', 'Error in '// &
     &              'an attempt to compute UTC minus TAI function on '// &
     &              'the requested moment of time' )
                RETURN 
           END IF
           L_PAR = 1
           PARS(1) = UTC_M_TAI
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
      IF ( NERS%TIM_START == NERS%TIM_STOP ) THEN
!
! ======== Get EOP at the requested moment of time without interpolation
!
           IF ( CPAR == 'mat'    .OR. &
     &          CPAR == 'matr'   .OR. &
     &          CPAR == 'matrr'  .OR. &
     &          CPAR == 'matall'      ) THEN
!
                IF ( CPAR == 'matall' .AND. &
     &               DABS(TIM_TAI - NERS%TIM_MATROT_LAST) < NERS__TIM_EPS ) THEN
                     CALL MEMCPY ( MAT_ROT, NERS%MATROT_LAST, %VAL(8*NERS__MPAR) )
                   ELSE
!
! ------------------ Get EVEC -- array of slowly variating EOPs, 
! ------------------ their first and second derivatives
!
                     IER = IUER
                     CALL NERS_GET_EVEC ( NERS, TIM_TAI_USED, EVEC, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 4318, IUER, 'NERS_GET_EOP', 'Error in '// &
     &                        'an attempt to compute the EOP vector on the '// &
     &                        'requested moment of time' )
                          RETURN 
                     END IF
!
! ------------------ Get empirical harmonic variations in the Earth rotation
!
                     IER = IUER
                     CALL NERS_GET_HEO ( NERS, TIM_TAI_USED, &
     &                                   EVEC(3,0)/UT1__TO__E3, DPSI, DEPS, HEO_VEC, &
     &                                   DPSI_RATE, DEPS_RATE, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 4319, IUER, 'NERS_GET_EOP', 'Error in '// &
     &                        'an attempt to compute the contribution of '// &
     &                        'harmonic EOP variation of teh requested '// &
     &                        'moment of time' )
                          RETURN 
                     END IF
! 
! ------------------ Compute MJD to the request moment of time

                     MJD = J2000__MJD  + INT(TIM_TAI_USED/86400.0D0)
                     TAI = TIM_TAI_USED - 86400.0D0*INT(TIM_TAI_USED/86400.0D0)
!
! ------------------ Transform EVEC to archaicc EOP notation
!
                     XPOL      = EVEC(2,0)
                     YPOL      = EVEC(1,0)
                     UT1_M_TAI = EVEC(3,0)/UT1__TO__E3
                     XPOL_RATE = EVEC(2,1)
                     YPOL_RATE = EVEC(1,1)
                     UT1_RATE  = EVEC(3,1)/UT1__TO__E3
!
! ------------------ Compute auxilliary angles of the Earth rotation
!
                     IER = IUER
                     CALL NERS_ERM_ANGS ( 1, IVRB, PREC__CAPITAINE2003, NUT__MHB2000, &
     &                                    MJD, TAI, XPOL, YPOL, UT1_M_TAI, &
     &                                    XPOL_RATE, YPOL_RATE, UT1_RATE,  &
     &                                    S_ANG, S_ANG_RATE, &
     &                                    DZETA, TETA, ZA, EPS_0, DZETA_RATE, &
     &                                    TETA_RATE, ZA_RATE, EPS_0_RATE, &
     &                                    E1_NUT, E2_NUT, DPSI, DEPS, &
     &                                    DPSI_SEC, DEPS_SEC, DPSIR_SEC, DEPSR_SEC, &
     &                                    E1_NUT_RATE, E2_NUT_RATE, DPSI_RATE, DEPS_RATE, &
     &                                    E1_GDS, E2_GDS, DPSI_GDS, DEPS_GDS, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 4320, IUER, 'NERS_GET_EOP', 'Error in '// &
     &                        'an attempt to compute the auxiliary angles of the '// &
     &                        'Earth rotation' )
                         RETURN 
                     END IF
!
! ------------------ Compute the Earth rotation matrix and its first and second
! ------------------ derivatives
!
                     IER = IUER
                     CALL NERS_ERM_MATS ( XPOL, YPOL, S_ANG, &
     &                                    XPOL_RATE, YPOL_RATE, S_ANG_RATE, &
     &                                    DZETA, TETA, ZA, EPS_0, &
     &                                    DZETA_RATE, TETA_RATE, ZA_RATE, EPS_0_RATE, &
     &                                    DPSI, DEPS, &
     &                                    DPSI_RATE, DEPS_RATE, &
     &                                    HEO_VEC(1,0), HEO_VEC(1,1), HEO_VEC(1,2), &
     &                                    0.0D0, 0.0D0, MAT_ROT, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 4321, IUER, 'NERS_GET_EOP', 'Error in '// &
     &                        'an attempt to compute the Earth rotation matrix' )
                          RETURN 
                     END IF
                     NERS%TIM_MATROT_LAST = TIM_TAI
                     CALL MEMCPY ( NERS%MATROT_LAST, MAT_ROT, %VAL(8*NERS__MPAR) )
                END IF
              ELSE
!
! ------------- A user requested something different than the Earth rotation
! ------------- matrix.
!
! ------------- Get EVEC -- array of slowly variating EOPs, 
! ------------- their first and second derivatives
!
                IER = IUER
                CALL NERS_GET_EVEC ( NERS, TIM_TAI_USED, EVEC, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 4322, IUER, 'NERS_GET_EOP', 'Error in '// &
     &                   'an attempt to compute the EOP vector on the '// &
     &                   'requested moment of time' )
                     RETURN 
                END IF
!
                IF ( CPAR == 'dpsi'  .OR. &
     &               CPAR == 'deps'  .OR. &
     &               CPAR == 'dpsir' .OR. &
     &               CPAR == 'depsr' .OR. &
     &               CPAR == 'eops'  .OR. &
     &               CPAR == 'nut'   .OR. &
     &               CPAR == 'nutr'  .OR. &
     &               CPAR == 'heo'   .OR. &
     &               CPAR == 'heor'  .OR. &
     &               CPAR == 'heorr' .OR. &
     &               CPAR == 'h1'    .OR. &
     &               CPAR == 'h2'    .OR. &
     &               CPAR == 'h3'    .OR. &
     &               CPAR == 'h1r'   .OR. &
     &               CPAR == 'h2r'   .OR. &
     &               CPAR == 'h3r'   .OR. &
     &               CPAR == 'h1rr'  .OR. &
     &               CPAR == 'h2rr'  .OR. &
     &               CPAR == 'h3rr'       ) THEN
!
! ------------------ Get empirical harmonic variations in the Earth rotation
!
                     IER = -1
                     CALL NERS_GET_HEO ( NERS, TIM_TAI_USED, EVEC(3,0)/UT1__TO__E3, &
     &                                   DPSI, DEPS, HEO_APS_VEC, &
     &                                   DPSI_RATE, DEPS_RATE, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 4323, IUER, 'NERS_GET_EOP', 'Error in '// &
     &                        'an attempt to compute the contribution of '// &
     &                        'harmonic EOP variation of the requested '// &
     &                        'moment of time' )
                          RETURN 
                     END IF
!
! ------------------ Compute auxilliary angles of the Earth rotation
!
                     MJD = J2000__MJD  + INT(TIM_TAI_USED/86400.0D0)
                     TAI = TIM_TAI_USED - 86400.0D0*INT(TIM_TAI_USED/86400.0D0)
                     CALL NERS_ERM_ANGS ( 1, IVRB, PREC__CAPITAINE2003, NUT__MHB2000, &
     &                                    MJD, TAI, XPOL, YPOL, UT1_M_TAI, &
     &                                    XPOL_RATE, YPOL_RATE, UT1_RATE,  &
     &                                    S_ANG, S_ANG_RATE, &
     &                                    DZETA, TETA, ZA, EPS_0, DZETA_RATE, &
     &                                    TETA_RATE, ZA_RATE, EPS_0_RATE, &
     &                                    E1_NUT, E2_NUT, DPSI, DEPS, &
     &                                    DPSI_SEC, DEPS_SEC, DPSIR_SEC, DEPSR_SEC, &
     &                                    E1_NUT_RATE, E2_NUT_RATE, DPSI_RATE, DEPS_RATE, &
     &                                    E1_GDS, E2_GDS, DPSI_GDS, DEPS_GDS, IER )
                   ELSE
                     IF ( CPAR == 'ezrr'  .OR. CPAR == 'eop3zr' ) THEN
                          MJD = J2000__MJD  + INT(TIM_TAI_USED/86400.0D0)
                          TAI = TIM_TAI_USED - 86400.0D0*INT(TIM_TAI_USED/86400.0D0)
                          IF ( NERS%FCS%E3Z_APR_MOD == NERS__E3Z_D93 ) THEN
                               CALL NERS_E3ZT_DICKMAN1993 ( 0, MJD, TAI, E3Z, E3Z_DOT, E3Z_DT2 ) 
                             ELSE IF ( NERS%FCS%E3Z_APR_MOD == NERS__E3Z_RE2014  ) THEN
                               CALL NERS_E3ZT_RE2014      ( 0, MJD, TAI, E3Z, E3Z_DOT, E3Z_DT2 ) 
                             ELSE
                               E3Z     = 0.0D0
                               E3Z_DOT = 0.0D0
                               E3Z_DT2 = 0.0D0
                          END IF
                       ELSE IF ( CPAR == 'e1'   .OR. CPAR == 'e2'   .OR. CPAR == 'e3'          .OR. &
     &                    CPAR == 'e1r'  .OR. CPAR == 'e2r'  .OR. CPAR == 'e3r'         .OR. &
     &                    CPAR == 'e1rr' .OR. CPAR == 'e2rr' .OR. CPAR == 'e3rr'        .OR. &
     &                    CPAR == 'e'    .OR. CPAR == 'er'   .OR. CPAR == 'err'         .OR. &
     &                    CPAR == 'ut1mtai' .OR. CPAR == 'ut1rat'                       .OR. &
     &                    CPAR == 'ut1rr'   .OR. CPAR == 'lod'    .OR. CPAR == 'lodr'   .OR. &
     &                    CPAR == 'eop3'    .OR. CPAR == 'eop3r'                        .OR. &
     &                    CPAR == 'xpol'    .OR. CPAR == 'ypol'                         .OR. &
     &                    CPAR == 'xpolr'   .OR. CPAR == 'ypolr'                        .OR. &
     &                    CPAR == 'xpolrr'  .OR. CPAR == 'ypolrr' .OR. CPAR == 'utcmtai'     ) THEN
                          CONTINUE 
                        ELSE
                          CALL ERR_LOG ( 4324, IUER, 'NERS_GET_EOP', 'EOP parameter '// &
     &                         TRIM(CPAR)//' is not supported' )
                          RETURN 
                     END IF
                END IF
           END IF
!
! -------- Now put the requested EOP in the output array PARS
!
           IF ( CPAR == 'e1' ) THEN
                L_PAR = 1
                PARS(1) = EVEC(1,0)
              ELSE IF ( CPAR == 'e2' ) THEN
                L_PAR = 1
                PARS(1) = EVEC(2,0)
              ELSE IF ( CPAR == 'e3' ) THEN
                L_PAR = 1
                PARS(1) = EVEC(3,0)
              ELSE IF ( CPAR == 'e' ) THEN
                L_PAR = 3
                PARS(1) = EVEC(1,0)
                PARS(2) = EVEC(2,0)
                PARS(3) = EVEC(3,0)
              ELSE IF ( CPAR == 'e1r' ) THEN
                L_PAR = 1
                PARS(1) = EVEC(1,1)
              ELSE IF ( CPAR == 'e2r' ) THEN
                L_PAR = 1
                PARS(1) = EVEC(2,1)
              ELSE IF ( CPAR == 'e3r' ) THEN
                L_PAR = 1
                PARS(1) = EVEC(3,1)
              ELSE IF ( CPAR == 'er' ) THEN
                L_PAR = 6
                PARS(1) = EVEC(1,0)
                PARS(2) = EVEC(2,0)
                PARS(3) = EVEC(3,0)
                PARS(4) = EVEC(1,1)
                PARS(5) = EVEC(2,1)
                PARS(6) = EVEC(3,1)
              ELSE IF ( CPAR == 'err' ) THEN
                L_PAR = 9
                PARS(1) = EVEC(1,0)
                PARS(2) = EVEC(2,0)
                PARS(3) = EVEC(3,0)
                PARS(4) = EVEC(1,1)
                PARS(5) = EVEC(2,1)
                PARS(6) = EVEC(3,1)
                PARS(7) = EVEC(1,2)
                PARS(8) = EVEC(2,2)
                PARS(9) = EVEC(3,2)
              ELSE IF ( CPAR == 'ezrr' ) THEN
                L_PAR = 9
                PARS(1) = EVEC(1,0)
                PARS(2) = EVEC(2,0)
                PARS(3) = EVEC(3,0) - E3Z
                PARS(4) = EVEC(1,1)
                PARS(5) = EVEC(2,1)
                PARS(6) = EVEC(3,1) - E3Z_DOT
                PARS(7) = EVEC(1,2)
                PARS(8) = EVEC(2,2)
                PARS(9) = EVEC(3,2) - E3Z_DT2
              ELSE IF ( CPAR == 'e1rr' ) THEN
                L_PAR = 1
                PARS(1) = EVEC(1,2)
              ELSE IF ( CPAR == 'e2rr' ) THEN
                L_PAR = 1
                PARS(1) = EVEC(2,2)
              ELSE IF ( CPAR == 'e3rr' ) THEN
                L_PAR = 1
                PARS(1) = EVEC(3,2)
              ELSE IF ( CPAR == 'ut1mtai' ) THEN
                L_PAR = 1
                PARS(1) = EVEC(3,0)/UT1__TO__E3
              ELSE IF ( CPAR == 'ut1rat' ) THEN
                L_PAR = 1
                PARS(1) = EVEC(3,1)/UT1__TO__E3
              ELSE IF ( CPAR == 'ut1rr' ) THEN
                L_PAR = 1
                PARS(1) = EVEC(3,2)/UT1__TO__E3
              ELSE IF ( CPAR == 'lod' ) THEN
                L_PAR = 1
                PARS(1) = EVEC(3,1)/LOD__TO__ER3
              ELSE IF ( CPAR == 'lodr' ) THEN
                L_PAR = 1
                PARS(1) = EVEC(3,2)/LOD__TO__ER3/86400.0D0
              ELSE IF ( CPAR == 'xpol' ) THEN
                L_PAR = 1
                PARS(1) = EVEC(2,0)*RAD__TO__ARCSEC
              ELSE IF ( CPAR == 'ypol' ) THEN
                L_PAR = 1
                PARS(1) = EVEC(1,0)*RAD__TO__ARCSEC
              ELSE IF ( CPAR == 'xpolr' ) THEN
                L_PAR = 1
                PARS(1) = EVEC(2,1)*RAD__TO__ARCSEC*86400.0D0
              ELSE IF ( CPAR == 'ypolr' ) THEN
                L_PAR = 1
                PARS(1) = EVEC(1,1)*RAD__TO__ARCSEC*86400.0D0
              ELSE IF ( CPAR == 'xpolrr' ) THEN
                L_PAR = 1
                PARS(1) = EVEC(2,2)*RAD__TO__ARCSEC*86400.0D0**2
              ELSE IF ( CPAR == 'ypolrr' ) THEN
                L_PAR = 1
                PARS(1) = EVEC(1,2)*RAD__TO__ARCSEC*86400.0D0**2
              ELSE IF ( CPAR == 'dpsi' ) THEN
                L_PAR = 1
                PARS(1) = DPSI
              ELSE IF ( CPAR == 'deps' ) THEN
                L_PAR = 1
                PARS(1) = DEPS
              ELSE IF ( CPAR == 'dpsir' ) THEN
                L_PAR = 1
                PARS(1) = DPSI_RATE
              ELSE IF ( CPAR == 'depsr' ) THEN
                L_PAR = 1
                PARS(1) = DEPS_RATE
              ELSE IF ( CPAR == 'nut' ) THEN
                L_PAR = 2
                PARS(1) = DPSI
                PARS(2) = DEPS
              ELSE IF ( CPAR == 'nutr' ) THEN
                L_PAR = 4
                PARS(1) = DPSI
                PARS(2) = DEPS
                PARS(3) = DPSI_RATE
                PARS(4) = DEPS_RATE
              ELSE IF ( CPAR == 'heo' ) THEN
                L_PAR = 3
                PARS(1) = HEO_APS_VEC(1,0)
                PARS(2) = HEO_APS_VEC(2,0)
                PARS(3) = HEO_APS_VEC(3,0)
              ELSE IF ( CPAR == 'heorr' ) THEN
                L_PAR = 9
                PARS(1) = HEO_APS_VEC(1,0)
                PARS(2) = HEO_APS_VEC(2,0)
                PARS(3) = HEO_APS_VEC(3,0)
                PARS(4) = HEO_APS_VEC(1,1)
                PARS(5) = HEO_APS_VEC(2,1)
                PARS(6) = HEO_APS_VEC(3,1)
                PARS(7) = HEO_APS_VEC(1,2)
                PARS(8) = HEO_APS_VEC(2,2)
                PARS(9) = HEO_APS_VEC(3,2)
              ELSE IF ( CPAR == 'h1' ) THEN
                L_PAR = 1
                PARS(1) = HEO_APS_VEC(1,0)
              ELSE IF ( CPAR == 'h2' ) THEN
                L_PAR = 1
                PARS(1) = HEO_APS_VEC(2,0)
              ELSE IF ( CPAR == 'h3' ) THEN
                L_PAR = 1
                PARS(1) = HEO_APS_VEC(3,0)
              ELSE IF ( CPAR == 'h1r' ) THEN
                L_PAR = 1
                PARS(1) = HEO_APS_VEC(1,1)
              ELSE IF ( CPAR == 'h2r' ) THEN
                L_PAR = 1
                PARS(1) = HEO_APS_VEC(2,1)
              ELSE IF ( CPAR == 'h3r' ) THEN
                L_PAR = 1
                PARS(1) = HEO_APS_VEC(3,1)
              ELSE IF ( CPAR == 'h1rr' ) THEN
                L_PAR = 1
                PARS(1) = HEO_APS_VEC(1,2)
              ELSE IF ( CPAR == 'h2rr' ) THEN
                L_PAR = 1
                PARS(1) = HEO_APS_VEC(2,2)
              ELSE IF ( CPAR == 'h3rr' ) THEN
                L_PAR = 1
                PARS(1) = HEO_APS_VEC(3,2)
              ELSE IF ( CPAR == 'mat' ) THEN
                L_PAR = 9
                CALL MEMCPY ( PARS, MAT_ROT(1,1,0), %VAL(9*8) )
              ELSE IF ( CPAR == 'matr' ) THEN
                L_PAR = 9
                CALL MEMCPY ( PARS, MAT_ROT(1,1,1), %VAL(9*8) )
              ELSE IF ( CPAR == 'matrr' ) THEN
                L_PAR = 9
                CALL MEMCPY ( PARS, MAT_ROT(1,1,2), %VAL(9*8) )
              ELSE IF ( CPAR == 'matall' ) THEN
                L_PAR = 27
                CALL MEMCPY ( PARS, MAT_ROT, %VAL(9*3*8) )
              ELSE IF ( CPAR == 'eop3' ) THEN
                L_PAR = 3
                PARS(1) = EVEC(2,0)*RAD__TO__ARCSEC
                PARS(2) = EVEC(1,0)*RAD__TO__ARCSEC
                PARS(3) = EVEC(3,0)/UT1__TO__E3
              ELSE IF ( CPAR == 'eop3r' ) THEN
                L_PAR = 6
                PARS(1) = EVEC(2,0)*RAD__TO__ARCSEC
                PARS(2) = EVEC(1,0)*RAD__TO__ARCSEC
                PARS(3) = EVEC(3,0)/UT1__TO__E3
                PARS(4) = EVEC(2,1)*RAD__TO__ARCSEC*86400.0D0
                PARS(5) = EVEC(1,1)*RAD__TO__ARCSEC*86400.0D0
                PARS(6) = EVEC(3,1)/UT1__TO__E3*86400.0D0
              ELSE IF ( CPAR == 'eop3zr' ) THEN
                L_PAR = 6
                PARS(1) = EVEC(2,0)*RAD__TO__ARCSEC
                PARS(2) = EVEC(1,0)*RAD__TO__ARCSEC
                PARS(3) = (EVEC(3,0) - E3Z)/UT1__TO__E3
                PARS(4) = EVEC(2,1)*RAD__TO__ARCSEC*86400.0D0
                PARS(5) = EVEC(1,1)*RAD__TO__ARCSEC*86400.0D0
                PARS(6) = (EVEC(3,1) - E3Z_DOT)/UT1__TO__E3*86400.0D0
              ELSE IF ( CPAR == 'eops' ) THEN
                L_PAR = 8
                PARS(1) = EVEC(2,0)*RAD__TO__ARCSEC
                PARS(2) = EVEC(1,0)*RAD__TO__ARCSEC
                PARS(3) = EVEC(3,0)/UT1__TO__E3
                PARS(4) = EVEC(2,1)*RAD__TO__ARCSEC*86400.0D0
                PARS(5) = EVEC(1,1)*RAD__TO__ARCSEC*86400.0D0
                PARS(6) = EVEC(3,1)/UT1__TO__E3*86400.0D0
                PARS(7) = DPSI*RAD__TO__ARCSEC
                PARS(8) = DEPS*RAD__TO__ARCSEC
              ELSE
                CALL ERR_LOG ( 4325, IUER, 'NERS_GET_EOP', 'Earth orientation '// &
     &              'parameter name '//TRIM(CPAR)//' is not supported' )
                RETURN 
           END IF
         ELSE
!
! ======== Get EOP at the requested moment of time using spline interpolation
!
           IF (   NERS%EXP_STATUS .NE. NERS__COMP                 .OR. &
     &          ( NERS%EXP_STATUS .EQ. NERS__COMP         .AND.        &
     &            (UTC_CUR - NERS%UTC_SPLN) > AGE_SPL_MAX       ) .OR. &
     &          TIM_TAI_USED < NERS%TIM_START                     .OR. &
     &          TIM_TAI_USED > NERS%TIM_STOP                           ) THEN
!
! ------------- If the spline was not computed or it was computed and
! ------------- the requested epoch is beyond the interval of spline,
! ------------- compute the coefficients of the EOP intp B-spline basis
! ------------- expansion.
!
                IF ( TIM_TAI_USED < NERS%TIM_START   .OR. &
     &               TIM_TAI_USED > NERS%TIM_STOP         ) THEN
!
! ------------------ The requested time is beyond the range of dates
! ------------------ specificed in NERS_INIT or previous call to NERS_GET_EOP.
! ------------------ We set the new range around the requested date.
! ------------------ Since we do not know what should be the range,
! ------------------ we just set it twice larger than the minumeum span:
! ------------------ 4 times NERS__EDG_NODES by interpolation step.
!
                     NERS%TIM_START = TIM_TAI_USED - 2*NERS__EDG_NODES*NERS__TIM_STEP
                     NERS%TIM_STOP  = TIM_TAI_USED + 2*NERS__EDG_NODES*NERS__TIM_STEP
                END IF
                FL_ERROR = .FALSE.
!$OMP CRITICAL (OMP_NERS_COMP_SPL)
!
! ------------- Compute the coefficients of B-spline expansion
!
                IER = IUER
                CALL NERS_COMP_SPL ( NERS, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 4326, IUER, 'NERS_GET_EOP', 'Error in '// &
     &                   'computation of the spline coefficients' )
                     FL_ERROR = .TRUE.
                END IF           
!$OMP  END CRITICAL (OMP_NERS_COMP_SPL)
                IF ( FL_ERROR ) THEN
                     RETURN 
                END IF
           END IF           
!
! -------- Compute the EOP using the spline expansion
!
           IER = IUER
           CALL NERS_GET_SPL  ( NERS, TIM_TAI_USED, CPAR, L_PAR, PARS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4327, IUER, 'NERS_GET_EOP', 'Error during '// &
     &              'spline interpolation' )
                RETURN 
           END IF           
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  NERS_GET_EOP  !#!#
