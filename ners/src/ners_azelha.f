      PROGRAM    NERS_AZELHA
! ************************************************************************
! *                                                                      *
! *   Program NERS_AZELHA  computes azimuth, elevation, and hour angle,  *
! *   as well as their time derivatives at moment of time TIM_TAI for    *
! *   a station with coordinates in the terrestrial crust-fixed          *
! *   coordinate system COO_TRS that observes a source with coordinates  *
! *   in the barycentric celestial coordinate system RA, DEC. Both       *
! *   annual and diurnal aberrations are taken into account. Elevation   *
! *   is computed with respect to the normal to the reference ellipsoid. *
! *   Vertical deflection is ignored. Optionally the elevation can be    *
! *   corrected for refraction.                                          *
! *                                                                      *
! *   This program can be used only for computation of azimuths,         *
! *   elevations and hour anlges of an object beyond the Solar system,   *
! *   such as a star or a galaxy.                                        *
! *                                                                      *
! *   Usage: ners_azelha date coo_trs ra dec [none|optic|radio]          *
! *                                                                      *
! *   where date is the date in YYYY.MM.DD_hh:mm:ss.sss format or now,   *
! *         coo_trs is the vector of station coordinates in the rotating *
! *                 crust-fixed coordinate system in meters,             *
! *         ra      is the right ascension of the observed body in rad;  *
! *         dec     is the declination of the observed body in rad;      *
! *         [none|optic|radio] is optional computation of refration:     *
! *                            none  -- not to account refraction,       *
! *                            optic -- to account refraction in optic   *
! *                                     range;                           *
! *                            radio -- to account for refraction in     *
! *                                     radio range.                     *
! *                                                                      *
! *  Units: radians for azimuth, elevation angle, and hour angle;        *
! *         rad/s for their time derivatives.                            *
! *                                                                      *
! *  ### 06-JUN-2018  NERS_AZELHA   v2.0 (c)  L. Petrov  28-JUN-2018 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'ners.i'
      TYPE     ( NERS__TYPE ) :: NERS
      CHARACTER  DATE_STR*32, STR*32, REFR_MODE*5
      REAL*8     TIM_TAI, SEC, COO_TRS(3), RA, DEC, AZ, EL, HA, &
     &           AZ_RATE, EL_RATE, HA_RATE
      REAL*8     UTC_CUR, UTC_M_TAI
      INTEGER*4  MJD, IUER
      INTEGER*4, EXTERNAL :: TIME
!
      IF ( IARGC() < 6 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: ners_azelha date coo_trs ra dec [none|optic|radio]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, DATE_STR )
!
           CALL GETARG ( 2, STR      )
           READ ( UNIT=STR, FMT=*, IOSTAT=IUER ) COO_TRS(1)
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4901, IUER, 'NERS_AZEL', 'Error in '// &
     &              'decoding the second argument, COO_TRS(1): a real '// &
     &              'number was expected, but got '//STR )
                CALL EXIT ( 1 ) 
           END IF
!
           CALL GETARG ( 3, STR      )
           READ ( UNIT=STR, FMT=*, IOSTAT=IUER ) COO_TRS(2)
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4902, IUER, 'NERS_AZEL', 'Error in '// &
     &              'decoding the third argument, COO_TRS(2): a real '// &
     &              'number was expected, but got '//STR )
                CALL EXIT ( 1 ) 
           END IF
!
           CALL GETARG ( 4, STR      )
           READ ( UNIT=STR, FMT=*, IOSTAT=IUER ) COO_TRS(3)
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4904, IUER, 'NERS_AZEL', 'Error in '// &
     &              'decoding the fourth argument, COO_TRS(3): a real '// &
     &              'number was expected, but got '//STR )
                CALL EXIT ( 1 ) 
           END IF
!
           CALL GETARG ( 5, STR      )
           READ ( UNIT=STR, FMT=*, IOSTAT=IUER ) RA
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4905, IUER, 'NERS_AZEL', 'Error in '// &
     &              'decoding the fifth argument, RA: a real '// &
     &              'number was expected, but got '//STR )
                CALL EXIT ( 1 ) 
           END IF
!
           CALL GETARG ( 6, STR      )
           READ ( UNIT=STR, FMT=*, IOSTAT=IUER ) DEC
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4906, IUER, 'NERS_AZEL', 'Error in '// &
     &              'decoding the fifth argument, DEC: a real '// &
     &              'number was expected, but got '//STR )
                CALL EXIT ( 1 ) 
           END IF
           IF ( IARGC() .GE. 7 ) THEN
                CALL GETARG ( 7, REFR_MODE )
              ELSE
                REFR_MODE = NERS__REFR_NONE
           END IF
      END IF
!
      IF ( DATE_STR == 'now' ) THEN
!
! -------- Get UTC tag for the current moment of time
!
           UTC_CUR = TIME ( %VAL(0) ) - UNIX__J2000_UTC
!
! -------- Initialize NERS
!
           IUER = -1
           CALL NERS_INIT ( 'NERS_CONFIG', NERS, -1.0D0, -1.0D0, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4907, IUER, 'NERS_AZEL', 'Error in initializing '// &
     &              'NERS data structure' )
                CALL EXIT ( 1 )
           END IF
!
! -------- Load NERS
!
           IUER = -1
           CALL NERS_LOAD ( NERS, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 4908, IUER, 'NERS_AZEL', 'Error in '// &
     &              'an attempt to retrieve NERS forecast parameters '// &
     &              'form the remote server' )
                CALL EXIT ( 1 )
           END IF
!
! -------- Copmpute function UTC_M_TAI at the current moment of time
!
           IUER = -1
           CALL NERS_GET_UTCMTAI ( NERS, UTC_CUR, UTC_M_TAI, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4909, IUER, 'NERS_AZEL', 'Error in getting '// &
     &              'UTC minus TAI' )
                CALL EXIT ( 1 )
           END IF
           TIM_TAI = UTC_CUR - UTC_M_TAI
         ELSE
           IUER = -1
           CALL DATE_TO_TIME ( DATE_STR, MJD, SEC, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4910, IUER, 'NERS_AZEL', 'Wrong time string '// &
     &                         DATE_STR )
                CALL EXIT ( 1 ) 
           END IF
           TIM_TAI = (MJD - J2000__MJD)*86400.0D0 + SEC
!
           IUER = -1
           CALL NERS_INIT ( 'NERS_CONFIG', NERS, TIM_TAI, TIM_TAI, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4911, IUER, 'NERS_AZEL', 'Error in '// &
     &              'initializing NERS data structure' )
                CALL EXIT ( 1 )
           END IF
!
           IUER = -1
           CALL NERS_LOAD ( NERS, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 4912, IUER, 'NERS_AZEL', 'Error in '// &
     &              'an attempt to retrieve NERS forecast parameters '// &
     &              'form the remote server' )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IUER = -1
      CALL NERS_AZELHA_COMP ( NERS, TIM_TAI, COO_TRS, RA, DEC, REFR_MODE, &
     &                        AZ, EL, HA, AZ_RATE, EL_RATE, HA_RATE, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4913, IUER, 'NERS_AZEL', 'Error in '// &
     &         'computing azimuth and elevation' )
           CALL EXIT ( 1 ) 
      END IF
!
      WRITE ( 6, 110 ) AZ, EL, HA, AZ_RATE, EL_RATE, HA_RATE
 110  FORMAT ( F13.10, 1X, F14.10, 1X, F14.10, 1X, 1PD17.10, 1X, 1PD18.10, 1X, 1PD18.10 ) 
      CALL GETENVAR ( 'NERS_MAT_FORMAT', STR )
      IF ( STR == 'yes' ) THEN
           WRITE ( 6, 210 ) AZ/DEG__TO__RAD, EL/DEG__TO__RAD, HA/DEG__TO__RAD
 210       FORMAT ( ' Azimuth: ', F13.9,' deg ', ' Elevation: ',F14.9, ' deg', &
     &              ' Hour angle: ',F14.9, ' deg' )
      END IF
!
      END  PROGRAM  NERS_AZELHA  !#!#
