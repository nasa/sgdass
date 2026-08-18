      PROGRAM    NERS_RADEC
! ************************************************************************
! *                                                                      *
! *   Program NERS_RADEC  computes right ascension and declination,      *
! *   as well as their time derivatives, in the barycentric celestial    *
! *   coordinate system corrected for annual and  diurnal aberration at  *
! *   the moment of time TIM_TAI for a station with coordinates in the   *
! *   terrestrial crust-fixed coordinate system COO_TRS that observes    *
! *   a source with given azimuth and elevation in the topocentirc       *
! *   coordinate system AZ, EL. Elevation is considered with respect to  *
! *   the normal to the reference ellipsoid. Vertical deflection is      *
! *   not accounted. Optionally the elevation can be be corrected for    *
! *   refraction.                                                        *
! *                                                                      *
! *   This program can be used only for computation of right ascensions  *
! *   and declinations of objects beyond the Solar system, such as stars *
! *   or a galaxies.                                                     *
! *                                                                      *
! *   Usage: ners_radec date coo_trs az el [none|optic|radio]            *
! *                                                                      *
! *   where date is the date in YYYY.MM.DD_hh:mm:ss.sss format or now,   *
! *         coo_trs is the vector of station coordinates in the rotating *
! *                 crust-fixed coordinate system in meters,             *
! *         az      is the azimuth of the observed body in rad;          *
! *         el      is the elevation of the observed body in rad;        *
! *         [none|optic|radio] is optional computation of refraction:    *
! *                            none  -- not to account refraction,       *
! *                            optic -- to account refraction in optic   *
! *                                     range;                           *
! *                            radio -- to account for refraction in     *
! *                                     radio range.                     *
! *                                                                      *
! *  Units: radians for azimuth, elevation angle, right ascension, and   *
! *         declination; rad/s for their time derivatives.               *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 21-JUL-2025  NERS_RADEC   v2.0 (d)  L. Petrov  22-JUL-2025 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'ners.i'
      TYPE     ( NERS__TYPE ) :: NERS
      CHARACTER  DATE_STR*32, STR*32, STR1*32, REFR_MODE*5
      REAL*8     TIM_TAI, SEC, COO_TRS(3), RA, DEC, AZ, EL, &
     &           RA_RATE, DEC_RATE
      REAL*8     UTC_CUR, UTC_M_TAI
      INTEGER*4  MJD, IUER
      INTEGER*4, EXTERNAL :: TIME
!
      IF ( IARGC() < 6 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: ners_radec date coo_trs az el [none|optic|radio]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, DATE_STR )
!
           CALL GETARG ( 2, STR      )
           READ ( UNIT=STR, FMT=*, IOSTAT=IUER ) COO_TRS(1)
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4701, IUER, 'NERS_RADEC', 'Error in '// &
     &              'decoding the second argument, COO_TRS(1): a real '// &
     &              'number was expected, but got '//STR )
                CALL EXIT ( 1 ) 
           END IF
!
           CALL GETARG ( 3, STR      )
           READ ( UNIT=STR, FMT=*, IOSTAT=IUER ) COO_TRS(2)
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4702, IUER, 'NERS_RADEC', 'Error in '// &
     &              'decoding the third argument, COO_TRS(2): a real '// &
     &              'number was expected, but got '//STR )
                CALL EXIT ( 1 ) 
           END IF
!
           CALL GETARG ( 4, STR      )
           READ ( UNIT=STR, FMT=*, IOSTAT=IUER ) COO_TRS(3)
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4703, IUER, 'NERS_RADEC', 'Error in '// &
     &              'decoding the fourth argument, COO_TRS(3): a real '// &
     &              'number was expected, but got '//STR )
                CALL EXIT ( 1 ) 
           END IF
!
           CALL GETARG ( 5, STR      )
           READ ( UNIT=STR, FMT=*, IOSTAT=IUER ) AZ
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4704, IUER, 'NERS_RADEC', 'Error in '// &
     &              'decoding the fifth argument, azimuth: a real '// &
     &              'number was expected, but got '//STR )
                CALL EXIT ( 1 ) 
           END IF
!
           CALL GETARG ( 6, STR      )
           READ ( UNIT=STR, FMT=*, IOSTAT=IUER ) EL
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4705, IUER, 'NERS_RADEC', 'Error in '// &
     &              'decoding the fifth argument, elevation: a real '// &
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
                CALL ERR_LOG ( 4706, IUER, 'NERS_RADEC', 'Error in initializing '// &
     &              'NERS data structure' )
                CALL EXIT ( 1 )
           END IF
!
! -------- Load NERS
!
           IUER = -1
           CALL NERS_LOAD ( NERS, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 4707, IUER, 'NERS_RADEC', 'Error in '// &
     &              'an attempt to retrieve NERS forecast parameters '// &
     &              'form the remote server' )
                CALL EXIT ( 1 )
           END IF
!
! -------- Compute function UTC_M_TAI at the current moment of time
!
           IUER = -1
           CALL NERS_GET_UTCMTAI ( NERS, UTC_CUR, UTC_M_TAI, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4708, IUER, 'NERS_RADEC', 'Error in getting '// &
     &              'UTC minus TAI' )
                CALL EXIT ( 1 )
           END IF
           TIM_TAI = UTC_CUR - UTC_M_TAI
         ELSE
           IUER = -1
           CALL DATE_TO_TIME ( DATE_STR, MJD, SEC, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4709, IUER, 'NERS_RADEC', 'Wrong time string '// &
     &                         DATE_STR )
                CALL EXIT ( 1 ) 
           END IF
           TIM_TAI = (MJD - J2000__MJD)*86400.0D0 + SEC
!
! -------- Initialize NERS object
!
           IUER = -1
           CALL NERS_INIT ( 'NERS_CONFIG', NERS, TIM_TAI, TIM_TAI, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4710, IUER, 'NERS_RADEC', 'Error in '// &
     &              'initializing NERS data structure' )
                CALL EXIT ( 1 )
           END IF
!
! -------- Load NERS information
!
           IUER = -1
           CALL NERS_LOAD ( NERS, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 4711, IUER, 'NERS_RADEC', 'Error in '// &
     &              'an attempt to retrieve NERS forecast parameters '// &
     &              'form the remote server' )
                CALL EXIT ( 1 )
           END IF
      END IF
!
! --- Compute right ascension and decliation, as well as their first 
! --- time derivatves, for a given azimith and elevation at a station
! --- with given geocentric coordinates
!
      IUER = -1
      CALL NERS_RADEC_COMP ( NERS, TIM_TAI, COO_TRS, AZ, EL, REFR_MODE, &
     &                       RA, DEC, RA_RATE, DEC_RATE, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4712, IUER, 'NERS_RADEC', 'Error in '// &
     &         'computing right asension and declination' )
           CALL EXIT ( 1 ) 
      END IF
!
      WRITE ( 6, 110 ) AZ, EL, RA, DEC, RA_RATE, DEC_RATE
 110  FORMAT ( F14.11, 1X, F14.11, 1X, F16.13, 1X, F16.13, 1X, 1PD18.10, 1X, 1PD18.10 ) 
      CALL GETENVAR ( 'NERS_MAT_FORMAT', STR )
      IF ( STR(1:) == 'y' .OR. STR(1:) == 'Y' ) THEN
#ifdef WITH_PETOOLS
!
! -------- A bit more elaborated form of printing with hour/min/sec or deg/min/sec
! -------- using PETOOLS routines
!
           CALL RH_TAT ( RA,  4, STR,  IUER )
           CALL RG_TAT ( DEC, 3, STR1, IUER )
           IF ( DEC > 0.0D0 ) STR1(1:1) = '+'
           WRITE ( 6, 210 ) RA/DEG__TO__RAD, STR(1:15), DEC/DEG__TO__RAD, STR1(1:15)
 210       FORMAT ( ' Right ascension: ', F13.8,' deg  ', A,  ' Declination: ',F13.8, ' deg ', A )
#else
!
! -------- A more simple form of no PETOOLS is available
!
           WRITE ( 6, 220 ) RA/DEG__TO__RAD, DEC/DEG__TO__RAD
 220       FORMAT ( ' Right ascension: ', F13.8,' deg ', ' Declination: ',F13.8, ' deg' )
#endif
      END IF
!
      END  PROGRAM  NERS_RADEC  !#!#
