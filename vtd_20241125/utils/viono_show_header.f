      PROGRAM    VIONO_SHOW_HEADER
! ************************************************************************
! *                                                                      *
! *   Progtram  VIONO_SHOW_HEADER
! *                                                                      *
! * ### 14-FEB-2024 VIONO_SHOW_HEADER v1.0 (c) L. Petrov 14-FEB-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'viono.i'
      TYPE     ( IONO__TYPE ) :: VIO
      CHARACTER  START_DATE*30, STOP_DATE*30
      CHARACTER  FILV*128
      INTEGER*4  IUER 
      CHARACTER  MJDSEC_TO_DATE*30
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Viono_show_header viono_file'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILV     )
      END IF
!
      IUER = -1
      CALL VIO_GET_HEADER ( FILV, VIO, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4402, IUER, 'VIONO_TO_HEB', 'Error in reading the '// &
     &         ' header of the ionospheric model file '//FILV )
           CALL EXIT ( 1 ) 
      END IF
!
      START_DATE = MJDSEC_TO_DATE ( VIO%HEADER%MJD_BEG, VIO%HEADER%UTC_BEG, IUER )
      STOP_DATE  = MJDSEC_TO_DATE ( VIO%HEADER%MJD_BEG, VIO%HEADER%UTC_BEG + VIO%HEADER%NEPC*VIO%HEADER%TIM_STEP, IUER )
!
      WRITE ( 6, 110 ) 'LABEL:      ', VIO%HEADER%LABEL    ! File label
      WRITE ( 6, 110 ) 'MODEL:      ', VIO%HEADER%MODEL    ! Model name
      WRITE ( 6, 110 ) 'AUTHOR:     ', VIO%HEADER%AUTHOR   ! Name of the model author
      WRITE ( 6, 120 ) 'NLON:       ', VIO%HEADER%NLON     ! The number of grid steps along longitude axis
      WRITE ( 6, 120 ) 'NLAT:       ', VIO%HEADER%NLAT     ! The number of grid steps along latitude axis
      WRITE ( 6, 130 ) 'NEPC:       ', VIO%HEADER%NEPC     ! The number of grid steps along time axis
      WRITE ( 6, 110 ) 'START_TIME: ', START_DATE(1:16)    
      WRITE ( 6, 110 ) 'STOP_TIME:  ', STOP_DATE(1:16)      
      WRITE ( 6, 130 ) 'MJD_BEG:    ', VIO%HEADER%MJD_BEG  ! MJD at TAI of the first epoch
      WRITE ( 6, 150 ) 'UTC_BEG:    ', VIO%HEADER%UTC_BEG  ! UTC Time tag of the first epoch. Units: seconds
      WRITE ( 6, 150 ) 'TIM_STEP:   ', VIO%HEADER%TIM_STEP ! Time step along the time axis. Units: seconds
      WRITE ( 6, 150 ) 'LON_MIN:    ', VIO%HEADER%LON_MIN/DEG__TO__RAD  ! Longitude at the beginning of the longitude axis. Units: radians
      WRITE ( 6, 150 ) 'LAT_MIN:    ', VIO%HEADER%LAT_MIN/DEG__TO__RAD  ! Latitude  at the beginning of the latitude  axis. Units: radians
      WRITE ( 6, 150 ) 'LON_STEP:   ', VIO%HEADER%LON_STEP/DEG__TO__RAD ! Longitude step along the longitude axis. Units: radians
      WRITE ( 6, 150 ) 'LAT_STEP:   ', VIO%HEADER%LAT_STEP/DEG__TO__RAD ! Latitude  step along the latitude  axis. Units: radians
      WRITE ( 6, 170 ) 'HEIGHT:     ', VIO%HEADER%HEIGHT   ! Height of the ionosphere. Units: meters
      WRITE ( 6, 160 ) 'SCALE:      ', VIO%HEADER%SCALE    ! The scaling factor which should be applied to raw TEC values
      WRITE ( 6, 140 ) 'MISSING:    ', VIO%HEADER%MISSING  ! Missing value
!
 110  FORMAT ( A, A   )
 120  FORMAT ( A10, I3  )
 130  FORMAT ( A10, I6  )
 140  FORMAT ( A10, I12 )
 150  FORMAT ( A10, F7.1 )
 160  FORMAT ( A10, F8.3 )
 170  FORMAT ( A10, F11.1 )
      END  PROGRAM  VIONO_SHOW_HEADER  !#!#
