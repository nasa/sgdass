      SUBROUTINE GET_UTC_M_TAI ( NERS, MJD_UTC, UTC, UTC_M_TAI, IUER )
! ************************************************************************
! *                                                                      *
! *   Routien GET_UTC_M_TAI returns the value of function UTC_M_TAI      *
! *   on ecpoh {MJD_UTC, UTC}.                                           *
! *                                                                      *
! *  ### 26-OCT-2007 GET_UTC_M_TAI v2.0 (c)  L. Petrov  18-OCT-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'ners.i'
      INCLUDE   'ners_local.i'
      TYPE     ( NERS__TYPE ) :: NERS
      INTEGER*4  MJD_UTC, IUER 
      REAL*8     UTC, UTC_M_TAI 
      CHARACTER  NERS_CONFIG*128
      LOGICAL*1  LEX
      INTEGER*8  SIZE_I8
      REAL*8     UTC_TAG
      INTEGER*4  IS, UNIX_DATE, IER
      INTEGER*4, EXTERNAL :: FILE_INFO, ILEN, I_LEN, TIME
!
      IF ( NERS%FCS_STATUS .NE. NERS__INIT ) THEN
           CALL GETENVAR ( 'HOME', NERS_CONFIG )
           NERS_CONFIG = TRIM(NERS_CONFIG)//'/.ners_config'
           INQUIRE ( FILE=NERS_CONFIG, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                NERS_CONFIG = NERS__CONFIG
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL NERS_INIT ( NERS_CONFIG, NERS, -1.0D0, -1.0D0, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4881, IUER, 'GET_UTC_M_TAI', 'Error in '// &
     &              'initializing NERS data structure for '// &
     &              'getting UTC-minus-TAI' )
                RETURN 
           END IF
      END IF
!
      UTC_TAG = (MJD_UTC - J2000__MJD)*86400.0D0 + UTC
      CALL ERR_PASS ( IUER, IER )
      CALL NERS_GET_UTCMTAI ( NERS, UTC_TAG, UTC_M_TAI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4882, IUER, 'GET_UTC_M_TAI', 'Error in getting '// &
     &         'UTC minus TAI' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END  SUBROUTINE   GET_UTC_M_TAI  !#!#
