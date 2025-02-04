      SUBROUTINE VTD_UTC_TO_TAI ( VTD, MJD, UTC, TAI, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine VTD_UTC_TO_TAI find time TAI which corresponds   *
! *   to the value of function UTC(t) at epoch MJD. Or by another        *
! *   words, it finds the leap second which corresponds to the date      *
! *   of interest, subtracts it from UTC and get TAI.                    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      VTD ( RECORD    ) -- Object which keeps configuration and data  *
! *                           related to VLBI Theoretical Delay (VTD)    *
! *                           package.                                   *
! *      MJD ( INTEGER*4 ) -- Modified Julian date which corresponds to  *
! *                           the UTC calendar date of the midnight of   * 
! *                           the day of interest.                       *
! *      UTC ( REAL*8    ) -- Value of the function UTC(T) which         *
! *                           corresponds the the moment of interest.    *
! *                           Units: seconds.                            *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      TAI ( REAL*8    ) -- Time in TAI in seconds elapsed from        *
! *                           midnight.                                  *
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
! * ### 29-JAN-2004  VTD_UTC_TO_TAI  v2.1 (c) L. Petrov  01-JUL-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  MJD, IUER
      REAL*8     UTC, TAI, UTC_M_TAI
      REAL*8     UTC_TAG
      INTEGER*4  J1, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( VTD%LEAPSEC%STATUS .EQ. VTD__NERS ) THEN
           IF ( .NOT. ( VTD%NERS%FCS_STATUS == NERS__LOAD .OR. &
     &                  VTD%NERS%FCS_STATUS == NERS__COMP .OR. &
     &                  VTD%NERS%FCS_STATUS == NERS__INIT      ) ) THEN
                CALL ERR_LOG ( 2371, IUER, 'VTD_UTC_TO_TAI', 'Trap '// &
     &              'of internal control: NERS control file has not '// &
     &              'been loaded' )
                RETURN 
           END IF
!
           UTC_TAG = (MJD - J2000__MJD)*86400.0D0 + UTC
           CALL ERR_PASS ( IUER, IER )
           CALL NERS_GET_UTCMTAI ( VTD%NERS, UTC_TAG, UTC_M_TAI, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2372, IUER, 'VTD_UTC_TO_TAI', 'Failure '// &
     &              'in inquiring UTC minus TAI difference using NERS' )
                RETURN 
           END IF
           TAI = UTC - UTC_M_TAI
!
           CALL ERR_LOG ( 0, IUER )
           RETURN 
         ELSE
!
! -------- Check the leap second status
!
           IF ( VTD%LEAPSEC%STATUS .NE. VTD__LOAD ) THEN
                WRITE ( 6, * ) 'VTD%LEAPSEC%STATUS= ', VTD%LEAPSEC%STATUS
                CALL ERR_LOG ( 2374, IUER, 'VTD_UTC_TO_TAI', 'Leap second file '// &
     &              'has not been yet loaded' ) 
                RETURN 
           END IF
      END IF
!
! --- Check whether the requested data falls into the range dates for which
! --- the leap second is defined
!
      IF ( MJD .LT. VTD%LEAPSEC%MJD_LPS(1) ) THEN
           IER = -1
           WRITE ( 6, * ) ' MJD=',MJD, ' UTC=',UTC
           WRITE ( 6, * ) ' VTD%LEAPSEC%MJD_LPS(1) = ', VTD%LEAPSEC%MJD_LPS(1) 
           CALL ERR_LOG ( 2375, IUER, 'VTD_UTC_TO_TAI', 'Requested date '// &
     &          MJDSEC_TO_DATE ( MJD, UTC, IER )//' is too early. '//        &
     &          'Leap second file '// &
     &          VTD%CONF%FINAM_LEAPSEC(1:I_LEN(VTD%CONF%FINAM_LEAPSEC))// &
     &          ' has information only for the range [ '//  &
     &          MJDSEC_TO_DATE ( VTD%LEAPSEC%MJD_LPS(1),                    &
     &                           VTD%LEAPSEC%TAI_LPS(1), IER )//' , '//      &
     &          MJDSEC_TO_DATE ( VTD%LEAPSEC%MJD_LPS(VTD%LEAPSEC%L_LPS),    &
     &                           VTD%LEAPSEC%TAI_LPS(VTD%LEAPSEC%L_LPS), IER )// &
     &                           ' ]' )
           RETURN 
      END IF
!
      IF ( MJD .GT. VTD%LEAPSEC%MJD_LPS(VTD%LEAPSEC%L_LPS) ) THEN
           IER = -1
           CALL ERR_LOG ( 2376, IUER, 'VTD_UTC_TO_TAI', 'Requested date '// &
     &          MJDSEC_TO_DATE ( MJD, UTC, IER )//' is too late. '//         &
     &          'Leap second file '// &
     &          VTD%CONF%FINAM_LEAPSEC(1:I_LEN(VTD%CONF%FINAM_LEAPSEC))// &
     &          ' has information only for the range [ '//  &
     &          MJDSEC_TO_DATE ( VTD%LEAPSEC%MJD_LPS(1),                    &
     &                           VTD%LEAPSEC%TAI_LPS(1), IER )//' , '//      &
     &          MJDSEC_TO_DATE ( VTD%LEAPSEC%MJD_LPS(VTD%LEAPSEC%L_LPS),    &
     &                           VTD%LEAPSEC%TAI_LPS(VTD%LEAPSEC%L_LPS), IER )// &
     &                           ' ]' )
           RETURN 
      END IF
!
! --- Search for leap second
!
      DO 410 J1=1,VTD%LEAPSEC%L_LPS
         IF ( MJD .GE. VTD%LEAPSEC%MJD_LPS(J1) ) THEN
              TAI = UTC - VTD%LEAPSEC%UTC_M_TAI(J1)
            ELSE 
              GOTO 810
         END IF
 410  CONTINUE 
 810  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END  SUBROUTINE  VTD_UTC_TO_TAI 
